// Command framed serves one Dev environment-protocol session over stdio, for a
// client that cannot speak ZAP: frames in and out are a u32 little-endian
// length and then one frame, as a ZAP session carries them.
//
// It is the reference Effects. The model is any OpenAI-compatible endpoint
// (-model-url), streamed; with none, a stand-in that repeats the last message
// back. Anything that needs a workspace is refused, and the state is kept in
// -state.
package main

import (
	"bufio"
	"bytes"
	"context"
	"encoding/binary"
	json "encoding/json/v2"
	"errors"
	"flag"
	"fmt"
	"io"
	"log"
	"net/http"
	"net/url"
	"os"
	"strings"
	"sync"

	dev "github.com/hanzoai/dev/go"
	"github.com/hanzoai/dev/go/frame"
)

func main() {
	envID := flag.String("env", "ec08e842-07b8-4297-87c9-3b1f4a91df02", "environment id the client believes in")
	project := flag.String("project", "", "id of the chat project; derived from the environment when empty")
	seq := flag.Int("seq", 0, "sequence of the snapshot the client already holds")
	label := flag.String("label", "Hanzo Cloud", "environment label")
	query := flag.String("query", "frameParts=1", "the query string the client opened its socket with")
	modelURL := flag.String("model-url", "", "base URL of an OpenAI-compatible endpoint")
	modelName := flag.String("model", "", "model to ask for; the thread's own when empty")
	statePath := flag.String("state", "", "file to keep the session's state in")
	flag.Parse()

	ctx := context.Background()
	core, err := dev.Open(ctx)
	if err != nil {
		log.Fatal(err)
	}
	defer core.Close(ctx)
	q, err := url.ParseQuery(*query)
	if err != nil {
		log.Fatal(err)
	}
	var state []byte
	if *statePath != "" {
		if state, err = os.ReadFile(*statePath); err != nil && !errors.Is(err, os.ErrNotExist) {
			log.Fatal(err)
		}
	}
	out := bufio.NewWriter(os.Stdout)
	var mu sync.Mutex
	a, err := frame.New(frame.Config{
		Core: core, Effects: &effects{url: *modelURL, model: *modelName, path: *statePath},
		Environment: frame.Environment{ID: *envID, Label: *label}, Project: *project, Sequence: *seq, Query: q, State: state,
		Send: func(b []byte) error {
			mu.Lock()
			defer mu.Unlock()
			if err := binary.Write(out, binary.LittleEndian, uint32(len(b))); err != nil {
				return err
			}
			if _, err := out.Write(b); err != nil {
				return err
			}
			return out.Flush()
		},
	})
	if err != nil {
		log.Fatal(err)
	}
	defer a.Close(ctx)

	in := bufio.NewReader(os.Stdin)
	for {
		var n uint32
		if err := binary.Read(in, binary.LittleEndian, &n); err != nil {
			if errors.Is(err, io.EOF) {
				return
			}
			log.Fatal(err)
		}
		b := make([]byte, n)
		if _, err := io.ReadFull(in, b); err != nil {
			log.Fatal(err)
		}
		err := a.Handle(ctx, b)
		var ns *frame.NeedsSandbox
		if errors.As(err, &ns) {
			err = a.Refuse(ns)
		}
		if err != nil {
			log.Print(err)
		}
	}
}

type effects struct {
	url, model, path string
}

func (e *effects) Perform(context.Context, dev.Op) (dev.Output, error) {
	return dev.Output{}, &frame.NeedsSandbox{Method: "workspace action"}
}

func (e *effects) Save(_ context.Context, state []byte) error {
	if e.path == "" {
		return nil
	}
	return os.WriteFile(e.path, state, 0o600)
}

func (e *effects) Model(ctx context.Context, ask dev.Ask, delta func(string)) (dev.Reply, error) {
	if e.url == "" {
		return echo(ask, delta), nil
	}
	model := e.model
	if model == "" && ask.Model != nil {
		model = *ask.Model
	}
	type msg struct {
		Role    string `json:"role"`
		Content string `json:"content"`
	}
	var ms []msg
	if ask.Prelude != nil {
		ms = append(ms, msg{"system", *ask.Prelude})
	}
	for _, m := range ask.Messages {
		switch {
		case m.User != nil:
			ms = append(ms, msg{"user", *m.User})
		case m.Agent != nil && m.Agent.Text != nil:
			ms = append(ms, msg{"assistant", *m.Agent.Text})
		case m.Tool != nil:
			ms = append(ms, msg{"user", m.Tool.Text})
		}
	}
	body, err := json.Marshal(map[string]any{"model": model, "messages": ms, "stream": true})
	if err != nil {
		return dev.Reply{}, err
	}
	req, err := http.NewRequestWithContext(ctx, http.MethodPost, strings.TrimRight(e.url, "/")+"/chat/completions", bytes.NewReader(body))
	if err != nil {
		return dev.Reply{}, err
	}
	req.Header.Set("Content-Type", "application/json")
	if k := os.Getenv("HANZO_API_KEY"); k != "" {
		req.Header.Set("Authorization", "Bearer "+k)
	}
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return dev.Reply{}, err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		b, _ := io.ReadAll(io.LimitReader(resp.Body, 512))
		return dev.Reply{}, fmt.Errorf("model: %s: %s", resp.Status, b)
	}
	var text strings.Builder
	sc := bufio.NewScanner(resp.Body)
	for sc.Scan() {
		data, ok := strings.CutPrefix(sc.Text(), "data: ")
		if !ok || data == "[DONE]" {
			continue
		}
		var chunk struct {
			Choices []struct {
				Delta struct{ Content string } `json:"delta"`
			} `json:"choices"`
		}
		if err := json.Unmarshal([]byte(data), &chunk); err != nil {
			return dev.Reply{}, err
		}
		for _, c := range chunk.Choices {
			if c.Delta.Content != "" {
				text.WriteString(c.Delta.Content)
				delta(c.Delta.Content)
			}
		}
	}
	if err := sc.Err(); err != nil {
		return dev.Reply{}, err
	}
	s := text.String()
	return dev.Reply{Text: &s}, nil
}

// echo repeats the last message, a word at a time.
func echo(ask dev.Ask, delta func(string)) dev.Reply {
	said := ""
	if len(ask.Messages) > 0 {
		if u := ask.Messages[len(ask.Messages)-1].User; u != nil {
			said = *u
		}
	}
	text := fmt.Sprintf("You said: %q", said)
	if ask.Prelude != nil && strings.HasPrefix(*ask.Prelude, "Reply with a title") {
		text = "Chat"
	}
	for i, w := range strings.Fields(text) {
		if i > 0 {
			w = " " + w
		}
		delta(w)
	}
	return dev.Reply{Text: &text}
}
