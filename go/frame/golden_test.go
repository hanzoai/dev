package frame

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strings"
	"testing"
)

// A frame is known by what it is: the method that asked for it, and the
// discriminator its value carries.
func category(tags map[int]string, f *O, v any) string {
	id, _ := f.Int("requestId")
	c := tags[id]
	if o, ok := v.(*O); ok {
		for _, k := range []string{"kind", "type", "_tag"} {
			if s := o.Str(k); s != "" {
				c += "/" + s
			}
		}
		if e := o.Obj("event"); e != nil {
			c += "/" + e.Str("type")
			if p := e.Obj("payload"); p != nil && e.Str("type") == "node.updated" {
				c += "/" + p.Str("kind")
			}
			if p := e.Obj("payload"); p != nil && e.Str("type") == "turn-item.updated" {
				c += "/" + p.Str("type")
			}
			if p := e.Obj("payload"); p != nil && e.Str("type") == "message.updated" {
				c += "/" + p.Str("role")
			}
		}
	}
	return c
}

// paths are the member paths of a value, an array's elements merged.
func paths(prefix string, v any, out map[string]bool) {
	switch x := v.(type) {
	case *O:
		for _, k := range x.Keys() {
			p := prefix + "." + k
			out[p] = true
			paths(p, x.Get(k), out)
		}
	case []any:
		for _, e := range x {
			paths(prefix+"[]", e, out)
		}
	}
}

// shapes groups what a log's server-to-client frames carry by category: for
// each, every member path of any sample, and those of all.
type shape struct {
	any, all map[string]bool
	n        int
}

func shapes(log []logged) map[string]*shape {
	tags := map[int]string{}
	out := map[string]*shape{}
	add := func(c string, v any) {
		s := out[c]
		if s == nil {
			s = &shape{any: map[string]bool{}}
			out[c] = s
		}
		p := map[string]bool{}
		paths("", v, p)
		for k := range p {
			s.any[k] = true
		}
		if s.all == nil {
			s.all = p
		} else {
			for k := range s.all {
				if !p[k] {
					delete(s.all, k)
				}
			}
		}
		s.n++
	}
	for _, l := range log {
		f := l.f
		switch {
		case l.dir == "out" && f.Str("_tag") == "Request":
			id, _ := f.Int("id")
			tags[id] = f.Str("tag")
		case l.dir == "in" && f.Str("_tag") == "Chunk":
			for _, v := range f.List("values") {
				add(category(tags, f, v), v)
			}
		case l.dir == "in" && f.Str("_tag") == "Exit":
			e := f.Obj("exit")
			if e.Str("_tag") == "Success" {
				add(category(tags, f, e.Get("value"))+"/exit", e.Get("value"))
			}
		}
	}
	return out
}

func recorded(t *testing.T) []logged {
	t.Helper()
	f, err := os.Open("testdata/chat.jsonl")
	if err != nil {
		t.Fatal(err)
	}
	defer f.Close()
	var log []logged
	sc := bufio.NewScanner(f)
	sc.Buffer(nil, 64<<20)
	for sc.Scan() {
		v, err := parse(sc.Bytes())
		if err != nil {
			t.Fatal(err)
		}
		o := v.(*O)
		log = append(log, logged{o.Str("step"), o.Str("dir"), o.Obj("f")})
	}
	return log
}

// Every frame the Adapter writes has the members the real server writes for
// the same thing: none the recording lacks, and every one it always has.
func compare(t *testing.T, got []logged) {
	t.Helper()
	want := shapes(recorded(t))
	have := shapes(got)
	var cats []string
	for c := range have {
		cats = append(cats, c)
	}
	sort.Strings(cats)
	checked := 0
	for _, c := range cats {
		w := want[c]
		if w == nil {
			continue
		}
		checked++
		var extra, missing []string
		for p := range have[c].any {
			if !w.any[p] {
				extra = append(extra, p)
			}
		}
		for p := range w.all {
			// An array holds entities of several kinds, and a sample need not
			// hold them all.
			if !have[c].any[p] && !strings.Contains(p, "[]") {
				missing = append(missing, p)
			}
		}
		sort.Strings(extra)
		sort.Strings(missing)
		if len(extra)+len(missing) > 0 {
			t.Errorf("%s\n  not in the recording: %s\n  missing:              %s", c, strings.Join(extra, " "), strings.Join(missing, " "))
		}
	}
	if checked < 25 {
		t.Errorf("only %d of the Adapter's %d frame kinds are in the recording", checked, len(cats))
	}
	fmt.Fprintf(os.Stderr, "golden: %d frame kinds compared\n", checked)
}

// compareCall holds a body the session's calls answer to the one the Dev server
// answers over HTTP.
func compareCall(t *testing.T, name string, got *O) {
	t.Helper()
	b, err := os.ReadFile("testdata/http.json")
	if err != nil {
		t.Fatal(err)
	}
	v, err := parse(b)
	if err != nil {
		t.Fatal(err)
	}
	want := map[string]bool{}
	paths("", v.(*O).Obj(name), want)
	have := map[string]bool{}
	paths("", got, have)
	for p := range have {
		if !want[p] {
			t.Errorf("%s: %s is not in the recording", name, p)
		}
	}
	for p := range want {
		if !have[p] && !strings.Contains(p, "[]") {
			t.Errorf("%s: %s is missing", name, p)
		}
	}
}
