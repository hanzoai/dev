package dev

import (
	"encoding/json/jsontext"
	json "encoding/json/v2"
	"fmt"
	"reflect"
)

// The types below are crates/protocol, field for field. A Rust enum is a Go
// struct whose fields are its variants, named as Rust names them: a pointer
// for a variant that carries a value, a bool for one that carries none.
// Exactly one is set. The encoder refuses any other count, and Step and New
// answer that with ErrMalformed, so a union that says two things, or nothing,
// never reaches the core.
//
// Encoded, a union is what serde writes for an enum: {"Turn":{...}} for a
// variant with a value, "Timer" for one without. An Option is a pointer, and a
// nil one is null.

// Config is what a session is born with.
type Config struct {
	// Model is named in every Model ask; the host routes it.
	Model *string `json:"model"`
	// Prelude is the standing instructions every ask carries.
	Prelude *string `json:"prelude"`
}

// Event is what the host tells the core.
type Event struct {
	// Turn is a person asking for work.
	Turn *Turn
	// Model answers the ask dispatched under its id.
	Model *Answer
	// Exec is a command dispatched under its id exiting.
	Exec *Output
	// File is a read, write or patch dispatched under its id finishing.
	File *Output
	// Git is a git command dispatched under its id exiting.
	Git *Output
	// Browse is a browser request dispatched under its id returning.
	Browse *Output
	// Timer is time passing; a long turn should checkpoint.
	Timer bool
	// Cancel stops the turn it names.
	Cancel *Cancel
}

// Action is what the core asks the host to do, under the id the host's ledger
// records it by and its result names.
type Action struct {
	ID uint64 `json:"id"`
	Op Op     `json:"op"`
}

// Op is the ten things a core can ask for.
type Op struct {
	Model  *Ask
	Read   *Read
	Write  *Write
	Patch  *Patch
	Exec   *Exec
	Git    *Git
	Browse *Browse
	// Emit is text to show whoever is watching.
	Emit *string
	// Save asks the host to persist the core's snapshot at this point in the
	// sequence.
	Save bool
	// Done ends a turn.
	Done *Done
}

// Turn is a prompt, under the id the host's journal gave the request. Ids
// rise and are never zero: the core refuses one at or below the highest it
// has accepted, so a redelivered turn asks for nothing.
type Turn struct {
	ID     uint64 `json:"id"`
	Prompt string `json:"prompt"`
}

// Cancel names the turn to stop, by the id its Turn carried.
type Cancel struct {
	Turn uint64 `json:"turn"`
}

// Done is the end of the turn Turn asked for.
type Done struct {
	Turn    uint64  `json:"turn"`
	Outcome Outcome `json:"outcome"`
}

// Ask is a request for the model: the whole conversation, every time.
type Ask struct {
	Model *string `json:"model"`
	// Prelude is the session's standing instructions, ahead of the
	// conversation.
	Prelude  *string   `json:"prelude"`
	Messages []Message `json:"messages"`
}

// Read asks for a file's text.
//
// A path in this protocol is relative to the workspace the host mounted for
// the session. The core refuses a call whose path would leave it, so no
// Action names one; resolving a path against the real tree, symlinks and
// all, is still the host's.
type Read struct {
	Path string `json:"path"`
}

// Write replaces a file's text.
type Write struct {
	Path string `json:"path"`
	Text string `json:"text"`
}

// Patch is a diff and the one file it edits: the host applies Diff to Path
// and to nothing else.
type Patch struct {
	Path string `json:"path"`
	Diff string `json:"diff"`
}

// Exec runs a command, in Cwd when it names one.
type Exec struct {
	Argv []string `json:"argv"`
	Cwd  *string  `json:"cwd"`
}

// Git runs git.
type Git struct {
	Argv []string `json:"argv"`
}

// Browse is a browser request, with a Body when it sends one.
type Browse struct {
	URL  string  `json:"url"`
	Body *string `json:"body"`
}

// Outcome is why a turn ended.
type Outcome struct {
	Complete  bool
	Cancelled bool
}

// Answer is a model's reply to the ask dispatched under ID.
type Answer struct {
	ID    uint64 `json:"id"`
	Reply Reply  `json:"reply"`
}

// Reply is text, tool calls, or both.
type Reply struct {
	Text  *string `json:"text"`
	Calls []Call  `json:"calls"`
}

// Call is a tool call, already typed: the host maps a provider's function
// call onto one, and the core never dispatches on a string.
type Call struct {
	Read   *Read
	Write  *Write
	Patch  *Patch
	Exec   *Exec
	Git    *Git
	Browse *Browse
}

// Dispatch is a call the core sent out, under the id its result will name.
type Dispatch struct {
	ID   uint64 `json:"id"`
	Call Call   `json:"call"`
}

// Output is the result of an effect, naming the action it answers.
type Output struct {
	ID     uint64 `json:"id"`
	Text   string `json:"text"`
	Failed bool   `json:"failed"`
}

// Message is one entry of the conversation the core carries. An Agent entry
// carries its calls under the ids they were dispatched with, and a Tool entry
// names the id it answers, so a provider that wants the call ahead of its
// result can be driven straight from the transcript.
type Message struct {
	// User is a person asking for work.
	User *string
	// Agent is the model speaking, asking for tools, or both.
	Agent *Agent
	// Tool is a tool answering the call dispatched under its id.
	Tool *Tool
}

// Agent is what the model said and the calls it made, each under the id the
// core minted for it, a call it refused included.
type Agent struct {
	Text  *string    `json:"text"`
	Calls []Dispatch `json:"calls"`
}

// Tool is the answer to the call dispatched under ID.
type Tool struct {
	ID     uint64 `json:"id"`
	Text   string `json:"text"`
	Failed bool   `json:"failed"`
}

func (e Event) MarshalJSONTo(enc *jsontext.Encoder) error      { return encode(enc, e) }
func (e *Event) UnmarshalJSONFrom(dec *jsontext.Decoder) error { return decode(dec, e) }

func (o Op) MarshalJSONTo(enc *jsontext.Encoder) error      { return encode(enc, o) }
func (o *Op) UnmarshalJSONFrom(dec *jsontext.Decoder) error { return decode(dec, o) }

func (o Outcome) MarshalJSONTo(enc *jsontext.Encoder) error      { return encode(enc, o) }
func (o *Outcome) UnmarshalJSONFrom(dec *jsontext.Decoder) error { return decode(dec, o) }

func (c Call) MarshalJSONTo(enc *jsontext.Encoder) error      { return encode(enc, c) }
func (c *Call) UnmarshalJSONFrom(dec *jsontext.Decoder) error { return decode(dec, c) }

func (m Message) MarshalJSONTo(enc *jsontext.Encoder) error      { return encode(enc, m) }
func (m *Message) UnmarshalJSONFrom(dec *jsontext.Decoder) error { return decode(dec, m) }

// encode writes the one variant a union sets.
func encode(enc *jsontext.Encoder, union any) error {
	v := reflect.ValueOf(union)
	t := v.Type()
	set := -1
	for i := range v.NumField() {
		if v.Field(i).IsZero() {
			continue
		}
		if set >= 0 {
			return fmt.Errorf("%s sets both %s and %s", t.Name(), t.Field(set).Name, t.Field(i).Name)
		}
		set = i
	}
	if set < 0 {
		return fmt.Errorf("%s sets no variant", t.Name())
	}
	name, value := t.Field(set).Name, v.Field(set)
	if value.Kind() == reflect.Bool {
		return enc.WriteToken(jsontext.String(name))
	}
	if err := enc.WriteToken(jsontext.BeginObject); err != nil {
		return err
	}
	if err := enc.WriteToken(jsontext.String(name)); err != nil {
		return err
	}
	if err := json.MarshalEncode(enc, value.Interface()); err != nil {
		return err
	}
	return enc.WriteToken(jsontext.EndObject)
}

// decode reads one variant into a union, and only one: a name the union does
// not have, or an object naming two, is refused rather than guessed at.
func decode(dec *jsontext.Decoder, union any) error {
	v := reflect.ValueOf(union).Elem()
	t := v.Type()
	v.SetZero()
	tok, err := dec.ReadToken()
	if err != nil {
		return err
	}
	switch tok.Kind() {
	case '"':
		f, ok := t.FieldByName(tok.String())
		if !ok || f.Type.Kind() != reflect.Bool {
			return fmt.Errorf("%s has no variant %q without a value", t.Name(), tok.String())
		}
		v.FieldByIndex(f.Index).SetBool(true)
		return nil
	case '{':
		if tok, err = dec.ReadToken(); err != nil {
			return err
		}
		f, ok := t.FieldByName(tok.String())
		if !ok || f.Type.Kind() != reflect.Pointer {
			return fmt.Errorf("%s has no variant %q with a value", t.Name(), tok.String())
		}
		value := reflect.New(f.Type.Elem())
		if err := json.UnmarshalDecode(dec, value.Interface()); err != nil {
			return err
		}
		v.FieldByIndex(f.Index).Set(value)
		if tok, err = dec.ReadToken(); err != nil {
			return err
		}
		if tok.Kind() != '}' {
			return fmt.Errorf("%s names more than one variant", t.Name())
		}
		return nil
	}
	return fmt.Errorf("%s is a variant name or an object of one, not %s", t.Name(), tok.Kind())
}
