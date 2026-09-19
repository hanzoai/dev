package dev_test

import (
	"context"
	"fmt"
	"log"

	dev "github.com/hanzoai/dev/go"
)

// A host drives the loop: it steps the session with an event, performs the
// actions that come back, and steps it again with what they observed. Here
// the model is a stand-in that reads one file and then answers.
func Example() {
	ctx := context.Background()
	core, err := dev.Open(ctx)
	if err != nil {
		log.Fatal(err)
	}
	defer core.Close(ctx)

	s, err := core.New(ctx, dev.Config{Model: new("zen5")})
	if err != nil {
		log.Fatal(err)
	}
	defer s.Close(ctx)

	var state []byte
	events := []dev.Event{{Turn: &dev.Turn{ID: 1, Prompt: "What does main.go do?"}}}
	for len(events) > 0 {
		actions, err := s.Step(ctx, events[0])
		if err != nil {
			log.Fatal(err)
		}
		events = events[1:]
		for _, a := range actions {
			switch op := a.Op; {
			case op.Model != nil:
				events = append(events, dev.Event{Model: &dev.Answer{ID: a.ID, Reply: model(op.Model)}})
			case op.Read != nil:
				fmt.Println("read", op.Read.Path)
				events = append(events, dev.Event{File: &dev.Output{ID: a.ID, Text: "package main\n\nfunc main() {}\n"}})
			case op.Emit != nil:
				fmt.Println(*op.Emit)
			case op.Save:
				// A real host writes this beside its journal, at this action's id.
				if state, err = s.Snapshot(ctx); err != nil {
					log.Fatal(err)
				}
			case op.Done != nil:
				fmt.Println("done", op.Done.Turn)
			}
		}
	}
	fmt.Println(len(state) > 0)
	// Output:
	// read main.go
	// It declares package main and does nothing.
	// done 1
	// true
}

// model stands in for a provider: it asks for main.go, and answers once the
// file is in the conversation.
func model(ask *dev.Ask) dev.Reply {
	if last := ask.Messages[len(ask.Messages)-1]; last.Tool == nil {
		return dev.Reply{Calls: []dev.Call{{Read: &dev.Read{Path: "main.go"}}}}
	}
	return dev.Reply{Text: new("It declares package main and does nothing.")}
}
