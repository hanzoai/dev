package dev

import (
	"errors"
	"fmt"
	"maps"
	"slices"
	"strings"
	"testing"

	"github.com/tetratelabs/wazero"
	"github.com/tetratelabs/wazero/api"
)

// compiled is the embedded module as wazero reads it, for the tests that look
// at its shape rather than run it.
func compiled(t *testing.T) wazero.CompiledModule {
	t.Helper()
	rt := wazero.NewRuntime(t.Context())
	t.Cleanup(func() { _ = rt.Close(t.Context()) })
	mod, err := rt.CompileModule(t.Context(), module)
	if err != nil {
		t.Fatal(err)
	}
	return mod
}

// The import list is the proof that the core touches nothing: no file, no
// socket, no clock, no randomness. A module that grows it has given the loop
// an effect of its own.
func TestTheModuleImportsFourWASICallsAndNothingElse(t *testing.T) {
	mod := compiled(t)
	var imports []string
	for _, f := range mod.ImportedFunctions() {
		module, name, _ := f.Import()
		imports = append(imports, module+"."+name)
	}
	slices.Sort(imports)
	want := []string{
		"wasi_snapshot_preview1.environ_get",
		"wasi_snapshot_preview1.environ_sizes_get",
		"wasi_snapshot_preview1.fd_write",
		"wasi_snapshot_preview1.proc_exit",
	}
	if !slices.Equal(imports, want) {
		t.Errorf("imports\n got %v\nwant %v", imports, want)
	}
	if n := len(mod.ImportedMemories()); n != 0 {
		t.Errorf("the module imports %d memories", n)
	}
}

// The exports are dev.h, lowered to wasm32. This package calls them by
// position, so a signature that moved is a call that reads the wrong argument.
func TestTheModuleExportsMemoryAndTheNineCalls(t *testing.T) {
	mod := compiled(t)
	want := map[string]string{
		"dev_abi":      "() (i32)",
		"dev_new":      "(i32 i32 i32) (i32)",
		"dev_step":     "(i64 i32 i32 i32) (i32)",
		"dev_snapshot": "(i64 i32) (i32)",
		"dev_restore":  "(i32 i32 i32) (i32)",
		"dev_drop":     "(i64) ()",
		// dev_free(dev_buf buf) takes twelve bytes by value, and the wasm32 C
		// ABI passes a struct that size as a pointer to a copy of it.
		"dev_free":    "(i32) ()",
		"dev_alloc":   "(i32) (i32)",
		"dev_release": "(i32 i32) ()",
	}
	got := map[string]string{}
	for name, f := range mod.ExportedFunctions() {
		got[name] = signature(f)
	}
	if !maps.Equal(got, want) {
		t.Errorf("exported functions\n got %v\nwant %v", got, want)
	}
	memories := slices.Collect(maps.Keys(mod.ExportedMemories()))
	if !slices.Equal(memories, []string{"memory"}) {
		t.Errorf("exported memories %v, want [memory]", memories)
	}
}

func signature(f api.FunctionDefinition) string {
	names := func(types []api.ValueType) string {
		var out []string
		for _, t := range types {
			out = append(out, api.ValueTypeName(t))
		}
		return "(" + strings.Join(out, " ") + ")"
	}
	return names(f.ParamTypes()) + " " + names(f.ResultTypes())
}

func TestTheModuleSpeaksThisPackagesABI(t *testing.T) {
	c := core(t)
	inst, err := c.module.Start(t.Context())
	if err != nil {
		t.Fatal(err)
	}
	defer inst.Close(t.Context())
	out, err := inst.Call(t.Context(), "dev_abi")
	if err != nil {
		t.Fatal(err)
	}
	if abi := uint32(out[0]); abi != ABI {
		t.Errorf("dev_abi() = %d, want %d", abi, ABI)
	}
}

// A module that answers dev_abi with another version is refused at Open, not
// discovered one malformed step at a time.
func TestOpenRefusesAnotherABI(t *testing.T) {
	for _, abi := range []byte{0, 2} {
		// (module (func (export "dev_abi") (result i32) i32.const <abi>))
		foreign := []byte{
			0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,
			0x01, 0x05, 0x01, 0x60, 0x00, 0x01, 0x7f,
			0x03, 0x02, 0x01, 0x00,
			0x07, 0x0b, 0x01, 0x07, 'd', 'e', 'v', '_', 'a', 'b', 'i', 0x00, 0x00,
			0x0a, 0x06, 0x01, 0x04, 0x00, 0x41, abi, 0x0b,
		}
		c, err := open(t.Context(), foreign)
		if err == nil {
			_ = c.Close(t.Context())
			t.Fatalf("a module speaking ABI %d was opened", abi)
		}
		if want := fmt.Sprintf("speaks ABI %d", abi); !strings.Contains(err.Error(), want) {
			t.Errorf("ABI %d refused for another reason: %v", abi, err)
		}
	}
}

// Every status dev.h names is an error errors.Is can match, and none is a
// bare number.
func TestEveryStatusIsASentinel(t *testing.T) {
	codes := map[int32]error{
		-1: ErrHandle, -2: ErrMalformed, -3: ErrPoison, -4: ErrPanic,
		-5: ErrNull, -6: ErrBusy, -7: ErrFull,
	}
	if err := status(0, nil); err != nil {
		t.Errorf("DEV_OK is %v", err)
	}
	for code, want := range codes {
		if err := status(uint64(uint32(code)), nil); !errors.Is(err, want) {
			t.Errorf("status %d is %v, want %v", code, err, want)
		}
	}
	for _, code := range []int32{1, -8} {
		if err := status(uint64(uint32(code)), nil); err == nil {
			t.Errorf("status %d is not an error", code)
		}
	}
}
