package frame

import (
	"bytes"
	"encoding/json/jsontext"
	"fmt"
	"strconv"
)

// O is a JSON object that keeps its members in the order they were written.
// Dev's frames are built from recorded frames, and the order the recording
// wrote is the order the server writes, so an O reads and writes members as
// they came. A nested object is an *O, an array a []any, a number its literal
// as a jsontext.Value.
type O struct {
	keys []string
	vals map[string]any
}

func newO() *O { return &O{vals: map[string]any{}} }

// obj builds an O from alternating keys and values.
func obj(kv ...any) *O {
	o := newO()
	for i := 0; i < len(kv); i += 2 {
		o.Set(kv[i].(string), kv[i+1])
	}
	return o
}

// Set writes a member, appending it when it is new and replacing it in place
// when it is not.
func (o *O) Set(k string, v any) *O {
	if _, ok := o.vals[k]; !ok {
		o.keys = append(o.keys, k)
	}
	o.vals[k] = norm(v)
	return o
}

// Del removes a member.
func (o *O) Del(k string) *O {
	if _, ok := o.vals[k]; !ok {
		return o
	}
	delete(o.vals, k)
	for i, x := range o.keys {
		if x == k {
			o.keys = append(o.keys[:i:i], o.keys[i+1:]...)
			break
		}
	}
	return o
}

// Get is a member, or nil when there is none.
func (o *O) Get(k string) any {
	if o == nil {
		return nil
	}
	return o.vals[k]
}

// Has reports whether the member exists, null included.
func (o *O) Has(k string) bool { _, ok := o.vals[k]; return ok }

// Str is a string member, empty when it is anything else.
func (o *O) Str(k string) string { s, _ := o.Get(k).(string); return s }

// Int is a whole-number member.
func (o *O) Int(k string) (int, bool) {
	v, ok := o.Get(k).(jsontext.Value)
	if !ok {
		return 0, false
	}
	n, err := strconv.Atoi(string(v))
	return n, err == nil
}

// Obj is an object member, nil when it is anything else.
func (o *O) Obj(k string) *O { x, _ := o.Get(k).(*O); return x }

// List is an array member.
func (o *O) List(k string) []any { x, _ := o.Get(k).([]any); return x }

// Keys are the member names in order.
func (o *O) Keys() []string { return o.keys }

// Clone is a deep copy.
func (o *O) Clone() *O {
	c := newO()
	for _, k := range o.keys {
		c.Set(k, cloneValue(o.vals[k]))
	}
	return c
}

func cloneValue(v any) any {
	switch x := v.(type) {
	case *O:
		return x.Clone()
	case []any:
		c := make([]any, len(x))
		for i := range x {
			c[i] = cloneValue(x[i])
		}
		return c
	}
	return v
}

// norm turns the Go values frames are built from into the ones an O holds.
func norm(v any) any {
	switch x := v.(type) {
	case int:
		return jsontext.Value(strconv.Itoa(x))
	case int64:
		return jsontext.Value(strconv.FormatInt(x, 10))
	case uint64:
		return jsontext.Value(strconv.FormatUint(x, 10))
	case []*O:
		l := make([]any, len(x))
		for i := range x {
			l[i] = x[i]
		}
		return l
	case []string:
		l := make([]any, len(x))
		for i := range x {
			l[i] = x[i]
		}
		return l
	}
	return v
}

// MarshalJSON writes the members in order.
func (o *O) MarshalJSON() ([]byte, error) {
	var b bytes.Buffer
	if err := o.write(&b); err != nil {
		return nil, err
	}
	return b.Bytes(), nil
}

func (o *O) write(b *bytes.Buffer) error {
	b.WriteByte('{')
	for i, k := range o.keys {
		if i > 0 {
			b.WriteByte(',')
		}
		if err := writeValue(b, k); err != nil {
			return err
		}
		b.WriteByte(':')
		if err := writeValue(b, o.vals[k]); err != nil {
			return err
		}
	}
	b.WriteByte('}')
	return nil
}

func writeValue(b *bytes.Buffer, v any) error {
	switch x := v.(type) {
	case nil:
		b.WriteString("null")
	case bool:
		b.WriteString(strconv.FormatBool(x))
	case string:
		b.Write(quote(x))
	case jsontext.Value:
		b.Write(x)
	case *O:
		return x.write(b)
	case []any:
		b.WriteByte('[')
		for i := range x {
			if i > 0 {
				b.WriteByte(',')
			}
			if err := writeValue(b, x[i]); err != nil {
				return err
			}
		}
		b.WriteByte(']')
	default:
		return fmt.Errorf("frame: cannot write %T", v)
	}
	return nil
}

// quote is s as a JSON string. HTML is not escaped, as JSON.stringify does not.
func quote(s string) []byte {
	var e bytes.Buffer
	enc := jsontext.NewEncoder(&e, jsontext.EscapeForHTML(false), jsontext.EscapeForJS(false))
	_ = enc.WriteToken(jsontext.String(s))
	return bytes.TrimRight(e.Bytes(), "\n")
}

// UnmarshalJSON reads an object, keeping the order of its members.
func (o *O) UnmarshalJSON(b []byte) error {
	v, err := decode(jsontext.NewDecoder(bytes.NewReader(b)))
	if err != nil {
		return err
	}
	x, ok := v.(*O)
	if !ok {
		return fmt.Errorf("frame: not an object")
	}
	*o = *x
	return nil
}

// parse reads one JSON value into O, []any, string, bool, nil or a literal.
func parse(b []byte) (any, error) {
	return decode(jsontext.NewDecoder(bytes.NewReader(b)))
}

func decode(d *jsontext.Decoder) (any, error) {
	switch d.PeekKind() {
	case '{':
		if _, err := d.ReadToken(); err != nil {
			return nil, err
		}
		o := newO()
		for d.PeekKind() != '}' {
			k, err := d.ReadToken()
			if err != nil {
				return nil, err
			}
			key := k.String() // a Token is good until the next read
			v, err := decode(d)
			if err != nil {
				return nil, err
			}
			o.Set(key, v)
		}
		_, err := d.ReadToken()
		return o, err
	case '[':
		if _, err := d.ReadToken(); err != nil {
			return nil, err
		}
		l := []any{}
		for d.PeekKind() != ']' {
			v, err := decode(d)
			if err != nil {
				return nil, err
			}
			l = append(l, v)
		}
		_, err := d.ReadToken()
		return l, err
	case '"':
		t, err := d.ReadToken()
		return t.String(), err
	case 'n':
		_, err := d.ReadToken()
		return nil, err
	case 't', 'f':
		t, err := d.ReadToken()
		return t.Bool(), err
	}
	v, err := d.ReadValue()
	return jsontext.Value(bytes.Clone(v)), err
}
