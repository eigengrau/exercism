// Package proverb generates proverbial rhymes.
package proverb

import (
	"bytes"
	"strings"
	"text/template"
)

var (
	rhymeDefs = map[string]string{
		"wantOf": `
{{- range .Items | chain -}}
For want of a {{ .Left }} the {{ .Right }} was lost.
{{ end -}}
And all for the want of a {{ .Items | first }}.`,
	}
	rhymes template.Template
)

func init() {
	rhymes.Funcs(funcMap)
	for name, data := range rhymeDefs {
		tpl := rhymes.New(name)
		tpl.Parse(data)
	}
}

// proverbData is data passed to rhyme templates.
type proverbData struct {
	Items []string
}

// pair contains two strings.
type pair struct {
	Left  string
	Right string
}

// funcMap maps function names to template functions.
var funcMap = template.FuncMap{
	"first": func(l []string) string {
		return l[0]
	},
	"chain": func(l []string) []pair {
		if len(l) < 2 {
			return make([]pair, 0)
		}
		chained := make([]pair, len(l)-1)
		for i, left := range l {
			if i == len(l)-1 {
				break
			}
			right := l[i+1]
			chained[i] = pair{left, right}
		}
		return chained
	},
}

// Proverb generates “for want of a” rhymes.
func Proverb(rhyme []string) []string {
	if len(rhyme) == 0 {
		return make([]string, 0)
	}
	var output bytes.Buffer
	if err := rhymes.ExecuteTemplate(
		&output, "wantOf", proverbData{rhyme}); err != nil {
		panic(err)
	}
	return strings.Split(output.String(), "\n")
}
