// Package isogram checks for isogrammaticity.
package isogram

import (
	"strings"
	"unicode"
)

type unit struct{}
type set map[rune]unit

// IsIsogram checks a word for isogrammaticity.
func IsIsogram(w string) bool {
	w = strings.ToLower(w)
	seen := make(set)
	for _, c := range w {
		if !unicode.IsLetter(c) {
			continue
		}
		if _, ok := seen[c]; ok {
			return false
		}
		seen[c] = unit{}
	}
	return true
}
