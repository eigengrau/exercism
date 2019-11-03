// Package pangram checks for pangrammaticity.
package pangram

import (
	"strings"
)

type unit struct{}
type set map[rune]unit

// IsPangram checks a word for pangrammaticity.
func IsPangram(w string) bool {
	// To check for pangrammaticity, we keep the target alphabet in a set and
	// delete items as we go. A Pangram will leave us with the empty set.
	latin := "abcdefghijklmnopqrstuvwxyz"
	alphabet := make(set, len(latin))
	for _, c := range latin {
		alphabet[c] = unit{}
	}

	w = strings.ToLower(w)
	for _, c := range w {
		delete(alphabet, c)
	}
	return len(alphabet) == 0
}
