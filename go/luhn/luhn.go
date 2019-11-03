// Package luhn implements the Luhn algorithm.
package luhn

import (
	"strings"
	"unicode"
)

func normalize(r rune) rune {
	if unicode.IsSpace(r) {
		return -1
	}
	return r
}

// Valid validates the data.
func Valid(s string) bool {
	s = strings.Map(normalize, s)
	if len(s) <= 1 {
		return false
	}
	sum := 0
	isSecondDigit := false
	for i := len(s) - 1; i >= 0; i-- {
		// We can rely on data being Unicode.
		d := int(s[i]) - 48
		if d < 0 || d > 9 {
			return false
		}
		if isSecondDigit {
			d *= 2
			if d > 9 {
				d -= 9
			}
		}
		sum += d
		isSecondDigit = !isSecondDigit
	}
	return sum%10 == 0
}
