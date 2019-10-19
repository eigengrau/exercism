// Package scrabble computes Scrabble scores.
package scrabble

import (
	"strings"
)

// Score scores a Scrabble word.
func Score(s string) int {
	// Letters are scored regardless of case.
	s = strings.ToLower(s)
	var result int
	for _, letter := range s {
		switch letter {
		case 'd', 'g':
			result += 2
		case 'b', 'c', 'm', 'p':
			result += 3
		case 'f', 'h', 'v', 'w', 'y':
			result += 4
		case 'k':
			result += 5
		case 'j', 'x':
			result += 8
		case 'q', 'z':
			result += 10
		default:
			result += 1
		}
	}
	return result
}
