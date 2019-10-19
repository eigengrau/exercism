// Package raindrops makes raindrop sounds.
package raindrops

import "strconv"

// Convert converts a number into a raindrop sound.
func Convert(n int) string {
	var result string
	if n%3 == 0 {
		result += "Pling"
	}
	if n%5 == 0 {
		result += "Plang"
	}
	if n%7 == 0 {
		result += "Plong"
	}
	if result == "" {
		result = strconv.Itoa(n)
	}
	return result
}
