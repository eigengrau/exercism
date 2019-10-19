// Package raindrops makes raindrop sounds.
package raindrops

import "strconv"

var (
	factors = [...]int{3, 5, 7}
	sounds  = [...]string{"Pling", "Plang", "Plong"}
)

// Convert converts a number into a raindrop sound.
func Convert(n int) string {
	var result string
	for i, factor := range factors {
		if n%factor == 0 {
			result += sounds[i]
		}
	}
	if result == "" {
		result = strconv.Itoa(n)
	}
	return result
}
