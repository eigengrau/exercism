// Package hamming calculates Hamming distances.
package hamming

import (
	"fmt"
	"golang.org/x/text/unicode/norm"
)

// Distance computes the Hamming distance between two strings.
func Distance(a, b string) (int, error) {
	// Since we are feeling playful, we decided to go beyond the scope of the
	// Exercism problem and calculate the distance for arbitrary strings. Hence,
	// we need to normalize comparands. We also convert to []rune, since we will
	// be comparing characters proper, not bytes.
	aNorm := []rune(norm.NFC.String(a))
	bNorm := []rune(norm.NFC.String(b))

	if len(aNorm) != len(bNorm) {
		err := fmt.Errorf("hamming: mismatched comparand lengths: %q, %q", a, b)
		return -1, err
	}

	distance := 0
	for i, left := range aNorm {
		right := bNorm[i]
		if left != right {
			distance++
		}
	}

	return distance, nil
}
