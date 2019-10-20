// Package dna computes aggregate data on DNA sequences.
package dna

import (
	"fmt"
)

type Nucleotide rune

// Histogram maps nucleotides to counts.
type Histogram map[Nucleotide]int

// DNA is a sequence of nucleotides.
type DNA []Nucleotide

// New creates a new histogram.
///
// Since we are defining an initializer, the Histogram constructor should
// probably not be exported; however, here, we leave it exported for sake of the
// test suite. For additional type safety, Histogram should probably be a
// struct. However, the test suite expects a mapping.
func New() Histogram {
	return Histogram{'G': 0, 'A': 0, 'T': 0, 'C': 0}
}

// Counts generates a histogram of valid nucleotides in the given DNA. Returns
// an error if d contains an invalid nucleotide.
func (d DNA) Counts() (Histogram, error) {
	h := New()
	for _, n := range d {
		switch n { // Validate input.
		case 'G', 'A', 'T', 'C':
		default:
			return nil, fmt.Errorf("Invalid nucleotide: %q", n)
		}
		h[n]++
	}
	return h, nil
}
