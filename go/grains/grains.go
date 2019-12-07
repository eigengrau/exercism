// Package grains calculates powers of two.
package grains

import (
	"fmt"
)

// Square calculates 2ⁿ⁻¹.
func Square(n int) (uint64, error) {
	if n <= 0 || n > 64 {
		return 0, fmt.Errorf("expecting 0 > n <= 64")
	}
	// Square() inputs are natural numbers (offsets into into a chess-board),
	// whereas square() assumes the input is integral.
	return square(n - 1), nil
}

// square calculates 2ⁿ.
func square(n int) uint64 {
	if n == 0 {
		return 1
	}
	return 2 << (n - 1)
}

// Total calculates Σ2⁰+…+2⁶³.
func Total() uint64 {
	// return 18446744073709551615 ;-)
	var sum uint64 = 1
	for i := 0; i < 64; i++ {
		sum += 2 << i
	}
	return sum
}
