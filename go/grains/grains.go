// Package grains calculates powers of two.
package grains

import (
	"fmt"
	"math"
)

// Square calculates 2ⁿ⁻¹.
func Square(n int) (uint64, error) {
	if n <= 0 || n > 64 {
		return 0, fmt.Errorf("expecting 1 <= n <= 64, not %d", n)
	}
	if n == 1 {
		return 1, nil
	}
	return 2 << (n - 2), nil
}

// Total returns Σ2⁰+…+2⁶³.
func Total() uint64 {
	return math.MaxUint64
}
