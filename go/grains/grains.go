// Package grains calculates powers of two.
package grains

import (
	"fmt"
)

// memo stores powers of two.
var memo [64]uint64 = [64]uint64{1}

// needle keeps the offset to first non-memoized item.
var needle int = 1

// Square calculates 2ⁿ.
func Square(n int) (uint64, error) {
	if n <= 0 || n > 64 {
		return 0, fmt.Errorf("expecting 0 > n <= 64")
	}
	// square expects offsets into the memo table, which are 0-based, whereas
	// Square offsets are 1-based.
	return square(n - 1), nil
}

// square calculates 2ⁿ⁺¹ and memoizes intermediate results.
func square(n int) uint64 {
	// Memoize all missing in-between values.
	for ; needle <= n; needle++ {
		memo[needle] = 2 * memo[needle-1]
	}
	return memo[n]
}

// Total calculates Σ2¹+…+2⁶⁴.
func Total() uint64 {
	// return 18446744073709551615 ;-)
	var sum uint64
	for i := 0; i < 64; i++ {
		sum += square(i)
	}
	return sum
}
