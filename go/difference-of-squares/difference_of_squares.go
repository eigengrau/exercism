// Package diffsquares runs computations on number series.
package diffsquares

// SquareOfSum calculates the square of the sum of the first n natural numbers.
// <https://proofwiki.org/wiki/Sum_of_Arithmetic_Progression>
func SquareOfSum(n int) int {
	sum := n * (1 + n) / 2
	return sum * sum
}

// SumOfSquares calculates the sum of the squares of the first n natural
// numbers. <https://proofwiki.org/wiki/Sum_of_Sequence_of_Squares>
func SumOfSquares(n int) int {
	return n * (n + 1) * (2*n + 1) / 6
}

// Difference calculates the difference between the sum of squares and the
// square of the sum.
func Difference(n int) int {
	return SquareOfSum(n) - SumOfSquares(n)
}
