// Package darts scores dart throws.
package darts

import (
	"math"
	"sort"
)

type ring struct {
	radius float64
	score  int
}

var board []ring = []ring{
	ring{radius: 1, score: 10},
	ring{radius: 5, score: 5},
	ring{radius: 10, score: 1},
}

// init ensures that the rings of the board are sorted descending by score,
// which is assumed by the Score function.
func init() {
	sort.Slice(board,
		func(i, j int) bool {
			return board[i].score > board[j].score
		})
}

// Score scores a dart throw for the given coordinates.
func Score(x float64, y float64) int {
	fromCenter := math.Sqrt(math.Pow(x, 2) + math.Pow(y, 2))
	for _, ring := range board {
		if fromCenter <= ring.radius {
			return ring.score
		}
	}
	return 0
}
