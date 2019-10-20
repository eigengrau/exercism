// Package implements the map operator.
package accumulate

// Accumulate implements the map operator on []string.
func Accumulate(l []string, f func (string) string) []string {
	result := make([]string, len(l))
	for i, arg := range l {
		result[i] = f(arg)
	}
	return result
}
