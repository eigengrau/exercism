// Package listops implements operations on lists.
package listops

type IntList []int
type unaryFunc func(int) int
type predFunc func(int) bool
type binFunc func(int, int) int

// Foldr folds the list from the right.
func (this IntList) Foldr(fn binFunc, acc int) int {
	for i := len(this) - 1; i >= 0; i-- {
		acc = fn(this[i], acc)
	}
	return acc
}

// Foldl folds the list from the left.
func (this IntList) Foldl(fn binFunc, acc int) int {
	for _, e := range this {
		acc = fn(acc, e)
	}
	return acc
}

// Length returns the length of the list.
func (this IntList) Length() int {
	length := 0
	for range this {
		length++
	}
	return length
}

// Reverse creates a list by reversing this list.
func (this IntList) Reverse() IntList {
	size := len(this)
	for i := 0; i < size/2; i++ {
		this[i], this[size-i-1] = this[size-i-1], this[i]
	}
	return this
}

// Append creates a concatenation (sic) of this list and another.
///
// I’m assuming that copy is fair game. Otherwise, we could re-implement it via
// iterated assignment.
func (this IntList) Append(other IntList) IntList {
	result := make(IntList, len(this)+len(other))
	copy(result, this)
	copy(result[len(this):], other)
	return result
}

// Concat creates a concatenatenation of this list with other lists.
func (this IntList) Concat(others []IntList) IntList {
	resultLen := len(this)
	for _, e := range others {
		resultLen += len(e)
	}
	result := make(IntList, resultLen)
	copy(result, this)
	offset := len(this)
	for _, other := range others {
		copy(result[offset:], other)
		offset += len(other)
	}
	return result
}

// Filter creates a list containing only those items matching the predicate.
func (this IntList) Filter(pred predFunc) IntList {
	needle := 0
	for _, e := range this {
		if pred(e) {
			this[needle] = e
			needle++
		}
	}
	return this[:needle]
}

// Map creates a list with the results from applying a function to each item.
func (this IntList) Map(fn unaryFunc) IntList {
	for i, e := range this {
		this[i] = fn(e)
	}
	return this
}
