// Package encode performs run-length encoding and decoding.
package encode

import (
	"strconv"
	"unicode"
)

// RunLengthEncode run-length encodes the input.
func RunLengthEncode(s string) string {
	encoded := ""
	runLen := 1
	for i, c := range s {
		isLastIteration := i+1 >= len(s)
		// Continue the current sequence.
		if !isLastIteration && rune(s[i+1]) == c {
			runLen++
			continue
		}
		// Flush the current sequence to output.
		if runLen == 1 {
			encoded += string(c)
		} else {
			encoded += strconv.Itoa(runLen) + string(c)
		}
		runLen = 1
	}
	return encoded
}

// RunLengthDecode decodes the run-length encoded input.
func RunLengthDecode(s string) string {
	decoded := ""
	lenStr := ""
	for _, c := range s {
		if unicode.IsNumber(c) {
			lenStr += string(c)
			continue
		}
		if lenStr == "" {
			lenStr = "1"
		}
		runLen, err := strconv.Atoi(lenStr)
		if err != nil {
			panic(err)
		}
		for i := 0; i < runLen; i++ {
			decoded += string(c)
		}
		lenStr = ""
	}
	return decoded
}
