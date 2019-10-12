// Package twofer builds two-fer sentences.
package twofer

import (
	"fmt"
)

var template = "One for %s, one for me."

// ShareWith returns a two-fer sentence with the beneficiary chosen based on
// name.
func ShareWith(name string) string {
	var beneficiary string
	if name == "" {
		beneficiary = "you"
	} else {
		beneficiary = name
	}
	return fmt.Sprintf(template, beneficiary)
}
