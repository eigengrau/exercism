// Package twofer builds two-fer sentences.
package twofer

import (
	"fmt"
)

// ShareWith returns a two-fer sentence with the beneficiary chosen based on
// name.
func ShareWith(name string) string {
	if name == "" {
		name = "you"
	}
	return fmt.Sprintf("One for %s, one for me.", name)
}
