// Build two-fer sentences.
package twofer

import (
	"fmt";
)

var template = "One for %s, one for me."

// Apply a default beneficiary when needed.
func beneficiary(name string) string {
	switch name {
	case "":
		return "you"
	default:
		return name
	}
}

// Return a two-fer sentence with the beneficiary chosen based on name.
func ShareWith(name string) string {
	return fmt.Sprintf(template, beneficiary(name))
}
