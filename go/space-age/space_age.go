// Package space calculates cosmic things.
package space

// Planet represents a celestial body.
type Planet string

// We must alias these since the test suite expects them.
type durationYears = float64
type durationSecs = float64

const secondsPerYear = 31557600

// OrbitalPeriod is a table of orbital periods for solar bodies.
var OrbitalPeriod = map[Planet]durationYears{
	"Earth":   1,
	"Mercury": 0.2408467,
	"Venus":   0.61519726,
	"Mars":    1.8808158,
	"Jupiter": 11.862615,
	"Saturn":  29.447498,
	"Uranus":  84.016846,
	"Neptune": 164.79132,
}

// Age convert the Earth age in seconds into the age in years relative to the
// given planet.
func Age(earthAge durationSecs, planet Planet) durationYears {
	return earthAge / secondsPerYear / OrbitalPeriod[planet]
}
