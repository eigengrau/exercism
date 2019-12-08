// Package diffiehellman implements Diffie-Hellman key exchange.
package diffiehellman

import (
	"crypto/rand"
	"math/big"
)

// PrivateKey generates a random key k, where 1<k<p.
func PrivateKey(p *big.Int) *big.Int {
	// rand.Int(p) returns one of p-1 possible results. To obtain p-3 results,
	// we need to call rand.Int(p-2). We refrain from remapping the range of
	// results using modulus, since it would skew the distribution of outcomes.
	pˈ := new(big.Int).Sub(p, big.NewInt(2))
	k, err := rand.Int(rand.Reader, pˈ)
	if err != nil {
		// The test suite does not check errors.
		panic(err)
	}
	// We now have a result [0, p-2). We obtain [2, p) by adding two.
	return k.Add(k, big.NewInt(2))
}

// PublicKey calculates a public key.
func PublicKey(priv, p *big.Int, g int64) *big.Int {
	gˈ := big.NewInt(g)
	return new(big.Int).Exp(gˈ, priv, p)
}

// NewPair generates a new public/private key pair.
func NewPair(p *big.Int, g int64) (*big.Int, *big.Int) {
	priv := PrivateKey(p)
	pub := PublicKey(priv, p, g)
	return priv, pub
}

// SecretKey generates a secret for symmetric encryption.
func SecretKey(priv, pub, p *big.Int) *big.Int {
	return new(big.Int).Exp(pub, priv, p)
}
