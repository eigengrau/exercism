def to_rna(dna):

    dna = dna.lower()
    return "".join(map(to_rna_char, dna))


def to_rna_char(nucleotide):

    if nucleotide not in nucleotide_map:

        raise ValueError("Invalid nucleotide: {}", nucleotide)

    return nucleotide_map[nucleotide].upper()


nucleotide_map = {
    'g': 'c',
    'c': 'g',
    't': 'a',
    'a': 'u'
}
