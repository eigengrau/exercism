def distance(s1, s2):

    if len(s1) != len(s2):
        raise ValueError(
            "Hamming distance undefined on unequal lengths: {} {}".format(
                s1, s2
            )
        )

    matching = [c1 == c2 for c1, c2 in zip(s1, s2)]
    return matching.count(False)
