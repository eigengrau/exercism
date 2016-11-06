import re

from collections import Counter


def word_count(phrase):

    return Counter(split_words(phrase))


def split_words(phrase):

    words = words_re.split(phrase.lower())

    # Get rid of empty string when last words ends in split-character.
    if not words[-1]:
        return words[:-1]
    else:
        return words


words_re = re.compile("[\W_]+", flags=re.UNICODE)
