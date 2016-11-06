import string


alphabet = set(string.ascii_lowercase)


def is_pangram(sent):

    sent = sent.lower()
    return set(sent).issuperset(alphabet)
