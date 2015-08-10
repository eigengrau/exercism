def is_divisible(n, m):

    return n % m == 0


def is_leap_year(year):

    return is_divisible(year, 4) and \
        (not is_divisible(year, 100) or is_divisible(year, 400))
