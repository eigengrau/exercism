
class Clock:

    def __init__(self, h, min_):

        h_from_min, self.min_ = divmod(min_, 60)
        self.h = (h + h_from_min) % 24

    def __str__(self):

        return "{:02d}:{:02d}".format(self.h, self.min_)

    def __repr__(self):

        return "{class_}({h}, {min_})".format(
            class_=self.__class__.__name__,
            h=self.h,
            min_=self.min_
        )

    def __eq__(self, other):

        return self.h == other.h and self.min_ == other.min_

    def add(self, min_):

        h_from_min, self.min_ = divmod(self.min_ + min_, 60)
        self.h = (self.h + h_from_min) % 24
        return self
