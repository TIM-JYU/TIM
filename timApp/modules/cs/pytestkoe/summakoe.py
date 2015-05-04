__author__ = 'vesal'

class Laskut:
    def summa(self):
        """
        Lasketaan a ja b yhteen
        :return: s.a+s.b

        >>> s = Laskut()
        >>> s.a,s.b = 3,2
        >>> s.summa()
        5
        """
        return self.a + self.b


if __name__ == '__main__':
    s = Laskut()
    s.a = 3
    s.b = 2
    c = s.summa()
    print(c)