# Ajattelin tehda pienen demon pythonin ihan 
# perusteista, toivottavasti saatte jotain irti.
# Lukaiskaa koodi ja testatkaa sitten komentorivilla.
# Muistakaa vaihtaa editorinne kayttamaan valilyonteja tabin sijaan.

# Muuttujaan arvon asettamiseen ei tarvita mitaan sen kummempia tarkennuksia,
# annatte vain muuttujalle nimen ja arvon

# esim import
import time

# poistuminen- import
from sys import exit


# Pari muuttujaa
totuus_arvo = True
totuus_arvo_F = False

luku = 1

luku2 = 2 + 3 + 4 + 5

stringi = "Jotain shaibelia" 
toinen = 'a'



# Hyvin perus IO, parempiakin varmaan loytyis.
nimi = raw_input("Annappa nimesi: ") 
print "Moi " + nimi + "!" + "\n" + "Rivi vaihtui!" 

# Stringin muokkailua
lause = "Tassa lauseessa on luku {0}, luku {1} ja viela nimi {2}".format(luku, luku2, nimi)
print lause

# Monta muuttujaa kerralla!
x, y, z = 1, 2, 3

# Huomioi sisennys 4 tyhjaa. While esim.
while x < 10:
    print x
    x, y = x + 1, y + x

# Esimerkki if
vastaus = False
while vastaus == False:    
    num = raw_input("Paljonko on 1+1?")
    try:
        if int(num) == 2:
            print "Hah \n"
            vastaus = True
        elif int(num) == 3:
            print "jahas..."
        else:
            print "Voi piruvie...."
    except (ValueError, TypeError, NameError):
        print "Ei ollut numero..."

# Aliohjelmia! Huom. sisennys 
def summa(x, y):
    return x + y

def erotus(x, y):
    return x - y

def kerto(x, y):
    """ Tama on docstring, kertoo mita funktio tekee.
        Tama funktio kertoo kaksi lukua yhteen. """
    return x * y

print kerto.__doc__

# Sanakirjat on kivoja! 
sanakirja = { "summa" : summa, "miinus" : erotus, "kertolasku" : kerto }

funktio = sanakirja.get("summa")
print funktio(1, 2)
funktio = sanakirja.get("miinus")
print funktio(2, 1)

funktio = sanakirja.get("EILOYDY", sanakirja.get("kertolasku")) 
                        #jalkimmainen parametri eli sanakirja.get("kertolasku) on 
                        #default arvo joka palautetaan jos ekaa arvoa ei loydy
print funktio(2, 4)

# Sanakirja voidaan tehda tosi monella tavalla!

a = dict(one=1, two=2, three=3)
b = {'one': 1, 'two': 2, 'three': 3}
c = dict(zip(['one', 'two', 'three'], [1, 2, 3]))
d = dict([('two', 2), ('one', 1), ('three', 3)])
e = dict({'three': 3, 'one': 1, 'two': 2})
# kaikki ylla on samoja.
print a == b == c == d == e

# pieni luokka

class omaLuokka: 
    """ Taas docstring"""

    joku_luokan_arvo = True

    # Konstruktori, eka parametri vain viittaa olioon, loput voidaan kayttaa 
    # mihin tarvii.
    def __init__(self, nimi, ika, sukupuoli): 
        self.nimi = nimi
        self.ika = ika
        self.sukupuoli = sukupuoli        


def vanhene(olio):
    olio.ika = olio.ika + 1 

samu = omaLuokka("samu", 22, "MIES")

print ("Moi, nimeni on " + samu.nimi + 
        ", ja ikani " + str(samu.ika) + 
        " ja olen " + samu.sukupuoli)

print "Nyt vanhenen"
vanhene(samu)
print "Ikani on " + str(samu.ika)

print "\n Tuon luokan docstring oli: " + samu.__doc__

while(True):
    lopetus = raw_input("Type exit\n")
    if (lopetus == "exit"):
        exit()



