import { setData } from './vars.js';
/*
Ref lista
Ref aku
Ref ankka
List $1 R 3
New $2 "Aku"
New $3 "Ankka"
lista -> $1
aku -> $2
aku -> $3
ankka -> $3
$1[0] -> $2
$1[1] -> $3
$1[-1] -> $3
$2[1] -> $3
$1[2] -> $2
$1[2] -> $3
value a = 3
V a = 4
kettu
a = 2
a -> $2
$1[0] -> aku
aku[0] -> aku

v a 5
r b
n $1 kissa
l $2 v 5
a $3 r 3
l $4 r 6
l $5 v a,b,c,d
l $6 r $1, $2

s $j1 V 1 "Ankka Aku" Ankkalinna 12345 ...
a $2 v 1 2 3

s $j2 V 2 "Susi Sepe" Takametsä 12555 ...
s $j3 V 4 "Ponteva Veli" Takametsä 12355 ...
a $alkiot R $j1 $j2 $j3
s st V 1 1 2 3 4 5 6 7 8 9

s jasenet V 1 "Kelmien kerho" nimet.dat 8 3 $1
s $j1 V 1 "Ankka Aku" Ankkalinna 12345 ...
a $2 v 1 2 3
s $j2 V 2 "Susi Sepe" Takametsä 12555 ...
s $j3 V 4 "Ponteva Veli" Takametsä 12355 ...
a $alkiot R $j1 $j2 $j3
s st V 0 1 2 3 4 5 6 7 8 9
v susi 20
v kettu 900
g w: 1, rank: 1, rankdir: 1
s $1 a 12 null
s $2 a 10 null
$1[1] -> $2
a $3 r 5
g rank: -1
v a 3
v b 4
r e
g rank: 2, y: 200
s $4@2 a 1,2,3
$2[1] -> $4
r d@4 -> $4
e -> $4
r f@2->$4
a $1 v5
a $2 v4
a $3 v3
g r 0
r t
a $4 rv $1 $2 $3
t -> $4
*/
    const code = `
class Naytto: kerho R
class Kerho: jasenet R, harrastukset R
class Jasenet: kokonimi S, tiedostonNimi S, maxLkm V, lkm V, alkiot R
class Jasen: tunnusNro V, nimi S, osoite S, postinumero V, _ S
class Harrastukset: _ S, tiedostonNimi S, maxLkm V, lkm V, alkiot R
class Harrastus: tunnusNro V, jasenNro V, ala S, aloitusvuosi V, tuntiaViikossa V

g rank 1, ax: 100, ay: 200, w:3, rx: 150, ry: 120
s.Jasen j1 V 1 "Ankka Aku" Ankkalinna 12345 ...
s.Jasen j2 V 2 "Susi Sepe" Takametsä 12555 ...
s.Jasen j3 V 4 "Ponteva Veli" Takametsä 12355 ...
t {"align": middle} Jasen
g rx: 0, ry: 160, w:2 
a +alkiot RV8 j1 j2 j3
g rx: 30, ry: 0, w: 3
s.Jasenet +jasenet A "Kelmien kerho" nimet.dat 8 3 $alkiot

g rank 2, ax: 360, ay: 200, w: 3, rx: 240, ry: 10, snap: 0
s.Harrastus h1 v 1 1 kalastus 1955 20
s.Harrastus h2 v 2 1 "laiskottelu" 1950 20
s.Harrastus h3 v 3 2 "kelmien kerho" 1962 2
s.Harrastus h4 v 4 1 "työn pakoilu" 1962 40
s.Harrastus h5 v 5 2 "possujen jah." 1954 20
s.Harrastus h6 v 7 4 "susiansojen..." 1956 15
t {"align": middle} Harrastus
g rx: 20, ry: 160, w : 2, snap: a
a alkioth RV10 h1 h2 h3 h4 h5 h6
g rx: 0, ry: 0, w: 3
s.Harrastukset harrastukset A "" harrastukset.dat 10 6 $alkioth

g rank 3, ax: 250, ay: 60, h:2, w: 2, rx: 0, ry: 0
s.Kerho +kerho RH jasenet harrastukset
t {"align": middle, sy: -120} Kerho
t {"align": end, sx: -70, sy: -20} jasenet
t {"align": start, sx: 70, sy: -20} harrastukset
g rx: 380, ry: 0, w: 3, h:2
s.Naytto +naytto RH kerho
t {sx: -130, sy: -100} Naytto naytto
#t {align: end, sx: -70, sy: -40} kerho
SVG <path d="M 730 130 Q 450 125 450 0" fill="none" stroke="#000" stroke-dasharray="3 3"></path>
`;
    setData({
        code: code, args: "1001", params:
            {mode: "static", errorlevel: 3, xanimate: "code"}
    });
