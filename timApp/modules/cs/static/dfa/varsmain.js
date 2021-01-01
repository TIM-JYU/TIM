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
*/
    const code = `
g x: 250, y: 320, w:3
s $j1 V 1 "Ankka Aku" Ankkalinna 12345 ...
s $j2 V 2 "Susi Sepe" Takametsä 12555 ...
s $j3 V 4 "Ponteva Veli" Takametsä 12355 ...
t {"align": middle} Jasen
g x: 100, y: 360, w:2 
a $alkiot RV 8
$alkiot[0] -> $j1
$alkiot[1] -> $j2
$alkiot[2] -> $j3
g x: 130, y: 200, w: 3
s $jasenet V "Kelmien kerho" nimet.dat 8 3 $alkiot
g x: 380, y: 360, w : 2
a $alkioth RV 10
g x: 360, y: 200, w: 3
s $harrastukset V "" harrastukset.dat 10 6 $alkioth
g x: 600, y: 210, w: 3
s $h1 v 1 kalastus 1955 20
s $h2 v 1 "laiskottelu" 1950 20
s $h3 v 2 "kelmien kerho" 1962 2
s $h4 v 1 "työn pakoilu" 1962 40
s $h5 v 2 "possujen jah." 1954 20
s $h6 v 4 "susiansojen..." 1956 15
$alkioth[0] -> $h1
$alkioth[1] -> $h2
$alkioth[2] -> $h3
$alkioth[3] -> $h4
$alkioth[4] -> $h5
$alkioth[5] -> $h6
t {"align": middle} Harrastus
g x: 250, y: 40, h:2, w: 2
s $kerho RH $jasenet $harrastukset
t {"align": middle, sy: -100} Kerho
t {"align": end, sx: -70, sy: -20} jasenet
t {"align": start, sx: 70, sy: -20} harrastukset
g x: 630, y: 40, w: 3, h:2
s $naytto AH $kerho
t {align: end, sx: -50, sy: -100} Naytto naytto
SVG <path d="M 730 130 Q 630 130 580 120 Q 530 110 500 85 Q 470 60 460 40 Q 450 20 450 15 Q 450 10 450 10" fill="none" stroke="#000000" stroke-miterlimit="10" stroke-dasharray="3 3" pointer-events="stroke"></path>
`;
    setData({
        code: code, args: "1001", params:
            {mode: "static", errorlevel: 3, xanimate: "commands"}
    });
