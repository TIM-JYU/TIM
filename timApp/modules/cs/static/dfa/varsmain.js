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
class Jasenet kokonimi S, tiedostonNimi S, maxLkm V, lkm V, alkiot R
class Jasen id V, nimi S, osoite S, postinumero V, _ S
code: /// Luodaan jäsenet
g r: 0,x: 130, y: 0, w: 3
s.Jasenet jasenet A "Kelmien kerho" nimet.dat 8 0 null
code: /// Luodaan taulukko
g r: 1,x: 100, y: 160, w:2 
a alkiot RV8 
jasenet.alkiot -> $alkiot
code: /// luodaan 1. jäsen
g r:2, x: 250, y: 120, w:3
s.Jasen $j1 A 1 "Ankka Aku" Ankkalinna 12345 ...
$alkiot[0] -> j1
$jasenet.lkm = 1
code: /// luodaan 2. jäsen
s.Jasen j2 A 2 "Susi Sepe" Takametsä 12555 ...
$alkiot[1] -> j2
$jasenet.lkm = 2
code: /// luodaan 3. jäsen
s.Jasen j3 A 4 "Ponteva Veli" Takametsä 12355 ...
alkiot[2] -> j3
jasenet.lkm = 3
t {"align": middle} Jasen
`;

setData({
    code: code, args: "1001", params:
        {mode: "static", errorlevel: 3, animate: "commands"}
});
