import {setData, varsStringToJson, VariableRelations, compareValsAndRefs, compareWithUserCode} from './vars.js';
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
rank global svgw: 400
rank 0 y:40, x:120, yextra: 0, rev: 1, w: 2, sref: 1, namelabel: 3, nonull: 1
rank 1 y:40, x:150, yextra: 0, rev: 1, w: 3, np: 2
gn 8010: h: 2 
gn 8040: h: 2
text {x:30, y:10} Kutsupino muuttujien osalta
text {x:240, y:10} Kekomuisti (heap)
code: String s1 = "eka";
ref s1:100
new 8010 eka
s1 -> 8010

code: String s2 = new String("eka");
ref s2:104
new 8040 toka
s2 -> 8040

code: int i1 = 11;
val i1:108
i1=11

code: int i2 = 10 + 1;
val i2:112
i2=11

code: Integer io1 = new Integer(3);
ref io1:116
new 8070 3
io1->8070

code: Integer io2 = new Integer(3);
ref io2:120
new 8080 3
io2->8080
new 8090 ""

`;
setData({
    code: code, args: "1001", params:
        {mode: "static", errorlevel: 0, animate: "code", allowLazy: true}
});

const code2 = `
Ref susi
New $1 Susi
susi -> $1
#Ref isosusi
`;

// let vars1 =  new VariableRelations(code, {mode: "static", errorlevel: 3, xanimate: "commands", allowLazy: true});
// let vars2 =  new VariableRelations(code2, {mode: "static", errorlevel: 3, xanimate: "commands", allowLazy: true});
// vars1.runUntil();
// vars2.runUntil();
let diff =  compareValsAndRefs(code, code2);
console.log(diff);
// BYCODEBEGIN
// BYCODEEND

let data = {answer_call_data: {markup: {fullprogram:
`
jarray t1 v5x4 6 3 2 9 | 0 44 0 22 | 0  0 55 | 33 | 0 0 81
ref tp1:t1 -> t1
jarray t v5x4
ref tp:t -> t
// BYCODEBEGIN
t[0][3] = 9
// BYCODEEND
`
}}, save_object: { usercode:
`
t[0][3] = 91
t[0][2] = 2
t[0][1] = 3
t[0][0] = 6

t[1][3] = 22
t[1][1] = 44

t[2][2] = 55

t[3][0] = 33

t[4][2] = 81
`
}};
let tcode =
`
t[0][0] = 6
t[0][1] = 3
t[0][2] = 2
t[0][3] = 9

t[1][0] = 0
t[1][1] = 44
t[1][2] = 0
t[1][3] = 22

t[2][0] = 0
t[2][1] = 0
t[2][2] = 55
t[2][3] = 0

t[3][0] = 33
t[3][1] = 0
t[3][2] = 0
t[3][3] = 0

t[4][0] = 0
t[4][1] = 0
t[4][2] = 81
t[4][3] = 0
`;
let diff2 = compareWithUserCode(data, tcode);
console.log(diff2);
