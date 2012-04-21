DECLARE SUB actionprint ()
DECLARE SUB apulaisvalm ()
DECLARE SUB apulaisvalm2 ()
DECLARE SUB arpod (voiz%)
DECLARE SUB arpol (voiz%)
DECLARE SUB arpols3 (voiz%)
DECLARE SUB arvoloukkaa (fat%)
DECLARE SUB colchk ()
DECLARE SUB colchk2 ()
DECLARE SUB colchk3 ()
DECLARE SUB colchk4 ()
DECLARE SUB inteprint ()
DECLARE SUB kuva (tied$)
DECLARE SUB mahmax ()
DECLARE SUB manap ()
DECLARE SUB haahaa (haha%)
DECLARE SUB krapulapiirto ()
DECLARE SUB keisi ()
DECLARE SUB luetexti (lue$)
DECLARE SUB lentti (sano!)
DECLARE SUB gene ()
DECLARE SUB inforuta ()
DECLARE SUB jaynamaar ()
DECLARE SUB jaynmahdprint ()
DECLARE SUB ketjutyhjenna ()
DECLARE SUB ketjumaar ()
DECLARE SUB klearstat (fat%)
DECLARE SUB luz (fat%)
DECLARE SUB naytaott ()
DECLARE SUB naytaottehl ()
DECLARE SUB moprint2 ()
DECLARE SUB mukachk2 ()
DECLARE SUB ont ()
DECLARE SUB ont2 ()
DECLARE SUB pelaamaar ()
DECLARE SUB ottelmaar ()
DECLARE SUB ottelmaarehl ()
DECLARE SUB pelpelpel ()
DECLARE SUB pisteet ()
DECLARE SUB piirtox2 ()
DECLARE SUB sortmaar ()
DECLARE SUB sortplayers ()
DECLARE SUB qelp (qel%)
DECLARE SUB sopuprint ()
DECLARE SUB pjn ()
DECLARE SUB printpels ()
DECLARE SUB poispel ()
DECLARE SUB printpel (fat%)
DECLARE SUB prm (juu%)
DECLARE SUB skoutmaar (fat%)
DECLARE SUB postia (posti%)
DECLARE SUB tarjousmaar (koo1%, koo2%)
DECLARE SUB tarka (jez%)
DECLARE SUB stratprint ()
DECLARE SUB tekijat ()

'$DYNAMIC
'$INCLUDE: 'mhm99.bi'

SUB meanstuff
DIM janum(23) AS INTEGER

jukero = 1

FOR xx = 1 TO 24
IF x(xx) <> u(pv) THEN janum(jukero) = x(xx): jukero = jukero + 1
NEXT xx

kurso = 1: oldkurso = 1

bekinado:
IF jaynteh = 1 THEN EXIT SUB
CLS
IF jaynateh(pv) = 3 THEN COLOR 15: PRINT "OLET JŽYNŽYTTŽNYT JO 3 KERTAA TŽLLŽ KAUDELLA!": pjn: EXIT SUB
IF krapu(pv) = 2 THEN krapulapiirto
LOCATE 1, 1
COLOR 15: PRINT "JŽYNŽT: VALITSE UHRISI - JA KOHTALOSI!";
haahaa (6)
DO
IF oldkurso = 2 AND ohj(janum(kurso)) <> 0 THEN oldkurso = 1
COLOR 14
FOR xx = 1 TO 23
IF sr(janum(xx)) = 1 THEN wallis = 0 ELSE wallis = 1
IF kurso = xx THEN COLOR 14, 13 ELSE COLOR 14, 0
LOCATE 2 + xx + wallis, 1: PRINT RTRIM$(l(janum(xx)));
LOCATE 2 + xx + wallis, 13: PRINT "("; mana(man(janum(xx))); ")";
NEXT xx

jaynmax = 7
IF ot = 44 THEN jaynmax = 1
jaynmahdprint

LOCATE 28, 1: COLOR 12, 0
FOR z = 1 TO 6
PRINT STRING$(80, " ")
NEXT z
LOCATE 28, 1
luetexti ("jaynk" + LTRIM$(STR$(oldkurso)))

jay$ = "": WHILE jay$ = "": jay$ = INKEY$: WEND
IF jay$ = "?" THEN qelp (6): GOTO bekinado
IF jay$ = CHR$(0) + CHR$(72) AND kurso > 1 THEN kurso = kurso - 1
IF jay$ = CHR$(0) + CHR$(80) AND kurso < 23 THEN kurso = kurso + 1

IF jay$ = CHR$(0) + CHR$(75) AND oldkurso > 1 THEN oldkurso = oldkurso - 1
IF jay$ = CHR$(0) + CHR$(77) AND oldkurso < jaynmax THEN oldkurso = oldkurso + 1
IF jay$ = CHR$(0) + CHR$(75) AND oldkurso = 2 AND ohj(janum(kurso)) <> 0 THEN oldkurso = 1
IF jay$ = CHR$(0) + CHR$(77) AND oldkurso = 2 AND ohj(janum(kurso)) <> 0 THEN oldkurso = 3
IF jay$ = CHR$(27) THEN EXIT DO

IF jay$ = CHR$(13) THEN jaynamaar: EXIT DO
LOOP

ERASE janum
END SUB

SUB miettiipois
CLS
manap
PRINT
PRINT
printpels
PRINT "pakkaa kamppeensa ja kertoo suuntaavansa kotiinp„in!"
IF pel(xx, pv).spe = 4 THEN PRINT "Joukkueen keitti”henkil”st” huokaisee helpotuksesta.": eri(30) = 0
poispel
pjn
END SUB

SUB moodiin
DIM mood(1 TO 45, 1 TO 3) AS INTEGER

OPEN "data\muudit.99" FOR INPUT AS #1
FOR zz = 1 TO 45
INPUT #1, mood(zz, 1), mood(zz, 2), mood(zz, 3)
NEXT zz
CLOSE #1
muud = CINT(44 * RND) + 1
arvoloukkaa (2)

IF mood(muud, 1) + pel(xxx, pv).psk > 0 THEN
COLOR 10: PRINT RTRIM$(pel(xxx, pv).nam); " ("; LTRIM$(STR$(pel(xxx, pv).age)); ","; pelip(pel(xxx, pv).ppp); ","; LTRIM$(STR$(pel(xxx, pv).psk)); ")";
COLOR 2
SELECT CASE mood(muud, 1)
CASE IS < 0
PRINT " vaeltaa murheen alhossa..."
CASE IS > 0
PRINT " on llloistavalla pelip„„ll„!"
END SELECT
luetexti (LTRIM$(STR$(muud)) + "m")
pel(xxx, pv).plus = mood(muud, 1)
pel(xxx, pv).kest = CINT((mood(muud, 3) - 1)) * RND + mood(muud, 2)
IF pel(xxx, pv).psk - pel(xxx, pv).plus < 0 THEN pel(xxx, pv).plus = pel(xxx, pv).plus - (pel(xxx, pv).psk - pel(xxx, pv).plus)
END IF

ERASE mood
END SUB

SUB moprint2
COLOR 6
PRINT LTRIM$(STR$(mo(pv)));

END SUB

SUB moramaar

IF mo(pv) <> -66 THEN
IF mo(pv) < momin(vai(pv)) THEN mo(pv) = momin(vai(pv))
IF mo(pv) > momax(vai(pv)) THEN mo(pv) = momax(vai(pv))
END IF

IF mo(pv) = -66 AND raha(pv) >= 0 THEN mo(pv) = momin(vai(pv))

IF pirtar(7) <> 1048 THEN mo(pv) = momin(vai(pv))
END SUB

SUB muilutus (fat%)
SELECT CASE fat%

CASE 1
muilu(xx) = 0
prm (2)
luz (51)
CASE 2
kouku(xx) = 0
prm (2)
luz (52)
CASE 3
prm (2)
luz (54)
END SELECT

zz = CINT(1 * RND) + 2

tarka (zz)
IF lukka = 0 THEN
tauti(zz, tox(xx)) = -(CINT(3 * RND) + 3)
tkest(zz, tox(xx)) = CINT(3 * RND) + 3
EXIT SUB
END IF
IF zz = 2 THEN zz = 3 ELSE zz = 2

tarka (zz)
IF lukka = 0 THEN
tauti(zz, tox(xx)) = -(CINT(2 * RND) + 2)
tkest(zz, tox(xx)) = CINT(3 * RND) + 3
END IF

END SUB

SUB mukachk
SELECT CASE ot

CASE 45
nn = qfin(1): mm = qfin(8): mukachk2
nn = qfin(2): mm = qfin(7): mukachk2
nn = qfin(3): mm = qfin(6): mukachk2
nn = qfin(4): mm = qfin(5): mukachk2
nn = qdin(1): mm = qdin(6): mukachk2
nn = qdin(2): mm = qdin(5): mukachk2
nn = qdin(3): mm = qdin(4): mukachk2

CASE 47
nn = sfin(1): mm = sfin(4): mukachk2
nn = sfin(2): mm = sfin(3): mukachk2

nn = sdin(1): mm = sdin(4): mukachk2
nn = sdin(2): mm = sdin(3): mukachk2

CASE 49
nn = fin(1): mm = fin(2): mukachk2
nn = bin(1): mm = bin(2): mukachk2
nn = din(1): mm = din(2): mukachk2

END SELECT
END SUB

SUB mukachk2
IF temp% = 1 THEN
IF ohj(nn) = pv THEN pwin(nn) = 0: pwin(mm) = 3
IF ohj(mm) = pv THEN pwin(mm) = 0: pwin(nn) = 3
EXIT SUB
END IF

ww = nn: GOSUB mukanach
ww = mm: GOSUB mukanach
EXIT SUB

mukanach:
IF pwin(nn) = 3 AND ohj(ww) <> 0 THEN mukp(ohj(ww)) = 0
IF pwin(mm) = 3 AND ohj(ww) <> 0 THEN mukp(ohj(ww)) = 0
RETURN

END SUB

SUB mukanollaus
FOR xx = 1 TO plkm
mukp(xx) = 0
NEXT xx
END SUB

SUB muutmestarit
CHDIR "data"
ooo = 25

CLS
alkuluku = 1: rando = 5: osmaara = 3: GOSUB jeesboks
alkuluku = 8: rando = 3: osmaara = 1: GOSUB jeesboks
alkuluku = 11: rando = 4: osmaara = 2: GOSUB jeesboks
alkuluku = 15: rando = 2: osmaara = 3: GOSUB jeesboks
alkuluku = 19: rando = 3: osmaara = 1: GOSUB jeesboks
alkuluku = 22: rando = 1: osmaara = 1: GOSUB jeesboks
alkuluku = 23: rando = 1: osmaara = 1: GOSUB jeesboks
alkuluku = 24: rando = 4: osmaara = 2: GOSUB jeesboks
alkuluku = 29: rando = 4: osmaara = 2: GOSUB jeesboks
alkuluku = 34: rando = 2: osmaara = 1: GOSUB jeesboks
CHDIR ".."
EXIT SUB

jeesboks:
wal = CINT((rando - 1) * RND) + alkuluku
FOR zz = wal TO wal + (osmaara - 1)
OPEN "data1.for" FOR INPUT AS #1
FOR zzz = 1 TO zz
INPUT #1, l(ooo), kk(ooo), halli(ooo), tazo(ooo)
tazo(ooo) = tazo(ooo) + CINT(2 * RND) - 1
NEXT zzz
ooo = ooo + 1
CLOSE #1
NEXT zz
RETURN

END SUB

SUB naytaott
s = 1
SELECT CASE ot

CASE IS < 44
LOCATE 3
kolo = 12
nn = x(a): mm = x(b): ont
nn = x(c): mm = x(d): ont
nn = x(E): mm = x(f): ont
nn = x(g): mm = x(h): ont
nn = x(i): mm = x(j): ont
nn = x(k): mm = x(m): ont
kolo = 4: LOCATE 12
nn = x(a + 12): mm = x(b + 12): ont
nn = x(c + 12): mm = x(d + 12): ont
nn = x(E + 12): mm = x(f + 12): ont
nn = x(g + 12): mm = x(h + 12): ont
nn = x(i + 12): mm = x(j + 12): ont
nn = x(k + 12): mm = x(m + 12): ont

CASE 45
LOCATE 3
kolo = 12
IF pwin(qfin(a)) < 3 AND pwin(qfin(b)) < 3 THEN nn = qfin(a): mm = qfin(b): ont
IF pwin(qfin(c)) < 3 AND pwin(qfin(d)) < 3 THEN nn = qfin(c): mm = qfin(d): ont
IF pwin(qfin(E)) < 3 AND pwin(qfin(f)) < 3 THEN nn = qfin(E): mm = qfin(f): ont
IF pwin(qfin(g)) < 3 AND pwin(qfin(h)) < 3 THEN nn = qfin(g): mm = qfin(h): ont
kolo = 4: LOCATE 12
IF pwin(qdin(aa)) < 3 AND pwin(qdin(bb)) < 3 THEN nn = qdin(aa): mm = qdin(bb): ont
IF pwin(qdin(cc)) < 3 AND pwin(qdin(dd)) < 3 THEN nn = qdin(cc): mm = qdin(dd): ont
IF pwin(qdin(ee)) < 3 AND pwin(qdin(ff)) < 3 THEN nn = qdin(ee): mm = qdin(ff): ont

CASE 47
LOCATE 3
kolo = 12
IF pwin(sfin(a)) < 3 AND pwin(sfin(b)) < 3 THEN nn = sfin(a): mm = sfin(b): ont
IF pwin(sfin(c)) < 3 AND pwin(sfin(d)) < 3 THEN nn = sfin(c): mm = sfin(d): ont
kolo = 4: LOCATE 12
IF pwin(sdin(a)) < 3 AND pwin(sdin(b)) < 3 THEN nn = sdin(a): mm = sdin(b): ont
IF pwin(sdin(c)) < 3 AND pwin(sdin(d)) < 3 THEN nn = sdin(c): mm = sdin(d): ont

CASE 49
LOCATE 3
kolo = 14
IF pwin(fin(a)) < 3 AND pwin(fin(b)) < 3 THEN nn = fin(a): mm = fin(b): ont
kolo = 6
IF pwin(bin(a)) < 3 AND pwin(bin(b)) < 3 THEN nn = bin(a): mm = bin(b): ont
kolo = 12: LOCATE 12
IF pwin(din(a)) < 3 AND pwin(din(b)) < 3 THEN nn = din(a): mm = din(b): ont

CASE 44, 46, 48, 50
LOCATE 3, 1: COLOR 12: PRINT "EI OTTELUITA"
LOCATE 12, 1: COLOR 4: PRINT "EI OTTELUITA"

END SELECT
END SUB

SUB naytaottehl
s = 1
LOCATE 3
kolo = 12
nn = ex(a): mm = ex(b): ont2
nn = ex(c): mm = ex(d): ont2
PRINT
nn = ex(a + 4): mm = ex(b + 4): ont2
nn = ex(c + 4): mm = ex(d + 4): ont2
PRINT
nn = ex(a + 8): mm = ex(b + 8): ont2
nn = ex(c + 8): mm = ex(d + 8): ont2
PRINT
nn = ex(a + 12): mm = ex(b + 12): ont2
nn = ex(c + 12): mm = ex(d + 12): ont2
PRINT
nn = ex(a + 16): mm = ex(b + 16): ont2
nn = ex(c + 16): mm = ex(d + 16): ont2
END SUB

SUB naytaottur
s = 1
LOCATE 3
kolo = 12
nn = tx(a): mm = tx(b): ont2
nn = tx(c): mm = tx(d): ont2
nn = tx(E): mm = tx(f): ont2
END SUB

SUB nhl
hablis = CINT(9 * RND) + 1
FOR zz = 48 TO 52
OPEN "data\nhlt.99" FOR INPUT AS #1

FOR zzz = 1 TO hablis
INPUT #1, l(zz), kk(zz), halli(zz), tazo(zz)
NEXT zzz

hablis = hablis + 1
CLOSE #1
NEXT zz
END SUB

SUB nollaux
FOR xxx = 1 TO plkm
FOR xx = 1 TO 32
IF pel(xx, xxx).ppp <> 1 OR ot = 0 THEN
pel(xx, xxx).gls = 0
pel(xx, xxx).ass = 0
END IF
NEXT xx
NEXT xxx

FOR xx = 1 TO 24 - plkm
FOR xxx = 1 TO 5
top(xx, xxx).gls = 0
top(xx, xxx).ass = 0
IF ot = 0 THEN top(xx, xxx).age = top(xx, xxx).age + 1
NEXT xxx
NEXT xx
END SUB

SUB nollaux2
FOR xxx = 1 TO plkm
FOR xx = 1 TO 32
pel(xx, xxx).inj = 0
pel(xx, xxx).kun = 0
pel(xx, xxx).plus = 0
pel(xx, xxx).kest = 0
pel(xx, xxx).pot = 0
NEXT xx
NEXT xxx
END SUB

SUB nollauxtauti
FOR xx = 1 TO 24
tautip(xx) = 1
tautik(xx) = 0
NEXT xx

FOR xx = 1 TO 24 - plkm
FOR z = 1 TO 3
tauti(z, xx) = 0
tkest(z, xx) = 0
NEXT z
NEXT xx


END SUB

SUB nostajunnu
pel(lpl(pv) + 1, pv) = jel(kurso, pv): lpl(pv) = lpl(pv) + 1: sortmaar: sortplayers

IF kurso < lpj(pv) THEN
FOR smut = kurso TO lpj(pv) - 1
jel(smut, pv) = jel(smut + 1, pv)
NEXT smut
END IF
lpj(pv) = lpj(pv) - 1
kurso = 1
IF lpj(pv) = 0 THEN lukka = 1
END SUB

SUB nyrkkeily
OPEN "data\nyrktul.mhm" FOR INPUT AS #1
z = CINT(4 * RND) + 1
FOR zz = 1 TO z
LINE INPUT #1, sano$
NEXT zz
CLOSE #1
PRINT sano$
END SUB

SUB oaamasopimus
DIM tarjo(1 TO 6) AS INTEGER
DIM kiinnos(1 TO 6) AS INTEGER
DIM tarjv(1 TO 6) AS INTEGER

SELECT CASE ottephl(pv) + ottediv(pv)
CASE IS < 100
tarjox% = 1
CASE 100 TO 200
tarjox% = 2
CASE ELSE
tarjox% = 3
END SELECT

IF ottephl(pv) >= (44 * 5) THEN tarjox% = tarjox% + 1
IF euros(pv) + ykkos(pv) + kakkos(pv) + kolmos(pv) >= 4 THEN tarjox% = tarjox% + 1

kiinnos(1) = 1
tarjo(1) = u(pv)
tarjv(1) = CINT(3 * RND) + 1
GOSUB tarjomaar

kurso = 1
oldkurso = 1
CLS
COLOR 13
PRINT "TY™SOPIMUKSESI ON KATKOLLA, MANAGERI ";
colchk
PRINT nimi(pv)
haahaa (15)
DO
LOCATE 3, 1
IF kurso = 1 THEN COLOR , 1 ELSE COLOR , 0
IF kiinnos(1) = 1 AND sr(u(pv)) = 1 THEN COLOR 14 ELSE IF kiinnos(1) = 1 AND sr(u(pv)) = 2 THEN COLOR 10 ELSE COLOR 7
PRINT RTRIM$(l(tarjo(1))); " (";
IF sr(tarjo(1)) = 1 THEN PRINT "PHL)";  ELSE PRINT "DIV)";

FOR zz = 2 TO 1 + tarjox%
LOCATE 3 + zz, 1
IF kurso = zz THEN COLOR , 1 ELSE COLOR , 0
IF kiinnos(zz) = 1 AND sr(tarjo(zz)) = 1 THEN COLOR 14 ELSE IF kiinnos(zz) = 1 AND sr(tarjo(zz)) = 2 THEN COLOR 10 ELSE COLOR 7
PRINT RTRIM$(l(tarjo(zz))); " (";
IF sr(tarjo(zz)) = 1 THEN PRINT "PHL)";  ELSE PRINT "DIV)";
NEXT zz
LOCATE 11, 1: COLOR , 0
IF kurso = 1 AND kiinnos(1) = 0 THEN
GOSUB klearskrex
LOCATE 11, 1: COLOR 15
PRINT "NYKYINEN TY™NANTAJASI EI HALUA KESKUSTELLA JATKOSOPIMUKSESTA!"
PRINT
lentti (99)
END IF
IF kurso > 1 OR kiinnos(1) = 1 THEN
xx = tarjo(kurso)
LOCATE 11, 1

COLOR 13: PRINT "SIJOITUS TOISSA KAUDELLA: ";
COLOR 14: PRINT LTRIM$(STR$(sedd(xx)))
PRINT
COLOR 13: PRINT "SIJOITUS VIIME KAUDELLA: ";
COLOR 14: PRINT LTRIM$(STR$(sed(xx)))
PRINT
COLOR 13: PRINT "JŽŽHALLIN TASO: ";
COLOR 14: PRINT hallina(halli(xx))
PRINT
COLOR 13: PRINT "MATERIAALIN TASO: ";
COLOR 14: IF kurso = 1 THEN PRINT "SAMA KUIN ENNENKIN" ELSE PRINT materia(materiax(tazo(xx))); tazo(xx)
PRINT
COLOR 13: PRINT "SOPIMUS (VUOTTA) ";
FOR zzzz = 1 TO tarjv(kurso)
IF oldkurso = zzzz THEN COLOR 1, 15 ELSE COLOR 14, 0
LOCATE 18 + zzzz, 18: PRINT LTRIM$(STR$(zzzz))
NEXT zzzz
COLOR , 0
END IF

neu$ = "": WHILE neu$ = "": neu$ = INKEY$: WEND
IF neu$ = CHR$(0) + CHR$(72) AND kurso > 1 THEN kurso = kurso - 1: oldkurso = 1: GOSUB klearskrex
IF neu$ = CHR$(0) + CHR$(80) AND kurso < tarjox% + 1 THEN kurso = kurso + 1: oldkurso = 1: GOSUB klearskrex
IF neu$ = CHR$(13) AND kiinnos(kurso) = 1 THEN COLOR , 0: EXIT DO
IF kiinnos(kurso) = 1 THEN
IF neu$ = CHR$(0) + CHR$(75) AND oldkurso > 1 THEN oldkurso = oldkurso - 1
IF neu$ = CHR$(0) + CHR$(77) AND oldkurso < tarjv(kurso) THEN oldkurso = oldkurso + 1
END IF
LOOP

IF kurso <> 1 THEN
ccc = tarjo(kurso)
gene
END IF
sopim(pv) = oldkurso


ERASE tarjo, kiinnos, tarjv
EXIT SUB

tarjomaar:
FOR zz = 2 TO 1 + tarjox%
dunkkis:
SELECT CASE zz
CASE 2
arpols3 (1)
CASE 3
arpols3 (2)
CASE 4
arpols3 (3)
CASE 5
arpols3 (4)
CASE 6
arpols3 (5)
END SELECT
IF zz > 2 THEN
FOR zzz = 2 TO zz - 1
IF xx = tarjo(zzz) THEN GOTO dunkkis
NEXT zzz
END IF
tarjo(zz) = xx
kiinnos(zz) = 1
tarjv(zz) = CINT(3 * RND) + 1
NEXT zz
RETURN

klearskrex:
FOR temp% = 1 TO 20
LOCATE 10 + temp%, 1: PRINT STRING$(80, " ");
NEXT temp%
RETURN


END SUB

SUB ont
LOCATE , s
COLOR kolo: colchk2
PRINT l(nn);
COLOR kolo
PRINT "(";
COLOR kolo: colchk2
PRINT LTRIM$(STR$(s(nn)));
COLOR kolo
PRINT ")";
IF s(nn) < 10 THEN PRINT " ";
PRINT " - ";
COLOR kolo: colchk3
PRINT l(mm);
COLOR kolo
PRINT "(";
COLOR kolo: colchk3
PRINT LTRIM$(STR$(s(mm)));
COLOR kolo
PRINT ") "
END SUB

SUB ont2
LOCATE , s
COLOR kolo: colchk2
PRINT l(nn);
COLOR kolo
PRINT " - ";
colchk3
PRINT l(mm)
END SUB

SUB opflisays
FOR xx = 1 TO plkm
IF sr(u(xx)) = 1 AND mukp(xx) = 1 THEN ottepof(xx) = ottepof(xx) + 1
IF sr(u(xx)) = 2 AND mukp(xx) = 1 THEN ottedof(xx) = ottedof(xx) + 1
NEXT xx
END SUB

SUB opzions
DIM optio(1 TO 7) AS INTEGER
DIM optname(1 TO 7) AS STRING

OPEN "data\optname.99" FOR INPUT AS #1
FOR z = 1 TO 7
INPUT #1, optname(z)
NEXT z
CLOSE #1

FOR z = 1 TO 6
optio(z) = naytajt(z)
NEXT z
optio(7) = pikaprot

kurso = 1
rokkimake:
CLS
haahaa (7)
DO
LOCATE 1, 1
FOR z = 1 TO 7
IF kurso = z THEN COLOR 9 ELSE COLOR 1
PRINT optname(z)
PRINT
NEXT z
FOR zz = 1 TO 7
IF kurso = zz THEN COLOR 10 ELSE COLOR 2
LOCATE 1 + (zz - 1) * 2, 30
IF optio(zz) = 0 THEN PRINT "EI   ";  ELSE PRINT "KYLLŽ";
NEXT zz
op$ = "": WHILE op$ = "": op$ = INKEY$: WEND
IF op$ = "?" THEN qelp (7): GOTO rokkimake
IF op$ = CHR$(0) + CHR$(72) AND kurso > 1 THEN kurso = kurso - 1
IF op$ = CHR$(0) + CHR$(80) AND kurso < 7 THEN kurso = kurso + 1
IF op$ = CHR$(0) + CHR$(75) AND optio(kurso) = 1 THEN optio(kurso) = 0
IF op$ = CHR$(0) + CHR$(77) AND optio(kurso) = 0 THEN optio(kurso) = 1

IF op$ = CHR$(27) THEN EXIT DO
LOOP

FOR z = 1 TO 6
naytajt(z) = optio(z)
NEXT z
pikaprot = optio(7)

ERASE optio, optname
END SUB

SUB orgamaar
SELECT CASE sed(ccc) + sedd(ccc)
CASE 2 TO 4
temp% = 1
CASE 5 TO 12
temp% = 2
CASE 13 TO 19
temp% = 3
CASE 20 TO 23
temp% = 4
CASE 24 TO 27
temp% = 5
CASE 28 TO 40
temp% = 6
CASE IS > 40
temp% = 7
END SELECT

OPEN "data\orga.99" FOR INPUT AS #1
FOR zz = 1 TO temp%
INPUT #1, valb(1, pv), valb(2, pv), valb(3, pv), valb(4, pv), valb(5, pv), mo(pv), fani(pv), mtap(pv), frank%, raha(pv)
NEXT zz
CLOSE #1

FOR zz = 2 TO 12
skout(zz, pv) = 0
NEXT zz
IF frank% > 0 THEN skoutmaar (frank%)

efed(pv) = 0
junnu(pv) = valb(3, pv)
END SUB

SUB organisaatio
kurso = 1
duccix:
COLOR , 0
CLS
haahaa (10)
LOCATE 1, 1
COLOR 14: PRINT RTRIM$(l(u(pv))); " ";
COLOR 13: PRINT "(";
COLOR 14: PRINT kk(u(pv));
COLOR 13: PRINT ")"
PRINT
COLOR 13: PRINT "         MANAGERI    ";
COLOR 14: PRINT nimi(pv)
PRINT
COLOR 13: PRINT "VALMENNUSBUDJETTI    ";
COLOR 14: PRINT LTRIM$(STR$(valbh(vai2(pv), 1, valb(1, pv))))
PRINT
COLOR 13: PRINT " MV-VALM.BUDJETTI    ";
COLOR 14: PRINT LTRIM$(STR$(valbh(vai2(pv), 2, valb(2, pv))))
PRINT
COLOR 13: PRINT "  JUNIORIBUDJETTI    ";
COLOR 14: PRINT LTRIM$(STR$(valbh(vai2(pv), 3, valb(3, pv))))
PRINT
COLOR 13: PRINT "   HUOLTOBUDJETTI    ";
COLOR 14: PRINT LTRIM$(STR$(valbh(vai2(pv), 4, valb(4, pv))))
PRINT
COLOR 13: PRINT "     LUONTAISEDUT    ";
COLOR 14: PRINT LTRIM$(STR$(valbh(vai2(pv), 5, valb(5, pv)) * lpl(pv)))
LOCATE 1, 40: COLOR 13: PRINT "SPONSORI    ";
COLOR 14: PRINT snimi(pv)

LOCATE 3
FOR xxx = 1 TO 8
temp% = 0
IF xxx = 2 AND muke(pv) = 0 THEN temp% = 1
IF xxx = 7 AND muke(pv) = 0 THEN temp% = 1
IF xxx = 3 AND sr(u(pv)) = 2 THEN temp% = 1
IF xxx = 4 AND sr(u(pv)) = 2 THEN temp% = 1
IF xxx = 5 AND sr(u(pv)) = 2 THEN temp% = 1
IF temp% = 0 THEN
LOCATE , 40
COLOR 7
PRINT sponsax(xxx);
LOCATE , 69
COLOR 15: PRINT LTRIM$(STR$(sraha(xxx, pv)))
PRINT
END IF
NEXT xxx

DO
FOR zz = 1 TO 6
LOCATE 20 + zz, 1: PRINT STRING$(80, " ");
NEXT zz
COLOR 14: LOCATE 21, 1
IF kurso = 1 THEN luetexti ("faniryhm") ELSE IF kurso = 2 THEN luetexti ("pekkiini") ELSE luetexti ("matktap")

LOCATE 28, 1
COLOR 15
IF kulta = u(pv) THEN PRINT "HALLUSSAMME ON PHL:N MESTARUUSPOKAALI!": PRINT
IF emestari = u(pv) THEN PRINT "HALLUSSAMME ON EHL:N MESTARUUSPYTTY!"


IF kurso = 1 THEN COLOR 12, 0 ELSE COLOR 4, 0
LOCATE 15, 1: PRINT "PALKATTU TOSIFANIEN RYHMŽ  ";
IF kurso = 1 THEN COLOR 10, 0 ELSE COLOR 2, 0
PRINT faniryhma(fani(pv));
IF kurso = 2 THEN COLOR 12, 0 ELSE COLOR 4, 0
LOCATE 17, 1: PRINT "        PEKKIINI-DOUPPAUS  ";
IF kurso = 2 THEN COLOR 10, 0 ELSE COLOR 2, 0
IF efed(pv) > 0 THEN PRINT efedriini(efed(pv) - 1);  ELSE PRINT efedriini(efed(pv));
IF kurso = 3 THEN COLOR 12, 0 ELSE COLOR 4, 0
LOCATE 19, 1: PRINT "            MATKUSTUSTAPA  ";
IF kurso = 3 THEN COLOR 10, 0 ELSE COLOR 2, 0
PRINT mtapa(mtap(pv));

va$ = "": WHILE va$ = "": va$ = INKEY$: WEND
IF va$ = "?" THEN qelp (10): GOTO duccix
IF va$ = CHR$(0) + CHR$(72) AND kurso > 1 THEN kurso = kurso - 1
IF va$ = CHR$(0) + CHR$(80) AND kurso < 3 THEN kurso = kurso + 1
SELECT CASE kurso

CASE 1
IF va$ = CHR$(0) + CHR$(75) AND fani(pv) > 0 THEN fani(pv) = fani(pv) - 1
IF va$ = CHR$(0) + CHR$(77) AND fani(pv) < 1 THEN fani(pv) = fani(pv) + 1
CASE 2
IF va$ = CHR$(0) + CHR$(75) AND efed(pv) > 0 THEN efed(pv) = efed(pv) - 2
IF va$ = CHR$(0) + CHR$(77) AND efed(pv) < 2 THEN efed(pv) = efed(pv) + 2
CASE 3
IF va$ = CHR$(0) + CHR$(75) AND mtap(pv) > 0 THEN mtap(pv) = mtap(pv) - 1
IF va$ = CHR$(0) + CHR$(77) AND mtap(pv) < 3 THEN mtap(pv) = mtap(pv) + 1
END SELECT

IF va$ = "p" AND kulta = u(pv) THEN SCREEN 13: kuva ("winphl"): SCREEN 0: WIDTH 80, 43: GOTO duccix
IF va$ = "e" AND emestari = u(pv) THEN SCREEN 13: kuva ("winehl"): SCREEN 0: WIDTH 80, 43: GOTO duccix
IF va$ = CHR$(27) THEN EXIT DO
LOOP

COLOR , 0
END SUB

SUB osto2
xxx = kurso
LOCATE 40, 1: COLOR 15: PRINT "KUINKA PALJON TARJOAT?"
CALL tarjousmaar(42, 1)
xx = sortc(kurso)
CLS : COLOR 13
PRINT "MANAGERI ";
ccc = borsch
colchk4
PRINT nimi(borsch);
COLOR 13: PRINT ", TEILLE ON PUHELU LINJALLA "; LTRIM$(STR$(CINT(4 * RND) + 1)); "!!"
LOCATE 3, 1
printpel (2)
LOCATE 5, 1
ccc = pv
colchk4
PRINT nimi(pv);
COLOR 13
PRINT " TARJOAA ";
COLOR 14: PRINT LTRIM$(STR$(tarjous));
COLOR 13: PRINT " EUROA KYSEISESTŽ PELAAJASTA!"

PRINT
ccc = borsch: colchk4
PRINT nimi(borsch);
COLOR 13: PRINT ", HYVŽKSYTK™ EHDOTUKSEN?"

kurso = 2
DO
LOCATE 9, 1
IF kurso = 1 THEN COLOR 14, 13 ELSE COLOR 14, 0
PRINT "TOTTA KAI!"
IF kurso = 2 THEN COLOR 14, 13 ELSE COLOR 14, 0
PRINT "EN MISSŽŽN TAPAUKSESSA!"

sop$ = "": WHILE sop$ = "": sop$ = INKEY$: WEND
IF sop$ = CHR$(0) + CHR$(72) AND kurso > 1 THEN kurso = kurso - 1
IF sop$ = CHR$(0) + CHR$(80) AND kurso < 2 THEN kurso = kurso + 1
IF sop$ = CHR$(13) AND kurso = 1 THEN EXIT DO
IF sop$ = CHR$(13) AND kurso = 2 THEN kurso = xxx: EXIT SUB
LOOP

lpl(pv) = lpl(pv) + 1
xxx = lpl(pv)

pel(xxx, pv) = pel(xx, borsch)
pel(xxx, pv).pok = 0

joukpois = pv
viimpel = xx
joukpois = borsch
viimpel = xx
ketjutyhjenna
ketjumaar
pelaamaar
kurso = 1
END SUB

SUB otmuut
IF gl(nn) > gl(mm) THEN win(nn) = win(nn) + 1: los(mm) = los(mm) + 1: p(nn) = p(nn) + 2
IF gl(nn) < gl(mm) THEN los(nn) = los(nn) + 1: win(mm) = win(mm) + 1: p(mm) = p(mm) + 2
IF gl(nn) = gl(mm) THEN dra(nn) = dra(nn) + 1: dra(mm) = dra(mm) + 1: p(nn) = p(nn) + 1: p(mm) = p(mm) + 1
gf(nn) = gf(nn) + gl(nn): gf(mm) = gf(mm) + gl(mm): ga(nn) = ga(nn) + gl(mm): ga(mm) = ga(mm) + gl(nn)
END SUB

SUB otmuutehl
ero(nn1) = ero(nn1) + gl(nn) - gl(mm)
ero(mm1) = ero(mm1) + gl(mm) - gl(nn)
IF gl(nn) > gl(mm) THEN ep(nn1) = ep(nn1) + 2
IF gl(nn) = gl(mm) THEN ep(nn1) = ep(nn1) + 1: ep(mm1) = ep(mm1) + 1
IF gl(nn) < gl(mm) THEN ep(mm1) = ep(mm1) + 2
END SUB

SUB otmuutpoff
IF gl(nn) > gl(mm) THEN pwin(nn) = pwin(nn) + 1
IF gl(nn) < gl(mm) THEN pwin(mm) = pwin(mm) + 1
IF sr(nn) = 1 THEN ppotti = ppotti + (lhinta(sr(nn)) * CLNG(ylm(nn)))
END SUB

SUB otmuuttur
tero(nn1) = tero(nn1) + gl(nn) - gl(mm)
tero(mm1) = tero(mm1) + gl(mm) - gl(nn)
IF gl(nn) > gl(mm) THEN tp(nn1) = tp(nn1) + 2
IF gl(nn) = gl(mm) THEN tp(nn1) = tp(nn1) + 1: tp(mm1) = tp(mm1) + 1
IF gl(nn) < gl(mm) THEN tp(mm1) = tp(mm1) + 2
END SUB

SUB ottelmaar
SELECT CASE ot
CASE IS < 44
a = fxt(1, ki2)
b = fxt(2, ki2)
c = fxt(3, ki2)
d = fxt(4, ki2)
E = fxt(5, ki2)
f = fxt(6, ki2)
g = fxt(7, ki2)
h = fxt(8, ki2)
i = fxt(9, ki2)
j = fxt(10, ki2)
k = fxt(11, ki2)
m = fxt(12, ki2)
IF ki > 11 THEN
SWAP a, b
SWAP c, d
SWAP E, f
SWAP g, h
SWAP i, j
SWAP k, m
END IF
CASE 45
IF pkr = 1 OR pkr = 3 OR pkr = 5 THEN a = 1: b = 8: c = 2: d = 7: E = 3: f = 6: g = 4: h = 5: aa = 1: bb = 6: cc = 2: dd = 5: ee = 3: ff = 4
IF pkr = 2 OR pkr = 4 THEN a = 8: b = 1: c = 7: d = 2: E = 6: f = 3: g = 5: h = 4: aa = 6: bb = 1: cc = 5: dd = 2: ee = 4: ff = 3
CASE 47
IF pkr = 1 OR pkr = 3 OR pkr = 5 THEN a = 1: b = 4: c = 2: d = 3
IF pkr = 2 OR pkr = 4 THEN a = 4: b = 1: c = 3: d = 2
CASE 49
IF pkr = 1 OR pkr = 3 OR pkr = 5 THEN a = 1: b = 2
IF pkr = 2 OR pkr = 4 THEN a = 2: b = 1
END SELECT
END SUB

SUB ottelmaarehl
SELECT CASE ekr
CASE 1
a = 1: b = 2: c = 3: d = 4
CASE 5
a = 1: b = 3: c = 2: d = 4
CASE 3
a = 1: b = 4: c = 2: d = 3
CASE 4
a = 2: b = 1: c = 4: d = 3
CASE 2
a = 3: b = 1: c = 4: d = 2
CASE 6
a = 4: b = 1: c = 3: d = 2
END SELECT
END SUB

SUB ottelmaarhaarax
SELECT CASE kr

CASE IS < 1
veikko = 0
runkosarja = 0

CASE 4, 7, 11, 15, 19, 23
ottelmaarehl
veikko = 0
runkosarja = 0

CASE 29
runkosarja = 0
veikko = 0

CASE 36
runkosarja = 0
veikko = 0

CASE ELSE
IF ot < 44 OR ot = 45 OR ot = 47 OR ot = 49 THEN ottelmaar
IF ot < 44 THEN
IF arvsulk = 1 THEN
IF CINT(100 * RND) < 8 THEN arvsulk = 0
END IF
IF arvsulk = 1 THEN veikko = 0 ELSE veikko = 1
END IF
IF ot >= 44 THEN veikko = 0
IF ot < 44 THEN runkosarja = 1 ELSE runkosarja = 0
END SELECT
END SUB

SUB ottpel
DIM maali(30, 2) AS STRING
DIM syotto(2, 14, 2) AS STRING

mwnn = mw(nn)
pwnn = pw(nn)
hwnn = hw(nn)

mwmm = mw(mm)
pwmm = pw(mm)
hwmm = hw(mm)

IF runkosarja = 1 THEN
kotiot(nn) = kotiot(nn) + 1
etu1 = 1
SELECT CASE ohj(mm)
CASE 0
SELECT CASE (sed(mm) + sedd(mm) + seddd(mm)) / 3
CASE 1 TO 4
etu2 = .95
CASE 4.0001 TO 8
etu2 = .93
CASE 8.0001 TO 16
etu2 = .91
CASE ELSE
etu2 = .9
END SELECT
CASE ELSE
SELECT CASE mtap(ohj(mm))
CASE 0
etu2 = .9
CASE 1
etu2 = .91
CASE 2
etu2 = .93
CASE 3
etu2 = .95
END SELECT
END SELECT

IF ot > 5 AND ot < 44 THEN
IF ohj(nn) = 0 THEN
IF s(mm) - s(nn) >= 4 THEN
zzz = (s(mm) - s(nn) - 3) * 7
IF CINT(100 * RND) < zzz THEN etu1 = etu1 - .15
END IF
END IF
END IF

END IF

IF ohj(nn) <> 0 THEN ww = nn: pv = ohj(nn): pelpelpel
IF ohj(mm) <> 0 THEN ww = mm: pv = ohj(mm): pelpelpel

IF ohj(nn) = 0 AND nn < 25 THEN
mwnn = mwnn + tauti(1, tox(nn))
pwnn = pwnn + tauti(2, tox(nn))
hwnn = hwnn + tauti(3, tox(nn))
IF laxa(nn) = 2 THEN mwnn = 0: pwnn = 0: hwnn = 0: laxa(nn) = 0
END IF

IF mwnn < 0 THEN mwnn = 0
IF pwnn < 0 THEN pwnn = 0
IF hwnn < 0 THEN hwnn = 0

IF ohj(mm) = 0 AND mm < 25 THEN
mwmm = mwmm + tauti(1, tox(mm))
pwmm = pwmm + tauti(2, tox(mm))
hwmm = hwmm + tauti(3, tox(mm))
IF laxa(mm) = 2 THEN mwmm = 0: pwmm = 0: hwmm = 0: laxa(mm) = 0
END IF

IF mwmm < 0 THEN mwmm = 0
IF pwmm < 0 THEN pwmm = 0
IF hwmm < 0 THEN hwmm = 0


IF nn < 25 THEN
mwnn = tautip(nn) * mwnn
pwnn = tautip(nn) * pwnn
hwnn = tautip(nn) * hwnn
END IF
IF mm < 25 THEN
mwmm = tautip(mm) * mwmm
pwmm = tautip(mm) * pwmm
hwmm = tautip(mm) * hwmm
END IF

IF mwnn = 0 THEN GOTO luovutus1
mf1 = mwnn / 10
mf1 = mf1 * etu1
IF nn < 25 THEN mf1 = mf1 * tre(nn)
mf1 = .5 * mf1

IF pwnn = 0 THEN GOTO luovutus1
pf1 = pwnn / 60
pf1 = pf1 * etu1
IF nn < 25 THEN pf1 = pf1 * tre(nn)


IF hwnn = 0 THEN GOTO luovutus1
hf1 = hwnn / 120
hf1 = hf1 * etu1
IF nn < 25 THEN hf1 = hf1 * tre(nn)


IF mwmm = 0 THEN GOTO luovutus2
mf2 = mwmm / 10
mf1 = mf1 * etu2
IF mm < 25 THEN mf2 = mf2 * tre(mm)
mf2 = .5 * mf2

IF pwmm = 0 THEN GOTO luovutus2
pf2 = pwmm / 60
pf2 = pf2 * etu2
IF mm < 25 THEN pf2 = pf2 * tre(mm)


IF hwmm = 0 THEN GOTO luovutus2
hf2 = hwmm / 120
hf2 = hf2 * etu2
IF mm < 25 THEN hf2 = hf2 * tre(mm)

FOR xx = 1 TO 15
IF hf1 * RND > pf2 * RND THEN
IF hf1 * RND > mf2 * RND + mf2 / 3 THEN gl(nn) = gl(nn) + 1
END IF

IF hf2 * RND > pf1 * RND THEN
IF hf2 * RND > mf1 * RND + mf1 / 3 THEN gl(mm) = gl(mm) + 1
END IF

NEXT xx

IF gl(nn) = gl(mm) THEN GOSUB jatkoaika

IF lm = 1 THEN
IF sopu(nn) = 1 AND sopu(mm) = 1 THEN
ja(nn) = 1: ja(mm) = 1
IF ohj(nn) = 0 THEN sopu(nn) = 0
IF ohj(mm) = 0 THEN sopu(mm) = 0
gl(nn) = CINT(4 * RND) + 1
gl(mm) = gl(nn)
END IF
IF sopu(nn) = 1 THEN
ja(mm) = 0: ja(nn) = 0
IF ohj(nn) = 0 THEN sopu(nn) = 0
IF gl(mm) = 0 THEN gl(mm) = 1
IF gl(nn) >= gl(mm) THEN gl(nn) = gl(mm) - CINT(2 * RND) - 1
IF gl(nn) < 0 THEN gl(nn) = 0
END IF
IF sopu(mm) = 1 THEN
IF ohj(mm) = 0 THEN sopu(mm) = 0
ja(mm) = 0: ja(nn) = 0
IF gl(nn) = 0 THEN gl(nn) = 1
IF gl(mm) >= gl(nn) THEN gl(mm) = gl(nn) - CINT(2 * RND) - 1
IF gl(mm) < 0 THEN gl(mm) = 0
END IF
END IF

ww = nn: pisteet
ww = mm: pisteet
IF runkosarja = 1 THEN
ylek(nn) = ylek(nn) + ylm(nn)
END IF
'PRINT mf1; pf1; hf1; " "; mf2; pf2; hf2
ERASE maali, syotto
EXIT SUB

luovutus1:
gl(nn) = 0: gl(mm) = 12
ylm(nn) = 0
ERASE maali, syotto
EXIT SUB
luovutus2:
ylm(nn) = 0
gl(mm) = 0: gl(nn) = 12
ERASE maali, syotto
EXIT SUB


jatkoaika:
ja(nn) = 1: ja(mm) = 1
SELECT CASE ot

CASE IS > 44
DO
IF hf1 * RND > pf2 * RND THEN
IF hf1 * RND > mf2 * RND + mf2 / 3 THEN gl(nn) = gl(nn) + 1: EXIT DO
END IF

IF hf2 * RND > pf1 * RND THEN
IF hf2 * RND > mf1 * RND + mf1 / 3 THEN gl(mm) = gl(mm) + 1: EXIT DO
END IF
LOOP

CASE ELSE

FOR xx = 1 TO 1
IF hf1 * RND > pf2 * RND THEN
IF hf1 * RND > mf2 * RND + mf2 / 3 THEN gl(nn) = gl(nn) + 1: EXIT FOR
END IF

IF hf2 * RND > pf1 * RND THEN
IF hf2 * RND > mf1 * RND + mf1 / 3 THEN gl(mm) = gl(mm) + 1: EXIT FOR
END IF
NEXT xx
END SELECT
RETURN
END SUB

SUB ottul
COLOR kolo: colchk2
PRINT l(nn); " ";
COLOR kolo
PRINT " - ";
COLOR kolo: colchk3
PRINT l(mm); " ";
COLOR kolo: colchk2
PRINT LTRIM$(STR$(gl(nn)));
IF gl(nn) < 10 THEN PRINT " ";
COLOR kolo
PRINT " - ";
COLOR kolo: colchk3
IF gl(mm) < 10 THEN PRINT " ";
PRINT LTRIM$(STR$(gl(mm)));
PRINT " ";
IF ja(mm) = 1 OR ja(nn) = 1 THEN PRINT "ja. ";
IF ja(mm) = 0 AND ja(nn) = 0 THEN PRINT "    ";
COLOR 5
colchk2
IF turnauz = 0 THEN
PRINT "Yleis”„: ";
IF ylm(nn) = halliip(halli(nn)) + hallisp(halli(nn)) THEN COLOR 14 ELSE COLOR 6
PRINT LTRIM$(STR$(ylm(nn)))
END IF
IF turnauz = 1 THEN PRINT

END SUB

SUB palkmaar (fat%)
SELECT CASE fat%
CASE 1
temp1% = pel(xxx, pv).psk
temp2% = pel(xxx, pv).age
temp3% = pel(xxx, pv).ego
CASE 2
temp1% = neup.psk
temp2% = neup.age
temp3% = neup.ego
END SELECT

palkmiinus = 1
IF temp2% > 26 THEN
joo = (temp2% - 25) / 80
palkmiinus = 1 - joo
END IF
IF temp2% < 24 THEN
joo = (26 - temp2%) / 80
palkmiinus = 1 + joo
END IF

palkehd = FIX(((20 + temp1%) * (temp1% + 3)) ^ 1.2)

IF temp1% > 4 THEN
palkehd = FIX(palkehd * (1 + ((temp1% - 3) / 35)))
END IF
IF temp1% > 6 THEN
palkehd = FIX(palkehd * (1 + ((temp1% - 5) / 15)))
END IF
IF temp1% > 9 THEN
palkehd = FIX(palkehd * (1 + ((temp1% - 8) / 15)))
END IF
IF temp1% > 12 THEN
palkehd = FIX(palkehd * (1 + ((temp1% - 11) / 35)))
END IF
IF temp1% > 14 THEN
palkehd = FIX(palkehd * (1 + ((temp1% - 14) / 55)))
END IF

palkehd = FIX(palkehd * palkmiinus)
palkehd = FIX(palkehd * (1 + (temp3% / 30)))

SELECT CASE fat%
CASE 1
IF pel(xxx, pv).ppp = 1 THEN palkehd = palkehd * 2
CASE 2
IF neup.ppp = 1 THEN palkehd = palkehd * 2
END SELECT
END SUB

SUB pankki
DIM luotto(1 TO 3) AS LONG
luotto(1) = 500000
luotto(2) = 1000000
luotto(3) = 3000000

kurso = 1
curso = 1

sulka:
CLS
COLOR 15
PRINT CHR$(14); " ";
COLOR 2: PRINT "JOS RAHAHUOLET PAINAA, HAE MEILTŽ LAINAA! ";
COLOR 15: PRINT CHR$(14)
DO
LOCATE 3, 1
FOR xxx = 1 TO 3
IF kurso = xxx THEN COLOR 14, 13 ELSE COLOR 14, 0
PRINT panknim(xxx);
COLOR , 0: PRINT "    ";
IF xxx = 3 THEN PRINT
NEXT xxx
klearstat (5)
haahaa (16)
COLOR 12: LOCATE 5, 1
luetexti ("panki" + LTRIM$(STR$(kurso)))
LOCATE 10, 1
COLOR 13: PRINT "VELKAA TŽNNE: ";
COLOR 14: PRINT LTRIM$(STR$(laina(kurso, pv)))
LOCATE 12, 1
COLOR 13: PRINT "VELKAA YHTEENSŽ: ";
COLOR 14: PRINT LTRIM$(STR$(laina(1, pv) + laina(2, pv) + laina(3, pv)))
LOCATE 10, 40
COLOR 13: PRINT "LUOTTORAJA: ";
COLOR 14: PRINT LTRIM$(STR$(luotto(kurso)))
LOCATE 12, 40
COLOR 13: PRINT "VUOSIKORKO: ";
COLOR 14: PRINT LTRIM$(STR$(pankkor(kurso))); "%"

LOCATE 14, 1
IF laina(kurso, pv) < luotto(kurso) THEN COLOR 14: xx = 1 ELSE COLOR 6: xx = 0
IF curso = 1 THEN COLOR , 1:  ELSE COLOR , 0
PRINT "OTAN VELKAA"
IF laina(kurso, pv) > 0 THEN COLOR 14: ccc = 1 ELSE COLOR 8: ccc = 0
IF curso = 2 THEN COLOR , 1 ELSE COLOR , 0
PRINT "LYHENNŽN VELKAA"


me$ = "": WHILE me$ = "": me$ = INKEY$: WEND
IF me$ = "?" THEN qelp (16): GOTO sulka
IF me$ = CHR$(0) + CHR$(77) AND kurso < 3 THEN kurso = kurso + 1
IF me$ = CHR$(0) + CHR$(75) AND kurso > 1 THEN kurso = kurso - 1
IF me$ = CHR$(0) + CHR$(72) AND curso > 1 THEN curso = curso - 1
IF me$ = CHR$(0) + CHR$(80) AND curso < 2 THEN curso = curso + 1
IF me$ = CHR$(13) AND curso = 1 AND xx = 1 THEN GOSUB johan: GOTO sulka
IF me$ = CHR$(13) AND curso = 2 AND ccc = 1 THEN GOSUB johan: GOTO sulka

IF me$ = CHR$(27) THEN EXIT DO
LOOP

ERASE luotto
COLOR , 0
EXIT SUB

johan:
tarjous = 0
LOCATE 17, 1: COLOR 13, 0
PRINT panknim(kurso);
PRINT " ON ILOINEN VOIDESSAAN AUTTAA! PALJONKO ";
IF curso = 1 THEN PRINT "LAINAATTE?" ELSE PRINT "LYHENNŽTTE?"
COLOR 12
DO
LOCATE 19, 1: PRINT LTRIM$(STR$(tarjous)); "       ";
tar$ = ""
WHILE tar$ = "": tar$ = INKEY$: WEND
IF tar$ = CHR$(0) + CHR$(72) THEN tarjous = tarjous + 10000
IF tar$ = CHR$(0) + CHR$(80) THEN tarjous = tarjous - 10000
IF tar$ = CHR$(0) + CHR$(75) THEN tarjous = tarjous - 100000
IF tar$ = CHR$(0) + CHR$(77) THEN tarjous = tarjous + 100000
SELECT CASE curso
CASE 1
IF tarjous > luotto(kurso) - laina(kurso, pv) THEN tarjous = luotto(kurso) - laina(kurso, pv)
CASE 2
IF tarjous > raha(pv) THEN tarjous = raha(pv)
IF tarjous > laina(kurso, pv) THEN tarjous = laina(kurso, pv)
END SELECT
IF tarjous < 0 THEN tarjous = 0
IF tar$ = CHR$(13) THEN EXIT DO
LOOP
SELECT CASE curso
CASE 1
laina(kurso, pv) = laina(kurso, pv) + tarjous
raha(pv) = raha(pv) + tarjous
CASE 2
laina(kurso, pv) = laina(kurso, pv) - tarjous
raha(pv) = raha(pv) - tarjous
END SELECT
RETURN

END SUB

SUB pelaajasiirrot
hoklays:
CLS
COLOR 13: PRINT "URANSA LOPETTAVIA (TAI KOTIMAAHANSA PALAAVIA) PELAAJIA:"
PRINT
FOR zzzz = 1 TO 24
zz = x(zzzz)
IF ohj(zz) = 0 THEN
FOR xx = 1 TO 5
IF top(tox(zz), xx).age > 32 THEN
ccc = CINT(100 * RND)
IF ccc < (top(tox(zz), xx).age - 32) * 10 THEN
IF xx = 1 THEN COLOR 15 ELSE IF xx = 2 THEN COLOR 7 ELSE COLOR 8
PRINT top(tox(zz), xx).nam; " ";
IF sr(zz) = 1 THEN COLOR 14 ELSE COLOR 6
PRINT "("; RTRIM$(l(zz)); ")"
top(tox(zz), xx).nam = ""
END IF
END IF
NEXT xx
FOR xxx = 1 TO 3
IF CINT(100 * RND) < 20 THEN
keisi
mahmax
COLOR 8
PRINT krj$ + "." + ptemp$; " ";
IF sr(zz) = 1 THEN COLOR 14 ELSE COLOR 6
LOCATE , 15: PRINT "("; RTRIM$(l(zz)); ")"
END IF
NEXT xxx
END IF
NEXT zzzz
pjn

CLS
COLOR 13: PRINT "MERKITTŽVIŽ PELAAJASIIRTOJA, I OSA"
PRINT
jahhas:
tarko = 0
FOR zzzz = 1 TO 24
zz = x(zzzz)
IF ohj(zz) = 0 THEN
FOR zzz = 1 TO 5
IF top(tox(zz), zzz).nam = "             " THEN tarko = tarko + 1: GOSUB jaahas
NEXT zzz
END IF
NEXT zzzz
IF tarko > 0 THEN GOTO jahhas
pjn
CLS
COLOR 13: PRINT "MERKITTŽVIŽ PELAAJASIIRTOJA, II OSA"
PRINT
sax% = 1
GOSUB siirtola
sax% = 13
GOSUB siirtola
pjn
EXIT SUB

jaahas:
SELECT CASE zzzz
CASE IS < 13
ccc = CINT(100 * RND)
SELECT CASE ccc
CASE 0 TO 30
arpod (1)
IF lukka = 0 AND top(tox(xx), zzz).nam <> "             " THEN
IF zzz = 1 THEN COLOR 15 ELSE IF zzz = 2 THEN COLOR 7 ELSE COLOR 8
PRINT top(tox(xx), zzz).nam; " ";
IF sr(xx) = 1 THEN COLOR 10 ELSE COLOR 2
PRINT l(xx);
COLOR 7: PRINT " --> ";
IF sr(zz) = 1 THEN COLOR 14 ELSE COLOR 6
PRINT l(zz)
SWAP top(tox(zz), zzz), top(tox(xx), zzz)
END IF
CASE 31 TO 65
arpol (1)
IF lukka = 0 AND top(tox(xx), zzz).nam <> "             " THEN
IF zzz = 1 THEN COLOR 15 ELSE IF zzz = 2 THEN COLOR 7 ELSE COLOR 8
PRINT top(tox(xx), zzz).nam; " ";
IF sr(xx) = 1 THEN COLOR 10 ELSE COLOR 2
PRINT l(xx);
COLOR 7: PRINT " --> ";
IF sr(zz) = 1 THEN COLOR 14 ELSE COLOR 6
PRINT l(zz)
SWAP top(tox(zz), zzz), top(tox(xx), zzz)
END IF
CASE ELSE
keisi
mahmax
top(tox(zz), zzz).nam = krj$ + "." + ptemp$
top(tox(zz), zzz).age = CINT(7 * RND) + 18
IF nats <> 1 THEN
IF zzz = 1 THEN COLOR 15 ELSE IF zzz = 2 THEN COLOR 7 ELSE COLOR 8
PRINT top(tox(zz), zzz).nam; " ";
COLOR 15: IF nats = 10 THEN PRINT kansal(nats);  ELSE PRINT kansap(nats);
LOCATE , 25: COLOR 7: PRINT " --> ";
IF sr(zz) = 1 THEN COLOR 14 ELSE COLOR 6
PRINT l(zz)
END IF
END SELECT
CASE ELSE
ccc = CINT(100 * RND)
SELECT CASE ccc
CASE 1 TO 50
arpod (1)
IF lukka = 0 AND top(tox(xx), zzz).nam <> "             " THEN
IF zzz = 1 THEN COLOR 15 ELSE IF zzz = 2 THEN COLOR 7 ELSE COLOR 8
PRINT top(tox(xx), zzz).nam; " ";
IF sr(xx) = 1 THEN COLOR 10 ELSE COLOR 2
PRINT l(xx);
COLOR 7: PRINT " --> ";
IF sr(zz) = 1 THEN COLOR 14 ELSE COLOR 6
PRINT l(zz)
SWAP top(tox(zz), zzz), top(tox(xx), zzz)
END IF
CASE ELSE
keisi
mahmax
top(tox(zz), zzz).nam = krj$ + "." + ptemp$
top(tox(zz), zzz).age = CINT(8 * RND) + 18
top(tox(zz), zzz).nat = nats
IF nats <> 1 THEN
IF zzz = 1 THEN COLOR 15 ELSE IF zzz = 2 THEN COLOR 7 ELSE COLOR 8
PRINT top(tox(zz), zzz).nam; " ";
COLOR 15: IF nats = 10 THEN PRINT kansal(nats);  ELSE PRINT kansap(nats);
LOCATE , 25: COLOR 7: PRINT " --> ";
COLOR 6: PRINT l(zz)
END IF
END SELECT
END SELECT
RETURN

siirtola:
FOR zz = 1 TO 6
zzz = CINT(500 * RND)
SELECT CASE zzz
CASE 0 TO 100
ccc = 0
CASE 101 TO 140
ccc = 1
CASE 141 TO 200
ccc = 2
CASE 201 TO 280
ccc = 3
CASE 281 TO 381
ccc = 4
CASE 381 TO 500
ccc = 5
END SELECT
IF ccc > 0 THEN
osa0:
xx = CINT(11 * RND) + sax%
IF ohj(x(xx)) <> 0 THEN GOTO osa0
osa1:
xxx = CINT(11 * RND) + sax%
IF xxx = xx OR ohj(x(xxx)) <> 0 THEN GOTO osa1
osa2:
xxxx = CINT(11 * RND) + sax%
IF xxxx = xx OR xxxx = xxx OR ohj(x(xxxx)) <> 0 THEN GOTO osa2

IF ccc = 1 THEN COLOR 15 ELSE IF ccc = 2 THEN COLOR 7 ELSE COLOR 8
PRINT top(tox(x(xx)), ccc).nam; ccc;
IF sr(x(xx)) = 1 THEN COLOR 10 ELSE COLOR 2
PRINT " "; l(x(xx));
COLOR 7: PRINT "-->";
IF sr(x(xxx)) = 1 THEN COLOR 14 ELSE COLOR 6
PRINT " "; l(x(xxx))
IF ccc = 1 THEN COLOR 15 ELSE IF ccc = 2 THEN COLOR 7 ELSE COLOR 8
PRINT top(tox(x(xxx)), ccc).nam; ; ccc;
IF sr(x(xxx)) = 1 THEN COLOR 10 ELSE COLOR 2
PRINT " "; l(x(xxx));
COLOR 7: PRINT "-->";
IF sr(x(xxx)) = 1 THEN COLOR 14 ELSE COLOR 6
PRINT " "; l(x(xxxx))
IF ccc = 1 THEN COLOR 15 ELSE IF ccc = 2 THEN COLOR 7 ELSE COLOR 8
PRINT top(tox(x(xxxx)), ccc).nam; ; ccc;
IF sr(x(xxxx)) = 1 THEN COLOR 10 ELSE COLOR 2
PRINT " "; l(x(xxxx));
COLOR 7: PRINT "-->";
IF sr(x(xx)) = 1 THEN COLOR 14 ELSE COLOR 6
PRINT " "; l(x(xx))
SWAP top(tox(x(xx)), ccc), top(tox(x(xxx)), ccc)
SWAP top(tox(x(xx)), ccc), top(tox(x(xxxx)), ccc)
END IF
NEXT zz
RETURN









END SUB

SUB pelaamaar
IF viimpel = lpl(joukpois) THEN GOTO smutero

FOR smut = viimpel + 1 TO lpl(joukpois)
pel(smut - 1, joukpois) = pel(smut, joukpois)
NEXT smut

smutero:
IF kurso = lpl(joukpois) THEN kurso = kurso - 1: oldkurso = oldkurso - 2
lpl(joukpois) = lpl(joukpois) - 1
END SUB

SUB pelhychk
avage(pv) = 0

IF lpl(pv) > 0 THEN
FOR ooo = 1 TO lpl(pv)
avage(pv) = avage(pv) + pel(ooo, pv).age
NEXT ooo
avage(pv) = avage(pv) / lpl(pv)
END IF

haikka = 0
IF maa(1, pv) = 0 THEN haikka = 1

FOR zzz = 1 TO 3
IF pk(zzz, pv).vp = 0 THEN haikka = 1
IF pk(zzz, pv).op = 0 THEN haikka = 1
NEXT zzz

FOR zzz = 1 TO 4
IF hk(zzz, pv).vh = 0 THEN haikka = 1
IF hk(zzz, pv).kh = 0 THEN haikka = 1
IF hk(zzz, pv).oh = 0 THEN haikka = 1
NEXT zzz

END SUB

SUB pelpelpel
SELECT CASE inte(pv)
CASE 0
kerr = .85
CASE 1
kerr = 1
CASE 2
kerr = 1.1
END SELECT

'IF jokeri(pv) > 5 THEN
'IF CINT(100 * RND) < jokerip(pv) THEN kerr = kerr - .35
'END IF

SELECT CASE ww

CASE nn
kerr = kerr + (mo(pv) / 125)
IF fani(pv) = 1 THEN kerr = kerr + .02
mwnn = mwnn * kerr
pwnn = pwnn * kerr
hwnn = hwnn * kerr
vast(pv) = mm: koti(pv) = 1
IF voodoo(pv) = 1 THEN
voodoo(pv) = 0
IF 100 * RND >= 50 THEN
mwmm = mwmm * .5
pwmm = pwmm * .5
hwmm = hwmm * .5
END IF
END IF
IF bantu(pv) = 1 THEN
bantu(pv) = 0
IF 100 * RND > 50 THEN
mwnn = mwnn * 1.1
pwnn = pwnn * 1.1
hwnn = hwnn * 1.1
END IF
END IF
FOR xx = 1 TO lpl(pv)
IF pel(xx, pv).spe = 10 AND pel(xx, pv).ket > 0 THEN raha(pv) = raha(pv) + 10000
IF pel(xx, pv).spe = 4 AND pel(xx, pv).ket > 0 THEN mwnn = mwnn + 25
NEXT xx

CASE mm
kerr = kerr + mo(pv) / 125
IF fani(pv) = 1 THEN kerr = kerr + .02
mwmm = mwmm * kerr
pwmm = pwmm * kerr
hwmm = hwmm * kerr
vast(pv) = nn: vieras(pv) = 1
IF voodoo(pv) = 1 THEN
voodoo(pv) = 0
IF 100 * RND >= 50 THEN
mwnn = mwnn * .5
pwnn = pwnn * .5
hwnn = hwnn * .5
END IF
END IF
IF bantu(pv) = 1 THEN
bantu(pv) = 0
IF 100 * RND > 50 THEN
mwmm = mwmm * 1.1
pwmm = pwmm * 1.1
hwmm = hwmm * 1.1
END IF
END IF
FOR xx = 1 TO lpl(pv)
IF pel(xx, pv).spe = 4 AND pel(xx, pv).ket > 0 THEN mwmm = mwmm + 25
IF pel(xx, pv).spe = 10 AND pel(xx, pv).ket > 0 THEN raha(pv) = raha(pv) + 10000
NEXT xx

END SELECT

FOR xx = 1 TO lpl(pv)
IF turnauz = 0 THEN
IF pel(xx, pv).inj > 1000 THEN pel(xx, pv).inj = pel(xx, pv).inj - 1
IF pel(xx, pv).inj = 1000 THEN pel(xx, pv).inj = 0
IF pel(xx, pv).svu > 10000 THEN pel(xx, pv).svu = pel(xx, pv).svu - 1
END IF
IF runkosarja = 1 OR mukp(pv) = 1 THEN
IF pel(xx, pv).ket > 0 THEN
pel(xx, pv).pok = pel(xx, pv).pok + 1
pel(xx, pv).pot = pel(xx, pv).pot + 1
END IF
END IF
NEXT xx

'PRINT kerr

END SUB

SUB pesc
LOCATE 43, 1
COLOR 15: PRINT "Paina ESC...";
DO
esk$ = ""
WHILE esk$ = "": esk$ = INKEY$: WEND
IF esk$ = CHR$(27) THEN EXIT DO
LOOP

END SUB

SUB piirtox
IF kr = 13 AND mukt(pv) = 0 THEN postia (1)
IF uhka(pv) = 1 THEN postia (2)

COLOR 8
LOCATE 1, 32: PRINT "ÉÍÍKAUSIÍÍ»";
LOCATE 2, 32: PRINT "º         º";
LOCATE 3, 32: PRINT "ÈÍÍÍÍÍÍÍÍÍ¼";
LOCATE 2, 33: COLOR 7: PRINT LTRIM$(STR$(kausi)); "-"; LTRIM$(STR$(kausi + 1));

LOCATE 43, 50: COLOR 15: PRINT "? "; : COLOR 7: PRINT CHR$(18); ":INTENS.  SHIFT-S:SOPUPELI";

apulaisvalm

IF runkosarja = 1 THEN apulaisvalm2

LOCATE 1, 45: COLOR 13: PRINT "Manageri  "; : colchk: PRINT nimi(pv);

LOCATE 2, 48: COLOR 13: PRINT "Saldo  ";
IF raha(pv) < 0 THEN COLOR 4 ELSE COLOR 14
PRINT LTRIM$(STR$(raha(pv)));
LOCATE 3, 44: COLOR 13: PRINT "Strategia  "; : COLOR 12: stratprint
LOCATE 4, 46: COLOR 13: PRINT "Moraali  "; : COLOR 12: moprint2
LOCATE 5, 40: COLOR 13: PRINT "Intensiteetti  "; : inteprint
LOCATE 6, 45: COLOR 13: PRINT "Sopupeli  "; : sopuprint

piirtox2
inforuta
COLOR 7
LOCATE 8, 45: PRINT "*. TULEVAT OTTELUT";

LOCATE 9, 45: PRINT "-. PHL:N PISTEP™RSSI";
LOCATE 10, 45: PRINT "+. DIVISIOONAN PISTEP™RSSI";
LOCATE 11, 45: PRINT "1. SARJATAULUKOT, PHL & DIV.";
LOCATE 12, 45: PRINT "2. SARJATAULUKOT, EHL";
LOCATE 13, 45: PRINT "3. TILASTOKESKUS  4. MENESTYSKŽYRŽ";
COLOR 10
LOCATE 15, 45: IF veikko = 1 THEN PRINT "V. VEDONLY™NTI";
IF mo(pv) <= -6 AND mo(pv) <> -66 AND kriisi = 0 THEN COLOR 30: LOCATE 16, 45: PRINT "K. KRIISIPALAVERI";
COLOR 14
LOCATE 17, 45: PRINT "P. PANKKILAITOKSET";
LOCATE 18, 45: PRINT "O. ORGANISAATIO";
IF plkm > 1 THEN LOCATE 17, 66: PRINT "T. TULONSIIRTO";
LOCATE 19, 45: PRINT "A. PELAAJARINKI";
LOCATE 20, 45: PRINT "B. KETJUKOKOONPANO";
IF ot <= 30 THEN LOCATE 21, 45: PRINT "C. PELAAJAMARKKINAT";
LOCATE 22, 45: PRINT "D. JŽŽHALLI";
LOCATE 23, 45: PRINT "E. KYKYJENETSIJŽT";
IF ot < 45 AND jaynateh(pv) < 3 THEN LOCATE 24, 45: PRINT "F. JŽYNŽT";

COLOR 2
LOCATE 26, 45: PRINT "!. TEKIJŽT   #. ASETUKSET";
IF ot < 45 AND vu = 1 THEN LOCATE 27, 45: PRINT "<. TALLENNA  %. RUUDUNSŽŽSTŽJŽ";
LOCATE 28, 45: PRINT ">. LOPETA";
LOCATE 43, 1: colchk
actionprint
IF krapu(pv) = 2 THEN krapulapiirto
END SUB

SUB piirtox2
SELECT CASE kr

CASE IS < 1
LOCATE 3, 1: COLOR 12: PRINT "KESŽTAUKO"

CASE 29
LOCATE 3, 1: COLOR 12: PRINT "JOULUTAUKO"

CASE 4, 7, 11, 15, 19, 23
LOCATE 1, 1: COLOR 5: PRINT "Seuraavat EHL:n ottelut:"
naytaottehl

CASE 36
LOCATE 1, 1: COLOR 5: PRINT "Seuraavat EHL:n ottelut:"
LOCATE 3, 2: COLOR 14: PRINT "LOPPUTURNAUS"

CASE ELSE
LOCATE 1, 1: COLOR 5: PRINT "Seuraavat PHL:n ottelut:"
LOCATE 10, 1: COLOR 5: PRINT "Seuraavat divisioonaottelut:"
naytaott

END SELECT
END SUB

SUB piirtoxvoima
DIM htarko(1 TO 4) AS INTEGER
DIM ptarko(1 TO 3) AS INTEGER
DIM mtarko(1) AS INTEGER

mw(u(pv)) = 0
pw(u(pv)) = 0
hw(u(pv)) = 0
jokeri(pv) = 0
jokerip(pv) = 0
FOR zz = 1 TO 3
pketju(zz, pv) = 0
hketju(zz, pv) = 0
NEXT zz
hketju(4, pv) = 0

IF maa(1, pv) = 0 THEN mtarko(1) = 1

FOR ddr = 1 TO 3
IF pk(ddr, pv).vp = 0 OR pk(ddr, pv).op = 0 THEN pketju(ddr, pv) = 0: ptarko(ddr) = 1
NEXT ddr

FOR ddr = 1 TO 4
IF hk(ddr, pv).vh = 0 OR hk(ddr, pv).kh = 0 OR hk(ddr, pv).oh = 0 THEN hketju(ddr, pv) = 0: htarko(ddr) = 1
NEXT ddr


FOR ddr = 1 TO 3
IF ptarko(ddr) = 0 THEN
ccc = pk(ddr, pv).vp
xxx = 2
GOSUB zzra
pketju(ddr, pv) = pketju(ddr, pv) + temp%
ccc = pk(ddr, pv).op
xxx = 2
GOSUB zzra
pketju(ddr, pv) = pketju(ddr, pv) + temp%
END IF
NEXT ddr

FOR ddr = 1 TO 4
IF htarko(ddr) = 0 THEN
ccc = hk(ddr, pv).vh
xxx = 3
GOSUB zzra
hketju(ddr, pv) = hketju(ddr, pv) + temp%
ccc = hk(ddr, pv).kh
xxx = 4
GOSUB zzra
hketju(ddr, pv) = hketju(ddr, pv) + temp%
ccc = hk(ddr, pv).oh
xxx = 5
GOSUB zzra
hketju(ddr, pv) = hketju(ddr, pv) + temp%
END IF
NEXT ddr


IF mtarko(1) = 0 THEN
ccc = maa(1, pv)
xxx = 1
GOSUB zzra
mw(u(pv)) = temp%
END IF

pw(u(pv)) = pketju(1, pv) + pketju(2, pv) + pketju(3, pv)
hw(u(pv)) = hketju(1, pv) + hketju(2, pv) + hketju(3, pv) + hketju(4, pv)

ERASE htarko, ptarko, mtarko
EXIT SUB

zzra:
temp% = 0
temp% = pel(ccc, pv).psk

IF pel(ccc, pv).psk > 12 THEN
jokeri(pv) = jokeri(pv) + 1
jokerip(pv) = jokerip(pv) + (temp% - 11)
END IF

SELECT CASE xxx
CASE 1
IF pel(ccc, pv).ppp <> 1 THEN temp% = 1
CASE 2
IF pel(ccc, pv).ppp <> 2 THEN temp% = FIX(.75 * temp%)
CASE 3, 4, 5
IF pel(ccc, pv).ppp <> xxx AND pel(ccc, pv).ppp = 2 THEN temp% = FIX(.75 * temp%)
IF pel(ccc, pv).ppp <> xxx AND pel(ccc, pv).ppp <> 2 THEN temp% = FIX(.99 * temp%)
END SELECT
IF pel(ccc, pv).spe = 8 THEN temp% = CINT(.75 * temp%)
temp% = temp% + efed(pv)
temp% = temp% + pel(ccc, pv).plus
SELECT CASE pel(ccc, pv).kun
CASE -1
temp% = FIX(.9 * temp%)
CASE -2
temp% = FIX(.75 * temp%)
CASE -3
temp% = FIX(.6 * temp%)
CASE IS < -3
temp% = FIX(.4 * temp%)
END SELECT
IF temp% < 0 THEN temp% = 0
RETURN
END SUB

SUB pisteet
IF ohj(ww) <> 0 THEN xx = ohj(ww)
IF ohj(ww) = 0 AND lm = 1 THEN GOSUB tietpisteet: EXIT SUB
IF ohj(ww) = 0 AND lm = 0 THEN EXIT SUB

IF ww = nn THEN perlos = 1
IF ww = mm THEN perlos = 2

hent1 = hketju(1, xx) ^ 2
hent2 = hketju(2, xx) ^ 2
hent3 = hketju(3, xx) ^ 2
hent4 = hketju(4, xx) ^ 2
pent1 = pketju(1, xx) ^ 2
pent2 = pketju(2, xx) ^ 2
pent3 = pketju(3, xx) ^ 2

FOR zzz = 1 TO gl(ww)
tek = 0: ass = 0
bent = CINT((hent1 + hent2 + hent3 + hent4) * RND)

SELECT CASE bent
CASE 0 TO hent1
kent = 1
CASE hent1 + 1 TO hent1 + hent2
kent = 2
CASE hent1 + hent2 + 1 TO hent1 + hent2 + hent3
kent = 3
CASE hent1 + hent2 + hent3 + 1 TO hent1 + hent2 + hent3 + hent4
kent = 4
END SELECT

bent = CINT((pent1 + pent2 + pent3) * RND)
SELECT CASE bent
CASE 0 TO pent1
pent = 1
CASE pent1 + 1 TO pent1 + pent2
pent = 2
CASE pent1 + pent2 + 1 TO pent1 + pent2 + pent3
pent = 3
END SELECT
te = CINT(100 * RND)
SELECT CASE te
CASE 0 TO 12
tek = 1
CASE 13 TO 25
tek = 2
CASE 26 TO 50
tek = 3
CASE 51 TO 75
tek = 4
CASE 76 TO 100
tek = 5
END SELECT
GOSUB tekija

IF lm = 1 THEN
az = CINT(100 * RND)
IF az < 30 THEN azz = 0 ELSE IF az < 70 THEN azz = 1 ELSE azz = 2
FOR zx = 1 TO azz
ax = CINT(100 * RND)
SELECT CASE ax
CASE 1 TO 25
IF tek > 2 AND ass = 1 THEN ass = 2
IF tek > 2 AND ass = 2 THEN ass = 1
IF ass = 1 AND tek = 2 THEN ass = CINT(2 * RND + 3)
IF ass = 2 AND tek = 1 THEN ass = CINT(2 * RND + 3)
IF tek = 1 AND ass = 0 THEN ass = 2
IF tek = 2 AND ass = 0 THEN ass = 1
IF tek > 2 AND ass = 0 THEN ass = CINT(1 * RND + 1)
IF tek > 2 AND ass > 2 THEN ass = CINT(1 * RND + 1)
CASE 26 TO 50
IF ass = 3 AND tek = 4 THEN ass = 5
IF ass = 3 AND tek = 5 THEN ass = 4
IF tek <> 3 AND ass = 0 THEN ass = 3 ELSE IF ass = 0 THEN ass = 4
CASE 51 TO 75
IF ass = 4 AND tek = 3 THEN ass = 5
IF ass = 4 AND tek = 5 THEN ass = 3
IF tek <> 4 AND ass = 0 THEN ass = 4 ELSE IF ass = 0 THEN ass = 5
CASE 76 TO 100
IF ass = 5 AND tek = 4 THEN ass = 3
IF ass = 5 AND tek = 3 THEN ass = 4
IF tek <> 5 AND ass = 0 THEN ass = 5 ELSE IF ass = 0 THEN ass = 3
END SELECT
GOSUB syottaja
NEXT zx
END IF

tekijat
NEXT zzz
EXIT SUB

tekija:
SELECT CASE tek
CASE 1
IF lm = 1 THEN pel(pk(pent, xx).vp, xx).gls = pel(pk(pent, xx).vp, xx).gls + 1
maali(zzz, perlos) = pel(pk(pent, xx).vp, xx).nam
CASE 2
IF lm = 1 THEN pel(pk(pent, xx).op, xx).gls = pel(pk(pent, xx).op, xx).gls + 1
maali(zzz, perlos) = pel(pk(pent, xx).op, xx).nam
CASE 3
IF lm = 1 THEN pel(hk(kent, xx).vh, xx).gls = pel(hk(kent, xx).vh, xx).gls + 1
maali(zzz, perlos) = pel(hk(kent, xx).vh, xx).nam
CASE 4
IF lm = 1 THEN pel(hk(kent, xx).kh, xx).gls = pel(hk(kent, xx).kh, xx).gls + 1
maali(zzz, perlos) = pel(hk(kent, xx).kh, xx).nam
CASE 5
IF lm = 1 THEN pel(hk(kent, xx).oh, xx).gls = pel(hk(kent, xx).oh, xx).gls + 1
maali(zzz, perlos) = pel(hk(kent, xx).oh, xx).nam
END SELECT
RETURN

syottaja:
SELECT CASE ass
CASE 1
pel(pk(pent, xx).vp, xx).ass = pel(pk(pent, xx).vp, xx).ass + 1
CASE 2
pel(pk(pent, xx).op, xx).ass = pel(pk(pent, xx).op, xx).ass + 1
CASE 3
pel(hk(kent, xx).vh, xx).ass = pel(hk(kent, xx).vh, xx).ass + 1
CASE 4
pel(hk(kent, xx).kh, xx).ass = pel(hk(kent, xx).kh, xx).ass + 1
CASE 5
pel(hk(kent, xx).oh, xx).ass = pel(hk(kent, xx).oh, xx).ass + 1
END SELECT
RETURN



tietpisteet:
FOR tpis = 1 TO gl(ww)
IF CINT(100 * RND) > 70 THEN
nummi = CINT(99 * RND) + 1
SELECT CASE nummi
CASE 1 TO 32
R = 1
CASE 33 TO 55
R = 2
CASE 56 TO 71
R = 3
CASE 72 TO 86
R = 4
CASE 87 TO 100
R = 5
END SELECT
top(tox(ww), R).gls = top(tox(ww), R).gls + 1
END IF

IF CINT(100 * RND) > 70 THEN
nummi = CINT(99 * RND) + 1
SELECT CASE nummi
CASE 1 TO 32
R = 1
CASE 33 TO 55
R = 2
CASE 56 TO 71
R = 3
CASE 72 TO 86
R = 4
CASE 87 TO 100
R = 5
END SELECT
top(tox(ww), R).ass = top(tox(ww), R).ass + 1
END IF
NEXT tpis
RETURN

END SUB

