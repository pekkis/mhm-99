DECLARE SUB egomaar (fat%)
DECLARE SUB annapotkut ()
DECLARE SUB cccptabl ()
DECLARE SUB cndcheck ()
DECLARE SUB colchk ()
DECLARE SUB colchk2 ()
DECLARE SUB ehllopturmaar2 ()
DECLARE SUB endseason ()
DECLARE SUB ennenkaudenalkua ()
DECLARE SUB erikoismuut ()
DECLARE SUB gameday ()
DECLARE SUB gamedayehl ()
DECLARE SUB ketjuissachk ()
DECLARE SUB kuva (tied$)
DECLARE SUB mahmax ()
DECLARE SUB klear ()
DECLARE SUB manap ()
DECLARE SUB haahaa (haha%)
DECLARE SUB keisi ()
DECLARE SUB keisi2 (fat%)
DECLARE SUB luetexti (lue$)
DECLARE SUB maarpalk (jepulis$)
DECLARE SUB ylmaar ()
DECLARE SUB kuntche ()
DECLARE SUB gamedayf ()
DECLARE SUB kuntomuut ()
DECLARE SUB joulutauko ()
DECLARE SUB gamedayqf ()
DECLARE SUB gamedaysf ()
DECLARE SUB ketlaita ()
DECLARE SUB ketjutyhjenna ()
DECLARE SUB luz (fat%)
DECLARE SUB moramaar ()
DECLARE SUB palkmaar (fat%)
DECLARE SUB naytaott ()
DECLARE SUB ottpel ()
DECLARE SUB ottul ()
DECLARE SUB otmuutpoff ()
DECLARE SUB opflisays ()
DECLARE SUB mukachk ()
DECLARE SUB orgamaar ()
DECLARE SUB nostajunnu ()
DECLARE SUB piirtoxvoima ()
DECLARE SUB qelp (qel%)
DECLARE SUB pjn ()
DECLARE SUB printpels ()
DECLARE SUB printpel (fat%)
DECLARE SUB sopimusext (fat%)
DECLARE SUB report ()
DECLARE SUB sattuma ()
DECLARE SUB playoffplajays ()
DECLARE SUB playoffplajays2 ()
DECLARE SUB playoffplajays3 ()
DECLARE SUB protesti ()
DECLARE SUB printketkehys ()
DECLARE SUB printketkai ()
DECLARE SUB printket ()
DECLARE SUB printpelket ()
DECLARE SUB poisjun ()
DECLARE SUB prm (juu%)
DECLARE SUB turnausaloita ()
DECLARE SUB turnaus ()
DECLARE SUB turnauslopeta ()
DECLARE SUB tasomuutokset ()
DECLARE SUB tasomaar ()
DECLARE SUB tautimuut ()
DECLARE SUB suunnitelma ()

'$DYNAMIC
'$INCLUDE: 'mhm99.bi'

SUB gamedayf
CLS
COLOR 13: LOCATE 1, 1: PRINT "PHL:N FINAALIOTTELUT OVAT:"
LOCATE 10, 1: PRINT "DIVISIOONAN FINAALIOTTELU ON:"

naytaott
pjn

CLS
COLOR 13: PRINT "PHL:N FINAALITULOKSET OVAT:": PRINT : kolo = 14
IF pwin(fin(1)) < 3 AND pwin(fin(2)) < 3 THEN nn = fin(a): mm = fin(b): ylmaar: ottpel: ottul: otmuutpoff
kolo = 6
IF pwin(bin(1)) < 3 AND pwin(bin(2)) < 3 THEN nn = bin(a): mm = bin(b): ylmaar: ottpel: ottul: otmuutpoff
LOCATE 10, 1: COLOR 13: PRINT "DIVISIOONAN FINAALITULOS ON:": PRINT : kolo = 12
IF pwin(din(1)) < 3 AND pwin(din(2)) < 3 THEN nn = din(a): mm = din(b): ylmaar: ottpel: ottul: otmuutpoff

pjn

opflisays

tautimuut

FOR vu = 1 TO plkm
pv = vuoro(vu)
erikoismuut
IF mukp(pv) = 1 THEN
CLS
report
sattuma
kuntche
END IF
NEXT vu

pkr = pkr + 1
cndcheck
temp% = 0: mukachk

END SUB

SUB gamedayhaara
DIM gl(1 TO 56) AS INTEGER
DIM ylm(1 TO 56) AS INTEGER
DIM ja(1 TO 56) AS INTEGER

SELECT CASE kr
CASE IS < 1
FOR pv = 1 TO plkm
FOR xx = 1 TO lpl(pv)
SELECT CASE pel(xx, pv).inj
CASE 1 TO 999, IS > 2000
pel(xx, pv).inj = pel(xx, pv).inj - 1
IF pel(xx, pv).inj = 2000 THEN pel(xx, pv).inj = 0
END SELECT
NEXT xx
NEXT pv

CASE 29
FOR pv = 1 TO plkm
IF mukt(pv) = 0 THEN
FOR xx = 1 TO lpl(pv)
IF pel(xx, pv).age < 18 THEN cunto = 18 ELSE IF pel(xx, pv).age > 33 THEN cunto = 33 ELSE cunto = pel(xx, pv).age
IF pel(xx, pv).kun < kuntomax(cunto) THEN pel(xx, pv).kun = pel(xx, pv).kun + 2
IF pel(xx, pv).inj > 0 AND pel(xx, pv).inj < 1000 AND pel(xx, pv).kun > 0 THEN pel(xx, pv).kun = 0
IF pel(xx, pv).kun > kuntomax(cunto) THEN pel(xx, pv).kun = kuntomax(cunto)
SELECT CASE pel(xx, pv).inj
CASE 1 TO 999, IS > 2000
pel(xx, pv).inj = pel(xx, pv).inj - 1
IF pel(xx, pv).inj = 2000 THEN pel(xx, pv).inj = 0
END SELECT
NEXT xx
END IF
NEXT pv

CASE 36
FOR pv = 1 TO plkm
IF muke(pv) = 0 THEN
FOR xx = 1 TO lpl(pv)
IF pel(xx, pv).age < 18 THEN cunto = 18 ELSE IF pel(xx, pv).age > 33 THEN cunto = 33 ELSE cunto = pel(xx, pv).age
IF pel(xx, pv).kun < kuntomax(cunto) THEN pel(xx, pv).kun = pel(xx, pv).kun + 2
IF pel(xx, pv).inj > 0 AND pel(xx, pv).inj < 1000 AND pel(xx, pv).kun > 0 THEN pel(xx, pv).kun = 0
IF pel(xx, pv).kun > kuntomax(cunto) THEN pel(xx, pv).kun = kuntomax(cunto)
SELECT CASE pel(xx, pv).inj
CASE 1 TO 999, IS > 2000
pel(xx, pv).inj = pel(xx, pv).inj - 1
IF pel(xx, pv).inj = 2000 THEN pel(xx, pv).inj = 0
END SELECT
NEXT xx
END IF
NEXT pv

CASE ELSE
FOR pv = 1 TO plkm
temp% = 0
FOR xx = 1 TO lpl(pv)
SELECT CASE pel(xx, pv).inj
CASE 1 TO 999, IS > 2000
pel(xx, pv).inj = pel(xx, pv).inj - 1
IF pel(xx, pv).inj = 2000 THEN pel(xx, pv).inj = 0
END SELECT
IF runkosarja = 1 OR mukp(pv) = 1 OR muke(pv) = 1 THEN
temp% = 1
IF pel(xx, pv).ket > 0 THEN
kuntomuut
END IF
END IF

IF temp% = 0 OR pel(xx, pv).ket = 0 THEN
IF pel(xx, pv).age < 18 THEN cunto = 18 ELSE IF pel(xx, pv).age > 33 THEN cunto = 33 ELSE cunto = pel(xx, pv).age
IF pel(xx, pv).kun < kuntomax(cunto) THEN pel(xx, pv).kun = pel(xx, pv).kun + 2
IF pel(xx, pv).inj > 0 AND pel(xx, pv).inj < 1000 AND pel(xx, pv).kun > 0 THEN pel(xx, pv).kun = 0
IF pel(xx, pv).kun > kuntomax(cunto) THEN pel(xx, pv).kun = kuntomax(cunto)
END IF
NEXT xx
NEXT pv

END SELECT

SELECT CASE kr

CASE IS < 1
ennenkaudenalkua

CASE 4, 7, 11, 15, 19, 23
gamedayehl

CASE 29
joulutauko

CASE 36
ehllopturmaar2

CASE ELSE
SELECT CASE ot
CASE IS < 44
gameday
CASE 44
playoffplajays
CASE 45
gamedayqf
CASE 46
playoffplajays2
CASE 47
gamedaysf
CASE 48
playoffplajays3
CASE 49
gamedayf
CASE 50
endseason
END SELECT

END SELECT
ERASE gl, ja, ylm
END SUB

SUB gamedayqf
CLS
COLOR 13: LOCATE 1, 1: PRINT "PHL:N 1/4-FINAALIOTTELUT OVAT:"
LOCATE 10, 1: PRINT "DIVISIOONAN 1/4-FINAALIOTTELUT OVAT:"
naytaott
pjn

CLS
COLOR 13: PRINT "PHL:N 1/4-FINAALITULOKSET OVAT:": PRINT : kolo = 12
IF pwin(qfin(1)) < 3 AND pwin(qfin(8)) < 3 THEN nn = qfin(a): mm = qfin(b): ylmaar: ottpel: ottul: otmuutpoff
IF pwin(qfin(2)) < 3 AND pwin(qfin(7)) < 3 THEN nn = qfin(c): mm = qfin(d): ylmaar: ottpel: ottul: otmuutpoff
IF pwin(qfin(3)) < 3 AND pwin(qfin(6)) < 3 THEN nn = qfin(E): mm = qfin(f): ylmaar: ottpel: ottul: otmuutpoff
IF pwin(qfin(4)) < 3 AND pwin(qfin(5)) < 3 THEN nn = qfin(g): mm = qfin(h): ylmaar: ottpel: ottul: otmuutpoff
LOCATE 10, 1: COLOR 13: PRINT "DIVISIOONAN 1/4-FINAALITULOKSET OVAT:": PRINT : kolo = 4
IF pwin(qdin(1)) < 3 AND pwin(qdin(6)) < 3 THEN nn = qdin(aa): mm = qdin(bb): ylmaar: ottpel: ottul: otmuutpoff
IF pwin(qdin(2)) < 3 AND pwin(qdin(5)) < 3 THEN nn = qdin(cc): mm = qdin(dd): ylmaar: ottpel: ottul: otmuutpoff
IF pwin(qdin(3)) < 3 AND pwin(qdin(4)) < 3 THEN nn = qdin(ee): mm = qdin(ff): ylmaar: ottpel: ottul: otmuutpoff
pjn

opflisays

tautimuut

FOR vu = 1 TO plkm
pv = vuoro(vu)
erikoismuut
IF mukp(pv) = 1 THEN
CLS
report
sattuma
kuntche
END IF
NEXT vu

pkr = pkr + 1
cndcheck
temp% = 0: mukachk

END SUB

SUB gamedaysf
CLS
COLOR 13: LOCATE 1, 1: PRINT "PHL:N SEMIFINAALIOTTELUT OVAT:"
LOCATE 10, 1: PRINT "DIVISIOONAN SEMIFINAALIOTTELUT OVAT:"

naytaott
pjn

CLS
COLOR 13: PRINT "PHL:N SEMIFINAALITULOKSET OVAT:": PRINT : kolo = 12
IF pwin(sfin(1)) < 3 AND pwin(sfin(4)) < 3 THEN nn = sfin(a): mm = sfin(b): ylmaar: ottpel: ottul: otmuutpoff
IF pwin(sfin(2)) < 3 AND pwin(sfin(3)) < 3 THEN nn = sfin(c): mm = sfin(d): ylmaar: ottpel: ottul: otmuutpoff
LOCATE 10, 1: COLOR 13: PRINT "DIVISIOONAN SEMIFINAALITULOKSET OVAT:": PRINT : kolo = 4
IF pwin(sdin(1)) < 3 AND pwin(sdin(4)) < 3 THEN nn = sdin(a): mm = sdin(b): ylmaar: ottpel: ottul: otmuutpoff
IF pwin(sdin(2)) < 3 AND pwin(sdin(3)) < 3 THEN nn = sdin(c): mm = sdin(d): ylmaar: ottpel: ottul: otmuutpoff

pjn
opflisays


tautimuut

FOR vu = 1 TO plkm
pv = vuoro(vu)
erikoismuut
IF mukp(pv) = 1 THEN
CLS
report
sattuma
kuntche
END IF
NEXT vu

pkr = pkr + 1
cndcheck
temp% = 0: mukachk

END SUB

SUB gene
DIM agep(1 TO 100) AS INTEGER
DIM tempp(1 TO lpl(pv)) AS pelaaja

OPEN "data\data5_2.99" FOR INPUT AS #1
FOR zz = 1 TO 100
INPUT #1, agep(zz)
NEXT zz
CLOSE #1

FOR zz = 1 TO lpl(pv)
tempp(zz) = pel(zz, pv)
NEXT zz
last% = lpl(pv)

FOR zz = 1 TO 32
pel(zz, pv) = blanko(1)
IF zz < 11 THEN jel(zz, pv) = blanko(1)
NEXT zz

perusp% = CINT(pw(ccc) / 6)
perush% = CINT(hw(ccc) / 12)

FOR xxx = 1 TO 20
GOSUB wall
GOSUB agewalthen
GOSUB wal
GOSUB wal2
GOSUB wal3
NEXT xxx

GOSUB wal4
GOSUB wal5
GOSUB wal6

FOR xxx = 21 TO 24
GOSUB wall
GOSUB agewalthen
pel(xxx, pv).ppp = xxx - 19
SELECT CASE pel(xxx, pv).ppp
CASE 2
pel(xxx, pv).psk = CINT(3 * RND) + perusp% - 4
CASE ELSE
pel(xxx, pv).psk = CINT(3 * RND) + perush% - 4
END SELECT
GOSUB wal3
NEXT xxx
lpl(pv) = xxx - 1

GOSUB etsipel
GOSUB etsipel2
orgamaar

FOR xxx = 1 TO lpl(pv)
palkmaar (1)
pel(xxx, pv).sra = palkehd
NEXT xxx

SWAP tox(ccc), tox(u(pv))
SWAP ohj(ccc), ohj(u(pv))
SWAP man(ccc), man(u(pv))
SWAP u(pv), ccc

laina(1, pv) = 0
laina(2, pv) = 0
laina(3, pv) = 0

xx = ccc
tasomuutokset
tasomaar

ERASE agep, tempp
EXIT SUB

wall:
IF CINT(100 * RND) < 70 THEN nats = 1 ELSE keisi
mahmax
pel(xxx, pv).nam = krj$ + "." + ptemp$
pel(xxx, pv).nat = nats
RETURN

wal:
SELECT CASE xxx
CASE 1 TO 2
pel(xxx, pv).ppp = 1
CASE 3 TO 8
pel(xxx, pv).ppp = 2
CASE 9 TO 12
pel(xxx, pv).ppp = 3
CASE 13 TO 16
pel(xxx, pv).ppp = 4
CASE 17 TO 20
pel(xxx, pv).ppp = 5
END SELECT

wal2:
SELECT CASE xxx
CASE 1
pel(xxx, pv).psk = mw(ccc)
CASE 2
pel(xxx, pv).psk = mw(ccc) - CINT(1 * RND) - 1
CASE 3 TO 8
pel(xxx, pv).psk = perusp%
CASE 9 TO 20
pel(xxx, pv).psk = perush%
END SELECT
RETURN

agewalthen:
agex = CINT(99 * RND) + 1
pel(xxx, pv).age = agep(agex)
RETURN

wal3:
pel(xxx, pv).svu = 1
pel(xxx, pv).neu = 0
pel(xxx, pv).lah = 0
egomaar (1)
RETURN

wal4:
kokop% = 0
kokoh% = 0
FOR z = 3 TO 8
kokop% = kokop% + pel(z, pv).psk
NEXT z
FOR z = 9 TO 20
kokoh% = kokoh% + pel(z, pv).psk
NEXT z
RETURN

wal5:
SELECT CASE kokop%
CASE IS < pw(ccc)
DO UNTIL kokop% = pw(ccc)
z = CINT(5 * RND) + 3
pel(z, pv).psk = pel(z, pv).psk + 1
GOSUB wal4
LOOP

CASE IS > pw(ccc)
DO UNTIL kokop% = pw(ccc)
z = CINT(5 * RND) + 3
pel(z, pv).psk = pel(z, pv).psk - 1
GOSUB wal4
LOOP

END SELECT

SELECT CASE kokoh%
CASE IS < hw(ccc)
DO UNTIL kokoh% = hw(ccc)
z = CINT(11 * RND) + 9
pel(z, pv).psk = pel(z, pv).psk + 1
GOSUB wal4
LOOP
CASE IS > hw(ccc)
DO UNTIL kokoh% = hw(ccc)
z = CINT(11 * RND) + 9
pel(z, pv).psk = pel(z, pv).psk - 1
GOSUB wal4
LOOP
END SELECT
RETURN

wal6:
FOR z = 1 TO 20
wal62:
c = CINT(5 * RND) + 3
cc = CINT(5 * RND) + 3
IF c = cc OR pel(cc, pv).psk = 1 THEN GOTO wal62
pel(c, pv).psk = pel(c, pv).psk + 1
pel(cc, pv).psk = pel(cc, pv).psk - 1
NEXT z
FOR z = 1 TO 20
wal63:
c = CINT(11 * RND) + 9
cc = CINT(11 * RND) + 9
IF c = cc THEN GOTO wal63
IF pel(c, pv).psk = 20 OR pel(cc, pv).psk = 1 THEN GOTO wal63
pel(c, pv).psk = pel(c, pv).psk + 1
pel(cc, pv).psk = pel(cc, pv).psk - 1
NEXT z
RETURN


etsipel:
DIM tampax(1 TO 3) AS INTEGER
ruxi% = CINT(2 * RND)
SELECT CASE ruxi%
CASE 0
tampax(1) = 3: tampax(2) = 4: tampax(3) = 5
CASE 1
tampax(1) = 4: tampax(2) = 5: tampax(3) = 3
CASE 2
tampax(1) = 5: tampax(2) = 3: tampax(3) = 4
END SELECT
FOR tttt = 1 TO 3

FOR xxx = 1 TO lpl(pv)
IF pel(xxx, pv).ppp = tampax(tttt) THEN EXIT FOR
NEXT xxx

jesboks:
FOR xx = 1 TO lpl(pv)
IF xxx <> xx THEN
IF pel(xx, pv).ppp = tampax(tttt) AND pel(xx, pv).psk > pel(xxx, pv).psk THEN xxx = xx: GOTO jesboks
END IF
NEXT xx

pel(xxx, pv).nam = top(tox(ccc), tttt).nam
pel(xxx, pv).age = top(tox(ccc), tttt).age
pel(xxx, pv).nat = top(tox(ccc), tttt).nat

NEXT tttt
ERASE tampax
RETURN

etsipel2:
FOR tttt = 1 TO 3
FOR zz = 1 TO last%
IF tempp(zz).ppp > 2 AND tempp(zz).nam <> "QQQQQQQQQQQQQ" THEN nn = zz: EXIT FOR
NEXT zz
sulkapallo:
FOR zz = 1 TO last%
IF tempp(zz).ppp > 2 AND tempp(zz).nam <> "QQQQQQQQQQQQQ" AND zz <> nn AND tempp(zz).psk > tempp(nn).psk THEN nn = zz: GOTO sulkapallo
NEXT zz

top(tox(ccc), tttt).nam = tempp(nn).nam
top(tox(ccc), tttt).age = tempp(nn).age
top(tox(ccc), tttt).nat = tempp(nn).nat
tempp(nn).nam = "QQQQQQQQQQQQQ"
NEXT tttt

FOR tttt = 4 TO 5
keisi
mahmax
top(tox(ccc), tttt).nam = krj$ + "." + ptemp$
top(tox(ccc), tttt).age = CINT(10 * RND) + 20
top(tox(ccc), tttt).nat = nats
NEXT tttt
RETURN
END SUB

SUB graph
CLS
COLOR 13
PRINT "KAUDEN KULKU, ";
colchk
manap

LOCATE 3, 1
COLOR 2
PRINT "JOUKKUE: ";
COLOR 10
PRINT RTRIM$(l(u(pv))); " (";
IF sr(u(pv)) = 1 THEN PRINT "PHL)" ELSE PRINT "DIV)"

IF ot < 1 THEN
LOCATE 5, 1
COLOR 15
PRINT "KAUSI EI OLE VIELŽ ALKANUT"
pjn
EXIT SUB
END IF

tempot = ot
IF ot > 44 THEN ot = 44

COLOR 14
FOR xxx = 12 TO 1 STEP -1
LOCATE 38 - (24 - (xxx * 2)), 1
PRINT LTRIM$(STR$(xxx));
NEXT xxx

temp% = 1
FOR xx = 1 TO ot
SELECT CASE sr(u(pv))
CASE 1
IF graf(xx, pv) < 9 THEN COLOR 10 ELSE IF graf(xx, pv) < 12 THEN COLOR 2 ELSE COLOR 4
CASE 2
IF graf(xx, pv) < 7 THEN COLOR 13 ELSE COLOR 5
END SELECT
FOR xxx = 12 TO graf(xx, pv) STEP -1
LOCATE 39 - (24 - (xxx * 2)), xx + 3
IF temp% = 1 THEN PRINT CHR$(177);  ELSE PRINT CHR$(176);
LOCATE 38 - (24 - (xxx * 2)), xx + 3
IF temp% = 1 THEN PRINT CHR$(177);  ELSE PRINT CHR$(176);
NEXT xxx
IF temp% = 1 THEN temp% = 2 ELSE temp% = 1
NEXT xx

pjn

ot = tempot
END SUB

SUB haahaa (haha%)
LOCATE 43, 1
COLOR 7
SELECT CASE haha%
CASE 1, 6, 7, 9, 10, 11, 14, 15, 16, 20
PRINT CHR$(17); CHR$(16); CHR$(18);
CASE 2, 3, 5, 13
PRINT CHR$(18);
IF haha% = 3 AND plkm > 1 THEN PRINT CHR$(17); CHR$(16);
CASE 8, 12
PRINT CHR$(17); CHR$(16);
END SELECT

SELECT CASE haha%
CASE 1, 2, 3, 4, 5, 6, 7, 10, 11, 16
COLOR 15: PRINT " ? ";
COLOR 7
CASE ELSE
PRINT " ";
END SELECT

SELECT CASE haha%
CASE 1
SELECT CASE lukka
CASE 1
PRINT "TAB:KETJUT  ";
IF lpj(pv) > 0 THEN PRINT "BACKSPACE:JUNIORIT  ";
PRINT "P:POTKUT  ";
PRINT "E:SOPIMUS  ";
IF cccp(pv) > 0 THEN PRINT "C:CCCP-TABLETTI  ";
CASE 2
PRINT "N:NOSTA KOKOONPANOON  P:POTKUT  ";
END SELECT
CASE 2
PRINT "TAB:PEL.RINKI  KETJUNAPIT  K:TYHJENNŽ  SHIFT-K:TYHJ.KAIKKI  ";
CASE 3
PRINT "O:OSTA  +/-:NOPEA VIERITYS  ";
CASE 4
PRINT "K:ILMAKUVA  ";
CASE 5
PRINT "P:PALKKAA/EROTA KYKYJENETSIJŽ  ";
CASE 6
PRINT "ENTER:SUORITA JŽYNŽ  ";
CASE 9
PRINT "ENTER:HYVŽKSY  ";
PRINT "I:ILTA-MASON ASIANTUNTIJARIVI  ";
CASE 10
IF u(pv) = kulta THEN PRINT "P:KATSO PHL-POKAALIA  ";
IF u(pv) = emestari THEN PRINT "E:KATSO EHL-PYTTYŽ  ";
CASE 12, 13, 15, 20
PRINT "ENTER:HYVŽKSY  ";
CASE 14
PRINT "ENTER:OSALLISTU TURNAUKSEEN  ";
END SELECT

SELECT CASE haha%
CASE 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 16
PRINT "ESC:POIS";
CASE 9, 14, 20
PRINT "ESC:PERUUTA";
END SELECT

END SUB

SUB inforuta
IF pirtar(1) <> 678 THEN SYSTEM
SELECT CASE ot
CASE IS < 45
FOR xx = 1 TO 12
nn = x(xx)
IF s(x(xx)) < 9 THEN COLOR 10: colchk2: LOCATE 29 + s(x(xx)), 50: PRINT l(x(xx)); p(x(xx));
IF s(x(xx)) > 8 AND s(x(xx)) < 12 THEN COLOR 2: colchk2: LOCATE 29 + s(x(xx)), 50: PRINT l(x(xx)); p(x(xx));
IF s(x(xx)) = 12 THEN COLOR 4: colchk2: LOCATE 29 + s(x(xx)), 50: PRINT l(x(xx)); p(x(xx));
NEXT xx
FOR xx = 13 TO 24
nn = x(xx)
IF s(x(xx)) < 7 THEN COLOR 13: colchk2: LOCATE 29 + s(x(xx)), 66: PRINT l(x(xx)); p(x(xx));
IF s(x(xx)) > 6 THEN COLOR 5: colchk2: LOCATE 29 + s(x(xx)), 66: PRINT l(x(xx)); p(x(xx));
NEXT xx
CASE 45, 46
COLOR 11
LOCATE 29, 58: PRINT "NELJŽNNESFINAALIEN"
LOCATE 30, 58: PRINT "    TILANTEET:    "
COLOR 12: LOCATE 32
LOCATE , 50: PRINT l(qfin(1)); "-"; l(qfin(8)); " "; pwin(qfin(1)); "-"; pwin(qfin(8))
LOCATE , 50: PRINT l(qfin(2)); "-"; l(qfin(7)); " "; pwin(qfin(2)); "-"; pwin(qfin(7))
LOCATE , 50: PRINT l(qfin(3)); "-"; l(qfin(6)); " "; pwin(qfin(3)); "-"; pwin(qfin(6))
LOCATE , 50: PRINT l(qfin(4)); "-"; l(qfin(5)); " "; pwin(qfin(4)); "-"; pwin(qfin(5))
COLOR 4
LOCATE , 50: PRINT l(qdin(1)); "-"; l(qdin(6)); " "; pwin(qdin(1)); "-"; pwin(qdin(6))
LOCATE , 50: PRINT l(qdin(2)); "-"; l(qdin(5)); " "; pwin(qdin(2)); "-"; pwin(qdin(5))
LOCATE , 50: PRINT l(qdin(3)); "-"; l(qdin(4)); " "; pwin(qdin(3)); "-"; pwin(qdin(4))
CASE 47, 48
COLOR 11
LOCATE 29, 58: PRINT "  SEMIFINAALIEN"
LOCATE 30, 58: PRINT "    TILANTEET:    "
COLOR 12: LOCATE 32
LOCATE , 50: PRINT l(sfin(1)); "-"; l(sfin(4)); " "; pwin(sfin(1)); "-"; pwin(sfin(4))
LOCATE , 50: PRINT l(sfin(2)); "-"; l(sfin(3)); " "; pwin(sfin(2)); "-"; pwin(sfin(3))
COLOR 4
LOCATE , 50: PRINT l(sdin(1)); "-"; l(sdin(4)); " "; pwin(sdin(1)); "-"; pwin(sdin(4))
LOCATE , 50: PRINT l(sdin(2)); "-"; l(sdin(3)); " "; pwin(sdin(2)); "-"; pwin(sdin(3))
CASE 49, 50
COLOR 11
LOCATE 29, 62: PRINT "FINAALIEN"
LOCATE 30, 62: PRINT "TILANTEET:"
COLOR 14: LOCATE 32
LOCATE , 50: PRINT l(fin(1)); "-"; l(fin(2)); " "; pwin(fin(1)); "-"; pwin(fin(2))
COLOR 6
LOCATE , 50: PRINT l(bin(1)); "-"; l(bin(2)); " "; pwin(bin(1)); "-"; pwin(bin(2))
COLOR 12
LOCATE , 50: PRINT l(din(1)); "-"; l(din(2)); " "; pwin(din(1)); "-"; pwin(din(2))
END SELECT
END SUB

SUB inteprint
IF kr = 29 OR kr = 36 THEN inte(pv) = 1
IF krapu(pv) <> 0 THEN inte(pv) = 0
IF pirtar(7) <> 1048 THEN inte(pv) = 0
COLOR 14: LOCATE 5, 55
PRINT intens(inte(pv));
PRINT STRING$(16 - LEN(intens(inte(pv))), " ");
IF krapu(pv) >= 1 THEN COLOR 4: LOCATE 5, 55: PRINT "LIIAN TIUKKA";
END SUB

SUB jaauniorit
PRINT valb(3, pv)

SELECT CASE valb(3, pv) - junnu(pv)


CASE 4
junnu(pv) = junnu(pv) + 2
CASE 1, 2, 3
junnu(pv) = junnu(pv) + 1
CASE -3, -2, -1
junnu(pv) = junnu(pv) - 1
CASE -4
junnu(pv) = junnu(pv) - 2
END SELECT

SELECT CASE junnu(pv)
CASE 1
hemp% = 3
CASE 2
hemp% = 4
CASE 3
hemp% = 6
CASE 4
hemp% = 7
CASE 5
hemp% = 10
END SELECT

FOR xxx = 1 TO hemp%
SELECT CASE xxx
CASE 1
min% = junnu(pv): max% = 2
CASE 2
min% = junnu(pv): max% = 3
CASE 3
min% = junnu(pv): max% = 3
CASE 4
min% = junnu(pv): max% = 4
CASE 5
min% = junnu(pv): max% = 4
CASE 6
min% = junnu(pv): max% = 5
CASE 7
min% = junnu(pv): max% = 6
CASE 8
min% = junnu(pv): max% = 6
CASE 9
min% = junnu(pv): max% = 7
CASE 10
min% = junnu(pv): max% = 9
END SELECT

nats = 1
mahmax
jel(xxx, pv).nam = krj$ + "." + ptemp$
keisi2 (2)
jel(xxx, pv).psk = CINT(max% * RND) + min%
jel(xxx, pv).age = 18
jel(xxx, pv).svu = 0
jel(xxx, pv).nat = nats
jel(xxx, pv).neu = 0
jel(xxx, pv).lah = 0
egomaar (3)
jel(xxx, pv).sra = 0

IF 100 * RND < 6 THEN jel(xxx, pv).spe = 5
IF 100 * RND < 2 AND jel(xxx, pv).spe = 0 THEN jel(xxx, pv).spe = 8
NEXT xxx

lpj(pv) = xxx - 1
END SUB

SUB jaynamaar
DIM jakys(2 TO 7) AS STRING

OPEN "data\jakys.99" FOR INPUT AS #1
FOR z = 2 TO 7
INPUT #1, jakys(z)
NEXT z
CLOSE #1

COLOR 13, 0
CLS

SELECT CASE oldkurso
CASE 1
xx = u(pv): xxx = janum(kurso)
jaynteh = 1
jaynateh(pv) = jaynateh(pv) + 1
protesti
CASE ELSE

SELECT CASE oldkurso
CASE 2
IF sr(janum(kurso)) = 2 THEN hinta = 50000 ELSE hinta = 100000
CASE 3
hinta = 150000
CASE 4
hinta = 80000
CASE 5
hinta = 75000
CASE 6
hinta = 115000
CASE 7
hinta = 100000
END SELECT

PRINT "Uhri: ";
COLOR 14: PRINT l(janum(kurso)): COLOR 13: PRINT
PRINT "Hinta: ";
COLOR 14: PRINT hinta
PRINT
IF raha(pv) < hinta THEN COLOR 12: PRINT "Sinulla ei ole tarpeeksi rahaa!": pjn: EXIT SUB
COLOR 12: PRINT jakys(oldkurso)
GOSUB jayks
IF jay$ = "e" THEN GOTO wallari666

raha(pv) = raha(pv) - hinta
jaynteh = 1
jaynateh(pv) = jaynateh(pv) + 1

SELECT CASE oldkurso
CASE 2
sopu(janum(kurso)) = 1
CASE 3
skanda(janum(kurso)) = 1
CASE 4
kouku(janum(kurso)) = 1
CASE 5
muilu(janum(kurso)) = 1
CASE 6
laxa(janum(kurso)) = 1
CASE 7
kytta(janum(kurso)) = u(pv)
END SELECT
END SELECT

wallari666:
ERASE jakys
EXIT SUB

jayks:
jay$ = ""
DO UNTIL jay$ = "k" OR jay$ = "e"
jay$ = INKEY$
LOOP
RETURN


END SUB

SUB jaynmahdprint
COLOR 9, 0
FOR rrr = 3 TO 11
LOCATE rrr, 41: PRINT "                                       "
NEXT rrr

FOR zz = 1 TO jaynmax
IF ohj(janum(kurso)) <> 0 AND zz = 2 THEN zz = 3
IF oldkurso = zz THEN COLOR , 1 ELSE COLOR , 0
LOCATE 2 + zz, 45: PRINT jayna(zz)
NEXT zz

END SUB

SUB joulutauko
turnausaloita
maarpalk ("jt1")
lusmu = 1
tnimi(1) = turnimi(6)
FOR zz = 1 TO plkm
IF mukt(zz) = 6 THEN tx(lusmu) = u(zz): lusmu = lusmu + 1
NEXT zz
FOR xx = lusmu TO 6
tx(xx) = 41 + xx
NEXT xx
IF naytajt(1) = 1 THEN naaturnaus = 1 ELSE naaturnaus = 0
turnaus
turnauslopeta

turnausaloita
maarpalk ("jt2")
lusmu = 1
tnimi(1) = turnimi(5)
FOR zz = 1 TO plkm
IF mukt(zz) = 5 THEN tx(lusmu) = u(zz): lusmu = lusmu + 1
NEXT zz

FOR xx = lusmu TO 6
tarko = 1
DO UNTIL tarko = 0
tarko = 1

IF xx = 1 THEN tarko = 0
sax = CINT(100 * RND)
SELECT CASE sax
CASE 0 TO 75
gg = CINT(11 * RND) + 13
CASE 76 TO 94
gg = CINT(5 * RND) + 42
CASE ELSE
gg = CINT(11 * RND) + 1
END SELECT
IF xx > 1 THEN
FOR xxx = 1 TO xx - 1
IF gg < 25 THEN
IF x(gg) = tx(xxx) THEN tarko = 1: EXIT FOR ELSE tarko = 0
END IF
IF gg > 24 THEN
IF gg = tx(xxx) THEN tarko = 1: EXIT FOR ELSE tarko = 0
END IF
NEXT xxx
END IF
FOR pv = 1 TO plkm
IF gg < 25 THEN
IF x(gg) = u(pv) THEN tarko = 1
END IF
NEXT pv
LOOP
IF gg < 25 THEN tx(xx) = x(gg) ELSE tx(xx) = gg
NEXT xx
IF naytajt(2) = 1 THEN naaturnaus = 1 ELSE naaturnaus = 0
turnaus
turnauslopeta

turnausaloita
maarpalk ("jt3")
lusmu = 1
tnimi(1) = turnimi(4)
FOR zz = 1 TO plkm
IF mukt(zz) = 4 THEN tx(lusmu) = u(zz): lusmu = lusmu + 1
NEXT zz

FOR xx = lusmu TO 6
tarko = 1
DO UNTIL tarko = 0
tarko = 1
IF xx = 1 THEN tarko = 0
sax = CINT(100 * RND)

gg = CINT(11 * RND) + 1

IF xx > 1 THEN
FOR xxx = 1 TO xx - 1
IF x(gg) = tx(xxx) THEN tarko = 1: EXIT FOR ELSE tarko = 0
NEXT xxx
END IF
FOR pv = 1 TO plkm
IF x(gg) = u(pv) THEN tarko = 1
NEXT pv
LOOP
tx(xx) = x(gg)
NEXT xx
IF naytajt(3) = 1 THEN naaturnaus = 1 ELSE naaturnaus = 0
turnaus
turnauslopeta

turnausaloita
maarpalk ("jt4")
lusmu = 1
tnimi(1) = turnimi(3)
FOR zz = 1 TO plkm
IF mukt(zz) = 3 THEN tx(lusmu) = u(zz): lusmu = lusmu + 1
NEXT zz

FOR xx = lusmu TO 6
tarko = 1
DO UNTIL tarko = 0
tarko = 1
IF xx = 1 THEN tarko = 0
sax = CINT(100 * RND)
SELECT CASE sax
CASE 0 TO 50
gg = CINT(11 * RND) + 1
CASE ELSE
gg = CINT(16 * RND) + 25
END SELECT

IF xx > 1 THEN
FOR xxx = 1 TO xx - 1
IF gg < 25 THEN
IF x(gg) = tx(xxx) THEN tarko = 1: EXIT FOR ELSE tarko = 0
END IF
IF gg > 24 THEN
IF gg = tx(xxx) THEN tarko = 1: EXIT FOR ELSE tarko = 0
END IF
NEXT xxx
END IF
FOR pv = 1 TO plkm
IF gg < 25 THEN
IF x(gg) = u(pv) THEN tarko = 1
END IF
NEXT pv
LOOP

IF gg < 25 THEN tx(xx) = x(gg) ELSE tx(xx) = gg
NEXT xx
IF naytajt(4) = 1 THEN naaturnaus = 1 ELSE naaturnaus = 0
turnaus
turnauslopeta

turnausaloita
maarpalk ("jt5")
lusmu = 1
tnimi(1) = turnimi(2)
FOR zz = 1 TO plkm
IF mukt(zz) = 2 THEN tx(lusmu) = u(zz): lusmu = lusmu + 1
NEXT zz

FOR xx = lusmu TO 6
tarko = 1
DO UNTIL tarko = 0
tarko = 1
IF xx = 1 THEN tarko = 0
gg = CINT(16 * RND) + 25

IF xx > 1 THEN
FOR xxx = 1 TO xx - 1
IF gg = tx(xxx) THEN tarko = 1: EXIT FOR ELSE tarko = 0
NEXT xxx
END IF
LOOP
tx(xx) = gg
NEXT xx
IF naytajt(5) = 1 THEN naaturnaus = 1 ELSE naaturnaus = 0
turnaus
turnauslopeta

turnausaloita
maarpalk ("jt6")
lusmu = 1
tnimi(1) = turnimi(1)
IF tietos = 0 THEN
FOR zz = 1 TO plkm
IF mukt(zz) = 1 THEN tx(lusmu) = u(zz): lusmu = lusmu + 1
NEXT zz
END IF
IF tietos > 0 THEN tx(lusmu) = tietos: lusmu = lusmu + 1
FOR xx = lusmu TO 6
tx(xx) = 46 + xx
NEXT xx
IF naytajt(6) = 1 THEN naaturnaus = 1 ELSE naaturnaus = 0
turnaus
turnauslopeta

tietos = 0
kr = kr + 1
ERASE turnimi

suunnitelma
END SUB

SUB katsoket

kurso = 1
lukka = 1
vaihtis% = 0
oldkurso = 1

tulkero:
CLS
haahaa (2)
LOCATE 1, 1: COLOR 13: PRINT "KETJUKOOSTUMUKSET   "; : COLOR 14: PRINT l(u(pv))
printketkehys
ketjuissachk
printketkai

LOCATE 3, 1: COLOR 7: PRINT "NIMI          PP IK TA  M+S = P  KU";

DO
printket
IF pelaa > 0 THEN
IF pel(sort2(kurso), pv).ppp = 1 THEN tempus = 0 ELSE IF pel(sort2(kurso), pv).ppp = 2 THEN tempus = 1 ELSE IF pel(sort2(kurso), pv).ppp = 3 THEN tempus = 2 ELSE IF pel(sort2(kurso), pv).ppp = 4 THEN tempus = 3 ELSE IF pel(sort2(kurso), pv).ppp = 5  _
THEN tempus = 4
IF pel(sort2(oldkurso), pv).ppp = 1 THEN oldtempus = 0 ELSE IF pel(sort2(oldkurso), pv).ppp = 2 THEN oldtempus = 1 ELSE IF pel(sort2(oldkurso), pv).ppp = 3 THEN oldtempus = 2 ELSE IF pel(sort2(oldkurso), pv).ppp = 4 THEN oldtempus = 3 ELSE IF pel( _
sort2(oldkurso), pv).ppp = 5 THEN oldtempus = 4
IF pel(sort2(oldkurso), pv).ket = 0 THEN COLOR 8: zra = 7 ELSE IF pel(sort2(oldkurso), pv).ket = 1 THEN COLOR 14: zra = 13 ELSE COLOR 4: zra = 2
LOCATE oldkurso + 4 + oldtempus, 1
xx = sort2(oldkurso): COLOR , 0: printpelket
IF pel(sort2(kurso), pv).ket = 0 THEN COLOR 8: zra = 1 ELSE IF pel(sort2(kurso), pv).ket = 1 THEN COLOR 14: zra = 13 ELSE COLOR 4: zra = 11
LOCATE kurso + 4 + tempus, 1
xx = sort2(kurso): COLOR , zra: printpelket
END IF

juu$ = ""
WHILE juu$ = "": juu$ = INKEY$: WEND
COLOR , 0
IF juu$ = "?" THEN qelp (2): GOTO tulkero
IF pelaa > 0 THEN
ketlaita
IF juu$ = CHR$(0) + CHR$(72) AND kurso > 1 THEN kurso = kurso - 1: oldkurso = kurso + 1
IF juu$ = CHR$(0) + CHR$(80) AND kurso < pelaa THEN kurso = kurso + 1: oldkurso = kurso - 1
IF juu$ = "k" THEN xx = sort2(kurso): joukpois = pv: ketjutyhjenna: ketjuissachk
IF juu$ = "K" THEN klear: ketjuissachk: printketkai
END IF
IF juu$ = CHR$(27) THEN COLOR , 0: EXIT DO
IF juu$ = CHR$(9) THEN COLOR , 0: vaihtis% = 2: EXIT DO
LOOP
END SUB

SUB katsopel
sivux = 1
lukka = 1
kurso = 1
oldkurso = 1
vaihtis% = 0

IF ot > 44 THEN sivmax = 4 ELSE sivmax = 3

dukka:
COLOR , 0
CLS
haahaa (1)

LOCATE 3, 1: COLOR 7
LOCATE , 1: PRINT "NIMI";
LOCATE , 15: PRINT "KAN";
LOCATE , 19: PRINT "PP";
LOCATE , 23: PRINT "IK";
LOCATE , 27: PRINT "TA";
LOCATE , 31: PRINT "TILA";
SELECT CASE sivux
CASE 1
LOCATE , 37: PRINT "KU";
LOCATE , 41: PRINT "POK";
LOCATE , 45: PRINT "PO";
LOCATE , 49: PRINT "M + S  = P";
CASE 2
LOCATE , 37: PRINT "SVU";
LOCATE , 41: PRINT "SRA";
LOCATE , 48: PRINT "MUUTA";
CASE 3
LOCATE , 37: PRINT "ERIK";
CASE 4
LOCATE , 37: PRINT " M + S  = P";
END SELECT
LOCATE 1, 1
SELECT CASE lukka
CASE 1
IF kurso = lpl(pv) THEN oldkurso = lpl(pv)
IF kurso = 1 THEN oldkurso = 1
IF lpl(pv) = 1 THEN kurso = 1: oldkurso = 1
pelbudj = 0
FOR xx = 1 TO lpl(pv)
IF pel(xx, pv).svu > 0 THEN
pelbudj = pelbudj + pel(xx, pv).sra + valbh(vai2(pv), 5, valb(5, pv))
END IF
NEXT xx

COLOR 13: PRINT "PEL.RINKI "; : COLOR 14: PRINT l(u(pv));
COLOR 13: PRINT "  PEL.BUDJETTI ";
COLOR 14: PRINT LTRIM$(STR$(pelbudj)); "/OTTELU";
COLOR 13: PRINT "  K-IKŽ ";
COLOR 14: PRINT USING "##.#"; avage(pv);
LOCATE , 66
COLOR 15
SELECT CASE sivux
CASE 1
PRINT "TILASTOT "; CHR$(16)
CASE 2
PRINT "SOPIMUKSET "; CHR$(17); CHR$(16)
CASE 3
PRINT "ERIKOISOM. ";
IF ot < 45 THEN PRINT CHR$(17) ELSE PRINT CHR$(17); CHR$(16)
CASE 4
PRINT "RUNKOSARJA "; CHR$(17)
END SELECT
LOCATE 4, 1
FOR xxx = 1 TO lpl(pv)
IF pel(sort(xxx), pv).ppp = 1 THEN tempus = 0 ELSE IF pel(sort(xxx), pv).ppp = 2 THEN tempus = 1 ELSE IF pel(sort(xxx), pv).ppp = 3 THEN tempus = 2 ELSE IF pel(sort(xxx), pv).ppp = 4 THEN tempus = 3 ELSE IF pel(sort(xxx), pv).ppp = 5 THEN tempus = 4
LOCATE xxx + 4 + tempus, 1
xx = sort(xxx): printpel (1)
NEXT xxx
CASE 2
IF kurso = lpj(pv) THEN oldkurso = lpj(pv)
IF kurso = 1 THEN oldkurso = 1
IF lpj(pv) = 1 THEN kurso = 1: oldkurso = 1
COLOR 13: PRINT "A-JUNIOREIDEN PARHAIMMISTO "; : COLOR 14: PRINT l(u(pv));
LOCATE 3, 1
FOR xxx = 1 TO lpj(pv)
LOCATE xxx + 4, 1
xx = xxx: printpel (3)
NEXT xxx
END SELECT

DO
SELECT CASE lukka
CASE 1
IF pel(sort(kurso), pv).ppp = 1 THEN tempus = 0 ELSE IF pel(sort(kurso), pv).ppp = 2 THEN tempus = 1 ELSE IF pel(sort(kurso), pv).ppp = 3 THEN tempus = 2 ELSE IF pel(sort(kurso), pv).ppp = 4 THEN tempus = 3 ELSE IF pel(sort(kurso), pv).ppp = 5 THEN  _
tempus = 4
IF pel(sort(oldkurso), pv).ppp = 1 THEN oldtempus = 0 ELSE IF pel(sort(oldkurso), pv).ppp = 2 THEN oldtempus = 1 ELSE IF pel(sort(oldkurso), pv).ppp = 3 THEN oldtempus = 2 ELSE IF pel(sort(oldkurso), pv).ppp = 4 THEN oldtempus = 3 ELSE IF pel(sort( _
oldkurso), pv).ppp = 5 THEN oldtempus = 4
LOCATE oldkurso + 4 + oldtempus, 1
xx = sort(oldkurso): COLOR , 0: PRINT STRING$(79, " "); : printpel (1)
LOCATE kurso + 4 + tempus, 1
xx = sort(kurso): COLOR , 13: PRINT STRING$(79, " "); : printpel (1)


CASE 2
LOCATE oldkurso + 4, 1
COLOR , 0: xx = oldkurso: printpel (3)
LOCATE kurso + 4, 1
COLOR , 13: xx = kurso: printpel (3)
END SELECT

joo$ = ""
WHILE joo$ = "": joo$ = INKEY$: WEND

IF joo$ = CHR$(27) THEN COLOR , 0: EXIT DO
IF joo$ = CHR$(9) THEN COLOR , 0: vaihtis% = 1: EXIT DO
IF joo$ = CHR$(0) + CHR$(75) AND sivux > 1 THEN sivux = sivux - 1: COLOR , 0: GOTO dukka
IF joo$ = CHR$(0) + CHR$(77) AND sivux < sivmax THEN sivux = sivux + 1: COLOR , 0: GOTO dukka
IF joo$ = "?" THEN qelp (1): GOTO dukka

SELECT CASE lukka
CASE 1
IF joo$ = CHR$(0) + CHR$(72) AND kurso > 1 THEN kurso = kurso - 1: oldkurso = kurso + 1
IF joo$ = CHR$(0) + CHR$(80) AND kurso < lpl(pv) THEN kurso = kurso + 1: oldkurso = kurso - 1
IF joo$ = "p" AND lpl(pv) > 5 AND ot < 45 THEN xx = sort(kurso): annapotkut: COLOR , 0: GOTO dukka

IF cheat(1) = 1 THEN
IF joo$ = "+" AND pel(sort(kurso), pv).psk < 20 THEN pel(sort(kurso), pv).psk = pel(sort(kurso), pv).psk + 1
IF joo$ = "-" AND pel(sort(kurso), pv).psk > 1 THEN pel(sort(kurso), pv).psk = pel(sort(kurso), pv).psk - 1
IF joo$ = "0" THEN pel(sort(kurso), pv).inj = 0
IF joo$ = "b" THEN pel(sort(kurso), pv).svu = 0: pel(sort(kurso), pv).sra = 0
END IF

IF actiox(pv) > 0 THEN
IF joo$ = "e" AND pel(sort(kurso), pv).svu = 0 THEN COLOR , 0: ote = pv: xx = sort(kurso): sopimusext (1): GOTO dukka
END IF
IF joo$ = "c" AND cccp(pv) > 0 THEN xx = sort(kurso): cccptabl: GOTO dukka
IF joo$ = "m" AND pel(sort(kurso), pv).spe = 5 THEN pel(sort(kurso), pv).spe = 666: joo$ = ""
IF joo$ = "m" AND pel(sort(kurso), pv).spe = 666 THEN pel(sort(kurso), pv).spe = 5: joo$ = ""
IF joo$ = CHR$(8) AND lpj(pv) > 0 THEN lukka = 2: kurso = 1: oldkurso = 1: GOTO dukka

CASE 2
IF joo$ = CHR$(0) + CHR$(72) AND kurso > 1 THEN kurso = kurso - 1: oldkurso = kurso + 1
IF joo$ = CHR$(0) + CHR$(80) AND kurso < lpj(pv) THEN kurso = kurso + 1: oldkurso = kurso - 1
IF joo$ = "n" AND lpl(pv) < 32 THEN nostajunnu: GOTO dukka
IF joo$ = CHR$(8) THEN lukka = 1: kurso = 1: oldkurso = 1: GOTO dukka
IF joo$ = "p" THEN poisjun: GOTO dukka
END SELECT
LOOP

END SUB

SUB kaudenalkuinho
mulkku = 1
CLS
manap
PRINT "!"
PRINT
duccax:
zz = CINT(23 * RND) + 1
IF ohj(zz) <> 0 THEN GOTO duccax

COLOR 14
PRINT mana(man(zz));
COLOR 13: PRINT " NOSTAA PROTESTIN JOUKKUETTASI VASTAAN!"
pjn
xx = zz
xxx = u(pv)
protesti

mulkku = 0
END SUB

SUB kausikorttimaar

FOR xx = 1 TO 24
ker2 = 0
IF halli(xx) = 7 THEN ker2 = 0
IF halli(xx) < 7 THEN
FOR zz = 6 TO halli(xx) STEP -1
ker2 = ker2 - .2
NEXT zz
END IF
IF halli(xx) > 7 THEN
FOR zz = 8 TO halli(xx)
ker2 = ker2 + .2
NEXT zz
END IF

IF sr(xx) = 1 THEN ker = 4 + ker2 ELSE ker = 3 + ker2

perus% = (74 - sed(xx) - sedd(xx) - seddd(xx)) * ker

IF sr(xx) = 1 THEN temp% = 0 ELSE temp% = 10
kausik(xx) = kausik(xx) + perus% + temp%
IF ohj(xx) <> 0 THEN
raha(ohj(xx)) = raha(ohj(xx)) + (CLNG(perus% + temp%) * lhinta(sr(xx)) * 22)
END IF
NEXT xx

END SUB

SUB keisi
keisix = CINT(369 * RND) + 1
SELECT CASE keisix
CASE 1 TO 200
nats = 1
CASE 201 TO 215
nats = 2
CASE 216 TO 222
nats = 3
CASE 223 TO 228
nats = 4
CASE 229 TO 244
nats = 5
CASE 244 TO 258
nats = 6
CASE 259 TO 263
nats = 7
CASE 264 TO 269
nats = 8
CASE 269 TO 284
nats = 9
CASE 285 TO 300
nats = 10
CASE 301 TO 307
nats = 11
CASE 308 TO 318
nats = 12
CASE 319 TO 370
nats = 1
END SELECT
END SUB

SUB keisi2 (fat%)
keisix = CINT(99 * RND) + 1

SELECT CASE keisix
CASE 1 TO 10
www = 1
CASE 11 TO 40
www = 2
CASE 41 TO 60
www = 3
CASE 61 TO 80
www = 4
CASE 81 TO 100
www = 5
END SELECT

SELECT CASE fat%
CASE 1
bel(xxx).ppp = www
CASE 2
jel(xxx, pv).ppp = www
END SELECT

END SUB

SUB ketjuchk
FOR zz = 1 TO lpl(pv)
IF pel(zz, pv).inj <> 0 THEN
IF maa(1, pv) = zz THEN maa(1, pv) = 0

FOR zzz = 1 TO 4
IF hk(zzz, pv).vh = zz THEN hk(zzz, pv).vh = 0
IF hk(zzz, pv).kh = zz THEN hk(zzz, pv).kh = 0
IF hk(zzz, pv).oh = zz THEN hk(zzz, pv).oh = 0
NEXT zzz

FOR zzz = 1 TO 3
IF pk(zzz, pv).vp = zz THEN pk(zzz, pv).vp = 0
IF pk(zzz, pv).op = zz THEN pk(zzz, pv).op = 0
NEXT zzz
END IF

NEXT zz
END SUB

SUB ketjuissachk
FOR xx = 1 TO lpl(pv)
keto = 0

IF maa(1, pv) = xx THEN keto = keto + 1


FOR xxx = 1 TO 3
IF pk(xxx, pv).vp = xx THEN keto = keto + 1
IF pk(xxx, pv).op = xx THEN keto = keto + 1
NEXT xxx

FOR xxx = 1 TO 4
IF hk(xxx, pv).vh = xx THEN keto = keto + 1
IF hk(xxx, pv).kh = xx THEN keto = keto + 1
IF hk(xxx, pv).oh = xx THEN keto = keto + 1
NEXT xxx

pel(xx, pv).ket = keto

IF pel(xx, pv).ket = 0 AND pel(xx, pv).spe = 666 THEN pel(xx, pv).spe = 5
NEXT xx

END SUB

SUB ketjumaar

IF maa(1, joukpois) > viimpel THEN maa(1, joukpois) = maa(1, joukpois) - 1

FOR zz = 1 TO 3
IF pk(zz, joukpois).vp > viimpel THEN pk(zz, joukpois).vp = pk(zz, joukpois).vp - 1
IF pk(zz, joukpois).op > viimpel THEN pk(zz, joukpois).op = pk(zz, joukpois).op - 1
NEXT zz

FOR zz = 1 TO 4
IF hk(zz, joukpois).vh > viimpel THEN hk(zz, joukpois).vh = hk(zz, joukpois).vh - 1
IF hk(zz, joukpois).kh > viimpel THEN hk(zz, joukpois).kh = hk(zz, joukpois).kh - 1
IF hk(zz, joukpois).oh > viimpel THEN hk(zz, joukpois).oh = hk(zz, joukpois).oh - 1
NEXT zz

END SUB

SUB ketjuswap
piirtoxvoima
DO UNTIL hketju(1, pv) >= hketju(2, pv) AND hketju(2, pv) >= hketju(3, pv) AND hketju(3, pv) >= hketju(4, pv) AND pketju(1, pv) >= pketju(2, pv) AND pketju(2, pv) >= pketju(3, pv)
IF hketju(4, pv) > hketju(3, pv) THEN SWAP hk(3, pv), hk(4, pv)
IF hketju(3, pv) > hketju(2, pv) THEN SWAP hk(2, pv), hk(3, pv)
IF hketju(2, pv) > hketju(1, pv) THEN SWAP hk(1, pv), hk(2, pv)
IF pketju(3, pv) > pketju(2, pv) THEN SWAP pk(2, pv), pk(3, pv)
IF pketju(2, pv) > pketju(1, pv) THEN SWAP pk(1, pv), pk(2, pv)
piirtoxvoima
LOOP

END SUB

SUB ketjutyhjenna

IF maa(1, joukpois) = xx THEN maa(1, joukpois) = 0

FOR zz = 1 TO 3
IF pk(zz, joukpois).op = xx THEN pk(zz, joukpois).op = 0
IF pk(zz, joukpois).vp = xx THEN pk(zz, joukpois).vp = 0
NEXT zz

FOR zz = 1 TO 4
IF hk(zz, joukpois).vh = xx THEN hk(zz, joukpois).vh = 0
IF hk(zz, joukpois).kh = xx THEN hk(zz, joukpois).kh = 0
IF hk(zz, joukpois).oh = xx THEN hk(zz, joukpois).oh = 0
NEXT zz
END SUB

SUB ketlaita
IF pel(sort2(kurso), pv).svu > 0 AND pel(sort2(kurso), pv).ket < 2 THEN
SELECT CASE juu$
CASE "-"
maa(1, pv) = sort2(kurso)

CASE "1"
IF pel(sort2(kurso), pv).ppp > 1 AND hk(1, pv).vh <> sort2(kurso) AND hk(1, pv).oh <> sort2(kurso) THEN hk(1, pv).vh = sort2(kurso)
CASE "2"
IF pel(sort2(kurso), pv).ppp > 1 AND hk(1, pv).vh <> sort2(kurso) AND hk(1, pv).oh <> sort2(kurso) THEN hk(1, pv).kh = sort2(kurso)
CASE "3"
IF pel(sort2(kurso), pv).ppp > 1 AND hk(1, pv).vh <> sort2(kurso) AND hk(1, pv).kh <> sort2(kurso) THEN hk(1, pv).oh = sort2(kurso)

CASE "q"
IF pel(sort2(kurso), pv).ppp > 1 AND hk(2, pv).kh <> sort2(kurso) AND hk(2, pv).oh <> sort2(kurso) THEN hk(2, pv).vh = sort2(kurso)
CASE "w"
IF pel(sort2(kurso), pv).ppp > 1 AND hk(2, pv).vh <> sort2(kurso) AND hk(2, pv).oh <> sort2(kurso) THEN hk(2, pv).kh = sort2(kurso)
CASE "e"
IF pel(sort2(kurso), pv).ppp > 1 AND hk(2, pv).vh <> sort2(kurso) AND hk(2, pv).kh <> sort2(kurso) THEN hk(2, pv).oh = sort2(kurso)

CASE "a"
IF pel(sort2(kurso), pv).ppp > 1 AND hk(3, pv).kh <> sort2(kurso) AND hk(3, pv).oh <> sort2(kurso) THEN hk(3, pv).vh = sort2(kurso)
CASE "s"
IF pel(sort2(kurso), pv).ppp > 1 AND hk(3, pv).vh <> sort2(kurso) AND hk(3, pv).oh <> sort2(kurso) THEN hk(3, pv).kh = sort2(kurso)
CASE "d"
IF pel(sort2(kurso), pv).ppp > 1 AND hk(3, pv).vh <> sort2(kurso) AND hk(3, pv).kh <> sort2(kurso) THEN hk(3, pv).oh = sort2(kurso)

CASE "z"
IF pel(sort2(kurso), pv).ppp > 1 AND hk(4, pv).kh <> sort2(kurso) AND hk(4, pv).oh <> sort2(kurso) THEN hk(4, pv).vh = sort2(kurso)
CASE "x"
IF pel(sort2(kurso), pv).ppp > 1 AND hk(4, pv).vh <> sort2(kurso) AND hk(4, pv).oh <> sort2(kurso) THEN hk(4, pv).kh = sort2(kurso)
CASE "c"
IF pel(sort2(kurso), pv).ppp > 1 AND hk(4, pv).vh <> sort2(kurso) AND hk(4, pv).kh <> sort2(kurso) THEN hk(4, pv).oh = sort2(kurso)

CASE "7"
IF pel(sort2(kurso), pv).ppp > 1 AND pk(1, pv).op <> sort2(kurso) THEN pk(1, pv).vp = sort2(kurso)
CASE "4"
IF pel(sort2(kurso), pv).ppp > 1 AND pk(1, pv).vp <> sort2(kurso) THEN pk(1, pv).op = sort2(kurso)
CASE "8"
IF pel(sort2(kurso), pv).ppp > 1 AND pk(2, pv).op <> sort2(kurso) THEN pk(2, pv).vp = sort2(kurso)
CASE "5"
IF pel(sort2(kurso), pv).ppp > 1 AND pk(2, pv).vp <> sort2(kurso) THEN pk(2, pv).op = sort2(kurso)
CASE "9"
IF pel(sort2(kurso), pv).ppp > 1 AND pk(3, pv).op <> sort2(kurso) THEN pk(3, pv).vp = sort2(kurso)
CASE "6"
IF pel(sort2(kurso), pv).ppp > 1 AND pk(3, pv).vp <> sort2(kurso) THEN pk(3, pv).op = sort2(kurso)
END SELECT
ketjuissachk
printket
printketkai
END IF
END SUB

SUB kiekkoliite
CLS
COLOR 15
PRINT "LŽTKŽLEHDEN ";
COLOR 13
PRINT "KUULUISA, JOKAVUOTINEN ";
COLOR 14
PRINT "KIEKKOLIITE";
COLOR 13: PRINT " ON ILMESTYNYT!"
PRINT "TŽSSŽ LEHDEN ASIANTUNTIJA-ARVIO JOUKKUEISTA EDELLISEEN KAUTEEN VERRATTUNA!"
PRINT
FOR ccc = 1 TO 24
xx = x(ccc)
IF ohj(xx) = 0 THEN
IF sr(xx) = 1 THEN COLOR 14 ELSE COLOR 6
PRINT l(xx); "   ";
SELECT CASE kiekkoli(xx)
CASE tazo(xx)
COLOR 7
PRINT "EI SUURIA MUUTOKSIA"
CASE IS < tazo(xx)
COLOR 15
PRINT "VAHVISTUNUT ";
SELECT CASE tazo(xx) - kiekkoli(xx)
CASE 1
PRINT "VŽHŽSEN"
CASE 2
PRINT "MERKITTŽVŽSTI"
CASE 3
PRINT "PALJON"
CASE ELSE
PRINT "HIRMUISESTI!"
END SELECT
CASE IS > tazo(xx)
COLOR 8
PRINT "HEIKENTYNYT ";
SELECT CASE kiekkoli(xx) - tazo(xx)
CASE 1
PRINT "VŽHŽSEN"
CASE 2
PRINT "MERKITTŽVŽSTI"
CASE 3
PRINT "PALJON"
CASE ELSE
PRINT "HIRMUISESTI!"
END SELECT
END SELECT
END IF
IF ccc = 12 THEN PRINT
NEXT ccc
pjn
ERASE kiekkoli
END SUB

SUB kieltoon
DIM pelki(1 TO 18) AS INTEGER
OPEN "data\pevvebe.99" FOR INPUT AS #1
FOR zz = 1 TO 18
INPUT #1, pelki(zz)
NEXT zz
CLOSE #1

COLOR 2: PRINT "Voi ei! ";
COLOR 10: PRINT RTRIM$(pel(xxx, pv).nam); " ("; LTRIM$(STR$(pel(xxx, pv).age)); ","; pelip(pel(xxx, pv).ppp); ","; LTRIM$(STR$(pel(xxx, pv).psk)); ")";
COLOR 2: PRINT " on pelikiellossa seuraavan ";
COLOR 10: PRINT LTRIM$(STR$(pelki(lukka)));
COLOR 2: PRINT " pelin ajan!"
luetexti (LTRIM$(STR$(lukka)) + "pk")
COLOR 10
pel(xxx, pv).inj = pelki(lukka) + 1000
ERASE pelki
END SUB

SUB kirjeluku
COLOR 13, 0
xxx = 6
xx = CINT(xxx * RND) + 1
CLS
luetexti ("uhka" + LTRIM$(STR$(xx)))
uhka(pv) = 0
pjn
END SUB

SUB klear
maa(1, pv) = 0

FOR zz = 1 TO 3
pk(zz, pv).vp = 0
pk(zz, pv).op = 0
NEXT zz

FOR zz = 1 TO 4
hk(zz, pv).vh = 0
hk(zz, pv).kh = 0
hk(zz, pv).oh = 0
NEXT zz
END SUB

SUB klearstat (fat%)

FOR zzz = fat% TO 43
LOCATE zzz, 1: PRINT STRING$(79, " ");
NEXT zzz
LOCATE fat%, 1
END SUB

SUB komme (sano)
kurso = 1
DO
COLOR , 0: LOCATE xxx, 1: PRINT STRING$(80, " ");
LOCATE xxx, 1: COLOR 14, 13
OPEN "data\komme" + LTRIM$(STR$(sano)) + ".mhm" FOR INPUT AS #1
FOR zz = 1 TO kurso
LINE INPUT #1, sano$
NEXT zz
CLOSE #1
LOCATE xxx, 1
PRINT sano$;
va$ = "": WHILE va$ = "": va$ = INKEY$: WEND
IF va$ = CHR$(0) + CHR$(72) AND kurso > 1 THEN kurso = kurso - 1
IF va$ = CHR$(0) + CHR$(80) AND kurso < 5 THEN kurso = kurso + 1
IF va$ = CHR$(13) THEN EXIT DO
LOOP
COLOR 14, 0
LOCATE xxx, 1: PRINT STRING$(80, " ");
OPEN "data\komme" + LTRIM$(STR$(sano)) + ".mhm" FOR INPUT AS #1
FOR zz = 1 TO kurso
LINE INPUT #1, sano$
NEXT zz
LOCATE xxx, 1
PRINT sano$
CLOSE #1
END SUB

SUB krapulapiirto
FOR zz = 1 TO 43
FOR zzz = 1 TO 80
krappu = CINT(11 * RND) + 1
COLOR (CINT(14 * RND) + 17)
IF krappu = 1 THEN LOCATE zz, zzz: PRINT CHR$(1);
IF krappu = 2 THEN LOCATE zz, zzz: PRINT CHR$(2);
NEXT zzz
NEXT zz
END SUB

SUB kriisipalaveri
CLS
COLOR 13
luetexti ("kriisipa")

kurso = 1
DO
IF kurso = 1 THEN COLOR 14, 13 ELSE COLOR 14, 0
LOCATE 6, 1: PRINT "KYLLŽ";
IF kurso = 2 THEN COLOR 14, 13 ELSE COLOR 14, 0
LOCATE 7, 1: PRINT "EI";
COLOR , 0: PRINT "   ";

me$ = "": WHILE me$ = "": me$ = INKEY$: WEND

IF me$ = CHR$(0) + CHR$(72) AND kurso = 2 THEN kurso = 1
IF me$ = CHR$(0) + CHR$(80) AND kurso = 1 THEN kurso = 2
IF me$ = CHR$(13) THEN EXIT DO
LOOP
IF kurso = 2 THEN EXIT SUB

LOCATE 9, 1: COLOR 2
kriisi = 1
raha(pv) = raha(pv) - 1000

onnis% = 1

IF valb(1, pv) < 3 THEN
IF valb(1, pv) = 1 THEN temp% = 20 ELSE temp% = 10
ccc = CINT(100 * RND)
IF ccc <= temp% THEN PRINT "Apulaisvalmentajasi panos: mies riehui humalassa rikkoen koko paikan ja teh-": PRINT "den kaikkien muiden olon varsin tukalaksi!": onnis% = 0
PRINT
END IF

FOR xx = 1 TO lpl(pv)
IF pel(xx, pv).ego > 9 THEN
IF CINT(100 * RND) < 10 THEN
printpels
PRINT "vietti illan piruilemalla muille syytt„en heit„ ongelmista!"
onnis% = 0
PRINT
END IF
END IF
NEXT xx

IF krapu(pv) > 0 THEN COLOR 2: PRINT "™rf! Ei maistu olut, ei...muutenkin jo p„„t„ s„rkee...": PRINT : onnis% = 0

IF cheat(2) = 1 AND onnis% = 0 THEN
PRINT
COLOR 15
PRINT "ONNEKSI ON KUSIJUKKA, HŽN PELASTAA SAUNAILLAN!"
PRINT
onnis% = 1
END IF

COLOR 6
SELECT CASE onnis%
CASE 1
luetexti ("krisis1")
mo(pv) = mo(pv) + 3
CASE 0
luetexti ("krisis2")
mo(pv) = mo(pv) - 3
moramaar
END SELECT
pjn
END SUB

SUB kuitt
SCREEN 13
kuva ("masoweb")
SCREEN 0: WIDTH 80, 25: COLOR 15, 0
CLS
PRINT "MASO SOFTWARE KIITTŽŽ. TOIVOTTAVASTI PALAAT MHM 99:N PARIIN TOISTEKIN!"
PRINT
COLOR 14
PRINT "MHM 99:N REKISTER™ITY VERSIO EI OLE SHAREWAREA - JOS PELAAT MUTTET"
PRINT "OLE REKISTER™ITYNYT, SYYLLISTYT RIKOKSEEN."
PRINT "(JA VAIKKA 99 PROSENTIN VARMUUDELLA ET JOUDU KOSKAAN VASTUUSEEN SIITŽ,"
PRINT "OLET POHJASAKKAA - SHAREWARE-PIRATISMI ON ŽŽRIMMŽISEN ALA-ARVOISTA)"
PRINT
COLOR 10
PRINT "HUOM! YHTEYSTIEDOISSAMME ON TAPAHTUNUT MUUTOS:"
PRINT
PRINT "MASOWEB: www.saunalahti.fi/forster"
PRINT
PRINT "E-MAIL: forster@saunalahti.fi"
COLOR 7: SYSTEM
END SUB

SUB kukka (sano)
COLOR 9
OPEN "data\kukka" + LTRIM$(STR$(sano)) + ".mhm" FOR INPUT AS #1
xxx = CINT(9 * RND) + 1
FOR xxxx = 1 TO xxx
LINE INPUT #1, sano$
NEXT xxxx
PRINT sano$
CLOSE #1
END SUB

SUB kuntche
CLS
temp% = 0
FOR xx = 1 TO lpl(pv)
IF pel(xx, pv).kun < -6 AND pel(xx, pv).inj = 0 THEN
IF temp% = 0 THEN
manap
PRINT "!!"
PRINT
END IF
temp% = 1
printpels
PRINT "joutuu sairaalaan - h„n on palanut fyysisesti loppuun!"
pel(xx, pv).inj = CINT(2 * RND) + 3
END IF
NEXT xx
IF temp% = 1 THEN pjn
END SUB

SUB kuntomuut
SELECT CASE inte(pv)
CASE 0
IF krapu(pv) = 0 THEN
IF pel(xx, pv).age < 18 THEN cunto = 18 ELSE IF pel(xx, pv).age > 33 THEN cunto = 33 ELSE cunto = pel(xx, pv).age
IF pel(xx, pv).kun < kuntomax(cunto) THEN pel(xx, pv).kun = pel(xx, pv).kun + 1
END IF
CASE 2
pel(xx, pv).kun = pel(xx, pv).kun - 2
END SELECT
IF pel(xx, pv).ket > 1 THEN pel(xx, pv).kun = pel(xx, pv).kun - 1
END SUB

SUB kutsuturnaus
SELECT CASE sed(u(pv))
CASE 1
lusmu = 1
CASE 2 TO 4
lusmu = 2
CASE 5 TO 8
lusmu = 3
CASE 9 TO 15
lusmu = 4
CASE 16 TO 20
lusmu = 5
CASE 21 TO 24
lusmu = 6
END SELECT

kurso = lusmu
CLS
COLOR 13: PRINT "KUTSU SEURAAVIIN TURNAUKSIIN ON SAAPUNUT, MANAGERI ";
colchk
PRINT nimi(pv)
LOCATE 16, 1: COLOR 13: PRINT "TURNAUKSEN KUVAUS:";

haahaa (14)

DO
FOR zz = lusmu TO 6
IF zz = kurso THEN COLOR 14, 13 ELSE COLOR 14, 0
LOCATE 1 + zz * 2, 1: PRINT turnimi(zz)
NEXT zz
COLOR 12, 0
FOR zz = 1 TO 6
LOCATE zz + 16: PRINT STRING$(79, " ")
NEXT zz
LOCATE 17, 1: luetexti ("tourna" + LTRIM$(STR$(kurso)))
ku$ = "": WHILE ku$ = "": ku$ = INKEY$: WEND
IF ku$ = CHR$(0) + CHR$(72) AND kurso > lusmu THEN kurso = kurso - 1
IF ku$ = CHR$(0) + CHR$(80) AND kurso < 6 THEN kurso = kurso + 1
IF ku$ = CHR$(13) THEN mukt(pv) = kurso: EXIT DO
IF ku$ = CHR$(27) THEN mukt(pv) = 0: EXIT DO
LOOP
END SUB

SUB kuva (tied$)
OUT &H3C8, 0
FOR hukka = 0 TO 767
OUT &H3C9, 0
NEXT hukka
OPEN "pics\" + tied$ + ".pal" FOR INPUT AS #1
DIM vari%(767)
FOR pekkis = 0 TO 767
INPUT #1, vari%(pekkis)
NEXT pekkis
CLOSE

BLOAD "pics\" + tied$ + ".kuv"
OUT &H3C8, 0
FOR vittu = 0 TO 767
OUT &H3C9, vari%(vittu)
NEXT vittu
WHILE INKEY$ = "": WEND
END SUB

SUB landa (sano)
COLOR 10
OPEN "data\landa" + LTRIM$(STR$(sano)) + ".mhm" FOR INPUT AS #1
xxx = CINT(5 * RND) + 1
FOR xxxx = 1 TO xxx
LINE INPUT #1, sano$
NEXT xxxx
PRINT sano$
CLOSE #1
END SUB

SUB laxati
prm (2)
luz (53)
laxa(xx) = 2
END SUB

SUB lentti (sano)
COLOR 2
OPEN "data\lentti" + LTRIM$(STR$(sano)) + ".mhm" FOR INPUT AS #1
xxx = CINT(5 * RND) + 1
FOR xxxx = 1 TO xxx
LINE INPUT #1, sano$
NEXT xxxx
PRINT sano$
CLOSE #1
END SUB

SUB lmmaar
SELECT CASE kr
CASE 4, 7, 11, 15, 19, 23, 29, 36
lm = 0
CASE ELSE
lm = 1
END SELECT
END SUB

SUB loukkaan
DIM loukka(1 TO 45, 1 TO 5) AS INTEGER
OPEN "data\injuries.99" FOR INPUT AS #1
FOR zz = 1 TO 45
INPUT #1, loukka(zz, 1), loukka(zz, 2), loukka(zz, 3), loukka(zz, 4), loukka(zz, 5)
NEXT zz
CLOSE #1
COLOR 2: PRINT "Auts! ";
COLOR 10: PRINT RTRIM$(pel(xxx, pv).nam); " ("; LTRIM$(STR$(pel(xxx, pv).age)); ","; pelip(pel(xxx, pv).ppp); ","; LTRIM$(STR$(pel(xxx, pv).psk)); ")";
COLOR 2: PRINT " on poissa vahvuudesta ";
IF loukka(lukka, valb(4, pv)) <> -1 THEN
COLOR 10: PRINT LTRIM$(STR$(loukka(lukka, valb(4, pv))));
COLOR 2: PRINT " kierroksen ajan!"
END IF
IF loukka(lukka, valb(4, pv)) = -1 THEN
COLOR 10: PRINT "koko loppukauden!": COLOR 2
END IF
luetexti (LTRIM$(STR$(lukka)) + "i")
COLOR 10
pel(xxx, pv).inj = loukka(lukka, valb(4, pv))
ERASE loukka
END SUB

SUB luetexti (lue$)
OPEN "data\" + lue$ + ".mhm" FOR INPUT AS #1
DO UNTIL EOF(1)
LINE INPUT #1, line$
IF LEFT$(line$, 1) = "Ï" THEN
WHILE INKEY$ = "": WEND
CLS
GOTO loopero
END IF
PRINT line$
loopero:
LOOP
CLOSE #1
END SUB

SUB lux (fat%)
COLOR 2
luetexti (LTRIM$(STR$(fat%)) + "e")
PRINT
END SUB

SUB luz (fat%)
COLOR 2
luetexti (LTRIM$(STR$(fat%)) + "n")
PRINT
END SUB

SUB maaliero
IF (gf(taul1) - ga(taul1)) > (gf(taul2) - ga(taul2)) THEN s(taul2) = s(taul2) + 1
IF (gf(taul1) - ga(taul1)) < (gf(taul2) - ga(taul2)) THEN s(taul1) = s(taul1) + 1
IF (gf(taul1) - ga(taul1)) = (gf(taul2) - ga(taul2)) THEN GOTO huke
EXIT SUB

huke:
IF gf(taul1) >= gf(taul2) THEN s(taul2) = s(taul2) + 1
IF gf(taul1) < gf(taul2) THEN s(taul1) = s(taul1) + 1
END SUB

SUB maalieroehl
IF ero(taul1) >= ero(taul2) THEN es(taul2) = es(taul2) + 1 ELSE es(taul1) = es(taul1) + 1
END SUB

SUB maalierotur
IF tero(taul1) >= tero(taul2) THEN ts(taul2) = ts(taul2) + 1 ELSE ts(taul1) = ts(taul1) + 1
END SUB

SUB maarpalk (jepulis$)

DIM palki(1 TO 6) AS SINGLE

PRINT jepulis$
OPEN "data\" + jepulis$ + ".plk" FOR INPUT AS #1
FOR z = 1 TO 6
INPUT #1, palki(z)
NEXT z
CLOSE #1

END SUB

SUB mahmax
nam = CINT(genre(nats) * RND) + 1
OPEN "data\" + LTRIM$(STR$(nats)) + ".mhx" FOR INPUT AS #1
FOR zz = 1 TO nam
INPUT #1, ptemp$
NEXT zz
CLOSE #1
IF nats = 1 THEN krj = CINT(15 * RND) + 1
IF nats > 1 THEN krj = CINT(22 * RND) + 1
krj$ = krjn(krj)
END SUB

SUB managerisiirrot
CLS : COLOR 13
PRINT "UUTISIA MANAGERIMARKKINOILTA!"
PRINT
FOR xx = 1 TO 24
IF ohj(xx) = 0 THEN
zz = CINT(100 * RND)
IF zz < 15 THEN
COLOR 15: PRINT l(xx); " ";
COLOR 10: PRINT mana(man(xx));
COLOR 7: PRINT " --> ";
xxx = CINT((5 + plkm) * RND) + (25 - plkm)
SWAP mana(man(xx)), mana(xxx)
COLOR 14: PRINT mana(man(xx))
PRINT
END IF
END IF
NEXT xx
pjn
END SUB

SUB manap
COLOR 13
PRINT "MANAGERI ";
colchk
PRINT nimi(pv);
COLOR 13
END SUB

SUB vuoromaar
xxx = 1
SELECT CASE nokka
CASE 1
FOR xx = 24 TO 1 STEP -1
IF ohj(x(xx)) <> 0 THEN vuoro(xxx) = ohj(x(xx)): xxx = xxx + 1
NEXT xx
CASE 2
FOR xx = 1 TO 24
IF ohj(x(xx)) <> 0 THEN vuoro(xxx) = ohj(x(xx)): xxx = xxx + 1
NEXT xx
END SELECT
END SUB

SUB ylmaar
stara% = 0

SELECT CASE runkosarja
CASE 1
perus% = CINT(kausik(nn) * 1.5)

CASE 0
perus% = CINT(ylek(nn) / 22)

IF sr(nn) = 1 THEN temp% = 1
IF sr(nn) = 2 AND sedd(nn) > 12 THEN temp% = 1
IF sr(nn) = 2 AND sedd(nn) < 13 THEN temp% = 2

SELECT CASE temp%

CASE 1
SELECT CASE ot
CASE 45
perus% = perus% * 1.3
CASE 47
perus% = perus% * 1.6
CASE 49
perus% = perus% * 1.9
END SELECT
CASE 2
SELECT CASE ot
CASE 45
perus% = perus% * .75
CASE 47
perus% = perus% * .9
CASE 49
perus% = perus% * 1.05
END SELECT
END SELECT
joo = 1 + (.1 * RND) - .05
ylm(nn) = perus% * joo
GOTO jucero
END SELECT

SELECT CASE ohj(nn)
CASE IS <> 0
IF kuume(ohj(nn)) = 1 THEN perus% = perus% * 1.5
FOR xxx = 1 TO lpl(ohj(nn))
IF pel(xxx, ohj(nn)).psk > 14 AND pel(xxx, ohj(nn)).ket > 0 THEN stara% = stara% + 1
IF stara% = 3 THEN EXIT FOR
NEXT xxx
CASE 0
IF tazo(nn) > 38 THEN stara% = 3 ELSE IF tazo(nn) > 36 THEN stara% = 2 ELSE IF tazo(nn) > 29 THEN stara% = 1 ELSE stara% = 0
END SELECT

IF ot < 3 THEN
temp1% = sed(nn) - ((sr(nn) - 1) * 12)
temp2% = sed(mm) - ((sr(mm) - 1) * 12)
END IF

IF ot >= 3 THEN
temp1% = s(nn)
temp2% = s(mm)
END IF

joo = 1

SELECT CASE temp1%
CASE IS < 6
joo = joo + ((6 - temp1%) * .04)
CASE IS > 7
joo = joo - ((temp1% - 7) * .08)
END SELECT

SELECT CASE temp2%
CASE IS < 6
joo = joo + ((6 - temp2%) * .05)
CASE IS > 7
joo = joo - ((temp2% - 7) * .06)
END SELECT

IF stara% = 1 THEN joo = joo + .02
IF stara% = 2 THEN joo = joo + .02
IF stara% = 3 THEN joo = joo + .02

joo = joo + (.2 * RND) - .1
ylm(nn) = FIX(kausik(nn) + (perus% * joo))


jucero:
IF pirtar(7) <> 1048 THEN ylm(nn) = 0
IF ylm(nn) < 0 THEN ylm(nn) = 0
IF ylm(nn) > halliip(halli(nn)) + hallisp(halli(nn)) THEN ylm(nn) = halliip(halli(nn)) + hallisp(halli(nn))
END SUB

SUB ylmaarehl

joo = 1

SELECT CASE nn
CASE IS <= 24
perus% = CINT(.75 * (ylek(nn) / kotiot(nn)))
CASE ELSE
perus% = CINT(.75 * (halliip(halli(nn)) + hallisp(halli(nn))))
END SELECT
perus% = CINT(perus% * (1 + (.1 * RND - .05)))

perus% = CINT(perus% * joo)

ylm(nn) = perus%
END SUB

