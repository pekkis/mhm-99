DECLARE SUB aaaargh ()
DECLARE SUB egomaar (fat%)
DECLARE SUB annarahaa (slok%)
DECLARE SUB arpo ()
DECLARE SUB arvoloukkaa (fat%)
DECLARE SUB cndcheck ()
DECLARE SUB colchk ()
DECLARE SUB colchk4 ()
DECLARE SUB erikoisp (fuk%)
DECLARE SUB mahmax ()
DECLARE SUB manap ()
DECLARE SUB haahaa (haha%)
DECLARE SUB krapulapiirto ()
DECLARE SUB luetexti (lue$)
DECLARE SUB ketjutyhjenna ()
DECLARE SUB ketjumaar ()
DECLARE SUB kieltoon ()
DECLARE SUB klearstat (fat%)
DECLARE SUB luz (fat%)
DECLARE SUB loukkaan ()
DECLARE SUB lux (fat%)
DECLARE SUB palkmaar (fat%)
DECLARE SUB mukachk ()
DECLARE SUB moodiin ()
DECLARE SUB mukanollaus ()
DECLARE SUB nollaux ()
DECLARE SUB nyrkkeily ()
DECLARE SUB pelaamaar ()
DECLARE SUB pesc ()
DECLARE SUB sortmaar ()
DECLARE SUB sortplayers ()
DECLARE SUB qelp (qel%)
DECLARE SUB pjn ()
DECLARE SUB printpels ()
DECLARE SUB protesti ()
DECLARE SUB printpelket ()
DECLARE SUB prm (juu%)
DECLARE SUB porsmaar ()
DECLARE SUB potk ()
DECLARE SUB pottiin (summa!)
DECLARE SUB printland ()
DECLARE SUB ripuli ()
DECLARE SUB skandalpl ()
DECLARE SUB sopkilpailu ()
DECLARE SUB staulmaar ()
DECLARE SUB topmatch ()
DECLARE SUB vieroitus ()

'$DYNAMIC
'$INCLUDE: 'mhm99.bi'

SUB pisteporssi (orja)

CLS
COLOR 13
IF orja = 1 THEN PRINT "PHL:N PISTEP™RSSI" ELSE PRINT "DIVISIOONAN PISTEP™RSSI"
PRINT
FOR xxx = 1 TO 20
IF ppors(orja, xxx).pi = 0 THEN COLOR 15: PRINT "EI VIELŽ PISTEITŽ!": pjn: EXIT SUB
COLOR 14: PRINT LTRIM$(STR$(xxx)); ". ";
IF xxx < 10 THEN PRINT " ";
COLOR 10
IF ohj(ppors(orja, xxx).jo) <> 0 THEN ccc = ohj(ppors(orja, xxx).jo): colchk4
PRINT ppors(orja, xxx).na; "  ";
PRINT "("; kansal(ppors(orja, xxx).ka); ")"; "  ";
COLOR 2
IF ohj(ppors(orja, xxx).jo) <> 0 THEN ccc = ohj(ppors(orja, xxx).jo): colchk4
PRINT l(ppors(orja, xxx).jo); "  ";

COLOR 12
IF ppors(orja, xxx).ma < 10 THEN PRINT " ";
PRINT LTRIM$(STR$(ppors(orja, xxx).ma));
PRINT "  +  ";
PRINT LTRIM$(STR$(ppors(orja, xxx).sy));
IF ppors(orja, xxx).sy < 10 THEN PRINT " ";
PRINT "  =  ";
COLOR 15
PRINT LTRIM$(STR$(ppors(orja, xxx).pi))
NEXT xxx

pjn
END SUB

SUB pisteporssimaar
ERASE ppors

DIM ppors(1 TO 2, 1 TO 20) AS borzzi

IF ot > 0 AND ot < 45 OR ot > 45 OR ot = 45 AND pkr > 1 THEN

FOR xx = plkm TO 1 STEP -1
GOSUB peltaul
NEXT xx

FOR xx = 24 TO 1 STEP -1
IF ohj(x(xx)) = 0 THEN
GOSUB peltaul2
END IF
NEXT xx
EXIT SUB

peltaul2:
FOR xxx = 5 TO 1 STEP -1
FOR xxxx = 1 TO 19
IF top(tox(x(xx)), xxx).gls + top(tox(x(xx)), xxx).ass >= ppors(sr(x(xx)), xxxx).pi THEN
FOR xxxxx = 19 TO xxxx STEP -1
ppors(sr(x(xx)), xxxxx + 1) = ppors(sr(x(xx)), xxxxx)
NEXT xxxxx
ppors(sr(x(xx)), xxxx).na = top(tox(x(xx)), xxx).nam
ppors(sr(x(xx)), xxxx).ma = top(tox(x(xx)), xxx).gls
ppors(sr(x(xx)), xxxx).sy = top(tox(x(xx)), xxx).ass
ppors(sr(x(xx)), xxxx).pi = top(tox(x(xx)), xxx).gls + top(tox(x(xx)), xxx).ass
ppors(sr(x(xx)), xxxx).ka = top(tox(x(xx)), xxx).nat
ppors(sr(x(xx)), xxxx).jo = x(xx)
EXIT FOR
END IF
NEXT xxxx
NEXT xxx
RETURN

peltaul:
FOR xxx = lpl(xx) TO 1 STEP -1
IF pel(xxx, xx).ppp <> 1 THEN
FOR xxxx = 1 TO 19
IF pel(xxx, xx).gls + pel(xxx, xx).ass >= ppors(sr(u(xx)), xxxx).pi THEN
FOR xxxxx = 19 TO xxxx STEP -1
ppors(sr(u(xx)), xxxxx + 1) = ppors(sr(u(xx)), xxxxx)
NEXT xxxxx
ppors(sr(u(xx)), xxxx).na = pel(xxx, xx).nam
ppors(sr(u(xx)), xxxx).ma = pel(xxx, xx).gls
ppors(sr(u(xx)), xxxx).sy = pel(xxx, xx).ass
ppors(sr(u(xx)), xxxx).pi = pel(xxx, xx).gls + pel(xxx, xx).ass
ppors(sr(u(xx)), xxxx).ka = pel(xxx, xx).nat
ppors(sr(u(xx)), xxxx).jo = u(xx)
EXIT FOR
END IF
NEXT xxxx
END IF
NEXT xxx
RETURN

END IF
END SUB

SUB pjn
LOCATE 43, 1
COLOR 14: PRINT "Paina jotakin n„pp„int„...";
WHILE INKEY$ = "": WEND
END SUB

SUB playoffplajays
DIM qfin(8) AS INTEGER, qdin(6) AS INTEGER, sfin(4) AS INTEGER, sdin(4) AS INTEGER, fin(2) AS INTEGER, din(2) AS INTEGER
DIM bin(2) AS INTEGER
DIM pwin(24) AS INTEGER
DIM rug(1 TO 32, 1 TO plkm) AS INTEGER
DIM rua(1 TO 32, 1 TO plkm) AS INTEGER
DIM pvoittaja(4) AS borzzi

etu1 = 1: etu2 = .92
mukanollaus
FOR zz = 1 TO 2
pvoittaja(zz) = ppors(zz, 1)
NEXT zz

FOR xx = 1 TO plkm
FOR xxx = 1 TO lpl(xx)
rug(xxx, xx) = pel(xxx, xx).gls
rua(xxx, xx) = pel(xxx, xx).ass
NEXT xxx
NEXT xx
nollaux
cnd = 0
pkr = 1
kulta = 0
hopea = 0
pronssi = 0
nousu = 0
puto = 0
FOR xx = 1 TO 24
pwin(xx) = 0
seddd(xx) = sedd(xx)
sedd(xx) = sed(xx)
sed(xx) = 0
NEXT xx
FOR ooo = 1 TO 12
IF s(x(ooo)) < 9 THEN qfin(s(x(ooo))) = x(ooo)
NEXT ooo
FOR ooo = 13 TO 24
IF s(x(ooo)) < 7 THEN qdin(s(x(ooo))) = x(ooo)
NEXT ooo

FOR xx = 1 TO 12
IF s(x(xx)) = 1 THEN
IF p(x(xx)) > pisenp(1) THEN
pisenp(1) = p(x(xx))
pisenj(1) = x(xx)
END IF
END IF
NEXT xx

FOR xx = 13 TO 24
IF s(x(xx)) = 1 THEN
IF p(x(xx)) > pisenp(2) THEN
pisenp(2) = p(x(xx))
pisenj(2) = x(xx)
END IF
END IF
NEXT xx



FOR xx = 1 TO plkm
IF sr(u(xx)) = 1 AND s(u(xx)) < 9 THEN mukp(xx) = 1
IF sr(u(xx)) = 2 AND s(u(xx)) < 7 THEN mukp(xx) = 1
NEXT xx

FOR xx = 1 TO 12
IF s(x(xx)) > 8 THEN sed(x(xx)) = s(x(xx))
NEXT xx
FOR xx = 13 TO 24
IF s(x(xx)) > 6 THEN sed(x(xx)) = s(x(xx)) + 12
NEXT xx

CLS
COLOR 14: PRINT "JA NELJŽNNESFINAALEISSA OTTELUPARIT OVAT SEURAAVAT:"
PRINT
COLOR 12
PRINT l(qfin(1)); " - "; l(qfin(8))
PRINT l(qfin(2)); " - "; l(qfin(7))
PRINT l(qfin(3)); " - "; l(qfin(6))
PRINT l(qfin(4)); " - "; l(qfin(5))
COLOR 4
PRINT l(qdin(1)); " - "; l(qdin(6))
PRINT l(qdin(2)); " - "; l(qdin(5))
PRINT l(qdin(3)); " - "; l(qdin(4))
PRINT
FOR pv = 1 TO plkm
IF s(u(pv)) < 9 AND sr(u(pv)) = 1 THEN annarahaa (8)
IF sr(u(pv)) = 2 AND s(u(pv)) < 7 THEN annarahaa (8)
NEXT pv

pesc
ot = ot + 1

END SUB

SUB playoffplajays2
mukanollaus
xxx = 1
FOR xx = 1 TO 8
IF pwin(qfin(xx)) = 3 THEN sfin(xxx) = qfin(xx): xxx = xxx + 1
NEXT xx
xxx = 2
FOR xx = 1 TO 6
IF pwin(qdin(xx)) = 3 THEN sdin(xxx) = qdin(xx): xxx = xxx + 1
NEXT xx

FOR xx = 1 TO 12
IF s(x(xx)) = 12 AND sr(x(xx)) = 1 THEN sdin(1) = x(xx): sr(x(xx)) = 2
NEXT xx

xxx = 5
FOR xx = 1 TO 8
IF pwin(qfin(xx)) < 3 THEN sed(qfin(xx)) = xxx: xxx = xxx + 1
NEXT xx
xxx = 16
FOR xx = 1 TO 6
IF pwin(qdin(xx)) < 3 THEN sed(qdin(xx)) = xxx: xxx = xxx + 1
NEXT xx

FOR xxx = 1 TO 4
IF ohj(sfin(xxx)) <> 0 THEN mukp(ohj(sfin(xxx))) = 1
IF ohj(sdin(xxx)) <> 0 THEN mukp(ohj(sdin(xxx))) = 1
NEXT xxx



CLS
COLOR 14: PRINT "JA SEMIFINAALEISSA OTTELUPARIT OVAT SEURAAVAT:"
PRINT
COLOR 12
PRINT l(sfin(1)); " - "; l(sfin(4))
PRINT l(sfin(2)); " - "; l(sfin(3))
COLOR 4
PRINT l(sdin(1)); " - "; l(sdin(4))
PRINT l(sdin(2)); " - "; l(sdin(3))
PRINT

pesc
ot = ot + 1
pkr = 1: cnd = 0
FOR ooo = 1 TO 24
pwin(ooo) = 0
NEXT ooo
END SUB

SUB playoffplajays3
mukanollaus
xxx = 1
FOR xx = 1 TO 4
IF pwin(sfin(xx)) = 3 THEN fin(xxx) = sfin(xx): xxx = xxx + 1
NEXT xx
xxx = 1
FOR xx = 1 TO 4
IF pwin(sfin(xx)) < 3 THEN bin(xxx) = sfin(xx): xxx = xxx + 1
NEXT xx
xxx = 1
FOR xx = 1 TO 4
IF pwin(sdin(xx)) = 3 THEN din(xxx) = sdin(xx): xxx = xxx + 1
NEXT xx

xxx = 14
FOR xx = 1 TO 4
IF pwin(sdin(xx)) < 3 THEN sed(sdin(xx)) = xxx: xxx = xxx + 1
NEXT xx

FOR xxx = 1 TO 2
IF ohj(fin(xxx)) <> 0 THEN mukp(ohj(fin(xxx))) = 1
IF ohj(bin(xxx)) <> 0 THEN mukp(ohj(bin(xxx))) = 1
IF ohj(din(xxx)) <> 0 THEN mukp(ohj(din(xxx))) = 1
NEXT xxx

CLS
COLOR 13: PRINT "JA FINAALEISSA OTTELUPARIT OVAT SEURAAVAT:"
PRINT
COLOR 14
PRINT l(fin(1)); " - "; l(fin(2))
COLOR 6
PRINT l(bin(1)); " - "; l(bin(2))
COLOR 12
PRINT l(din(1)); " - "; l(din(2))
PRINT
pesc
ot = ot + 1
pkr = 1: cnd = 0
FOR ooo = 1 TO 24
pwin(ooo) = 0
NEXT ooo

END SUB

SUB poisjun

IF lastbors < 300 THEN
bel(lastbors + 1) = jel(kurso, pv)
lastbors = lastbors + 1
END IF
IF kurso < lpj(pv) THEN
FOR smut = kurso TO lpj(pv) - 1
jel(smut, pv) = jel(smut + 1, pv)
NEXT smut
END IF
lpj(pv) = lpj(pv) - 1
kurso = 1
IF lpj(pv) = 0 THEN lukka = 1
END SUB

SUB poisne
temp% = 0

poskeen:
FOR xx = 1 TO lpl(pv)
IF pel(xx, pv).svu = 0 THEN
IF temp% = 0 THEN
GOSUB huuskonen
END IF
temp% = 1
printpels
PRINT "evakuoituu sopimuksettomana p”rssin puolelle!"
GOSUB poisxe
GOTO poskeen
END IF
NEXT xx
PRINT
poskeen2:
FOR xx = 1 TO lpj(pv)
IF temp% = 0 THEN GOSUB huuskonen
temp% = 1
COLOR 10
PRINT RTRIM$(jel(xx, pv).nam); " ";
COLOR 2: PRINT "l„htee etsim„„n n„yt”npaikkaa maailmalta!"
IF lastbors < 300 THEN
bel(lastbors + 1) = jel(xx, pv)
bel(lastbors + 1).neu = 1
lastbors = lastbors + 1
END IF
NEXT xx
lpj(pv) = 0
IF temp% = 1 THEN pjn
aaaargh

FOR xx = lpl(pv) + 1 TO 32
pel(xx, pv) = blanko(1)
NEXT xx
FOR xx = 1 TO 10
jel(xx, pv) = blanko(1)
NEXT xx

EXIT SUB

poisxe:
IF lastbors < 300 THEN
bel(lastbors + 1) = pel(xx, pv)
bel(lastbors + 1).pok = 0
bel(lastbors + 1).neu = 0
lastbors = lastbors + 1
END IF
joukpois = pv
viimpel = xx
ketjutyhjenna
ketjumaar
pelaamaar
RETURN

huuskonen:
CLS
manap
PRINT "!!"
PRINT
RETURN

END SUB

SUB poispel
joukpois = pv
viimpel = xx
ketjutyhjenna
ketjumaar
pelaamaar
sortmaar
sortplayers
END SUB

SUB porsmaar
FOR smut = sortb(xx) + 1 TO lastbors
bel(smut - 1) = bel(smut)
NEXT smut
END SUB

SUB postia (posti%)
COLOR 4
LOCATE 39, 1: PRINT "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍPOSTIA!ÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»";
LOCATE 40, 1: PRINT "º                                   º";
LOCATE 41, 1: PRINT "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼";

LOCATE 40, 2: COLOR 12: PRINT "L: ";

SELECT CASE posti%

CASE 1
PRINT "TURNAUSKUTSU"
CASE 2
PRINT "RUSKEA KIRJEKUORI"
END SELECT
END SUB

SUB potk
COLOR 15: PRINT RTRIM$(l(xx));
COLOR 8: PRINT " on p„„tynyt palaverissa seuraavaanlaiseen ratkaisuun:"
COLOR 7: PRINT mana(man(xx));
COLOR 8: PRINT " saa l„hte„, h„nen tilalleen saapuu ";

xxx = CINT((5 + plkm) * RND) + (25 - plkm)
SWAP mana(man(xx)), mana(xxx)
COLOR 7: PRINT mana(man(xx))
PRINT
END SUB

SUB pottiin (summa)
COLOR 7
LOCATE CSRLIN - 1, 1
PRINT LTRIM$(STR$(summa));
COLOR 8: PRINT " euroa kilahtaa hallinrakennusrahastoosi!"

potti(pv) = potti(pv) + summa
END SUB

SUB pporsvoit (gr%)
COLOR 14, 0
OPEN "game_" + LTRIM$(STR$(lokero)) + "\2" + LTRIM$(STR$(gr%)) + ".sta" FOR INPUT AS #1
DO UNTIL EOF(1)
INPUT #1, tempk, temp$, temp1, temp2, temp3, temp4
COLOR 13: PRINT tempk; "  ";
COLOR 10: PRINT temp$; "  ";
COLOR 2: PRINT l(temp1); "  ";
COLOR 14: PRINT temp2; "+"; temp3; "="; temp4
IF CSRLIN = 42 THEN pjn: klearstat (4)
LOOP
CLOSE #1
END SUB

SUB printket

LOCATE 3, 50
LOCATE , 50: COLOR , 0
IF maa(1, pv) <> 0 THEN
IF sort2(kurso) = maa(1, pv) THEN COLOR , 1
IF pel(maa(1, pv), pv).ppp = 1 THEN COLOR 14 ELSE COLOR 4
PRINT pel(maa(1, pv), pv).nam; " (";
IF krapu(pv) = 0 THEN PRINT LTRIM$(STR$(pel(maa(1, pv), pv).psk));  ELSE PRINT CHR$(1); CHR$(2);
PRINT ") "
END IF
IF maa(1, pv) = 0 THEN COLOR 4: PRINT "-                  "


sijatus = 9
FOR ooo = 1 TO 3
IF ooo > 1 THEN sijatus = sijatus + 5
LOCATE sijatus, 50: COLOR , 0
IF pk(ooo, pv).vp <> 0 THEN
IF sort2(kurso) = pk(ooo, pv).vp THEN COLOR , 1
IF pel(pk(ooo, pv).vp, pv).ppp = 2 THEN COLOR 14 ELSE COLOR 4
PRINT pel(pk(ooo, pv).vp, pv).nam; " (";
IF krapu(pv) = 0 THEN PRINT LTRIM$(STR$(pel(pk(ooo, pv).vp, pv).psk));  ELSE PRINT CHR$(1); CHR$(2);
PRINT ") "
END IF
IF pk(ooo, pv).vp = 0 THEN COLOR 4: PRINT "-                  "
LOCATE , 50: COLOR , 0
IF pk(ooo, pv).op <> 0 THEN
IF sort2(kurso) = pk(ooo, pv).op THEN COLOR , 1
IF pel(pk(ooo, pv).op, pv).ppp = 2 THEN COLOR 14 ELSE COLOR 4
PRINT pel(pk(ooo, pv).op, pv).nam; " (";
IF krapu(pv) = 0 THEN PRINT LTRIM$(STR$(pel(pk(ooo, pv).op, pv).psk));  ELSE PRINT CHR$(1); CHR$(2);
PRINT ") "
END IF
IF pk(ooo, pv).op = 0 THEN COLOR 4: PRINT "-                  "
NEXT ooo


FOR ooo = 1 TO 4
IF ooo = 1 OR ooo = 3 THEN sijatus = 39 ELSE sijatus = 58
IF ooo = 1 OR ooo = 2 THEN sijatuz = 25 ELSE sijatuz = 31

LOCATE sijatuz, sijatus: COLOR , 0
IF hk(ooo, pv).vh <> 0 THEN
IF sort2(kurso) = hk(ooo, pv).vh THEN COLOR , 1
IF pel(hk(ooo, pv).vh, pv).ppp = 3 THEN COLOR 14 ELSE COLOR 4
PRINT pel(hk(ooo, pv).vh, pv).nam; " (";
IF krapu(pv) = 0 THEN PRINT LTRIM$(STR$(pel(hk(ooo, pv).vh, pv).psk));  ELSE PRINT CHR$(1); CHR$(2);
PRINT ") "
END IF
IF hk(ooo, pv).vh = 0 THEN COLOR 4: PRINT "-                  "
LOCATE , sijatus: COLOR , 0
IF hk(ooo, pv).kh <> 0 THEN
IF sort2(kurso) = hk(ooo, pv).kh THEN COLOR , 1
IF pel(hk(ooo, pv).kh, pv).ppp = 4 THEN COLOR 14 ELSE COLOR 4
PRINT pel(hk(ooo, pv).kh, pv).nam; " (";
IF krapu(pv) = 0 THEN PRINT LTRIM$(STR$(pel(hk(ooo, pv).kh, pv).psk));  ELSE PRINT CHR$(1); CHR$(2);
PRINT ") "
END IF
IF hk(ooo, pv).kh = 0 THEN COLOR 4: PRINT "-                  "
LOCATE , sijatus: COLOR , 0
IF hk(ooo, pv).oh <> 0 THEN
IF sort2(kurso) = hk(ooo, pv).oh THEN COLOR , 1
IF pel(hk(ooo, pv).oh, pv).ppp = 5 THEN COLOR 14 ELSE COLOR 4
PRINT pel(hk(ooo, pv).oh, pv).nam; " (";
IF krapu(pv) = 0 THEN PRINT LTRIM$(STR$(pel(hk(ooo, pv).oh, pv).psk));  ELSE PRINT CHR$(1); CHR$(2);
PRINT ") "
END IF
IF hk(ooo, pv).oh = 0 THEN COLOR 4: PRINT "-                  "
NEXT ooo

COLOR , 0
END SUB

SUB printketkai

FOR xxx = 1 TO pelaa
IF pel(sort2(xxx), pv).ket = 0 THEN COLOR 8 ELSE IF pel(sort2(xxx), pv).ket = 1 THEN COLOR 14 ELSE COLOR 4
IF pel(sort2(xxx), pv).ppp = 1 THEN tempus = 0 ELSE IF pel(sort2(xxx), pv).ppp = 2 THEN tempus = 1 ELSE IF pel(sort2(xxx), pv).ppp = 3 THEN tempus = 2 ELSE IF pel(sort2(xxx), pv).ppp = 4 THEN tempus = 3 ELSE IF pel(sort2(xxx), pv).ppp = 5 THEN  _
tempus = 4
LOCATE xxx + 4 + tempus, 1
xx = sort2(xxx): printpelket
NEXT xxx
END SUB

SUB printketkehys
COLOR 7: LOCATE 1, 50: PRINT "MAALIVAHTI"
COLOR 8: LOCATE , 50: PRINT "ÍÍÍÍÍÍÍÍÍÍÍ"
COLOR 7: LOCATE 7, 50: PRINT "PAKKIPARI 1"
COLOR 8: LOCATE , 50: PRINT "ÍÍÍÍÍÍÍÍÍÍÍ"
COLOR 7: LOCATE 12, 50: PRINT "PAKKIPARI 2"
COLOR 8: LOCATE , 50: PRINT "ÍÍÍÍÍÍÍÍÍÍÍ"
COLOR 7: LOCATE 17, 50: PRINT "PAKKIPARI 3"
COLOR 8: LOCATE , 50: PRINT "ÍÍÍÍÍÍÍÍÍÍÍ"


COLOR 7: LOCATE 23, 39: PRINT "HY™KKŽYSKETJU 1"
COLOR 8: LOCATE , 39: PRINT "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ"

COLOR 7: LOCATE 23, 58: PRINT "HY™KKŽYSKETJU 2"
COLOR 8: LOCATE , 58: PRINT "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ"

COLOR 7: LOCATE 29, 39: PRINT "HY™KKŽYSKETJU 3"
COLOR 8: LOCATE , 39: PRINT "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ"
COLOR 7: LOCATE 29, 58: PRINT "HY™KKŽYSKETJU 4"
COLOR 8: LOCATE , 58: PRINT "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ"
END SUB

SUB printland
PRINT kansap(xx);

LOCATE , 20
COLOR 12, 0
IF skout(xx, pv) = 1 THEN PRINT "YHTEYS PELAA              ";
IF skout(xx, pv) > 1 THEN PRINT "TUTKIMUS MENEILLŽŽN ("; LTRIM$(STR$(skout(xx, pv) - 1)); ")"
IF skout(xx, pv) = 0 THEN PRINT "EI MIEHITYSTŽ                ";
COLOR 14
END SUB

SUB printpel (fat%)
SELECT CASE fat%
CASE 1
neup = pel(xx, pv)
CASE 2
neup = pel(xx, borsch)
CASE 3
neup = jel(xx, pv)
CASE 5
neup = bel(sortb(xx))
END SELECT

IF fat% = 5 THEN
IF neup.nat < 13 THEN
IF skout(neup.nat, pv) = 1 THEN nakyy = 1
END IF
IF neup.nat > 12 THEN nakyy = 1
IF neup.nat = 1 THEN nakyy = 1
END IF

IF neup.nat = 1 THEN COLOR 10 ELSE COLOR 2
IF fat% = 1 OR fat% = 3 THEN GOSUB klohma

LOCATE , 1: PRINT neup.nam;
LOCATE , 15: PRINT kansal(neup.nat);
LOCATE , 19
IF fat% = 5 AND nakyy = 0 THEN PRINT "??"; : COLOR , 0: PRINT "                ": EXIT SUB
PRINT pelip(neup.ppp);
LOCATE , 23: IF neup.spe = 12 THEN PRINT "??";  ELSE PRINT LTRIM$(STR$(neup.age));
LOCATE , 27

SELECT CASE fat%
CASE 1, 3
IF krapu(pv) = 0 THEN
pluss = neup.plus + efed(pv)
IF pluss < 0 THEN COLOR 8 ELSE IF pluss = 0 THEN COLOR 7 ELSE COLOR 15
END IF
IF krapu(pv) >= 1 THEN PRINT CHR$(1); CHR$(2);
IF krapu(pv) = 0 THEN PRINT LTRIM$(STR$(neup.psk + pluss)); " ";
GOSUB klohma
CASE ELSE
PRINT LTRIM$(STR$(neup.psk)); " ";
END SELECT

IF fat% = 5 THEN EXIT SUB

LOCATE , 31
IF neup.inj = 0 THEN PRINT "OK";
IF neup.inj <> 0 THEN
SELECT CASE neup.inj
CASE IS < 1000
PRINT "LO ";
IF neup.inj = -1 THEN PRINT "??";  ELSE PRINT LTRIM$(STR$(neup.inj));
CASE 1001 TO 1999
PRINT "PK ";
PRINT LTRIM$(STR$(neup.inj - 1000));
CASE IS > 2000
PRINT "VI ";
PRINT LTRIM$(STR$(neup.inj - 2000));
END SELECT
END IF

IF fat% <> 1 AND fat% <> 3 THEN sivux = 1
SELECT CASE sivux
CASE 1
LOCATE , 37
IF neup.kun < 0 THEN COLOR 1 ELSE COLOR 15
PRINT STR$(neup.kun);
GOSUB klohma
LOCATE , 41
PRINT LTRIM$(STR$(neup.pok));
LOCATE , 45
PRINT LTRIM$(STR$(neup.pot));
IF neup.ppp <> 1 THEN
IF neup.gls < 10 THEN LOCATE , 49 ELSE LOCATE , 48
PRINT LTRIM$(STR$(neup.gls));
PRINT " + ";
PRINT LTRIM$(STR$(neup.ass));
LOCATE , 55
PRINT " = ";
PRINT LTRIM$(STR$(neup.gls + neup.ass));
END IF
IF fat% = 1 AND neup.ppp = 1 THEN
LOCATE , 49
IF neup.gls = 0 THEN PRINT "-";  ELSE PRINT LEFT$(LTRIM$(STR$((neup.gls / neup.pot))), 3); " M/OTT.";
END IF

IF neup.spe = 5 AND neup.ket > 0 AND fat% = 1 THEN COLOR 15: LOCATE , 63: PRINT "(M)URJO";
IF neup.spe = 666 AND neup.ket > 0 AND fat% = 1 THEN COLOR 15: LOCATE , 63: PRINT "(M)URJOO!";
IF fat% <> 1 AND fat% <> 3 THEN
LOCATE , 64
PRINT LTRIM$(STR$(neup.svu));
LOCATE , 66
PRINT LTRIM$(STR$(neup.sra));
END IF

CASE 2
LOCATE , 37
SELECT CASE neup.svu
CASE IS < 98
PRINT LTRIM$(STR$(neup.svu));
LOCATE , 41
PRINT LTRIM$(STR$(neup.sra));
LOCATE , 48
SELECT CASE neup.lah
CASE 96
PRINT "P™RSSI";
CASE 97
PRINT "NHL";
CASE 98
PRINT "KOTIMAA";
CASE 99
PRINT "ELŽKE";
END SELECT

CASE IS > 10000
LOCATE , 48
PRINT "MIETTII";

CASE 666
LOCATE , 48
PRINT "LAINASSA";
END SELECT

CASE 3
LOCATE , 37
SELECT CASE neup.spe
CASE 1 TO 7, 9 TO 12
PRINT spezi(neup.spe);
CASE 8
IF neup.pok > 30 THEN PRINT spezi(neup.spe);
END SELECT

CASE 4
LOCATE , 37
IF pel(xx, pv).ppp <> 1 THEN
IF rug(xx, pv) < 10 THEN PRINT " ";
PRINT LTRIM$(STR$(rug(xx, pv)));
PRINT " + ";
PRINT LTRIM$(STR$(rua(xx, pv)));
IF rua(xx, pv) < 10 THEN PRINT " ";
PRINT " = ";
PRINT LTRIM$(STR$(rug(xx, pv) + rua(xx, pv)))
END IF
END SELECT
EXIT SUB

klohma:
IF neup.inj = 0 THEN
IF neup.ket = 0 THEN COLOR 10 ELSE COLOR 14
END IF

IF neup.inj <> 0 THEN
SELECT CASE neup.inj
CASE IS < 1000
COLOR 6
CASE 1001 TO 1999
COLOR 12
CASE IS > 2000
COLOR 2
END SELECT
END IF
RETURN

END SUB

SUB printpelket
LOCATE , 1
PRINT pel(xx, pv).nam;
LOCATE , 15
PRINT pelip(pel(xx, pv).ppp);
LOCATE , 18
PRINT LTRIM$(STR$(pel(xx, pv).age));
LOCATE , 21
IF krapu(pv) = 0 THEN
pluss = pel(xx, pv).plus + efed(pv)
PRINT LTRIM$(STR$(pel(xx, pv).psk + pluss));
END IF
IF krapu(pv) >= 1 THEN PRINT CHR$(1); CHR$(2);
IF pel(xx, pv).ppp <> 1 THEN
IF pel(xx, pv).gls < 10 THEN LOCATE , 25 ELSE LOCATE , 24
PRINT LTRIM$(STR$(pel(xx, pv).gls));
LOCATE , 26: PRINT "+";
PRINT LTRIM$(STR$(pel(xx, pv).ass));
LOCATE , 29: PRINT "= ";
PRINT LTRIM$(STR$(pel(xx, pv).gls + pel(xx, pv).ass));
END IF
LOCATE , 34
IF pel(xx, pv).kun < 0 THEN COLOR 1 ELSE COLOR 15
PRINT STR$(pel(xx, pv).kun)
END SUB

SUB printpels
COLOR 10
PRINT RTRIM$(pel(xx, pv).nam);
PRINT " ("; LTRIM$(STR$(pel(xx, pv).age)); ","; pelip(pel(xx, pv).ppp); ","; LTRIM$(STR$(pel(xx, pv).psk)); ") ";
COLOR 2
END SUB

SUB prm (juu%)
COLOR 10

SELECT CASE juu%
CASE 1
PRINT RTRIM$(l(xx)); " ("; mana(man(xx)); ") ";
CASE 2
PRINT mana(man(xx)); " ("; RTRIM$(l(xx)); ") ";
CASE 3
PRINT RTRIM$(l(xx)); " ";
CASE 4
PRINT mana(xx); " ";

END SELECT
END SUB

SUB protesti
puolesta = 0: vastaan = 0
CLS
COLOR 14: PRINT "PHL:N JOHTOPORRAS KOKOONTUU KŽSITTELEMŽŽN PROTESTIA..."
PRINT "JOHTOKUNNAN 11 JŽSENTŽ ŽŽNESTŽVŽT JOKO PUOLESTA TAI VASTAAN."
PRINT
COLOR 13
PRINT "SYYTTŽJŽ: ";
COLOR 14: PRINT l(xx);
COLOR 10: PRINT "("; mana(man(xx)); ")"
PRINT : COLOR 13
PRINT "SYYTETTY: ";
COLOR 14
PRINT l(xxx);
COLOR 10: PRINT "("; mana(man(xxx)); ")"
PRINT
FOR zz = 1 TO 11
COLOR 7
PRINT "JŽSEN "; LTRIM$(STR$(zz)); ": ";
sukka = 1.3 * RND
IF pikaprot = 0 THEN
dukka = TIMER + sukka: WHILE dukka > TIMER: WEND
END IF
zzz = CINT(100 * RND)
IF mulkku = 1 THEN zzz = 99
COLOR 15
IF zzz <= 50 THEN PRINT "VASTAAN": vastaan = vastaan + 1
IF zzz > 50 THEN PRINT "PUOLESTA": puolesta = puolesta + 1
PRINT
NEXT zz

COLOR 13
SELECT CASE puolesta
CASE IS < vastaan
PRINT "JOHTOKUNNAN PŽŽT™S ON KIELTEINEN!"
COLOR 14
PRINT RTRIM$(l(xx));
COLOR 13
PRINT " TUOMITAAN MENETTŽMŽŽN 2 PISTETTŽ VALHEELLISISTA SYYT™KSISTŽ!"
p(xx) = p(xx) - 2
CASE IS > vastaan
PRINT "JOHTOKUNNAN PŽŽT™S ON MY™NTEINEN!"
COLOR 14
PRINT RTRIM$(l(xxx));
COLOR 13
PRINT " TUOMITAAN MENETTŽMŽŽN 2 PISTETTŽ!"
p(xxx) = p(xxx) - 2
END SELECT

staulmaar
pjn


CLS
END SUB

SUB qelp (qel%)
COLOR , 0
CLS : COLOR 7
luetexti (LTRIM$(STR$(qel%)) + "help")
pjn
END SUB

SUB rakprosessi
SELECT CASE rstage(pv)

CASE 1
ractio(pv) = ractio(pv) + 2
IF CINT(100 * RND) + 1 < ractio(pv) THEN
ractio(pv) = 0
hmm = CINT(100 * RND)
CLS
manap
PRINT "!!!"
PRINT
PRINT "BYROKRATIAN RATTAAT OVAT SAANEET RAKENNUSLUPA-ANOMUKSESI KŽSITELTYŽ!"
PRINT
PRINT "SE ON ";
SELECT CASE hmm
CASE IS < 14
COLOR 14: PRINT "HYLŽTTY!"
rstage(pv) = 0
CASE ELSE
COLOR 14: PRINT "HYVŽKSYTTY";
COLOR 13: PRINT "!"
rstage(pv) = 2
IF halliai(halli(u(pv)) + 1) = 0 THEN rstage(pv) = 3
END SELECT
pesc
END IF

CASE 2
SELECT CASE CINT(100 * RND)
CASE 50 TO 90
ractio(pv) = ractio(pv) + 1
CASE 91 TO 100
ractio(pv) = ractio(pv) + 2
END SELECT
IF ractio(pv) >= halliai(halli(u(pv)) + 1) THEN
ractio(pv) = 0
rstage(pv) = 3
CLS
manap
PRINT "!!!"
PRINT
PRINT "KAIKKI UUDEN HALLISI AITIOT ON MYYTY, JA YHTEISTY™KUMPPANIN ETSINTŽ VOI"
PRINT "ALKAA!"
pesc
END IF

CASE 3
ractio(pv) = ractio(pv) + 1
IF CINT(100 * RND) + 1 < ractio(pv) THEN
DIM sponsor(1 TO 37) AS STRING
DIM sponsal(1 TO 37) AS INTEGER
DIM ala(1 TO 9) AS STRING
OPEN "data\data4.99" FOR INPUT AS #1
FOR zz = 1 TO 37
INPUT #1, sponsor(zz), sponsal(zz)
NEXT zz
FOR zz = 1 TO 9
INPUT #1, ala(zz)
NEXT zz
CLOSE #1
zz = CINT(36 * RND) + 1
CLS
manap
PRINT "!!!"
PRINT
PRINT "YHTEISTY™KUMPPANI HALLIN RAHOITUKSEEN ON L™YTYNYT!"
PRINT : COLOR 14
PRINT sponsor(zz); " ("; ala(sponsal(zz)); ") "
PRINT
COLOR 13: PRINT "ON MUKANA RAHOITUKSESSA, KUNHAN HE SAAVAT NIMETŽ UUDEN AREENAN."
pesc
temp% = CINT(4 * RND) + 1
SELECT CASE temp%
CASE 1
haln(u(pv)) = sponsor(zz) + " Areena"
CASE 2
haln(u(pv)) = sponsor(zz) + "-Monitoimikeskus"
CASE 3
haln(u(pv)) = sponsor(zz) + "-Multimediacenter"
CASE 4
haln(u(pv)) = sponsor(zz) + " Thunderdome"
CASE 5
haln(u(pv)) = sponsor(zz) + "-Halli"
END SELECT
ERASE sponsor, sponsal, ala
rstage(pv) = 4
ractio(pv) = 30 + halli(u(pv))
END IF
CASE 4
ractio(pv) = ractio(pv) - 1
IF ractio(pv) = 1 THEN
CLS
manap
PRINT "!!!"
PRINT
PRINT "UUSI JŽŽHALLISI ON NYT VALMIS, JA KŽYT™SSŽ SEURAAVAA OTTELUA VARTEN!"
potti(pv) = potti(pv) - halvaat(halli(u(pv)))
halli(u(pv)) = halli(u(pv)) + 1

IF halli(u(pv)) = 14 THEN
IF potti(pv) > 0 THEN
raha(pv) = raha(pv) + potti(pv): potti(pv) = 0
PRINT
PRINT "HALLISI ON NYT SUURIN MAHDOLLINEN - POTTI TYHJENNETŽŽN JA LIITETŽŽN SALDOOSI"
END IF
END IF

pesc
rstage(pv) = 0
END IF
END SELECT

END SUB

SUB report
rabban:
CLS
COLOR 13
PRINT "TAPAHTUMAKATSAUS,MANAGERI ";
colchk
PRINT nimi(pv)
IF runkosarja = 1 OR muke(pv) = 1 OR mukp(pv) = 1 THEN
COLOR 12
LOCATE 3, 1
PRINT l(u(pv)); " - "; l(vast(pv));
PRINT " ";
IF koti(pv) = 1 THEN PRINT "(K)";  ELSE PRINT "(V)";
PRINT gl(u(pv)); " - "; gl(vast(pv))

COLOR 13: LOCATE 5, 1
IF koti(pv) = 1 THEN
PRINT "YLEIS™Ž: ";
COLOR 14
PRINT ylm(u(pv))
PRINT
END IF
END IF
COLOR 6
IF gl(u(pv)) > gl(vast(pv)) THEN tuloste = 1
IF gl(u(pv)) = gl(vast(pv)) THEN tuloste = 2
IF gl(u(pv)) < gl(vast(pv)) THEN tuloste = 3

IF lm = 1 THEN
IF maa(1, pv) <> 0 THEN
IF pel(maa(1, pv), pv).ppp = 1 THEN
pel(maa(1, pv), pv).gls = pel(maa(1, pv), pv).gls + gl(vast(pv))
END IF
END IF
END IF


IF runkosarja = 1 THEN
SELECT CASE tuloste
CASE 1
IF sr(u(pv)) = 1 THEN voittophl(pv) = voittophl(pv) + 1 ELSE voittodiv(pv) = voittodiv(pv) + 1
CASE 2
IF sr(u(pv)) = 1 THEN tasanphl(pv) = tasanphl(pv) + 1 ELSE tasandiv(pv) = tasandiv(pv) + 1
CASE 3
IF sr(u(pv)) = 1 THEN tappiophl(pv) = tappiophl(pv) + 1 ELSE tappiodiv(pv) = tappiodiv(pv) + 1
END SELECT
END IF

SELECT CASE tuloste
CASE 1
mo(pv) = mo(pv) + 1
IF gl(u(pv)) - gl(vast(pv)) < 6 THEN PRINT "JEAH! VOITTO TULI!" ELSE PRINT "JEAH! R™KŽLEVOITTO!!"
vputki(pv) = vputki(pv) + 1
IF dputki(pv) > 0 THEN dputki(pv) = 0
IF tputki(pv) > 0 THEN tputki(pv) = 0
CASE 2
PRINT "HMM... TASAN MENI!"
dputki(pv) = dputki(pv) + 1
IF vputki(pv) > 0 THEN vputki(pv) = 0
IF tputki(pv) > 0 THEN tputki(pv) = 0
CASE 3
IF gl(vast(pv)) - gl(u(pv)) < 6 THEN PRINT "AARGH! KIRVELEVŽ TAPPIO!" ELSE PRINT "AARGH! R™KŽLETAPPIO!"
tputki(pv) = tputki(pv) + 1
IF tputki(pv) >= 3 THEN mo(pv) = mo(pv) - 2 ELSE mo(pv) = mo(pv) - 1
IF vputki(pv) > 0 THEN vputki(pv) = 0
IF dputki(pv) > 0 THEN dputki(pv) = 0
END SELECT
IF vputki(pv) > 1 OR dputki(pv) > 1 OR tputki(pv) > 1 THEN
IF vputki(pv) > 0 THEN
PRINT "JO ";
PRINT LTRIM$(STR$(vputki(pv)));
PRINT " VOITTOA";
PRINT " PUTKEEN!"
END IF
IF dputki(pv) > 0 THEN
PRINT "JO ";
PRINT LTRIM$(STR$(dputki(pv)));
PRINT " TASAPELIŽ";
PRINT " PUTKEEN!"
END IF
IF tputki(pv) > 0 THEN
PRINT "JO ";
PRINT LTRIM$(STR$(tputki(pv)));
PRINT " TAPPIOTA";
PRINT " PUTKEEN!"
END IF
END IF

muutk = 0: tulot = 0: menot = 0: temp% = 0
PRINT
LOCATE 1

xxx = 0
IF koti(pv) = 1 THEN
IF ot < 44 THEN xxx = 1
IF ot > 44 AND sr(u(pv)) = 2 THEN xxx = 1

IF xxx = 1 THEN
LOCATE , 50
COLOR 10: PRINT "YLEIS™TULOT      ";

SELECT CASE runkosarja
CASE 1
tulot = tulot + (CLNG(ylm(u(pv))) - kausik(u(pv))) * lhinta(sr(u(pv)))
CASE 0
tulot = tulot + CLNG(ylm(u(pv))) * lhinta(sr(u(pv)))
END SELECT
IF tulot < 0 THEN COLOR 4 ELSE COLOR 14
PRINT STR$(tulot)
END IF
END IF

IF runkosarja = 1 THEN
LOCATE , 50
COLOR 10: PRINT "SPONSORITULOT    ";
hokla& = sraha(1, pv)
IF tuloste = 1 THEN hokla& = hokla& + sraha(6, pv) ELSE IF tuloste = 2 THEN hokla& = hokla& + (sraha(6, pv) / 2)
COLOR 14: PRINT STR$(hokla&)
tulot = tulot + hokla&
END IF

raha(pv) = raha(pv) + tulot

IF vieras(pv) = 1 THEN
LOCATE , 50
COLOR 2: PRINT "MATKAKULUT       ";
IF vast(pv) > 24 THEN temp% = 4 ELSE temp% = mtap(pv)
SELECT CASE temp%
CASE 0
temp% = 4000
CASE 1
temp% = 8000
CASE 2
temp% = 13000
CASE 3
temp% = 18000
CASE 4
temp% = 30000
END SELECT
COLOR 4: PRINT STR$(-temp%)
menot = menot + temp%
IF fani(pv) = 1 THEN muutk = muutk + 10000
IF efed(pv) = 2 THEN muutk = muutk + 1000
END IF

IF runkosarja = 1 OR muke(pv) = 1 THEN
LOCATE , 50
COLOR 2: PRINT "PELAAJAPALKKIOT  ";
pmenot& = 0
FOR zz = 1 TO lpl(pv)
pmenot& = pmenot& + pel(zz, pv).sra + valbh(vai2(pv), 5, valb(5, pv))
NEXT zz
menot = menot + pmenot&
COLOR 4: PRINT STR$(-pmenot&)
END IF
IF runkosarja = 1 THEN
LOCATE , 50
COLOR 2: PRINT "BUDJETTI         ";
doh& = CLNG(valbh(vai2(pv), 1, valb(1, pv))) + CLNG(valbh(vai2(pv), 2, valb(2, pv))) + CLNG(valbh(vai2(pv), 3, valb(3, pv))) + CLNG(valbh(vai2(pv), 4, valb(4, pv)))
COLOR 4: PRINT STR$(-doh&)
menot = menot + doh&
korkul& = 0
IF laina(1, pv) > 0 OR laina(2, pv) > 0 OR laina(3, pv) > 0 THEN
FOR xxx = 1 TO 3
korkul& = korkul& + laina(xxx, pv) * ((pankkor(xxx) / 100) / 44)
NEXT xxx
LOCATE , 50
COLOR 2: PRINT "KORKOKULUT       ";
COLOR 4: PRINT STR$(-korkul&)
menot = menot + korkul&
END IF
temp% = 0
FOR zz = 2 TO 12
IF skout(zz, pv) > 0 THEN menot = menot + 1000: temp% = temp% + 1
NEXT zz
IF temp% > 0 THEN
LOCATE , 50
COLOR 2: PRINT "KYKYJENETSIJŽT   ";
COLOR 4: PRINT -1000 * temp%
END IF
IF efed(pv) = 2 THEN muutk = muutk + 1
END IF

IF muutk > 0 THEN
LOCATE , 50
COLOR 2: PRINT "MUUT KULUT       ";
COLOR 4: PRINT -muutk
menot = menot + muutk
END IF

IF menot <> 0 OR tulot <> 0 THEN
LOCATE , 50
COLOR 8: PRINT "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
IF tulot - menot < 0 THEN COLOR 4 ELSE COLOR 14
LOCATE , 67: PRINT STR$(tulot - menot)
PRINT
END IF

IF raha(pv) < 0 THEN
IF mo(pv) <> 66 THEN mo(pv) = -65
END IF
raha(pv) = raha(pv) - menot

IF runkosarja = 1 THEN
IF sovtap(pv) = 1 THEN
sovtap(pv) = 0
IF tuloste <> 3 THEN
sopuhu(pv) = sopuhu(pv) + 1
seuraus% = CINT(100 * RND) + 1
SELECT CASE seuraus%
CASE IS < 50
muilu(u(pv)) = 1
CASE 50 TO 80
laxa(u(pv)) = 1
CASE 81 TO 100
laxa(u(pv)) = 1
muilu(u(pv)) = 1
skanda(u(pv)) = 1
END SELECT
END IF
END IF
END IF
END SUB

SUB ripuli
lux (19)
FOR xx = 1 TO lpl(pv)
IF pel(xx, pv).inj = 0 THEN
IF 100 * RND > 60 THEN pel(xx, pv).inj = 1
END IF
NEXT xx
laxa(u(pv)) = 0
END SUB

SUB sarjamaar

FOR xx = 1 TO 24
x(sed(xx)) = xx
NEXT xx

END SUB

SUB sattuma
LOCATE 12, 1

IF mo(pv) = -65 THEN
COLOR 15
luetexti ("bankrupt")
PRINT
mo(pv) = -66
END IF

IF runkosarja = 1 THEN
IF sr(u(pv)) = 1 AND kr > 10 AND s(u(pv)) < 5 AND s(vast(pv)) < 5 THEN topmatch
END IF

IF efed(pv) = 2 THEN
SELECT CASE runkosarja
CASE 1
IF CINT(100 * RND) < 10 THEN
COLOR 2
luetexti ("kiinp1")
PRINT
p(u(pv)) = 0
raha(pv) = raha(pv) - 1200000
mo(pv) = 0
FOR xx = 1 TO lpl(pv)
pel(xx, pv).gls = 0
pel(xx, pv).ass = 0
NEXT xx
END IF
CASE 0
SELECT CASE lm
CASE 1
IF CINT(100 * RND) < 50 THEN
COLOR 2
luetexti ("kiinp2")
PRINT
temp% = 1: mukachk
raha(pv) = raha(pv) - 1200000
cndcheck
END IF
CASE 0
IF CINT(100 * RND) < 33 THEN
COLOR 2
luetexti ("kiinp3")
PRINT
FOR xx = 1 TO 20
IF ohj(ex(xx)) = pv THEN EXIT FOR
NEXT xx
ex(xx) = 52 + pv
muke(pv) = 0
ep(xx) = 0
ero(xx) = 0
raha(pv) = raha(pv) - 500000
END IF
END SELECT
END SELECT
END IF

IF muilu(u(pv)) = 1 THEN
arvoloukkaa (1)
IF lukka = 0 THEN
lukka = 44
loukkaan
muilu(u(pv)) = 0
END IF
END IF

IF vai(pv) = 1 THEN louktod% = 6 ELSE IF vai(pv) = 5 THEN louktod% = 14 ELSE louktod% = 10

IF CINT(100 * RND) < louktod% THEN
arvoloukkaa (1)
IF lukka = 0 THEN
lukka = CINT(43 * RND) + 1
loukkaan
PRINT
END IF
END IF

IF CINT(100 * RND) < 20 THEN
moodiin
PRINT
END IF

IF CINT(100 * RND) < 5 THEN
arvoloukkaa (1)
IF lukka = 0 THEN
lukka = CINT(15 * RND) + 1
kieltoon
PRINT
END IF
END IF

FOR xxx = 1 TO lpl(pv)
IF pel(xxx, pv).spe = 666 THEN
murjo(pv) = 1
lukka = 17
kieltoon
END IF
IF pel(xxx, pv).spe = 2 AND pel(xxx, pv).ket > 0 THEN
IF CINT(100 * RND) < 5 THEN
lukka = 18
kieltoon
END IF
END IF
NEXT xxx

FOR xxx = 1 TO plkm
IF vast(xxx) = u(pv) AND murjo(xxx) = 1 THEN
murjo(xxx) = 0
arvoloukkaa (1)
IF lukka = 0 THEN
lukka = 45
loukkaan
PRINT
END IF
END IF
NEXT xxx

IF kytta(u(pv)) <> 0 THEN
xx = kytta(u(pv))
kytta(u(pv)) = 0
prm (2)
lux (13)
mo(pv) = mo(pv) - 15
END IF

FOR xx = 1 TO 24
IF ohj(xx) = 0 AND kytta(xx) <> 0 THEN
kytta(xx) = 0
prm (2)
lux (36)
IF tautip(xx) = 1 THEN
tautip(xx) = .94
tautik(xx) = CINT(2 * RND) + 2
END IF
END IF
NEXT xx

vallenthius:
IF runkosarja = 1 THEN
dat% = CINT(520 * RND) + 1

COLOR 2
IF skanda(u(pv)) = 1 THEN skandalpl
IF laxa(u(pv)) = 1 THEN ripuli
IF kouku(u(pv)) = 1 THEN vieroitus
SELECT CASE dat%

CASE 1, 2
lux (1)
temp% = CSRLIN
pjn
LOCATE temp%, 1
COLOR 2: PRINT "Uskoitko? Ensimm„isell„ kerralla on moni mennyt halpaan!"

CASE 3, 4, 5
lux (2)
mo(pv) = mo(pv) + 2

CASE 6, 7
lux (3)
GOSUB skys
IF sv$ = "k" THEN PRINT "Korkialahti ilahtuu suuresti lahjoituksesi johdosta."
IF sv$ = "e" THEN PRINT "Erkin Liha & Leikkele maksaa ronsusta 5.000 euroa (ja myy seuraavan viikon": PRINT "ajan sikanauta-jauhelihaa erikoistarjouksena)": raha(pv) = raha(pv) + 5000

CASE 8, 9, 10
lux (4)
raha(pv) = raha(pv) + 35000

CASE 11, 12, 13
lux (5)
raha(pv) = raha(pv) - 50000

CASE 14, 15, 16
lux (6)
raha(pv) = raha(pv) + 50000

CASE 17, 18, 19, 20
lux (7)
raha(pv) = raha(pv) + 20000

CASE 21, 22, 23, 24
lux (8)
raha(pv) = raha(pv) - 20000

CASE 25
IF raha(pv) > 500000 THEN
lux (9)
raha(pv) = raha(pv) - FIX(.1 * raha)
END IF

CASE 26, 27, 28
lux (10)
raha(pv) = raha(pv) + 10000

CASE 29, 30
xx = CINT((lpl(pv) - 1) * RND) + 1
printpels
lux (11)
GOSUB skys
IF sv$ = "k" THEN krapu(pv) = 2 ELSE krapu(pv) = 1
IF sv$ = "k" THEN mo(pv) = mo(pv) + 4: PRINT "Jees! Ei kun menoksi!" ELSE PRINT "No, kenties joskus toiste?"

CASE 31, 32
lux (12)
GOSUB skys
IF sv$ = "k" THEN cccp(pv) = cccp(pv) + 1: raha(pv) = raha(pv) - 39999: PRINT "OK. K„yt„ tabletteja A-menusta tarpeen tullen."
IF sv$ = "e" THEN PRINT "Mies k„velee ulos, mutisten jotakin itsekseen."

CASE 33, 34
arpo
kytta(u(pv)) = xx

CASE 35
lux (14)
GOSUB skys
voitto% = 100 * RND
SELECT CASE sv$
CASE "k"
IF voitto% >= 65 THEN PRINT "Jees! Musta tuli, ja raha tuplaantui!": raha(pv) = raha(pv) + 200000 ELSE PRINT "Hupsan saa...tulos: 27, punainen!": raha(pv) = raha(pv) - 100000
CASE "e"
PRINT "O.K. (Tulos: 13, musta)";
END SELECT

CASE 36
lux (15)
GOSUB skys
IF sv$ = "k" THEN voodoo(pv) = 1: raha(pv) = raha(pv) - 10000: PRINT "OK. Voodooheppuli liittyy ryhm„„si!"
IF sv$ = "e" THEN PRINT "Voodoomies poistuu, j„tt„en j„lkeens„ oudon migreenin!"

CASE 37
lux (16)
GOSUB skys
IF sv$ = "k" THEN bantu(pv) = 1: raha(pv) = raha(pv) - 10000: PRINT "Ryhm„ lupaa saapua pikimmiten!"
IF sv$ = "e" THEN PRINT "Soittaja ly” luurin korvaasi rumpujen soidessa taustalla."

CASE 38, 39, 40
lux (17)
summa = 15000
LOCATE CSRLIN - 2
COLOR 10: PRINT LTRIM$(STR$(summa)); " ";
COLOR 2: PRINT "euron palkkio ropsahtaa tilillesi!"
raha(pv) = raha(pv) + summa

CASE 41
summa = FIX(200000 * RND) + 1
lux (18)
LOCATE CSRLIN - 4, 42: COLOR 10: PRINT LTRIM$(STR$(summa))
raha(pv) = raha(pv) + summa

CASE 42
ripuli

CASE 43, 44, 45
lux (20)
arpo
LOCATE CSRLIN - 3, 10
prm (2)
GOSUB skys
IF sv$ = "k" THEN verk% = xx

CASE 46, 47
lux (21)
mo(pv) = mo(pv) - 4

CASE 48, 49
lux (22)
GOSUB skys
SELECT CASE sv$
CASE "e"
PRINT "Selv„ - antaa rienaajien rienata niin paljon kuin jaksavat!"
CASE "k"
kuinka% = 100 * RND
summa = 100000
SELECT CASE kuinka%
CASE IS < 50
PRINT "Arh! Ilta-Mason juttu todetaan todeksi ja joudut maksamaan heid„n kulunsa,"
COLOR 10: PRINT LTRIM$(STR$(summa));
COLOR 2: PRINT " euroa."
raha(pv) = raha(pv) - summa
CASE ELSE
PRINT "Jes! Ilta-Maso joutuu perumaan pahat puheensa ja maksamaan oikeuskulusi,"
COLOR 10: PRINT LTRIM$(STR$(summa));
COLOR 2: PRINT " euroa."
raha(pv) = raha(pv) + summa
END SELECT
END SELECT

CASE 50, 51
IF tautip(u(pv)) = 1 THEN
lux (23)
tautip(u(pv)) = .93
tautik(u(pv)) = 3
END IF

CASE 52
nats = 1: mahmax
COLOR 10: PRINT krj$; "."; ptemp$;
lux (24)
mo(pv) = momin(vai(pv))

CASE 53, 54, 55
IF runkosarja = 1 THEN
IF ohj(vast(pv)) = 0 THEN
COLOR 10
PRINT RTRIM$(l(vast(pv))); " ("; mana(man(vast(pv))); ") ";
lux (25)
pjn
xx = vast(pv): xxx = u(pv): protesti: EXIT SUB
END IF
END IF

CASE 56, 57, 58
lux (26)
LOCATE CSRLIN - 1, 1
temp% = CINT(99 * RND) + 1
SELECT CASE temp%
CASE IS < 51
PRINT "ja joukkueen moraali nousee kohisten!"
mo(pv) = mo(pv) + 4
CASE ELSE
PRINT "...mutta voi! Pertti p„„tt„„kin j„tt„„ rahat tilitt„m„tt„ ja sin„ saat"
PRINT "syyt niskoillesi! Moraali laskee!"
mo(pv) = mo(pv) - 4
END SELECT

CASE 59
lux (27)
pottiin (400000)

CASE 60, 61, 62
lux (28)
juke = CINT(9 * RND) + 1
OPEN "data\enne1.mhm" FOR INPUT AS #1
FOR z = 1 TO juke
LINE INPUT #1, ennus$
NEXT z
CLOSE #1
PRINT ennus$

CASE 63
IF snimi(pv) <> "Liverpool OY" THEN
COLOR 10: PRINT snimi(pv); " ";
lux (29)
snimi(pv) = "Liverpool OY"
FOR z = 1 TO 8
sraha(z, pv) = sraha(z, pv) * .8
NEXT z
END IF

CASE 64, 65
lukka = 1
FOR xxx = 1 TO 20
xx = CINT((lpl(pv) - 1) * RND) + 1
IF pel(xx, pv).ego > 9 THEN lukka = 0: EXIT FOR
NEXT xxx
IF lukka = 0 THEN
printpels
PRINT " ";
lux (30)
mo(pv) = mo(pv) - 7
END IF

CASE 66, 67
lux (31)
mo(pv) = mo(pv) - 2

CASE 68, 69
IF lpl(pv) > 0 THEN
xx = CINT((lpl(pv) - 1) * RND) + 1
IF pel(xx, pv).psk >= 5 THEN
IF pel(xx, pv).plus = 0 THEN
printpels
PRINT " ";
lux (32)
GOSUB skys
IF sv$ = "k" THEN PRINT "Pelaaja kiitt„„ ja poistuu hymyillen leve„sti.": pel(xx, pv).sra = FIX(pel(xx, pv).sra * 1.1): pel(xx, pv).plus = 1: pel(xx, pv).kest = CINT(2 * RND) + 2
IF sv$ = "e" THEN PRINT "Pelaaja kiroaa sinulle, vannoen ett„ kadut ratkaisua.": pel(xx, pv).plus = -2: pel(xx, pv).kest = 1000
END IF
END IF
END IF

CASE 70
IF kuume(pv) = 0 AND sr(u(pv)) = 1 THEN
lux (33)
kuume(pv) = 1
END IF

CASE 71
IF kuume(pv) = 0 AND sr(u(pv)) = 2 THEN
lux (34)
kuume(pv) = 1
END IF

CASE 72, 73, 74
IF sopuhu(pv) <= 10 AND sovtap(pv) = 0 THEN
summa = 100000 - ((sr(u(pv)) - 1) * 50000)
lux (35)
LOCATE CSRLIN - 5, 63
COLOR 10: PRINT LTRIM$(STR$(summa));
LOCATE CSRLIN + 4, 1
GOSUB skys
COLOR 2
IF sv$ = "k" THEN raha(pv) = raha(pv) + summa: sovtap(pv) = 1: PRINT "'Rahat siirtyv„t v„litt”m„sti.' mies sanoo ja sulkee puhelimen."
IF sv$ = "e" THEN PRINT "Mies sulkee puhelimen v„litt”m„sti."
END IF

CASE 77, 78
lux (37)
mo(pv) = momax(vai(pv))

CASE 79, 80
lux (38)
mo(pv) = mo(pv) - 8

CASE 81, 82, 182
IF kr < 40 THEN
IF lpl(pv) < 32 THEN
lukka = 0
FOR z = 1 TO 10
xx = CINT(10 * RND) + 2
IF skout(xx, pv) = 1 THEN lukka = 1: EXIT FOR
NEXT z
IF lukka = 1 THEN
COLOR 10: PRINT kansap(xx); " ";
lux (39)
GOSUB skys
IF sv$ = "e" THEN PRINT "'Selv„. Enk”h„n min„ kaverille ottajan l”yd„'"
IF sv$ = "k" THEN
PRINT "'Hienoa! Kaveri l„htee t„„lt„ seuraavalla koneella'"
lpl(pv) = lpl(pv) + 1
nats = xx
xxx = lpl(pv)
pel(xxx, pv) = blanko(1)
mahmax
pel(xxx, pv).nam = krj$ + "." + ptemp$
pel(xxx, pv).psk = CINT(10 * RND) + 2
pel(xxx, pv).svu = 1
pel(xxx, pv).age = CINT(19 * RND) + 19
pel(xxx, pv).nat = nats
pel(xxx, pv).ppp = CINT(4 * RND) + 1
egomaar (1)
palkmaar (1)
pel(xxx, pv).sra = palkehd
temp% = CINT(100 * RND) + 1
SELECT CASE temp%
CASE 90 TO 91
pel(xxx, pv).spe = 1
CASE 92 TO 94
pel(xxx, pv).spe = 5
END SELECT
END IF
END IF
END IF
END IF

CASE 83, 84
lux (40)
GOSUB skys
IF sv$ = "e" THEN nyrkkeily
IF sv$ = "k" THEN
lukka = CINT(100 * RND)
SELECT CASE lukka
CASE IS > 50
PRINT "Hatti-Vatti joutuu pyyt„m„„n sinulta julkisesti anteeksi... Miehen tuntien"
PRINT "se lienee rangaistuksista pahin."
CASE ELSE
PRINT "Oikeus ei n„e perusteita julkiselle anteeksipyynn”lle..."
END SELECT
END IF

CASE 85, 86
arpo
prm (2)
lux (41)
GOSUB skys
IF sv$ = "k" THEN nyrkkeily
IF sv$ = "e" THEN PRINT "Mies nauraa r„k„isesti. 'L„l l„l l„„' h„n parjaa sinua.": mo(pv) = mo(pv) - 2

CASE 87
juke = CINT(5 * RND) + 1
OPEN "data\42e1.mhm" FOR INPUT AS #1
FOR z = 1 TO juke
LINE INPUT #1, ennus$
NEXT z
CLOSE #1
COLOR 2: PRINT ennus$;
PRINT ", kuuluisa kirjailija, hahmottelee uutta teostaan;"
OPEN "data\42e2.mhm" FOR INPUT AS #1
juke = CINT(9 * RND) + 1
FOR z = 1 TO juke
LINE INPUT #1, ennus$
NEXT z
CLOSE #1
COLOR 10: PRINT ennus$;
COLOR 2: PRINT " on kirjan nimi, ja se kertoo sinun el„m„st„si!"

CASE 88, 89
lukka = 0
FOR z = 1 TO 10
xx = CINT(10 * RND) + 2
IF skout(xx, pv) = 0 THEN lukka = 1: EXIT FOR
NEXT z
IF lukka = 1 THEN
COLOR 10: PRINT kansap(xx); " ";
lux (43)
GOSUB skys
IF sv$ = "k" THEN PRINT "Mies ilmoittaa l„hett„v„ns„ raportin faxilla jo t„n„„n!": skout(xx, pv) = 1
IF sv$ = "e" THEN PRINT "'Kiitos ajastanne' mies tokaisee ja sulkee puhelimen."
END IF

CASE 90, 91
IF kr < 40 THEN
lux (44)
GOSUB skys
PRINT "Grossman purskahtaa toivottomaan itkuun - kukaan ei huoli h„nt„ mukaan."
END IF

CASE 92
skandalpl

CASE 93, 155
IF kr < 40 THEN
fat% = CINT(15 * RND) + 1
erikoisp (fat%)
END IF

CASE 94, 95, 96
arpo
lux (46)
LOCATE CSRLIN - 1, 1
prm (2)
COLOR 2: PRINT "majailee listan toisessa p„„ss„!"

CASE 97, 98, 99
arpo
lux (47)
LOCATE CSRLIN - 1, 1
prm (2)
COLOR 2: PRINT "majailee listan h„nnill„ - ha haa!"

CASE 100
arvoloukkaa (1)
IF lukka = 0 THEN
xx = xxx
printpels
lux (48)
GOSUB skys
SELECT CASE sv$
CASE "k"
PRINT "'Kiitosh' pelaaja mongertaa, ja lupaa my”hemmin parantaa tapansa."
raha(pv) = raha(pv) - 20000
CASE "e"
van% = CINT(10 * RND) + 5
PRINT "Pelaaja purskahtaa itkuun...h„n viett„„ putkassa seuraavat ";
COLOR 10: PRINT LTRIM$(STR$(van%));
COLOR 2: PRINT " kierrosta"
pel(xx, pv).inj = 1000 + van%
END SELECT
END IF

CASE 101
xx = CINT((lpl(pv) - 1) * RND) + 1
IF pel(xx, pv).psk > 1 THEN
printpels
lux (49)
pel(xx, pv).psk = pel(xx, pv).psk - 1
END IF

CASE 102
xx = CINT((lpl(pv) - 1) * RND) + 1
IF pel(xx, pv).psk < 20 THEN
printpels
lux (50)
pel(xx, pv).psk = pel(xx, pv).psk + 1
END IF

CASE 103, 104
IF kr < 40 THEN
IF lpl(pv) < 32 THEN
IF valb(1, pv) + valb(4, pv) + valb(5, pv) >= 9 THEN
z = CINT(20 * RND) + 1
DIM sel(1 TO 1) AS pelaaja
OPEN "data\lakkola.mhz" FOR INPUT AS #1
FOR zz = 1 TO z
INPUT #1, ptemp$, sel(1).nam, sel(1).ppp, sel(1).nat, sel(1).age, sel(1).psk, sel(1).svu, sel(1).sra, sel(1).lah, sel(1).spe
NEXT zz
CLOSE #1
COLOR 10: PRINT ptemp$; " ("; pelip(sel(1).ppp); ") ";
lux (51)
GOSUB skys
IF sv$ = "e" THEN PRINT "Mies kertoo ymm„rt„v„ns„, ja sulkee puhelimen."
IF sv$ = "k" THEN
PRINT "Mies kiitt„„ mahdollisuudesta, ja kertoo saapuvansa pikimmiten!"
lpl(pv) = lpl(pv) + 1
pel(lpl(pv), pv) = sel(1)
END IF
ERASE sel
END IF
END IF
END IF

CASE 105
IF ot > 22 THEN
lux (52)
tre(u(pv)) = tre(u(pv)) - .02
END IF

CASE 106
IF ot > 22 THEN
lux (53)
tre(u(pv)) = tre(u(pv)) + .02
END IF

CASE 107, 108
lux (54)
LOCATE CSRLIN - 3, 58
COLOR 10
PRINT 40000 * halli(u(pv))
raha(pv) = raha(pv) + (40000 * halli(u(pv)))

CASE 109, 110
IF kr < 40 THEN
lukka = 1
FOR temp% = 1 TO 15
xx = CINT((lpl(pv) - 1) * RND) + 1
IF pel(xx, pv).psk >= 13 THEN lukka = 0: EXIT FOR
NEXT temp%
IF lukka = 0 THEN
printpels
lux (55)
GOSUB skys
IF sv$ = "e" THEN PRINT "Daizley kritisoi p„„t”st„si: 'fuckin' idiot'."
IF sv$ = "k" THEN
PRINT "OK. Mies l„htee ja rahaa ropsahtaa tilillesi - jo t„n„„n!"
joukpois = pv
viimpel = xx
ketjutyhjenna
ketjumaar
pelaamaar
raha(pv) = raha(pv) + 500000
END IF
END IF
END IF

CASE 111, 112, 113
lux (56)

CASE 114, 115
lukka = 1
FOR temp% = 1 TO 20
xx = CINT((lpl(pv) - 1) * RND) + 1
IF pel(xx, pv).lah = 99 AND pel(xx, pv).svu = 1 THEN lukka = 0: EXIT FOR
NEXT temp%
IF lukka = 0 THEN
printpels
lux (57)
GOSUB skys
IF sv$ = "e" THEN PRINT "Pelaaja k„velee ulos, hitaasti. Aika on ilmiselv„sti ajanut h„nen ohitseen..."
IF sv$ = "k" THEN
PRINT "Hymy levi„„ miehen kasvoille. 'kiitoss kiitoss kiitossh!' h„n riemuitsee."
pel(xx, pv).svu = pel(xx, pv).svu + 1
END IF
END IF

CASE 116, 117
lukka = 1
FOR temp% = 1 TO 20
xx = CINT((lpl(pv) - 1) * RND) + 1
IF pel(xx, pv).svu = 1 THEN lukka = 0: EXIT FOR
NEXT temp%
IF lukka = 0 THEN
printpels
lux (58)
GOSUB skys
IF sv$ = "e" THEN PRINT "Pelaaja kertoo ymm„rt„v„ns„ n„k”kantasi 'oikein hyvin'."
IF sv$ = "k" THEN PRINT "Pelaaja kiitt„„ sinua l„mpim„sti.": pel(xx, pv).svu = pel(xx, pv).svu + 1
END IF

CASE 118, 119
lukka = 1
FOR temp% = 1 TO 20
xx = CINT((lpl(pv) - 1) * RND) + 1
IF pel(xx, pv).lah = 98 THEN lukka = 0: EXIT FOR
NEXT temp%
IF lukka = 0 THEN
printpels
lux (59)
pel(xx, pv).lah = 0
END IF

CASE 120, 121
lukka = 1
FOR temp% = 1 TO 20
xx = CINT((lpl(pv) - 1) * RND) + 1
IF pel(xx, pv).lah = 97 AND pel(xx, pv).svu > 1 THEN lukka = 0: EXIT FOR
NEXT temp%
IF lukka = 0 THEN
printpels
lux (60)
GOSUB skys
IF sv$ = "e" THEN PRINT "'Min„ muistan t„m„n viel„' pelaaja uhoaa poistuessaan."
IF sv$ = "k" THEN PRINT "'Min„ muistan t„m„n viel„!' pelaaja lupaa poistuessaan.": pel(xx, pv).svu = 1
END IF

CASE 122, 123
lukka = 1
FOR temp% = 1 TO 20
xx = CINT((lpl(pv) - 1) * RND) + 1
IF pel(xx, pv).svu > 1 THEN lukka = 0: EXIT FOR
NEXT temp%
IF lukka = 0 THEN
printpels
lux (61)
GOSUB skys
IF sv$ = "e" THEN PRINT "'No, ei sitten' pelaaja toteaa murheissaan."
IF sv$ = "k" THEN PRINT "Pelaaja kiitt„„ syv„„n niiaten.": pel(xx, pv).svu = 1
END IF

CASE 124, 125
lukka = 1
FOR temp% = 1 TO 20
xx = CINT((lpl(pv) - 1) * RND) + 1
IF pel(xx, pv).lah = 97 THEN lukka = 0: EXIT FOR
NEXT temp%
IF lukka = 0 THEN
printpels
lux (62)
pel(xx, pv).lah = 0
END IF

CASE 126, 127, 141, 142, 143, 144, 145, 146, 147
IF kr < 40 THEN
IF lpl(pv) < 32 THEN erikoisp (28)
END IF

CASE 128, 129, 130, 131, 132, 133, 134, 139, 140
IF kr < 40 THEN
IF lpl(pv) < 32 THEN erikoisp (29)
END IF

CASE 135, 136, 137
uhka(pv) = 1

CASE 138
IF kr < 40 THEN
IF lpl(pv) < 32 THEN erikoisp (30)
END IF

CASE 148, 149
IF mafia(pv) = 1 THEN
lux (63)
mo(pv) = momin(vai(pv))
END IF

CASE 150, 151, 160, 161
IF mafia(pv) = 1 THEN
lux (64)
sovtap(pv) = 1
END IF

CASE 152, 159
IF mafia(pv) = 1 THEN
juke = CINT(4 * RND) + 1
OPEN "data\mafioso.mhm" FOR INPUT AS #1
FOR z = 1 TO juke
LINE INPUT #1, ennus$
NEXT z
CLOSE #1
COLOR 10: PRINT ennus$;
COLOR 2
lux (65)
GOSUB skys
SELECT CASE sv$
CASE "k"
PRINT "'Nyt tied„mme ket„ t„stedes tarkkailla' poliisi toteaa j„„t„v„sti."
IF CINT(100 * RND) > 30 THEN
PRINT "(seuraavana aamuna huomaat tilillesi ilmestyneen 100.000 euroa!)"
raha(pv) = raha(pv) + 100000
END IF
CASE "e"
PRINT "'N„in arvelimmekin' poliisi sanoo. 'Kiitos avustanne!'"
laxa(u(pv)) = 1
muilu(u(pv)) = 1
skanda(u(pv)) = 1
END SELECT
END IF

CASE 500, 501
arvoloukkaa (4)
IF lukka = 0 THEN
xx = xxx
printpels
lux (66)
pel(xx, pv).spe = 8
END IF

CASE 502, 503
arvoloukkaa (5)
IF lukka = 0 THEN
xx = xxx
printpels
lux (67)
pel(xx, pv).ego = 10
END IF

CASE 504, 505
arvoloukkaa (6)
IF lukka = 0 THEN
xx = xxx
printpels
lux (68)
pel(xx, pv).ego = 0
END IF

END SELECT
END IF

pjn
EXIT SUB

skys:
sv$ = ""
DO UNTIL sv$ = "k" OR sv$ = "e"
sv$ = INKEY$
LOOP
RETURN
END SUB

SUB scoutit
kurso = 2: oldkurso = 2
alkusco:
CLS
COLOR 13: PRINT "KYKYJENETSIJŽT ERI MAISSA (";
COLOR 14: PRINT RTRIM$(l(u(pv)));
COLOR 13: PRINT ")"
haahaa (5)
FOR xx = 2 TO 12
COLOR 14
LOCATE xx * 2, 1: printland
NEXT xx
IF krapu(pv) = 2 THEN krapulapiirto
DO
LOCATE oldkurso * 2, 1: xx = oldkurso: COLOR , 0: printland
LOCATE kurso * 2, 1: xx = kurso: COLOR , 4: printland
sco$ = "": WHILE sco$ = "": sco$ = INKEY$: WEND
IF sco$ = "?" THEN qelp (5): GOTO alkusco
IF sco$ = CHR$(0) + CHR$(72) AND kurso > 2 THEN kurso = kurso - 1: oldkurso = kurso + 1
IF sco$ = CHR$(0) + CHR$(80) AND kurso < 12 THEN kurso = kurso + 1: oldkurso = kurso - 1
IF sco$ = CHR$(27) THEN COLOR , 0: EXIT DO
IF sco$ = "p" THEN GOSUB perotascout
LOOP
EXIT SUB

perotascout:
IF skout(kurso, pv) = 0 THEN skout(kurso, pv) = 40 ELSE skout(kurso, pv) = 0
RETURN
END SUB

SUB skandal
skanda(xx) = 0
prm (2)
luz (35)
IF 100 * RND > 30 THEN
IF tautip(xx) = 1 THEN
tautip(xx) = .95
tautik(xx) = CINT(4 * RND) + 2
END IF
potk
EXIT SUB
END IF

IF tautip(xx) = 1 THEN
tautip(xx) = 1.05
tautik(xx) = CINT(4 * RND) + 2
END IF
END SUB

SUB skandalpl
skanda(u(pv)) = 0
luetexti ("sandaali")
PRINT
lukka = CINT(100 * RND)
SELECT CASE lukka
CASE IS >= 70
luetexti ("sandal1")
mo(pv) = mo(pv) + 10
CASE ELSE
luetexti ("sandal2")
mo(pv) = mo(pv) - 10
END SELECT
END SUB

SUB skillmaarbors
zzz = bel(xxx).nat

tasso = CINT(299 * RND) + 1

SELECT CASE tasso

CASE 0 TO pros(zzz, 1)
bel(xxx).psk = CINT(4 * RND) - 2 + taso(zzz, 1)

CASE pros(zzz, 1) + 1 TO pros(zzz, 1) + pros(zzz, 2)
bel(xxx).psk = CINT(4 * RND) - 2 + taso(zzz, 2)

CASE pros(zzz, 1) + pros(zzz, 2) + 1 TO pros(zzz, 1) + pros(zzz, 2) + pros(zzz, 3)
bel(xxx).psk = CINT(4 * RND) - 2 + taso(zzz, 3)

CASE pros(zzz, 1) + pros(zzz, 2) + pros(zzz, 3) + 1 TO pros(zzz, 1) + pros(zzz, 2) + pros(zzz, 3) + pros(zzz, 4)
bel(xxx).psk = CINT(4 * RND) - 2 + taso(zzz, 4)

CASE pros(zzz, 1) + pros(zzz, 2) + pros(zzz, 3) + pros(zzz, 4) + 1 TO pros(zzz, 1) + pros(zzz, 2) + pros(zzz, 3) + pros(zzz, 4) + pros(zzz, 5)
bel(xxx).psk = CINT(4 * RND) - 2 + taso(zzz, 5)

END SELECT
END SUB

SUB skoutmaar (fat%)
FOR xx = 1 TO fat%
SELECT CASE xx
CASE 1
xxx = 2
CASE 2
xxx = 6
CASE 3
xxx = 9
CASE 4
xxx = 5
CASE 5
xxx = 10
CASE 6
xxx = 12
CASE 7
xxx = 11
CASE 8
xxx = 4
CASE 9
xxx = 8
CASE 10
xxx = 3
CASE 11
xxx = 7
END SELECT
skout(xxx, pv) = 1
NEXT xx
END SUB

SUB skreensaver

DO WHILE INKEY$ = ""
FOR zz = 1 TO 43
FOR zzz = 1 TO 76
krappu = CINT(100 * RND) + 1
COLOR (CINT(14 * RND) + 1)
IF krappu = 1 THEN LOCATE zz, zzz: PRINT "MHM99";
NEXT zzz
NEXT zz
LOOP

END SUB

SUB sopimusext (fat%)
oldkurso = kurso
SELECT CASE fat%
CASE 1
neup = pel(xx, ote)
pel(xx, ote).neu = 1
CASE 2
neup = bel(sortb(kurso))
bel(sortb(kurso)).neu = 1
END SELECT

CLS
manap
PRINT ", PYRIT NEUVOTTELUP™YTŽŽN PELAAJAN KANSSA!"
PRINT
actiox(pv) = actiox(pv) - 1
IF neup.nat = 1 THEN COLOR 10 ELSE COLOR 2
temp% = 0
IF neup.nat = 1 THEN temp% = 1
IF neup.nat > 12 THEN temp% = 1
IF neup.nat < 13 THEN
IF neup.nat > 1 AND skout(neup.nat, pv) = 1 THEN temp% = 1
END IF

IF fat% = 1 THEN temp% = 1
SELECT CASE temp%
CASE 1
PRINT neup.nam; " (";
PRINT pelip(neup.ppp); ", ";
PRINT kansal(neup.nat); ", ";
PRINT LTRIM$(STR$(neup.age)); ", ";
PRINT LTRIM$(STR$(neup.psk)); ")"
CASE 0
PRINT neup.nam; " (??)"
END SELECT
IF neup.age >= 26 THEN max% = 4
IF neup.age < 26 THEN
SELECT CASE neup.age - neup.psk
CASE IS <= 10
max% = 2
CASE 11 TO 16
max% = 3
CASE ELSE
max% = 4
END SELECT
END IF

LOCATE 5, 1

SELECT CASE neup.ppp
CASE 1
ccc = 2
CASE ELSE
ccc = 1
END SELECT

IF neup.neu = 1 THEN GOTO eines
IF neup.psk > 5 THEN
IF valb(ccc, pv) + valb(4, pv) + (valb(5, pv) * 2) < neup.psk THEN GOTO eines
IF neup.psk > 6 AND valb(5, pv) < 2 THEN GOTO eines
IF neup.psk > 9 AND valb(5, pv) < 3 THEN GOTO eines
END IF
COLOR 13: PRINT "MONENKO VUODEN SOPIMUS ON MIELESSŽSI?"
kurso = 1

DO
FOR zz = 1 TO max%
LOCATE 7, 1 + (zz - 1) * 2
IF kurso = zz THEN COLOR 14, 1 ELSE COLOR 14, 0
PRINT LTRIM$(STR$(zz));
NEXT zz
sop$ = "": WHILE sop$ = "": sop$ = INKEY$: WEND
IF sop$ = CHR$(0) + CHR$(75) AND kurso > 1 THEN kurso = kurso - 1
IF sop$ = CHR$(0) + CHR$(77) AND kurso < max% THEN kurso = kurso + 1
IF sop$ = CHR$(13) THEN EXIT DO
LOOP
COLOR , 0
sopvplus = kurso
palkmaar (2)
GOSUB palkkavaatim

kurso = 1
ccc = 0
DO
LOCATE 11, 1
IF kurso = 1 THEN COLOR , 1 ELSE COLOR , 0
IF ccc = 3 THEN COLOR 6 ELSE COLOR 14
PRINT "KŽTTŽ PŽŽLLE JA NIMEŽ PAPERIIN!"
IF kurso = 2 THEN COLOR , 1 ELSE COLOR , 0
IF ccc = 0 THEN COLOR 14 ELSE COLOR 6
PRINT "MIETTISITK™ UUDESTAAN?"
IF kurso = 3 THEN COLOR 14, 1 ELSE COLOR 14, 0
PRINT "UNOHDA KOKO HOMMA!"
COLOR , 0
sop$ = "": WHILE sop$ = "": sop$ = INKEY$: WEND
IF sop$ = CHR$(0) + CHR$(72) AND kurso > 1 THEN kurso = kurso - 1
IF sop$ = CHR$(0) + CHR$(80) AND kurso < 3 THEN kurso = kurso + 1
IF sop$ = CHR$(13) THEN
IF kurso = 3 AND fat% = 2 THEN EXIT SUB ELSE IF kurso = 3 THEN EXIT DO
IF kurso = 2 AND ccc = 0 THEN GOSUB uusitarjous
IF kurso = 1 AND ccc <> 3 THEN EXIT DO
END IF
LOOP

SELECT CASE fat%
CASE 1
SELECT CASE kurso
CASE 1
IF ote = pv THEN
kurso = oldkurso
pel(xx, ote).svu = sopvplus
pel(xx, ote).sra = palkehd
pel(xx, ote).neu = 0
END IF
IF ote <> pv THEN sopkilpailu
EXIT SUB
CASE 3
kurso = oldkurso
EXIT SUB
END SELECT

CASE 2
ccc = lpl(pv) + 1
pel(ccc, pv) = bel(sortb(oldkurso))
pel(ccc, pv).svu = sopvplus
pel(ccc, pv).sra = palkehd
lpl(pv) = ccc
IF sortb(oldkurso) < lastbors THEN xx = oldkurso: porsmaar
kurso = oldkurso
kurso = kurso - 1
IF kurso = 0 THEN kurso = 1
lastbors = lastbors - 1
END SELECT
EXIT SUB

uusitarjous:
temp% = CINT(99 * RND) + 1
SELECT CASE temp%
CASE 0 TO 20
palkehd = FIX(.92 * palkehd)
IF 100 * RND < 50 THEN ccc = 1
GOSUB palkkavaatim
CASE 81 TO 100
palkehd = FIX(1.08 * palkehd)
IF 100 * RND < 30 THEN ccc = 1
GOSUB palkkavaatim
END SELECT
IF 100 * RND < 20 THEN ccc = 3
RETURN

palkkavaatim:
LOCATE 9, 1
COLOR 13: PRINT "PALKKAVAATIMUS: ";
COLOR 14: PRINT LTRIM$(STR$(palkehd)); " ";
RETURN

eines:
COLOR 13: PRINT "PELAAJA ILMOITTAA TYLYSTI ETTEI HALUA EDES KESKUSTELLA JOUKKUEESSASI"
PRINT "PELAAMISESTA, JA SULKEE PUHELIMEN."
pjn
EXIT SUB

END SUB

SUB sopkilpailu
lukka = 1

highest = palkehd

sota1:
klearstat (15)
LOCATE 15, 1
COLOR 13: PRINT "MANAGERI ";
ccc = ote: colchk4
PRINT nimi(ote);
COLOR 13: PRINT ", VASTAATKO TARJOUKSEEN?"
GOSUB nokkiin
IF lukka = 0 THEN GOTO sotaohi

klearstat (15)
LOCATE 19, 1
COLOR 13: PRINT "MANAGERI ";
ccc = pv: colchk4
PRINT nimi(pv);
COLOR 13: PRINT ", NOKITATKO?"
GOSUB nokkiin
IF lukka = 0 THEN GOTO sotaohi
GOTO sota1

sotaohi:
palkehd = highest
SELECT CASE ccc
CASE pv
pel(xx, ote).svu = sopvplus
pel(xx, ote).sra = palkehd
pel(xx, ote).neu = 0
CASE ote
pel(lpl(pv), pv) = pel(xx, ote)
pel(lpl(pv), pv).svu = sopvplus
pel(lpl(pv), pv).sra = palkehd
pel(lpl(pv), pv).neu = 0
pel(lpl(pv), pv).pok = 0
joukpois = ote
viimpel = xx
ketjutyhjenna
ketjumaar
pelaamaar
END SELECT
EXIT SUB

nokkiin:
temp% = CSRLIN + 1
tarjous = highest
DO
LOCATE temp%, 1
IF tarjous = highest THEN COLOR 4: PRINT "JŽTŽN VŽLIIN...       ";
IF tarjous > highest THEN
COLOR 12: PRINT "TARJOAN ";
COLOR 14: PRINT LTRIM$(STR$(tarjous)); "     ";
END IF
sop$ = "": WHILE sop$ = "": sop$ = INKEY$: WEND
IF sop$ = CHR$(0) + CHR$(72) THEN tarjous = tarjous + 50
IF sop$ = CHR$(0) + CHR$(80) AND tarjous > highest THEN tarjous = tarjous - 50
IF sop$ = CHR$(13) THEN EXIT DO
LOOP
IF tarjous = highest THEN lukka = 0 ELSE highest = tarjous
RETURN

END SUB

SUB sopuprint
IF sopu(u(pv)) = 0 THEN COLOR 4 ELSE COLOR 20
LOCATE 6, 55
PRINT "EI";
END SUB

SUB sortbors (paikka%)

SELECT CASE paikka%
CASE 1
xxx = 1
FOR z = 1 TO 5
FOR zz = 1 TO 15
FOR zzzz = 1 TO lastbors
GOSUB nacyyco
IF nakyy = 1 AND bel(zzzz).nat = zz AND bel(zzzz).ppp = z THEN sortb(xxx) = zzzz: xxx = xxx + 1
NEXT zzzz
NEXT zz
NEXT z

FOR zzzz = 1 TO lastbors
GOSUB nacyyco
IF nakyy = 0 THEN sortb(xxx) = zzzz: xxx = xxx + 1
NEXT zzzz

CASE 2
sortero = 1
FOR zz = 1 TO 5
FOR ccc = 20 TO 1 STEP -1
FOR z = 1 TO lpl(borsch)
IF pel(z, borsch).ppp = zz AND pel(z, borsch).psk = ccc THEN sortc(sortero) = z: sortero = sortero + 1
NEXT z
NEXT ccc
NEXT zz

END SELECT
EXIT SUB

nacyyco:
IF bel(zzzz).nat < 13 THEN
IF skout(bel(zzzz).nat, pv) = 1 THEN nakyy = 1 ELSE nakyy = 0
END IF
IF bel(zzzz).nat > 12 THEN nakyy = 1
RETURN

END SUB

SUB sortmaar
ERASE sort
ERASE sort2
pelaa = 0
IF lpl(pv) = 0 THEN EXIT SUB
FOR ooo = 1 TO lpl(pv)
IF pel(ooo, pv).inj = 0 THEN pelaa = pelaa + 1
NEXT ooo
DIM sort(1 TO lpl(pv)) AS INTEGER
IF pelaa = 0 THEN EXIT SUB
DIM sort2(1 TO pelaa) AS INTEGER
END SUB

SUB sortplayers
sortero = 1
sortero2 = 1

FOR temp% = 1 TO 5
FOR ccc = 20 TO 1 STEP -1
FOR xx = 1 TO lpl(pv)
IF pel(xx, pv).ppp = temp% AND pel(xx, pv).psk = ccc THEN
sort(sortero) = xx
sortero = sortero + 1
IF pel(xx, pv).inj = 0 THEN sort2(sortero2) = xx: sortero2 = sortero2 + 1
END IF
NEXT xx
NEXT ccc
NEXT temp%

END SUB

SUB specmaar
ccc = CINT(100 * RND)
IF ccc > 40 THEN spec(pv) = CINT(9 * RND) + 1 ELSE spec(pv) = 0

END SUB

SUB sponsorit
walhe:
DIM spo(3) AS INTEGER
DIM spr(1 TO 3, 1 TO 8) AS LONG
DIM sponsor(1 TO 37) AS STRING
DIM sponsal(1 TO 37) AS INTEGER
DIM ala(1 TO 9) AS STRING

OPEN "data\data4.99" FOR INPUT AS #1
FOR zz = 1 TO 37
INPUT #1, sponsor(zz), sponsal(zz)
NEXT zz
FOR zz = 1 TO 9
INPUT #1, ala(zz)
NEXT zz
CLOSE #1

vaihtis = 1
IF sed(u(pv)) > 12 THEN max% = 1 ELSE IF sed(u(pv)) > 3 THEN max% = 2 ELSE max% = 3

FOR zz = 1 TO 3
spo(zz) = CINT(36 * RND) + 1
NEXT zz
jaa = 1

joo = 12.5
juu = (sed(u(pv)) + sedd(u(pv)) + seddd(u(pv))) / 3

SELECT CASE juu
CASE IS < joo
jaa = jaa + ((joo - juu) * .035)
CASE IS > joo
jaa = jaa - ((juu - joo) * .04)
END SELECT

spr(1, 1) = jaa * 65000
spr(1, 2) = spr(1, 1) / 2
spr(1, 3) = spr(1, 1) / 3
spr(1, 4) = spr(1, 1) / 4
spr(1, 5) = spr(1, 1) / 5
spr(1, 6) = spr(1, 1) / 30
spr(1, 7) = spr(1, 1) / 8
spr(1, 8) = spr(1, 1) / 10

spr(2, 1) = jaa * 50000
spr(2, 2) = spr(2, 1) * 6.2
spr(2, 3) = spr(2, 1) * 5.9
spr(2, 4) = spr(2, 1) * 4
spr(2, 5) = spr(2, 1) * 3.5
spr(2, 6) = spr(2, 1) / 1.7
spr(2, 7) = spr(2, 1) * 2.6
spr(2, 8) = spr(2, 1) * 2.3

spr(3, 1) = jaa * 10000
spr(3, 2) = spr(3, 1) * 140
spr(3, 3) = spr(3, 1) * 130
spr(3, 4) = spr(3, 1) * 110
spr(3, 5) = spr(3, 1) * 80
spr(3, 6) = spr(3, 1) * 3
spr(3, 7) = spr(3, 1) * 24
spr(3, 8) = spr(3, 1) * 22

FOR xx = 1 TO 3
FOR xxx = 1 TO 8
spr(xx, xxx) = spr(xx, xxx) * stingy(pv)
NEXT xxx
NEXT xx

DO
CLS
haahaa (12)
LOCATE 1, 1
manap
PRINT
PRINT
COLOR 2
PRINT "SPONSORINEUVOTTELUT ";
COLOR 10
PRINT "(VAIHTOEHTO "; LTRIM$(STR$(vaihtis)); "/"; LTRIM$(STR$(max%)); ") ";
IF vaihtis > 1 THEN PRINT CHR$(17);
IF vaihtis < max% THEN PRINT CHR$(16);
PRINT
PRINT
COLOR 13
PRINT "PŽŽSPONSORIEHDOKAS  ";
COLOR 14
PRINT sponsor(spo(vaihtis))
COLOR 13
PRINT
PRINT "EHDOKKAAN ALA       ";
COLOR 14
PRINT ala(sponsal(spo(vaihtis)))
PRINT : COLOR 13
PRINT "SPONSORIN TARJOUS ON SEURAAVA"
PRINT
FOR xxx = 1 TO 8
temp% = 0
IF xxx = 2 AND muke(pv) = 0 THEN temp% = 1
IF xxx = 7 AND muke(pv) = 0 THEN temp% = 1
IF xxx = 3 AND sr(u(pv)) = 2 THEN temp% = 1
IF xxx = 4 AND sr(u(pv)) = 2 THEN temp% = 1
IF xxx = 5 AND sr(u(pv)) = 2 THEN temp% = 1
IF temp% = 0 THEN
COLOR 7: PRINT sponsax(xxx);
LOCATE , 32
COLOR 15: PRINT LTRIM$(STR$(spr(vaihtis, xxx)));
IF xxx = 1 THEN COLOR 13: LOCATE , 40: PRINT "   YHTEENSŽ "; : COLOR 14: PRINT LTRIM$(STR$(spr(vaihtis, 1) * 44));
IF xxx = 6 THEN COLOR 13: LOCATE , 40: PRINT "    MAKSIMI "; : COLOR 14: PRINT LTRIM$(STR$(spr(vaihtis, 6) * 44));
PRINT
PRINT
END IF
NEXT xxx
PRINT
COLOR 14: luetexti ("spoku" + LTRIM$(STR$(vaihtis)))
spon$ = "": WHILE spon$ = "": spon$ = INKEY$: WEND
IF spon$ = CHR$(0) + CHR$(75) AND vaihtis > 1 THEN vaihtis = vaihtis - 1
IF spon$ = CHR$(0) + CHR$(77) AND vaihtis < max% THEN vaihtis = vaihtis + 1
IF spon$ = CHR$(13) THEN EXIT DO
LOOP
snimi(pv) = sponsor(spo(vaihtis))
FOR zz = 1 TO 8
sraha(zz, pv) = spr(vaihtis, zz)
NEXT zz
ERASE spo, spr, sponsor, sponsal, ala
END SUB

