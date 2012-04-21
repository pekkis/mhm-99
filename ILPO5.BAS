DECLARE SUB aaaargh ()
DECLARE SUB arpo ()
DECLARE SUB arpod (voiz%)
DECLARE SUB arpods (sij%)
DECLARE SUB arpol (voiz%)
DECLARE SUB arpol2 (voiz%)
DECLARE SUB arpols2 (sij%)
DECLARE SUB arvoloukkaa (fat%)
DECLARE SUB colchk ()
DECLARE SUB colchk2 ()
DECLARE SUB colchk4 ()
DECLARE SUB euromaar ()
DECLARE SUB manap ()
DECLARE SUB haahaa (haha%)
DECLARE SUB krapulapiirto ()
DECLARE SUB luetexti (lue$)
DECLARE SUB lentti (sano!)
DECLARE SUB komme (sano!)
DECLARE SUB klearstat (fat%)
DECLARE SUB laxati ()
DECLARE SUB luz (fat%)
DECLARE SUB maaliero ()
DECLARE SUB maalieroehl ()
DECLARE SUB maalierotur ()
DECLARE SUB ottelmaarhaarax ()
DECLARE SUB naytaott ()
DECLARE SUB ottpel ()
DECLARE SUB ottul ()
DECLARE SUB muilutus (fat%)
DECLARE SUB naytaottur ()
DECLARE SUB nollaux ()
DECLARE SUB nollaux2 ()
DECLARE SUB nollauxtauti ()
DECLARE SUB otmuuttur ()
DECLARE SUB ottelmaar ()
DECLARE SUB pesc ()
DECLARE SUB qelp (qel%)
DECLARE SUB pjn ()
DECLARE SUB printpels ()
DECLARE SUB poispel ()
DECLARE SUB protesti ()
DECLARE SUB prm (juu%)
DECLARE SUB potk ()
DECLARE SUB pporsvoit (gr%)
DECLARE SUB skandal ()
DECLARE SUB tarka (jez%)
DECLARE SUB staulturmaar ()
DECLARE SUB teet (jup%, jupp%)
DECLARE SUB veiknayta ()

'$DYNAMIC
'$INCLUDE: 'mhm99.bi'


SUB statistiikka
DIM mital(1 TO 3) AS INTEGER, hassu(1 TO 4) AS INTEGER
DIM stat$(1 TO 12)

DIM msij(1 TO 24) AS INTEGER
DIM msij2(1 TO 24) AS INTEGER
DIM tgraf(1 TO 44) AS INTEGER

FOR xx = 1 TO 24
msij(xx) = 1
NEXT xx

FOR xx = 1 TO 23
FOR xxx = xx + 1 TO 24
SELECT CASE tmes(x(xx))
CASE IS < tmes(x(xxx))
msij(x(xx)) = msij(x(xx)) + 1
CASE IS > tmes(x(xxx))
msij(x(xxx)) = msij(x(xxx)) + 1
CASE IS = tmes(x(xxx))
SELECT CASE thop(x(xx))
CASE IS < thop(x(xxx))
msij(x(xx)) = msij(x(xx)) + 1
CASE IS > thop(x(xxx))
msij(x(xxx)) = msij(x(xxx)) + 1
CASE IS = thop(x(xxx))
SELECT CASE tpro(x(xx))
CASE IS < tpro(x(xxx))
msij(x(xx)) = msij(x(xx)) + 1
CASE IS > tpro(x(xxx))
msij(x(xxx)) = msij(x(xxx)) + 1
CASE IS = tpro(x(xxx))
SELECT CASE tkausl(x(xx))
CASE IS < tkausl(x(xxx))
msij(x(xx)) = msij(x(xx)) + 1
CASE IS > tkausl(x(xxx))
msij(x(xxx)) = msij(x(xxx)) + 1
CASE IS = tkausl(x(xxx))
msij(x(xxx)) = msij(x(xxx)) + 1
END SELECT
END SELECT
END SELECT
END SELECT
NEXT xxx
NEXT xx

FOR xx = 1 TO 24
msij2(msij(xx)) = xx
NEXT xx

OPEN "data\statex.99" FOR INPUT AS #1
FOR z = 1 TO 12
INPUT #1, stat$(z)
NEXT z
CLOSE #1

CLS
grunt% = 1
curso = 1
kurso = 1
oldkurso = kausi - 1

alasta:
LOCATE 1, 1
IF krapu(pv) = 2 THEN curso = CINT(10 * RND) + 1
FOR z = 1 TO 12
IF z = 8 THEN LOCATE 2, 1
IF z < 9 THEN COLOR 8 ELSE COLOR 1
IF curso = z THEN COLOR , 15 ELSE COLOR , 0
PRINT stat$(z);
COLOR , 0: PRINT "  ";
NEXT z
LOCATE 4, 1
SELECT CASE curso
CASE 1
GOSUB henkkoht
CASE 2
GOSUB personal
CASE 4
GOSUB mitalit
CASE 5
GOSUB prorele
CASE 7
GOSUB rekordholders
CASE 8
GOSUB maraton
CASE IS > 8
GOSUB pistepvoit
END SELECT

DO
haahaa (11)
COLOR , 0
st$ = ""
IF curso = 3 THEN GOSUB omjouk
IF curso = 6 THEN GOSUB runkos
WHILE st$ = "": st$ = INKEY$: WEND
IF st$ = "?" THEN qelp (11): CLS : GOTO alasta
IF curso = 3 THEN
IF st$ = CHR$(0) + CHR$(72) AND kurso > 1 THEN kurso = kurso - 1
IF st$ = CHR$(0) + CHR$(80) AND kurso < 24 THEN kurso = kurso + 1
END IF
IF curso = 6 AND kausi > 1999 THEN
IF st$ = CHR$(0) + CHR$(72) AND oldkurso > 1998 THEN oldkurso = oldkurso - 1
IF st$ = CHR$(0) + CHR$(80) AND oldkurso < kausi - 1 THEN oldkurso = oldkurso + 1
END IF
IF curso = 6 THEN
IF st$ = "1" THEN grunt% = 1
IF st$ = "2" THEN grunt% = 2
END IF

IF st$ = CHR$(0) + CHR$(75) AND curso > 1 THEN curso = curso - 1: CLS : GOTO alasta
IF st$ = CHR$(0) + CHR$(77) AND curso < 12 THEN curso = curso + 1: CLS : GOTO alasta
IF st$ = CHR$(27) THEN EXIT DO
LOOP
ERASE mital, hassu, stat$, msij, msij2, tgraf
EXIT SUB

maraton:
LOCATE 4, 1
COLOR 13: PRINT "JOUKKUE      ";
COLOR 5: PRINT "PHL-K.      ";
COLOR 14: PRINT "K      ";
COLOR 7: PRINT "H      ";
COLOR 6: PRINT "P      ";

LOCATE 6, 1
FOR xx = 1 TO 24
IF sr(msij2(xx)) = 1 THEN COLOR 10 ELSE COLOR 2

IF ohj(msij2(xx)) <> 0 THEN ccc = ohj(msij2(xx)): colchk4

PRINT l(msij2(xx));
LOCATE , 16: PRINT LTRIM$(STR$(tkausl(msij2(xx))));
LOCATE , 26: PRINT LTRIM$(STR$(tmes(msij2(xx))));
LOCATE , 33: PRINT LTRIM$(STR$(thop(msij2(xx))));
LOCATE , 40: PRINT LTRIM$(STR$(tpro(msij2(xx))))


NEXT xx
RETURN


runkos:
IF kausi = 1998 THEN LOCATE 4, 1: COLOR 15: PRINT "ENSIMMŽINEN TILASTOITAVA KAUSI MENEILLŽŽN!": RETURN

OPEN "game_" + LTRIM$(STR$(lokero)) + "\" + LTRIM$(STR$(oldkurso)) + ".gr" + LTRIM$(STR$(pv)) FOR INPUT AS #1
INPUT #1, gruuh%
FOR xxx = 1 TO 44
INPUT #1, tgraf(xxx)
NEXT xxx
CLOSE #1

klearstat (4)
haahaa (11)
LOCATE 4, 1: COLOR 15: PRINT "RUNKOSARJAT, KAUSI "; LTRIM$(STR$(oldkurso)); "-"; LTRIM$(STR$(oldkurso + 1)); "  ";
COLOR 2
PRINT "1: ";
COLOR 10
PRINT "LYHYT  ";
COLOR 2
PRINT "2: ";
COLOR 10
PRINT "PITKŽ"

PRINT
OPEN "game_" + LTRIM$(STR$(lokero)) + "\" + LTRIM$(STR$(oldkurso)) + ".tbl" FOR INPUT AS #1
FOR zz = 1 TO 24
INPUT #1, temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9
SELECT CASE zz
CASE 1 TO 8
COLOR 10
CASE 9 TO 11
COLOR 2
CASE 12
COLOR 4
CASE 13 TO 18
COLOR 13
CASE ELSE
COLOR 5
END SELECT
IF temp9 <> 0 THEN ccc = temp9: colchk4
PRINT l(temp1); " "; LTRIM$(STR$(temp2));
LOCATE , 16: PRINT LTRIM$(STR$(temp3));
LOCATE , 19: PRINT LTRIM$(STR$(temp4));
LOCATE , 22: PRINT LTRIM$(STR$(temp5));
IF grunt% = 2 THEN
LOCATE , 29: PRINT LTRIM$(STR$(temp6));
LOCATE , 33: PRINT "-";
LOCATE , 35: PRINT LTRIM$(STR$(temp7));
END IF
SELECT CASE grunt%
CASE 1
LOCATE , 27
CASE 2
LOCATE , 43
END SELECT
PRINT LTRIM$(STR$(temp8))
IF zz = 8 OR zz = 11 OR zz = 18 THEN
COLOR 8: PRINT "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ";
IF grunt% = 2 THEN PRINT "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" ELSE PRINT
END IF
IF zz = 12 THEN PRINT
NEXT zz
CLOSE #1
IF grunt% = 1 THEN

COLOR 14
FOR xxx = 12 TO 1 STEP -1
LOCATE 32 - (24 - (xxx * 2)), 32
PRINT LTRIM$(STR$(xxx));
NEXT xxx

temp% = 1
FOR xx = 1 TO 44
SELECT CASE gruuh%
CASE 1
IF tgraf(xx) < 9 THEN COLOR 10 ELSE IF tgraf(xx) < 12 THEN COLOR 2 ELSE COLOR 4
CASE 2
IF tgraf(xx) < 7 THEN COLOR 13 ELSE COLOR 5
END SELECT
FOR xxx = 12 TO tgraf(xx) STEP -1
LOCATE 33 - (24 - (xxx * 2)), xx + 34
IF temp% = 1 THEN PRINT CHR$(177);  ELSE PRINT CHR$(176);
LOCATE 32 - (24 - (xxx * 2)), xx + 34
IF temp% = 1 THEN PRINT CHR$(177);  ELSE PRINT CHR$(176);
NEXT xxx
IF temp% = 1 THEN temp% = 2 ELSE temp% = 1
NEXT xx
END IF
RETURN

henkkoht:
voittoprosphl = 0
voittoprosdiv = 0
IF ottephl(pv) > 0 THEN
voittoprosphl = voittoprosphl + voittophl(pv)
voittoprosphl = voittoprosphl + (.5 * tasanphl(pv))
voittoprosphl = (voittoprosphl / ottephl(pv)) * 100
END IF
IF ottediv(pv) > 0 THEN
voittoprosdiv = voittoprosdiv + voittodiv(pv)
voittoprosdiv = voittoprosdiv + (.5 * tasandiv(pv))
voittoprosdiv = (voittoprosdiv / ottediv(pv)) * 100
END IF

COLOR 13, 0
PRINT "NIMI ";
COLOR 14: PRINT nimi(pv);
LOCATE , 30: COLOR 13: PRINT "KOTIMAA ";
COLOR 14: PRINT kansap(kans(pv))
PRINT
PRINT
COLOR 13: PRINT "PHL-OTTELUT (RS/PF)   ";
COLOR 14: PRINT LTRIM$(STR$(ottephl(pv))); "/"; LTRIM$(STR$(ottepof(pv)))
PRINT
COLOR 13: PRINT "  VOITTOJA RS  ";
COLOR 14: PRINT LTRIM$(STR$(voittophl(pv)))
COLOR 13: PRINT "TASAPELEJŽ RS  ";
COLOR 14: PRINT LTRIM$(STR$(tasanphl(pv)))
COLOR 13: PRINT " TAPPIOITA RS  ";
COLOR 14: PRINT LTRIM$(STR$(tappiophl(pv)))
COLOR 13: PRINT "     VOITTO-%  ";
COLOR 14
IF ottephl(pv) = 0 THEN PRINT "-" ELSE PRINT LTRIM$(STR$(voittoprosphl))
PRINT
PRINT
COLOR 13: PRINT "DIV-OTTELUT (RS/PF)   ";
COLOR 14: PRINT LTRIM$(STR$(ottediv(pv))); "/"; LTRIM$(STR$(ottedof(pv)))
PRINT
COLOR 13: PRINT "  VOITTOJA RS  ";
COLOR 14: PRINT LTRIM$(STR$(voittodiv(pv)))
COLOR 13: PRINT "TASAPELEJŽ RS  ";
COLOR 14: PRINT LTRIM$(STR$(tasandiv(pv)))
COLOR 13: PRINT " TAPPIOITA RS  ";
COLOR 14: PRINT LTRIM$(STR$(tappiodiv(pv)))
COLOR 13: PRINT "     VOITTO-%  ";
COLOR 14
IF ottediv(pv) = 0 THEN PRINT "-" ELSE PRINT LTRIM$(STR$(voittoprosdiv))
PRINT
PRINT
COLOR 13: PRINT "EHL-OTTELUT           ";
COLOR 14: PRINT LTRIM$(STR$(otteehl(pv)))
PRINT
COLOR 13: PRINT "EUROOPAN MESTARUUKSIA ";
COLOR 14: PRINT LTRIM$(STR$(euros(pv)))
PRINT
PRINT
COLOR 13: PRINT "KULTAA      ";
COLOR 14: PRINT LTRIM$(STR$(ykkos(pv)))
COLOR 13: PRINT "HOPEAA      ";
COLOR 14: PRINT LTRIM$(STR$(kakkos(pv)))
COLOR 13: PRINT "PRONSSIA    ";
COLOR 14: PRINT LTRIM$(STR$(kolmos(pv)))
COLOR 13: PRINT "NOUSUJA     ";
COLOR 14: PRINT LTRIM$(STR$(nousus(pv)))
COLOR 13: PRINT "PUTOAMISIA  ";
COLOR 14: PRINT LTRIM$(STR$(putos(pv)))
RETURN

omjouk:
COLOR 14
FOR xx = 1 TO 24
IF sr(x(xx)) = 1 THEN tempus = 1 ELSE tempus = 2
LOCATE 2 + xx + tempus, 1: COLOR , 0: PRINT RTRIM$(l(x(xx)))
NEXT xx
IF sr(x(kurso)) = 1 THEN tempus = 1 ELSE tempus = 2
LOCATE 2 + kurso + tempus, 1: COLOR , 13: PRINT RTRIM$(l(x(kurso)))
COLOR 13, 0
LOCATE 4, 35: PRINT "KAUSIA PHL:SSŽ: ";
COLOR 14
PRINT LTRIM$(STR$(tkausl(x(kurso)))); "  "
COLOR 13
LOCATE 6, 35: PRINT "KAUSIA DIVARISSA: ";
COLOR 14
PRINT LTRIM$(STR$(tkausd(x(kurso)))); "  "
COLOR 13
LOCATE 8, 35: PRINT "EUROOPAN MESTARUUKSIA: ";
COLOR 14
PRINT LTRIM$(STR$(temes(x(kurso)))); "  "
COLOR 13
LOCATE 10, 35: PRINT "MESTARUUKSIA: ";
COLOR 14
PRINT LTRIM$(STR$(tmes(x(kurso)))); "  "
COLOR 13
LOCATE 12, 35: PRINT "HOPEAA: ";
COLOR 14
PRINT LTRIM$(STR$(thop(x(kurso)))); "  "
COLOR 13
LOCATE 14, 35: PRINT "PRONSSIA: ";
COLOR 14
PRINT LTRIM$(STR$(tpro(x(kurso)))); "  "
COLOR 13
LOCATE 16, 35: PRINT "MANAGERI: ";
COLOR 10
PRINT mana(man(x(kurso))); "           ";
COLOR 13
LOCATE 18, 35: PRINT "YLEIS™KESKIARVO: ";
COLOR 14
IF kotiot(x(kurso)) = 0 THEN PRINT "EI KOTIOTTELUITA";  ELSE PRINT LTRIM$(STR$(FIX(ylek(x(kurso)) / kotiot(x(kurso))))); "               ";
RETURN

mitalit:
COLOR 12: PRINT "KAUSI";
COLOR 14: PRINT "       KULTAA          ";
COLOR 7: PRINT "HOPEAA          ";
COLOR 6: PRINT "PRONSSIA        ";
COLOR 15: PRINT "EUR. MESTARI"
PRINT
OPEN "game_" + LTRIM$(STR$(lokero)) + "\1.sta" FOR INPUT AS #1
DO UNTIL EOF(1)
INPUT #1, temp, mital(1), hassu(1), mital(2), hassu(2), mital(3), hassu(3), jepjepjep$, hassu(4)
COLOR 13: PRINT temp; "      ";
IF hassu(1) = 0 THEN COLOR 2 ELSE ccc = hassu(1): colchk4
PRINT l(mital(1)); "      ";
IF hassu(2) = 0 THEN COLOR 2 ELSE ccc = hassu(2): colchk4
PRINT l(mital(2)); "      ";
IF hassu(3) = 0 THEN COLOR 2 ELSE ccc = hassu(3): colchk4
PRINT l(mital(3)); "      ";
IF hassu(4) = 0 THEN COLOR 2 ELSE ccc = hassu(4): colchk4
PRINT jepjepjep$
IF CSRLIN = 42 THEN pjn: klearstat (6)
LOOP
CLOSE #1
RETURN

personal:
COLOR 13: PRINT "MENESTYKSESI KAUSI KAUDELTA, MANAGERI ";
colchk
PRINT nimi(pv)
PRINT
IF kausi = 1998 THEN COLOR 14: PRINT "ENSIMMŽINEN KAUTESI PHL:SSŽ ON VIELŽ KESKEN!": RETURN
OPEN "game_" + LTRIM$(STR$(lokero)) + "\stat." + LTRIM$(STR$(pv)) FOR INPUT AS #1
DO UNTIL EOF(1)
IF POS(0) > 1 THEN PRINT
INPUT #1, temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10, temp11, temp12
COLOR 12
PRINT LTRIM$(STR$(temp1 - 1)); "-"; LTRIM$(STR$(temp1)); "   ";

IF temp11 < 13 THEN COLOR 10 ELSE COLOR 2
PRINT l(temp2); " ";
PRINT LTRIM$(STR$(temp4));
IF temp4 < 10 THEN PRINT "  ";  ELSE PRINT " ";
PRINT LTRIM$(STR$(temp5));
IF temp5 < 10 THEN PRINT "  ";  ELSE PRINT " ";
PRINT LTRIM$(STR$(temp6));
IF temp6 < 10 THEN PRINT "  ";  ELSE PRINT " ";
IF temp7 < 10 THEN PRINT "   ";  ELSE IF temp7 < 100 THEN PRINT "  ";  ELSE PRINT " ";
PRINT LTRIM$(STR$(temp7));
PRINT "-";
PRINT LTRIM$(STR$(temp8));
IF temp8 < 10 THEN PRINT "   ";  ELSE IF temp8 < 100 THEN PRINT "  ";  ELSE PRINT " ";
PRINT " ";
PRINT LTRIM$(STR$(temp9));
PRINT " (";
PRINT LTRIM$(STR$(temp10)); ")";
LOCATE , 52
IF temp12 > 4 AND temp12 < 9 THEN PRINT "1/4-FINAALI"
IF temp12 = 4 THEN PRINT "NELJŽS"
IF temp12 = 3 THEN PRINT "PRONSSIA"
IF temp12 = 2 THEN PRINT "HOPEAA"
IF temp12 = 1 THEN PRINT "KULTAA"
IF temp11 < 13 AND temp12 > 12 THEN COLOR 2: PRINT "PUTOAMINEN DIVARIIN"
IF temp11 > 12 AND temp12 < 13 THEN COLOR 10: PRINT "NOUSU LIIGAAN!"
IF temp11 > 12 AND temp12 = 13 THEN PRINT "KARSINNAN FINAALI"
IF temp11 > 12 AND temp12 > 13 AND temp12 < 16 THEN PRINT "KARSINNAN SEMIFINAALI"
IF temp12 > 15 AND temp12 < 19 THEN PRINT "KARSINNAN 1/4-FINAALI"
IF CSRLIN = 42 THEN pjn: klearstat (6)
LOOP
CLOSE #1
RETURN

rekordholders:
COLOR 13: PRINT "RUNKOSARJAN PISTE-ENNŽTYS, PHL   ";
COLOR 10
PRINT l(pisenj(1)); "  "; pisenp(1)
PRINT
COLOR 13: PRINT "RUNKOSARJAN PISTE-ENNŽTYS, DIV.  ";
COLOR 10
PRINT l(pisenj(2)); "  "; pisenp(2)
PRINT
PRINT
FOR zz = 1 TO 4
COLOR 13, 0
SELECT CASE zz
CASE 1
PRINT "PHL, RUNKOSARJA  ";
CASE 2
PRINT "DIV, RUNKOSARJA  ";
CASE 3
PRINT "PHL, PLAY-OFFIT  ";
CASE 4
PRINT "DIV, PLAY-OFFIT  ";
END SELECT
COLOR 10
PRINT rekord(zz).na;
COLOR 2
PRINT l(rekord(zz).jo); " ";
COLOR 14
IF rekord(zz).ma < 10 THEN PRINT " ";
PRINT LTRIM$(STR$(rekord(zz).ma));
PRINT " + ";
PRINT LTRIM$(STR$(rekord(zz).sy));
IF rekord(zz).sy < 10 THEN PRINT " ";
PRINT " = "; LTRIM$(STR$(rekord(zz).pi))
PRINT
NEXT zz
RETURN

pistepvoit:
pporsvoit (curso - 8)
RETURN

prorele:
COLOR 12: PRINT "KAUSI";
COLOR 14: PRINT "       NOUSIJA         ";
COLOR 7: PRINT "PUTOAJA         "
PRINT
OPEN "game_" + LTRIM$(STR$(lokero)) + "\3.sta" FOR INPUT AS #1
DO UNTIL EOF(1)
INPUT #1, temp, mital(1), hassu(1), mital(2), hassu(2)
COLOR 13: PRINT temp; "      ";
IF hassu(1) = 0 THEN COLOR 2 ELSE ccc = hassu(1): colchk4
IF mital(1) = 0 THEN PRINT "-         "; "      ";  ELSE PRINT l(mital(1)); "      ";
IF hassu(2) = 0 THEN COLOR 2 ELSE ccc = hassu(2): colchk4
IF mital(2) = 0 THEN PRINT "-" ELSE PRINT l(mital(2))
IF CSRLIN = 42 THEN pjn: klearstat (6)
LOOP
CLOSE #1
RETURN
END SUB

SUB staulehlmaar
FOR xx = 1 TO 20
es(xx) = 1
NEXT xx

FOR taul1 = 1 TO 4
FOR taul2 = taul1 + 1 TO 4
GOSUB maar2
NEXT taul2
NEXT taul1

FOR taul1 = 5 TO 8
FOR taul2 = taul1 + 1 TO 8
GOSUB maar2
NEXT taul2
NEXT taul1

FOR taul1 = 9 TO 12
FOR taul2 = taul1 + 1 TO 12
GOSUB maar2
NEXT taul2
NEXT taul1

FOR taul1 = 13 TO 16
FOR taul2 = taul1 + 1 TO 16
GOSUB maar2
NEXT taul2
NEXT taul1

FOR taul1 = 17 TO 20
FOR taul2 = taul1 + 1 TO 20
GOSUB maar2
NEXT taul2
NEXT taul1

EXIT SUB

maar2:
IF ep(taul1) > ep(taul2) THEN es(taul2) = es(taul2) + 1 ELSE IF ep(taul1) < ep(taul2) THEN es(taul1) = es(taul1) + 1 ELSE maalieroehl
RETURN

END SUB

SUB staulmaar
FOR xx = 1 TO 24
s(xx) = 1
NEXT xx

FOR xx = 1 TO 11
taul1 = x(xx)
FOR xxx = xx + 1 TO 12
taul2 = x(xxx)
GOSUB maar1
NEXT xxx
NEXT xx

FOR xx = 13 TO 23
taul1 = x(xx)
FOR xxx = xx + 1 TO 24
taul2 = x(xxx)
GOSUB maar1
NEXT xxx
NEXT xx

IF ot >= 1 AND ot < 45 THEN
FOR xx = 1 TO plkm
graf(ot, xx) = s(u(xx))
NEXT xx
END IF
EXIT SUB

maar1:
IF p(taul1) > p(taul2) THEN s(taul2) = s(taul2) + 1 ELSE IF p(taul1) < p(taul2) THEN s(taul1) = s(taul1) + 1 ELSE maaliero
RETURN

END SUB

SUB staulnayt
CLS
COLOR 14
LOCATE 1, 1: PRINT "PHL:N SARJATAULUKKO"
LOCATE 18, 1: PRINT "DIVISIOONAN SARJATAULUKKO"
COLOR 8
LOCATE 11, 1: PRINT "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
LOCATE 15, 1: PRINT "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
LOCATE 26, 1: PRINT "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
R = 2: s = 2
FOR xx = 1 TO 12
nn = x(xx)
viiva = 0
IF s(x(xx)) > 8 AND s(x(xx)) < 12 THEN viiva = 1
IF s(x(xx)) = 12 THEN viiva = 2
LOCATE R + s(x(xx)) + viiva, s
IF s(x(xx)) < 9 THEN COLOR 10
IF s(x(xx)) > 8 AND s(x(xx)) < 12 THEN COLOR 2
IF s(x(xx)) = 12 THEN COLOR 4
colchk2
GOSUB piirrarivi
NEXT xx

R = 19: s = 2
FOR xx = 13 TO 24
nn = x(xx)
viiva = 0
IF s(x(xx)) > 6 THEN viiva = 1
LOCATE R + s(x(xx)) + viiva, s
IF s(x(xx)) < 7 THEN COLOR 13 ELSE COLOR 5
colchk2
GOSUB piirrarivi
NEXT xx
IF krapu(pv) = 2 THEN krapulapiirto
pjn
EXIT SUB

piirrarivi:
PRINT l(x(xx));
LOCATE , 13: PRINT "("; kk(x(xx)); ")";
LOCATE , 28
IF ot < 44 THEN PRINT LTRIM$(STR$(ot));  ELSE PRINT 44;
IF win(x(xx)) < 10 THEN grr% = 33 ELSE grr% = 32
LOCATE , grr%: PRINT LTRIM$(STR$(win(x(xx))));
LOCATE , 36: PRINT LTRIM$(STR$(dra(x(xx))));
LOCATE , 39: PRINT LTRIM$(STR$(los(x(xx))));
SELECT CASE gf(x(xx))
CASE IS < 10
grr% = 46
CASE 10 TO 99
grr% = 45
CASE ELSE
grr% = 44
END SELECT
LOCATE , grr%: PRINT LTRIM$(STR$(gf(x(xx))));
LOCATE , 48: PRINT "-";
LOCATE , 50: PRINT LTRIM$(STR$(ga(x(xx))));
LOCATE , 58: PRINT LTRIM$(STR$(p(x(xx))))
RETURN
END SUB

SUB staulnaytehl
CLS
COLOR 15: LOCATE 1, 1
PRINT "EHL:N TAULUKOT KIERROKSEN "; LTRIM$(STR$(eot)); " JŽLKEEN"
COLOR 14

FOR xx = 1 TO 5
SELECT CASE xx
CASE 1
www = 3: xxx = 1
CASE 2
www = 10: xxx = 1
CASE 3
www = 17: xxx = 1
CASE 4
www = 3: xxx = 40
CASE 5
www = 10: xxx = 40
END SELECT

COLOR 14
LOCATE www, xxx: PRINT "LOHKO "; LTRIM$(STR$(xx))

FOR temp% = 1 TO 4
nn = (xx - 1) * 4 + temp%
IF es(nn) = 1 THEN COLOR 10 ELSE COLOR 2
IF ohj(ex(nn)) <> 0 THEN ccc = ohj(ex(nn)): colchk4
LOCATE es(nn) + www + 1, xxx

PRINT l(ex(nn));
LOCATE , xxx + 11: PRINT "("; kk(ex(nn)); ")";
LOCATE , xxx + 29: PRINT ero(nn);
LOCATE , xxx + 33: PRINT LTRIM$(STR$(ep(nn)))
NEXT temp%

NEXT xx

IF krapu(pv) = 2 THEN krapulapiirto
pjn

END SUB

SUB staulturmaar
FOR xx = 1 TO 6
ts(xx) = 1
NEXT xx
FOR taul1 = 1 TO 5
FOR taul2 = taul1 + 1 TO 6
GOSUB tmaar2
NEXT taul2
NEXT taul1
PRINT
FOR xx = 1 TO 6
nn = tx(xx)
IF ts(xx) = 1 THEN COLOR 14 ELSE COLOR 2
colchk2
LOCATE R + ts(xx), 1: PRINT l(tx(xx)); " ("; kk(tx(xx)); ")";
LOCATE , 35: PRINT tero(xx);
LOCATE , 41: PRINT tp(xx);
IF tkr < 6 THEN PRINT
IF tkr = 6 THEN
LOCATE , 45: PRINT "("; LTRIM$(STR$(CLNG(palki(ts(xx))))); ")"
IF ohj(tx(xx)) <> 0 THEN raha(ohj(tx(xx))) = raha(ohj(tx(xx))) + palki(ts(xx))
END IF
NEXT xx
EXIT SUB

tmaar2:
IF tp(taul1) > tp(taul2) THEN ts(taul2) = ts(taul2) + 1 ELSE IF tp(taul1) < tp(taul2) THEN ts(taul1) = ts(taul1) + 1 ELSE maalierotur
RETURN
END SUB

SUB stratprint
IF valm(u(pv)) <> 0 THEN PRINT valms(valm(u(pv)));  ELSE PRINT "EI VALITTU";
END SUB

SUB suunnitelma

DIM elake(31 TO 38) AS INTEGER
DIM anhooal(18 TO 25) AS INTEGER


OPEN "data\data12.99" FOR INPUT AS #1

FOR z = 31 TO 38
INPUT #1, elake(z)
NEXT z

FOR z = 18 TO 25
INPUT #1, anhooal(z)
NEXT z

CLOSE #1



FOR vu = 1 TO plkm
pv = vuoro(vu)
CLS
manap
PRINT ", PELAAJASI ILMOITTAVAT:"
PRINT

FOR xx = 1 TO lpl(pv)
IF pel(xx, pv).age >= 31 AND pel(xx, pv).lah = 0 AND pel(xx, pv).svu < 10 AND pel(xx, pv).psk < 14 THEN
jaako = CINT(100 * RND)
temp% = pel(xx, pv).age
IF temp% > 38 THEN temp% = 38
IF jaako <= elake(temp%) THEN
printpels
PRINT " aikoo j„„d„ el„kkeelle sopimuksen umpeuduttua."
pel(xx, pv).lah = 99
END IF
END IF

IF pel(xx, pv).nat <> 1 AND pel(xx, pv).lah = 0 AND pel(xx, pv).psk < 10 AND pel(xx, pv).svu < 10 THEN
jaako = CINT(100 * RND)
IF jaako < 20 THEN
printpels
PRINT " aikoo palata kotimaahansa heti kun mahdollista!"
pel(xx, pv).lah = 98
END IF
END IF

IF pel(xx, pv).psk > 12 AND pel(xx, pv).age < 26 AND pel(xx, pv).lah = 0 AND pel(xx, pv).svu < 10 THEN
jaako = CINT(100 * RND)
IF jaako <= anhooal(pel(xx, pv).age) + ((pel(xx, pv).psk - 13) * 10) AND pel(xx, pv).svu < 10 THEN
printpels
PRINT " ilmoittaa l„htev„ns„ rapakon taakse taaloja tienaamaan!"
pel(xx, pv).lah = 97
END IF
END IF

IF pel(xx, pv).ppp = 1 THEN ccc = 2 ELSE ccc = 1
IF valb(ccc, pv) + valb(4, pv) + (valb(5, pv) * 2) + 2 < pel(xx, pv).psk AND pel(xx, pv).lah = 0 AND pel(xx, pv).svu < 10 THEN
printpels
PRINT "ei viihdy joukkueessa ja aikoo l„hte„ p”rssiin!"
pel(xx, pv).lah = 96
END IF

NEXT xx

IF CSRLIN = 3 THEN COLOR 2: PRINT "PELAAJASI ILMOITTAVAT ETTEI ILMOITETTAVAA OLE!"

pesc
NEXT vu

ERASE elake, anhooal
END SUB

SUB taaah

IF tautip(u(pv)) = 1 THEN
SELECT CASE valb(1, pv)
CASE 1
huono% = 30: hyva% = 100
CASE 2
huono% = 20: hyva% = 95
CASE 3
huono% = 15: hyva% = 90
CASE 4
huono% = 5: hyva% = 87
CASE 5
huono% = 0: hyva% = 80
END SELECT

temp% = CINT(99 * RND) + 1

IF temp% < huono% THEN
tautip(u(pv)) = .95
tautik(u(pv)) = 1
END IF

IF temp% > hyva% THEN
tautip(u(pv)) = 1.05
tautik(u(pv)) = 1
END IF

END IF
END SUB

SUB takaisinhl
CLS
manap
PRINT "!"
PRINT
COLOR 10
printpels
COLOR 2
PRINT "on sopinut palkastaan ja palaa Amerikkaan!"
poispel
pjn
END SUB

SUB tallennus
CHDIR "game_" + LTRIM$(STR$(lokero))

OPEN "game.id" FOR OUTPUT AS #1
PRINT #1, plkm
PRINT #1, kausi
FOR tt = 1 TO plkm
PRINT #1, nimi(tt)
NEXT tt
FOR tt = 1 TO plkm
PRINT #1, l(u(tt))
NEXT tt
CLOSE #1
OPEN "savetus1.xxx" FOR OUTPUT AS #1
WRITE #1, kulta, hopea, pronssi, emestari, nousu, puto, pelaa, ot, ki, ki2, lastbors, eot, cnd, pkr, kausi, kr, ekr, tietos, jaynteh, ppotti, arvsulk, kriisi, nokka, lhinta(1), lhinta(2)
CLOSE #1

OPEN "savetus2.xxx" FOR OUTPUT AS #1
FOR tt = 1 TO plkm
WRITE #1, lpj(tt), u(tt), vuoro(tt), nimi(tt), vai(tt), vai2(tt), raha(tt), mukp(tt), muke(tt), ottephl(tt), ottediv(tt), ottepof(tt), ottedof(tt), otteehl(tt), ykkos(tt), kakkos(tt), kolmos(tt), euros(tt), nousus(tt), putos(tt), lpl(tt), peln(tt),  _
mo(tt), snimi(tt), inte(tt), avage(tt), mveik(tt), oikein(tt), vputki(tt), dputki(tt), tputki(tt), krapu(tt), kans(tt), mtap(tt), fani(tt), efed(tt), rstage(tt), ractio(tt), uhka(tt), voodoo(tt), sovtap(tt), kuume(tt), sopuhu(tt), murjo(tt), potti( _
tt), sopim(tt), junnu(tt), laina(1, tt), laina(2, tt), laina(3, tt), mafia(tt), actiox(tt), jaynateh(tt), cccp(tt), bantu(tt), voittophl(tt), tasanphl(tt), tappiophl(tt), voittodiv(tt), tasandiv(tt), tappiodiv(tt), mukt(tt), stingy(tt)
NEXT tt
CLOSE #1
OPEN "savetus3.xxx" FOR OUTPUT AS #1
FOR tt = 1 TO plkm
FOR zz = 1 TO lpl(tt)
WRITE #1, pel(zz, tt).nam, pel(zz, tt).ppp, pel(zz, tt).nat, pel(zz, tt).age, pel(zz, tt).psk, pel(zz, tt).spe, pel(zz, tt).svu, pel(zz, tt).sra, pel(zz, tt).ego, pel(zz, tt).pok, pel(zz, tt).kun, pel(zz, tt).gls, pel(zz, tt).ass, pel(zz, tt).pot,  _
pel(zz, tt).inj, pel(zz, tt).neu, pel(zz, tt).lah, pel(zz, tt).plus, pel(zz, tt).ket
NEXT zz
FOR zz = 1 TO lpj(tt)
WRITE #1, jel(zz, tt).nam, jel(zz, tt).ppp, jel(zz, tt).nat, jel(zz, tt).age, jel(zz, tt).psk, jel(zz, tt).spe, jel(zz, tt).svu, jel(zz, tt).sra, jel(zz, tt).ego, jel(zz, tt).pok, jel(zz, tt).kun, jel(zz, tt).gls, jel(zz, tt).ass, jel(zz, tt).pot,  _
jel(zz, tt).inj, jel(zz, tt).neu, jel(zz, tt).lah, jel(zz, tt).plus
NEXT zz
NEXT tt
FOR tt = 1 TO plkm
WRITE #1, maa(1, tt)
FOR zz = 1 TO 3
WRITE #1, pk(zz, tt).vp
WRITE #1, pk(zz, tt).op
NEXT zz
FOR zz = 1 TO 4
WRITE #1, hk(zz, tt).vh
WRITE #1, hk(zz, tt).kh
WRITE #1, hk(zz, tt).oh
NEXT zz
NEXT tt
CLOSE #1
OPEN "savetus4.xxx" FOR OUTPUT AS #1
FOR tt = 1 TO 41
WRITE #1, l(tt), kk(tt), ohj(tt), mw(tt), pw(tt), hw(tt), halli(tt), tazo(tt)
NEXT tt
FOR tt = 1 TO 24
WRITE #1, seddd(tt), sedd(tt), sed(tt), valm(tt), tre(tt), sr(tt), x(tt), s(tt), p(tt), gf(tt), ga(tt), win(tt), dra(tt), los(tt), tkausl(tt), tkausd(tt), temes(tt), tmes(tt), thop(tt), tpro(tt), tox(tt), tautip(tt), tautik(tt), haln(tt), sopu(tt),  _
skanda(tt), laxa(tt), muilu(tt), kouku(tt), kausik(tt), menestys(tt), ylek(tt), kotiot(tt), kytta(tt)
NEXT tt
FOR tt = 1 TO 20
WRITE #1, ex(tt), ero(tt), ep(tt), es(tt)
NEXT tt
FOR tt = 1 TO plkm
FOR zz = 1 TO 5
WRITE #1, valb(zz, tt)
NEXT zz
NEXT tt
FOR zz = 42 TO 56
WRITE #1, l(zz), kk(zz), halli(zz), mw(zz), pw(zz), hw(zz)
NEXT zz

CLOSE #1
OPEN "savetus5.xxx" FOR OUTPUT AS #1
FOR tt = 1 TO lastbors
WRITE #1, bel(tt).nam, bel(tt).ppp, bel(tt).nat, bel(tt).age, bel(tt).psk, bel(tt).spe, bel(tt).svu, bel(tt).sra, bel(tt).ego, bel(tt).pok, bel(tt).kun, bel(tt).gls, bel(tt).ass, bel(tt).pot, bel(tt).inj, bel(tt).neu, bel(tt).lah, bel(tt).plus
NEXT tt
CLOSE #1
OPEN "savetus6.xxx" FOR OUTPUT AS #1
FOR tt = 1 TO 24 - plkm
FOR zz = 1 TO 3
WRITE #1, tauti(zz, tt), tkest(zz, tt)
NEXT zz
FOR zz = 1 TO 5
WRITE #1, top(tt, zz).nam, top(tt, zz).gls, top(tt, zz).ass, top(tt, zz).age, top(tt, zz).nat
NEXT zz
NEXT tt
FOR tt = 1 TO plkm
FOR zz = 1 TO 12
WRITE #1, skout(zz, tt), veika(zz, tt)
NEXT zz
NEXT tt
FOR tt = 1 TO plkm
FOR zz = 1 TO 8
WRITE #1, sraha(zz, tt)
NEXT zz
NEXT tt
CLOSE #1
OPEN "savetus7.xxx" FOR OUTPUT AS #1
FOR tt = 1 TO 30 + plkm
WRITE #1, mana(tt)
NEXT tt
FOR tt = 1 TO 24
WRITE #1, man(tt)
NEXT tt
FOR tt = 1 TO 4
WRITE #1, rekord(tt).na, rekord(tt).jo, rekord(tt).ma, rekord(tt).sy, rekord(tt).pi
NEXT tt
FOR tt = 1 TO 2
WRITE #1, pisenj(tt), pisenp(tt)
NEXT tt
CLOSE #1

IF kr > 23 AND kr < 37 THEN
OPEN "elt.xxx" FOR OUTPUT AS #1
FOR tt = 1 TO 6
WRITE #1, elt(tt)
NEXT tt
CLOSE #1
END IF

IF kr < -2 AND kausi > 1998 THEN
OPEN "kiekkoli.xxx" FOR OUTPUT AS #1
FOR tt = 1 TO 24
WRITE #1, kiekkoli(tt)
NEXT tt
CLOSE #1
END IF

OPEN "savetus8.xxx" FOR OUTPUT AS #1
FOR tt = 1 TO 30
WRITE #1, eri(tt)
NEXT tt
CLOSE #1

OPEN "opzion.xxx" FOR OUTPUT AS #1
FOR z = 1 TO 6
WRITE #1, naytajt(z)
NEXT z
WRITE #1, pikaprot
CLOSE #1

IF ot >= 1 THEN
OPEN "graf.xxx" FOR OUTPUT AS #1
FOR tt = 1 TO plkm
FOR zz = 1 TO ot
WRITE #1, graf(zz, tt)
NEXT zz
NEXT tt
CLOSE #1
END IF

CHDIR ".."
END SUB

SUB tarjousmaar (koo1%, koo2%)
tarjous = 0
COLOR 12
DO
LOCATE koo1%, koo2%: PRINT LTRIM$(STR$(tarjous)); "        ";
tar$ = ""
WHILE tar$ = "": tar$ = INKEY$: WEND
IF tar$ = CHR$(0) + CHR$(72) THEN tarjous = tarjous + 10000
IF tar$ = CHR$(0) + CHR$(80) THEN tarjous = tarjous - 10000
IF tar$ = CHR$(0) + CHR$(75) THEN tarjous = tarjous - 100000
IF tar$ = CHR$(0) + CHR$(77) THEN tarjous = tarjous + 100000
IF tarjous > 0 AND tarjous > raha(pv) THEN tarjous = raha(pv)
IF tarjous < 0 THEN tarjous = 0
IF tar$ = CHR$(13) THEN EXIT DO
LOOP

END SUB

SUB tarka (jez%)
IF tauti(jez%, tox(xx)) = 0 THEN lukka = 0 ELSE lukka = 1
END SUB

SUB tarkistanhlc
tietos = 0
IF ohj(kulta) = 0 THEN tietos = kulta: EXIT SUB

FOR zz = 1 TO plkm
IF mukt(zz) = 1 THEN EXIT SUB
NEXT zz

IF ohj(hopea) = 0 THEN tietos = hopea: EXIT SUB

FOR pv = 1 TO plkm
IF hopea = u(pv) THEN GOSUB otarjous
NEXT pv

IF ohj(pronssi) = 0 THEN tietos = pronssi: EXIT SUB

FOR pv = 1 TO plkm
IF pronssi = u(pv) THEN GOSUB otarjous
NEXT pv

FOR zz = 1 TO 24
IF sed(zz) = 4 AND ohj(zz) = 0 THEN tietos = zz: EXIT SUB
IF sed(zz) = 4 AND ohj(zz) <> 0 THEN pv = ohj(zz): GOSUB otarjous
NEXT zz

FOR zz = 1 TO 24
IF sed(zz) = 5 AND ohj(zz) = 0 THEN tietos = zz: EXIT SUB
NEXT zz

otarjous:
CLS
manap
PRINT ", TEILLE TARJOTAAN PAIKKAA NHL CHALLENGE-TURNAUKSESSA!"
PRINT "OTATTEKO PAIKAN VASTAAN (MAHD. AIEMMAT SITOUMUKSET PURETAAN) ? (K/E)"
iz$ = ""
DO UNTIL iz$ = "k" OR iz$ = "e"
iz$ = "": WHILE iz$ = "": iz$ = INKEY$: WEND
LOOP
IF iz$ = "k" THEN mukt(pv) = 1: EXIT SUB
IF iz$ = "e" THEN RETURN
END SUB

SUB tasomaar
DIM lvl(1 TO 58) AS tazzo

OPEN "data\data1.99" FOR INPUT AS #1
FOR zz = 1 TO 58
INPUT #1, lvl(zz).maz, lvl(zz).puz, lvl(zz).hyz
NEXT zz
CLOSE #1

IF ohj(xx) = 0 THEN
mw(xx) = lvl(tazo(xx)).maz
pw(xx) = lvl(tazo(xx)).puz
hw(xx) = lvl(tazo(xx)).hyz
mw(xx) = mw(xx) + CINT(2 * RND) - 1
pw(xx) = pw(xx) + CINT(4 * RND) - 2
hw(xx) = hw(xx) + CINT(8 * RND) - 4
END IF

ERASE lvl
END SUB

SUB tasomuutokset
IF ohj(xx) = 0 THEN
kiekkoli(xx) = tazo(xx)
sijs = (sed(xx) + sedd(xx) + seddd(xx)) / 3

SELECT CASE xx
CASE nousu
GOSUB olennousija
CASE puto
GOSUB olenputoaja
CASE ELSE
SELECT CASE sijs
CASE 1
tazo(xx) = 38 + CINT(2 * RND) - 1
CASE 1.0001 TO 2
tazo(xx) = 37 + CINT(2 * RND) - 1
CASE 2.0001 TO 3
tazo(xx) = 36 + CINT(2 * RND) - 1
CASE 3.0001 TO 5
tazo(xx) = 33 + CINT(2 * RND) - 1
CASE 5.0001 TO 8
tazo(xx) = 31 + CINT(2 * RND) - 1
CASE 8.0001 TO 10
tazo(xx) = 30 + CINT(2 * RND) - 1
CASE 10.0001 TO 11.4
tazo(xx) = 27 + CINT(2 * RND) - 1
CASE 11.0001 TO 13
tazo(xx) = 25 + CINT(2 * RND) - 1
CASE 13.0001 TO 16
tazo(xx) = 23 + CINT(4 * RND) - 2
CASE 16.0001 TO 20
tazo(xx) = 20 + CINT(4 * RND) - 2
CASE 21.0001 TO 24
tazo(xx) = 17 + CINT(4 * RND) - 2
END SELECT
IF emestari = xx THEN tazo(xx) = tazo(xx) + 1
SELECT CASE xx
CASE kulta
GOSUB olenmestari
CASE hopea
GOSUB olenhopea
CASE pronssi
GOSUB olenpronssi
END SELECT
END SELECT

mahd1% = 3: mahd2% = 3: mahd3% = 3
GOSUB arvomahdo
SELECT CASE tullo%
CASE 1
tazo(xx) = tazo(xx) + CINT(1 * RND) + 1
CASE 2
tazo(xx) = tazo(xx)
CASE 3
tazo(xx) = tazo(xx) - CINT(2 * RND) - 1
END SELECT

mahd1% = 3: mahd2% = 3: mahd3% = 3
GOSUB arvomahdo
SELECT CASE tullo%
CASE 1
tazo(xx) = tazo(xx) + CINT(4 * RND) + 1
CASE 2
tazo(xx) = tazo(xx) + CINT(4 * RND) - 2
CASE 3
tazo(xx) = tazo(xx) - CINT(4 * RND) - 1
END SELECT

END IF
EXIT SUB

olennousija:
SELECT CASE sijs
CASE IS <= 10
mahd1% = 80: mahd2% = 20: mahd3% = 0
CASE 10.0001 TO 13
mahd1% = 50: mahd2% = 50: mahd3% = 0
CASE 13.0001 TO 15
mahd1% = 40: mahd2% = 40: mahd3% = 20
CASE ELSE
mahd1% = 20: mahd2% = 40: mahd3% = 40
END SELECT
GOSUB arvomahdo
SELECT CASE tullo%
CASE 1
IF tazo(xx) < 31 THEN tazo(xx) = tazo(xx) + CINT(2 * RND) + 1
CASE 2
tazo(xx) = tazo(xx)
CASE 3
tazo(xx) = tazo(xx) - CINT(2 * RND) - 1
END SELECT
RETURN

olenputoaja:
SELECT CASE sijs
CASE IS <= 8
mahd1% = 90: mahd2% = 10: mahd3% = 0
CASE 8.0001 TO 10
mahd1% = 70: mahd2% = 28: mahd3% = 2
CASE 10.0001 TO 12
mahd1% = 30: mahd2% = 60: mahd3% = 10
CASE 12.0001 TO 15
mahd1% = 15: mahd2% = 30: mahd3% = 55
CASE ELSE
mahd1% = 5: mahd2% = 20: mahd3% = 75
END SELECT

GOSUB arvomahdo
SELECT CASE tullo%
CASE 1
tazo(xx) = tazo(xx) + CINT(2 * RND) + 1
CASE 2
tazo(xx) = tazo(xx)
CASE 3
tazo(xx) = tazo(xx) - CINT(2 * RND) - 2
END SELECT
RETURN

olenmestari:
SELECT CASE sijs
CASE IS < 2
mahd1% = 10: mahd2% = 75: mahd3% = 15
CASE 2.0001 TO 4
mahd1% = 30: mahd2% = 60: mahd3% = 10
CASE 4.0001 TO 8
mahd1% = 60: mahd2% = 30: mahd3% = 0
CASE ELSE
mahd1% = 80: mahd2% = 20: mahd3% = 0
END SELECT
GOSUB arvomahdo
SELECT CASE tullo%
CASE 1
tazo(xx) = tazo(xx) + CINT(2 * RND) + 1
CASE 2
tazo(xx) = tazo(xx)
CASE 3
tazo(xx) = tazo(xx) - CINT(2 * RND) - 1
END SELECT
RETURN

olenhopea:
SELECT CASE sijs
CASE IS < 2
mahd1% = 4: mahd2% = 70: mahd3% = 24
CASE 2.0001 TO 4
mahd1% = 20: mahd2% = 70: mahd3% = 10
CASE 4.0001 TO 8
mahd1% = 45: mahd2% = 55: mahd3% = 5
CASE ELSE
mahd1% = 70: mahd2% = 30: mahd3% = 0
END SELECT
RETURN

olenpronssi:
SELECT CASE sijs
CASE IS < 2
mahd1% = 0: mahd2% = 70: mahd3% = 30
CASE 2.0001 TO 4
mahd1% = 15: mahd2% = 70: mahd3% = 15
CASE 4.0001 TO 8
mahd1% = 40: mahd2% = 55: mahd3% = 5
CASE ELSE
mahd1% = 65: mahd2% = 35: mahd3% = 0
END SELECT
GOSUB arvomahdo
SELECT CASE tullo%
CASE 1
tazo(xx) = tazo(xx) + CINT(1 * RND) + 1
CASE 2
tazo(xx) = tazo(xx)
CASE 3
tazo(xx) = tazo(xx) - CINT(1 * RND) - 1
END SELECT
RETURN

arvomahdo:
rando% = CINT(99 * RND) + 1
SELECT CASE rando%
CASE 1 TO mahd1%
tullo% = 1
CASE mahd1% + 1 TO mahd1% + mahd2%
tullo% = 2
CASE mahd1% + mahd2% + 1 TO mahd1% + mahd2% + mahd3%
tullo% = 3
CASE ELSE
tullo% = 0
END SELECT
RETURN

END SUB

SUB tautimuut
FOR zzz = 1 TO 24 - plkm
FOR zz = 1 TO 3
IF tkest(zz, zzz) > 0 THEN tkest(zz, zzz) = tkest(zz, zzz) - 1
IF tkest(zz, zzz) = 0 AND tauti(zz, zzz) <> 0 THEN tauti(zz, zzz) = 0
NEXT zz
NEXT zzz

FOR zz = 1 TO 24
IF tautik(zz) > 0 THEN tautik(zz) = tautik(zz) - 1
IF tautik(zz) = 0 AND tautip(zz) <> 1 THEN tautip(zz) = 1
NEXT zz

END SUB

SUB teet (jup%, jupp%)
tauti(jup%, tox(xx)) = jupp%
tkest(jup%, tox(xx)) = xxx
END SUB

SUB tekijat
wallari = CSRLIN

fff = (ohj(ww) - 1) * 19
pv = ohj(ww): colchk
LOCATE 21, 1 + fff
FOR zz = 1 TO gl(ww)
LOCATE , 1 + fff: PRINT maali(zz, perlos)
NEXT zz
LOCATE wallari, 1
END SUB

SUB tmankom (sano)
COLOR 14
OPEN "data\tmankom" + LTRIM$(STR$(sano)) + ".mhm" FOR INPUT AS #1
xxx = CINT(5 * RND) + 1
FOR xxxx = 1 TO xxx
LINE INPUT #1, sano$
NEXT xxxx
PRINT sano$
CLOSE #1
END SUB

SUB topmatch
COLOR 13
luetexti ("topmatch")
raha(pv) = raha(pv) + 50000
PRINT
IF vputki(pv) > 0 THEN xx = 82
IF dputki(pv) > 0 THEN xx = 81
IF tputki(pv) > 0 THEN xx = 80

lentti (xx)
xxx = CSRLIN: komme (xx)

PRINT
END SUB

SUB tremaar
FOR xx = 1 TO 24
SELECT CASE valm(xx)
CASE 1
tre(xx) = .945#
CASE 2
tre(xx) = 1.055#
CASE 3
tre(xx) = 1!
END SELECT
NEXT xx
END SUB

SUB tremuut
FOR xx = 1 TO 24
SELECT CASE valm(xx)
CASE 1
tre(xx) = tre(xx) + .0025#
CASE 2
tre(xx) = tre(xx) - .0025#
END SELECT
NEXT xx
END SUB

SUB tulevatottelut
tempki = ki
tempki2 = ki2
xxx = ot
CLS
haahaa (8)
COLOR 14
LOCATE 1, 1
PRINT "PHL"
LOCATE 10, 1: PRINT "DIVISIOONA"
IF krapu(pv) = 2 THEN krapulapiirto
DO
COLOR 5
LOCATE 1, 70
PRINT STRING$(10, " ")
LOCATE 1, 70
PRINT "KIERROS "; LTRIM$(STR$(xxx + 1))
LOCATE 2, 70: PRINT STRING$(10, " ")
IF xxx = ot THEN COLOR 13: LOCATE 2, 70: PRINT "(SEURAAVA)"
SELECT CASE ki
CASE 0 TO 11
ki2 = ki
CASE 12 TO 22
ki2 = ki - 11
END SELECT

ottelmaar
naytaott
joo$ = "": WHILE joo$ = "": joo$ = INKEY$: WEND
IF joo$ = CHR$(0) + CHR$(75) AND xxx - 1 >= ot THEN
xxx = xxx - 1
SELECT CASE ki - 1
CASE 0
ki = 22
CASE ELSE
ki = ki - 1
END SELECT
END IF
IF joo$ = CHR$(0) + CHR$(77) AND xxx + 1 <= 43 THEN
xxx = xxx + 1
SELECT CASE ki + 1
CASE 23
ki = 1
CASE ELSE
ki = ki + 1
END SELECT
END IF
IF joo$ = CHR$(27) THEN EXIT DO
LOOP

ki = tempki
ki2 = tempki2
ottelmaarhaarax
END SUB

SUB tulonsiirto
kurso = 1
summa = 0

DIM kenelle(1 TO plkm - 1)

z = 1
FOR x = 1 TO plkm
IF pv <> x THEN
kenelle(z) = x: z = z + 1
END IF
NEXT x

CLS
COLOR 13: PRINT "TULONSIIRTO - VALITSE JOUKKUE JONKA TILILLE TAHDOT SIIRTŽŽ RAHAA"
LOCATE 11, 1: PRINT "VALITSE SIIRRETTŽVŽ RAHAMŽŽRŽ"
haahaa (20)

DO
FOR zz = 1 TO plkm - 1
LOCATE 1 + (zz * 2), 1
IF kurso = zz THEN COLOR 14, 13 ELSE COLOR 14, 0
PRINT RTRIM$(l(u(kenelle(zz)))); " ("; nimi(kenelle(zz)); ")"
NEXT zz
COLOR 12, 0
LOCATE 13, 1: PRINT STRING$(20, " ")
LOCATE 13, 1: PRINT LTRIM$(STR$(summa))

ra$ = "": WHILE ra$ = "": ra$ = INKEY$: WEND

IF ra$ = CHR$(0) + CHR$(72) AND kurso > 1 THEN kurso = kurso - 1
IF ra$ = CHR$(0) + CHR$(80) AND kurso < plkm - 1 THEN kurso = kurso + 1

IF ra$ = CHR$(0) + CHR$(75) AND summa > 0 THEN summa = summa - 10000
IF ra$ = CHR$(0) + CHR$(77) AND summa <= (raha(pv) - 10000) THEN summa = summa + 10000

IF ra$ = CHR$(27) THEN EXIT SUB
IF ra$ = CHR$(13) THEN EXIT DO
LOOP

raha(pv) = raha(pv) - summa
raha(kenelle(kurso)) = raha(kenelle(kurso)) + summa

END SUB

SUB turnaus
turnauz = 1
etu1 = 1: etu2 = 1
tkr = 1: R = 6
paluutul:
FOR xx = 1 TO 6
gl(tx(xx)) = 0
ja(tx(xx)) = 0
NEXT xx
GOSUB turottmaar
IF naaturnaus = 1 THEN
CLS
COLOR 13
PRINT tnimi(1) + " "; ", kierros "; tkr
LOCATE 3: kolo = 12
naytaottur
pjn
CLS
COLOR 13: PRINT "TULOKSET OVAT SEURAAVAT:": PRINT : kolo = 12
END IF

nn1 = a: nn = tx(a): mm1 = b: mm = tx(b)
ottpel
IF naaturnaus = 1 THEN ottul
otmuuttur
nn1 = c: nn = tx(c): mm1 = d: mm = tx(d)
ottpel
IF naaturnaus = 1 THEN ottul
otmuuttur
nn1 = E: nn = tx(E): mm1 = f: mm = tx(f)
ottpel
IF naaturnaus = 1 THEN ottul
otmuuttur
IF naaturnaus = 1 THEN
staulturmaar
LOCATE 15
pjn
END IF
tkr = tkr + 1
IF tkr < 6 THEN GOTO paluutul

CLS
COLOR 13
PRINT tnimi(1) + " "; ", LOPULLINEN TILANNE"
R = 2
staulturmaar
LOCATE 9
pjn
turnauz = 0


EXIT SUB

turottmaar:
SELECT CASE tkr
CASE 1
a = 1: b = 2: c = 3: d = 4: E = 5: f = 6
CASE 2
a = 1: b = 3: c = 2: d = 6: E = 4: f = 5
CASE 3
a = 1: b = 4: c = 2: d = 5: E = 3: f = 6
CASE 4
a = 1: b = 5: c = 2: d = 3: E = 4: f = 6
CASE 5
a = 1: b = 6: c = 2: d = 4: E = 3: f = 5
END SELECT
RETURN
END SUB

SUB turnausaloita
DIM tx(1 TO 6) AS INTEGER
DIM ts(1 TO 6) AS INTEGER
DIM tero(1 TO 6) AS INTEGER
DIM tp(1 TO 6) AS INTEGER
DIM tnimi(1) AS STRING
END SUB

SUB turnauslopeta
ERASE tx, ts, tero, tp, tnimi, palki
END SUB

SUB tyytyma
xxx = 0
indiana:
FOR xx = 1 TO lpl(pv)
temp% = 0
IF pel(xx, pv).svu = 0 THEN
IF pel(xx, pv).psk > 5 THEN
IF pel(xx, pv).ppp = 1 THEN ccc = 2 ELSE ccc = 1
IF valb(ccc, pv) + valb(4, pv) + (valb(5, pv) * 2) < pel(xx, pv).psk THEN temp% = 1
IF pel(xx, pv).psk > 6 AND valb(5, pv) < 2 THEN temp% = 1
IF pel(xx, pv).psk > 9 AND valb(5, pv) < 3 THEN temp% = 1

IF temp% = 1 THEN
IF xxx = 0 THEN GOSUB jylha
xxx = 1
printpels
PRINT "ei ole tyytyv„inen organisaatioosi ja l„htee p”rssiin!"
IF lastbors < 300 THEN
bel(lastbors + 1) = pel(xx, pv)
bel(lastbors + 1).pok = 0
bel(lastbors + 1).neu = 1
lastbors = lastbors + 1
poispel
GOTO indiana
END IF
END IF

END IF
END IF
NEXT xx

IF CSRLIN = 3 THEN PRINT "EI YLLŽTTŽVIŽ LŽHTIJ™ITŽ!"
IF xxx = 1 THEN pjn
aaaargh
EXIT SUB

jylha:
CLS
manap
PRINT "!"
PRINT
RETURN
END SUB

SUB uusikausi
ppotti = 0

eri(29) = 0
eri(28) = 0
ekr = 1
eot = 0
kr = -9
ki = 1
ki2 = 1
ot = 0
kausi = kausi + 1

FOR xx = 1 TO 24
ylek(xx) = 0
kotiot(xx) = 0
valm(xx) = 0
tre(xx) = 1
gf(xx) = 0
ga(xx) = 0
p(xx) = 0
win(xx) = 0
dra(xx) = 0
los(xx) = 0
kausik(xx) = 0
NEXT xx

FOR xxx = 1 TO plkm
IF laina(3, xxx) = 0 AND mafia(xxx) = 1 THEN
IF 100 * RND < 15 THEN mafia(xxx) = 0
END IF

mukp(xxx) = 0
mo(xxx) = 0
muke(xxx) = 0
mukt(xxx) = 0
kuume(xxx) = 0
inte(xxx) = 1
jaynateh(xxx) = 0
NEXT xxx

nollaux
nollaux2
nollauxtauti
euromaar
END SUB

SUB uutisia
CLS
COLOR 13: PRINT "JŽŽKIEKKOUUTISET TEILLE TARJOAA ";
COLOR 14: PRINT "URHEILURUUTTU";
COLOR 13: PRINT "!"
PRINT

FOR xx = 1 TO plkm
IF murjo(xx) = 1 AND ohj(vast(xx)) = 0 THEN
murjo(xx) = 0
xx = vast(xx)
muilutus (3)
END IF
NEXT xx

FOR xx = 1 TO 24
IF ohj(xx) = 0 AND skanda(xx) = 1 THEN skandal
IF ohj(xx) = 0 AND muilu(xx) = 1 THEN muilutus (1)
IF ohj(xx) = 0 AND kouku(xx) = 1 THEN muilutus (2)
IF ohj(xx) = 0 AND laxa(xx) = 1 THEN laxati
NEXT xx

IF verk% <> 0 THEN xx = verk%: prm (1): luz (39): verk% = 0

FOR zzz = 1 TO 3
dat% = CINT(570 * RND) + 1

SELECT CASE dat%

CASE 1 TO 48, 501 TO 550
arpo
z = CINT(110 * RND)
SELECT CASE z
CASE IS <= 10
kuk% = 1
CASE 11 TO 60
kuk% = 2
CASE IS >= 61
kuk% = 3
END SELECT
tarka (kuk%)
IF lukka = 0 THEN
prm (3)
COLOR 2: PRINT "k„rsii ";
SELECT CASE kuk%
CASE 1
PRINT "loukkaantumisista maalivahtiosaston puolella!"
pax% = -(CINT(1 * RND) + 1)
IF 100 * RND < 10 THEN pax% = pax% - 1
CASE 2
PRINT "loukkaantumisista pakkikalustossaan!"
pax% = -(CINT(7 * RND) + 1)
CASE 3
PRINT "loukkaantumisista hy”kk„„jien keskuudessa!"
pax% = -(CINT(14 * RND) + 1)
END SELECT
xxx = CINT(7 * RND) + 1
CALL teet(kuk%, pax%)
PRINT
END IF

CASE 50, 60, 70
arpo
IF tautip(xx) = 1 THEN
prm (2)
luz (1)
tautip(xx) = 1.1
tautik(xx) = CINT(5 * RND) + 3
END IF

CASE 55, 65, 75
arpo
IF tautip(xx) = 1 THEN
prm (2)
luz (2)
tautip(xx) = .9
tautik(xx) = CINT(5 * RND) + 3
potk
END IF

CASE 51, 54
arpo
tarka (3)
IF lukka = 0 THEN
prm (3)
luz (3)
xxx = CINT(2 * RND) + 2
CALL teet(3, -10)
END IF

CASE 66
arpo
IF tautip(xx) = 1 THEN
prm (3)
luz (4)
tautip(xx) = .65
tautik(xx) = 1000
END IF

CASE 58, 79
arpo
IF tautip(xx) = 1 THEN
prm (1)
luz (5)
tautip(xx) = .84
tautik(xx) = CINT(10 * RND) + 3
END IF


CASE 49, 64
arpol (33)
IF lukka = 0 THEN
IF tautip(xx) = 1 THEN
prm (1)
luz (6)
tautip(xx) = .84
tautik(xx) = CINT(6 * RND) + 2
IF 100 * RND < 25 THEN potk
END IF
END IF

CASE 63
arpo
prm (1)
luz (7)
tre(xx) = tre(xx) - .05

CASE 56, 57
IF kr < 40 THEN
arpo
prm (1)
luz (8)
hw(xx) = hw(xx) + CINT(4 * RND) + 4
END IF

CASE 59
IF kr < 40 THEN
arpo
prm (1)
luz (9)
mw(xx) = mw(xx) + CINT(1 * RND) + 1
END IF

CASE 52, 53
IF kr < 40 THEN
arpo
prm (1)
luz (10)
pw(xx) = pw(xx) + CINT(2 * RND) + 2
END IF

CASE 61, 190
IF kr > 13 AND kr < 40 THEN
arpods (3)
IF lukka = 0 THEN
j1% = xx
arpols2 (11)
IF lukka = 0 THEN
j2% = xx
COLOR 10: PRINT l(j1%);
riww% = CSRLIN
luz (11)
LOCATE riww% + 1, 38: COLOR 10: PRINT "("; RTRIM$(l(j2%)); ")";
IF mw(j1%) < mw(j2%) THEN
mw(j1%) = mw(j2%): mw(j2%) = mw(j2%) - (CINT(2 * RND) + 1)
END IF
xxx = CINT(3 * RND) + 3
pw(j1%) = pw(j1%) + xxx
pw(j2%) = pw(j2%) - xxx
xxx = CINT(6 * RND) + 4
hw(j1%) = hw(j1%) + xxx
hw(j2%) = hw(j2%) - xxx
LOCATE riww% + 5, 1
END IF
END IF
END IF


CASE 62, 67, 68
arpo
IF tautip(xx) = 1 THEN
prm (1)
luz (12)
tautip(xx) = 1.1
tautik(xx) = CINT(5 * RND) + 5
END IF


CASE 69
arpol (35)
IF lukka = 0 THEN
IF tautip(xx) = 1 THEN
prm (1)
luz (13)
tautip(xx) = .8
tautik(xx) = 1000
potk
END IF
END IF

CASE 71, 73
arpol (20)
IF lukka = 0 THEN
prm (1)
luz (14)
pw(xx) = pw(xx) - CINT(2 * RND) - 1
END IF

CASE 72, 74
arpol (20)
IF lukka = 0 THEN
prm (1)
luz (15)
hw(xx) = hw(xx) - CINT(4 * RND) - 2
END IF

CASE 78, 80
arpo
IF tautip(xx) = 1 THEN
prm (1)
luz (16)
tautip(xx) = .7
tautik(xx) = CINT(3 * RND) + 2
END IF

CASE 76, 77, 81
IF kr < 40 THEN
arpol (20)
IF lukka = 0 THEN
prm (1)
luz (17)
hw(xx) = hw(xx) + CINT(2 * RND) + 2
END IF
END IF


CASE 82, 83
IF kr < 40 THEN
arpol (26)
IF lukka = 0 THEN
prm (1)
luz (18)
hw(xx) = hw(xx) - CINT(2 * RND) - 2
END IF
END IF

CASE 84, 85
IF kr < 40 THEN
arpol (26)
IF lukka = 0 THEN
prm (1)
luz (19)
pw(xx) = pw(xx) - CINT(1 * RND) - 1
END IF
END IF

CASE 86
IF kr < 40 THEN
arpol (26)
IF lukka = 0 THEN
prm (1)
luz (20)
mw(xx) = mw(xx) - 1
END IF
END IF

CASE 87, 89
arpo
IF lukka = 0 THEN
IF tautip(xx) = 1 THEN
prm (1)
luz (21)
tautip(xx) = .6
tautik(xx) = CINT(10 * RND) + 3
IF 100 * RND > 50 THEN potk
END IF
END IF


CASE 88, 90, 91, 92
IF kr < 40 THEN
arpo
prm (1)
luz (22)
hw(xx) = hw(xx) + CINT(3 * RND)
END IF

CASE 93, 95
arpol2 (30)
IF lukka = 0 THEN
IF tautip(xx) = 1 THEN
prm (1)
luz (23)
tautip(xx) = 1.25
tautik(xx) = 1000
END IF
END IF

CASE 94, 96
arpol (10)
j1% = xx
arpol (10)
j2% = xx
IF j2% <> j1% THEN
COLOR 10: PRINT RTRIM$(l(j1%));
COLOR 2: PRINT " ja ";
COLOR 10: PRINT RTRIM$(l(j2%)); " ";
luz (24)
LOCATE CSRLIN - 1, 1
xx = j1%: prm (2)
COLOR 2: PRINT "sek„ ";
xx = j2%: prm (2)
COLOR 2: PRINT "osallisuutensa": PRINT "veril”ylyss„ ja lupaavat pit„„ joukkueillensa puhuttelun!": PRINT
xx = j1%: IF 100 * RND > 65 THEN potk: PRINT
xx = j2%: IF 100 * RND > 65 THEN potk
pw(j1%) = pw(j1%) - CINT(3 * RND) - 1
pw(j2%) = pw(j2%) - CINT(3 * RND) - 1
hw(j1%) = hw(j1%) - CINT(4 * RND) - 2
hw(j2%) = hw(j2%) - CINT(4 * RND) - 2
END IF


CASE 97, 98
arpo
prm (1)
luz (25)
potk
p(xx) = 0
FOR xxx = 1 TO 5
top(tox(xx), xxx).gls = 0
top(tox(xx), xxx).ass = 0
NEXT xxx

CASE 99, 100, 101, 102
arpo
prm (3)
luz (26)
potk

CASE 103, 104, 105
arpo
IF tautip(xx) = 1 THEN
prm (2)
luz (27)
tautip(xx) = .95
tautik(xx) = CINT(2 * RND) + 1
END IF

CASE 106, 107, 108, 109
arpo
IF tautip(xx) = 1 THEN
prm (2)
luz (28)
tautip(xx) = .95
tautik(xx) = CINT(4 * RND) + 2
END IF

CASE 110
arpo
IF tautip(xx) = 1 THEN
prm (2)
luz (29)
IF 100 * RND > 50 THEN lukka = 1 ELSE lukka = 0
IF lukka = 1 THEN potk
IF lukka = 0 THEN tautip(xx) = .88: tautik(xx) = CINT(10 * RND) + 8
END IF

CASE 111, 112, 113, 114
arpo
IF tautip(xx) = 1 THEN
prm (2)
luz (30)
tautip(xx) = 1.05
tautik(xx) = CINT(5 * RND) + 3
END IF

CASE 115, 116
arpo
prm (1)
luz (31)
pw(xx) = pw(xx) + CINT(2 * RND) - 1
hw(xx) = hw(xx) + CINT(4 * RND) - 2

CASE 117, 118, 119, 120
hmm:
arpol (1)
prot1% = xx
arpol (1)
prot2% = xx
IF prot1% = prot2% THEN GOTO hmm
protu% = 1
GOSUB protux

CASE 121, 122, 123, 124
hmm2:
arpod (1)
prot1% = xx
arpod (1)
prot2% = xx
IF prot1% = prot2% THEN GOTO hmm2
protu% = 1
GOSUB protux

CASE 125, 126
arpo
prm (1)
luz (32)


CASE 127, 128
arpo
IF tautip(xx) = 1 THEN
prm (1)
luz (33)
tautip(xx) = .6
tautik(xx) = 1
END IF

CASE 129, 130
arpo
IF tautip(xx) = 1 THEN
prm (1)
luz (34)
tautip(xx) = .85
tautik(xx) = CINT(5 * RND) + 2
IF 100 * RND < 25 THEN potk
END IF

CASE 131
arpo
skandal

CASE 132
arpo
prm (1)
luz (36)

CASE 133
arpo
prm (2)
luz (37)
potk

CASE 134, 135, 136, 137
arpo
prm (2)
luz (38)

CASE 138, 139, 140
arpo
prm (1)
luz (39)

CASE 141, 142, 143
arpo
prm (1)
luz (40)

CASE 144, 145, 146
xx = CINT((5 + plkm) * RND) + (25 - plkm)
prm (4)
luz (41)

CASE 147, 148
hmme:
arpo
j1% = xx
arpo
j2% = xx
IF j2% = j1% THEN GOTO hmme
COLOR 10: PRINT mana(man(j1%)); " ";
COLOR 2: PRINT "ja ";
COLOR 10: PRINT mana(man(j2%)); " ";
luz (42)

CASE 149, 150, 151, 152
xx = CINT((5 + plkm) * RND) + (25 - plkm)
prm (4)
luz (43)

CASE 153, 154
arpo
prm (2)
luz (44)

CASE 155, 156, 157
arpo
prm (2)
luz (45)

CASE 158, 159, 160
arpo
prm (2)
luz (46)

CASE 161
arpol (1)
IF lukka = 0 THEN
prm (2)
luz (47)
potk
END IF

CASE 162
arpol (1)
IF lukka = 0 THEN
prm (2)
luz (48)
END IF


CASE 163, 164
arpo
prm (2)
luz (49)

CASE 165
arpo
prm (2)
luz (50)

CASE 166
arpo
muilutus (1)

CASE 167
arpo
muilutus (2)

CASE 168, 169, 170, 171, 172
arpo
sopu(xx) = 1

CASE 173
arpo
prm (1)
luz (55)
tre(xx) = tre(xx) + .02

CASE 174
arpo
prm (1)
luz (56)
tre(xx) = tre(xx) - .02

CASE 175, 176
arpo
prm (2)
luz (57)

CASE 177, 178
arpod (25)
IF lukka = 0 THEN
prm (2)
luz (58)
END IF

CASE 179, 180
IF ohj(kulta) = 0 THEN
xx = kulta
prm (1)
luz (59)
END IF

CASE 181
lukka = 1
FOR xxx = 1 TO 4
arpo
IF halli(xx) < 8 THEN lukka = 0: EXIT FOR
NEXT xxx
IF lukka = 0 THEN
prm (1)
luz (60)
halli(xx) = halli(xx) + 1
END IF

CASE 182, 183
arpo
prm (2)
luz (61)

CASE 184, 185
arpo
prm (2)
luz (62)

CASE 186, 187
arpol (1)
jouk1% = xx
prm (1)
dulek:
arpol (1)
IF xx = jouk1% THEN GOTO dulek
COLOR 2: PRINT "ja ";
prm (1)
luz (64)
temp% = CINT(1 * RND) + 1
LOCATE CSRLIN - 1, 1: COLOR 7
PRINT RTRIM$(top(tox(jouk1%), temp%).nam);
COLOR 8: PRINT " <--> ";
COLOR 7: PRINT top(tox(xx), temp%).nam
SWAP top(tox(jouk1%), temp%), top(tox(xx), temp%)
PRINT

CASE 188, 189
IF arvsulk = 0 THEN
luz (63)
arvsulk = 1
END IF

CASE 191
xx = CINT(23 * RND) + 1
muilu(xx) = 1

CASE 192
xx = CINT(23 * RND) + 1
laxa(xx) = 1

CASE 193
xx = CINT(23 * RND) + 1
kouku(xx) = 1

CASE 194
xx = CINT(23 * RND) + 1
skanda(xx) = 1

END SELECT

NEXT zzz
IF CSRLIN = 3 THEN COLOR 2: PRINT "HILJAISTA UUTISRINTAMALLA!"
pjn

IF protu% = 1 THEN
xx = prot1%
xxx = prot2%
protesti
protu% = 0
END IF
EXIT SUB

protux:
xx = prot1%: prm (1)
COLOR 2: PRINT "nostaa protestin viel„ tuntemattomasta syyst„!"
PRINT "Syytettyn„ prosessina toimii ";
xx = prot2%: prm (1)
PRINT : PRINT
RETURN
END SUB

SUB valitsestrat
CLS
COLOR 13: PRINT "MANAGERI ";
colchk
PRINT nimi(pv);
COLOR 13: PRINT " , VALITSE VALMENNUSSTRATEGIASI TŽLLE KAUDELLE!"

FOR zz = 1 TO 3
PRINT : COLOR 14
PRINT LTRIM$(STR$(zz)); ". "; valms(zz)
COLOR 12: luetexti ("strat" + LTRIM$(STR$(zz)))
NEXT zz

val$ = ""
DO
val$ = INKEY$
IF val$ = "1" THEN valm(u(pv)) = 1: EXIT DO
IF val$ = "2" THEN valm(u(pv)) = 2: EXIT DO
IF val$ = "3" THEN valm(u(pv)) = 3: EXIT DO
LOOP
END SUB

SUB valitsestrattie
ERASE sort
last = 12
mkoko = 0: pkoko = 0: hkoko = 0
FOR zz = 1 TO plkm
IF sr(u(zz)) = 1 THEN last = last - 1
NEXT zz
DIM mpros(1 TO last) AS SINGLE
DIM ppros(1 TO last) AS SINGLE
DIM hpros(1 TO last) AS SINGLE
DIM kpros(1 TO last) AS SINGLE
DIM sort(1 TO last) AS INTEGER
dad = 1

FOR xx = 1 TO 12
IF ohj(x(xx)) = 0 THEN
mkoko = mkoko + mw(x(xx))
pkoko = pkoko + pw(x(xx))
hkoko = hkoko + hw(x(xx))
sort(dad) = x(xx): dad = dad + 1
END IF
NEXT xx
GOSUB laskuri
ERASE mpros, ppros, hpros, sort, kpros
last = 12
mkoko = 0: pkoko = 0: hkoko = 0
FOR zz = 1 TO plkm
IF sr(u(zz)) = 2 THEN last = last - 1
NEXT zz
DIM mpros(1 TO last) AS SINGLE
DIM ppros(1 TO last) AS SINGLE
DIM hpros(1 TO last) AS SINGLE
DIM kpros(1 TO last) AS SINGLE
DIM sort(1 TO last) AS INTEGER
dad = 1
FOR xx = 13 TO 24
IF ohj(x(xx)) = 0 THEN
mkoko = mkoko + mw(x(xx))
pkoko = pkoko + pw(x(xx))
hkoko = hkoko + hw(x(xx))
sort(dad) = x(xx): dad = dad + 1
END IF
NEXT xx
GOSUB laskuri
ERASE mpros, ppros, hpros, sort, kpros
EXIT SUB

laskuri:
mkes! = mkoko / last
pkes! = pkoko / last
hkes! = hkoko / last
FOR xx = 1 TO last
mpros(xx) = mw(sort(xx)) / mkes!
ppros(xx) = pw(sort(xx)) / pkes!
hpros(xx) = hw(sort(xx)) / hkes!
kpros(xx) = (mpros(xx) + ppros(xx) + hpros(xx)) / 3
NEXT xx

FOR xx = 1 TO last
SELECT CASE kpros(xx)
CASE IS < .7
juri = 20: kaikki = 40: tasa = 40
CASE .7 TO .8
juri = 10: kaikki = 40: tasa = 50
CASE .81 TO .9
juri = 20: kaikki = 30: tasa = 50
CASE .91 TO .5
juri = 30: kaikki = 10: tasa = 60
CASE .96 TO 1
juri = 50: kaikki = 1: tasa = 49
CASE 1.01 TO 1.05
juri = 70: kaikki = 1: tasa = 29
CASE 1.05 TO 1.1
juri = 80: kaikki = 1: tasa = 19
CASE 1.11 TO 1.2
juri = 90: kaikki = 1: tasa = 9
CASE IS > 1.21
juri = 98: kaikki = 1: tasa = 1
END SELECT
valme = CINT((juri + kaikki + tasa) * RND)
SELECT CASE valme
CASE 0 TO juri
valm(sort(xx)) = 1
CASE juri + 1 TO juri + kaikki
valm(sort(xx)) = 2
CASE juri + kaikki + 1 TO juri + kaikki + tasa
valm(sort(xx)) = 3
END SELECT
IF mana(man(sort(xx))) = "Juri Simonov" THEN valm(sort(xx)) = 1
NEXT xx
RETURN
END SUB

SUB varalle
DIM vara(1 TO 4)

FOR xx = 1 TO 4
runkku:
vara(xx) = CINT(7 * RND) + 1
IF xx > 1 THEN
FOR xxx = 1 TO xx - 1
IF vara(xxx) = vara(xx) THEN GOTO runkku
NEXT xxx
END IF
NEXT xx

FOR xx = 1 TO 4
OPEN "data\data1.var" FOR INPUT AS #1
FOR xxx = 1 TO vara(xx)
INPUT #1, l(52 + xx), kk(52 + xx), halli(52 + xx), tazo(52 + xx)
NEXT xxx
CLOSE #1
NEXT xx
ERASE vara
END SUB

SUB vastmaar
IF sr(u(pv)) = 1 THEN rukka = 0 ELSE rukka = 12

SELECT CASE u(pv)
CASE x(a + rukka)
vast(pv) = x(b + rukka)
CASE x(b + rukka)
vast(pv) = x(a + rukka)
CASE x(c + rukka)
vast(pv) = x(d + rukka)
CASE x(d + rukka)
vast(pv) = x(c + rukka)
CASE x(E + rukka)
vast(pv) = x(f + rukka)
CASE x(f + rukka)
vast(pv) = x(E + rukka)
CASE x(g + rukka)
vast(pv) = x(h + rukka)
CASE x(h + rukka)
vast(pv) = x(g + rukka)
CASE x(i + rukka)
vast(pv) = x(j + rukka)
CASE x(j + rukka)
vast(pv) = x(i + rukka)
CASE x(k + rukka)
vast(pv) = x(m + rukka)
CASE x(m + rukka)
vast(pv) = x(k + rukka)
END SELECT
END SUB

SUB vedonlyonti
kurso = 1
CLS
COLOR 13: PRINT "MONOPOLIYHTI™ ARVAUKSEN SUPER-ISOVETO-PHL-VAKIO-KUPONKI!";
LOCATE 29, 1: COLOR 14: luetexti ("arvaus")
haahaa (9)
LOCATE 2, 1
veiknayta
FOR zz = 1 TO 12
veika(zz, pv) = 1
NEXT zz

DO
COLOR 8
FOR zz = 1 TO 12
LOCATE 1 + zz * 2, 30
IF veika(zz, pv) = 1 THEN PRINT "1";  ELSE IF veika(zz, pv) = 2 THEN PRINT "X";  ELSE PRINT "2";
NEXT zz
COLOR 15
LOCATE 1 + kurso * 2, 30
IF veika(kurso, pv) = 1 THEN PRINT "1";  ELSE IF veika(kurso, pv) = 2 THEN PRINT "X";  ELSE PRINT "2";

vv$ = "": WHILE vv$ = "": vv$ = INKEY$: WEND
IF vv$ = CHR$(0) + CHR$(72) AND kurso > 1 THEN kurso = kurso - 1
IF vv$ = CHR$(0) + CHR$(80) AND kurso < 12 THEN kurso = kurso + 1
IF vv$ = CHR$(0) + CHR$(75) AND veika(kurso, pv) > 1 THEN veika(kurso, pv) = veika(kurso, pv) - 1
IF vv$ = CHR$(0) + CHR$(77) AND veika(kurso, pv) < 3 THEN veika(kurso, pv) = veika(kurso, pv) + 1
IF vv$ = "i" THEN
FOR z = 1 TO 12
veika(z, pv) = CINT(2 * RND) + 1
NEXT z
END IF
IF vv$ = CHR$(27) THEN mveik(pv) = 0: EXIT DO
IF vv$ = CHR$(13) THEN mveik(pv) = 1: EXIT DO
LOOP

EXIT SUB

END SUB

SUB veiknayta
plussa = 0: COLOR 12: GOSUB veiknayta2
plussa = 12: COLOR 4: GOSUB veiknayta2
EXIT SUB

veiknayta2:
PRINT
PRINT l(x(a + plussa)); " - "; l(x(b + plussa))
PRINT
PRINT l(x(c + plussa)); " - "; l(x(d + plussa))
PRINT
PRINT l(x(E + plussa)); " - "; l(x(f + plussa))
PRINT
PRINT l(x(g + plussa)); " - "; l(x(h + plussa))
PRINT
PRINT l(x(i + plussa)); " - "; l(x(j + plussa))
PRINT
PRINT l(x(k + plussa)); " - "; l(x(m + plussa))
RETURN
END SUB

SUB veiktark
oikein(pv) = 0
FOR zz = 1 TO 12
IF veika(zz, pv) = tulos(zz) THEN oikein(pv) = oikein(pv) + 1
NEXT zz
IF pirtar(7) <> 1048 THEN oikein(pv) = 0
CLS
COLOR 13: PRINT "VEIKKAUKSESI MENESTYS, MANAGERI ";
colchk
PRINT nimi(pv)
LOCATE 2, 1
veiknayta
FOR zz = 1 TO 12
COLOR 7
LOCATE 1 + zz * 2, 30
IF tulos(zz) = 1 THEN PRINT "1";  ELSE IF tulos(zz) = 2 THEN PRINT "X";  ELSE PRINT "2";
IF tulos(zz) = veika(zz, pv) THEN COLOR 15 ELSE COLOR 8
PRINT " ";
IF veika(zz, pv) = 1 THEN PRINT "1";  ELSE IF veika(zz, pv) = 2 THEN PRINT "X";  ELSE PRINT "2";
NEXT zz

LOCATE 3, 40: COLOR 14
PRINT oikein(pv);
COLOR 13: PRINT " OIKEIN!"

IF oikein(pv) > 8 THEN
LOCATE 5, 40
PRINT "VOITTOSUMMA: ";
COLOR 14
SELECT CASE oikein(pv)
CASE 9
veikvoit& = 1000
CASE 10
veikvoit& = 10000
CASE 11
veikvoit& = 100000
CASE 12
veikvoit& = 500000
END SELECT
PRINT LTRIM$(STR$(veikvoit&))
raha(pv) = raha(pv) + veikvoit&
END IF

pjn
END SUB

SUB veiktulmaar
plu1 = 0: plu2 = 0: GOSUB maaritatulos
plu1 = 12: plu2 = 6: GOSUB maaritatulos
EXIT SUB



maaritatulos:
IF gl(x(a + plu1)) > gl(x(b + plu1)) THEN tulos(1 + plu2) = 1 ELSE IF gl(x(b + plu1)) > gl(x(a + plu1)) THEN tulos(1 + plu2) = 3 ELSE tulos(1 + plu2) = 2
IF gl(x(c + plu1)) > gl(x(d + plu1)) THEN tulos(2 + plu2) = 1 ELSE IF gl(x(d + plu1)) > gl(x(c + plu1)) THEN tulos(2 + plu2) = 3 ELSE tulos(2 + plu2) = 2
IF gl(x(E + plu1)) > gl(x(f + plu1)) THEN tulos(3 + plu2) = 1 ELSE IF gl(x(f + plu1)) > gl(x(E + plu1)) THEN tulos(3 + plu2) = 3 ELSE tulos(3 + plu2) = 2
IF gl(x(g + plu1)) > gl(x(h + plu1)) THEN tulos(4 + plu2) = 1 ELSE IF gl(x(h + plu1)) > gl(x(g + plu1)) THEN tulos(4 + plu2) = 3 ELSE tulos(4 + plu2) = 2
IF gl(x(i + plu1)) > gl(x(j + plu1)) THEN tulos(5 + plu2) = 1 ELSE IF gl(x(j + plu1)) > gl(x(i + plu1)) THEN tulos(5 + plu2) = 3 ELSE tulos(5 + plu2) = 2
IF gl(x(k + plu1)) > gl(x(m + plu1)) THEN tulos(6 + plu2) = 1 ELSE IF gl(x(m + plu1)) > gl(x(k + plu1)) THEN tulos(6 + plu2) = 3 ELSE tulos(6 + plu2) = 2
RETURN

END SUB

SUB vieroitus
kouku(u(pv)) = 0
arvoloukkaa (3)
IF lukka = 0 THEN
COLOR 10
PRINT RTRIM$(pel(xxx, pv).nam);
PRINT " ("; LTRIM$(STR$(pel(xxx, pv).age)); ","; pelip(pel(xxx, pv).ppp); ","; LTRIM$(STR$(pel(xxx, pv).psk)); ") ";
COLOR 2
PRINT "joutuu huumevieroitukseen ";
temp% = CINT(10 * RND) + 6
COLOR 10: PRINT LTRIM$(STR$(temp%));
COLOR 2: PRINT " kierroksen ajaksi!"
pel(xxx, pv).inj = 2000 + temp%
PRINT
END IF
END SUB

