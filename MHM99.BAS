DECLARE SUB varalle ()
DECLARE SUB skoutmaar (fat%)
DECLARE SUB orgamaar ()
DECLARE SUB etsipel (fat%)
DECLARE SUB omajoukkue ()
DECLARE SUB luetexti (lue$)
DECLARE SUB annanimihallille ()
DECLARE SUB pelpaikalkuj ()
DECLARE SUB tasomaar ()
DECLARE SUB nhlalku ()
DECLARE SUB alasarjaalku ()
DECLARE SUB historia ()
DECLARE SUB valitsekansallisuus ()
DECLARE SUB lataus ()
DECLARE SUB avaa ()
DECLARE SUB oppnas2 ()
DECLARE SUB exitus ()
DECLARE SUB tyhjennalokero ()
DECLARE SUB printident ()
DECLARE SUB piraatti ()
DECLARE SUB colchkalku ()
DECLARE SUB uusipelitop ()
DECLARE SUB topmaar ()
DECLARE SUB uusipeliman ()
DECLARE SUB uusipeligene ()
DECLARE SUB alku2 ()
DECLARE SUB vihat ()
DECLARE SUB uusipeli ()
DECLARE SUB options ()
DECLARE SUB vihamuutos ()
DECLARE SUB alku ()
DECLARE SUB lenmaar ()
DECLARE SUB keisialku ()
DECLARE SUB mahmaxalku ()
DECLARE SUB etunimialku ()
DECLARE SUB sarjamaaralku ()
DECLARE SUB pelpaikalku ()
DECLARE SUB egomaaralku ()
DECLARE SUB palkmaaralku ()
DECLARE SUB genremaaralku ()
DECLARE SUB muutmestaritalku ()
DECLARE SUB colchk ()
DECLARE SUB kuvax (tied$)
'$DYNAMIC
'$INCLUDE: 'mhm99.bi'

ON ERROR GOTO errori

DIM pirtar(1 TO 7) AS INTEGER

ver1 = "1.3 WG (REKISTER™ITY)"

rekkax = "MHM 99 FREEWARE"

SCREEN 13
kuvax ("m_soft")
kuvax ("mhm99")
SCREEN 0: WIDTH 80, 25


DIM ident(1 TO 8) AS INTEGER
DIM nimmi(4, 8) AS STRING
DIM jokke(4, 8) AS STRING
DIM wirhe(1 TO 3) AS INTEGER
DIM cauzi(1 TO 8) AS INTEGER

DIM maajomin(1 TO 12) AS INTEGER
DIM cheat(1 TO 10) AS INTEGER
DIM panknim(1 TO 3) AS STRING
DIM pankkor(1 TO 3) AS INTEGER
DIM sponsax(1 TO 8) AS STRING
DIM spezi(1 TO 11) AS STRING
DIM valbh(1 TO 3, 1 TO 5, 1 TO 5) AS LONG
DIM materia(1 TO 20) AS STRING
DIM materiax(1 TO 58) AS INTEGER
DIM naytajt(1 TO 6) AS INTEGER
DIM eri(1 TO 30) AS INTEGER
DIM lhinta(1 TO 2) AS LONG
DIM rekord(1 TO 4) AS borzzi
DIM pisenj(1 TO 2) AS INTEGER
DIM pisenp(1 TO 2) AS INTEGER
DIM kuntomax(18 TO 33) AS INTEGER
DIM sedd(1 TO 24)  AS INTEGER
DIM seddd(1 TO 24) AS INTEGER
DIM haln(1 TO 24) AS STRING
DIM sed(1 TO 24)  AS INTEGER
DIM l(1 TO 56)  AS STRING * 10
DIM kk(1 TO 56)  AS STRING
DIM valm(1 TO 24)  AS INTEGER
DIM tre(1 TO 24)  AS SINGLE
DIM sr(1 TO 24)  AS INTEGER
DIM x(1 TO 24)  AS INTEGER
DIM s(1 TO 24)  AS INTEGER
DIM p(1 TO 24)  AS INTEGER
DIM gf(1 TO 24) AS INTEGER
DIM ga(1 TO 24)  AS INTEGER
DIM win(1 TO 24)  AS INTEGER
DIM dra(1 TO 24)  AS INTEGER
DIM los(1 TO 24)  AS INTEGER
DIM momax(1 TO 5) AS INTEGER
DIM momin(1 TO 5) AS INTEGER
DIM ex(1 TO 20)  AS INTEGER
DIM ero(1 TO 20)  AS INTEGER
DIM ep(1 TO 20)  AS INTEGER
DIM es(1 TO 20)  AS INTEGER
DIM ohj(1 TO 56)  AS INTEGER
DIM mw(1 TO 56) AS INTEGER
DIM pw(1 TO 56) AS INTEGER
DIM hw(1 TO 56) AS INTEGER
DIM halvaat(1 TO 13) AS LONG
DIM hallina(1 TO 14) AS STRING
DIM hallisp(1 TO 14) AS INTEGER
DIM halliip(1 TO 14) AS INTEGER
DIM halliai(1 TO 14) AS INTEGER
DIM halli(1 TO 56) AS INTEGER
DIM tkausl(1 TO 24) AS INTEGER
DIM tkausd(1 TO 24) AS INTEGER
DIM temes(1 TO 24) AS INTEGER
DIM tmes(1 TO 24) AS INTEGER
DIM thop(1 TO 24) AS INTEGER
DIM tpro(1 TO 24) AS INTEGER
DIM sopu(1 TO 24) AS INTEGER
DIM skanda(1 TO 24) AS INTEGER
DIM laxa(1 TO 24) AS INTEGER
DIM muilu(1 TO 24) AS INTEGER
DIM kouku(1 TO 24) AS INTEGER
DIM kytta(1 TO 24) AS INTEGER
DIM kansap(1 TO 16) AS STRING
DIM kansal(1 TO 16) AS STRING * 3
DIM intens(0 TO 2) AS STRING
DIM yhteis(0 TO 10) AS STRING
DIM genre(1 TO 12) AS INTEGER
DIM krjn(23) AS STRING * 1
DIM kausik(1 TO 24) AS SINGLE
DIM menestys(1 TO 24) AS INTEGER
DIM kotiot(1 TO 24) AS INTEGER
DIM ylek(1 TO 24) AS LONG

DIM pelip(1 TO 5) AS STRING * 2

DIM efedriini(0 TO 1) AS STRING * 10
DIM faniryhma(0 TO 1) AS STRING * 10
DIM mtapa(0 TO 3) AS STRING * 10

DIM jayna(1 TO 7) AS STRING
DIM valms(1 TO 3) AS STRING
DIM valn(1 TO 5) AS STRING
DIM tazo(1 TO 56) AS INTEGER
DIM blanko(1 TO 1) AS pelaaja
DIM fxt(1 TO 12, 1 TO 11) AS INTEGER

RANDOMIZE TIMER
kurso = 1

alcus:
lhinta(1) = 30
lhinta(2) = 25

CLS
LOCATE 25, 1
COLOR 15
PRINT "K R Y P T O N A A T O R  9 9  ";
COLOR 14
COLOR 12: PRINT "TERVETULOA, ";
COLOR 14
PRINT rekkax

FOR z = 1 TO 8
OPEN "game_" + LTRIM$(STR$(z)) + "\game.id" FOR INPUT AS #1
INPUT #1, ident(z)
IF ident(z) > 0 THEN
INPUT #1, cauzi(z)
FOR zz = 1 TO ident(z)
INPUT #1, nimmi(zz, z)
NEXT zz
FOR zz = 1 TO ident(z)
INPUT #1, jokke(zz, z)
NEXT zz
END IF
CLOSE #1
NEXT z
LOCATE 1, 66: COLOR 9: PRINT "MADE IN FINLAND";
LOCATE 2, 68: COLOR 15: PRINT "ÛÛÛÛÛÛÛÛÛÛÛ";
LOCATE 3, 68: COLOR 1: PRINT "ÛÛÛÛÛÛÛÛÛÛÛ";
LOCATE 4, 68: COLOR 15: PRINT "ÛÛÛÛÛÛÛÛÛÛÛ";
LOCATE 2, 71: COLOR 1: PRINT "ÛÛ";
LOCATE 4, 71: COLOR 1: PRINT "ÛÛ";

COLOR 14
LOCATE 13, 44: PRINT "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»";
LOCATE 14, 44: PRINT "º                                 º";
LOCATE 15, 44: PRINT "º                                 º";
LOCATE 16, 44: PRINT "º                                 º";
LOCATE 17, 44: PRINT "º                                 º";
LOCATE 18, 44: PRINT "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼";
COLOR 6
LOCATE 14, 45: PRINT "*MASO SOFTWAREN AUTTAVA PUHELIN!*"
LOCATE 15, 45: PRINT "   (MAKSAT VAIN PUHELUMAKSUN)"
LOCATE 16, 45: PRINT ""
COLOR 15: LOCATE 17, 45: PRINT "     0 5 0 -  3 0 2 8 0 3 0"


COLOR 5
LOCATE 19, 44: PRINT "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»";
LOCATE 20, 44: PRINT "º                                 º";
LOCATE 21, 44: PRINT "º                                 º";
LOCATE 22, 44: PRINT "º                                 º";
LOCATE 23, 44: PRINT "º                                 º";
LOCATE 24, 44: PRINT "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼";
COLOR 13
LOCATE 20, 45: PRINT "KAIKKI PELISSŽ ESIINTYVŽT HAHMOT";
LOCATE 21, 45: PRINT "OVAT FIKTIIVISIŽ, JA MAHDOLLISET";
LOCATE 22, 45: PRINT "VIITTAUKSET TODELLISIIN HENKIL™I-";
LOCATE 23, 45: PRINT "HIN TAHATTOMIA. NŽIN ON NŽREET!!";

LOCATE 1, 1
COLOR 14
PRINT "MASO SOFTWARE HOCKEY MANAGER 99 ";
COLOR 6
PRINT ver1
LOCATE 3, 1
COLOR 13
PRINT "VALITSE PELILOKEROSI"

DO
FOR x = 1 TO 8
IF ident(x) = 0 THEN COLOR 2, 0 ELSE COLOR 10, 0
vukka = x * 2 + 1
LOCATE 2 + vukka, 1: PRINT "LOKERO "; LTRIM$(STR$(x)); : printident
NEXT x
vukka = kurso * 2 + 1
IF ident(kurso) = 0 THEN COLOR 2, 9 ELSE COLOR 10, 9
LOCATE 2 + vukka, 1: PRINT "LOKERO "; LTRIM$(STR$(kurso)); : printident
LOCATE 23, 1: COLOR 9: PRINT "ESC: PALUU SYSTEEMIIN";
LOCATE 22, 1
COLOR 11, 0
IF ident(kurso) > 0 THEN PRINT "ENTER: JATKA PELIŽ, D: TYHJENNŽ LOKERO";  ELSE PRINT "ENTER: ALOITA UUSI PELI                ";
alo$ = ""
WHILE alo$ = "": alo$ = INKEY$: WEND
IF alo$ = CHR$(0) + CHR$(72) AND kurso > 1 THEN kurso = kurso - 1
IF alo$ = CHR$(0) + CHR$(80) AND kurso < 8 THEN kurso = kurso + 1
IF alo$ = "d" AND ident(kurso) > 0 THEN tyhjennalokero: GOTO alcus
IF alo$ = CHR$(27) THEN exitus
IF alo$ = CHR$(13) THEN EXIT DO
LOOP
COLOR , 0
lokero = kurso

IF ident(lokero) = 0 THEN alku ELSE plkm = ident(kurso)

DIM jokeri(1 TO plkm) AS INTEGER
DIM jokerip(1 TO plkm) AS INTEGER

DIM graf(1 TO 44, 1 TO plkm) AS INTEGER
DIM stingy(1 TO plkm) AS SINGLE
DIM actiox(1 TO plkm) AS INTEGER
DIM jaynateh(1 TO plkm) AS INTEGER
DIM laina(1 TO 3, 1 TO plkm) AS LONG
DIM mafia(1 TO plkm) AS INTEGER
DIM spec(1 TO plkm) AS INTEGER
DIM sopim(1 TO plkm) AS INTEGER
DIM kuume(1 TO plkm) AS INTEGER
DIM u(1 TO plkm) AS INTEGER
DIM vuoro(1 TO plkm) AS INTEGER
DIM nimi(1 TO plkm) AS STRING
DIM kans(1 TO plkm) AS INTEGER
DIM vai(1 TO plkm)  AS INTEGER
DIM vai2(1 TO plkm) AS INTEGER
DIM raha(1 TO plkm) AS LONG
DIM mukp(1 TO plkm)  AS INTEGER
DIM muke(1 TO plkm)  AS INTEGER

DIM ottephl(1 TO plkm) AS INTEGER
DIM voittophl(1 TO plkm) AS INTEGER
DIM tasanphl(1 TO plkm) AS INTEGER
DIM tappiophl(1 TO plkm) AS INTEGER
DIM ottediv(1 TO plkm) AS INTEGER
DIM voittodiv(1 TO plkm) AS INTEGER
DIM tasandiv(1 TO plkm) AS INTEGER
DIM tappiodiv(1 TO plkm) AS INTEGER

DIM ottepof(1 TO plkm) AS INTEGER
DIM ottedof(1 TO plkm) AS INTEGER
DIM otteehl(1 TO plkm) AS INTEGER
DIM ykkos(1 TO plkm) AS INTEGER
DIM kakkos(1 TO plkm) AS INTEGER
DIM kolmos(1 TO plkm) AS INTEGER
DIM euros(1 TO plkm) AS INTEGER
DIM nousus(1 TO plkm) AS INTEGER
DIM putos(1 TO plkm) AS INTEGER
DIM vputki(1 TO plkm) AS INTEGER
DIM dputki(1 TO plkm) AS INTEGER
DIM tputki(1 TO plkm) AS INTEGER
DIM vast(1 TO plkm) AS INTEGER
DIM koti(1 TO plkm) AS INTEGER
DIM vieras(1 TO plkm) AS INTEGER
DIM krapu(1 TO plkm) AS INTEGER
DIM cccp(1 TO plkm) AS INTEGER
DIM bantu(1 TO plkm) AS INTEGER
DIM voodoo(1 TO plkm) AS INTEGER
DIM mukt(1 TO plkm) AS INTEGER
DIM uhka(1 TO plkm) AS INTEGER
DIM sovtap(1 TO plkm) AS INTEGER
DIM sopuhu(1 TO plkm) AS INTEGER
DIM junnu(1 TO plkm) AS INTEGER
DIM rstage(1 TO plkm) AS INTEGER
DIM ractio(1 TO plkm) AS INTEGER

DIM pel(1 TO 32, 1 TO plkm) AS pelaaja
DIM jel(1 TO 10, 1 TO plkm) AS pelaaja
DIM bel(1 TO 300) AS pelaaja

DIM lpl(1 TO plkm) AS INTEGER
DIM lpj(1 TO plkm) AS INTEGER

DIM top(1 TO 24 - plkm, 1 TO 5) AS topp

DIM tauti(1 TO 3, 1 TO 24 - plkm) AS INTEGER
DIM tkest(1 TO 3, 1 TO 24 - plkm) AS INTEGER

DIM tautip(1 TO 24) AS SINGLE
DIM tautik(1 TO 24) AS INTEGER


DIM tox(1 TO 24) AS INTEGER

DIM mana(1 TO 30 + plkm) AS STRING
DIM man(1 TO 24) AS INTEGER


DIM peln(1 TO plkm) AS INTEGER
DIM mo(1 TO plkm) AS INTEGER

DIM hk(1 TO 4, 1 TO plkm) AS ketkuh
DIM pk(1 TO 3, 1 TO plkm) AS ketkup
DIM maa(1 TO 1, 1 TO plkm) AS INTEGER

DIM snimi(1 TO plkm) AS STRING
DIM sraha(1 TO 8, 1 TO plkm) AS LONG
DIM pketju(1 TO 3, 1 TO plkm) AS INTEGER
DIM hketju(1 TO 4, 1 TO plkm) AS INTEGER
DIM skout(1 TO 12, 1 TO plkm) AS INTEGER

DIM valb(1 TO 5, 1 TO plkm) AS INTEGER
DIM inte(1 TO plkm) AS INTEGER
DIM avage(1 TO plkm) AS SINGLE
DIM veika(1 TO 12, 1 TO plkm) AS INTEGER
DIM mveik(1 TO plkm) AS INTEGER
DIM oikein(1 TO plkm) AS INTEGER
DIM tulos(1 TO 12) AS INTEGER
DIM fani(1 TO plkm) AS INTEGER
DIM efed(1 TO plkm) AS INTEGER
DIM mtap(1 TO plkm) AS INTEGER
DIM murjo(1 TO plkm) AS INTEGER
DIM potti(1 TO plkm) AS LONG

avaa

SELECT CASE ident(lokero)
CASE 0
FOR zz = 1 TO 24
haln(zz) = "J„„halli"
NEXT zz
oppnas2
sarjamaaralku
alku2
uusipeli
topmaar
uusipeliman
uusipelitop

FOR xx = 1 TO plkm
FOR xxx = 1 TO lpl(xx)
palkmaaralku
NEXT xxx
NEXT xx
vihat

CASE ELSE
COLOR , 0
CLS
COLOR 31
PRINT "ODOTA HETKINEN!"
lataus
END SELECT

ensintoinen = 1
ERASE nimmi, ident, jokke
CHAIN "ilexreg"

errori:
piraatti

REM $STATIC
SUB alasarjaalku
hablis = CINT(15 * RND) + 1
FOR zz = 42 TO 47
OPEN "data\alasarja.99" FOR INPUT AS #1
FOR zzz = 1 TO hablis
INPUT #1, l(zz), kk(zz), halli(zz), tazo(zz)
NEXT zzz
hablis = hablis + 1
CLOSE #1
NEXT zz
END SUB

SUB alku
SCREEN 0: WIDTH 80, 25
kurso = 1
CLS
COLOR 15: PRINT "VALITSE PELAAJIEN LUKUMŽŽRŽ..."
DO
FOR zz = 1 TO 4
IF kurso = zz THEN COLOR 14, 13 ELSE COLOR 14, 0
LOCATE 3, 3 * zz
PRINT LTRIM$(STR$(zz));
NEXT zz

joo$ = "": WHILE joo$ = "": joo$ = INKEY$: WEND
IF joo$ = CHR$(0) + CHR$(75) AND kurso > 1 THEN kurso = kurso - 1
IF joo$ = CHR$(0) + CHR$(77) AND kurso < 4 THEN kurso = kurso + 1
IF joo$ = CHR$(13) THEN EXIT DO
LOOP
plkm = kurso

IF plkm > 1 THEN
kurso = 1
LOCATE 5, 1: COLOR 15, 0
PRINT "VALITSE NOKKIMISJŽRJESTYS"
DO
LOCATE 10, 1: COLOR 12, 0
luetexti ("nokka" + LTRIM$(STR$(kurso)))
LOCATE 7, 1
IF kurso = 1 THEN COLOR 14, 13 ELSE COLOR 14, 0
PRINT "REILU"
IF kurso = 2 THEN COLOR 14, 13 ELSE COLOR 14, 0
PRINT "REALISTINEN"
joo$ = "": WHILE joo$ = "": joo$ = INKEY$: WEND
IF joo$ = CHR$(0) + CHR$(72) AND kurso > 1 THEN kurso = kurso - 1
IF joo$ = CHR$(0) + CHR$(80) AND kurso < 2 THEN kurso = kurso + 1
IF joo$ = CHR$(13) THEN EXIT DO
LOOP
END IF

IF plkm = 1 THEN nokka = 1 ELSE nokka = kurso

kurso = 30
COLOR , 0
CLS
COLOR 13
PRINT "VALITSE LIPPUJEN HINTA (OLETUS 30 & 25)"
LOCATE 7, 1
COLOR 12
PRINT "Lippujen hintojen muuttaminen aiheuttaa valtavia muutoksia pelin tasa-"
PRINT "painoon. Ne vaikuttavat yht„lailla sek„ lipputuloihin ett„ play-off-pottiin."
PRINT "Kaikki joukkueet, niin tietokoneen kuin ihmistenkin ohjaamat, k„ytt„v„t"
PRINT "samaa hintaa."

DO
LOCATE 3, 1: COLOR 2
PRINT "PHL-OTTELU: ";
COLOR 10
PRINT LTRIM$(STR$(kurso)); " ";
LOCATE 5, 1: COLOR 2
PRINT "DIVISIOONAOTTELU: ";
COLOR 10
PRINT LTRIM$(STR$(kurso - 5)); " ";
joo$ = "": WHILE joo$ = "": joo$ = INKEY$: WEND
IF joo$ = CHR$(0) + CHR$(72) AND kurso < 50 THEN kurso = kurso + 1
IF joo$ = CHR$(0) + CHR$(80) AND kurso > 10 THEN kurso = kurso - 1
IF joo$ = CHR$(13) THEN EXIT DO
LOOP

lhinta(1) = kurso
lhinta(2) = kurso - 5

COLOR 15, 0
LOCATE 20, 1
COLOR 12, 0: PRINT : PRINT "ODOTA - MHM 99 ALUSTAA PELIŽ...";








END SUB

SUB alku2
DIM ntemp(21) AS STRING * 1
FOR xxx = 1 TO plkm

FOR ggg = 1 TO 21
ntemp(ggg) = ""
NEXT ggg

kurso = 1
COLOR , 0
CLS : COLOR 13
PRINT "KIRJOITA NIMESI, PELAAJA NUMERO ";
COLOR 14: PRINT LTRIM$(STR$(xxx))

DO
FOR zz = 1 TO 21
LOCATE 3, zz: PRINT ntemp(zz);
NEXT zz
LOCATE 3, kurso: PRINT "Û"
jep$ = "": WHILE jep$ = "": jep$ = INKEY$: WEND
IF ASC(jep$) >= 33 AND kurso < 21 THEN ntemp(kurso) = jep$: kurso = kurso + 1
IF jep$ = CHR$(8) AND kurso > 1 THEN ntemp(kurso - 1) = "": kurso = kurso - 1
IF jep$ = CHR$(32) AND kurso < 20 THEN ntemp(kurso) = " ": kurso = kurso + 1
IF jep$ = CHR$(13) THEN EXIT DO
LOOP
mukka = kurso - 1

IF mukka = 0 THEN nimi(xxx) = "Pelaaja " + LTRIM$(STR$(xxx)): GOTO mulkerox

FOR zzz = 1 TO mukka
nimi(xxx) = nimi(xxx) + ntemp(zzz)
NEXT zzz

mulkerox:
mana(30 + xxx) = nimi(xxx)

valitsekansallisuus
options

annanimihallille

NEXT xxx

ERASE ntemp
END SUB

SUB annanimihallille
COLOR , 0: CLS
kurso = 1
DIM ntemp(26) AS STRING * 1
haln(u(xxx)) = ""
COLOR 13
PRINT "NIMEŽ JŽŽHALLISI, MANAGERI ";
pv = xxx: colchkalku
PRINT nimi(xxx)
COLOR 14
DO
FOR zz = 1 TO 26
LOCATE 3, zz: PRINT ntemp(zz);
NEXT zz
LOCATE 3, kurso: PRINT "Û"
jep$ = "": WHILE jep$ = "": jep$ = INKEY$: WEND
IF ASC(jep$) >= 33 AND kurso < 26 THEN ntemp(kurso) = jep$: kurso = kurso + 1
IF jep$ = CHR$(8) AND kurso > 1 THEN ntemp(kurso - 1) = "": kurso = kurso - 1
IF jep$ = CHR$(32) AND kurso < 25 THEN ntemp(kurso) = " ": kurso = kurso + 1
IF jep$ = CHR$(13) THEN EXIT DO
LOOP
mukka = kurso - 1

IF mukka = 0 THEN haln(u(xxx)) = "MasoSports Arena": GOTO mulkerooo

FOR zzz = 1 TO mukka
haln(u(xxx)) = haln(u(xxx)) + ntemp(zzz)
NEXT zzz

mulkerooo:
ERASE ntemp

COLOR , 0
CLS
COLOR 31
PRINT "ODOTA HETKINEN!"
END SUB

SUB avaa
CHDIR "data"

OPEN "spezi.99" FOR INPUT AS #1
FOR zz = 1 TO 11
INPUT #1, spezi(zz)
NEXT zz
CLOSE #1

OPEN "materiax.99" FOR INPUT AS #1
FOR zz = 1 TO 58
INPUT #1, materiax(zz)
NEXT zz
CLOSE #1

OPEN "materia.99" FOR INPUT AS #1
FOR zz = 1 TO 20
INPUT #1, materia(zz)
NEXT zz
CLOSE #1

OPEN "data2.99" FOR INPUT AS #1
FOR zz = 1 TO 14
INPUT #1, hallina(zz), hallisp(zz), halliip(zz), halliai(zz)
NEXT zz
FOR zz = 1 TO 21
NEXT zz
CLOSE #1

OPEN "data7.99" FOR INPUT AS #1
FOR zz = 1 TO 16
INPUT #1, kansap(zz), kansal(zz)
NEXT zz
CLOSE #1

OPEN "data8.99" FOR INPUT AS #1
FOR zz = 0 TO 2
INPUT #1, intens(zz)
NEXT zz
FOR zz = 1 TO 5
INPUT #1, pelip(zz)
NEXT zz
INPUT #1, faniryhma(0)
INPUT #1, faniryhma(1)
INPUT #1, efedriini(0)
INPUT #1, efedriini(1)
INPUT #1, mtapa(0)
INPUT #1, mtapa(1)
INPUT #1, mtapa(2)
INPUT #1, mtapa(3)

FOR zz = 1 TO 7
INPUT #1, jayna(zz)
NEXT zz
FOR zz = 1 TO 3
INPUT #1, valms(zz)
NEXT zz
FOR zz = 1 TO 5
INPUT #1, valn(zz)
NEXT zz
CLOSE #1

FOR zz = 1 TO 12
OPEN LTRIM$(STR$(zz)) + ".mhx" FOR INPUT AS #1
genre(zz) = -1
DO UNTIL EOF(1)
INPUT #1, temp$
genre(zz) = genre(zz) + 1
LOOP
CLOSE #1
NEXT zz

OPEN "data9.99" FOR INPUT AS #1
FOR zz = 1 TO 23
INPUT #1, krjn(zz)
NEXT zz
CLOSE #1

OPEN "data11.99" FOR INPUT AS #1
FOR zz = 18 TO 33
INPUT #1, kuntomax(zz)
NEXT zz
FOR zz = 1 TO 5
INPUT #1, momax(zz), momin(zz)
NEXT zz
CLOSE #1

OPEN "data666.99" FOR INPUT AS #1
FOR zz = 1 TO 11
FOR zzz = 1 TO 12
INPUT #1, fxt(zzz, zz)
NEXT zzz
NEXT zz
CLOSE #1

OPEN "data13.99" FOR INPUT AS #1
FOR zz = 1 TO 13
INPUT #1, halvaat(zz)
NEXT zz
CLOSE #1

OPEN "data14.99" FOR INPUT AS #1
FOR zzz = 1 TO 3
FOR zz = 1 TO 5
INPUT #1, valbh(zzz, zz, 1), valbh(zzz, zz, 2), valbh(zzz, zz, 3), valbh(zzz, zz, 4), valbh(zzz, zz, 5)
NEXT zz
NEXT zzz
CLOSE #1

OPEN "data15.99" FOR INPUT AS #1
FOR zz = 1 TO 12
INPUT #1, maajomin(zz)
NEXT zz
CLOSE #1

OPEN "sponsax.99" FOR INPUT AS #1
FOR zz = 1 TO 8
INPUT #1, sponsax(zz)
NEXT zz
CLOSE #1

OPEN "panki.99" FOR INPUT AS #1
FOR zz = 1 TO 3
INPUT #1, panknim(zz), pankkor(zz)
NEXT zz
CLOSE #1


CHDIR ".."
END SUB

SUB colchkalku
SELECT CASE pv
CASE 1
COLOR 15
CASE 2
COLOR 3
CASE 3
COLOR 11
CASE 4
COLOR 9
END SELECT
END SUB

SUB egomaaralku
pel(xxx, xx).ego = CINT(10 * RND)
END SUB

SUB etsipel (fat%)

FOR xxx = 1 TO lpl(pv)
IF pel(xxx, pv).ppp = fat% THEN EXIT FOR
NEXT xxx

jesboks:
FOR xx = 1 TO lpl(pv)
IF xxx <> xx THEN
IF pel(xx, pv).ppp = fat% AND pel(xx, pv).psk > pel(xxx, pv).psk THEN xxx = xx: GOTO jesboks
END IF
NEXT xx

xx = xxx
END SUB

SUB exitus
SCREEN 13
kuvax ("masoweb")
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

SUB historia
FOR zz = 1 TO 12
tkausl(zz) = 2
tkausd(zz + 12) = 2
NEXT zz
temes(3) = 1
tmes(3) = 1
tmes(1) = 1
thop(5) = 1
thop(2) = 1
tpro(10) = 1
tpro(3) = 1
CHDIR "game_" + LTRIM$(STR$(lokero))
OPEN "1.sta" FOR OUTPUT AS #1
WRITE #1, 1997, 3, 0, 5, 0, 10, 0, "Jokerit   ", 0
WRITE #1, 1998, 1, 0, 2, 0, 3, 0, "Feldkirch ", 0
CLOSE #1

OPEN "3.sta" FOR OUTPUT AS #1
WRITE #1, 1997, 0, 0, 0, 0
WRITE #1, 1998, 0, 0, 0, 0
CLOSE #1

rekord(1).na = "H.Relminen"
rekord(1).ma = 11
rekord(1).sy = 39
rekord(1).pi = 50
rekord(1).jo = 2

rekord(2).na = "I.Ojansivu"
rekord(2).ma = 43
rekord(2).sy = 22
rekord(2).pi = 65
rekord(2).jo = 13


rekord(3).na = "O.J.Okinen"
rekord(3).ma = 6
rekord(3).sy = 6
rekord(3).pi = 12
rekord(3).jo = 1

rekord(4).na = "T.Pirkkonen"
rekord(4).ma = 10
rekord(4).sy = 5
rekord(4).pi = 15
rekord(4).jo = 12

pisenj(1) = 1
pisenp(1) = 60

pisenj(2) = 13
pisenp(2) = 65

OPEN "21.sta" FOR OUTPUT AS #1
WRITE #1, 1997, "J.Onecky     ", 3, 15, 29, 44
WRITE #1, 1998, rekord(1).na, rekord(1).jo, rekord(1).ma, rekord(1).sy, rekord(1).pi
CLOSE #1

OPEN "22.sta" FOR OUTPUT AS #1
WRITE #1, 1997, "O.J.Okinen   ", 14, 20, 39, 59
WRITE #1, 1998, rekord(2).na, rekord(2).jo, rekord(2).ma, rekord(2).sy, rekord(2).pi
CLOSE #1

OPEN "23.sta" FOR OUTPUT AS #1
WRITE #1, 1997, "J.Onecky     ", 3, 3, 8, 11
WRITE #1, 1998, rekord(3).na, rekord(3).jo, rekord(3).ma, rekord(3).sy, rekord(3).pi
CLOSE #1

OPEN "24.sta" FOR OUTPUT AS #1
WRITE #1, 1997, "T.Pirkkonen  ", 12, 6, 8, 14
WRITE #1, 1998, rekord(4).na, rekord(4).jo, rekord(4).ma, rekord(4).sy, rekord(4).pi
CLOSE #1

CHDIR ".."
END SUB

SUB keisialku
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

SUB kuvax (tied$)
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

SUB lataus
CHDIR "game_" + LTRIM$(STR$(lokero))

OPEN "savetus1.xxx" FOR INPUT AS #1
INPUT #1, kulta, hopea, pronssi, emestari, nousu, puto, pelaa, ot, ki, ki2, lastbors, eot, cnd, pkr, kausi, kr, ekr, tietos, jaynteh, ppotti, arvsulk, kriisi, nokka, lhinta(1), lhinta(2)
CLOSE #1

OPEN "savetus2.xxx" FOR INPUT AS #1
FOR tt = 1 TO plkm
INPUT #1, lpj(tt), u(tt), vuoro(tt), nimi(tt), vai(tt), vai2(tt), raha(tt), mukp(tt), muke(tt), ottephl(tt), ottediv(tt), ottepof(tt), ottedof(tt), otteehl(tt), ykkos(tt), kakkos(tt), kolmos(tt), euros(tt), nousus(tt), putos(tt), lpl(tt), peln(tt),  _
mo(tt), snimi(tt), inte(tt), avage(tt), mveik(tt), oikein(tt), vputki(tt), dputki(tt), tputki(tt), krapu(tt), kans(tt), mtap(tt), fani(tt), efed(tt), rstage(tt), ractio(tt), uhka(tt), voodoo(tt), sovtap(tt), kuume(tt), sopuhu(tt), murjo(tt), potti( _
tt), sopim(tt), junnu(tt), laina(1, tt), laina(2, tt), laina(3, tt), mafia(tt), actiox(tt), jaynateh(tt), cccp(tt), bantu(tt), voittophl(tt), tasanphl(tt), tappiophl(tt), voittodiv(tt), tasandiv(tt), tappiodiv(tt), mukt(tt), stingy(tt)
NEXT tt
CLOSE #1
OPEN "savetus3.xxx" FOR INPUT AS #1
FOR tt = 1 TO plkm
FOR zz = 1 TO lpl(tt)
INPUT #1, pel(zz, tt).nam, pel(zz, tt).ppp, pel(zz, tt).nat, pel(zz, tt).age, pel(zz, tt).psk, pel(zz, tt).spe, pel(zz, tt).svu, pel(zz, tt).sra, pel(zz, tt).ego, pel(zz, tt).pok, pel(zz, tt).kun, pel(zz, tt).gls, pel(zz, tt).ass, pel(zz, tt).pot,  _
pel(zz, tt).inj, pel(zz, tt).neu, pel(zz, tt).lah, pel(zz, tt).plus, pel(zz, tt).ket
NEXT zz
FOR zz = 1 TO lpj(tt)
INPUT #1, jel(zz, tt).nam, jel(zz, tt).ppp, jel(zz, tt).nat, jel(zz, tt).age, jel(zz, tt).psk, jel(zz, tt).spe, jel(zz, tt).svu, jel(zz, tt).sra, jel(zz, tt).ego, jel(zz, tt).pok, jel(zz, tt).kun, jel(zz, tt).gls, jel(zz, tt).ass, jel(zz, tt).pot,  _
jel(zz, tt).inj, jel(zz, tt).neu, jel(zz, tt).lah, jel(zz, tt).plus
NEXT zz
NEXT tt
FOR tt = 1 TO plkm
INPUT #1, maa(1, tt)
FOR zz = 1 TO 3
INPUT #1, pk(zz, tt).vp
INPUT #1, pk(zz, tt).op
NEXT zz
FOR zz = 1 TO 4
INPUT #1, hk(zz, tt).vh
INPUT #1, hk(zz, tt).kh
INPUT #1, hk(zz, tt).oh
NEXT zz
NEXT tt
CLOSE #1
OPEN "savetus4.xxx" FOR INPUT AS #1
FOR tt = 1 TO 41
INPUT #1, l(tt), kk(tt), ohj(tt), mw(tt), pw(tt), hw(tt), halli(tt), tazo(tt)
NEXT tt
FOR tt = 1 TO 24
INPUT #1, seddd(tt), sedd(tt), sed(tt), valm(tt), tre(tt), sr(tt), x(tt), s(tt), p(tt), gf(tt), ga(tt), win(tt), dra(tt), los(tt), tkausl(tt), tkausd(tt), temes(tt), tmes(tt), thop(tt), tpro(tt), tox(tt), tautip(tt), tautik(tt), haln(tt), sopu(tt),  _
skanda(tt), laxa(tt), muilu(tt), kouku(tt), kausik(tt), menestys(tt), ylek(tt), kotiot(tt), kytta(tt)
NEXT tt
FOR tt = 1 TO 20
INPUT #1, ex(tt), ero(tt), ep(tt), es(tt)
NEXT tt
FOR tt = 1 TO plkm
FOR zz = 1 TO 5
INPUT #1, valb(zz, tt)
NEXT zz
NEXT tt
FOR zz = 42 TO 56
INPUT #1, l(zz), kk(zz), halli(zz), mw(zz), pw(zz), hw(zz)
NEXT zz
CLOSE #1

OPEN "savetus5.xxx" FOR INPUT AS #1
FOR tt = 1 TO lastbors
INPUT #1, bel(tt).nam, bel(tt).ppp, bel(tt).nat, bel(tt).age, bel(tt).psk, bel(tt).spe, bel(tt).svu, bel(tt).sra, bel(tt).ego, bel(tt).pok, bel(tt).kun, bel(tt).gls, bel(tt).ass, bel(tt).pot, bel(tt).inj, bel(tt).neu, bel(tt).lah, bel(tt).plus
NEXT tt
CLOSE #1
OPEN "savetus6.xxx" FOR INPUT AS #1
FOR tt = 1 TO 24 - plkm
FOR zz = 1 TO 3
INPUT #1, tauti(zz, tt), tkest(zz, tt)
NEXT zz
FOR zz = 1 TO 5
INPUT #1, top(tt, zz).nam, top(tt, zz).gls, top(tt, zz).ass, top(tt, zz).age, top(tt, zz).nat
NEXT zz
NEXT tt
FOR tt = 1 TO plkm
FOR zz = 1 TO 12
INPUT #1, skout(zz, tt), veika(zz, tt)
NEXT zz
NEXT tt
FOR tt = 1 TO plkm
FOR zz = 1 TO 8
INPUT #1, sraha(zz, tt)
NEXT zz
NEXT tt
CLOSE #1
OPEN "savetus7.xxx" FOR INPUT AS #1
FOR tt = 1 TO 30 + plkm
INPUT #1, mana(tt)
NEXT tt
FOR tt = 1 TO 24
INPUT #1, man(tt)
NEXT tt
FOR tt = 1 TO 4
INPUT #1, rekord(tt).na, rekord(tt).jo, rekord(tt).ma, rekord(tt).sy, rekord(tt).pi
NEXT tt
FOR tt = 1 TO 2
INPUT #1, pisenj(tt), pisenp(tt)
NEXT tt
CLOSE #1

IF kr > 23 AND kr < 37 THEN
DIM elt(1 TO 6) AS INTEGER
OPEN "elt.xxx" FOR INPUT AS #1
FOR tt = 1 TO 6
INPUT #1, elt(tt)
NEXT tt
CLOSE #1
END IF

IF kr < -2 AND kausi > 1998 THEN
DIM kiekkoli(1 TO 24) AS INTEGER
OPEN "kiekkoli.xxx" FOR INPUT AS #1
FOR tt = 1 TO 24
INPUT #1, kiekkoli(tt)
NEXT tt
CLOSE #1
END IF

OPEN "savetus8.xxx" FOR INPUT AS #1
FOR tt = 1 TO 30
INPUT #1, eri(tt)
NEXT tt
CLOSE #1

OPEN "opzion.xxx" FOR INPUT AS #1
FOR z = 1 TO 6
INPUT #1, naytajt(z)
NEXT z
INPUT #1, pikaprot
CLOSE #1

IF ot >= 1 THEN
OPEN "graf.xxx" FOR INPUT AS #1
FOR tt = 1 TO plkm
FOR zz = 1 TO ot
INPUT #1, graf(zz, tt)
NEXT zz
NEXT tt
END IF
CLOSE #1

CHDIR ".."
ladattu = 1
END SUB

SUB luetexti (lue$)
OPEN "data\" + lue$ + ".mhm" FOR INPUT AS #1
DO UNTIL EOF(1)
LINE INPUT #1, line$
PRINT line$
LOOP
CLOSE #1
END SUB

SUB mahmaxalku
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

SUB muutmestaritalku
CHDIR "data"
ooo = 25

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
NEXT zzz
ooo = ooo + 1
CLOSE #1
NEXT zz
RETURN

END SUB

SUB nhlalku
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

SUB omajoukkue
pv = xxx

SELECT CASE ottephl(pv)
CASE 0
max% = 20
IF plkm = 6 THEN max% = 19
CASE 440
max% = 1
CASE ELSE
max% = 9
END SELECT

kurso = max%
COLOR , 0: CLS
LOCATE 1, 1: COLOR 15: PRINT "MINKŽ JOUKKUEEN KORVAAT OMALLASI?"

DO
FOR zz = 1 TO 12
IF ohj(zz) = 0 AND zz >= max% THEN COLOR 14 ELSE COLOR 8
IF zz = kurso THEN COLOR , 13 ELSE COLOR , 0
LOCATE 2 + zz, 1: PRINT l(zz); " ("; kk(zz); ")";
NEXT zz
FOR zz = 13 TO 24
IF ohj(zz) = 0 AND zz >= max% THEN COLOR 14 ELSE COLOR 8
IF zz = kurso THEN COLOR , 13 ELSE COLOR , 0
LOCATE 2 + (zz - 12), 30: PRINT l(zz); " ("; kk(zz); ")";
NEXT zz
vj$ = "": WHILE vj$ = "": vj$ = INKEY$: WEND

IF kurso > 12 AND kurso - 12 >= max% THEN
IF vj$ = CHR$(0) + CHR$(75) THEN kurso = kurso - 12
END IF

IF kurso < 13 THEN
IF vj$ = CHR$(0) + CHR$(77) THEN kurso = kurso + 12
END IF

IF kurso > 1 AND kurso - 1 >= max% THEN
IF vj$ = CHR$(0) + CHR$(72) THEN kurso = kurso - 1
END IF

IF kurso < 24 THEN
IF vj$ = CHR$(0) + CHR$(80) THEN kurso = kurso + 1
END IF

IF vj$ = CHR$(13) AND ohj(kurso) = 0 THEN EXIT DO
LOOP

u(pv) = kurso
ohj(u(xxx)) = pv

DIM ntemp(26) AS STRING * 1
jepulisz:
jep$ = ""
COLOR , 0: CLS
kurso = 1

COLOR 13
PRINT "NIMEŽ JOUKKUEESI, MANAGERI ";
colchkalku
PRINT nimi(pv)
COLOR 14
DO
FOR zz = 1 TO 26
LOCATE 3, zz: PRINT ntemp(zz);
NEXT zz
LOCATE 3, kurso: PRINT "Û"
jep$ = "": WHILE jep$ = "": jep$ = INKEY$: WEND
IF ASC(jep$) >= 33 AND kurso < 11 THEN ntemp(kurso) = jep$: kurso = kurso + 1
IF jep$ = CHR$(8) AND kurso > 1 THEN ntemp(kurso - 1) = "": kurso = kurso - 1
IF jep$ = CHR$(32) AND kurso < 10 THEN ntemp(kurso) = " ": kurso = kurso + 1
IF jep$ = CHR$(13) THEN EXIT DO
LOOP
mukka = kurso - 1

IF mukka = 0 THEN GOTO jepulisz
jep$ = ""
FOR zzz = 1 TO mukka
jep$ = jep$ + ntemp(zzz)
NEXT zzz
l(u(pv)) = jep$
ERASE ntemp

DIM ntemp(26) AS STRING * 1
kurso = 1

jepulisx:

COLOR 13: LOCATE 5, 1
PRINT "JOUKKUEESI KOTIKAUPUNKI, MANAGERI ";
colchkalku
PRINT nimi(pv)
COLOR 14
DO
FOR zz = 1 TO 26
LOCATE 7, zz: PRINT ntemp(zz);
NEXT zz
LOCATE 7, kurso: PRINT "Û"
jep$ = "": WHILE jep$ = "": jep$ = INKEY$: WEND
IF ASC(jep$) >= 33 AND kurso < 13 THEN ntemp(kurso) = jep$: kurso = kurso + 1
IF jep$ = CHR$(8) AND kurso > 1 THEN ntemp(kurso - 1) = "": kurso = kurso - 1
IF jep$ = CHR$(32) AND kurso < 12 THEN ntemp(kurso) = " ": kurso = kurso + 1
IF jep$ = CHR$(13) THEN EXIT DO
LOOP
mukka = kurso - 1

IF mukka = 0 THEN GOTO jepulisx

jep$ = ""
FOR zzz = 1 TO mukka
jep$ = jep$ + ntemp(zzz)
NEXT zzz
kk(u(pv)) = jep$

ERASE ntemp
END SUB

SUB oppnas2
CHDIR "data"

OPEN "data1.pln" FOR INPUT AS #1
FOR ooo = 1 TO 24
INPUT #1, l(ooo), kk(ooo), sr(ooo), sed(ooo), sedd(ooo), seddd(ooo), halli(ooo), tazo(ooo)
NEXT ooo
INPUT #1, kulta, hopea, pronssi
CLOSE #1

OPEN "data3.99" FOR INPUT AS #1
FOR zz = 1 TO 30
INPUT #1, mana(zz)
NEXT zz

FOR z = 1 TO 200
xx = CINT(29 * RND) + 1
ducca:
xxx = CINT(29 * RND) + 1
IF xx = xxx THEN GOTO ducca
SWAP mana(xxx), mana(xx)
NEXT z
CLOSE #1



CHDIR ".."
END SUB

SUB options

DIM waic(1 TO 5) AS STRING
OPEN "data\jepjep.99" FOR INPUT AS #1
FOR z = 1 TO 5
INPUT #1, waic(z)
NEXT z
CLOSE #1
kurso = 3

CLS
COLOR 13
PRINT "VALITSE VAIKEUSTASOSI, MANAGERI ";
pv = xxx: colchkalku
PRINT nimi(xxx)
COLOR 13
DO
FOR z = 1 TO 5
IF kurso = z THEN COLOR 14, 13 ELSE COLOR 14, 0
LOCATE 2 + z, 1: PRINT waic(z)
NEXT z
COLOR 12, 0
LOCATE 9, 1
FOR z = 1 TO 14
PRINT STRING$(80, " ")
NEXT z
LOCATE 9, 1
luetexti ("vaik" + LTRIM$(STR$(kurso)))

v$ = ""
WHILE v$ = "": v$ = INKEY$: WEND
IF v$ = CHR$(0) + CHR$(72) AND kurso > 1 THEN kurso = kurso - 1
IF v$ = CHR$(0) + CHR$(80) AND kurso < 5 THEN kurso = kurso + 1
IF v$ = CHR$(13) THEN vai(xxx) = kurso: EXIT DO
LOOP

ERASE waic

kurso = 2

LOCATE 13, 1: COLOR 13
PRINT "VALITSE APULAISTESI PALKKAVAATIMUSTASO, MANAGERI ";
pv = xxx: colchkalku
PRINT nimi(xxx)
DO
LOCATE 15, 1
IF kurso = 1 THEN COLOR 14, 13 ELSE COLOR 14, 0
PRINT "HALPA"
IF kurso = 2 THEN COLOR 14, 13 ELSE COLOR 14, 0
PRINT "NORMAALI"
IF kurso = 3 THEN COLOR 14, 13 ELSE COLOR 14, 0
PRINT "AHNE"
v$ = ""
WHILE v$ = "": v$ = INKEY$: WEND
IF v$ = CHR$(0) + CHR$(72) AND kurso > 1 THEN kurso = kurso - 1
IF v$ = CHR$(0) + CHR$(80) AND kurso < 3 THEN kurso = kurso + 1
IF v$ = CHR$(13) THEN EXIT DO
LOOP

COLOR , 0
vai2(xxx) = kurso

kurso = 100

COLOR 13
LOCATE 15, 23: PRINT "VALITSE SPONSOREITTEN ANTELIAISUUSTASO (% NORMAALISTA)";
DO
LOCATE 17, 23
COLOR 14, 13: PRINT LTRIM$(STR$(kurso));
COLOR , 0: PRINT " ";
v$ = ""
WHILE v$ = "": v$ = INKEY$: WEND
IF v$ = CHR$(0) + CHR$(80) THEN kurso = kurso - 1
IF v$ = CHR$(0) + CHR$(72) THEN kurso = kurso + 1
IF v$ = CHR$(0) + CHR$(75) THEN kurso = kurso - 10
IF v$ = CHR$(0) + CHR$(77) THEN kurso = kurso + 10
IF kurso > 200 THEN kurso = 200
IF kurso < 25 THEN kurso = 25
IF v$ = CHR$(13) THEN EXIT DO
LOOP

stingy(xxx) = kurso / 100

COLOR , 0

CLS
pv = xxx
SELECT CASE ottephl(pv)
CASE 0
max% = 20
IF plkm = 6 THEN max% = 19
CASE 440
max% = 1
CASE ELSE
max% = 9
END SELECT

kurso = max%

DO UNTIL ohj(kurso) = 0
kurso = kurso + 1
LOOP
LOCATE 1, 1: COLOR 15: PRINT "Valitse joukkueesi (suluissa materiaalin taso)"
LOCATE 3, 1: COLOR 9: PRINT "PHL";
LOCATE 3, 41: COLOR 9: PRINT "1. DIVISIOONA";

DO
FOR zz = 1 TO 12
IF ohj(zz) = 0 AND zz >= max% THEN COLOR 14 ELSE COLOR 8
IF zz = kurso THEN COLOR , 13 ELSE COLOR , 0
LOCATE 4 + zz, 1: PRINT l(zz); " ("; materia(materiax(tazo(zz))); ")";
NEXT zz
FOR zz = 13 TO 24
IF ohj(zz) = 0 AND zz >= max% THEN COLOR 14 ELSE COLOR 8
IF zz = kurso THEN COLOR , 13 ELSE COLOR , 0
LOCATE 4 + (zz - 12), 41: PRINT l(zz); " ("; materia(materiax(tazo(zz))); ")";
NEXT zz
LOCATE 18, 41
IF kurso = 25 THEN COLOR 14, 13 ELSE COLOR 14, 0
PRINT "OMA JOUKKUE";

vj$ = "": WHILE vj$ = "": vj$ = INKEY$: WEND
IF kurso > 12 AND kurso - 12 >= max% THEN
IF vj$ = CHR$(0) + CHR$(75) THEN kurso = kurso - 12
END IF
IF kurso < 13 THEN
IF vj$ = CHR$(0) + CHR$(77) THEN kurso = kurso + 12
END IF
IF kurso > 1 AND kurso - 1 >= max% THEN
IF vj$ = CHR$(0) + CHR$(72) THEN kurso = kurso - 1
END IF
IF kurso < 25 THEN
IF vj$ = CHR$(0) + CHR$(80) THEN kurso = kurso + 1
END IF
IF vj$ = CHR$(13) AND ohj(kurso) = 0 THEN EXIT DO
LOOP

IF kurso = 25 THEN
omajoukkue
pv = xxx
orgamaar
EXIT SUB
END IF

u(xxx) = kurso
ohj(u(xxx)) = xxx
pv = xxx
orgamaar

END SUB

SUB orgamaar
ccc = u(pv)

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

IF frank% > 0 THEN skoutmaar (frank%)

efed(pv) = 0
sopim(pv) = 1
junnu(pv) = valb(3, pv)
END SUB

SUB palkmaaralku
temp1% = pel(xxx, xx).psk
temp2% = pel(xxx, xx).age
temp3% = pel(xxx, xx).ego

palkmiinus = 1
IF temp2% > 26 THEN
ccc = (temp2% - 25) / 80
palkmiinus = 1 - ccc
END IF

IF temp2% < 24 THEN
ccc = (26 - temp2%) / 80
palkmiinus = 1 + ccc
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

IF pel(xxx, xx).ppp = 1 THEN palkehd = palkehd * 2
pel(xxx, xx).sra = palkehd


END SUB

SUB pelpaikalkuj
jel(xxx, xx).ppp = CINT(4 * RND) + 1
END SUB

SUB piraatti
CLS
COLOR 15
PRINT "M H M  9 9  (C) 1998 Maso Software - Feel The Thrill!"
PRINT
COLOR 14
PRINT "VIRHE! SEN ON AIHEUTTANUT JOKO:"
PRINT
PRINT "-REGISTER.ED TIEDOSTOJA EI OLE"
PRINT "-REGISTER.ED TIEDOSTOJA ON MUUTETTU JA NE OVAT VŽŽRISTYNEET"
PRINT
PRINT "-YRITŽT LADATA YHTEENSOPIMATTOMAN VERSION TALLENNETTUA TILANNETTA"
PRINT "-TALLENNETTU TILANNE ON VŽŽRISTYNYT (SITŽ ON EDITOITU TMS.)"
COLOR 7
SYSTEM
END SUB

SUB printident
SHARED ident() AS INTEGER, jokke() AS STRING, nimmi() AS STRING, kurso AS INTEGER

COLOR , 0
FOR fuk = 1 TO 6
LOCATE 4 + fuk, 30
PRINT "                                                 ";
NEXT fuk



SELECT CASE ident(kurso)

CASE IS > 0
FOR fuk = 1 TO ident(kurso)
COLOR 15
LOCATE 4 + fuk, 30: PRINT nimmi(fuk, kurso);
PRINT STRING$(25 - LEN(nimmi(fuk, kurso)), " ");
COLOR 14
PRINT jokke(fuk, kurso);
NEXT fuk
LOCATE 3, 30: COLOR 15: PRINT "KAUSI "; LTRIM$(STR$(cauzi(kurso))); "-"; LTRIM$(STR$(cauzi(kurso) + 1))


CASE 0
COLOR 15
LOCATE 3, 30: PRINT "TYHJŽ                              "
END SELECT

COLOR 2
END SUB

SUB sarjamaaralku
FOR xx = 1 TO 24
x(sed(xx)) = xx
NEXT xx
END SUB

SUB skoutmaar (fat%)
FOR xx = 1 TO fat%
SELECT CASE xx
CASE 1
temp% = 2
CASE 2
temp% = 6
CASE 3
temp% = 9
CASE 4
temp% = 5
CASE 5
temp% = 10
CASE 6
temp% = 12
CASE 7
temp% = 11
CASE 8
temp% = 4
CASE 9
temp% = 8
CASE 10
temp% = 3
CASE 11
temp% = 7
END SELECT
skout(temp%, pv) = 1
NEXT xx
END SUB

SUB tasomaar
DIM lvl(1 TO 80) AS tazzo

OPEN "data\data1.99" FOR INPUT AS #1
FOR zz = 1 TO 58
INPUT #1, lvl(zz).maz, lvl(zz).puz, lvl(zz).hyz
NEXT zz
CLOSE #1

FOR zz = 1 TO 56
mw(zz) = lvl(tazo(zz)).maz
pw(zz) = lvl(tazo(zz)).puz
hw(zz) = lvl(tazo(zz)).hyz
mw(zz) = mw(zz) + CINT(2 * RND) - 1
pw(zz) = pw(zz) + CINT(4 * RND) - 2
hw(zz) = hw(zz) + CINT(8 * RND) - 4
NEXT zz

ERASE lvl
END SUB

SUB topmaar
toxik = 1
FOR zzz = 1 TO 24
IF ohj(zzz) = 0 THEN tox(zzz) = toxik: toxik = toxik + 1
NEXT zzz
COLOR 14

FOR zz = 1 TO 24 - plkm
FOR zzz = 1 TO 3
tauti(zzz, zz) = 0
tkest(zzz, zz) = 0
NEXT zzz
NEXT zz

FOR zz = 1 TO 24
tautip(zz) = 1
tautik(zz) = 0
NEXT zz
END SUB

SUB tyhjennalokero
SHARED kurso AS INTEGER

COLOR 11
LOCATE 22, 1: PRINT "OLETKO TŽYSIN VARMA??? (K/E)              "
varma$ = ""
DO UNTIL varma$ = "k" OR varma$ = "e"
varma$ = ""
WHILE varma$ = "": varma$ = INKEY$: WEND
LOOP
IF varma$ = "e" THEN EXIT SUB

CHDIR "game_" + LTRIM$(STR$(kurso))
KILL "*.*"
OPEN "game.id" FOR OUTPUT AS #1
PRINT #1, "0"
CLOSE #1
CHDIR ".."
END SUB

SUB uusipeli
ki = 1
ot = 51
kausi = 1997
pikaprot = 1

historia
muutmestaritalku
alasarjaalku
nhlalku
varalle

tasomaar
uusipeligene

FOR z = 1 TO 6
naytajt(z) = 1
NEXT z


END SUB

SUB uusipeligene

DIM agep(1 TO 100) AS INTEGER

OPEN "data\data5_2.99" FOR INPUT AS #1
FOR zz = 1 TO 100
INPUT #1, agep(zz)
NEXT zz
CLOSE #1

FOR xx = 1 TO plkm
skout(1, xx) = 1

perusp% = CINT(pw(u(xx)) / 6)
perush% = CINT(hw(u(xx)) / 12)


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
GOSUB hunior

FOR xxx = 21 TO 24
GOSUB wall
GOSUB agewalthen
pel(xxx, xx).ppp = xxx - 19
SELECT CASE pel(xxx, xx).ppp
CASE 2
pel(xxx, xx).psk = CINT(3 * RND) + perusp% - 4
CASE ELSE
pel(xxx, xx).psk = CINT(3 * RND) + perush% - 4
END SELECT
GOSUB wal3
NEXT xxx
lpl(xx) = xxx - 1

NEXT xx

ERASE agep
EXIT SUB



wall:
IF CINT(100 * RND) < 70 THEN nats = 1 ELSE keisialku
mahmaxalku
pel(xxx, xx).nam = krj$ + "." + ptemp$
pel(xxx, xx).nat = nats
RETURN

wal:
SELECT CASE xxx
CASE 1 TO 2
pel(xxx, xx).ppp = 1
CASE 3 TO 8
pel(xxx, xx).ppp = 2
CASE 9 TO 12
pel(xxx, xx).ppp = 3
CASE 13 TO 16
pel(xxx, xx).ppp = 4
CASE 17 TO 20
pel(xxx, xx).ppp = 5
END SELECT

wal2:
SELECT CASE xxx
CASE 1
pel(xxx, xx).psk = mw(u(xx))
CASE 2
pel(xxx, xx).psk = mw(u(xx)) - CINT(1 * RND) - 1
CASE 3 TO 8
pel(xxx, xx).psk = perusp%
CASE 9 TO 20
pel(xxx, xx).psk = perush%
END SELECT
RETURN

agewalthen:
agex = CINT(99 * RND) + 1
pel(xxx, xx).age = agep(agex)
RETURN

wal3:
IF u(xx) = 1 THEN
IF 100 * RND < 40 THEN pel(xxx, xx).spe = 5
END IF
pel(xxx, xx).svu = 1
pel(xxx, xx).neu = 0
pel(xxx, xx).lah = 0
egomaaralku
RETURN

wal4:
kokop% = 0
kokoh% = 0
FOR z = 3 TO 8
kokop% = kokop% + pel(z, xx).psk
NEXT z
FOR z = 9 TO 20
kokoh% = kokoh% + pel(z, xx).psk
NEXT z
RETURN

wal5:
SELECT CASE kokop%
CASE IS < pw(u(xx))
DO UNTIL kokop% = pw(u(xx))
z = CINT(5 * RND) + 3
pel(z, xx).psk = pel(z, xx).psk + 1
GOSUB wal4
LOOP

CASE IS > pw(u(xx))
DO UNTIL kokop% = pw(u(xx))
z = CINT(5 * RND) + 3
pel(z, xx).psk = pel(z, xx).psk - 1
GOSUB wal4
LOOP

END SELECT

SELECT CASE kokoh%
CASE IS < hw(u(xx))
DO UNTIL kokoh% = hw(u(xx))
z = CINT(11 * RND) + 9
pel(z, xx).psk = pel(z, xx).psk + 1
GOSUB wal4
LOOP
CASE IS > hw(u(xx))
DO UNTIL kokoh% = hw(u(xx))
z = CINT(11 * RND) + 9
pel(z, xx).psk = pel(z, xx).psk - 1
GOSUB wal4
LOOP
END SELECT
RETURN

wal6:
FOR z = 1 TO 20
wal62:
c = CINT(5 * RND) + 3
cc = CINT(5 * RND) + 3
IF c = cc OR pel(cc, xx).psk = 1 THEN GOTO wal62
pel(c, xx).psk = pel(c, xx).psk + 1
pel(cc, xx).psk = pel(cc, xx).psk - 1
NEXT z
FOR z = 1 TO 20
wal63:
c = CINT(11 * RND) + 9
cc = CINT(11 * RND) + 9
IF c = cc THEN GOTO wal63
IF pel(c, xx).psk = 20 OR pel(cc, xx).psk = 1 THEN GOTO wal63
pel(c, xx).psk = pel(c, xx).psk + 1
pel(cc, xx).psk = pel(cc, xx).psk - 1
NEXT z
RETURN

hunior:
FOR xxx = 1 TO 5
mahmaxalku
jel(xxx, xx).nam = krj$ + "." + ptemp$
pelpaikalkuj
jel(xxx, xx).psk = CINT(2 * RND) + 1
jel(xxx, xx).age = 16
jel(xxx, xx).svu = 1
jel(xxx, xx).nat = nats
jel(xxx, xx).neu = 0
jel(xxx, xx).lah = 0
jel(xxx, xx).ego = 0
jel(xxx, xx).sra = 0
NEXT xxx
lpj(xx) = xxx - 1
RETURN

END SUB

SUB uusipeliman
FOR zz = 1 TO 24
IF ohj(zz) = 0 THEN man(zz) = tox(zz)
NEXT zz
FOR ggg = 1 TO plkm
man(u(ggg)) = 30 + ggg
NEXT ggg

END SUB

SUB uusipelitop
FOR zz = 1 TO 24 - plkm
FOR zzz = 1 TO 5
keisialku
mahmaxalku
top(zz, zzz).nam = krj$ + "." + ptemp$
top(zz, zzz).age = CINT(8 * RND) + 28
top(zz, zzz).nat = nats
NEXT zzz
NEXT zz

SELECT CASE ohj(1)
CASE 0
top(tox(1), 1).nam = "C.Jaloun"
top(tox(1), 1).age = 25
top(tox(1), 1).nat = 6
top(tox(1), 2).nam = "K.Jaralahti"
top(tox(1), 2).age = 25
top(tox(1), 2).nat = 1
CASE ELSE
pv = ohj(1)
etsipel (5)
pel(xx, pv).nam = "C.Jaloun"
pel(xx, pv).age = 25
pel(xx, pv).nat = 6
IF pel(xx, pv).psk < 15 THEN pel(xx, pv).psk = 15
etsipel (2)
pel(xx, pv).nam = "K.Jaralahti"
pel(xx, pv).age = 23
pel(xx, pv).nat = 1
etsipel (3)
pel(xx, pv).nam = "T.Murjoaho"
pel(xx, pv).age = 23
pel(xx, pv).nat = 1
pel(xx, pv).spe = 5
END SELECT

SELECT CASE ohj(2)
CASE 0
top(tox(2), 1).nam = "H.Relminen"
top(tox(2), 1).age = 33
top(tox(2), 1).nat = 1
top(tox(2), 2).nam = "M.Easures"
top(tox(2), 2).age = 32
top(tox(2), 2).nat = 9
CASE ELSE
pv = ohj(2)
etsipel (4)
pel(xx, pv).nam = "H.Relminen"
pel(xx, pv).age = 33
pel(xx, pv).nat = 1
IF pel(xx, pv).psk < 15 THEN pel(xx, pv).psk = 15
etsipel (2)
pel(xx, pv).nam = "M.Easures"
pel(xx, pv).age = 32
pel(xx, pv).nat = 9
END SELECT

SELECT CASE ohj(3)
CASE 0
top(tox(3), 1).nam = "J.Onecky"
top(tox(3), 1).age = 37
top(tox(3), 1).nat = 6
top(tox(3), 2).nam = "R.Ycine"
top(tox(3), 2).age = 29
top(tox(3), 2).nat = 9
CASE ELSE
pv = ohj(3)
etsipel (4)
pel(xx, pv).nam = "J.Onecky"
pel(xx, pv).age = 35
pel(xx, pv).nat = 6
etsipel (2)
pel(xx, pv).nam = "R.Ycine"
pel(xx, pv).age = 27
pel(xx, pv).nat = 9
END SELECT

SELECT CASE ohj(4)
CASE 0
top(tox(4), 1).nam = "I.Jukonen"
top(tox(4), 1).age = 27
top(tox(4), 1).nat = 1
top(tox(4), 2).nam = "V.Jykoukal"
top(tox(4), 2).age = 29
top(tox(4), 2).nat = 6
CASE ELSE
pv = ohj(4)
etsipel (4)
pel(xx, pv).nam = "I.Jukonen"
pel(xx, pv).age = 27
pel(xx, pv).nat = 1
etsipel (2)
pel(xx, pv).nam = "V.Jykoukal"
pel(xx, pv).age = 29
pel(xx, pv).nat = 6
END SELECT

SELECT CASE ohj(5)
CASE 0
top(tox(5), 1).nam = "S.Kahlstedt"
top(tox(5), 1).age = 25
top(tox(5), 1).nat = 1
top(tox(5), 2).nam = "K.Miprusoff"
top(tox(5), 2).age = 25
top(tox(5), 2).nat = 1
CASE ELSE
pv = ohj(5)
etsipel (4)
pel(xx, pv).nam = "S.Kahlstedt"
pel(xx, pv).age = 26
pel(xx, pv).nat = 1
etsipel (2)
pel(xx, pv).nam = "K.Miprusoff"
pel(xx, pv).age = 27
pel(xx, pv).nat = 1
END SELECT

SELECT CASE ohj(6)
CASE 0
top(tox(6), 1).nam = "J.Ojanne"
top(tox(6), 1).age = 31
top(tox(6), 1).nat = 1
top(tox(6), 2).nam = "P.Jutila"
top(tox(6), 2).age = 34
top(tox(6), 2).nat = 1
CASE ELSE
pv = ohj(6)
etsipel (4)
pel(xx, pv).nam = "J.Ojanne"
pel(xx, pv).age = 30
pel(xx, pv).nat = 1
etsipel (2)
pel(xx, pv).nam = "P.Jutila"
pel(xx, pv).age = 33
pel(xx, pv).nat = 1
END SELECT

SELECT CASE ohj(7)
CASE 0
top(tox(7), 1).nam = "T.McDavish"
top(tox(7), 1).age = 27
top(tox(7), 1).nat = 1
top(tox(7), 2).nam = "K.Juznetsov"
top(tox(7), 2).age = 33
top(tox(7), 2).nat = 5
CASE ELSE
pv = ohj(7)
etsipel (4)
pel(xx, pv).nam = "T.McDavish"
pel(xx, pv).age = 28
pel(xx, pv).nat = 1
etsipel (2)
pel(xx, pv).nam = "K.Juznetsov"
pel(xx, pv).age = 33
pel(xx, pv).nat = 5
END SELECT

SELECT CASE ohj(8)
CASE 0
top(tox(8), 1).nam = "F.Vandal"
top(tox(8), 1).age = 29
top(tox(8), 1).nat = 8
top(tox(8), 2).nam = "A.Terkiomaa"
top(tox(8), 2).age = 30
top(tox(8), 2).nat = 1
CASE ELSE
pv = ohj(8)
etsipel (4)
pel(xx, pv).nam = "F.Vandal"
pel(xx, pv).age = 29
pel(xx, pv).nat = 8
etsipel (3)
pel(xx, pv).nam = "A.Terkiomaa"
pel(xx, pv).age = 30
pel(xx, pv).nat = 1
END SELECT

SELECT CASE ohj(9)
CASE 0
top(tox(9), 1).nam = "C.Aipruss"
top(tox(9), 1).age = 26
top(tox(9), 1).nat = 8
top(tox(9), 2).nam = "T.Lambijevs"
top(tox(9), 2).age = 26
top(tox(9), 2).nat = 8
CASE ELSE
pv = ohj(9)
etsipel (4)
pel(xx, pv).nam = "C.Aipruss"
pel(xx, pv).age = 26
pel(xx, pv).nat = 8
etsipel (3)
pel(xx, pv).nam = "T.Lambijevs"
pel(xx, pv).age = 27
pel(xx, pv).nat = 8
END SELECT

SELECT CASE ohj(10)
CASE 0
top(tox(10), 1).nam = "K.Napanen"
top(tox(10), 1).age = 19
top(tox(10), 1).nat = 1
top(tox(10), 2).nam = "P-Virtanen"
top(tox(10), 2).age = 35
top(tox(10), 2).nat = 1
CASE ELSE
pv = ohj(10)
etsipel (4)
pel(xx, pv).nam = "K.Napanen"
pel(xx, pv).age = 20
pel(xx, pv).nat = 1
etsipel (3)
pel(xx, pv).nam = "P-Virtanen"
pel(xx, pv).age = 35
pel(xx, pv).nat = 1
END SELECT

SELECT CASE ohj(11)
CASE 0
top(tox(11), 1).nam = "T.Jarvainen"
top(tox(11), 1).age = 22
top(tox(11), 1).nat = 1
top(tox(11), 2).nam = "W.Kortman"
top(tox(11), 2).age = 29
top(tox(11), 2).nat = 10
CASE ELSE
pv = ohj(11)
etsipel (4)
pel(xx, pv).nam = "T.Jarvainen"
pel(xx, pv).age = 22
pel(xx, pv).nat = 1
etsipel (2)
pel(xx, pv).nam = "W.Kortman"
pel(xx, pv).age = 29
pel(xx, pv).nat = 10
END SELECT

SELECT CASE ohj(12)
CASE 0
top(tox(12), 1).nam = "T.Pirkkonen"
top(tox(12), 1).age = 30
top(tox(12), 1).nat = 1
top(tox(12), 2).nam = "R.Lob"
top(tox(12), 2).age = 28
top(tox(12), 2).nat = 6
CASE ELSE
pv = ohj(12)
etsipel (4)
pel(xx, pv).nam = "T.Pirkkonen"
pel(xx, pv).age = 30
pel(xx, pv).nat = 1
etsipel (2)
pel(xx, pv).nam = "R.Lob"
pel(xx, pv).age = 28
pel(xx, pv).nat = 6
END SELECT

SELECT CASE ohj(13)
CASE 0
top(tox(13), 1).nam = "I.Ojansivu"
top(tox(13), 1).age = 31
top(tox(13), 1).nat = 1
CASE ELSE
pv = ohj(13)
etsipel (4)
IF pel(xx, pv).psk < 15 THEN pel(xx, pv).psk = 15
pel(xx, pv).nam = "I.Ojansivu"
pel(xx, pv).age = 31
END SELECT

END SUB

SUB valitsekansallisuus
kurso = 1
CLS
COLOR 13: PRINT "VALITSE KANSALLISUUTESI, MANAGERI ";
pv = xxx
colchkalku
PRINT nimi(xxx)
DO
FOR zz = 1 TO 14
IF zz = kurso THEN COLOR 14, 13 ELSE COLOR 14, 0
LOCATE 2 + zz, 1: PRINT kansap(zz);
NEXT zz
COLOR 1, 14
va$ = "": WHILE va$ = "": va$ = INKEY$: WEND
IF va$ = CHR$(0) + CHR$(72) AND kurso > 1 THEN kurso = kurso - 1
IF va$ = CHR$(0) + CHR$(80) AND kurso < 14 THEN kurso = kurso + 1
IF va$ = CHR$(13) THEN EXIT DO
LOOP
kans(xxx) = kurso
COLOR 15, 0

kurso = 1
LOCATE 3, 30: COLOR 13: PRINT "MANAGERINA OLET..."
DO
IF kurso = 1 THEN COLOR 14, 13 ELSE COLOR 14, 0
LOCATE 5, 30: PRINT "UUSI KASVO";
IF kurso = 2 THEN COLOR 14, 13 ELSE COLOR 14, 0
LOCATE 6, 30: PRINT "KOKENUT KONKARI";
IF kurso = 3 THEN COLOR 14, 13 ELSE COLOR 14, 0
LOCATE 7, 30: PRINT "ELŽVŽ LEGENDA";

va$ = "": WHILE va$ = "": va$ = INKEY$: WEND
IF va$ = CHR$(0) + CHR$(72) AND kurso > 1 THEN kurso = kurso - 1
IF va$ = CHR$(0) + CHR$(80) AND kurso < 3 THEN kurso = kurso + 1
IF va$ = CHR$(13) THEN EXIT DO
LOOP
pv = xxx
SELECT CASE kurso
CASE 1
ottephl(pv) = 0
ottepof(pv) = 0
ottediv(pv) = 0
ottedof(pv) = 0
euros(pv) = 0
ykkos(pv) = 0
kakkos(pv) = 0
kolmos(pv) = 0
nousus(pv) = 0
putos(pv) = 0

CASE 2
ottephl(pv) = 44 * 3

voittophl(pv) = 53
tasanphl(pv) = 20
tappiophl(pv) = 59

ottepof(pv) = 20
ottediv(pv) = 44 * 2

voittodiv(pv) = 52
tasandiv(pv) = 9
tappiodiv(pv) = 27

ottedof(pv) = 8
otteehl(pv) = 12
euros(pv) = 0
ykkos(pv) = 0
kakkos(pv) = 1
kolmos(pv) = 1
nousus(pv) = 0
putos(pv) = 0

CASE 3
ottephl(pv) = 44 * 10

voittophl(pv) = 217
tasanphl(pv) = 39
tappiophl(pv) = 193
ottepof(pv) = 50

ottediv(pv) = 44 * 4

voittodiv(pv) = 106
tasandiv(pv) = 11
tappiodiv(pv) = 59

ottedof(pv) = 18
otteehl(pv) = 24
euros(pv) = 1
ykkos(pv) = 2
kakkos(pv) = 3
kolmos(pv) = 2
nousus(pv) = 1
putos(pv) = 1
END SELECT

COLOR , 0
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

SUB vihamuutos

l(xxx) = "Maso HT"
kk(xxx) = "Nurmij„rvi"
tazo(xxx) = 17

ccc = 13
DO UNTIL ohj(x(ccc)) = 0
ccc = ccc + 1
LOOP

IF sr(xxx) = 1 THEN
SWAP x(xxx), x(ccc)
SWAP sr(x(xxx)), sr(x(ccc))
END IF
END SUB

SUB vihat
kurso = 0
COLOR , 0: CLS
COLOR 13
PRINT "TERVETULOA MEGAVIHAN PARIIN, JUONTAJANA ";
COLOR 10
PRINT "JALO-PEKKA JUKKA!"
COLOR 13
PRINT "VALITSE YKSI SEURAAVISTA VAIHTOEHDOISTA!"

DO
IF kurso = 0 THEN COLOR 9, 9 ELSE COLOR 9, 0
LOCATE 4, 1: PRINT "EI VIHOJA TŽNŽŽN - PŽŽTŽNI SŽRKEE!";


FOR zz = 1 TO 12
COLOR , 0
IF ohj(zz) = 0 THEN COLOR 14 ELSE COLOR 8
IF kurso = zz AND ohj(zz) = 0 THEN COLOR 14, 13 ELSE IF kurso = zz THEN COLOR 8, 7
LOCATE 5 + zz, 1: PRINT RTRIM$(l(zz)); "-VIHA";
NEXT zz

FOR zz = 13 TO 24
COLOR , 0
IF ohj(zz) = 0 THEN COLOR 14 ELSE COLOR 8
IF kurso = zz AND ohj(zz) = 0 THEN COLOR 14, 13 ELSE IF kurso = zz THEN COLOR 8, 7
LOCATE 5 + zz - 12, 40: PRINT RTRIM$(l(zz)); "-VIHA";
NEXT zz

vj$ = "": WHILE vj$ = "": vj$ = INKEY$: WEND
IF kurso > 12 THEN
IF vj$ = CHR$(0) + CHR$(75) THEN kurso = kurso - 12
END IF
IF kurso < 13 THEN
IF vj$ = CHR$(0) + CHR$(77) THEN kurso = kurso + 12
END IF
IF kurso > 0 THEN
IF vj$ = CHR$(0) + CHR$(72) THEN kurso = kurso - 1
END IF
IF kurso < 24 THEN
IF vj$ = CHR$(0) + CHR$(80) THEN kurso = kurso + 1
END IF
IF vj$ = CHR$(13) AND kurso = 0 THEN EXIT DO
IF kurso > 0 THEN
IF vj$ = CHR$(13) AND ohj(kurso) = 0 THEN EXIT DO
END IF
LOOP

IF kurso > 0 THEN xxx = kurso: vihamuutos
COLOR , 0
CLS
COLOR 31: PRINT "ODOTA HETKINEN!"
END SUB

