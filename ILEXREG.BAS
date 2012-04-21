'$DYNAMIC
'$INCLUDE: 'mhm99.bi'

CLS
IF ensintoinen = 0 THEN RUN "mhm99"

ver$ = "1.3 WG (REKISTER™ITY)"
IF ver1 <> ver$ THEN PRINT "YHTEENSOPIMATTOMAT VERSIOT!": SYSTEM

RANDOMIZE TIMER
SCREEN 0: WIDTH 80, 43
menu1:
IF ot = 51 THEN
uusikausi
vuoromaar
FOR vu = 1 TO plkm
pv = vuoro(vu)
jaauniorit
sponsorit
budget
NEXT vu
END IF
IF kr = -9 AND ladattu = 0 THEN
COLOR 31, 0: CLS : PRINT "ODOTA HETKINEN!"
firstborsgene = lastbors + 1: borsgenex = 240: borsgene
END IF
IF ladattu = 0 AND kr = 13 THEN avaaturnausdata
IF ladattu = 1 AND kr >= 13 AND kr < 30 THEN avaaturnausdata
IF ladattu = 0 AND kr = 14 THEN tarkistanhlc

IF kr = -9 AND ladattu = 0 THEN
FOR vu = 1 TO plkm
pv = vuoro(vu)
IF vai(pv) = 5 THEN kaudenalkuinho
NEXT vu
valitsestrattie
tremaar
END IF

IF kr = -2 AND ladattu = 0 AND kausi > 1998 THEN
kiekkoliite
END IF

ottelmaarhaarax
pisteporssimaar
lmmaar
vu = 0
menu2:
vu = vu + 1
IF vu > plkm THEN
IF kr < 1 THEN baaorsmuutos
gamedayhaara
GOTO menu1
END IF

pv = vuoro(vu)
koti(pv) = 0: vieras(pv) = 0

IF ot <= 44 THEN staulmaar
IF kr <= 28 THEN staulehlmaar
IF runkosarja = 1 THEN vastmaar

moramaar

ww = 0

IF ladattu = 0 THEN
jaynteh = 0
kriisi = 0
IF rstage(pv) > 0 THEN rakprosessi
IF runkosarja = 1 THEN specmaar

IF kr > 0 THEN taaah

SELECT CASE kr
CASE -9, -8, -7
actiox(pv) = 3
CASE -6 TO -4
actiox(pv) = 4
CASE -3 TO -1
actiox(pv) = 5
CASE 0 TO 1
actiox(pv) = 6
CASE ELSE
actiox(pv) = 99
END SELECT
IF vai(pv) = 1 THEN actiox(pv) = actiox(pv) + 2 ELSE IF vai(pv) = 2 THEN actiox(pv) = actiox(pv) + 1

FOR ooo = 2 TO 12
IF skout(ooo, pv) > 1 THEN skout(ooo, pv) = skout(ooo, pv) - 1
NEXT ooo

FOR xx = 1 TO lpl(pv)
pel(xx, pv).neu = 0
IF pel(xx, pv).spe = 3 AND pel(xx, pv).pok > 5 AND 100 * RND < 20 THEN fbimiehet: EXIT FOR
IF pel(xx, pv).svu = 666 AND 100 * RND < 5 THEN takaisinhl
IF pel(xx, pv).svu = 10000 THEN miettiipois
IF pel(xx, pv).spe = 1 AND mo(pv) < 4 AND mo(pv) <> -66 THEN mo(pv) = -4
NEXT xx

IF kr = -2 AND ladattu = 0 THEN
valitsestrat
tremaar
END IF

IF kr = 1 AND ladattu = 0 THEN poisne

IF kr = -9 AND ladattu = 0 THEN tyytyma

FOR xx = 1 TO lastbors
bel(xx).neu = 0
NEXT xx
END IF

IF ladattu = 0 AND kr = -9 THEN tallennus
ladattu = 0

menu3:
IF laina(3, pv) > 0 THEN mafia(pv) = 1
IF raha(pv) > 2000000000 THEN raha(pv) = 2000000000
sortmaar
sortplayers
ketjuchk
pelhychk
ketjuswap
ketjuissachk

IF vaihtis% = 1 THEN katsoket: GOTO menu3
IF vaihtis% = 2 THEN katsopel: GOTO menu3
CLS
piirtox

DO
me$ = ""
WHILE me$ = "": me$ = INKEY$: WEND
IF me$ = "?" THEN qelp (0): EXIT DO
IF me$ = "2" THEN staulnaytehl: EXIT DO
IF me$ = "1" THEN staulnayt: EXIT DO
IF me$ = "3" THEN statistiikka: EXIT DO
IF me$ = "4" THEN graph: EXIT DO

IF me$ = "I" AND plkm = 1 THEN cheatmenu: EXIT DO
IF me$ = "#" THEN opzions: EXIT DO
IF me$ = "p" THEN pankki: EXIT DO
IF me$ = "o" THEN organisaatio: EXIT DO
IF me$ = "a" THEN katsopel: EXIT DO
IF me$ = "b" THEN katsoket: EXIT DO
IF me$ = "c" AND ot <= 30 THEN bors: EXIT DO
IF me$ = "d" THEN areena: EXIT DO
IF me$ = "e" THEN scoutit: EXIT DO
IF me$ = "f" AND jaynteh = 0 AND jaynateh(pv) < 3 AND ot < 45 THEN meanstuff: EXIT DO
IF me$ = "v" AND veikko = 1 THEN vedonlyonti: EXIT DO
IF me$ = "l" AND kr = 13 AND mukt(pv) = 0 THEN kutsuturnaus: EXIT DO
IF me$ = "l" AND uhka(pv) = 1 THEN kirjeluku: EXIT DO
IF me$ = "t" AND plkm > 1 THEN tulonsiirto: EXIT DO
IF me$ = "S" THEN
IF sopu(u(pv)) = 0 THEN sopu(u(pv)) = 1 ELSE sopu(u(pv)) = 0
sopuprint
END IF

IF mo(pv) <= -6 AND mo(pv) <> -66 AND kriisi = 0 AND me$ = "k" THEN kriisipalaveri: EXIT DO


IF me$ = "*" THEN tulevatottelut: EXIT DO
IF me$ = "M" THEN checkaus: EXIT DO

IF me$ = "-" THEN pisteporssi (1): EXIT DO
IF me$ = "+" THEN pisteporssi (2): EXIT DO
IF me$ = "%" THEN skreensaver: EXIT DO

IF me$ = CHR$(0) + CHR$(72) AND inte(pv) < 2 THEN inte(pv) = inte(pv) + 1: inteprint
IF me$ = CHR$(0) + CHR$(80) AND inte(pv) > 0 THEN inte(pv) = inte(pv) - 1: inteprint

IF me$ = ">" THEN kuitt

IF me$ = "<" AND ot < 45 AND vu = 1 THEN tallennus: CLS : COLOR 13: PRINT "PELITILANNE TALLENNETTU!": pjn: EXIT DO

IF me$ = CHR$(13) THEN
GOTO menu2
END IF

IF me$ = "!" THEN SCREEN 13: kuva ("credits"): kuva ("hanx"): kuva ("suomi"): SCREEN 0: WIDTH 80, 43: EXIT DO
LOOP

GOTO menu3

SUB aaaargh
IF lpl(pv) < 5 THEN
FOR xxx = lpl(pv) + 1 TO 5
pel(xxx, pv) = blanko(1)
nats = 1
mahmax
pel(xxx, pv).nam = krj$ + "." + ptemp$
pel(xxx, pv).age = CINT(10 * RND) + 18
pel(xxx, pv).nat = nats
pel(xxx, pv).ppp = CINT(3 * RND) + 2
pel(xxx, pv).psk = CINT(1 * RND) + 1
pel(xxx, pv).svu = 1
egomaar (1)
palkmaar (1)
pel(xxx, pv).sra = palkehd
lpl(pv) = lpl(pv) + 1
NEXT xxx
END IF
END SUB

SUB aaalkukevat
lastbors = 0

FOR xx = 1 TO plkm
FOR xxx = 1 TO lpl(xx)
IF pel(xxx, xx).spe <> 12 THEN
pel(xxx, xx).neu = 0
IF pel(xxx, xx).svu < 50 THEN pel(xxx, xx).svu = pel(xxx, xx).svu - 1
IF pel(xxx, xx).svu = 0 THEN pel(xxx, xx).sra = 0
END IF
NEXT xxx
NEXT xx

FOR vu = 1 TO plkm
pv = vuoro(vu)
GOSUB arumba
aaaargh
NEXT vu
ERASE rug, rua

FOR xx = 1 TO lastbors
bel(xx).pot = 0
bel(xx).pok = 0
bel(xx).gls = 0
bel(xx).ass = 0
bel(xx).kun = 0
bel(xx).inj = 0
bel(xx).lah = 0
bel(xx).plus = 0
bel(xx).kest = 0
NEXT xx

EXIT SUB

arumba:
klear
sortmaar
sortplayers
DIM muutoz(1 TO lpl(pv)) AS INTEGER
DIM vaat(1 TO 3, 1 TO 20) AS INTEGER
OPEN "data\data6.99" FOR INPUT AS #1
FOR ddd = 1 TO 20
INPUT #1, vaat(1, ddd), vaat(2, ddd), vaat(3, ddd)
FOR fff = 1 TO 3
IF sedd(u(pv)) > 12 THEN vaat(fff, ddd) = vaat(fff, ddd) * 2
NEXT fff
NEXT ddd
CLOSE #1
GOSUB muutokzet
GOSUB lahtevat
ERASE muutoz, vaat
RETURN


muutokzet:

CLS
FOR xx = 1 TO lpl(pv)
IF pel(xx, pv).spe <> 12 THEN
IF pel(xx, pv).pot > 5 + pel(xx, pv).psk THEN
agem = CINT(100 * RND)
SELECT CASE pel(xx, pv).age
CASE IS < 19
IF agem > 60 THEN muutoz(xx) = muutoz(xx) + 1
IF agem > 40 THEN muutoz(xx) = muutoz(xx) + 1
IF agem > 20 THEN muutoz(xx) = muutoz(xx) + 1
CASE 19 TO 23
IF agem > 70 THEN muutoz(xx) = muutoz(xx) + 1
IF agem > 40 THEN muutoz(xx) = muutoz(xx) + 1
CASE 24 TO 25
IF agem > 80 THEN muutoz(xx) = muutoz(xx) + 1
END SELECT
END IF

agem = CINT(100 * RND)
SELECT CASE pel(xx, pv).age
CASE 29 TO 31
IF agem > 95 THEN muutoz(xx) = muutoz(xx) - 1
IF agem > 60 THEN muutoz(xx) = muutoz(xx) - 1
CASE 32 TO 33
IF agem > 70 THEN muutoz(xx) = muutoz(xx) - 1
IF agem > 50 THEN muutoz(xx) = muutoz(xx) - 1
IF agem > 30 THEN muutoz(xx) = muutoz(xx) - 1
CASE IS > 33
IF agem > 80 THEN muutoz(xx) = muutoz(xx) - 1
IF agem > 50 THEN muutoz(xx) = muutoz(xx) - 1
IF agem > 30 THEN muutoz(xx) = muutoz(xx) - 1
IF agem > 10 THEN muutoz(xx) = muutoz(xx) - 1
END SELECT

IF pel(xx, pv).pot < pel(xx, pv).psk THEN muutoz(xx) = muutoz(xx) - 1

IF pel(xx, pv).pot = 0 THEN
SELECT CASE pel(xx, pv).psk
CASE 2, 3, 4, 5, 6
muutoz(xx) = muutoz(xx) - 1
CASE 7, 8, 9, 10, 11, 12
muutoz(xx) = muutoz(xx) - 2
CASE 13, 14, 15, 16
muutoz(xx) = muutoz(xx) - 3
CASE IS > 16
muutoz(xx) = muutoz(xx) - 4
END SELECT
END IF

IF pel(xx, pv).ppp > 2 THEN
FOR fff = 3 TO 1 STEP -1
IF rug(xx, pv) + rua(xx, pv) + pel(xx, pv).gls + pel(xx, pv).ass >= CINT(vaat(fff, pel(xx, pv).psk)) THEN muutoz(xx) = muutoz(xx) + fff: EXIT FOR
NEXT fff
END IF
IF pel(xx, pv).ppp = 2 THEN
FOR fff = 3 TO 1 STEP -1
IF rug(xx, pv) + rua(xx, pv) + pel(xx, pv).gls + pel(xx, pv).ass >= CINT(vaat(fff, pel(xx, pv).psk) / 1.5) THEN muutoz(xx) = muutoz(xx) + fff: EXIT FOR
NEXT fff
END IF

bantu% = CINT(100 * RND)
SELECT CASE bantu
CASE 1 TO 9
muutoz(xx) = muutoz(xx) - 1
CASE 92 TO 100
muutoz(xx) = muutoz(xx) + 1
END SELECT
END IF
NEXT xx

FOR xx = 1 TO lpl(pv)
IF pel(xx, pv).psk + muutoz(xx) < 1 THEN
DO UNTIL pel(xx, pv).psk + muutoz(xx) = 1
muutoz(xx) = muutoz(xx) + 1
LOOP
END IF
IF pel(xx, pv).psk + muutoz(xx) > 20 THEN
DO UNTIL pel(xx, pv).psk + muutoz(xx) = 20
muutoz(xx) = muutoz(xx) - 1
LOOP
END IF
NEXT xx

manap
PRINT ", PELAAJIESI TAIDON MUUTOKSET!"
PRINT
FOR xxx = 1 TO lpl(pv)
xx = sort(xxx)
COLOR 14
PRINT pel(xx, pv).nam;
LOCATE , 16: PRINT pelip(pel(xx, pv).ppp);
LOCATE , 20: PRINT LTRIM$(STR$(pel(xx, pv).age));
COLOR 7
LOCATE , 25: PRINT LTRIM$(STR$(pel(xx, pv).psk));
IF muutoz(xx) > 0 THEN COLOR 15 ELSE IF muutoz(xx) = 0 THEN COLOR 7 ELSE COLOR 8
LOCATE , 28: PRINT "---> ";
PRINT LTRIM$(STR$(pel(xx, pv).psk + muutoz(xx)))
NEXT xxx
pjn

FOR xx = 1 TO lpl(pv)
pel(xx, pv).psk = pel(xx, pv).psk + muutoz(xx)
IF pel(xx, pv).spe <> 12 THEN pel(xx, pv).age = pel(xx, pv).age + 1
NEXT xx
RETURN

lahtevat:
CLS
manap
PRINT ", TAPAHTUMIA:"
PRINT
jeps:
FOR xx = 1 TO lpl(pv)
IF pel(xx, pv).lah >= 96 AND pel(xx, pv).svu = 0 THEN
printpels
SELECT CASE pel(xx, pv).lah
CASE 99
PRINT "ripustaa hokkarinsa lopullisesti naulaan."
CASE 98
PRINT "pakkaa kimpsunsa ja kampsunsa ja palaa kotimaahansa."
CASE 97
PRINT "l„htee isoihin ympyr”ihin rapakon taakse!";
COLOR 14: PRINT "(saat 100.000)": raha(pv) = raha(pv) + 100000
CASE 96
PRINT "evakuoituu p”rssin puolelle odottamaan tarjouksia!"
bel(lastbors + 1) = pel(xx, pv)
lastbors = lastbors + 1
END SELECT
poispel
GOTO jeps
END IF
NEXT xx
IF CSRLIN = 3 THEN PRINT "EI TAPAHTUMIA."
pjn
RETURN
END SUB

SUB actionprint
PRINT "ENTER: ";
SELECT CASE kr
CASE IS < 1
PRINT LTRIM$(STR$(1 - kr)); " VIIKKO";
IF kr < 0 THEN PRINT "A";
PRINT " KAUDEN ALKUUN";
CASE 4, 7, 11, 15, 19, 23
IF muke(pv) = 1 THEN PRINT "EHL-PELIPŽIVŽ";  ELSE PRINT "LEPOPŽIVŽ";
CASE 29
IF mukt(pv) > 0 THEN PRINT turnimi(mukt(pv));  ELSE PRINT "LEPOPŽIVŽ";
CASE 36
IF muke(pv) = 1 THEN PRINT "EHL:N LOPPUTURNAUS";  ELSE PRINT "LEPOPŽIVŽ";
CASE ELSE
IF ot < 44 THEN PRINT "RUNKOSARJAPELIPŽIVŽ";
IF ot = 44 THEN PRINT "NELJŽNNESFINAALIPLŽJŽYS";
IF ot = 45 AND mukp(pv) = 1 THEN PRINT "NELJŽNNESFINAALIPELIPŽIVŽ";  ELSE IF ot = 45 THEN PRINT "LEPOPŽIVŽ";
IF ot = 46 THEN PRINT "SEMIFINAALIPLŽJŽYS";
IF ot = 47 AND mukp(pv) = 1 THEN PRINT "SEMIFINAALIPELIPŽIVŽ";  ELSE IF ot = 47 THEN PRINT "LEPOPŽIVŽ";
IF ot = 48 THEN PRINT "FINAALIPLŽJŽYS";
IF ot = 49 AND mukp(pv) = 1 THEN PRINT "FINAALIPELIPŽIVŽ";  ELSE IF ot = 49 THEN PRINT "LEPOPŽIVŽ";
IF ot = 50 THEN PRINT "PHL:N JUHLAGAALA";
END SELECT
END SUB

SUB alasarja
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

SUB annapotkut
IF pel(xx, pv).inj <> 0 THEN EXIT SUB
COLOR , 0
tarjous = 0

LOCATE 41, 1

IF pel(xx, pv).svu > 0 THEN
COLOR 13: PRINT "IRTISANOMISRAHA: ";
COLOR 14
tarjous = 20 * pel(xx, pv).sra * pel(xx, pv).svu
PRINT LTRIM$(STR$(tarjous)); "  ";
END IF

COLOR 13
IF pel(xx, pv).svu > 0 THEN PRINT "IRTISANOMMEKO SOPIMUKSEN? (K/E)";  ELSE PRINT "POTKUT KAVERILLE? (K/E)";

sv$ = ""
DO UNTIL sv$ = "k" OR sv$ = "e"
sv$ = "": WHILE sv$ = "": sv$ = INKEY$: WEND
LOOP
IF sv$ = "e" THEN EXIT SUB

raha(pv) = raha(pv) - tarjous

IF lastbors < 300 THEN
bel(lastbors + 1) = pel(xx, pv)
bel(lastbors + 1).pok = 0
bel(lastbors + 1).neu = 1
lastbors = lastbors + 1
END IF

poispel
END SUB

SUB annarahaa (slok%)
manap
PRINT ", SPONSORITULOSI: ";
COLOR 14: PRINT sraha(slok%, pv)
raha(pv) = raha(pv) + sraha(slok%, pv)
PRINT
END SUB

SUB apulaisvalm
COLOR 2
LOCATE 19, 1: PRINT "ÉÍAPULAISVALMENTAJAN FLŽPPITAULUÍÍÍÍÍMEÍÍ»";
LOCATE 20, 1: PRINT "º                                 ³      º";
LOCATE 21, 1: PRINT "ºÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´      º";
LOCATE 22, 1: PRINT "º                                 ³      º";
LOCATE 23, 1: PRINT "º                                 ³      º";
LOCATE 24, 1: PRINT "º                                 ³      º";
LOCATE 25, 1: PRINT "º                                 ³      º";
LOCATE 26, 1: PRINT "º                                 ³      º";
LOCATE 27, 1: PRINT "º                                 ³      º";
LOCATE 28, 1: PRINT "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼";
COLOR 10
LOCATE 20, 36: PRINT "MV:"; LTRIM$(STR$(mw(u(pv))))
LOCATE 22, 36: PRINT "PU:"; LTRIM$(STR$(pw(u(pv))))
LOCATE 23, 36: PRINT "HY:"; LTRIM$(STR$(hw(u(pv))))
LOCATE 25, 36: PRINT "SI:"; LTRIM$(STR$(s(u(pv))))

IF haikka = 1 THEN LOCATE 20, 4: PRINT "VAILLINAINEN KETJUKOKOONPANO"

LOCATE 22
IF kr <= 1 THEN LOCATE , 2: PRINT "NEUVOTTELUAIKAA JŽLJELLŽ: "; : COLOR 14: PRINT LTRIM$(STR$(actiox(pv))): PRINT
IF krapu(pv) = 0 THEN
COLOR 10
IF kr <= 1 THEN LOCATE , 2: PRINT "KAUSIKORTTEJA MYYTY: "; : COLOR 14: PRINT LTRIM$(STR$(kausik(u(pv)))): COLOR 10
IF lpj(pv) > 0 THEN LOCATE , 2: PRINT "TUOREITA A-JUNNUJA SAATAVANA!"
IF tautip(u(pv)) = .95 THEN LOCATE , 2: PRINT "TREENIT MENIVŽT HUONOSTI!"
IF tautip(u(pv)) = 1.05 THEN LOCATE , 2: PRINT "TREENIT MENIVŽT HYVIN!"
IF ot = 30 THEN LOCATE , 2: PRINT "TREIDAUKSEN TAKARAJA UMPEUTUU!"
IF voodoo(pv) = 1 THEN LOCATE , 2: PRINT "VOODOO-UKKELI LURKKII NURKISSAMME"
IF bantu(pv) = 1 THEN LOCATE , 2: PRINT "PELAAJAT KUMMEKSUVAT TANSSIJOITA!"
IF kuume(pv) = 1 THEN LOCATE , 2: PRINT "KAUPUNGISSA VALLITSEE KIEKKOKUUME"
IF raha(pv) < 0 THEN LOCATE , 2: PRINT "TARVITSEMME JOSTAKIN ŽKKIŽ RAHAA!"

temp% = 0: ccc = 0
FOR z = 1 TO lpl(pv)
IF pel(z, pv).ket > 0 AND pel(z, pv).kun < 0 THEN temp% = 1
IF pel(z, pv).ket > 0 AND pel(z, pv).kun <= -5 THEN temp% = 2
IF pel(z, pv).svu = 0 THEN ccc = 1
NEXT z
IF temp% = 1 THEN LOCATE , 2: PRINT "KOKOONPANOSSAMME ON VŽSYMYSTŽ!"

IF ccc = 1 THEN LOCATE , 2:   PRINT "KAIKILLA PELAAJILLA EI SOPIMUSTA!"
END IF
IF krapu(pv) >= 1 THEN LOCATE , 2: PRINT "™RGH...PŽŽTŽNI SŽRKEE VIETŽVŽSTI!"
END SUB

SUB apulaisvalm2
IF valb(1, pv) > 1 THEN
COLOR 5
LOCATE 30, 1: PRINT "ÉÍÍÍÍKATSOMOVALMENTAJAN FLŽPPITAULUÍÍÍÍARVIOÍ»";
LOCATE 31, 1: PRINT "º                                     ³      º";
LOCATE 32, 1: PRINT "ºÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´      º";
LOCATE 33, 1: PRINT "º                                     ³      º";
LOCATE 34, 1: PRINT "º                                     ³      º";
LOCATE 35, 1: PRINT "º                                     ³      º";
LOCATE 36, 1: PRINT "º                                     ÀÄÄÄÄÄÄº";
LOCATE 37, 1: PRINT "º                                            º";
LOCATE 38, 1: PRINT "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼";
COLOR 13
LOCATE 31
LOCATE , 2
PRINT RTRIM$(l(vast(pv))); " "; "("; mana(man(vast(pv))); ")"
LOCATE 33
IF ohj(vast(pv)) = 0 THEN
IF tauti(1, tox(vast(pv))) < 0 THEN LOCATE , 2: PRINT "YKK™SMAALIVAHTI ON SAIRAANA!"
IF tauti(2, tox(vast(pv))) < 0 THEN LOCATE , 2: PRINT "LOUKKAANTUMISIA PAKKIKALUSTOSSA!"
IF tauti(3, tox(vast(pv))) < 0 THEN LOCATE , 2: PRINT "LOUKKAANTUMISIA HY™KKŽYSKALUSTOSSA!"
END IF
LOCATE , 2
IF tautip(vast(pv)) < 1 THEN PRINT "JOUKKUE VAIKUTTAA MASENTUNEELTA!"
IF tautip(vast(pv)) > 1 THEN PRINT "JOUKKUE VAIKUTTAA MOTIVOITUNEELTA!"

IF ww = 0 THEN
SELECT CASE valb(1, pv)
CASE 2
xx = 20
xxx = 40
xxxx = 80
CASE 3
xx = 2
xxx = 4
xxxx = 8
CASE 4
xx = 0
xxx = 2
xxxx = 4
END SELECT

IF valb(1, pv) < 4 THEN
wirhe(1) = CINT(-xx * RND) + (xx / 2)
IF mw(vast(pv)) + wirhe(1) < 0 THEN wirhe(1) = wirhe(1) - (mw(vast(pv)) + wirhe(1) - 1)
END IF
IF valb(1, pv) < 5 THEN
wirhe(2) = CINT(-xxx * RND) + (xxx / 2)
IF pw(vast(pv)) + wirhe(2) < 0 THEN wirhe(2) = wirhe(2) - (pw(vast(pv)) + wirhe(1) - 1)
wirhe(3) = CINT(-xxxx * RND) + (xxxx / 2)
IF hw(vast(pv)) + wirhe(2) < 0 THEN wirhe(3) = wirhe(3) - (hw(vast(pv)) + wirhe(1) - 1)
END IF
END IF

LOCATE 31, 40: PRINT "MV:"; LTRIM$(STR$(mw(vast(pv)) + wirhe(1)))
LOCATE 32, 40: PRINT "PU:"; LTRIM$(STR$(pw(vast(pv)) + wirhe(2)))
LOCATE 33, 40: PRINT "HY:"; LTRIM$(STR$(hw(vast(pv)) + wirhe(1)))
LOCATE 35, 40
PRINT "SI:";
SELECT CASE valb(1, pv)
CASE IS < 3
PRINT LTRIM$(STR$(CINT(11 * RND) + 1))
CASE ELSE
PRINT LTRIM$(STR$(s(vast(pv))))
END SELECT

IF valb(1, pv) = 2 OR valb(1, pv) = 5 AND spec(pv) > 0 THEN
OPEN "data\spec" + LTRIM$(STR$(valb(1, pv))) + ".mhm" FOR INPUT AS #1
FOR zz = 1 TO spec(pv)
LINE INPUT #1, temp$
NEXT zz
CLOSE #1
LOCATE 37, 2: COLOR 12
IF ohj(vast(pv)) = 0 THEN
IF valb(1, pv) = 2 THEN PRINT temp$ ELSE PRINT UCASE$(RTRIM$(top(tox(vast(pv)), 1).nam)); " "; temp$
END IF
END IF

END IF
ww = 1
END SUB

SUB areena

areenalku:
CLS : COLOR 13
PRINT "JŽŽHALLIN NIMI: ";
COLOR 12: PRINT haln(u(pv))
PRINT : COLOR 13
PRINT "JŽŽHALLIN TASO: ";
COLOR 14: PRINT hallina(halli(u(pv))); "     ("; LTRIM$(STR$(halli(u(pv)))); "/14)"
PRINT : COLOR 10
IF halliip(halli(u(pv))) > 0 THEN PRINT " ISTUMAPAIKKOJA  "; : COLOR 14: PRINT LTRIM$(STR$(halliip(halli(u(pv)))))
COLOR 10
IF hallisp(halli(u(pv))) > 0 THEN PRINT "SEISOMAPAIKKOJA  "; : COLOR 14: PRINT LTRIM$(STR$(hallisp(halli(u(pv)))))
COLOR 10
IF halliai(halli(u(pv))) > 0 THEN PRINT "       AITIOITA  "; : COLOR 14: PRINT LTRIM$(STR$(halliai(halli(u(pv)))))
PRINT : COLOR 10
haahaa (4)
IF halli(u(pv)) < 14 THEN
LOCATE 9, 1
COLOR 13: PRINT "           SALDO  ";
COLOR 14: PRINT LTRIM$(STR$(raha(pv)))
PRINT
COLOR 13: PRINT "   RAKENNUSPOTTI  ";
COLOR 14: PRINT LTRIM$(STR$(potti(pv)))
COLOR 13: PRINT "VAADITTAVA SUMMA  ";
COLOR 14: PRINT LTRIM$(STR$(halvaat(halli(u(pv)))))
PRINT : COLOR 12
IF rstage(pv) = 0 THEN PRINT "P: LAITA RAHAA POTTIIN"
COLOR 13
SELECT CASE rstage(pv)
CASE 0
IF potti(pv) >= halvaat(halli(u(pv))) THEN COLOR 29: PRINT "A: ANO RAKENNUSLUPAA"
CASE 1
PRINT "RAKENNUSLUPA-ANOMUS ON BYROKRAATTIEN PIHDEISSŽ!"
CASE 2
PRINT "AITIOIDEN MYYNTI MENEILLŽŽN!"
CASE 3
PRINT "YHTEISTY™KUMPPANIN ETSINTŽ MENEILLŽŽN!"
CASE 4
PRINT "RAKENNUSTY™T KŽYNNISSŽ! ("; LTRIM$(STR$(ractio(pv) - 1)); " KR)"
END SELECT
END IF
IF krapu(pv) = 2 THEN krapulapiirto
DO
ar$ = ""
WHILE ar$ = "": ar$ = INKEY$: WEND
IF ar$ = "?" THEN qelp (4): GOTO areenalku
IF halli(u(pv)) < 14 THEN
IF ar$ = "a" AND rstage(pv) = 0 AND potti(pv) >= halvaat(halli(u(pv))) THEN
LOCATE 17, 1: COLOR 7: PRINT "OLETKO TŽYSIN VARMA, PROSESSI ON PERUUTTAMATON (K/E) ? ?"
var$ = ""
DO UNTIL var$ = "k" OR var$ = "e"
var$ = INKEY$
LOOP
IF var$ = "k" THEN rstage(pv) = 1: GOTO areenalku ELSE GOTO areenalku
END IF
IF ar$ = "p" THEN
COLOR 15: LOCATE 3, 50: PRINT "KUINKA PALJON?";
CALL tarjousmaar(5, 52)
potti(pv) = potti(pv) + tarjous
raha(pv) = raha(pv) - tarjous
GOTO areenalku
END IF
END IF

IF ar$ = "k" THEN SCREEN 13: kuva ("halli" + LTRIM$(STR$(halli(u(pv))))): SCREEN 0: WIDTH 80, 43: GOTO areenalku
IF ar$ = "z" AND halli(u(pv)) < 14 THEN halli(u(pv)) = halli(u(pv)) + 1
IF ar$ = CHR$(27) THEN EXIT DO
LOOP

END SUB

SUB arpo
IF pirtar(4) <> 723 THEN SYSTEM

xx = CINT(23 * RND) + 1
DO UNTIL ohj(xx) = 0
xx = CINT(23 * RND) + 1
LOOP
END SUB

SUB arpod (voiz%)

FOR z = 1 TO 10
xx = CINT(11 * RND) + 13
DO UNTIL ohj(x(xx)) = 0
xx = CINT(11 * RND) + 13
LOOP
IF tazo(x(xx)) >= voiz% THEN lukka = 0: xx = x(xx): EXIT SUB
NEXT z
lukka = 1
END SUB

SUB arpods (sij%)
FOR zz = 1 TO sij%
FOR z = 13 TO 24
IF s(x(z)) = zz AND ohj(x(z)) = 0 THEN xx = x(z): lukka = 0: EXIT SUB
NEXT z
NEXT zz
lukka = 1
END SUB

SUB arpods2 (sij%)
FOR zz = 12 TO sij% STEP -1
FOR z = 13 TO 24
IF s(x(z)) = zz AND ohj(x(z)) = 0 THEN xx = x(z): lukka = 0: EXIT SUB
NEXT z
NEXT zz
lukka = 1
END SUB

SUB arpol (voiz%)
FOR z = 1 TO 10
xx = CINT(11 * RND) + 1
DO UNTIL ohj(x(xx)) = 0
xx = CINT(11 * RND) + 1
LOOP
IF tazo(x(xx)) >= voiz% THEN lukka = 0: xx = x(xx): EXIT SUB
NEXT z
lukka = 1
END SUB

SUB arpol2 (voiz%)
FOR z = 1 TO 10
xx = CINT(11 * RND) + 1
DO UNTIL ohj(x(xx)) = 0
xx = CINT(11 * RND) + 1
LOOP
IF tazo(x(xx)) <= voiz% THEN lukka = 0: xx = x(xx): EXIT SUB
NEXT z
lukka = 1
END SUB

SUB arpols (sij%)
FOR zz = 1 TO sij%
FOR z = 1 TO 12
IF s(x(z)) = zz AND ohj(x(z)) = 0 THEN xx = x(z): lukka = 0: EXIT SUB
NEXT z
NEXT zz
lukka = 1
END SUB

SUB arpols2 (sij%)
FOR zz = 12 TO sij% STEP -1
FOR z = 1 TO 12
IF s(x(z)) = zz AND ohj(x(z)) = 0 THEN xx = x(z): lukka = 0: EXIT SUB
NEXT z
NEXT zz
lukka = 1
END SUB

SUB arpols3 (voiz%)
alkunaat:
SELECT CASE voiz%
CASE 1
min% = 18: rando% = 6
GOSUB perse
IF lukka = 1 THEN GOTO alkunaat

CASE 2
min% = 13: rando% = 11
GOSUB perse
IF lukka = 1 THEN GOTO alkunaat
CASE 3
min% = 8: rando% = 7
GOSUB perse
IF lukka = 1 THEN min% = 8: rando% = 16: GOSUB perse
IF lukka = 1 THEN GOTO alkunaat
CASE 4
min% = 4: rando% = 8
GOSUB perse
IF lukka = 1 THEN GOTO alkunaat

CASE 5
min% = 1: rando% = 4
GOSUB perse
IF lukka = 1 THEN min% = 1: rando% = 12: GOSUB perse
IF lukka = 1 THEN GOTO alkunaat

END SELECT

EXIT SUB

perse:
FOR zzzz = 1 TO 10
xxx = CINT(rando% * RND) + min%
xx = x(xxx)
IF ohj(xx) = 0 THEN lukka = 0: RETURN
NEXT zzzz
lukka = 1: RETURN
END SUB

SUB arvoloukkaa (fat%)
lukka = 1
xxx = 0
jj = lpl(pv) - 1
FOR zz = 1 TO 10
xxx = CINT(jj * RND) + 1
SELECT CASE fat%
CASE 1
IF pel(xxx, pv).inj = 0 AND pel(xxx, pv).ket > 0 THEN lukka = 0: EXIT SUB
CASE 2
IF pel(xxx, pv).plus = 0 AND pel(xxx, pv).kest = 0 THEN lukka = 0: EXIT SUB
CASE 3
IF pel(xxx, pv).inj = 0 THEN lukka = 0: EXIT SUB
CASE 4
IF pel(xxx, pv).spe = 0 AND pel(xxx, pv).psk > 9 THEN lukka = 0: EXIT SUB
CASE 5
IF pel(xxx, pv).ego < 10 THEN lukka = 0: EXIT SUB
CASE 6
IF pel(xxx, pv).ego > 9 THEN lukka = 0: EXIT SUB
END SELECT
NEXT zz
END SUB

SUB avaaturnausdata
DIM turnimi(1 TO 6) AS STRING
OPEN "data\data10.99" FOR INPUT AS #1
FOR zz = 1 TO 6
INPUT #1, turnimi(zz)
NEXT zz
CLOSE #1
END SUB

SUB baaorsmuutos

IF lastbors > 10 THEN
FOR xx = 1 TO lastbors
IF CINT(100 * RND) > 80 - bel(xx).psk THEN GOSUB tekeesopim
NEXT xx
END IF

FOR xx = 1 TO 10
IF CINT(100 * RND) > 50 THEN
firstborsgene = lastbors + 1: borsgenex = lastbors + 1: borsgene
END IF
NEXT xx

EXIT SUB

tekeesopim:
IF xx < lastbors THEN
FOR smut = xx + 1 TO lastbors
bel(smut - 1) = bel(smut)
NEXT smut
END IF
lastbors = lastbors - 1
RETURN


END SUB

SUB bors

IF lastbors < 10 THEN
firstborsgene = lastbors + 1
borsgenex = 10
borsgene
END IF

DIM sortb(1 TO lastbors) AS INTEGER
DIM sortc(1 TO 32) AS INTEGER
borsch = 0
kurso = 1: oldkurso = 1
beginnato:
CLS
COLOR 15, 0: PRINT "ODOTA - PELAAJAT JŽRJESTŽYTYVŽT PARIJONOON"
IF kurso < 1 THEN kurso = 1: oldkurso = 1
IF borsch > 0 THEN
IF kurso > lpl(borsch) THEN kurso = lpl(borsch): oldkurso = lpl(borsch)
END IF
jukka = 0
IF borsch = 0 THEN sortbors (1) ELSE sortbors (2)
CLS
borschpiirto
DO
IF krapu(pv) = 2 AND borsch = 0 THEN
kurso = CINT((lastbors - 1) * RND) + 1
END IF
SELECT CASE borsch
CASE 0
IF lastbors > 0 THEN
IF kurso < 21 THEN
IF lastbors < 39 THEN grr = lastbors ELSE grr = 39
FOR xx = 1 TO grr
IF kurso = xx THEN COLOR , 1 ELSE COLOR , 0
LOCATE xx + 2, 1: printpel (5)
NEXT xx
END IF
IF kurso > 20 AND kurso + 19 <= lastbors THEN
COLOR , 1: LOCATE 22, 1: xx = kurso: printpel (5)
COLOR , 0
FOR xx = kurso - 1 TO kurso - 19 STEP -1
LOCATE 22 - (kurso - xx), 1: printpel (5)
NEXT xx
IF lastbors < kurso + 19 THEN grr = lastbors ELSE grr = kurso + 19
FOR xx = kurso + 1 TO grr
LOCATE 22 + (xx - kurso), 1: printpel (5)
NEXT xx
END IF
IF kurso > 20 AND kurso + 19 > lastbors THEN
zzz = 1
IF lastbors - 38 < 1 THEN grr = 1 ELSE grr = lastbors - 38
FOR xx = grr TO lastbors
IF kurso = xx THEN COLOR , 1 ELSE COLOR , 0
LOCATE 2 + zzz, 1: printpel (5)
zzz = zzz + 1
NEXT xx
END IF
END IF
CASE IS > 0
COLOR 13, 0
IF lpl(borsch) > 0 THEN
FOR zz = 1 TO lpl(borsch)
IF zz = kurso THEN COLOR , 1 ELSE COLOR , 0
xx = sortc(zz)
LOCATE zz + 2, 1: printpel (2)
NEXT zz
END IF
END SELECT
joo$ = ""
WHILE joo$ = "": joo$ = INKEY$: WEND
IF joo$ = CHR$(27) THEN COLOR , 0: GOTO enteron
IF joo$ = "?" THEN qelp (3): EXIT DO
IF joo$ = CHR$(0) + CHR$(75) AND borsch > 0 AND borsch - 1 <> pv THEN borsch = borsch - 1: EXIT DO ELSE IF joo$ = CHR$(0) + CHR$(75) AND borsch > 1 AND borsch - 1 = pv THEN borsch = borsch - 2: EXIT DO
IF joo$ = CHR$(0) + CHR$(77) AND borsch < plkm AND borsch + 1 <> pv THEN borsch = borsch + 1: EXIT DO ELSE IF joo$ = CHR$(0) + CHR$(77) AND borsch < plkm - 1 AND borsch + 1 = pv THEN borsch = borsch + 2: EXIT DO
SELECT CASE borsch
CASE 0
IF joo$ = CHR$(0) + CHR$(72) AND kurso > 1 THEN kurso = kurso - 1
IF joo$ = CHR$(0) + CHR$(80) AND kurso < lastbors THEN kurso = kurso + 1
IF joo$ = "o" AND actiox(pv) > 0 AND lpl(pv) < 32 AND lastbors > 0 THEN COLOR , 0: sopimusext (2): EXIT DO
IF joo$ = "+" AND kurso + 10 <= lastbors THEN kurso = kurso + 10
IF joo$ = "-" AND kurso - 10 >= 1 THEN kurso = kurso - 10
COLOR , 0
CASE IS > 0
IF joo$ = CHR$(0) + CHR$(72) AND kurso > 1 THEN kurso = kurso - 1: oldkurso = kurso + 1
IF joo$ = CHR$(0) + CHR$(80) AND kurso < lpl(borsch) THEN kurso = kurso + 1: oldkurso = kurso - 1
IF actiox(pv) > 0 THEN
IF pel(sortc(kurso), borsch).ket = 0 THEN
IF joo$ = "o" AND lpl(pv) < 32 AND lpl(borsch) > 5 AND pel(sortc(kurso), borsch).svu > 0 THEN COLOR , 0: osto2: COLOR , 0: EXIT DO
IF joo$ = "o" AND lpl(pv) < 32 AND lpl(borsch) > 5 AND pel(sortc(kurso), borsch).svu = 0 THEN COLOR , 0: ote = borsch: xx = sortc(kurso): sopimusext (1): COLOR , 0: EXIT DO
END IF
END IF
END SELECT
LOOP
COLOR , 0
GOTO beginnato
enteron:

ERASE sortb, sortc
END SUB

SUB borschpiirto
LOCATE 1, 1: COLOR 14
IF borsch = 0 THEN COLOR , 14 ELSE COLOR , 0
PRINT "PELAAJAMARKKINAT";
COLOR , 0: PRINT "    ";
IF plkm = 1 THEN GOTO sivup
FOR zz = 1 TO plkm
IF zz = pv THEN zz = zz + 1
IF zz = plkm + 1 THEN EXIT FOR
IF borsch = zz THEN COLOR , 14 ELSE COLOR , 0
PRINT nimi(zz);
COLOR , 0: PRINT "   ";
NEXT zz
sivup:
haahaa (3)
END SUB

SUB borsgene
DIM taso(1 TO 12, 1 TO 5) AS INTEGER
DIM pros(1 TO 12, 1 TO 5) AS INTEGER
DIM agep(1 TO 100) AS INTEGER
OPEN "data\data5.99" FOR INPUT AS #1
FOR zz = 1 TO 12
INPUT #1, taso(zz, 1), taso(zz, 2), taso(zz, 3), taso(zz, 4), taso(zz, 5)
NEXT zz
FOR zz = 1 TO 12
INPUT #1, pros(zz, 1), pros(zz, 2), pros(zz, 3), pros(zz, 4), pros(zz, 5)
NEXT zz
CLOSE #1
OPEN "data\data5_2.99" FOR INPUT AS #1
FOR zz = 1 TO 100
INPUT #1, agep(zz)
NEXT zz
CLOSE #1
FOR xxx = firstborsgene TO borsgenex
bel(xxx) = blanko(1)
keisi
keisi2 (1)
mahmax
bel(xxx).nam = krj$ + "." + ptemp$
bel(xxx).nat = nats
skillmaarbors
IF bel(xxx).psk < 1 THEN bel(xxx).psk = 1
agex = CINT(96 * RND) + 4
bel(xxx).age = agep(agex)
bel(xxx).gls = 0
bel(xxx).ass = 0
egomaar (2)

IF bel(xxx).psk > 6 THEN
temp% = CINT(100 * RND)
SELECT CASE temp%
CASE 1 TO 10
bel(xxx).spe = 8
CASE 11 TO 15
bel(xxx).spe = 5
CASE 95, 96, 97
bel(xxx).spe = 2
CASE 98, 99, 100
IF bel(xxx).age >= 30 THEN bel(xxx).spe = 1
END SELECT
END IF

NEXT xxx
lastbors = xxx - 1
ERASE taso, pros, agep
END SUB

SUB budget
IF pirtar(5) <> 509 THEN SYSTEM
CLS
haahaa (13)
LOCATE 1, 1
manap
PRINT ", BUDJETOINTI"

LOCATE 4, 1
FOR zz = 1 TO 5
nude = valb(zz, pv)
IF sr(u(pv)) = 1 THEN min% = 3 ELSE min% = 1
IF zz = 5 THEN min% = 1
IF nude < min% THEN nude = min%
DO
LOCATE 3 + 7 * (zz - 1), 1: COLOR 13
PRINT valn(zz); "  ";
COLOR 14: PRINT LTRIM$(STR$(valbh(vai2(pv), zz, nude)));
IF zz < 5 THEN PRINT " (KIERROS)"; "  ";  ELSE PRINT " (PELAAJA/KIERROS)";
COLOR 6
IF nude < 5 THEN PRINT CHR$(24);
IF nude > min% THEN PRINT CHR$(25);
PRINT "     ";
LOCATE 3 + 7 * (zz - 1) + 2, 1
FOR ggg = 1 TO 5
PRINT STRING$(80, " ")
NEXT ggg
LOCATE 3 + 7 * (zz - 1) + 2, 1: COLOR 12: luetexti (LTRIM$(STR$(zz)) + LTRIM$(STR$(nude)) + "valm")
tar$ = ""
WHILE tar$ = "": tar$ = INKEY$: WEND
IF tar$ = CHR$(0) + CHR$(72) AND nude < 5 THEN nude = nude + 1
IF tar$ = CHR$(0) + CHR$(80) AND nude > min% THEN nude = nude - 1
IF tar$ = CHR$(13) THEN EXIT DO
LOOP
valb(zz, pv) = nude
NEXT zz
END SUB

SUB cccptabl
IF pirtar(6) <> 993 THEN SYSTEM
COLOR 14, 0
CLS
PRINT RTRIM$(pel(xx, pv).nam);
SELECT CASE pel(xx, pv).inj
CASE 1 TO 999
COLOR 13
PRINT " tuntee olonsa yh„ sairaammaksi, ja loma pitenee!"
pel(xx, pv).inj = pel(xx, pv).inj + 5
CASE ELSE
kuinka% = CINT(99 * RND) + 1
IF pel(xx, pv).inj > 1000 THEN kuinka% = 33
SELECT CASE kuinka%
CASE 1 TO 30
COLOR 13
PRINT " tuntee olonsa sairaaksi, ja joutuu vuoteen omaksi 5 viikon ajaksi."
pel(xx, pv).inj = pel(xx, pv).inj + 5
CASE 31 TO 70
COLOR 13
PRINT " tuntee olonsa samanlaiseksi kuin ennenkin.Pilleri ei vaikuttanut."
CASE 71 TO 100
COLOR 13
PRINT " tuntee olonsa rrrautaiseksi. Pillerin vaikutus on ihmeellinen!"
pel(xx, pv).psk = pel(xx, pv).psk + 1
END SELECT
END SELECT
cccp(pv) = cccp(pv) - 1
pjn
END SUB

SUB cheatmenu
DIM ntemp(21) AS STRING * 1

FOR ggg = 1 TO 21
ntemp(ggg) = ""
NEXT ggg

code$ = ""

kurso = 1
COLOR , 0
CLS : COLOR 13
PRINT "AHHA HAA!! L™YSIT MHM 99 - HUIJAUSVALIKON!!!"
PRINT "TŽSTŽ EDESPŽIN KAIKKI ONKIN HELPPOA, KIRJOITAT VAIN KOODIN"
PRINT "JA PAINAT ENTERIŽ???"

DO
FOR zz = 1 TO 21
LOCATE 5, zz: COLOR 12: PRINT ntemp(zz);
NEXT zz
LOCATE 5, kurso: COLOR 14: PRINT "Û"
jep$ = "": WHILE jep$ = "": jep$ = INKEY$: WEND
IF ASC(jep$) >= 33 AND kurso < 21 THEN ntemp(kurso) = jep$: kurso = kurso + 1
IF jep$ = CHR$(8) AND kurso > 1 THEN ntemp(kurso - 1) = "": kurso = kurso - 1
IF jep$ = CHR$(32) AND kurso < 20 THEN ntemp(kurso) = " ": kurso = kurso + 1
IF jep$ = CHR$(13) THEN EXIT DO
LOOP
mukka = kurso - 1

IF mukka > 0 THEN
FOR zzz = 1 TO mukka
code$ = code$ + ntemp(zzz)
NEXT zzz
END IF

LOCATE 7, 1: COLOR 15

SELECT CASE code$
CASE "jumalainenvalta"
cheat(1) = 1
PRINT "JEES! KOKEILE ERI NŽPPŽINYHDISTELMIŽ A-MENUSSA, SAATAT YLLŽTTYŽ!"
CASE "harryharkimo"
PRINT "JEES! NYT EI RAHAN AINAKAAN PITŽISI LOPPUA???"
raha(pv) = 1000000000
CASE "nuoruudenl„hde"
FOR xx = 1 TO lpl(pv)
pel(xx, pv).age = 18
NEXT xx
PRINT "HOPLAA! PELAAJASI OVAT NYT JUONEET NUORUUDENLŽHTEEN MAKOISAA VETTŽ!"
CASE "palkka-ale"
FOR xx = 1 TO lpl(pv)
pel(xx, pv).sra = pel(xx, pv).sra * .5
NEXT xx
PRINT "50% LEIKKAUS PELAAJIEN PALKKOIHIN ON NYT TOSIASIA!!!"
CASE "kusijukka"
cheat(2) = 1
PRINT "KUN KUSIJUKKA(TM) ON MUKANA SAUNA-ILLASSA, EI MIKŽŽN VOI MENNŽ PIELEEN!?!"
PRINT "(Maso Software pahoittelee t„llaista t”rke„„ inside-vitsi„)"
CASE "annamullemegahalli"
halli(u(pv)) = 14
PRINT "NO ANNANHAN MINŽ...VORTEX-LUOKAN AREENAN NIMITTŽIN!"
CASE ELSE
PRINT "PAH! MIKŽS KOODI SINŽ LUULET OIKEIN OLEVASI???"
END SELECT

pjn
ERASE ntemp

END SUB

SUB checkaus
CLS
COLOR 14: PRINT "MHM 99 v1.3 WG, Huhtikuu 1999"
PRINT
PRINT "IleX versio 4.8.99 "; stingy(pv)
PRINT
PRINT "WAYNE GRETZKYŽ KAIVATEN - MIKKO FORSSTR™M."
PRINT
COLOR 29
IF FRE(-1) > 20000 THEN PRINT "MUISTIA ON RIITTŽVŽSTI! ("; LTRIM$(STR$(FRE(-1))); ")" ELSE PRINT "ONGELMIA SAATTAA ILMETŽ - MUISTI ON VŽHISSŽ! "; LTRIM$(STR$(FRE(-1)))
PRINT
COLOR 2
PRINT "MHM 99 SUUNNITTELU & OHJELMOINTI: ";
COLOR 10
PRINT "MIKKO FORSSTR™M"
PRINT
COLOR 2
PRINT "KUVANKŽSITTELYRUTIINIT: ";
COLOR 10
PRINT "BRILLIANTS SOFTWARE"
PRINT
COLOR 13
PRINT "MHM 99 (C) 1998, 1999 MASO SOFTWARE. ALL RIGHTS RESERVED."
PRINT "                                     KAIKKI OIKEUDET PIDŽTETŽŽN."
PRINT
PRINT "WORLD WIDE WEB: WWW.SAUNALAHTI.FI/FORSTER"
PRINT
PRINT "E-MAIL: FORSTER@SAUNALAHTI.FI"
PRINT
COLOR 9
PRINT "KIITOS KAIKILLE KEN TEKIVŽT MHM 99:N MAHDOLLISEKSI. TIE ON OLLUT"
PRINT "PITKŽ JA RASKAS, MUTTA NYT SE ON LOPPU. MHM 2K TULLEE JA TAPPANEE!"
pjn
END SUB

SUB cndcheck
cnd = 0
SELECT CASE ot
CASE 45
IF pwin(qfin(1)) = 3 OR pwin(qfin(8)) = 3 THEN cnd = cnd + 1
IF pwin(qfin(2)) = 3 OR pwin(qfin(7)) = 3 THEN cnd = cnd + 1
IF pwin(qfin(3)) = 3 OR pwin(qfin(6)) = 3 THEN cnd = cnd + 1
IF pwin(qfin(4)) = 3 OR pwin(qfin(5)) = 3 THEN cnd = cnd + 1
IF pwin(qdin(1)) = 3 OR pwin(qdin(6)) = 3 THEN cnd = cnd + 1
IF pwin(qdin(2)) = 3 OR pwin(qdin(5)) = 3 THEN cnd = cnd + 1
IF pwin(qdin(3)) = 3 OR pwin(qdin(4)) = 3 THEN cnd = cnd + 1
IF cnd = 7 AND ot = 45 THEN ot = ot + 1
CASE 47
IF pwin(sfin(1)) = 3 OR pwin(sfin(4)) = 3 THEN cnd = cnd + 1
IF pwin(sfin(2)) = 3 OR pwin(sfin(3)) = 3 THEN cnd = cnd + 1
IF pwin(sdin(1)) = 3 OR pwin(sdin(4)) = 3 THEN cnd = cnd + 1
IF pwin(sdin(2)) = 3 OR pwin(sdin(3)) = 3 THEN cnd = cnd + 1
IF cnd = 4 AND ot = 47 THEN ot = ot + 1
CASE 49
IF pwin(fin(1)) = 3 OR pwin(fin(2)) = 3 THEN cnd = cnd + 1
IF pwin(bin(1)) = 3 OR pwin(bin(2)) = 3 THEN cnd = cnd + 1
IF pwin(din(1)) = 3 OR pwin(din(2)) = 3 THEN cnd = cnd + 1
IF cnd = 3 AND ot = 49 THEN ot = ot + 1
END SELECT
END SUB

SUB colchk
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

SUB colchk2
IF nn = u(1) THEN COLOR 15
IF plkm < 2 THEN EXIT SUB
IF nn = u(2) THEN COLOR 3
IF plkm < 3 THEN EXIT SUB
IF nn = u(3) THEN COLOR 11
IF plkm < 4 THEN EXIT SUB
IF nn = u(4) THEN COLOR 9
END SUB

SUB colchk3
IF mm = u(1) THEN COLOR 15
IF plkm < 2 THEN EXIT SUB
IF mm = u(2) THEN COLOR 3
IF plkm < 3 THEN EXIT SUB
IF mm = u(3) THEN COLOR 11
IF plkm < 4 THEN EXIT SUB
IF mm = u(4) THEN COLOR 9
END SUB

SUB colchk4
SELECT CASE ccc
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

SUB egomaar (fat%)
temp% = CINT(10 * RND)
IF pirtar(7) <> 1048 THEN temp% = 99
SELECT CASE fat%
CASE 1
pel(xxx, pv).ego = temp%
CASE 2
bel(xxx).ego = temp%
CASE 3
jel(xxx, pv).ego = temp%
END SELECT
END SUB

SUB ehllopturmaar
DIM kak(5) AS INTEGER
DIM kaks(5) AS INTEGER
DIM elt(1 TO 6) AS INTEGER
staulehlmaar
FOR xx = 1 TO 4
IF es(xx) = 1 THEN elt(1) = ex(xx)
IF es(xx) = 2 THEN kak(1) = xx
IF es(xx + 4) = 1 THEN elt(2) = ex(xx + 4)
IF es(xx + 4) = 2 THEN kak(2) = xx + 4
IF es(xx + 8) = 1 THEN elt(3) = ex(xx + 8)
IF es(xx + 8) = 2 THEN kak(3) = xx + 8
IF es(xx + 12) = 1 THEN elt(4) = ex(xx + 12)
IF es(xx + 12) = 2 THEN kak(4) = xx + 12
IF es(xx + 16) = 1 THEN elt(5) = ex(xx + 16)
IF es(xx + 16) = 2 THEN kak(5) = xx + 16
NEXT xx

FOR xx = 1 TO plkm
muke(xx) = 0
NEXT xx

FOR xx = 1 TO 4
FOR xxx = xx + 1 TO 5
IF ep(kak(xx)) > ep(kak(xxx)) THEN kaks(xxx) = kaks(xxx) + 1
IF ep(kak(xx)) < ep(kak(xxx)) THEN kaks(xx) = kaks(xx) + 1
IF ep(kak(xx)) = ep(kak(xxx)) THEN GOSUB kaksis
NEXT xxx
NEXT xx
FOR xx = 1 TO 5
IF kaks(xx) = 0 THEN elt(6) = ex(kak(xx)): EXIT FOR
NEXT xx
ERASE kak, kaks
CLS
COLOR 14: PRINT "EHL:N lopputurnaukseen ovat selvinneet seuraavat joukkueet:"
PRINT
FOR xx = 1 TO 6
nn = elt(xx)
COLOR 12
colchk2
PRINT l(elt(xx))
NEXT xx
PRINT
FOR xx = 1 TO 6
nn = elt(xx)
colchk2
IF ohj(elt(xx)) <> 0 THEN PRINT "Onneksi olkoon, manageri "; nimi(ohj(elt(xx))); "! P„„sitte lopputurnaukseen!": muke(ohj(elt(xx))) = 1: PRINT
NEXT xx
PRINT
FOR vu = 1 TO plkm
pv = vuoro(vu)
IF muke(pv) = 1 THEN annarahaa (7)
NEXT vu
pjn
EXIT SUB
kaksis:
IF ero(kak(xx)) >= ero(kak(xxx)) THEN kaks(xxx) = kaks(xxx) + 1 ELSE kaks(xx) = kaks(xx) + 1
RETURN
END SUB

SUB ehllopturmaar2
etu1 = 1: etu2 = 1
turnausaloita
maarpalk ("elt")
tnimi(1) = "EHL:N LOPPUTURNAUS"
FOR xx = 1 TO 6
tx(xx) = elt(xx)
NEXT xx
naaturnaus = 1
turnaus
FOR xx = 1 TO 6
IF ts(xx) = 1 THEN EXIT FOR
NEXT xx
emestari = tx(xx)
IF ohj(emestari) <> 0 THEN pv = 1: SCREEN 13: kuva ("winehl"): SCREEN 0: WIDTH 80, 43
CLS
COLOR 13: PRINT "JŽŽKIEKON EUROOPAN MESTARI ON"
PRINT : COLOR 14
PRINT RTRIM$(l(emestari)); " ("; kk(emestari); ")"
PRINT
IF ohj(emestari) <> 0 THEN PRINT "Onneksi olkoon, manageri "; nimi(ohj(emestari)); "!!": euros(ohj(emestari)) = euros(ohj(emestari)) + 1
PRINT
FOR vu = 1 TO plkm
pv = vuoro(vu)
IF emestari = u(pv) THEN annarahaa (2)
IF muke(pv) = 1 THEN muke(pv) = 0
NEXT vu
pjn
turnauslopeta
kr = kr + 1
ERASE elt
END SUB

SUB endseason
FOR zz = 1 TO 2
pvoittaja(zz + 2) = ppors(zz, 1)
NEXT zz
IF pwin(fin(1)) = 3 THEN sed(fin(1)) = 1: sed(fin(2)) = 2: kulta = fin(1): hopea = fin(2) ELSE sed(fin(1)) = 2: sed(fin(2)) = 1: kulta = fin(2): hopea = fin(1)
IF pwin(bin(1)) = 3 THEN sed(bin(1)) = 3: sed(bin(2)) = 4: pronssi = bin(1) ELSE sed(bin(2)) = 3: sed(bin(1)) = 4: pronssi = bin(2)
IF pwin(din(1)) = 3 THEN sed(din(1)) = 12: sr(din(1)) = 1: sed(din(2)) = 13 ELSE sed(din(2)) = 12: sr(din(2)) = 1: sed(din(1)) = 13
FOR xx = 1 TO 24
IF sedd(xx) < 13 THEN tkausl(xx) = tkausl(xx) + 1
IF sedd(xx) > 12 THEN tkausd(xx) = tkausd(xx) + 1
IF emestari = xx THEN temes(xx) = temes(xx) + 1
IF kulta = xx THEN tmes(xx) = tmes(xx) + 1
IF hopea = xx THEN thop(xx) = thop(xx) + 1
IF pronssi = xx THEN tpro(xx) = tpro(xx) + 1
IF sed(xx) = 12 AND sedd(xx) > 12 THEN nousu = xx
IF sed(xx) > 12 AND sedd(xx) < 13 THEN puto = xx
NEXT xx

IF ohj(kulta) <> 0 THEN ykkos(ohj(kulta)) = ykkos(ohj(kulta)) + 1: SCREEN 13: kuva ("winphl"): SCREEN 0: WIDTH 80, 43
IF ohj(hopea) <> 0 THEN kakkos(ohj(hopea)) = kakkos(ohj(hopea)) + 1
IF ohj(pronssi) <> 0 THEN kolmos(ohj(pronssi)) = kolmos(ohj(pronssi)) + 1
IF nousu <> 0 THEN
IF ohj(nousu) <> 0 THEN nousus(ohj(nousu)) = nousus(ohj(nousu)) + 1
END IF
IF puto <> 0 THEN
IF ohj(puto) <> 0 THEN putos(ohj(puto)) = putos(ohj(puto)) + 1
END IF
alkuxxx:
CLS
COLOR 14: PRINT "MASO TV ";
COLOR 13
PRINT "ESITTŽŽ... SUORA LŽHETYS ITŽMEREN SUURIMMALTA JA KAUNEIMMALTA"
PRINT "LOISTORISTEILIJŽLTŽ, ";
COLOR 14
PRINT "PHL:N SUURESTA PALKINTOGAALASTA ";
COLOR 13
PRINT "JOKA ON JUURI ALKAMASSA!"
PRINT "JUONTAJINA TIETENKIN OMA MR. JŽŽKIEKKOMME ";
COLOR 2: PRINT "LENTTI PINDEGREN ";
COLOR 13: PRINT "SEKŽ "
PRINT "KAIKILLE TUTTU SELLISTI-MALLI-VIRTUOOSI AMERIIKASTA...";
COLOR 10: PRINT "LANDA BRAVO";
COLOR 13: PRINT "!!!"
PRINT "ERIKOISKOMMENTAATTORIMME TŽNŽŽN ON KRIITIKKO ";
COLOR 9: PRINT "KUKKA JAJAVA";
COLOR 13: PRINT "."
COLOR 10
PRINT
lentti (1)
landa (1)
PRINT
lentti (2)
landa (2)
PRINT : COLOR 13: PRINT "...": PRINT
lentti (3)
landa (3)
PRINT : COLOR 13: PRINT "PISTEP™RSSIEN VOITTAJAT:": PRINT
FOR zz = 1 TO 4
COLOR 7
SELECT CASE zz
CASE 1
PRINT "RUNKOSARJA, PHL   ";
CASE 2
PRINT "RUNKOSARJA, DIV   ";
CASE 3
PRINT "PLAY-OFFIT, PHL   ";
CASE 4
PRINT "PLAY-OFFIT, DIV   ";
END SELECT
COLOR 12
PRINT pvoittaja(zz).na;
COLOR 4
PRINT " ("; l(pvoittaja(zz).jo); ")  ";
COLOR 14
PRINT pvoittaja(zz).ma; " + "; pvoittaja(zz).sy; " = "; pvoittaja(zz).pi;
IF pvoittaja(zz).pi <= rekord(zz).pi THEN PRINT
IF pvoittaja(zz).pi > rekord(zz).pi THEN
COLOR 15: PRINT "  (E)"
rekord(zz) = pvoittaja(zz)
END IF
NEXT zz
COLOR 13: PRINT : PRINT "...": PRINT
lentti (4)
landa (4)
PRINT
COLOR 31: PRINT CHR$(14); " "; CHR$(14); " "; CHR$(14)
PRINT
COLOR 13: PRINT "KUKAN LEVYRAATI:"
kukka (1)
PRINT
COLOR 13: PRINT "..."
pjn
CLS
IF nousu <> 0 THEN
landa (7)
COLOR 2: PRINT "Niin, ";
COLOR 14: PRINT RTRIM$(l(nousu));
COLOR 2: PRINT " on uusi PHL-joukkue! Manageri ";
COLOR 14: PRINT mana(man(nousu));
COLOR 2: PRINT ", kertokaa!"
PRINT
IF ohj(nousu) = 0 THEN tmankom (4) ELSE xxx = CSRLIN: komme (4)
PRINT
COLOR 13: PRINT "...": PRINT
END IF
lentti (5)
COLOR 10
PRINT "-Ja h„n on, tietenkin... pronssijoukkueen manageri ";
COLOR 14: PRINT mana(man(pronssi))
PRINT
COLOR 30: PRINT "(APLODEJA)"
PRINT
lentti (6)
PRINT
IF ohj(pronssi) = 0 THEN tmankom (1) ELSE xxx = CSRLIN: komme (1)
PRINT : COLOR 13: PRINT "...": PRINT
lentti (7)
COLOR 10
PRINT "-Hopeaa joukkueineen saavuttanut ";
COLOR 14: PRINT mana(man(hopea));
COLOR 10: PRINT ", milt„ teist„ tuntuu?"
PRINT
IF ohj(hopea) = 0 THEN tmankom (2) ELSE xxx = CSRLIN: komme (2)
PRINT
COLOR 13: PRINT "..."
PRINT
COLOR 10
landa (5)
PRINT : COLOR 2
PRINT "-Mestarijoukkueen manageri ";
COLOR 14: PRINT mana(man(kulta));
COLOR 2: PRINT ", milt„ nyt tuntuu?!"
PRINT
IF ohj(kulta) = 0 THEN tmankom (3) ELSE xxx = CSRLIN: komme (3)
PRINT
COLOR 13: PRINT " MESTARI: ";
COLOR 14: PRINT l(kulta)
COLOR 13: PRINT "  HOPEAA: ";
COLOR 7: PRINT l(hopea)
COLOR 13: PRINT "PRONSSIA: ";
COLOR 6: PRINT l(pronssi)
PRINT : COLOR 13: PRINT "...": PRINT
lentti (8)
landa (6)
pjn
DIM osuus(1 TO 11)
OPEN "data\potti.99" FOR INPUT AS #1
FOR z = 1 TO 11
INPUT #1, osuus(z)
NEXT z
CLOSE #1

sarjamaar

CLS
COLOR 15
PRINT "LOPULLINEN KUNNIATAULUKKO ON:"
PRINT
FOR xxx = 1 TO 24
IF xxx = 1 THEN COLOR 14
IF xxx = 2 THEN COLOR 7
IF xxx = 3 THEN COLOR 6
IF xxx > 3 AND xxx < 9 THEN COLOR 10
IF xxx > 8 AND xxx < 12 THEN COLOR 2
IF xxx = 12 THEN COLOR 4
IF xxx > 12 AND xxx < 19 THEN COLOR 13
IF xxx > 18 THEN COLOR 5
PRINT l(x(xxx));
COLOR 14
IF xxx < 12 THEN PRINT " ("; LTRIM$(STR$(FIX(osuus(xxx) * ppotti))); ")";
IF ohj(x(xxx)) <> 0 AND xxx < 12 THEN raha(ohj(x(xxx))) = raha(ohj(x(xxx))) + FIX(osuus(xxx) * ppotti)
IF puto = x(xxx) THEN PRINT CHR$(25);
IF nousu = x(xxx) THEN PRINT CHR$(24);
PRINT
IF xxx = 12 THEN PRINT
NEXT xxx

LOCATE 3
FOR vu = 1 TO plkm
pv = vuoro(vu)
LOCATE , 30
SELECT CASE sed(u(pv))
CASE 1
annarahaa (3)
CASE 2
annarahaa (4)
CASE 3
annarahaa (5)
CASE 4
annarahaa (6)
END SELECT
PRINT
NEXT vu
pjn

ERASE osuus

FOR zz = 1 TO plkm
OPEN "game_" + LTRIM$(STR$(lokero)) + "\stat." + LTRIM$(STR$(zz)) FOR APPEND AS #1
WRITE #1, kausi + 1, u(zz), 44, win(u(zz)), dra(u(zz)), los(u(zz)), gf(u(zz)), ga(u(zz)), p(u(zz)), s(u(zz)), sedd(u(zz)), sed(u(zz))
CLOSE #1
NEXT zz

OPEN "game_" + LTRIM$(STR$(lokero)) + "\1.sta" FOR APPEND AS #1
WRITE #1, kausi + 1, kulta, ohj(kulta), hopea, ohj(hopea), pronssi, ohj(pronssi), l(emestari), ohj(emestari)
CLOSE #1

OPEN "game_" + LTRIM$(STR$(lokero)) + "\3.sta" FOR APPEND AS #1
IF nousu <> 0 THEN
WRITE #1, kausi + 1, nousu, ohj(nousu), puto, ohj(puto)
END IF
IF nousu = 0 THEN WRITE #1, kausi + 1, 0, 0, 0, 0
CLOSE #1

FOR zz = 1 TO 4
OPEN "game_" + LTRIM$(STR$(lokero)) + "\2" + LTRIM$(STR$(zz)) + ".sta" FOR APPEND AS #1
WRITE #1, kausi + 1, pvoittaja(zz).na, pvoittaja(zz).jo, pvoittaja(zz).ma, pvoittaja(zz).sy, pvoittaja(zz).pi
CLOSE #1
NEXT zz

OPEN "game_" + LTRIM$(STR$(lokero)) + "\" + LTRIM$(STR$(kausi)) + ".tbl" FOR OUTPUT AS #1
FOR xxx = 1 TO 12
FOR xx = 1 TO 24
IF sedd(xx) < 13 THEN
IF s(xx) = xxx THEN
WRITE #1, xx, 44, win(xx), dra(xx), los(xx), gf(xx), ga(xx), p(xx), ohj(xx)
END IF
END IF
NEXT xx
NEXT xxx
FOR xxx = 1 TO 12
FOR xx = 1 TO 24
IF sedd(xx) > 12 THEN
IF s(xx) = xxx THEN
WRITE #1, xx, 44, win(xx), dra(xx), los(xx), gf(xx), ga(xx), p(xx), ohj(xx)
END IF
END IF
NEXT xx
NEXT xxx
CLOSE #1

FOR xx = 1 TO plkm
OPEN "game_" + LTRIM$(STR$(lokero)) + "\" + LTRIM$(STR$(kausi)) + ".gr" + LTRIM$(STR$(xx)) FOR OUTPUT AS #1
WRITE #1, sr(u(xx))
FOR xxx = 1 TO 44
WRITE #1, graf(xxx, xx)
NEXT xxx
CLOSE #1
NEXT xx

ot = ot + 1
ERASE qfin, qdin, sfin, sdin, fin, din
ERASE bin, pwin, pvoittaja

aaalkukevat
muutmestarit
alasarja
nhl
varalle

CLS
DIM kiekkoli(1 TO 24) AS INTEGER
FOR ccc = 1 TO 24
xx = x(ccc)
tasomuutokset
NEXT ccc

FOR xx = 1 TO 56
tasomaar
NEXT xx

FOR vu = 1 TO plkm
pv = vuoro(vu)
sopim(pv) = sopim(pv) - 1
IF sopim(pv) = 0 THEN oaamasopimus
NEXT vu


pelaajasiirrot
managerisiirrot

END SUB

SUB ennenkaudenalkua

kausikorttimaar
kr = kr + 1

END SUB

SUB erikoismuut
IF krapu(pv) > 0 THEN krapu(pv) = 0

FOR xx = 1 TO lpl(pv)
IF pel(xx, pv).plus <> 0 THEN
IF pel(xx, pv).kest > 0 THEN pel(xx, pv).kest = pel(xx, pv).kest - 1
IF pel(xx, pv).kest = 0 THEN pel(xx, pv).plus = 0
END IF
NEXT xx

END SUB

SUB erikoisp (fuk%)
IF pirtar(3) <> 101 THEN SYSTEM

IF eri(fuk%) = 0 THEN
OPEN "data\" + LTRIM$(STR$(fuk%)) + "p.mhz" FOR INPUT AS #1
INPUT #1, montako
DIM sel(1 TO montako) AS pelaaja
FOR z = 1 TO montako
INPUT #1, sel(z).nam, sel(z).ppp, sel(z).nat, sel(z).age, sel(z).psk, sel(z).svu, sel(z).sra, sel(z).ego, sel(z).lah, sel(z).spe
NEXT z
LINE INPUT #1, kylla$
LINE INPUT #1, eipa$
CLOSE #1
IF lpl(pv) < 33 - montako THEN
SELECT CASE sel(1).ppp
CASE 1
ccc = 2
CASE ELSE
ccc = 1
END SELECT
temp% = valb(ccc, pv) + valb(4, pv) + (valb(5, pv) * 2)
IF temp% >= (sel(1).psk - 2) THEN
IF fuk% = 28 THEN eri(28) = 1
IF fuk% = 29 THEN eri(29) = 1
luetexti (LTRIM$(STR$(fuk%)) + "p")
sv$ = ""
DO UNTIL sv$ = "k" OR sv$ = "e"
sv$ = INKEY$
LOOP
PRINT
SELECT CASE sv$
CASE "k"
eri(fuk%) = 1
PRINT kylla$
FOR z = 1 TO montako
pel(lpl(pv) + z, pv) = sel(z)
NEXT z
lpl(pv) = lpl(pv) + montako
IF fuk% = 30 THEN pel(lpl(pv), pv).svu = pel(lpl(pv), pv).svu + CINT(6 * RND) + 1
CASE "e"
PRINT eipa$
END SELECT
END IF
END IF
ERASE sel
END IF
END SUB

SUB euromaar
ekr = 1
FOR xx = 1 TO 20
ex(xx) = 0
ep(xx) = 0
ero(xx) = 0
NEXT xx

FOR zz = 1 TO plkm
IF kulta = u(zz) OR hopea = u(zz) OR pronssi = u(zz) THEN muke(zz) = 1
NEXT zz
xx = INT(19 * RND) + 1
ex(xx) = kulta
maare1:
xx = INT(19 * RND) + 1
IF ex(xx) > 0 THEN GOTO maare1
ex(xx) = hopea
maare2:
xx = INT(19 * RND) + 1
IF ex(xx) > 0 THEN GOTO maare2
ex(xx) = pronssi
FOR xxx = 25 TO 41
maare3:
xx = CINT(19 * RND) + 1
IF ex(xx) > 0 THEN GOTO maare3
ex(xx) = xxx
NEXT xxx
END SUB

SUB fbimiehet
CLS
manap
PRINT "!"
PRINT
luetexti ("fbi")
PRINT
printpels
COLOR 13: PRINT "istuu mustapukuisten miesten keskell„ raudoissa!"
PRINT
lentti (66)
xxx = CSRLIN: komme (66)
PRINT
lentti (67)
xxx = CSRLIN: komme (67)
PRINT
lentti (68)
PRINT
COLOR 13: PRINT "Miehet poistuvat, pelaajasi mukanaan."
poispel

pjn

END SUB

SUB gameday
IF pirtar(2) <> 432 THEN SYSTEM
CLS
COLOR 13: LOCATE 1, 1: PRINT "PHL:N OTTELUT OVAT SEURAAVAT:"
COLOR 13: LOCATE 10, 1: PRINT "DIVISIOONAN OTTELUT OVAT SEURAAVAT:"
naytaott
pjn

CLS
COLOR 13: PRINT "PHL:N TULOKSET OVAT SEURAAVAT:": PRINT : kolo = 12
nn = x(a): mm = x(b): ylmaar: ottpel: ottul: otmuut
nn = x(c): mm = x(d): ylmaar: ottpel: ottul: otmuut
nn = x(E): mm = x(f): ylmaar: ottpel: ottul: otmuut
nn = x(g): mm = x(h): ylmaar: ottpel: ottul: otmuut
nn = x(i): mm = x(j): ylmaar: ottpel: ottul: otmuut
nn = x(k): mm = x(m): ylmaar: ottpel: ottul: otmuut
LOCATE 10, 1: COLOR 13: PRINT "DIVISIOONAN TULOKSET OVAT SEURAAVAT:": PRINT : kolo = 4
nn = x(a + 12): mm = x(b + 12): ylmaar: ottpel: ottul: otmuut
nn = x(c + 12): mm = x(d + 12): ylmaar: ottpel: ottul: otmuut
nn = x(E + 12): mm = x(f + 12): ylmaar: ottpel: ottul: otmuut
nn = x(g + 12): mm = x(h + 12): ylmaar: ottpel: ottul: otmuut
nn = x(i + 12): mm = x(j + 12): ylmaar: ottpel: ottul: otmuut
nn = x(k + 12): mm = x(m + 12): ylmaar: ottpel: ottul: otmuut
veiktulmaar
pjn

FOR vu = 1 TO plkm
pv = vuoro(vu)
IF mveik(pv) = 1 THEN veiktark
IF mveik(pv) = 1 THEN mveik(pv) = 0
NEXT vu

ot = ot + 1
kr = kr + 1
FOR xx = 1 TO plkm
IF sr(u(xx)) = 1 THEN ottephl(xx) = ottephl(xx) + 1
IF sr(u(xx)) = 2 THEN ottediv(xx) = ottediv(xx) + 1
NEXT xx

tremuut
tautimuut

FOR vu = 1 TO plkm
pv = vuoro(vu)
CLS
erikoismuut
report
sattuma
kuntche
NEXT vu

uutisia

ki = ki + 1: IF ki = 23 THEN ki = 1
ki2 = ki2 + 1: IF ki2 = 12 THEN ki2 = 1
END SUB

SUB gamedayehl
etu1 = 1: etu2 = .85
CLS
COLOR 13: LOCATE 1, 1: PRINT "EHL:N OTTELUT OVAT SEURAAVAT:"
naytaottehl
pjn

CLS
COLOR 13: PRINT "EHL:N TULOKSET OVAT SEURAAVAT:": PRINT : kolo = 12
nn1 = a: nn = ex(a): mm1 = b: mm = ex(b): ylmaarehl: ottpel: ottul: otmuutehl
nn1 = c: nn = ex(c): mm1 = d: mm = ex(d): ylmaarehl: ottpel: ottul: otmuutehl
PRINT
nn1 = a + 4: nn = ex(a + 4): mm1 = b + 4: mm = ex(b + 4): ylmaarehl: ottpel: ottul: otmuutehl
nn1 = c + 4: nn = ex(c + 4): mm1 = d + 4: mm = ex(d + 4): ylmaarehl: ottpel: ottul: otmuutehl
PRINT
nn1 = a + 8: nn = ex(a + 8): mm1 = b + 8: mm = ex(b + 8): ylmaarehl: ottpel: ottul: otmuutehl
nn1 = c + 8: nn = ex(c + 8): mm1 = d + 8: mm = ex(d + 8): ylmaarehl: ottpel: ottul: otmuutehl
PRINT
nn1 = a + 12: nn = ex(a + 12): mm1 = b + 12: mm = ex(b + 12): ylmaarehl: ottpel: ottul: otmuutehl
nn1 = c + 12: nn = ex(c + 12): mm1 = d + 12: mm = ex(d + 12): ylmaarehl: ottpel: ottul: otmuutehl
PRINT
nn1 = a + 16: nn = ex(a + 16): mm1 = b + 16: mm = ex(b + 16): ylmaarehl: ottpel: ottul: otmuutehl
nn1 = c + 16: nn = ex(c + 16): mm1 = d + 16: mm = ex(d + 16): ylmaarehl: ottpel: ottul: otmuutehl

pjn
eot = eot + 1
ekr = ekr + 1
kr = kr + 1
FOR xx = 1 TO plkm
IF muke(xx) = 1 THEN otteehl(xx) = otteehl(xx) + 1
NEXT xx


tautimuut

FOR vu = 1 TO plkm
pv = vuoro(vu)
erikoismuut
IF muke(pv) = 1 THEN
CLS
report
sattuma
kuntche
END IF
NEXT vu

IF eot = 6 THEN ehllopturmaar
END SUB

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

