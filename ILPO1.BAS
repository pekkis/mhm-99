DECLARE SUB budget ()
DECLARE SUB borsgene ()
DECLARE SUB avaaturnausdata ()
DECLARE SUB baaorsmuutos ()
DECLARE SUB fbimiehet ()
DECLARE SUB cheatmenu ()
DECLARE SUB bors ()
DECLARE SUB areena ()
DECLARE SUB checkaus ()
DECLARE SUB aaaargh ()
DECLARE SUB egomaar (fat%)
DECLARE SUB aaalkukevat ()
DECLARE SUB alasarja ()
DECLARE SUB annarahaa (slok%)
DECLARE SUB borschpiirto ()
DECLARE SUB colchk2 ()
DECLARE SUB ehllopturmaar ()
DECLARE SUB erikoismuut ()
DECLARE SUB vuoromaar ()
DECLARE SUB jaauniorit ()
DECLARE SUB kaudenalkuinho ()
DECLARE SUB kiekkoliite ()
DECLARE SUB lmmaar ()
DECLARE SUB gamedayhaara ()
DECLARE SUB ketjuchk ()
DECLARE SUB ketjuswap ()
DECLARE SUB ketjuissachk ()
DECLARE SUB katsoket ()
DECLARE SUB katsopel ()
DECLARE SUB graph ()
DECLARE SUB kutsuturnaus ()
DECLARE SUB kirjeluku ()
DECLARE SUB kriisipalaveri ()
DECLARE SUB inteprint ()
DECLARE SUB kuitt ()
DECLARE SUB kuva (tied$)
DECLARE SUB mahmax ()
DECLARE SUB klear ()
DECLARE SUB manap ()
DECLARE SUB haahaa (haha%)
DECLARE SUB krapulapiirto ()
DECLARE SUB keisi ()
DECLARE SUB keisi2 (fat%)
DECLARE SUB luetexti (lue$)
DECLARE SUB maarpalk (jepulis$)
DECLARE SUB lentti (sano!)
DECLARE SUB landa (sano!)
DECLARE SUB kukka (sano!)
DECLARE SUB komme (sano!)
DECLARE SUB managerisiirrot ()
DECLARE SUB kausikorttimaar ()
DECLARE SUB ylmaar ()
DECLARE SUB kuntche ()
DECLARE SUB ylmaarehl ()
DECLARE SUB ottelmaarhaarax ()
DECLARE SUB moramaar ()
DECLARE SUB miettiipois ()
DECLARE SUB pelhychk ()
DECLARE SUB piirtox ()
DECLARE SUB opzions ()
DECLARE SUB pankki ()
DECLARE SUB organisaatio ()
DECLARE SUB meanstuff ()
DECLARE SUB palkmaar (fat%)
DECLARE SUB osto2 ()
DECLARE SUB muutmestarit ()
DECLARE SUB nhl ()
DECLARE SUB oaamasopimus ()
DECLARE SUB pelaajasiirrot ()
DECLARE SUB naytaott ()
DECLARE SUB ottpel ()
DECLARE SUB ottul ()
DECLARE SUB otmuut ()
DECLARE SUB naytaottehl ()
DECLARE SUB otmuutehl ()
DECLARE SUB sponsorit ()
DECLARE SUB pisteporssimaar ()
DECLARE SUB rakprosessi ()
DECLARE SUB specmaar ()
DECLARE SUB poisne ()
DECLARE SUB sortmaar ()
DECLARE SUB sortplayers ()
DECLARE SUB qelp (qel%)
DECLARE SUB scoutit ()
DECLARE SUB sopuprint ()
DECLARE SUB pisteporssi (orja!)
DECLARE SUB skreensaver ()
DECLARE SUB pjn ()
DECLARE SUB printpels ()
DECLARE SUB poispel ()
DECLARE SUB sortbors (paikka%)
DECLARE SUB printpel (fat%)
DECLARE SUB sopimusext (fat%)
DECLARE SUB skillmaarbors ()
DECLARE SUB sarjamaar ()
DECLARE SUB report ()
DECLARE SUB sattuma ()
DECLARE SUB uusikausi ()
DECLARE SUB tarkistanhlc ()
DECLARE SUB valitsestrattie ()
DECLARE SUB tremaar ()
DECLARE SUB staulmaar ()
DECLARE SUB staulehlmaar ()
DECLARE SUB vastmaar ()
DECLARE SUB taaah ()
DECLARE SUB takaisinhl ()
DECLARE SUB valitsestrat ()
DECLARE SUB tyytyma ()
DECLARE SUB tallennus ()
DECLARE SUB staulnaytehl ()
DECLARE SUB staulnayt ()
DECLARE SUB statistiikka ()
DECLARE SUB vedonlyonti ()
DECLARE SUB tulonsiirto ()
DECLARE SUB tulevatottelut ()
DECLARE SUB tarjousmaar (koo1%, koo2%)
DECLARE SUB turnausaloita ()
DECLARE SUB turnaus ()
DECLARE SUB turnauslopeta ()
DECLARE SUB tmankom (sano!)
DECLARE SUB varalle ()
DECLARE SUB tasomuutokset ()
DECLARE SUB tasomaar ()
DECLARE SUB veiktulmaar ()
DECLARE SUB veiktark ()
DECLARE SUB tremuut ()
DECLARE SUB tautimuut ()
DECLARE SUB uutisia ()


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

