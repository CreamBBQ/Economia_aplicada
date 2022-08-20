********** OUTPUT FILE: CARD & KRUEGER (1994)***********

/* Article:
Minimum Wages and Employment: A Case Study of the Fast-Food Industry in New Jersey and Pennsylvania by David Card and Alan B Krueger
The American Economic Review, Vol 84, No 4(Sep, 1994), pp 772-793
*/

/*VARIABLE DESCRIPTION
SHEET sheet number (unique store id)
CHAIN chain 1=Burger King; 2=KFC; 3=Roy Rogers; 4=Wendys
CO_OWNED 1 if company owned
STATE 1 if NJ; 0 if Pa

Dummies for location:

SOUTHJ 1 if in southern NJ
CENTRALJ 1 if in central NJ
NORTHJ 1 if in northern NJ
PA1 1 if in PA, northeast suburbs of Philadelphia
PA2 1 if in PA, Easton etc
SHORE 1 if on NJ shore

First Interview:
NCALLS number of call-backs*
EMPFT # full-time employees
EMPPT # part-time employees
NMGRS # managers/ass't managers
WAGE_ST starting wage ($/hr)
INCTIME months to usual first raise
FIRSTINC usual amount of first raise ($/hr)
BONUS 1 if cash bounty for new workers
PCTAFF % employees affected by new minimum
MEALS free/reduced price code (See below)
OPEN hour of opening
HRSOPEN number hrs open per day
PSODA price of medium soda, including tax
PFRY price of small fries, including tax
PENTREE price of entree, including tax
NREGS number of cash registers in store
NREGS11 number of registers open at 11:00 am

Second Interview:

TYPE2 type 2nd interview 1=phone; 2=personal
STATUS2 status of second interview: see below
DATE2 date of second interview MMDDYY format
NCALLS2 number of call-backs*
EMPFT2 # full-time employees
EMPPT2 # part-time employees
NMGRS2 # managers/ass't managers
WAGE_ST2 starting wage ($/hr)
INCTIME2 months to usual first raise
FIRSTIN2 usual amount of first raise ($/hr)
SPECIAL2 1 if special program for new workers
MEALS2 free/reduced price code (See below)
OPEN2R hour of opening
HRSOPEN2 number hrs open per day
PSODA2 price of medium soda, including tax
PFRY2 price of small fries, including tax
PENTREE2 price of entree, including tax
NREGS2 number of cash registers in store
NREGS112 number of registers open at 11:00 am

Codes:

Free/reduced Meal Variable:
0 = none
1 = free meals
2 = reduced price meals
3 = both free and reduced price meals

Second Interview Status:

0 = refused second interview (count = 1)
1 = answered 2nd interview (count = 399)
2 = closed for renovations (count = 2)
3 = closed "permanently" (count = 6)
4 = closed for highway construction (count = 1)
5 = closed due to Mall fire (count = 1)

Note: number of call-backs = 0 if contacted on first call
*/

********** SET THE VALUES AND OPEN THE FILE *****************

clear all
set mem 5m

cd "C:\Users\hruffo\Dropbox\DeUTDT\TEA\TPS\stata\card-krueger"
use "2.Card&Krueger(1994).dta",clear

gen fte=empft+0.5*emppt

gen fte2=empft2+0.5*emppt2

label var fte "Full-time-equivalent pre"

label var fte2 "Full-time-equivalent post"

label var wage_st "Starting Wage pre"

label var wage_st2 "Starting Wage post"

**balanced panel

*drop if sample==0

*Table 2
*some results

ttest fte, by(state) unequal

ttest fte2, by (state) unequal

ttest wage_st, by(state) unequal

ttest wage_st2, by(state) unequal

ttest hrsopen, by(state) unequal

ttest hrsopen2, by (state) unequal

ttest psoda, by(state) unequal

ttest psoda2, by(state) unequal 

gen wage_st_Pa=wage_st if state==0

label var wage_st_Pa "Starting wage"

gen wage_st_Nj=wage_st if state==1

label var wage_st_Nj "Starting wage"

gen wage_st2_Pa=wage_st2 if state==0

label var wage_st2_Pa "Starting wage"

gen wage_st2_Nj=wage_st2 if state==1

label var wage_st2_Nj "Starting wage"

histogram wage_st_Pa, bin(50) percent fcolor(black) legend(label (1 "Pennsylvania")) addplot(histogram wage_st_Nj, bin(50) percent legend(label (2 "New Jersey")))

histogram wage_st2_Pa, bin(50) percent fcolor(black) legend(label (1 "Pennsylvania")) addplot(histogram wage_st2_Nj, bin(50) percent legend(label (2 "New Jersey")))

*generate difference in differences for employment

egen fteNJ=mean(fte) if state==1

egen fteNJ2=mean(fte2) if state==1

egen ftePA=mean(fte) if state==0

egen ftePA2=mean(fte2) if state==0

gen dfteNJ=fteNJ2-fteNJ

gen dftePA=ftePA2-ftePA

gen dfte = dfteNJ 
replace dfte = dftePA if dfte==.

*generate difference in differences for starting wages

egen wage_stNJ=mean(wage_st) if state==1

egen wage_stNJ2=mean(wage_st2) if state==1

egen wage_stPA=mean(wage_st) if state==0

egen wage_stPA2=mean(wage_st2) if state==0

gen dwage_stNJ=wage_stNJ2-wage_stNJ

gen dwage_stPA=wage_stPA2-wage_stPA

*regression analog of difference in difference

generate dwm=0.8 if state==1

replace dwm=0 if state==0

reg dfte state

reg dfte dwm


