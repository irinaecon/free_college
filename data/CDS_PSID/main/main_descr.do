clear all
set more off

* SET PATH
cd "C:\Users\S6150145\Dropbox\irina_jmp\ReplicationFiles\data\schools_cds\main"

*use datasets/cds_final, clear
use datasets/cds_final, clear

by pid, sort: gen nvals_id = _n == 1
count if nvals_id // 2419 

cap drop nvals_pid
by hh_faminc, sort: gen nvals_pid = _n == 1
count if nvals_pid // 2419 


*gen chrcost =  chrcost0+chrcost1
*replace chrcost = . if chrcost>25000
*replace chrcost2 = . if chrcost2>5000
*replace schcost = . if schcost>25000

* replace all missing sub elements with zeros
foreach var of varlist chrcost0 chrcost1 chrcost2 schcurr schprev xcost1 xcost2 {

	replace `var'=0 if `var'==.

}


* total childcare cost
gen childcare = chrcost0 +  chrcost1 + chrcost2

* total schooling cost
gen schooling = schcurr + schprev

* total extracur cost
gen extracur = xcost1 + xcost2


* drop outliers
drop if childcare>25000
drop if extracur>25000
drop if schooling>25000
*drop if childcare==0

* total cost
gen moninv = childcare +schooling + extracur

su moninv [aweight=cds_weight]


su moninv [aweight=cds_weight] if age_cds<6
su moninv [aweight=cds_weight] if age_cds>=6 & age_cds<12
su moninv [aweight=cds_weight] if age_cds>=12 & age_cds<18

su moninv if age_cds<6
su moninv if age_cds>=6 & age_cds<12
su moninv if age_cds>=12 & age_cds<18


gen head_married = 0
replace head_married = 1 if head_marriage==1

bysort head_married: su moninv [aweight=cds_weight]

gen headlevel=.
replace headlevel=1 if educhead<12
replace headlevel=2 if educhead>=12 & educhead<16
replace headlevel=3 if educhead>=16
gen wifelevel=.
replace wifelevel=1 if educwife<12
replace wifelevel=2 if educwife>=12 & educwife<16
replace wifelevel=3 if educwife>=16

bysort head_married headlevel age_cds: su moninv [aweight=cds_weight]

su moninv [aweight=cds_weight] if age_cds<=2
su moninv [aweight=cds_weight] if age_cds>2 & age_cds<=4
su moninv [aweight=cds_weight] if age_cds>4 & age_cds<=6
su moninv [aweight=cds_weight] if age_cds>6 & age_cds<=8
su moninv [aweight=cds_weight] if age_cds>8 & age_cds<=10
su moninv [aweight=cds_weight] if age_cds>10 & age_cds<=12
su moninv [aweight=cds_weight] if age_cds>12 & age_cds<=14
su moninv [aweight=cds_weight] if age_cds>14 & age_cds<=16

su moninv [aweight=cds_weight] if age_cds<=6
su moninv [aweight=cds_weight] if age_cds>6 & age_cds<=12
su moninv [aweight=cds_weight] if age_cds>12 & age_cds<=18

su moninv [aweight=cds_weight] if age_cds>6 & age_cds<=16
*******************************
*** TIME **********************
*******************************
* replace all missing sub elements with zeros
foreach var of varlist headtime headtime2 wifetime wifetime2 {

	replace `var'=0 if `var'==.

}
* ACTIVE time
gen tinv =0
replace  tinv = headtime if head_married==0
replace tinv = headtime + wifetime if head_married==1

su tinv [aweight=cds_weight]
bysort head_married: su tinv [aweight=cds_weight]

bysort headlevel: su tinv [aweight=cds_weight]

bysort head_married headlevel: su tinv [aweight=cds_weight]

su tinv [aweight=cds_weight] if age_cds<=2
su tinv [aweight=cds_weight] if age_cds>2 & age_cds<=4
su tinv [aweight=cds_weight] if age_cds>4 & age_cds<=6
su tinv [aweight=cds_weight] if age_cds>6 & age_cds<=8
su tinv [aweight=cds_weight] if age_cds>8 & age_cds<=10
su tinv [aweight=cds_weight] if age_cds>10 & age_cds<=12
su tinv [aweight=cds_weight] if age_cds>12 & age_cds<=14
su tinv [aweight=cds_weight] if age_cds>14 & age_cds<=16

su tinv [aweight=cds_weight] if age_cds<=6
su tinv [aweight=cds_weight] if age_cds>6 & age_cds<=12
su tinv [aweight=cds_weight] if age_cds>12 & age_cds<=18

su tinv [aweight=cds_weight] if age_cds>6 & age_cds<=16

scatter tinv moninv

gen logt = log(tinv)
gen logm = log(moninv)
reg logt logm if (tinv>0 & moninv>0)

asgen mean_t = tinv, weight(cds_weight)
asgen mean_m = moninv, weight(cds_weight)
gen tinv_n = tinv/mean_t
gen moninv_n = moninv/mean_m

graph twoway (lfitci moninv_n tinv_n) (scatter moninv_n tinv_n) if (age_cds>4) // (tinv>0 & moninv>0)

correlate logt logm [aweight=cds_weight]
correlate tinv moninv [aweight=cds_weight]

bysort head_married headlevel: su rawscore [aweight=cds_weight]
bysort head_married headlevel: su adjscore [aweight=cds_weight]

su adjscore if age_cds>=14 & age_cds<16 [aweight=cds_weight]
su adjscore if age_cds>=16 & age_cds<18 [aweight=cds_weight]

gen logtinv = log(tinv + sqrt(tinv^(2) + 1))

asgen avscore = adjscore, weight(cds_weight)
* express everything as a proportion of this
gen scoreprop = .
replace scoreprop = adjscore / avscore

bysort head_married headlevel age_cds: asgen avscore_msj = adjscore, weight(cds_weight)
gen scoregap=.
forvalue jc=0(1)16 {
	su avscore_msj if head_married==0 & headlevel==1 & age_cds==`jc'
	scalar score_ref_j`jc' = r(mean)
	replace scoregap = adjscore / score_ref_j`jc' if age_cds==`jc'
}

*gen scoregap = adjscore / score_ref * 100

reg logtinv age_cds
levpredict tinv_pred
su tinv_pred [aweight=cds_weight]
su tinv [aweight=cds_weight]

bysort age_cds: su tinv_pred [aweight=cds_weight]
bysort age_cds: su tinv [aweight=cds_weight]

matrix P = J(6,19,.)
matrix PTT = J(2,19,.)
matrix PC = J(2,3,.)

matrix Pt = J(6,19,.)
matrix PCt = J(2,3,.)

matrix PS = J(2,3,.)

matrix PSC= J(6,19,.)

scalar ic = 1
forvalue mc=0(1)1 {

	forvalue ec=1(1)3 {
	
		quietly su moninv [aweight=cds_weight] if (head_married==`mc' & headlevel==`ec')
		matrix PC[`mc'+1,`ec'] = r(mean)
		
		quietly su tinv [aweight=cds_weight] if (head_married==`mc' & headlevel==`ec')
		matrix PCt[`mc'+1,`ec'] = r(mean)
		
		quietly su scoreprop [aweight=cds_weight] if (head_married==`mc' & headlevel==`ec')
		matrix PS[`mc'+1,`ec'] = r(mean)

		forvalue jc=1(1)18 {
			quietly su moninv [aweight=cds_weight] if (head_married==`mc' & headlevel==`ec' & age_cds==`jc-1') //, d 
			
			matrix P[ic,`jc'] = r(mean)
			
			
			quietly su tinv [aweight=cds_weight] if (head_married==`mc' & headlevel==`ec' & age_cds==`jc-1') //, d 
			
			matrix Pt[ic,`jc'] = r(mean)
			
			su scoregap [aweight=cds_weight] if (head_married==`mc' & headlevel==`ec' & age_cds==`jc-1')
			matrix PSC[ic,`jc'] = r(mean)
			
			quietly su tinv [aweight=cds_weight] if  (age_cds==`jc-1') //, d 
			
			matrix PTT[1,`jc'] = r(mean)
			
			quietly su moninv [aweight=cds_weight] if  (age_cds==`jc-1') //, d 
			
			matrix PTT[2,`jc'] = r(mean)
			
        }

		
		scalar ic=ic+1

	}
}

matrix list P, format(%5.3f)
putexcel set output/moninv_age.xlsx, modify
putexcel B1=0 C1=1 D1=2 E1=3 F1=4 G1=5 H1=6 I1=7 J1=8 K1=9 L1=10 M1=11 N1=12 O1=13 P1=14 Q1=15 R1=16 A2="si, lo" A3="si, me" A4="si, hi" A5="ma, lo" A6="ma, me" A7="ma, hi"  B2 = matrix(P) 

matrix list PC, format(%5.3f)
putexcel set output/moninv.xlsx, modify
putexcel B1="lo" C1="me" D1="hi" A2="si" A3="ma" B2 = matrix(PC) 

matrix list Pt, format(%5.3f)
putexcel set output/tinv_age.xlsx, modify
putexcel B1=0 C1=1 D1=2 E1=3 F1=4 G1=5 H1=6 I1=7 J1=8 K1=9 L1=10 M1=11 N1=12 O1=13 P1=14 Q1=15 R1=16 A2="si, lo" A3="si, me" A4="si, hi" A5="ma, lo" A6="ma, me" A7="ma, hi"  B2 = matrix(Pt) 

matrix list PTT, format(%5.3f)
putexcel set output/inv_age.xlsx, modify
putexcel B1=0 C1=1 D1=2 E1=3 F1=4 G1=5 H1=6 I1=7 J1=8 K1=9 L1=10 M1=11 N1=12 O1=13 P1=14 Q1=15 R1=16 A2="time" A3="money"  B2 = matrix(PTT) 


matrix list PSC, format(%5.3f)
putexcel set output/gap_age.xlsx, modify
putexcel B1=0 C1=1 D1=2 E1=3 F1=4 G1=5 H1=6 I1=7 J1=8 K1=9 L1=10 M1=11 N1=12 O1=13 P1=14 Q1=15 R1=16 A2="si, lo" A3="si, me" A4="si, hi" A5="ma, lo" A6="ma, me" A7="ma, hi"  B2 = matrix(PSC) 


matrix list PCt, format(%5.3f)
putexcel set output/tinv.xlsx, modify
putexcel B1="lo" C1="me" D1="hi" A2="si" A3="ma" B2 = matrix(PCt) 

matrix list PS, format(%5.3f)
putexcel set output/lwscore.xlsx, modify
putexcel B1="lo" C1="me" D1="hi" A2="si" A3="ma" B2 = matrix(PS) 
