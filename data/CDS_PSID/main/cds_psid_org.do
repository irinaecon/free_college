*------------------------------------------------------------------------
* first, all the individual data in psid
*------------------------------------------------------------------------
cd hhheadrel
do J304216
do J304216_formats
rename *, lower

g pid = er30001*1000 + er30002
drop er*1 er3000*

forval i = 4/9{
	local j = 1990+`i'*2-1
	rename er33`i'02 seq_no`j'
	rename er33`i'03 rel_head`j'
	rename er33`i'04 age_psid`j'
}

reshape long seq_no rel_head age_psid, i(pid) j(year)
g byte keep = rel_head<30|rel_head==88|rel_head==90
replace keep = 1 if missing(rel_head)
drop if !keep
drop keep

replace age_psid = . if age_psid==0|age_psid==999

cd ../
compress
save datasets/psid_relhead, replace

*-----------------------
* create cds-psid matching file for caregivers
*-----------------------
insheet using hhheadrel/cds_ta_map.csv, clear
g pid = er30001*1000 + er30002
keep pid pcg* ocg*

foreach t in "97" "02" "07" {
	rename *`t'_rel *rel_`t'
}
rename *_02 *_2
rename *_07 *_7

reshape long pcgid_ pcgpn_ pcgrel_ ocgid_ ocgpn_ ocgrel_, i(pid) j(year)
rename *_ *
replace year = 1997 if year==97
replace year = 2002 if year==02
replace year = 2007 if year==07

g pcg_pid = pcgid*1000 + pcgpn
g ocg_pid = ocgid*1000 + ocgpn

drop *gid *gpn

sort pid year
compress
save datasets/cds_cg_pid, replace
*------------------------------------------------------------------------

foreach i in pcg ocg{
	sort `i'_pid year
	save datasets/cds_match, replace
	use datasets/psid_relhead, clear

	keep if year==1997|year==2001|year==2003|year==2007

	* for 2002, have both age and age0
	sort pid year
	g age_psid0 = age_psid[_n-1] if pid==pid[_n-1]&year==2003&year[_n-1]==2001
	drop if year==2001
	replace year=2002 if year==2003

	keep year pid seq_no rel_head age_psid age_psid0
	rename pid `i'_pid
	rename seq_no `i'_seq_no
	rename rel_head `i'_head
	rename age_psid `i'_age
	rename age_psid0 `i'_age0

	merge 1:m `i'_pid year using datasets/cds_match
	drop if _m==1
	drop _m
}



save datasets/cds_match, replace

********************************************************************
cd cds97
clear all

quietly do J294225
*quietly do J294225_formats
rename *, lower
g pid = er30001*1000+er30002
save temp, replace

quietly do J301947
*quietly do J301947_formats
rename *, lower

g pid = er30001*1000+er30002


merge 1:1 pid using temp
drop _merge

rename ch97prwt cds_weight
rename ageatpcg age_cds

g year=1997

drop q12* q13*
drop if age_cds>900 | age_cds==.

gen ageyear = age_cds/12

scalar nweeks = 52/12

*******************
* Schooling costs *
*******************

rename q1g11c school0
rename q1g11d type0
 
rename q1g10 schoolpub
rename q1g10a schooleverpub
rename q1g11 schoolpubtype

drop schoolpub schooleverpub schoolpubtype

replace school0 = . if (school0==0 | school0>90000)
replace school0 = . if (type0==0| type0>6)

* normalize fee rate type to years
preserve
	keep if type0>0&type0<7
	egen shre = total(cds_weight) if !missing(school0), by(type0)
	egen avrg = total(school0*cds_weight) if !missing(school0), by(type0)

	collapse shre avrg, by(type0)
	replace avrg = avrg/shre

	scalar y3 = avrg[_N]/avrg[1]
	scalar y4 = avrg[_N]/avrg[2]
	scalar y5 = avrg[_N]/avrg[3]
restore
replace school0 = school0* y3 if type==3
replace school0 = school0* y4 if type==4
replace school0 = school0* y5 if type==5
drop type*

*-----------------------------------------------------------------------------
* These kids are mostly young (0-12)
* THIS IS FOR ARRNGMNT 1-9
*-----------------------------------------------------------------------------
*g byte none = (q1h13==1 | q1h14==11 | q1h15==11 | q1h16==11 | q1h17==11)

gen byte allmissing = 1
quietly forval j = 1/4 {
	rename q1h7_`j' ccost`j'
	rename q1h7a_`j' ctype`j'
	rename q1h6_`j' hrswk`j'
	rename q1h5_`j' dyswk`j'

* replace missing values
	replace ctype`j' = . if ctype`j'==0|ctype`j'>6
	replace ccost`j' = . if missing(ctype`j')
*|ccost`j'==0

	replace ccost`j' = . if ccost`j'>90000 & `j'==1
	replace ccost`j' = . if ccost`j'>9900 & (`j'>=2|`j'<=4)
	replace ccost`j' = . if ccost`j'>990 & (`j'>=5)

	replace hrswk`j' = . if hrswk`j'==0
	replace hrswk`j' = . if hrswk`j'>900 & `j'>=1 & `j'<=4
	replace hrswk`j' = . if hrswk`j'>90 & `j'>=5

	replace dyswk`j' = . if dyswk`j'==0|dyswk`j'>7
}

* some reportings are nonsense, trim them
preserve
	keep ctype* ccost*
	g byte cmissing = missing(ccost1)
	quietly forval j = 1/4{
		replace cmissing = cmissing&missing(ccost`j')
	}
	drop if cmissing
	drop cmissing

	quietly forval j = 1/4{
		egen mmax`j' = max(ccost`j') if !missing(ccost`j'), by(ctype`j')
	}
	keep mmax* *type*
	g id = _n
	reshape long mmax ctype, i(id) j(type)
	drop id
	duplicates drop
	drop if missing(ctype)|missing(mmax)

	collapse (max) mmax, by(ctype)
	scalar dcap = mmax[3]/2
	scalar hcap = dcap/2
restore

* replace values
quietly forval j = 1/4{
*	replace ccost`j' = . if ctype`j'==1 & ccost`j'>=hcap
*	replace ccost`j' = . if ctype`j'==2 & ccost`j'>=dcap

* hourly, daily, biweekly, monthly, annually
	replace ccost`j' = ccost`j' *hrswk`j' if (ctype`j'==1)
	replace ccost`j' = ccost`j' *dyswk`j' if (ctype`j'==2)
	replace ctype`j' = 3 if ctype`j'==1|ctype`j'==2
}

* normalize to annual costs
preserve
	keep cds_weight ctype* ccost*
	g byte cmissing = missing(ccost1)
	quietly forval j = 1/4{
		replace cmissing = cmissing&missing(ccost`j')
	}
	drop if cmissing
	drop cmissing

	quietly forval j = 1/4{
		egen shre`j' = total(cds_weight) if !missing(ccost`j'), by(ctype`j')
		egen avrg`j' = total(ccost`j'*cds_weight) if !missing(ccost`j'), by(ctype`j')
	}
	drop cds_weight ccost*
	g id = _n
	reshape long shre avrg ctype, i(id) j(type)
	drop id
	duplicates drop
	drop if missing(ctype)|missing(shre)

	collapse (sum) shre avrg, by(ctype)
	replace avrg = avrg/shre

	scalar y3 = avrg[_N]/avrg[1]
	scalar y4 = avrg[_N]/avrg[2]
	scalar y5 = avrg[_N]/avrg[3]
restore

quietly forval j=1/4{
* change to annual cost
	replace ccost`j' = ccost`j'* y3 if ctype`j'==3
	replace ccost`j' = ccost`j'* y4 if ctype`j'==4
	replace ccost`j' = ccost`j'* y5 if ctype`j'==5

* duration: months
	rename q1h3_`j'y starty`j'
	rename q1h3_`j'm startm`j'
	rename q1h8_`j'y stopy`j'
	rename q1h8_`j'm stopm`j'

	replace starty`j' = . if starty`j'>7 & (`j'<3|`j'>=7)
	replace starty`j' = . if starty`j'>90 & (`j'>=3&`j'<7)
	replace startm`j' = . if startm`j'>96

	replace stopy`j' = . if stopy`j'>96 & `j'<7
	replace stopy`j' = . if stopy`j'>7 & `j'>=7
	replace stopm`j' = . if stopm`j'>96 & `j'<8
	replace stopm`j' = . if stopm`j'>7 & `j'>=8

	replace starty`j' = 12*starty`j'
	replace stopy`j' = 12*stopy`j'

	replace startm`j' = starty`j' if missing(startm`j')
	replace startm`j' = min(startm`j',starty`j') if !missing(startm`j')&!missing(starty`j')

	replace stopm`j' = stopy`j' if missing(stopm`j')
	replace stopm`j' = max(stopm`j',stopy`j') if !missing(stopm`j')&!missing(stopy`j')

	replace stopy`j' = ageyear if stopy`j' == 96
	replace stopm`j' = age_cds if stopm`j' == 96

* fill duration: makes it annual!
	g caredur`j' = stopm`j' - startm`j'
	replace caredur`j' = max(1,min(12,caredur`j'))
	replace caredur`j' = 1 if (missing(caredur`j'))

	replace ccost`j' = ccost`j'*caredur`j'/12
	replace ccost`j' = . if stopm`j'<age_cds-12

	replace allmissing = allmissing&missing(ccost`j')

}

order ccost*
egen chrcost1= 	rsum(ccost1-ccost4)
replace chrcost1 = . if allmissing
replace chrcost1 = chrcost1
drop ccost* caredur* ctype* *wk*

*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* THIS IS FOR USUAL ARRNGMNT 1-3 (excluding 4, none exist)
*-----------------------------------------------------------------------------*-----------------------------------------------------------------------------
replace allmissing = 1
quietly forval j=1/3{
	local k = 21 + (`j'-1)*6
	local l = `k'-2
	local m = `k'-3

	rename q1h`k' ccost`j'
	rename q1h`k'a ctype`j'
	rename q1h`l' hrswk`j'
	rename q1h`m' dyswk`j'

* replace missing values: costs, time, kids
	replace ctype`j' = . if ctype`j'==0|ctype`j'>6
	replace ccost`j' = . if missing(ctype`j')
*|ccost`j'==0

	replace ccost`j' = . if ccost`j'>9000 & `j'==1
	replace ccost`j' = . if ccost`j'>900 & `j'==2
	replace ccost`j' = . if ccost`j'>5 & `j'==3

	replace hrswk`j' = . if hrswk`j'==0
	replace hrswk`j' = . if hrswk`j'>900 & `j'==1
	replace hrswk`j' = . if hrswk`j'>90 & `j'>1
	replace dyswk`j' = . if dyswk`j'==0|dyswk`j'>7

** some reportings are nonsense, trim them
*	replace ccost`j' = . if ctype`j'==1 & ccost`j'>hcap
*	replace ccost`j' = . if ctype`j'==2 & ccost`j'>dcap

* to weekly: hourly, daily, biweekly, monthly, annually
	replace ccost`j' = ccost`j' *hrswk`j' if (ctype`j'==1)
	replace ccost`j' = ccost`j' *dyswk`j' if (ctype`j'==2)
	replace ctype`j' = 3 if ctype`j'==1|ctype`j'==2
}

preserve
	keep cds_weight ctype* ccost*
	drop if missing(ccost1)&missing(ccost2)&missing(ccost3)

	quietly forval j =1/3{
		egen shre`j' = total(cds_weight) if !missing(ccost`j'), by(ctype`j')
		egen avrg`j' = total(ccost`j'*cds_weight) if !missing(ccost`j'), by(ctype`j')
	}
	drop cds_weight ccost*
	g id = _n
	reshape long shre avrg ctype, i(id) j(type)
	drop id
	duplicates drop
	drop if missing(ctype)|missing(shre)

	collapse (sum) shre avrg, by(ctype)
	replace avrg = avrg/shre

	scalar y3 = avrg[_N]/avrg[1]
	scalar y4 = avrg[_N]/avrg[2]
	scalar y5 = avrg[_N]/avrg[3]
restore

quietly forval j = 1/3{
* change to annual cost
	replace ccost`j' = ccost`j'* y3 if ctype`j'==3
	replace ccost`j' = ccost`j'* y4 if ctype`j'==4
	replace ccost`j' = ccost`j'* y5 if ctype`j'==5

* duration: months
	local k = 20 + (`j'-1)*6
	rename q1h`k'm durm`j'
	if `j'<3{
		rename q1h`k'w durw`j'
	}
	else{
		g durw`j' = .
	}

	replace durm`j' = . if durm`j'==0
	replace durm`j' = . if durm`j'>900 & `j'==1
	replace durm`j' = . if durm`j'>90 & `j'>1

	replace durw`j' = . if (durw`j'==0|durw`j'>90) & `j'<3
	replace durw`j' = durw`j'/nweeks

* fill duration: makes it annual!
	g caredur`j' = max(durw`j',durm`j') if !missing(durw`j')&!missing(durm`j')
	replace caredur`j' = durm`j' if missing(caredur`j')
	replace caredur`j' = durw`j' if missing(caredur`j')

	replace caredur`j' = max(1,min(12,caredur`j'))
	replace caredur`j' = 1 if (missing(caredur`j'))

	replace ccost`j' = ccost`j'*caredur`j'/12

	replace allmissing = allmissing&missing(ccost`j')



}

order ccost*
egen chrcost0= rsum(ccost1-ccost3)
replace chrcost0 = . if allmissing
replace chrcost0 = chrcost0
drop ccost* caredur* ctype* *wk*


cd ../

quietly compress


save datasets/cds_psid, replace

*------------------------------------------------------------
* 2002: have both 2001 and 2003
*------------------------------------------------------------
cd cds02
clear all

quietly do J301650
quietly do J301650_formats
rename *, lower
g pid = er30001*1000 + er30002
rename er20457 educhead
rename er20458 educwife

order pid

* parent demographic
rename er17012 num_fam0
rename er17013 head_age0
rename er17015 wife_age0
rename er17016 num_child0

rename er21016 num_fam
rename er21017 head_age
rename er21018 head_sex
rename er21019 wife_age
rename er21023 head_marriage
rename er21020 num_child
rename er21021 youngest_age

* parent economic
rename er24100 hh_taxable
rename er24099 hh_faminc
rename er24116 head_labinc
rename er24135 wife_labinc

* correct top-coding
foreach var of varlist hh* *labinc{
	replace `var' = `var'*1.5 if hh_faminc>=9999999
}

rename er24078 head_weekly
rename er24080 head_annual
rename er24089 wife_weekly
rename er24091 wife_annual

* psid identifiers
drop er33*1

*rename er336*1 invw_no0
rename er336*2 seq_no0
rename er336*3 rel_head0
rename er336*4 age_psid0
rename er33606 byear_psid0

*rename er337*1 invw_no
rename er337*2 seq_no
rename er337*3 rel_head
rename er337*4 age_psid
rename er33706 byear_psid

* cds identifiers
rename q21iwage age_cds
rename relpcg02 rel_pcg
rename relocg02 rel_ocg
rename ch02prwt cds_weight

* scores
drop cdi*
rename q24lw* lwq*
rename lwqss lwqstd
drop lwq58

quietly {
	foreach score of varlist lwq*{
		label var `score' ""
	}
	forvalues i=1/57{
		replace lwq`i' = 0 if lwq`i'==8
		replace lwq`i' = . if lwq`i'==9
	}
}

g year=2002
quietly compress


save ../datasets/cds02_tmp, replace

* money inv
cd ../cds0207
clear all

quietly do J294224.do
quietly do J294224_formats.do
rename *, lower 
g pid = er30001*1000+er30002
save cost0207tmp.dta, replace

clear all
quietly do J301939.do
quietly do J301939_formats.do
rename *, lower

g pid = er30001*1000+er30002
rename q21iwage ageatpcg

merge 1:1 pid using cost0207tmp.dta
drop _merge

save ../datasets/raw0207, replace
drop q3*
drop er*
rename ch02prwt cdswgt
drop ch0*

rename q21b11a1 school1
rename q21b11a2 type1

rename q21b12a1 school0
rename q21b12a2 type0

rename q21b11 schoolpub
rename q21b12_2 schoolpub0
rename q21b13 schooleverpub
rename q21b14 schoolpubtype

rename q21b15a schpubgr1
rename q21b15b schpubgr2
rename q21b15c schpubgr3
rename q21b15d schpubgr4
rename q21b15e schpubgr5
rename q21b15f schpubgr6
rename q21b15g schpubgr7
rename q21b15h schpubgr8
rename q21b15i schpubgr9
rename q21b15j schpubgr10
rename q21b15k schpubgr11
rename q21b15l schpubgr12

drop schoolpub schoolpub0 schooleverpub schoolpubtype

scalar nweeks = 52/12

* normalize fee rate type to years
quietly foreach num in 0 1{
	replace school`num' = . if (school`num'==0|school`num'>99997|type`num'==0|type`num'>6)

	preserve
		keep if (type`num'>0 & type`num'<7) 
		egen shre = total(cdswgt) if !missing(school`num'), by(type`num')
		egen avrg = total(school`num'*cdswgt) if !missing(school`num'), by(type`num')

		collapse shre avrg, by(type`num')
		replace avrg = avrg/shre
		drop if missing(avrg)

		local y3 = avrg[_N]/avrg[1]
		local y5 = avrg[_N]/avrg[2]
	restore
	replace school`num' = school`num'*`y3' if type`num'==3
	replace school`num' = school`num'*`y5' if type`num'==5
}
drop type*

gen byte allmissing = 1
quietly forval j = 1/3{
	rename q21c7a_`j' ccost`j'
	rename q21c7b_`j' ctype`j'
	rename q21c6_`j' hrswk`j'
	rename q21c5_`j' dyswk`j'

* replace missing values
	replace ctype`j' = . if ctype`j'==0|ctype`j'>6
	replace ccost`j' = . if missing(ctype`j')
*|ccost`j'==0

	replace ccost`j' = . if ccost`j'>99997 & `j'==1
	replace ccost`j' = . if ccost`j'>9997 & `j'==2|`j'==3
	replace ccost`j' = . if ccost`j'>997 & `j'==4

	replace hrswk`j' = . if hrswk`j'==0|hrswk`j'>900
	replace dyswk`j' = . if dyswk`j'==0|dyswk`j'>7
}


preserve
	keep ctype* ccost*
	g byte cmissing = missing(ccost1)
	quietly forval j = 1/3{
		replace cmissing = cmissing&missing(ccost`j')
	}
	drop if cmissing
	drop cmissing

	quietly forval j = 1/3{
		egen mmax`j' = max(ccost`j') if !missing(ccost`j'), by(ctype`j')
	}
	keep mmax* *type*
	g id = _n
	reshape long mmax ctype, i(id) j(type)
	drop id
	duplicates drop
	drop if missing(ctype)|missing(mmax)

	collapse (max) mmax, by(ctype)
	scalar dcap = mmax[3]/2
	scalar hcap = dcap/2
restore

* replace values
quietly forval j = 1/3{

* to weekly: hourly, daily, biweekly, monthly, annually
	replace ccost`j' = ccost`j' *hrswk`j' if (ctype`j'==1)
	replace ccost`j' = ccost`j' *dyswk`j' if (ctype`j'==2)
	replace ctype`j' = 3 if ctype`j'==1|ctype`j'==2
}

preserve
	keep cdswgt ctype* ccost*
	g byte cmissing = missing(ccost1)
	quietly forval j = 1/3{
		replace cmissing = cmissing&missing(ccost`j')
	}
	drop if cmissing
	drop cmissing

	quietly forval j =1/3{
		egen shre`j' = total(cdswgt) if !missing(ccost`j'), by(ctype`j')
		egen avrg`j' = total(ccost`j'*cdswgt) if !missing(ccost`j'), by(ctype`j')
	}
	drop cdswgt ccost*
	g id = _n
	reshape long shre avrg ctype, i(id) j(type)
	drop id
	duplicates drop
	drop if missing(ctype)|missing(shre)

	collapse (sum) shre avrg, by(ctype)
	replace avrg = avrg/shre

	scalar y3 = avrg[_N]/avrg[1]
	scalar y4 = avrg[_N]/avrg[2]
	scalar y5 = avrg[_N]/avrg[3]
restore

quietly forval j = 1/3{
* change to annual cost
	replace ccost`j' = ccost`j'* y3 if ctype`j'==3
	replace ccost`j' = ccost`j'* y4 if ctype`j'==4
	replace ccost`j' = ccost`j'* y5 if ctype`j'==5

* duration: months
	rename q21c4a_`j' start`j'
	rename q21c4b_`j' strut`j'

	rename q21c8a_`j' stop`j'
	rename q21c8b_`j' sput`j'

	replace strut`j' = . if strut`j'==0|strut`j'>3
	replace start`j' = . if start`j'>97|missing(strut`j')

	replace sput`j' = . if sput`j'==0|sput`j'>3
	replace stop`j' = . if stop`j'>97|missing(sput`j')

	replace start`j' = start`j'/nweeks if strut`j'==1
	replace start`j' = start`j'*12 if strut`j'==3

	replace stop`j' = stop`j'/nweeks if sput`j'==1
	replace stop`j' = stop`j'*12 if sput`j'==3

	replace stop`j' = ageatpcg*12 if stop`j'==97

* fill duration: makes it annual!
	g caredur`j' = stop`j'-start`j'
	replace caredur`j' = max(1,min(12,caredur`j'))
	replace caredur`j' = 1 if (missing(caredur`j'))

	replace ccost`j' = ccost`j'*caredur`j'/12
	replace ccost`j' = . if stop`j'<(ageatpcg-1)*12

	replace allmissing = allmissing&missing(ccost`j')
}
order ccost*
egen chrcost1 = rsum(ccost1-ccost3)
replace chrcost1 = . if allmissing
replace chrcost1 = chrcost1
drop ccost* caredur* ctype* *wk*


replace allmissing = 1
forval j = 1/2{
	local k = 15 + (`j'-1)*6
	if `j'<5{
		local l = `k'-2
	}
	else{
		local l = `k'-3
	}

	rename q21c`k'a ccost`j'
	rename q21c`k'b ctype`j'
	rename q21c`l' hrswk`j'

* replace missing values
	replace ctype`j' = . if ctype`j'==0|ctype`j'>6
	replace ccost`j' = . if missing(ctype`j')
*|ccost`j'==0

	replace ccost`j' = . if ccost`j'>9997 & `j'==1|`j'>3
	replace ccost`j' = . if ccost`j'>997 & `j'==2|`j'==3

	replace hrswk`j' = . if hrswk`j'==0|hrswk`j'>900

* hourly, daily, biweekly, monthly, annually to weekly
	replace ccost`j' = ccost`j' *hrswk`j' if (ctype`j'==1)
	replace ctype`j' = 3 if ctype`j'==1
}

* normalize to annual costs
preserve
	keep cdswgt ctype* ccost*
	g byte cmissing = missing(ccost1)
	quietly forval j = 1/2{
		replace cmissing = cmissing&missing(ccost`j')
	}
	drop if cmissing
	drop cmissing

	quietly forval j = 1/2{
		egen shre`j' = total(cdswgt) if !missing(ccost`j'), by(ctype`j')
		egen avrg`j' = total(ccost`j'*cdswgt) if !missing(ccost`j'), by(ctype`j')
	}
	drop cdswgt ccost*
	g id = _n
	reshape long shre avrg ctype, i(id) j(type)
	drop id
	duplicates drop
	drop if missing(ctype)|missing(shre)

	collapse (sum) shre avrg, by(ctype)
	replace avrg = avrg/shre

	scalar y2 = avrg[_N]/avrg[1]
	scalar y3 = avrg[_N]/avrg[2]
	scalar y4 = avrg[_N]/avrg[3]
	scalar y5 = avrg[_N]/avrg[4]
restore

quietly forval j = 1/2{
* change to annual cost
	replace ccost`j' = ccost`j'* y2 if ctype`j'==2
	replace ccost`j' = ccost`j'* y3 if ctype`j'==3
	replace ccost`j' = ccost`j'* y4 if ctype`j'==4
	replace ccost`j' = ccost`j'* y5 if ctype`j'==5

* duration
	if `j'<5{
		local k = 14 + (`j'-1)*6
	}
	else{
		local k = 37
	}
	rename q21c`k'a caredur`j'
	rename q21c`k'b cdu`j'

	replace cdu`j' = . if cdu`j'==0|cdu`j'>3
	replace caredur`j' = . if missing(cdu`j')
	replace caredur`j' = . if caredur`j'>97 & `j'==1|`j'==3
	replace caredur`j' = . if caredur`j'>997 & `j'!=1&`j'!=3

	replace caredur`j' = caredur`j'/nweeks if cdu`j'==1
	replace caredur`j' = caredur`j'*12 if cdu`j'==3

	replace caredur`j' = max(1,min(12,caredur`j'))
	replace caredur`j' = 1 if (missing(caredur`j'))

	replace ccost`j' = ccost`j'*caredur`j'/12

	replace allmissing = allmissing&missing(ccost`j')
}
order ccost*
egen chrcost2 = rsum(ccost1-ccost2)
replace chrcost2 = . if allmissing
drop ccost* ctype* *wk*

*-----------------------------------------------------------------------------
* EXTRACURRICULAR COSTS
*-----------------------------------------------------------------------------
drop q21h23c q21h32*

g xcost1 = 0
replace allmissing = 1

quietly foreach var of varlist q21g* q21h* {
	replace `var' = . if `var'==0 | `var'>=9998
	replace xcost1 = xcost1 + `var' if !missing(`var')
	replace allmissing = allmissing&missing(`var')
}
replace xcost1 = . if allmissing

g xcost2 = 0
replace allmissing = 1

quietly foreach var of varlist q23* {
	replace `var' = . if `var'==0
	replace `var' = . if `var'>=9998 & "`var'"=="q23l6d"
	replace `var' = . if `var'>=998 & "`var'"=="q23l6e"
	replace xcost2 = xcost2 + `var' if !missing(`var')
	replace allmissing = allmissing&missing(`var')
}
replace xcost2 = . if allmissing

keep pid sch* *cost*

gen year = 2002

cd ../

merge 1:1 pid year using datasets/cds02_tmp, keep(3)
drop _m

save datasets/cds02_tmp, replace


append using datasets/cds_psid
save datasets/cds_psid, replace

*------------------------------------------------------------
* 2007
*------------------------------------------------------------
clear all
cd cds07

quietly do J301668
quietly do J301668_formats
rename *, lower
g pid = er30001*1000 + er30002
rename er41037 educhead
rename er41038 educwife

drop er30*
order pid

* parent demographic
rename er25016 num_fam0
rename er25020 num_child0

rename er36016 num_fam
rename er36017 head_age
rename er36018 head_sex
rename er36019 wife_age
rename er36023 head_marriage
rename er36020 num_child
rename er36021 youngest_age

* parent economic
rename er40943 hh_taxable
rename er41027 hh_faminc
rename er40921 head_labinc
rename er40933 wife_labinc

* correct top-coding
foreach var of varlist hh* *labinc{
	replace `var' = `var'*1.5 if hh_faminc>=9999999
}

rename er40874 head_weekly
rename er40876 head_annual
rename er40885 wife_weekly
rename er40887 wife_annual

* psid identifiers
drop er33*1
*rename er33*1 invw_no
*label var invw_no "INTERVIEW NUMBER"

rename er338*2 seq_no0
rename er338*3 rel_head0

rename er339*2 seq_no
rename er339*3 rel_head
rename er339*4 age_psid
rename er339*6 byear_psid

* cds identifiers
rename q31iwage age_cds
rename relpcg07 rel_pcg
rename relocg07 rel_ocg
rename ch07wt cds_weight

* scores
rename q34lw* lwq*
rename lwqss lwqstd
drop lwq58

quietly {
	foreach score of varlist lwq*{
		label var `score' ""
	}
	forvalues i=1/57{
		replace lwq`i' = 0 if lwq`i'==5|lwq`i'==8
		replace lwq`i' = . if lwq`i'==0|lwq`i'==9
	}
}

cd ../

g year=2007

quietly compress

save datasets/cds07_tmp, replace

********************
* 2007 Money costs *
********************
cd datasets
use raw0207.dta, clear

*-----------------------------------------------------------------------------
* CDS 07
*-----------------------------------------------------------------------------
drop q2*
drop er*
rename ch07wt cdswgt
drop ch0*

*-----------------------------------------------------------------------------
* PARENTAL SCH COSTS
*-----------------------------------------------------------------------------
rename q31b11a1 school1
rename q31b11a2 type1

rename q31b12a1 school0
rename q31b12a2 type0

rename q31b11 schoolpub
rename q31b12_2 schoolpub0
rename q31b13 schooleverpub
rename q31b14 schoolpubtype

rename q31b15a schpubgr1
rename q31b15b schpubgr2
rename q31b15c schpubgr3
rename q31b15d schpubgr4
rename q31b15e schpubgr5
rename q31b15f schpubgr6
rename q31b15g schpubgr7
rename q31b15h schpubgr8
rename q31b15i schpubgr9
rename q31b15j schpubgr10
rename q31b15k schpubgr11
rename q31b15l schpubgr12

drop schoolpub schoolpub0 schooleverpub schoolpubtype

* normalize fee rate type to years
quietly foreach num in 0 1{
	replace school`num' = . if (school`num'==0|school`num'>99997|type`num'==0|type`num'>6)
	preserve
		keep if (type`num'>0 & type`num'<7) 
		egen shre = total(cdswgt) if !missing(school`num'), by(type`num')
		egen avrg = total(school`num'*cdswgt) if !missing(school`num'), by(type`num')

		collapse shre avrg, by(type`num')
		replace avrg = avrg/shre
		drop if missing(avrg)

		local y5 = avrg[_N]/avrg[1]
	restore
	replace school`num' = school`num'*`y5' if type`num'==5
}
drop type*

g allmissing = 1
quietly forval j = 1/2{
	local k = 15 + (`j'-1)*6
	if `j'<5{
		local l = `k'-2
	}
	else{
		local l = `k'-3
	}

	rename q31c`k'a ccost`j'
	rename q31c`k'b ctype`j'
	rename q31c`l' hrswk`j'

* replace missing values
	replace ctype`j' = . if ctype`j'==0|ctype`j'>6
	replace ccost`j' = . if missing(ctype`j')|ccost`j'>9997

	replace hrswk`j' = . if hrswk`j'==0|hrswk`j'>900
}

preserve
	keep ctype* ccost*
	g byte cmissing = missing(ccost1)
	quietly forval j = 1/2{
		replace cmissing = cmissing&missing(ccost`j')
	}
	drop if cmissing
	drop cmissing

	quietly forval j = 1/2{
		egen mmax`j' = max(ccost`j') if !missing(ccost`j'), by(ctype`j')
	}
	keep mmax* *type*
	g id = _n
	reshape long mmax ctype, i(id) j(type)
	drop id
	duplicates drop
	drop if missing(ctype)|missing(mmax)

	collapse (max) mmax, by(ctype)
	scalar dcap = mmax[3]/2
	scalar hcap = dcap/2
restore

quietly forval j = 1/2{

* hourly, daily, biweekly, monthly, annually to weekly
	replace ccost`j' = ccost`j' *hrswk`j' if (ctype`j'==1)
	replace ctype`j' = 3 if ctype`j'==1
}

* normalize to annual costs
preserve
	keep cdswgt ctype* ccost*
	g byte cmissing = missing(ccost1)
	quietly forval j = 1/2{
		replace cmissing = cmissing&missing(ccost`j')
	}
	drop if cmissing
	drop cmissing

	quietly forval j = 1/2{
		egen shre`j' = total(cdswgt) if !missing(ccost`j'), by(ctype`j')
		egen avrg`j' = total(ccost`j'*cdswgt) if !missing(ccost`j'), by(ctype`j')
	}
	drop cdswgt ccost*
	g id = _n
	reshape long shre avrg ctype, i(id) j(type)
	drop id
	duplicates drop
	drop if missing(ctype)|missing(shre)

	collapse (sum) shre avrg, by(ctype)
	replace avrg = avrg/shre

	scalar y2 = avrg[_N]/avrg[1]
	scalar y3 = avrg[_N]/avrg[2]
	scalar y4 = avrg[_N]/avrg[3]
	scalar y5 = avrg[_N]/avrg[4]
restore

scalar nweeks=52/12
quietly forval j = 1/2{
* change to annual cost
	replace ccost`j' = ccost`j'* y2 if ctype`j'==2
	replace ccost`j' = ccost`j'* y3 if ctype`j'==3
	replace ccost`j' = ccost`j'* y4 if ctype`j'==4
	replace ccost`j' = ccost`j'* y5 if ctype`j'==5

* duration
	if `j'<5{
		local k = 14 + (`j'-1)*6
	}
	else{
		local k = 37
	}
	rename q31c`k'a caredur`j'
	rename q31c`k'b cdu`j'

	replace cdu`j' = . if cdu`j'==0|cdu`j'>3
	replace caredur`j' = . if missing(cdu`j')|caredur`j'>997

	replace caredur`j' = caredur`j'/nweeks if cdu`j'==1
	replace caredur`j' = caredur`j'*12 if cdu`j'==3

	replace caredur`j' = max(1,min(12,caredur`j'))
	replace caredur`j' = 1 if (missing(caredur`j'))

	replace ccost`j' = ccost`j'*caredur`j'/12

	replace allmissing = allmissing&missing(ccost`j')
}
order ccost*
egen chrcost2 = rsum(ccost1-ccost2)
replace chrcost2 = . if allmissing
replace chrcost2 = chrcost2
drop ccost* ctype* *wk*

*-----------------------------------------------------------------------------
* EXTRACURRICULAR COSTS
*-----------------------------------------------------------------------------
drop q31h23c q31h32*

g xcost1 = 0
replace allmissing = 1

quietly foreach var of varlist q31h* {
	replace `var' = . if `var'==0
	replace `var' = . if `var'>=9998 & "`var'"!="q31h23c1"
	replace `var' = . if `var'>=99998 & "`var'"=="q31h23c1"
	replace xcost = xcost + `var' if !missing(`var')
	replace allmissing = allmissing&missing(`var')
}
replace xcost = . if allmissing

g xcost2 = 0
replace allmissing = 1

quietly foreach var of varlist q33* {
	replace `var' = . if `var'==0 | `var'>=99998
	replace xcost2 = xcost2 + `var' if !missing(`var')
	replace allmissing = allmissing&missing(`var')
}
replace xcost2 = . if allmissing


keep pid sch* *cost*
g year =2007

*-----------------------------------------------------------------------------
cd ../

merge 1:1 pid year using datasets/cds07_tmp, keep(3)
drop _m

quietly compress

save datasets/cds07_tmp, replace

append using datasets/cds_psid
save datasets/cds_psid, replace

*-----------------------------------------------------------------------------

order pid year sch* chr* xcost*

scalar pce1996 = 1.304836426352660
scalar pce1997 = 1.282515812889740

scalar pce2001 = 1.200242670250630
scalar pce2002 = 1.184623557201360

scalar pce2006 = 1.073234027044450
scalar pce2007 = 1.046665627708470

quietly foreach year of numlist 1997 2002 2007{
	foreach var of varlist school0 *cost* {
		local j = `year'-1
		replace `var' = `var'*pce`j' if year==`year'
	}
	replace school1 = school1*pce`year' if year==`year'
}

save datasets/cds_psid, replace
save datasets/cds_costs, replace

*------------------------------------------------------------
*------------------------------------------------------------
replace age_psid0 = . if age_psid0==0|age_psid0==999
replace age_psid = . if age_psid==0|age_psid==999
replace byear_psid0 = . if byear_psid0==0|byear_psid0==9999
replace byear_psid = . if byear_psid==0|byear_psid==9999

replace head_marriage = . if head_marriage>7
replace youngest_age = . if youngest_age==0|youngest_age==999

foreach i in head wife{
	replace `i'_age = . if `i'_age>120
	replace `i'_age0 = . if `i'_age0>120
}
replace age_cds = . if age_cds>900
replace rel_pcg = . if rel_pcg==0	//|rel_pcg==99
replace rel_ocg = . if rel_ocg==0	//|rel_ocg==99

replace lwqraw = 57 if lwqraw==58
replace lwqraw = . if lwqraw==99
replace lwqstd = . if lwqstd==999
*------------------------------------------------------------

scalar pce1997 = 1.304836426352660
scalar pce2002 = 1.184623557201360
scalar pce2007 = 1.073234027044450


quietly foreach dollars of varlist head_labinc-hh_faminc{
	foreach t of numlist 1997 2002 2007 {
		replace `dollars' = `dollars'*pce`t' if year==`t'
	}
}
*------------------------------------------------------------
* annualize ages
replace age_cds = round(age_cds)



* fill missing ages: it turns out only missing in year 2007
* (we already fixed 1997)
replace age_cds = age_psid if missing(age_cds) & year!=2002
replace age_cds = year - byear_psid if missing(age_cds) & year!=2002
drop byear* age_psid*

drop if age_cds==.
* if kid age unknown, useless
assert missing(age_cds)==0

* normalize weights by year
egen tweight = total(cds_weight), by(year)
replace cds_weight = cds_weight/tweight
drop tweight
*------------------------------------------------------------
*drop *weekly
order pid year
sort pid year
assert (pid==pid[_n-1]&year==year[_n-1])==0


drop q1* ageyear

save datasets/cds_psid, replace

quietly do cds_base
use datasets/cds_base, clear
drop lwqstd
rename lwqraw rawscore

* first, generate difficulties
quietly forval i=1/57{
	egen lwqdiff`i' = total(cds_weight*lwq`i'), by(age)
	egen lwdenom`i' = total(cds_weight*!missing(lwq`i')), by(age)
	replace lwqdiff`i' = lwqdiff`i'/lwdenom`i'
}

* by age
*----------------------
preserve
	collapse lwqdiff* lwdenom*, by(age)
	order age lwqdiff*
	compress
restore
*----------------------

quietly forval i=1/57{
	egen mlwqdiff`i' = total(lwdenom`i'*lwqdiff`i')
	egen mlwdenom`i' = total(lwdenom`i'*!missing(lwqdiff`i'))
	replace mlwqdiff`i' = mlwqdiff`i'/mlwdenom`i'

	drop lwqdiff`i' lwdenom`i'
	rename mlwqdiff`i' lwqdiff`i'
	rename mlwdenom`i' lwdenom`i'
}

*----------------------
preserve
	collapse lwqdiff* lwdenom*

	g one = 1
	reshape long lwqdiff lwdenom, i(one) j(no)
	drop one

restore
*----------------------

* create adjusted scores and normalize
drop lwdenom*

* 1- or 1/?
quietly forval i=1/57{
	replace lwqdiff`i' = 1/lwqdiff`i'
}
egen adjtot = rsum(lwqdiff1-lwqdiff57)

quietly forval i=1/57{
	replace lwqdiff`i' = lwq`i'*lwqdiff`i'
}
egen adjscore = rsum(lwqdiff1-lwqdiff57)
drop lwq*

replace rawscore = rawscore/57 * 100
replace adjscore = adjscore/adjtot * 100
replace adjscore = . if rawscore==.
drop adjtot

*----------------------
preserve
	reg rawscore i.age [pw=cds_weight], vce(cluster pid)
	predict avgrawscore
	g resid = rawscore - avgrawscore
	egen varraw = mean(resid^2), by(age)
	drop resid

	reg adjscore i.age [pw=cds_weight], vce(cluster pid)
	predict avgadjscore
	g resid = adjscore - avgadjscore
	egen varadj = mean(resid^2), by(age)
	drop resid

	collapse avg* var*, by(age)
	replace varraw = sqrt(varraw)
	replace varadj = sqrt(varadj)

	
restore

count

merge 1:1 year pid using datasets/cds_match, keep(3)
drop _m

*-------------------------------
* some cleaning
*-------------------------------
* i. first, based only on psid child

* drop if kid is not in household
keep if seq_no<50 | missing(seq_no)
keep if seq_no0<50 | missing(seq_no0)
keep if seq_no1<50 | missing(seq_no1)
drop seq_no*

* drop if head of household is not a parent
keep if (rel_head>=30&rel_head<=35)|rel_head==83|missing(rel_head)
keep if (rel_head0>=30&rel_head0<=35)|rel_head0==83|missing(rel_head0)
keep if (rel_head1>=30&rel_head1<=35)|rel_head1==83|missing(rel_head1)
*drop rel_head*

*-------------------------------
* ii. now, based on psid caregivers

* at least 1 biological parent
assert (missing(rel_pcg)&missing(rel_ocg))==0
keep if (rel_pcg==1|rel_ocg==1|rel_pcg==4|rel_ocg==4|rel_head==30)

* drop if caregiver is not in the household
* at least 1 head or wife of household
* both must be some kind of parent
* pcg or ocg cannot be wife who is not present
foreach i in pcg ocg{
	keep if `i'_seq_no<50 | missing(`i'_seq_no)

	g byte `i'_is_head = `i'_head==10
	g byte `i'_is_wife = `i'_head==20|`i'_head==22|`i'_head==88|`i'_head==90

	drop if !`i'_is_head&!`i'_is_wife&!missing(`i'_head)
	drop if !missing(rel_`i') & (rel_`i'>6)
	keep if `i'rel<3|missing(`i'rel)
}
drop *seq_no

keep if pcg_is_head|ocg_is_head|pcg_is_wife|ocg_is_wife
drop if !pcg_is_head&!ocg_is_head


*-------------------------------
* iii. control for adults' age

* REMEMBER THAT WIFE_AGE==0 MEANS NO WIFE
drop if wife_age!=0 & !pcg_is_wife & !ocg_is_wife
drop if wife_age==0 & (pcg_is_wife | ocg_is_wife)

* drop too young and too old parents
* drop too young and too old caregivers
foreach i in head wife pcg ocg{
	g pbirth = `i'_age-age_cds if `i'_age!=0

	g byte keep = 0
	replace keep = 1 if (pbirth>=18&pbirth<=41&year!=2002)|missing(pbirth)|(`i'_age==0)
	replace keep = 1 if (pbirth>=19&pbirth<=42&year==2002)|missing(pbirth)|(`i'_age==0)
	keep if keep
	drop pbirth keep
}

* check if household changes in consecutive years by age
foreach i in head wife {
	g byte keep = 0
	replace keep = 1 if abs(`i'_age-`i'_age0)<5
	replace keep = 1 if missing(`i'_age)
	replace keep = 1 if missing(`i'_age0)
	keep if keep
	drop keep
}

* head/wife's age must be equal to one of the pcg/ocg
foreach i in pcg ocg{
	foreach j in head wife {
		g byte keep = 0
		replace keep = 1 if !`i'_is_`j'
		replace keep = 1 if `i'_is_`j' & `j'_age!=0 & (abs(`i'_age-`j'_age)<2|missing(`i'_age)|missing(`j'_age))
		keep if keep
		drop keep
	}
}

*-------------------------------
* iv. find if head-wives are bio or step parents

* now, we know at least 1 bio-parent
* head-wife-pcg-ocg all living together, are same couple

order pid year *pcg pcgrel pcg_head *ocg ocgrel ocg_head

* now use extra info to determine rel_cg from cgrel and other stuff
* not just missing and not missing

foreach i in ocg pcg{
	replace rel_`i'=4 if `i'_is_head&head_sex==1&rel_head==30
	replace rel_`i'=5 if `i'_is_head&head_sex==1&(rel_head==33|rel_head==35)

	replace rel_`i'=1 if `i'_is_head&head_sex==2&rel_head==30
	replace rel_`i'=2 if `i'_is_head&head_sex==2&(rel_head==33|rel_head==35)
}

assert (missing(rel_pcg)&!missing(pcgrel))==0

* most guys, we can easily find that ocgrel=1 means rel_ocg=1,
* by looking across the time panel and cross-checking.
* But for one set of twins, checked in individual psid: 3317 family
replace rel_ocg=1 if missing(rel_ocg)&ocgrel==1

assert (missing(rel_ocg)&!missing(ocgrel))==0

* some guys miss both rel_ocg AND ocgrel, even if they should have both
* both family 391 and 779 can be checked by siblings or panel
* both are step moms
replace rel_ocg=2 if (missing(rel_ocg)&missing(ocgrel)&wife_age!=0)

assert (missing(rel_pcg)&missing(pcgrel))==0
assert (missing(rel_ocg)&missing(ocgrel)&wife_age!=0)==0

* check if actually 2 mom, 2 dad households...NOT
* 2 moms: actually, divorced couple...
* according to above selection criterion,
* must drop if no wife but ocg is there
drop if rel_pcg==1&rel_ocg==1&ocgrel==2&head_sex==2&wife_age==0
assert (rel_pcg<4&rel_ocg<4)==0

* 2 dads: 1 is obviously a mom, looking at panel
* 1845003 is can be checked by siblings!
replace rel_pcg=1 if rel_pcg==4&rel_ocg==4&pcgrel==1
replace rel_ocg=1 if rel_pcg==4&rel_ocg==4&ocgrel==1

assert (rel_pcg>3&rel_pcg<7&rel_ocg>3&rel_ocg<7)==0

* at this point, all head-wives should be bio or step parent couples
g byte headparent=.
g byte wifeparent=.

quietly foreach i in head wife{
	assert (`i'_age<61 & `i'_age>18)|`i'_age==0
	foreach j in pcg ocg{
		forval k = 1/6{
			replace `i'parent = `k' if `j'_is_`i'&rel_`j'==`k'
		}
	}
}
replace wifeparent = 0 if wife_age==0

assert !missing(headparent)&!missing(wifeparent)

save datasets/temp, replace

*clear all
quietly do parent_time
use datasets/temp, clear
merge 1:1 year pid using datasets/time_parents, keep(3)
drop _m
sort pid year

* can get times by head-wife
* DON'T ADD WIFE IF WIFE_PARENT==0
foreach i in head wife {
	g double `i'time = wtime_gb if `i'parent==1|`i'parent==3
	g double `i'time2 = wtime_hb if `i'parent==1|`i'parent==3

	replace `i'time = wtime_gc if `i'parent==4|`i'parent==6
	replace `i'time2 = wtime_hc if `i'parent==4|`i'parent==6

	replace `i'time = wtime_ge if `i'parent==2
	replace `i'time2 = wtime_he if `i'parent==2

	replace `i'time = wtime_gf if `i'parent==5
	replace `i'time2 = wtime_hf if `i'parent==5
}
replace wifetime = 0 if wifeparent==0
replace wifetime2 = 0 if wifeparent==0

egen double xxxtime = rsum(wtime_gb-wtime_gf)
egen double xxxtime2 = rsum(wtime_hb-wtime_hf)

replace xxxtime = xxxtime - headtime - wifetime
replace xxxtime2 = xxxtime2 - headtime2 - wifetime2

keep if xxxtime==0&xxxtime2==0
drop *g_age
*keep pid *weight year age_cds *score head_sex *age *parent *annual *weekly *labinc hh* headt* wifet* educ*

foreach i in head wife {
	replace `i'time = `i'time/3600
	replace `i'time2 = `i'time2/3600
}

* drop if time==0
drop if headtime+wifetime==0

*------------------------------------------------------------------------
* parental time over ages: we have head-wife
*------------------------------------------------------------------------
preserve
* need to change to mom-dad
	g dadtime1 = headtime if head_sex==1
	g dadtime2 = headtime2 if head_sex==1
	replace dadtime1 = wifetime if head_sex==2 & wifeparent!=0
	replace dadtime2 = wifetime2 if head_sex==2 & wifeparent!=0

	g momtime1 = headtime if head_sex==2
	g momtime2 = headtime2 if head_sex==2
	replace momtime1 = wifetime if head_sex==1 & wifeparent!=0
	replace momtime2 = wifetime2 if head_sex==1 & wifeparent!=0

	* time per single parent
	g ptime1 = (headtime+wifetime*(wifeparent!=0))/(1+(wifeparent!=0))
	g ptime2 = (headtime2+wifetime2*(wifeparent!=0))/(1+(wifeparent!=0))

	foreach i in mom dad p {
		g `i'time3 = `i'time1 + `i'time2
		forval j=1/3 {
			egen t`i'time`j' = total(cds_weight*`i'time`j'), by(age_cds)
			egen c`i'time`j' = total(cds_weight*!missing(`i'time`j')), by(age_cds)
			g `i'time`j'mean = t`i'time`j' / c`i'time`j'
		}
	}
	collapse *mean, by(age_cds)

	order mom* dad* p*
	
restore

*------------------------------------------------------------------------
* TIME COSTS
*------------------------------------------------------------------------
foreach parent in head wife{
	g `parent'_hourly = `parent'_labinc/`parent'_annual
}

g tcost1 = head_hourly*headtime + wife_hourly*wifetime*(wifeparent!=0)
g tcost2 = head_hourly*headtime2 + wife_hourly*wifetime2*(wifeparent!=0)
g tcost3 = tcost1 + tcost2

g pcost1 = tcost1 / (1+(wifeparent!=0))
g pcost2 = tcost2 / (1+(wifeparent!=0))
g pcost3 = pcost1 + pcost2

scalar pce1968 = 4.074649544162340

preserve
	foreach cat in annual labinc {
		drop if head_`cat'==0 | (wife_`cat'==0&wifeparent!=0)
	}

	drop if head_labinc<1000/pce1968 |(wife_labinc<1000/pce1968 & wifeparent==1)
	drop if head_annual<260 & head_age<=30
	drop if head_annual<520 & head_age>30
	drop if wife_annual<260 & wife_age<=30 &wifeparent!=0
	drop if wife_annual<520 & wife_age>30 &wifeparent!=0

	foreach x in t p{
		forval j=1/3 {
			egen t`x'cost`j' = total(cds_weight*`x'cost`j'), by(age_cds)
			egen c`x'cost`j' = total(cds_weight*!missing(`x'cost`j')), by(age_cds)
			g mean`x'cost`j' = t`x'cost`j' / c`x'cost`j' * 52
		}
	}
	collapse mean*, by(age_cds)


restore

merge 1:1 year pid using datasets/cds_psid, keep(3)
drop _m

merge 1:1 year pid using datasets/cds_costs, keep(3)
drop _m

compress
save datasets/cds_psid, replace


