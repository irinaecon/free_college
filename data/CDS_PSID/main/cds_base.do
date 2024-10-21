*------------------------------------------------------------
* 1997
*------------------------------------------------------------
cd cds97
clear all
quietly do J278571
quietly do J278571_formats
rename *, lower
g pid = er30001*1000 + er30002
rename er12222 educhead
rename er12223 educwife
keep pid educ*
save temp, replace

quietly do J301870
*quietly do J301870_base
rename *, lower

* psid unique id
g pid = er30001*1000 + er30002

merge 1:1 pid using temp
keep if _merge==3
drop _merge
count

drop er30*
order pid

* parent demographic
rename er10008 num_fam
rename er10009 head_age
rename er10010 head_sex
rename er10011 wife_age
rename er10016 head_marriage
rename er10012 num_child
rename er10013 youngest_age

rename er13009 num_fam1
rename er13013 num_child1

* parent economic
rename er12069 hh_taxable
rename er12079 hh_faminc
rename er12080 head_labinc
rename er12082 wife_labinc

* correct top-coding
foreach var of varlist hh* *labinc{
	replace `var' = `var'*1.5 if hh_faminc>=999999
}

rename er12171 head_weekly
rename er12174 head_annual
rename er12182 wife_weekly
rename er12185 wife_annual

* psid identifiers
drop er33*1
*rename er33*1 invw_no
*label var invw_no "INTERVIEW NUMBER"

rename er334*2 seq_no
rename er334*3 rel_head
rename er334*4 age_psid
rename er334*6 byear_psid

rename er335*2 seq_no1
rename er335*3 rel_head1

* cds identifiers
rename ageatpcg age_cds
rename relpcg97 rel_pcg
rename relocg97 rel_ocg
rename ch97prwt cds_weight

replace age_cds = . if age_cds>900
replace age_cds = age_psid*12 if missing(age_cds)&age_psid>0&age_psid<999
replace age_cds = age_cds/12

drop q1*
*rename q1b17 age_cds1
*rename q1g1 age_cds2
*rename q1g22 age_cds3
*drop age_cds1 age_cds2 age_cds3

* scores
rename q3lw* lwq*
rename lwq_ss lwqstd
quietly {
	foreach score of varlist lwq*{
		label var `score' ""
	}
	forvalues i=1/57{
		replace lwq`i' = 0 if lwq`i'==8
		replace lwq`i' = . if lwq`i'==9
	}
}

cd ..
gen year=1997

save datasets/cds_base, replace

*------------------------------------------------------------
* 2002: have both 2001 and 2003
*------------------------------------------------------------
cd cds02

clear all
quietly do J278572
*quietly do J278572_formats
rename *, lower
g pid = er30001*1000 + er30002
rename er20457 educhead
rename er20458 educwife
keep pid educ*
*drop ch02prwt er17016 chrel
save temp, replace

quietly do J301928
*quietly do J301928_formats
rename *, lower

* psid unique id
g pid = er30001*1000 + er30002

merge m:m pid using temp
keep if _merge==3
drop _merge

drop er30*
order pid
count

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
rename er336*6 byear_psid0

*rename er337*1 invw_no
rename er337*2 seq_no
rename er337*3 rel_head
rename er337*4 age_psid
rename er337*6 byear_psid

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

cd ../
g year=2002
quietly compress

append using datasets/cds_base
save datasets/cds_base, replace

*------------------------------------------------------------
* 2007
*------------------------------------------------------------
cd cds07

quietly do J278573
*quietly do J278573_formats
rename *, lower
g pid = er30001*1000 + er30002
rename er41037 educhead
rename er41038 educwife
keep pid educ*
save temp, replace

quietly do J301929
*quietly do J301929_formats
rename *, lower

* psid unique id
g pid = er30001*1000 + er30002

merge m:m pid using temp
keep if _merge==3
drop _merge

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
gen year=2007
quietly compress
append using datasets/cds_base
save datasets/cds_base, replace

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
/*
* deflate to 2000 dollars: note that pce2002 is actually 2003
scalar pce1997 = 1.066041728113260
scalar pce2002 = 0.968069125336252
scalar pce2007 = 0.877566531896251
*/
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

compress
save datasets/cds_base, replace
