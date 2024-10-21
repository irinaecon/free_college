**************************************
* TIME INVESTMENT                *
**************************************

*--------------------
* 1. extract and merge
*--------------------
foreach t in "97" "02" "07" {
    clear all
 
    if ("`t'"=="97") {
		cd cds97_diary
		quietly do J301635
		quietly do J301635_formats
	}
	if ("`t'"=="02") {
		cd cds02_diary
		quietly do J301638
		quietly do J301638_formats
	}
	if ("`t'"=="07") {
		cd cds07_diary
		quietly do J301665
		quietly do J301665_formats
	}
	
	
	rename *, lower

	* identifiers
	g pid = er30001*1000 + er30002
	drop er30*
	order pid

	drop er33*1
*	rename er33*1 invw_no
*	label var invw_no "INTERVIEW NUMBER"

	rename er33*2 seq_no
	rename er33*3 rel_head

	* harmonize var names across years
	if ("`t'"!="97") {
		rename *_`t' *
		rename diary wdaywend
		rename dur duration
		replace t1 = t1-1
		replace t1 = 7 if t1==0
	}
	if ("`t'"=="07") {
		rename colh* colg*
		rename coli* colh*
		drop tdrel* colf*
	}

	* check vars
	replace colb = . if colb>90000
	replace colc = . if colc>90000
	replace duration = . if duration>99998

	assert (missing(colb)&missing(colc)&!missing(duration))==0
	assert (!missing(colb)&!missing(colc)&missing(duration))==0

	g tdiff = colc-colb
	assert tdiff==duration

	* turns out to be useless
	drop cola colb colc cold colj tdiff

	* check days of week: ARE NO WEEKS MISSING? (coded 9)
	assert (t1<8)

	egen check = mean(t1), by(pid wdaywend)
	assert check==t1
	drop check

	* construct durations
	foreach i in a b c e f {
		if ("`t'"!="97") rename *`i' *_`i'

		replace colg_`i' = colg_`i'==1
		replace colh_`i' = colh_`i'==1

		replace colg_`i' = colg_`i'*duration
		replace colh_`i' = colh_`i'*duration
	}
	collapse (sum) colg_* colh_*, by(pid seq_no rel_head t1 wdaywend)

	* check if 1 obs each for wday/wend
	sort pid wdaywend t1
	assert ((pid==pid[_n-1])&(wdaywend==wdaywend[_n-1]))==0

	* check if weekdays are weekdays and vice versa
	assert wdaywend<2
	assert (t1<6&wdaywend==0) == 0
	assert (t1>5&wdaywend==1) == 0
	drop wdaywend

	reshape wide col*, i(pid) j(t1)

	foreach i in g h{
		foreach j in a b c e f {

			g check = 0
			forvalues k=1/5 {
				replace check = check + !missing(col`i'_`j'`k')
			}
			assert check<=1
			replace check = !missing(col`i'_`j'6)&!missing(col`i'_`j'7)
			assert check==0
			drop check
		}
	}

	
	if ("`t'"=="97") {
		g year = 19`t'
		quietly compress
		save ../datasets/time_tmp, replace
	}
	else {
		g year = 20`t'
		quietly compress
	    append using ../datasets/time_tmp
		save ../datasets/time_tmp, replace
	
		
	}
	
	cd ..
	
}

order pid year seq_no rel_head
sort pid year
assert (pid==pid[_n-1]&year==year[_n-1])==0
compress
save datasets/time_tmp, replace


merge 1:1 pid year using datasets/cds_base, keep(3)
drop _m

save datasets/time_parents, replace

*--------------------
* weekday/weekend control
*--------------------

quietly{
	foreach i in g h{
		foreach j in a b c e f {
			forvalues k=1/7 {
				egen numer_`i'`j'`k' = total(cds_weight*col`i'_`j'`k'), by(age_cds)
				egen denom_`i'`j'`k' = total(cds_weight*!missing(col`i'_`j'`k')), by(age_cds)
			}

			order numer_`i'`j'* denom_`i'`j'*
			egen tnumer_`i'`j' = rsum(numer_`i'`j'1-numer_`i'`j'5)
			egen tdenom_`i'`j' = rsum(denom_`i'`j'1-denom_`i'`j'5)
		}
	}
}

collapse numer* denom* tnumer* tdenom*, by(age_cds)

* means
quietly{
	foreach i in g h{
		foreach j in a b c e f {
			forvalues k=1/7 {
				g imean_`i'`j'`k' = numer_`i'`j'`k'/denom_`i'`j'`k'
			}
			forvalues k=1/5 {
				g emean_`i'`j'`k' = (tnumer_`i'`j'-numer_`i'`j'`k')/(tdenom_`i'`j'-denom_`i'`j'`k')
			}
		}
	}
}


merge 1:m age using datasets/time_parents, assert(3)
drop _m

* now, adjust: also check again 1 obs each for wday/wend
quietly{
	foreach i in g h{
		foreach j in a b c e f {

			g byte testmissing`i'`j' = 0

			g check = 0
			forvalues k=1/5 {
				replace testmissing`i'`j' = 1 if !missing(col`i'_`j'`k')
				replace check = check + !missing(col`i'_`j'`k')
				g wdadj_`i'`j'`k' = col`i'_`j'`k'*emean_`i'`j'`k'/imean_`i'`j'`k'
			}
			assert check<=1

			replace testmissing`i'`j' = 1 if !missing(col`i'_`j'6)
			replace testmissing`i'`j' = 1 if !missing(col`i'_`j'7)

			replace check = !missing(col`i'_`j'6)&!missing(col`i'_`j'7)
			assert check==0
			drop check

			g weadj_`i'`j'6 = col`i'_`j'6*imean_`i'`j'7/imean_`i'`j'6
			g weadj_`i'`j'7 = col`i'_`j'7*imean_`i'`j'6/imean_`i'`j'7

			egen wdhours_`i'`j' = rsum(wdadj_`i'`j'1-wdadj_`i'`j'5)
			egen wehours_`i'`j' = rsum(weadj_`i'`j'6-weadj_`i'`j'7)

			g wtime_`i'`j' = wdhours_`i'`j'*5 + wehours_`i'`j'*2
			replace wtime_`i'`j' = wtime_`i'`j'	// /3600
		}
	}
}

* for now, don't use child's own time
drop col* imean* emean* wd* we*
drop wtime*a

foreach i in g h{
	foreach j in b c e f{
		recast double wtime_`i'`j'
		replace wtime_`i'`j'=. if !testmissing`i'`j'
	}
}

count if year==1997
count if year==2002
count if year==2007
count

keep pid year wtime*
sort pid year
compress
save datasets/time_parents, replace



