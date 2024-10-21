/* COMPUTE IQ GRADIENTS */
** Running the do file performs the wage analysis using the CPS-style wage measure available in the NLSY79, as discussed in Appendix E of the paper.  
clear
pause on
set more off

/***********************/
/*REAL HOURLY EARNINGS */
/***********************/

use NLSY_clean2.dta, replace

/********************************************************************************************************************************/
/* CPS WAGE ANALYSIS */

/* OBTAIN AGE-FREE REAL WAGES (use estimates of age profiles from PSID) */
/* Newage powers */
gen newage2 = newage^2
/* Profiles estimated in main.do */
/* Edu 1 */
gen logCPS_WAGES_AF_m = logCPS_WAGES - (.041*newage) - (-.0004*newage2) if edugroup==1 & sex==1
gen logCPS_WAGES_AF_f = logCPS_WAGES - (.0295213*newage) - (-.0002874*newage2) if edugroup==1 & sex==2
/* Edu 2 */
replace logCPS_WAGES_AF_m = logCPS_WAGES - (.067*newage) - (-.00067*newage2) if edugroup==2 & sex==1
replace logCPS_WAGES_AF_f = logCPS_WAGES - (.0330905*newage) - (-.0003257*newage2) if edugroup==2 & sex==2
/* Edu 3 */
replace logCPS_WAGES_AF_m = logCPS_WAGES - (.1197*newage) - (-.00119*newage2) if edugroup==3 & sex==1
replace logCPS_WAGES_AF_f = logCPS_WAGES - (.0794265*newage) - (-.000845*newage2) if edugroup==3 & sex==2

* age-free wages w/o gender differentiation
gen logCPS_WAGES_AF = .
replace logCPS_WAGES_AF = logCPS_WAGES_AF_m if sex==1
replace logCPS_WAGES_AF = logCPS_WAGES_AF_f if sex==2

/* Year Effects and Oversamples' dummy*/
forval num = 1968/1994 { 
generate dummy`num'=year==`num' 
}
forval num = 1996(2)2010 { 
generate dummy`num'=year==`num'
}   

quietly gen smpdum=sample_id>8

/* AFQT regressions */

**************************************************
*** TEST REGRESSIONS *****************************
**************************************************
* generate dummy for above and below the median
xtile AFQThalf = log(AFQT89), nq(3)
xtile AFQTquart = log(AFQT89), nq(5)

egen afqt_av =mean(AFQT89)

* log of original AFQT (original regressions)
gen logAFQT89 = log(AFQT89)

/* NORMALIZED AFQT: activate this to get a log-distribution of ability centred on zero */
egen M_logAFQT89=mean(logAFQT89)
replace logAFQT89 = logAFQT89 - M_logAFQT89

*gen AFQTreg = log(AFQT89/afqt_av)
cap drop AFQTreg
*gen AFQTreg = AFQT89 /afqt_av
gen AFQTreg = logAFQT89

bysort edugroup: su AFQTreg


/* Estimates of ability gradients from age-free wages */
cap drop levCPS_WAGES_AF
*gen levCPS_WAGES_AF =exp(logCPS_WAGES_AF)
gen levCPS_WAGES_AF =logCPS_WAGES_AF
/* edugroup 1 , males and females*/
* both
reg levCPS_WAGES_AF smpdum  AFQTreg i.year i.nkids i.mard i.sex if edugroup==1, cluster(ID79) 
outreg2 logAFQT89 using "ability_gradients_CPSwage.xls",  replace br ctitle ("LTHS-both")  
outreg2 logAFQT89 using "ability_gradients_CPSwage_latex.tex",  tex replace br ctitle ("LTHS-both")   
*scalar define slope11=_b[AFQTreg]

/* edugroup 2 , males and females*/
* both
reg levCPS_WAGES_AF smpdum  AFQTreg i.year i.nkids i.mard i.sex if edugroup==2, cluster(ID79)
outreg2 logAFQT89 using "ability_gradients_CPSwage.xls",  append br ctitle ("HSG-both")
outreg2 logAFQT89 using "ability_gradients_CPSwage_latex.tex",  tex append br ctitle ("HSG-both")
*scalar define slope21=_b[AFQTreg] 

/* edugroup 3 , males and females*/
* both
reg levCPS_WAGES_AF smpdum  AFQTreg i.year i.nkids i.mard i.sex if edugroup==3 & AFQTreg<1, cluster(ID79) 
outreg2 logAFQT89 using "ability_gradients_CPSwage.xls",  append br ctitle ("CG-both") 
outreg2 logAFQT89 using "ability_gradients_CPSwage_latex.tex",  tex append br ctitle ("CG-both")
*scalar define slope31=_b[AFQTreg]

**************************************************
*** END OF TEST REGRESSIONS **********************
**************************************************


