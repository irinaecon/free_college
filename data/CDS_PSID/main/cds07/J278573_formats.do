
label define ER30000L  ///
       1 "Release number 1, February 2019"  ///
       2 "Release number 2, May 2019"  ///
       3 "Release number 3, August 2019"  ///
       4 "Release number 4, February 2020"
forvalues n = 1/20 {
    label define ER33902L `n' "Individuals in the family at the time of the 2007 interview"  , modify
}
forvalues n = 51/59 {
    label define ER33902L `n' "Individuals in institutions at the time of the 2007 interview"  , modify
}
forvalues n = 71/80 {
    label define ER33902L `n' "Individuals who moved out of the FU or out of institutions and established their own households between the 2005 and 2007 interviews"  , modify
}
forvalues n = 81/89 {
    label define ER33902L `n' "Individuals who were living in 2005 but died by the time of the 2007 interview"  , modify
}
label define ER33902L        0 "Inap.:  from Latino sample (ER30001=7001-9308); from Immigrant 2017 sample (ER30001=4001-4462); main family nonresponse by 2007 or mover-out nonresponse by 2005 (ER33901=0)"  , modify

label define ER33903L  ///
      10 "Head in 2007; 2005 Head who was mover-out nonresponse by the time of the 2007 interview"  ///
      20 "Legal Wife in 2007; 2005 Wife who was mover-out nonresponse by the time of the 2007 interview"  ///
      22 `""Wife"--female cohabitor who has lived with Head for 12 months or more; 2005 "Wife" who was mover-out nonresponse by the time of the 2007 interview"'  ///
      30 "Son or daughter of Head (includes adopted children but not stepchildren)"  ///
      33 "Stepson or stepdaughter of Head (children of legal Wife [code 20] who are not children of Head)"  ///
      35 `"Son or daughter of "Wife" but not Head (includes only those children of mothers whose relationship to Head is 22 but who are not children of Head)"'  ///
      37 "Son-in-law or daughter-in-law of Head (includes stepchildren-in-law)"  ///
      38 "Foster son or foster daughter, not legally adopted"  ///
      40 "Brother or sister of Head (includes step and half sisters and brothers)"  ///
      47 "Brother-in-law or sister-in-law of Head; i.e., brother or sister of legal Wife, or spouse of Head`=char(146)'s brother or sister"  ///
      48 "Brother or sister of Head`=char(146)'s cohabitor (the cohabitor is coded 22 or 88)"  ///
      50 "Father or mother of Head (includes stepparents)"  ///
      57 "Father-in-law or mother-in-law of Head (includes parents of legal wives [code 20] only)"  ///
      58 "Father or mother of Head`=char(146)'s cohabitor (the cohabitor is coded 22 or 88)"  ///
      60 "Grandson or granddaughter of Head (includes grandchildren of legal Wife [code 20] only; those of a cohabitor are coded 97)"  ///
      65 "Great-grandson or great-granddaughter of Head (includes great-grandchildren of legal Wife [code 20]; those of a cohabitor are coded 97)"  ///
      66 "Grandfather or grandmother of Head (includes stepgrandparents)"  ///
      67 "Grandfather or grandmother of legal Wife (code 20)"  ///
      68 "Great-grandfather or great-grandmother of Head"  ///
      69 "Great-grandfather or great-grandmother of legal Wife (code 20)"  ///
      70 "Nephew or niece of Head"  ///
      71 "Nephew or niece of legal Wife (code 20)"  ///
      72 "Uncle or Aunt of Head"  ///
      73 "Uncle or Aunt of legal Wife (code 20)"  ///
      74 "Cousin of Head"  ///
      75 "Cousin of legal Wife (code 20)"  ///
      83 "Children of first-year cohabitor but not of Head (the parent of this child is coded 88)"  ///
      88 "First-year cohabitor of Head"  ///
      90 "Legal husband of Head"  ///
      95 "Other relative of Head"  ///
      96 "Other relative of legal Wife (code 20)"  ///
      97 "Other relative of cohabitor (the cohabitor is code 22 or 88)"  ///
      98 "Other nonrelatives (includes homosexual partners, friends of children of the FU, etc.)"  ///
       0 "Inap.:  from Latino sample (ER30001=7001-9308); from Immigrant 2017 sample (ER30001=4001-4462); main family nonresponse by 2007 or mover-out nonresponse by 2005 (ER33901=0)"
forvalues n = 1/17 {
    label define ER33917L `n' "Highest grade or year of school completed"  , modify
}
label define ER33917L       98 "DK"  , modify
label define ER33917L       99 "NA"  , modify
label define ER33917L        0 "Inap.:  from Latino sample (ER30001=7001-9308); from Immigrant 2017 sample (ER30001=4001-4462); main family nonresponse by 2007 or mover-out nonresponse by 2005 (ER33901=0); in an institution in both 2005 and 2007 (ER33902=51-59 and ER33908=0); not a person aged 16 or older (ER33904=001-015, 999); associated with 2007 FU but actually moved out before 2006 (ER33908=5, 6, or 8 and ER33910<2006) or moved in in 2007 (ER33908=1 and ER33910=2007)"  , modify

label define ER36001L  ///
       1 "Release number 1, June 2009"  ///
       2 "Release number 2, October 2009"  ///
       3 "Release number 3, January 2012"  ///
       4 "Release number 4, December 2013"  ///
       5 "Release number 5, February 2014"  ///
       6 "Release number 6, January 2016"  ///
       7 "Release number 7, November 2017"
label define ER36020L        0 "None"  , modify
forvalues n = 1/18 {
    label define ER36020L `n' "Actual number"  , modify
}
label define ER41037L        0 "Completed no grades of school"  , modify
forvalues n = 1/16 {
    label define ER41037L `n' "Actual number"  , modify
}
label define ER41037L       17 "At least some post-graduate work"  , modify
label define ER41037L       99 "NA; DK"  , modify
forvalues n = 1/16 {
    label define ER41038L `n' "Actual number"  , modify
}
label define ER41038L       17 "At least some post-graduate work"  , modify
label define ER41038L       99 "NA; DK"  , modify
label define ER41038L        0 `"Inap.:   no wife/"wife" in FU; completed no grades of school"'  , modify

label define PCHREL07L  ///
       1 "Release number 1 - October, 2009"  ///
       2 "Release number 2 - January, 2010"  ///
       3 "Release number 3 - June, 2013"

label values ER30000    ER30000L
label values ER33902    ER33902L
label values ER33903    ER33903L
label values ER33917    ER33917L
label values ER36001    ER36001L
label values ER36020    ER36020L
label values ER41037    ER41037L
label values ER41038    ER41038L
label values PCHREL07   PCHREL07L
