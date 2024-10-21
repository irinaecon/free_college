
label define CHREL97L  ///
       1 "RELEASE NUMBER 1 - APRIL 2000"  ///
       2 "RELEASE NUMBER 2 - MARCH 2006"

label define ER10001L  ///
       1 "Release number 1 - April 1999"  ///
       2 "Release number 2 - May 1999"  ///
       3 "Release number 3- June 1999"  ///
       4 "Release number 4 - May 2008"  ///
       5 "Release number 5 - November 2013"  ///
       6 "Release number 6 - January 2016"  ///
       7 "Release number 7 - March 2016"
label define ER10012L        0 "None"  , modify
forvalues n = 1/18 {
    label define ER10012L `n' "Actual number"  , modify
}
label define ER12222L        0 "Completed no grades of school"  , modify
forvalues n = 1/16 {
    label define ER12222L `n' "Actual number"  , modify
}
label define ER12222L       17 "At least some post-graduate work"  , modify
label define ER12222L       99 "NA; DK"  , modify
forvalues n = 1/16 {
    label define ER12223L `n' "Actual number"  , modify
}
label define ER12223L       17 "At least some post-graduate work"  , modify
label define ER12223L       99 "NA; DK"  , modify
label define ER12223L        0 `"Inap.:   no wife/"wife" in FU; completed no grades of school"'  , modify

label define ER30000L  ///
       1 "Release number 1, February 2019"  ///
       2 "Release number 2, May 2019"  ///
       3 "Release number 3, August 2019"  ///
       4 "Release number 4, February 2020"
forvalues n = 1/20 {
    label define ER33402L `n' "Individuals in the family at the time of the 1997 interview"  , modify
}
forvalues n = 51/59 {
    label define ER33402L `n' "Individuals in institutions at the time of the 1997 interview"  , modify
}
forvalues n = 71/80 {
    label define ER33402L `n' "Individuals who moved out of the FU or out of institutions and established their own households between the 1996 and 1997 interviews"  , modify
}
forvalues n = 81/89 {
    label define ER33402L `n' "Individuals who were living in 1996 but died by the time of the 1997 interview"  , modify
}
label define ER33402L        0 "Inap.:  born or moved in after the 1997 interview; from Immigrant or Latino samples (ER30001=3001-3511,4001-4462,7001-9308); main family nonresponse by 1997 or mover-out nonresponse by 1996 (ER33401=0)"  , modify

label define ER33403L  ///
      10 "Head in 1997; 1996 Head who was mover-out nonresponse by the time of the 1997 interview"  ///
      20 "Legal Wife in 1997; 1996 Wife who was mover-out nonresponse by the time of the 1997 interview"  ///
      22 `""Wife"--female cohabitor who has lived with Head for 12 months or more or who was present in the 1996 family, since consecutive interviews may be taken less or more than twelve months apart; 1996 "Wife" who was mover-out nonresponse by the time of the 1997 interview"'  ///
      30 "Son or daughter of Head (includes adopted children but not stepchildren)"  ///
      33 "Stepson or stepdaughter of Head (children of legal Wife (code 20) who are not children of Head)"  ///
      35 `"Son or daughter of "Wife" but not Head (includes only those children of mothers whose relationship to Head is 22 but who are not children of Head)"'  ///
      37 "Son-in-law or daughter-in-law of Head (includes stepchildren-in-law)"  ///
      38 "Foster son or foster daughter, not legally adopted"  ///
      40 "Brother or sister of Head (includes step and half sisters and brothers)"  ///
      47 "Brother-in-law or sister-in-law of Head; i.e., brother or sister of legal Wife, or spouse of Head`=char(146)'s brother or sister."  ///
      48 "Brother or sister of Head`=char(146)'s cohabitor (the cohabitor is coded 22 or 88)"  ///
      50 "Father or mother of Head (includes stepparents)"  ///
      57 "Father-in-law or mother-in-law of Head (includes parents of legal wives (code 20) only)"  ///
      58 "Father or mother of Head`=char(146)'s cohabitor (the cohabitor is coded 22 or 88)"  ///
      60 "Grandson or granddaughter of Head (includes grandchildren of legal Wife (code 20), but those of a cohabitor are coded 97)"  ///
      65 "Great-grandson or great-granddaughter of Head (includes great-grandchildren of legal Wife (code 20), but those of a cohabitor are coded 97)"  ///
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
       0 "Inap. from Immigrant Sample added in 1999, from Immigrant Sample added in 2017 or Latino samples (ER30001=3442-3511, 4001-4462, 7001-9308); main family nonresponse by 1997 or mover-out nonresponse by 1996 (ER33401=0); born or moved in after the 1997 interview (ER33401>0 and ER33402=0)"
forvalues n = 1/17 {
    label define ER33415L `n' "Highest grade or year of school completed"  , modify
}
label define ER33415L       98 "DK"  , modify
label define ER33415L       99 "NA"  , modify
label define ER33415L        0 "Inap.:  from Immigrant 99 recontact, Immigrant 17 or Latino samples (ER30001=3442-3511,4001-4462,7001-9308); main family nonresponse by 1997 or mover-out nonresponse by 1996 (ER33401=0); in an institution in both 1996 and 1997 (ER33402=51-59 and ER33408=0); born or moved in after the 1997 interview (ER33401>0 and ER33402=0); not a person aged 16 or older (ER33404=001-015)"  , modify

label define Q1B17L  ///
       1 "CHILD IS UNDER 3 YEARS"  ///
       2 "CHILD IS AGE 3 - 5"  ///
       3 "CHILD IS AGE 6 - 9"  ///
       4 "CHILD IS AGE 10 OR OLDER"

label values CHREL97    CHREL97L
label values ER10001    ER10001L
label values ER10012    ER10012L
label values ER12222    ER12222L
label values ER12223    ER12223L
label values ER30000    ER30000L
label values ER33402    ER33402L
label values ER33403    ER33403L
label values ER33415    ER33415L
label values Q1B17      Q1B17L
