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
label define ER33402L        0 "Inap.:  born or moved in after the 1997 interview; from Immigrant or Latino samples (ER30001=3001-3511,4001-4851,7001-9308); main family nonresponse by 1997 or mover-out nonresponse by 1996 (ER33401=0)"  , modify

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
       0 "Inap. from Immigrant Sample added in 1999, from Immigrant Sample added in 2017 or Latino samples (ER30001=3442-3511, 4001-4851, 7001-9308); main family nonresponse by 1997 or mover-out nonresponse by 1996 (ER33401=0); born or moved in after the 1997 interview (ER33401>0 and ER33402=0)"

label define PCGCHREL97L  ///
       1 "Release number 1 - April 2000"  ///
       2 "Release number 2 - March 2006"  ///
       3 "Release number 3 - May 2019"  ///
       4 "Release number 4 - March 2021"

label define PDAREL97L  ///
       1 "Release number 1 - June 2006"  ///
       2 "Release number 2 - November 2006"  ///
       3 "Release number 3 - May 2019"

label define Q13A20FL  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA/Refused"  ///
       0 "Inap.:"

label define Q13A20GL  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA/Refused"  ///
       0 "Inap.:"

label define Q1G10L  ///
       1 "PUBLIC SCHOOL"  ///
       2 "PRIVATE SCHOOL"  ///
       3 "HOME SCHOOL"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1G9 NE 1"

label define Q1G10AL  ///
       1 "YES"  ///
       5 "NO"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1G9 NE 1/ Q1G10 NE 1 AND 3"

label define Q1G11L  ///
       1 "PRIVATE/RELIGIOUS SCHOOL"  ///
       2 "PRIVATE/NON-RELIGIOUS SCHOOL"  ///
       3 "BOTH"  ///
       7 "OTHER"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1G9 NE 1/ Q1G10=9/ Q1G10A NE 1"

label define Q1G11BAL  ///
       1 "YES"  ///
       5 "NO"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1G9 NE 1/ Q1G10=9/ Q1G10A NE 1"

label values ER33402    ER33402L
label values ER33403    ER33403L
label values PCGCHREL97 PCGCHREL97L
label values PDAREL97   PDAREL97L
label values Q13A20F    Q13A20FL
label values Q13A20G    Q13A20GL
label values Q1G10      Q1G10L
label values Q1G10A     Q1G10AL
label values Q1G11      Q1G11L
label values Q1G11BA    Q1G11BAL