
label define COLDL  ///
       1 "Video tape"  ///
       2 "Television program"  ///
       9 "NA; refused"  ///
       0 "Inap.: Child was not watching TV"

label define COLG_AL  ///
       1 "Yes"  ///
       9 "NA; refused"  ///
       0 "Inap.: Not selected; Activity code is 481 or 579 or 984"

label define COLG_BL  ///
       1 "Yes"  ///
       9 "NA; refused"  ///
       0 "Inap.: Not selected; Activity code is 481 or 579 or 984"

label define COLG_CL  ///
       1 "Yes"  ///
       9 "NA; refused"  ///
       0 "Inap.: Not selected; Activity code is 481 or 579 or 984"

label define COLG_EL  ///
       1 "Yes"  ///
       9 "NA; refused"  ///
       0 "Inap.: Not selected; Activity code is 481 or 579 or 984"

label define COLG_FL  ///
       1 "Yes"  ///
       9 "NA; refused"  ///
       0 "Inap.: Not selected; Activity code is 481 or 579 or 984"

label define COLH_AL  ///
       1 "Yes"  ///
       9 "NA; refused"  ///
       0 "Inap.: Not selected; Activity code is 481 or 579 or 984"

label define COLH_BL  ///
       1 "Yes"  ///
       9 "NA; refused"  ///
       0 "Inap.: Not selected; Activity code is 481 or 579 or 984"

label define COLH_CL  ///
       1 "Yes"  ///
       9 "NA; refused"  ///
       0 "Inap.: Not selected; Activity code is 481 or 579 or 984"

label define COLH_EL  ///
       1 "Yes"  ///
       9 "NA; refused"  ///
       0 "Inap.: Not selected; Activity code is 481 or 579 or 984"

label define COLH_FL  ///
       1 "Yes"  ///
       9 "NA; refused"  ///
       0 "Inap.: Not selected; Activity code is 481 or 579 or 984"
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

label define T1L  ///
       1 "Monday"  ///
       2 "Tuesday"  ///
       3 "Wednesday"  ///
       4 "Thursday"  ///
       5 "Friday"  ///
       6 "Saturday"  ///
       7 "Sunday"

label define TDREL97L  ///
       1 "Release number 1 - August 2010"  ///
       2 "Release number 2 - May 2019"

label define WDAYWENDL  ///
       0 "Weekend"  ///
       1 "Weekday"

label values COLD       COLDL
label values COLG_A     COLG_AL
label values COLG_B     COLG_BL
label values COLG_C     COLG_CL
label values COLG_E     COLG_EL
label values COLG_F     COLG_FL
label values COLH_A     COLH_AL
label values COLH_B     COLH_BL
label values COLH_C     COLH_CL
label values COLH_E     COLH_EL
label values COLH_F     COLH_FL
label values ER33402    ER33402L
label values ER33403    ER33403L
label values T1         T1L
label values TDREL97    TDREL97L
label values WDAYWEND   WDAYWENDL
