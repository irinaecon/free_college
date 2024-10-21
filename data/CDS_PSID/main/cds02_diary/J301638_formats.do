
label define COLD_02L  ///
       1 "Video tape or DVD"  ///
       2 "Television program"  ///
       9 "NA; refused"  ///
       0 "Inap.: Child was not watching TV; COLA_02 = 0000, 4810, 5790 or 9840"

label define COLGA_02L  ///
       1 "Selected"  ///
       9 "NA; refused"  ///
       0 "Inap.: Not selected; COLA_02 = 0000, 4810, 5790 or 9840"

label define COLGB_02L  ///
       1 "Selected"  ///
       9 "NA; refused"  ///
       0 "Inap.: Not selected; COLA_02 = 0000, 4810, 5790 or 9840"

label define COLGC_02L  ///
       1 "Selected"  ///
       9 "NA; refused"  ///
       0 "Inap.: Not selected; COLA_02 = 0000, 4810, 5790 or 9840"

label define COLGE_02L  ///
       1 "Selected"  ///
       9 "NA; refused"  ///
       0 "Inap.: Not selected; COLA_02 = 0000, 4810, 5790 or 9840"

label define COLGF_02L  ///
       1 "Selected"  ///
       9 "NA; refused"  ///
       0 "Inap.: Not selected; COLA_02 = 0000, 4810, 5790 or 9840"

label define COLHA_02L  ///
       1 "Selected"  ///
       9 "NA; refused"  ///
       0 "Inap.: Not selected; COLA_02 = 0000, 4810, 5790 or 9840"

label define COLHB_02L  ///
       1 "Selected"  ///
       9 "NA; refused"  ///
       0 "Inap.: Not selected; COLA_02 = 0000, 4810, 5790 or 9840"

label define COLHC_02L  ///
       1 "Selected"  ///
       9 "NA; refused"  ///
       0 "Inap.: Not selected; COLA_02 = 0000, 4810, 5790 or 9840"

label define COLHE_02L  ///
       1 "Selected"  ///
       9 "NA; refused"  ///
       0 "Inap.: Not selected; COLA_02 = 0000, 4810, 5790 or 9840"

label define COLHF_02L  ///
       1 "Selected"  ///
       9 "NA; refused"  ///
       0 "Inap.: Not selected; COLA_02 = 0000, 4810, 5790 or 9840"

label define DIARY_02L  ///
       0 "Weekend"  ///
       1 "Weekday"
forvalues n = 1/20 {
    label define ER33602L `n' "Individuals in the family at the time of the 2001 interview"  , modify
}
forvalues n = 51/59 {
    label define ER33602L `n' "Individuals in institutions at the time of the 2001 interview"  , modify
}
forvalues n = 71/80 {
    label define ER33602L `n' "Individuals who moved out of the FU or out of institutions and established their own households between the 1999 and 2001 interviews"  , modify
}
forvalues n = 81/89 {
    label define ER33602L `n' "Individuals who were living in 1999 but died by the time of the 2001 interview"  , modify
}
label define ER33602L        0 "Inap.:  born or moved in after the 2001 interview; from Latino sample (ER30001=7001-9308); from Immigrant 2017 sample (ER30001=4001-4851); main family nonresponse by 2001 or mover-out nonresponse by 1999 (ER33601=0)"  , modify

label define ER33603L  ///
      10 "Head in 2001; 1999 Head who was mover-out nonresponse by the time of the 2001 interview"  ///
      20 "Legal Wife in 2001; 1999 Wife who was mover-out nonresponse by the time of the 2001 interview"  ///
      22 `""Wife"--female cohabitor who has lived with Head for 12 months or more; 1999 "Wife" who was mover-out nonresponse by the time of the 2001 interview"'  ///
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
       0 "Inap.:  from Latino sample (ER30001=7001-9308); from Immigrant 2017 sample (ER30001=4001-4851); main family nonresponse by 2001 or mover-out nonresponse by 1999 (ER33601=0); born or moved in after the 2001 interview (ER33601>0 and ER33602=0)"

label define T1_02L  ///
       1 "Sunday"  ///
       2 "Monday"  ///
       3 "Tuesday"  ///
       4 "Wednesday"  ///
       5 "Thursday"  ///
       6 "Friday"  ///
       7 "Saturday"  ///
       9 "NA; refused"

label define TDREL02L  ///
       1 "Release number 1 - October 2004"  ///
       2 "Release number 2 - November 2015"

label values COLD_02    COLD_02L
label values COLGA_02   COLGA_02L
label values COLGB_02   COLGB_02L
label values COLGC_02   COLGC_02L
label values COLGE_02   COLGE_02L
label values COLGF_02   COLGF_02L
label values COLHA_02   COLHA_02L
label values COLHB_02   COLHB_02L
label values COLHC_02   COLHC_02L
label values COLHE_02   COLHE_02L
label values COLHF_02   COLHF_02L
label values DIARY_02   DIARY_02L
label values ER33602    ER33602L
label values ER33603    ER33603L
label values T1_02      T1_02L
label values TDREL02    TDREL02L
