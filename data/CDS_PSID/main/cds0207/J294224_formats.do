
label define CHRELL  ///
       1 "Release number 1 - July 2004"  ///
       2 "Release number 2 - January 2005"  ///
       3 "Release number 3 - March 2006"  ///
       4 "Release number 4 - November 2015"
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
label define ER33902L        0 "Inap.:  from Latino sample (ER30001=7001-9308); from Immigrant 2017 sample (ER30001=4001-4851); main family nonresponse by 2007 or mover-out nonresponse by 2005 (ER33901=0)"  , modify

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
       0 "Inap.:  from Latino sample (ER30001=7001-9308); from Immigrant 2017 sample (ER30001=4001-4851); main family nonresponse by 2007 or mover-out nonresponse by 2005 (ER33901=0)"

label define PCHREL07L  ///
       1 "Release number 1 - October, 2009"  ///
       2 "Release number 2 - January, 2010"  ///
       3 "Release number 3 - June, 2013"

label define Q21B11L  ///
       1 "Public school"  ///
       2 "Private school"  ///
       3 "Attending school at home"  ///
       4 "IF VOL: Not in school"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if (Q21B4=5) or (Q21B6=14,15,98,99)"

label define Q21B12_2L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if (Q21B4=5 and Q21IwAge<7)"

label define Q21B13L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if (Q21B4=5) or (Q21B6=14,15,98,99 and Q21IwAge<7)"

label define Q21B14L  ///
       1 "Private/religious school"  ///
       2 "Private/non-religious school"  ///
       3 "Both"  ///
       7 "Other - Specify"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21B13=0,5,8,9"

label define Q21B14BL  ///
       1 "Religious private school"  ///
       2 "Non-religious private school"  ///
       3 "Time was half and half"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21B14=0,1,2,7,8,9"

label define Q21B15AL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21B13=0,5,8,9"

label define Q21B15BL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21B13=0,5,8,9"

label define Q21B15CL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21B13=0,5,8,9"

label define Q21B15DL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21B13=0,5,8,9"

label define Q21B15EL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21B13=0,5,8,9"

label define Q21B15FL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21B13=0,5,8,9"

label define Q21B15GL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21B13=0,5,8,9"

label define Q21B15HL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21B13=0,5,8,9"

label define Q21B15IL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21B13=0,5,8,9"

label define Q21B15JL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21B13=0,5,8,9"

label define Q21B15KL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21B13=0,5,8,9"

label define Q21B15LL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21B13=0,5,8,9"

label define Q21B15ML  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21B13=0,5,8,9"

label define Q21B15NL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21B13=0,5,8,9"

label define Q21B15OL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21B13=0,5,8,9"

label define Q21H32EL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21H32D=1,8,9"

label define Q31B11L  ///
       1 "Public school"  ///
       2 "Private school"  ///
       3 "Attending school at home"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  child not currently enrolled in school (Q31B4=5,8,9)"

label define Q31B12_2L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  child not currently enrolled in school (Q31B4=5,8,9)"

label define Q31B13L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  child not currently enrolled in school (Q31B4=5,8,9); child attended private school previous school year (Q31B12_2=1)"

label define Q31B14L  ///
       1 "Private/religious school"  ///
       2 "Private/non-religious school"  ///
       3 "Both"  ///
       4 "Other - specify"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  child not currently enrolled in school (Q31B4=5,8,9); child has never attended a religious or other private school (Q31B13=5,8,9)"

label define Q31B15AL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  child not currently enrolled in school (Q31B4=5,8,9); child has never attended a religious or other private school (Q31B13=5,8,9)"

label define Q31B15BL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  child not currently enrolled in school (Q31B4=5,8,9); child has never attended a religious or other private school (Q31B13=5,8,9)"

label define Q31B15CL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  child not currently enrolled in school (Q31B4=5,8,9); child has never attended a religious or other private school (Q31B13=5,8,9)"

label define Q31B15DL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  child not currently enrolled in school (Q31B4=5,8,9); child has never attended a religious or other private school (Q31B13=5,8,9)"

label define Q31B15EL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  child not currently enrolled in school (Q31B4=5,8,9); child has never attended a religious or other private school (Q31B13=5,8,9)"

label define Q31B15FL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  child not currently enrolled in school (Q31B4=5,8,9); child has never attended a religious or other private school (Q31B13=5,8,9)"

label define Q31B15GL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  child not currently enrolled in school (Q31B4=5,8,9); child has never attended a religious or other private school (Q31B13=5,8,9)"

label define Q31B15HL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  child not currently enrolled in school (Q31B4=5,8,9); child has never attended a religious or other private school (Q31B13=5,8,9)"

label define Q31B15IL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  child not currently enrolled in school (Q31B4=5,8,9); child has never attended a religious or other private school (Q31B13=5,8,9)"

label define Q31B15JL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  child not currently enrolled in school (Q31B4=5,8,9); child has never attended a religious or other private school (Q31B13=5,8,9)"

label define Q31B15KL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  child not currently enrolled in school (Q31B4=5,8,9); child has never attended a religious or other private school (Q31B13=5,8,9)"

label define Q31B15LL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  child not currently enrolled in school (Q31B4=5,8,9); child has never attended a religious or other private school (Q31B13=5,8,9)"

label define Q31B15ML  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  child not currently enrolled in school (Q31B4=5,8,9); child has never attended a religious or other private school (Q31B13=5,8,9)"

label define Q31B15NL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  child not currently enrolled in school (Q31B4=5,8,9); child has never attended a religious or other private school (Q31B13=5,8,9)"

label define Q31B15OL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  child not currently enrolled in school (Q31B4=5,8,9); child has never attended a religious or other private school (Q31B13=5,8,9)"

label define Q31H32EL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  private or out-of-state colleges will be considered (Q31H32D=1)"

label values CHREL      CHRELL
label values ER33602    ER33602L
label values ER33603    ER33603L
label values ER33902    ER33902L
label values ER33903    ER33903L
label values PCHREL07   PCHREL07L
label values Q21B11     Q21B11L
label values Q21B12_2   Q21B12_2L
label values Q21B13     Q21B13L
label values Q21B14     Q21B14L
label values Q21B14B    Q21B14BL
label values Q21B15A    Q21B15AL
label values Q21B15B    Q21B15BL
label values Q21B15C    Q21B15CL
label values Q21B15D    Q21B15DL
label values Q21B15E    Q21B15EL
label values Q21B15F    Q21B15FL
label values Q21B15G    Q21B15GL
label values Q21B15H    Q21B15HL
label values Q21B15I    Q21B15IL
label values Q21B15J    Q21B15JL
label values Q21B15K    Q21B15KL
label values Q21B15L    Q21B15LL
label values Q21B15M    Q21B15ML
label values Q21B15N    Q21B15NL
label values Q21B15O    Q21B15OL
label values Q21H32E    Q21H32EL
label values Q31B11     Q31B11L
label values Q31B12_2   Q31B12_2L
label values Q31B13     Q31B13L
label values Q31B14     Q31B14L
label values Q31B15A    Q31B15AL
label values Q31B15B    Q31B15BL
label values Q31B15C    Q31B15CL
label values Q31B15D    Q31B15DL
label values Q31B15E    Q31B15EL
label values Q31B15F    Q31B15FL
label values Q31B15G    Q31B15GL
label values Q31B15H    Q31B15HL
label values Q31B15I    Q31B15IL
label values Q31B15J    Q31B15JL
label values Q31B15K    Q31B15KL
label values Q31B15L    Q31B15LL
label values Q31B15M    Q31B15ML
label values Q31B15N    Q31B15NL
label values Q31B15O    Q31B15OL
label values Q31H32E    Q31H32EL
