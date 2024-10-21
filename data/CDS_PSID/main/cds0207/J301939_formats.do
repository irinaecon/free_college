
label define CHLDRELL  ///
       1 "Release number 1 - July 2004"  ///
       2 "Release number 2 - January 2005"  ///
       3 "Release number 3 - October 2005"  ///
       4 "Release number 4 - June 2010"  ///
       5 "Release number 5 - November 2015"  ///
       6 "Release number 6 - May 2019"

label define CHLREL07L  ///
       1 "Release number 1 - October, 2009"  ///
       2 "Release number 2 - May, 2019"

label define CHRELL  ///
       1 "Release number 1 - July 2004"  ///
       2 "Release number 2 - January 2005"  ///
       3 "Release number 3 - March 2006"  ///
       4 "Release number 4 - November 2015"

label define DEMREL02L  ///
       1 "Release number 1 - August 2004"  ///
       2 "Release number 2 - February 2005"  ///
       3 "Release number 3 - October 2005"  ///
       4 "Release number 4 - March 2006"  ///
       5 "Release number 5 - June 2013"  ///
       6 "Release number 6 - November 2015"

label define DEMREL07L  ///
       1 "Release number 1 - October, 2009"  ///
       2 "Release number 2 - June, 2013"

label define ER30000L  ///
       1 "Release number 1, March 2021"  ///
       2 "Release number 2, April 2021"
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

label define Q21B11A2L  ///
       1 "Per hour"  ///
       2 "Per day"  ///
       3 "Per week"  ///
       4 "Every two weeks"  ///
       5 "Every month"  ///
       6 "For the year"  ///
       7 "Other - Specify"  ///
       8 "Per term or semester"  ///
      98 "DK"  ///
      99 "NA; refused"  ///
       0 "Inap.: if (Q21B11A1=0) or (Q21B11=0,1,3,4,8,9)"

label define Q21B12A2L  ///
       1 "Per hour"  ///
       2 "Per day"  ///
       3 "Per week"  ///
       4 "Every two weeks"  ///
       5 "Every month"  ///
       6 "For the year"  ///
       7 "Other - Specify"  ///
       8 "Per term or semester"  ///
      98 "DK"  ///
      99 "NA; refused"  ///
       0 "Inap.: if Q21B12A1=0"

label define Q21C12L  ///
       1 "Relative under 13 years"  ///
       2 "Relative 13 or older"  ///
       3 "Non-relative in child`=char(146)'s home (sitter)"  ///
       5 "Care in a non-relative`=char(146)'s home (family daycare provider)"  ///
       6 "Head Start program"  ///
       7 "Pre-kindergarten program, nursery school, pre-school or child care center"  ///
       8 "Before or after-school program"  ///
       9 "Extra-curricular activities"  ///
      10 "Work"  ///
      97 "Other type of child care"  ///
      98 "DK"  ///
      99 "NA; refused"  ///
       0 "Inap.: if (All Q21C10A-Q21C10P=0,5,8,9) or (only one of Q21C10A-Q21C10P=1) or (Q21C10M=1) or (Q21C10N=1)"

label define Q21C14BL  ///
       1 "Weeks"  ///
       2 "Months"  ///
       3 "Years"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21C12=0,98,99"

label define Q21C15BL  ///
       1 "Per hour"  ///
       2 "Daily"  ///
       3 "Weekly"  ///
       4 "Every 2 weeks"  ///
       5 "Every month"  ///
       6 "Every year"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21C15A=0,9999998,9999999"

label define Q21C18L  ///
       1 "Relative under 13 years"  ///
       2 "Relative 13 or older"  ///
       3 "Non-relative in child`=char(146)'s home (sitter)"  ///
       5 "Care in a non-relative`=char(146)'s home (family daycare provider)"  ///
       6 "Head Start program"  ///
       7 "Pre-kindergarten program, nursery school, pre-school or child care center"  ///
       8 "Before or after-school program"  ///
       9 "Extra-curricular activities"  ///
      10 "Work"  ///
      97 "Other type of child care"  ///
      98 "DK"  ///
      99 "NA; refused"  ///
       0 "Inap.: if Q21C12=0,98,99"

label define Q21C20BL  ///
       1 "Weeks"  ///
       2 "Months"  ///
       3 "Years"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21C20A=0,99998,99999"

label define Q21C21BL  ///
       1 "Per hour"  ///
       2 "Daily"  ///
       3 "Weekly"  ///
       4 "Every 2 weeks"  ///
       5 "Every month"  ///
       6 "Every year"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21C21A=0,9999998,9999999"

label define Q21C24L  ///
       1 "Relative under 13 years"  ///
       2 "Relative 13 or older"  ///
       3 "Non-relative in child`=char(146)'s home (sitter)"  ///
       5 "Care in a non-relative`=char(146)'s home (family daycare provider)"  ///
       6 "Head Start program"  ///
       7 "Pre-kindergarten program, nursery school, pre-school or child care center"  ///
       8 "Before or after-school program"  ///
       9 "Extra-curricular activities"  ///
      10 "Work"  ///
      97 "Other type of child care"  ///
      98 "DK"  ///
      99 "NA; refused"  ///
       0 "Inap.: if (All Q21C22A-Q21C22P=0,5,8,9) or (only one of Q21C22A-Q21C22P=1) or (Q21C22M=1) or (Q21C22N=1)"

label define Q21C26BL  ///
       1 "Weeks"  ///
       2 "Months"  ///
       3 "Years"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21C26A=0,99998,99999"

label define Q21C27BL  ///
       1 "Per hour"  ///
       2 "Daily"  ///
       3 "Weekly"  ///
       4 "Every 2 weeks"  ///
       5 "Every month"  ///
       6 "Every year"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21C27A=0,9999998,9999999"

label define Q21C30L  ///
       1 "Relative under 13 years"  ///
       2 "Relative 13 or older"  ///
       3 "Non-relative in child`=char(146)'s home (sitter)"  ///
       5 "Care in a non-relative`=char(146)'s home (family daycare provider)"  ///
       6 "Head Start program"  ///
       7 "Pre-kindergarten program, nursery school, pre-school or child care center"  ///
       8 "Before or after-school program"  ///
       9 "Extra-curricular activities"  ///
      10 "Work"  ///
      11 "Overnight camp"  ///
      12 "Day camp"  ///
      97 "Other type of child care"  ///
      98 "DK"  ///
      99 "NA; refused"  ///
       0 "Inap.: if (All Q21C28A-Q21C28R=0,5,8,9) or (only one of Q21C28A-Q21C28R=1) or (Q21C28P=1) or (Q21C28Q=1)"

label define Q21C32BL  ///
       1 "Days"  ///
       2 "Weeks"  ///
       3 "Months"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21C32A=0,99998,99999"

label define Q21C33BL  ///
       1 "Per hour"  ///
       2 "Daily"  ///
       3 "Weekly"  ///
       4 "Every 2 weeks"  ///
       5 "Every month"  ///
       6 "Every year"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21C33A= 0,9999998,9999999"

label define Q21C35L  ///
       1 "Relative under 13 years"  ///
       2 "Relative 13 or older"  ///
       3 "Non-relative in child`=char(146)'s home (sitter)"  ///
       5 "Care in a non-relative`=char(146)'s home (family daycare provider)"  ///
       6 "Head Start program"  ///
       7 "Pre-kindergarten program, nursery school, pre-school or child care center"  ///
       8 "Before or after-school program"  ///
       9 "Extra-curricular activities"  ///
      10 "Work"  ///
      11 "Overnight camp"  ///
      12 "Day camp"  ///
      97 "Other type of child care"  ///
      98 "DK"  ///
      99 "NA; refused"  ///
       0 "Inap.: if Q21C30=0,98,99"

label define Q21C37BL  ///
       1 "Days"  ///
       2 "Weeks"  ///
       3 "Months"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21C37B=0,99998,99999"

label define Q21C39BL  ///
       1 "Per hour"  ///
       2 "Daily"  ///
       3 "Weekly"  ///
       4 "Every 2 weeks"  ///
       5 "Every month"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21C39A=0,9999998,9999999"

label define Q21C3_1L  ///
       1 "Relative in child`=char(146)'s home"  ///
       2 "Non-relative in child`=char(146)'s home (sitter)"  ///
       3 "Care in a relative`=char(146)'s home"  ///
       4 "Care in a non-relative`=char(146)'s home (family day care provider)"  ///
       5 "Head start program"  ///
       6 "Pre-kindergarten program, nursery school, preschool, or child care center"  ///
       7 "Before or after-school program"  ///
       8 "Child cares for self alone"  ///
      11 "None of the above, one parent always cares for child"  ///
      97 "Other type of child care"  ///
      98 "DK"  ///
      99 "NA; refused"  ///
       0 "Inap.: if Q21C2C=0,2,8,9"

label define Q21C3_2L  ///
       1 "Relative in child`=char(146)'s home"  ///
       2 "Non-relative in child`=char(146)'s home (sitter)"  ///
       3 "Care in a relative`=char(146)'s home"  ///
       4 "Care in a non-relative`=char(146)'s home (family day care provider)"  ///
       5 "Head start program"  ///
       6 "Pre-kindergarten program, nursery school, preschool, or child care center"  ///
       7 "Before or after-school program"  ///
       8 "Child cares for self alone"  ///
      11 "None of the above, one parent always cares for child"  ///
      97 "Other type of child care"  ///
      98 "DK"  ///
      99 "NA; refused"  ///
       0 "Inap.: if Q21C9_1=0,5,8,9"

label define Q21C3_3L  ///
       1 "Relative in child`=char(146)'s home"  ///
       2 "Non-relative in child`=char(146)'s home (sitter)"  ///
       3 "Care in a relative`=char(146)'s home"  ///
       4 "Care in a non-relative`=char(146)'s home (family day care provider)"  ///
       5 "Head start program"  ///
       6 "Pre-kindergarten program, nursery school, preschool, or child care center"  ///
       7 "Before or after-school program"  ///
       8 "Child cares for self alone"  ///
      11 "None of the above, one parent always cares for child"  ///
      97 "Other type of child care"  ///
      98 "DK"  ///
      99 "NA; refused"  ///
       0 "Inap.: if Q21C9_2=0,5,8,9"

label define Q21C3_4L  ///
       1 "Relative in child`=char(146)'s home"  ///
       2 "Non-relative in child`=char(146)'s home (sitter)"  ///
       3 "Care in a relative`=char(146)'s home"  ///
       4 "Care in a non-relative`=char(146)'s home (family day care provider)"  ///
       5 "Head start program"  ///
       6 "Pre-kindergarten program, nursery school, preschool, or child care center"  ///
       7 "Before or after-school program"  ///
       8 "Child cares for self alone"  ///
      11 "None of the above, one parent always cares for child"  ///
      97 "Other type of child care"  ///
      98 "DK"  ///
      99 "NA; refused"  ///
       0 "Inap.: if Q21C9_3=0,5,8,9"

label define Q21C4B_1L  ///
       1 "Weeks"  ///
       2 "Months"  ///
       3 "Years"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Q21C4a_1=0,99998,99999"

label define Q21C4B_2L  ///
       1 "Weeks"  ///
       2 "Months"  ///
       3 "Years"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Q21C4a_2=0,99998,99999"

label define Q21C4B_3L  ///
       1 "Weeks"  ///
       2 "Months"  ///
       3 "Years"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Q21C4a_3=0,99998,99999"

label define Q21C4B_4L  ///
       1 "Weeks"  ///
       2 "Months"  ///
       3 "Years"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Q21C4a_4=0,99998,99999"
forvalues n = 1/7 {
    label define Q21C5_1L `n' "#Days"  , modify
}
label define Q21C5_1L        8 "DK"  , modify
label define Q21C5_1L        9 "NA; refused"  , modify
label define Q21C5_1L        0 "Inap.: if Q21C3_1=0,11,98,99"  , modify
forvalues n = 1/7 {
    label define Q21C5_2L `n' "#Days"  , modify
}
label define Q21C5_2L        8 "DK"  , modify
label define Q21C5_2L        9 "NA; refused"  , modify
label define Q21C5_2L        0 "Inap.: if Q21C3_2=0,11,98,99"  , modify
forvalues n = 1/7 {
    label define Q21C5_3L `n' "#Days"  , modify
}
label define Q21C5_3L        8 "DK"  , modify
label define Q21C5_3L        9 "NA; refused"  , modify
label define Q21C5_3L        0 "Inap.: if Q21C3_3=0,11,98,99"  , modify
forvalues n = 1/7 {
    label define Q21C5_4L `n' "#Days"  , modify
}
label define Q21C5_4L        8 "DK"  , modify
label define Q21C5_4L        9 "NA; refused"  , modify
label define Q21C5_4L        0 "Inap.: if Q21C3_4=0,11,98,99"  , modify

label define Q21C7B_1L  ///
       1 "Per hour"  ///
       2 "Per day"  ///
       3 "Per week"  ///
       4 "Every 2 weeks"  ///
       5 "Every month"  ///
       6 "Every year"  ///
       7 "Other"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21C7A_1=0,9999998,9999999"

label define Q21C7B_2L  ///
       1 "Per hour"  ///
       2 "Per day"  ///
       3 "Per week"  ///
       4 "Every 2 weeks"  ///
       5 "Every month"  ///
       6 "Every year"  ///
       7 "Other"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21C7A_2=0,9999998,9999999"

label define Q21C7B_3L  ///
       1 "Per hour"  ///
       2 "Per day"  ///
       3 "Per week"  ///
       4 "Every 2 weeks"  ///
       5 "Every month"  ///
       6 "Every year"  ///
       7 "Other"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21C7A_3=0,9999998,9999999"

label define Q21C7B_4L  ///
       1 "Per hour"  ///
       2 "Per day"  ///
       3 "Per week"  ///
       4 "Every 2 weeks"  ///
       5 "Every month"  ///
       6 "Every year"  ///
       7 "Other"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21C7A_4=0,9999998,9999999"

label define Q21C8B_1L  ///
       1 "Weeks"  ///
       2 "Months"  ///
       3 "Years"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21C8a_1=0,99998,99999"

label define Q21C8B_2L  ///
       1 "Weeks"  ///
       2 "Months"  ///
       3 "Years"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21C8a_2=0,99998,99999"

label define Q21C8B_3L  ///
       1 "Weeks"  ///
       2 "Months"  ///
       3 "Years"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21C8a_3=0,99998,99999"

label define Q21C8B_4L  ///
       1 "Weeks"  ///
       2 "Months"  ///
       3 "Years"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21C8a_4=0,99998,99999"

label define Q21C9B_1L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21C3_1=0,11,98,99"

label define Q21C9B_2L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21C3_2=0,11,98,99"

label define Q21C9B_3L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21C3_3=0,11,98,99"

label define Q21C9B_4L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.: if Q21C3_4=0,11,98,99"

label define Q21H23CL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"

label define Q21H32L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"

label define Q31B11A2L  ///
       1 "Per hour"  ///
       2 "Per day"  ///
       3 "Per week"  ///
       4 "Every two weeks"  ///
       5 "Every month"  ///
       6 "For the year"  ///
       7 "Other - Specify"  ///
       8 "Per term or semester"  ///
      98 "DK"  ///
      99 "NA; refused"  ///
       0 "Inap.:  did not pay anything for private school; child not currently enrolled in school (Q31B4=5,8,9); child attends public school or is schooled at home or NA, DK, or RF whether child is attending a public school, a private school, or is attending school at home (Q31B11=1,3,8,9)"

label define Q31B12A2L  ///
       1 "Per hour"  ///
       2 "Per day"  ///
       3 "Per week"  ///
       4 "Every two weeks"  ///
       5 "Every month"  ///
       6 "For the year"  ///
       7 "Other - Specify"  ///
       8 "Per term or semester"  ///
      98 "DK"  ///
      99 "NA; refused"  ///
       0 "Inap.:  did not pay anything for private school previous school year; child not currently enrolled in school (Q31B4=5,8,9); child did not attend private school previous school year (Q31B12_2=5,8,9)"

label define Q31C12L  ///
       1 "Relative under 13 in the child`=char(146)'s home"  ///
       2 "Relative 13 or older in the child`=char(146)'s home"  ///
       3 "Non-relative in child`=char(146)'s home (sitter)"  ///
       4 "Care in a relative`=char(146)'s home"  ///
       5 "Care in a non-relative`=char(146)'s home (family daycare provider)"  ///
       8 "Before or after-school program"  ///
       9 "Other type of child care"  ///
      98 "DK"  ///
      99 "NA; refused"  ///
       0 "Inap:  NA, DK, or RF type of childcare arrangement used on weekdays (Q31C10A-Q31C10R=8,9); only childcare arrangement used on weekdays is: one parent always cares for child, child always cares for self, or parent cares for child part-time and child cares for self part-time (Q31C10M=1 or Q31C10N=1 or Q31C10Q=1)"

label define Q31C14BL  ///
       1 "Years"  ///
       2 "Months"  ///
       3 "Weeks"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap:  NA, DK, or RF type of childcare arrangement used on weekdays (Q31C10A-Q31C10R=8,9); only childcare arrangement used on weekdays is: one parent always cares for child, child always cares for self, or parent cares for child part-time and child cares for self part-time (Q31C10M=1 or Q31C10N=1 or Q31C10Q=1); NA, DK, RF which weekday arrangement used the most hours each week (Q31C12=98,99)"

label define Q31C15BL  ///
       1 "Per hour"  ///
       2 "Daily"  ///
       3 "Weekly"  ///
       4 "Every 2 weeks"  ///
       5 "Every month"  ///
       6 "Every year"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap:  NA, DK, or RF type of childcare arrangement used on weekdays (Q31C10A-Q31C10R=8,9); only childcare arrangement used on weekdays is: one parent always cares for child, child always cares for self, or parent cares for child part-time and child cares for self part-time (Q31C10M=1 or Q31C10N=1 or Q31C10Q=1); NA, DK, RF which weekday arrangement used the most hours each week (Q31C12=98,99); amount paid for child care program/arrangement on weekdays is zero (Q31C15A=0)"

label define Q31C18L  ///
       1 "Relative under 13 in the child`=char(146)'s home"  ///
       2 "Relative 13 or older in the child`=char(146)'s home"  ///
       3 "Non-relative in child`=char(146)'s home (sitter)"  ///
       4 "Care in a relative`=char(146)'s home"  ///
       5 "Care in a non-relative`=char(146)'s home (family daycare provider)"  ///
       8 "Before or after-school program"  ///
       9 "Other type of child care"  ///
      98 "DK"  ///
      99 "NA; refused"  ///
       0 "Inap:  only one type of childcare arrangement used on weekdays (only one of the variables Q31C10A-Q31C10R=1); NA, DK, or RF type of childcare arrangement used on weekdays (Q31C10A-Q31C10R=8,9); only childcare arrangement used on weekdays is: one parent always cares for child, child always cares for self, or parent cares for child part-time and child cares for self part-time (Q31C10M=1 or Q31C10N=1 or Q31C10Q=1)"

label define Q31C20BL  ///
       1 "Years"  ///
       2 "Months"  ///
       3 "Weeks"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap:  only one type of childcare arrangement used on weekdays (only one of the variables Q31C10A-Q31C10R=1); NA, DK, or RF type of childcare arrangement used on weekdays (Q31C10A-Q31C10R=8,9); only childcare arrangement used on weekdays is: one parent always cares for child, child always cares for self, or parent cares for child part-time and child cares for self part-time (Q31C10M=1 or Q31C10N=1 or Q31C10Q=1); NA, DK, or RF childcare arrangement used second most frequently on weekdays (Q31C18=98,99)"

label define Q31C21BL  ///
       1 "Per hour"  ///
       2 "Daily"  ///
       3 "Weekly"  ///
       4 "Every 2 weeks"  ///
       5 "Every month"  ///
       6 "Every year"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap:  only one type of childcare arrangement used on weekdays (only one of the variables Q31C10A-Q31C10R=1); NA, DK, or RF type of childcare arrangement used on weekdays (Q31C10A-Q31C10R=8,9); only childcare arrangement used on weekdays is: one parent always cares for child, child always cares for self, or parent cares for child part-time and child cares for self part-time (Q31C10M=1 or Q31C10N=1 or Q31C10Q=1); NA, DK, or RF childcare arrangement used second most frequently on weekdays (Q31C18=98,99); amount paid for child care program/arrangement used second most frequently on weekdays is zero (Q31C21A=0)"

label define Q31C24L  ///
       1 "Relative under 13 years in child`=char(146)'s home"  ///
       2 "Relative 13 or older in child`=char(146)'s home"  ///
       3 "Non-relative in child`=char(146)'s home (sitter)"  ///
       4 "Care in a relative`=char(146)'s home"  ///
       5 "Care in a non-relative`=char(146)'s home (family daycare provider)"  ///
       8 "Before or after-school program"  ///
       9 "Other type of child care"  ///
      98 "DK"  ///
      99 "NA; refused"  ///
       0 "Inap:  NA, DK, or RF type of childcare arrangement used on weekends (Q31C22A-Q31C22R=8,9); only childcare arrangement used on weekdays is: one parent always cares for child, child always cares for self, or parent cares for child part-time and child cares for self part-time (Q31C22M=1 or Q31C22N=1 or Q31C22Q=1)"

label define Q31C26BL  ///
       1 "Years"  ///
       2 "Months"  ///
       3 "Weeks"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap:  NA, DK, or RF type of childcare arrangement used on weekends (Q31C22A-Q31C22R=8,9); only childcare arrangement used on weekdays is: one parent always cares for child, child always cares for self, or parent cares for child part-time and child cares for self part-time (Q31C22M=1 or Q31C22N=1 or Q31C22Q=1); NA, DK, or RF which child care arrangement used most hours each weekend (Q31C24=98,99)"

label define Q31C27BL  ///
       1 "Per hour"  ///
       2 "Daily"  ///
       3 "Weekly"  ///
       4 "Every 2 weeks"  ///
       5 "Every month"  ///
       6 "Every year"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap:  NA, DK, or RF type of childcare arrangement used on weekends (Q31C22A-Q31C22R=8,9); only childcare arrangement used on weekdays is: one parent always cares for child, child always cares for self, or parent cares for child part-time and child cares for self part-time (Q31C22M=1 or Q31C22N=1 or Q31C22Q=1); NA, DK, or RF which child care arrangement used most hours each weekend (Q31C24=98,99); amount paid for child care program/arrangement used/used most frequently on weekdends is zero (Q31C27A=0)"

label define Q31C30L  ///
       1 "Relative under 13 years in child`=char(146)'s home"  ///
       2 "Relative 13 or older in child`=char(146)'s home"  ///
       3 "Non-relative in child`=char(146)'s home (sitter)"  ///
       4 "Care in a relative`=char(146)'s home"  ///
       5 "Care in a non-relative`=char(146)'s home (family daycare provider)"  ///
       8 "Before or after-school program"  ///
       9 "Overnight camp"  ///
      10 "Day camp"  ///
      11 "Other type of child care"  ///
      98 "DK"  ///
      99 "NA; refused"  ///
       0 "Inap:  NA, DK, or RF type of childcare arrangement used last summer (Q31C28A-Q31C28T=8,9); only childcare arrangement used on weekdays is: one parent always cares for child, child always cares for self, or parent cares for child part-time and child cares for self part-time (Q31C28P=1 or Q31C28Q=1 or Q31C28S=1)"

label define Q31C32BL  ///
       1 "Years"  ///
       2 "Months"  ///
       3 "Weeks"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap:  NA, DK, or RF type of childcare arrangement used last summer (Q31C28A-Q31C28T=8,9); only childcare arrangement used on weekdays is: one parent always cares for child, child always cares for self, or parent cares for child part-time and child cares for self part-time (Q31C28P=1 or Q31C28Q=1 or Q31C28S=1); NA, DK, or RF type of summer care used the most hours each week (Q31C30=98,99)"

label define Q31C33BL  ///
       1 "Per hour"  ///
       2 "Daily"  ///
       3 "Weekly"  ///
       4 "Every 2 weeks"  ///
       5 "Every month"  ///
       6 "Every year"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap:  NA, DK, or RF type of childcare arrangement used last summer (Q31C28A-Q31C28T=8,9); only childcare arrangement used on weekdays is: one parent always cares for child, child always cares for self, or parent cares for child part-time and child cares for self part-time (Q31C28P=1 or Q31C28Q=1 or Q31C28S=1); NA, DK, or RF type of summer care used the most hours each week (Q31C30=98,99); amount paid for child care program/arrangement used/used most often last summer was zero (Q31C33A=0)"

label define Q31C35L  ///
       1 "Relative under 13 years"  ///
       2 "Relative 13 or older"  ///
       3 "Non-relative in child`=char(146)'s home (sitter)"  ///
       4 "Care in a relative`=char(146)'s home"  ///
       5 "Care in a non-relative`=char(146)'s home (family daycare provider)"  ///
       8 "Before or after-school program"  ///
       9 "Overnight camp"  ///
      10 "Day camp"  ///
      11 "Other type of child care"  ///
      98 "DK"  ///
      99 "NA; refused"  ///
       0 "Inap:  only one type of childcare arrangement used last summer (only one of the variables Q31C28A-Q31C28T=1); NA, DK, or RF type of childcare arrangement used last summer (Q31C28A-Q31C28T=8,9); only childcare arrangement used on weekdays is: one parent always cares for child, child always cares for self, or parent cares for child part-time and child cares for self part-time (Q31C28P=1 or Q31C28Q=1 or Q31C28S=1)"

label define Q31C37BL  ///
       1 "Years"  ///
       2 "Months"  ///
       3 "Weeks"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap:  only one type of childcare arrangement used last summer (only one of the variables Q31C28A-Q31C28T=1); NA, DK, or RF type of childcare arrangement used last summer (Q31C28A-Q31C28T=8,9); only childcare arrangement used on weekdays is: one parent always cares for child, child always cares for self, or parent cares for child part-time and child cares for self part-time (Q31C28P=1 or Q31C28Q=1 or Q31C28S=1); NA, DK, or RF type of childcare arrangement used second most frequently last summer (Q31C35=98,99)"

label define Q31C39BL  ///
       1 "Per hour"  ///
       2 "Daily"  ///
       3 "Weekly"  ///
       4 "Every 2 weeks"  ///
       5 "Every month"  ///
       6 "Every year"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap:  only one type of childcare arrangement used last summer (only one of the variables Q31C28A-Q31C28T=1); NA, DK, or RF type of childcare arrangement used last summer (Q31C28A-Q31C28T=8,9); only childcare arrangement used on weekdays is: one parent always cares for child, child always cares for self, or parent cares for child part-time and child cares for self part-time (Q31C28P=1 or Q31C28Q=1 or Q31C28S=1); NA, DK, or RF type of childcare arrangement used second most frequently last summer (Q31C35=98,99); amount paid for child care used second most frequently last summer was zero (Q31C39A=0)"

label define Q31H23CL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"

label define Q31H32L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"

label define Q31H32F1L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"

label define Q31H32F2L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"

label define Q31H32F3L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"

label define Q31H32F4L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"

label define Q31H32F5L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"

label define Q31H32F6L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"

label define Q31H32F7L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"

label define Q31H32F8L  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; refused"

label define Q31H32GL  ///
       1 "Yes"  ///
       5 "No"  ///
       8 "DK"  ///
       9 "NA; Refused"

label values CHLDREL    CHLDRELL
label values CHLREL07   CHLREL07L
label values CHREL      CHRELL
label values DEMREL02   DEMREL02L
label values DEMREL07   DEMREL07L
label values ER30000    ER30000L
label values ER33602    ER33602L
label values ER33603    ER33603L
label values ER33902    ER33902L
label values ER33903    ER33903L
label values PCHREL07   PCHREL07L
label values Q21B11A2   Q21B11A2L
label values Q21B12A2   Q21B12A2L
label values Q21C12     Q21C12L
label values Q21C14B    Q21C14BL
label values Q21C15B    Q21C15BL
label values Q21C18     Q21C18L
label values Q21C20B    Q21C20BL
label values Q21C21B    Q21C21BL
label values Q21C24     Q21C24L
label values Q21C26B    Q21C26BL
label values Q21C27B    Q21C27BL
label values Q21C30     Q21C30L
label values Q21C32B    Q21C32BL
label values Q21C33B    Q21C33BL
label values Q21C35     Q21C35L
label values Q21C37B    Q21C37BL
label values Q21C39B    Q21C39BL
label values Q21C3_1    Q21C3_1L
label values Q21C3_2    Q21C3_2L
label values Q21C3_3    Q21C3_3L
label values Q21C3_4    Q21C3_4L
label values Q21C4B_1   Q21C4B_1L
label values Q21C4B_2   Q21C4B_2L
label values Q21C4B_3   Q21C4B_3L
label values Q21C4B_4   Q21C4B_4L
label values Q21C5_1    Q21C5_1L
label values Q21C5_2    Q21C5_2L
label values Q21C5_3    Q21C5_3L
label values Q21C5_4    Q21C5_4L
label values Q21C7B_1   Q21C7B_1L
label values Q21C7B_2   Q21C7B_2L
label values Q21C7B_3   Q21C7B_3L
label values Q21C7B_4   Q21C7B_4L
label values Q21C8B_1   Q21C8B_1L
label values Q21C8B_2   Q21C8B_2L
label values Q21C8B_3   Q21C8B_3L
label values Q21C8B_4   Q21C8B_4L
label values Q21C9B_1   Q21C9B_1L
label values Q21C9B_2   Q21C9B_2L
label values Q21C9B_3   Q21C9B_3L
label values Q21C9B_4   Q21C9B_4L
label values Q21H23C    Q21H23CL
label values Q21H32     Q21H32L
label values Q31B11A2   Q31B11A2L
label values Q31B12A2   Q31B12A2L
label values Q31C12     Q31C12L
label values Q31C14B    Q31C14BL
label values Q31C15B    Q31C15BL
label values Q31C18     Q31C18L
label values Q31C20B    Q31C20BL
label values Q31C21B    Q31C21BL
label values Q31C24     Q31C24L
label values Q31C26B    Q31C26BL
label values Q31C27B    Q31C27BL
label values Q31C30     Q31C30L
label values Q31C32B    Q31C32BL
label values Q31C33B    Q31C33BL
label values Q31C35     Q31C35L
label values Q31C37B    Q31C37BL
label values Q31C39B    Q31C39BL
label values Q31H23C    Q31H23CL
label values Q31H32     Q31H32L
label values Q31H32F1   Q31H32F1L
label values Q31H32F2   Q31H32F2L
label values Q31H32F3   Q31H32F3L
label values Q31H32F4   Q31H32F4L
label values Q31H32F5   Q31H32F5L
label values Q31H32F6   Q31H32F6L
label values Q31H32F7   Q31H32F7L
label values Q31H32F8   Q31H32F8L
label values Q31H32G    Q31H32GL
