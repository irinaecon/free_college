
label define ASMRELL  ///
       1 "Release number 1 - July 2004"  ///
       2 "Release number 2 - January 2005"  ///
       3 "Release number 3 - October 2005"  ///
       4 "Release number 4 - January 2014"  ///
       5 "Release number 5 - May 2019"
forvalues n = 1/18 {
    label define CDI_02L `n' "Depression score"  , modify
}
label define CDI_02L       99 "Missing score: Child had one or more of the ten items at L16 missing and thus a score was not computed."  , modify
label define CDI_02L        0 "Inap.: if not eligible for ACASI Section L (Age=8-11 yrs old) or valid response of zero"  , modify

label define CHLDRELL  ///
       1 "Release number 1 - July 2004"  ///
       2 "Release number 2 - January 2005"  ///
       3 "Release number 3 - October 2005"  ///
       4 "Release number 4 - June 2010"  ///
       5 "Release number 5 - November 2015"  ///
       6 "Release number 6 - May 2019"

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

label define ER17001L  ///
       1 "Release number 1 - November 2002"  ///
       2 "Release number 2 - May 2008"  ///
       3 "Release number 3 - November 2013"  ///
       4 "Release number 4 - February 2014"  ///
       5 "Release number 5 - January 2016"  ///
       6 "Release number 6 - November 2017"
forvalues n = 1/20 {
    label define ER17012L `n' "Actual number"  , modify
}
label define ER17016L        0 "None"  , modify
forvalues n = 1/18 {
    label define ER17016L `n' "Actual number"  , modify
}
label define ER20457L        0 "Completed no grades of school"  , modify
forvalues n = 1/16 {
    label define ER20457L `n' "Actual number"  , modify
}
label define ER20457L       17 "At least some post-graduate work"  , modify
label define ER20457L       99 "NA; DK"  , modify
forvalues n = 1/16 {
    label define ER20458L `n' "Actual number"  , modify
}
label define ER20458L       17 "At least some post-graduate work"  , modify
label define ER20458L       99 "NA; DK"  , modify
label define ER20458L        0 `"Inap.:   no wife/"wife" in FU; completed no grades of school"'  , modify

label define ER21001L  ///
       1 "Release number 1 - December 2004"  ///
       2 "Release number 2 - October 2005"  ///
       3 "Release number 3 - November 2005"  ///
       4 "Release number 4 - May 2008"  ///
       5 "Release number 5 - November 2013"  ///
       6 "Release number 6 - February 2014"  ///
       7 "Release number 7 - January 2016"  ///
       8 "Release number 8 - November 2017"
forvalues n = 1/20 {
    label define ER21016L `n' "Actual number"  , modify
}

label define ER21018L  ///
       1 "Male"  ///
       2 "Female"
label define ER21020L        0 "None"  , modify
forvalues n = 1/18 {
    label define ER21020L `n' "Actual number"  , modify
}

label define ER21023L  ///
       1 "Married"  ///
       2 "Never married"  ///
       3 "Widowed"  ///
       4 "Divorced, annulled"  ///
       5 "Separated"  ///
       8 "DK"  ///
       9 "NA; refused"
label define ER24148L        0 "Completed no grades of school"  , modify
forvalues n = 1/16 {
    label define ER24148L `n' "Actual number"  , modify
}
label define ER24148L       17 "At least some post-graduate work"  , modify
label define ER24148L       99 "NA; DK"  , modify
forvalues n = 1/16 {
    label define ER24149L `n' "Actual number"  , modify
}
label define ER24149L       17 "At least some post-graduate work"  , modify
label define ER24149L       99 "NA; DK"  , modify
label define ER24149L        0 `"Inap.:   no wife/"wife" in FU; completed no grades of school"'  , modify

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
forvalues n = 1/17 {
    label define ER33616L `n' "Highest grade or year of school completed"  , modify
}
label define ER33616L       98 "DK"  , modify
label define ER33616L       99 "NA"  , modify
label define ER33616L        0 "Inap.:  from Latino sample (ER30001=7001-9308); from Immigrant 2017 sample (ER30001=4001-4851); main family nonresponse by 2001 or mover-out nonresponse by 1999 (ER33601=0); in an institution in both 1999 and 2001 (ER33602=51-59 and ER33608=0); associated with a 2001 FU but moved out before 2000 (ER33608=5, 6, or 8 and ER33610<2000) or born or moved in after the 2001 interview (ER33601>0 and ER33602=0); not a person aged 16 or older (ER33604=001-015, 999)"  , modify
forvalues n = 1/20 {
    label define ER33702L `n' "Individuals in the family at the time of the 2003 interview"  , modify
}
forvalues n = 51/59 {
    label define ER33702L `n' "Individuals in institutions at the time of the 2003 interview"  , modify
}
forvalues n = 71/80 {
    label define ER33702L `n' "Individuals who moved out of the FU or out of institutions and established their own households between the 2001 and 2003 interviews"  , modify
}
forvalues n = 81/89 {
    label define ER33702L `n' "Individuals who were living in 2001 but died by the time of the 2003 interview"  , modify
}
label define ER33702L        0 "Inap.:  born or moved in after the 2003 interview; from Latino sample (ER30001=7001-9308); from Immigrant 2017 sample (ER30001=4001-4851); main family nonresponse by 2003 or mover-out nonresponse by 2001 (ER33701=0)"  , modify

label define ER33703L  ///
      10 "Head in 2003; 2001 Head who was mover-out nonresponse by the time of the 2003 interview"  ///
      20 "Legal Wife in 2003; 2001 Wife who was mover-out nonresponse by the time of the 2003 interview"  ///
      22 `""Wife"--female cohabitor who has lived with Head for 12 months or more; 2001 "Wife" who was mover-out nonresponse by the time of the 2003 interview"'  ///
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
      60 "Grandson or granddaughter of Head (includes grandchildren of legal Wife (code 20); those of a cohabitor are coded 97)"  ///
      65 "Great-grandson or great-granddaughter of Head (includes great-grandchildren of legal Wife (code 20); those of a cohabitor are coded 97)"  ///
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
       0 "Inap.:  from Latino sample (ER30001=7001-9308); from Immigrant 2017 sample (ER30001=4001-4851); main family nonresponse by 2003 or mover-out nonresponse by 2001 (ER33701=0); born or moved in after the 2003 interview (ER33701>0 and ER33702=0)"
forvalues n = 1/17 {
    label define ER33716L `n' "Highest grade or year of school completed"  , modify
}
label define ER33716L       98 "DK"  , modify
label define ER33716L       99 "NA"  , modify
label define ER33716L        0 "Inap.:  from Latino sample (ER30001=7001-9308); from Immigrant 2017 sample (ER30001=4001-4851); main family nonresponse by 2003 or mover-out nonresponse by 2001 (ER33701=0); born or moved in after the 2003 interview (ER33701>0 and ER33702=0); in an institution in both 2001 and 2003 (ER33702=51-59 and ER33708=0); associated with 2003 FU but actually moved out before 2002 (ER33708=5, 6, or 8 and ER33710<2002) or moved in in 2003 (ER33708=1 and ER33710=2003); not a person aged 16 or older (ER33704=001-015, 999)"  , modify
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

label define Q24LW1L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW10L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW11L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW12L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW13L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW14L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW15L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW16L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW17L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW18L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW19L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW2L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW20L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW21L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW22L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW23L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW24L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW25L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW26L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW27L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW28L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW29L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW3L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW30L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW31L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW32L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW33L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW34L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW35L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW36L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW37L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW38L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW39L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW4L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW40L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW41L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW42L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW43L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW44L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW45L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW46L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW47L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW48L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW49L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW5L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW50L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW51L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW52L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW53L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW54L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW55L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW56L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW57L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW58L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained) or Q24LANG = 1: English version (57 items only)"

label define Q24LW6L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW7L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW8L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"

label define Q24LW9L  ///
       0 "Incorrect response"  ///
       1 "Correct response"  ///
       9 "NA (Not ascertained)"
forvalues n = 0/58 {
    label define Q24LWRAWL `n' "Actual score"  , modify
}
label define Q24LWRAWL       99 "NA (Not ascertained); Letter Word not administered"  , modify

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

label define RELOCG02L  ///
       1 "Biological mother"  ///
       2 "Stepmother"  ///
       3 "Adoptive mother"  ///
       4 "Biological father"  ///
       5 "Stepfather"  ///
       6 "Adoptive father"  ///
       7 "Grandmother"  ///
       8 "Grandfather"  ///
       9 "Female partner of primary caregiver"  ///
      10 "Male partner of primary caregiver"  ///
      11 "Aunt"  ///
      12 "Uncle"  ///
      13 "Sister"  ///
      14 "Brother"  ///
      15 "Other relative"  ///
      16 "Legal guardian"  ///
      17 "Foster mother"  ///
      18 "Foster father"  ///
      19 "Nonrelative"  ///
       0 "Inap: no OCG interview"

label define RELPCG02L  ///
       1 "Biological mother"  ///
       2 "Stepmother"  ///
       3 "Adoptive mother"  ///
       4 "Biological father"  ///
       5 "Stepfather"  ///
       6 "Adoptive father"  ///
       7 "Grandmother"  ///
       8 "Grandfather"  ///
       9 "Female partner of other caregiver"  ///
      10 "Male partner of other caregiver"  ///
      11 "Aunt"  ///
      12 "Uncle"  ///
      13 "Sister"  ///
      14 "Brother"  ///
      15 "Other relative"  ///
      16 "Legal guardian"  ///
      17 "Foster mother"  ///
      18 "Foster father"  ///
      19 "Nonrelative"

label values ASMREL     ASMRELL
label values CDI_02     CDI_02L
label values CHLDREL    CHLDRELL
label values CHREL      CHRELL
label values DEMREL02   DEMREL02L
label values ER17001    ER17001L
label values ER17012    ER17012L
label values ER17016    ER17016L
label values ER20457    ER20457L
label values ER20458    ER20458L
label values ER21001    ER21001L
label values ER21016    ER21016L
label values ER21018    ER21018L
label values ER21020    ER21020L
label values ER21023    ER21023L
label values ER24148    ER24148L
label values ER24149    ER24149L
label values ER30000    ER30000L
label values ER33602    ER33602L
label values ER33603    ER33603L
label values ER33616    ER33616L
label values ER33702    ER33702L
label values ER33703    ER33703L
label values ER33716    ER33716L
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
label values Q24LW1     Q24LW1L
label values Q24LW10    Q24LW10L
label values Q24LW11    Q24LW11L
label values Q24LW12    Q24LW12L
label values Q24LW13    Q24LW13L
label values Q24LW14    Q24LW14L
label values Q24LW15    Q24LW15L
label values Q24LW16    Q24LW16L
label values Q24LW17    Q24LW17L
label values Q24LW18    Q24LW18L
label values Q24LW19    Q24LW19L
label values Q24LW2     Q24LW2L
label values Q24LW20    Q24LW20L
label values Q24LW21    Q24LW21L
label values Q24LW22    Q24LW22L
label values Q24LW23    Q24LW23L
label values Q24LW24    Q24LW24L
label values Q24LW25    Q24LW25L
label values Q24LW26    Q24LW26L
label values Q24LW27    Q24LW27L
label values Q24LW28    Q24LW28L
label values Q24LW29    Q24LW29L
label values Q24LW3     Q24LW3L
label values Q24LW30    Q24LW30L
label values Q24LW31    Q24LW31L
label values Q24LW32    Q24LW32L
label values Q24LW33    Q24LW33L
label values Q24LW34    Q24LW34L
label values Q24LW35    Q24LW35L
label values Q24LW36    Q24LW36L
label values Q24LW37    Q24LW37L
label values Q24LW38    Q24LW38L
label values Q24LW39    Q24LW39L
label values Q24LW4     Q24LW4L
label values Q24LW40    Q24LW40L
label values Q24LW41    Q24LW41L
label values Q24LW42    Q24LW42L
label values Q24LW43    Q24LW43L
label values Q24LW44    Q24LW44L
label values Q24LW45    Q24LW45L
label values Q24LW46    Q24LW46L
label values Q24LW47    Q24LW47L
label values Q24LW48    Q24LW48L
label values Q24LW49    Q24LW49L
label values Q24LW5     Q24LW5L
label values Q24LW50    Q24LW50L
label values Q24LW51    Q24LW51L
label values Q24LW52    Q24LW52L
label values Q24LW53    Q24LW53L
label values Q24LW54    Q24LW54L
label values Q24LW55    Q24LW55L
label values Q24LW56    Q24LW56L
label values Q24LW57    Q24LW57L
label values Q24LW58    Q24LW58L
label values Q24LW6     Q24LW6L
label values Q24LW7     Q24LW7L
label values Q24LW8     Q24LW8L
label values Q24LW9     Q24LW9L
label values Q24LWRAW   Q24LWRAWL
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
label values RELOCG02   RELOCG02L
label values RELPCG02   RELPCG02L
