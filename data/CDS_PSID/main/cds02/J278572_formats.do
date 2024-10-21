
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
label define ER21020L        0 "None"  , modify
forvalues n = 1/18 {
    label define ER21020L `n' "Actual number"  , modify
}
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
       1 "Release number 1, February 2019"  ///
       2 "Release number 2, May 2019"  ///
       3 "Release number 3, August 2019"  ///
       4 "Release number 4, February 2020"
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
label define ER33602L        0 "Inap.:  born or moved in after the 2001 interview; from Latino sample (ER30001=7001-9308); from Immigrant 2017 sample (ER30001=4001-4462); main family nonresponse by 2001 or mover-out nonresponse by 1999 (ER33601=0)"  , modify

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
       0 "Inap.:  from Latino sample (ER30001=7001-9308); from Immigrant 2017 sample (ER30001=4001-4462); main family nonresponse by 2001 or mover-out nonresponse by 1999 (ER33601=0); born or moved in after the 2001 interview (ER33601>0 and ER33602=0)"
forvalues n = 1/17 {
    label define ER33616L `n' "Highest grade or year of school completed"  , modify
}
label define ER33616L       98 "DK"  , modify
label define ER33616L       99 "NA"  , modify
label define ER33616L        0 "Inap.:  from Latino sample (ER30001=7001-9308); from Immigrant 2017 sample (ER30001=4001-4462); main family nonresponse by 2001 or mover-out nonresponse by 1999 (ER33601=0); in an institution in both 1999 and 2001 (ER33602=51-59 and ER33608=0); associated with a 2001 FU but moved out before 2000 (ER33608=5, 6, or 8 and ER33610<2000) or born or moved in after the 2001 interview (ER33601>0 and ER33602=0); not a person aged 16 or older (ER33604=001-015, 999)"  , modify
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
label define ER33702L        0 "Inap.:  born or moved in after the 2003 interview; from Latino sample (ER30001=7001-9308); from Immigrant 2017 sample (ER30001=4001-4462); main family nonresponse by 2003 or mover-out nonresponse by 2001 (ER33701=0)"  , modify

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
       0 "Inap.:  from Latino sample (ER30001=7001-9308); from Immigrant 2017 sample (ER30001=4001-4462); main family nonresponse by 2003 or mover-out nonresponse by 2001 (ER33701=0); born or moved in after the 2003 interview (ER33701>0 and ER33702=0)"
forvalues n = 1/17 {
    label define ER33716L `n' "Highest grade or year of school completed"  , modify
}
label define ER33716L       98 "DK"  , modify
label define ER33716L       99 "NA"  , modify
label define ER33716L        0 "Inap.:  from Latino sample (ER30001=7001-9308); from Immigrant 2017 sample (ER30001=4001-4462); main family nonresponse by 2003 or mover-out nonresponse by 2001 (ER33701=0); born or moved in after the 2003 interview (ER33701>0 and ER33702=0); in an institution in both 2001 and 2003 (ER33702=51-59 and ER33708=0); associated with 2003 FU but actually moved out before 2002 (ER33708=5, 6, or 8 and ER33710<2002) or moved in in 2003 (ER33708=1 and ER33710=2003); not a person aged 16 or older (ER33704=001-015, 999)"  , modify

label values CHREL      CHRELL
label values DEMREL02   DEMREL02L
label values ER17001    ER17001L
label values ER17016    ER17016L
label values ER20457    ER20457L
label values ER20458    ER20458L
label values ER21001    ER21001L
label values ER21020    ER21020L
label values ER24148    ER24148L
label values ER24149    ER24149L
label values ER30000    ER30000L
label values ER33602    ER33602L
label values ER33603    ER33603L
label values ER33616    ER33616L
label values ER33702    ER33702L
label values ER33703    ER33703L
label values ER33716    ER33716L
