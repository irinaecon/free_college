forvalues n = 1/20 {
    label define ER25016L `n' "Actual number"  , modify
}
label define ER25020L        0 "None"  , modify
forvalues n = 1/18 {
    label define ER25020L `n' "Actual number"  , modify
}
forvalues n = 1/20 {
    label define ER33802L `n' "Individuals in the family at the time of the 2005 interview"  , modify
}
forvalues n = 51/59 {
    label define ER33802L `n' "Individuals in institutions at the time of the 2005 interview"  , modify
}
forvalues n = 71/80 {
    label define ER33802L `n' "Individuals who moved out of the FU or out of institutions and established their own households between the 2003 and 2005 interviews"  , modify
}
forvalues n = 81/89 {
    label define ER33802L `n' "Individuals who were living in 2003 but died by the time of the 2005 interview"  , modify
}
label define ER33802L        0 "Inap.:  from Latino sample (ER30001=7001-9308); main family nonresponse by 2005 or mover-out nonresponse by 2003 (ER33801=0)"  , modify

label define ER33803L  ///
      10 "Head in 2005; 2003 Head who was mover-out nonresponse by the time of the 2005 interview"  ///
      20 "Legal Wife in 2005; 2003 Wife who was mover-out nonresponse by the time of the 2005 interview"  ///
      22 `""Wife"--female cohabitor who has lived with Head for 12 months or more; 2003 "Wife" who was mover-out nonresponse by the time of the 2005 interview"'  ///
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
       0 "Inap.:  from Latino sample (ER30001=7001-9308); main family nonresponse by 2005 or mover-out nonresponse by 2003 (ER33801=0)"
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
label define ER33902L        0 "Inap.:  from Latino sample (ER30001=7001-9308); main family nonresponse by 2007 or mover-out nonresponse by 2005 (ER33901=0)"  , modify

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
       0 "Inap.:  from Latino sample (ER30001=7001-9308); main family nonresponse by 2007 or mover-out nonresponse by 2005 (ER33901=0)"
forvalues n = 1/20 {
    label define ER36016L `n' "Actual number"  , modify
}

label define ER36018L  ///
       1 "Male"  ///
       2 "Female"
label define ER36020L        0 "None"  , modify
forvalues n = 1/18 {
    label define ER36020L `n' "Actual number"  , modify
}

label define ER36023L  ///
       1 "Married"  ///
       2 "Never married"  ///
       3 "Widowed"  ///
       4 "Divorced, annulled"  ///
       5 "Separated"  ///
       8 "DK"  ///
       9 "NA; refused"

label define Q34LW1L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW10L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW11L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW12L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW13L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW14L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW15L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW16L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW17L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW18L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW19L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW2L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW20L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW21L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW22L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW23L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW24L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW25L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW26L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW27L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW28L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW29L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW3L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW30L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW31L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW32L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW33L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW34L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW35L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW36L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW37L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW38L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW39L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW4L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW40L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW41L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW42L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW43L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW44L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW45L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW46L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW47L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW48L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW49L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW5L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW50L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW51L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW52L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW53L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW54L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW55L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW56L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW57L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW58L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW6L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW7L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW8L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"

label define Q34LW9L  ///
       1 "Correct response"  ///
       5 "Incorrect response"  ///
       8 "DK"  ///
       9 "NA; refused"  ///
       0 "Inap.:  refer to the User Guide (see note above)"
forvalues n = 0/58 {
    label define Q34LWRAWL `n' "Actual score"  , modify
}
label define Q34LWRAWL       99 "NA; Letter Word not administered"  , modify

label define RELOCG07L  ///
       1 "Biological mother"  ///
       2 "Stepmother"  ///
       3 "Adoptive mother"  ///
       4 "Biological father"  ///
       5 "Stepfather"  ///
       6 "Adoptive father"  ///
       7 "Grandmother"  ///
       8 "Grandfather"  ///
       9 "Partner of father"  ///
      10 "Partner of mother"  ///
      11 "Aunt"  ///
      12 "Uncle"  ///
      13 "Sister/Step-sister/Half-sister"  ///
      14 "Brother/Step-brother/Half-brother"  ///
      15 "Son/Daughter of partner"  ///
      16 "Parent of partner"  ///
      17 "Other relative"  ///
      18 "Legal guardian"  ///
      19 "Foster mother"  ///
      20 "Foster father"  ///
      21 "Other non-relative"  ///
      99 "NA"  ///
       0 "Inap.: no OCG interview"

label define RELPCG07L  ///
       1 "Biological mother"  ///
       2 "Stepmother"  ///
       3 "Adoptive mother"  ///
       4 "Biological father"  ///
       5 "Stepfather"  ///
       6 "Adoptive father"  ///
       7 "Grandmother"  ///
       8 "Grandfather"  ///
       9 "Partner of father"  ///
      10 "Partner of mother"  ///
      11 "Aunt"  ///
      12 "Uncle"  ///
      13 "Sister/Step-sister/Half-sister"  ///
      14 "Brother/Step-brother/Half-brother"  ///
      15 "Son/Daughter of partner"  ///
      16 "Parent of partner"  ///
      17 "Other relative"  ///
      18 "Legal guardian"  ///
      19 "Foster mother"  ///
      20 "Foster father"  ///
      21 "Other non-relative"  ///
      99 "NA"

label values ER25016  ER25016L
label values ER25020  ER25020L
label values ER33802  ER33802L
label values ER33803  ER33803L
label values ER33902  ER33902L
label values ER33903  ER33903L
label values ER36016  ER36016L
label values ER36018  ER36018L
label values ER36020  ER36020L
label values ER36023  ER36023L
label values Q34LW1   Q34LW1L
label values Q34LW10  Q34LW10L
label values Q34LW11  Q34LW11L
label values Q34LW12  Q34LW12L
label values Q34LW13  Q34LW13L
label values Q34LW14  Q34LW14L
label values Q34LW15  Q34LW15L
label values Q34LW16  Q34LW16L
label values Q34LW17  Q34LW17L
label values Q34LW18  Q34LW18L
label values Q34LW19  Q34LW19L
label values Q34LW2   Q34LW2L
label values Q34LW20  Q34LW20L
label values Q34LW21  Q34LW21L
label values Q34LW22  Q34LW22L
label values Q34LW23  Q34LW23L
label values Q34LW24  Q34LW24L
label values Q34LW25  Q34LW25L
label values Q34LW26  Q34LW26L
label values Q34LW27  Q34LW27L
label values Q34LW28  Q34LW28L
label values Q34LW29  Q34LW29L
label values Q34LW3   Q34LW3L
label values Q34LW30  Q34LW30L
label values Q34LW31  Q34LW31L
label values Q34LW32  Q34LW32L
label values Q34LW33  Q34LW33L
label values Q34LW34  Q34LW34L
label values Q34LW35  Q34LW35L
label values Q34LW36  Q34LW36L
label values Q34LW37  Q34LW37L
label values Q34LW38  Q34LW38L
label values Q34LW39  Q34LW39L
label values Q34LW4   Q34LW4L
label values Q34LW40  Q34LW40L
label values Q34LW41  Q34LW41L
label values Q34LW42  Q34LW42L
label values Q34LW43  Q34LW43L
label values Q34LW44  Q34LW44L
label values Q34LW45  Q34LW45L
label values Q34LW46  Q34LW46L
label values Q34LW47  Q34LW47L
label values Q34LW48  Q34LW48L
label values Q34LW49  Q34LW49L
label values Q34LW5   Q34LW5L
label values Q34LW50  Q34LW50L
label values Q34LW51  Q34LW51L
label values Q34LW52  Q34LW52L
label values Q34LW53  Q34LW53L
label values Q34LW54  Q34LW54L
label values Q34LW55  Q34LW55L
label values Q34LW56  Q34LW56L
label values Q34LW57  Q34LW57L
label values Q34LW58  Q34LW58L
label values Q34LW6   Q34LW6L
label values Q34LW7   Q34LW7L
label values Q34LW8   Q34LW8L
label values Q34LW9   Q34LW9L
label values Q34LWRAW Q34LWRAWL
label values RELOCG07 RELOCG07L
label values RELPCG07 RELPCG07L
