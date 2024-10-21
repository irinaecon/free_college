forvalues n = 1/20 {
    label define ER10008L `n' "Actual number"  , modify
}

label define ER10010L  ///
       1 "Male"  ///
       2 "Female"
label define ER10012L        0 "None"  , modify
forvalues n = 1/18 {
    label define ER10012L `n' "Actual number"  , modify
}
label define ER10013L        1 "Newborn up to second birthday"  , modify
forvalues n = 2/17 {
    label define ER10013L `n' "Actual age"  , modify
}
label define ER10013L        0 "Inap.: no persons aged 17 or younger in FU"  , modify

label define ER10016L  ///
       1 "Married"  ///
       2 "Never married"  ///
       3 "Widowed"  ///
       4 "Divorced, annulled"  ///
       5 "Separated"  ///
       9 "NA; DK"
forvalues n = 1/20 {
    label define ER13009L `n' "Actual number"  , modify
}
label define ER13013L        0 "None"  , modify
forvalues n = 1/18 {
    label define ER13013L `n' "Actual number"  , modify
}
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
label define ER33402L        0 "Inap.:  born or moved in after the 1997 interview; from Immigrant or Latino samples (ER30001=3001-3511, 7001-9308); main family nonresponse by 1997 or mover-out nonresponse by 1996 (ER33401=0)"  , modify

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
       0 "Inap. from Immigrant or Latino samples (ER30001=3001-3511, 7001-9308); main family nonresponse by 1997 or mover-out nonresponse by 1996 (ER33401=0); born or moved in after the 1997 interview (ER33401>0 and ER33402=0)"
forvalues n = 1/20 {
    label define ER33502L `n' "Individuals in the family at the time of the 1999 interview"  , modify
}
forvalues n = 51/59 {
    label define ER33502L `n' "Individuals in institutions at the time of the 1999 interview"  , modify
}
forvalues n = 71/80 {
    label define ER33502L `n' "Individuals who moved out of the FU or out of institutions and established their own households between the 1997 and 1999 interviews"  , modify
}
forvalues n = 81/89 {
    label define ER33502L `n' "Individuals who were living in 1997 but died by the time of the 1999 interview"  , modify
}
label define ER33502L        0 "Inap.:  born or moved in after the 1999 interview; from Latino sample (ER30001=7001-9308); main family nonresponse by 1999 or mover-out nonresponse by 1997 (ER33501=0)"  , modify

label define ER33503L  ///
      10 "Head in 1999; 1997 Head who was mover-out nonresponse by the time of the 1999 interview"  ///
      20 "Legal Wife in 1999; 1997 Wife who was mover-out nonresponse by the time of the 1999 interview"  ///
      22 `""Wife"--female cohabitor who has lived with Head for 12 months or more; 1997 "Wife" who was mover-out nonresponse by the time of the 1999 interview"'  ///
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
       0 "Inap.:  from Latino sample (ER30001=7001-9308); main family nonresponse by 1999 or mover-out nonresponse by 1997 (ER33501=0); born or moved in after the 1999 interview (ER33501>0 and ER33502=0)"

label define Q1B17L  ///
       1 "CHILD IS UNDER 3 YEARS"  ///
       2 "CHILD IS AGE 3 - 5"  ///
       3 "CHILD IS AGE 6 - 9"  ///
       4 "CHILD IS AGE 10 OR OLDER"

label define Q1G1L  ///
       1 "CHILD IS 5 OR YOUNGER"  ///
       2 "CHILD IS 6 - 12 YEARS OLD"

label define Q1G22L  ///
       1 "CHILD IS AGE 3 - 12"  ///
       2 "CHILD IS AGE 0 - 2"

label define Q3LW1L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW10L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW11L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW12L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW13L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW14L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW15L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW16L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW17L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW18L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW19L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW2L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW20L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW21L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW22L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW23L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW24L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW25L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW26L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW27L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW28L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW29L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW3L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW30L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW31L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW32L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW33L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW34L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW35L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW36L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW37L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW38L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW39L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW4L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW40L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW41L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW42L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW43L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW44L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW45L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW46L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW47L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW48L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW49L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW5L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW50L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW51L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW52L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW53L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW54L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW55L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW56L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW57L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW6L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW7L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW8L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"

label define Q3LW9L  ///
       0 "WRONG"  ///
       1 "CORRECT"  ///
       8 "DK"  ///
       9 "NA/REFUSED"
forvalues n = 0/57 {
    label define Q3LWRAWL `n' "ACTUAL SCORE"  , modify
}
label define Q3LWRAWL       99 "NOT ASCERTAINED"  , modify

label define RELOCG97L  ///
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
      99 "Not ascertained"  ///
       0 "Inap: no OCG interview"

label define RELPCG97L  ///
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

label values ER10008  ER10008L
label values ER10010  ER10010L
label values ER10012  ER10012L
label values ER10013  ER10013L
label values ER10016  ER10016L
label values ER13009  ER13009L
label values ER13013  ER13013L
label values ER33402  ER33402L
label values ER33403  ER33403L
label values ER33502  ER33502L
label values ER33503  ER33503L
label values Q1B17    Q1B17L
label values Q1G1     Q1G1L
label values Q1G22    Q1G22L
label values Q3LW1    Q3LW1L
label values Q3LW10   Q3LW10L
label values Q3LW11   Q3LW11L
label values Q3LW12   Q3LW12L
label values Q3LW13   Q3LW13L
label values Q3LW14   Q3LW14L
label values Q3LW15   Q3LW15L
label values Q3LW16   Q3LW16L
label values Q3LW17   Q3LW17L
label values Q3LW18   Q3LW18L
label values Q3LW19   Q3LW19L
label values Q3LW2    Q3LW2L
label values Q3LW20   Q3LW20L
label values Q3LW21   Q3LW21L
label values Q3LW22   Q3LW22L
label values Q3LW23   Q3LW23L
label values Q3LW24   Q3LW24L
label values Q3LW25   Q3LW25L
label values Q3LW26   Q3LW26L
label values Q3LW27   Q3LW27L
label values Q3LW28   Q3LW28L
label values Q3LW29   Q3LW29L
label values Q3LW3    Q3LW3L
label values Q3LW30   Q3LW30L
label values Q3LW31   Q3LW31L
label values Q3LW32   Q3LW32L
label values Q3LW33   Q3LW33L
label values Q3LW34   Q3LW34L
label values Q3LW35   Q3LW35L
label values Q3LW36   Q3LW36L
label values Q3LW37   Q3LW37L
label values Q3LW38   Q3LW38L
label values Q3LW39   Q3LW39L
label values Q3LW4    Q3LW4L
label values Q3LW40   Q3LW40L
label values Q3LW41   Q3LW41L
label values Q3LW42   Q3LW42L
label values Q3LW43   Q3LW43L
label values Q3LW44   Q3LW44L
label values Q3LW45   Q3LW45L
label values Q3LW46   Q3LW46L
label values Q3LW47   Q3LW47L
label values Q3LW48   Q3LW48L
label values Q3LW49   Q3LW49L
label values Q3LW5    Q3LW5L
label values Q3LW50   Q3LW50L
label values Q3LW51   Q3LW51L
label values Q3LW52   Q3LW52L
label values Q3LW53   Q3LW53L
label values Q3LW54   Q3LW54L
label values Q3LW55   Q3LW55L
label values Q3LW56   Q3LW56L
label values Q3LW57   Q3LW57L
label values Q3LW6    Q3LW6L
label values Q3LW7    Q3LW7L
label values Q3LW8    Q3LW8L
label values Q3LW9    Q3LW9L
label values Q3LWRAW  Q3LWRAWL
label values RELOCG97 RELOCG97L
label values RELPCG97 RELPCG97L
