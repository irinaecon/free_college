
label define DEMREL97L  ///
       1 "Release number 1 - September 1999"  ///
       2 "Release number 2 - April 2000"  ///
       3 "Release number 3 - December 2000"  ///
       4 "Release number 4 - October 2001"  ///
       5 "Release number 5 - March 2006"  ///
       6 "Release number 6 - May 2019"

label define EMSAREL97L  ///
       1 "Release 1 - April 2006"  ///
       2 "Release 2 - November 2006"  ///
       3 "Release 3 - May 2019"

label define ER30000L  ///
       1 "Release number 1, March 2021"  ///
       2 "Release number 2, April 2021"
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

label define Q12A28L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA/Refused"

label define Q12A28BL  ///
       2 "Per day"  ///
       5 "Monthly"  ///
       6 "Yearly"  ///
       7 "Other"  ///
       9 "NA/Refused"  ///
       0 "Inap.:"

label define Q12A28CL  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA/Refused"  ///
       0 "Inap.:"

label define Q12A28DL  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA/Refused"  ///
       0 "Inap.:"

label define Q12A2AL  ///
       1 "Yes"  ///
       5 "No"

label define Q12A2BL  ///
       1 "Yes"  ///
       5 "No"

label define Q12A2CL  ///
       1 "Yes"  ///
       5 "No"

label define Q12A2DL  ///
       1 "Yes"  ///
       5 "No"

label define Q12A2EL  ///
       1 "Yes"  ///
       5 "No"

label define Q12A2FL  ///
       1 "Yes"  ///
       5 "No"

label define Q12A2GL  ///
       1 "Yes"  ///
       5 "No"

label define Q12A2HL  ///
       1 "Yes"  ///
       5 "No"

label define Q12A2IL  ///
       1 "Yes"  ///
       5 "No"

label define Q12A2JL  ///
       1 "Yes"  ///
       5 "No"

label define Q12A2KL  ///
       1 "Yes"  ///
       5 "No"

label define Q12A2LL  ///
       1 "Yes"  ///
       5 "No"

label define Q12A2ML  ///
       1 "Yes"  ///
       5 "No"

label define Q12A2NL  ///
       1 "Yes"  ///
       5 "No"

label define Q13A21L  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA/Refused"

label define Q13A21BL  ///
       1 "Per hour"  ///
       2 "Per day"  ///
       3 "Per week"  ///
       4 "Every two weeks"  ///
       5 "Monthly"  ///
       6 "Yearly"  ///
       7 "Other"  ///
       9 "NA/Refused"  ///
       0 "Inap.:"

label define Q13A21CL  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA/Refused"  ///
       0 "Inap.:"

label define Q13A21DL  ///
       1 "Yes"  ///
       5 "No"  ///
       9 "NA/Refused"  ///
       0 "Inap.:"

label define Q1B17L  ///
       1 "CHILD IS UNDER 3 YEARS"  ///
       2 "CHILD IS AGE 3 - 5"  ///
       3 "CHILD IS AGE 6 - 9"  ///
       4 "CHILD IS AGE 10 OR OLDER"

label define Q1G1L  ///
       1 "CHILD IS 5 OR YOUNGER"  ///
       2 "CHILD IS 6 - 12 YEARS OLD"

label define Q1G11DL  ///
       1 "PER HOUR"  ///
       2 "PER DAY"  ///
       3 "PER WEEK"  ///
       4 "EVERY TWO WEEKS"  ///
       5 "EVERY MONTH"  ///
       6 "EVERY YEAR"  ///
       7 "OTHER"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1G11C=0"

label define Q1G22L  ///
       1 "CHILD IS AGE 3 - 12"  ///
       2 "CHILD IS AGE 0 - 2"

label define Q1H10_1L  ///
       1 "YES"  ///
       5 "NO"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1"

label define Q1H10_2L  ///
       1 "YES"  ///
       5 "NO"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 1 ARRNGMNT"

label define Q1H10_3L  ///
       1 "YES"  ///
       5 "NO"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 2 ARRNGMNTS"

label define Q1H10_4L  ///
       1 "YES"  ///
       5 "NO"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 3 ARRNGMNTS"

label define Q1H10_5L  ///
       1 "YES"  ///
       5 "NO"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 4 ARRNGMNTS"

label define Q1H10_6L  ///
       1 "YES"  ///
       5 "NO"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 5 ARRNGMNTS"

label define Q1H10_7L  ///
       1 "YES"  ///
       5 "NO"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 6 ARRNGMNTS"

label define Q1H10_8L  ///
       1 "YES"  ///
       5 "NO"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 7 ARRNGMNTS"

label define Q1H10_9L  ///
       1 "YES"  ///
       5 "NO"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 8 ARRNGMNTS"

label define Q1H13L  ///
       1 "NONE"  ///
       2 "MORE THAN ONE ARRANGEMENT"  ///
       3 "ONLY ONE ARRANGEMENT"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H11 NE 1"

label define Q1H14L  ///
       1 "RELATIVE UNDER 13"  ///
       2 "RELATIVE 13 OR OLDER"  ///
       3 "NON-RELATIVE"  ///
       4 "CARE IN RELATIVE`=char(146)'S HOME"  ///
       5 "FAMILY DAYCARE PROVIDER"  ///
       6 "HEAD START"  ///
       7 "PREKINDERGARTEN, NURSERY"  ///
       8 "BEFORE/AFTER-SCHOOL"  ///
       9 "CHILD CARES FOR SELF"  ///
      10 "OTHER TYPE"  ///
      11 "NONE, PARENT ALWAYS CARES"  ///
      98 "DK"  ///
      99 "NA; REFUSED"  ///
       0 "Inap.: Q1H13 NE 2 AND NE 3"

label define Q1H15L  ///
       1 "RELATIVE UNDER 13"  ///
       2 "RELATIVE 13 OR OLDER"  ///
       3 "NON-RELATIVE"  ///
       4 "CARE IN RELATIVE`=char(146)'S HOME"  ///
       5 "FAMILY DAYCARE PROVIDER"  ///
       6 "HEAD START"  ///
       7 "PREKINDERGARTEN, NURSERY"  ///
       8 "BEFORE/AFTER-SCHOOL"  ///
       9 "CHILD CARES FOR SELF"  ///
      10 "OTHER TYPE"  ///
      11 "NONE, PARENT ALWAYS CARES"  ///
      98 "DK"  ///
      99 "NA; REFUSED"  ///
       0 "Inap.: Q1H13 NE 2"

label define Q1H16L  ///
       1 "RELATIVE UNDER 13"  ///
       2 "RELATIVE 13 OR OLDER"  ///
       3 "NON-RELATIVE"  ///
       4 "CARE IN RELATIVE`=char(146)'S HOME"  ///
       5 "FAMILY DAYCARE PROVIDER"  ///
       6 "HEAD START"  ///
       7 "PREKINDERGARTEN, NURSERY"  ///
       8 "BEFORE/AFTER-SCHOOL"  ///
       9 "CHILD CARES FOR SELF"  ///
      10 "OTHER TYPE"  ///
      11 "NONE, PARENT ALWAYS CARES"  ///
      98 "DK"  ///
      99 "NA; REFUSED"  ///
       0 "Inap.: Q1H13 NE 2 AND UP TO 2 ARRNGMNTS"

label define Q1H17L  ///
       1 "RELATIVE UNDER 13"  ///
       2 "RELATIVE 13 OR OLDER"  ///
       3 "NON-RELATIVE"  ///
       4 "CARE IN RELATIVE`=char(146)'S HOME"  ///
       5 "FAMILY DAYCARE PROVIDER"  ///
       6 "HEAD START"  ///
       7 "PREKINDERGARTEN, NURSERY"  ///
       8 "BEFORE/AFTER-SCHOOL"  ///
       9 "CHILD CARES FOR SELF"  ///
      10 "OTHER TYPE"  ///
      11 "NONE, PARENT ALWAYS CARES"  ///
      98 "DK"  ///
      99 "NA; REFUSED"  ///
       0 "Inap.: Q1H13 NE 2 AND UP TO 3 ARRNGMNTS"
forvalues n = 1/7 {
    label define Q1H18L `n' "DAYS PER WEEK"  , modify
}
label define Q1H18L        8 "DK"  , modify
label define Q1H18L        9 "NA; REFUSED"  , modify
label define Q1H18L        0 "Inap.: Q1H13 NE 2 AND NE 3"  , modify
forvalues n = 1/18 {
    label define Q1H20WL `n' "WEEKS"  , modify
}
label define Q1H20WL       98 "DK"  , modify
label define Q1H20WL       99 "NA; REFUSED"  , modify
label define Q1H20WL        0 "WEEKS NOT REPORTED OR Inap.: Q1H13 NE 2 AND NE 3"  , modify

label define Q1H21AL  ///
       1 "PER HOUR"  ///
       2 "DAILY"  ///
       3 "WEEKLY"  ///
       4 "EVERY 2 WEEKS"  ///
       5 "EVERY MONTH"  ///
       6 "EVERY YEAR"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H21=0"

label define Q1H22L  ///
       1 "INCLUDES OTHER CHILDREN"  ///
       2 "CHILD ONLY"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H21=0"
forvalues n = 1/4 {
    label define Q1H22AL `n' "NUMBER OF CHILDREN"  , modify
}
label define Q1H22AL        8 "DK"  , modify
label define Q1H22AL        9 "NA; REFUSED"  , modify
label define Q1H22AL        0 "Inap.: Q1H21=0/ Q1H22 NE 1"  , modify

label define Q1H23L  ///
       1 "R HAS ANSWER IN H15"  ///
       2 "ALL OTHERS"  ///
       0 "Inap.: UP TO 1 ARRANGMNT"
forvalues n = 1/5 {
    label define Q1H24L `n' "DAYS PER WEEK"  , modify
}
label define Q1H24L        8 "DK"  , modify
label define Q1H24L        9 "NA; REFUSED"  , modify
label define Q1H24L        0 "Inap.: Q1H23 NE 1"  , modify
forvalues n = 1/16 {
    label define Q1H26WL `n' "WEEKS"  , modify
}
label define Q1H26WL       98 "DK"  , modify
label define Q1H26WL       99 "NA; REFUSED"  , modify
label define Q1H26WL        0 "WEEKS NOT REPORTED OR Inap.: Q1H23 NE 1"  , modify

label define Q1H27AL  ///
       1 "PER HOUR"  ///
       2 "DAILY"  ///
       3 "WEEKLY"  ///
       4 "EVERY 2 WEEKS"  ///
       5 "EVERY MONTH"  ///
       6 "EVERY YEAR"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H27=0"

label define Q1H28L  ///
       1 "INCLUDES OTHER CHILDREN"  ///
       2 "CHILD ONLY"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H27=0"
forvalues n = 1/2 {
    label define Q1H28AL `n' "No code text"  , modify
}
label define Q1H28AL        8 "DK"  , modify
label define Q1H28AL        9 "NA; REFUSED"  , modify
label define Q1H28AL        0 "Inap.: Q1H27=0/ Q1H28 NE 1"  , modify

label define Q1H29L  ///
       1 "R HAS ANSWER IN H16"  ///
       2 "ALL OTHERS"  ///
       0 "Inap.: UP TO 2 ARRANGMNTS"

label define Q1H2_1L  ///
       1 "STARTED/RETURNED WORK"  ///
       2 "INCREASED/CHANGED WORK HOURS"  ///
       3 "STARTED LOOKING FOR WORK"  ///
       4 "STARTED SCHOOL"  ///
       5 "STARTED OTHER ACTIVITY"  ///
       6 "CHILD NEEDED PLAYMATES/ACTIVITIES"  ///
       7 "OTHER"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1"

label define Q1H2_2L  ///
       1 "STARTED/RETURNED WORK"  ///
       2 "INCREASED/CHANGED WORK HOURS"  ///
       3 "STARTED LOOKING FOR WORK"  ///
       4 "STARTED SCHOOL"  ///
       5 "STARTED OTHER ACTIVITY"  ///
       6 "CHILD NEEDED PLAYMATES/ACTIVITIES"  ///
       7 "OTHER"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 1 ARRNGMNT"

label define Q1H2_3L  ///
       1 "STARTED/RETURNED WORK"  ///
       2 "INCREASED/CHANGED WORK HOURS"  ///
       3 "STARTED LOOKING FOR WORK"  ///
       4 "STARTED SCHOOL"  ///
       5 "STARTED OTHER ACTIVITY"  ///
       6 "CHILD NEEDED PLAYMATES/ACTIVITIES"  ///
       7 "OTHER"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 2 ARRNGMNTS"

label define Q1H2_4L  ///
       1 "STARTED/RETURNED WORK"  ///
       2 "INCREASED/CHANGED WORK HOURS"  ///
       3 "STARTED LOOKING FOR WORK"  ///
       4 "STARTED SCHOOL"  ///
       5 "STARTED OTHER ACTIVITY"  ///
       6 "CHILD NEEDED PLAYMATES/ACTIVITIES"  ///
       7 "OTHER"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 3 ARRNGMNTS"

label define Q1H2_5L  ///
       1 "STARTED/RETURNED WORK"  ///
       2 "INCREASED/CHANGED WORK HOURS"  ///
       3 "STARTED LOOKING FOR WORK"  ///
       4 "STARTED SCHOOL"  ///
       5 "STARTED OTHER ACTIVITY"  ///
       6 "CHILD NEEDED PLAYMATES/ACTIVITIES"  ///
       7 "OTHER"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 4 ARRNGMNTS"

label define Q1H2_6L  ///
       1 "STARTED/RETURNED WORK"  ///
       2 "INCREASED/CHANGED WORK HOURS"  ///
       3 "STARTED LOOKING FOR WORK"  ///
       4 "STARTED SCHOOL"  ///
       5 "STARTED OTHER ACTIVITY"  ///
       6 "CHILD NEEDED PLAYMATES/ACTIVITIES"  ///
       7 "OTHER"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 5 ARRNGMNTS"

label define Q1H2_7L  ///
       1 "STARTED/RETURNED WORK"  ///
       2 "INCREASED/CHANGED WORK HOURS"  ///
       3 "STARTED LOOKING FOR WORK"  ///
       4 "STARTED SCHOOL"  ///
       5 "STARTED OTHER ACTIVITY"  ///
       6 "CHILD NEEDED PLAYMATES/ACTIVITIES"  ///
       7 "OTHER"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 6 ARRNGMNTS"

label define Q1H2_8L  ///
       1 "STARTED/RETURNED WORK"  ///
       2 "INCREASED/CHANGED WORK HOURS"  ///
       3 "STARTED LOOKING FOR WORK"  ///
       4 "STARTED SCHOOL"  ///
       5 "STARTED OTHER ACTIVITY"  ///
       6 "CHILD NEEDED PLAYMATES/ACTIVITIES"  ///
       7 "OTHER"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 7 ARRNGMNTS"

label define Q1H2_9L  ///
       1 "STARTED/RETURNED WORK"  ///
       2 "INCREASED/CHANGED WORK HOURS"  ///
       3 "STARTED LOOKING FOR WORK"  ///
       4 "STARTED SCHOOL"  ///
       5 "STARTED OTHER ACTIVITY"  ///
       6 "CHILD NEEDED PLAYMATES/ACTIVITIES"  ///
       7 "OTHER"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 8 ARRNGMNTS"
forvalues n = 1/5 {
    label define Q1H30L `n' "DAYS PER WEEK"  , modify
}
label define Q1H30L        8 "DK"  , modify
label define Q1H30L        9 "NA; REFUSED"  , modify
label define Q1H30L        0 "Inap.: Q1H29 NE 1"  , modify
forvalues n = 1/36 {
    label define Q1H32ML `n' "MONTHS"  , modify
}
label define Q1H32ML       98 "DK"  , modify
label define Q1H32ML       99 "NA; REFUSED"  , modify
label define Q1H32ML        0 "Inap.: Q1H29 NE 1"  , modify

label define Q1H33AL  ///
       1 "PER HOUR"  ///
       2 "DAILY"  ///
       3 "WEEKLY"  ///
       4 "EVERY 2 WEEKS"  ///
       5 "EVERY MONTH"  ///
       6 "EVERY YEAR"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H33=0"

label define Q1H34L  ///
       1 "INCLUDES OTHER CHILDREN"  ///
       2 "CHILD ONLY"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H33=0"

label define Q1H34AL  ///
       2 "NUMBER OF CHILDREN"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H33=0/ Q1H34 NE 1"

label define Q1H35L  ///
       1 "R HAS ANSWER IN H17"  ///
       2 "ALL OTHERS"  ///
       0 "Inap.: UP TO 3 ARRANGMNTS"

label define Q1H36L  ///
       1 "DAYS PER WEEK"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H35 NE 1"
forvalues n = 1/3 {
    label define Q1H37L `n' "HOURS PER WEEK"  , modify
}
label define Q1H37L        8 "DK"  , modify
label define Q1H37L        9 "NA; REFUSED"  , modify
label define Q1H37L        0 "Inap.: Q1H35 NE 1"  , modify
forvalues n = 5/12 {
    label define Q1H38L `n' "MONTHS"  , modify
}
label define Q1H38L       98 "DK"  , modify
label define Q1H38L       99 "NA; REFUSED"  , modify
label define Q1H38L        0 "Inap.: Q1H35 NE 1"  , modify

label define Q1H39L  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "$0 OR Inap.: Q1H35 NE 1"
forvalues n = 1/5 {
    label define Q1H3_1YL `n' "YEARS"  , modify
}
label define Q1H3_1YL        8 "DK"  , modify
label define Q1H3_1YL        9 "NA; REFUSED"  , modify
label define Q1H3_1YL        0 "0 YRS OLD OR Inap.: Q1H1A NE 1"  , modify
forvalues n = 1/7 {
    label define Q1H3_2YL `n' "YEARS"  , modify
}
label define Q1H3_2YL        8 "DK"  , modify
label define Q1H3_2YL        9 "NA; REFUSED"  , modify
label define Q1H3_2YL        0 "0 YRS OLD OR Inap.: Q1H1A NE 1/ USED ONLY 1 ARRNGMNT"  , modify
forvalues n = 1/10 {
    label define Q1H3_3YL `n' "YEARS"  , modify
}
label define Q1H3_3YL       98 "DK"  , modify
label define Q1H3_3YL       99 "NA; REFUSED"  , modify
label define Q1H3_3YL        0 "0 YRS OLD OR Inap.: Q1H1A NE 1/ USED ONLY 2 ARRNGMNTS"  , modify
forvalues n = 1/12 {
    label define Q1H3_4YL `n' "YEARS"  , modify
}
label define Q1H3_4YL       98 "DK"  , modify
label define Q1H3_4YL       99 "NA; REFUSED"  , modify
label define Q1H3_4YL        0 "0 YRS OLD OR Inap.: Q1H1A NE 1/ USED ONLY 3 ARRNGMNTS"  , modify
forvalues n = 1/8 {
    label define Q1H3_5YL `n' "YEARS"  , modify
}
label define Q1H3_5YL       98 "DK"  , modify
label define Q1H3_5YL       99 "NA; REFUSED"  , modify
label define Q1H3_5YL        0 "Inap.: Q1H1A NE 1/ USED ONLY 4 ARRNGMNTS"  , modify
forvalues n = 1/8 {
    label define Q1H3_6YL `n' "YEARS"  , modify
}
label define Q1H3_6YL       98 "DK"  , modify
label define Q1H3_6YL       99 "NA; REFUSED"  , modify
label define Q1H3_6YL        0 "Inap.: Q1H1A NE 1/ USED ONLY 4 ARRNGMNTS"  , modify
forvalues n = 1/5 {
    label define Q1H3_7YL `n' "YEARS"  , modify
}
label define Q1H3_7YL        8 "DK"  , modify
label define Q1H3_7YL        9 "NA; REFUSED"  , modify
label define Q1H3_7YL        0 "Inap.: Q1H1A NE 1/ USED ONLY 6 ARRNGMNTS"  , modify
forvalues n = 2/4 {
    label define Q1H3_8YL `n' "YEARS"  , modify
}
label define Q1H3_8YL        8 "DK"  , modify
label define Q1H3_8YL        9 "NA; REFUSED"  , modify
label define Q1H3_8YL        0 "Inap.: Q1H1A NE 1/ USED ONLY 7 ARRNGMNTS"  , modify
forvalues n = 3/5 {
    label define Q1H3_9YL `n' "YEARS"  , modify
}
label define Q1H3_9YL        8 "DK"  , modify
label define Q1H3_9YL        9 "NA; REFUSED"  , modify
label define Q1H3_9YL        0 "Inap.: Q1H1A NE 1/ USED ONLY 8 ARRNGMNTS"  , modify

label define Q1H4_1L  ///
       1 "RELATIVE IN THE CHILD`=char(146)'S HOME"  ///
       2 "NON-RELATIVE IN CHILD`=char(146)'S HOME"  ///
       3 "CARE IN RELATIVES HOME"  ///
       4 "CARE IN NON-RELATIVES HOME"  ///
       5 "HEAD-START PROGRAM"  ///
       6 "PRESCHOOL OR CHILD CARE CENTER"  ///
       7 "BEFORE OR AFTER SCHOOL PROGRAM"  ///
       8 "CHILD CARES FOR SELF ALONE"  ///
      97 "OTHER"  ///
      98 "DK"  ///
      99 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1"

label define Q1H4_2L  ///
       1 "RELATIVE IN THE CHILD`=char(146)'S HOME"  ///
       2 "NON-RELATIVE IN CHILD`=char(146)'S HOME"  ///
       3 "CARE IN RELATIVES HOME"  ///
       4 "CARE IN NON-RELATIVES HOME"  ///
       5 "HEAD-START PROGRAM"  ///
       6 "PRESCHOOL OR CHILD CARE CENTER"  ///
       7 "BEFORE OR AFTER SCHOOL PROGRAM"  ///
       8 "CHILD CARES FOR SELF ALONE"  ///
      97 "OTHER"  ///
      98 "DK"  ///
      99 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 1 ARRNGMNT"

label define Q1H4_3L  ///
       1 "RELATIVE IN THE CHILD`=char(146)'S HOME"  ///
       2 "NON-RELATIVE IN CHILD`=char(146)'S HOME"  ///
       3 "CARE IN RELATIVES HOME"  ///
       4 "CARE IN NON-RELATIVES HOME"  ///
       5 "HEAD-START PROGRAM"  ///
       6 "PRESCHOOL OR CHILD CARE CENTER"  ///
       7 "BEFORE OR AFTER SCHOOL PROGRAM"  ///
       8 "CHILD CARES FOR SELF ALONE"  ///
      97 "OTHER"  ///
      98 "DK"  ///
      99 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 2 ARRNGMNTS"

label define Q1H4_4L  ///
       1 "RELATIVE IN THE CHILD`=char(146)'S HOME"  ///
       2 "NON-RELATIVE IN CHILD`=char(146)'S HOME"  ///
       3 "CARE IN RELATIVES HOME"  ///
       4 "CARE IN NON-RELATIVES HOME"  ///
       5 "HEAD-START PROGRAM"  ///
       6 "PRESCHOOL OR CHILD CARE CENTER"  ///
       7 "BEFORE OR AFTER SCHOOL PROGRAM"  ///
       8 "CHILD CARES FOR SELF ALONE"  ///
      97 "OTHER"  ///
      98 "DK"  ///
      99 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 3 ARRNGMNTS"

label define Q1H4_5L  ///
       1 "RELATIVE IN THE CHILD`=char(146)'S HOME"  ///
       2 "NON-RELATIVE IN CHILD`=char(146)'S HOME"  ///
       3 "CARE IN RELATIVES HOME"  ///
       4 "CARE IN NON-RELATIVES HOME"  ///
       5 "HEAD-START PROGRAM"  ///
       6 "PRESCHOOL OR CHILD CARE CENTER"  ///
       7 "BEFORE OR AFTER SCHOOL PROGRAM"  ///
       8 "CHILD CARES FOR SELF ALONE"  ///
      97 "OTHER"  ///
      98 "DK"  ///
      99 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 4 ARRNGMNTS"

label define Q1H4_6L  ///
       1 "RELATIVE IN THE CHILD`=char(146)'S HOME"  ///
       2 "NON-RELATIVE IN CHILD`=char(146)'S HOME"  ///
       3 "CARE IN RELATIVES HOME"  ///
       4 "CARE IN NON-RELATIVES HOME"  ///
       5 "HEAD-START PROGRAM"  ///
       6 "PRESCHOOL OR CHILD CARE CENTER"  ///
       7 "BEFORE OR AFTER SCHOOL PROGRAM"  ///
       8 "CHILD CARES FOR SELF ALONE"  ///
      97 "OTHER"  ///
      98 "DK"  ///
      99 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 5 ARRNGMNTS"

label define Q1H4_7L  ///
       1 "RELATIVE IN THE CHILD`=char(146)'S HOME"  ///
       2 "NON-RELATIVE IN CHILD`=char(146)'S HOME"  ///
       3 "CARE IN RELATIVES HOME"  ///
       4 "CARE IN NON-RELATIVES HOME"  ///
       5 "HEAD-START PROGRAM"  ///
       6 "PRESCHOOL OR CHILD CARE CENTER"  ///
       7 "BEFORE OR AFTER SCHOOL PROGRAM"  ///
       8 "CHILD CARES FOR SELF ALONE"  ///
      97 "OTHER"  ///
      98 "DK"  ///
      99 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 6 ARRNGMNTS"

label define Q1H4_8L  ///
       1 "RELATIVE IN THE CHILD`=char(146)'S HOME"  ///
       2 "NON-RELATIVE IN CHILD`=char(146)'S HOME"  ///
       3 "CARE IN RELATIVES HOME"  ///
       4 "CARE IN NON-RELATIVES HOME"  ///
       5 "HEAD-START PROGRAM"  ///
       6 "PRESCHOOL OR CHILD CARE CENTER"  ///
       7 "BEFORE OR AFTER SCHOOL PROGRAM"  ///
       8 "CHILD CARES FOR SELF ALONE"  ///
      97 "OTHER"  ///
      98 "DK"  ///
      99 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 7 ARRNGMNTS"

label define Q1H4_9L  ///
       1 "RELATIVE IN THE CHILD`=char(146)'S HOME"  ///
       2 "NON-RELATIVE IN CHILD`=char(146)'S HOME"  ///
       3 "CARE IN RELATIVES HOME"  ///
       4 "CARE IN NON-RELATIVES HOME"  ///
       5 "HEAD-START PROGRAM"  ///
       6 "PRESCHOOL OR CHILD CARE CENTER"  ///
       7 "BEFORE OR AFTER SCHOOL PROGRAM"  ///
       8 "CHILD CARES FOR SELF ALONE"  ///
      97 "OTHER"  ///
      98 "DK"  ///
      99 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 8 ARRNGMNTS"
forvalues n = 1/7 {
    label define Q1H5_2L `n' "DAYS PER WEEK"  , modify
}
label define Q1H5_2L        8 "DK"  , modify
label define Q1H5_2L        9 "NA; REFUSED"  , modify
label define Q1H5_2L        0 "Inap.: Q1H1A NE 1/ USED ONLY 1 ARRNGMNT"  , modify
forvalues n = 1/7 {
    label define Q1H5_3L `n' "DAYS PER WEEK"  , modify
}
label define Q1H5_3L        8 "DK"  , modify
label define Q1H5_3L        9 "NA; REFUSED"  , modify
label define Q1H5_3L        0 "Inap.: Q1H1A NE 1/ USED ONLY 2 ARRNGMNTS"  , modify
forvalues n = 1/7 {
    label define Q1H5_4L `n' "DAYS PER WEEK"  , modify
}
label define Q1H5_4L        8 "DK"  , modify
label define Q1H5_4L        9 "NA; REFUSED"  , modify
label define Q1H5_4L        0 "Inap.: Q1H1A NE 1/ USED ONLY 3 ARRNGMNTS"  , modify
forvalues n = 1/7 {
    label define Q1H5_5L `n' "DAYS PER WEEK"  , modify
}
label define Q1H5_5L        8 "DK"  , modify
label define Q1H5_5L        9 "NA; REFUSED"  , modify
label define Q1H5_5L        0 "Inap.: Q1H1A NE 1/ USED ONLY 4 ARRNGMNTS"  , modify
forvalues n = 3/5 {
    label define Q1H5_6L `n' "DAYS PER WEEK"  , modify
}
label define Q1H5_6L        8 "DK"  , modify
label define Q1H5_6L        9 "NA; REFUSED"  , modify
label define Q1H5_6L        0 "Inap.: Q1H1A NE 1/ USED ONLY 5 ARRNGMNTS"  , modify
forvalues n = 2/6 {
    label define Q1H5_7L `n' "DAYS PER WEEK"  , modify
}
label define Q1H5_7L        8 "DK"  , modify
label define Q1H5_7L        9 "NA; REFUSED"  , modify
label define Q1H5_7L        0 "Inap.: Q1H1A NE 1/ USED ONLY 6 ARRNGMNTS"  , modify
forvalues n = 3/5 {
    label define Q1H5_8L `n' "DAYS PER WEEK"  , modify
}
label define Q1H5_8L        8 "DK"  , modify
label define Q1H5_8L        9 "NA; REFUSED"  , modify
label define Q1H5_8L        0 "Inap.: Q1H1A NE 1/ USED ONLY 7 ARRNGMNTS"  , modify
forvalues n = 2/5 {
    label define Q1H5_9L `n' "DAYS PER WEEK"  , modify
}
label define Q1H5_9L        8 "DK"  , modify
label define Q1H5_9L        9 "NA; REFUSED"  , modify
label define Q1H5_9L        0 "Inap.: Q1H1A NE 1/ USED ONLY 8 ARRNGMNTS"  , modify
forvalues n = 5/84 {
    label define Q1H6_7L `n' "HOURS PER WEEK"  , modify
}
label define Q1H6_7L       98 "DK"  , modify
label define Q1H6_7L       99 "NA; REFUSED"  , modify
label define Q1H6_7L        0 "Inap.: Q1H1A NE 1/ USED ONLY 6 ARRNGMNTS"  , modify
forvalues n = 12/30 {
    label define Q1H6_9L `n' "HOURS PER WEEK"  , modify
}
label define Q1H6_9L       98 "DK"  , modify
label define Q1H6_9L       99 "NA; REFUSED"  , modify
label define Q1H6_9L        0 "Inap.: Q1H1A NE 1/ USED ONLY 8 ARRNGMNTS"  , modify

label define Q1H7A_1L  ///
       1 "PER HOUR"  ///
       2 "PER DAY"  ///
       3 "PER WEEK"  ///
       4 "EVERY TWO WEEKS"  ///
       5 "EVERY MONTH"  ///
       6 "EVERY YEAR"  ///
       7 "OTHER"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H7_1=0"

label define Q1H7A_2L  ///
       1 "PER HOUR"  ///
       2 "PER DAY"  ///
       3 "PER WEEK"  ///
       4 "EVERY TWO WEEKS"  ///
       5 "EVERY MONTH"  ///
       6 "EVERY YEAR"  ///
       7 "OTHER"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H7A_2=0"

label define Q1H7A_3L  ///
       1 "PER HOUR"  ///
       2 "PER DAY"  ///
       3 "PER WEEK"  ///
       4 "EVERY TWO WEEKS"  ///
       5 "EVERY MONTH"  ///
       6 "EVERY YEAR"  ///
       7 "OTHER"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H7_3=0"

label define Q1H7A_4L  ///
       1 "PER HOUR"  ///
       2 "PER DAY"  ///
       3 "PER WEEK"  ///
       4 "EVERY TWO WEEKS"  ///
       5 "EVERY MONTH"  ///
       6 "EVERY YEAR"  ///
       7 "OTHER"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H7_4=0"

label define Q1H7A_5L  ///
       1 "PER HOUR"  ///
       2 "PER DAY"  ///
       3 "PER WEEK"  ///
       4 "EVERY TWO WEEKS"  ///
       5 "EVERY MONTH"  ///
       6 "EVERY YEAR"  ///
       7 "OTHER"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: IN Q1H7_5=0"

label define Q1H7A_6L  ///
       1 "PER HOUR"  ///
       2 "PER DAY"  ///
       3 "PER WEEK"  ///
       4 "EVERY TWO WEEKS"  ///
       5 "EVERY MONTH"  ///
       6 "EVERY YEAR"  ///
       7 "OTHER"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H7_6=0"

label define Q1H7A_7L  ///
       1 "PER HOUR"  ///
       2 "PER DAY"  ///
       3 "PER WEEK"  ///
       4 "EVERY TWO WEEKS"  ///
       5 "EVERY MONTH"  ///
       6 "EVERY YEAR"  ///
       7 "OTHER"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 6 ARRNGMNTS"

label define Q1H7A_8L  ///
       1 "PER HOUR"  ///
       2 "PER DAY"  ///
       3 "PER WEEK"  ///
       4 "EVERY TWO WEEKS"  ///
       5 "EVERY MONTH"  ///
       6 "EVERY YEAR"  ///
       7 "OTHER"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 7 ARRNGMNTS"

label define Q1H7A_9L  ///
       1 "PER HOUR"  ///
       2 "PER DAY"  ///
       3 "PER WEEK"  ///
       4 "EVERY TWO WEEKS"  ///
       5 "EVERY MONTH"  ///
       6 "EVERY YEAR"  ///
       7 "OTHER"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 8 ARRNGMNTS"

label define Q1H7B_1L  ///
       1 "CHILD ONLY"  ///
       2 "OTHER CHILDREN IN HOUSEHOLD"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H7_1=0"

label define Q1H7B_2L  ///
       1 "CHILD ONLY"  ///
       2 "OTHER CHILDREN IN HOUSEHOLD"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H7A_2=0"

label define Q1H7B_3L  ///
       1 "CHILD ONLY"  ///
       2 "OTHER CHILDREN IN HOUSEHOLD"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H7_3=0"

label define Q1H7B_4L  ///
       1 "CHILD ONLY"  ///
       2 "OTHER CHILDREN IN HOUSEHOLD"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H7_4=0"

label define Q1H7B_5L  ///
       1 "CHILD ONLY"  ///
       2 "OTHER CHILDREN IN HOUSEHOLD"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: IN Q1H7_5=0"

label define Q1H7B_6L  ///
       1 "CHILD ONLY"  ///
       2 "OTHER CHILDREN IN HOUSEHOLD"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H7_6=0"

label define Q1H7B_7L  ///
       1 "CHILD ONLY"  ///
       2 "OTHER CHILDREN IN HOUSEHOLD"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 6 ARRNGMNTS"

label define Q1H7B_8L  ///
       1 "CHILD ONLY"  ///
       2 "OTHER CHILDREN IN HOUSEHOLD"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 7 ARRNGMNTS"

label define Q1H7B_9L  ///
       1 "CHILD ONLY"  ///
       2 "OTHER CHILDREN IN HOUSEHOLD"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 8 ARRNGMNTS"
forvalues n = 1/5 {
    label define Q1H7C_1L `n' "NUMBER OF CHILDREN"  , modify
}
label define Q1H7C_1L        8 "DK"  , modify
label define Q1H7C_1L        9 "NA; REFUSED"  , modify
label define Q1H7C_1L        0 "Inap.: Q1H7_1=0/ Q1H7B_1 NE 2"  , modify
forvalues n = 1/3 {
    label define Q1H7C_2L `n' "NUMBER OF CHILDREN"  , modify
}
label define Q1H7C_2L        8 "DK"  , modify
label define Q1H7C_2L        9 "NA; REFUSED"  , modify
label define Q1H7C_2L        0 "Inap.: Q1H7A_2=0/ Q1H7B_2 NE 2"  , modify
forvalues n = 1/2 {
    label define Q1H7C_3L `n' "NUMBER OF CHILDREN"  , modify
}
label define Q1H7C_3L        8 "DK"  , modify
label define Q1H7C_3L        9 "NA; REFUSED"  , modify
label define Q1H7C_3L        0 "Inap.: Q1H7_3=0/ Q1H7B_3 NE 2"  , modify
forvalues n = 1/2 {
    label define Q1H7C_4L `n' "NUMBER OF CHILDREN"  , modify
}
label define Q1H7C_4L        8 "DK"  , modify
label define Q1H7C_4L        9 "NA; REFUSED"  , modify
label define Q1H7C_4L        0 "Inap.: Q1H7_4=0/ Q1H7B_4 NE 2"  , modify
forvalues n = 1/2 {
    label define Q1H7C_5L `n' "NUMBER OF CHILDREN"  , modify
}
label define Q1H7C_5L        8 "DK"  , modify
label define Q1H7C_5L        9 "NA; REFUSED"  , modify
label define Q1H7C_5L        0 "Inap.: IN Q1H7_=0/ Q1H7B_5 NE 2"  , modify
forvalues n = 1/2 {
    label define Q1H7C_6L `n' "NUMBER OF CHILDREN"  , modify
}
label define Q1H7C_6L        8 "DK"  , modify
label define Q1H7C_6L        9 "NA; REFUSED"  , modify
label define Q1H7C_6L        0 "Inap.: Q1H7_6=0/ Q1H7B_6 NE 2"  , modify

label define Q1H7C_7L  ///
       2 "NUMBER OF CHILDREN"  ///
       8 "DK"  ///
       9 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 6 ARRNGMNTS/ Q1H7B_7 NE 2"
forvalues n = 1/12 {
    label define Q1H8_1YL `n' "YEARS"  , modify
}
label define Q1H8_1YL       96 "ARRNGMNT #1 NOT ENDED YET"  , modify
label define Q1H8_1YL       98 "DK"  , modify
label define Q1H8_1YL       99 "NA; REFUSED"  , modify
label define Q1H8_1YL        0 "0 YRS OLD OR Inap.: Q1H1A NE 1"  , modify
forvalues n = 1/11 {
    label define Q1H8_2YL `n' "YEARS"  , modify
}
label define Q1H8_2YL       96 "ARRNGMNT #2 NOT ENDED YET"  , modify
label define Q1H8_2YL       98 "DK"  , modify
label define Q1H8_2YL       99 "NA; REFUSED"  , modify
label define Q1H8_2YL        0 "0 YRS OLD OR Inap.: Q1H1A NE 1/ USED ONLY 1 ARRNGMNT"  , modify
forvalues n = 1/11 {
    label define Q1H8_3YL `n' "YEARS"  , modify
}
label define Q1H8_3YL       96 "ARRANGMENT #3 NOT ENDED YET"  , modify
label define Q1H8_3YL       98 "DK"  , modify
label define Q1H8_3YL       99 "NA; REFUSED"  , modify
label define Q1H8_3YL        0 "0 YRS OLD OR Inap.: Q1H1A NE 1/ USED ONLY 2 ARRNGMNTS"  , modify
forvalues n = 1/8 {
    label define Q1H8_4YL `n' "YEARS"  , modify
}
label define Q1H8_4YL       96 "ARRNGMNT #4 NOT ENDED YET"  , modify
label define Q1H8_4YL       98 "DK"  , modify
label define Q1H8_4YL       99 "NA; REFUSED"  , modify
label define Q1H8_4YL        0 "0 YRS OLD OR Inap.: Q1H1A NE 1/ USED ONLY 3 ARRNGMNTS"  , modify
forvalues n = 1/8 {
    label define Q1H8_5YL `n' "YEARS"  , modify
}
label define Q1H8_5YL       96 "ARRNGMNT #5 NOT ENDED YET"  , modify
label define Q1H8_5YL       98 "DK"  , modify
label define Q1H8_5YL       99 "NA; REFUSED"  , modify
label define Q1H8_5YL        0 "0 YRS OLD OR Inap.: Q1H1A NE 1/ USED ONLY 4 ARRNGMNTS"  , modify
forvalues n = 1/13 {
    label define Q1H8_6YL `n' "YEARS"  , modify
}
label define Q1H8_6YL       96 "ARRNGMNT #6 NOT ENDED YET"  , modify
label define Q1H8_6YL       98 "DK"  , modify
label define Q1H8_6YL       99 "NA; REFUSED"  , modify
label define Q1H8_6YL        0 "Inap.: Q1H1A NE 1/ USED ONLY 5 ARRNGMNTS"  , modify
forvalues n = 2/5 {
    label define Q1H8_7YL `n' "YEARS"  , modify
}
label define Q1H8_7YL        8 "DK"  , modify
label define Q1H8_7YL        9 "NA; REFUSED"  , modify
label define Q1H8_7YL        0 "Inap.: Q1H1A NE 1/ USED ONLY 6 ARRNGMNTS"  , modify
forvalues n = 3/5 {
    label define Q1H8_8YL `n' "YEARS"  , modify
}
label define Q1H8_8YL        8 "DK"  , modify
label define Q1H8_8YL        9 "NA; REFUSED"  , modify
label define Q1H8_8YL        0 "Inap.: Q1H1A NE 1/ USED ONLY 7 ARRNGMNTS"  , modify
forvalues n = 3/5 {
    label define Q1H8_9YL `n' "YEARS"  , modify
}
label define Q1H8_9YL        8 "DK"  , modify
label define Q1H8_9YL        9 "NA; REFUSED"  , modify
label define Q1H8_9YL        0 "Inap.: Q1H1A NE 1/ USED ONLY 8 ARRNGMNTS"  , modify

label define Q1H9_1L  ///
       1 "WANT TO STAY W/ CHILDREN"  ///
       2 "CARE NO LONGER AVAILABLE"  ///
       3 "COULDN`=char(146)'T AFFORD"  ///
       4 "CHILD TOO OLD"  ///
       5 "CHILD UNHAPPY"  ///
       6 "STOPPED WORKING"  ///
       7 "CHANGED JOBS"  ///
       8 "MOVED"  ///
       9 "SCHOOL YEAR STARTED/ENDED"  ///
      10 "STAY HOME W/ OTHER DEPEND"  ///
      11 "ARRANGMT. WAS TEMPORARY"  ///
      12 "OTHER SPECIFY"  ///
      96 "ARRNGMNT #1 NOT ENDED YET"  ///
      98 "DK"  ///
      99 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1"

label define Q1H9_2L  ///
       1 "WANT TO STAY W/ CHILDREN"  ///
       2 "CARE NO LONGER AVAILABLE"  ///
       3 "COULDN`=char(146)'T AFFORD"  ///
       4 "CHILD TOO OLD"  ///
       5 "CHILD UNHAPPY"  ///
       6 "STOPPED WORKING"  ///
       7 "CHANGED JOBS"  ///
       8 "MOVED"  ///
       9 "SCHOOL YEAR STARTED/ENDED"  ///
      10 "STAY HOME W/ OTHER DEPEND"  ///
      11 "ARRANGMT. WAS TEMPORARY"  ///
      12 "OTHER SPECIFY"  ///
      96 "ARRNGMNT #2 NOT ENDED YET"  ///
      98 "DK"  ///
      99 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 1 ARRNGMNT"

label define Q1H9_3L  ///
       1 "WANT TO STAY W/ CHILDREN"  ///
       2 "CARE NO LONGER AVAILABLE"  ///
       3 "COULDN`=char(146)'T AFFORD"  ///
       4 "CHILD TOO OLD"  ///
       5 "CHILD UNHAPPY"  ///
       6 "STOPPED WORKING"  ///
       7 "CHANGED JOBS"  ///
       8 "MOVED"  ///
       9 "SCHOOL YEAR STARTED/ENDED"  ///
      10 "STAY HOME W/ OTHER DEPEND"  ///
      11 "ARRANGMT. WAS TEMPORARY"  ///
      12 "OTHER SPECIFY"  ///
      96 "ARRNGMNT #3 NOT ENDED YET"  ///
      98 "DK"  ///
      99 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 2 ARRNGMNTS"

label define Q1H9_4L  ///
       1 "WANT TO STAY W/ CHILDREN"  ///
       2 "CARE NO LONGER AVAILABLE"  ///
       3 "COULDN`=char(146)'T AFFORD"  ///
       4 "CHILD TOO OLD"  ///
       5 "CHILD UNHAPPY"  ///
       6 "STOPPED WORKING"  ///
       7 "CHANGED JOBS"  ///
       8 "MOVED"  ///
       9 "SCHOOL YEAR STARTED/ENDED"  ///
      10 "STAY HOME W/ OTHER DEPEND"  ///
      11 "ARRANGMT. WAS TEMPORARY"  ///
      12 "OTHER SPECIFY"  ///
      96 "ARRNGMNT #4 NOT ENDED YET"  ///
      98 "DK"  ///
      99 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 3 ARRNGMNTS"

label define Q1H9_5L  ///
       1 "WANT TO STAY W/ CHILDREN"  ///
       2 "CARE NO LONGER AVAILABLE"  ///
       3 "COULDN`=char(146)'T AFFORD"  ///
       4 "CHILD TOO OLD"  ///
       5 "CHILD UNHAPPY"  ///
       6 "STOPPED WORKING"  ///
       7 "CHANGED JOBS"  ///
       8 "MOVED"  ///
       9 "SCHOOL YEAR STARTED/ENDED"  ///
      10 "STAY HOME W/ OTHER DEPEND"  ///
      11 "ARRANGMT. WAS TEMPORARY"  ///
      12 "OTHER SPECIFY"  ///
      96 "ARRNGMNT #5 NOT ENDED YET"  ///
      98 "DK"  ///
      99 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 4 ARRNGMNTS"

label define Q1H9_6L  ///
       1 "WANT TO STAY W/ CHILDREN"  ///
       2 "CARE NO LONGER AVAILABLE"  ///
       3 "COULDN`=char(146)'T AFFORD"  ///
       4 "CHILD TOO OLD"  ///
       5 "CHILD UNHAPPY"  ///
       6 "STOPPED WORKING"  ///
       7 "CHANGED JOBS"  ///
       8 "MOVED"  ///
       9 "SCHOOL YEAR STARTED/ENDED"  ///
      10 "STAY HOME W/ OTHER DEPEND"  ///
      11 "ARRANGMT. WAS TEMPORARY"  ///
      12 "OTHER SPECIFY"  ///
      96 "ARRNGMNT #6 NOT ENDED YET"  ///
      98 "DK"  ///
      99 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 5 ARRNGMNTS"

label define Q1H9_7L  ///
       1 "WANT TO STAY W/ CHILDREN"  ///
       2 "CARE NO LONGER AVAILABLE"  ///
       3 "COULDN`=char(146)'T AFFORD"  ///
       4 "CHILD TOO OLD"  ///
       5 "CHILD UNHAPPY"  ///
       6 "STOPPED WORKING"  ///
       7 "CHANGED JOBS"  ///
       8 "MOVED"  ///
       9 "SCHOOL YEAR STARTED/ENDED"  ///
      10 "STAY HOME W/ OTHER DEPEND"  ///
      11 "ARRANGMT. WAS TEMPORARY"  ///
      12 "OTHER SPECIFY"  ///
      98 "DK"  ///
      99 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 6 ARRNGMNTS"

label define Q1H9_8L  ///
       1 "WANT TO STAY W/ CHILDREN"  ///
       2 "CARE NO LONGER AVAILABLE"  ///
       3 "COULDN`=char(146)'T AFFORD"  ///
       4 "CHILD TOO OLD"  ///
       5 "CHILD UNHAPPY"  ///
       6 "STOPPED WORKING"  ///
       7 "CHANGED JOBS"  ///
       8 "MOVED"  ///
       9 "SCHOOL YEAR STARTED/ENDED"  ///
      10 "STAY HOME W/ OTHER DEPEND"  ///
      11 "ARRANGMT. WAS TEMPORARY"  ///
      12 "OTHER SPECIFY"  ///
      98 "DK"  ///
      99 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 7 ARRNGMNTS"

label define Q1H9_9L  ///
       1 "WANT TO STAY W/ CHILDREN"  ///
       2 "CARE NO LONGER AVAILABLE"  ///
       3 "COULDN`=char(146)'T AFFORD"  ///
       4 "CHILD TOO OLD"  ///
       5 "CHILD UNHAPPY"  ///
       6 "STOPPED WORKING"  ///
       7 "CHANGED JOBS"  ///
       8 "MOVED"  ///
       9 "SCHOOL YEAR STARTED/ENDED"  ///
      10 "STAY HOME W/ OTHER DEPEND"  ///
      11 "ARRANGMT. WAS TEMPORARY"  ///
      12 "OTHER SPECIFY"  ///
      98 "DK"  ///
      99 "NA; REFUSED"  ///
       0 "Inap.: Q1H1A NE 1/ USED ONLY 8 ARRNGMNTS"

label values DEMREL97   DEMREL97L
label values EMSAREL97  EMSAREL97L
label values ER30000    ER30000L
label values ER33402    ER33402L
label values ER33403    ER33403L
label values PCGCHREL97 PCGCHREL97L
label values PDAREL97   PDAREL97L
label values Q12A28     Q12A28L
label values Q12A28B    Q12A28BL
label values Q12A28C    Q12A28CL
label values Q12A28D    Q12A28DL
label values Q12A2A     Q12A2AL
label values Q12A2B     Q12A2BL
label values Q12A2C     Q12A2CL
label values Q12A2D     Q12A2DL
label values Q12A2E     Q12A2EL
label values Q12A2F     Q12A2FL
label values Q12A2G     Q12A2GL
label values Q12A2H     Q12A2HL
label values Q12A2I     Q12A2IL
label values Q12A2J     Q12A2JL
label values Q12A2K     Q12A2KL
label values Q12A2L     Q12A2LL
label values Q12A2M     Q12A2ML
label values Q12A2N     Q12A2NL
label values Q13A21     Q13A21L
label values Q13A21B    Q13A21BL
label values Q13A21C    Q13A21CL
label values Q13A21D    Q13A21DL
label values Q1B17      Q1B17L
label values Q1G1       Q1G1L
label values Q1G11D     Q1G11DL
label values Q1G22      Q1G22L
label values Q1H10_1    Q1H10_1L
label values Q1H10_2    Q1H10_2L
label values Q1H10_3    Q1H10_3L
label values Q1H10_4    Q1H10_4L
label values Q1H10_5    Q1H10_5L
label values Q1H10_6    Q1H10_6L
label values Q1H10_7    Q1H10_7L
label values Q1H10_8    Q1H10_8L
label values Q1H10_9    Q1H10_9L
label values Q1H13      Q1H13L
label values Q1H14      Q1H14L
label values Q1H15      Q1H15L
label values Q1H16      Q1H16L
label values Q1H17      Q1H17L
label values Q1H18      Q1H18L
label values Q1H20W     Q1H20WL
label values Q1H21A     Q1H21AL
label values Q1H22      Q1H22L
label values Q1H22A     Q1H22AL
label values Q1H23      Q1H23L
label values Q1H24      Q1H24L
label values Q1H26W     Q1H26WL
label values Q1H27A     Q1H27AL
label values Q1H28      Q1H28L
label values Q1H28A     Q1H28AL
label values Q1H29      Q1H29L
label values Q1H2_1     Q1H2_1L
label values Q1H2_2     Q1H2_2L
label values Q1H2_3     Q1H2_3L
label values Q1H2_4     Q1H2_4L
label values Q1H2_5     Q1H2_5L
label values Q1H2_6     Q1H2_6L
label values Q1H2_7     Q1H2_7L
label values Q1H2_8     Q1H2_8L
label values Q1H2_9     Q1H2_9L
label values Q1H30      Q1H30L
label values Q1H32M     Q1H32ML
label values Q1H33A     Q1H33AL
label values Q1H34      Q1H34L
label values Q1H34A     Q1H34AL
label values Q1H35      Q1H35L
label values Q1H36      Q1H36L
label values Q1H37      Q1H37L
label values Q1H38      Q1H38L
label values Q1H39      Q1H39L
label values Q1H3_1Y    Q1H3_1YL
label values Q1H3_2Y    Q1H3_2YL
label values Q1H3_3Y    Q1H3_3YL
label values Q1H3_4Y    Q1H3_4YL
label values Q1H3_5Y    Q1H3_5YL
label values Q1H3_6Y    Q1H3_6YL
label values Q1H3_7Y    Q1H3_7YL
label values Q1H3_8Y    Q1H3_8YL
label values Q1H3_9Y    Q1H3_9YL
label values Q1H4_1     Q1H4_1L
label values Q1H4_2     Q1H4_2L
label values Q1H4_3     Q1H4_3L
label values Q1H4_4     Q1H4_4L
label values Q1H4_5     Q1H4_5L
label values Q1H4_6     Q1H4_6L
label values Q1H4_7     Q1H4_7L
label values Q1H4_8     Q1H4_8L
label values Q1H4_9     Q1H4_9L
label values Q1H5_2     Q1H5_2L
label values Q1H5_3     Q1H5_3L
label values Q1H5_4     Q1H5_4L
label values Q1H5_5     Q1H5_5L
label values Q1H5_6     Q1H5_6L
label values Q1H5_7     Q1H5_7L
label values Q1H5_8     Q1H5_8L
label values Q1H5_9     Q1H5_9L
label values Q1H6_7     Q1H6_7L
label values Q1H6_9     Q1H6_9L
label values Q1H7A_1    Q1H7A_1L
label values Q1H7A_2    Q1H7A_2L
label values Q1H7A_3    Q1H7A_3L
label values Q1H7A_4    Q1H7A_4L
label values Q1H7A_5    Q1H7A_5L
label values Q1H7A_6    Q1H7A_6L
label values Q1H7A_7    Q1H7A_7L
label values Q1H7A_8    Q1H7A_8L
label values Q1H7A_9    Q1H7A_9L
label values Q1H7B_1    Q1H7B_1L
label values Q1H7B_2    Q1H7B_2L
label values Q1H7B_3    Q1H7B_3L
label values Q1H7B_4    Q1H7B_4L
label values Q1H7B_5    Q1H7B_5L
label values Q1H7B_6    Q1H7B_6L
label values Q1H7B_7    Q1H7B_7L
label values Q1H7B_8    Q1H7B_8L
label values Q1H7B_9    Q1H7B_9L
label values Q1H7C_1    Q1H7C_1L
label values Q1H7C_2    Q1H7C_2L
label values Q1H7C_3    Q1H7C_3L
label values Q1H7C_4    Q1H7C_4L
label values Q1H7C_5    Q1H7C_5L
label values Q1H7C_6    Q1H7C_6L
label values Q1H7C_7    Q1H7C_7L
label values Q1H8_1Y    Q1H8_1YL
label values Q1H8_2Y    Q1H8_2YL
label values Q1H8_3Y    Q1H8_3YL
label values Q1H8_4Y    Q1H8_4YL
label values Q1H8_5Y    Q1H8_5YL
label values Q1H8_6Y    Q1H8_6YL
label values Q1H8_7Y    Q1H8_7YL
label values Q1H8_8Y    Q1H8_8YL
label values Q1H8_9Y    Q1H8_9YL
label values Q1H9_1     Q1H9_1L
label values Q1H9_2     Q1H9_2L
label values Q1H9_3     Q1H9_3L
label values Q1H9_4     Q1H9_4L
label values Q1H9_5     Q1H9_5L
label values Q1H9_6     Q1H9_6L
label values Q1H9_7     Q1H9_7L
label values Q1H9_8     Q1H9_8L
label values Q1H9_9     Q1H9_9L
