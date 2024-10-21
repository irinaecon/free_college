#delimit ;
*  PSID DATA CENTER *****************************************************
   JOBID            : 278572                            
   DATA_DOMAIN      : CDSi                              
   USER_WHERE       : NULL                              
   FILE_TYPE        : All Individuals from CDS Families 
   OUTPUT_DATA_TYPE : ASCII                             
   STATEMENTS       : do                                
   CODEBOOK_TYPE    : PDF                               
   N_OF_VARIABLES   : 23                                
   N_OF_OBSERVATIONS: 8379                              
   MAX_REC_LENGTH   : 56                                
   DATE & TIME      : June 13, 2020 @ 3:17:21
*************************************************************************
;

infix
      ER30000              1 - 1           ER30001              2 - 5           ER30002              6 - 8     
      ER17001              9 - 9           ER17016             10 - 11          ER20457             12 - 13    
      ER20458             14 - 15          ER33601             16 - 19          ER33602             20 - 21    
      ER33603             22 - 23          ER33616             24 - 25          DEMREL02            26 - 26    
      CH02PRWT            27 - 32          CHREL               33 - 33          Q21IWAGE            34 - 38    
      ER21001             39 - 39          ER21020             40 - 41          ER24148             42 - 43    
      ER24149             44 - 45          ER33701             46 - 50          ER33702             51 - 52    
      ER33703             53 - 54          ER33716             55 - 56    
using J278572.txt, clear 
;
label variable ER30000       "RELEASE NUMBER"                           ;
label variable ER30001       "1968 INTERVIEW NUMBER"                    ;
label variable ER30002       "PERSON NUMBER                         68" ;
label variable ER17001       "RELEASE NUMBER"                           ;
label variable ER17016       "# CHILDREN IN FU"                         ;
label variable ER20457       "COMPLETED ED-HD"                          ;
label variable ER20458       "COMPLETED ED-WF"                          ;
label variable ER33601       "2001 INTERVIEW NUMBER"                    ;
label variable ER33602       "SEQUENCE NUMBER                       01" ;
label variable ER33603       "RELATION TO HEAD                      01" ;
label variable ER33616       "YEARS COMPLETED EDUCATION             01" ;
label variable DEMREL02      "DEMOG FILE RELEASE NUMBER 02"             ;
label variable CH02PRWT      "CHILD LEVEL WEIGHT 02"                    ;
label variable CHREL         "PCG CHILD FILE RELEASE NUMBER 02"         ;
label variable Q21IWAGE      "CHILD AGE AT TIME OF PCG IW - YEARS 02"   ;
label variable ER21001       "RELEASE NUMBER"                           ;
label variable ER21020       "# CHILDREN IN FU"                         ;
label variable ER24148       "COMPLETED ED-HD"                          ;
label variable ER24149       "COMPLETED ED-WF"                          ;
label variable ER33701       "2003 INTERVIEW NUMBER"                    ;
label variable ER33702       "SEQUENCE NUMBER                       03" ;
label variable ER33703       "RELATION TO HEAD                      03" ;
label variable ER33716       "YEARS COMPLETED EDUCATION             03" ;
