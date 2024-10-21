#delimit ;
*  PSID DATA CENTER *****************************************************
   JOBID            : 278573                            
   DATA_DOMAIN      : CDSi                              
   USER_WHERE       : NULL                              
   FILE_TYPE        : All Individuals from CDS Families 
   OUTPUT_DATA_TYPE : ASCII                             
   STATEMENTS       : do                                
   CODEBOOK_TYPE    : PDF                               
   N_OF_VARIABLES   : 13                                
   N_OF_OBSERVATIONS: 5301                              
   MAX_REC_LENGTH   : 32                                
   DATE & TIME      : June 13, 2020 @ 3:19:14
*************************************************************************
;

infix
      ER30000              1 - 1           ER30001              2 - 5           ER30002              6 - 8     
      ER36001              9 - 9           ER36020             10 - 11          ER41037             12 - 13    
      ER41038             14 - 15          ER33901             16 - 20          ER33902             21 - 22    
      ER33903             23 - 24          ER33917             25 - 26          PCHREL07            27 - 27    
      Q31IWAGE            28 - 32    
using J278573.txt, clear 
;
label variable ER30000       "RELEASE NUMBER"                           ;
label variable ER30001       "1968 INTERVIEW NUMBER"                    ;
label variable ER30002       "PERSON NUMBER                         68" ;
label variable ER36001       "RELEASE NUMBER"                           ;
label variable ER36020       "# CHILDREN IN FU"                         ;
label variable ER41037       "COMPLETED ED-HD"                          ;
label variable ER41038       "COMPLETED ED-WF"                          ;
label variable ER33901       "2007 INTERVIEW NUMBER"                    ;
label variable ER33902       "SEQUENCE NUMBER                       07" ;
label variable ER33903       "RELATION TO HEAD                      07" ;
label variable ER33917       "YEARS COMPLETED EDUCATION             07" ;
label variable PCHREL07      "PCG CHILD FILE RELEASE NUMBER 07"         ;
label variable Q31IWAGE      "CHILD AGE AT TIME OF PCG IW - YEARS 07"   ;
