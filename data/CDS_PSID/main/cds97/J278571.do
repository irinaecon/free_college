#delimit ;
*  PSID DATA CENTER *****************************************************
   JOBID            : 278571                            
   DATA_DOMAIN      : CDSi                              
   USER_WHERE       : NULL                              
   FILE_TYPE        : All Individuals from CDS Families 
   OUTPUT_DATA_TYPE : ASCII                             
   STATEMENTS       : do                                
   CODEBOOK_TYPE    : PDF                               
   N_OF_VARIABLES   : 13                                
   N_OF_OBSERVATIONS: 9770                              
   MAX_REC_LENGTH   : 28                                
   DATE & TIME      : June 13, 2020 @ 3:11:36
*************************************************************************
;

infix
      ER30000              1 - 1           ER30001              2 - 5           ER30002              6 - 8     
      ER10001              9 - 9           ER10012             10 - 11          ER12222             12 - 13    
      ER12223             14 - 15          ER33401             16 - 20          ER33402             21 - 22    
      ER33403             23 - 24          ER33415             25 - 26          CHREL97             27 - 27    
      Q1B17               28 - 28    
using J278571.txt, clear 
;
label variable ER30000       "RELEASE NUMBER"                           ;
label variable ER30001       "1968 INTERVIEW NUMBER"                    ;
label variable ER30002       "PERSON NUMBER                         68" ;
label variable ER10001       "RELEASE NUMBER"                           ;
label variable ER10012       "# CHILDREN IN FU"                         ;
label variable ER12222       "COMPLETED ED-HD"                          ;
label variable ER12223       "COMPLETED ED-WF"                          ;
label variable ER33401       "1997 INTERVIEW NUMBER"                    ;
label variable ER33402       "SEQUENCE NUMBER                       97" ;
label variable ER33403       "RELATION TO HEAD                      97" ;
label variable ER33415       "YEARS COMPLETED EDUCATION             97" ;
label variable CHREL97       "PCG CHILD FILE RELEASE NUMBER 97"         ;
label variable Q1B17         "AGE OF CHILD 97"                          ;
