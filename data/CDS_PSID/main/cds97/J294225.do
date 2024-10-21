#delimit ;
*  PSID DATA CENTER *****************************************************
   JOBID            : 294225                            
   DATA_DOMAIN      : CDS                               
   USER_WHERE       : NULL                              
   FILE_TYPE        : CDS Kids Only                     
   OUTPUT_DATA_TYPE : ASCII                             
   STATEMENTS       : do                                
   CODEBOOK_TYPE    : PDF                               
   N_OF_VARIABLES   : 15                                
   N_OF_OBSERVATIONS: 3563                              
   MAX_REC_LENGTH   : 28                                
   DATE & TIME      : June 11, 2021 @ 8:31:05
*************************************************************************
;

infix
      KIDS                 1 - 1           KID97                2 - 4           ER30001              5 - 8     
      ER30002              9 - 11          ER33401             12 - 16          ER33402             17 - 18    
      ER33403             19 - 20          PCGCHREL97          21 - 21          Q1G10               22 - 22    
      Q1G10A              23 - 23          Q1G11               24 - 24          Q1G11BA             25 - 25    
      PDAREL97            26 - 26          Q13A20F             27 - 27          Q13A20G             28 - 28    
using J294225.txt, clear 
;
label variable KIDS            "Sum of All KID Flags"                     ;
label variable KID97           "KID1997 = 1 if exists, else missing"      ;
label variable ER30001         "1968 INTERVIEW NUMBER"                    ;
label variable ER30002         "PERSON NUMBER                         68" ;
label variable ER33401         "1997 INTERVIEW NUMBER"                    ;
label variable ER33402         "SEQUENCE NUMBER                       97" ;
label variable ER33403         "RELATION TO HEAD                      97" ;
label variable PCGCHREL97      "PCG CHILD FILE RELEASE NUMBER 97"         ;
label variable Q1G10           "PRIV OR PUBL SCH 97"                      ;
label variable Q1G10A          "EVER ATTEND PRIV SCH 97"                  ;
label variable Q1G11           "TYPE ATTEND 97"                           ;
label variable Q1G11BA         "ATTEND PRIV SCH K'GARTN 97"               ;
label variable PDAREL97        "PDA 1997 RELEASE NUMBER"                  ;
label variable Q13A20F         "PUBLIC SCHOOL RELIGIOUS"                  ;
label variable Q13A20G         "PUBLIC SCHOOL NON-RELIGIOUS"              ;
