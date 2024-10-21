#delimit ;
*  PSID DATA CENTER *****************************************************
   JOBID            : 301635                            
   DATA_DOMAIN      : TD                                
   USER_WHERE       : NULL                              
   FILE_TYPE        : NULL                              
   OUTPUT_DATA_TYPE : ASCII                             
   STATEMENTS       : do                                
   CODEBOOK_TYPE    : PDF                               
   N_OF_VARIABLES   : 24                                
   N_OF_OBSERVATIONS: 131060                            
   MAX_REC_LENGTH   : 51                                
   DATE & TIME      : January 10, 2022 @ 12:28:43
*************************************************************************
;

infix
      ER30001              1 - 4           ER30002              5 - 7           ER33401              8 - 12    
      ER33402             13 - 14          ER33403             15 - 16          TDREL97             17 - 17    
      T1                  18 - 18          COLA                19 - 21     long COLB                22 - 26    
 long COLC                27 - 31          COLD                32 - 32          COLG_A              33 - 33    
      COLG_B              34 - 34          COLG_C              35 - 35          COLG_E              36 - 36    
      COLG_F              37 - 37          COLH_A              38 - 38          COLH_B              39 - 39    
      COLH_C              40 - 40          COLH_E              41 - 41          COLH_F              42 - 42    
      COLJ                43 - 45          WDAYWEND            46 - 46     long DURATION            47 - 51    
using J301635.txt, clear 
;
label variable ER30001         "1968 INTERVIEW NUMBER"                    ;
label variable ER30002         "PERSON NUMBER                         68" ;
label variable ER33401         "1997 INTERVIEW NUMBER"                    ;
label variable ER33402         "SEQUENCE NUMBER                       97" ;
label variable ER33403         "RELATION TO HEAD                      97" ;
label variable TDREL97         "TIME DIARY 1997 RELEASE NUMBER"           ;
label variable T1              "DAY OF WEEK"                              ;
label variable COLA            "ACTIVITY CODE"                            ;
label variable COLB            "START TIME"                               ;
label variable COLC            "END TIME"                                 ;
label variable COLD            "TV TYPE"                                  ;
label variable COLG_A          "NO ONE ELSE PARTICIPATING"                ;
label variable COLG_B          "MOTHER PARTICIPATING"                     ;
label variable COLG_C          "FATHER PARTICIPATING"                     ;
label variable COLG_E          "STEP-MOTHER PARTICIPATING"                ;
label variable COLG_F          "STEP-FATHER PARTICIPATING"                ;
label variable COLH_A          "NO ONE ELSE AROUND, NOT PARTICIPATING"    ;
label variable COLH_B          "MOTHER AROUND, NOT PARTICIPATING"         ;
label variable COLH_C          "FATHER AROUND, NOT PARTICIPATING"         ;
label variable COLH_E          "STEP-MOTHER AROUND, NOT PARTICIPATING"    ;
label variable COLH_F          "STEP-FATHER AROUND, NOT PARTICIPATING"    ;
label variable COLJ            "ACTIVITY CODE"                            ;
label variable WDAYWEND        "1=WEEKDAY/0=WEEKEND"                      ;
label variable DURATION        "DURATION OF ACTIVITY SPELL"               ;
