#delimit ;
*  PSID DATA CENTER *****************************************************
   JOBID            : 301665                            
   DATA_DOMAIN      : TD                                
   USER_WHERE       : NULL                              
   FILE_TYPE        : NULL                              
   OUTPUT_DATA_TYPE : ASCII                             
   STATEMENTS       : do                                
   CODEBOOK_TYPE    : PDF                               
   N_OF_VARIABLES   : 25                                
   N_OF_OBSERVATIONS: 57813                             
   MAX_REC_LENGTH   : 56                                
   DATE & TIME      : January 10, 2022 @ 13:51:42
*************************************************************************
;

infix
      ER30001              1 - 4           ER30002              5 - 7           ER33901              8 - 12    
      ER33902             13 - 14          ER33903             15 - 16          TDREL07             17 - 17    
      T1_07               18 - 18          COLA_07             19 - 22     long COLB_07             23 - 27    
 long COLC_07             28 - 32          COLD_07             33 - 34          COLF_07             35 - 36    
      COLHA_07            37 - 37          COLHB_07            38 - 38          COLHC_07            39 - 39    
      COLHE_07            40 - 40          COLHF_07            41 - 41          COLIA_07            42 - 42    
      COLIB_07            43 - 43          COLIC_07            44 - 44          COLIE_07            45 - 45    
      COLIF_07            46 - 46          COLJ_07             47 - 50          DIARY_07            51 - 51    
 long DUR_07              52 - 56    
using J301665.txt, clear 
;
label variable ER30001         "1968 INTERVIEW NUMBER"                    ;
label variable ER30002         "PERSON NUMBER                         68" ;
label variable ER33901         "2007 INTERVIEW NUMBER"                    ;
label variable ER33902         "SEQUENCE NUMBER                       07" ;
label variable ER33903         "RELATION TO HEAD                      07" ;
label variable TDREL07         "TIME DIARY ACTIVITY RELEASE NUMBER 07"    ;
label variable T1_07           "DAY OF WEEK 07"                           ;
label variable COLA_07         "PRIMARY ACTIVITY CODE 07"                 ;
label variable COLB_07         "START TIME 07"                            ;
label variable COLC_07         "END TIME 07"                              ;
label variable COLD_07         "TV TYPE 07"                               ;
label variable COLF_07         "WEBSITE USED 07"                          ;
label variable COLHA_07        "NO ONE ELSE PARTICIPATING 07"             ;
label variable COLHB_07        "MOTHER PARTICIPATING 07"                  ;
label variable COLHC_07        "FATHER PARTICIPATING 07"                  ;
label variable COLHE_07        "STEP-MOTHER PARTICIPATING 07"             ;
label variable COLHF_07        "STEP-FATHER PARTICIPATING 07"             ;
label variable COLIA_07        "NO ONE ELSE AROUND, NOT PARTICIPATING 07" ;
label variable COLIB_07        "MOTHER AROUND, NOT PARTICIPATING 07"      ;
label variable COLIC_07        "FATHER AROUND, NOT PARTICIPATING 07"      ;
label variable COLIE_07        "STEP-MOTHER AROUND, NOT PARTICIPATING 07" ;
label variable COLIF_07        "STEP-FATHER AROUND, NOT PARTICIPATING 07" ;
label variable COLJ_07         "SECONDARY ACTIVITY CODE 07"               ;
label variable DIARY_07        "1=WEEKDAY/0=WEEKEND 07"                   ;
label variable DUR_07          "DURATION OF ACTIVITY SPELL 07"            ;
