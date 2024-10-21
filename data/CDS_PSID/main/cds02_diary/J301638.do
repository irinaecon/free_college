#delimit ;
*  PSID DATA CENTER *****************************************************
   JOBID            : 301638                            
   DATA_DOMAIN      : TD                                
   USER_WHERE       : NULL                              
   FILE_TYPE        : NULL                              
   OUTPUT_DATA_TYPE : ASCII                             
   STATEMENTS       : do                                
   CODEBOOK_TYPE    : PDF                               
   N_OF_VARIABLES   : 24                                
   N_OF_OBSERVATIONS: 99467                             
   MAX_REC_LENGTH   : 52                                
   DATE & TIME      : January 10, 2022 @ 12:53:49
*************************************************************************
;

infix
      ER30001              1 - 4           ER30002              5 - 7           ER33601              8 - 11    
      ER33602             12 - 13          ER33603             14 - 15          TDREL02             16 - 16    
      T1_02               17 - 17          COLA_02             18 - 21     long COLB_02             22 - 26    
 long COLC_02             27 - 31          COLD_02             32 - 32          COLGA_02            33 - 33    
      COLGB_02            34 - 34          COLGC_02            35 - 35          COLGE_02            36 - 36    
      COLGF_02            37 - 37          COLHA_02            38 - 38          COLHB_02            39 - 39    
      COLHC_02            40 - 40          COLHE_02            41 - 41          COLHF_02            42 - 42    
      COLJ_02             43 - 46          DIARY_02            47 - 47     long DUR_02              48 - 52    
using J301638.txt, clear 
;
label variable ER30001         "1968 INTERVIEW NUMBER"                    ;
label variable ER30002         "PERSON NUMBER                         68" ;
label variable ER33601         "2001 INTERVIEW NUMBER"                    ;
label variable ER33602         "SEQUENCE NUMBER                       01" ;
label variable ER33603         "RELATION TO HEAD                      01" ;
label variable TDREL02         "TD_ACTIVITY FILE RELEASE NUMBER 02"       ;
label variable T1_02           "DAY OF WEEK 02"                           ;
label variable COLA_02         "ACTIVITY CODE 02"                         ;
label variable COLB_02         "START TIME 02"                            ;
label variable COLC_02         "END TIME 02"                              ;
label variable COLD_02         "TV TYPE 02"                               ;
label variable COLGA_02        "NO ONE ELSE PARTICIPATING 02"             ;
label variable COLGB_02        "MOTHER PARTICIPATING 02"                  ;
label variable COLGC_02        "FATHER PARTICIPATING 02"                  ;
label variable COLGE_02        "STEP-MOTHER PARTICIPATING 02"             ;
label variable COLGF_02        "STEP-FATHER PARTICIPATING 02"             ;
label variable COLHA_02        "NO ONE ELSE AROUND, NOT PARTICIPATING 02" ;
label variable COLHB_02        "MOTHER AROUND, NOT PARTICIPATING 02"      ;
label variable COLHC_02        "FATHER AROUND, NOT PARTICIPATING 02"      ;
label variable COLHE_02        "STEP-MOTHER AROUND, NOT PARTICIPATING 02" ;
label variable COLHF_02        "STEP-FATHER AROUND, NOT PARTICIPATING 02" ;
label variable COLJ_02         "ACTIVITY CODE 02"                         ;
label variable DIARY_02        "1=WEEKDAY/0=WEEKEND 02"                   ;
label variable DUR_02          "DURATION OF ACTIVITY SPELL 02"            ;
