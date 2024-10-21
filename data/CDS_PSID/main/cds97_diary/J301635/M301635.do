#delimit ;
*  PSID DATA CENTER *****************************************************
   JOBID            : 301635                            
   OUTPUT_DATA_TYPE : ASCII                             
   STATEMENTS       : do                                
   N_OF_VARIABLES   : 14                                
   N_OF_OBSERVATIONS: 3563                              
   MAX_REC_LENGTH   : 49                                
   DATE & TIME      : January 10, 2022 @ 12:28:43
*************************************************************************
;

infix
      ER30001              1 - 4           ER30002              5 - 7           PCGID_97             8 - 11    
      PCGPN_97            12 - 14          OCGID_97            15 - 18          OCGPN_97            19 - 21    
      PCGID_02            22 - 25          PCGPN_02            26 - 28          OCGID_02            29 - 32    
      OCGPN_02            33 - 35          PCGID_07            36 - 39          PCGPN_07            40 - 42    
      OCGID_07            43 - 46          OCGPN_07            47 - 49    
using [path]\M301635.txt, clear 
;
label variable ER30001         "CHILD: 1968 INTERVI_EW NUMBER"            ;
label variable ER30002         "CHILD: PERSON NUMBER 68"                  ;
label variable PCGID_97        "PCG 1997: 1968 INTERVIEW NUMBER"          ;
label variable PCGPN_97        "PCG 1997: PERSON NUMBER 68"               ;
label variable OCGID_97        "OCG 1997: 1968 INTERVIEW NUMBER"          ;
label variable OCGPN_97        "OCG 1997: PERSON NUMBER 68"               ;
label variable PCGID_02        "PCG 2002: 1968 INTERVIEW NUMBER"          ;
label variable PCGPN_02        "PCG 2002: PERSON NUMBER 68"               ;
label variable OCGID_02        "OCG 2002: 1968 INTERVIEW NUMBER"          ;
label variable OCGPN_02        "OCG 2002: PERSON NUMBER 68"               ;
label variable PCGID_07        "PCG 2007: 1968 INTERVIEW NUMBER"          ;
label variable PCGPN_07        "PCG 2007: PERSON NUMBER 68"               ;
label variable OCGID_07        "OCG 2007: 1968 INTERVIEW NUMBER"          ;
label variable OCGPN_07        "OCG 2007: PERSON NUMBER 68"               ;
