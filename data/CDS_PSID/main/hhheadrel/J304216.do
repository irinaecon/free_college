#delimit ;
*  PSID DATA CENTER *****************************************************
   JOBID            : 304216                            
   DATA_DOMAIN      : IND                               
   USER_WHERE       : NULL                              
   FILE_TYPE        : All Individuals Data              
   OUTPUT_DATA_TYPE : ASCII                             
   STATEMENTS       : do                                
   CODEBOOK_TYPE    : PDF                               
   N_OF_VARIABLES   : 27                                
   N_OF_OBSERVATIONS: 29900                             
   MAX_REC_LENGTH   : 79                                
   DATE & TIME      : March 13, 2022 @ 14:21:42
*************************************************************************
;

infix
      ER30000              1 - 1           ER30001              2 - 5           ER30002              6 - 8     
      ER33401              9 - 13          ER33402             14 - 15          ER33403             16 - 17    
      ER33404             18 - 20          ER33501             21 - 25          ER33502             26 - 27    
      ER33503             28 - 29          ER33504             30 - 32          ER33601             33 - 36    
      ER33602             37 - 38          ER33603             39 - 40          ER33604             41 - 43    
      ER33701             44 - 48          ER33702             49 - 50          ER33703             51 - 52    
      ER33704             53 - 55          ER33801             56 - 60          ER33802             61 - 62    
      ER33803             63 - 64          ER33804             65 - 67          ER33901             68 - 72    
      ER33902             73 - 74          ER33903             75 - 76          ER33904             77 - 79    
using J304216.txt, clear 
;
label variable ER30000         "RELEASE NUMBER"                           ;
label variable ER30001         "1968 INTERVIEW NUMBER"                    ;
label variable ER30002         "PERSON NUMBER                         68" ;
label variable ER33401         "1997 INTERVIEW NUMBER"                    ;
label variable ER33402         "SEQUENCE NUMBER                       97" ;
label variable ER33403         "RELATION TO HEAD                      97" ;
label variable ER33404         "AGE OF INDIVIDUAL                     97" ;
label variable ER33501         "1999 INTERVIEW NUMBER"                    ;
label variable ER33502         "SEQUENCE NUMBER                       99" ;
label variable ER33503         "RELATION TO HEAD                      99" ;
label variable ER33504         "AGE OF INDIVIDUAL                     99" ;
label variable ER33601         "2001 INTERVIEW NUMBER"                    ;
label variable ER33602         "SEQUENCE NUMBER                       01" ;
label variable ER33603         "RELATION TO HEAD                      01" ;
label variable ER33604         "AGE OF INDIVIDUAL                     01" ;
label variable ER33701         "2003 INTERVIEW NUMBER"                    ;
label variable ER33702         "SEQUENCE NUMBER                       03" ;
label variable ER33703         "RELATION TO HEAD                      03" ;
label variable ER33704         "AGE OF INDIVIDUAL                     03" ;
label variable ER33801         "2005 INTERVIEW NUMBER"                    ;
label variable ER33802         "SEQUENCE NUMBER                       05" ;
label variable ER33803         "RELATION TO HEAD                      05" ;
label variable ER33804         "AGE OF INDIVIDUAL                     05" ;
label variable ER33901         "2007 INTERVIEW NUMBER"                    ;
label variable ER33902         "SEQUENCE NUMBER                       07" ;
label variable ER33903         "RELATION TO HEAD                      07" ;
label variable ER33904         "AGE OF INDIVIDUAL                     07" ;
