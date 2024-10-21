#delimit ;
*  PSID DATA CENTER *****************************************************
   JOBID            : 294224                            
   DATA_DOMAIN      : CDS                               
   USER_WHERE       : NULL                              
   FILE_TYPE        : CDS Kids Only                     
   OUTPUT_DATA_TYPE : ASCII                             
   STATEMENTS       : do                                
   CODEBOOK_TYPE    : PDF                               
   N_OF_VARIABLES   : 55                                
   N_OF_OBSERVATIONS: 3036                              
   MAX_REC_LENGTH   : 82                                
   DATE & TIME      : June 11, 2021 @ 8:16:35
*************************************************************************
;

infix
      KIDS                 1 - 1           KID02                2 - 4           KID07                5 - 7     
      ER30001              8 - 11          ER30002             12 - 14          ER33601             15 - 18    
      ER33602             19 - 20          ER33603             21 - 22          CHREL               23 - 23    
      Q21B11              24 - 24          Q21B12_2            25 - 25          Q21B13              26 - 26    
      Q21B14              27 - 27          Q21B14B             28 - 28          Q21B15A             29 - 29    
      Q21B15B             30 - 30          Q21B15C             31 - 31          Q21B15D             32 - 32    
      Q21B15E             33 - 33          Q21B15F             34 - 34          Q21B15G             35 - 35    
      Q21B15H             36 - 36          Q21B15I             37 - 37          Q21B15J             38 - 38    
      Q21B15K             39 - 39          Q21B15L             40 - 40          Q21B15M             41 - 41    
      Q21B15N             42 - 42          Q21B15O             43 - 43          Q21H32E             44 - 44    
      ER33901             45 - 49          ER33902             50 - 51          ER33903             52 - 53    
      PCHREL07            54 - 54          Q31B11              55 - 55          Q31B12_2            56 - 56    
 long Q31B12A1            57 - 64          Q31B13              65 - 65          Q31B14              66 - 66    
      Q31B15A             67 - 67          Q31B15B             68 - 68          Q31B15C             69 - 69    
      Q31B15D             70 - 70          Q31B15E             71 - 71          Q31B15F             72 - 72    
      Q31B15G             73 - 73          Q31B15H             74 - 74          Q31B15I             75 - 75    
      Q31B15J             76 - 76          Q31B15K             77 - 77          Q31B15L             78 - 78    
      Q31B15M             79 - 79          Q31B15N             80 - 80          Q31B15O             81 - 81    
      Q31H32E             82 - 82    
using J294224.txt, clear 
;
label variable KIDS            "Sum of All KID Flags"                     ;
label variable KID02           "KID2002 = 1 if exists, else missing"      ;
label variable KID07           "KID2007 = 1 if exists, else missing"      ;
label variable ER30001         "1968 INTERVIEW NUMBER"                    ;
label variable ER30002         "PERSON NUMBER                         68" ;
label variable ER33601         "2001 INTERVIEW NUMBER"                    ;
label variable ER33602         "SEQUENCE NUMBER                       01" ;
label variable ER33603         "RELATION TO HEAD                      01" ;
label variable CHREL           "PCG CHILD FILE RELEASE NUMBER 02"         ;
label variable Q21B11          "PRIV OR PUBL SCH 02"                      ;
label variable Q21B12_2        "PRIV SCH PREV YR 02"                      ;
label variable Q21B13          "EVER ATTEND PRIV SCH 02"                  ;
label variable Q21B14          "TYPE ATTEND 02"                           ;
label variable Q21B14B         "AMT TIME IN PRIV SCH 02"                  ;
label variable Q21B15A         "ATTEND PRIV SCH  1ST GRADE 02"            ;
label variable Q21B15B         "ATTEND PRIV SCH  2ND GRADE 02"            ;
label variable Q21B15C         "ATTEND PRIV SCH  3RD GRADE 02"            ;
label variable Q21B15D         "ATTEND PRIV SCH  4TH GRADE 02"            ;
label variable Q21B15E         "ATTEND PRIV SCH  5TH GRADE 02"            ;
label variable Q21B15F         "ATTEND PRIV SCH  6TH GRADE 02"            ;
label variable Q21B15G         "ATTEND PRIV SCH  7TH GRADE 02"            ;
label variable Q21B15H         "ATTEND PRIV SCH  8TH GRADE 02"            ;
label variable Q21B15I         "ATTEND PRIV SCH  9TH GRADE 02"            ;
label variable Q21B15J         "ATTEND PRIV SCH  10TH GRADE 02"           ;
label variable Q21B15K         "ATTEND PRIV SCH  11TH GRADE 02"           ;
label variable Q21B15L         "ATTEND PRIV SCH  12TH GRADE 02"           ;
label variable Q21B15M         "ATTEND PRIV SCH  K'GARTN 02"              ;
label variable Q21B15N         "ATTEND PRIV SCH PRE K 02"                 ;
label variable Q21B15O         "ATTEND PRIV SCH POST K 02"                ;
label variable Q21H32E         "EXCLUDE PRIVATE SCHOOLS DUE TO COSTS 02"  ;
label variable ER33901         "2007 INTERVIEW NUMBER"                    ;
label variable ER33902         "SEQUENCE NUMBER                       07" ;
label variable ER33903         "RELATION TO HEAD                      07" ;
label variable PCHREL07        "PCG CHILD FILE RELEASE NUMBER 07"         ;
label variable Q31B11          "PRIVATE OR PUBLIC SCHOOL 07"              ;
label variable Q31B12_2        "PRIV SCH PREV YR 07"                      ;
label variable Q31B12A1        "PREV YR SCHOOL COSTS - AMT 07"            ;
label variable Q31B13          "EVER ATTEND PRIVATE SCHOOL 07"            ;
label variable Q31B14          "TYPE OF PRIV SCHOOL ATTEND 07"            ;
label variable Q31B15A         "ATTEND PRIV SCH  1ST GRADE 07"            ;
label variable Q31B15B         "ATTEND PRIV SCH  2ND GRADE 07"            ;
label variable Q31B15C         "ATTEND PRIV SCH  3RD GRADE 07"            ;
label variable Q31B15D         "ATTEND PRIV SCH  4TH GRADE 07"            ;
label variable Q31B15E         "ATTEND PRIV SCH  5TH GRADE 07"            ;
label variable Q31B15F         "ATTEND PRIV SCH  6TH GRADE 07"            ;
label variable Q31B15G         "ATTEND PRIV SCH  7TH GRADE 07"            ;
label variable Q31B15H         "ATTEND PRIV SCH  8TH GRADE 07"            ;
label variable Q31B15I         "ATTEND PRIV SCH  9TH GRADE 07"            ;
label variable Q31B15J         "ATTEND PRIV SCH  10TH GRADE 07"           ;
label variable Q31B15K         "ATTEND PRIV SCH  11TH GRADE 07"           ;
label variable Q31B15L         "ATTEND PRIV SCH  12TH GRADE 07"           ;
label variable Q31B15M         "ATTEND PRIV SCH  K'GARTN 07"              ;
label variable Q31B15N         "ATTEND PRIV SCH PRE K 07"                 ;
label variable Q31B15O         "ATTEND PRIV SCH POST K 07"                ;
label variable Q31H32E         "EXCLUDE PRIVATE SCHOOLS DUE TO COST 07"   ;
