#delimit ;
*  PSID DATA CENTER *****************************************************
   JOBID            : 301928                            
   DATA_DOMAIN      : CDSi                              
   USER_WHERE       : NULL                              
   FILE_TYPE        : CDS Kids Only                     
   OUTPUT_DATA_TYPE : ASCII                             
   STATEMENTS       : do                                
   CODEBOOK_TYPE    : PDF                               
   N_OF_VARIABLES   : 105                               
   N_OF_OBSERVATIONS: 2907                              
   MAX_REC_LENGTH   : 196                               
   DATE & TIME      : January 14, 2022 @ 20:16:20
*************************************************************************
;

infix
      ER30001      1 - 4     ER30002      5 - 7     ER17012      8 - 9
      ER17013     10 - 12    ER17015     13 - 15    ER17016     16 - 17
      ER33601     18 - 21    ER33602     22 - 23    ER33603     24 - 25
      ER33604     26 - 28    ER33606     29 - 32    RELPCG02    33 - 34
      RELOCG02    35 - 36    CH02PRWT    37 - 42    Q21IWAGE    43 - 47
      CDI_02      48 - 49    Q24LW1      50 - 50    Q24LW2      51 - 51
      Q24LW3      52 - 52    Q24LW4      53 - 53    Q24LW5      54 - 54
      Q24LW6      55 - 55    Q24LW7      56 - 56    Q24LW8      57 - 57
      Q24LW9      58 - 58    Q24LW10     59 - 59    Q24LW11     60 - 60
      Q24LW12     61 - 61    Q24LW13     62 - 62    Q24LW14     63 - 63
      Q24LW15     64 - 64    Q24LW16     65 - 65    Q24LW17     66 - 66
      Q24LW18     67 - 67    Q24LW19     68 - 68    Q24LW20     69 - 69
      Q24LW21     70 - 70    Q24LW22     71 - 71    Q24LW23     72 - 72
      Q24LW24     73 - 73    Q24LW25     74 - 74    Q24LW26     75 - 75
      Q24LW27     76 - 76    Q24LW28     77 - 77    Q24LW29     78 - 78
      Q24LW30     79 - 79    Q24LW31     80 - 80    Q24LW32     81 - 81
      Q24LW33     82 - 82    Q24LW34     83 - 83    Q24LW35     84 - 84
      Q24LW36     85 - 85    Q24LW37     86 - 86    Q24LW38     87 - 87
      Q24LW39     88 - 88    Q24LW40     89 - 89    Q24LW41     90 - 90
      Q24LW42     91 - 91    Q24LW43     92 - 92    Q24LW44     93 - 93
      Q24LW45     94 - 94    Q24LW46     95 - 95    Q24LW47     96 - 96
      Q24LW48     97 - 97    Q24LW49     98 - 98    Q24LW50     99 - 99
      Q24LW51    100 - 100   Q24LW52    101 - 101   Q24LW53    102 - 102
      Q24LW54    103 - 103   Q24LW55    104 - 104   Q24LW56    105 - 105
      Q24LW57    106 - 106   Q24LW58    107 - 107   Q24LWRAW   108 - 109
      Q24LWSS    110 - 112   ER21016    113 - 114   ER21017    115 - 117
      ER21018    118 - 118   ER21019    119 - 121   ER21020    122 - 123
      ER21021    124 - 126   ER21023    127 - 127   ER24078    128 - 130
      ER24080    131 - 134   ER24089    135 - 137   ER24091    138 - 141
      ER24099    142 - 148   ER24100    149 - 155   ER24116    156 - 162
      ER24135    163 - 169   ER33701    170 - 174   ER33702    175 - 176
      ER33703    177 - 178   ER33704    179 - 181   ER33706    182 - 185   
using J301928.txt, clear 
;

label variable ER30001         "1968 INTERVIEW NUMBER"                    ;
label variable ER30002         "PERSON NUMBER                         68" ;
label variable ER17012         "# IN FU"                                  ;
label variable ER17013         "AGE OF HEAD"                              ;
label variable ER17015         "AGE OF WIFE"                              ;
label variable ER17016         "# CHILDREN IN FU"                         ;
label variable ER33601         "2001 INTERVIEW NUMBER"                    ;
label variable ER33602         "SEQUENCE NUMBER                       01" ;
label variable ER33603         "RELATION TO HEAD                      01" ;
label variable ER33604         "AGE OF INDIVIDUAL                     01" ;
label variable ER33606         "YEAR INDIVIDUAL BORN                  01" ;
label variable RELPCG02        "RELATION PCG 02"                          ;
label variable RELOCG02        "RELATION OCG 02"                          ;
label variable CH02PRWT        "CHILD LEVEL WEIGHT 02"                    ;
label variable Q21IWAGE        "CHILD AGE AT TIME OF PCG IW - YEARS 02"   ;
label variable CDI_02          "CDI Short Form 02"                        ;
label variable Q24LW1          "LW TEST ITEM 1 02"                        ;
label variable Q24LW2          "LW TEST ITEM 2 02"                        ;
label variable Q24LW3          "LW TEST ITEM 3 02"                        ;
label variable Q24LW4          "LW TEST ITEM 4 02"                        ;
label variable Q24LW5          "LW TEST ITEM 5 02"                        ;
label variable Q24LW6          "LW TEST ITEM 6 02"                        ;
label variable Q24LW7          "LW TEST ITEM 7 02"                        ;
label variable Q24LW8          "LW TEST ITEM 8 02"                        ;
label variable Q24LW9          "LW TEST ITEM 9 02"                        ;
label variable Q24LW10         "LW TEST ITEM 10 02"                       ;
label variable Q24LW11         "LW TEST ITEM 11 02"                       ;
label variable Q24LW12         "LW TEST ITEM 12 02"                       ;
label variable Q24LW13         "LW TEST ITEM 13 02"                       ;
label variable Q24LW14         "LW TEST ITEM 14 02"                       ;
label variable Q24LW15         "LW TEST ITEM 15 02"                       ;
label variable Q24LW16         "LW TEST ITEM 16 02"                       ;
label variable Q24LW17         "LW TEST ITEM 17 02"                       ;
label variable Q24LW18         "LW TEST ITEM 18 02"                       ;
label variable Q24LW19         "LW TEST ITEM 19 02"                       ;
label variable Q24LW20         "LW TEST ITEM 20 02"                       ;
label variable Q24LW21         "LW TEST ITEM 21 02"                       ;
label variable Q24LW22         "LW TEST ITEM 22 02"                       ;
label variable Q24LW23         "LW TEST ITEM 23 02"                       ;
label variable Q24LW24         "LW TEST ITEM 24 02"                       ;
label variable Q24LW25         "LW TEST ITEM 25 02"                       ;
label variable Q24LW26         "LW TEST ITEM 26 02"                       ;
label variable Q24LW27         "LW TEST ITEM 27 02"                       ;
label variable Q24LW28         "LW TEST ITEM 28 02"                       ;
label variable Q24LW29         "LW TEST ITEM 29 02"                       ;
label variable Q24LW30         "LW TEST ITEM 30 02"                       ;
label variable Q24LW31         "LW TEST ITEM 31 02"                       ;
label variable Q24LW32         "LW TEST ITEM 32 02"                       ;
label variable Q24LW33         "LW TEST ITEM 33 02"                       ;
label variable Q24LW34         "LW TEST ITEM 34 02"                       ;
label variable Q24LW35         "LW TEST ITEM 35 02"                       ;
label variable Q24LW36         "LW TEST ITEM 36 02"                       ;
label variable Q24LW37         "LW TEST ITEM 37 02"                       ;
label variable Q24LW38         "LW TEST ITEM 38 02"                       ;
label variable Q24LW39         "LW TEST ITEM 39 02"                       ;
label variable Q24LW40         "LW TEST ITEM 40 02"                       ;
label variable Q24LW41         "LW TEST ITEM 41 02"                       ;
label variable Q24LW42         "LW TEST ITEM 42 02"                       ;
label variable Q24LW43         "LW TEST ITEM 43 02"                       ;
label variable Q24LW44         "LW TEST ITEM 44 02"                       ;
label variable Q24LW45         "LW TEST ITEM 45 02"                       ;
label variable Q24LW46         "LW TEST ITEM 46 02"                       ;
label variable Q24LW47         "LW TEST ITEM 47 02"                       ;
label variable Q24LW48         "LW TEST ITEM 48 02"                       ;
label variable Q24LW49         "LW TEST ITEM 49 02"                       ;
label variable Q24LW50         "LW TEST ITEM 50 02"                       ;
label variable Q24LW51         "LW TEST ITEM 51 02"                       ;
label variable Q24LW52         "LW TEST ITEM 52 02"                       ;
label variable Q24LW53         "LW TEST ITEM 53 02"                       ;
label variable Q24LW54         "LW TEST ITEM 54 02"                       ;
label variable Q24LW55         "LW TEST ITEM 55 02"                       ;
label variable Q24LW56         "LW TEST ITEM 56 02"                       ;
label variable Q24LW57         "LW TEST ITEM 57 02"                       ;
label variable Q24LW58         "LW TEST ITEM 58 02"                       ;
label variable Q24LWRAW        "LW RAW SCORE 02"                          ;
label variable Q24LWSS         "LW STD SCORE 02"                          ;
label variable ER21016         "# IN FU"                                  ;
label variable ER21017         "AGE OF HEAD"                              ;
label variable ER21018         "SEX OF HEAD"                              ;
label variable ER21019         "AGE OF WIFE"                              ;
label variable ER21020         "# CHILDREN IN FU"                         ;
label variable ER21021         "AGE YOUNGEST CHILD"                       ;
label variable ER21023         "HEAD MARITAL STATUS"                      ;
label variable ER24078         "HEAD WEEKLY WORK HOURS LAST YEAR"         ;
label variable ER24080         "HEAD TOTAL HOURS OF WORK LAST YEAR"       ;
label variable ER24089         "WIFE WEEKLY WORK HOURS LAST YEAR"         ;
label variable ER24091         "WIFE TOTAL HOURS OF WORK LAST YEAR"       ;
label variable ER24099         "TOTAL FAMILY INCOME LAST YEAR"            ;
label variable ER24100         "HEAD AND TAXABLE INCOME WIFE LAST YEAR"   ;
label variable ER24116         "LABOR INCOME OF HEAD LAST YEAR"           ;
label variable ER24135         "LABOR INCOME OF WIFE LAST YEAR"           ;
label variable ER33701         "2003 INTERVIEW NUMBER"                    ;
label variable ER33702         "SEQUENCE NUMBER                       03" ;
label variable ER33703         "RELATION TO HEAD                      03" ;
label variable ER33704         "AGE OF INDIVIDUAL                     03" ;
label variable ER33706         "YEAR INDIVIDUAL BORN                  03" ;
