#delimit ;
*  PSID DATA CENTER *****************************************************
   JOBID            : 301929                            
   DATA_DOMAIN      : CDSi                              
   USER_WHERE       : NULL                              
   FILE_TYPE        : CDS Kids Only                     
   OUTPUT_DATA_TYPE : ASCII                             
   STATEMENTS       : do                                
   CODEBOOK_TYPE    : PDF                               
   N_OF_VARIABLES   : 99                                
   N_OF_OBSERVATIONS: 1623                              
   MAX_REC_LENGTH   : 181                               
   DATE & TIME      : January 14, 2022 @ 20:35:05
*************************************************************************
;

infix
      ER30001      1 - 4     ER30002      5 - 7     ER25016      8 - 9
      ER25020     10 - 11    ER33801     12 - 16    ER33802     17 - 18
      ER33803     19 - 20    ER36016     21 - 22    ER36017     23 - 25
      ER36018     26 - 26    ER36019     27 - 29    ER36020     30 - 31
      ER36021     32 - 34    ER36023     35 - 35    ER40874     36 - 38
      ER40876     39 - 42    ER40885     43 - 45    ER40887     46 - 49
      ER40921     50 - 56    ER40933     57 - 63    ER40943     64 - 70
      ER41027     71 - 77    ER33901     78 - 82    ER33902     83 - 84
      ER33903     85 - 86    ER33904     87 - 89    ER33906     90 - 93
      RELPCG07    94 - 95    RELOCG07    96 - 97    CH07WT      98 - 103
      Q31IWAGE   104 - 108   Q34LW1     109 - 109   Q34LW2     110 - 110
      Q34LW3     111 - 111   Q34LW4     112 - 112   Q34LW5     113 - 113
      Q34LW6     114 - 114   Q34LW7     115 - 115   Q34LW8     116 - 116
      Q34LW9     117 - 117   Q34LW10    118 - 118   Q34LW11    119 - 119
      Q34LW12    120 - 120   Q34LW13    121 - 121   Q34LW14    122 - 122
      Q34LW15    123 - 123   Q34LW16    124 - 124   Q34LW17    125 - 125
      Q34LW18    126 - 126   Q34LW19    127 - 127   Q34LW20    128 - 128
      Q34LW21    129 - 129   Q34LW22    130 - 130   Q34LW23    131 - 131
      Q34LW24    132 - 132   Q34LW25    133 - 133   Q34LW26    134 - 134
      Q34LW27    135 - 135   Q34LW28    136 - 136   Q34LW29    137 - 137
      Q34LW30    138 - 138   Q34LW31    139 - 139   Q34LW32    140 - 140
      Q34LW33    141 - 141   Q34LW34    142 - 142   Q34LW35    143 - 143
      Q34LW36    144 - 144   Q34LW37    145 - 145   Q34LW38    146 - 146
      Q34LW39    147 - 147   Q34LW40    148 - 148   Q34LW41    149 - 149
      Q34LW42    150 - 150   Q34LW43    151 - 151   Q34LW44    152 - 152
      Q34LW45    153 - 153   Q34LW46    154 - 154   Q34LW47    155 - 155
      Q34LW48    156 - 156   Q34LW49    157 - 157   Q34LW50    158 - 158
      Q34LW51    159 - 159   Q34LW52    160 - 160   Q34LW53    161 - 161
      Q34LW54    162 - 162   Q34LW55    163 - 163   Q34LW56    164 - 164
      Q34LW57    165 - 165   Q34LW58    166 - 166   Q34LWRAW   167 - 168
      Q34LWSS    169 - 171   
using J301929.txt, clear 
;
label variable ER30001  "1968 INTERVIEW NUMBER"                    ;
label variable ER30002  "PERSON NUMBER                         68" ;
label variable ER25016  "# IN FU"                                  ;
label variable ER25020  "# CHILDREN IN FU"                         ;
label variable ER33801  "2005 INTERVIEW NUMBER"                    ;
label variable ER33802  "SEQUENCE NUMBER                       05" ;
label variable ER33803  "RELATION TO HEAD                      05" ;
label variable ER36016  "# IN FU"                                  ;
label variable ER36017  "AGE OF HEAD"                              ;
label variable ER36018  "SEX OF HEAD"                              ;
label variable ER36019  "AGE OF WIFE"                              ;
label variable ER36020  "# CHILDREN IN FU"                         ;
label variable ER36021  "AGE YOUNGEST CHILD"                       ;
label variable ER36023  "HEAD MARITAL STATUS"                      ;
label variable ER40874  "HEAD WEEKLY WORK HOURS-2006"              ;
label variable ER40876  "HEAD TOTAL HOURS OF WORK-2006"            ;
label variable ER40885  "WIFE WEEKLY WORK HOURS-2006"              ;
label variable ER40887  "WIFE TOTAL HOURS OF WORK-2006"            ;
label variable ER40921  "LABOR INCOME OF HEAD-2006"                ;
label variable ER40933  "LABOR INCOME OF WIFE-2006"                ;
label variable ER40943  "HEAD AND WIFE TAXABLE INCOME-2006"        ;
label variable ER41027  "TOTAL FAMILY INCOME-2006"                 ;
label variable ER33901  "2007 INTERVIEW NUMBER"                    ;
label variable ER33902  "SEQUENCE NUMBER                       07" ;
label variable ER33903  "RELATION TO HEAD                      07" ;
label variable ER33904  "AGE OF INDIVIDUAL                     07" ;
label variable ER33906  "YEAR INDIVIDUAL BORN                  07" ;
label variable RELPCG07 "RELATION PCG 07"                          ;
label variable RELOCG07 "RELATION OCG 07"                          ;
label variable CH07WT   "CHILD INTERVIEW / CHILD WEIGHT 07"        ;
label variable Q31IWAGE "CHILD AGE AT TIME OF PCG IW - YEARS 07"   ;
label variable Q34LW1   "LW TEST ITEM 1 07"                        ;
label variable Q34LW2   "LW TEST ITEM 2 07"                        ;
label variable Q34LW3   "LW TEST ITEM 3 07"                        ;
label variable Q34LW4   "LW TEST ITEM 4 07"                        ;
label variable Q34LW5   "LW TEST ITEM 5 07"                        ;
label variable Q34LW6   "LW TEST ITEM 6 07"                        ;
label variable Q34LW7   "LW TEST ITEM 7 07"                        ;
label variable Q34LW8   "LW TEST ITEM 8 07"                        ;
label variable Q34LW9   "LW TEST ITEM 9 07"                        ;
label variable Q34LW10  "LW TEST ITEM 10 07"                       ;
label variable Q34LW11  "LW TEST ITEM 11 07"                       ;
label variable Q34LW12  "LW TEST ITEM 12 07"                       ;
label variable Q34LW13  "LW TEST ITEM 13 07"                       ;
label variable Q34LW14  "LW TEST ITEM 14 07"                       ;
label variable Q34LW15  "LW TEST ITEM 15 07"                       ;
label variable Q34LW16  "LW TEST ITEM 16 07"                       ;
label variable Q34LW17  "LW TEST ITEM 17 07"                       ;
label variable Q34LW18  "LW TEST ITEM 18 07"                       ;
label variable Q34LW19  "LW TEST ITEM 19 07"                       ;
label variable Q34LW20  "LW TEST ITEM 20 07"                       ;
label variable Q34LW21  "LW TEST ITEM 21 07"                       ;
label variable Q34LW22  "LW TEST ITEM 22 07"                       ;
label variable Q34LW23  "LW TEST ITEM 23 07"                       ;
label variable Q34LW24  "LW TEST ITEM 24 07"                       ;
label variable Q34LW25  "LW TEST ITEM 25 07"                       ;
label variable Q34LW26  "LW TEST ITEM 26 07"                       ;
label variable Q34LW27  "LW TEST ITEM 27 07"                       ;
label variable Q34LW28  "LW TEST ITEM 28 07"                       ;
label variable Q34LW29  "LW TEST ITEM 29 07"                       ;
label variable Q34LW30  "LW TEST ITEM 30 07"                       ;
label variable Q34LW31  "LW TEST ITEM 31 07"                       ;
label variable Q34LW32  "LW TEST ITEM 32 07"                       ;
label variable Q34LW33  "LW TEST ITEM 33 07"                       ;
label variable Q34LW34  "LW TEST ITEM 34 07"                       ;
label variable Q34LW35  "LW TEST ITEM 35 07"                       ;
label variable Q34LW36  "LW TEST ITEM 36 07"                       ;
label variable Q34LW37  "LW TEST ITEM 37 07"                       ;
label variable Q34LW38  "LW TEST ITEM 38 07"                       ;
label variable Q34LW39  "LW TEST ITEM 39 07"                       ;
label variable Q34LW40  "LW TEST ITEM 40 07"                       ;
label variable Q34LW41  "LW TEST ITEM 41 07"                       ;
label variable Q34LW42  "LW TEST ITEM 42 07"                       ;
label variable Q34LW43  "LW TEST ITEM 43 07"                       ;
label variable Q34LW44  "LW TEST ITEM 44 07"                       ;
label variable Q34LW45  "LW TEST ITEM 45 07"                       ;
label variable Q34LW46  "LW TEST ITEM 46 07"                       ;
label variable Q34LW47  "LW TEST ITEM 47 07"                       ;
label variable Q34LW48  "LW TEST ITEM 48 07"                       ;
label variable Q34LW49  "LW TEST ITEM 49 07"                       ;
label variable Q34LW50  "LW TEST ITEM 50 07"                       ;
label variable Q34LW51  "LW TEST ITEM 51 07"                       ;
label variable Q34LW52  "LW TEST ITEM 52 07"                       ;
label variable Q34LW53  "LW TEST ITEM 53 07"                       ;
label variable Q34LW54  "LW TEST ITEM 54 07"                       ;
label variable Q34LW55  "LW TEST ITEM 55 07"                       ;
label variable Q34LW56  "LW TEST ITEM 56 07"                       ;
label variable Q34LW57  "LW TEST ITEM 57 07"                       ;
label variable Q34LW58  "LW TEST ITEM 58 07"                       ;
label variable Q34LWRAW "LW RAW SCORE 07"                          ;
label variable Q34LWSS  "LW STD SCORE 07"                          ;
