#delimit ;
*  PSID DATA CENTER *****************************************************
   JOBID            : 301870                            
   DATA_DOMAIN      : CDSi                              
   USER_WHERE       : NULL                              
   FILE_TYPE        : CDS Kids Only                     
   OUTPUT_DATA_TYPE : ASCII                             
   STATEMENTS       : do                                
   CODEBOOK_TYPE    : PDF                               
   N_OF_VARIABLES   : 98                                
   N_OF_OBSERVATIONS: 3563                              
   MAX_REC_LENGTH   : 181                               
   DATE & TIME      : January 13, 2022 @ 7:34:07
*************************************************************************
;

infix
      ER30001      1 - 4     ER30002      5 - 7     ER10008      8 - 9
      ER10009     10 - 12    ER10010     13 - 13    ER10011     14 - 16
      ER10012     17 - 18    ER10013     19 - 20    ER10016     21 - 21
      ER12069     22 - 30    ER12079     31 - 39    ER12080     40 - 48
      ER12082     49 - 54    ER12171     55 - 57    ER12174     58 - 61
      ER12182     62 - 64    ER12185     65 - 68    ER33401     69 - 73
      ER33402     74 - 75    ER33403     76 - 77    ER33404     78 - 80
      ER33406     81 - 84    AGEATPCG    85 - 89    RELPCG97    90 - 91
      RELOCG97    92 - 93    CH97PRWT    94 - 100   Q1B17      101 - 101
      Q1G1       102 - 102   Q1G22      103 - 103   Q3LW1      104 - 104
      Q3LW2      105 - 105   Q3LW3      106 - 106   Q3LW4      107 - 107
      Q3LW5      108 - 108   Q3LW6      109 - 109   Q3LW7      110 - 110
      Q3LW8      111 - 111   Q3LW9      112 - 112   Q3LW10     113 - 113
      Q3LW11     114 - 114   Q3LW12     115 - 115   Q3LW13     116 - 116
      Q3LW14     117 - 117   Q3LW15     118 - 118   Q3LW16     119 - 119
      Q3LW17     120 - 120   Q3LW18     121 - 121   Q3LW19     122 - 122
      Q3LW20     123 - 123   Q3LW21     124 - 124   Q3LW22     125 - 125
      Q3LW23     126 - 126   Q3LW24     127 - 127   Q3LW25     128 - 128
      Q3LW26     129 - 129   Q3LW27     130 - 130   Q3LW28     131 - 131
      Q3LW29     132 - 132   Q3LW30     133 - 133   Q3LW31     134 - 134
      Q3LW32     135 - 135   Q3LW33     136 - 136   Q3LW34     137 - 137
      Q3LW35     138 - 138   Q3LW36     139 - 139   Q3LW37     140 - 140
      Q3LW38     141 - 141   Q3LW39     142 - 142   Q3LW40     143 - 143
      Q3LW41     144 - 144   Q3LW42     145 - 145   Q3LW43     146 - 146
      Q3LW44     147 - 147   Q3LW45     148 - 148   Q3LW46     149 - 149
      Q3LW47     150 - 150   Q3LW48     151 - 151   Q3LW49     152 - 152
      Q3LW50     153 - 153   Q3LW51     154 - 154   Q3LW52     155 - 155
      Q3LW53     156 - 156   Q3LW54     157 - 157   Q3LW55     158 - 158
      Q3LW56     159 - 159   Q3LW57     160 - 160   Q3LWRAW    161 - 162
      Q3LW_SS    163 - 165   ER13009    166 - 167   ER13013    168 - 169
      ER33501    170 - 174   ER33502    175 - 176   ER33503    177 - 178
	  
using J301870.txt, clear 
;
label variable ER30001  "1968 INTERVIEW NUMBER"                    ;
label variable ER30002  "PERSON NUMBER                         68" ;
label variable ER10008  "# IN FU"                                  ;
label variable ER10009  "AGE OF HEAD"                              ;
label variable ER10010  "SEX OF HEAD"                              ;
label variable ER10011  "AGE OF WIFE"                              ;
label variable ER10012  "# CHILDREN IN FU"                         ;
label variable ER10013  "AGE YOUNGEST CHILD"                       ;
label variable ER10016  "HEAD MARITAL STATUS"                      ;
label variable ER12069  "HD+WF TAXABLE INCOME"                     ;
label variable ER12079  "TOTAL FAMILY INCOME"                      ;
label variable ER12080  "LABOR INCOME-HEAD"                        ;
label variable ER12082  "LABOR INCOME-WIFE"                        ;
label variable ER12171  "HEAD WEEKLY WORK HOURS-1996"              ;
label variable ER12174  "HEAD TOTAL HOURS OF WORK-1996"            ;
label variable ER12182  "WIFE WEEKLY WORK HOURS-1996"              ;
label variable ER12185  "WIFE TOTAL HOURS OF WORK-1996"            ;
label variable ER33401  "1997 INTERVIEW NUMBER"                    ;
label variable ER33402  "SEQUENCE NUMBER                       97" ;
label variable ER33403  "RELATION TO HEAD                      97" ;
label variable ER33404  "AGE OF INDIVIDUAL                     97" ;
label variable ER33406  "YEAR INDIVIDUAL BORN                  97" ;
label variable AGEATPCG "AGE AT PCG INTERVIEW IN MONTHS"           ;
label variable RELPCG97 "RELATION PCG 97"                          ;
label variable RELOCG97 "RELATION OCG 97"                          ;
label variable CH97PRWT "CHILD LEVEL WEIGHT"                       ;
label variable Q1B17    "AGE OF CHILD 97"                          ;
label variable Q1G1     "CHILD'S AGE 97"                           ;
label variable Q1G22    "CHILD'S AGE 97"                           ;
label variable Q3LW1    "LW TEST ITEM 1 97"                        ;
label variable Q3LW2    "LW TEST ITEM 2 97"                        ;
label variable Q3LW3    "LW TEST ITEM 3 97"                        ;
label variable Q3LW4    "LW TEST ITEM 4 97"                        ;
label variable Q3LW5    "LW TEST ITEM 5 97"                        ;
label variable Q3LW6    "LW TEST ITEM 6 97"                        ;
label variable Q3LW7    "LW TEST ITEM 7 97"                        ;
label variable Q3LW8    "LW TEST ITEM 8 97"                        ;
label variable Q3LW9    "LW TEST ITEM 9 97"                        ;
label variable Q3LW10   "LW TEST ITEM 10 97"                       ;
label variable Q3LW11   "LW TEST ITEM 11 97"                       ;
label variable Q3LW12   "LW TEST ITEM 12 97"                       ;
label variable Q3LW13   "LW TEST ITEM 13 97"                       ;
label variable Q3LW14   "LW TEST ITEM 14 97"                       ;
label variable Q3LW15   "LW TEST ITEM 15 97"                       ;
label variable Q3LW16   "LW TEST ITEM 16 97"                       ;
label variable Q3LW17   "LW TEST ITEM 16 97"                       ;
label variable Q3LW18   "LW TEST ITEM 18 97"                       ;
label variable Q3LW19   "LW TEST ITEM 19 97"                       ;
label variable Q3LW20   "LW TEST ITEM 20 97"                       ;
label variable Q3LW21   "LW TEST ITEM 21 97"                       ;
label variable Q3LW22   "LW TEST ITEM 22 97"                       ;
label variable Q3LW23   "LW TEST ITEM 23 97"                       ;
label variable Q3LW24   "LW TEST ITEM 24 97"                       ;
label variable Q3LW25   "LW TEST ITEM 25 97"                       ;
label variable Q3LW26   "LW TEST ITEM 26 97"                       ;
label variable Q3LW27   "LW TEST ITEM 27 97"                       ;
label variable Q3LW28   "LW TEST ITEM 28 97"                       ;
label variable Q3LW29   "LW TEST ITEM 29 97"                       ;
label variable Q3LW30   "LW TEST ITEM 30 97"                       ;
label variable Q3LW31   "LW TEST ITEM 31 97"                       ;
label variable Q3LW32   "LW TEST ITEM 32 97"                       ;
label variable Q3LW33   "LW TEST ITEM 33 97"                       ;
label variable Q3LW34   "LW TEST ITEM 34 97"                       ;
label variable Q3LW35   "LW TEST ITEM 35 97"                       ;
label variable Q3LW36   "LW TEST ITEM 36 97"                       ;
label variable Q3LW37   "LW TEST ITEM 37 97"                       ;
label variable Q3LW38   "LW TEST ITEM 38 97"                       ;
label variable Q3LW39   "LW TEST ITEM 39 97"                       ;
label variable Q3LW40   "LW TEST ITEM 40 97"                       ;
label variable Q3LW41   "LW TEST ITEM 41 97"                       ;
label variable Q3LW42   "LW TEST ITEM 42 97"                       ;
label variable Q3LW43   "LW TEST ITEM 43 97"                       ;
label variable Q3LW44   "LW TEST ITEM 44 97"                       ;
label variable Q3LW45   "LW TEST ITEM 45 97"                       ;
label variable Q3LW46   "LW TEST ITEM 46 97"                       ;
label variable Q3LW47   "LW TEST ITEM 47 97"                       ;
label variable Q3LW48   "LW TEST ITEM 48 97"                       ;
label variable Q3LW49   "LW TEST ITEM 49 97"                       ;
label variable Q3LW50   "LW TEST ITEM 50 97"                       ;
label variable Q3LW51   "LW TEST ITEM 51 97"                       ;
label variable Q3LW52   "LW TEST ITEM 52 97"                       ;
label variable Q3LW53   "LW TEST ITEM 53 97"                       ;
label variable Q3LW54   "LW TEST ITEM 54 97"                       ;
label variable Q3LW55   "LW TEST ITEM 55 97"                       ;
label variable Q3LW56   "LW TEST ITEM 56 97"                       ;
label variable Q3LW57   "LW TEST ITEM 57 97"                       ;
label variable Q3LWRAW  "LW RAW SCORE 97"                          ;
label variable Q3LW_SS  "LW STND SCORE 97"                         ;
label variable ER13009  "# IN FU"                                  ;
label variable ER13013  "# CHILDREN IN FU"                         ;
label variable ER33501  "1999 INTERVIEW NUMBER"                    ;
label variable ER33502  "SEQUENCE NUMBER                       99" ;
label variable ER33503  "RELATION TO HEAD                      99" ;
