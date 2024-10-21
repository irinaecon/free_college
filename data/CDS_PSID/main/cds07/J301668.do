#delimit ;
*  PSID DATA CENTER *****************************************************
   JOBID            : 301668                            
   DATA_DOMAIN      : CDSi                              
   USER_WHERE       : NULL                              
   FILE_TYPE        : CDS Kids Only                     
   OUTPUT_DATA_TYPE : ASCII                             
   STATEMENTS       : do                                
   CODEBOOK_TYPE    : PDF                               
   N_OF_VARIABLES   : 102                               
   N_OF_OBSERVATIONS: 1623                              
   MAX_REC_LENGTH   : 187                               
   DATE & TIME      : January 10, 2022 @ 13:56:35
*************************************************************************
;

infix
      KIDS                 1 - 1           KID07                2 - 4           ER30000              5 - 5     
      ER30001              6 - 9           ER30002             10 - 12          ER25001             13 - 13    
      ER25016             14 - 15          ER25020             16 - 17          ER33801             18 - 22    
      ER33802             23 - 24          ER33803             25 - 26          ER36001             27 - 27    
      ER36016             28 - 29          ER36017             30 - 32          ER36018             33 - 33    
      ER36019             34 - 36          ER36020             37 - 38          ER36021             39 - 41    
      ER36023             42 - 42          ER40874             43 - 45          ER40876             46 - 49    
      ER40885             50 - 52          ER40887             53 - 56     long ER40921             57 - 63    
 long ER40933             64 - 70     long ER40943             71 - 77     long ER41027             78 - 84    
      ER41037             85 - 86          ER41038             87 - 88          ER33901             89 - 93    
      ER33902             94 - 95          ER33903             96 - 97          ER33904             98 - 100   
      ER33906            101 - 104         ER33917            105 - 106         DEMREL07           107 - 107   
      RELPCG07           108 - 109         RELOCG07           110 - 111         CH07WT             112 - 117   
      PCHREL07           118 - 118         Q31IWAGE           119 - 123         ASMREL07           124 - 124   
      Q34LW1             125 - 125         Q34LW2             126 - 126         Q34LW3             127 - 127   
      Q34LW4             128 - 128         Q34LW5             129 - 129         Q34LW6             130 - 130   
      Q34LW7             131 - 131         Q34LW8             132 - 132         Q34LW9             133 - 133   
      Q34LW10            134 - 134         Q34LW11            135 - 135         Q34LW12            136 - 136   
      Q34LW13            137 - 137         Q34LW14            138 - 138         Q34LW15            139 - 139   
      Q34LW16            140 - 140         Q34LW17            141 - 141         Q34LW18            142 - 142   
      Q34LW19            143 - 143         Q34LW20            144 - 144         Q34LW21            145 - 145   
      Q34LW22            146 - 146         Q34LW23            147 - 147         Q34LW24            148 - 148   
      Q34LW25            149 - 149         Q34LW26            150 - 150         Q34LW27            151 - 151   
      Q34LW28            152 - 152         Q34LW29            153 - 153         Q34LW30            154 - 154   
      Q34LW31            155 - 155         Q34LW32            156 - 156         Q34LW33            157 - 157   
      Q34LW34            158 - 158         Q34LW35            159 - 159         Q34LW36            160 - 160   
      Q34LW37            161 - 161         Q34LW38            162 - 162         Q34LW39            163 - 163   
      Q34LW40            164 - 164         Q34LW41            165 - 165         Q34LW42            166 - 166   
      Q34LW43            167 - 167         Q34LW44            168 - 168         Q34LW45            169 - 169   
      Q34LW46            170 - 170         Q34LW47            171 - 171         Q34LW48            172 - 172   
      Q34LW49            173 - 173         Q34LW50            174 - 174         Q34LW51            175 - 175   
      Q34LW52            176 - 176         Q34LW53            177 - 177         Q34LW54            178 - 178   
      Q34LW55            179 - 179         Q34LW56            180 - 180         Q34LW57            181 - 181   
      Q34LW58            182 - 182         Q34LWRAW           183 - 184         Q34LWSS            185 - 187   
using J301668.txt, clear 
;
label variable KIDS            "Sum of All KID Flags"                     ;
label variable KID07           "KID2007 = 1 if exists, else missing"      ;
label variable ER30000         "RELEASE NUMBER"                           ;
label variable ER30001         "1968 INTERVIEW NUMBER"                    ;
label variable ER30002         "PERSON NUMBER                         68" ;
label variable ER25001         "RELEASE NUMBER"                           ;
label variable ER25016         "# IN FU"                                  ;
label variable ER25020         "# CHILDREN IN FU"                         ;
label variable ER33801         "2005 INTERVIEW NUMBER"                    ;
label variable ER33802         "SEQUENCE NUMBER                       05" ;
label variable ER33803         "RELATION TO HEAD                      05" ;
label variable ER36001         "RELEASE NUMBER"                           ;
label variable ER36016         "# IN FU"                                  ;
label variable ER36017         "AGE OF HEAD"                              ;
label variable ER36018         "SEX OF HEAD"                              ;
label variable ER36019         "AGE OF WIFE"                              ;
label variable ER36020         "# CHILDREN IN FU"                         ;
label variable ER36021         "AGE YOUNGEST CHILD"                       ;
label variable ER36023         "HEAD MARITAL STATUS"                      ;
label variable ER40874         "HEAD WEEKLY WORK HOURS-2006"              ;
label variable ER40876         "HEAD TOTAL HOURS OF WORK-2006"            ;
label variable ER40885         "WIFE WEEKLY WORK HOURS-2006"              ;
label variable ER40887         "WIFE TOTAL HOURS OF WORK-2006"            ;
label variable ER40921         "LABOR INCOME OF HEAD-2006"                ;
label variable ER40933         "LABOR INCOME OF WIFE-2006"                ;
label variable ER40943         "HEAD AND WIFE TAXABLE INCOME-2006"        ;
label variable ER41027         "TOTAL FAMILY INCOME-2006"                 ;
label variable ER41037         "COMPLETED ED-HD"                          ;
label variable ER41038         "COMPLETED ED-WF"                          ;
label variable ER33901         "2007 INTERVIEW NUMBER"                    ;
label variable ER33902         "SEQUENCE NUMBER                       07" ;
label variable ER33903         "RELATION TO HEAD                      07" ;
label variable ER33904         "AGE OF INDIVIDUAL                     07" ;
label variable ER33906         "YEAR INDIVIDUAL BORN                  07" ;
label variable ER33917         "YEARS COMPLETED EDUCATION             07" ;
label variable DEMREL07        "DEMOG FILE RELEASE NUMBER 07"             ;
label variable RELPCG07        "RELATION PCG 07"                          ;
label variable RELOCG07        "RELATION OCG 07"                          ;
label variable CH07WT          "CHILD INTERVIEW / CHILD WEIGHT 07"        ;
label variable PCHREL07        "PCG CHILD FILE RELEASE NUMBER 07"         ;
label variable Q31IWAGE        "CHILD AGE AT TIME OF PCG IW - YEARS 07"   ;
label variable ASMREL07        "ASSESSMENT FILE RELEASE NUMBER 07"        ;
label variable Q34LW1          "LW TEST ITEM 1 07"                        ;
label variable Q34LW2          "LW TEST ITEM 2 07"                        ;
label variable Q34LW3          "LW TEST ITEM 3 07"                        ;
label variable Q34LW4          "LW TEST ITEM 4 07"                        ;
label variable Q34LW5          "LW TEST ITEM 5 07"                        ;
label variable Q34LW6          "LW TEST ITEM 6 07"                        ;
label variable Q34LW7          "LW TEST ITEM 7 07"                        ;
label variable Q34LW8          "LW TEST ITEM 8 07"                        ;
label variable Q34LW9          "LW TEST ITEM 9 07"                        ;
label variable Q34LW10         "LW TEST ITEM 10 07"                       ;
label variable Q34LW11         "LW TEST ITEM 11 07"                       ;
label variable Q34LW12         "LW TEST ITEM 12 07"                       ;
label variable Q34LW13         "LW TEST ITEM 13 07"                       ;
label variable Q34LW14         "LW TEST ITEM 14 07"                       ;
label variable Q34LW15         "LW TEST ITEM 15 07"                       ;
label variable Q34LW16         "LW TEST ITEM 16 07"                       ;
label variable Q34LW17         "LW TEST ITEM 17 07"                       ;
label variable Q34LW18         "LW TEST ITEM 18 07"                       ;
label variable Q34LW19         "LW TEST ITEM 19 07"                       ;
label variable Q34LW20         "LW TEST ITEM 20 07"                       ;
label variable Q34LW21         "LW TEST ITEM 21 07"                       ;
label variable Q34LW22         "LW TEST ITEM 22 07"                       ;
label variable Q34LW23         "LW TEST ITEM 23 07"                       ;
label variable Q34LW24         "LW TEST ITEM 24 07"                       ;
label variable Q34LW25         "LW TEST ITEM 25 07"                       ;
label variable Q34LW26         "LW TEST ITEM 26 07"                       ;
label variable Q34LW27         "LW TEST ITEM 27 07"                       ;
label variable Q34LW28         "LW TEST ITEM 28 07"                       ;
label variable Q34LW29         "LW TEST ITEM 29 07"                       ;
label variable Q34LW30         "LW TEST ITEM 30 07"                       ;
label variable Q34LW31         "LW TEST ITEM 31 07"                       ;
label variable Q34LW32         "LW TEST ITEM 32 07"                       ;
label variable Q34LW33         "LW TEST ITEM 33 07"                       ;
label variable Q34LW34         "LW TEST ITEM 34 07"                       ;
label variable Q34LW35         "LW TEST ITEM 35 07"                       ;
label variable Q34LW36         "LW TEST ITEM 36 07"                       ;
label variable Q34LW37         "LW TEST ITEM 37 07"                       ;
label variable Q34LW38         "LW TEST ITEM 38 07"                       ;
label variable Q34LW39         "LW TEST ITEM 39 07"                       ;
label variable Q34LW40         "LW TEST ITEM 40 07"                       ;
label variable Q34LW41         "LW TEST ITEM 41 07"                       ;
label variable Q34LW42         "LW TEST ITEM 42 07"                       ;
label variable Q34LW43         "LW TEST ITEM 43 07"                       ;
label variable Q34LW44         "LW TEST ITEM 44 07"                       ;
label variable Q34LW45         "LW TEST ITEM 45 07"                       ;
label variable Q34LW46         "LW TEST ITEM 46 07"                       ;
label variable Q34LW47         "LW TEST ITEM 47 07"                       ;
label variable Q34LW48         "LW TEST ITEM 48 07"                       ;
label variable Q34LW49         "LW TEST ITEM 49 07"                       ;
label variable Q34LW50         "LW TEST ITEM 50 07"                       ;
label variable Q34LW51         "LW TEST ITEM 51 07"                       ;
label variable Q34LW52         "LW TEST ITEM 52 07"                       ;
label variable Q34LW53         "LW TEST ITEM 53 07"                       ;
label variable Q34LW54         "LW TEST ITEM 54 07"                       ;
label variable Q34LW55         "LW TEST ITEM 55 07"                       ;
label variable Q34LW56         "LW TEST ITEM 56 07"                       ;
label variable Q34LW57         "LW TEST ITEM 57 07"                       ;
label variable Q34LW58         "LW TEST ITEM 58 07"                       ;
label variable Q34LWRAW        "LW RAW SCORE 07"                          ;
label variable Q34LWSS         "LW STD SCORE 07"                          ;
