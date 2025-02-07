Saturn Assembler                                       Thu Feb 06 17:40:09 2025
V3.0.9 (04/15/2007)                                      primes.a       Page    1

    1             * File  primes.s        1
    2             * RPL
    3             * File  primes.s        2
    4             * C:\Users\cmorgan\OneDrive - National Nonwovens\Documents\Debug4x\Projects\primes\sys\primes.s, part of the primes.hpp project, created by <> on 2/4/2025
    5             * File  primes.s        3
    6
    7             * File  primes.s        4
    8             *|| Reading from C:\Users\cmorgan\OneDrive - National Nonwovens\Documents\Debug4x\Projects\primes\sys\primes.h
    9
   10             =RomId  EQU     #200
   11             * ROMID set to 200
   12
   13
   14             *|| Resuming file primes.s at line 4
   15
   16             * RPL
   17             * File  primes.s        7
   18 00000 00000         CON(6)  =~xPRIMES
            0
   19 00006       =xPRIMES
   20             * File  primes.s        8
   21 00006 00000         CON(5)  =DOCOL
   22             * File  primes.s        9
   23 0000B 00000         CON(5)  =CK1NOLASTWD
   24             * File  primes.s        10
   25 00010 00000         CON(5)  =CK&DISPATCH0
   26             * File  primes.s        11
   27 00015 00000         CON(5)  =real
   28 0001A 00000         CON(5)  =DOCOL
   29             * if the argument from stack pos 1 is a real
   30             * File  primes.s        12
   31 0001F 00000         CON(5)  =DOLIST
   32 00024 00000         CON(5)  =DOLAM
   33 00029 10            CON(2)  1
   34 0002B E6            NIBASC  \n\
   35 0002D 00000         CON(5)  =SEMI
   36 00032 00000         CON(5)  =BIND
   37             * grab the argument from stack pos 1 and store to var n
   38             * File  primes.s        13
   39
   40             * File  primes.s        14
   41             * **note the following form of "LAM var func"
   42             * File  primes.s        15
   43             * I beleive this is basically treating the subroutine like a
   44             * File  primes.s        16
   45             * function from a procedural language
   46             * File  primes.s        17
   47             * the return is then left in position 1 of the stack
   48             * File  primes.s        18
   49             * if 'LAM var STO is used, the function changes it's input
   50             * File  primes.s        19
   51 00037 00000         CON(5)  =DOLAM
   52 0003C 10            CON(2)  1
   53 0003E E6            NIBASC  \n\
   54 00040 00000         CON(5)  =DOROMP
   55 00045 00000         CON(6)  =~LESS_THAN_4
            0
   56             * check if n < 1 or n == 2 or n == 3
   57             * File  primes.s        20
   58
   59             *                       if n is a list, this is our answer. time to exit
   60             * File  primes.s        22
   61 0004B 00000         CON(5)  =DUPTYPELIST?
   62 00050 00000         CON(5)  =IT
   63 00055 00000         CON(5)  =DOCOL
   64             * File  primes.s        23
   65 0005A 00000         CON(5)  =DOLAM
   66 0005F 10            CON(2)  1
   67 00061 E6            NIBASC  \n\
   68             * File  primes.s        24
   69 00063 00000         CON(5)  =SWAP
   70             * File  primes.s        25
   71 00068 00000         CON(5)  =DOROMP
   72 0006D 00000         CON(6)  =~GRACEFUL_EXIT
            0
   73             * File  primes.s        26
   74 00073 00000         CON(5)  =SEMI
   75             * File  primes.s        27
   76
   77             *                       create list, A and begin Seive of Eratothenes Algorithm
   78             * File  primes.s        29
   79 00078 00000         CON(5)  =DOLIST
   80 0007D 00000         CON(5)  =FALSE
   81 00082 00000         CON(5)  =SEMI
   82             * push a 0 on the begining of a list to represent 1
   83             *                       let a be a list of true bools in the amount of 2 through n
   84             * File  primes.s        31
   85 00087 00000         CON(5)  =DOLIST
   86 0008C 00000         CON(5)  =DOLAM
   87 00091 10            CON(2)  1
   88 00093 14            NIBASC  \A\
   89 00095 00000         CON(5)  =SEMI
   90 0009A 00000         CON(5)  =BIND
   91             * File  primes.s        32
   92 0009F 00000         CON(5)  =DOCOL
   93             * File  primes.s        33
   94 000A4 00000         CON(5)  =DOLAM
   95 000A9 10            CON(2)  1
   96 000AB E6            NIBASC  \n\
   97 000AD 00000         CON(5)  =%1
   98 000B2 00000         CON(5)  =%+
   99 000B7 00000         CON(5)  =COERCE
  100 000BC 00000         CON(5)  =DOBINT
  101 000C1 20000         CON(5)  #2
  102             * File  primes.s        34
  103 000C6 00000         CON(5)  =DO
  104             * File  primes.s        35
  105 000CB 00000         CON(5)  =DOLAM
  106 000D0 10            CON(2)  1
  107 000D2 14            NIBASC  \A\
  108 000D4 00000         CON(5)  =TRUE
  109 000D9 00000         CON(5)  =>TCOMP
  110             * File  primes.s        36
  111 000DE 00000         CON(5)  ='
  112 000E3 00000         CON(5)  =DOLAM
  113 000E8 10            CON(2)  1
  114 000EA 14            NIBASC  \A\
  115 000EC 00000         CON(5)  =STO
  116             * File  primes.s        37
  117 000F1 00000         CON(5)  =LOOP
  118             * File  primes.s        38
  119 000F6 00000         CON(5)  =SEMI
  120             * File  primes.s        39
  121
  122             *                       for i = 2, 3, 4, ..., not exceeding ƒn n do
  123             * File  primes.s        41
  124 000FB 00000         CON(5)  =DOCOL
  125             * File  primes.s        42
  126 00100 00000         CON(5)  =DOLAM
  127 00105 10            CON(2)  1
  128 00107 E6            NIBASC  \n\
  129 00109 00000         CON(5)  =%SQRT
  130 0010E 00000         CON(5)  =%IP
  131 00113 00000         CON(5)  =COERCE
  132 00118 00000         CON(5)  =DOBINT
  133 0011D 10000         CON(5)  #1
  134 00122 00000         CON(5)  =#+
  135 00127 00000         CON(5)  =DOBINT
  136 0012C 20000         CON(5)  #2
  137             * stop value is always -1
  138             * File  primes.s        43
  139 00131 00000         CON(5)  =DO
  140             * File  primes.s        44
  141 00136 00000         CON(5)  =DOLAM
  142 0013B 10            CON(2)  1
  143 0013D 14            NIBASC  \A\
  144 0013F 00000         CON(5)  =INDEX@
  145 00144 00000         CON(5)  =NTHELCOMP
  146 00149 00000         CON(5)  =DROP
  147 0014E 00000         CON(5)  =IT
  148 00153 00000         CON(5)  =DOCOL
  149             * if A[i] is true, i is INDEX@
  150             * File  primes.s        45
  151 00158 00000         CON(5)  =DOCOL
  152             * for j = i^2, i^2 +i, i^2 + 2i, i^2 + 3i, ..., not exceeding n do
  153             * File  primes.s        46
  154 0015D 00000         CON(5)  =%0
  155 00162 00000         CON(5)  =DOBINT
  156 00167 00000         CON(5)  #0
  157             * File  primes.s        47
  158 0016C 00000         CON(5)  =DOLIST
  159 00171 00000         CON(5)  =DOLAM
  160 00176 10            CON(2)  1
  161 00178 26            NIBASC  \b\
  162 0017A 00000         CON(5)  =DOLAM
  163 0017F 10            CON(2)  1
  164 00181 36            NIBASC  \c\
  165 00183 00000         CON(5)  =SEMI
  166 00188 00000         CON(5)  =BIND
  167             * File  primes.s        48
  168 0018D 00000         CON(5)  =INDEX@
  169 00192 00000         CON(5)  =UNCOERCE
  170 00197 00000         CON(5)  =%2
  171 0019C 00000         CON(5)  =%^
  172 001A1 00000         CON(5)  =COERCE
  173             * i^2
  174             * File  primes.s        49
  175 001A6 00000         CON(5)  =DOLAM
  176 001AB 10            CON(2)  1
  177 001AD 26            NIBASC  \b\
  178 001AF 00000         CON(5)  =COERCE
  179 001B4 00000         CON(5)  =INDEX@
  180 001B9 00000         CON(5)  =#*
  181 001BE 00000         CON(5)  =#+
  182             * + b * i
  183             * File  primes.s        50
  184 001C3 00000         CON(5)  ='
  185 001C8 00000         CON(5)  =DOLAM
  186 001CD 10            CON(2)  1
  187 001CF 36            NIBASC  \c\
  188 001D1 00000         CON(5)  =STO
  189             * update c
  190             * File  primes.s        51
  191
  192             * File  primes.s        52
  193 001D6 00000         CON(5)  =DOLAM
  194 001DB 10            CON(2)  1
  195 001DD E6            NIBASC  \n\
  196 001DF 00000         CON(5)  =COERCE
  197 001E4 00000         CON(5)  =DOLAM
  198 001E9 10            CON(2)  1
  199 001EB 36            NIBASC  \c\
  200             * File  primes.s        53
  201 001ED 00000         CON(5)  =DO
  202             * File  primes.s        54
  203 001F2 00000         CON(5)  =DOLAM
  204 001F7 10            CON(2)  1
  205 001F9 14            NIBASC  \A\
  206 001FB 00000         CON(5)  =JINDEX@
  207 00200 00000         CON(5)  =NTHELCOMP
  208 00205 00000         CON(5)  =DROP
  209 0020A 00000         CON(5)  =IT
  210 0020F 00000         CON(5)  =DOCOL
  211             * File  primes.s        55
  212 00214 00000         CON(5)  =FALSE
  213 00219 00000         CON(5)  =DOLAM
  214 0021E 10            CON(2)  1
  215 00220 36            NIBASC  \c\
  216 00222 00000         CON(5)  =DOLAM
  217 00227 10            CON(2)  1
  218 00229 14            NIBASC  \A\
  219 0022B 00000         CON(5)  =PUTLIST
  220             * File  primes.s        56
  221 00230 00000         CON(5)  ='
  222 00235 00000         CON(5)  =DOLAM
  223 0023A 10            CON(2)  1
  224 0023C 14            NIBASC  \A\
  225 0023E 00000         CON(5)  =STO
  226             * udate the list of prime bools
  227             * File  primes.s        57
  228 00243 00000         CON(5)  =SEMI
  229             * File  primes.s        58
  230 00248 00000         CON(5)  =DOLAM
  231 0024D 10            CON(2)  1
  232 0024F 26            NIBASC  \b\
  233 00251 00000         CON(5)  =%1
  234 00256 00000         CON(5)  =%+
  235 0025B 00000         CON(5)  ='
  236 00260 00000         CON(5)  =DOLAM
  237 00265 10            CON(2)  1
  238 00267 26            NIBASC  \b\
  239 00269 00000         CON(5)  =STO
  240             * File  primes.s        59
  241 0026E 00000         CON(5)  =JINDEX@
  242 00273 00000         CON(5)  =UNCOERCE
  243 00278 00000         CON(5)  =%2
  244 0027D 00000         CON(5)  =%^
  245 00282 00000         CON(5)  =COERCE
  246             * i^2
  247             * File  primes.s        60
  248 00287 00000         CON(5)  =DOLAM
  249 0028C 10            CON(2)  1
  250 0028E 26            NIBASC  \b\
  251 00290 00000         CON(5)  =COERCE
  252 00295 00000         CON(5)  =JINDEX@
  253 0029A 00000         CON(5)  =#*
  254 0029F 00000         CON(5)  =#+
  255             * + b * i
  256             * File  primes.s        61
  257 002A4 00000         CON(5)  ='
  258 002A9 00000         CON(5)  =DOLAM
  259 002AE 10            CON(2)  1
  260 002B0 36            NIBASC  \c\
  261 002B2 00000         CON(5)  =STO
  262             * update c
  263             * File  primes.s        62
  264 002B7 00000         CON(5)  =DOLAM
  265 002BC 10            CON(2)  1
  266 002BE E6            NIBASC  \n\
  267 002C0 00000         CON(5)  =COERCE
  268 002C5 00000         CON(5)  =DOLAM
  269 002CA 10            CON(2)  1
  270 002CC 36            NIBASC  \c\
  271 002CE 00000         CON(5)  =#<
  272 002D3 00000         CON(5)  =IT
  273 002D8 00000         CON(5)  =DOCOL
  274             * File  primes.s        63
  275 002DD 00000         CON(5)  =ExitAtLOOP
  276             * File  primes.s        64
  277 002E2 00000         CON(5)  =SEMI
  278             * File  primes.s        65
  279 002E7 00000         CON(5)  =LOOP
  280             * File  primes.s        66
  281 002EC 00000         CON(5)  =ABND
  282             * free b and c
  283             * File  primes.s        67
  284 002F1 00000         CON(5)  =SEMI
  285             * File  primes.s        68
  286 002F6 00000         CON(5)  =SEMI
  287             * File  primes.s        69
  288 002FB 00000         CON(5)  =LOOP
  289             * File  primes.s        70
  290 00300 00000         CON(5)  =SEMI
  291             * File  primes.s        71
  292
  293             * File  primes.s        72
  294 00305 00000         CON(5)  =DOCOL
  295             * File  primes.s        73
  296 0030A 00000         CON(5)  =NULL{}
  297 0030F 00000         CON(5)  =DOLIST
  298 00314 00000         CON(5)  =DOLAM
  299 00319 60            CON(2)  6
  300 0031B 07279         NIBASC  \primes\
            6D656
            37
  301 00327 00000         CON(5)  =SEMI
  302 0032C 00000         CON(5)  =BIND
  303             * File  primes.s        74
  304 00331 00000         CON(5)  =DOLAM
  305 00336 10            CON(2)  1
  306 00338 E6            NIBASC  \n\
  307 0033A 00000         CON(5)  =COERCE
  308 0033F 00000         CON(5)  =DOBINT
  309 00344 10000         CON(5)  #1
  310             * File  primes.s        75
  311 00349 00000         CON(5)  =DO
  312             * z = for 1 to n
  313             * File  primes.s        76
  314 0034E 00000         CON(5)  =DOLAM
  315 00353 10            CON(2)  1
  316 00355 14            NIBASC  \A\
  317 00357 00000         CON(5)  =INDEX@
  318 0035C 00000         CON(5)  =NTHELCOMP
  319 00361 00000         CON(5)  =DROP
  320 00366 00000         CON(5)  =IT
  321 0036B 00000         CON(5)  =DOCOL
  322             * if A[z] True
  323             * File  primes.s        77
  324 00370 00000         CON(5)  =DOLAM
  325 00375 60            CON(2)  6
  326 00377 07279         NIBASC  \primes\
            6D656
            37
  327 00383 00000         CON(5)  =INDEX@
  328 00388 00000         CON(5)  =UNCOERCE
  329 0038D 00000         CON(5)  =>TCOMP
  330 00392 00000         CON(5)  ='
  331 00397 00000         CON(5)  =DOLAM
  332 0039C 60            CON(2)  6
  333 0039E 07279         NIBASC  \primes\
            6D656
            37
  334 003AA 00000         CON(5)  =STO
  335             * File  primes.s        78
  336 003AF 00000         CON(5)  =SEMI
  337             * File  primes.s        79
  338 003B4 00000         CON(5)  =LOOP
  339             * File  primes.s        80
  340 003B9 00000         CON(5)  =SEMI
  341             * File  primes.s        81
  342
  343             * File  primes.s        82
  344 003BE 00000         CON(5)  =DOLAM
  345 003C3 60            CON(2)  6
  346 003C5 07279         NIBASC  \primes\
            6D656
            37
  347             * Display Primes
  348             * File  primes.s        83
  349 003D1 00000         CON(5)  =ABND
  350             * free primes
  351             * File  primes.s        84
  352 003D6 00000         CON(5)  =ABND
  353             * free A
  354             * File  primes.s        85
  355
  356             * File  primes.s        86
  357 003DB 00000         CON(5)  =DOROMP
  358 003E0 00000         CON(6)  =~GRACEFUL_EXIT
            0
  359             * File  primes.s        87
  360 003E6 00000         CON(5)  =SEMI
  361             * File  primes.s        88
  362 003EB 00000         CON(5)  =SEMI
  363             * File  primes.s        89
  364
  365             * a function that takes a real from pos 1
  366             * if the argument is greater than 4, it passes it back unchanged
  367             * if the argument is less than 4, it passes back a list
  368             * that represents what the primes are up to that argument
  369             * File  primes.s        94
  370 003F0       =LESS_THAN_4
  371             * File  primes.s        95
  372 003F0 00000         CON(5)  =DOCOL
  373             * File  primes.s        96
  374 003F5 00000         CON(5)  =DOLIST
  375 003FA 00000         CON(5)  =NULLLAM
  376 003FF 00000         CON(5)  =SEMI
  377 00404 00000         CON(5)  =BIND
  378             * File  primes.s        97
  379 00409 00000         CON(5)  =DOCOL
  380             * File  primes.s        98
  381 0040E 00000         CON(5)  =1GETLAM
  382 00413 00000         CON(5)  =%2
  383 00418 00000         CON(5)  =%<
  384 0041D 00000         CON(5)  =case
  385 00422 00000         CON(5)  =DOCOL
  386             * File  primes.s        99
  387 00427 00000         CON(5)  =NULL{}
  388             * File  primes.s        100
  389 0042C 00000         CON(5)  =SEMI
  390             * File  primes.s        101
  391 00431 00000         CON(5)  =1GETLAM
  392 00436 00000         CON(5)  =%2
  393 0043B 00000         CON(5)  =%=
  394 00440 00000         CON(5)  =case
  395 00445 00000         CON(5)  =DOCOL
  396             * File  primes.s        102
  397 0044A 00000         CON(5)  =%2
  398 0044F 00000         CON(5)  =ONE{}N
  399             * File  primes.s        103
  400 00454 00000         CON(5)  =SEMI
  401             * File  primes.s        104
  402 00459 00000         CON(5)  =1GETLAM
  403 0045E 00000         CON(5)  =%3
  404 00463 00000         CON(5)  =%=
  405 00468 00000         CON(5)  =case
  406 0046D 00000         CON(5)  =DOCOL
  407             * File  primes.s        105
  408 00472 00000         CON(5)  =%2
  409 00477 00000         CON(5)  =%3
  410 0047C 00000         CON(5)  =TWO{}N
  411             * File  primes.s        106
  412 00481 00000         CON(5)  =SEMI
  413             * File  primes.s        107
  414 00486 00000         CON(5)  =1GETLAM
  415             * File  primes.s        108
  416 0048B 00000         CON(5)  =SEMI
  417             * File  primes.s        109
  418 00490 00000         CON(5)  =ABND
  419             * File  primes.s        110
  420 00495 00000         CON(5)  =SEMI
  421             * File  primes.s        111
  422
  423             *       graceful exit (free all local variables) and end program
  424             * File  primes.s        113
  425 0049A       =GRACEFUL_EXIT
  426             * File  primes.s        114
  427 0049A 00000         CON(5)  =DOCOL
  428             * File  primes.s        115
  429 0049F 00000         CON(5)  =ABND
  430             * File  primes.s        116
  431 004A4 00000         CON(5)  =xKILL
  432             * File  primes.s        117
  433 004A9 00000         CON(5)  =SEMI
Saturn Assembler                                       Thu Feb 06 17:40:09 2025
V3.0.9 (04/15/2007)   Symbol Table                       primes.a       Page    2

 #*                                Ext                   -   180   253
 #+                                Ext                   -   134   181   254
 #<                                Ext                   -   271
 %+                                Ext                   -    98   234
 %0                                Ext                   -   154
 %1                                Ext                   -    97   233
 %2                                Ext                   -   170   243   382
                                         392   397   408
 %3                                Ext                   -   403   409
 %<                                Ext                   -   383
 %=                                Ext                   -   393   404
 %IP                               Ext                   -   130
 %SQRT                             Ext                   -   129
 %^                                Ext                   -   171   244
 '                                 Ext                   -   111   184   221
                                         235   257   330
 1GETLAM                           Ext                   -   381   391   402
                                         414
 >TCOMP                            Ext                   -   109   329
 ABND                              Ext                   -   281   349   352
                                         418   429
 BIND                              Ext                   -    36    90   166
                                         302   377
 CK&DISPATCH0                      Ext                   -    25
 CK1NOLASTWD                       Ext                   -    23
 COERCE                            Ext                   -    99   131   172
                                         178   196   245   251   267   307
 DO                                Ext                   -   103   139   201
                                         311
 DOBINT                            Ext                   -   100   132   135
                                         155   308
 DOCOL                             Ext                   -    21    28    63
                                          92   124   148   151   210   273   294
                                         321   372   379   385   395   406   427
 DOLAM                             Ext                   -    32    51    65
                                          86    94   105   112   126   141   159
                                         162   175   185   193   197   203   213
                                         216   222   230   236   248   258   264
                                         268   298   304   314   324   331   344
 DOLIST                            Ext                   -    31    79    85
                                         158   297   374
 DOROMP                            Ext                   -    54    71   357
 DROP                              Ext                   -   146   208   319
 DUPTYPELIST?                      Ext                   -    61
 ExitAtLOOP                        Ext                   -   275
 FALSE                             Ext                   -    80   212
=GRACEFUL_EXIT                     Rel    1178 #0000049A -   425
 INDEX@                            Ext                   -   144   168   179
                                         317   327
 IT                                Ext                   -    62   147   209
                                         272   320
 JINDEX@                           Ext                   -   206   241   252
=LESS_THAN_4                       Rel    1008 #000003F0 -   370
 LOOP                              Ext                   -   117   279   288
                                         338
 NTHELCOMP                         Ext                   -   145   207   318
 NULLLAM                           Ext                   -   375
 NULL{}                            Ext                   -   296   387
 ONE{}N                            Ext                   -   398
 PUTLIST                           Ext                   -   219
=RomId                             Abs     512 #00000200 -    10
 SEMI                              Ext                   -    35    74    81
                                          89   119   165   228   277   284   286
                                         290   301   336   340   360   362   376
                                         389   400   412   416   420   433
 STO                               Ext                   -   115   188   225
                                         239   261   334
 SWAP                              Ext                   -    69
 TRUE                              Ext                   -   108
 TWO{}N                            Ext                   -   410
 UNCOERCE                          Ext                   -   169   242   328
 case                              Ext                   -   384   394   405
 real                              Ext                   -    27
 xKILL                             Ext                   -   431
=xPRIMES                           Rel       6 #00000006 -    19
 ~GRACEFUL_EXIT                    Ext                   -    72   358
 ~LESS_THAN_4                      Ext                   -    55
 ~xPRIMES                          Ext                   -    18
Saturn Assembler                                       Thu Feb 06 17:40:09 2025
V3.0.9 (04/15/2007)   Statistics                         primes.a       Page    3

Input Parameters

  Source file name is primes.a

  Listing file name is primes.l

  Object file name is primes.o

  Flags set on command line
    None

Warnings:

  None

Errors:

  None
