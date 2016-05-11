************************************************************************
************************************************************************
*    T H I S   D O E S   N O T   D O   A N Y T H I N G ,   Y E T ! ! ! 
************************************************************************
************************************************************************
* lexer.f
************************************************************************
* D. Everhart
* 11 MAY 2016
************************************************************************
* The MIT License (MIT)
* 
* Copyright (c) 2016 Daniel Everhart
* 
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the 
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject
* to the following conditions:
* 
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
* 
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
* 
************************************************************************
      PROGRAM LEXER
      IMPLICIT NONE
*
*     NALPHC - Number of letter characters
*     NNUMC  - NUmber of numeric characters
*     NWDSC  - Number of word symbols
*     NWSC   - Number of whitespace characters
*     NOPC   - Number of operator characters
*     NGRPC  - Number of grouing characters
*     NDLC   - Number of delimiter characters
*
      INTEGER*1   NALPHC,    NNUMC,    NWDSC,   NWSC,
     &              NOPC,    NGRPC,     NDLC,   NQTC
      PARAMETER ( NALPHC=26, NNUMC=10, NWDSC=2, NWSC=4,
     &              NOPC=19, NGRPC=6,   NDLC=3, NQTC=2)
*
*     NWD    - Number of word characters
*
      INTEGER*1 NWD
      PARAMETER (NWD=NALPHC*2+NNUMC+NWDSC)
*
*     NALLC  - Number of all characters
*
      INTEGER*1 NALLC
      PARAMETER (NALLC=NWD+NWSC+NOPC+NGRPC+NDLC+NQTC)
*
*     UALPHC - Upper case alpha characters
*     LALPHC - Lower case alpha characters
*     ALPHC  - Upper and Lower case alpha characters
*     NUMC   - Numeral characters
*     WDSMC  - Word symbol characters '.' and '_'
*     WORDC  - All word characters
*     WSC    - Whitespace characters
*     OPC    - Operator characters
*     GRPC   - Grouping characters
*     DELC   - Delimiting characters
*     QTC    - Quoting characters
*     ALLC   - All characters
*
      CHARACTER UALPHC*(NALPHC),LALPHC*(NALPHC),ALPHC*(2*NALPHC),
     &          NUMC*(NNUMC),WDSMC*(NWDSC),WORDC*(NWD),WSC*(NWSC),
     &          OPC*(NOPC),GRPC*(NGRPC),DELC*(NDLC),QTC*(NQTC),
     &          ALLC*(NALLC)
*
*     IALLC  - 1-BYTE integer array for all data.
*     IWSC   - 1-BYTE integer array for whitespace data used for 
*              initialization
*
      INTEGER*1 IALLC(NALLC),IWSC(NWSC)
*
      EQUIVALENCE (ALLC,   IALLC(1                          )),
     &            (WORDC,  IALLC(1                          )),
     &            (ALPHC,  IALLC(1                          )),
     &            (UALPHC, IALLC(1                          )),
     &            (LALPHC, IALLC(1+  NALPHC                 )),
     &            (NUMC,   IALLC(1+2*NALPHC                 )),
     &            (WDSMC,  IALLC(1+2*NALPHC+NNUMC           )),
     &            (WSC,    IALLC(1+NWD                      )),
     &            (IWSC,   IALLC(1+NWD                      )),
     &            (OPC,    IALLC(1+NWD+NWSC                 )),
     &            (GRPC,   IALLC(1+NWD+NWSC+NOPC            )),
     &            (DELC,   IALLC(1+NWD+NWSC+NOPC+NGRPC      )),
     &            (QTC,    IALLC(1+NWD+NWSC+NOPC+NGRPC+NDLC ))
*                                               
*        +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
*        | -1- | -2- | -3- | -4- | -5- | -6- | -7- | -8- | -9- |-10- |
*  +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
*  | -0- |  A  |  B  |  C  |  D  |  E  |  F  |  G  |  H  |  I  |  J  |
*  +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
*  | -1- |  K  |  L  |  M  |  N  |  O  |  P  |  Q  |  R  |  S  |  T  |
*  +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
*  | -2- |  U  |  V  |  W  |  X  |  Y  |  Z  |  a  |  b  |  c  |  d  |
*  +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
*  | -3- |  e  |  f  |  g  |  h  |  i  |  j  |  k  |  l  |  m  |  n  |
*  +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
*  | -4- |  o  |  p  |  q  |  r  |  s  |  t  |  u  |  v  |  w  |  x  |
*  +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
*  | -5- |  y  |  z  |  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |
*  +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
*  | -6- |  8  |  9  |  _  |  .  | SPC | \t  | \r  |  \n |  `  |  ~  |
*  +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
*  | -7- |  !  |  @  |  #  |  $  |  %  |  ^  |  &  |  *  |  -  |  +  |
*  +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
*  | -8- |  =  |  |  |  \  |  /  |  ?  |  <  |  >  |  (  |  [  |  {  |
*  +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
*  | -9- |  }  |  ]  |  )  |  ,  |  ;  |  :  |  '  |  "  |XXXXX|XXXXX|
*  +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
*                                               
      DATA UALPHC    /      'ABCDEFGHIJKLMNOPQRSTUVWXYZ'      /
      DATA LALPHC    /      'abcdefghijklmnopqrstuvwxyz'      /
      DATA NUMC      /      '0123456789'                      /
      DATA WDSMC     /      '_.'                              /
******DATA IWSC      /   SPACE, TAB,  CR, LF                  /
      DATA IWSC      /      32,   9,  13, 11                  /
      DATA OPC       /      '`~!@#$%^&*-+=|\/?<>'             /
      DATA GRPC      /      '([{}])'                          /
      DATA DELC      /      ',;:'                             /
      DATA QTC       /      '''"'                             /
*
      INTEGER SZ,UK,WS,WD,OP,GR,DL,QT
      PARAMETER (SZ=64,UK=0,WS=1,WD=2,OP=3,GR=4,DL=5,QT=6)
      CHARACTER*(SZ) RAW,BUFF
      INTEGER LNGTH,CURTYP,PRVTYP,BUFLN
      INTEGER I
*
*                  /           1         2         3     /
*                  /  123456789012345678901234567890123  /
*     DATA RAW     / ' "MOVE FWD" 1,3 4.e+3*sin(42*pi);' /
      PARAMETER (RAW=' "MOVE FWD" 1,3 4.e+3/-sin(42**pi);')
      DATA            CURTYP, PRVTYP, BUFLN 
     &             /      UK,     UK,     0 /
*
      LNGTH = LEN_TRIM(RAW)
      DO 1011 I=1,LNGTH
*
      IF     (SCAN(WORDC,RAW(I:I)).GT.0) THEN
        CURTYP = WD
      ELSEIF (SCAN(  WSC,RAW(I:I)).GT.0) THEN
        CURTYP = WS
      ELSEIF (SCAN(  OPC,RAW(I:I)).GT.0) THEN
        CURTYP = OP
      ELSEIF (SCAN( GRPC,RAW(I:I)).GT.0) THEN
        CURTYP = GR
      ELSEIF (SCAN( DELC,RAW(I:I)).GT.0) THEN
        CURTYP = DL
      ELSEIF (SCAN(  QTC,RAW(I:I)).GT.0) THEN
        CURTYP = QT
      ENDIF
*
***   IF (CURTYP.EQ.UK) WRITE(6,9011)    'UNKNOWN', RAW(I:I)
***   IF (CURTYP.EQ.WD) WRITE(6,9011)       'WORD', RAW(I:I)
***   IF (CURTYP.EQ.WS) WRITE(6,9011) 'WHITESPACE', RAW(I:I)
***   IF (CURTYP.EQ.OP) WRITE(6,9011)   'OPERATOR', RAW(I:I)
***   IF (CURTYP.EQ.GR) WRITE(6,9011)   'GROUPING', RAW(I:I)
***   IF (CURTYP.EQ.DL) WRITE(6,9011)  'DELIMITER', RAW(I:I)
***   IF (CURTYP.EQ.QT) WRITE(6,9011)      'QUOTE', RAW(I:I)
*
      IF (CURTYP.EQ.UK) THEN
        WRITE(6,9011)    'UNKNOWN TOKEN:', RAW(I:I)
        GOTO 8999
      ENDIF
*
      IF     (PRVTYP.EQ.UK) THEN
        PRVTYP = CURTYP
        BUFLN = 1
        BUFF(BUFLN:BUFLN) = RAW(I:I)
      ELSEIF ((PRVTYP.EQ.CURTYP).AND.
     &        (PRVTYP.EQ.WS     .OR.
     &         PRVTYP.EQ.WD     .OR.
     &         PRVTYP.EQ.OP)    ) THEN
        BUFLN = BUFLN + 1
        BUFF(BUFLN:BUFLN) = RAW(I:I)
      ELSE
        WRITE(6,9021) BUFF(1:BUFLN)
        BUFF = RAW(I:I)
        BUFLN = 1
        PRVTYP = CURTYP
      ENDIF
*
      CURTYP = UK
 1011 CONTINUE
*
      IF (BUFLN.GT.0) THEN
        WRITE(6,9021) BUFF(1:BUFLN)
        BUFF = RAW(I:I)
        BUFLN = 1
        PRVTYP = CURTYP
      ENDIF
*
      WRITE(6,9111) TRIM(RAW)
*
 8999 STOP
 9011 FORMAT('0*** ',A12,' CHAR: ',A1)
 9021 FORMAT('0*** TOKEN: -->',A,'<--')
 9111 FORMAT('0***   RAW: -->',A,'<--')
      END PROGRAM LEXER
************************************************************************
