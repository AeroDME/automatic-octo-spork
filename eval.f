************************************************************************
* EVAL
* D. Everhart
* 05 JUN 2016
************************************************************************
* So far, this only scans for whitespace and alpha sequences.  So, it
* doesn't really do anything impressive, yet.  You may ask, "Why did
* you do it in FORTRAN 77?"  Well, that is a good question. I guess
* if I can do it in F77, then the algorithm can be adapted to pretty
* much any language.
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
************************************************************************
      PROGRAM EVAL
      IMPLICIT NONE
      CHARACTER*64 RAW
      PARAMETER (RAW='CALC A*.314**2*3')
      CALL SCAN(RAW)
 8999 GOTO 9999
 9999 STOP
      END PROGRAM EVAL
************************************************************************
      SUBROUTINE SCAN(INP)
      IMPLICIT NONE
      CHARACTER INP*(*),C
      INTEGER I,J,LNGTH,STATE,ACTN,NXTST,IND
      INTEGER STRT,ENDT
* ----------------------------------------------------------------------
      INTEGER    NCLS,     NRWS
      PARAMETER (NCLS=5,   NRWS=53)
*
      INTEGER    UNKN,      WHIT,      IDEN      
      PARAMETER (UNKN=0,    WHIT=101,  IDEN=201)
      INTEGER    INTG,      FLOT,       SCI,      SCIS
      PARAMETER (INTG=301,  FLOT=302,   SCI=303,  SCIS=304)
      INTEGER     POW,       MUL
      PARAMETER ( POW=401,   MUL=402)
*
      INTEGER     ERR,       NEW,      PUSH,      FLSH
      PARAMETER ( ERR=0,     NEW=101,  PUSH=102,  FLSH=103)
*
      INTEGER TABLE(NCLS,NRWS)
      COMMON /CASTAB/ TABLE
* ----------------------------------------------------------------------
      WRITE(6,9001) TRIM(INP)
*
****  INIT
      LNGTH =  LEN_TRIM(INP)
      STATE =  UNKN
      ACTN  =  ERR
      STRT  =  0
      ENDT  = -1
*
****  LOOP THROUGH CHARACTER ARRAY.
      DO 1001 I=1,LNGTH
*
****  FIND INDEX TABLE
      C = INP(I:I)
      IND = ICHAR(C)
      DO 1011 J=1,NRWS
      IF (TABLE(1,J).EQ.STATE) THEN
        IF (TABLE(2,J).LE.IND.AND.TABLE(3,J).GE.IND) THEN
          ACTN  = TABLE(4,J)
          NXTST = TABLE(5,J)
          EXIT
        END IF
      ELSE IF (TABLE(1,J).GT.STATE) THEN
        CALL SCNERR(INP,I,STATE)
      END IF
 1011 CONTINUE
*
****  PERFORM ACTION
      SELECT CASE (ACTN)
      CASE(ERR)
        CALL SCNERR(INP,I,STATE)
      CASE(NEW)
        STRT = I
        ENDT = STRT
      CASE(PUSH)
        ENDT = ENDT + 1
      CASE(FLSH)
        WRITE(6,9011) STATE,ENDT-STRT+1,INP(STRT:ENDT)
        STRT = I
        ENDT = STRT
      END SELECT
 1001 STATE = NXTST
*
      IF (ENDT.GE.STRT) THEN
        WRITE(6,9011) STATE,ENDT-STRT+1,INP(STRT:ENDT)
      END IF
*
 8999 RETURN
 9001 FORMAT('INPUT STRING: -->',A,'<--')
 9011 FORMAT('       TOKEN:    ',I4,X,I4,X,'-->',A,'<--')
      END SUBROUTINE SCAN
************************************************************************
      SUBROUTINE SCNERR(INP,IND,STATE)
      IMPLICIT NONE
      CHARACTER*(*) INP
      INTEGER IND,STATE
      WRITE(0,9001)
      WRITE(0,9011) TRIM(INP)
      WRITE(0,9011) REPEAT(' ',IND-1)//'^'
      WRITE(0,9021) ICHAR(INP(IND:IND))
      WRITE(0,9031) STATE
      GOTO 9999
 8999 RETURN
 9001 FORMAT('0*** ERROR SCANNING')
 9011 FORMAT('0***  ',A)
 9021 FORMAT('0***  CHARACTER CODE: ',I4)
 9031 FORMAT('0***      SCAN STATE: ',I4)
 9999 STOP 'SCAN ERROR'
      END SUBROUTINE SCNERR
************************************************************************
      BLOCK DATA ASCTAB
      IMPLICIT NONE
* ----------------------------------------------------------------------
      INTEGER    NCLS,     NRWS
      PARAMETER (NCLS=5,   NRWS=53)
*
      INTEGER    UNKN,      WHIT,      IDEN      
      PARAMETER (UNKN=0,    WHIT=101,  IDEN=201)
      INTEGER    INTG,      FLOT,       SCI,      SCIS
      PARAMETER (INTG=301,  FLOT=302,   SCI=303,  SCIS=304)
      INTEGER     POW,       MUL
      PARAMETER ( POW=401,   MUL=402)
*
      INTEGER     ERR,       NEW,      PUSH,      FLSH
      PARAMETER ( ERR=0,     NEW=101,  PUSH=102,  FLSH=103)
*
      INTEGER TABLE(NCLS,NRWS)
      COMMON /CASTAB/ TABLE
* ----------------------------------------------------------------------
      DATA TABLE /
*****      STATE       START          END      ACTION       NEXTSTATE
     &      UNKN,         32,          32,        NEW,           WHIT,     SPC  
     &      UNKN,         46,          46,        NEW,           FLOT,      .   
     &      UNKN,         48,          57,        NEW,           INTG,    0 - 9 
     &      UNKN,         65,          90,        NEW,           IDEN,    A - Z 
     &      UNKN,         95,          95,        NEW,           IDEN,      _   
     &      UNKN,         97,         122,        NEW,           IDEN,    a - z 
*
     &      WHIT,         32,          32,       PUSH,           WHIT,     SPC  
     &      WHIT,         42,          42,       FLSH,            MUL,      *   
     &      WHIT,         46,          46,       FLSH,           FLOT,      .   
     &      WHIT,         48,          57,       FLSH,           INTG,    0 - 9 
     &      WHIT,         65,          90,       FLSH,           IDEN,    A - Z 
     &      WHIT,         94,          94,       FLSH,            POW,      ^   
     &      WHIT,         95,          95,       FLSH,           IDEN,      _   
     &      WHIT,         97,         122,       FLSH,           IDEN,    a - z 
*
     &      IDEN,         32,          32,       FLSH,           WHIT,     SPC  
     &      IDEN,         42,          42,       FLSH,            MUL,      *   
     &      IDEN,         48,          57,       PUSH,           IDEN,    0 - 9 
     &      IDEN,         65,          90,       PUSH,           IDEN,    A - Z 
     &      IDEN,         94,          94,       FLSH,            POW,      ^   
     &      IDEN,         95,          95,       PUSH,           IDEN,      _   
     &      IDEN,         97,         122,       PUSH,           IDEN,    a - z 
*
     &      INTG,         32,          32,       FLSH,           WHIT,     SPC  
     &      INTG,         42,          42,       FLSH,            MUL,      *   
     &      INTG,         46,          46,       PUSH,           FLOT,      .   
     &      INTG,         48,          57,       PUSH,           INTG,    0 - 9 
     &      INTG,         94,          94,       FLSH,            POW,      ^   
     &      INTG,        101,         101,       PUSH,            SCI,      e   
*
     &      FLOT,         32,          32,       FLSH,           WHIT,     SPC  
     &      FLOT,         42,          42,       FLSH,            MUL,      *   
     &      FLOT,         48,          57,       PUSH,           FLOT,    0 - 9 
     &      FLOT,         94,          94,       FLSH,            POW,      ^   
     &      FLOT,        101,         101,       PUSH,            SCI,      e   
*
     &       SCI,         43,          43,       PUSH,           SCIS,      +   
     &       SCI,         45,          45,       PUSH,           SCIS,      -   
     &       SCI,         48,          57,       PUSH,           SCIS,    0 - 9 
*
     &      SCIS,         32,          32,       FLSH,           WHIT,     SPC  
     &      SCIS,         42,          42,       FLSH,            MUL,      *   
     &      SCIS,         48,          57,       PUSH,           SCIS,    0 - 9 
     &      SCIS,         94,          94,       FLSH,            POW,      ^   
*
     &       POW,         32,          32,       FLSH,           WHIT,     SPC  
     &       POW,         46,          46,       FLSH,           FLOT,      .   
     &       POW,         48,          57,       FLSH,           INTG,    0 - 9 
     &       POW,         65,          90,       FLSH,           IDEN,    A - Z 
     &       POW,         95,          95,       FLSH,           IDEN,      _   
     &       POW,         97,         122,       FLSH,           IDEN,    a - z 
*
     &       MUL,         32,          32,       FLSH,           WHIT,     SPC  
     &       MUL,         42,          42,       PUSH,            POW,      *   
     &       MUL,         46,          46,       FLSH,           FLOT,      .   
     &       MUL,         48,          57,       FLSH,           INTG,    0 - 9 
     &       MUL,         65,          90,       FLSH,           IDEN,    A - Z 
     &       MUL,         95,          95,       FLSH,           IDEN,      _   
     &       MUL,         97,         122,       FLSH,           IDEN,    a - z 
*
     &     99999,      99999,       99999,        ERR,           UNKN /
      END BLOCK DATA ASCTAB
************************************************************************
