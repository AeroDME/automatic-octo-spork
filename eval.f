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
      PARAMETER (RAW='MAZOVE FWD')
      CALL SCAN(RAW)
 8999 GOTO 9999
 9999 STOP
      END PROGRAM EVAL
************************************************************************
      SUBROUTINE SCAN(INP)
      IMPLICIT NONE
      CHARACTER INP*(*),C,BUFF*(128)
      INTEGER I,J,LNGTH,STATE,ACTN,NXTST,IND,BUFLN
* ----------------------------------------------------------------------
      INTEGER    NRWS
      PARAMETER (NRWS=7)
      INTEGER    UNKN,     WHIT,      IDEN
      PARAMETER (UNKN=0,   WHIT=1001, IDEN=1002 )
      INTEGER     ERR,      NEW,      PUSH,      FLSH
      PARAMETER ( ERR=0,    NEW=1001, PUSH=1002, FLSH=1003 )
      INTEGER TABLE(5,NRWS)
      COMMON /CASTAB/ TABLE
* ----------------------------------------------------------------------
      WRITE(6,9001) TRIM(INP)
*
****  INIT
      LNGTH = LEN_TRIM(INP)
      BUFLN = 0
      STATE = UNKN
      ACTN  = ERR
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
        CALL SCNERR(INP,I)
      END IF
 1011 CONTINUE
*
****  PERFORM ACTION
      SELECT CASE (ACTN)
      CASE(ERR)
        CALL SCNERR(INP,I)
      CASE(NEW)
        BUFLN = 1
        BUFF = C
      CASE(PUSH)
        BUFLN = BUFLN + 1
        BUFF(BUFLN:BUFLN) = C
      CASE(FLSH)
        WRITE(6,9031) STATE,BUFLN,BUFF(1:BUFLN)
        BUFLN = 1
        BUFF = C
      END SELECT
      STATE = NXTST
*
***   WRITE(6,9011) C,IND
***   WRITE(6,9021) BUFF(1:BUFLN)
 1001 CONTINUE
*
      IF (BUFLN.GT.0) THEN
        WRITE(6,9031) STATE,BUFLN,BUFF(1:BUFLN)
      END IF
*
 8999 RETURN
 9001 FORMAT('INPUT STRING: -->',A,'<--')
 9011 FORMAT('        CHAR: -->',A1,'<--',1X,I6)
 9021 FORMAT('      BUFFER: -->',A,'<--')
 9031 FORMAT('       TOKEN:    ',I4,X,I4,X,A)
      END SUBROUTINE SCAN
************************************************************************
      SUBROUTINE SCNERR(INP,IND)
      IMPLICIT NONE
      CHARACTER*(*) INP
      INTEGER IND
      WRITE(0,9001)
      WRITE(0,9011) TRIM(INP)
      WRITE(0,9011) REPEAT(' ',IND-1)//'^'
      WRITE(0,9021) ICHAR(INP(IND:IND))
      GOTO 9999
 8999 RETURN
 9001 FORMAT('0*** ERROR SCANNING')
 9011 FORMAT('0***  ',A)
 9021 FORMAT('0***  CHARACTER CODE: ',I3)
 9999 STOP 'SCAN ERROR'
      END SUBROUTINE SCNERR
************************************************************************
      BLOCK DATA ASCTAB
      IMPLICIT NONE
* ----------------------------------------------------------------------
      INTEGER    NRWS
      PARAMETER (NRWS=7)
      INTEGER    UNKN,     WHIT,      IDEN
      PARAMETER (UNKN=0,   WHIT=1001, IDEN=1002 )
      INTEGER     ERR,      NEW,      PUSH,      FLSH
      PARAMETER ( ERR=0,    NEW=1001, PUSH=1002, FLSH=1003 )
      INTEGER TABLE(5,NRWS)
      COMMON /CASTAB/ TABLE
* ----------------------------------------------------------------------
      DATA TABLE /
*****      STATE       START          END      ACTION       NEXTSTATE
     &      UNKN,         32,          32,        NEW,           WHIT, 
     &      UNKN,         65,          90,        NEW,           IDEN, 
     &      UNKN,         97,         122,        NEW,           IDEN, 
*
     &      WHIT,         32,          32,       PUSH,           WHIT, 
     &      WHIT,         65,          90,       FLSH,           IDEN, 
     &      WHIT,         97,         122,       FLSH,           IDEN, 
*
     &      IDEN,         32,          32,       FLSH,           WHIT, 
     &      IDEN,         65,          90,       PUSH,           IDEN, 
     &      IDEN,         97,         122,       PUSH,           IDEN, 
*
     &     99999,      99999,       99999,        ERR,           UNKN /
      END BLOCK DATA ASCTAB
************************************************************************
*               97 - 122  a - z
*               65 -  80  A - Z
************************************************************************
