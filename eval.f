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
      PARAMETER (RAW='2e3+.314 * 4')
      CALL SCAN(RAW)
      CALL PARSE(RAW)
 8999 GOTO 9999
 9999 STOP
      END PROGRAM EVAL
************************************************************************
      SUBROUTINE DOOPER(OPS,IDS,SZ,NOP,NID,STR)
      IMPLICIT NONE
      INTEGER SZ,NOP,NID,OPS(3,SZ),IDS(3,SZ)
      CHARACTER*(*) STR
* ----------------------------------------------------------------------
      INTEGER    NCLS,     NRWS,       MTOK
      PARAMETER (NCLS=5,   NRWS=106,   MTOK=128)
*
      INTEGER    UNKN,      WHIT,      IDEN      
      PARAMETER (UNKN=0,    WHIT=101,  IDEN=201)
      INTEGER    INTG,      FLOT,       SCI,      SCIS,      NUMB
      PARAMETER (INTG=301,  FLOT=302,   SCI=303,  SCIS=304,  NUMB=300)
      INTEGER     POW,       MUL,       DIV,      FDIV,      MODU
      PARAMETER ( POW=401,   MUL=402,   DIV=403,  FDIV=404,  MODU=405)
      INTEGER     ADD,       SUB,      OPER
      PARAMETER ( ADD=406,   SUB=407,  OPER=400)
*
      INTEGER     ERR,       NEW,      PUSH,      FLSH
      PARAMETER ( ERR=0,     NEW=101,  PUSH=102,  FLSH=103)
*
      INTEGER TABLE(NCLS,NRWS),NTOK,TOKLST(3,MTOK)
      COMMON /CASTAB/ TABLE,NTOK,TOKLST
* ----------------------------------------------------------------------
      DOUBLE PRECISION FVAL(2)
      INTEGER IVAL(4)
      EQUIVALENCE (FVAL,IVAL)
*
      IVAL(3) = IDS(2,NID)
      IVAL(4) = IDS(3,NID)
      NID = NID - 1
      IVAL(1) = IDS(2,NID)
      IVAL(2) = IDS(3,NID)
*
      SELECT CASE (OPS(1,NOP))
      CASE(MUL)
        FVAL(1) = FVAL(1) * FVAL(2)
      CASE(ADD)
        FVAL(1) = FVAL(1) + FVAL(2)
      CASE DEFAULT
        WRITE(0,9901) STR(OPS(2,NOP):OPS(3,NOP))
        GOTO 9999
      END SELECT
*
      IDS(2,NID) =  IVAL(1)
      IDS(3,NID) =  IVAL(2)
*
      NOP = NOP - 1
*
 8999 RETURN
 9901 FORMAT('0*** OPERATOR NOT SUPPORTED: ',A)
 9999 STOP
      END SUBROUTINE DOOPER
************************************************************************
      SUBROUTINE PARSE(STR)
      IMPLICIT NONE
      CHARACTER STR*(*)
* ----------------------------------------------------------------------
      INTEGER    NCLS,     NRWS,       MTOK
      PARAMETER (NCLS=5,   NRWS=106,   MTOK=128)
*
      INTEGER    UNKN,      WHIT,      IDEN      
      PARAMETER (UNKN=0,    WHIT=101,  IDEN=201)
      INTEGER    INTG,      FLOT,       SCI,      SCIS,      NUMB
      PARAMETER (INTG=301,  FLOT=302,   SCI=303,  SCIS=304,  NUMB=300)
      INTEGER     POW,       MUL,       DIV,      FDIV,      MODU
      PARAMETER ( POW=401,   MUL=402,   DIV=403,  FDIV=404,  MODU=405)
      INTEGER     ADD,       SUB,      OPER
      PARAMETER ( ADD=406,   SUB=407,  OPER=400)
*
      INTEGER     ERR,       NEW,      PUSH,      FLSH
      PARAMETER ( ERR=0,     NEW=101,  PUSH=102,  FLSH=103)
*
      INTEGER TABLE(NCLS,NRWS),NTOK,TOKLST(3,MTOK)
      COMMON /CASTAB/ TABLE,NTOK,TOKLST
* ----------------------------------------------------------------------
      INTEGER I,NOP,NID,OPSTK(3,MTOK),IDSTK(3,MTOK),IVAL(2)
      DOUBLE PRECISION FVAL
      EQUIVALENCE (FVAL,IVAL)
*
      FVAL = 0D0
      NOP  = 0
      NID  = 0
*
      DO 1001 I=1,NTOK
      SELECT CASE(100*(TOKLST(1,I)/100))
*
      CASE (NUMB)
        READ(STR(TOKLST(2,I):TOKLST(3,I)),*) FVAL
        PRINT*,'PUSHING ',FVAL
        NID = NID + 1
        IDSTK(1,NID) = TOKLST(1,I)
        IDSTK(2,NID) = IVAL(1)
        IDSTK(3,NID) = IVAL(2)
*
      CASE (OPER)
*****   Check to see if operator precendence requires us to push, or
*****   perform the operation
        IF (NOP.GT.0) THEN
          DO 3001 WHILE (OPSTK(1,NOP).LE.TOKLST(1,I))
            CALL DOOPER(OPSTK,IDSTK,MTOK,NOP,NID,STR)
            IF (NID.EQ.0.OR.NOP.EQ.0) EXIT
 3001     CONTINUE
        END IF
        PRINT*,'PUSHING ',STR(TOKLST(2,I):TOKLST(3,I))
        NOP = NOP + 1
        OPSTK(1,NOP) = TOKLST(1,I)
        OPSTK(2,NOP) = TOKLST(2,I)
        OPSTK(3,NOP) = TOKLST(3,I)
*
      CASE DEFAULT
        WRITE(0,9001) STR(TOKLST(2,I):TOKLST(3,I))
        GOTO 9999
      END SELECT
 1001 CONTINUE
      DO 6001 WHILE(NOP.GT.0)
 6001 CALL DOOPER(OPSTK,IDSTK,MTOK,NOP,NID,STR)
*
      IF (NID.NE.1) GOTO 9999
      PRINT*,'NID=',NID,' NOP=',NOP
      IVAL(1) = IDSTK(2,1)
      IVAL(2) = IDSTK(3,1)
      WRITE(6,9101) FVAL
 8999 RETURN
 9001 FORMAT('0*** ERROR TOKEN: -->',A,'<--')
 9101 FORMAT('0***      RESULT: ',E12.6)
 9999 STOP 'ERROR IN PARSE'
      END SUBROUTINE PARSE
************************************************************************
      SUBROUTINE SCAN(INP)
      IMPLICIT NONE
      CHARACTER INP*(*),C
      INTEGER I,J,LNGTH,STATE,ACTN,NXTST,IND
      INTEGER STRT,ENDT
* ----------------------------------------------------------------------
      INTEGER    NCLS,     NRWS,       MTOK
      PARAMETER (NCLS=5,   NRWS=106,   MTOK=128)
*
      INTEGER    UNKN,      WHIT,      IDEN      
      PARAMETER (UNKN=0,    WHIT=101,  IDEN=201)
      INTEGER    INTG,      FLOT,       SCI,      SCIS,      NUMB
      PARAMETER (INTG=301,  FLOT=302,   SCI=303,  SCIS=304,  NUMB=300)
      INTEGER     POW,       MUL,       DIV,      FDIV,      MODU
      PARAMETER ( POW=401,   MUL=402,   DIV=403,  FDIV=404,  MODU=405)
      INTEGER     ADD,       SUB,      OPER
      PARAMETER ( ADD=406,   SUB=407,  OPER=400)
*
      INTEGER     ERR,       NEW,      PUSH,      FLSH
      PARAMETER ( ERR=0,     NEW=101,  PUSH=102,  FLSH=103)
*
      INTEGER TABLE(NCLS,NRWS),NTOK,TOKLST(3,MTOK)
      COMMON /CASTAB/ TABLE,NTOK,TOKLST
* ----------------------------------------------------------------------
      WRITE(6,9001) TRIM(INP)
      WRITE(6,9011)
*
****  INIT
      LNGTH =  LEN_TRIM(INP)
      STATE =  UNKN
      ACTN  =  ERR
      NTOK  =  0
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
        IF (STATE.NE.WHIT) THEN
          NTOK = NTOK + 1
          TOKLST(1,NTOK) = STATE
          TOKLST(2,NTOK) = STRT
          TOKLST(3,NTOK) = ENDT
          WRITE(6,9021)STATE,STRT,ENDT,
     &                 REPEAT(' ',STRT-1)//INP(STRT:ENDT)
        END IF
        STRT = I
        ENDT = STRT
      END SELECT
 1001 STATE = NXTST
*
      IF (ENDT.GE.STRT.AND.STATE.NE.WHIT) THEN
        NTOK = NTOK + 1
        TOKLST(1,NTOK) = STATE
        TOKLST(2,NTOK) = STRT
        TOKLST(3,NTOK) = ENDT
        WRITE(6,9021)STATE,STRT,ENDT,
     &                 REPEAT(' ',STRT-1)//INP(STRT:ENDT)
      END IF
*
 8999 RETURN
 9001 FORMAT('     INPUT STRING:',2X,A)
 9011 FORMAT(' STATE START   END')
 9021 FORMAT(3(2X,I4),2X,A)
 9031 FORMAT(3(2X,I4))
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
      INTEGER    NCLS,     NRWS,       MTOK
      PARAMETER (NCLS=5,   NRWS=106,   MTOK=128)
*
      INTEGER    UNKN,      WHIT,      IDEN      
      PARAMETER (UNKN=0,    WHIT=101,  IDEN=201)
      INTEGER    INTG,      FLOT,       SCI,      SCIS,      NUMB
      PARAMETER (INTG=301,  FLOT=302,   SCI=303,  SCIS=304,  NUMB=300)
      INTEGER     POW,       MUL,       DIV,      FDIV,      MODU
      PARAMETER ( POW=401,   MUL=402,   DIV=403,  FDIV=404,  MODU=405)
      INTEGER     ADD,       SUB,      OPER
      PARAMETER ( ADD=406,   SUB=407,  OPER=400)
*
      INTEGER     ERR,       NEW,      PUSH,      FLSH
      PARAMETER ( ERR=0,     NEW=101,  PUSH=102,  FLSH=103)
*
      INTEGER TABLE(NCLS,NRWS),NTOK,TOKLST(3,MTOK)
      COMMON /CASTAB/ TABLE,NTOK,TOKLST
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
     &      WHIT,         37,          37,       FLSH,           MODU,      %   
     &      WHIT,         42,          42,       FLSH,            MUL,      *   
     &      WHIT,         43,          43,       FLSH,            ADD,      +   
     &      WHIT,         45,          45,       FLSH,            SUB,      -   
     &      WHIT,         46,          46,       FLSH,           FLOT,      .   
     &      WHIT,         47,          47,       FLSH,            DIV,      /   
     &      WHIT,         48,          57,       FLSH,           INTG,    0 - 9 
     &      WHIT,         65,          90,       FLSH,           IDEN,    A - Z 
     &      WHIT,         92,          92,       FLSH,           FDIV,      \   
     &      WHIT,         94,          94,       FLSH,            POW,      ^   
     &      WHIT,         95,          95,       FLSH,           IDEN,      _   
     &      WHIT,         97,         122,       FLSH,           IDEN,    a - z 
*
     &      IDEN,         32,          32,       FLSH,           WHIT,     SPC  
     &      IDEN,         42,          42,       FLSH,            MUL,      *   
     &      IDEN,         43,          43,       FLSH,            ADD,      +   
     &      IDEN,         45,          45,       FLSH,            SUB,      -   
     &      IDEN,         47,          47,       FLSH,            DIV,      /   
     &      IDEN,         48,          57,       PUSH,           IDEN,    0 - 9 
     &      IDEN,         65,          90,       PUSH,           IDEN,    A - Z 
     &      IDEN,         94,          94,       FLSH,            POW,      ^   
     &      IDEN,         95,          95,       PUSH,           IDEN,      _   
     &      IDEN,         97,         122,       PUSH,           IDEN,    a - z 
*
     &      INTG,         32,          32,       FLSH,           WHIT,     SPC  
     &      INTG,         37,          37,       FLSH,           MODU,      %   
     &      INTG,         42,          42,       FLSH,            MUL,      *   
     &      INTG,         43,          43,       FLSH,            ADD,      +   
     &      INTG,         45,          45,       FLSH,            SUB,      -   
     &      INTG,         47,          47,       FLSH,            DIV,      /   
     &      INTG,         46,          46,       PUSH,           FLOT,      .   
     &      INTG,         48,          57,       PUSH,           INTG,    0 - 9 
     &      INTG,         92,          92,       FLSH,           FDIV,      \   
     &      INTG,         94,          94,       FLSH,            POW,      ^   
     &      INTG,        101,         101,       PUSH,            SCI,      e   
*
     &      FLOT,         32,          32,       FLSH,           WHIT,     SPC  
     &      FLOT,         37,          37,       FLSH,           MODU,      %   
     &      FLOT,         42,          42,       FLSH,            MUL,      *   
     &      FLOT,         43,          43,       FLSH,            ADD,      +   
     &      FLOT,         45,          45,       FLSH,            SUB,      -   
     &      FLOT,         47,          47,       FLSH,            DIV,      /   
     &      FLOT,         48,          57,       PUSH,           FLOT,    0 - 9 
     &      FLOT,         92,          92,       FLSH,           FDIV,      \   
     &      FLOT,         94,          94,       FLSH,            POW,      ^   
     &      FLOT,        101,         101,       PUSH,            SCI,      e   
*
     &       SCI,         43,          43,       PUSH,           SCIS,      +   
     &       SCI,         45,          45,       PUSH,           SCIS,      -   
     &       SCI,         48,          57,       PUSH,           SCIS,    0 - 9 
*
     &      SCIS,         32,          32,       FLSH,           WHIT,     SPC  
     &      SCIS,         37,          37,       FLSH,           MODU,      %   
     &      SCIS,         42,          42,       FLSH,            MUL,      *   
     &      SCIS,         43,          43,       FLSH,            ADD,      +   
     &      SCIS,         45,          45,       FLSH,            SUB,      -   
     &      SCIS,         47,          47,       FLSH,            DIV,      /   
     &      SCIS,         48,          57,       PUSH,           SCIS,    0 - 9 
     &      SCIS,         92,          92,       FLSH,           FDIV,      \   
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
     &       DIV,         32,          32,       FLSH,           WHIT,     SPC  
     &       DIV,         46,          46,       FLSH,           FLOT,      .   
     &       DIV,         48,          57,       FLSH,           INTG,    0 - 9 
     &       DIV,         65,          90,       FLSH,           IDEN,    A - Z 
     &       DIV,         95,          95,       FLSH,           IDEN,      _   
     &       DIV,         97,         122,       FLSH,           IDEN,    a - z 
*
     &      FDIV,         32,          32,       FLSH,           WHIT,     SPC  
     &      FDIV,         46,          46,       FLSH,           FLOT,      .   
     &      FDIV,         48,          57,       FLSH,           INTG,    0 - 9 
     &      FDIV,         65,          90,       FLSH,           IDEN,    A - Z 
     &      FDIV,         95,          95,       FLSH,           IDEN,      _   
     &      FDIV,         97,         122,       FLSH,           IDEN,    a - z 
*
     &      MODU,         32,          32,       FLSH,           WHIT,     SPC  
     &      MODU,         46,          46,       FLSH,           FLOT,      .   
     &      MODU,         48,          57,       FLSH,           INTG,    0 - 9 
     &      MODU,         65,          90,       FLSH,           IDEN,    A - Z 
     &      MODU,         95,          95,       FLSH,           IDEN,      _   
     &      MODU,         97,         122,       FLSH,           IDEN,    a - z 
*
     &       ADD,         32,          32,       FLSH,           WHIT,     SPC  
     &       ADD,         46,          46,       FLSH,           FLOT,      .   
     &       ADD,         48,          57,       FLSH,           INTG,    0 - 9 
     &       ADD,         65,          90,       FLSH,           IDEN,    A - Z 
     &       ADD,         95,          95,       FLSH,           IDEN,      _   
     &       ADD,         97,         122,       FLSH,           IDEN,    a - z 
*
     &       SUB,         32,          32,       FLSH,           WHIT,     SPC  
     &       SUB,         46,          46,       FLSH,           FLOT,      .   
     &       SUB,         48,          57,       FLSH,           INTG,    0 - 9 
     &       SUB,         65,          90,       FLSH,           IDEN,    A - Z 
     &       SUB,         95,          95,       FLSH,           IDEN,      _   
     &       SUB,         97,         122,       FLSH,           IDEN,    a - z 
*
     &     99999,      99999,       99999,        ERR,           UNKN /
      END BLOCK DATA ASCTAB
************************************************************************
