!------------------------------------------------------------------------------+
! EVAL.F90
! D. Everhart
! 05 JUL 2016
!------------------------------------------------------------------------------+
! Based on the eval.f F77 version.  F90 features allow for simpler, more
! robust code.
!------------------------------------------------------------------------------+
! The MIT License (MIT)
! 
! Copyright (c) 2016 Daniel Everhart
! 
! Permission is hereby granted, free of charge, to any person obtaining
! a copy of this software and associated documentation files (the 
! "Software"), to deal in the Software without restriction, including
! without limitation the rights to use, copy, modify, merge, publish,
! distribute, sublicense, and/or sell copies of the Software, and to
! permit persons to whom the Software is furnished to do so, subject
! to the following conditions:
! 
! The above copyright notice and this permission notice shall be
! included in all copies or substantial portions of the Software.
! 
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
! EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
! MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
! IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
! CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
! TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
! SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
!------------------------------------------------------------------------------+
      MODULE MOD_VARIABLES
      INTEGER               :: VCNT
      INTEGER,PARAMETER     :: MAXVCNT=128, MAXNSZ=8, MAXVSZ=128
      CHARACTER(LEN=MAXNSZ) :: NAMES(MAXVCNT)
      CHARACTER(LEN=MAXVSZ) :: VALUES(MAXVCNT)
      DATA       VCNT,         NAMES,         VALUES /  &
                    0,    MAXVCNT*'',     MAXVCNT*'' /
      CONTAINS
!------------------------------------------------------------------------------+
      SUBROUTINE SETPAIR(PAIR)
      IMPLICIT NONE
!     SET a name/value PAIR variable given 'name=value' string.
!     -------------------------------------------------------------------------+
      CHARACTER(LEN=*),INTENT(IN) :: PAIR   ! example: 'pi=3.14'
!     -------------------------------------------------------------------------+
      INTEGER(KIND=4) :: I
      I = INDEX(PAIR, '=')
      IF (I.GT.0) THEN
        CALL SETVALUE(PAIR(1:I-1), PAIR(I+1:LEN_TRIM(PAIR)))
      END IF
      END SUBROUTINE SETPAIR
!------------------------------------------------------------------------------+
      SUBROUTINE SETVALUE(NAME, VALUE)
      IMPLICIT NONE
!     SET a name/value pair VARiable (Character).
!     -------------------------------------------------------------------------+
!     Sets variable value.  Variable names are case INSENSITIVE.  Variable
!     values are not altered by case.  Values are, adjusted left.
!     -------------------------------------------------------------------------+
      CHARACTER(LEN=*),INTENT(IN) :: NAME     ! Variable name
      CHARACTER(LEN=*),INTENT(IN) :: VALUE    ! Variable value
!     -------------------------------------------------------------------------+
      INTEGER(KIND=4)       :: I
      INTEGER(KIND=4)       :: IND
      CHARACTER(LEN=MAXNSZ) :: N
      CHARACTER(LEN=MAXVSZ) :: V
!     -------------------------------------------------------------------------+
!
!   FIRST, check to see if we have exceeded our bounds.
!
      IF (VCNT.EQ.MAXVCNT) THEN
        WRITE(0,999) 'Maximum variable count reached: ', MAXVCNT
        WRITE(0,998) 'Cannot write variable: ', NAME(:LEN_TRIM(NAME))
      END IF
!
!   Condition values.
!
      N = ADJUSTL(NAME)
      V = ADJUSTL(VALUE)
      CALL UCASE(N)
!
!   Set index of NAME if it already exists.
!     
      IND = INDEXOF(N)
!
!  If index was not found, then increment and set value.
!  Otherwise, set variable at index.
!
      IF (IND.LE.0) THEN
        IND = VCNT + 1
        NAMES(IND)  = N
        VALUES(IND) = V
        VCNT = IND
      ELSE
        VALUES(IND) = V
      END IF
!
  998 FORMAT(A,A)
  999 FORMAT(A,I6)
      END SUBROUTINE SETVALUE
!------------------------------------------------------------------------------+
      INTEGER(KIND=4) FUNCTION INDEXOF(NAME)
!     INDEXOF: Gets VARiable INDex based on name.
!     Returns the index of NAME if it exists, otherwise, 0.
      IMPLICIT NONE
!     -------------------------------------------------------------------------+
      CHARACTER(LEN=*),INTENT(IN) :: NAME  ! Name to locate in NAMES() array.
!     -------------------------------------------------------------------------+
      CHARACTER*(MAXNSZ) UN, CN
      INTEGER*4 I
      INDEXOF = 0
      UN = ADJUSTL(NAME)
      CALL UCASE(UN)
      DO 100 I=1,VCNT
      CN=NAMES(I)
      IF (CN(1:LEN_TRIM(CN)).EQ.UN(1:LEN_TRIM(UN))) THEN
        INDEXOF = I
        EXIT
      END IF
  100 CONTINUE
  999 RETURN
      END FUNCTION INDEXOF
!------------------------------------------------------------------------------+
! VALAT: Gets variable value at a given index.
!------------------------------------------------------------------------------+
      SUBROUTINE VALAT(IND, BUFF)
      IMPLICIT NONE
      INTEGER IND
      CHARACTER*(*) BUFF
      IF (IND.LE.VCNT) THEN
        BUFF = VALUES(IND)(:LEN_TRIM(VALUES(IND)))
      ELSE
        BUFF = ''
      END IF
      END SUBROUTINE VALAT
!------------------------------------------------------------------------------+
! GETVAL: Gets a variable value (character) related to a name.
!------------------------------------------------------------------------------+
! NAME  - I - Variable name
! VALUE - O - Variable value
!------------------------------------------------------------------------------+
! Gets variable value.  Variable names are case INSENSITIVE.  Variable
! values are not altered by case.  Values are, adjusted left.
!------------------------------------------------------------------------------+
      SUBROUTINE GETVAL(NAME, VALUE)
      IMPLICIT NONE
!
      INTEGER IND
      CHARACTER*(*) NAME, VALUE
      IND = INDEXOF(NAME)
      IF (IND.GT.0) THEN
        CALL VALAT(IND, VALUE)
      ELSE
        VALUE = ''
      END IF
      END SUBROUTINE GETVAL
!------------------------------------------------------------------------------+
! PVARS: Print table of VARiableS.
!------------------------------------------------------------------------------+
! UNT - I - File unit number to which to write data.
!------------------------------------------------------------------------------+
! Prints variables to desired output unit.
!------------------------------------------------------------------------------+
      SUBROUTINE PVARS(UNT)
      IMPLICIT NONE
      INTEGER*4 I, UNT
      WRITE(UNT,197)
  197 FORMAT('VARABLE',8X,'VALUE')
      DO 200 I=1,VCNT
      WRITE(UNT,199) NAMES(I),VALUES(I)
  199 FORMAT(A8,8X,A64)
  200 CONTINUE
      END SUBROUTINE PVARS
!------------------------------------------------------------------------------+
      END MODULE MOD_VARIABLES
!------------------------------------------------------------------------------+
      MODULE MOD_EVALUATE
      IMPLICIT NONE
      INTEGER,PARAMETER :: NCLS=5,   NRWS=143, MTOK=128
      INTEGER,PARAMETER :: UNKN=0,   WHIT=101, IDEN=200
      INTEGER,PARAMETER :: INTG=301, FLOT=302,  SCI=303, SCIS=304, NUMB=300
      INTEGER,PARAMETER ::  POW=401,  MUL=402,  DIV=403, FDIV=404, MODU=405
      INTEGER,PARAMETER ::  ADD=406,  SUB=407, OPER=400
      INTEGER,PARAMETER ::  GRP=500, OGRP=510, CGRP=520, OPAR=511, CPAR=521
      INTEGER,PARAMETER :: DELM=600, COMA=601
      INTEGER,PARAMETER :: FUNC=700
      INTEGER,PARAMETER ::  ERR=0,    NEW=101, PUSH=102, FLSH=103
      INTEGER           ::  TABLE(NCLS,NRWS),NTOK,TOKLST(3,MTOK)
      DATA  TABLE /   &
!                 STATE  START    END ACTION NXTSTATE                 
                  UNKN,    32,    32,   NEW,   WHIT,             &     !   SPC  
                  UNKN,    46,    46,   NEW,   FLOT,             &     !    .   
                  UNKN,    48,    57,   NEW,   INTG,             &     !  0 - 9 
                  UNKN,    65,    90,   NEW,   IDEN,             &     !  A - Z 
                  UNKN,    95,    95,   NEW,   IDEN,             &     !    _   
                  UNKN,    97,   122,   NEW,   IDEN,             &     !  a - z 
!                                                                     
                  WHIT,    32,    32,  PUSH,   WHIT,             &     !   SPC  
                  WHIT,    37,    37,  FLSH,   MODU,             &     !    %   
                  WHIT,    40,    40,  FLSH,   OPAR,             &     !    (   
                  WHIT,    41,    41,  FLSH,   CPAR,             &     !    )   
                  WHIT,    42,    42,  FLSH,    MUL,             &     !    *   
                  WHIT,    43,    43,  FLSH,    ADD,             &     !    +   
                  WHIT,    45,    45,  FLSH,    SUB,             &     !    -   
                  WHIT,    46,    46,  FLSH,   FLOT,             &     !    .   
                  WHIT,    47,    47,  FLSH,    DIV,             &     !    /   
                  WHIT,    48,    57,  FLSH,   INTG,             &     !  0 - 9 
                  WHIT,    65,    90,  FLSH,   IDEN,             &     !  A - Z 
                  WHIT,    92,    92,  FLSH,   FDIV,             &     !    \   
                  WHIT,    94,    94,  FLSH,    POW,             &     !    ^   
                  WHIT,    95,    95,  FLSH,   IDEN,             &     !    _   
                  WHIT,    97,   122,  FLSH,   IDEN,             &     !  a - z 
!                                                                     
                  IDEN,    32,    32,  FLSH,   WHIT,             &     !   SPC  
                  IDEN,    40,    40,  FLSH,   OPAR,             &     !    (   
                  IDEN,    41,    41,  FLSH,   CPAR,             &     !    )   
                  IDEN,    42,    42,  FLSH,    MUL,             &     !    *   
                  IDEN,    43,    43,  FLSH,    ADD,             &     !    +   
                  IDEN,    45,    45,  FLSH,    SUB,             &     !    -   
                  IDEN,    47,    47,  FLSH,    DIV,             &     !    /   
                  IDEN,    48,    57,  PUSH,   IDEN,             &     !  0 - 9 
                  IDEN,    65,    90,  PUSH,   IDEN,             &     !  A - Z 
                  IDEN,    94,    94,  FLSH,    POW,             &     !    ^   
                  IDEN,    95,    95,  PUSH,   IDEN,             &     !    _   
                  IDEN,    97,   122,  PUSH,   IDEN,             &     !  a - z 
!                                                                     
                  INTG,    32,    32,  FLSH,   WHIT,             &     !   SPC  
                  INTG,    37,    37,  FLSH,   MODU,             &     !    %   
                  INTG,    41,    41,  FLSH,   CPAR,             &     !    )   
                  INTG,    42,    42,  FLSH,    MUL,             &     !    *   
                  INTG,    43,    43,  FLSH,    ADD,             &     !    +   
                  INTG,    44,    44,  FLSH,   COMA,             &     !    +   
                  INTG,    45,    45,  FLSH,    SUB,             &     !    -   
                  INTG,    47,    47,  FLSH,    DIV,             &     !    /   
                  INTG,    46,    46,  PUSH,   FLOT,             &     !    .   
                  INTG,    48,    57,  PUSH,   INTG,             &     !  0 - 9 
                  INTG,    92,    92,  FLSH,   FDIV,             &     !    \   
                  INTG,    94,    94,  FLSH,    POW,             &     !    ^   
                  INTG,   101,   101,  PUSH,    SCI,             &     !    e   
!                                                                     
                  FLOT,    32,    32,  FLSH,   WHIT,             &     !   SPC  
                  FLOT,    37,    37,  FLSH,   MODU,             &     !    %   
                  FLOT,    41,    41,  FLSH,   CPAR,             &     !    )   
                  FLOT,    42,    42,  FLSH,    MUL,             &     !    *   
                  FLOT,    43,    43,  FLSH,    ADD,             &     !    +   
                  FLOT,    45,    45,  FLSH,    SUB,             &     !    -   
                  FLOT,    47,    47,  FLSH,    DIV,             &     !    /   
                  FLOT,    48,    57,  PUSH,   FLOT,             &     !  0 - 9 
                  FLOT,    92,    92,  FLSH,   FDIV,             &     !    \   
                  FLOT,    94,    94,  FLSH,    POW,             &     !    ^   
                  FLOT,   101,   101,  PUSH,    SCI,             &     !    e   
!                                                                     
                   SCI,    43,    43,  PUSH,   SCIS,             &     !    +   
                   SCI,    45,    45,  PUSH,   SCIS,             &     !    -   
                   SCI,    48,    57,  PUSH,   SCIS,             &     !  0 - 9 
!                                                                     
                  SCIS,    32,    32,  FLSH,   WHIT,             &     !   SPC  
                  SCIS,    37,    37,  FLSH,   MODU,             &     !    %   
                  SCIS,    41,    41,  FLSH,   CPAR,             &     !    )   
                  SCIS,    42,    42,  FLSH,    MUL,             &     !    *   
                  SCIS,    43,    43,  FLSH,    ADD,             &     !    +   
                  SCIS,    45,    45,  FLSH,    SUB,             &     !    -   
                  SCIS,    47,    47,  FLSH,    DIV,             &     !    /   
                  SCIS,    48,    57,  PUSH,   SCIS,             &     !  0 - 9 
                  SCIS,    92,    92,  FLSH,   FDIV,             &     !    \   
                  SCIS,    94,    94,  FLSH,    POW,             &     !    ^   
!                                                                     
                   POW,    32,    32,  FLSH,   WHIT,             &     !   SPC  
                   POW,    40,    40,  FLSH,   OPAR,             &     !    (   
                   POW,    46,    46,  FLSH,   FLOT,             &     !    .   
                   POW,    48,    57,  FLSH,   INTG,             &     !  0 - 9 
                   POW,    65,    90,  FLSH,   IDEN,             &     !  A - Z 
                   POW,    95,    95,  FLSH,   IDEN,             &     !    _   
                   POW,    97,   122,  FLSH,   IDEN,             &     !  a - z 
!                                                                     
                   MUL,    32,    32,  FLSH,   WHIT,             &     !   SPC  
                   MUL,    40,    40,  FLSH,   OPAR,             &     !    (   
                   MUL,    42,    42,  PUSH,    POW,             &     !    *   
                   MUL,    46,    46,  FLSH,   FLOT,             &     !    .   
                   MUL,    48,    57,  FLSH,   INTG,             &     !  0 - 9 
                   MUL,    65,    90,  FLSH,   IDEN,             &     !  A - Z 
                   MUL,    95,    95,  FLSH,   IDEN,             &     !    _   
                   MUL,    97,   122,  FLSH,   IDEN,             &     !  a - z 
!                                                                     
                   DIV,    32,    32,  FLSH,   WHIT,             &     !   SPC  
                   DIV,    40,    40,  FLSH,   OPAR,             &     !    (   
                   DIV,    46,    46,  FLSH,   FLOT,             &     !    .   
                   DIV,    48,    57,  FLSH,   INTG,             &     !  0 - 9 
                   DIV,    65,    90,  FLSH,   IDEN,             &     !  A - Z 
                   DIV,    95,    95,  FLSH,   IDEN,             &     !    _   
                   DIV,    97,   122,  FLSH,   IDEN,             &     !  a - z 
!                                                                     
                  FDIV,    32,    32,  FLSH,   WHIT,             &     !   SPC  
                  FDIV,    40,    40,  FLSH,   OPAR,             &     !    (   
                  FDIV,    46,    46,  FLSH,   FLOT,             &     !    .   
                  FDIV,    48,    57,  FLSH,   INTG,             &     !  0 - 9 
                  FDIV,    65,    90,  FLSH,   IDEN,             &     !  A - Z 
                  FDIV,    95,    95,  FLSH,   IDEN,             &     !    _   
                  FDIV,    97,   122,  FLSH,   IDEN,             &     !  a - z 
!                                                                     
                  MODU,    32,    32,  FLSH,   WHIT,             &     !   SPC  
                  MODU,    40,    40,  FLSH,   OPAR,             &     !    (   
                  MODU,    46,    46,  FLSH,   FLOT,             &     !    .   
                  MODU,    48,    57,  FLSH,   INTG,             &     !  0 - 9 
                  MODU,    65,    90,  FLSH,   IDEN,             &     !  A - Z 
                  MODU,    95,    95,  FLSH,   IDEN,             &     !    _   
                  MODU,    97,   122,  FLSH,   IDEN,             &     !  a - z 
!                                                                     
                   ADD,    32,    32,  FLSH,   WHIT,             &     !   SPC  
                   ADD,    40,    40,  FLSH,   OPAR,             &     !    (   
                   ADD,    46,    46,  FLSH,   FLOT,             &     !    .   
                   ADD,    48,    57,  FLSH,   INTG,             &     !  0 - 9 
                   ADD,    65,    90,  FLSH,   IDEN,             &     !  A - Z 
                   ADD,    95,    95,  FLSH,   IDEN,             &     !    _   
                   ADD,    97,   122,  FLSH,   IDEN,             &     !  a - z 
!                                                                     
                   SUB,    32,    32,  FLSH,   WHIT,             &     !   SPC  
                   SUB,    40,    40,  FLSH,   OPAR,             &     !    (   
                   SUB,    46,    46,  FLSH,   FLOT,             &     !    .   
                   SUB,    48,    57,  FLSH,   INTG,             &     !  0 - 9 
                   SUB,    65,    90,  FLSH,   IDEN,             &     !  A - Z 
                   SUB,    95,    95,  FLSH,   IDEN,             &     !    _   
                   SUB,    97,   122,  FLSH,   IDEN,             &     !  a - z 
!                                                                     
                  OPAR,    32,    32,  FLSH,   WHIT,             &     !   SPC  
                  OPAR,    40,    40,  FLSH,   OPAR,             &     !    (   
                  OPAR,    41,    41,  FLSH,   CPAR,             &     !    )   
                  OPAR,    46,    46,  FLSH,   FLOT,             &     !    .   
                  OPAR,    48,    57,  FLSH,   INTG,             &     !  0 - 9 
                  OPAR,    65,    90,  FLSH,   IDEN,             &     !  A - Z 
                  OPAR,    95,    95,  FLSH,   IDEN,             &     !    _   
                  OPAR,    97,   122,  FLSH,   IDEN,             &     !  a - z 
!                                                                     
                  CPAR,    32,    32,  FLSH,   WHIT,             &     !   SPC  
                  CPAR,    41,    41,  FLSH,   CPAR,             &     !    )   
                  CPAR,    42,    42,  FLSH,    MUL,             &     !    *   
                  CPAR,    43,    43,  FLSH,    ADD,             &     !    +   
                  CPAR,    45,    45,  FLSH,    SUB,             &     !    -   
                  CPAR,    47,    47,  FLSH,    DIV,             &     !    /   
                  CPAR,    94,    94,  FLSH,    POW,             &     !    ^   
!                                                                     
                  COMA,    32,    32,  FLSH,   WHIT,             &     !   SPC  
                  COMA,    40,    40,  FLSH,   OPAR,             &     !    (   
                  COMA,    46,    46,  FLSH,   FLOT,             &     !    .   
                  COMA,    48,    57,  FLSH,   INTG,             &     !  0 - 9 
                  COMA,    65,    90,  FLSH,   IDEN,             &     !  A - Z 
                  COMA,    95,    95,  FLSH,   IDEN,             &     !    _   
                  COMA,    97,   122,  FLSH,   IDEN,             &     !  a - z 
!                                                                     
                  99999, 99999, 99999,   ERR,   UNKN   /
      CONTAINS
!------------------------------------------------------------------------------+
      SUBROUTINE SCAN(INP)
      IMPLICIT NONE
      CHARACTER INP*(*),C
      INTEGER I,J,LNGTH,STATE,ACTN,NXTST,IND
      INTEGER STRT,ENDT
      WRITE(6,9001) TRIM(INP)
      WRITE(6,9011)
!
!     INIT
      LNGTH =  LEN_TRIM(INP)
      STATE =  UNKN
      ACTN  =  ERR
      NTOK  =  0
!
!     LOOP THROUGH CHARACTER ARRAY.
      DO 1001 I=1,LNGTH
!
!     FIND INDEX TABLE
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
        GOTO 8999
      END IF
 1011 CONTINUE
!
!     PERFORM ACTION
      SELECT CASE (ACTN)
      CASE(ERR)
        CALL SCNERR(INP,I,STATE)
        GOTO 8999
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
          WRITE(6,9021)STATE,STRT,ENDT, &
                       REPEAT(' ',STRT-1)//INP(STRT:ENDT)
        END IF
        STRT = I
        ENDT = STRT
      END SELECT
 1001 STATE = NXTST
!
      IF (ENDT.GE.STRT.AND.STATE.NE.WHIT) THEN
        NTOK = NTOK + 1
        TOKLST(1,NTOK) = STATE
        TOKLST(2,NTOK) = STRT
        TOKLST(3,NTOK) = ENDT
        WRITE(6,9021)STATE,STRT,ENDT, &
                       REPEAT(' ',STRT-1)//INP(STRT:ENDT)
      END IF
!
 8999 RETURN
 9001 FORMAT('     INPUT STRING:',2X,A)
 9011 FORMAT(' STATE START   END')
 9021 FORMAT(3(2X,I4),2X,A)
 9031 FORMAT(3(2X,I4))
      END SUBROUTINE SCAN
!------------------------------------------------------------------------------+
      SUBROUTINE SCNERR(INP,IND,STATE)
      IMPLICIT NONE
      CHARACTER*(*) INP
      INTEGER IND,STATE
      WRITE(0,9001)
      WRITE(0,9011) TRIM(INP)
      WRITE(0,9011) REPEAT(' ',IND-1)//'^'
      WRITE(0,9021) ICHAR(INP(IND:IND))
      WRITE(0,9031) STATE
!     GOTO 9999
 8999 RETURN
 9001 FORMAT('0*** ERROR SCANNING')
 9011 FORMAT('0***  ',A)
 9021 FORMAT('0***  CHARACTER CODE: ',I4)
 9031 FORMAT('0***      SCAN STATE: ',I4)
 9999 STOP 'SCAN ERROR'
      END SUBROUTINE SCNERR
!------------------------------------------------------------------------------+
      SUBROUTINE PARSE(STR)
      USE MOD_VARIABLES
      IMPLICIT NONE
      CHARACTER STR*(*)
      INTEGER I,J,NS,NQ,S(3,MTOK),Q(3,MTOK),IVAL(4)
      DOUBLE PRECISION FVAL(2)
      CHARACTER*128 SVAL
      EQUIVALENCE (FVAL,IVAL)
!
      FVAL = 0D0
      NS   = 0
      NQ   = 0
!
!     Begin building POSTFIX queue from INFIX.
      DO 1001 I=1,NTOK
      SELECT CASE(100*(TOKLST(1,I)/100))
!
      CASE (IDEN,NUMB)
        NQ = NQ + 1
        Q(1,NQ) = TOKLST(1,I)
        Q(2,NQ) = TOKLST(2,I)
        Q(3,NQ) = TOKLST(3,I)
!
      CASE (OPER)
        IF (NS.EQ.0) THEN
          NS = NS + 1
          S(1,NS) = TOKLST(1,I)      ! PUSH OPERATOR ONTO STACK
          S(2,NS) = TOKLST(2,I)
          S(3,NS) = TOKLST(3,I)
        ELSE
!         Note that if the stack holds an open parenthesis, this check
!         will always result in pushing the operator on the stack
!         because the parenthesis states are 500 while operator states
!         are 400.
          IF (S(1,NS).LE.TOKLST(1,I)) THEN
            NQ = NQ + 1
            Q(1,NQ) = S(1,NS)  ! POP OPERATOR FROM STACK INTO QUEUE
            Q(2,NQ) = S(2,NS)
            Q(3,NQ) = S(3,NS)
            S(1,NS) = TOKLST(1,I)      ! PUSH OPERATOR ONTO STACK
            S(2,NS) = TOKLST(2,I)
            S(3,NS) = TOKLST(3,I)
          ELSE
            NS = NS + 1
            S(1,NS) = TOKLST(1,I)      ! PUSH OPERATOR ONTO STACK
            S(2,NS) = TOKLST(2,I)
            S(3,NS) = TOKLST(3,I)
          ENDIF
        END IF
!
      CASE (GRP)
        SELECT CASE (TOKLST(1,I))
        CASE (OPAR)
!         If an identifier preceded this open parenthesis, then it
!         is a function identifier.
          IF (I.GT.1) THEN
            IF (TOKLST(1,I-1).EQ.IDEN) THEN
              Q(1,NQ) = FUNC
            ENDIF
          END IF
          NS = NS + 1
          S(1,NS) = TOKLST(1,I)      ! PUSH OPEN PARENTHESIS ONTO STACK
          S(2,NS) = TOKLST(2,I)
          S(3,NS) = TOKLST(3,I)
        CASE (CPAR)
          DO 1021 WHILE (NS.GT.0)
          IF (S(1,NS).EQ.OPAR) THEN
            NS = NS - 1
            EXIT
          ELSE 
            NQ = NQ + 1
            Q(1,NQ) = S(1,NS)  ! POP OPERATOR FROM STACK INTO QUEUE
            Q(2,NQ) = S(2,NS)
            Q(3,NQ) = S(3,NS)
            NS = NS - 1
          END IF
 1021     CONTINUE
        END SELECT
!
      CASE DEFAULT
        WRITE(0,9001) STR(TOKLST(2,I):TOKLST(3,I))
        GOTO 8999
      END SELECT
 1001 CONTINUE
      DO 2001 WHILE(NS.GT.0)
      NQ = NQ + 1
      Q(1,NQ) = S(1,NS)  ! POP OPERATOR FROM STACK INTO QUEUE
      Q(2,NQ) = S(2,NS)
      Q(3,NQ) = S(3,NS)
      NS = NS - 1
 2001 CONTINUE
!
!     Evaluate POSTFIX Queue
      DO 3001 I=1,NQ
      WRITE(6,9021) (Q(J,I),J=1,3),STR(Q(2,I):Q(3,I))
!     WRITE(6,9031) NS
      SELECT CASE(100*(Q(1,I)/100))
!
      CASE (IDEN)
        CALL GETVAL(STR(Q(2,I):Q(3,I)),SVAL)
        IF (LEN_TRIM(SVAL).EQ.0) THEN
          WRITE(0,9041)STR(Q(2,I):Q(3,I))
          GOTO 8999
        END IF
        READ(SVAL,*)FVAL(1)
        NS = NS + 1
        S(1,NS) = Q(1,I)
        S(2,NS) = IVAL(1)
        S(3,NS) = IVAL(2)
!
      CASE(NUMB)
        READ(STR(Q(2,I):Q(3,I)),*)FVAL(1)
        NS = NS + 1
        S(1,NS) = Q(1,I)
        S(2,NS) = IVAL(1)
        S(3,NS) = IVAL(2)
!
      CASE(OPER)
        IVAL(3) = S(2,NS)
        IVAL(4) = S(3,NS)
        NS = NS -1
        IVAL(1) = S(2,NS)
        IVAL(2) = S(3,NS)
        SELECT CASE (Q(1,I))
        CASE (POW)
          FVAL(1) = FVAL(1) ** FVAL(2)
        CASE (MUL)
          FVAL(1) = FVAL(1)  * FVAL(2)
        CASE (DIV)
          FVAL(1) = FVAL(1)  / FVAL(2)
        CASE (ADD)
          FVAL(1) = FVAL(1)  + FVAL(2)
        CASE (SUB)
          FVAL(1) = FVAL(1)  - FVAL(2)
        CASE DEFAULT
          WRITE(0,9001) STR(Q(2,I):Q(3,I))
          GOTO 8999
        END SELECT
        S(2,NS) = IVAL(1)
        S(3,NS) = IVAL(2)
!
      CASE DEFAULT
        WRITE(0,9001) STR(Q(2,I):Q(3,I))
        GOTO 8999
      END SElECT
 3001 CONTINUE
!     WRITE(6,9031) NS
      IF (NS.EQ.1) THEN
        IVAL(1) = S(2,NS)
        IVAL(2) = S(3,NS)
        CALL FMTDP(FVAL(1),SVAL,8)
        WRITE(6,9101) SVAL
      ELSE
        WRITE(0,'(A)')'ERROR EVALUATING STACK'
        GOTO 8999
      END IF
!
 8999 RETURN
 9001 FORMAT('0*** ERROR TOKEN: -->',A,'<--')
 9021 FORMAT(3(2X,I4),2X,A)
 9031 FORMAT('0***   STACKSIZE: ',I4)
 9041 FORMAT('0*** UNDEFINED VARIABLE: ',A)
 9101 FORMAT('0***      RESULT: ',A8)
 9999 STOP 'ERROR IN PARSE'
      END SUBROUTINE PARSE
!------------------------------------------------------------------------------+
      SUBROUTINE EVALUATE(STR)
      IMPLICIT NONE
      CHARACTER STR*(*)
      CALL SCAN(STR)
      CALL PARSE(STR)
      END SUBROUTINE EVALUATE
!------------------------------------------------------------------------------+
      END MODULE MOD_EVALUATE
!------------------------------------------------------------------------------+
      PROGRAM EVAL
      USE MOD_EVALUATE
      USE MOD_VARIABLES
      IMPLICIT NONE
      INTEGER SZ
      PARAMETER (SZ=64)
      CHARACTER*(SZ) RAW
      DO 1001 WHILE (.TRUE.)
      READ(5,9001,END=8999)RAW
      CALL UCASE(RAW)
      IF (TRIM(RAW).EQ.'P') THEN
        CALL PVARS(6)
        CYCLE
      ELSE IF (INDEX(RAW,'=').GT.0) THEN
        CALL SETPAIR(RAW)
        CYCLE
      END IF
      CALL EVALUATE(RAW)
 1001 CONTINUE
 8999 GOTO 9999
 9001 FORMAT(A64)
 9999 STOP
      END PROGRAM EVAL
!------------------------------------------------------------------------------+
! CHCASE: CHange CASE of string to upper or lower.
!------------------------------------------------------------------------------+
! STR - I/O - Character data to be converted to upper/lower case.
! TOUPR - I - .TRUE. if data is to be converted to upper case.
!------------------------------------------------------------------------------+
! Converts character data to upper or lower case.
!
! The difference between the character values of lower and upper
! case.  In ASCII, A < a, in EBCDIC, A > a.  So, if this case is
! ASCII, DIFF will be a negative number, and converting from lower
! to upper will subtract the magnitude of the offset.
!------------------------------------------------------------------------------+
      SUBROUTINE CHCASE(STR,TOUPR)
      IMPLICIT NONE
      CHARACTER*(*) STR
      CHARACTER C
      LOGICAL TOUPR
      INTEGER I,DIFF
      DIFF = ICHAR('A') - ICHAR('a')
      DO 1001 I = 1, LEN_TRIM(STR)
      C = STR(I:I)
      IF (TOUPR) THEN
        IF(C.ge.'a'.and.C.le.'z') STR(I:I) = CHAR(ICHAR(C)+DIFF)
      ELSE
        IF(C.ge.'A'.and.C.le.'Z') STR(I:I) = CHAR(ICHAR(C)-DIFF)
      ENDIF
 1001 CONTINUE
      END SUBROUTINE CHCASE
!------------------------------------------------------------------------------+
! LCASE: Lower CASE.
!------------------------------------------------------------------------------+
! STR - I/O - Character data to be converted to upper/lower case.
!------------------------------------------------------------------------------+
! Converts character data to lower case.  Wraps CHGCASE
!------------------------------------------------------------------------------+
      SUBROUTINE LCASE(STR)
      IMPLICIT NONE
      CHARACTER*(*) STR
      CALL CHCASE(STR, .FALSE.)
      END SUBROUTINE LCASE
!------------------------------------------------------------------------------+
! UCASE: Upper CASE.
!------------------------------------------------------------------------------+
! STR - I/O - Character data to be converted to upper/lower case.
!------------------------------------------------------------------------------+
! Converts character data to upper case.  Wraps CHGCASE
!------------------------------------------------------------------------------+
      SUBROUTINE UCASE(STR)
      IMPLICIT NONE
      CHARACTER*(*) STR
      CALL CHCASE(STR, .TRUE.)
      END SUBROUTINE UCASE
!------------------------------------------------------------------------------+
! FMTDP: Format a DOUBLE PRECISION value to fit in buffer with max
! precision.
!------------------------------------------------------------------------------+
!  VAL * I * DOUBLE PRECISION value to format
!   WD * I * Desired field width
! BUFF * O * Buffer in which to write formatted value
!------------------------------------------------------------------------------+
! Error will result in writing '*' characters to the buffer.
! The format for real numbers is:
!                           Fw.d
!                           Ew.dEe
!                           Gw.d
! W is the total width, D is the number of digits to the right of
! the floating point.  This subroutine starts with the total width,
! and figures out how much width is allowaded for D.
!------------------------------------------------------------------------------+
! W    * 'w' of Gw.d format descriptor
! D    * 'd' of Gw.d format descriptor
! E    * 'e' of Ew.dEe format descriptor
! IOS  * IOSTAT for read/write operations
! FIOS * IOSTAT for reading FVAL from TBUF
! EIOS * IOSTAT for reading EVAL from TBUF
! FSTR * Fw.d format string
! ESTR * Ew.dEe format string
! TBUF * Temporary buffer for writing formated real value
! FVAL * Value read from buffer based of Fw.d format string
! EVAL * Value read from buffer based of Ew.d format string
!------------------------------------------------------------------------------+
      SUBROUTINE FMTDP(VAL, BUFF, WD)
      IMPLICIT NONE
      DOUBLE PRECISION VAL
      CHARACTER*(*) BUFF
      INTEGER WD
!
      INTEGER W, D, E, POW10
      INTEGER IOS, FIOS, EIOS
      CHARACTER*16 FSTR, ESTR
      CHARACTER*32 TBUF
      DOUBLE PRECISION FVAL, EVAL
!
      FVAL = 0D0
      EVAL = 0D0
      W = MIN(LEN(BUFF), WD)
      DO 100 D = 1, W
  100 BUFF(D:D) = '*'
!
!  Special case when value is 0D0
!
      IF (VAL.EQ.0D0) THEN
        IF (WD.EQ.1) THEN
          BUFF = '.'
        ELSEIF (WD.EQ.2) THEN
          BUFF = '.0'
        ELSE
          WRITE(FSTR,920, IOSTAT=IOS) W, W-2
          WRITE(BUFF, FSTR, IOSTAT=IOS) VAL
        ENDIF
        RETURN
      ENDIF
!
!  Attempt to write value using Fw.d
!
      D = W
      TBUF = '*'
      DO 200 WHILE (TBUF(1:1).EQ.'*')
      D = D - 1
      IF (D.LT.0) EXIT
      WRITE(FSTR,920, IOSTAT=IOS) W, D
  200 WRITE(TBUF, FSTR, IOSTAT=IOS) VAL
      READ(TBUF,*,IOSTAT=FIOS) FVAL
!
!  Attempt to write value using Ew.d
!
      D = W
      E = POW10(ABS(REAL(POW10(VAL),8)))
      TBUF = '*'
      DO 300 WHILE (TBUF(1:1).EQ.'*')
      D = D - 1
      IF (D.LT.0) EXIT
      WRITE(ESTR,930, IOSTAT=IOS) W, D, E
  300 WRITE(TBUF, ESTR, IOSTAT=IOS) VAL
      READ(TBUF,*,IOSTAT=EIOS) EVAL
!
!  Determine which method yields a value which is nearest the 
!  original value.
!
      IF(FIOS.NE.0) THEN
        WRITE(BUFF, ESTR, IOSTAT=IOS) VAL
      ELSE
        IF (ABS(EVAL-VAL).GE.ABS(FVAL-VAL)) THEN
          WRITE(BUFF, FSTR, IOSTAT=IOS) VAL
        ELSE
          WRITE(BUFF, ESTR, IOSTAT=IOS) VAL
        END IF
      END IF
!
      RETURN
  920 FORMAT('(F'I2'.'I2')')
  930 FORMAT('(E'I2'.'I2'E'I1')')
      END SUBROUTINE FMTDP
!------------------------------------------------------------------------------+
! POW10
!------------------------------------------------------------------------------+
! Returns the power of 10 for the value.  12.3 -> 2  0.12 -> 0
!------------------------------------------------------------------------------+
!  X  * I * DOUBLE PRECISION for which to return the power of 10
!------------------------------------------------------------------------------+
      INTEGER FUNCTION POW10(X)
      IMPLICIT NONE
      DOUBLE PRECISION X, XC
!
!     In the special case that the value is exactly 0.0, we need to
!     exit.  Otherwise, there will be an endless loop.
!
      XC = ABS(X)
      POW10 = 0
      IF (XC.EQ.0D0) RETURN
!
!     This is set to 1 so that a value less than one will go through
!     one cycle setting it to zero to meet the expection in the 
!     description above.
!
      POW10 = 1
!
      DO 100 WHILE(XC.GE.10D0)
      XC = XC / 10D0
100   POW10 = POW10 + 1
!
      DO 200 WHILE(XC.LT.1D0)
      XC = XC * 10D0
200   POW10 = POW10 - 1
!
      RETURN
      END FUNCTION POW10
!------------------------------------------------------------------------------+
