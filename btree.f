************************************************************************
* D. Everhart 
* 22 NOV 2016
* BTREE     
************************************************************************
*              B I N A R Y   T R E E   A L G O R I T H M S
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
*************************************************************************
*            +------+       +------+------+
*  IND       |VALUES|       |BEFORE|AFTER |              44       
*            +------+       +------+------+             /  \      
*   1        |  44  |       |  4   |   2  |           42    64    
*   2        |  64  |       |  7   |   3  |          /     /   \  
*   3        |  87  |       |  5   |   0  |        14    60    87 
*   4        |  42  |       |  6   |   0  |          \        /   
*   5        |  85  |       |  8   |   0  |           18    85    
*   6        |  14  |       |  0   |  10  |                /      
*   7        |  60  |       |  0   |   0  |              78       
*   8        |  78  |       |  9   |   0  |             /         
*   9        |  72  |       |  0   |   0  |           72          
*  10        |  18  |       |  0   |   0  |                       
*            +------+       +------+------+                       
*
************************************************************************
      PROGRAM BTREE
      IMPLICIT NONE
      LOGICAL BTEXISTS
      INTEGER SZ,CT,RES,BTADDHC
      INTEGER HCI1,HCI2,HCI4,HCI8,HCR4,HCR8,HCSTR
      INTEGER I,J
      INTEGER*1 IDIV 
      INTEGER*1 INT1
      INTEGER*2 INT2 
      INTEGER*4 INT4
      INTEGER*8 INT8
      REAL*4    REAL4
      REAL*8    REAL8
      CHARACTER STR12*12,STR16*16,STR20*20,STR14*14
      PARAMETER(IDIV=11,SZ=64)
      INTEGER BT(3,SZ),SD(SZ)
      DATA REAL4,REAL8 / 4.0634579236475D8,4.0634579236475D8/
      DATA BT,CT,SD /192*0, 0, SZ*0/
*                123456789012345678901234
        INT1 = HUGE(INT1) / IDIV
        INT2 = HUGE(INT2) / IDIV
        INT4 = HUGE(INT4) / IDIV
        INT8 = HUGE(INT8) / IDIV
*                123456789012345678901234
        STR12 = ' Some String'
        STR16 = '   Some String  '
        STR20 = '1 34 tmp xyz  t '
        STR14 = '  ABCDEFGHIJK '
*
        WRITE(6,8100)
        WRITE(6,8101)  INT1, HCI1( INT1)
        WRITE(6,8102)  INT2, HCI2( INT2)
        WRITE(6,8104)  INT4, HCI4( INT4)
        WRITE(6,8108)  INT8, HCI8( INT8)
        WRITE(6,8200)
        WRITE(6,8204) REAL4, HCR4(REAL4)
        WRITE(6,8208) REAL8, HCR8(REAL8)
        WRITE(6,8300)
        WRITE(6,8312) STR12, HCSTR(STR12)
        WRITE(6,8316) STR16, HCSTR(STR16)
        WRITE(6,8320) STR20, HCSTR(STR20)
        WRITE(6,8324) STR14, HCSTR(STR14)

        RES = BTADDHC(BT,SZ,CT,44)    
        RES = BTADDHC(BT,SZ,CT,64)    
        RES = BTADDHC(BT,SZ,CT,87)    
        RES = BTADDHC(BT,SZ,CT,42)    
        RES = BTADDHC(BT,SZ,CT,85)    
        RES = BTADDHC(BT,SZ,CT,14)    
        RES = BTADDHC(BT,SZ,CT,60)    
        RES = BTADDHC(BT,SZ,CT,78)    
        RES = BTADDHC(BT,SZ,CT,72)    
        RES = BTADDHC(BT,SZ,CT,18)    

        DO 1001 I = 1, CT
          WRITE(6,'(3I8)') (BT(J,I),J=1,3)
 1001   CONTINUE

        I = 1
        CT = 0
        CALL HLSORT(HLSORT,BT,SZ,I,SD,CT,.TRUE.)
        WRITE(6,*)
        DO 1011 I = 1, CT
          WRITE(6,'(I8)') SD(I)
 1011   CONTINUE

        I = 1
        CT = 0
        CALL HLSORT(HLSORT,BT,SZ,I,SD,CT,.FALSE.)
        WRITE(6,*)
        DO 1021 I = 1, CT
          WRITE(6,'(I8)') SD(I)
 1021   CONTINUE

        WRITE(6,*) '44 exists ->', BTEXISTS(44,BT,SZ)
        WRITE(6,*) '12 exists ->', BTEXISTS(12,BT,SZ)
      STOP
 8100 FORMAT('0*** HASHTEST - INTEGER ********')
 8101 FORMAT('       HASHCODE OF INT1  ',I20,    ' = ',I11)
 8102 FORMAT('       HASHCODE OF INT2  ',I20,    ' = ',I11)
 8104 FORMAT('       HASHCODE OF INT4  ',I20,    ' = ',I11)
 8108 FORMAT('       HASHCODE OF INT8  ',I20,    ' = ',I11)
 8200 FORMAT('0*** HASHTEST - REAL    ********')
 8204 FORMAT('       HASHCODE OF REAL4 ',G20.10, ' = ',I11)
 8208 FORMAT('       HASHCODE OF REAL8 ',G20.10, ' = ',I11)
 8300 FORMAT('0*** HASHTEST - STRING  ********')
 8312 FORMAT('       HASHCODE OF STR12 ',12X,A12,' = ',I11)
 8316 FORMAT('       HASHCODE OF STR16 ', 8X,A16,' = ',I11)
 8320 FORMAT('       HASHCODE OF STR20 ', 4X,A20,' = ',I11)
 8324 FORMAT('       HASHCODE OF STR14 ',10X,A14,' = ',I11)

      END PROGRAM BTREE
************************************************************************
      LOGICAL FUNCTION BTEXISTS(VALUE,TREE,SZ)
      IMPLICIT NONE
      INTEGER VALUE,TREE(3,SZ),SZ
      INTEGER J
      BTEXISTS = .FALSE.
      IF (SZ.GT.0) THEN
        J =1
        DO  111 WHILE (J.LE.SZ)
          IF (VALUE.EQ.TREE(1,J)) THEN
            BTEXISTS = .TRUE.
            RETURN
          ELSE IF (VALUE.LT.TREE(1,J)) THEN
            J = TREE(2,J)
          ELSE
            J = TREE(3,J)
          END IF
          IF (J.EQ.0) EXIT
  111   CONTINUE
      END IF
  999 RETURN
      END FUNCTION BTEXISTS
********************************************************************************
      SUBROUTINE HLSORT(SUB,BT,SZ,I,ARRAY,C,ASCENDING)
      IMPLICIT NONE
      EXTERNAL SUB
      INTEGER SZ,BT(3,SZ),I
      INTEGER ARRAY(SZ),C
      LOGICAL ASCENDING
      IF (ASCENDING) THEN
        IF (BT(2,I).NE.0) CALL SUB(SUB,BT,SZ,BT(2,I),ARRAY,C,ASCENDING)
        IF (C.GT.SZ) RETURN
        C = C + 1
        ARRAY(C) = BT(1,I)
        IF (BT(3,I).NE.0) CALL SUB(SUB,BT,SZ,BT(3,I),ARRAY,C,ASCENDING)
      ELSE
        IF (BT(3,I).NE.0) CALL SUB(SUB,BT,SZ,BT(3,I),ARRAY,C,ASCENDING)
        IF (C.GT.SZ) RETURN
        C = C + 1
        ARRAY(C) = BT(1,I)
        IF (BT(2,I).NE.0) CALL SUB(SUB,BT,SZ,BT(2,I),ARRAY,C,ASCENDING)
      END IF
      END SUBROUTINE HLSORT
********************************************************************************
      INTEGER FUNCTION BTADDHC(HL,SZ,CT,HC)
      IMPLICIT NONE
      INTEGER   :: HL(3,SZ),SZ,CT,HC
      INTEGER   :: I,J

*
** Special case where this is the first value.
*
        IF (CT.EQ.0) THEN
          CT = 1
          HL(1,CT) = HC
          HL(2,1) = 0
          HL(3,1) = 0
          BTADDHC = CT
          RETURN
        ELSE IF (CT.GE.SZ) THEN
          WRITE(0,9001)
          BTADDHC = -1
          RETURN
        ENDIF

*
** Start at top of the table.
*
        J = 1
        DO 1001 WHILE (J.LE.CT)
          IF (J.EQ.0) RETURN

          IF (HC.LT.HL(1,J)) THEN
            I = 2
          ELSE IF (HC.GT.HL(1,J)) THEN
            I = 3
          ELSE
            I = 0
          END IF
          BTADDHC = J
          IF (I.EQ.0) RETURN
         
          IF (HL(I,J).EQ.0) THEN
            CT = CT + 1
            HL(1,CT) = HC
            HL(I,J) = CT
            BTADDHC = CT
            J = 0
          ELSE
            J = HL(I,J)
          END IF

 1001   CONTINUE

      RETURN
 9001 FORMAT('0*** HASH IS FULL.')
      END FUNCTION BTADDHC
********************************************************************************
      INTEGER*4 FUNCTION HCI1(VALUE)
      INTEGER*1 VALUE
      INTEGER*1 VALUES(4)
      INTEGER*4 RVALUE
      EQUIVALENCE(VALUES,RVALUE)
        RVALUE = 0
        VALUES(1) = VALUE
        HCI1 = RVALUE
      END FUNCTION HCI1
********************************************************************************
      INTEGER*4 FUNCTION HCI2(VALUE)
      INTEGER*2 VALUE
      INTEGER*2 VALUES(2)
      INTEGER*4 RVALUE
      EQUIVALENCE(VALUES,RVALUE)
        RVALUE = 0
        VALUES(1) = VALUE
        HCI2 = RVALUE
      END FUNCTION HCI2
********************************************************************************
      INTEGER*4 FUNCTION HCI4(VALUE)
      INTEGER*4 VALUE
        HCI4 = VALUE
      END FUNCTION HCI4
********************************************************************************
      INTEGER*4 FUNCTION HCI8(VALUE)
      INTEGER*8 VALUE
      INTEGER*4 VALUES(2)
      INTEGER*8 SETVALUE
      EQUIVALENCE(VALUES,SETVALUE)
        SETVALUE = VALUE
        HCI8 = IEOR(VALUES(1),VALUES(2))
      END FUNCTION HCI8
********************************************************************************
      INTEGER*4 FUNCTION HCR4(VALUE)
      REAL*4    VALUE,RVALUE
      INTEGER*4 IVALUE,HCI4
      EQUIVALENCE(RVALUE,IVALUE)
        RVALUE = VALUE
        HCR4 = HCI4(IVALUE)
      END FUNCTION HCR4
********************************************************************************
      INTEGER*4 FUNCTION HCR8(VALUE)
      REAL*8    VALUE,RVALUE
      INTEGER*8 IVALUE
      INTEGER*4 HCI8
      EQUIVALENCE(RVALUE,IVALUE)
        RVALUE = VALUE
        HCR8 = HCI8(IVALUE)
      END FUNCTION HCR8
********************************************************************************
* HASHCODE_STRING
*
* This subroutine will loop through in 4-byte words calculating the hashcode
* and IEOR (XOR) it with a running hashcode. This is done ignoring leading and
* trailing space characters.
*
* To illustrate, consider the following 14 character string:
*
*                               '  ABCDEFGHI   '
*
*         +---+---+---+---+---+---+---+---+---+---+---+---+---+---+
*         |   |   | A | B | C | D | E | F | G | H | I | J | K |   |
*         +---+---+---+---+---+---+---+---+---+---+---+---+---+---+
*
********************************************************************************
      INTEGER*4 FUNCTION HCSTR(VALUE)
      CHARACTER*(*) VALUE
      INTEGER*4   IVALUE,JVALUE
      CHARACTER*4 CVALUE
      INTEGER*4   I,ISTART,IEND,IREM
      EQUIVALENCE(CVALUE,JVALUE)
********************************************************************************
*  First, the start and end are found excluding spaces on the ends:
*                 ISTART                                  IEND
*                   3                                      13
*         +---+---+---+---+---+---+---+---+---+---+---+---+---+---+
*         |   |   | A | B | C | D | E | F | G | H | I | J | K |   |
*         +---+---+---+---+---+---+---+---+---+---+---+---+---+---+
*                 |<--- WORD ---->|<--- WORD ---->|<-- IREM ->|
*                                                      IREM = MOD(13-3+1,4) = 3
********************************************************************************
      ISTART = VERIFY(VALUE,' ',.FALSE.)
      IEND   = VERIFY(VALUE,' ', .TRUE.)
      IREM   = MOD(IEND-ISTART+1,4)
      IVALUE = 0
********************************************************************************
* Loop through first part of string excluding the remainder.
* In our example, the indicies will will be:        3:6,      7:10
* The 4-byte char words to be XORed are:         'ABCD',    'EFGH'
* The corresponding integers will be:        1145258561, 201589764 
********************************************************************************
      DO 1001 I = ISTART,IEND-IREM,4
        CVALUE = VALUE(I:I+3)
        IVALUE = IEOR(IVALUE,JVALUE)
*       WRITE(0,'(I4,X,A4,X,I16)') I,CVALUE,IVALUE
 1001 CONTINUE
********************************************************************************
* If there is a remainder put it into CVALUE LEFT JUSTIFIED.
* Note that I = 11 from the termination of the previous loop.  11 + 3 - 1 = 13
* In our example, the indicies will will be:        11:13
* The 4-byte character word to be XORed is:         'IJK'
*                                               743394893
********************************************************************************
      IF (IREM.GT.0) THEN
        CVALUE = VALUE(I:I+IREM-1)
        IVALUE = IEOR(IVALUE,JVALUE)
*       WRITE(0,'(I4,X,A4,X,I16)') I,CVALUE,IVALUE
      END IF
      HCSTR = IVALUE
      END FUNCTION HCSTR
********************************************************************************
