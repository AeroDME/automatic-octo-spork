! ---------------------------------------------------------------------------------------------------------------------------------+
!                                                                                                                        D. Everhart
!                                                                                                                        05 MAY 2017
! ---------------------------------------------------------------------------------------------------------------------------------+
! The MIT License (MIT)
! 
! Copyright (c) 2016 Daniel Everhart
! 
! Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files
! (the  "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify,
! merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
! 
! The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
! 
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
! OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
! LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
! CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
! ---------------------------------------------------------------------------------------------------------------------------------+
MODULE MODHASH
IMPLICIT NONE
PRIVATE
PUBLIC :: CALCULATE_HASH,HASHTABLE_INSERT_AT
PUBLIC :: HASHTABLE
PUBLIC :: HASHTABLE_INIT
TYPE HASHTABLE
  INTEGER(KIND=4)             :: TABLE_SIZE
  INTEGER(KIND=4)             :: RECORD_SIZE
  INTEGER(KIND=4)             :: RECORD_COUNT
  INTEGER(KIND=4),ALLOCATABLE :: INDICIES(:,:)
  INTEGER(KIND=1),ALLOCATABLE :: RECORDS(:)
END TYPE HASHTABLE
! ---------------------------------------------------------------------------------------------------------------------------------+
INTEGER(KIND=4)             :: HASHTABLE_COUNT, HASHTABLES_SIZE
TYPE(HASHTABLE),ALLOCATABLE :: HASHTABLES(:)
! ---------------------------------------------------------------------------------------------------------------------------------+
INTERFACE CALCULATE_HASH
  MODULE PROCEDURE CALCULATE_HASH_INT1
  MODULE PROCEDURE CALCULATE_HASH_INT2
  MODULE PROCEDURE CALCULATE_HASH_INT4
  MODULE PROCEDURE CALCULATE_HASH_INT8
  MODULE PROCEDURE CALCULATE_HASH_REAL4
  MODULE PROCEDURE CALCULATE_HASH_REAL8
  MODULE PROCEDURE CALCULATE_HASH_STRING
END INTERFACE CALCULATE_HASH
! ---------------------------------------------------------------------------------------------------------------------------------+
INTERFACE HASHTABLE_INSERT_AT
  MODULE PROCEDURE HASHTABLE_INSERT_INT1_AT
! MODULE PROCEDURE HASHTABLE_INSERT_INT2_AT
! MODULE PROCEDURE HASHTABLE_INSERT_INT4_AT
! MODULE PROCEDURE HASHTABLE_INSERT_INT8_AT
! MODULE PROCEDURE HASHTABLE_INSERT_REAL4_AT
! MODULE PROCEDURE HASHTABLE_INSERT_REAL8_AT
! MODULE PROCEDURE HASHTABLE_INSERT_STRING_AT
END INTERFACE HASHTABLE_INSERT_AT
! ---------------------------------------------------------------------------------------------------------------------------------+
                                                           CONTAINS
! ---------------------------------------------------------------------------------------------------------------------------------+
SUBROUTINE HASHTABLE_INSERT_INT1_AT(HTABLE,VALUE)
TYPE(HASHTABLE),INTENT(INOUT) :: HTABLE
INTEGER(KIND=1),INTENT(IN)    :: VALUE
END SUBROUTINE HASHTABLE_INSERT_INT1_AT
! ---------------------------------------------------------------------------------------------------------------------------------+
SUBROUTINE HASHTABLE_INIT(HTABLE, RECORDSIZE, TABLESIZE)
TYPE(HASHTABLE),INTENT(INOUT) :: HTABLE
INTEGER(KIND=4),INTENT(IN)    :: RECORDSIZE,TABLESIZE
  HTABLE%TABLE_SIZE   = TABLESIZE
  HTABLE%RECORD_SIZE  = RECORDSIZE
  HTABLE%RECORD_COUNT = 0
  IF (.NOT.ALLOCATED(HTABLE%INDICIES)) ALLOCATE(HTABLE%INDICIES(         3,TABLESIZE))
  IF (.NOT.ALLOCATED(HTABLE%RECORDS))  ALLOCATE( HTABLE%RECORDS(RECORDSIZE*TABLESIZE))
END SUBROUTINE HASHTABLE_INIT
! ---------------------------------------------------------------------------------------------------------------------------------+
INTEGER(KIND=4) FUNCTION CALCULATE_HASH_INT1(VALUE)
INTEGER(KIND=1),INTENT(IN)   :: VALUE
INTEGER(KIND=1),DIMENSION(4) :: VALUES
INTEGER(KIND=4)              :: RVALUE = 0
EQUIVALENCE(VALUES,RVALUE)
  VALUES(1) = VALUE
  CALCULATE_HASH_INT1 = RVALUE
END FUNCTION CALCULATE_HASH_INT1
! ---------------------------------------------------------------------------------------------------------------------------------+
INTEGER(KIND=4) FUNCTION CALCULATE_HASH_INT2(VALUE)
INTEGER(KIND=2),INTENT(IN)   :: VALUE
INTEGER(KIND=2),DIMENSION(2) :: VALUES
INTEGER(KIND=4)              :: RVALUE = 0
EQUIVALENCE(VALUES,RVALUE)
  VALUES(1) = VALUE
  CALCULATE_HASH_INT2 = RVALUE
END FUNCTION CALCULATE_HASH_INT2
! ---------------------------------------------------------------------------------------------------------------------------------+
INTEGER(KIND=4) FUNCTION CALCULATE_HASH_INT4(VALUE)
INTEGER(KIND=4),INTENT(IN) :: VALUE
  CALCULATE_HASH_INT4 = VALUE
END FUNCTION CALCULATE_HASH_INT4
! ---------------------------------------------------------------------------------------------------------------------------------+
INTEGER(KIND=4) FUNCTION CALCULATE_HASH_INT8(VALUE)
INTEGER(KIND=8),INTENT(IN)   :: VALUE
INTEGER(KIND=4),DIMENSION(2) :: VALUES
INTEGER(KIND=8)              :: SETVALUE
EQUIVALENCE(VALUES,SETVALUE)
  SETVALUE = VALUE
  CALCULATE_HASH_INT8 = IEOR(VALUES(1),VALUES(2))
END FUNCTION CALCULATE_HASH_INT8
! ---------------------------------------------------------------------------------------------------------------------------------+
INTEGER(KIND=4) FUNCTION CALCULATE_HASH_REAL4(VALUE)
REAL   (KIND=4),INTENT(IN)   :: VALUE
INTEGER(KIND=4)              :: IVALUE
  IVALUE=TRANSFER(VALUE,IVALUE)
  CALCULATE_HASH_REAL4 = CALCULATE_HASH_INT4(IVALUE)
END FUNCTION CALCULATE_HASH_REAL4
! ---------------------------------------------------------------------------------------------------------------------------------+
INTEGER(KIND=4) FUNCTION CALCULATE_HASH_REAL8(VALUE)
REAL   (KIND=8),INTENT(IN)   :: VALUE
INTEGER(KIND=8)              :: IVALUE
  IVALUE=TRANSFER(VALUE,IVALUE)
  CALCULATE_HASH_REAL8 = CALCULATE_HASH_INT8(IVALUE)
END FUNCTION CALCULATE_HASH_REAL8
! ---------------------------------------------------------------------------------------------------------------------------------+
! CALCULATE_HASH_STRING
!
! This subroutine will loop through in 4-byte words calculating the hashcode and IEOR (XOR) it with a running hashcode. This is done
! ignoring leading and trailing space characters.
!
! To illustrate, consider the following 24 character string:
!
!                                            "  ABCDEFGHIJKLMNO       "
!
!                   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
!                   |   |   | A | B | C | D | E | F | G | H | I | J | K | L | M | N | O |   |   |   |   |   |   |   |
!                   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
!
! ---------------------------------------------------------------------------------------------------------------------------------+
INTEGER(KIND=4) FUNCTION CALCULATE_HASH_STRING(VALUE)
CHARACTER(LEN=*),INTENT(IN) :: VALUE
INTEGER  (KIND=4)           :: IVALUE,JVALUE
CHARACTER(LEN=4)            :: CVALUE
INTEGER  (KIND=4)           :: I,ISTART,IEND,IREM
EQUIVALENCE(CVALUE,JVALUE)
! ---------------------------------------------------------------------------------------------------------------------------------+
!  First, the start and end are found excluding spaces on the ends:
!                           ISTART                                                  IEND
!                             3                                                      17
!                   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
!                   |   |   | A | B | C | D | E | F | G | H | I | J | K | L | M | N | O |   |   |   |   |   |   |   |
!                   +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+
!                           |<--- WORD ---->|<--- WORD ---->|<--- WORD ---->|<-- IREM ->|
!                                                                                IREM = 17 - 3 + 1 = 3
! ---------------------------------------------------------------------------------------------------------------------------------+
  ISTART = VERIFY(VALUE,' ',.FALSE.)
  IEND   = VERIFY(VALUE,' ', .TRUE.)
  IREM   = MOD(IEND-ISTART+1,4)
  IVALUE = 0
! ---------------------------------------------------------------------------------------------------------------------------------+
! Loop through first part of string excluding the remainder.
! In our example, the indicies will will be:                    3:6,         7:10,         11:14
! The 4-byte character words to be XORed are:                "ABCD",       "EFGH",        "IJKL"
! The corresponding integers will be:                    1145258561,    201589764,    1078939213 
! ---------------------------------------------------------------------------------------------------------------------------------+
  DO I = ISTART,IEND-IREM,4
    CVALUE = VALUE(I:I+3)
    IVALUE = IEOR(IVALUE,JVALUE)
    WRITE(0,'(I4,X,A4,X,I16)') I,CVALUE,IVALUE
  END DO
! ---------------------------------------------------------------------------------------------------------------------------------+
! If there is a remainder put it into CVALUE LEFT JUSTIFIED.
! Note that I = 15 from the termination of the previous loop.                         15 + 3 - 1 = 17
! In our example, the indicies will will be:                                              15:17
! The 4-byte character word to be XORed is:                                              "MNO "
!                                                                                    1610612736
! ---------------------------------------------------------------------------------------------------------------------------------+
  IF (IREM.GT.0) THEN
    CVALUE = VALUE(I:I+IREM-1)
    IVALUE = IEOR(IVALUE,JVALUE)
    WRITE(0,'(I4,X,A4,X,I16)') I,CVALUE,IVALUE
  END IF
  CALCULATE_HASH_STRING = IVALUE
END FUNCTION CALCULATE_HASH_STRING
!
END MODULE MODHASH
! ---------------------------------------------------------------------------------------------------------------------------------+
PROGRAM HASHTEST
USE MODHASH
IMPLICIT NONE
INTEGER(KIND=1),PARAMETER :: IDIV = 11
INTEGER(KIND=1)           :: INT1 = HUGE(INT1) / IDIV
INTEGER(KIND=2)           :: INT2 = HUGE(INT2) / IDIV
INTEGER(KIND=4)           :: INT4 = HUGE(INT4) / IDIV
INTEGER(KIND=8)           :: INT8 = HUGE(INT8) / IDIV
REAL   (KIND=4)           :: REAL4 = 4.0634579236475D8
REAL   (KIND=8)           :: REAL8 = 4.0634579236475D8
!                                     123456789012345678901234
CHARACTER(LEN=12)         :: STR12 = ' Some String'
CHARACTER(LEN=16)         :: STR16 = '   Some String  '
CHARACTER(LEN=20)         :: STR20 = '1 34 tmp xyz  t '
CHARACTER(LEN=24)         :: STR24 = '  ABCDEFGHIJKLMNO       '
TYPE(HASHTABLE)           :: HT
  WRITE(6,'("0*** HASHTEST - INTEGER ********")')
  WRITE(6,'("     HASH VALUE OF INT1  ",I20,   " = ",I11)')  INT1, CALCULATE_HASH( INT1)
  WRITE(6,'("     HASH VALUE OF INT2  ",I20,   " = ",I11)')  INT2, CALCULATE_HASH( INT2)
  WRITE(6,'("     HASH VALUE OF INT4  ",I20,   " = ",I11)')  INT4, CALCULATE_HASH( INT4)
  WRITE(6,'("     HASH VALUE OF INT8  ",I20,   " = ",I11)')  INT8, CALCULATE_HASH( INT8)
  WRITE(6,'("0*** HASHTEST - REAL    ********")')
  WRITE(6,'("     HASH VALUE OF REAL4 ",G20.10," = ",I11)') REAL4, CALCULATE_HASH(REAL4)
  WRITE(6,'("     HASH VALUE OF REAL8 ",G20.10," = ",I11)') REAL8, CALCULATE_HASH(REAL8)
  WRITE(6,'("0*** HASHTEST - STRING  ********")')
  WRITE(6,'("     HASH VALUE OF STR12 ",12X,A12," = ",I11)') STR12, CALCULATE_HASH(STR12)
  WRITE(6,'("     HASH VALUE OF STR16 ", 8X,A16," = ",I11)') STR16, CALCULATE_HASH(STR16)
  WRITE(6,'("     HASH VALUE OF STR20 ", 4X A20," = ",I11)') STR20, CALCULATE_HASH(STR20)
  WRITE(6,'("     HASH VALUE OF STR24 ",    A24," = ",I11)') STR24, CALCULATE_HASH(STR24)

  CALL HASHTABLE_INIT(HT,8,64)
END PROGRAM HASHTEST
