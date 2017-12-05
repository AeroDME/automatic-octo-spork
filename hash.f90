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
                                                      MODULE MODHASHCODE
! ---------------------------------------------------------------------------------------------------------------------------------+
IMPLICIT NONE
PRIVATE
PUBLIC :: HASHCODE
! ---------------------------------------------------------------------------------------------------------------------------------+
INTERFACE HASHCODE
  MODULE PROCEDURE HASHCODE_INT1
  MODULE PROCEDURE HASHCODE_INT2
  MODULE PROCEDURE HASHCODE_INT4
  MODULE PROCEDURE HASHCODE_INT8
  MODULE PROCEDURE HASHCODE_REAL4
  MODULE PROCEDURE HASHCODE_REAL8
  MODULE PROCEDURE HASHCODE_STRING
END INTERFACE HASHCODE
! ---------------------------------------------------------------------------------------------------------------------------------+
                                                           CONTAINS
! ---------------------------------------------------------------------------------------------------------------------------------+
INTEGER(KIND=4) FUNCTION HASHCODE_INT1(VALUE)
INTEGER(KIND=1),INTENT(IN)   :: VALUE
INTEGER(KIND=1),DIMENSION(4) :: VALUES
INTEGER(KIND=4)              :: RVALUE = 0
EQUIVALENCE(VALUES,RVALUE)
  VALUES(1) = VALUE
  HASHCODE_INT1 = RVALUE
END FUNCTION HASHCODE_INT1
! ---------------------------------------------------------------------------------------------------------------------------------+
INTEGER(KIND=4) FUNCTION HASHCODE_INT2(VALUE)
INTEGER(KIND=2),INTENT(IN)   :: VALUE
INTEGER(KIND=2),DIMENSION(2) :: VALUES
INTEGER(KIND=4)              :: RVALUE = 0
EQUIVALENCE(VALUES,RVALUE)
  VALUES(1) = VALUE
  HASHCODE_INT2 = RVALUE
END FUNCTION HASHCODE_INT2
! ---------------------------------------------------------------------------------------------------------------------------------+
INTEGER(KIND=4) FUNCTION HASHCODE_INT4(VALUE)
INTEGER(KIND=4),INTENT(IN) :: VALUE
  HASHCODE_INT4 = VALUE
END FUNCTION HASHCODE_INT4
! ---------------------------------------------------------------------------------------------------------------------------------+
INTEGER(KIND=4) FUNCTION HASHCODE_INT8(VALUE)
INTEGER(KIND=8),INTENT(IN)   :: VALUE
INTEGER(KIND=4),DIMENSION(2) :: VALUES
INTEGER(KIND=8)              :: SETVALUE
EQUIVALENCE(VALUES,SETVALUE)
  SETVALUE = VALUE
  HASHCODE_INT8 = IEOR(VALUES(1),VALUES(2))
END FUNCTION HASHCODE_INT8
! ---------------------------------------------------------------------------------------------------------------------------------+
INTEGER(KIND=4) FUNCTION HASHCODE_REAL4(VALUE)
REAL   (KIND=4),INTENT(IN)   :: VALUE
INTEGER(KIND=4)              :: IVALUE
  IVALUE=TRANSFER(VALUE,IVALUE)
  HASHCODE_REAL4 = HASHCODE_INT4(IVALUE)
END FUNCTION HASHCODE_REAL4
! ---------------------------------------------------------------------------------------------------------------------------------+
INTEGER(KIND=4) FUNCTION HASHCODE_REAL8(VALUE)
REAL   (KIND=8),INTENT(IN)   :: VALUE
INTEGER(KIND=8)              :: IVALUE
  IVALUE=TRANSFER(VALUE,IVALUE)
  HASHCODE_REAL8 = HASHCODE_INT8(IVALUE)
END FUNCTION HASHCODE_REAL8
! ---------------------------------------------------------------------------------------------------------------------------------+
! HASHCODE_STRING
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
INTEGER(KIND=4) FUNCTION HASHCODE_STRING(VALUE)
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
!   WRITE(0,'(I4,X,A4,X,I16)') I,CVALUE,IVALUE
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
!   WRITE(0,'(I4,X,A4,X,I16)') I,CVALUE,IVALUE
  END IF
  HASHCODE_STRING = IVALUE
END FUNCTION HASHCODE_STRING
!
END MODULE MODHASHCODE
! ---------------------------------------------------------------------------------------------------------------------------------+
                                                     MODULE MODHASHLIST
! ---------------------------------------------------------------------------------------------------------------------------------+
PUBLIC :: HASHLIST_ADD
PUBLIC :: HASHLIST_RESIZE
PUBLIC :: HASHLIST_SORT
TYPE HASHLIST
  INTEGER(KIND=4)             :: CAPACITY
  INTEGER(KIND=4)             :: COUNT
  INTEGER(KIND=4),ALLOCATABLE :: HASH(:,:)
END TYPE HASHLIST
! ---------------------------------------------------------------------------------------------------------------------------------+
                                                           CONTAINS
! ---------------------------------------------------------------------------------------------------------------------------------+
SUBROUTINE HASHLIST_RESIZE(HTABLE, CAPACITY)
TYPE(HASHLIST),INTENT(INOUT) :: HTABLE
INTEGER(KIND=4),INTENT(IN)   :: CAPACITY
INTEGER(KIND=4),ALLOCATABLE  :: TEMP(:,:)
INTEGER(KIND=4)              :: OLDCAPACITY,I,J,IERR
  IF (ALLOCATED(HTABLE%HASH)) THEN
    OLDCAPACITY = SIZE(HTABLE%HASH,2)
    ALLOCATE(TEMP(3,OLDCAPACITY),STAT=IERR)
    IF (IERR.NE.0) THEN
      WRITE(0,'(A)') '0*** UNABLE TO ALLOCATE TEMPORARY MEMORY FOR HASHLIST.'
      STOP
    END IF
    TEMP = HTABLE%HASH
    CALL HASHLIST_ALLOCATE(HTABLE, CAPACITY)
    DO J = 1, MIN(OLDCAPACITY,CAPACITY)
      DO I = 1,3
        HTABLE%HASH(I,J) = TEMP(I,J)
      END DO
    END DO
    DEALLOCATE(TEMP,STAT=IERR)
    IF (IERR.NE.0) THEN
      WRITE(0,'(A)') '0*** UNABLE TO DEALLOCATE TEMPORARY MEMORY FOR HASHLIST.'
      STOP
    END IF
  ELSE
    CALL HASHLIST_ALLOCATE(HTABLE, CAPACITY)
  END IF
END SUBROUTINE HASHLIST_RESIZE
! ---------------------------------------------------------------------------------------------------------------------------------+
SUBROUTINE HASHLIST_ALLOCATE(HTABLE, CAPACITY)
TYPE(HASHLIST),INTENT(INOUT) :: HTABLE
INTEGER(KIND=4),INTENT(IN)   :: CAPACITY
INTEGER(KIND=4)              :: IERR
  IF (ALLOCATED(HTABLE%HASH)) THEN
    DEALLOCATE(HTABLE%HASH,STAT=IERR)
    IF (IERR.NE.0) THEN
      WRITE(0,'(A)') '0*** UNABLE TO DEALLOCATE MEMORY FOR HASHLIST.'
      STOP
    END IF
  END IF
  ALLOCATE(HTABLE%HASH(3,CAPACITY),STAT=IERR)
  IF (IERR.NE.0) THEN
    WRITE(0,'(A)') '0*** UNABLE TO ALLOCATE MEMORY FOR HASHLIST.'
    STOP
  END IF
  HTABLE%HASH = 0
  HTABLE%CAPACITY = CAPACITY
END SUBROUTINE HASHLIST_ALLOCATE
! ---------------------------------------------------------------------------------------------------------------------------------+
INTEGER(KIND=4) FUNCTION ADDHASHCODE(HL,HASHCODE)
TYPE(HASHLIST),INTENT(INOUT) :: HL
INTEGER(KIND=4),INTENT(IN)   :: HASHCODE
INTEGER(KIND=4)              :: I,J

  IF (HL%COUNT.EQ.0) THEN                   ! Special case where this is the first value.
    HL%COUNT = 1
    HL%HASH(1,HL%COUNT) = HASHCODE
    HL%HASH(2,1) = 0
    HL%HASH(3,1) = 0
    ADDHASHCODE = HL%COUNT
    RETURN
  ELSE IF (HL%COUNT.EQ.SIZE(HL%HASH,2)) THEN
    WRITE(0,'(A)') '0*** HASH IS FULL.'
    FLUSH(0)
    STOP
  ENDIF

  J = 1                                      ! Start at top of the table.
  DO WHILE (J.LE.HL%COUNT)
    IF (J.EQ.0) RETURN

    IF (HASHCODE.LT.HL%HASH(1,J)) THEN
      I = 2
    ELSE IF (HASHCODE.GT.HL%HASH(1,J)) THEN
      I = 3
    ELSE
      I = 0
    END IF
    ADDHASHCODE = J
    IF (I.EQ.0) RETURN
    
    IF (HL%HASH(I,J).EQ.0) THEN
      HL%COUNT = HL%COUNT + 1
      HL%HASH(1,HL%COUNT) = HASHCODE
      HL%HASH(I,J) = HL%COUNT
      ADDHASHCODE = HL%COUNT
      J = 0
    ELSE
      J = HL%HASH(I,J)
    END IF

  END DO

END FUNCTION ADDHASHCODE
! ---------------------------------------------------------------------------------------------------------------------------------+
SUBROUTINE HASHLIST_SORT(HL, ARRAY, ASCENDING)
TYPE(HASHLIST), INTENT(IN)             :: HL
INTEGER(KIND=4),INTENT(INOUT)          :: ARRAY(:)
LOGICAL(KIND=4),INTENT(IN),   OPTIONAL :: ASCENDING
INTEGER(KIND=4)                        :: I,C
LOGICAL(KIND=4)                        :: ASC
  IF (PRESENT(ASCENDING)) THEN
    ASC = ASCENDING
  ELSE
    ASC = .TRUE.
  END IF
  I = 1
  C = 0
  CALL HASHLIST_SORTED(HL,ARRAY,I,C,ASC)
END SUBROUTINE HASHLIST_SORT
! ---------------------------------------------------------------------------------------------------------------------------------+
RECURSIVE SUBROUTINE HASHLIST_SORTED(HL,ARRAY,I,C,ASCENDING)
TYPE(HASHLIST), INTENT(IN)    :: HL
INTEGER(KIND=4),INTENT(INOUT) :: ARRAY(:)
INTEGER(KIND=4),INTENT(IN)    :: I
INTEGER(KIND=4),INTENT(INOUT) :: C
LOGICAL(KIND=4),INTENT(IN)    :: ASCENDING
IF (ASCENDING) THEN
  IF (HL%HASH(2,I).NE.0) CALL HASHLIST_SORTED(HL,ARRAY,HL%HASH(2,I),C,ASCENDING)
  IF (C.GT.SIZE(ARRAY)) RETURN
  C = C + 1
  ARRAY(C) = HL%HASH(1,I)
  IF (HL%HASH(3,I).NE.0) CALL HASHLIST_SORTED(HL,ARRAY,HL%HASH(3,I),C,ASCENDING)
ELSE
  IF (HL%HASH(3,I).NE.0) CALL HASHLIST_SORTED(HL,ARRAY,HL%HASH(3,I),C,ASCENDING)
  IF (C.GT.SIZE(ARRAY)) RETURN
  C = C + 1
  ARRAY(C) = HL%HASH(1,I)
  IF (HL%HASH(2,I).NE.0) CALL HASHLIST_SORTED(HL,ARRAY,HL%HASH(2,I),C,ASCENDING)
END IF
END SUBROUTINE HASHLIST_SORTED
! ---------------------------------------------------------------------------------------------------------------------------------+
END MODULE MODHASHLIST
! ---------------------------------------------------------------------------------------------------------------------------------+
PROGRAM HASHTEST
USE MODHASHCODE
USE MODHASHLIST
IMPLICIT NONE
INTEGER(KIND=4)           :: RES,I,J
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
TYPE(HASHLIST)            :: HL
INTEGER(KIND=4)           :: ASC(10),DESC(10)
  WRITE(6,'("0*** HASHTEST - INTEGER ********")')
  WRITE(6,'("     HASH VALUE OF INT1  ",I20,    " = ",I11)')  INT1, HASHCODE( INT1)
  WRITE(6,'("     HASH VALUE OF INT2  ",I20,    " = ",I11)')  INT2, HASHCODE( INT2)
  WRITE(6,'("     HASH VALUE OF INT4  ",I20,    " = ",I11)')  INT4, HASHCODE( INT4)
  WRITE(6,'("     HASH VALUE OF INT8  ",I20,    " = ",I11)')  INT8, HASHCODE( INT8)
  WRITE(6,'("0*** HASHTEST - REAL    ********")')
  WRITE(6,'("     HASH VALUE OF REAL4 ",G20.10, " = ",I11)') REAL4, HASHCODE(REAL4)
  WRITE(6,'("     HASH VALUE OF REAL8 ",G20.10, " = ",I11)') REAL8, HASHCODE(REAL8)
  WRITE(6,'("0*** HASHTEST - STRING  ********")')
  WRITE(6,'("     HASH VALUE OF STR12 ",12X,A12," = ",I11)') STR12, HASHCODE(STR12)
  WRITE(6,'("     HASH VALUE OF STR16 ", 8X,A16," = ",I11)') STR16, HASHCODE(STR16)
  WRITE(6,'("     HASH VALUE OF STR20 ", 4X A20," = ",I11)') STR20, HASHCODE(STR20)
  WRITE(6,'("     HASH VALUE OF STR24 ",    A24," = ",I11)') STR24, HASHCODE(STR24)

  CALL HASHLIST_RESIZE(HL,16)
  WRITE(6,'("CAPACITY = ",I4,", COUNT = ",I4)') SIZE(HL%HASH,2), HL%COUNT

                                              !            +------+       +------+------+
                                              !  IND       |VALUES|       |BEFORE|AFTER |              44
                                              !            +------+       +------+------+             /  \
  RES = ADDHASHCODE(HL,44)                    !   1        |  44  |       |  4   |   2  |           42    64
  RES = ADDHASHCODE(HL,64)                    !   2        |  64  |       |  7   |   3  |          /     /   \
  RES = ADDHASHCODE(HL,87)                    !   3        |  87  |       |  5   |   0  |        14    60    87
  RES = ADDHASHCODE(HL,42)                    !   4        |  42  |       |  6   |   0  |          \        /
  RES = ADDHASHCODE(HL,85)                    !   5        |  85  |       |  8   |   0  |           18    85
  RES = ADDHASHCODE(HL,14)                    !   6        |  14  |       |  0   |  10  |                /
  RES = ADDHASHCODE(HL,60)                    !   7        |  60  |       |  0   |   0  |              78
  RES = ADDHASHCODE(HL,78)                    !   8        |  78  |       |  9   |   0  |             /
  RES = ADDHASHCODE(HL,72)                    !   9        |  72  |       |  0   |   0  |           72
  RES = ADDHASHCODE(HL,18)                    !  10        |  18  |       |  0   |   0  |
                                              !            +------+       +------+------+

  !CALL HASHLIST_RESIZE(HL,10)
  DO I = 1, SIZE(HL%HASH,2)
    WRITE(6,'(3I8)') (HL%HASH(J,I),J=1,3)
  END DO


                                              !  +------+------+
                                              !  | ASC  | DESC |
                                              !  +------+------+
                                              !  |  14  |  87  |
                                              !  |  18  |  85  |
                                              !  |  42  |  78  |
  CALL HASHLIST_SORT(HL,ASC)                  !  |  44  |  72  |        
  CALL HASHLIST_SORT(HL,DESC,.FALSE.)         !  |  60  |  64  |
  DO I = 1, 10                                !  |  64  |  60  |
    WRITE(6,'(2I8)') ASC(I),DESC(I)           !  |  72  |  44  |
  END DO                                      !  |  78  |  42  |
                                              !  |  85  |  18  |
                                              !  |  87  |  14  |
                                              !  +------+------+









  
END PROGRAM HASHTEST

