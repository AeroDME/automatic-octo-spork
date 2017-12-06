! ---------------------------------------------------------------------------------------------------------------------------------+
!                                                                                                                        D. Everhart
!                                                                                                                        05 MAY 2017
! ---------------------------------------------------------------------------------------------------------------------------------+
! The MIT License (MIT)
! 
! Copyright (c) 2017 Daniel Everhart
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
                                                     MODULE MODBTREE
! ---------------------------------------------------------------------------------------------------------------------------------+
IMPLICIT NONE
PRIVATE
PUBLIC :: BTREE,BTREE_INIT,BTREE_SETBLOCKSIZE,BTREE_ADD,BTREE_SORT
TYPE TREENODE
  SEQUENCE
  INTEGER(KIND=4)             :: HASHCODE
  INTEGER(KIND=4)             :: LEFT
  INTEGER(KIND=4)             :: RIGHT
END TYPE TREENODE
TYPE BTREE                                                      ! BTREE data type
  SEQUENCE
  INTEGER(KIND=4)             :: CAPACITY                       ! Tree maximum capacity
  INTEGER(KIND=4)             :: COUNT                          ! Current count of items stored in hash.
  TYPE(TREENODE),ALLOCATABLE  :: TREE(:)                        ! Array of tree nodes
END TYPE BTREE
INTEGER(KIND=4),PARAMETER     :: DEFAULT_BLOCKSIZE = 64
INTEGER(KIND=4)               :: BLOCKSIZE = DEFAULT_BLOCKSIZE  ! Block size for resizing hash
! ---------------------------------------------------------------------------------------------------------------------------------+
                                                           CONTAINS
! ---------------------------------------------------------------------------------------------------------------------------------+
SUBROUTINE BTREE_SETBLOCKSIZE(BSIZE)
INTEGER(KIND=4),INTENT(IN) :: BSIZE
  IF (BSIZE.GT.0) BLOCKSIZE = BSIZE
END SUBROUTINE BTREE_SETBLOCKSIZE
! ---------------------------------------------------------------------------------------------------------------------------------+
SUBROUTINE BTREE_INIT(BT)
TYPE(BTREE),INTENT(INOUT) :: BT                          ! Hash to be initialized
  CALL BTREE_RESIZE(BT,BLOCKSIZE)
END SUBROUTINE BTREE_INIT
! ---------------------------------------------------------------------------------------------------------------------------------+
SUBROUTINE BTREE_RESIZE(BT, CAPACITY)
TYPE(BTREE),INTENT(INOUT)   :: BT                        ! Hash to be resized
INTEGER(KIND=4),INTENT(IN)  :: CAPACITY                  ! Capacity to which to resize BTREE
TYPE(TREENODE),ALLOCATABLE  :: TEMP(:)                   ! Temporary storage array for existing data
INTEGER(KIND=4)             :: OLDCAPACITY,I,IERR        ! Temp variables, error status, and counters
  IF (ALLOCATED(BT%TREE)) THEN
    OLDCAPACITY = SIZE(BT%TREE)
    ALLOCATE(TEMP(OLDCAPACITY),STAT=IERR)
    IF (IERR.NE.0) THEN
      WRITE(0,'(A)') '0*** BTREE_RESIZE: UNABLE TO ALLOCATE TEMPORARY MEMORY FOR BTREE.'
      STOP
    END IF
    TEMP = BT%TREE
    CALL BTREE_ALLOCATE(BT, CAPACITY)
    DO I = 1, MIN(OLDCAPACITY,CAPACITY)
        BT%TREE(I) = TEMP(I)
    END DO
    DEALLOCATE(TEMP,STAT=IERR)
    IF (IERR.NE.0) THEN
      WRITE(0,'(A)') '0*** BTREE_RESIZE: UNABLE TO DEALLOCATE TEMPORARY MEMORY FOR BTREE.'
      STOP
    END IF
  ELSE
    CALL BTREE_ALLOCATE(BT, CAPACITY)
  END IF
END SUBROUTINE BTREE_RESIZE
! ---------------------------------------------------------------------------------------------------------------------------------+
SUBROUTINE BTREE_ALLOCATE(BT, CAPACITY)
TYPE(BTREE),INTENT(INOUT)  :: BT
INTEGER(KIND=4),INTENT(IN) :: CAPACITY
INTEGER(KIND=4)            :: IERR
  IF (ALLOCATED(BT%TREE)) THEN
    DEALLOCATE(BT%TREE,STAT=IERR)
    IF (IERR.NE.0) THEN
      WRITE(0,'(A)') '0*** BTREE_ALLOCATE: UNABLE TO DEALLOCATE MEMORY FOR BTREE.'
      STOP
    END IF
  END IF
  ALLOCATE(BT%TREE(CAPACITY),STAT=IERR)
  IF (IERR.NE.0) THEN
    WRITE(0,'(A)') '0*** BTREE_ALLOCATE: UNABLE TO ALLOCATE MEMORY FOR BTREE.'
    STOP
  END IF
  BT%TREE = TREENODE(0,0,0)
  BT%CAPACITY = CAPACITY
END SUBROUTINE BTREE_ALLOCATE
! ---------------------------------------------------------------------------------------------------------------------------------+
INTEGER(KIND=4) FUNCTION BTREE_ADD(BT,HASHCODE)
TYPE(BTREE),INTENT(INOUT)  :: BT
INTEGER(KIND=4),INTENT(IN) :: HASHCODE

  IF (BT%COUNT.EQ.0) THEN                   ! Special case where this is the first value.
    BT%COUNT = 1
    BT%TREE(BT%COUNT)%HASHCODE = HASHCODE
    BT%TREE(BT%COUNT)%LEFT  = 0
    BT%TREE(BT%COUNT)%RIGHT = 0
    BTREE_ADD = BT%COUNT
    RETURN
  ENDIF

  IF (BT%COUNT.EQ.SIZE(BT%TREE)) CALL BTREE_RESIZE(BT,BT%CAPACITY+BLOCKSIZE)

  BTREE_ADD = 1                                      ! Start at top of the table.
  DO WHILE (BTREE_ADD.LE.BT%COUNT)
    IF (BTREE_ADD.EQ.0) RETURN

    IF (HASHCODE.LT.BT%TREE(BTREE_ADD)%HASHCODE) THEN
      IF (BT%TREE(BTREE_ADD)%LEFT.EQ.0) THEN
        BT%COUNT = BT%COUNT + 1
        BT%TREE(BT%COUNT)%HASHCODE = HASHCODE
        BT%TREE(BTREE_ADD)%LEFT = BT%COUNT
        BTREE_ADD = BT%COUNT
        RETURN
      ELSE
        BTREE_ADD = BT%TREE(BTREE_ADD)%LEFT
      END IF
    ELSE IF (HASHCODE.GT.BT%TREE(BTREE_ADD)%HASHCODE) THEN
      IF (BT%TREE(BTREE_ADD)%RIGHT.EQ.0) THEN
        BT%COUNT = BT%COUNT + 1
        BT%TREE(BT%COUNT)%HASHCODE = HASHCODE
        BT%TREE(BTREE_ADD)%RIGHT = BT%COUNT
        BTREE_ADD = BT%COUNT
        RETURN
      ELSE
        BTREE_ADD = BT%TREE(BTREE_ADD)%RIGHT
      END IF
    ELSE
      RETURN
    END IF
  END DO

END FUNCTION BTREE_ADD
! ---------------------------------------------------------------------------------------------------------------------------------+
SUBROUTINE BTREE_SORT(BT, ARRAY, ASCENDING)
TYPE(BTREE), INTENT(IN)                :: BT
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
  CALL BTREE_SORTED(BT,ARRAY,I,C,ASC)
END SUBROUTINE BTREE_SORT
! ---------------------------------------------------------------------------------------------------------------------------------+
RECURSIVE SUBROUTINE BTREE_SORTED(BT,ARRAY,I,C,ASCENDING)
TYPE(BTREE), INTENT(IN)       :: BT
INTEGER(KIND=4),INTENT(INOUT) :: ARRAY(:)
INTEGER(KIND=4),INTENT(IN)    :: I
INTEGER(KIND=4),INTENT(INOUT) :: C
LOGICAL(KIND=4),INTENT(IN)    :: ASCENDING
IF (ASCENDING) THEN
  IF (BT%TREE(I)%LEFT.NE.0) CALL BTREE_SORTED(BT,ARRAY,BT%TREE(I)%LEFT,C,ASCENDING)
  IF (C.GT.SIZE(ARRAY)) RETURN
  C = C + 1
  ARRAY(C) = BT%TREE(I)%HASHCODE
  IF (BT%TREE(I)%RIGHT.NE.0) CALL BTREE_SORTED(BT,ARRAY,BT%TREE(I)%RIGHT,C,ASCENDING)
ELSE
  IF (BT%TREE(I)%RIGHT.NE.0) CALL BTREE_SORTED(BT,ARRAY,BT%TREE(I)%RIGHT,C,ASCENDING)
  IF (C.GT.SIZE(ARRAY)) RETURN
  C = C + 1
  ARRAY(C) = BT%TREE(I)%HASHCODE
  IF (BT%TREE(I)%LEFT.NE.0) CALL BTREE_SORTED(BT,ARRAY,BT%TREE(I)%LEFT,C,ASCENDING)
END IF
END SUBROUTINE BTREE_SORTED
! ---------------------------------------------------------------------------------------------------------------------------------+
END MODULE MODBTREE
! ---------------------------------------------------------------------------------------------------------------------------------+
PROGRAM HASHTEST
USE MODHASHCODE
USE MODBTREE
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
TYPE(BTREE)               :: BT
INTEGER(KIND=4)           :: ASC(10),DESC(10)
  WRITE(6,'("0*** HASHTEST - INTEGER ********")')
  WRITE(6,'("       HASHCODE OF INT1  ",I20,    " = ",I11)')  INT1, HASHCODE( INT1)
  WRITE(6,'("       HASHCODE OF INT2  ",I20,    " = ",I11)')  INT2, HASHCODE( INT2)
  WRITE(6,'("       HASHCODE OF INT4  ",I20,    " = ",I11)')  INT4, HASHCODE( INT4)
  WRITE(6,'("       HASHCODE OF INT8  ",I20,    " = ",I11)')  INT8, HASHCODE( INT8)
  WRITE(6,'("0*** HASHTEST - REAL    ********")')
  WRITE(6,'("       HASHCODE OF REAL4 ",G20.10, " = ",I11)') REAL4, HASHCODE(REAL4)
  WRITE(6,'("       HASHCODE OF REAL8 ",G20.10, " = ",I11)') REAL8, HASHCODE(REAL8)
  WRITE(6,'("0*** HASHTEST - STRING  ********")')
  WRITE(6,'("       HASHCODE OF STR12 ",12X,A12," = ",I11)') STR12, HASHCODE(STR12)
  WRITE(6,'("       HASHCODE OF STR16 ", 8X,A16," = ",I11)') STR16, HASHCODE(STR16)
  WRITE(6,'("       HASHCODE OF STR20 ", 4X A20," = ",I11)') STR20, HASHCODE(STR20)
  WRITE(6,'("       HASHCODE OF STR24 ",    A24," = ",I11)') STR24, HASHCODE(STR24)

  !CALL BTREE_SETBLOCKSIZE(4)
  CALL BTREE_INIT(BT)
  WRITE(6,'("CAPACITY = ",I4,", COUNT = ",I4)') SIZE(BT%TREE), BT%COUNT

                                              !            +------+       +------+------+
                                              !  IND       |VALUES|       | LEFT | RIGHT|              44
                                              !            +------+       +------+------+             /  \
  RES = BTREE_ADD(BT,44)                      !   1        |  44  |       |  4   |   2  |           42    64
  RES = BTREE_ADD(BT,64)                      !   2        |  64  |       |  7   |   3  |          /     /   \
  RES = BTREE_ADD(BT,87)                      !   3        |  87  |       |  5   |   0  |        14    60    87
  RES = BTREE_ADD(BT,42)                      !   4        |  42  |       |  6   |   0  |          \        /
  RES = BTREE_ADD(BT,85)                      !   5        |  85  |       |  8   |   0  |           18    85
  RES = BTREE_ADD(BT,14)                      !   6        |  14  |       |  0   |  10  |                /
  RES = BTREE_ADD(BT,60)                      !   7        |  60  |       |  0   |   0  |              78
  RES = BTREE_ADD(BT,78)                      !   8        |  78  |       |  9   |   0  |             /
  RES = BTREE_ADD(BT,72)                      !   9        |  72  |       |  0   |   0  |           72
  RES = BTREE_ADD(BT,18)                      !  10        |  18  |       |  0   |   0  |
                                              !            +------+       +------+------+


  WRITE(6,'("CAPACITY = ",I4,", COUNT = ",I4)') SIZE(BT%TREE), BT%COUNT
  DO I = 1, BT%COUNT
    WRITE(6,'(3I8)') BT%TREE(I)
  END DO

                                              !  +------+------+
                                              !  | ASC  | DESC |
                                              !  +------+------+
                                              !  |  14  |  87  |
                                              !  |  18  |  85  |
                                              !  |  42  |  78  |
  CALL BTREE_SORT(BT,ASC)                     !  |  44  |  72  |        
  CALL BTREE_SORT(BT,DESC,.FALSE.)            !  |  60  |  64  |
  DO I = 1, 10                                !  |  64  |  60  |
    WRITE(6,'(2I8)') ASC(I),DESC(I)           !  |  72  |  44  |
  END DO                                      !  |  78  |  42  |
                                              !  |  85  |  18  |
                                              !  |  87  |  14  |
                                              !  +------+------+








  
END PROGRAM HASHTEST

