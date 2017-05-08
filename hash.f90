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
MODULE MODHASH
IMPLICIT NONE
PRIVATE
PUBLIC :: CALCULATE_HASH
! ---------------------------------------------------------------------------------------------------------------------------------+
INTERFACE CALCULATE_HASH
  MODULE PROCEDURE CALCULATE_HASH_INT1
  MODULE PROCEDURE CALCULATE_HASH_INT2
  MODULE PROCEDURE CALCULATE_HASH_INT4
  MODULE PROCEDURE CALCULATE_HASH_INT8
END INTERFACE CALCULATE_HASH
! ---------------------------------------------------------------------------------------------------------------------------------+
                                                           CONTAINS
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
!
END MODULE MODHASH
! ---------------------------------------------------------------------------------------------------------------------------------+
PROGRAM HASHTEST
USE MODHASH
IMPLICIT NONE
INTEGER(KIND=1),PARAMETER :: IDIV = 31
INTEGER(KIND=1) :: INT1 = HUGE(INT1) / IDIV
INTEGER(KIND=2) :: INT2 = HUGE(INT2) / IDIV
INTEGER(KIND=4) :: INT4 = HUGE(INT4) / IDIV
INTEGER(KIND=8) :: INT8 = HUGE(INT8) / IDIV
  WRITE(6,'("0*** ",A)') 'HASHTEST'
  WRITE(6,'("     HASH VALUE OF INT1 ",I20," = ",I20)') INT1, CALCULATE_HASH(INT1)
  WRITE(6,'("     HASH VALUE OF INT2 ",I20," = ",I20)') INT2, CALCULATE_HASH(INT2)
  WRITE(6,'("     HASH VALUE OF INT4 ",I20," = ",I20)') INT4, CALCULATE_HASH(INT4)
  WRITE(6,'("     HASH VALUE OF INT8 ",I20," = ",I20)') INT8, CALCULATE_HASH(INT8)
END PROGRAM HASHTEST

