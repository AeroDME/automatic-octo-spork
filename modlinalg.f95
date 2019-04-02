!----------------------------------------------------------------------------------------------------------------------------------+
!                                                        M O D L I N A L G                                                         |
!                                                                                                                      D. Everhart |
!                                                                                                                      02 APR 2019 |
!----------------------------------------------------------------------------------------------------------------------------------+
! The MIT License (MIT)
! 
! Copyright (c) 2019 Daniel Everhart
! 
! Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files
! (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify,
! merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
! 
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
! OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
! LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
! IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
!----------------------------------------------------------------------------------------------------------------------------------+
! These subroutines have evolved significantly over the years.  They began as a undergraduate classroom assignment, and evolved to
! what they are now.  When writing code for numerical analysis, I inevitability have the need to do some matrix operations and most
! of the time, this includes matrix inversion.  While this began as FORTRAN77, it has been migrated to the F90 standard. This 
! should allow for the more modern construction and operator overrides for matrix math.
!----------------------------------------------------------------------------------------------------------------------------------+
! AE 227 FALL 1994 - Wichita State University
!   MGAUSJ - Gauss-Jordan elimination
! AE 527 FALL 1996 - Wichita State University
!   MGAUSB - Gauss elimination with back substitution (more efficient)
! 2004 - PRESENT - Abstraction of procedures and cleanup.
! MAR 2016 - Cleanup and preparation of messages for use by others.
! APR 2019 - Conversion to F90 (F95 standard uses SIZE intrinsic).
!----------------------------------------------------------------------------------------------------------------------------------+
MODULE MODLINALG
IMPLICIT NONE
PRIVATE
!----------------------------------------------------------------------------------------------------------------------------------+
PUBLIC :: MATRIX_PRINT,TEST_MODLINALG,MATRIX_RANDOMIZE
!----------------------------------------------------------------------------------------------------------------------------------+
                                                            CONTAINS
!----------------------------------------------------------------------------------------------------------------------------------+
SUBROUTINE MATRIX_PRINT(MATRIX)
  IMPLICIT NONE
  REAL(KIND=8),INTENT(IN),DIMENSION(:,:) :: MATRIX
  INTEGER(KIND=4)                        :: I, J, COLS
  CHARACTER(LEN=9)                       :: F
  COLS = SIZE(MATRIX,2)
  WRITE(F,'(A1I2A6)') '(', COLS, 'F10.4)'
  DO I = 1, SIZE(MATRIX,1)
    WRITE(6,F) (MATRIX(I,J), J=1, COLS)
  END DO
END SUBROUTINE MATRIX_PRINT
!----------------------------------------------------------------------------------------------------------------------------------+
SUBROUTINE MATRIX_RANDOMIZE(MATRIX,SEED)
  IMPLICIT NONE
  REAL(KIND=8),INTENT(INOUT),DIMENSION(:,:) :: MATRIX ! Matrix to be initialized.
  INTEGER(KIND=4),INTENT(IN),OPTIONAL       :: SEED   ! Optional seed for random number generator.
  INTEGER(KIND=4)                           :: I,J    ! Indicies for random number.
  IF (PRESENT(SEED)) CALL SRAND(SEED)
  DO I = 1,SIZE(MATRIX,1)
    DO J = 1,SIZE(MATRIX,2)
      MATRIX(I,J) = DBLE(RAND())
    END DO
  END DO
END SUBROUTINE MATRIX_RANDOMIZE
!----------------------------------------------------------------------------------------------------------------------------------+
SUBROUTINE MATRIX_IDENTITY(MATRIX)
  IMPLICIT NONE
  REAL(KIND=8),INTENT(INOUT),DIMENSION(:,:) :: MATRIX ! Matrix to be initialized.
  INTEGER(KIND=4)                           :: I      ! Index for random number.
  MATRIX = 0D0
  DO I = 1,MIN(SIZE(MATRIX,1),SIZE(MATRIX,2))
    MATRIX(I,I) = 1D0
  END DO
END SUBROUTINE MATRIX_IDENTITY
!----------------------------------------------------------------------------------------------------------------------------------+
SUBROUTINE MATRIX_MULTIPLY(A,B,P)
!----------------------------------------------------------------------------------------------------------------------------------+
! Product of [A]*[B] is placed in P.
!----------------------------------------------------------------------------------------------------------------------------------+
  IMPLICIT NONE
  REAL(KIND=8),INTENT(IN),DIMENSION(:,:)    :: A      ! Matrix A
  REAL(KIND=8),INTENT(IN),DIMENSION(:,:)    :: B      ! Matrix B
  REAL(KIND=8),INTENT(INOUT),DIMENSION(:,:) :: P      ! Matrix P (product: [P] = [A] * [B])
  INTEGER(KIND=4)                           :: AROWS  ! Rows of A
  INTEGER(KIND=4)                           :: ACOLS  ! Cols of A (Must match BROWS)
  INTEGER(KIND=4)                           :: BROWS  ! Rows of B (Must match ACOLS)
  INTEGER(KIND=4)                           :: BCOLS  ! Cols of B
  INTEGER(KIND=4)                           :: PROWS  ! Rows of P (Must be .GE. AROWS)
  INTEGER(KIND=4)                           :: PCOLS  ! Cols of P (Must be .GE. BCOLS)       
  INTEGER(KIND=4)                           :: I,J,K  ! Counter indicies
  REAL(KIND=8)                              :: SM     ! Accumulator for matrix multiplicaiton.
!----------------------------------------------------------------------------------------------------------------------------------+
!
!                                   [A]     *         [B]         =         [P]
!                           
!                               | A11 A12 |   | B11 B12 B13 B14 |   | P11 P12 P13 P14 |
!                               | A21 A22 | * | B21 B22 B23 B12 | = | P21 P22 P23 P24 |
!                               | A31 A32 |                         | P31 P32 P33 P34 |
!                           
!                           
!                                               <-------- K -------->
!                           
!                                         P11 = A11 * B11 + A12 * B21   ^   ^
!                                         P21 = A21 * B11 + A22 * B21   I   |
!                                         P31 = A31 * B11 + A32 * B21   v   |
!                                                                           |
!                                         P12 = A11 * B12 + A12 * B22   ^   |
!                                         P22 = A21 * B12 + A22 * B22   I   |
!                                         P32 = A31 * B12 + A32 * B22   v    
!                                                                           J
!                                         P13 = A11 * B13 + A12 * B23   ^    
!                                         P23 = A21 * B13 + A22 * B23   I   |
!                                         P33 = A31 * B13 + A32 * B23   v   |
!                                                                           |
!                                         P14 = A11 * B14 + A12 * B24   ^   |
!                                         P24 = A21 * B14 + A22 * B24   I   |
!                                         P34 = A31 * B14 + A32 * B24   v   v
!
!----------------------------------------------------------------------------------------------------------------------------------+
  AROWS = SIZE(A,1)
  ACOLS = SIZE(A,2)
  BROWS = SIZE(B,1)
  BCOLS = SIZE(B,2)
  PROWS = SIZE(B,1)
  PCOLS = SIZE(B,2)
!----------------------------------------------------------------------------------------------------------------------------------+
  IF (ACOLS.NE.BROWS) THEN
    WRITE(0,910) ACOLS, BROWS
    STOP
  END IF
  IF (PROWS.LT.AROWS) THEN
    WRITE(0,920) PROWS, AROWS
    STOP
  END IF
  IF (PCOLS.LT.BCOLS) THEN
    WRITE(0,930) PCOLS, BCOLS
    STOP
  END IF
!----------------------------------------------------------------------------------------------------------------------------------+
  DO I = 1, PROWS
    DO J = 1, PCOLS
      SM = 0D0
      DO K = 1, ACOLS
        SM = SM + A(I,K) * B(K,J)
      END DO
      P(I,J) = SM
    END DO
  END DO
!----------------------------------------------------------------------------------------------------------------------------------+
 910 FORMAT('*** FATAL ERRROR: MATRIX_MULTIPLY: MATRIX A COLUMNS MUST MATCH MATRIX B ROWS:',/,18X,I8, ' .NE. ', I8)
 920 FORMAT('*** FATAL ERRROR: MATRIX_MULTIPLY: MATRIX P ROWS MUST AT LEAST EQUAL MATRIX A ROWS:',/,18X,I8, ' .LT. ', I8)
 930 FORMAT('*** FATAL ERRROR: MATRIX_MULTIPLY: MATRIX P COLUMNS MUST AT LEAST EQUAL MATRIX B COLUMNS:',/,18X,I8, ' .LT. ', I8)
END SUBROUTINE MATRIX_MULTIPLY
!----------------------------------------------------------------------------------------------------------------------------------+
SUBROUTINE PARTIAL_PIVOT(MATRIX,ROW)
!----------------------------------------------------------------------------------------------------------------------------------+
  IMPLICIT NONE
  REAL(KIND=8),INTENT(INOUT),DIMENSION(:,:) :: MATRIX
  INTEGER(KIND=4),INTENT(IN)                :: ROW
  INTEGER(KIND=4)                           :: J, K
  REAL(KIND=8),PARAMETER                    :: TOL = 1D-12
  REAL(KIND=8)                              :: TMP
!----------------------------------------------------------------------------------------------------------------------------------+
  IF (ABS(MATRIX(ROW,ROW)).LE.TOL) THEN
     DO J = ROW+1, SIZE(MATRIX,1)
       IF (ABS(MATRIX(J,ROW)).GE.TOL) EXIT
     END DO
     DO K = ROW, SIZE(MATRIX,2)
       TMP = MATRIX(ROW,K)
       MATRIX(ROW,K) = MATRIX(J,K)
       MATRIX(J,K) = TMP
     END DO
  END IF
END SUBROUTINE PARTIAL_PIVOT
!----------------------------------------------------------------------------------------------------------------------------------+
SUBROUTINE MODLINALG_GAUSS_ELIMINATE_ROW(MATRIX,ROW,COL,SCOL)
!----------------------------------------------------------------------------------------------------------------------------------+
! Performs Gauss elimination on a particular row.
!----------------------------------------------------------------------------------------------------------------------------------+
  IMPLICIT NONE
  REAL(KIND=8),INTENT(INOUT),DIMENSION(:,:) :: MATRIX      ! Matrix on which this operation is performed.
  INTEGER(KIND=4),INTENT(IN)                :: ROW         ! Matrix row on which to perform elimination
  INTEGER(KIND=4),INTENT(IN)                :: COL         ! Associated column to be zeroed for elimination
  INTEGER(KIND=4),INTENT(IN)                :: SCOL        ! Column to begin elimination
  REAL(KIND=8),PARAMETER                    :: TOL = 1D-12 ! Values who's magnitude are less that TOL are set to 0D0
  REAL(KIND=8)                              :: RATIO       ! Ratio used to scale row to be subtracted from rows being eliminated.
  INTEGER(KIND=4)                           :: I,J         ! Counter indicies.
!----------------------------------------------------------------------------------------------------------------------------------+
! Performs Gauss elimination on a particular row.  This procedure is called multiple times to eliminate a particular column.
! Consider the following matrix:
!   
!                                               | A11 A12 A13 A14 A15 A16 A17 |
!                                               | 0.0 A22 A23 A24 A25 A26 A27 |
!                                               | 0.0 A32 A33 A34 A35 A36 A37 |
!                                               | 0.0 A42 A43 A44 A45 A46 A47 |
!                                               | 0.0 A52 A53 A54 A55 A56 A57 |
!
! Row 3, column 2 is the next to be zeroed.  This is done by  subtracting a scaled row 2 from row 3.  Scale factor RATIO is
! calculated by:
!   
!                                                      RATIO = A32 / A22
!
! This ratio is chosen so that the result at row 3, column 2 will calculate to zero.
!             
!                                                A32 = A32 - A22 * A32 / A22  (Results in 0.0)
!                                                A33 = A33 - A23 * A32 / A22
!                                                A34 = A34 - A24 * A32 / A22
!                                                A35 = A35 - A25 * A32 / A22
!                                                A36 = A36 - A26 * A32 / A22
!                                                A37 = A37 - A27 * A32 / A22
!
! The resulting state of the matrix after this procedure is:
!
!                                               | A11 A12 A13 A14 A15 A16 A17 |
!                                               | 0.0 A22 A23 A24 A25 A26 A27 |
!                                               | 0.0 0.0 A33 A34 A35 A36 A37 |
!                                               | 0.0 A42 A43 A44 A45 A46 A47 |
!                                               | 0.0 A52 A53 A54 A55 A56 A57 |
!
! Note:  This procedure does not require that ROW .GT. COL.  The result is that in the above example, the same operation could be
! performed on row 1 to zero out A12.  This is useful if the elimination is Gauss-Jordan where all elements not on the diagonal
! are zeroed out.
!
! Additionally, this procedure is called with a starting column. To save operations, the start column may be set to COL+1 when
! calling and nothing will be done to the elements to the left of COL.  This is useful when reducing the matrix to a upper triangle
! matrix.
!
!----------------------------------------------------------------------------------------------------------------------------------+
  IF (ABS(MATRIX(ROW,COL)).GE.TOL) THEN
    RATIO = MATRIX(ROW,COL) / MATRIX(COL,COL)
    DO I = SCOL, SIZE(MATRIX,2)
      IF (I.EQ.COL) THEN
        MATRIX(ROW,I) = 0D0 ! By definition this location must be zero.  Set the value to zero instead of doing calculation.
      ELSE
        MATRIX(ROW,I) = MATRIX(ROW,I) - RATIO * MATRIX(COL,I)
      ENDIF
    END DO
  ELSE
    MATRIX(ROW,COL) = 0D0
  END IF
END SUBROUTINE MODLINALG_GAUSS_ELIMINATE_ROW
!----------------------------------------------------------------------------------------------------------------------------------+
SUBROUTINE BACK_SUBSTITUTION(MATRIX)
!----------------------------------------------------------------------------------------------------------------------------------+
! Performs on a matrix representing a system of equations.
!----------------------------------------------------------------------------------------------------------------------------------+
  IMPLICIT NONE
  REAL(KIND=8),INTENT(INOUT),DIMENSION(:,:) :: MATRIX              ! Matrix system on which backwards substitution is performed.
  INTEGER(KIND=4)                           :: I,J,K,ROWS,COLS     ! Counters and indicies
  REAL(KIND=8)                              :: SM                  ! Accumulator variable.
!----------------------------------------------------------------------------------------------------------------------------------+
! Consider the reduced system.
!
!                                              | A11 A12 A13 A14 A15 B16 B17 |
!                                              | JNK A22 A23 A24 A25 B26 B27 |
!                                              | JNK JNK A33 A34 A35 B36 B37 |
!                                              | JNK JNK JNK A44 A45 B46 B47 |
!                                              | JNK JNK JNK JNK A55 B56 B57 |
!  
! JNK values are assumed to be 0.0.  Recall that this is a set of equations for each B column vector:
!
!                                    A11*X16 + A12*X26 + A13*X36 + A14*X46 + A15*X56 = B16
!                                              A22*X26 + A23*X36 + A24*X46 + A25*X56 = B26
!                                                        A33*X36 + A34*X46 + A35*X56 = B36
!                                                                  A44*X46 + A45*X56 = B46
!                                                                            A55*X56 = B56
!                              
!                                    A11*X17 + A12*X27 + A13*X37 + A14*X47 + A15*X57 = B17
!                                              A22*X27 + A23*X37 + A24*X47 + A25*X57 = B27
!                                                        A33*X37 + A34*X47 + A35*X57 = B37
!                                                                  A44*X47 + A45*X57 = B47
!                                                                            A55*X57 = B57
! 
! These equations may be solved starting from the bottom:
! 
!                                                        <------------- K --------------->
!                               
!                                |  |     X56 = (B56                                        ) / A55
!                                |  |     X46 = (B46 - A45*X56                              ) / A44
!                                |  J     X36 = (B36 - A34*X46 - A35*X56                    ) / A33
!                                |  |     X26 = (B26 - A23*X36 - A24*X46 - A25*X56          ) / A22
!                                |  v     X16 = (B16 - A12*X26 - A13*X36 - A14*X46 - A15*X56) / A11
!                                I
!                                |  |     X57 = (B57                                        ) / A55
!                                |  |     X47 = (B47 - A45*X57                              ) / A44
!                                |  J     X37 = (B37 - A34*X47 - A35*X57                    ) / A33
!                                |  |     X27 = (B27 - A23*X37 - A24*X47 - A25*X57          ) / A22
!                                v  v     X17 = (B17 - A12*X27 - A13*X37 - A14*X47 - A15*X57) / A11
!
! The resulting aray has the solution vectors in place of the B vectors.
!
!                                              | A11 A12 A13 A14 A15 X16 X17 |
!                                              | JNK A22 A23 A24 A25 X26 X27 |
!                                              | JNK JNK A33 A34 A35 X36 X37 |
!                                              | JNK JNK JNK A44 A45 X46 X47 |
!                                              | JNK JNK JNK JNK A55 X56 X57 |
!
!----------------------------------------------------------------------------------------------------------------------------------+
  ROWS = SIZE(MATRIX,1)
  COLS = SIZE(MATRIX,2)
!----------------------------------------------------------------------------------------------------------------------------------+
  DO I = ROWS+1, COLS
    DO J = ROWS, 1, -1
      SM = 0D0
      DO K = ROWS, J+1, -1
        SM = SM + MATRIX(J,K) * MATRIX(K,I)
      END DO
      MATRIX(J,I) = (MATRIX(J,I) - SM) / MATRIX(J,J)
    END DO
  END DO
!----------------------------------------------------------------------------------------------------------------------------------+
END SUBROUTINE BACK_SUBSTITUTION
!----------------------------------------------------------------------------------------------------------------------------------+
SUBROUTINE GAUSS_ELIMINATION(MATRIX,JORDAN)
!----------------------------------------------------------------------------------------------------------------------------------+
! Performs Gauss elimination on the matrix.
!----------------------------------------------------------------------------------------------------------------------------------+
  IMPLICIT NONE
  REAL(KIND=8),INTENT(INOUT),DIMENSION(:,:) :: MATRIX        ! Matrix on which Gauss elimination is performed
  LOGICAL,INTENT(IN)                        :: JORDAN        ! .TRUE. to request Gauss-Jordan elimination
  INTEGER(KIND=4)                           :: I,J,ROWS,COLS ! Index and counter variables.
  REAL(KIND=8)                              :: TMP           ! Temporary storage value
!----------------------------------------------------------------------------------------------------------------------------------+
! Consider the matrix below representing the system of equations:
!
!                                                          [A]*[X] = B
!                                
!                                                | A11 A12 A13 A14 A15 B16 B17 |
!                                                | A21 A22 A23 A24 A25 B26 B27 |
!                                                | A31 A32 A33 A34 A35 B36 B37 |
!                                                | A41 A42 A43 A44 A45 B46 B47 |
!                                                | A51 A52 A53 A54 A55 B56 B57 |
!
!
! With the JORDAN flag set to .TRUE., this procedure performs Gauss-Jordan elimination with repeated calls to
! MODLINALG_ELIMINATE_COLUMN to zero out non-diagonal elements, and normalize to 1.0 resulting in the following:
!
!
!                                                | 1.0 0.0 0.0 0.0 0.0 X16 X17 |
!                                                | 0.0 1.0 0.0 0.0 0.0 X26 X27 |
!                                                | 0.0 0.0 1.0 0.0 0.0 X36 X37 |
!                                                | 0.0 0.0 0.0 1.0 0.0 X46 X47 |
!                                                | 0.0 0.0 0.0 0.0 1.0 X56 X57 |
!
! X values show where the solution matrix is stored.
!
! Without the JORDAN flag set, this procedure performs Gauss elimination with repeated calls to GAUSS_ELIMINATE_COLUMN
! leaveing an upper triangle matrix.  Junk values are left in place below the diagonal to save operations.  This results in the
! following:
!
!                                               | A11 A12 A13 A14 A15 B16 B17 |
!                                               | JNK A22 A23 A24 A25 B26 B27 |
!                                               | JNK JNK A33 A34 A35 B36 B37 |
!                                               | JNK JNK JNK A44 A45 B46 B47 |
!                                               | JNK JNK JNK JNK A55 B56 B57 |
!
!----------------------------------------------------------------------------------------------------------------------------------+
  ROWS = SIZE(MATRIX,1)
  COLS = SIZE(MATRIX,2)
  IF (ROWS.GT.COLS) THEN
    WRITE(0,910), ROWS, COLS
    STOP
  END IF
!----------------------------------------------------------------------------------------------------------------------------------+
  DO I = 1, ROWS
    CALL PARTIAL_PIVOT(MATRIX,I)
    IF (JORDAN) THEN
      CALL GAUSS_ELIMINATE_COLUMN(MATRIX,I,1)
      TMP = MATRIX(I,I)
      DO J = I, COLS                          ! This loop results in a 1.0 on the diagonal.
         MATRIX(I,J) = MATRIX(I,J) / TMP
      END DO
    ELSE
      CALL GAUSS_ELIMINATE_COLUMN(MATRIX,I,I+1)
    END IF
  END DO
910 FORMAT('*** FATAL ERRROR: GAUSS_ELIMINATION: MATRIX ROWS LARGER THAN COLUMNS',/,18X,I8, '   .GT.   ',I8)
!----------------------------------------------------------------------------------------------------------------------------------+
END SUBROUTINE GAUSS_ELIMINATION
!----------------------------------------------------------------------------------------------------------------------------------+
SUBROUTINE GAUSS_ELIMINATE_COLUMN(MATRIX,COL,SCOL)
!----------------------------------------------------------------------------------------------------------------------------------+
! Performs gauss elimination on a particular column.
!----------------------------------------------------------------------------------------------------------------------------------+
  IMPLICIT NONE
  REAL(KIND=8),INTENT(INOUT),DIMENSION(:,:) :: MATRIX  ! Matrix on which the column is zeroed.
  INTEGER(KIND=4),INTENT(IN)                :: COL     ! Associated column to be zeroed for elimination.
  INTEGER(KIND=4),INTENT(IN)                :: SCOL    ! Column to begin elimination
  INTEGER(KIND=4)                           :: J
!----------------------------------------------------------------------------------------------------------------------------------+
! Performs Gauss elimination on a particular column.  This procedure is called multiple times to columns for Gauss-Jordan
! elimination.  Consider the following matrix:
!   
!                                              | A11 A12 A13 A14 A15 A16 A17 |
!                                              | JNK A22 A23 A24 A25 A26 A27 |
!                                              | JNK A32 A33 A34 A35 A36 A37 |
!                                              | JNK A42 A43 A44 A45 A46 A47 |
!                                              | JNK A52 A53 A54 A55 A56 A57 |
!
!
! Column 2 is the next to be 'zeroed'.  This is done by repeated calls  to MODLINALG_GAUSS_ELIMINATE_ROW with SCOL=COL+1.  The
! result is the following:
!
!                                              | A11 A12 A13 A14 A15 A16 A17 |
!                                              | JNK A22 A23 A24 A25 A26 A27 |
!                                              | JNK JNK A33 A34 A35 A36 A37 |
!                                              | JNK JNK A43 A44 A45 A46 A47 |
!                                              | JNK JNK A53 A54 A55 A56 A57 |
!
!   Note:  This procedure calls MODLINALG_GAUSS_ELIMINATE_ROW for rows below the diagonal at the particular column in the matrix. 
!
!   Repeated calls, result in a matrix with junk (JNK) values below the diagonal and actuals values on and above the diagonal.  The
!   junk values are left in place to save operations for large systems.
!
!----------------------------------------------------------------------------------------------------------------------------------+
  DO J = SCOL, SIZE(MATRIX,1)
    IF (J.NE.COL) CALL MODLINALG_GAUSS_ELIMINATE_ROW(MATRIX,J,COL,SCOL)
  END DO
END SUBROUTINE GAUSS_ELIMINATE_COLUMN
!----------------------------------------------------------------------------------------------------------------------------------+
SUBROUTINE TEST_MODLINALG
  INTEGER(KIND=4),PARAMETER :: ROWS = 3
  INTEGER(KIND=4),PARAMETER :: COLS = 6
  REAL(KIND=8),DIMENSION(ROWS,COLS) :: D  = 0D0
  REAL(KIND=8),DIMENSION(ROWS,ROWS) :: D1
  REAL(KIND=8),DIMENSION(ROWS,ROWS) :: D2
  REAL(KIND=8),DIMENSION(ROWS,ROWS) :: E  = 0D0
  EQUIVALENCE (D1(1,1), D(1,1)), (D2(1,1), D(1,ROWS+1))
!----------------------------------------------------------------------------------------------------------------------------------+
  WRITE(6,910)
  WRITE(6,920)
  WRITE(6,910)
  WRITE(6,900)
  WRITE(6,980)'MATRIX D IS COMPRISED OF D1 AND D2.'
  WRITE(6,980)'       [ D ] =  [ D1 | D2 ]'
  WRITE(6,980)'MATRIX E IS SQUARE DIMENSIONED THE SAME AS'
  WRITE(6,980)'            D1 AND D2'
  WRITE(6,900)
  WRITE(6,910)
!----------------------------------------------------------------------------------------------------------------------------------+
  WRITE(6,900)
  WRITE(6,980)'TESTING MATRIX_IDENTITY ON [D].'
  WRITE(6,900)
  CALL MATRIX_IDENTITY(D)
  CALL MATRIX_PRINT(D)
!----------------------------------------------------------------------------------------------------------------------------------+
  WRITE(6,900)
  WRITE(6,980)'TESTING MATRIX_IDENTITY ON [D2].'
  WRITE(6,900)
  CALL MATRIX_IDENTITY(D2)
  CALL MATRIX_PRINT(D)
!----------------------------------------------------------------------------------------------------------------------------------+
  WRITE(6,900)
  WRITE(6,980)'TESTING MATRIX_RANDOMIZE ON [D1].'
  WRITE(6,900)
  CALL MATRIX_RANDOMIZE(D1,0)
  CALL MATRIX_PRINT(D)
!----------------------------------------------------------------------------------------------------------------------------------+
  WRITE(6,900)
  WRITE(6,980)'ASSIGNING [D1] = [E] FOR LATER USE.'
  WRITE(6,900)
  E = D1
  CALL MATRIX_PRINT(E)
!----------------------------------------------------------------------------------------------------------------------------------+
  WRITE(6,900)
  WRITE(6,980)'RUNNING NEW GAUSS ELIMINATION ON [D]'
  WRITE(6,900)
  CALL MATRIX_IDENTITY(D2)
  CALL MATRIX_RANDOMIZE(D1,0)
  CALL GAUSS_ELIMINATION(D,.FALSE.)
  CALL BACK_SUBSTITUTION(D)
  CALL MATRIX_PRINT(D)
!----------------------------------------------------------------------------------------------------------------------------------+
  WRITE(6,900)
  WRITE(6,980)'RUNNING NEW GAUSS ELIMINATION WITH JORDAN=.TRUE. ON [D]'
  WRITE(6,900)
  CALL MATRIX_IDENTITY(D2)
  CALL MATRIX_RANDOMIZE(D1,0)
  CALL GAUSS_ELIMINATION(D,.TRUE.)
  CALL MATRIX_PRINT(D)
!----------------------------------------------------------------------------------------------------------------------------------+
  WRITE(6,900)
  WRITE(6,980)'RUNNING GAUSS ELIMINATION AND BACK SUBSTITUTION ON [D]'
  WRITE(6,900)
  CALL MATRIX_RANDOMIZE(D1,0)
  CALL MATRIX_IDENTITY(D2)
  CALL GAUSS_ELIMINATION(D,.FALSE.)
  CALL BACK_SUBSTITUTION(D)
  CALL MATRIX_PRINT(D)
!----------------------------------------------------------------------------------------------------------------------------------+
  WRITE(6,900)
  WRITE(6,980)'MULTIPLYING MATRIX BY INVERSE [D1] = [D2] * [E]'
  WRITE(6,900)
  CALL MATRIX_MULTIPLY(D2,E,D1)
  CALL MATRIX_PRINT(D1)
!----------------------------------------------------------------------------------------------------------------------------------+
900 FORMAT()
910 FORMAT('********************************************************************************')
920 FORMAT('******************************** LINALG UNIT TESTS *****************************')
980 FORMAT(15X,A)
END SUBROUTINE TEST_MODLINALG
!----------------------------------------------------------------------------------------------------------------------------------+
END MODULE MODLINALG
!----------------------------------------------------------------------------------------------------------------------------------+
PROGRAM LINAGLTEST
USE MODLINALG
  IMPLICIT NONE
  CALL TEST_MODLINALG
!  REAL(KIND=8),DIMENSION(3,3) :: A, B
!  REAL(KIND=8),DIMENSION(3,3) :: C
!
!  CALL MATRIX_RANDOMIZE(A)
!  CALL MATRIX_RANDOMIZE(B)
!
!  CALL MATRIX_PRINT(A)
!  WRITE(6,*)
!  CALL MATRIX_PRINT(B)
!  WRITE(6,*)
!  C = A + B
!  CALL MATRIX_PRINT(C)
!  WRITE(6,*)
!  C = A * B
!  CALL MATRIX_PRINT(C)
END PROGRAM LINAGLTEST
