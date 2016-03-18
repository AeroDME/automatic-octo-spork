************************************************************************
* MATRIX PROCEDURES
* D. Everhart
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
************************************************************************
* These subroutines have evolved significantly over the years.
* They began as a undergraduate classroom assignment, and evolved to
* what they are now. 
*
* When writing code for numerical analysis, I inevitability have
* the need to do some matrix operations and most of the time, this
* includes matrix inversion.
*
* The PROGRAM and two (2) following SUBROUTINEs demonstrate how to use
* this module.  They may be commented out in order to compile this as a
* library.
************************************************************************
* AE 227 FALL 1994 - Wichita State University
*   MGAUSJ - Gauss-Jordan elimination
* AE 527 FALL 1996 - Wichita State University
*   MGAUSB - Gauss elimination with back substitution (more efficient)
* 2004 - PRESENT - Abstraction of procedures and cleanup.
* MAR 2016 - Cleanup and preparation of messages for use by others.
************************************************************************
      PROGRAM MTEST
      IMPLICIT NONE
      CALL SMTEST
      WRITE(*,*)
      CALL BNMK
  999 STOP
      END PROGRAM MTEST
************************************************************************
      SUBROUTINE BNMK
      IMPLICIT NONE
      INTEGER ROWS,COLS,SZ,STIME,ETIME,TM(9)
      PARAMETER (ROWS=600, COLS=2*ROWS, SZ=ROWS*COLS)
      DOUBLE PRECISION D(ROWS,COLS)
      DOUBLE PRECISION D1(ROWS,ROWS), D2(ROWS,ROWS)
      EQUIVALENCE (D1(1,1), D(1,1)), (D2(1,1), D(1,ROWS+1))
      DATA D /SZ*0D0/
*
      STIME = TIME()
      WRITE(*,810) 'START MGAUSJ'
      CALL MRND1(D1,ROWS*ROWS,0)
      CALL MIDEN(D2,ROWS,ROWS)
      CALL MGAUSJ(D,ROWS,COLS)
      ETIME = TIME()
      CALL GMTIME(ETIME-STIME, TM)
      WRITE(*,820) 'END MGAUSJ  ', TM(3),':',TM(2),':',TM(1)
*  
      WRITE(*,*)
*  
      STIME = TIME()
      WRITE(*,810) 'START MGAUSB'
      CALL MRND1(D1,ROWS*ROWS,0)
      CALL MIDEN(D2,ROWS,ROWS)
      CALL MGAUSB(D,ROWS,COLS)
      ETIME = TIME()
      CALL GMTIME(ETIME-STIME, TM)
      WRITE(*,820) 'END MGAUSB  ', TM(3),':',TM(2),':',TM(1)
*
  810 FORMAT('0*** ',A12)
  820 FORMAT('0*** ',A12,I2.2,A,I2.2,A,I2.2)
  999 RETURN
      END SUBROUTINE BNMK
************************************************************************
      SUBROUTINE SMTEST
      IMPLICIT NONE
      INTEGER COLS, ROWS, SZ
      INTEGER I, J
      PARAMETER (COLS=6, ROWS=3, SZ=COLS*ROWS)
      DOUBLE PRECISION D(ROWS,COLS), E(ROWS,ROWS)
      DOUBLE PRECISION D1(ROWS,ROWS), D2(ROWS,ROWS)
      EQUIVALENCE (D1(1,1), D(1,1)), (D2(1,1), D(1,4))
      DATA D /SZ*0D0/
*
      WRITE(6,910)
      WRITE(6,920)
      WRITE(6,910)
      WRITE(6,980)'MATRIX D IS COMPRISED OF D1 AND D2.'
      WRITE(6,980)'       [ D ] =  [ D1 | D2 ]'
      WRITE(6,980)'MATRIX E IS SQUARE DIMENSIONED THE SAME AS'
      WRITE(6,980)'            D1 AND D2'
      WRITE(6,910)
*
      WRITE(6,*)
      WRITE(6,990)'TESTING MINIT1 ON [D].'
      CALL MINIT1(D, COLS*ROWS, 1D0)
      CALL MPRT(D,ROWS,COLS)
*
      WRITE(6,*)
      WRITE(6,990)'TESTING MINIT2 ON [D].'
      CALL MINIT2(D, ROWS, COLS, 2D0)
      CALL MPRT(D,ROWS,COLS)
*
      WRITE(6,*)
      WRITE(6,990)'TESTING MRND1 ON [D1].'
      CALL MRND1(D1,ROWS*ROWS,0)
      CALL MPRT(D,ROWS,COLS)
*
      WRITE(6,*)
      WRITE(6,990)'TESTING MIDEN ON [D2].'
      CALL MIDEN(D2,ROWS,COLS)
      CALL MPRT(D,ROWS,COLS)
*
      WRITE(6,*)
      WRITE(6,990)'ZEROING ELEMENTS D(2,1), D(2,2), D(2,3)...'
      D(2,1) = 0D0
      D(2,2) = 0D0
      D(2,3) = 0D0
      CALL MPRT(D,ROWS,COLS)
*
      WRITE(6,*)
      WRITE(6,990)'RUNNING GAUSS-JORDAN ELIMINATION ON [D]'
      CALL MGAUSJ(D, ROWS, COLS)
      CALL MPRT(D,ROWS,COLS)
*
      WRITE(6,*)
      WRITE(6,990)'RANDOMIZE [D1].'
      CALL MRND1(D1,ROWS*ROWS,0)
      CALL MPRT(D,ROWS,COLS)
*
      WRITE(6,*)
      WRITE(6,990)'IDENTITY [D2].'
      CALL MIDEN(D2,ROWS,ROWS)
      CALL MPRT(D,ROWS,COLS)
*
**    Copy D1 into E
*
      DO 110, I=1,ROWS
      DO 110, J=1,ROWS
  110 E(I,J) = D1(I,J)
*
      WRITE(6,*)
      WRITE(6,990)'RUNNING GAUSS ELIMINATION AND BACK SUBSTITUTION ON'
     &             //' [D]'
      CALL MGAUSB(D, ROWS, COLS)
      CALL MPRT(D,ROWS,COLS)
*
      WRITE(6,*)
      WRITE(6,990)'MULTIPLYING MATRIX BY INVERSE [D1] = [D2] * [E]'
      CALL MMUL(D2,ROWS,ROWS,E,ROWS,ROWS,D1,ROWS,ROWS)
      CALL MPRT(D1,ROWS,ROWS)
*
  910 FORMAT('********************************************************'
     2       '************************')
  920 FORMAT('******************************* RUNNING UNIT TESTS '
     1       '*****************************')
  980 FORMAT(15X,A)
  990 FORMAT('0*** ',A)
  999 RETURN
      END SUBROUTINE SMTEST
************************************************************************
* MINIT1: Matrix INITialize 1-D
************************************************************************
* Initialize matrix elements to a value based on a single dimension. 
* It matters not if the value is a 1, 2, N dimensional array, as long
* as the SZ does not overrun the array.
************************************************************************
*   M * Matrix to be initialized
*  SZ * Size of matrix
* VAL * Value to which matrix is initialized.
************************************************************************
      SUBROUTINE MINIT1(M, SZ, VAL)
      IMPLICIT NONE
      DOUBLE PRECISION M(*), VAL
      INTEGER SZ
      INTEGER I
      DO 110 I = 1, SZ
  110 M(I) = VAL
  999 RETURN
      END SUBROUTINE MINIT1
************************************************************************
* MINIT2: Matrix INITialize 2-D
************************************************************************
* Initialize matrix elements to a value based on a two dimensions.  It
* matters not if the value is a 1, 2, N dimensional array, as long as
* the product of ROWS X COLS does not overrun the array.
************************************************************************
*   M  * Matrix to be initialized
* ROWS * Rows (index 1)
* COLS * Columns (index 2)
* VAL  * Value to which matrix is initialized.
************************************************************************
      SUBROUTINE MINIT2(M, ROWS, COLS, VAL)
      IMPLICIT NONE
      DOUBLE PRECISION M(ROWS,COLS), VAL
      INTEGER COLS, ROWS
      INTEGER I, J
      DO 110 I = 1, ROWS
      DO 110 J = 1, COLS
  110 M(I,J) = VAL
  999 RETURN
      END SUBROUTINE MINIT2
************************************************************************
* MIDEN: Matrix IDENtity
************************************************************************
* Initializes given matrix as an identity based on the rows and columns.
* Identity matricies are necessarily square.  However, this procedure
* does not require that ROWS .EQ. COLS.  All elements on the diagonal
* (A(I,J) where I.EQ.J) are initialized to 1D0, and all other element
* are set to 0D0.
************************************************************************
*   M  * Matrix to be initialized to identity.
* ROWS * Rows (index 1)
* COLS * Columns (index 2)
************************************************************************
      SUBROUTINE MIDEN(M,ROWS,COLS)
      IMPLICIT NONE
      DOUBLE PRECISION M(ROWS,COLS)
      INTEGER ROWS,COLS
      INTEGER I, J
      DO 110 I = 1, ROWS
      DO 110 J = 1, COLS
      IF (I.EQ.J) THEN
        M(I,J) = 1D0
      ELSE
        M(I,J) = 0D0
      END IF
  110 CONTINUE
  999 RETURN
      END SUBROUTINE MIDEN
************************************************************************
* MRND1: Matrix RaNDomize 1-D
************************************************************************
* Initialize matrix elements to a random value based on a single
* dimension. It matters not if the value is a 1, 2, N dimensional
* array, as long as the SZ does not overrun the array.
************************************************************************
*   M  * Matrix to be initialized
*  SZ  * Size of matrix
* SEED * Seed for random number generator
************************************************************************
      SUBROUTINE MRND1(M,SZ,SEED)
      IMPLICIT NONE
      DOUBLE PRECISION M(*), VAL
      INTEGER SZ,SEED
      INTEGER I
      CALL SRAND(SEED)
      DO 110 I = 1,SZ
  110 M(I) = DBLE(RAND())
  999 RETURN
      END SUBROUTINE MRND1
************************************************************************
* MRND2: Matrix RaNDomize 2-D
************************************************************************
* Initialize matrix elements to a random value based on a two
* dimensions.  It matters not if the value is a 1, 2, N dimensional
* array, as long as the product of ROWS X COLS does not overrun
* the array.
************************************************************************
*   M  * Matrix to be initialized
* ROWS * Rows (index 1)
* COLS * Columns (index 2)
* SEED * Seed for random number generator
************************************************************************
      SUBROUTINE MRND2(M,ROWS,COLS,SEED)
      IMPLICIT NONE
      DOUBLE PRECISION M(ROWS,COLS)
      INTEGER COLS,ROWS,SEED
      INTEGER I,J
      CALL SRAND(SEED)
      DO 110 I = 1,ROWS
      DO 110 J = 1,COLS
  110 M(I,J) = DBLE(RAND())
  999 RETURN
      END SUBROUTINE MRND2
************************************************************************
* MGJAUS: Matrix GAUSs-Jordan elimination
************************************************************************
*    M : Matrix
* ROWS : Number of rows
* COLS : Number of columns
************************************************************************
* Consider the matrix below representing the system of equations:
*
*                          [A]*[x] = B
*
*                | A11 A12 A13 A14 A15 B16 B17 |
*                | A21 A22 A23 A24 A25 B26 B27 |
*                | A31 A32 A33 A34 A35 B36 B37 |
*                | A41 A42 A43 A44 A45 B46 B47 |
*                | A51 A52 A53 A54 A55 B56 B57 |
*
* This procedure performs Gauss-Jordan elimination with repeated calls
* to MGJECOL to zero out non-diagonal elements, and normalize to 1.0 
* resulting in the following:
*
*                | 1.0 0.0 0.0 0.0 0.0 X16 X17 |
*                | 0.0 1.0 0.0 0.0 0.0 X26 X27 |
*                | 0.0 0.0 1.0 0.0 0.0 X36 X37 |
*                | 0.0 0.0 0.0 1.0 0.0 X46 X47 |
*                | 0.0 0.0 0.0 0.0 1.0 X56 X57 |
*
* X values are where the solution matrix is stored.
*
************************************************************************
      SUBROUTINE MGAUSJ(M, ROWS, COLS)
      IMPLICIT NONE
      DOUBLE PRECISION M(ROWS,COLS),TMP
      INTEGER ROWS, COLS
      INTEGER I,J
*
      IF (ROWS.GT.COLS) THEN
        WRITE(0,910), ROWS, COLS
        STOP
      END IF
*
      DO 110 I = 1, ROWS
      CALL MPPVT(M,ROWS,COLS,I)
      CALL MGJECOL(M,ROWS,COLS,I)
      TMP = M(I,I)
      DO 110 J = I, COLS
      M(I,J) = M(I,J) / TMP
  110 CONTINUE
*
  910 FORMAT('*** FATAL ERRROR: MGAUSJ: MATRIX ROWS LARGER THAN'
     1' COLUMNS',/,18X,I8, '   .GT.   ',I8)
  999 RETURN
      END SUBROUTINE
************************************************************************
* MGAUSB: Matrix GAUSs elimination with Back substitution
************************************************************************
*    M : Matrix
* ROWS : Number of rows
* COLS : Number of columns
************************************************************************
* Consider the matrix below representing the system of equations:
*
*                          [A]*[x] = B
*
*                | A11 A12 A13 A14 A15 B16 B17 |
*                | A21 A22 A23 A24 A25 B26 B27 |
*                | A31 A32 A33 A34 A35 B36 B37 |
*                | A41 A42 A43 A44 A45 B46 B47 |
*                | A51 A52 A53 A54 A55 B56 B57 |
*
* This procedure performs Gauss elimination with repeated calls
* to MGECOL leave an upper triangle matrix.  Junk values are left in
* place below the diagonal to save operations.  This results in the
* following:
*
*                | A11 A12 A13 A14 A15 B16 B17 |
*                | JNK A22 A23 A24 A25 B26 B27 |
*                | JNK JNK A33 A34 A35 B36 B37 |
*                | JNK JNK JNK A44 A45 B46 B47 |
*                | JNK JNK JNK JNK A55 B56 B57 |
*
* The final step is to perform back substitution callin MBSUB resulting
* in the following:
*
*                | A11 A12 A13 A14 A15 X16 X17 |
*                | JNK A22 A23 A24 A25 X26 X27 |
*                | JNK JNK A33 A34 A35 X36 X37 |
*                | JNK JNK JNK A44 A45 X46 X47 |
*                | JNK JNK JNK JNK A55 X56 X57 |
*
* X values are where the solution matrix is stored.
*
************************************************************************
      SUBROUTINE MGAUSB(M, ROWS, COLS)
      IMPLICIT NONE
      DOUBLE PRECISION M(ROWS,COLS)
      INTEGER ROWS, COLS
      INTEGER I
*
      IF (ROWS.GT.COLS) THEN
        WRITE(0,910), ROWS, COLS
        STOP
      END IF
*
      CALL MGAUS(M,ROWS,COLS)
      CALL MBSUB(M,ROWS,COLS)
*
  910 FORMAT('*** FATAL ERRROR: MGAUSB: MATRIX ROWS LARGER THAN'
     1' COLUMNS',/,18X,I8, '   .GT.   ',I8)
  999 RETURN
      END SUBROUTINE
************************************************************************
* MGAUS: Matrix GAUSs elimination
************************************************************************
*    M : Matrix
* ROWS : Number of rows
* COLS : Number of columns
************************************************************************
* Consider the matrix below representing the system of equations:
*
*                          [A]*[x] = B
*
*                | A11 A12 A13 A14 A15 B16 B17 |
*                | A21 A22 A23 A24 A25 B26 B27 |
*                | A31 A32 A33 A34 A35 B36 B37 |
*                | A41 A42 A43 A44 A45 B46 B47 |
*                | A51 A52 A53 A54 A55 B56 B57 |
*
* This procedure performs Gauss elimination with repeated calls
* to MGECOL leave an upper triangle matrix.  Junk values are left in
* place below the diagonal to save operations.  This results in the
* following:
*
*                | A11 A12 A13 A14 A15 B16 B17 |
*                | JNK A22 A23 A24 A25 B26 B27 |
*                | JNK JNK A33 A34 A35 B36 B37 |
*                | JNK JNK JNK A44 A45 B46 B47 |
*                | JNK JNK JNK JNK A55 B56 B57 |
*
************************************************************************
      SUBROUTINE MGAUS(M, ROWS, COLS)
      IMPLICIT NONE
      DOUBLE PRECISION M(ROWS,COLS)
      INTEGER ROWS, COLS
      INTEGER I
*
      IF (ROWS.GT.COLS) THEN
        WRITE(0,910), ROWS, COLS
        STOP
      END IF
*
      DO 110 I = 1, ROWS
      CALL MPPVT(M,ROWS,COLS,I)
  110 CALL MGECOL(M,ROWS,COLS,I)
*
  910 FORMAT('*** FATAL ERRROR: MGAUS: MATRIX ROWS LARGER THAN'
     1' COLUMNS',/,18X,I8, '   .GT.   ',I8)
  999 RETURN
      END SUBROUTINE
************************************************************************
* MPPVT: Matrix Partial PiVoT
************************************************************************
      SUBROUTINE MPPVT(M,ROWS,COLS,ROW)
      IMPLICIT NONE
      DOUBLE PRECISION M(ROWS,COLS)
      INTEGER COLS,ROWS,ROW
      INTEGER J,K
      DOUBLE PRECISION TOL,TMP
      PARAMETER (TOL=1.0D-12)
*
      IF (ABS(M(ROW,ROW)).LE.TOL) THEN
      DO 110 J = ROW+1, ROWS
  110 IF (ABS(M(J,ROW)).GE.TOL) EXIT
      DO 120 K = ROW,COLS
      TMP = M(ROW,K)
      M(ROW,K) = M(J,K)
  120 M(J,K) = TMP
      END IF
  999 RETURN
      END SUBROUTINE MPPVT
************************************************************************
* MGJECOL: Matrix Gauss-Jordan Eliminate COLumn
************************************************************************
* Performs Gauss elimination on a particular column.  This procedure is
* called multiple times to columns for Gauss-Jordan elimination.
* Consider the following matrix:
*   
*                | A11 A12 A13 A14 A15 A16 A17 |
*                | 0.0 A22 A23 A24 A25 A26 A27 |
*                | 0.0 A32 A33 A34 A35 A36 A37 |
*                | 0.0 A42 A43 A44 A45 A46 A47 |
*                | 0.0 A52 A53 A54 A55 A56 A57 |
*
*
*   Column 2 is the next to be zeroed.  This is done by repeated calls
*   to MGJEROW.  The result is the following:
*
*                | A11 0.0 A13 A14 A15 A16 A17 |
*                | 0.0 A22 A23 A24 A25 A26 A27 |
*                | 0.0 0.0 A33 A34 A35 A36 A37 |
*                | 0.0 0.0 A43 A44 A45 A46 A47 |
*                | 0.0 0.0 A53 A54 A55 A56 A57 |
*
*   Note:  This procedure calls MGJEROW for every row in the matrix. 
*   Repeated calls, result in a matrix with non-zero values only on
*   the diagonal.  This is less efficient that elimination resulting
*   in a upper diagonal matrix to be used for backward substitution.
*
************************************************************************
*    M : Matrix
* ROWS : Matrix rows
* COLS : Matrix columns
*  COL : Associated column to be zeroed for elimination.
************************************************************************
      SUBROUTINE MGJECOL(M,ROWS,COLS,COL)
      IMPLICIT NONE
      DOUBLE PRECISION M(ROWS,COLS)
      INTEGER ROWS,COLS,COL
      INTEGER J
*
      DO 110 J = 1, ROWS
  110 IF (J.NE.COL) CALL MGEROW(M,ROWS,COLS,J,COL,1)
  999 RETURN
      END SUBROUTINE MGJECOL
************************************************************************
* MGECOL: Matrix Gauss Eliminate COLumn
************************************************************************
* Performs Gauss elimination on a particular column.  This procedure is
* called multiple times to columns for Gauss-Jordan elimination.
* Consider the following matrix:
*   
*                | A11 A12 A13 A14 A15 A16 A17 |
*                | JNK A22 A23 A24 A25 A26 A27 |
*                | JNK A32 A33 A34 A35 A36 A37 |
*                | JNK A42 A43 A44 A45 A46 A47 |
*                | JNK A52 A53 A54 A55 A56 A57 |
*
*
*   Column 2 is the next to be 'zeroed'.  This is done by repeated calls
*   to MGEROW with SCOL=COL+1.  The result is the following:
*
*                | A11 A12 A13 A14 A15 A16 A17 |
*                | JNK A22 A23 A24 A25 A26 A27 |
*                | JNK JNK A33 A34 A35 A36 A37 |
*                | JNK JNK A43 A44 A45 A46 A47 |
*                | JNK JNK A53 A54 A55 A56 A57 |
*
*   Note:  This procedure calls MGEROW for rows below the diagonal at
*   the particular column in the matrix. 
*
*   Repeated calls, result in a matrix with junk (JNK) values below
*   the diagonal and actuals values on and above the diagonal.  The
*   junk values are left in place to save operations for large systems.
*
************************************************************************
*    M : Matrix
* ROWS : Matrix rows
* COLS : Matrix columns
*  COL : Associated column to be zeroed for elimination.
************************************************************************
      SUBROUTINE MGECOL(M,ROWS,COLS,COL)
      IMPLICIT NONE
      DOUBLE PRECISION M(ROWS,COLS)
      INTEGER ROWS,COLS,COL
      INTEGER J
*
      DO 110 J = COL+1, ROWS
  110 CALL MGEROW(M,ROWS,COLS,J,COL,COL+1)
  999 RETURN
      END SUBROUTINE MGECOL
************************************************************************
* MGJEROW: Matrix Gauss-Jordan Eliminate ROW
************************************************************************
* Performs Gauss elimination on a particular row.  This procedure is
* called multiple times to eliminate a particular column. Consider
* the following matrix:
*   
*                | A11 A12 A13 A14 A15 A16 A17 |
*                | 0.0 A22 A23 A24 A25 A26 A27 |
*                | 0.0 A32 A33 A34 A35 A36 A37 |
*                | 0.0 A42 A43 A44 A45 A46 A47 |
*                | 0.0 A52 A53 A54 A55 A56 A57 |
*
*   Row 3, column 2 is the next to be zeroed.  This is done by
*   subtracting a scaled row 2 from row 3.  Scale factor RATIO is
*   calculated by:
*   
*                        RATIO = A32 / A22
*
*   This ratio is chosen so that the result at row 3, column 2 will
*   calculate to zero.
*             
*                  A32 = A32 - A22 * A32 / A22  (Results in 0.0)
*                  A33 = A33 - A23 * A32 / A22
*                  A34 = A34 - A24 * A32 / A22
*                  A35 = A35 - A25 * A32 / A22
*                  A36 = A36 - A26 * A32 / A22
*                  A37 = A37 - A27 * A32 / A22
*
*   The resulting state of the matrix after this procedure is:
*
*                | A11 A12 A13 A14 A15 A16 A17 |
*                | 0.0 A22 A23 A24 A25 A26 A27 |
*                | 0.0 0.0 A33 A34 A35 A36 A37 |
*                | 0.0 A42 A43 A44 A45 A46 A47 |
*                | 0.0 A52 A53 A54 A55 A56 A57 |
*
*   Note:  This procedure does not require that ROW .GT. COL.  The
*   result is that in the above example, the same operation could be
*   performed on row 1 to zero out A12.  This is useful if the
*   elimination is Gauss-Jordan where all elements not on the diagonal
*   are zeroed out.
*
*   Additionally, this procedure is called with a starting column. To
*   save operations, the start column may be set to COL+1 when calling
*   and nothing will be done to the elements to the left of COL.  This
*   is useful when reducing the matrix to a upper triangle matrix.
*
************************************************************************
*    M : Matrix
* ROWS : Matrix rows
* COLS : Matrix columns
*  ROW : Matrix row on which to perform elimination
*  COL : Associated column to be zeroed for elimination.
* SCOL : Column to begin elimination
************************************************************************
      SUBROUTINE MGEROW(M,ROWS,COLS,ROW,COL,SCOL)
      IMPLICIT NONE
      DOUBLE PRECISION M(ROWS,COLS)
      INTEGER ROWS,COLS,ROW,COL,SCOL
      DOUBLE PRECISION TOL,RATIO
      PARAMETER (TOL=1.0D-12)
      INTEGER I,J
*
      IF (ABS(M(ROW,COL)).GE.TOL) THEN
        RATIO = M(ROW,COL) / M(COL,COL)
        DO 110 I = SCOL, COLS
        IF (I.EQ.COL) THEN
*
** By definition this location must be zero.  Instead of doing the
** calculation, just set the value to zero.
*
          M(ROW,I) = 0D0
        ELSE
          M(ROW,I) = M(ROW,I) - RATIO * M(COL,I)
        ENDIF
  110   CONTINUE
      ELSE
        M(ROW,COL) = 0D0
      END IF
  999 RETURN
      END SUBROUTINE MGEROW
************************************************************************
* MBSUB: Matrix Back SUBstitution
************************************************************************
* Consider the reduced system.
*
*                | A11 A12 A13 A14 A15 B16 B17 |
*                | JNK A22 A23 A24 A25 B26 B27 |
*                | JNK JNK A33 A34 A35 B36 B37 |
*                | JNK JNK JNK A44 A45 B46 B47 |
*                | JNK JNK JNK JNK A55 B56 B57 |
*  
* JNK values are assumed to be 0.0.  Recall that this is a set
* of equations for each B column vector:
*
*      A11*X16 + A12*X26 + A13*X36 + A14*X46 + A15*X56 = B16
*                A22*X26 + A23*X36 + A24*X46 + A25*X56 = B26
*                          A33*X36 + A34*X46 + A35*X56 = B36
*                                    A44*X46 + A45*X56 = B46
*                                              A55*X56 = B56
*
*      A11*X17 + A12*X27 + A13*X37 + A14*X47 + A15*X57 = B17
*                A22*X27 + A23*X37 + A24*X47 + A25*X57 = B27
*                          A33*X37 + A34*X47 + A35*X57 = B37
*                                    A44*X47 + A45*X57 = B47
*                                              A55*X57 = B57
*
* These equations may be solved starting from the bottom:
* 
*                          <------------- K --------------->
* 
*  |  |     X56 = (B56                                        ) / A55
*  |  |     X46 = (B46 - A45*X56                              ) / A44
*  |  J     X36 = (B36 - A34*X46 - A35*X56                    ) / A33
*  |  |     X26 = (B26 - A23*X36 - A24*X46 - A25*X56          ) / A22
*  |  v     X16 = (B16 - A12*X26 - A13*X36 - A14*X46 - A15*X56) / A11
*  I
*  |  |     X57 = (B57                                        ) / A55
*  |  |     X47 = (B47 - A45*X57                              ) / A44
*  |  J     X37 = (B37 - A34*X47 - A35*X57                    ) / A33
*  |  |     X27 = (B27 - A23*X37 - A24*X47 - A25*X57          ) / A22
*  v  v     X17 = (B17 - A12*X27 - A13*X37 - A14*X47 - A15*X57) / A11
*
* The resulting aray has the solution vectors in place of the B
* vectors.
*
*                | A11 A12 A13 A14 A15 X16 X17 |
*                | JNK A22 A23 A24 A25 X26 X27 |
*                | JNK JNK A33 A34 A35 X36 X37 |
*                | JNK JNK JNK A44 A45 X46 X47 |
*                | JNK JNK JNK JNK A55 X56 X57 |
*
************************************************************************
*    M : Matrix
* ROWS : Matrix rows
* COLS : Matrix columns
************************************************************************
      SUBROUTINE MBSUB(M,ROWS,COLS)
      IMPLICIT NONE
      DOUBLE PRECISION M(ROWS,COLS)
      INTEGER ROWS,COLS
      INTEGER I,J,K
      DOUBLE PRECISION SUM
*
      DO 110 I = ROWS+1, COLS
      DO 110 J = ROWS, 1, -1
      SUM = 0D0
      DO 120 K = ROWS, J+1, -1
  120 SUM = SUM + M(J,K) * M(K,I)
  110 M(J,I) = (M(J,I) - SUM) / M(J,J)
  999 RETURN
      END SUBROUTINE MBSUB
************************************************************************
* MMUL:  Matrix Multipliction
************************************************************************
* Product of [A]*[B] is placed in P.
************************************************************************
* A     : Matrix A
* AROWS : Rows of A
* ACOLS : Cols of A (Must match BROWS)
* B     : Matrix B
* BROWS : Rows of B (Must match ACOLS)
* BCOLS : Cols of B
* P     : Matrix P (product: [P] = [A] * [B])
* PROWS : Rows of P (Must be .GE. AROWS)
* PCOLS : Cols of P (Must be .GE. BCOLS)
************************************************************************
*
*          [A]     *         [B]         =         [P]
*
*      | A11 A12 |   | B11 B12 B13 B14 |   | P11 P12 P13 P14 |
*      | A21 A22 | * | B21 B22 B23 B12 | = | P21 P22 P23 P24 |
*      | A31 A32 |                         | P31 P32 P33 P34 |
*
*
*                      <-------- K -------->
*
*                P11 = A11 * B11 + A12 * B21   ^   ^
*                P21 = A21 * B11 + A22 * B21   I   |
*                P31 = A31 * B11 + A32 * B21   v   |
*                                                  |
*                P12 = A11 * B12 + A12 * B22   ^   |
*                P22 = A21 * B12 + A22 * B22   I   |
*                P32 = A31 * B12 + A32 * B22   v    
*                                                  J
*                P13 = A11 * B13 + A12 * B23   ^    
*                P23 = A21 * B13 + A22 * B23   I   |
*                P33 = A31 * B13 + A32 * B23   v   |
*                                                  |
*                P14 = A11 * B14 + A12 * B24   ^   |
*                P24 = A21 * B14 + A22 * B24   I   |
*                P34 = A31 * B14 + A32 * B24   v   v
*
************************************************************************
      SUBROUTINE MMUL(A,AROWS,ACOLS,B,BROWS,BCOLS,P,PROWS,PCOLS)
      IMPLICIT NONE
      DOUBLE PRECISION A(AROWS,ACOLS), B(BROWS,BCOLS), P(PROWS,PCOLS)
      INTEGER AROWS, ACOLS, BROWS, BCOLS, PSIZE, PROWS, PCOLS
      INTEGER I, J, K
      DOUBLE PRECISION SUM
*
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
*
      DO 10 I = 1, PROWS
      DO 10 J = 1, PCOLS
      SUM = 0D0
      DO 20 K = 1, ACOLS
   20 SUM = SUM + A(I,K) * B(K,J)
   10 P(I,J) = SUM
*
  910 FORMAT('*** FATAL ERRROR: MMUL: MATRIX A COLUMNS MUST MATCH'
     1' MATRIX B ROWS:',/,18X,I8, '   .NE.   ', I8)
  920 FORMAT('*** FATAL ERRROR: MMUL: MATRIX P ROWS MUST AT LEAST'
     1' EQUAL MATRIX A ROWS:',/,18X,I8, '   .LT.   ', I8)
  930 FORMAT('*** FATAL ERRROR: MMUL: MATRIX P COLUMNS MUST AT LEAST'
     1' EQUAL MATRIX B COLUMNS:',/,18X,I8, '   .LT.   ', I8)
  999 RETURN
      END SUBROUTINE MMUL
************************************************************************
* MIPADD: Matrix In-Place ADDition
************************************************************************
      SUBROUTINE MIPADD(A,AROWS,ACOLS,B,BROWS,BCOLS)
      IMPLICIT NONE
      DOUBLE PRECISION A(AROWS,ACOLS),B(BROWS,BCOLS)
      INTEGER AROWS,ACOLS,BROWS,BCOLS
      INTEGER I,J
*
      DO 110 I = 1, MIN(AROWS,BROWS)
      DO 110 J = 1, MIN(ACOLS,BCOLS)
  110 A(I,J) = A(I,J) + B(I,J)
  999 RETURN
      END SUBROUTINE MIPADD
************************************************************************
* MIPSUB: Matrix In-Place SUBtraction
************************************************************************
      SUBROUTINE MIPSUB(A,AROWS,ACOLS,B,BROWS,BCOLS)
      IMPLICIT NONE
      DOUBLE PRECISION A(AROWS,ACOLS),B(BROWS,BCOLS)
      INTEGER AROWS,ACOLS,BROWS,BCOLS
      INTEGER I,J
*
      DO 110 I = 1, MIN(AROWS,BROWS)
      DO 110 J = 1, MIN(ACOLS,BCOLS)
  110 A(I,J) = A(I,J) + B(I,J)
  999 RETURN
      END SUBROUTINE MIPSUB
************************************************************************
* MPRT: Matrix PRinT
************************************************************************
      SUBROUTINE MPRT(M,ROWS,COLS)
      IMPLICIT NONE
      DOUBLE PRECISION M(ROWS,COLS)
      INTEGER ROWS, COLS
      INTEGER I, J
      CHARACTER F*9
*
      WRITE(F,'(A1I2A6)') '(', COLS, 'F10.4)'
      DO 10 I = 1, ROWS
   10 WRITE(6,F) (M(I,J), J=1, COLS)
  999 RETURN
      END SUBROUTINE MPRT
************************************************************************
