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
      INTEGER BTINSERT,BTIMIN,BTIMAX
      INTEGER SZ,TSZ
      PARAMETER (SZ=10,TSZ=3*SZ)
      INTEGER VALUES(SZ),TREE(3,SZ),PERM(SZ)
      DATA VALUES /44,64,87,42,85,14,60,78,72,18/
      DATA PERM   /SZ*1/
      DATA TREE   /TSZ*0/
      INTEGER I,J,K,RET
*
      CALL PBTREE(TREE,SZ,6)
*
      K = 1
      DO  111 WHILE (K.LE.SZ)
      IF (.NOT.BTEXISTS(VALUES(K),TREE,K)) THEN
        RET = BTINSERT(VALUES(K),TREE,K)
      END IF
      K = K + 1
  111 CONTINUE
*
      WRITE(6,*)
      WRITE(6,*)
      CALL PBTREE(TREE,SZ,6)
      WRITE(6,*)
      WRITE(6,*) 'MIN VALUE =', TREE(1,BTIMIN(TREE,SZ))
      WRITE(6,*) 'MAX VALUE =', TREE(1,BTIMAX(TREE,SZ))
      J = 1
      CALL BTPERM(TREE,1,PERM,J,SZ)
      WRITE(6,*)
      WRITE(6,*)
      WRITE(6,*)
      WRITE(6,811)
      WRITE(6,816)
      WRITE(6,821)
      WRITE(6,811)
      DO  131 I = 1,SZ
  131 WRITE(6,831) TREE(1,PERM(I))
      WRITE(6,811)
*
  799 GOTO 999
  811 FORMAT('  +--------+')
  816 FORMAT('  | SORTED |')
  821 FORMAT('  |  VALUE |')
  831 FORMAT('  | ',I6,' |')
  999 STOP
      END PROGRAM BTREE
************************************************************************
      SUBROUTINE PBTREE(TREE,SIZE,UNIT)
      IMPLICIT NONE
      INTEGER TREE(3,SIZE),SIZE,UNIT
      INTEGER I,J
      WRITE(UNIT,811)
      WRITE(UNIT,821)
      WRITE(UNIT,811)
      DO  111 I = 1,SIZE
  111 WRITE(UNIT,831) (TREE(J,I), J=1,3)
      WRITE(UNIT,811)
  799 GOTO 999
  811 FORMAT('  +--------+--------+--------+')
  821 FORMAT('  |  VALUE | BEFORE |  AFTER |')
  831 FORMAT('  | ',I6,' | ',I6,' | ',I6,' |')
  999 RETURN
      END SUBROUTINE PBTREE
************************************************************************
      RECURSIVE SUBROUTINE BTPERM(TREE,I,PERM,J,SIZE)
      IMPLICIT NONE
      INTEGER TREE(3,SIZE),I,PERM(SIZE),J,SIZE
      IF (TREE(2,I).NE.0) THEN
        CALL BTPERM(TREE,TREE(2,I),PERM,J,SIZE)
      END IF
      PERM(J) = I
      J = J + 1
      IF (TREE(3,I).NE.0) THEN
        CALL BTPERM(TREE,TREE(3,I),PERM,J,SIZE)
      END IF
  999 RETURN
      END SUBROUTINE BTPERM
************************************************************************
      INTEGER FUNCTION BTIMIN(TREE,COUNT)
      IMPLICIT NONE
      INTEGER TREE(3,COUNT),COUNT
      INTEGER J
      BTIMIN = 0
      IF (COUNT.EQ.0) RETURN
      J = 1
      DO  111 WHILE (J.LE.COUNT)
      IF (TREE(2,J).EQ.0) THEN
        BTIMIN = J
        RETURN
      ELSE
        J = TREE(2,J)
      END IF
      IF (J.EQ.0) EXIT
  111 CONTINUE
      END FUNCTION BTIMIN
************************************************************************
      INTEGER FUNCTION BTIMAX(TREE,COUNT)
      IMPLICIT NONE
      INTEGER TREE(3,COUNT),COUNT
      INTEGER J
      BTIMAX = 0
      IF (COUNT.EQ.0) RETURN
      J = 1
      DO  111 WHILE (J.LE.COUNT)
      IF (TREE(3,J).EQ.0) THEN
        BTIMAX = J
        RETURN
      ELSE
        J = TREE(3,J)
      END IF
      IF (J.EQ.0) EXIT
  111 CONTINUE
      END FUNCTION BTIMAX
************************************************************************
      LOGICAL FUNCTION BTEXISTS(VALUE,TREE,COUNT)
      IMPLICIT NONE
      INTEGER VALUE,TREE(3,COUNT),COUNT
      INTEGER J
      BTEXISTS = .FALSE.
      IF (COUNT.GT.0) THEN
        J =1
        DO  111 WHILE (J.LE.COUNT)
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
************************************************************************
      INTEGER FUNCTION BTINSERT(VALUE,TREE,INDEX)
      IMPLICIT NONE
      INTEGER VALUE,TREE(3,INDEX),INDEX
      INTEGER I, J
      BTINSERT = 0
*
      IF (INDEX.LT.1) RETURN
      IF (INDEX.EQ.1) THEN
        TREE(1,INDEX) = VALUE
        TREE(2,INDEX) = 0
        TREE(3,INDEX) = 0
        BTINSERT = INDEX
        RETURN
      END IF
*
      J = 1
      DO  111 WHILE (J.LT.INDEX)
        IF (J.EQ.0) EXIT
        IF (VALUE.LT.TREE(1,J)) THEN
          I = 2
        ELSE IF (VALUE.GT.TREE(1,J)) THEN
          I = 3
        ELSE
          I = 0
        END IF
        IF (I.EQ.0) EXIT
        IF(TREE(I,J).EQ.0) THEN
          TREE(1,INDEX) = VALUE
          TREE(I,J) = INDEX
          BTINSERT = INDEX
          J = 0
        ELSE
          J = TREE(I,J)
        END IF
  111 CONTINUE
  999 RETURN
      END FUNCTION BTINSERT
************************************************************************
