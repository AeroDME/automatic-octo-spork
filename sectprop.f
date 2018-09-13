************************************************************************
*                              S E C T P R O P                         *
*                                                          D. Everhart *
*                                                          06 SEP 2018 *
************************************************************************
* The MIT License (MIT)
* 
* Copyright (c) 2018 Daniel Everhart
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
* This program is to demonstrate the calculation of section properties
* of a general polygon.  To test this, the following closed form 
* solution of a right triangle is used.
*
*
*                ^ |\
*                | | \
*                | |  \                   B*H**3   B*H**3
*                | |   \            Ix =  ------ - ------
*                | |    \                   12       18
*                | |     \                         
*                | |      \                        
*                | |       \              H*B**3   H*B**3
*                  |        \       Iy =  ------ - ------
*                H |         \              12       18
*                  |          \
*                | |    |      \
*                | |<-->|<---- 1/3 B
*                | |    |        \                      B**2 *  H**2
*                | |    +-----    \             Ixy = -  ------------
*                | |  CENT  ^      \                         72     
*                | |        |       \
*                | |        |        \
*                | |                  \
*                | |      1/3 H        \
*                | |                    \
*                | |        |            \
*                v |________v_____________\
*                  <------- B --------->
*
************************************************************************
      PROGRAM SECTPROP
      IMPLICIT NONE
      INTEGER SIZE,STDOUT,STDIN,STDERR,I,J
      REAL B,H,ANG
      PARAMETER(SIZE=3,B=1.0,H=1.0,ANG=.78540,STDOUT=6,STDIN=5,STDERR=0)
      REAL POINTS(2,SIZE)
      REAL AREA,XCEN,YCEN,IXX,IYY,IXY
      REAL AINC,XI,XIP1,YI,YIP1
      DATA POINTS /        
     &                     0.,    0.,
     &                      B,    0.,
     &                     0.,     H 
     &                                      /
      DATA AREA,XCEN,YCEN,IXX,IYY,IXY/6*0.0/
      WRITE(STDOUT,9001)
*
      DO 1001 I=1,SIZE
 1001 WRITE(STDOUT,9002)(POINTS(J,I),J=1,2)
*
      DO 2001 I=1,SIZE
*
      XI = POINTS(1,I)
      YI = POINTS(2,I)
      IF (I.EQ.SIZE) THEN
      XIP1 = POINTS(1,1)
      YIP1 = POINTS(2,1)
      ELSE
      XIP1 = POINTS(1,I+1)
      YIP1 = POINTS(2,I+1)
      ENDIF
*
      AINC = XI * YIP1 - XIP1 * YI
      AREA = AREA + AINC
      XCEN = XCEN + AINC * (XI + XIP1)
      YCEN = YCEN + AINC * (YI + YIP1)
       IXX =  IXX + AINC * (YI**2.0 + YI*YIP1 + YIP1**2.0)
       IYY =  IYY + AINC * (XI**2.0 + XI*XIP1 + XIP1**2.0)
       IXY =  IXY + AINC * (XI*YIP1+2.0*XI*YI+2.0*XIP1*YIP1+XIP1*YI)
*
 2001 CONTINUE
*
      AREA = AREA /  2.0
      XCEN = XCEN /  6.0 / AREA
      YCEN = YCEN /  6.0 / AREA
       IXX =  IXX / 12.0
       IYY =  IYY / 12.0
       IXY =  IXY / 24.0
*
      WRITE(STDOUT,9003)
      WRITE(STDOUT,9004)
      WRITE(STDOUT,9002) AREA,XCEN,YCEN,IXX,IYY,IXY
*
      CALL INRTR2(AREA,XCEN,YCEN,IXX,IYY,IXY,0.0,0.0,XCEN,YCEN)
*
      WRITE(STDOUT,9005)
      WRITE(STDOUT,9004)
      WRITE(STDOUT,9002) AREA,XCEN,YCEN,IXX,IYY,IXY
*
      CALL T2DROT(IXX,IYY,IXY,ANG)
*
      WRITE(STDOUT,9006) ANG
      WRITE(STDOUT,9004)
      WRITE(STDOUT,9002) AREA,XCEN,YCEN,IXX,IYY,IXY
*
      AREA = 0.5 * B * H
      XCEN = B / 3.0
      YCEN = H / 3.0
      IXX  = 0.02777778 * B * H ** 3.0
      IYY  = 0.02777778 * H * B ** 3.0
      IXY  = - B ** 2.0 * H ** 2.0 / 72.0
*
      WRITE(STDOUT,9007)
      WRITE(STDOUT,9004)
      WRITE(STDOUT,9002) AREA,XCEN,YCEN,IXX,IYY,IXY
*
*
 8999 STOP
 9001 FORMAT(/'$**** VERTEX DATA')
 9002 FORMAT(5X,6F12.5)
 9003 FORMAT(/'$**** SECTION PROPERTY DATA')
 9004 FORMAT(5X,'    AREA    ','    XCEN    ','    YCEN    ',
     &          '     IXX    ','     IYY    ','     IXY    ')
 9005 FORMAT(/'$**** TRANSLATED SECTION PROPERTY DATA')
 9006 FORMAT(/'$**** ROTATED SECTION PROPERTY DATA, ',F6.4,' RADIANS')
 9007 FORMAT(/'$**** CLOSED FORM SECTION PROPERTY DATA')
      END PROGRAM SECTPROP
************************************************************************
*                             I N R T R 2
************************************************************************
* Translates inertias from one axis system to another.
*
************************************************************************
      SUBROUTINE INRTR2(AREA,XCEN,YCEN,IXX,IYY,IXY,XOLD,YOLD,XNEW,YNEW)
      IMPLICIT NONE
      REAL AREA,XCEN,YCEN,IXX,IYY,IXY,XOLD,YOLD,XNEW,YNEW
      REAL X,XP,Y,YP
*
      X  = XOLD - XCEN
      Y  = YOLD - YCEN
      XP = XNEW - XCEN
      YP = YNEW - YCEN
*     Ixy'= Ixy +  A   * ( x' * y' - x * y )
      IXY = IXY + AREA * ( XP * YP - X * Y )
*
*     Ixy'= Ixy +  A   * ( d' ** 2.0 - d ** 2.0 )
      IXX = IXX + AREA * ( YP ** 2.0 - Y ** 2.0 )
      IYY = IYY + AREA * ( XP ** 2.0 - X ** 2.0 )
 8999 RETURN
      END SUBROUTINE INRTR2
************************************************************************
*                             T 2 D R O T
************************************************************************
* Rotates a 2D tensor (eg: stress or inertia...)
*
************************************************************************
      SUBROUTINE T2DROT(TXX,TYY,TXY,RAD)
      IMPLICIT NONE
      REAL TXX,TYY,TXY,RAD
      REAL TMPP,TMPM,C2,S2
      TMPP = ( TXX + TYY ) / 2.0
      TMPM = ( TXX - TYY ) / 2.0
      C2 = COS( 2.0 * RAD )
      S2 = SIN( 2.0 * RAD )
      TXX = TMPP + TMPM * C2 - TXY * S2
      TYY = TMPP - TMPM * C2 + TXY * S2
      TXY = TMPM * S2 + TXY * C2
 8999 RETURN
      END SUBROUTINE T2DROT
************************************************************************
