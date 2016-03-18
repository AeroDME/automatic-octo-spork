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
* FMTDP: Format a DOUBLE PRECISION value to fit in buffer with max
* precision.
************************************************************************
*  VAL * I * DOUBLE PRECISION value to format
*   WD * I * Desired field width
* BUFF * O * Buffer in which to write formatted value
************************************************************************
* Error will result in writing '*' characters to the buffer.
* The format for real numbers is:
*                           Fw.d
*                           Ew.dEe
*                           Gw.d
* W is the total width, D is the number of digits to the right of
* the floating point.  This subroutine starts with the total width,
* and figures out how much width is allowaded for D.
************************************************************************
* W    * 'w' of Gw.d format descriptor
* D    * 'd' of Gw.d format descriptor
* E    * 'e' of Ew.dEe format descriptor
* IOS  * IOSTAT for read/write operations
* FIOS * IOSTAT for reading FVAL from TBUF
* EIOS * IOSTAT for reading EVAL from TBUF
* FSTR * Fw.d format string
* ESTR * Ew.dEe format string
* TBUF * Temporary buffer for writing formated real value
* FVAL * Value read from buffer based of Fw.d format string
* EVAL * Value read from buffer based of Ew.d format string
************************************************************************
      SUBROUTINE FMTDP(VAL, BUFF, WD)
      IMPLICIT NONE
      DOUBLE PRECISION VAL
      CHARACTER*(*) BUFF
      INTEGER WD
*
      INTEGER W, D, E, POW10
      INTEGER IOS, FIOS, EIOS
      CHARACTER*16 FSTR, ESTR
      CHARACTER*32 TBUF
      DOUBLE PRECISION FVAL, EVAL
*
      FVAL = 0D0
      EVAL = 0D0
      W = MIN(LEN(BUFF), WD)
      DO 100 D = 1, W
  100 BUFF(D:D) = '*'
*
** Special case when value is 0D0
*
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
*
** Attempt to write value using Fw.d
*
      D = W
      TBUF = '*'
      DO 200 WHILE (TBUF(1:1).EQ.'*')
      D = D - 1
      IF (D.LT.0) EXIT
      WRITE(FSTR,920, IOSTAT=IOS) W, D
  200 WRITE(TBUF, FSTR, IOSTAT=IOS) VAL
      READ(TBUF,*,IOSTAT=FIOS) FVAL
*
** Attempt to write value using Ew.d
*
      D = W
      E = POW10(ABS(REAL(POW10(VAL),8)))
      TBUF = '*'
      DO 300 WHILE (TBUF(1:1).EQ.'*')
      D = D - 1
      IF (D.LT.0) EXIT
      WRITE(ESTR,930, IOSTAT=IOS) W, D, E
  300 WRITE(TBUF, ESTR, IOSTAT=IOS) VAL
      READ(TBUF,*,IOSTAT=EIOS) EVAL
*
** Determine which method yields a value which is nearest the 
** original value.
*
      IF(FIOS.NE.0) THEN
        WRITE(BUFF, ESTR, IOSTAT=IOS) VAL
      ELSE
        IF (ABS(EVAL-VAL).GT.ABS(FVAL-VAL)) THEN
          WRITE(BUFF, FSTR, IOSTAT=IOS) VAL
        ELSE
          WRITE(BUFF, ESTR, IOSTAT=IOS) VAL
        END IF
      END IF
*
      RETURN
  920 FORMAT('(F'I2'.'I2')')
  930 FORMAT('(E'I2'.'I2'E'I1')')
      END SUBROUTINE FMTDP
************************************************************************
* POW10
************************************************************************
* Returns the power of 10 for the value.  12.3 -> 2  0.12 -> 0
************************************************************************
*  X  * I * DOUBLE PRECISION for which to return the power of 10
************************************************************************
      INTEGER FUNCTION POW10(X)
      IMPLICIT NONE
      DOUBLE PRECISION X, XC
*
**    In the special case that the value is exactly 0.0, we need to
**    exit.  Otherwise, there will be an endless loop.
*
      XC = ABS(X)
      POW10 = 0
      IF (XC.EQ.0D0) RETURN
*
**    This is set to 1 so that a value less than one will go through
**    one cycle setting it to zero to meet the expection in the 
**    description above.
*
      POW10 = 1
*
      DO 100 WHILE(XC.GE.10D0)
      XC = XC / 10D0
100   POW10 = POW10 + 1
*
      DO 200 WHILE(XC.LT.1D0)
      XC = XC * 10D0
200   POW10 = POW10 - 1
*
      RETURN
      END FUNCTION POW10
************************************************************************
