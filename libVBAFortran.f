************************************************************************
* D. Everhart
* 22 AUG 2018
* FLIB
************************************************************************
* FORTRAN library template for VBA DLL libraries.
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
************************************************************************
*  This is meant to compile with the GNU gfortran compiler. It may be
*  useful as a guide for other compilers, but I don't guarantee it.
*
*  gfortran -m32 -static -shared flib.f -o libflib.dll
*
*  -m32 compiles 32-bit.
*  -static adds all runtime routines to dll.
*  -share compiles to dll.
*
*  Compiling with the above options will allow the VBA code below
*  to work correctly in a 32-bit MS Excel worksheet.
*
************************************************************************
*  Public Declare Function add_ Lib "C:\Users\k0145355\libflib.dll" _
*          Alias "add_@8" (ByRef a As Long, ByRef b As Long) As Long
*  Public Declare Sub subtract_ Lib "C:\Users\k0145355\libflib.dll" _
*          Alias "subtract_@8" (ByRef a As Long, ByRef b As Long)
*  Public Declare Sub hypo_ Lib "C:\Users\k0145355\libflib.dll" _
*          Alias "hypo_@4" (ByRef a As Single)
*  Public Declare Sub point_ Lib "C:\Users\k0145355\libflib.dll" _
*          Alias "point_@4" (ByRef p As TPoint)
*  Public Declare Sub strmod_ Lib "C:\Users\k0145355\libflib.dll" _
*          Alias "strmod_@8" (ByRef bstr As Byte, n As Long)
*                     
*  Type TPoint
*    ID As Long
*    X As Single
*    Y As Single
*    Z As Single
*    R As Single
*  End Type
*  
*  Public Sub runall()
*    Dim a As Long, b As Long, c As Long
*    Dim x(0 To 2) As Single
*      
*    a = 3
*    b = 4
*    c = add_(a, b)
*    Debug.Print c
*  
*    Debug.Print a, b
*    Call subtract_(a, b)
*    Debug.Print a, b
*    
*    x(0) = 3#
*    x(1) = 4#
*    Debug.Print x(0), x(1), x(2)
*    Call hypo_(x(0))
*    Debug.Print x(0), x(1), x(2)
*      
*    pt.X = 1#
*    pt.Y = 2#
*    pt.Z = 3#
*    Debug.Print pt.ID, pt.X, pt.Y, pt.Z, pt.R
*    Call point_(pt)
*    Debug.Print pt.ID, pt.X, pt.Y, pt.Z, pt.R
*    s = "This is a string that is much longer than the dll will handle."
*    Debug.Print "<" & s & ">"
*    bt = StrConv(s, vbFromUnicode)
*    Call strmod_(bt(0), 32)
*    s = StrConv(bt, vbUnicode)
*    Debug.Print "<" & s & ">"
*      
*  End Sub
************************************************************************
*
*  Using the nm tool, the entry point names can be found.
*
*                  67401450 T _add_@8
*                  6740146c T _subtract_@8
*                  67401484 T _hypo_@4
*
*  The leading underscore is not in the VBA declarations... not sure
*  why
*
************************************************************************
*  Alternately, adding the -mrtd option will disable the name mangling.
*  so that the "add_@8" reference is "add_".  This option is only
*  available when compiling 32-bit DLLs (-m32).
************************************************************************
*
*  The Alias strings are added which tell VBA how to find the entry
*  point.
*
************************************************************************
      INTEGER FUNCTION ADD(A,B)
*GCC$ ATTRIBUTES STDCALL,DLLEXPORT :: ADD
      INTEGER A,B
      ADD = A + B
      END FUNCTION
************************************************************************
      SUBROUTINE SUBTRACT(A,B)
*GCC$ ATTRIBUTES STDCALL,DLLEXPORT :: SUBTRACT
      INTEGER A,B
      B = B - A
      END SUBROUTINE
************************************************************************
      SUBROUTINE HYPO(A)
*GCC$ ATTRIBUTES STDCALL,DLLEXPORT :: HYPO
      REAL A(3)
      A(3) = SQRT(A(1)**2 + A(2)**2)
      END SUBROUTINE
************************************************************************
      SUBROUTINE POINT(A)
*GCC$ ATTRIBUTES STDCALL,DLLEXPORT :: POINT
      TYPE TPOINT
      SEQUENCE
      INTEGER ID
      REAL X,Y,Z,R
      END TYPE TPOINT
      TYPE(TPOINT) A
      A%ID = 9999
      A%R = SQRT(A%X**2 + A%Y**2 + A%Z**2)
      END SUBROUTINE
************************************************************************
      SUBROUTINE STRMOD(S,N)
*GCC$ ATTRIBUTES STDCALL,DLLEXPORT :: STRMOD
      INTEGER N,SZ,I
      PARAMETER (SZ=32)
      INTEGER*1 S(N)
      INTEGER*1 BYTES(SZ)
      CHARACTER STRING*(SZ)
      EQUIVALENCE (BYTES,STRING)
      STRING = '0**** HELLO FROM FORTRAN!'
      DO 10 I = 1, MIN(N,SZ)
   10 S(I) = BYTES(I)
      END SUBROUTINE
************************************************************************
