'-------------------------------------------------------------------------------
' D. Everhart
' 20200110                                             (Happy Birthday, Ethan!)
'-------------------------------------------------------------------------------
' The MIT License (MIT)
'
' Copyright (c) 2020 Daniel Everhart
'
' Permission is hereby granted, free of charge, to any person obtaining
' a copy of this software and associated documentation files (the
' "Software"), to deal in the Software without restriction, including
' without limitation the rights to use, copy, modify, merge, publish,
' distribute, sublicense, and/or sell copies of the Software, and to
' permit persons to whom the Software is furnished to do so, subject
' to the following conditions:
'
' The above copyright notice and this permission notice shall be
' included in all copies or substantial portions of the Software.
'
' THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
' EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
' MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
' IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
' CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
' TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
' SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'
'-------------------------------------------------------------------------------
Option Explicit
'-------------------------------------------------------------------------------
' Values that are returned by the VarType(function)
Public Const VarEmpty            As Integer = 0
Public Const VarNull             As Integer = 1
Public Const VarInteger          As Integer = 2
Public Const VarLong             As Integer = 3
Public Const VarSingle           As Integer = 4
Public Const VarDouble           As Integer = 5
Public Const VarCurrency         As Integer = 6
Public Const VarDate             As Integer = 7
Public Const VarString           As Integer = 8
Public Const VarObject           As Integer = 9
Public Const VarError            As Integer = 10
Public Const VarBoolean          As Integer = 11
Public Const VarVariant          As Integer = 12    ' only used with arrays of variants
Public Const VarDataAccessObject As Integer = 13
Public Const VarDecimal          As Integer = 14
Public Const VarByte             As Integer = 17
Public Const VarUserDefined      As Integer = 36
Public Const VarArray            As Integer = 8192
'-------------------------------------------------------------------------------
Private Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" _
                      (Destination As Any, Source As Any, ByVal length As Long)

Public Function HashCode(Value As Variant) As Long
  Select Case VarType(Value)
  Case VarInteger
    HashCode = HashCodeLong(CLng(Value))
  Case VarDouble
    HashCode = HashCodeDouble(CDbl(Value))
  Case VarString
    HashCode = HashCodeString(CStr(Value))
  Case Else
    Debug.Print "type not supported", VarType(Value)
    HashCode = 0
  End Select
End Function

Private Function HashCodeLong(Value As Long) As Long
  HashCodeLong = Value
End Function

Private Function HashCodeDouble(Value As Double) As Long
  Dim code(1) As Long
  CopyMemory code(0), Value, 8
  HashCodeDouble = code(0) Xor code(1)
End Function

Private Function HashCodeString(Value As String) As Long
  Dim code As Long, length As Integer, nWords As Integer, remainder As Integer
  Dim i As Integer, j As Integer
  Dim word(3) As Byte
  
  HashCodeString = 0
  length = Len(Value)
  nWords = length / 4
  
  For i = 0 To nWords - 1
    For j = 0 To 3
      word(j) = Asc(Mid(Value, 4 * i + j + 1, 1))
    Next j
    CopyMemory code, word(0), 4
    HashCodeString = HashCodeString Xor code
  Next i
  
  remainder = length Mod 4
  For j = 0 To 3
    If j < remainder Then
      word(j) = Asc(Mid(Value, 4 * i + j + 1, 1))
    Else
      word(j) = 0
    End If
  Next j
  CopyMemory code, word(0), 4
  HashCodeString = HashCodeString Xor code
End Function
