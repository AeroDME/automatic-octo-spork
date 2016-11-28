'-------------------------------------------------------------------------------
' D. Everhart
' 28 NOV 2016
'-------------------------------------------------------------------------------
' The MIT License (MIT)
'
' Copyright (c) 2016 Daniel Everhart
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
'
'            +------+       +------+------+
'  IND       |VALUES|       |BEFORE|AFTER |              44
'            +------+       +------+------+             /  \
'   1        |  44  |       |  4   |   2  |           42    64
'   2        |  64  |       |  7   |   3  |          /     /   \
'   3        |  87  |       |  5   |   0  |        14    60    87
'   4        |  42  |       |  6   |   0  |          \        /
'   5        |  85  |       |  8   |   0  |           18    85
'   6        |  14  |       |  0   |  10  |                /
'   7        |  60  |       |  0   |   0  |              78
'   8        |  78  |       |  9   |   0  |             /
'   9        |  72  |       |  0   |   0  |           72
'  10        |  18  |       |  0   |   0  |
'            +------+       +------+------+
'
'-------------------------------------------------------------------------------
Option Explicit
Private Const size As Long = 8192
Private hash(1 To size, 1 To 3) As Long
Private count_ As Long
Public Property Get Count() As Long
  Count = count_
End Property

Private Sub Class_Initialize()
  Dim i As Integer, j As Integer
  count_ = 0
  For i = 1 To size
  For j = 1 To 3
    hash(i, j) = 0
  Next j
  Next i
End Sub
Public Function IndexOf(Value As Long) As Long
  IndexOf = 0
  If count_ > 0 Then
    IndexOf = 1
    While IndexOf <= count_
      If Value = hash(IndexOf, 1) Then
        Exit Function
      ElseIf Value < hash(IndexOf, 1) Then
        IndexOf = hash(IndexOf, 2)
      Else
        IndexOf = hash(IndexOf, 3)
      End If
      If IndexOf = 0 Then Exit Function
    Wend
  End If
End Function

Public Function Contains(Value As Long) As Boolean
  Contains = IndexOf(Value) > 0
End Function

Public Function Add(Value As Long) As Long
  Dim i As Long, j As Long
  Add = 0
  
  If (count_ = 0) Then
    count_ = count_ + 1
    hash(count_, 1) = Value
    hash(count_, 2) = 0
    hash(count_, 3) = 0
    Add = count_
    Exit Function
  End If
  
  j = 1
  While (j <= count_)
    If j = 0 Then Exit Function
    If Value < hash(j, 1) Then
      i = 2
    ElseIf Value > hash(j, 1) Then
      i = 3
    Else
      i = 0
    End If
    
    If i = 0 Then Exit Function
    
    If hash(j, i) = 0 Then
      count_ = count_ + 1
      hash(count_, 1) = Value
      hash(j, i) = count_
      Add = count_
      j = 0
    Else
      j = hash(j, i)
    End If
    
  Wend
End Function

Public Sub sorted(s As Collection, i As Long)
  If hash(i, 2) <> 0 Then
    sorted s, hash(i, 2)
  End If
  s.Add hash(i, 1)
  If hash(i, 3) <> 0 Then
    sorted s, hash(i, 3)
  End If
End Sub
