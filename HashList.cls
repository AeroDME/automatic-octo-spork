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
Private Const size As Long = 256
Private hash(1 To size, 1 To 3) As Long
Private count As Long

Private Sub Class_Initialize()
  Dim i As Integer, j As Integer
  count = 0
  For i = 1 To size
  For j = 1 To 3
    hash(i, j) = 0
  Next j
  Next i
End Sub

Public Function Contains(value As Long) As Boolean
  Dim j As Long
  Contains = False
  If count > 0 Then
    j = 1
    While j <= count
      If value = hash(j, 1) Then
        Contains = True
        Exit Function
      ElseIf value < hash(j, 1) Then
        j = hash(j, 2)
      Else
        j = hash(j, 3)
      End If
      If j = 0 Then Exit Function
    Wend
  End If
End Function

Public Function Add(value As Long) As Long
  Dim i As Long, j As Long
  Add = 0
  
  If (count = 0) Then
    count = count + 1
    hash(count, 1) = value
    hash(count, 2) = 0
    hash(count, 3) = 0
    Add = count
    Exit Function
  End If
  
  j = 1
  While (j <= count)
    If j = 0 Then Exit Function
    If value < hash(j, 1) Then
      i = 2
    ElseIf value > hash(j, 1) Then
      i = 3
    Else
      i = 0
    End If
    
    If i = 0 Then Exit Function
    
    If hash(j, i) = 0 Then
      count = count + 1
      hash(count, 1) = value
      hash(j, i) = count
      Add = count
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
