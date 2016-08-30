VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "Dictionary"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Dictionary
' D. Everhart
' 30 AUG 2016
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Dependancies:  None
' TODO: Dictionary documentation
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'2345678901234567890123456789012345678901234567890123456789012345678901234567890
'        1         2         3         4         5         6         7         8
Option Explicit
Public Keys As New Collection
Public Values As New Collection

Public Sub Remove(key As String)
  Dim i As Integer
  i = Me.IndexOf(key)
  Me.Keys.Remove (i)
  Me.Values.Remove (i)
End Sub

Public Function ContainsKey(key As String) As Boolean
  ContainsKey = Me.IndexOf(key) > 0
End Function

Public Function GetItem(key As String) As String
  GetItem = Me.Values.Item(Me.IndexOf(key))
End Function

Public Sub SetItem(key As String, value As String)
  Dim i As Integer
  i = Me.IndexOf(key)
  If i > 1 Then
    Me.Values.Remove (i)
    Me.Values.Add value, , , i - 1
  ElseIf i > 0 Then
    Me.Values.Remove (i)
    Me.Values.Add value, , i
  Else
    Me.Keys.Add key
    Me.Values.Add value
  End If
End Sub

Public Function IndexOf(key As String) As Integer
  Dim i As Integer
  For i = 1 To Me.Keys.Count
    If Me.Keys(i) = key Then
      IndexOf = i
      Exit Function
    End If
  Next i
  IndexOf = 0
End Function

Public Function NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
  ' Hidden attribute statement:
  ' Attribute NewEnum.VB_UserMemID = -4
  Set NewEnum = Me.Keys.[_NewEnum]
End Function

Public Function Count() As Integer
  Count = Me.Keys.Count
End Function
