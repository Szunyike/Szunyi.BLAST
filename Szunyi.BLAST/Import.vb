
Imports System.Globalization
Imports System.IO
Imports System.Text
Imports System.Xml
Imports Bio.Web.Blast
Imports Bio.Extensions
Imports Bio.Util.Logging
Imports Bio.IO
Imports Szunyi.BLAST.Parsers

Public Class Import
    Shared Iterator Function From_Files(Files As List(Of FileInfo), log As StringBuilder) As IEnumerable(Of BlastSearchRecord)
        Dim x As New List(Of BlastResult)
        For Each File In Files
            Yield From_File(File, log)
        Next
    End Function
    Shared Iterator Function From_Files_Small(Files As List(Of FileInfo), log As StringBuilder) As IEnumerable(Of BlastSearchRecord)
        Dim x As New List(Of BlastResult)
        For Each File In Files
            Yield From_File(File, log, True)
        Next
    End Function
    Shared Iterator Function From_File(File As FileInfo, log As StringBuilder, Optional Compressed As Boolean = False) As IEnumerable(Of OwnBlastRecord)
        If File.Exists = True Then
            Try
                Select Case File.Extension
                    Case Szunyi.IO.File_Extension.Xml
                        For Each Item In OwnBlastXmlParser.ParseXMLFile(File)
                            ' Yield Item
                        Next
                    Case Szunyi.IO.File_Extension.b5
                        For Each Item In OwnBlastXmlParser.ParseXMLFile(File)
                            ' Yield Item
                        Next
                    Case Szunyi.IO.File_Extensions.Blast_tabular
                        For Each Item In Szunyi.BLAST.Parsers.b6.Parse(File)

                        Next
                    Case Szunyi.IO.File_Extension.b7
                        For Each Item In Szunyi.BLAST.Parsers.b7.Parse(File, Compressed)
                            Yield Item
                        Next
                End Select

            Catch ex As Exception
                log.Append("Not avalible:" & File.Name)

            End Try

        End If

    End Function
    Shared Iterator Function From_File_HSP(File As FileInfo, log As StringBuilder, Optional Compressed As Boolean = False) As IEnumerable(Of OwnHsp)
        If File.Exists = True Then
            Try
                Select Case File.Extension
                    Case Szunyi.IO.File_Extension.Xml
                        For Each Item In OwnBlastXmlParser.ParseXMLFile(File)
                            ' Yield Item
                        Next
                    Case Szunyi.IO.File_Extension.b5
                        For Each Item In OwnBlastXmlParser.ParseXMLFile(File)
                            ' Yield Item
                        Next
                    Case Szunyi.IO.File_Extensions.Blast_tabular
                        For Each Item In Szunyi.BLAST.Parsers.b6.Parse(File)

                        Next
                    Case Szunyi.IO.File_Extension.b7
                        For Each Item In Szunyi.BLAST.Parsers.b7.ParseHsp(File)
                            Yield Item
                        Next
                End Select

            Catch ex As Exception
                log.Append("Not avalible:" & File.Name)

            End Try

        End If

    End Function

End Class





