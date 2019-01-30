Imports System.IO
Imports System.Text
Imports System.Threading
Imports Szunyi.Common
Imports Szunyi.IO.Util_Helpers

Namespace Console
    Public Class CreateDatabase
        Private Files As New List(Of FileInfo)
        Private isDNA As Boolean
        Private BlastPath As DirectoryInfo
        Private DBPath As DirectoryInfo
        Private TaxID As String
        Private TaxFile As FileInfo
        Public Sub New(Files As List(Of FileInfo), isdna As Boolean, BlastPath As DirectoryInfo, DBPath As DirectoryInfo, Optional TaxID As String = "")
            Me.Files = Files
            Me.isDNA = isdna
            Me.BlastPath = BlastPath
            Me.DBPath = DBPath
            Me.TaxID = TaxID
        End Sub
        Public Sub New(File As FileInfo, isdna As Boolean, BlastPath As DirectoryInfo, DBPath As DirectoryInfo, TaxFile As FileInfo)
            Me.Files.Add(File)
            Me.isDNA = isdna
            Me.BlastPath = BlastPath
            Me.DBPath = DBPath
            Me.TaxFile = TaxFile
        End Sub
        Public Sub New(File As FileInfo, isdna As Boolean, BlastPath As DirectoryInfo, DBPath As DirectoryInfo, Optional TaxID As String = "")
            Me.Files.Add(File)
            Me.isDNA = isdna
            Me.BlastPath = BlastPath
            Me.DBPath = DBPath
            Me.TaxID = TaxID
        End Sub
        Public Sub DoIt()
            For Each File In Files
                Dim consoleApp As New Process
                With consoleApp
                    .EnableRaisingEvents = True
                    .StartInfo.FileName = Me.BlastPath.FullName & "\" & "makeblastdb.exe"
                    .StartInfo.RedirectStandardError = True
                    .StartInfo.UseShellExecute = False
                    .StartInfo.CreateNoWindow = True
                    .StartInfo.WindowStyle = ProcessWindowStyle.Hidden
                End With
                Dim Arguments As New StringBuilder

                Arguments.Append(" -in " & File.FullName)
                Arguments.Append(" -out " & Me.DBPath.FullName & "\" & File.Name)
                Arguments.Append(" -parse_seqids ")
                If isDNA = True Then
                    Arguments.Append("-dbtype nucl ")
                Else
                    Arguments.Append("-dbtype prot ")
                End If
                If Me.TaxID <> "" Then
                    Arguments.Append("-taxid " & TaxID)
                ElseIf IsNothing(Me.TaxFile) = False Then
                    Arguments.Append("-taxid_map " & Me.TaxFile.FullName)
                End If
                consoleApp.StartInfo.Arguments = Arguments.ToString

                Dim out1 As System.IO.StreamReader

                Try
                    consoleApp.Start()
                    out1 = consoleApp.StandardError
                    Dim alfr = out1.ReadToEnd
                    If alfr <> "" Then

                        MsgBox(alfr)
                    Else

                    End If
                Catch ex As Exception
                    MsgBox(ex.ToString)
                End Try

            Next
        End Sub
    End Class

    Public Class DoBlast
        Private dbFiles As List(Of FileInfo)
        Private queryFiles As List(Of FileInfo)
        Private SelectedProgram As String
        Private OutFmt As Integer
        Private BlastPath As DirectoryInfo
        Private Result As DirectoryInfo
        Private Qulifiers As String
        Public Sub New(queryFiles As List(Of FileInfo),
                       dbFiles As List(Of FileInfo),
                       selectedProgram As String,
                       fmt As Integer,
                       BlastPath As DirectoryInfo,
                       Result As DirectoryInfo, Optional Qulifiers As String = "")
            Me.queryFiles = queryFiles
            Me.dbFiles = dbFiles
            Me.SelectedProgram = selectedProgram
            Me.OutFmt = fmt
            Me.BlastPath = BlastPath
            Me.Result = Result
            Me.Qulifiers = Qulifiers
        End Sub
        Public Sub DoIt()
            Dim log As New System.Text.StringBuilder
            Dim out1 As System.IO.StreamReader
            Dim ResultFiles As New List(Of String)
            For Each Query In queryFiles
                For Each DbFile In dbFiles
                    Dim consoleApp As New Process
                    consoleApp.StartInfo.LoadUserProfile = True
                    With consoleApp
                        .StartInfo.WorkingDirectory = BlastPath.FullName
                        .EnableRaisingEvents = True
                        '
                        .StartInfo.FileName = BlastPath.FullName & "\" & SelectedProgram
                        .StartInfo.RedirectStandardError = True
                        .StartInfo.UseShellExecute = False
                        Dim ResultFile As New FileInfo(Me.Result.FullName & "\" & DbFile.Name.Replace(DbFile.Extension, "") &
                            "_" & Query.Name & ".b" & Me.OutFmt)

                        .StartInfo.Arguments = "-query " & Query.FullName &
                           " -db " & DbFile.FullName.Replace(DbFile.Extension, "") &
                           " -out " & ResultFile.FullName

                        If OutFmt = 7 AndAlso Qulifiers <> "" Then
                            .StartInfo.Arguments = .StartInfo.Arguments & " -outfmt " & Chr(34) & OutFmt & " " & Qulifiers & Chr(34) &
                           " -max_target_seqs 5000  -evalue .001 -num_threads " & Environment.ProcessorCount
                        Else
                            .StartInfo.Arguments = .StartInfo.Arguments & " -outfmt " & OutFmt & " -max_target_seqs 5000  -evalue 0.0001 -num_threads " & Environment.ProcessorCount
                        End If

                        .StartInfo.CreateNoWindow = True
                        .StartInfo.WindowStyle = ProcessWindowStyle.Hidden

                        consoleApp.Start()
                        out1 = consoleApp.StandardError
                        Dim alfr = out1.ReadToEnd
                        If alfr <> "" Then
                            ' MsgBox("Error Blast Database")
                            MsgBox(alfr)
                        Else
                            Beep()
                        End If

                    End With
                Next
            Next
        End Sub
    End Class
    Public Class Retrive
        Public Shared Function GetSeqsFromBlastDatabase(DatabaseFile As FileInfo,
                                                        IDs As List(Of String),
                                                        log As StringBuilder,
                                                 BlastPath As String) As List(Of Bio.ISequence)
            Dim x As New Retrive_It
            Dim Reader = x.GetSeqsFromBlastDatabase(DatabaseFile, IDs, log, BlastPath)

            Dim out As New List(Of Bio.ISequence)
            Dim fa As New Bio.IO.FastA.FastAParser()
            Reader.Position = 0
            Try
                For Each Seq In fa.Parse(Reader)
                    Seq.ID = Seq.ID.Replace("lcl|", "").Trim
                    out.Add(Seq)
                Next
            Catch ex As Exception
                MsgBox(log.ToString)
            End Try
            Return out
        End Function

        Private Class Retrive_It
            Public Shared Property Ms As MemoryStream
            Public Shared Property TxtWriter As TextWriter

            Public Shared Property output As New StringBuilder()
            Public Shared Property [error] As New StringBuilder()
            Public Shared Property outputWaitHandle As New AutoResetEvent(False)
            Public Shared Property errorWaitHandle As New AutoResetEvent(False)
            Public Property Finished As Boolean = False
            Public Function GetSeqsFromBlastDatabase(DatabaseFile As FileInfo,
                                                            IDs As List(Of String),
                                                            log As StringBuilder,
                                                     BlastPath As String) As Stream

                output.Length = 0
                [error].Length = 0

                Dim Arguments As New StringBuilder
                Dim tmpFile As New FileInfo(BlastPath & "2.tmp")
                If tmpFile.Exists = True Then tmpFile.Delete()
                Szunyi.IO.Export.Text(IDs.GetText(vbCrLf), tmpFile)

                Arguments.Append("-db " & DatabaseFile.FullName)
                Arguments.Append(" -entry_batch " & tmpFile.FullName)
                Arguments.Append(" -outfmt %f ") ' As Fasta File


                Using consoleApp As New Process

                    With consoleApp
                        .EnableRaisingEvents = True
                        .StartInfo.FileName = BlastPath & "\blastdbcmd.exe"
                        .StartInfo.RedirectStandardError = True
                        .StartInfo.RedirectStandardOutput = True
                        .StartInfo.UseShellExecute = False
                        .StartInfo.CreateNoWindow = True
                        .StartInfo.WindowStyle = ProcessWindowStyle.Hidden

                    End With

                    consoleApp.StartInfo.Arguments = Arguments.ToString
                    log.Append("blastdbcmd.exe " & Arguments.ToString)

                    AddHandler consoleApp.OutputDataReceived, AddressOf OutputUpdateReceived
                    AddHandler consoleApp.ErrorDataReceived, AddressOf ErrorDataReceived
                    Using outputWaitHandle As New AutoResetEvent(False)
                        Using errorWaitHandle As New AutoResetEvent(False)

                            consoleApp.Start()

                            consoleApp.BeginOutputReadLine()
                            consoleApp.BeginErrorReadLine()

                            ' Process completed. Check process.ExitCode here.
                            Do
                                If consoleApp.WaitForExit(1000) AndAlso outputWaitHandle.WaitOne(1000) AndAlso errorWaitHandle.WaitOne(1000) Then
                                    ' Timed out.
                                    Dim alf As Int16 = 54
                                Else
                                    If Me.Finished = True Then
                                        Dim ascii As Encoding = Encoding.ASCII
                                        Dim ms As MemoryStream
                                        SyncLock output
                                            Dim k = ascii.GetBytes(output.ToString)
                                            ms = New MemoryStream(k)
                                        End SyncLock
                                        Return ms
                                    End If
                                End If

                            Loop

                        End Using
                    End Using
                End Using


                Return Ms
            End Function

            Public Function GetSeqsFromBlastDatabase(DatabaseFile As FileInfo,
                                                            tmpFile As FileInfo,
                                                            log As StringBuilder,
                                                     BlastPath As DirectoryInfo) As Stream

                output.Length = 0
                [error].Length = 0

                Dim Arguments As New StringBuilder

                Arguments.Append("-db " & DatabaseFile.FullName)
                Arguments.Append(" -entry_batch " & tmpFile.FullName)
                Arguments.Append(" -outfmt %f ") ' As Fasta File


                Using consoleApp As New Process

                    With consoleApp
                        .EnableRaisingEvents = True
                        .StartInfo.FileName = BlastPath.FullName & "\blastdbcmd.exe"
                        .StartInfo.RedirectStandardError = True
                        .StartInfo.RedirectStandardOutput = True
                        .StartInfo.UseShellExecute = False
                        .StartInfo.CreateNoWindow = True
                        .StartInfo.WindowStyle = ProcessWindowStyle.Hidden

                    End With

                    consoleApp.StartInfo.Arguments = Arguments.ToString
                    log.Append("blastdbcmd.exe " & Arguments.ToString)

                    AddHandler consoleApp.OutputDataReceived, AddressOf OutputUpdateReceived
                    AddHandler consoleApp.ErrorDataReceived, AddressOf ErrorDataReceived
                    Using outputWaitHandle As New AutoResetEvent(False)
                        Using errorWaitHandle As New AutoResetEvent(False)

                            consoleApp.Start()

                            consoleApp.BeginOutputReadLine()
                            consoleApp.BeginErrorReadLine()

                            ' Process completed. Check process.ExitCode here.
                            Do
                                If consoleApp.WaitForExit(1000) AndAlso outputWaitHandle.WaitOne(1000) AndAlso errorWaitHandle.WaitOne(1000) Then
                                    ' Timed out.
                                    Dim alf As Int16 = 54
                                Else
                                    If Me.Finished = True Then
                                        Dim ascii As Encoding = Encoding.ASCII
                                        Dim ms As MemoryStream
                                        SyncLock output
                                            Dim k = ascii.GetBytes(output.ToString)
                                            ms = New MemoryStream(k)
                                        End SyncLock
                                        Return ms
                                    End If
                                End If

                            Loop

                        End Using
                    End Using
                End Using


                Return Ms
            End Function

            Private Sub ErrorDataReceived(sender As Object, e As DataReceivedEventArgs)
                If e.Data Is Nothing Then
                    errorWaitHandle.[Set]()
                Else
                    [error].AppendLine(e.Data)
                End If
            End Sub

            Private Sub OutputUpdateReceived(sender As Object, e As DataReceivedEventArgs)
                If e.Data Is Nothing Then
                    Me.Finished = True
                    outputWaitHandle.[Set]()
                Else
                    output.AppendLine(e.Data)
                End If
            End Sub


        End Class
    End Class

End Namespace