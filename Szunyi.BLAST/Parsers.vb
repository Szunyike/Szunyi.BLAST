Imports System.Globalization
Imports System.IO
Imports System.Text
Imports System.Xml
Imports Bio.Extensions
Imports Bio.IO
Imports Bio.Web.Blast
Imports Szunyi.Common.Extensions
Imports Szunyi.IO.Extensions

Namespace Parsers
    Public Class OwnBlastXmlParser
        Implements IBlastParser
        Public Shared Function Get_Statistic(F As FileInfo)
            Dim settings = New XmlReaderSettings With {
                    .DtdProcessing = DtdProcessing.Ignore
                }
            Dim curStatistics As BlastStatistics = Nothing
            Dim curElement As String = String.Empty

            Using r As XmlReader = XmlReader.Create(F.FullName, settings)
                While r.Read()
                    Select Case r.NodeType
                        Case XmlNodeType.Element
                            curElement = r.Name
                            Select Case curElement
                                Case "Statistics"
                                    curStatistics = New BlastStatistics()
                            End Select

                        Case XmlNodeType.Text
                            If curElement.StartsWith("Statistics_", StringComparison.OrdinalIgnoreCase) Then
                                DoStatistics(curElement, r.Value, curStatistics)

                            End If

                        Case XmlNodeType.XmlDeclaration, XmlNodeType.ProcessingInstruction, XmlNodeType.Comment
                        Case XmlNodeType.EndElement


                            If r.Name = "Statistics" Then
                                r.Close()
                                Exit While
                            End If
                    End Select

                End While
            End Using

            Return curStatistics
        End Function

        Public Shared Function Get_Metadata(F As FileInfo)
            Dim settings = New XmlReaderSettings With {
                    .DtdProcessing = DtdProcessing.Ignore
                }
            Dim curMetadata As BlastXmlMetadata = Nothing
            Dim curElement As String = String.Empty

            Using r As XmlReader = XmlReader.Create(F.FullName, settings)
                While r.Read()
                    Select Case r.NodeType
                        Case XmlNodeType.Element
                            curElement = r.Name
                            Select Case curElement
                                Case "BlastOutput"
                                    curMetadata = New BlastXmlMetadata()
                            End Select

                        Case XmlNodeType.Text
                            If curElement.StartsWith("BlastOutput_", StringComparison.OrdinalIgnoreCase) Then
                                DoBlastOutput(curElement, r.Value, curMetadata)
                            ElseIf curElement.StartsWith("Parameters_", StringComparison.OrdinalIgnoreCase) Then
                                DoParameters(curElement, r.Value, curMetadata)
                            End If


                        Case XmlNodeType.XmlDeclaration, XmlNodeType.ProcessingInstruction, XmlNodeType.Comment
                        Case XmlNodeType.EndElement


                            If r.Name = "BlastOutput" Then
                                r.Close()
                                Exit While
                            End If
                    End Select

                End While
            End Using

            Return curMetadata
        End Function
        Private Shared Sub DoBlastOutput(ByVal element As String, ByVal value As String, ByVal metadata As BlastXmlMetadata)
            Select Case element
                Case "BlastOutput_program"
                    metadata.Program = value
                Case "BlastOutput_version"
                    metadata.Version = value
                Case "BlastOutput_reference"
                    metadata.Reference = value
                Case "BlastOutput_db"
                    metadata.Database = value
                Case "BlastOutput_query-ID"
                    metadata.QueryId = value
                Case "BlastOutput_query-def"
                    metadata.QueryDefinition = value
                Case "BlastOutput_query-len"
                    metadata.QueryLength = Integer.Parse(value, CultureInfo.InvariantCulture)
                Case "BlastOutput_query-seq"
                    metadata.QuerySequence = value
                Case Else

            End Select
        End Sub

        Private Shared Sub DoParameters(ByVal element As String, ByVal value As String, ByVal metadata As BlastXmlMetadata)
            Select Case element
                Case "Parameters_matrix"
                    metadata.ParameterMatrix = value
                Case "Parameters_expect"
                    metadata.ParameterExpect = Double.Parse(value, CultureInfo.InvariantCulture)
                Case "Parameters_include"
                    metadata.ParameterInclude = Double.Parse(value, CultureInfo.InvariantCulture)
                Case "Parameters_sc-match"
                    metadata.ParameterMatchScore = Integer.Parse(value, CultureInfo.InvariantCulture)
                Case "Parameters_sc-mismatch"
                    metadata.ParameterMismatchScore = Integer.Parse(value, CultureInfo.InvariantCulture)
                Case "Parameters_gap-open"
                    metadata.ParameterGapOpen = Integer.Parse(value, CultureInfo.InvariantCulture)
                Case "Parameters_gap-extend"
                    metadata.ParameterGapExtend = Integer.Parse(value, CultureInfo.InvariantCulture)
                Case "Parameters_filter"
                    metadata.ParameterFilter = value
                Case "Parameters_pattern"
                    metadata.ParameterPattern = value
                Case "Parameters_entrez-query"
                    metadata.ParameterEntrezQuery = value
                Case Else

            End Select
        End Sub

        Private Shared Sub DoIteration(ByVal element As String, ByVal value As String, ByVal curRecord As BlastSearchRecord)
            Select Case element
                Case "Iteration_iter-num"
                    curRecord.IterationNumber = Integer.Parse(value, CultureInfo.InvariantCulture)
                Case "Iteration_query-ID"
                    curRecord.IterationQueryId = value
                Case "Iteration_query-def"
                    curRecord.IterationQueryDefinition = value
                Case "Iteration_query-len"
                    curRecord.IterationQueryLength = Integer.Parse(value, CultureInfo.InvariantCulture)
                Case "Iteration_hits"
                Case "Iteration_stat"
                Case "Iteration_message"
                    curRecord.IterationMessage = value
                Case Else

            End Select
        End Sub

        Private Shared Sub DoHit(ByVal element As String, ByVal value As String, ByVal curHit As Bio.Web.Blast.Hit)
            Select Case element
                Case "Hit_num"
                Case "Hit_id"
                    curHit.Id = value
                Case "Hit_def"
                    curHit.Def = value
                Case "Hit_accession"
                    curHit.Accession = value
                Case "Hit_len"
                    curHit.Length = Long.Parse(value, CultureInfo.InvariantCulture)
                Case "Hit_hsps"
                Case Else

            End Select
        End Sub

        Private Shared Sub DoHsp(ByVal element As String, ByVal value As String, ByVal hsp As Hsp)
            Select Case element
                Case "Hsp_num"
                Case "Hsp_bit-score"
                    hsp.BitScore = Double.Parse(value, CultureInfo.InvariantCulture)
                Case "Hsp_score"
                    hsp.Score = Double.Parse(value, CultureInfo.InvariantCulture)
                Case "Hsp_evalue"
                    hsp.EValue = Double.Parse(value, CultureInfo.InvariantCulture)
                Case "Hsp_query-from"
                    hsp.QueryStart = Long.Parse(value, CultureInfo.InvariantCulture)
                Case "Hsp_query-to"
                    hsp.QueryEnd = Long.Parse(value, CultureInfo.InvariantCulture)
                Case "Hsp_hit-from"
                    hsp.HitStart = Long.Parse(value, CultureInfo.InvariantCulture)
                Case "Hsp_hit-to"
                    hsp.HitEnd = Long.Parse(value, CultureInfo.InvariantCulture)
                Case "Hsp_query-frame"
                    hsp.QueryFrame = Long.Parse(value, CultureInfo.InvariantCulture)
                Case "Hsp_hit-frame"
                    hsp.HitFrame = Long.Parse(value, CultureInfo.InvariantCulture)
                Case "Hsp_identity"
                    hsp.IdentitiesCount = Long.Parse(value, CultureInfo.InvariantCulture)
                Case "Hsp_positive"
                    hsp.PositivesCount = Long.Parse(value, CultureInfo.InvariantCulture)
                Case "Hsp_align-len"
                    hsp.AlignmentLength = Long.Parse(value, CultureInfo.InvariantCulture)
                Case "Hsp_density"
                    hsp.Density = Integer.Parse(value, CultureInfo.InvariantCulture)
                Case "Hsp_qseq"
                    hsp.QuerySequence = value
                Case "Hsp_hseq"
                    hsp.HitSequence = value
                Case "Hsp_midline"
                    hsp.Midline = value
                Case "Hsp_pattern-from"
                    hsp.PatternFrom = Integer.Parse(value, CultureInfo.InvariantCulture)
                Case "Hsp_pattern-to"
                    hsp.PatternTo = Integer.Parse(value, CultureInfo.InvariantCulture)
                Case "Hsp_gaps"
                    hsp.Gaps = Integer.Parse(value, CultureInfo.InvariantCulture)
                Case Else

            End Select
        End Sub

        Public Shared Sub DoStatistics(ByVal element As String, ByVal value As String, ByVal curStats As BlastStatistics)
            Select Case element
                Case "Statistics_db-num"
                Case "Statistics_db-len"
                    curStats.DatabaseLength = Long.Parse(value, CultureInfo.InvariantCulture)
                Case "Statistics_hsp-len"
                    curStats.HspLength = Long.Parse(value, CultureInfo.InvariantCulture)
                Case "Statistics_eff-space"
                    curStats.EffectiveSearchSpace = Double.Parse(value, CultureInfo.InvariantCulture)
                Case "Statistics_kappa"
                    curStats.Kappa = Double.Parse(value, CultureInfo.InvariantCulture)
                Case "Statistics_lambda"
                    curStats.Lambda = Double.Parse(value, CultureInfo.InvariantCulture)
                Case "Statistics_entropy"
                    curStats.Entropy = Double.Parse(value, CultureInfo.InvariantCulture)
                Case Else

            End Select
        End Sub

        Public Shared Function ParseXML(ByVal doc As StringBuilder) As BlastResult
            Dim result As BlastResult = New BlastResult()

            Try
                Dim settings = New XmlReaderSettings With {
                    .DtdProcessing = DtdProcessing.Ignore
                }
                Dim sr As StringReader = Nothing

                Try
                    sr = New StringReader(doc.ToString())

                    Using r As XmlReader = XmlReader.Create(sr, settings)
                        Dim curElement As String = String.Empty
                        Dim curRecord As BlastSearchRecord = New BlastSearchRecord()
                        Dim curHit As Bio.Web.Blast.Hit = Nothing
                        Dim curHsp As Bio.Web.Blast.Hsp = Nothing
                        Dim curStatistics As BlastStatistics = Nothing
                        Dim curMetadata As BlastXmlMetadata = Nothing

                        While r.Read()

                            Select Case r.NodeType
                                Case XmlNodeType.Element
                                    curElement = r.Name

                                    Select Case curElement
                                        Case "Hit"
                                            curHit = New Bio.Web.Blast.Hit()
                                        Case "Hsp"
                                            curHsp = New Bio.Web.Blast.Hsp()
                                        Case "Statistics"
                                            curStatistics = New BlastStatistics()
                                        Case "BlastOutput"
                                            curMetadata = New BlastXmlMetadata()
                                    End Select

                                Case XmlNodeType.Text

                                    If curElement.StartsWith("BlastOutput_", StringComparison.OrdinalIgnoreCase) Then
                                        DoBlastOutput(curElement, r.Value, curMetadata)
                                    ElseIf curElement.StartsWith("Parameters_", StringComparison.OrdinalIgnoreCase) Then
                                        DoParameters(curElement, r.Value, curMetadata)
                                    ElseIf curElement.StartsWith("Iteration_", StringComparison.OrdinalIgnoreCase) Then
                                        DoIteration(curElement, r.Value, curRecord)
                                    ElseIf curElement.StartsWith("Statistics_", StringComparison.OrdinalIgnoreCase) Then
                                        DoStatistics(curElement, r.Value, curStatistics)
                                    ElseIf curElement.StartsWith("Hit_", StringComparison.OrdinalIgnoreCase) Then
                                        DoHit(curElement, r.Value, curHit)
                                    ElseIf curElement.StartsWith("Hsp_", StringComparison.OrdinalIgnoreCase) Then
                                        DoHsp(curElement, r.Value, curHsp)
                                    End If

                                Case XmlNodeType.XmlDeclaration, XmlNodeType.ProcessingInstruction, XmlNodeType.Comment
                                Case XmlNodeType.EndElement

                                    If r.Name = "Iteration" Then
                                        result.Records.Add(curRecord)
                                        curRecord = New BlastSearchRecord()
                                    ElseIf r.Name = "Statistics" Then
                                        curRecord.Statistics = curStatistics
                                    ElseIf r.Name = "Hit" Then
                                        curRecord.Hits.Add(curHit)
                                    ElseIf r.Name = "Hsp" Then
                                        curHit.Hsps.Add(curHsp)
                                    ElseIf r.Name = "BlastOutput" Then
                                        result.Metadata = curMetadata
                                    End If
                            End Select
                        End While
                    End Using

                Finally
                    If sr IsNot Nothing Then sr.Dispose()
                End Try

            Catch e As Exception

            End Try

            Return result
        End Function
        Public Shared Iterator Function ParseXMLFile(F As FileInfo) As IEnumerable(Of BlastSearchRecord)
            Dim result As BlastResult = New BlastResult()

            Try
                Dim settings = New XmlReaderSettings With {
                    .DtdProcessing = DtdProcessing.Ignore
                }
                Dim sr As StringReader = Nothing

                Try
                    'sr = New StringReader(doc.ToString())

                    Using r As XmlReader = XmlReader.Create(F.FullName, settings)
                        Dim curElement As String = String.Empty
                        Dim curRecord As BlastSearchRecord = New BlastSearchRecord()
                        Dim curHit As Bio.Web.Blast.Hit = Nothing
                        Dim curHsp As Bio.Web.Blast.Hsp = Nothing
                        Dim curStatistics As BlastStatistics = Nothing
                        Dim curMetadata As BlastXmlMetadata = Nothing

                        While r.Read()

                            Select Case r.NodeType
                                Case XmlNodeType.Element
                                    curElement = r.Name

                                    Select Case curElement
                                        Case "Hit"
                                            curHit = New Bio.Web.Blast.Hit()
                                        Case "Hsp"
                                            curHsp = New Hsp()
                                        Case "Statistics"
                                            curStatistics = New BlastStatistics()
                                        Case "BlastOutput"
                                            curMetadata = New BlastXmlMetadata()
                                    End Select

                                Case XmlNodeType.Text

                                    If curElement.StartsWith("BlastOutput_", StringComparison.OrdinalIgnoreCase) Then
                                        DoBlastOutput(curElement, r.Value, curMetadata)
                                    ElseIf curElement.StartsWith("Parameters_", StringComparison.OrdinalIgnoreCase) Then
                                        DoParameters(curElement, r.Value, curMetadata)
                                    ElseIf curElement.StartsWith("Iteration_", StringComparison.OrdinalIgnoreCase) Then
                                        DoIteration(curElement, r.Value, curRecord)
                                    ElseIf curElement.StartsWith("Statistics_", StringComparison.OrdinalIgnoreCase) Then
                                        DoStatistics(curElement, r.Value, curStatistics)
                                    ElseIf curElement.StartsWith("Hit_", StringComparison.OrdinalIgnoreCase) Then
                                        DoHit(curElement, r.Value, curHit)
                                    ElseIf curElement.StartsWith("Hsp_", StringComparison.OrdinalIgnoreCase) Then
                                        DoHsp(curElement, r.Value, curHsp)
                                    End If

                                Case XmlNodeType.XmlDeclaration, XmlNodeType.ProcessingInstruction, XmlNodeType.Comment
                                Case XmlNodeType.EndElement

                                    If r.Name = "Iteration" Then
                                        Yield curRecord.Clone
                                        'result.Records.Add(curRecord)
                                        curRecord = New BlastSearchRecord()
                                    ElseIf r.Name = "Statistics" Then
                                        curRecord.Statistics = curStatistics
                                    ElseIf r.Name = "Hit" Then
                                        curRecord.Hits.Add(curHit)
                                    ElseIf r.Name = "Hsp" Then
                                        curHit.Hsps.Add(curHsp)
                                    ElseIf r.Name = "BlastOutput" Then
                                        result.Metadata = curMetadata
                                    End If
                            End Select
                        End While
                    End Using

                Finally
                    If sr IsNot Nothing Then sr.Dispose()
                End Try

            Catch e As Exception

            End Try


        End Function

        Public Function Parse(ByVal stream As Stream) As IEnumerable(Of BlastResult) Implements IParser(Of BlastResult).Parse
            Dim records = New List(Of BlastResult)()
            Dim sb = New StringBuilder()
            Dim lineNumber As Long = 1

            Using reader = stream.OpenRead()
                Dim line As String = ReadNextLine(reader, False)

                While Not String.IsNullOrEmpty(line)

                    If line.StartsWith("RPS-BLAST", StringComparison.OrdinalIgnoreCase) Then
                        line = ReadNextLine(reader, False)
                        lineNumber += 1
                        Continue While
                    End If

                    If line.StartsWith("<?xml version", StringComparison.OrdinalIgnoreCase) AndAlso lineNumber > 1 Then
                        records.Add(ParseXML(sb))
                        sb = New StringBuilder()
                    End If

                    sb.AppendLine(line)
                    line = ReadNextLine(reader, False)
                    lineNumber += 1
                End While

                If sb.Length > 0 Then records.Add(ParseXML(sb))
                If records.Count = 0 Then

                End If
            End Using

            Return records
        End Function

        Public Function ParseOne(ByVal stream As Stream) As BlastResult Implements IParser(Of BlastResult).ParseOne
            Return Parse(stream).First()
        End Function

        Private Shared Function ReadNextLine(ByVal reader As TextReader, ByVal skipBlankLines As Boolean) As String
            If reader.Peek() = -1 Then
                Return Nothing
            End If

            Dim line As String = reader.ReadLine()

            While skipBlankLines AndAlso String.IsNullOrWhiteSpace(line) AndAlso reader.Peek() <> -1
                line = reader.ReadLine()
            End While

            Return line
        End Function


        Public ReadOnly Property Name As String Implements IParser.Name
            Get
                Return "OwnXMLBLAStParser"
            End Get
        End Property

        Public ReadOnly Property Description As String Implements IParser.Description
            Get
                Return "OwnXMLBLAStParser"
            End Get
        End Property

        Public ReadOnly Property SupportedFileTypes As String Implements IParser.SupportedFileTypes
            Get
                Return String.Empty
            End Get
        End Property


    End Class

    Public Class b6
        Implements IBlastParser

        Public ReadOnly Property Name As String Implements IParser.Name
            Get
                Return "b6"
            End Get
        End Property

        Public ReadOnly Property Description As String Implements IParser.Description
            Get
                Return "Default tab delemited wo Header"
            End Get
        End Property

        Public ReadOnly Property SupportedFileTypes As String Implements IParser.SupportedFileTypes
            Get
                Throw New NotImplementedException()
            End Get
        End Property

        Public Function Parse(stream As Stream) As IEnumerable(Of BlastResult) Implements IParser(Of BlastResult).Parse
            Throw New NotImplementedException()
        End Function
        Public Shared Iterator Function Parse(File As FileInfo) As IEnumerable(Of BlastResult)
            For Each Line In File.Parse_Lines
                Dim x As New Hsp
                ' 'qaccver saccver pident length mismatch gapopen qstart qend sstart send
                ' evalue bitscore
                Dim b = Split(Line, vbTab)
                Dim _hsp As New Hsp

            Next
        End Function
        Public Function ParseOne(stream As Stream) As BlastResult Implements IParser(Of BlastResult).ParseOne
            Throw New NotImplementedException()
        End Function
    End Class

    Public Class b7
        Implements IBlastParser

        Public ReadOnly Property Name As String Implements IParser.Name
            Get
                Return "b7"
            End Get
        End Property

        Public ReadOnly Property Description As String Implements IParser.Description
            Get
                Return "Default tab delemited w Header"
            End Get
        End Property

        Public ReadOnly Property SupportedFileTypes As String Implements IParser.SupportedFileTypes
            Get
                Throw New NotImplementedException()
            End Get
        End Property
        Public Shared Iterator Function Parse(File As FileInfo, Compressed As Boolean) As IEnumerable(Of OwnBlastRecord)
            If Compressed = False Then
                For Each Item In Parse(File)
                    Yield Item
                Next
            Else
                For Each Item In Parse_Compressed_HSP(File)
                    ' Yield Item
                Next
            End If
        End Function
        Public Shared Iterator Function ParseHsp(File As FileInfo) As IEnumerable(Of OwnHsp)
            Dim Fields = Split(File.Parse_First_Line_StartWith("# Fields:"), ":").Last.Split(",").ToList
            Fields = Fields.Trim(" ")
            Dim d = Oualifiers.Get_Indexed_Properties(Fields.ToList)
            Dim Headers = File.Parse_First_LineS_StartWith("#")
            Dim DB = (From x In Headers Where x.StartsWith("# Database: ") = True).First
            Dim cDB = Split(DB, "# Database: ").Last
            For Each Lines In File.Parse_Lines_by_Group("#") ' Szunyi.IO.Import.Text.Parse_Group_Lines(File, "#")
                ' 1 Record, Several Hits, Several Hsps
                Dim NewRecord As New OwnBlastRecord
                Dim oHsps As New List(Of OwnHsp)
                For Each Line In Lines
                    Dim s = Split(Line, vbTab)
                    Dim oriHSp As New Szunyi.BLAST.OwnHsp(s, d)
                    oriHSp.File = File
                    Yield oriHSp
                Next
            Next

        End Function

        Public Shared Iterator Function Parse(File As FileInfo) As IEnumerable(Of OwnBlastRecord)
            Dim Fields = Split(File.Parse_First_Line_StartWith("# Fields:"), ":").Last.Split(",").ToList
            Fields = Fields.Trim(" ").ToList
            Dim d = Oualifiers.Get_Indexed_Properties(Fields.ToList)

            Dim Headers = File.Parse_First_LineS_StartWith("#")
            Dim DB = (From x In Headers Where x.StartsWith("# Database: ") = True).First
            Dim cDB = Split(DB, "# Database: ").Last
            For Each Lines In File.Parse_Lines_by_Group("#") ' Szunyi.IO.Import.Text.Parse_Group_Lines(File, "#")
                ' 1 Record, Several Hits, Several Hsps
                Dim NewRecord As New OwnBlastRecord
                Dim oHsps As New List(Of OwnHsp)
                For Each Line In Lines
                    Dim s = Split(Line, vbTab)
                    Dim oriHSp As New Szunyi.BLAST.OwnHsp(s, d)
                    oriHSp.File = File
                    oHsps.Add(oriHSp)
                    NewRecord.IterationQueryDefinition = oriHSp.Record_IterationQueryDefinition
                    NewRecord.IterationQueryLength = oriHSp.Record_IterationQueryLength
                Next
                For Each gHits In From x In oHsps Group By x.HitID Into Group
                    Dim t As New Bio.Web.Blast.Hit
                    t.Id = gHits.Group.First.HitID
                    t.Def = gHits.Group.First.Hit_Accession
                    t.Length = gHits.Group.First.Hit_Length
                    Dim cHSP = From d1 In gHits.Group Select d1.HSP

                    t.Hsps.AddRange(cHSP)

                    Dim jj As New OwnHit(t)
                    For Each Item In gHits.Group
                        jj.OwnHsps.Add(Item)
                    Next
                    jj.Length = oHsps.First.Hit_Length
                    jj.DB_File_Name = cDB
                    NewRecord.Hits.Add(t)
                    NewRecord.Hit_TaxID = oHsps.First.TaxID
                    NewRecord.OwnHits.Add(jj)
                Next
                If NewRecord.Hits.Count <> NewRecord.OwnHits.Count Then
                    Dim kjh As Int16 = 54
                End If
                For i1 = 0 To NewRecord.Hits.Count - 1
                    If NewRecord.Hits(i1).Hsps.Count <> NewRecord.OwnHits(i1).OwnHsps.Count Then
                        Dim kj As Int16 = 54
                    End If
                Next
                If IsNothing(NewRecord.IterationQueryDefinition) = False Then
                    Yield NewRecord
                Else
                    Dim jk As Int16 = 54
                End If


            Next

        End Function
        Public Shared Iterator Function Parse_Compressed_HSP(File As FileInfo) As IEnumerable(Of OwnHsp)
            Dim Fields = Split(File.Parse_First_Line_StartWith("# Fields:"), ":").Last.Split(",").ToList
            Fields = Fields.Trim(" ")
            Dim d = Oualifiers.Get_Indexed_Properties(Fields.ToList)
            For Each Lines In File.Parse_Lines_by_Group("#")
                ' 1 Record, Several Hits, Several Hsps
                Dim NewRecord As New BlastSearchRecord
                Dim oHsps As New List(Of OwnHsp)
                For Each Line In Lines
                    Dim s = Split(Line, vbTab)
                    Dim oriHSp As New Szunyi.BLAST.OwnHsp(s, d)
                    Yield oriHSp
                Next


            Next

        End Function

        Public Function ParseOne(stream As Stream) As BlastResult Implements IParser(Of BlastResult).ParseOne
            Dim x As New BlastSearchRecord
            Dim x1 As New Bio.Web.Blast.Hsp

        End Function

        Public Function Parse(stream As Stream) As IEnumerable(Of BlastResult) Implements IParser(Of BlastResult).Parse
            Throw New NotImplementedException()
        End Function
    End Class

    Public Class Oualifiers
        Public Shared Function Get_Indexed_Properties(Fields As List(Of String)) As Dictionary(Of Integer, String)
            Dim d As New Dictionary(Of Integer, String)
            For i1 = 0 To Fields.Count - 1
                Dim f = Fields(i1)
                Select Case f
                    Case "query seq"
                        d.Add(i1, "Hsp.QuerySeq")
                    Case "subject seq"
                        d.Add(i1, "Hsp.HitSeq")
                    Case "query id"
                        d.Add(i1, "Record.IterationQueryId")
                    Case "query gi"
                        d.Add(i1, "Record.IterationQueryGI")
                    Case "query acc."
                        d.Add(i1, "Record.IterationQueryDefinition")
                    Case "subject id"
                        d.Add(i1, "Hit.Id")
                    Case "subject length"
                        d.Add(i1, "Hit.Length")
                    Case "sallseqid"

                    Case "subject gi"
                        d.Add(i1, "Hit.GI")
                    Case "sallgi"
                    Case "alignment length"
                        d.Add(i1, "Hsp.AlignmentLength")
                    Case "subject acc."
                        d.Add(i1, "Hit.Accession")
                    Case "sallacc"
                    Case "s. start"
                        d.Add(i1, "Hsp.HitStart")
                    Case "s. end"
                        d.Add(i1, "Hsp.HitEnd")
                    Case "q. start"
                        d.Add(i1, "Hsp.QueryStart")
                    Case "q. end"
                        d.Add(i1, "Hsp.QueryEnd")
                    Case "sstart"
                        d.Add(i1, "Hsp.HitStart")
                    Case "send"
                        d.Add(i1, "Hsp.HitEnd")
                    Case "qseq"
                        d.Add(i1, "Hsp.QuerySequence")
                    Case "sseq"
                        d.Add(i1, "Hsp.HitSequence")
                    Case "evalue"
                        d.Add(i1, "Hsp.Evalue")
                    Case "bit score"

                        d.Add(i1, "Hsp.BitScore")
                    Case "score"
                        d.Add(i1, "Hsp.Score")
                    Case "length"
                        d.Add(i1, "Hsp.AlignmentLength")
                    Case "% identity"
                        d.Add(i1, "Hsp.Identity_Percent")
                    Case "identical"
                        d.Add(i1, "Hsp.IdentitiesCount")
                    Case "mismatches"
                        d.Add(i1, "Hsp.MisMatch")
                    Case "positives"
                        d.Add(i1, "Hsp.PositivesCount")
                    Case "gap opens"
                        d.Add(i1, "Hsp.GapOpens")
                    Case "gaps"
                        d.Add(i1, "Hsp.Gaps")
                    Case "% positives"
                        d.Add(i1, "Hsp.Positives_Percent")
                    Case "frames"
                        d.Add(i1, "Hsp.Evalue")
                    Case "query frame"
                        d.Add(i1, "Hsp.QueryFrame")
                    Case "sbjct frame"
                        d.Add(i1, "Hsp.HitFrame")
                    Case "BTOP"
                        d.Add(i1, "Hsp.Evalue")
                    Case "subject tax ids"
                        d.Add(i1, "staxids")
                    Case "subject sci names"
                        d.Add(i1, "sscinames")
                    Case "subject com names"
                        d.Add(i1, "scomnames")
                    Case "subject blast names"
                        d.Add(i1, "sblastnames")
                    Case " subject super kingdoms"
                        d.Add(i1, "sskingdoms")
                    Case "stitle"
                        d.Add(i1, "stitle")
                    Case "salltitles"
                        d.Add(i1, "salltitles")
                    Case "sstrand"
                        d.Add(i1, "Hsp.Evalue")
                    Case "qcovs"
                        d.Add(i1, "qcovs")
                    Case "qcovhsp"
                        d.Add(i1, "qcovhsp")
                    Case "qcovus"
                        d.Add(i1, "qcovus")
                    Case "subject title"
                        d.Add(i1, "Hit.Accession")
                    Case "query length"
                        d.Add(i1, "Record.ItarationQueryLength")
                    Case Else
                        Dim kj As Int16 = 54

                End Select
            Next
            Return d
        End Function
    End Class
End Namespace

