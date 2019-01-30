Imports System.ComponentModel
Imports System.IO
Imports System.Runtime.CompilerServices
Imports Bio.Web.Blast
Imports Szunyi.IO.Extensions

Namespace Enums
    Public Enum Hsp_Hit_Aggregate
        Record_Hit_Length = 0
        Record_Hit_HSP_AlignmentLength = 1
        Record_Hit_HSP_IdentitiesCount = 2
        Record_Hit_HSP_PositivesCount = 3
        Record_Hit_HSP_Gaps = 4
        Hit_HSP_AlignmentLength = 5
        Hit_HSP_IdentitiesCount = 6
        Hit_HSP_PositivesCount = 7
        Hit_HSP_Gaps = 8
    End Enum
    Public Enum Hsp_Hit_Record_Numeric
        Hsp_AlignmentLength = 0
        Hsp_BitScore = 1
        Hsp_Density = 2
        Hsp_Evalue = 3
        Hsp_Gaps = 4
        Hsp_HitEnd = 5
        Hsp_HitStart = 6
        Hsp_PositivesCount = 9
        Hsp_IdentitiesCount = 7
        Hsp_QueryStart = 10
        Hsp_QueryEnd = 11
        Hsp_QueryFrame = 12
        Hsp_Score = 14
        Hit_Length = 23
        Record_IterationQueryLength = 35
    End Enum
    Public Enum Hsp
        AlignmentLength = 0
        BitScore = 1
        Density = 2
        Evalue = 3
        Gaps = 4
        HitEnd = 5
        HitStart = 6
        IdentitiesCount = 7
        PositivesCount = 9
        QueryStart = 10
        QueryEnd = 11
        QueryFrame = 12
        HitFrame = 13
        QuerySequence = 14
        Midline = 15
        HitSequence = 16
        Score = 17
    End Enum
    Public Enum Hit
        Accession = 20
        Def = 21
        ID = 22
        Length = 23
        '      Hsps_Count = 4
    End Enum
    Public Enum Record
        IterationMessage = 30
        IterationNumber = 31
        IterationQueryDefinition = 32
        IterationQueryId = 33
        '   Hit_Counts = 4
        IterationQueryLength = 35
    End Enum

End Namespace

Public Class Helper
    Dim FileTypes As New Dictionary(Of String, Integer)
    Dim Qulifiers As New Dictionary(Of String, String)
    Public Sub New()
        FileTypes.Add("Pairwise", 0)
        FileTypes.Add("Query - anchoreFIleTypes showing iFIleTypesentities", 1)
        FileTypes.Add("Query-anchoreFIleTypes no iFIleTypesentities", 2)
        FileTypes.Add("Flat query-anchoreFIleTypes showing iFIleTypesentities", 3)
        FileTypes.Add("Flat query-anchoreFIleTypes no iFIleTypesentities", 4)
        FileTypes.Add("BLAST XML", 5)
        FileTypes.Add("Tabular", 6)
        FileTypes.Add("Tabular with comment lines", 7)
        FileTypes.Add("Seqalign (Text ASN.1)", 8)
        FileTypes.Add("Seqalign (Binary ASN.1)", 9)
        FileTypes.Add("Comma-separateFIleTypes values", 10)
        FileTypes.Add("BLAST archive (ASN.1)", 11)
        FileTypes.Add("Seqalign (JSON)", 12)
        FileTypes.Add("Multiple-file BLAST JSON", 13)
        FileTypes.Add("Multiple-file BLAST XML2", 14)
        FileTypes.Add("Single-file BLAST JSON", 15)
        FileTypes.Add("Single-file BLAST XML2", 16)
        FileTypes.Add("Sequence Alignment/Map (SAM)", 17)
        FileTypes.Add("Organism Report", 18)

        Qulifiers.Add("qacc", "Query accesion")
        Qulifiers.Add("qaccver", "Query accesion.version")
        Qulifiers.Add("qlen", "Query sequence length")
        Qulifiers.Add("sseqid", "Subject Seq-id")
        Qulifiers.Add("sallseqid", "All subject Seq-id(s), separated by a ';'")
        Qulifiers.Add("sgi", "Subject GI")
        Qulifiers.Add("sallgi", "All subject GIs")
        Qulifiers.Add("sacc", "Subject accession")
        Qulifiers.Add("saccver", "Subject accession.version")
        Qulifiers.Add("sallacc", "All subject accessions")
        Qulifiers.Add("slen", "Subject sequence length")
        Qulifiers.Add("qstart", "Start of alignment in query")
        Qulifiers.Add("qend", "End of alignment in query")
        Qulifiers.Add("sstart", "Start of alignment in subject")
        Qulifiers.Add("send", "End of alignment in subject")
        Qulifiers.Add("qseq", "Aligned part of query sequence")
        Qulifiers.Add("sseq", "Aligned part of subject sequence")
        Qulifiers.Add("evalue", "Expect value")
        Qulifiers.Add("bitscore", "Bit score")
        Qulifiers.Add("score", "Raw score")
        Qulifiers.Add("length", "Alignment length")
        Qulifiers.Add("pident", "Percentage of identical matches")
        Qulifiers.Add("nident", "Number of identical matches")
        Qulifiers.Add("mismatch", "Number of mismatches")
        Qulifiers.Add("positive", "Number of positive-scoring matches")
        Qulifiers.Add("gapopen", "Number of gap openings")
        Qulifiers.Add("gaps", "Total number of gaps")
        Qulifiers.Add("ppos", "Percentage of positive-scoring matches")
        Qulifiers.Add("frames", "Query and subject frames separated by a '/'")
        Qulifiers.Add("qframe", "Query frame")
        Qulifiers.Add("sframe", "Subject frame")
        Qulifiers.Add("btop", "Blast traceback operations (BTOP)")
        Qulifiers.Add("staxid", "Subject Taxonomy ID")
        Qulifiers.Add("ssciname", "Subject Scientific Name")
        Qulifiers.Add("scomname", "Subject Common Name")
        Qulifiers.Add("sblastname", "Subject Blast Name")
        Qulifiers.Add("sskingdom", "Subject Super Kingdom")
        Qulifiers.Add("staxids", "unique Subject Taxonomy ID(s), separated by a ';' (in numerical order)")
        Qulifiers.Add("sscinames", "unique Subject Scientific Name(s), separated by a ';'")
        Qulifiers.Add("scomnames", "unique Subject Common Name(s), separated by a ';'")
        Qulifiers.Add("sblastnames", "unique Subject Blast Name(s), separated by a ';' (in alphabetical order)")
        Qulifiers.Add("sskingdoms", "unique Subject Super Kingdom(s), separated by a ';' (in alphabetical order)")
        Qulifiers.Add("stitle", "Subject Title")
        Qulifiers.Add("salltitles", "All Subject Title(s), separated by a '<>'")
        Qulifiers.Add("sstrand", "Subject Strand")
        Qulifiers.Add("qcovs", "Query Coverage Per Subject")
        Qulifiers.Add("qcovhsp", "Query Coverage Per HSP")
        Qulifiers.Add("qcovus", "Query Coverage Per Unique Subject (blastn only)")

    End Sub
    Public Function Get_FileTypes() As List(Of String)
        Return (From x In Me.FileTypes Select x.Key).ToList
    End Function
    Public Function Get_outfmt_Value(x As String) As Integer
        Dim x1 = From t In Me.FileTypes Where t.Key = x
        If x1.Count = 1 Then
            Return x1.First.Value
        End If
    End Function
#Region "BgWork"
    Private Sub CreateBgWork(Type As String, t As Object)
        Dim w = New BackgroundWorker
        w.WorkerReportsProgress = True
        w.WorkerSupportsCancellation = True
        AddHandler w.DoWork, AddressOf WorkerDoWork
        AddHandler w.ProgressChanged, AddressOf WorkerProgressChanged
        AddHandler w.RunWorkerCompleted, AddressOf WorkerCompleted

        w.RunWorkerAsync(t)

    End Sub
    Private Sub WorkerCompleted(sender As Object, e As RunWorkerCompletedEventArgs)

    End Sub

    Private Sub WorkerProgressChanged(sender As Object, e As ProgressChangedEventArgs)
        'Throw New NotImplementedException()
    End Sub

    Private Sub WorkerDoWork(sender As Object, e As DoWorkEventArgs)

        e.Result = e.Argument
        Try
            e.Argument.DoIt
        Catch ex As Exception
            Dim alf As Int16 = 43

        End Try


    End Sub
#End Region
End Class

Public Class extHSP
    Public Property RecordID As Integer
    Public Property HitID As Integer
    Public Property HSPID As Integer
    Public Property Hsp As Hsp
    Public Property Hit As Bio.Web.Blast.Hit
    Public Property Record As BlastSearchRecord

    Public Property Hit_TaxID As String
    Public ReadOnly Query_TaxID As String
    Public Sub New(hsp As Hsp, hit As Bio.Web.Blast.Hit, record As BlastSearchRecord)
        Me.Hsp = hsp
        Me.Hit = hit
        Me.Record = record

    End Sub
    Public Sub New(hsp As Hsp, hit As Bio.Web.Blast.Hit, record As BlastSearchRecord, hspID As Integer, hitID As Integer, recordID As Integer)
        Me.Hsp = hsp
        Me.Hit = hit
        Me.Record = record
        Me.HSPID = hspID
        Me.HitID = hitID
        Me.RecordID = recordID
    End Sub
    Public Sub New()

    End Sub

    Public Shadows Function Get_Hsp_Value(tmp As String)
        Dim c = tmp.Substring(4)

        Select Case c
            Case "AlignmentLength"
                Return Hsp.AlignmentLength

            Case "BitScore"
                Return Hsp.BitScore

            Case "Density"
                Return Hsp.Density

            Case "EValue"
                Return Hsp.EValue


        End Select
        Dim Value = Szunyi.Common.Util_Helpers.Get_Property_Value(Me.Hsp, c)
        Return Value
    End Function
    Public Shadows Function Get_Hit_Value(tmp As String)
        Dim c = tmp.Substring(4)
        Select Case c
            Case "Length"
                Return Hit.Length
        End Select
        Dim Value = Szunyi.Common.Util_Helpers.Get_Property_Value(Me.Hit, c)
        Return Value
    End Function
    Public Shadows Function Get_Record_Value(tmp As String)
        Dim c = tmp.Substring(7)
        Select Case c
            Case "IterationQueryLength"
                Return Record.IterationQueryLength
        End Select
        Dim Value = Szunyi.Common.Util_Helpers.Get_Property_Value(Me.Record, c)
        Return Value
    End Function
    Public Shared Function Get_Values_By_Aggregate_Name(extHsps As List(Of extHSP), Prop_Name As String, maxNof As Integer) As List(Of Double)
        Dim s = Split(Prop_Name, " ")
        Dim x = Szunyi.Math.mxparser.Get_Blast_Arguments(s)
        Dim vals = Get_Agg_Values_By_Prop_Name(extHsps, s.Last, maxNof)
        Dim Values As New List(Of Double)
        If Prop_Name.Contains("Sum") Then
            For Each Item In vals
                Values.Add(Item.Sum)
            Next

        ElseIf Prop_Name.Contains("Max") Then
            For Each Item In vals
                Values.Add(Item.Max)
            Next
        ElseIf Prop_Name.Contains("Min") Then
            For Each Item In vals
                Values.Add(Item.Min)
            Next
        ElseIf Prop_Name.Contains("Aver") Then
            For Each Item In vals
                Values.Add(Item.Sum / Item.Count)
            Next
        End If
        Return Values
    End Function
    Public Shared Function Get_Agg_Values_By_Prop_Name(extHsps As List(Of extHSP), Prop_Name As String, maxNof As Integer) As List(Of Double())
        Dim Values2 As New List(Of Double())
        Prop_Name = Prop_Name.Replace(" ", "").Replace("[", "").Replace("]", "")
        Dim Index As Integer = 0
        If Prop_Name.StartsWith("Record_Hit_HSP_") Then
            Dim c = Prop_Name.Substring(15)

            For Each Item In extHsps
                Dim HSPs = Szunyi.BLAST.BlastManipulation.Hsp.All(Item.Record)
                Dim x As New List(Of Double)
                For Each H In HSPs
                    x.Add(Szunyi.Common.Util_Helpers.Get_Property_Value(H.Hsp, c))
                Next
                Values2.Add(x.ToArray)
                Index += 1
                If Index = maxNof Then Return Values2
            Next


        ElseIf Prop_Name.StartsWith("Record_Hit_") Then
            Dim c = Prop_Name.Substring(12)
            For Each Item In extHsps
                Dim x As New List(Of Double)
                For Each H In Item.Record.Hits
                    x.Add(Szunyi.Common.Util_Helpers.Get_Property_Value(H, c))
                Next
                Values2.Add(x.ToArray)
                Index += 1
                If Index = maxNof Then Return Values2
            Next

        ElseIf Prop_Name.StartsWith("Hit_HSP_") Then
            Dim c = Prop_Name.Substring(8)
            For Each Item In extHsps
                Dim x As New List(Of Double)
                For Each H In Item.Hit.Hsps
                    x.Add(Szunyi.Common.Util_Helpers.Get_Property_Value(H, c))
                Next
                Values2.Add(x.ToArray)
                Index += 1
                If Index = maxNof Then Return Values2

            Next
        Else
            Dim jk As Int16 = 54
        End If
        Return Values2
    End Function
    Public Shared Function Get_Values_By_Prop_Name(extHsps As List(Of extHSP), Prop_Name As String, MaxNof As Integer) As List(Of Double)
        Dim Values As New List(Of Double)
        Prop_Name = Prop_Name.Replace(" ", "").Replace("[", "").Replace("]", "")
        If Prop_Name.StartsWith("Record_Hit_HSP_") Then
            Dim c = Prop_Name.Substring(15)
            Dim Index As Integer = 0
            For Each Item In extHsps
                Dim HSPs = Szunyi.BLAST.BlastManipulation.Hsp.All(Item.Record)
                Dim x As New List(Of Double)
                For Each H In HSPs
                    x.Add(Szunyi.Common.Util_Helpers.Get_Property_Value(H.Hsp, c))
                Next
                Values.Add(x.Sum)
                Index += 1
                If Index = MaxNof Then Return Values
            Next


        ElseIf Prop_Name.StartsWith("Record_Hit_") Then
            Dim c = Prop_Name.Substring(11)
            Dim Index As Integer = 0
            For Each Item In extHsps

                Dim x As New List(Of Double)
                For Each H In Item.Record.Hits
                    x.Add(Szunyi.Common.Util_Helpers.Get_Property_Value(H, c))
                Next
                Values.Add(x.Sum)
                Index += 1
                If Index = MaxNof Then Return Values
            Next

        ElseIf Prop_Name.StartsWith("Hit_HSP_") Then
            Dim c = Prop_Name.Substring(8)
            Dim Index As Integer = 0
            For Each Item In extHsps

                Dim x As New List(Of Double)
                For Each H In Item.Hit.Hsps
                    x.Add(Szunyi.Common.Util_Helpers.Get_Property_Value(H, c))
                Next
                Values.Add(x.Sum)
                Index += 1
                If Index = MaxNof Then Return Values
            Next

        ElseIf Prop_Name.StartsWith("Hsp") Then
            Dim c = Prop_Name.Substring(4)
            Dim Index As Integer = 0
            For Each Item In extHsps
                Values.Add(Szunyi.Common.Util_Helpers.Get_Property_Value(Item.Hsp, c))
                Index += 1
                If Index = MaxNof Then Return Values
            Next
        ElseIf Prop_Name.StartsWith("Hit") Then
            Dim c = Prop_Name.Substring(4)
            Dim Index As Integer = 0
            For Each Item In extHsps
                Values.Add(Szunyi.Common.Util_Helpers.Get_Property_Value(Item.Hit, c))
                Index += 1
                If Index = MaxNof Then Return Values
            Next

        ElseIf Prop_Name.StartsWith("Record") Then
            Dim c = Prop_Name.Substring(7)
            Dim Index As Integer = 0
            For Each Item In extHsps
                Values.Add(Szunyi.Common.Util_Helpers.Get_Property_Value(Item.Record, c))
                Index += 1
                If Index = MaxNof Then Return Values
            Next
        End If
        Return Values
    End Function
End Class

Public Class OwnHsp
    Private line As String
    Private d As Dictionary(Of Integer, String)
    Public Property HSP As Bio.Web.Blast.Hsp
    Public Property RecordID As String
    Public Property HitID As String
    Public Property HitGi As String
    Public Property Hit_Accession As String
    Public Property HSPID As Integer
    Public Property QueryStart As Integer
    Public Property QueryEnd As Integer
    Public Property HitStart As Integer
    Public Property HitEnd As Integer
    Public Property Query_GI As String
    Public Property Subject_GI As String
    Public Property IdentitiesCount As Integer
    Public Property PositiveCOunt As Integer
    Public Property AlignmentLength As Integer
    Public Property Percentage_of_identical_matches As Double
    Public Property Number_of_identical_matches As Integer
    Public Property GapOpens As Integer
    Public Property BTOP As String
    Public Property TaxID As String
    Public Property TaxIDs As List(Of String)
    Public Property SciName As String
    Public Property SciNames As List(Of String)
    Public Property CommonName As String
    Public Property CommonNames As List(Of String)
    Public Property BlastName As String
    Public Property BlastNames As List(Of String)
    Public Property KingdomName As String
    Public Property KingdomNames As List(Of String)

    Public Property RecordGI As String
    Public Property Record_IterationQueryDefinition As String
    Public Property Record_IterationQueryLength As String
    Public Property Score As Double
    Public Property BitScore As Double
    Public Property Evalue As Double
    Public Property Hit_Length As Integer

    Public Property File As System.IO.FileInfo
    Public Sub New(HSP As Bio.Web.Blast.Hsp)
        Me.HSP = HSP
    End Sub
    Public Sub New(HSP As Bio.Web.Blast.Hsp, RecordID As Integer, HitID As Integer, HSPID As Integer)
        Me.HSP = HSP
        Me.RecordID = RecordID
        Me.HitID = HitID
        Me.HSPID = HSPID
    End Sub

    Public Sub New(s() As String, d As Dictionary(Of Integer, String))
        Dim Hsp As New Hsp
        Try


            For Each Item In d
                Select Case Item.Value
                    Case "Hsp.QueryStart"
                        Hsp.QueryStart = s(Item.Key)
                        Me.QueryStart = Hsp.QueryStart
                    Case "Hsp.QueryEnd"
                        Hsp.QueryEnd = s(Item.Key)
                        Me.QueryEnd = Hsp.QueryEnd
                    Case "Hsp.HitStart"
                        Hsp.HitStart = s(Item.Key)
                        Me.HitStart = Hsp.HitStart
                    Case "Hsp.HitEnd"
                        Hsp.HitEnd = s(Item.Key)
                        Me.HitEnd = Hsp.HitEnd
                    Case "Hsp.QuerySeq"
                        Hsp.QuerySequence = s(Item.Key)

                    Case "Hsp.HitSeq"
                        Hsp.HitSequence = s(Item.Key)
                    Case "Hsp.Evalue"
                        Hsp.EValue = Szunyi.Common.ToDouble(s(Item.Key))
                        Me.Evalue = Hsp.EValue
                    Case "Hsp.BitScore"
                        Hsp.BitScore = Szunyi.Common.ToDouble(s(Item.Key))
                        Me.BitScore = Hsp.BitScore
                    Case "Hsp.Score"
                        Hsp.Score = Szunyi.Common.ToDouble(s(Item.Key))
                        Me.Score = Hsp.Score
                    Case "Hsp.AlignmentLength"
                        Hsp.AlignmentLength = s(Item.Key)
                        Me.AlignmentLength = Hsp.AlignmentLength
                    Case "Hsp.IdentitiesCount"
                        Hsp.IdentitiesCount = s(Item.Key)
                        Me.IdentitiesCount = Hsp.IdentitiesCount
                    Case "Hsp.PositivesCount"
                        Hsp.PositivesCount = s(Item.Key)
                        Me.PositiveCOunt = Hsp.IdentitiesCount
                    Case "Hsp.GapOpens"
                        Me.GapOpens = s(Item.Key)

                    Case "Hsp.Gaps"
                        Hsp.Gaps = s(Item.Key)

                    Case "Hsp.QueryFrame"
                        Hsp.QueryFrame = s(Item.Key)
                    Case "Hsp.HitFrame"
                        Hsp.HitFrame = s(Item.Key)
                    Case "btop"
                        Me.BTOP = s(Item.Key)

                    Case "staxids"
                        Me.TaxIDs = Split(s(Item.Key), ",").ToList
                        Me.TaxID = Me.TaxIDs.First
                    Case "sscinames"
                        Me.SciNames = Split(s(Item.Key), ",").ToList
                        Me.SciName = Me.SciNames.First
                    Case "scomnames"
                        Me.CommonNames = Split(s(Item.Key), ",").ToList
                        Me.CommonName = Me.CommonNames.First
                    Case "sblastnames"
                        Me.BlastNames = Split(s(Item.Key), ",").ToList
                        Me.BlastName = Me.BlastNames.First
                    Case "sskingdoms"
                        Me.KingdomNames = Split(s(Item.Key), ",").ToList
                        Me.KingdomName = Me.KingdomNames.First
                    Case "stitle"
                  '  d.Add(i1, "stitle")
                    Case "salltitles"
                   ' d.Add(i1, "salltitles")
                    Case "sstrand"
                 '   d.Add(i1, "Hsp.Evalue")
                    Case "qcovs"
                 '   d.Add(i1, "qcovs")
                    Case "qcovhsp"
                 '   d.Add(i1, "qcovhsp")
                    Case "qcovus"
                    '    d.Add(i1, "qcovus")
                    Case "Record.IterationQueryId"
                        Me.RecordID = s(Item.Key)
                    Case "Record.IterationQueryGI"
                        Me.RecordGI = s(Item.Key)
                    Case "Record.IterationQueryDefinition"
                        Me.Record_IterationQueryDefinition = s(Item.Key)
                    Case "Record.ItarationQueryLength"
                        Me.Record_IterationQueryLength = s(Item.Key)
                    Case "Hit.Id"
                        Me.HitID = s(Item.Key)
                    Case "Hit.GI"
                        Me.HitGi = s(Item.Key)
                    Case "Hit.Accession"
                        Me.Hit_Accession = s(Item.Key)
                    Case "Hit.Length"
                        Me.Hit_Length = s(Item.Key)
                    Case "Hsp.Identity_Percent"
                        '     Hsp.Identity_Percent = s(Item.Key)
                        Dim kj As Int16 = 54
                    Case "Hsp.MisMatch"
                        '     Hsp.Identity_Percent = s(Item.Key)
                        Dim kj As Int16 = 54
                    Case "Hsp.Positives_Percent"
                        '     Hsp.Identity_Percent = s(Item.Key)
                        Dim kj As Int16 = 54

                    Case Else
                        Dim kj As Int16 = 43
                End Select
            Next
        Catch ex As Exception
            Dim kj As Int16 = 54
        End Try
        Me.HSP = Hsp
        Me.d = d
    End Sub
End Class

Public Class OwnHit
    Inherits Bio.Web.Blast.Hit
    Public Property RecordID As Integer
    Public Property HitID As Integer
    Public Property OwnHsps As New List(Of OwnHsp)
    Public Property DB_File_Name As String
    Public hit As Bio.Web.Blast.Hit
    Public TaxID As String
    Public Sub New(Hit As Bio.Web.Blast.Hit)
        Me.hit = Hit
        Me.Id = Hit.Id
        Me.Def = Hit.Def
    End Sub
    Public Sub New(Hit As Bio.Web.Blast.Hit, RecordID As Integer, HitID As Integer)
        Me.hit = Hit
        Me.RecordID = RecordID
        Me.HitID = HitID

    End Sub
End Class

Public Class OwnBlastRecord
    Inherits Bio.Web.Blast.BlastSearchRecord
    Public Property Query_TaxID As String
    Public Property Hit_TaxID As String
    Public Property OwnHits As New List(Of OwnHit)
End Class

Public Module Extensions
#Region "extHSPs"
    <Extension()>
    Public Iterator Function ToExtHsp(clonedAndFilteredBlastSearchRecords As List(Of OwnBlastRecord)) As IEnumerable(Of extHSP)
        Dim out As New List(Of extHSP)
        Dim RecordID As Integer = -1
        For Each record In clonedAndFilteredBlastSearchRecords
            RecordID += 1
            Dim HitID As Integer = -1
            For Each oHit As OwnHit In record.OwnHits
                HitID += 1
                Dim HspId As Integer = -1
                For Each oHsp In oHit.OwnHsps
                    HspId += 1
                    Yield (New extHSP(oHsp.HSP, oHit, record, HspId, HitID, RecordID))
                Next
            Next
        Next
    End Function
    <Extension()>
    Public Iterator Function ToExtHsps(record As OwnBlastRecord) As IEnumerable(Of extHSP)
        Dim out As New List(Of extHSP)
        Dim RecordID As Integer = -1
        RecordID += 1
        Dim HitID As Integer = -1
        For Each oHit As OwnHit In record.OwnHits
            HitID += 1
            Dim HspId As Integer = -1
            For Each oHsp In oHit.OwnHsps
                HspId += 1
                Yield (New extHSP(oHsp.HSP, oHit, record, HspId, HitID, RecordID))
            Next
        Next

    End Function
#End Region

    <Extension()>
    Public Function Positives_Percent(hsp As Bio.Web.Blast.Hsp) As Double
        Return hsp.PositivesCount / hsp.AlignmentLength * 100
    End Function

    <Extension()>
    Public Function MisMatch(hsp As Bio.Web.Blast.Hsp) As Integer
        Return hsp.AlignmentLength - hsp.IdentitiesCount
    End Function
    <Extension()>
    Public Function Identity_Percent(hsp As Bio.Web.Blast.Hsp) As Double
        Return hsp.IdentitiesCount / hsp.AlignmentLength * 100
    End Function

    <Extension()>
    Public Function Clone(Item As BlastSearchRecord) As BlastSearchRecord
        Dim r As New BlastSearchRecord
        r.IterationMessage = Item.IterationMessage
        r.IterationNumber = Item.IterationNumber
        r.IterationQueryDefinition = Item.IterationQueryDefinition
        r.IterationQueryId = Item.IterationQueryId
        r.IterationQueryLength = Item.IterationQueryLength
        If IsNothing(Item.Statistics) = False Then
            r.Statistics = New Bio.Web.Blast.BlastStatistics
            r.Statistics.DatabaseLength = Item.Statistics.DatabaseLength
            r.Statistics.EffectiveSearchSpace = Item.Statistics.EffectiveSearchSpace
            r.Statistics.Entropy = Item.Statistics.Entropy
            r.Statistics.HspLength = Item.Statistics.HspLength
            r.Statistics.Kappa = Item.Statistics.Kappa
            r.Statistics.Lambda = Item.Statistics.Lambda
            r.Statistics.SequenceCount = Item.Statistics.SequenceCount
        End If
        For Each Hit In Item.Hits
            r.Hits.Add(Hit.Clone)
        Next

        Return r
    End Function
    <Extension()>
    Public Function Clone(Item As Bio.Web.Blast.Hit) As Bio.Web.Blast.Hit
        Dim Hit As New Bio.Web.Blast.Hit
        Hit.Accession = Item.Accession
        Hit.Def = Item.Def
        Hit.Id = Item.Id
        Hit.Length = Item.Length
        For Each hsp In Item.Hsps
            Hit.Hsps.Add(hsp.Clone)
        Next
        Return Hit
    End Function


    <Extension()>
    Public Function Current_Value(Item As Hsp, flag As Enums.Hsp) As String
        Select Case flag
            Case Enums.Hsp.AlignmentLength
                Return Item.AlignmentLength
            Case Enums.Hsp.BitScore
                Return Item.BitScore
            Case Enums.Hsp.Density
                Return Item.Density
            Case Enums.Hsp.Evalue
                Return Item.EValue
            Case Enums.Hsp.Gaps
                Return Item.Gaps
            Case Enums.Hsp.HitEnd
                Return Item.HitEnd
            Case Enums.Hsp.HitStart
                Return Item.HitStart
            Case Enums.Hsp.IdentitiesCount
                Return Item.IdentitiesCount
            Case Enums.Hsp.Midline
                Return Item.Midline
            Case Enums.Hsp.PositivesCount
                Return Item.PositivesCount
            Case Enums.Hsp.QueryEnd
                Return Item.QueryEnd
            Case Enums.Hsp.QueryFrame
                Return Item.QueryFrame
            Case Enums.Hsp.QuerySequence
                Return Item.QuerySequence
            Case Enums.Hsp.QueryStart
                Return Item.QueryStart
            Case Enums.Hsp.Score
                Return Item.Score
        End Select

    End Function
    <Extension()>
    Public Function Current_Value(Item As Bio.Web.Blast.Hit, flag As Enums.Hit) As String
        Select Case flag
            Case Enums.Hit.Accession
                Return Item.Accession
            Case Enums.Hit.Def
                Return Item.Def
                ' Case Enums.Hit.Hsps_Count
           '     Return Item.Hsps.Count
            Case Enums.Hit.ID
                Return Item.Id
            Case Enums.Hit.Length
                Return Item.Length
            Case Else
                Return Nothing
        End Select

    End Function
    <Extension()>
    Public Function Current_Value(Item As BlastSearchRecord, flag As Enums.Record) As String
        Select Case flag
           ' Case Enums.Record.Hit_Counts
            '    Return Item.Hits.Count
            Case Enums.Record.IterationMessage
                Return Item.IterationMessage
            Case Enums.Record.IterationNumber
                Return Item.IterationNumber
            Case Enums.Record.IterationQueryDefinition
                Return Item.IterationQueryDefinition
            Case Enums.Record.IterationQueryId
                Return Item.IterationQueryId
            Case Enums.Record.IterationQueryLength
                Return Item.IterationQueryLength
            Case Else
                Return Nothing
        End Select

    End Function
    <Extension()>
    Public Function DisplayMember(Record As BlastSearchRecord)
        Return Record.IterationQueryDefinition.ToUpper

    End Function

    <Extension()>
    Public Function Clone(Items As List(Of BlastSearchRecord)) As List(Of BlastSearchRecord)
        Dim out As New List(Of BlastSearchRecord)
        For Each Item In Items
            out.Add(Item.Clone)
        Next
        Return out
    End Function

    <Extension()>
    Public Function Clone(Items As List(Of OwnBlastRecord)) As List(Of OwnBlastRecord)
        Dim out As New List(Of OwnBlastRecord)
        For Each item In Items
            out.Add(item.Clone)
        Next
        Return out
    End Function
    <Extension()>
    Public Function Clone(Item As OwnBlastRecord) As OwnBlastRecord
        Dim x As New OwnBlastRecord
        '  x.Hits = Item.Hits
        x.IterationQueryDefinition = Item.IterationQueryDefinition
        x.IterationQueryLength = Item.IterationQueryLength
        For Each Hit In Item.Hits
            x.Hits.Add(Hit.Clone)
        Next
        For Each oHit In Item.OwnHits
            x.OwnHits.Add(oHit.Clone)
        Next
        Return x
    End Function
    <Extension()>
    Public Function Clone(Items As List(Of OwnHit)) As List(Of OwnHit)
        Dim out As New List(Of OwnHit)
        For Each Item In Items
            out.Add(Item.Clone)
        Next
        Return out
    End Function
    <Extension()>
    Public Function Clone(Item As OwnHit) As OwnHit
        Dim Hit As New OwnHit(Item.hit)

        Hit.HitID = Item.HitID
        Hit.RecordID = Item.RecordID
        Hit.hit = Item.hit.Clone
        Hit.DB_File_Name = Item.DB_File_Name
        For Each hsp In Item.OwnHsps
            Hit.OwnHsps.Add(hsp.Clone)
        Next
        Return Hit
    End Function

    <Extension()>
    Public Function Clone(Item As OwnHsp) As OwnHsp
        Dim HSP As New OwnHsp(Item.HSP)
        HSP.HSP = Item.HSP.Clone()
        HSP.File = Item.File

        HSP.AlignmentLength = Item.AlignmentLength
        HSP.BitScore = Item.BitScore
        HSP.BlastName = Item.BlastName
        HSP.BlastNames = Item.BlastNames
        HSP.BTOP = Item.BTOP
        HSP.CommonName = Item.CommonName
        HSP.CommonNames = Item.CommonNames
        HSP.Evalue = Item.Evalue
        HSP.HitEnd = Item.HitEnd
        HSP.HitGi = Item.HitGi
        HSP.HitID = Item.HitID
        HSP.HitStart = Item.HitStart
        HSP.Hit_Accession = Item.Hit_Accession
        HSP.Hit_Length = Item.Hit_Length
        HSP.HSPID = Item.HSPID
        HSP.IdentitiesCount = Item.IdentitiesCount
        HSP.KingdomName = Item.KingdomName
        HSP.KingdomNames = Item.KingdomNames
        HSP.Number_of_identical_matches = Item.Number_of_identical_matches
        HSP.Percentage_of_identical_matches = Item.Percentage_of_identical_matches
        HSP.PositiveCOunt = Item.PositiveCOunt

        HSP.QueryEnd = Item.QueryEnd
        HSP.QueryStart = Item.QueryStart
        HSP.Query_GI = Item.Query_GI
        HSP.RecordGI = Item.RecordGI
        HSP.RecordID = Item.RecordID
        HSP.Record_IterationQueryDefinition = Item.Record_IterationQueryDefinition

        HSP.Record_IterationQueryLength = Item.Record_IterationQueryLength
        HSP.SciName = Item.SciName
        HSP.SciNames = Item.SciNames
        HSP.Score = Item.Score
        HSP.Subject_GI = Item.Subject_GI
        HSP.TaxID = Item.TaxID
        HSP.TaxIDs = Item.TaxIDs

        Return HSP
    End Function
    <Extension()>
    Public Function Clone(Item As Hsp) As Hsp
        Dim HSP As New Hsp
        HSP.AlignmentLength = Item.AlignmentLength
        HSP.BitScore = Item.BitScore
        HSP.Density = Item.Density
        HSP.EValue = Item.EValue
        HSP.Gaps = Item.Gaps
        HSP.HitEnd = Item.HitEnd
        HSP.HitFrame = Item.HitFrame
        HSP.HitSequence = Item.HitSequence
        HSP.HitStart = Item.HitStart
        HSP.IdentitiesCount = Item.IdentitiesCount
        HSP.Midline = Item.Midline
        HSP.PatternFrom = Item.PatternFrom
        HSP.PatternTo = Item.PatternTo
        HSP.PositivesCount = Item.PositivesCount
        HSP.QueryEnd = Item.QueryEnd
        HSP.QueryFrame = Item.QueryFrame
        HSP.QuerySequence = Item.QuerySequence
        HSP.QueryStart = Item.QueryStart
        HSP.Score = Item.Score
        Return HSP
    End Function

    ''' <summary>
    ''' Return Last Part Of File (separtor = ,)
    ''' </summary>
    ''' <param name="File"></param>
    ''' <param name="DirPath"></param>
    ''' <returns></returns>
    <Extension()>
    Public Function QueryFastaFile(File As FileInfo, DirPath As String) As FileInfo
        Dim QFileName = Split(File.Name, ",").Last
        Dim QFIle = New FileInfo(DirPath & QFileName)
        QFIle = QFIle.woExtension
        QFIle.Change_Directory(New DirectoryInfo(DirPath))
        If QFIle.Exists = True Then
            Return QFIle
        Else
            Return Nothing
        End If
    End Function

End Module

