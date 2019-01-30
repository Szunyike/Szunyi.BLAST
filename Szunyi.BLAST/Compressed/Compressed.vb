Imports System.IO
Imports Szunyi.IO.Extensions

Namespace Compressed
    Public Class Organism
        Public Property CommonName As String
        Public Property SciName As String
        Public Property BlastName As String
        Public Property TaxId As Integer
        Public Property SuperKingdom As String
    End Class
    Public Class Seq
        Public Property ID As String
        Public Property GI As String
        Public Property Accession As String
        Public Property Length As Integer
    End Class
    Public Class cHSP
        Public Property HSP As Bio.Web.Blast.Hsp
        Public Property HitOrg As Organism
        Public Property Query As Seq
        Public Property Hit As Seq
        Public Property File As FileInfo
        Public Property Database As FileInfo
    End Class

    Public Class Common
        Public Property Organisms As New SortedList(Of Integer, Organism)
        Public Property Seqs As New SortedList(Of String, Seq)
        Public Property Database As System.IO.FileInfo
        Public Property HSPs As New List(Of cHSP)
#Region "New"
        Public Sub New(File As FileInfo)
            Dim db = New FileInfo(Split(File.Parse_Lines(3, 3).Last, "Database: ").Last)
            Dim log As New System.Text.StringBuilder

            For Each eHSP In Szunyi.BLAST.Import.From_File_HSP(File, log)
                Me.HSPs.Add(New cHSP With {.HitOrg = Get_Hit_Org(eHSP),
                            .HSP = eHSP.HSP,
                            .Hit = Get_Hit_Seq(eHSP),
                            .Query = Get_Query_Seq(eHSP),
                            .File = File, .Database = db})
            Next

        End Sub
        Public Sub New(Files As List(Of FileInfo))
            Dim log As New System.Text.StringBuilder
            For Each File In Files
                Dim db = New FileInfo(Split(File.Parse_Lines(3, 3).Last, "Database: ").Last)


                For Each eHSP In Szunyi.BLAST.Import.From_File_HSP(File, log)
                    Me.HSPs.Add(New cHSP With {.HitOrg = Get_Hit_Org(eHSP),
                                .HSP = eHSP.HSP,
                                .Hit = Get_Hit_Seq(eHSP),
                                .Query = Get_Query_Seq(eHSP),
                                .File = File, .Database = db})
                Next
            Next
        End Sub

        Public Sub New(File As FileInfo, SeqFile As FileInfo)

        End Sub
#End Region

#Region "Private Functions"
        Private Function Get_Hit_Org(eHSP As OwnHsp) As Organism
            If Me.Organisms.ContainsKey(eHSP.TaxID) = True Then
                Return Me.Organisms(eHSP.TaxID)
            Else
                Dim x As New Organism With {.TaxId = eHSP.TaxID, .CommonName = eHSP.CommonName, .SciName = eHSP.SciName, .BlastName = eHSP.BlastName, .SuperKingdom = eHSP.KingdomName}
                Me.Organisms.Add(x.TaxId, x)
                Return x
            End If
        End Function
        Private Function Get_Hit_Seq(eHSP As OwnHsp) As Seq
            If Me.Seqs.ContainsKey(eHSP.HitID) Then
                Return Me.Seqs(eHSP.HitID)
            Else
                Dim x As New Seq With {.Accession = eHSP.Hit_Accession, .Length = eHSP.Hit_Length, .GI = eHSP.HitGi, .ID = eHSP.HitID}
                Me.Seqs.Add(x.ID, x)
                Return Me.Seqs(x.ID)
            End If
        End Function
        Private Function Get_Query_Seq(eHSP As OwnHsp) As Seq
            If Me.Seqs.ContainsKey(eHSP.Record_IterationQueryDefinition) Then
                Return Me.Seqs(eHSP.Record_IterationQueryDefinition)
            Else
                Dim x As New Seq With {.Accession = eHSP.Record_IterationQueryDefinition, .Length = eHSP.Record_IterationQueryLength, .GI = eHSP.Query_GI, .ID = eHSP.RecordID}
                Me.Seqs.Add(x.ID, x)
                Return Me.Seqs(x.ID)
            End If
        End Function
#End Region

    End Class
End Namespace

