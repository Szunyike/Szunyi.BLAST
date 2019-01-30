Imports Bio.Web.Blast
Imports Szunyi.BLAST
Imports Szunyi.Common.Extensions

Public Class BlastManipulation
    Public Class Hsp
        Public Shared Function All(Records As List(Of BlastSearchRecord)) As List(Of extHSP)
            Dim Out As New List(Of extHSP)
            For recordID = 0 To Records.Count - 1
                For HitID = 0 To Records(recordID).Hits.Count - 1
                    For HSPID = 0 To Records(recordID).Hits(HitID).Hsps.Count - 1
                        Dim x As New extHSP(Records(recordID).Hits(HitID).Hsps(HSPID), Records(recordID).Hits(HitID), Records(recordID), HSPID, HitID, recordID)
                        Out.Add(x)
                    Next
                Next
            Next
            Dim res = (From x In Out Order By x.HSPID Descending, x.HitID Descending).ToList
            Return res
        End Function
        Public Shared Function All(bRecord As BlastSearchRecord) As List(Of extHSP)
            Dim Out As New List(Of extHSP)
            For Each H In bRecord.Hits
                For Each Hsp In H.Hsps
                    Dim x As New extHSP(Hsp, H, bRecord)
                    Out.Add(x)
                Next
            Next
            Return Out
        End Function
        Public Shared Function All(bReacord As BlastSearchRecord, Hit As Bio.Web.Blast.Hit) As List(Of extHSP)
            Dim Out As New List(Of extHSP)



            Return Out
        End Function

        Public Shared Function All(clonedAndFilteredBlastSearchRecords As List(Of OwnBlastRecord)) As List(Of extHSP)
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
                        out.Add(New extHSP(oHsp.HSP, oHit, record, HspId, HitID, RecordID))
                    Next
                Next
            Next

            Return out
        End Function

        Public Shared Function All_Own(Record As OwnBlastRecord) As List(Of extHSP)
            Dim out As New List(Of extHSP)

            For Each oHit As OwnHit In Record.OwnHits
                For Each oHsp In oHit.OwnHsps
                    out.Add(New extHSP(oHsp.HSP, oHit, Record))
                Next
            Next

            Return out
        End Function
        Public Shared Function All_Own(Record As OwnBlastRecord, H As OwnHit) As List(Of extHSP)
            Dim out As New List(Of extHSP)


            For Each oHsp In H.OwnHsps
                out.Add(New extHSP(oHsp.HSP, H.hit, Record))

            Next


            Return out
        End Function
        Public Shared Function All_Own(Record As OwnBlastRecord, H As Bio.Web.Blast.Hit) As List(Of extHSP)
            Dim out As New List(Of extHSP)


            For Each oHsp In H.Hsps
                out.Add(New extHSP(oHsp, H, Record))

            Next


            Return out
        End Function
    End Class

    Public Shared Function Get_Unique_HitIDs(ownBlastRecord As OwnBlastRecord) As List(Of String)
        Dim out = From x In ownBlastRecord.OwnHits Select x.hit.Id

        If out.Count > 0 Then Return out.ToList
        Return New List(Of String)
    End Function

    Public Shared Function Get_FileNames(ownBlastRecord As OwnBlastRecord) As List(Of String)
        Dim out = From x In ownBlastRecord.OwnHits Select x.OwnHsps.First.File.Name

        If out.Count > 0 Then Return out.ToList
        Return New List(Of String)
    End Function
    Public Shared Function Get_DB_FileNames(ownBlastRecord As OwnBlastRecord) As List(Of String)
        Dim out = From x In ownBlastRecord.OwnHits Select x.DB_File_Name

        If out.Count > 0 Then Return out.ToList
        Return New List(Of String)
    End Function
End Class
Public Class Hit
        Public Shared Function All(bRecord As BlastSearchRecord) As List(Of Bio.Web.Blast.Hit)
            Return bRecord.Hits
        End Function
        ''' <summary>
        ''' Return All Hit Definitions
        ''' </summary>
        ''' <param name="blastRecords"></param>
        ''' <returns></returns>
        Public Shared Function All_Definitions(blastRecords As List(Of BlastSearchRecord)) As List(Of String)
            Dim out As New List(Of String)
            For Each BlastRecord In blastRecords
                For Each Hit In BlastRecord.Hits
                    out.Add(Hit.Def)
                Next
            Next
            Return out
        End Function

        ''' <summary>
        ''' Return Unique Hit Definitions
        ''' </summary>
        ''' <param name="blastRecords"></param>
        ''' <returns></returns>
        Public Shared Function Unique_Definitions(blastRecords As List(Of BlastSearchRecord)) As List(Of String)
            Dim out = All_Definitions(blastRecords)

            Return out.Distinct.ToList
        End Function

        ''' <summary>
        ''' Return All Hit Accession
        ''' </summary>
        ''' <param name="blastRecords"></param>
        ''' <returns></returns>
        Public Shared Function All_Accessions(blastRecords As List(Of BlastSearchRecord)) As List(Of String)
            Dim out As New List(Of String)
            For Each BlastRecord In blastRecords
                For Each Hit In BlastRecord.Hits
                    out.Add(Hit.Accession)
                Next
            Next
            Return out
        End Function

        ''' <summary>
        ''' Return Unique Hit Accession
        ''' </summary>
        ''' <param name="blastRecords"></param>
        ''' <returns></returns>
        Public Shared Function Unique_Accessions(blastRecords As List(Of BlastSearchRecord)) As List(Of String)
            Dim out = All_Accessions(blastRecords)
            Return out.Distinct.ToList
        End Function

        ''' <summary>
        ''' Return All Hit IDs
        ''' </summary>
        ''' <param name="blastRecords"></param>
        ''' <returns></returns>
        Public Shared Function All_IDs(blastRecords As List(Of BlastSearchRecord)) As List(Of String)
            Dim out As New List(Of String)
            For Each BlastRecord In blastRecords
                For Each Hit In BlastRecord.Hits
                    out.Add(Hit.Id)
                Next
            Next
            Return out
        End Function

        ''' <summary>
        ''' Return Unique Hit IDs
        ''' </summary>
        ''' <param name="blastRecords"></param>
        ''' <returns></returns>
        Public Shared Function Unique_IDs(blastRecords As List(Of BlastSearchRecord)) As List(Of String)
            Dim out = All_IDs(blastRecords)

            Return out.Distinct.ToList
        End Function


    End Class
    Public Class Record
        ''' <summary>
        ''' Return All Query Definitons
        ''' </summary>
        ''' <param name="bRecords"></param>
        ''' <returns></returns>
        Public Shared Function Custom(bRecords As List(Of BlastSearchRecord), Type As Szunyi.BLAST.Enums.Record) As List(Of String)
            Dim out As New List(Of String)
            For Each bRecord In bRecords

                out.Add(bRecord.Current_Value(Type))
            Next
            Return out
        End Function
        ''' <summary>
        ''' Return All Query Definitons
        ''' </summary>
        ''' <param name="bRecords"></param>
        ''' <returns></returns>
        Public Shared Function Custom(bRecords As List(Of BlastSearchRecord),
                                      Types As IEnumerable(Of Szunyi.BLAST.Enums.Record),
                                      Optional Separator As String = vbTab) As List(Of String)
            Dim out As New List(Of String)
            For Each bRecord In bRecords
                Dim c As New List(Of String)
                For Each Type In Types
                    c.Add(bRecord.Current_Value(Type))
                Next
            out.Add(c.GetText(Separator))
        Next
            Return out
        End Function

        ''' <summary>
        ''' Return All Query Definitons
        ''' </summary>
        ''' <param name="bRecords"></param>
        ''' <returns></returns>
        Public Shared Function All_QueryDefintions(bRecords As List(Of BlastSearchRecord)) As List(Of String)
            Dim out As New List(Of String)
            For Each bRecord In bRecords

                out.Add(bRecord.IterationQueryDefinition)
            Next
            Return out
        End Function
        ''' <summary>
        ''' Return Unique QueryDefinitions as List Of String
        ''' </summary>
        ''' <param name="BlastRecords"></param>
        ''' <returns></returns>
        Public Shared Function GetUniqueQueryDefintions(BlastRecords As List(Of BlastSearchRecord)) As List(Of String)
            Dim out = All_QueryDefintions(BlastRecords)

            Return out.Distinct.ToList
        End Function
    End Class




