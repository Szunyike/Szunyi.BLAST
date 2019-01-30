Imports Szunyi.BLAST

Public Class Analysis
    Public Shared Function Get_HitIDs_w_DbFileNames(l_OwnRecors As List(Of OwnBlastRecord), QueryIDs As List(Of String)) As Dictionary(Of String, List(Of String))
        Dim c As New Szunyi.BLAST.Sort.cOwnBlastRecord
        l_OwnRecors.Sort(c)
        Dim tmp As New OwnBlastRecord
        Dim ForRetrive As New Dictionary(Of String, List(Of String))
        For Each Item In QueryIDs
            tmp.IterationQueryDefinition = Item
            Dim Index = l_OwnRecors.BinarySearch(tmp, c)
            If Index > -1 Then
                Dim HitIDs = Szunyi.BLAST.BlastManipulation.Get_Unique_HitIDs(l_OwnRecors(Index))
                Dim FileNames = Szunyi.BLAST.BlastManipulation.Get_FileNames(l_OwnRecors(Index))
                Dim DB_FIleNames = Szunyi.BLAST.BlastManipulation.Get_DB_FileNames(l_OwnRecors(Index))
                For i1 = 0 To HitIDs.Count - 1
                    If ForRetrive.ContainsKey(DB_FIleNames(i1)) = False Then ForRetrive.Add(DB_FIleNames(i1), New List(Of String))
                    ForRetrive(DB_FIleNames(i1)).Add(HitIDs(i1))
                Next
            Else
                Dim kj As Int16 = 54
            End If
        Next
        Return ForRetrive
    End Function
    Public Shared Function ByFileName(clonedAndFilteredBlastSearchRecords As List(Of OwnBlastRecord), AllTaxName As List(Of String)) As Dictionary(Of String, Dictionary(Of String, Integer))
        Dim Res As New Dictionary(Of String, Dictionary(Of String, Integer))
        Dim Index As Integer = 0
        For Each Record In clonedAndFilteredBlastSearchRecords
            If IsNothing(Record.IterationQueryDefinition) = False Then
                If Res.ContainsKey(Record.IterationQueryDefinition) = False Then Res.Add(Record.IterationQueryDefinition, New Dictionary(Of String, Integer))
                For Each oHit In Record.OwnHits
                    For Each oHsp In oHit.OwnHsps
                        If Res(Record.IterationQueryDefinition).ContainsKey(oHsp.File.Name) = False Then
                            Res(Record.IterationQueryDefinition).Add(oHsp.File.Name, 0)
                            If AllTaxName.Contains(oHsp.File.Name) = False Then AllTaxName.Add(oHsp.File.Name)
                        End If
                        Res(Record.IterationQueryDefinition)(oHsp.File.Name) += 1
                        Index += 1
                    Next
                Next
            End If
        Next
        Return Res
    End Function

    Public Shared Function Get_Uniques(res As Dictionary(Of String, Dictionary(Of String, Integer)), count As Integer) As List(Of String)
        Dim Out As New List(Of String)
        For Each Item In res
            Dim OK As Boolean = True
            For Each FileName In Item.Value
                If FileName.Value <> 1 Then
                    OK = False
                    Exit For
                End If
            Next
            If OK = True Then Out.Add(Item.Key)
        Next
        Return Out
    End Function
    ''' <summary>
    ''' Return QueryIDs Where Hits.Count >= 
    ''' </summary>
    ''' <param name="res"></param>
    ''' <param name="File_Counts"></param>
    ''' <returns></returns>
    Public Shared Function Get_Common(res As Dictionary(Of String, Dictionary(Of String, Integer)), File_Counts As Integer) As List(Of String)

        Dim t = From x In res Where x.Value.Values.Count >= File_Counts Select x.Key

        If t.Count >= 0 Then Return t.ToList

        Return New List(Of String)
    End Function

    Public Shared Function Get_Common_Uniques(res As Dictionary(Of String, Dictionary(Of String, Integer)), count As Integer) As List(Of String)
        Dim c = Get_Common(res, count)
        Dim u = Get_Uniques(res, count)
        Return c.Intersect(u).ToList
    End Function
End Class
