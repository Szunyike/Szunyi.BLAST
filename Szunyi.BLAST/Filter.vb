Imports Bio.Web.Blast
Imports Szunyi.BLAST
Imports Szunyi.Common.Extensions

Imports Szunyi.IO

Public Class Filter
    Public Class Hit
        Public Shared Iterator Function Parse_Hits(ClonedAndFilteredBlastSearchRecords As List(Of BlastSearchRecord)) As IEnumerable(Of OwnHit)
            Dim IndexOfRecord As Integer = -1
            For Each Record In ClonedAndFilteredBlastSearchRecords
                IndexOfRecord += 1
                Dim IndexOfHit As Integer = -1
                For Each Hit In Record.Hits
                    IndexOfHit += 1

                    Yield New OwnHit(Hit, IndexOfRecord, IndexOfHit)
                Next
            Next
        End Function
        Public Shared Iterator Function Parse_Hits(ClonedAndFilteredBlastSearchRecords As List(Of OwnBlastRecord)) As IEnumerable(Of OwnHit)
            Dim IndexOfRecord As Integer = -1
            For Each Record In ClonedAndFilteredBlastSearchRecords
                IndexOfRecord += 1
                Dim IndexOfHit As Integer = -1
                For Each Hit In Record.OwnHits
                    IndexOfHit += 1

                    Yield Hit
                Next
            Next
        End Function
        ''' <summary>
        ''' Filter Out Self Hsps QueryDef = HitDef
        ''' </summary>
        ''' <param name="ClonedAndFilteredBlastSearchRecords"></param>
        ''' <returns></returns>
        Public Shared Function Self_Hits(ClonedAndFilteredBlastSearchRecords As List(Of BlastSearchRecord)) As List(Of BlastSearchRecord)

            Dim t As New List(Of BlastSearchRecord)
            For Each Item In ClonedAndFilteredBlastSearchRecords
                t.Add(Item)
                For i1 = t.Last.Hits.Count - 1 To 0 Step -1
                    If t.Last.IterationQueryDefinition = t.Last.Hits(i1).Id Then
                        t.Last.Hits.RemoveAt(i1)
                    End If
                Next
                If t.Last.Hits.Count = 0 Then t.RemoveAt(t.Count - 1)
            Next
            Return t
        End Function

        Public Shared Function Set_Nothing(clonedAndFilteredBlastSearchRecords As List(Of BlastSearchRecord), toDelete As List(Of OwnHit)) As List(Of BlastSearchRecord)
            For i1 = toDelete.Count - 1 To 0 Step -1
                Dim HSP = toDelete(i1)
                clonedAndFilteredBlastSearchRecords(HSP.RecordID).Hits(HSP.HitID) = Nothing
            Next
            Return clonedAndFilteredBlastSearchRecords
        End Function
        Public Shared Function Set_Nothing(clonedAndFilteredBlastSearchRecords As List(Of OwnBlastRecord), toDelete As List(Of OwnHit)) As List(Of OwnBlastRecord)
            For i1 = toDelete.Count - 1 To 0 Step -1
                Dim HSP = toDelete(i1)
                clonedAndFilteredBlastSearchRecords(HSP.RecordID).Hits(HSP.HitID) = Nothing
            Next
            Return clonedAndFilteredBlastSearchRecords
        End Function
        Public Shared Function Clear(ClonedAndFilteredBlastSearchRecords As List(Of OwnBlastRecord)) As List(Of OwnBlastRecord)
            For i = ClonedAndFilteredBlastSearchRecords.Count - 1 To 0 Step -1
                Dim bRecord = ClonedAndFilteredBlastSearchRecords(i)
                For i1 = bRecord.Hits.Count - 1 To 0 Step -1

                    If IsNothing(bRecord.Hits(i1)) = True OrElse bRecord.Hits(i1).Hsps.Count = 0 Then
                        ClonedAndFilteredBlastSearchRecords(i).Hits.RemoveAt(i1)
                    End If

                Next
                If bRecord.Hits.Count = 0 Then
                    ClonedAndFilteredBlastSearchRecords.RemoveAt(i)
                End If
            Next
            Return ClonedAndFilteredBlastSearchRecords
        End Function

        Public Shared Function Clear(ClonedAndFilteredBlastSearchRecords As List(Of BlastSearchRecord)) As List(Of BlastSearchRecord)
            For i = ClonedAndFilteredBlastSearchRecords.Count - 1 To 0 Step -1
                Dim bRecord = ClonedAndFilteredBlastSearchRecords(i)
                For i1 = bRecord.Hits.Count - 1 To 0 Step -1

                    If IsNothing(bRecord.Hits(i1)) = True OrElse bRecord.Hits(i1).Hsps.Count = 0 Then
                        ClonedAndFilteredBlastSearchRecords(i).Hits.RemoveAt(i1)
                    End If

                Next
                If bRecord.Hits.Count = 0 Then
                    ClonedAndFilteredBlastSearchRecords.RemoveAt(i)
                End If
            Next
            Return ClonedAndFilteredBlastSearchRecords
        End Function
    End Class

    Public Class HSP

        Public Shared Iterator Function Parse_HSPs(ClonedAndFilteredBlastSearchRecords As List(Of BlastSearchRecord)) As IEnumerable(Of OwnHsp)
            Dim IndexOfRecord As Integer = -1
            For Each Record In ClonedAndFilteredBlastSearchRecords
                IndexOfRecord += 1
                Dim IndexOfHit As Integer = -1
                For Each Hit In Record.Hits
                    IndexOfHit += 1
                    Dim IndexOfHSP As Integer = -1
                    For Each HSP In Hit.Hsps
                        IndexOfHSP += 1
                        Yield New OwnHsp(HSP, IndexOfRecord, IndexOfHit, IndexOfHSP)
                    Next
                Next
            Next
        End Function
        Public Shared Function Clear(ClonedAndFilteredBlastSearchRecords As List(Of BlastSearchRecord)) As List(Of BlastSearchRecord)
            For i = ClonedAndFilteredBlastSearchRecords.Count - 1 To 0 Step -1
                Dim bRecord = ClonedAndFilteredBlastSearchRecords(i)
                For i1 = bRecord.Hits.Count - 1 To 0 Step -1
                    For i2 = bRecord.Hits(i1).Hsps.Count - 1 To 0 Step -1
                        If IsNothing(bRecord.Hits(i1).Hsps(i2)) = True Then
                            bRecord.Hits(i1).Hsps.RemoveAt(i2)
                        End If
                    Next
                    If bRecord.Hits(i1).Hsps.Count = 0 Then
                        ClonedAndFilteredBlastSearchRecords(i).Hits.RemoveAt(i1)
                    End If

                Next
                If bRecord.Hits.Count = 0 Then
                    ClonedAndFilteredBlastSearchRecords.RemoveAt(i)
                End If
            Next
            Return ClonedAndFilteredBlastSearchRecords
        End Function

        Public Shared Function Clear(ClonedAndFilteredBlastSearchRecords As List(Of Szunyi.BLAST.OwnBlastRecord)) As List(Of Szunyi.BLAST.OwnBlastRecord)
            For i = ClonedAndFilteredBlastSearchRecords.Count - 1 To 0 Step -1
                Dim bRecord = ClonedAndFilteredBlastSearchRecords(i)
                For i1 = bRecord.Hits.Count - 1 To 0 Step -1
                    For i2 = bRecord.Hits(i1).Hsps.Count - 1 To 0 Step -1
                        If IsNothing(bRecord.Hits(i1).Hsps(i2)) = True Then
                            bRecord.Hits(i1).Hsps.RemoveAt(i2)
                            bRecord.OwnHits(i1).OwnHsps.RemoveAt(i2)
                            '    bRecord.OwnHits(i1).Hsps.RemoveAt(i2)
                        End If
                    Next
                    If bRecord.Hits(i1).Hsps.Count <> bRecord.OwnHits(i1).OwnHsps.Count Then
                        Dim kj As Int16 = 43
                    End If
                    If bRecord.Hits(i1).Hsps.Count = 0 Then
                        ClonedAndFilteredBlastSearchRecords(i).Hits.RemoveAt(i1)
                        ClonedAndFilteredBlastSearchRecords(i).OwnHits.RemoveAt(i1)
                    End If

                Next
                If bRecord.Hits.Count = 0 Then
                    ClonedAndFilteredBlastSearchRecords.RemoveAt(i)
                End If
            Next
            Return ClonedAndFilteredBlastSearchRecords
        End Function

        ''' <summary>
        ''' Filter Out Hsps which e-value is lower than
        ''' </summary>
        ''' <param name="ClonedAndFilteredBlastSearchRecords"></param>
        ''' <param name="evalue"></param>
        ''' <returns></returns>
        Public Shared Function E_Value_IsLower(ClonedAndFilteredBlastSearchRecords As List(Of BlastSearchRecord),
                                                          evalue As Double) As List(Of Bio.Web.Blast.BlastSearchRecord)
            Dim ToDelete As New List(Of OwnHsp)
            For Each HSP In Parse_HSPs(ClonedAndFilteredBlastSearchRecords)
                If HSP.HSP.EValue > evalue Then ToDelete.Add(HSP)
            Next
            Set_Hsp_Nothing(ClonedAndFilteredBlastSearchRecords, ToDelete)

            Return Clear(ClonedAndFilteredBlastSearchRecords)

        End Function
        Private Shared Sub Set_Hsp_Nothing(ClonedAndFilteredBlastSearchRecords As List(Of BlastSearchRecord), Hsps As IEnumerable(Of OwnHsp))
            For i1 = Hsps.Count - 1 To 0 Step -1
                Dim HSP = Hsps(i1)
                ClonedAndFilteredBlastSearchRecords(HSP.RecordID).Hits(HSP.HitID).Hsps(HSP.HSPID) = Nothing
            Next

        End Sub
        Private Shared Sub Set_Hsp_Nothing(ClonedAndFilteredBlastSearchRecords As List(Of BlastSearchRecord), Hsp As OwnHsp)
            ClonedAndFilteredBlastSearchRecords(Hsp.RecordID).Hits(Hsp.HitID).Hsps(Hsp.HSPID) = Nothing
        End Sub
        ''' <summary>
        ''' Filter Out Hsps which e-value is lower than
        ''' </summary>
        ''' <param name="ClonedAndFilteredBlastSearchRecords"></param>
        ''' <param name="evalue"></param>
        ''' <returns></returns>
        Public Shared Function E_Value_IsHigher(ClonedAndFilteredBlastSearchRecords As List(Of BlastSearchRecord),
                                                          evalue As Double) As List(Of Bio.Web.Blast.BlastSearchRecord)
            For Each HSP In Parse_HSPs(ClonedAndFilteredBlastSearchRecords)
                If HSP.HSP.EValue < evalue Then
                    HSP = Nothing
                End If
            Next
            Return Clear(ClonedAndFilteredBlastSearchRecords)

        End Function

    End Class

    Public Shared Function Filter_Hits(FSs As List(Of Filter_Setting), clonedAndFilteredBlastSearchRecords As List(Of BlastSearchRecord)) As List(Of BlastSearchRecord)
        Dim ToDelete As New List(Of OwnHit)
        For Each Item In Filter.Hit.Parse_Hits(clonedAndFilteredBlastSearchRecords)
            Dim Passed As Boolean = True
            For Each Fs In FSs
                Select Case Fs.EnumValue
                    Case Szunyi.Common.Enums.Filter.Bigger
                        If Not (Item.hit.Current_Value(Fs.EnumValue).ToDouble > Fs.Value.ToDouble) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Bigger_or_Equal
                        If Not (Item.hit.Current_Value(Fs.EnumValue).ToDouble >= Fs.Value.ToDouble) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Contain
                        If Not (Item.Hit.Current_Value(Fs.EnumValue).Contains(Fs.Value)) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.End_with
                        If Not (Item.Hit.Current_Value(Fs.EnumValue).EndsWith(Fs.Value)) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Equal
                        If Not (Item.Hit.Current_Value(Fs.EnumValue) = (Fs.Value)) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Not_Contain
                        If Not (Item.Hit.Current_Value(Fs.EnumValue).Contains(Fs.Value)) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Not_End_with
                        If (Item.Hit.Current_Value(Fs.EnumValue).EndsWith(Fs.Value)) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Not_Start_with
                        If (Item.Hit.Current_Value(Fs.EnumValue).StartsWith(Fs.Value)) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Smaller
                        If Not (Item.hit.Current_Value(Fs.EnumValue).ToDouble < Fs.Value.ToDouble) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Smaller_or_Equal
                        If Not (Item.hit.Current_Value(Fs.EnumValue).ToDouble <= Fs.Value.ToDouble) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Start_with
                        If Not (Item.hit.Current_Value(Fs.EnumValue).StartsWith(Fs.Value)) Then
                            Passed = False
                            Exit For
                        End If
                    Case Else
                End Select
            Next ' End of FIrst Prop
            If Passed = False Then
                ToDelete.Add(Item)
            Else
                Dim kj As Int16 = 54
            End If
        Next
        Dim x = Filter.Hit.Set_Nothing(clonedAndFilteredBlastSearchRecords, ToDelete)
        Return Filter.Hit.Clear(x)

    End Function
    Public Shared Function Filter_Hits(FSs As List(Of Filter_Setting), clonedAndFilteredBlastSearchRecords As List(Of OwnBlastRecord)) As List(Of OwnBlastRecord)
        Dim ToDelete As New List(Of OwnHit)
        For Each Item In Filter.Hit.Parse_Hits(clonedAndFilteredBlastSearchRecords)
            Dim Passed As Boolean = True
            For Each Fs In FSs
                Select Case Fs.EnumValue
                    Case Szunyi.Common.Enums.Filter.Bigger
                        If Not (Item.hit.Current_Value(Fs.EnumValue).ToDouble > Fs.Value.ToDouble) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Bigger_or_Equal
                        If Not (Item.hit.Current_Value(Fs.EnumValue).ToDouble >= Fs.Value.ToDouble) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Contain
                        If Not (Item.hit.Current_Value(Fs.EnumValue).Contains(Fs.Value)) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.End_with
                        If Not (Item.hit.Current_Value(Fs.EnumValue).EndsWith(Fs.Value)) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Equal
                        If Not (Item.hit.Current_Value(Fs.EnumValue) = (Fs.Value)) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Not_Contain
                        If Not (Item.hit.Current_Value(Fs.EnumValue).Contains(Fs.Value)) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Not_End_with
                        If (Item.hit.Current_Value(Fs.EnumValue).EndsWith(Fs.Value)) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Not_Start_with
                        If (Item.hit.Current_Value(Fs.EnumValue).StartsWith(Fs.Value)) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Smaller
                        If Not (Item.hit.Current_Value(Fs.EnumValue).ToDouble < Fs.Value.ToDouble) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Smaller_or_Equal
                        If Not (Item.hit.Current_Value(Fs.EnumValue).ToDouble <= Fs.Value.ToDouble) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Start_with
                        If Not (Item.hit.Current_Value(Fs.EnumValue).StartsWith(Fs.Value)) Then
                            Passed = False
                            Exit For
                        End If
                    Case Else
                End Select
            Next ' End of FIrst Prop
            If Passed = False Then
                ToDelete.Add(Item)
            Else
                Dim kj As Int16 = 54
            End If
        Next
        Dim x = Filter.Hit.Set_Nothing(clonedAndFilteredBlastSearchRecords, ToDelete)
        Return Filter.Hit.Clear(x)

    End Function

    Public Shared Function Filter_Records(FSs As List(Of Filter_Setting), ClonedAndFilteredBlastSearchRecords As IEnumerable(Of BlastSearchRecord)) As IEnumerable(Of BlastSearchRecord)

        Dim c As New List(Of BlastSearchRecord)
        For Each Item In ClonedAndFilteredBlastSearchRecords
            Dim Passed As Boolean = True
            For Each Fs In FSs
                Select Case Fs.EnumValue
                    Case Szunyi.Common.Enums.Filter.Bigger
                        If Not (Item.Current_Value(Fs.EnumValue).ToDouble > Fs.Value.ToDouble) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Bigger_or_Equal
                        If Not (Item.Current_Value(Fs.EnumValue).ToDouble >= Fs.Value.ToDouble) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Contain
                        If Not (Item.Current_Value(Fs.EnumValue).Contains(Fs.Value)) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.End_with
                        If Not (Item.Current_Value(Fs.EnumValue).EndsWith(Fs.Value)) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Equal
                        If Not (Item.Current_Value(Fs.EnumValue) = (Fs.Value)) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Not_Contain
                        If Not (Item.Current_Value(Fs.EnumValue).Contains(Fs.Value)) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Not_End_with
                        If (Item.Current_Value(Fs.EnumValue).EndsWith(Fs.Value)) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Not_Start_with
                        If (Item.Current_Value(Fs.EnumValue).StartsWith(Fs.Value)) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Smaller
                        If Not (Item.Current_Value(Fs.EnumValue).ToDouble < Fs.Value.ToDouble) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Smaller_or_Equal
                        If Not (Item.Current_Value(Fs.EnumValue).ToDouble <= Fs.Value.ToDouble) Then
                            Passed = False
                            Exit For
                        End If
                    Case Szunyi.Common.Enums.Filter.Start_with
                        If Not (Item.Current_Value(Fs.EnumValue).StartsWith(Fs.Value)) Then
                            Passed = False
                            Exit For
                        End If
                    Case Else
                End Select
            Next ' End of FIrst Prop
            If Passed = True Then
                c.Add(Item.Clone)
            End If
        Next
        Return c
    End Function

    Public Shared Function GetFilteredBlastSearchRecordsNearHitEnd5(ClonedAndFilteredBlastSearchRecords As List(Of BlastSearchRecord), NearEnd As Integer) As List(Of BlastSearchRecord)
        Dim res As New List(Of BlastSearchRecord)

        Try
            For Each bRecord In ClonedAndFilteredBlastSearchRecords
                res.Add(bRecord)
                For i1 = res.Last.Hits.Count - 1 To 0 Step -1
                    For i2 = res.Last.Hits(i1).Hsps.Count - 1 To 0 Step -1
                        If res.Last.Hits(i1).Hsps(i2).HitEnd + NearEnd > res.Last.Hits(i1).Length Or
                            res.Last.Hits(i1).Hsps(i2).HitStart + NearEnd > res.Last.Hits(i1).Length Then
                        Else
                            res.Last.Hits(i1).Hsps.RemoveAt(i2)
                        End If

                    Next ' end hsps
                    If res.Last.Hits(i1).Hsps.Count = 0 Then
                        res.Last.Hits.RemoveAt(i1)
                    End If

                Next
                If res.Last.Hits.Count = 0 Then
                    res.Remove(res.Last)
                End If
            Next

        Catch ex As Exception
            Dim alf As Integer = 43
        End Try

        MsgBox(res.Count)

        Return res
    End Function
    Public Shared Function GetFilteredBlastSearchRecordsNearEnd(ClonedAndFilteredBlastSearchRecords As List(Of BlastSearchRecord), NearEnd As Integer) As List(Of BlastSearchRecord)
        Dim res As New List(Of BlastSearchRecord)

        Try
            For Each bRecord In ClonedAndFilteredBlastSearchRecords
                res.Add(bRecord)
                For i1 = res.Last.Hits.Count - 1 To 0 Step -1
                    For i2 = res.Last.Hits(i1).Hsps.Count - 1 To 0 Step -1
                        If res.Last.Hits(i1).Hsps(i2).QueryEnd + NearEnd < res.Last.IterationQueryLength Then
                            res.Last.Hits(i1).Hsps.RemoveAt(i2)
                        Else
                            If res.Last.Hits(i1).Hsps(i2).HitFrame = 1 Then
                                If res.Last.Hits(i1).Hsps(i2).HitEnd + 20 > res.Last.Hits(i1).Length Then
                                    res.Last.Hits(i1).Hsps.RemoveAt(i2)
                                End If
                            ElseIf res.Last.Hits(i1).Hsps(i2).HitFrame = -1 Then

                                If res.Last.Hits(i1).Hsps(i2).HitEnd < 20 Then
                                    res.Last.Hits(i1).Hsps.RemoveAt(i2)
                                End If
                            End If

                        End If
                    Next ' end hsps
                    If res.Last.Hits(i1).Hsps.Count = 0 Then
                        res.Last.Hits.RemoveAt(i1)
                    End If

                Next
                If res.Last.Hits.Count = 0 Then
                    res.Remove(res.Last)
                End If
            Next

        Catch ex As Exception
            Dim alf As Integer = 43
        End Try

        MsgBox(res.Count)

        Return res
    End Function
    Public Shared Function Get_Records_Same_Lengths(clonedAndFilteredBlastSearchRecords As List(Of BlastSearchRecord), diffPercent As Double) As List(Of BlastSearchRecord)
        Dim t As New List(Of BlastSearchRecord)

        For Each Item In clonedAndFilteredBlastSearchRecords
            t.Add(Item)
            For i1 = t.Last.Hits.Count - 1 To 0 Step -1
                For i2 = t.Last.Hits(i1).Hsps.Count - 1 To 0 Step -1

                    Dim HSP = t.Last.Hits(i1).Hsps(i2)
                    If (Item.IterationQueryLength / t.Last.Hits(i1).Length) * 100 > 100 - diffPercent And (Item.IterationQueryLength / t.Last.Hits(i1).Length) * 100 < 100 + diffPercent Then

                    Else
                        t.Last.Hits(i1).Hsps.RemoveAt(i2)
                    End If

                Next
                If t.Last.Hits(i1).Hsps.Count = 0 Then t.Last.Hits.RemoveAt(i1)
            Next
            If t.Last.Hits.Count = 0 Then t.RemoveAt(t.Count - 1)
        Next
        Return t
    End Function
#Region "By Hit Count"
    Public Shared Function GetRecordswoHits(BlastSearchRecords As List(Of BlastSearchRecord)) As List(Of Bio.Web.Blast.BlastSearchRecord)
        Dim res = From x In BlastSearchRecords Where x.Hits.Count = 0

        If res.Count > 0 Then Return res.ToList
        Return New List(Of Bio.Web.Blast.BlastSearchRecord)

    End Function

    ''' <summary>
    ''' Retrun Records Which Have Hits
    ''' </summary>
    ''' <param name="BlastSearchRecords"></param>
    ''' <returns></returns>
    Public Shared Function GetRecordswithHits(BlastSearchRecords As List(Of BlastSearchRecord)) _
        As List(Of Bio.Web.Blast.BlastSearchRecord)
        Dim res = From x In BlastSearchRecords Where x.Hits.Count > 0

        If res.Count > 0 Then Return res.ToList
        Return New List(Of Bio.Web.Blast.BlastSearchRecord)

    End Function

#End Region

#Region "Get Records wPerfect Hits Return All Hits"
    ''' <summary>
    ''' IdentityCount Of HSP = HitLength, Return All Hits
    ''' </summary>
    ''' <param name="clonedAndFilteredBlastSearchRecords"></param>
    ''' <returns></returns>
    Public Shared Function GetRecordswPerfectHits_Identity_Hit(clonedAndFilteredBlastSearchRecords As List(Of BlastSearchRecord)) As List(Of BlastSearchRecord)
        Dim res As New List(Of BlastSearchRecord)
        For Each bRecord In clonedAndFilteredBlastSearchRecords
            res.Add(bRecord)
            Dim HasPerfect As Boolean = False
            For i1 = res.Last.Hits.Count - 1 To 0 Step -1
                For i2 = res.Last.Hits(i1).Hsps.Count - 1 To 0 Step -1
                    If res.Last.Hits(i1).Hsps(i2).IdentitiesCount = res.Last.Hits(i1).Length Then
                        HasPerfect = True
                    End If
                Next
            Next
            If HasPerfect = False Then res.RemoveAt(res.Count - 1)
        Next
        Return res
    End Function

    ''' <summary>
    ''' IdentityCount Of HSP = QueryLength, Return All Hits
    ''' </summary>
    ''' <param name="clonedAndFilteredBlastSearchRecords"></param>
    ''' <returns></returns>
    Public Shared Function GetRecordswPerfectHits_Identity_Query(clonedAndFilteredBlastSearchRecords As List(Of BlastSearchRecord)) As List(Of BlastSearchRecord)
        Dim res As New List(Of BlastSearchRecord)
        For Each bRecord In clonedAndFilteredBlastSearchRecords
            res.Add(bRecord)
            Dim HasPerfect As Boolean = False
            For i1 = res.Last.Hits.Count - 1 To 0 Step -1
                For i2 = res.Last.Hits(i1).Hsps.Count - 1 To 0 Step -1
                    If res.Last.Hits(i1).Hsps(i2).IdentitiesCount = bRecord.IterationQueryLength Then
                        HasPerfect = True
                    End If
                Next
            Next
            If HasPerfect = False Then res.RemoveAt(res.Count - 1)
        Next
        Return res
    End Function

    ''' <summary>
    ''' IdentityCount of HSP = HitLength = QueryLength
    ''' </summary>
    ''' <param name="clonedAndFilteredBlastSearchRecords"></param>
    ''' <returns></returns>
    Public Shared Function GetRecordswPerfectHits_Identity_Query_Hit(clonedAndFilteredBlastSearchRecords As List(Of BlastSearchRecord)) As List(Of BlastSearchRecord)
        Dim res As New List(Of BlastSearchRecord)
        For Each bRecord In clonedAndFilteredBlastSearchRecords
            res.Add(bRecord)
            Dim HasPerfect As Boolean = False
            For i1 = res.Last.Hits.Count - 1 To 0 Step -1
                For i2 = res.Last.Hits(i1).Hsps.Count - 1 To 0 Step -1
                    If res.Last.Hits(i1).Hsps(i2).IdentitiesCount = bRecord.IterationQueryLength = res.Last.Hits(i1).Length Then
                        HasPerfect = True
                    End If
                Next
            Next
            If HasPerfect = False Then res.RemoveAt(res.Count - 1)
        Next
        Return res
    End Function
#End Region

#Region "Get Records WithOut Perfect Hits"
    ''' <summary>
    ''' Return Records WithOut Perfect Match IdentityCount = HitLength
    ''' </summary>
    ''' <param name="clonedAndFilteredBlastSearchRecords"></param>
    ''' <returns></returns>
    Public Shared Function GetRecordswoPerfectHits_Identity_Hit(clonedAndFilteredBlastSearchRecords As List(Of BlastSearchRecord)) As List(Of BlastSearchRecord)
        Dim res As New List(Of BlastSearchRecord)
        For Each bRecord In clonedAndFilteredBlastSearchRecords
            res.Add(bRecord)
            Dim HasPerfect As Boolean = False
            For i1 = res.Last.Hits.Count - 1 To 0 Step -1
                For i2 = res.Last.Hits(i1).Hsps.Count - 1 To 0 Step -1
                    If res.Last.Hits(i1).Hsps(i2).IdentitiesCount = res.Last.Hits(i1).Length Then
                        HasPerfect = True
                    End If
                Next
            Next
            If HasPerfect = True Then res.RemoveAt(res.Count - 1)
        Next
        Return res
    End Function

    ''' <summary>
    ''' Return Records WithOut Perfect Match IdentityCount=HitLength=QueryLength
    ''' </summary>
    ''' <param name="clonedAndFilteredBlastSearchRecords"></param>
    ''' <returns></returns>
    Public Shared Function GetRecordswoPerfectHits_Identity_Query_Hit(clonedAndFilteredBlastSearchRecords As List(Of BlastSearchRecord)) As List(Of BlastSearchRecord)
        Dim res As New List(Of BlastSearchRecord)
        For Each bRecord In clonedAndFilteredBlastSearchRecords
            res.Add(bRecord)
            Dim HasPerfect As Boolean = False
            For i1 = res.Last.Hits.Count - 1 To 0 Step -1
                For i2 = res.Last.Hits(i1).Hsps.Count - 1 To 0 Step -1
                    If res.Last.Hits(i1).Hsps(i2).IdentitiesCount = res.Last.Hits(i1).Length And
                        res.Last.Hits(i1).Hsps(i2).IdentitiesCount = bRecord.IterationQueryLength Then
                        HasPerfect = True
                    End If
                Next
            Next
            If HasPerfect = True Then res.RemoveAt(res.Count - 1)
        Next
        Return res
    End Function

    ''' <summary>
    ''' Reture Records WithOut Perfex Match IdentityCount = QueryLength
    ''' </summary>
    ''' <param name="clonedAndFilteredBlastSearchRecords"></param>
    ''' <returns></returns>
    Public Shared Function GetRecordswoPerfectHits_Identity_Query(clonedAndFilteredBlastSearchRecords As List(Of BlastSearchRecord)) As List(Of BlastSearchRecord)
        Dim res As New List(Of BlastSearchRecord)
        For Each bRecord In clonedAndFilteredBlastSearchRecords
            res.Add(bRecord)
            Dim HasPerfect As Boolean = False
            For i1 = res.Last.Hits.Count - 1 To 0 Step -1
                For i2 = res.Last.Hits(i1).Hsps.Count - 1 To 0 Step -1
                    If res.Last.Hits(i1).Hsps(i2).IdentitiesCount = bRecord.IterationQueryLength Then
                        HasPerfect = True
                    End If
                Next
            Next
            If HasPerfect = True Then res.RemoveAt(res.Count - 1)
        Next
        Return res
    End Function


#End Region

#Region "Remove Not Perfect Hits"
    ''' <summary>
    ''' Return Only Perfect Matches IdentityCount = HitLength
    ''' </summary>
    ''' <param name="BlastSearchRecords"></param>
    ''' <returns></returns>
    Public Shared Function RemoveNotPerfectMatchesByHitLength(BlastSearchRecords As List(Of BlastSearchRecord)) _
        As List(Of BlastSearchRecord)
        Dim res As New List(Of BlastSearchRecord)
        For Each bRecord In BlastSearchRecords
            res.Add(bRecord)
            Dim HasPerfect As Boolean = False
            For i1 = res.Last.Hits.Count - 1 To 0 Step -1
                For i2 = res.Last.Hits(i1).Hsps.Count - 1 To 0 Step -1
                    If res.Last.Hits(i1).Hsps(i2).IdentitiesCount <> res.Last.Hits(i1).Length Then
                        res.Last.Hits(i1).Hsps.RemoveAt(i2)
                    End If
                Next
                If res.Last.Hits(i1).Hsps.Count = 0 Then
                    res.Last.Hits.RemoveAt(i1)
                End If
            Next
            If res.Last.Hits.Count = 0 Then
                res.RemoveAt(res.Count - 1)
            End If
        Next
        Return res
    End Function

    ''' <summary>
    ''' Return Only Perfect Matches IdentityCount = QueryLength
    ''' </summary>
    ''' <param name="BlastSearchRecords"></param>
    ''' <returns></returns>
    Public Shared Function RemoveNotPerfectMatchesByQueryLength(BlastSearchRecords As List(Of BlastSearchRecord)) _
        As List(Of BlastSearchRecord)
        Dim res As New List(Of BlastSearchRecord)
        For Each bRecord In BlastSearchRecords
            res.Add(bRecord)
            Dim HasPerfect As Boolean = False
            For i1 = res.Last.Hits.Count - 1 To 0 Step -1
                For i2 = res.Last.Hits(i1).Hsps.Count - 1 To 0 Step -1
                    If res.Last.Hits(i1).Hsps(i2).IdentitiesCount <> bRecord.IterationQueryLength Then
                        res.Last.Hits(i1).Hsps.RemoveAt(i2)
                    End If
                Next
                If res.Last.Hits(i1).Hsps.Count = 0 Then
                    res.Last.Hits.RemoveAt(i1)
                End If
            Next
            If res.Last.Hits.Count = 0 Then
                res.RemoveAt(res.Count - 1)
            End If
        Next
        Return res
    End Function

    ''' <summary>
    ''' Return Only Perfect Matches IdentityCount = QueryLength = HitLength
    ''' </summary>
    ''' <param name="BlastSearchRecords"></param>
    ''' <returns></returns>
    Public Shared Function RemoveNotPerfectMatchesByHitLengthAndQueryLength(BlastSearchRecords As List(Of BlastSearchRecord)) _
        As List(Of BlastSearchRecord)
        Dim res As New List(Of BlastSearchRecord)
        For Each bRecord In BlastSearchRecords
            res.Add(bRecord)
            Dim HasPerfect As Boolean = False
            For i1 = res.Last.Hits.Count - 1 To 0 Step -1
                For i2 = res.Last.Hits(i1).Hsps.Count - 1 To 0 Step -1
                    If res.Last.Hits(i1).Hsps(i2).IdentitiesCount <> res.Last.Hits(i1).Length Or
                        res.Last.Hits(i1).Hsps(i2).IdentitiesCount <> bRecord.IterationQueryLength Then
                        res.Last.Hits(i1).Hsps.RemoveAt(i2)
                    End If
                Next
                If res.Last.Hits(i1).Hsps.Count = 0 Then
                    res.Last.Hits.RemoveAt(i1)
                End If
            Next
            If res.Last.Hits.Count = 0 Then
                res.RemoveAt(res.Count - 1)
            End If
        Next
        Return res
    End Function

    Public Shared Function GetFilteredBlastSearchRecordsByHit(QueryString As String,
                                                              BlastSearchRecords As List(Of BlastSearchRecord)) _
        As List(Of BlastSearchRecord)

        Dim res As New List(Of BlastSearchRecord)
        For Each bRecord In BlastSearchRecords
            res.Add(bRecord)
            Dim HasPerfect As Boolean = False
            For i1 = res.Last.Hits.Count - 1 To 0 Step -1
                If res.Last.Hits(i1).Def.Contains(QueryString) = False AndAlso
                        res.Last.Hits(i1).Id.Contains(QueryString) = False Then
                    res.Last.Hits.RemoveAt(i1)
                End If

            Next
            If res.Last.Hits.Count = 0 Then
                res.RemoveAt(res.Count - 1)
            End If
        Next
        Return res
    End Function


#End Region

#Region "By Query Definiton"
    Public Shared Function Discard_Equal_Query_Definitions(BlastSearchRecords As List(Of BlastSearchRecord), query_IDs As List(Of String)) As List(Of BlastSearchRecord)
        Dim res As New List(Of BlastSearchRecord)
        For Each bRecord In BlastSearchRecords
            Dim Index = query_IDs.BinarySearch(bRecord.IterationQueryDefinition)
            If Index < 0 Then
                res.Add(bRecord)
            End If
        Next
        Return res
    End Function

    Public Shared Function Discard_Contain_Query_Definitions(BlastSearchRecords As List(Of BlastSearchRecord), query_IDs As List(Of String)) As List(Of BlastSearchRecord)
        Dim res As New List(Of BlastSearchRecord)
        For Each bRecord In BlastSearchRecords
            If Is_Query_Definition_Contain_One_of_The_Id(bRecord, query_IDs) = False Then
                res.Add(bRecord)
            End If
        Next
        Return res
    End Function

    Public Shared Function Discard_Start_With_Query_Definitions(BlastSearchRecords As List(Of BlastSearchRecord), query_IDs As List(Of String)) As List(Of BlastSearchRecord)
        Dim res As New List(Of BlastSearchRecord)
        For Each bRecord In BlastSearchRecords
            If Is_Query_Definition_Start_With_One_of_The_Id(bRecord, query_IDs) = False Then
                res.Add(bRecord)
            End If
        Next
        Return res
    End Function


    Public Shared Function Maintain_Equal_With_Query_Definitions(BlastSearchRecords As List(Of BlastSearchRecord), query_IDs As List(Of String)) As List(Of BlastSearchRecord)
        Dim res As New List(Of BlastSearchRecord)
        For Each bRecord In BlastSearchRecords
            Dim Index = query_IDs.BinarySearch(bRecord.IterationQueryDefinition)
            If Index > -1 Then
                res.Add(bRecord)
            End If
        Next
        Return res
    End Function

    Public Shared Function Maintain_Contain_With_Query_Definitions(BlastSearchRecords As List(Of BlastSearchRecord), query_IDs As List(Of String)) As List(Of BlastSearchRecord)
        Dim res As New List(Of BlastSearchRecord)
        For Each bRecord In BlastSearchRecords
            If Is_Query_Definition_Contain_One_of_The_Id(bRecord, query_IDs) = True Then
                res.Add(bRecord)
            End If
        Next
        Return res
    End Function

    Public Shared Function Maintain_Start_With_Query_Definitions(BlastSearchRecords As List(Of BlastSearchRecord), query_IDs As List(Of String)) As List(Of BlastSearchRecord)
        Dim res As New List(Of BlastSearchRecord)
        For Each bRecord In BlastSearchRecords
            If Is_Query_Definition_Contain_One_of_The_Id(bRecord, query_IDs) = True Then
                res.Add(bRecord)
            End If
        Next
        Return res
    End Function

    Public Shared Function Is_Query_Definition_Contain_One_of_The_Id(x As BlastSearchRecord, query_IDs As List(Of String)) As Boolean
        For Each Query_id In query_IDs
            If x.IterationQueryDefinition.IndexOf(Query_id, comparisonType:=StringComparison.InvariantCultureIgnoreCase) > -1 Then Return True
        Next
        Return False
    End Function
    Public Shared Function Is_Query_Definition_Start_With_One_of_The_Id(x As BlastSearchRecord, query_IDs As List(Of String)) As Boolean
        For Each Query_id In query_IDs
            If x.IterationQueryDefinition.IndexOf(Query_id, comparisonType:=StringComparison.InvariantCultureIgnoreCase) = 0 Then Return True
        Next
        Return False
    End Function

    Public Shared Function Maintain_Opposite_Hit_Frames(clonedAndFilteredBlastSearchRecords As List(Of BlastSearchRecord)) As List(Of BlastSearchRecord)
        Dim ToRemoveBS As New List(Of BlastSearchRecord)
        For Each R In clonedAndFilteredBlastSearchRecords
            Dim toRemove As New List(Of Bio.Web.Blast.Hit)
            For Each Hit In R.Hits
                Dim HASFw As Boolean = False
                Dim HasRev As Boolean = False

                For Each hsp In Hit.Hsps
                    If hsp.HitFrame < 0 Then
                        HasRev = True
                    Else
                        HASFw = True
                    End If
                Next
                If HASFw = False Or HasRev = False Then
                    toRemove.Add(Hit)
                End If
            Next
            For Each Item In toRemove
                R.Hits.Remove(Item)
            Next
            If R.Hits.Count = 0 Then ToRemoveBS.Add(R)
        Next
        For Each Item In ToRemoveBS
            clonedAndFilteredBlastSearchRecords.Remove(Item)
        Next
        Return clonedAndFilteredBlastSearchRecords
    End Function
    Public Shared Function Remove_Single_HSPs(clonedAndFilteredBlastSearchRecords As List(Of BlastSearchRecord)) As List(Of BlastSearchRecord)
        Dim t As New List(Of BlastSearchRecord)
        Dim ToRemoveBS As New List(Of BlastSearchRecord)
        For Each R In clonedAndFilteredBlastSearchRecords
            Dim ToRemove As New List(Of Bio.Web.Blast.Hit)
            For Each Hit In R.Hits
                If Hit.Hsps.Count = 1 Then ToRemove.Add(Hit)
            Next
            For Each t1 In ToRemove
                R.Hits.Remove(t1)
            Next
            If R.Hits.Count = 0 Then ToRemoveBS.Add(R)
        Next
        For Each Item In ToRemoveBS
            clonedAndFilteredBlastSearchRecords.Remove(Item)
        Next
        Return clonedAndFilteredBlastSearchRecords
    End Function
    Private Shared Function Remove_OverLapped_HSPs(HSPs As IList(Of Bio.Web.Blast.Hsp), maxOverLap As Integer) As IList(Of HSP)
        Dim out As New List(Of Bio.Web.Blast.Hsp)
        If HSPs.Count = 0 Then Return HSPs
        Dim cHSPs = (From x In HSPs Order By x.QueryStart Ascending).ToList
        Dim toRemove As New List(Of Integer)
        For i1 = 0 To HSPs.Count - 1
            For i2 = 0 To HSPs.Count - 1
                If i1 < i2 Then
                    If cHSPs(i1).QueryEnd > cHSPs(i2).QueryStart AndAlso (cHSPs(i1).QueryEnd - cHSPs(i2).QueryStart) >= maxOverLap Then
                        toRemove.Add(i1)
                        toRemove.Add(i2)
                    End If
                End If
            Next
        Next
        If toRemove.Count = 0 Then Return HSPs

        toRemove = toRemove.Distinct.ToList
        toRemove.Sort()
        toRemove.Reverse()
        For Each Item In toRemove
            cHSPs.RemoveAt(Item)
        Next
        If cHSPs.Count > 1 Then
            Dim kj As Int16 = 54
        End If
        Return cHSPs
    End Function
#End Region
End Class
