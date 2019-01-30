Imports Szunyi.BLAST

Public Class Sort
    Public Class cOwnBlastRecord
        Implements IComparer(Of OwnBlastRecord)

        Public Function Compare(x As OwnBlastRecord, y As OwnBlastRecord) As Integer Implements IComparer(Of OwnBlastRecord).Compare

            Return x.IterationQueryDefinition.CompareTo(y.IterationQueryDefinition)

        End Function
    End Class

    Public Class cextHSPs
        Implements IComparer(Of extHSP)

        Public Function Compare(x As extHSP, y As extHSP) As Integer Implements IComparer(Of extHSP).Compare

            If x.Record.IterationQueryDefinition <> y.Record.IterationQueryDefinition Then
                Return x.Record.IterationQueryDefinition.CompareTo(y.Record.IterationQueryDefinition)
            End If

        End Function
    End Class
End Class
