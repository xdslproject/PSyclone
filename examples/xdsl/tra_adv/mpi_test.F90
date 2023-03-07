

subroutine main()
  integer :: sv, rv, rank, comm_size, rankp1, rankm1

  rank=MPI_CommRank()
  comm_size=MPI_CommSize()
  if (rank .lt. comm_size -1) then
    sv=rank*100
    rankp1=rank+1
    call MPI_Send(sv, 1, rankp1, 0)
  end if

  if (rank .gt. 0) then
    rankm1=rank-1
    call MPI_Recv(rv, 1, rankm1, 0)
    print *, "Received message at ", rank, " from ", rankm1, " value: ", rv
  end if
end subroutine main


!program mpi_test
!use mpi_tester
!implicit none

!  call test()

!end program mpi_test