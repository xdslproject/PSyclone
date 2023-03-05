

subroutine test()
  integer :: a, b, c

  a=MPI_CommRank()
  if (a==0) then
    b=100
    call MPI_Send(b, 1, 1, 0)
  else
    call MPI_Recv(c, 1, 0, 0)
    print *, "Received message ", c
  end if
end subroutine test


!program mpi_test
!use mpi_tester
!implicit none

!  call test()

!end program mpi_test