

subroutine main()
  integer :: rank, v, i, els

  integer, allocatable, dimension(:) :: send_data, recv_data

  els=10

  rank=MPI_CommRank()
  if (rank == 0) then
    allocate(send_data(els))  
    do i=1, els
      send_data(i)=i
    end do
    call MPI_Send(send_data, els, 1, 0)
    print *, rank, "Sent ", els, " elements"
    deallocate(send_data)    
  end if

  if (rank == 1) then
    allocate(recv_data(els))
    call MPI_Recv(recv_data, els, 0, 0)
    do i=1, els
      v=recv_data(i)
      print *, "Data: ", i, "=", v
    end do
    deallocate(recv_data)  
  end if
end subroutine main


!program mpi_test
!use mpi_tester
!implicit none

!  call test()

!end program mpi_test