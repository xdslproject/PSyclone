subroutine gauss_seidel(data)
    real*8, dimension(256,256), intent(inout) :: data
    integer*4 :: i
    integer*4 :: j
    integer*4 :: k

    do k = 1, 1000, 1
      do i = 2, 255, 1
        do j = 2, 255, 1
          data(j,i) = (data(j,i - 1) + data(j,i + 1) + data(j - 1,i) + data(j + 1,i)) * 0.25
        enddo
      enddo
    enddo

end subroutine gauss_seidel