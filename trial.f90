program main
  implicit none
  real(8) z
  integer i
  z=0
  do i=1, 10
    if(i==4)then
      z=z+20.0
    endif
    z=z+1.0
  enddo

  write(*,*)z
end program main
