module calc_variables
  implicit none
contains
  function calc_bathy(vertice_index, vertice_matrix, vertice_n) result(bathy)
    integer vertice_n
    real(8), intent(in)::vertice_matrix(vertice_n,3)
    integer, intent(in)::vertice_index(vertice_n)
    integer i
    real(8)::z=0.0d0, bathy
    integer::count=0

    do i=1, vertice_n
      if(vertice_index(i)==1)then
        z=z+vertice_matrix(i,3)
        count=count+1
      endif
    enddo
    bathy=z/count
  end function calc_bathy


end module calc_variables
