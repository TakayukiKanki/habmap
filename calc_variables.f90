module calc_variables
  implicit none
contains
  function calc_bathy(vertice_index, vertice_matrix, vertice_n) result(bathy)
    integer vertice_n
    real(8), intent(in)::vertice_matrix(vertice_n,3)
    integer, intent(in)::vertice_index(vertice_n)
    integer i
    real(8)::z, bathy
    integer::count

    z=0.0d0
    count=0
    bathy=0.0d0
    do i=1, vertice_n
      if(vertice_index(i)==1)then
        z=z+vertice_matrix(i,3)
        count=count+1
      endif
    enddo
    bathy=z/count
  end function calc_bathy

  function calc_orix(vertice_index, vertice_vector, vertice_n) result(ori_x)
    integer vertice_n
    real(8), intent(in)::vertice_vector(vertice_n,3)
    integer, intent(in)::vertice_index(vertice_n)
    integer i
    real(8)::x, ori_x
    integer::count
    x=0.0d0
    count=0
    ori_x=0.0d0
    do i=1, vertice_n
      if(vertice_index(i)==1)then
        x=x+vertice_vector(i,1)
        count=count+1
      endif
    enddo
    ori_x=x/count
  end function calc_orix

  function calc_oriy(vertice_index, vertice_vector, vertice_n) result(ori_y)
    integer vertice_n
    real(8), intent(in)::vertice_vector(vertice_n,3)
    integer, intent(in)::vertice_index(vertice_n)
    integer i
    real(8)::y, ori_y
    integer::count
    y=0.0d0
    count=0
    ori_y=0.0d0
    do i=1, vertice_n
      if(vertice_index(i)==1)then
        y=y+vertice_vector(i,2)
        count=count+1
      endif
    enddo
    ori_y=y/count
  end function calc_oriy

  function calc_roughness(vertice_index, vertice_matrix, vertice_n) result(roughness)
    integer vertice_n
    real(8), intent(in)::vertice_matrix(vertice_n,3)
    integer, intent(in)::vertice_index(vertice_n)
    integer::count, i
    real(8)::meanz,roughness,z
    z=0.0d0
    count=0
    roughness=0.0d0

    do i=1, vertice_n
      if(vertice_index(i)==1)then
        z=z+vertice_matrix(i,3)
        count=count+1
      endif
    enddo

    meanz=z/count

    do i=1, vertice_n
      if(vertice_index(i)==1)then
        roughness=roughness+abs(vertice_matrix(i,3)-meanz)
      endif
    enddo

    roughness=roughness/count

  end function calc_roughness

  function calc_slopeness(vertice_index, vertice_vector, vertice_n) result(slopeness)
    integer vertice_n
    integer, intent(in)::vertice_index(vertice_n)
    real(8), intent(in)::vertice_vector(vertice_n,3)
    real,parameter :: pi = 3.1415927
    integer::count, i, j
    real(8)::sum_vec(3), slopeness
    count=0
    sum_vec=(/0.0d0, 0.0d0, 0.0d0/)
    !法線ベクトルを合成して平均する
    do i =1, vertice_n
      if(vertice_index(i)==1)then
        count=count+1
        do j =1, 3
          sum_vec(j)=sum_vec(j)+vertice_vector(i, j)
        enddo
      endif
    enddo
    sum_vec=sum_vec/count
    !法線ベクトルからz軸基準の角度に直すよ

    slopeness=atan(sum_vec(3)/sqrt(sum_vec(1)**2+sum_vec(2)**2))/pi*2*90
    if(slopeness>=0)then
      slopeness=90+abs(slopeness)
    endif
  end function calc_slopeness

end module calc_variables
