program main
  use get_neighbor
  !use calc_terrain_vals
  implicit none
  real(8),allocatable:: vertice_matrix(:,:),  extract_vertice(:,:)
  integer,allocatable:: faces_matrix(:,:)
  integer, allocatable:: vertice_index_0(:)
  integer, allocatable:: vertice_index_1(:)
  integer, allocatable:: vertice_index_2(:)
  integer, allocatable:: vertice_index_3(:)

  integer:: faces_n, vertice_n
  integer:: i,j
  integer:: pr_file=10, fa_file=11, ve_file=12
  real(8) t1,t2
  call cpu_time(t1)


  !vertex数とface数の読み込み
  open(pr_file, file="property.d")
  read(pr_file, *) vertice_n, faces_n
  !write(*, *) vertice_n, faces_n
  close(pr_file)
  !上の読み込みを元に，配列サイズの割り当て
  allocate(vertice_matrix(vertice_n,3), faces_matrix(faces_n,3))
  allocate(extract_vertice(faces_n,3))
  allocate(vertice_index_0(vertice_n))
  allocate(vertice_index_1(vertice_n))
  allocate(vertice_index_2(vertice_n))
  allocate(vertice_index_3(vertice_n))
  !faceの読み込みと格納
  open(fa_file, file='faces_dataset.csv')
  read(fa_file, *) ((faces_matrix(i,j), j=1,3),i=1,faces_n)
  !faceを配列で読み込めたか確認
  !write(*,*) faces_matrix(1,1:3)
  close(fa_file)

  !verticeの読み込みと格納
  open(ve_file, file='vertice_dataset.csv')
  read(ve_file, *) ((vertice_matrix(i,j), j=1,3),i=1, vertice_n)
  !verticeを配列で読み込めたか確認
  !write(*,*) vertice_matrix(1,1:3)
  close(ve_file)

  !do i =1, 3 !お試しイテレート
  do i = 1, faces_n
    !ループ0の点インデックスリストを取得
    vertice_index_0 = get_neighbor0_index(i, faces_matrix, faces_n, vertice_n)
    do j=1, vertice_n
      if (vertice_index_0(j)==1)then
        write(*,*) i,0,j
      endif
    enddo
    !ループ1の点インデックスリストを取得
    vertice_index_1 = get_neighbor1_index(i, faces_matrix, vertice_matrix, faces_n, vertice_n)
    do j=1, vertice_n
      if(vertice_index_1(j)==1)then
        write(*,*) i,1,j
      endif
    enddo
    !ループ2の点インデックスリストを取得
    vertice_index_2 = get_neighbor2_index(i, faces_matrix, vertice_matrix, faces_n, vertice_n)
    do j=1, vertice_n
      if(vertice_index_2(j)==1)then
        write(*,*) i,2,j
      endif
    enddo
  enddo
  !ループ3の点インデックスリストを取得
  vertice_index_3 = get_neighbor3_index(i, faces_matrix, vertice_matrix, faces_n, vertice_n)
  do j=1, vertice_n
    if(vertice_index_3(j)==1)then
      write(*,*) i,3,j
    endif
  enddo
enddo


  !配列の割り当て終了
  deallocate(faces_matrix)
  deallocate(vertice_matrix)
  deallocate(vertice_index_0)
  deallocate(vertice_index_1)
  deallocate(vertice_index_2)
  deallocate(vertice_index_3)

  !計算時間計測
  call cpu_time(t2)
  write(*,*) "cpu time:", t2-t1
  write(*,*) "cpu time per loop:", (t2-t1)/faces_n
end program main
