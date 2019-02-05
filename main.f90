program main
  use get_neighbor
  use calc_variables
  implicit none
  real(8),allocatable:: vertice_matrix(:,:),  extract_vertice(:,:)
  real(8),allocatable:: vertice_normal(:,:)
  integer,allocatable:: faces_matrix(:,:)
  integer, allocatable:: vertice_index_0(:), vertice_index_1(:), vertice_index_2(:), vertice_index_3(:)
  integer, allocatable:: vertice_index(:,:)
  integer::loop_k=4

  integer:: faces_n, vertice_n
  integer:: i,j
  integer:: pr_file=10, fa_file=11, ve_file=12
  real(8),allocatable:: bathy0f(:), bathy1f(:), bathy2f(:), bathy3f(:)
  real(8),allocatable:: ori_x0f(:), ori_x1f(:), ori_x2f(:), ori_x3f(:)
  real(8),allocatable:: ori_y0f(:), ori_y1f(:), ori_y2f(:), ori_y3f(:)
  real(8),allocatable:: slope0f(:), slope1f(:), slope2f(:), slope3f(:)
  real(8),allocatable:: rough0f(:), rough1f(:), rough2f(:), rough3f(:)

  real(8) t1,t2
  call cpu_time(t1)


  !vertex数とface数の読み込み
  open(pr_file, file="property.d")
  read(pr_file, *) vertice_n, faces_n
  !write(*, *) vertice_n, faces_n
  close(pr_file)
  !上の読み込みを元に，配列サイズの割り当て
  allocate(vertice_matrix(vertice_n,3), faces_matrix(faces_n,3))
  allocate(vertice_normal(vertice_n,3))
  allocate(extract_vertice(faces_n,3))
  allocate(vertice_index_0(vertice_n))
  allocate(vertice_index_1(vertice_n))
  allocate(vertice_index_2(vertice_n))
  allocate(vertice_index_3(vertice_n))
  allocate(vertice_index(vertice_n,k_loop))
  allocate(bathy0f(faces_n), bathy1f(faces_n), bathy2f(faces_n), bathy3f(faces_n))
  allocate(ori_x0f(faces_n), ori_x1f(faces_n), ori_x2f(faces_n), ori_x3f(faces_n))
  allocate(ori_y0f(faces_n), ori_y1f(faces_n), ori_y2f(faces_n), ori_y3f(faces_n))
  allocate(slope0f(faces_n), slope1f(faces_n), slope2f(faces_n), slope3f(faces_n))
  allocate(rough0f(faces_n), rough1f(faces_n), rough2f(faces_n), rough3f(faces_n))

  !faceの読み込みと格納
  open(fa_file, file='faces_dataset.csv')
  read(fa_file, *) ((faces_matrix(i,j), j=1,3),i=1,faces_n)
  !faceを配列で読み込めたか確認
  !write(*,*) faces_matrix(1,1:3)
  close(fa_file)

  !verticeの読み込みと格納
  open(ve_file, file='vertice_dataset.csv')
  read(ve_file, *) ((vertice_matrix(i,j), j=1,3),i=1, vertice_n)
  read(ve_file, *) ((vertice_normal(i,j), j=4,6),i=1, vertice_n)
  !verticeを配列で読み込めたか確認
  !write(*,*) vertice_matrix(1,1:3)
  close(ve_file)



  do i =1, 3 !お試しイテレート
  !do i = 1, faces_n

    !ループ0の点インデックスリストを取得
    vertice_index_0 = get_neighbor0_index(i, faces_matrix, faces_n, vertice_n)
    do j=1, vertice_n
      if (vertice_index_0(j)==1)then
        !write(*,*) i,0,j
      endif
    enddo
    bathy0f(i)=calc_bathy(vertice_index_0, vertice_matrix, vertice_n)
    ori_x0f(i)=calc_orix(vertice_index_0, vertice_vector, vertice_n)
    ori_y0f(i)=calc_oriy(vertice_index_0, vertice_vector, vertice_n)

    !ループ1の点インデックスリストを取得
    vertice_index_1 = get_neighbor1_index(i, faces_matrix, vertice_matrix, faces_n, vertice_n)
    do j=1, vertice_n
      if(vertice_index_1(j)==1)then
        !write(*,*) i,1,j
      endif
    enddo
    bathy1f(i)=calc_bathy(vertice_index_1, vertice_matrix, vertice_n)

    !ループ2の点インデックスリストを取得
    vertice_index_2 = get_neighbor2_index(i, faces_matrix, vertice_matrix, faces_n, vertice_n)
    do j=1, vertice_n
      if(vertice_index_2(j)==1)then
        !write(*,*) i,2,j
      endif
    enddo
    bathy2f(i)=calc_bathy(vertice_index_2, vertice_matrix, vertice_n)

    !ループ3の点インデックスリストを取得
    vertice_index_3 = get_neighbor3_index(i, faces_matrix, vertice_matrix, faces_n, vertice_n)
    do j=1, vertice_n
      if(vertice_index_3(j)==1)then
        !write(*,*) i,3,j
      endif
    enddo
    bathy3f(i)=calc_bathy(vertice_index_3, vertice_matrix, vertice_n)

  enddo

  open (18, file='mydata.csv', status='replace')
  do i = 1, faces_n
      write (18, *) bathy0f(i), ',', bathy1f(i), ',', bathy2f(i), ',', bathy3f(i)
  end do
  close (18)

  !配列の割り当て終了
  deallocate(faces_matrix)
  deallocate(vertice_matrix)
  deallocate(vertice_normal)
  deallocate(vertice_index_0,vertice_index_1,vertice_index_2,vertice_index_3)
  deallocate(bathy0f,bathy1f,bathy2f,bathy3f)
  deallocate(ori_x0f,ori_x1f,ori_x2f,ori_x3f)
  deallocate(ori_y0f,ori_y1f,ori_y2f,ori_y3f)
  deallocate(slope0f,slope1f,slope2f,slope3f)
  deallocate(rough0f,rough1f,rough2f,rough3f)

  !計算時間計測
  call cpu_time(t2)
  write(*,*) "cpu time:", t2-t1
  write(*,*) "cpu time per loop:", (t2-t1)/faces_n
end program main
