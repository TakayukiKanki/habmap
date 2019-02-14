program main
  use get_neighbor
  use calc_variables
  implicit none
  real(8),allocatable:: vertice_matrix(:,:), vertice_normal(:,:)
  integer,allocatable:: faces_matrix(:,:), vertice_index(:,:)

  !ループ0からループk_loopまで探索．ゆえに，k_loop+1パターン．
  integer::k_loop=1!>=1
  integer:: faces_n, vertice_n
  integer:: i,j,k
  integer:: pr_file=10, fa_file=11, ve_file=12, nor_file=13, var_file=14
  real(8),allocatable:: bathyf(:,:),ori_xf(:,:),ori_yf(:,:),slopef(:,:),roughf(:,:)

  real(8) t1,t2,t3
  call cpu_time(t1)

  !vertex数とface数の読み込み
  open(pr_file, file="property.d")
  read(pr_file, *) vertice_n, faces_n
  !write(*, *) vertice_n, faces_n
  close(pr_file)
  !上の読み込みを元に，配列サイズの割り当て

  allocate(vertice_matrix(vertice_n,3), faces_matrix(faces_n,3), vertice_normal(vertice_n, 3))
  allocate(vertice_index(vertice_n,k_loop+1))
  allocate(bathyf(faces_n, k_loop+1))
  allocate(ori_xf(faces_n, k_loop+1))
  allocate(ori_yf(faces_n, k_loop+1))
  allocate(slopef(faces_n, k_loop+1))
  allocate(roughf(faces_n, k_loop+1))

  !faceの読み込みと格納
  open(fa_file, file='faces_dataset.csv')
  read(fa_file, *) ((faces_matrix(i,j), j=1,3),i=1, faces_n)
  !faceを配列で読み込めたか確認
  !write(*,*) faces_matrix(1,1:3)
  close(fa_file)

  !verticeの読み込みと格納
  open(ve_file, file='vertice_dataset.csv')
  read(ve_file, *) ((vertice_matrix(i,j), j=1,3),i=1, vertice_n)
  !write(*,*) vertice_matrix(1,1:3)
  close(ve_file)

  open(nor_file, file='vertice_normal.csv')
  read(nor_file, *) ((vertice_normal(i,j), j=1,3),i=1, vertice_n)
  !verticeを配列で読み込めたか確認
  !write(*,*) vertice_matrix(1,1:3)
  close(nor_file)

  !探索と地形条件計算のメイン部分
  !do i =1, 3 !お試しイテレート
  !do i =1, 100
  do i = 1, faces_n
    vertice_index(:,:)=get_neighbork_index(i, faces_matrix, vertice_matrix, faces_n, vertice_n, k_loop)

    do k=1, k_loop+1
      bathyf(i,k)=calc_bathy(vertice_index(:,k), vertice_matrix, vertice_n)
      ori_xf(i,k)=calc_orix(vertice_index(:,k), vertice_normal, vertice_n)
      ori_yf(i,k)=calc_oriy(vertice_index(:,k), vertice_normal, vertice_n)
      slopef(i,k)=calc_slopeness(vertice_index(:,k), vertice_normal, vertice_n)
      roughf(i,k)=calc_roughness(vertice_index(:,k), vertice_matrix, vertice_n)
    enddo

    !終わりそうな時間の予測
    if(i==100)then
      call cpu_time(t3)
      write(*,*) "time per loop:", (t3-t1)/100
      write(*,*) "finish after ", (t3-t1)/100*faces_n/60, " minutes"
    endif


    !探索点の表示
    !do k=1, k_loop+1
    !  do j =1, vertice_n
    !    if (vertice_index(j,k)==1)then
    !      write(*,*) i,k-1,j
    !    endif
    !  enddo
    !enddo
  enddo

  !地形条件パラメータの保存
  open (var_file, file='mydata.csv', status='replace')
  do i = 1, faces_n
    do j=1, k_loop+1
      !改行なし
      write (var_file, '(e12.4, a)', advance='no') bathyf(i,j), ','!bathyf(i,2), ',', bathyf(i,3), ',', bathyf(i,4), ',', bathyf(i,5)
      write (var_file, '(e12.4, a)', advance='no') ori_xf(i,j), ','! ori_xf(i,2), ',', ori_xf(i,3), ',', ori_xf(i,4), ',', ori_xf(i,5)
      write (var_file, '(e12.4, a)', advance='no') ori_yf(i,j), ','! ori_yf(i,2), ',', ori_yf(i,3), ',', ori_yf(i,4), ',', ori_yf(i,5)
      write (var_file, '(e12.4, a)', advance='no') slopef(i,j), ','! slopef(i,2), ',', slopef(i,3), ',', slopef(i,4), ',', slopef(i,5)
      write (var_file, '(e12.4, a)' , advance='no') roughf(i,j), ','!, ',', roughf(i,2), ',', roughf(i,3), ',', roughf(i,4), ',', roughf(i,5)
      if(j==k_loop+1)then
        write(var_file, *)
      endif
    enddo
  enddo
  close (var_file)

  !配列の割り当て終了
  deallocate(faces_matrix, vertice_matrix, vertice_normal)
  deallocate(vertice_index)
  deallocate(bathyf, ori_xf, ori_yf, slopef, roughf)

  !計算時間計測
  call cpu_time(t2)
  write(*,*) "cpu time:", t2-t1
  write(*,*) "cpu time per loop:", (t2-t1)/faces_n
end program main
