module get_neighbor
  implicit none
contains
  !index_0にループ0の点インデックスを格納する（これはface(1:3)に等しい）
  function get_neighbor0_index(face_index, faces_matrix, faces_n, vertice_n) result(vertice_index_0)
    integer, intent(in)::face_index, faces_matrix(faces_n,3), faces_n, vertice_n
    integer vertice_index_0(vertice_n)
    integer i

    !ゼロクリア
    do i = 1, vertice_n
      vertice_index_0(i)=0
    enddo
    !vertice_index_0にループ0の点インデックスを格納する
    do i = 1, 3
      vertice_index_0(faces_matrix(face_index,i))=1
    enddo
  end function get_neighbor0_index

  !index_1にループ1の点インデックスを格納する
  !index_0とは，表現方法が違うので注意すること．
  function get_neighbor1_index(face_index, faces_matrix, vertice_matrix, faces_n, vertice_n) result(vertice_index_1)
    integer, intent(in)::face_index, faces_matrix(faces_n,3), faces_n
    integer, intent(in)::vertice_n
    real(8), intent(in)::vertice_matrix(vertice_n,3)
    integer faces_index_1(faces_n), faces_index_0
    integer vertice_index_0(3), vertice_index_1(vertice_n)
    integer i, j, k, f_index, a

    faces_index_0=face_index
    vertice_index_0(1:3)=faces_matrix(faces_index_0, 1:3)

    !ゼロクリア
    !write(*,*) faces_n
    do i =1, faces_n
      faces_index_1(i)=0
    enddo

    do i=1, vertice_n
      vertice_index_1(i)=0
    enddo

    !ループ1のface_indexを取得する
    !iは深いほうが速いだろう．検討
    do i=1, faces_n
        do k=1, size(vertice_index_0)
          do j=1, 3
          !面iを構成する座標にvertice_index_0と同じ座標が含まれているならば，
          if(faces_matrix(i,j)==vertice_index_0(k)) then
           !append的にインデックスを追加したい
           faces_index_1(i)=1
           !write(*,*) i
           !0/1配列にすることで，うまく同じインデックスの除去作業を避けられている．
           !しかし，無駄の多い実装だと思われるので修正必要．
          endif
        enddo
      enddo
    enddo
    !write(*,*) faces_index_1

    !ループ1のface_indexから，ループ1のvertice_indexを取得する
    do i=1, faces_n
      if(faces_index_1(i)==1) then
        do j=1, 3
          vertice_index_1(faces_matrix(i,j))=1
        enddo
      endif
    enddo
    !write(*,*) vertice_index_1
  end function get_neighbor1_index



  function get_neighbor2_index(face_index, faces_matrix, vertice_matrix, faces_n, vertice_n) result(vertice_index_2)
    integer, intent(in)::face_index, faces_matrix(faces_n,3), faces_n
    integer, intent(in)::vertice_n
    real(8), intent(in)::vertice_matrix(vertice_n,3)
    integer faces_index_2(faces_n), faces_index_1(faces_n), faces_index_0
    integer vertice_index_0(3), vertice_index_1(vertice_n), vertice_index_2(vertice_n)
    integer i, j, k, f_index, a

    faces_index_0=face_index
    vertice_index_0(1:3)=faces_matrix(faces_index_0, 1:3)

    !ゼロクリア
    !write(*,*) faces_n
    do i =1, faces_n
      faces_index_1(i)=0
    enddo

    do i=1, vertice_n
      vertice_index_1(i)=0
    enddo

    !ループ1のface_indexを取得する
    !iは深いほうが速いだろう．検討
    do i=1, faces_n
        do k=1, size(vertice_index_0)
          do j=1, 3
          !面iを構成する座標にvertice_index_0と同じ座標が含まれているならば，
          if(faces_matrix(i,j)==vertice_index_0(k)) then
           !append的にインデックスを追加したい
           faces_index_1(i)=1
           !write(*,*) i
           !0/1配列にすることで，うまく同じインデックスの除去作業を避けられている．
           !しかし，無駄の多い実装だと思われるので修正必要．
          endif
        enddo
      enddo
    enddo
    !write(*,*) faces_index_1

    !ループ1のface_indexから，ループ1のvertice_indexを取得する
    do i=1, faces_n
      if(faces_index_1(i)==1) then
        do j=1, 3
          vertice_index_1(faces_matrix(i,j))=1
        enddo
      endif
    enddo


    !ゼロクリア
    do i =1, faces_n
      faces_index_2(i)=0
    enddo

    do i=1, vertice_n
      vertice_index_2(i)=0
    enddo

    !ループ2のface_indexを取得する
    !iは深いほうが速いだろう．検討
    do i=1, faces_n
        do k=1, vertice_n
          do j=1, 3
          !vertice_index_1でチェックされた点kに対して，
          !面iを構成する座標のどれかに点kと同じ座標が含まれているならば，その面を追加する
          if(vertice_index_1(k)==1 .and. faces_matrix(i,j)==k) then
           !本当はappend的にインデックスを追加したい
           faces_index_2(i)=1
           !write(*,*) i
           !0/1配列にすることで，うまく同じインデックスの除去作業を避けられている．
           !しかし，無駄の多い実装だと思われるので修正必要．
          endif
        enddo
      enddo
    enddo
    !write(*,*) faces_index_1
    !ループ2のface_indexから，ループ2のvertice_indexを取得する
    do i=1, faces_n
      if(faces_index_2(i)==1) then
        do j=1, 3
          vertice_index_2(faces_matrix(i,j))=1
          !write(*,*) faces_matrix(i,j)
        enddo
      endif
    enddo
  end function get_neighbor2_index

  function get_neighbor3_index(face_index, faces_matrix, vertice_matrix, faces_n, vertice_n) result(vertice_index_3)
    integer, intent(in)::face_index, faces_matrix(faces_n,3), faces_n
    integer, intent(in)::vertice_n
    real(8), intent(in)::vertice_matrix(vertice_n,3)
    integer faces_index_3(faces_n), faces_index_2(faces_n), faces_index_1(faces_n), faces_index_0
    integer vertice_index_0(3), vertice_index_1(vertice_n), vertice_index_2(vertice_n), vertice_index_3(vertice_n)
    integer i, j, k, f_index, a

    faces_index_0=face_index
    vertice_index_0(1:3)=faces_matrix(faces_index_0, 1:3)

    !ゼロクリア
    !write(*,*) faces_n
    do i =1, faces_n
      faces_index_1(i)=0
    enddo

    do i=1, vertice_n
      vertice_index_1(i)=0
    enddo

    !ループ1のface_indexを取得する
    !iは深いほうが速いだろう．検討
    do i=1, faces_n
        do k=1, size(vertice_index_0)
          do j=1, 3
          !面iを構成する座標にvertice_index_0と同じ座標が含まれているならば，
          if(faces_matrix(i,j)==vertice_index_0(k)) then
           !append的にインデックスを追加したい
           faces_index_1(i)=1
           !write(*,*) i
           !0/1配列にすることで，うまく同じインデックスの除去作業を避けられている．
           !しかし，無駄の多い実装だと思われるので修正必要．
          endif
        enddo
      enddo
    enddo
    !write(*,*) faces_index_1

    !ループ1のface_indexから，ループ1のvertice_indexを取得する
    do i=1, faces_n
      if(faces_index_1(i)==1) then
        do j=1, 3
          vertice_index_1(faces_matrix(i,j))=1
        enddo
      endif
    enddo


    !ゼロクリア
    do i =1, faces_n
      faces_index_2(i)=0
    enddo

    do i=1, vertice_n
      vertice_index_2(i)=0
    enddo

    !ループ2のface_indexを取得する
    !iは深いほうが速いだろう．検討
    do i=1, faces_n
        do k=1, vertice_n
          do j=1, 3
          !vertice_index_1でチェックされた点kに対して，
          !面iを構成する座標のどれかに点kと同じ座標が含まれているならば，その面を追加する
          if(vertice_index_1(k)==1 .and. faces_matrix(i,j)==k) then
           !本当はappend的にインデックスを追加したい
           faces_index_2(i)=1
           !write(*,*) i
           !0/1配列にすることで，うまく同じインデックスの除去作業を避けられている．
           !しかし，無駄の多い実装だと思われるので修正必要．
          endif
        enddo
      enddo
    enddo
    !write(*,*) faces_index_1
    !ループ2のface_indexから，ループ2のvertice_indexを取得する
    do i=1, faces_n
      if(faces_index_2(i)==1) then
        do j=1, 3
          vertice_index_2(faces_matrix(i,j))=1
          !write(*,*) faces_matrix(i,j)
        enddo
      endif
    enddo

    !ゼロクリア
    do i =1, faces_n
      faces_index_3(i)=0
    enddo

    do i=1, vertice_n
      vertice_index_3(i)=0
    enddo

    !ループ2のface_indexを取得する
    !iは深いほうが速いだろう．検討
    do i=1, faces_n
        do k=1, vertice_n
          do j=1, 3
          !vertice_index_1でチェックされた点kに対して，
          !面iを構成する座標のどれかに点kと同じ座標が含まれているならば，その面を追加する
          if(vertice_index_2(k)==1 .and. faces_matrix(i,j)==k) then
           !本当はappend的にインデックスを追加したい
           faces_index_3(i)=1
           !write(*,*) i
           !0/1配列にすることで，うまく同じインデックスの除去作業を避けられている．
           !しかし，無駄の多い実装だと思われるので修正必要．
          endif
        enddo
      enddo
    enddo
    !write(*,*) faces_index_1
    !ループ2のface_indexから，ループ2のvertice_indexを取得する
    do i=1, faces_n
      if(faces_index_3(i)==1) then
        do j=1, 3
          vertice_index_3(faces_matrix(i,j))=1
          !write(*,*) faces_matrix(i,j)
        enddo
      endif
    enddo
  end function get_neighbor3_index

end module get_neighbor
