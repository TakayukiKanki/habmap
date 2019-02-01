module get_neighbor
  implicit none
contains

  recursive function get_neighbork_index(k) result(v_list)
    integer, intent(in)::k
    real(8):: v_list(:,3)
    if(k==0)then
      get_neighbor1_index(k)
    if(k==1)then
      get_neighbor2_index(k)
    else
      get_neighbork_index(k)
    endif
  end function get_neighbork_index
end module get_neighbor
