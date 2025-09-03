!---------------------------------------------------------------------------------------
! PREP_FLEXPART: mod_tools
!---------------------------------------------------------------------------------------
!  FLEXINVERT is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation, either version 3 of the License, or
!  (at your option) any later version.
!
!  FLEXINVERT is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with FLEXINVERT.  If not, see <http://www.gnu.org/licenses/>.
!
!  Copyright 2017, Rona Thompson
!---------------------------------------------------------------------------------------
!
!> mod_tools
!!
!! Purpose:    Module of numerical functions and subroutines.
!!
!---------------------------------------------------------------------------------------

module mod_tools

  use mod_var

  implicit none
  private

  public :: sort, skiplines

  contains

  ! --------------------------------------------------
  ! sort
  ! --------------------------------------------------
  !> sort
  !!
  !! Purpose:  Sort the elements in a vector in 
  !!           ascending order.
  !! 
  ! --------------------------------------------------

  subroutine sort(n, arr, ind)

    implicit none

    integer,                    intent(in)     :: n
    real(kind=8), dimension(n), intent(in out) :: arr
    integer, dimension(n),      intent(in out) :: ind

    integer, parameter                         :: m=7, nstack=50
    integer                                    :: i, ir, j, k, l, jstack
    integer, dimension(nstack)                 :: istack
    real(kind=8)                               :: a, temp
    integer                                    :: ia, itemp

      do i=1,n
        ind(i)=i
      end do
      jstack=0
      l=1
      ir=n
1     if(ir-l.lt.m) then
        do j=l+1,ir
          a=arr(j)
          ia=ind(j)
          do i=j-1,1,-1
            if(arr(i).le.a) go to 2
            arr(i+1)=arr(i)
            ind(i+1)=ind(i)
          end do
          i=0
2         arr(i+1)=a
          ind(i+1)=ia
        end do
        if(jstack.eq.0) return
        ir=istack(jstack)
        l=istack(jstack-1)
        jstack=jstack-2
      else
        k=(l+ir)/2
        temp=arr(k)
        itemp=ind(k)
        arr(k)=arr(l+1)
        arr(l+1)=temp
        ind(k)=ind(l+1)
        ind(l+1)=itemp
        if(arr(l+1).gt.arr(ir)) then
          temp=arr(l+1)
          itemp=ind(l+1)
          arr(l+1)=arr(ir)
          arr(ir)=temp
          ind(l+1)=ind(ir)
          ind(ir)=itemp
        endif
        if(arr(l).gt.arr(ir)) then
          temp=arr(l)
          itemp=ind(l)
          arr(l)=arr(ir)
          arr(ir)=temp
          ind(l)=ind(ir)
          ind(ir)=itemp
        endif
        if(arr(l+1).gt.arr(l)) then
          temp=arr(l+1)
          itemp=ind(l+1)
          arr(l+1)=arr(l)
          arr(l)=temp
          ind(l+1)=ind(l)
          ind(l)=itemp
        endif
        i=l+1
        j=ir
        a=arr(l)
        ia=ind(l)
3       continue
          i=i+1
        if(arr(i).lt.a) go to 3
4       continue
          j=j-1
        if(arr(j).gt.a) go to 4
        if(j.lt.i) go to 5
        temp=arr(i)
        itemp=ind(i)
        arr(i)=arr(j)
        arr(j)=temp
        ind(i)=ind(j)
        ind(j)=itemp
        go to 3
5       arr(l)=arr(j)
        arr(j)=a
        ind(l)=ind(j)
        ind(j)=ia
        jstack=jstack+2
        if(jstack.gt.nstack) print*, 'ERROR sort: nstack too small'
        if(ir-i+1.ge.j-l) then
          istack(jstack)=ir
          istack(jstack-1)=i
          ir=j-1
        else
          istack(jstack)=j-1
          istack(jstack-1)=l
          l=i
        endif
      endif
      goto 1

  end subroutine sort

  ! --------------------------------------------------
  ! skiplines
  ! --------------------------------------------------
  !> skiplines
  !!
  !! Purpose:  Writes empty lines to a file
  !!
  ! --------------------------------------------------

  subroutine skiplines(uout, num)

    implicit none
 
    integer, intent(in) :: uout
    integer, intent(in) :: num
    integer             :: n

    do n = 1, num
      write(uout,*) ''
    end do

  end subroutine skiplines

  ! --------------------------------------------------

end module mod_tools

