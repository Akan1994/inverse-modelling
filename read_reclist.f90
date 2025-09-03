!---------------------------------------------------------------------------------------
! PREP_FLEXPART: read_reclist
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
!> read_reclist
!!
!! Purpose:    Reads the receptor list and counts the number of receptors.
!!
!! Note:       The lat, lon, alt coordinates are only used if lrelease = 0, i.e.
!!             the releases are to be made at regular intervals and not matching the
!!             observation timestamps. For data from a moving platform (aircraft, ship,
!!             or satellite) need to use lrelease = 1.
!!
!---------------------------------------------------------------------------------------

subroutine read_reclist(filename)

  use mod_var

  implicit none

  character(len=max_path_len), intent(in) :: filename
  character(len=200)                      :: line
  character(len=max_path_len)             :: rowfmt
  integer                                 :: ierr
  integer                                 :: cnt

  ! count number of receptors

  open(100,file=trim(filename),action='read',status='old',iostat=ierr)
  if(ierr.gt.0) then
    write(*,*) 'ERROR: cannot open: '//trim(filename)
    stop
  endif
  write(*,*) 'Reading receptors file: '//trim(filename)

  cnt = 0
  do while (ierr.eq.0)
    read(100,*,iostat=ierr,end=10) line
    cnt = cnt + 1
  end do
10 continue
  close(100)

  nrec = cnt
  write(*,*) 'Number of receptors: ',nrec

  ! read receptors

  allocate ( recname(nrec) )
  allocate ( reclon(nrec) )
  allocate ( reclat(nrec) )
  allocate ( recalt(nrec) )

  open(100,file=trim(filename),action='read',status='old',iostat=ierr)
  write(*,*) 'Receptors: '

!  write(rowfmt,fmt='(A,I1,A)') '(A',recname_len,',1X,F7.2,1X,F7.2,1X,F7.2)'
  write(rowfmt,fmt='(A,I1,A)') '(A',3,',1X,F7.2,1X,F7.2,1X,F7.2)'
  do cnt = 1, nrec
     read(100,fmt=rowfmt) recname(cnt), reclat(cnt), reclon(cnt), recalt(cnt)
     print*, recname(cnt), reclat(cnt), reclon(cnt), recalt(cnt)
  end do

  close(100)

end subroutine read_reclist


