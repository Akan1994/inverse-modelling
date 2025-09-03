!---------------------------------------------------------------------------------------
! PREP_FLEXPART: list_obsfiles
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
!> list_obsfiles
!!
!! Purpose:    Writes a list of the available observation files.
!!
!---------------------------------------------------------------------------------------

subroutine list_obsfiles(settings)

  use mod_var
  use mod_settings
  use mod_dates

  implicit none

  type (settings_t), intent(in)    :: settings
  integer                          :: nf, ierr
  character(len=6)                 :: adate
  integer                          :: jjjjmmdd, hhmiss

  ! list observation files
  ! ----------------------

  if ( settings%lappendfile.eq.1 ) then
    call system('rm -f '//trim(settings%path_obsout)//'obsfiles.dat')
    call system('ls '//trim(settings%path_obs)//'*'//trim(settings%suffix)//&
                ' | wc -l > '//trim(settings%path_obsout)//'obsfiles.dat')
    call system('ls '//trim(settings%path_obs)//' | grep '//trim(settings%suffix)//&
                ' >> '//trim(settings%path_obsout)//'obsfiles.dat')
  else
    call caldate(jdatei, jjjjmmdd, hhmiss)
    write(adate,'(I6)') jjjjmmdd/100
    call system('ls '//trim(settings%path_obs)//'*'//trim(settings%suffix)//&
                ' | wc -l > '//trim(settings%path_obsout)//adate//'/'//'obsfiles.dat')
    call system('ls '//trim(settings%path_obs)//' | grep '//trim(settings%suffix)//&
                ' >> '//trim(settings%path_obsout)//adate//'/'//'obsfiles.dat')
  endif    

  if ( settings%lappendfile.eq.1 ) then
    open(100,file=trim(settings%path_obsout)//'obsfiles.dat',action='read',status='old',iostat=ierr)
  else
    open(100,file=trim(settings%path_obsout)//adate//'/'//'obsfiles.dat',action='read',status='old',iostat=ierr)
  endif
  if ( ierr.ne. 0 ) then
    write(*,*) 'ERROR: cannot open obsfiles.dat'
    stop
  endif
  read(100,*,iostat=ierr) nfiles
  allocate ( filelist(nfiles), stat = ierr )
  do nf = 1, nfiles
    read(100,fmt='(A)',iostat=ierr) filelist(nf)
    if (ierr.ne.0) exit
  end do
  close(100)

end subroutine list_obsfiles

