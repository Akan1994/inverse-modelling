!---------------------------------------------------------------------------------------
! PREP_FLEXPART: prep_pathnames
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
!> prep_pathnames
!!
!! Purpose:    Prepares the file pathnames
!!
!! Interface:
!!
!!    Inputs
!!             settings  -  settings data structure
!!             jd        -  julian day for start of month
!!             nr        -  index to the receptors list
!!
!!    Externals
!!             caldate
!!
!---------------------------------------------------------------------------------------

subroutine prep_pathnames(settings, jd, nr)

  use mod_var
  use mod_settings
  use mod_dates

  implicit none

  type (settings_t), intent(in) :: settings
  real(kind=8),      intent(in) :: jd
  integer,           intent(in) :: nr

  character(len=max_path_len)   :: filename
  character(len=6)              :: adate
  integer                       :: jjjjmmdd, hhmiss
  integer                       :: ierr

  call caldate(jd, jjjjmmdd, hhmiss)
  write(adate,fmt='(I6.6)') jjjjmmdd/100
  filename = trim(settings%path_options)//trim(recname(nr))//'/'//adate//'/pathnames'

  call system('mkdir -p '//trim(settings%path_options)//trim(recname(nr))//'/'//adate) 
  call system('mkdir -p '//trim(settings%path_output)//trim(recname(nr))//'/'//adate) 

  open(100,file=trim(filename),status='replace',action='write',iostat=ierr)

  if ( settings%lscratch.eq.0 ) then
    ! not using scratch
    write(100,fmt='(A)') trim(settings%path_options)//trim(recname(nr))//'/'//adate//'/options/'
    write(100,fmt='(A)') trim(settings%path_output)//trim(recname(nr))//'/'//adate//'/'
  else
    ! using scratch
    write(100,fmt='(A)') './options/'
    write(100,fmt='(A)') './output/'
  endif
  write(100,fmt='(A1)') '/'
  write(100,fmt='(A)') trim(settings%file_avail)

  if( settings%file_availnest.ne.'' ) then
    write(100,fmt='(A1)') '/'
    write(100,fmt='(A)') trim(settings%file_availnest)
  endif

  close(100)

end subroutine prep_pathnames

