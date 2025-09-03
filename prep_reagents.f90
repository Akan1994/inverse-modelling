!---------------------------------------------------------------------------------------
! PREP_FLEXPART: prep_reagents
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
!> prep_reagents
!!
!! Purpose:    Prepares the options file REAGENTS
!!
!! Interface:
!!
!!    Inputs
!!             settings  -  settings data structure
!!             reagent   -  character string of reagent name (see SPECIES files)
!!
!---------------------------------------------------------------------------------------

subroutine prep_reagents(settings,reagents,nr,jd)

  use mod_var
  use mod_dates
  use mod_settings

  implicit none

  type (settings_t), intent(in)      :: settings
  character(len=6), dimension(nreagent), intent(in) :: reagents
  integer, intent(in)                :: nr
  real(kind=8), intent(in)           :: jd

  character(len=max_path_len)        :: filename, preag_path
  character(len=max_name_len)        :: preagent, punit
  character(len=6)                   :: adate
  integer                            :: phourly
  integer                            :: jjjjmmdd, hhmiss, ierr, n

  namelist /reagent_params/ &
    preagent, &
    preag_path, &
    phourly, &
    punit

  call caldate(jd, jjjjmmdd, hhmiss)
  write(adate,fmt='(I6.6)') jjjjmmdd/100
  filename = trim(settings%path_options)//trim(recname(nr))//'/'//adate//'/options/REAGENTS'
  open(100,file=trim(filename),status='replace',action='write',iostat=ierr)
  do n = 1, nreagent
    if (reagents(n).ne."") then
      preagent = trim(reagents(n))
      preag_path = trim(settings%path_reagents(n))
      phourly = settings%reagent_interp(n)
      punit = trim(settings%reagent_units(n))
      write(100,nml=reagent_params)
    endif
  end do
  close(100)
  
end subroutine prep_reagents

