!---------------------------------------------------------------------------------------
! PREP_FLEXPART: mod_prep_ageclass
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
!> mod_prep_ageclass
!!
!! Purpose:    Prepares the options file AGECLASS.
!!
!! Interface:
!!
!!    Inputs
!!             settings  -  settings data structure
!!             jd        -  julian day of start of month
!!             nr        -  index to receptor list
!!
!!    Externals
!!             caldate
!!
!---------------------------------------------------------------------------------------

module mod_prep_ageclass

  use mod_var
  use mod_settings
  use mod_dates

  implicit none
  private

  public :: prep_ageclass_FP10, prep_ageclass

  character(len=max_path_len)   :: filename
  character(len=6)              :: adate
  integer                       :: jjjjmm, jjjjmmdd, hhmiss
  integer                       :: datei, datef, eomday
  integer                       :: ierr
  integer                       :: nageclass, lage

  contains

    ! --------------------------------------------------
    ! prep_ageclass
    ! --------------------------------------------------
    !
    ! Prepares the AGECLASS file for FLEXPART v11
    ! --------------------------------------------------

    subroutine prep_ageclass(settings, jd, nr)

      implicit none

      type (settings_t), intent(in) :: settings
      real(kind=8),      intent(in) :: jd
      integer,           intent(in) :: nr

      namelist /nage/ &
        nageclass
      namelist /ageclass/ &
        lage

      ! preset namelist variables
      nageclass = 1
      lage = settings%ageclass

      call caldate(jd, jjjjmmdd, hhmiss)
      write(adate,fmt='(I6.6)') jjjjmmdd/100
      filename = trim(settings%path_options)//trim(recname(nr))//'/'//adate//'/options/AGECLASSES'

      open(100,file=trim(filename),status='replace',action='write',iostat=ierr)
      write(100,nml=nage)
      write(100,nml=ageclass)
      close(100)

    end subroutine prep_ageclass

    ! --------------------------------------------------
    ! prep_ageclass_FP10
    ! --------------------------------------------------
    !
    ! Prepares the AGECLASS file for FLEXPART v10.4
    ! --------------------------------------------------

    subroutine prep_ageclass_FP10(settings, jd, nr)

      implicit none

      type (settings_t), intent(in) :: settings
      real(kind=8),      intent(in) :: jd
      integer,           intent(in) :: nr

      namelist /ageclass/ &
        nageclass, &
        lage

      ! preset namelist variables
      nageclass = 1
      lage = settings%ageclass

      call caldate(jd, jjjjmmdd, hhmiss)
      write(adate,fmt='(I6.6)') jjjjmmdd/100
      filename = trim(settings%path_options)//trim(recname(nr))//'/'//adate//'/options/AGECLASSES'

      open(100,file=trim(filename),status='replace',action='write',iostat=ierr)

      if( settings%lnamelist.eq.1 ) then
        ! use namelist file format
        write(100,nml=ageclass)
      else
        ! use old file format
        write(100,fmt='(A48)') '************************************************'
        write(100,fmt='(A48)') '*                                              *'
        write(100,fmt='(A48)') '*Lagrangian particle dispersion model FLEXPART *'
        write(100,fmt='(A48)') '*         Please select your options           *'
        write(100,fmt='(A48)') '*                                              *'
        write(100,fmt='(A48)') '*This file determines the ageclasses to be used*'
        write(100,fmt='(A48)') '*                                              *'
        write(100,fmt='(A48)') '*Ages are given in seconds. The first class    *'
        write(100,fmt='(A48)') '*starts at age zero and goes up to the first   *'
        write(100,fmt='(A48)') '*age specified. The last age gives the maximum *'
        write(100,fmt='(A48)') '*time a particle is carried in the simulation. *'
        write(100,fmt='(A48)') '*                                              *'
        write(100,fmt='(A48)') '************************************************'
        write(100,fmt='(A48)') '1          Integer        Number of age classes '
        write(100,fmt=*) settings%ageclass
      endif

      close(100)

    end subroutine prep_ageclass_FP10

end module mod_prep_ageclass

