!---------------------------------------------------------------------------------------
! PREP_FLEXPART: mod_var
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
!> mod_var
!!
!---------------------------------------------------------------------------------------

module mod_var

  implicit none

  ! general variables
  ! -----------------

  integer, parameter                            :: max_path_len=200
  integer, parameter                            :: max_name_len=100
  integer, parameter                            :: maxobs=5000
  integer, parameter                            :: recname_len=3
  real(kind=8)                                  :: jdatei, jdatef
  real(kind=8)                                  :: jreldatei, jreldatef
  integer                                       :: nrec
  integer                                       :: nfiles
  integer                                       :: nreagent
  character(len=recname_len), dimension(:), allocatable   :: recname
  character(len=256), dimension(:), allocatable :: filelist
  real, dimension(:), allocatable               :: reclon, reclat, recalt

  ! flexpart variables
  ! ------------------

  integer, parameter                            :: lsync=600          ! synchronisation interval of flexpart (secs)


end module mod_var

