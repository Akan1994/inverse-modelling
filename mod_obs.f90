!---------------------------------------------------------------------------------------
! PREP_FLEXPART: mod_obs
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
!> mod_obs
!!
!! Purpose:    Module for construction of the observations data structure
!!
!---------------------------------------------------------------------------------------

module mod_obs

  implicit none
  private

  public :: obs_t, alloc_obs

  type :: obs_t

    real(kind=8), dimension(:), allocatable :: jdi
    real(kind=8), dimension(:), allocatable :: jdf
    real, dimension(:), allocatable         :: lat
    real, dimension(:), allocatable         :: lon
    real, dimension(:), allocatable         :: alt
    real, dimension(:), allocatable         :: con
    real, dimension(:), allocatable         :: err
    integer, dimension(:), allocatable      :: num

  end type obs_t

  contains 

    ! --------------------------------------------------
    ! allocate obs
    ! --------------------------------------------------

    subroutine alloc_obs(nobs, obs)

      implicit none

      integer,      intent(in)     :: nobs
      type (obs_t), intent(in out) :: obs
      integer                      :: ierr
     
      if( .not.allocated(obs%jdi) ) allocate( obs%jdi(nobs), stat=ierr )
      if( ierr.ne.0 ) print*, 'ERROR: not enough memory'
      if( .not.allocated(obs%jdf) ) allocate( obs%jdf(nobs), stat=ierr )
      if( ierr.ne.0 ) print*, 'ERROR: not enough memory'
      if( .not.allocated(obs%lat) ) allocate( obs%lat(nobs), stat=ierr )
      if( ierr.ne.0 ) print*, 'ERROR: not enough memory'
      if( .not.allocated(obs%lon) ) allocate( obs%lon(nobs), stat=ierr )
      if( ierr.ne.0 ) print*, 'ERROR: not enough memory'
      if( .not.allocated(obs%alt) ) allocate( obs%alt(nobs), stat=ierr )
      if( ierr.ne.0 ) print*, 'ERROR: not enough memory'
      if( .not.allocated(obs%con) ) allocate( obs%con(nobs), stat=ierr )
      if( ierr.ne.0 ) print*, 'ERROR: not enough memory'
      if( .not.allocated(obs%err) ) allocate( obs%err(nobs), stat=ierr )
      if( ierr.ne.0 ) print*, 'ERROR: not enough memory'
      if( .not.allocated(obs%num) ) allocate( obs%num(nobs), stat=ierr )
      if( ierr.ne.0 ) print*, 'ERROR: not enough memory'

    end subroutine alloc_obs

    ! --------------------------------------------------

end module mod_obs

