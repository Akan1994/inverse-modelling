!---------------------------------------------------------------------------------------
! PREP_FLEXPART: mod_prep_command
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
!> mod_prep_command
!!
!! Purpose:    Prepares the options file COMMAND.
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
!!             juldate
!!
!---------------------------------------------------------------------------------------

module mod_prep_command

  use mod_var
  use mod_settings
  use mod_dates
  use mod_tools

  implicit none
  private

  public :: prep_command, prep_command_FP10

  character(len=max_path_len)   :: filename
  character(len=6)              :: adate
  real(kind=8)                  :: jdf
  integer                       :: jjjjmm, jjjjmmdd, hhmiss
  integer                       :: datei, datef, eomday
  integer                       :: ierr
  integer                       :: ldirect                ! runtime mode (currently only backwards: ldirect = -1)
  integer                       :: ibdate, ibtime
  integer                       :: iedate, ietime
  integer                       :: loutstep
  integer                       :: loutaver
  integer                       :: loutsample
  integer                       :: loutrestart
  integer                       :: itsplit                ! time constant for particle splitting (secs)
  integer                       :: lsynctime              ! synchronisation interval of flexpart (secs)
  real                          :: ctl                    ! factor by which time step must be smaller than tl
  integer                       :: ifine                  ! factor by which to decrease time step for vertical motion
  integer                       :: iout                   ! output type (1 = concentration/residence time, 2 = mixing ratio, 3 = both, 4 = plume trajectory)
  integer                       :: ipout                  ! particle dump (0 = never, 1 = every output interval, 2 = only at end)
  integer                       :: lsubgrid               ! use subgrid terrain effect parameterization (0 = no, 1 = yes)
  integer                       :: lconvection            ! use convection (0 = no, 1 = yes)
  integer                       :: lturbulence            ! FP11
  integer                       :: lturbulence_meso       ! FP11
  integer                       :: nxshift                ! FP11
  integer                       :: maxthreadgrid          ! FP11
  integer                       :: maxfilesize            ! FP11
  integer                       :: logvertinterp          ! FP11
  integer                       :: lagespectra            ! calculate age spectra (0 = no, 1 = yes)
  integer                       :: ipin                   ! continue simulation with dumped particle data (0 = no, 1 = yes)
  integer                       :: ioutputforeachrelease  ! create output file for each release location (0 = no, 1 = yes)
  integer                       :: mdomainfill            ! domain filling option (0 = no, 1 = yes)
  integer                       :: mquasilag              ! quasilagrangian mode to track particles (0 = no, 1 = yes)
  integer                       :: sfc_only               ! FP11 save only surface layer in grid_time files = 1, full resolution = 0
  integer                       :: surf_only              ! save only surface layer in grid_time files = 1, full resolution = 0
  integer                       :: lnetcdfout             ! netcdf output (0 = no, 1 = yes)
  integer                       :: cblflag                ! 
  integer                       :: linversionout          ! one grid_time file per release = 1, or per timestep (original format) = 0
  integer                       :: iflux                  ! calculate fluxes (0 = no, 1 = yes)
  integer                       :: ind_source
  integer                       :: ind_receptor
  integer                       :: nested_output
  integer                       :: linit_cond
  character(len=max_path_len)   :: ohfields_path

  contains

    ! --------------------------------------------------
    ! prep_command
    ! --------------------------------------------------
    !
    ! Prepares COMMAND for FLEXPARTv11
    ! --------------------------------------------------

    subroutine prep_command(settings, jd, nr)
  
      implicit none

      type (settings_t), intent(in) :: settings
      real(kind=8),      intent(in) :: jd
      integer,           intent(in) :: nr
 
      namelist /command/ &
        ldirect, &
        ibdate,ibtime, &
        iedate,ietime, &
        loutstep, &
        loutaver, &
        loutsample, &
        lsynctime, &
        ctl, &
        ifine, &
        iout, &
        ipout, &
        lsubgrid, &
        lconvection, &
        lturbulence, &
        lturbulence_meso, &
        nxshift, &
        maxthreadgrid, &
        maxfilesize, &
        logvertinterp, &
        loutrestart, &
        lagespectra, &
        ipin, &
        ioutputforeachrelease, &
        iflux, &
        mdomainfill, &
        ind_source, &
        ind_receptor, &
        mquasilag, &
        nested_output, &
        linit_cond, &
        lnetcdfout, &
        sfc_only, &
        cblflag, &
        linversionout, &
        ohfields_path

      ! end of month day
      call caldate(jd, jjjjmmdd, hhmiss)
      eomday = calceomday(jjjjmmdd/100)

      ! initialize command file path and name
      write(adate,fmt='(I6.6)') jjjjmmdd/100
      filename = trim(settings%path_options)//trim(recname(nr))//'/'//adate//'/options/COMMAND'

      call system('mkdir -p '//trim(settings%path_options)//trim(recname(nr))//'/'//adate//'/options/')

      ! get dates
      ! correct start date for length of back trajectory
      call caldate((jd-settings%ageclass/3600d0/24d0), jjjjmmdd, hhmiss)
      datei = jjjjmmdd
      call caldate(jd, jjjjmmdd, hhmiss)
      jdf = juldate(jjjjmmdd+eomday,0)
      call caldate(min(jdf,jdatef),jjjjmmdd,hhmiss)
      datef = jjjjmmdd
  
      ! preset namelist variables
      ldirect = -1
      ibdate = datei
      ibtime = 0
      iedate = datef
      ietime = 0
      loutstep = settings%outrate
      loutaver = settings%outaverage
      loutsample = settings%outsample
      itsplit = 999999999
      lsynctime = lsync
      ctl = -5
      ifine = 4
      iout = 1
      ipout = 0
      lsubgrid = 1
      lconvection = 1
      lturbulence = 1
      lturbulence_meso = 0
      lnetcdfout = 0
      nxshift = 0
      maxthreadgrid = 1
      maxfilesize = 10000
      logvertinterp = 0
      loutrestart = -1
      lagespectra = 1
      ipin = 0
      ioutputforeachrelease = 1
      iflux = 0
      mdomainfill = 0
      ind_source = settings%ind_source
      ind_receptor = settings%ind_receptor
      mquasilag = 0
      nested_output = settings%lnested
      linit_cond = settings%linit_cond
      sfc_only = 1
      cblflag = 0
      linversionout = 1
      ohfields_path = trim(settings%path_ohfield)
 
      open(100,file=trim(filename),status='replace',action='write',iostat=ierr)    
      write(100,nml=command)
      close(100)

    end subroutine prep_command
 
    ! --------------------------------------------------
    ! prep_command_FP10
    ! --------------------------------------------------
    !
    ! Prepares COMMAND for FLEXPARTv10
    ! --------------------------------------------------

    subroutine prep_command_FP10(settings, jd, nr)

      implicit none

      type (settings_t), intent(in) :: settings
      real(kind=8),      intent(in) :: jd
      integer,           intent(in) :: nr

      namelist /command/ &
        ldirect, &
        ibdate,ibtime, &
        iedate,ietime, &
        loutstep, &
        loutaver, &
        loutsample, &
        itsplit, &
        lsynctime, &
        ctl, &
        ifine, &
        iout, &
        ipout, &
        lsubgrid, &
        lconvection, &
        lagespectra, &
        ipin, &
        ioutputforeachrelease, &
        iflux, &
        mdomainfill, &
        ind_source, &
        ind_receptor, &
        mquasilag, &
        nested_output, &
        linit_cond, &
        lnetcdfout, &
        surf_only, &
        cblflag, &
        linversionout, &
        ohfields_path

      ! end of month day
      call caldate(jd, jjjjmmdd, hhmiss)
      eomday = calceomday(jjjjmmdd/100)

      ! initialize command file path and name
      write(adate,fmt='(I6.6)') jjjjmmdd/100
      filename = trim(settings%path_options)//trim(recname(nr))//'/'//adate//'/options/COMMAND'

      call system('mkdir -p '//trim(settings%path_options)//trim(recname(nr))//'/'//adate//'/options/')

      ! get dates
      ! correct start date for length of back trajectory
      call caldate((jd-settings%ageclass/3600d0/24d0), jjjjmmdd, hhmiss)
      datei = jjjjmmdd
      call caldate(jd, jjjjmmdd, hhmiss)
      jdf = juldate(jjjjmmdd+eomday,0)
      call caldate(min(jdf,jdatef),jjjjmmdd,hhmiss)
      datef = jjjjmmdd

      ! preset namelist variables
      ldirect = -1
      ibdate = datei
      ibtime = 0
      iedate = datef
      ietime = 0
      loutstep = settings%outrate
      loutaver = settings%outaverage
      loutsample = settings%outsample
      itsplit = 999999999
      lsynctime = lsync
      ctl = -5
      ifine = 4
      iout = 1
      ipout = 2
      lsubgrid = 1
      lconvection = 1
      lagespectra = 1
      ipin = 0
      ioutputforeachrelease = 1
      iflux = 0
      mdomainfill = 0
      ind_source = settings%ind_source
      ind_receptor = settings%ind_receptor
      mquasilag = 0
      nested_output = settings%lnested
      linit_cond = settings%linit_cond
      lnetcdfout = 0
      surf_only = 1
      cblflag = 0
      linversionout = 1
      ohfields_path = trim(settings%path_ohfield)

      ! open command file
      open(100,file=trim(filename),status='replace',action='write',iostat=ierr)

      if( settings%lnamelist.eq.1 ) then
        ! use namelist file format
        write(100,nml=command)
      else
        ! use old file format
        write(100,fmt='(A)') '********************************************************************************'
        write(100,fmt='(A)') '*                                                                              *'
        write(100,fmt='(A)') '*      Input file for the Lagrangian particle dispersion model FLEXPART        *'
        write(100,fmt='(A)') '*                           Please select your options                         *'
        write(100,fmt='(A)') '*                                                                              *'
        write(100,fmt='(A)') '*                                                                              *'
        write(100,fmt='(A)') '********************************************************************************'
        write(100,fmt='(I2)')          ldirect
        write(100,fmt='(I8,1X,I6.6)')  datei,0
        write(100,fmt='(I8,1X,I6.6)')  datef,0
        write(100,fmt='(I5)')          settings%outrate
        write(100,fmt='(I5)')          settings%outaverage
        write(100,fmt='(I5)')          settings%outsample
        write(100,fmt='(I9)')          itsplit
        write(100,fmt='(I3,3X,A14)')   lsynctime,'SYNC'
        write(100,fmt='(F5.2,1X,A10)') ctl,'CTL'
        write(100,fmt='(I3,3X,A14)')   ifine,'IFINE'
        write(100,fmt='(I1,5X,A14)')   iout,'IOUT'
        write(100,fmt='(I1,5X,A14)')   ipout,'IPOUT'
        write(100,fmt='(I1,5X,A14)')   lsubgrid,'LSUBGRID'
        write(100,fmt='(I1,5X,A14)')   lconvection,'LCONVECTION'
        write(100,fmt='(I1,5X,A14)')   lagespectra,'LAGESPECTRA'
        write(100,fmt='(I1,5X,A14)')   ipin,'IPIN'
        write(100,fmt='(I1,5X,A14)')   ioutputforeachrelease,'IOUTPUTFOREACHRELEASE'
        write(100,fmt='(I1,5X,A14)')   iflux,'IFLUX'
        write(100,fmt='(I1,5X,A14)')   mdomainfill,'MDOMAINFILL'
        write(100,fmt='(I1,5X,A14)')   settings%ind_source,'IND_SOURCE'
        write(100,fmt='(I1,5X,A14)')   settings%ind_receptor,'IND_RECEPTOR'
        write(100,fmt='(I1,5X,A14)')   mquasilag,'MQUASILAG'
        write(100,fmt='(I1,5X,A14)')   settings%lnested,'NESTED_OUTPUT'
        write(100,fmt='(I1,5X,A14)')   settings%linit_cond,'LINIT_COND'
        write(100,fmt='(I1,5X,A14)')   sfc_only,'SURF_ONLY'
        write(100,fmt='(I1,5X,A14)')   linversionout,'LINVERSIONOUT'
      endif

      close(100)

    end subroutine prep_command_FP10

end module mod_prep_command


