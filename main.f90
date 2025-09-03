!---------------------------------------------------------------------------------------
! PREP_FLEXPART: main
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
!> main
!!
!! Purpose:    Prepares the options files needed for multiple flexpart runs. 
!!
!! Details:    Creates one options folder for each receptor (i.e. station or campaign)
!!             and month for corresponding flexpart runs. Prepares the following:
!!             
!!             COMMAND   -  flexpart settings
!!             OUTGRID   -  definition of flexpart output grid
!!             AGECLASS  -  definition of age of particles
!!             RELEASES  -  definition of all releases for each receptor
!!
!!             These files can be written using the old ascii or namelist format.
!!             The RELEASES are calculated based on observations read from file and
!!             can be optionally averaged or filtered.
!! 
!---------------------------------------------------------------------------------------

program main

  use mod_var
  use mod_dates
  use mod_settings
  use mod_obs
  use mod_prep_ageclass
  use mod_prep_command

  implicit none

  ! set types
  type (settings_t)        :: settings
  type (obs_t)             :: obs
  
  character(max_path_len)  :: settings_file
  real(kind=8)             :: jd
  character(len=6)         :: adate
  logical                  :: lexist
  integer                  :: nobs, nr
  integer                  :: jjjjmmdd, hhmiss, eomday

  ! initialization
  ! --------------

  call getarg(1,settings_file)
  if (settings_file == '') then
    stop 'ERROR: need to specify SETTINGS file'
  endif
 
  ! read settings
  call read_settings(settings_file, settings)

  jdatei = juldate(settings%datei, 0)
  jdatef = juldate(settings%datef, 0)
  jreldatei = juldate(settings%reldatei, 0)
  jreldatef = juldate(settings%reldatef, 0)

  write(*,*) 'datei: ', settings%datei, jdatei
  write(*,*) 'datef: ', settings%datef, jdatef
  write(*,*) 'reldatei: ', settings%reldatei, jreldatei
  write(*,*) 'reldatef: ', settings%reldatef, jreldatef
  
  ! checks
  if( jdatei.ne.jreldatei.or.jdatef.ne.jreldatef ) then 
    write(*,*) 'WARNING: different run and release times' 
  endif
  if( (jreldatei.lt.jdatei).or.(jreldatei.gt.jdatef) ) then
    write(*,*) 'ERROR: release date outside run time'
    stop
  endif
  if( (jreldatef.lt.jdatei).or.(jreldatef.gt.jdatef) ) then
    write(*,*) 'ERROR: release date outside run time'
    stop
  endif
  inquire(file=settings%file_avail,exist=lexist)
  if( .not.lexist ) then
    write(*,*) 'ERROR: AVAILABLE file not found'
    stop
  endif
  if( settings%laverage.eq.0 ) then
    settings%intaverage = 0.
    write(*,*) 'WARNING: laverage is 0 but intaverage > 0'
    write(*,*) 'setting intaverage to 0'
  endif
  if( settings%lrelease.eq.0 ) then
    write(*,*) 'FLEXPART releases at regular intervals'
  else
    write(*,*) 'FLEXPART releases at observation timestamps'
  endif

  ! clean-up previous run
  if ( settings%lappendfile.eq.0 ) then
    write(*,*) 'WARNING: cleaning-up existing directory'
    call caldate(jdatei, jjjjmmdd, hhmiss)
    write(adate,'(I6)') jjjjmmdd/100
    print*, trim(settings%path_obsout)//adate//'/'
    call system('rm -rf '//trim(settings%path_obsout)//adate//'/')
    call system('mkdir -p '//trim(settings%path_obsout)//adate//'/')
  endif

  ! read receptor list
  call read_reclist(settings%file_recept)  

  ! create list of observation files
  call list_obsfiles(settings)

  ! separate runs by month and receptor
  ! -----------------------------------

  do nr = 1, nrec

    jd = jdatei
    do while (jd.lt.jdatef) 

      ! write pathnames
      call prep_pathnames(settings, jd, nr)

      ! write COMMAND
      if ( settings%FPversion.eq.10 ) then
        call prep_command_FP10(settings, jd, nr)
      else if ( settings%FPversion.eq.11 ) then
        call prep_command(settings, jd, nr)
      endif
 
      ! write OUTGRID
      call prep_outgrid(settings, jd, nr)

      ! write AGECLASS
      if ( settings%FPversion.eq.10 ) then
        call prep_ageclass_FP10(settings, jd, nr)
      else if ( settings%FPversion.eq.11 ) then
        call prep_ageclass(settings, jd, nr)
      endif

      ! read and process observations
      if ( settings%obsformat.eq.'obspack' ) then
        call read_obspack(settings, jd, nr, nobs, obs)  
      else if ( settings%obsformat.eq.'wdcgg' ) then
        call read_wdcgg(settings, jd, nr, nobs, obs)  
      else if ( settings%obsformat.eq.'noaa' ) then
        call read_noaa(settings, jd, nr, nobs, obs)
      else if ( settings%obsformat.eq.'icos' ) then
        call read_icos(settings, jd, nr, nobs, obs)
      else if ( settings%obsformat.eq.'basic' ) then
        call read_basic(settings, jd, nr, nobs, obs)
      else  
        write(*,*) 'ERROR: unknown observation file format'
        stop
      endif

      ! write RELEASES
      if( settings%lrelease.eq.0 ) then
        ! flexpart releases at regular intervals
        call prep_releases_reg(settings, jd, nr)
      else
        ! flexpart releases for observation timestamps
        ! if no observations don't create releases file
        if ( nobs.eq.0 ) go to 10
        call prep_releases(settings, jd, nr, nobs, obs)
      endif

10    continue

      call caldate(jd, jjjjmmdd, hhmiss)
      eomday = calceomday(jjjjmmdd/100)
      jd = jd + real(eomday,kind=8)

    end do

  end do 


end program



