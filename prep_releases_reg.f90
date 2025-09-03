!---------------------------------------------------------------------------------------
! PREP_FLEXPART: prep_releases_reg
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
!> prep_releases_reg
!!
!! Purpose:    Prepares the options file RELEASES for regular release times
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
!!             skiplines
!!
!! Note:       For releases made at regular intervals (and not matching the
!!             observation timestamps) the lat,lon, alt coordinates are stationary. 
!!             For data from a moving platform (aircraft, ship, or satellite) need to 
!!             use prep_releases.f90 (so lrelease = 1)
!!
!---------------------------------------------------------------------------------------

subroutine prep_releases_reg(settings, jd, nr)

  use mod_var
  use mod_settings
  use mod_dates
  use mod_tools
  use mod_strings

  implicit none

  type (settings_t), intent(in) :: settings
  real(kind=8),      intent(in) :: jd
  integer,           intent(in) :: nr

  character(len=max_path_len)   :: filename, pathname
  character(len=100)            :: line
  character(len=6)              :: adate
  character(len=3)              :: acnt, aspec
  integer                       :: jjjjmmdd, hhmiss, ss
  integer                       :: ierr
  integer                       :: spec
  integer                       :: nchar, n
  integer                       :: eomday
  real(kind=8)                  :: jdi, jdf, jdi_start, jdi_end, relfreq
 
  integer                       :: nspec, specnum_rel 
  integer                       :: idate1, itime1
  integer                       :: idate2, itime2
  real                          :: lon1, lon2
  real                          :: lat1, lat2
  real                          :: z1, z2
  integer                       :: zkind, parts
  real                          :: mass
  character(len=40)             :: comment
  logical                       :: lexist

  character(len=16)  :: pspecies, pemis_name
  character(len=256) :: pemis_path, pemis_file
  integer :: pemis_unit
  real :: pemis_coeff
  real :: pdecay, pweta_gas, pwetb_gas, preldiff, phenry, pf0, pdensity, pdquer
  real :: pdsigma, pdryvel, pweightmolar, pdia
  character(len=10), allocatable, dimension(:) :: preactions
  real, allocatable, dimension(:) :: pcconst, pdconst, pnconst
  real, allocatable, dimension(:) :: pohnconst, pohcconst, pohdconst
  real :: pcrain_aero, pcsnow_aero, pccn_aero, pin_aero
  real :: parea_dow(7), parea_hour(24), ppoint_dow(7), ppoint_hour(24)
  integer :: ios
  integer :: pshape,porient
  real ::pla,pia,psa,f,e,paspectratio

  namelist /releases_ctrl/ &
    nspec, &
    specnum_rel

  namelist /release/ &
    idate1, itime1, &
    idate2, itime2, &
    lon1, lon2, &
    lat1, lat2, &
    z1, z2, &
    zkind, &
    mass, &
    parts, &
    comment

  namelist /species_params/ &
       pspecies, pdecay, pweta_gas, pwetb_gas, &
       pcrain_aero, pcsnow_aero, pccn_aero, pin_aero, &
       preldiff, phenry, pf0, pdensity, pdquer, pdia, &
       pdsigma, pdryvel, pweightmolar, pohnconst, &
       preactions, pcconst, pdconst, pnconst, pohcconst, pohdconst, &
       pemis_path, pemis_file, pemis_name, pemis_unit, pemis_coeff, &
       parea_dow, parea_hour, ppoint_dow, ppoint_hour, &
       pshape, paspectratio, pla, pia, psa, porient

  ! initialize date/time variables
  call caldate(jd, jjjjmmdd, hhmiss)
  write(adate,fmt='(I6.6)') jjjjmmdd/100
  eomday = calceomday(jjjjmmdd/100)
  jdf = jd + real(eomday,kind=8)
  jdi = jd
  relfreq = dble(settings%relfreq)/24d0
  print*, 'prep_releases_reg: relfreq = ',relfreq
  print*, 'prep_releases_reg: jdi = ',jdi
  print*, 'prep_releases_reg: jdf = ',jdf

  ! read species file
  if ( settings%FPversion.eq.10 ) then
    inquire(file=trim(settings%path_flexpart)//'options/SPECIES/spec_overview',exist=lexist)
    if ( .not.lexist ) then
      write(*,*) 'ERROR: cannot find file '//trim(settings%path_flexpart)//'options/SPECIES/spec_overview'
      stop
    endif
    open(100,file=trim(settings%path_flexpart)//'options/SPECIES/spec_overview',status='old',action='read',iostat=ierr)
    nchar = len_trim(settings%species)
    do while ( ierr.eq.0 )
      read (100, fmt='(A)', iostat=ierr) line
      if ( line(len_trim(line)-nchar+1:len_trim(line)) == trim(settings%species) ) ierr = 1
    end do
    read(line(9:11),*) spec
    write(aspec,fmt='(I3.3)') spec
  else if ( settings%FPversion.eq.11 ) then
    ! FPv11 new version SPECIES files contain species name not number
    spec = 1
    inquire(file=trim(settings%path_flexpart)//'options/SPECIES/SPECIES_'//to_upper(trim(settings%species)),&
            exist=lexist)
    if ( .not.lexist ) then
      write(*,*) 'ERROR: cannot find file '&
                 //trim(settings%path_flexpart)//'options/SPECIES/SPECIES_'//to_upper(trim(settings%species))
      stop
    endif
    write(aspec,fmt='(I3.3)') spec
    call system('cp '//trim(settings%path_flexpart)//'options/SPECIES/SPECIES_'//to_upper(trim(settings%species))//&
                ' '//trim(settings%path_flexpart)//'options/SPECIES/SPECIES_'//aspec)
    write(aspec,fmt='(I3.3)') spec
    if (nreagent.gt.0) then
      ! read SPECIES file for reagents info
      allocate(preactions(nreagent))
      allocate(pcconst(nreagent))
      allocate(pdconst(nreagent))
      allocate(pnconst(nreagent))
      open(100,file=trim(settings%path_flexpart)//'options/SPECIES/SPECIES_'//to_upper(trim(settings%species)),&
            action='read',iostat=ierr)
      read(100,species_params,iostat=ierr)
      if ( ierr.ne.0 ) then
        write(*,*) 'ERROR: reading SPECIES_'//to_upper(trim(settings%species))
        stop
      endif
      write(*,*) 'Chemical reactions: ',preactions
      if (any(preactions.ne."")) then
        ! write reagents file
        call prep_reagents(settings,preactions,nr,jd)
      endif
    endif
  endif

  ! preset namelist variables releases_ctrl
  nspec = 1
  specnum_rel = spec

  ! copy standard input files to options folder
  pathname = trim(settings%path_options)//trim(recname(nr))//'/'//adate//'/options/'

  print*, 'prep_releases_reg: pathname = ',pathname
  print*, 'prep_releases_reg: pathflexpart = ',settings%path_flexpart

  call system('mkdir -p '//trim(pathname)//'SPECIES/')
  filename = trim(settings%path_flexpart)//'options/SPECIES/SPECIES_'//aspec

  print*, 'prep_releases_reg: filename = ',filename

  call system('cp '//trim(filename)//' '//trim(pathname)//'SPECIES/')
  call system('cp '//trim(settings%path_flexpart)//'options/*.dat'//' '//trim(pathname))
  call system('cp '//trim(settings%path_flexpart)//'options/*.t'//' '//trim(pathname))

  ! open RELEASES file
  filename = trim(pathname)//'RELEASES'
  open(100,file=trim(filename),status='replace',action='write',iostat=ierr)
  
  ! write RELEASES file

  if( settings%lnamelist.eq.1 ) then
    ! use namelist format
    write(100,nml=releases_ctrl)  
    n = 0
    do while ( jdi.le.(jdf-relfreq) )  
      n = n + 1
      ! set namelist variables releases
      jdi_start = dnint(jdi*1.e6)
      jdi_start = jdi_start/1.e6
      call caldate(jdi_start, jjjjmmdd, hhmiss)
      idate1 = jjjjmmdd
      itime1 = hhmiss
      jdi_end = dnint((jdi+relfreq)*1.e6)
      jdi_end = jdi_end/1.e6
      call caldate(jdi_end, jjjjmmdd, hhmiss)
      idate2 = jjjjmmdd
      itime2 = hhmiss
      lon1 = reclon(nr)
      lon2 = reclon(nr)
      lat1 = reclat(nr)
      lat2 = reclat(nr)
      z1 = recalt(nr)
      z2 = recalt(nr)
      zkind = settings%zref
      mass = 1000.
      parts = settings%npart
      write(acnt,fmt='(I3.3)') n
      comment = trim(recname(nr))//'_'//adate//'_'//acnt
      write(100,nml=release)
      jdi = jdi + relfreq
    end do
  else
    ! use old file format
    write(100,fmt='(A)') '*************************************************************************'
    write(100,fmt='(A)') '*                                                                       *'
    write(100,fmt='(A)') '*                                                                       *'
    write(100,fmt='(A)') '*                                                                       *'
    write(100,fmt='(A)') '*                                                                       *'
    write(100,fmt='(A)') '*   Input file for the Lagrangian particle dispersion model FLEXPART    *'
    write(100,fmt='(A)') '*                                                                       *'
    write(100,fmt='(A)') '*                                                                       *'
    write(100,fmt='(A)') '*                                                                       *'
    write(100,fmt='(A)') '*                                                                       *'
    write(100,fmt='(A)') '*************************************************************************'
    call skiplines(100,1)
    write(100,fmt='(I3)') 1
    write(100,fmt='(I3)') spec
    write(100,fmt='(A)') '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
    n = 0
    do while ( jdi.le.(jdf-relfreq) )
      n = n + 1
      jdi_start = dnint(jdi*1.e6)
      jdi_start = jdi_start/1.e6
      call caldate(jdi_start, jjjjmmdd, hhmiss) 
      write(100,fmt='(I8,1X,I6)') jjjjmmdd,hhmiss
      jdi_end = dnint((jdi+relfreq)*1.e6)
      jdi_end = jdi_end/1.e6
      call caldate(jdi_end, jjjjmmdd, hhmiss) 
      write(100,fmt='(I8,1X,I6)') jjjjmmdd,hhmiss
      write(100,fmt='(F9.4)')     reclon(nr)
      write(100,fmt='(F9.4)')     reclat(nr)
      write(100,fmt='(F9.4)')     reclon(nr)
      write(100,fmt='(F9.4)')     reclat(nr)
      write(100,fmt='(I9)')       settings%zref
      write(100,fmt='(F10.3)')    recalt(nr)
      write(100,fmt='(F10.3)')    recalt(nr)
      write(100,fmt='(I9)')       settings%npart
      write(100,fmt='(F9.4)')     mass 
      write(acnt,fmt='(I3.3)')    n
      write(100,fmt='(A)')        trim(recname(nr))//'_'//adate//'_'//acnt
      write(100,fmt='(A)') '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      jdi = jdi + relfreq
    end do
  endif

  close(100)

end subroutine prep_releases_reg

