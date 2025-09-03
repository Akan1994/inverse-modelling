!---------------------------------------------------------------------------------------
! PREP_FLEXPART: read_basic
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
!> read_basic
!!
!! Purpose:    Reads observations from a simple file format and averages and/or 
!!             filters them. Writes the averaged/filtered observations to file in the
!!             format required by FLEXINVERT. Input format is:
!!             YYYY MM DD HH MI SS CONC ERR LON LAT ALT
!!
!! Interface:
!!
!!    Inputs
!!             settings  -  settings data structure
!!             jd        -  julian day for start of month
!!             nr        -  index to the receptors list
!!
!!    Outputs
!!             nobs      -  number of observations
!!             obs       -  observations data structure
!!
!!    Externals
!!             caldate
!!             alloc_obs
!!             parse_string
!!
!---------------------------------------------------------------------------------------

subroutine read_basic(settings, jd, nr, nobs, obs)

  use mod_settings
  use mod_var
  use mod_dates
  use mod_obs
  use mod_strings
  use mod_tools

  implicit none

  type (settings_t), intent(in)                         :: settings
  real(kind=8),      intent(in)                         :: jd
  integer,           intent(in)                         :: nr
  integer,           intent(out)                        :: nobs
  type (obs_t),      intent(out)                        :: obs

  character(len=max_path_len)                           :: file_out
  character(len=max_path_len)                           :: file_obs
  character(len=max_path_len)                           :: rowfmt
  character(len=200)                                    :: line
  character(len=200), dimension(50)                     :: args
  character(len=6)                                      :: adate
  character(len=3)                                      :: flag
  real, dimension(12)                                   :: temp
  logical                                               :: lexist
  integer                                               :: ierr
  integer                                               :: cnt 
  integer                                               :: narg
  integer                                               :: skip
  integer                                               :: c, i, j, n
  integer                                               :: jjjjmmdd, hhmiss, yyyy, mm, dd, hh, mi, ss
  integer                                               :: eomday, hloc
  real                                                  :: dt, tmp, freq
  real                                                  :: conc, err
  real(kind=8)                                          :: jdate, out_repr_time
  real(kind=8), dimension(maxobs)                       :: jdobs  
  real, dimension(maxobs)                               :: latobs, lonobs, altobs, concobs, errobs

  ! freq determines length of the release period in the flexpart file RELEASES
  ! only used if not averaging
  freq = 0.25
  call caldate(jd, jjjjmmdd, hhmiss)
  eomday = calceomday(jjjjmmdd/100)

  print*, filelist

  ! find file matching receptor name
  do n = 1, nfiles
    do i = 1, len_trim(filelist(n))-2
      if ( to_lower(filelist(n)(i:(i+2))).eq.to_lower(trim(recname(nr))) ) go to 10
    end do
  end do
10 continue 
  if ( n.gt.nfiles ) then
    print*, 'WARNING: file not found for receptor '//trim(recname(nr))
    go to 20
  endif
  file_obs = filelist(n)

  ! open obs file
  open(100,file=trim(settings%path_obs)//trim(file_obs),action='read',status='old',iostat=ierr)
  if ( ierr.ne.0 ) then
    print*, 'ERROR: cannot open: '//trim(settings%path_obs)//trim(file_obs)
    stop
  endif 
  print*, 'Reading file: '//trim(settings%path_obs)//trim(file_obs)

  ! read header
  skip = 1  ! default 1 line skip for the header line.
  print*, 'number to skip: ',skip
  do i = 1, skip 
    read (100, fmt='(A)') line
  end do  

  ! read data
  cnt = 0
  read_loop: do 
    read(100,fmt='(A)',iostat=ierr) line
    if(ierr.gt.0) exit read_loop
    call parse_string(line, " ", args(:), narg)
    i = 0
    do n = 1, 6 ! yyyy mm dd hh mi ss
      i = i + 1
      read(args(n),*) temp(i)
    end do
    do n = 7, 8 ! conc, err
      i = i + 1
      read(args(n),*) temp(i)
    end do
    do n = 9, 11 ! lon, lat, alt
      i = i + 1
      read(args(n),*) temp(i)
    end do 
    yyyy = int(temp(1))
    mm = int(temp(2))
    dd = int(temp(3))
    hh = int(temp(4))
    mi = int(temp(5))
    ss = int(temp(6))
    conc = temp(7)
    err = temp(8)
    ! calculate julian date 
    jjjjmmdd = yyyy*10000+mm*100+dd
    hhmiss = hh*10000+mi*100+ss
    jdate = juldate(jjjjmmdd, hhmiss)
    if ( conc.le.-999. ) cycle read_loop
    !modified by AA
  !  print *, 'DEBUG:', jdate, jd, jreldatef, conc
    
    if ( (jdate.lt.jd).or.(jdate.ge.min(jreldatef,(jd+real(eomday,kind=8)))) ) cycle read_loop
    cnt = cnt + 1
    jdobs(cnt) = jdate
    concobs(cnt) = conc
    errobs(cnt) = err
    latobs(cnt) = temp(9)
    lonobs(cnt) = temp(10)
    altobs(cnt) = temp(11)
    if ( errobs(cnt).le.-9.99 ) errobs(cnt) = 0.
  end do read_loop
  nobs = cnt

  ! close input file
  close(100)

  print*, 'nobs: ',nobs

  ! no observations go to end
  if ( nobs.eq.0 ) then
    call caldate(jd, jjjjmmdd, hhmiss)
    print*, 'WARNING: no data for ',trim(recname(nr)),' in year/month ',jjjjmmdd/100
    go to 20
  endif
  print*,'aka:', nobs
  ! allocate obs
  call alloc_obs(nobs, obs)
  print*,'aka:', nobs

  ! average and/or select observations
  call process_obs(settings, nobs, freq, jdobs, latobs, lonobs, altobs, concobs, errobs, obs)
  print*,'done'
  ! no observations after selection
  if ( nobs.eq.0 ) go to 20

  ! write formatted obs to file
  if ( settings%lappendfile.eq.1 ) then
    ! append file
    ! generic output directory
    file_out = trim(settings%path_obsout)//trim(recname(nr))//'_'//trim(settings%species)//'.txt'
  else
    ! do not append to file when this is a new run
    ! create subdirectory for ouput based on start year, month
    call caldate(jdatei, jjjjmmdd, hhmiss)
    write(adate,'(I6)') jjjjmmdd/100
    file_out = trim(settings%path_obsout)//adate//'/'//trim(recname(nr))//'_'//trim(settings%species)//'.txt'
  endif
  inquire(file=trim(file_out),exist=lexist)
  if( lexist ) then
    ! append to existing
    print*, 'WARNING: appending to file '//trim(file_out)
    open(100,file=trim(file_out),status='old',action='write',access='append',iostat=ierr)
  else
    ! create new file
    open(100,file=trim(file_out),status='new',action='write',iostat=ierr)
    write(100,fmt='(A)') 'rec yyyymmdd hhmmss juldate avetime conc err num'
  endif

  ! time over which observations are averaged (or measured) 
  ! if use e.g. hourly footprints for daily averaged measurements the obs-file needs to show these are daily
  out_repr_time=real(freq/24.,kind=8)
  if ( settings%laverage.gt.0 )then
    if (settings%intaverage.gt.0. ) then
      out_repr_time=real(settings%intaverage/24,kind=8)
    endif
  endif

  ! write observations to file
  write(rowfmt,fmt='(A,I1,A)') '(A',recname_len,',1X,I8,1X,I6,1X,F14.6,1X,F10.6,1X,F10.4,1X,F10.4,1X,I4)'
  do i = 1, nobs
    call caldate(obs%jdi(i), jjjjmmdd, hhmiss)
    write(100,fmt=rowfmt) &
            trim(recname(nr)), jjjjmmdd, hhmiss, obs%jdi(i), out_repr_time, obs%con(i), obs%err(i), obs%num(i)
  end do
  close(100)

  ! write detailed station list file
  if ( settings%lappendfile.eq.1 ) then
    ! append file
    file_out = trim(settings%path_obsout)//'stnlist_detail.dat'
  else
    ! do not append
    file_out = trim(settings%path_obsout)//adate//'/'//'stnlist_detail.dat'
  endif
  inquire(file=trim(file_out),exist=lexist)
  write(rowfmt,fmt='(A,I1,A)') '(A',recname_len,',1X,F7.2,1X,F7.2,1X,F7.1)'
  if( lexist ) then
    ! append to existing
    open(100,file=trim(file_out),status='old',action='write',access='append',iostat=ierr)
    write(100,fmt=rowfmt) trim(recname(nr)), obs%lat(1), obs%lon(1), obs%alt(1)
    close(100)
  else
    ! create new file
    open(100,file=trim(file_out),status='new',action='write',iostat=ierr)
    write(100,fmt=rowfmt) trim(recname(nr)), obs%lat(1), obs%lon(1), obs%alt(1)
    close(100)
  endif

20 continue


end subroutine read_basic



