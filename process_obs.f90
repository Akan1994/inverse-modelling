!---------------------------------------------------------------------------------------
! PREP_FLEXPART: process_obs
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
!> process_obs
!!
!! Purpose:    Averages and/or filters the observations.
!!
!! Interface:
!!
!!    Inputs
!!             settings  -  settings data structure
!!             nobs      -  number of observations
!!             freq      -  raw data observation frequency
!!             jdobs     -  observation julian days
!!             latobs    -  observation latitudes
!!             lonobs    -  observation longitudes
!!             altobs    -  observation altitudes
!!             concobs   -  observation concentrations
!!             errobs    -  observation errors
!!             obs       -  observations data structure
!!
!!    Outputs
!!             nobs      -  number of observations
!!             obs       -  observations data structure
!!
!!    Externals
!!             caldate
!!             alloc_obs
!!             parse_string
!!             sort
!!
!---------------------------------------------------------------------------------------

subroutine process_obs(settings, nobs, freq, jdobs, latobs, lonobs, altobs, concobs, errobs, obs)

  use mod_settings
  use mod_var
  use mod_dates
  use mod_obs
  use mod_tools

  implicit none

  type (settings_t),               intent(in)           :: settings
  integer,                         intent(in out)       :: nobs
  real,                            intent(in)           :: freq
  real(kind=8), dimension(maxobs), intent(in)           :: jdobs
  real, dimension(maxobs),         intent(in out)       :: latobs, lonobs, altobs, concobs, errobs
  type (obs_t),                    intent(in out)       :: obs

  integer                                               :: cnt
  integer                                               :: c, i, m, n, nprev
  integer                                               :: jjjjmmdd, hhmiss, yyyy, mm, dd, hh, mi, ss
  integer                                               :: eomday, hloc
  real                                                  :: conc, err
  real(kind=8)                                          :: jdate, jdatestart, jdateend, jdt, jdsom, jdeom
  real(kind=8), dimension(nobs,6)                       :: buffer
  real(kind=8), dimension(nobs)                         :: jdobs_out
  real(kind=8), dimension(nobs)                         :: work
  real, dimension(nobs)                                 :: concobs_out, latobs_out, lonobs_out, altobs_out, errobs_out
  integer, dimension(nobs)                              :: ind


  ! check that time series are increasing 
  ! if two different time series were patched together they need 
  ! to be sorted before averaging can be done
  jdobs_out = jdobs(1:nobs)
  call sort(nobs, jdobs_out, ind) 
  do i = 1, nobs
    concobs_out(ind(i)) = concobs(i)
    latobs_out(ind(i)) = latobs(i)
    lonobs_out(ind(i)) = lonobs(i)
    altobs_out(ind(i)) = altobs(i)
    errobs_out(ind(i)) = errobs(i)
  end do
  concobs(1:nobs) = concobs_out
  latobs(1:nobs) = latobs_out
  lonobs(1:nobs) = lonobs_out
  altobs(1:nobs) = altobs_out
  errobs(1:nobs) = errobs_out
!  print*, 'process_obs: ind = ',ind

  ! if average and/or select obs
  if( settings%laverage.eq.1.or.settings%lselect.eq.1 ) then
    n = 1
    nprev = 0
    i = 0
    jdate = jdobs(n)
    call caldate(jdate, jjjjmmdd, hhmiss)
    eomday = calceomday(jjjjmmdd/100)
    jdsom = juldate((jjjjmmdd/100)*100+1, 0)
    jdeom = jdsom + real(eomday,kind=8)
!    print*, 'jdsom: ',jdsom
!    print*, 'jdeom: ',jdeom
    ! loop over all observations in current month
    do while( jdate.le.jdobs(nobs) )
      jdt = dnint(jdate*1.e6)/1.e6
      if ( n.ne.nprev ) then
        if ( settings%intaverage.ge.2. ) then
          if ( n.eq.1 ) then
            ! if long averaging interval start at 00:00
            jdatestart = dint(jdate)
          else
            ! continue regular intervals (start at end of last interval)
            jdatestart = jdateend
          endif
        else
          ! otherwise start at current timestamp
          jdatestart = jdt
        endif
      endif
      if( n.eq.nprev .and. settings%intaverage.ge.2. ) then
        ! if no observation in last averaging interval (i.e. did not enter while loop below)
        ! increment start date
        print*, 'incrementing start date'
        jdatestart = dnint((jdatestart+real(settings%intaverage/24.,kind=8))*1.e6)/1.e6
      endif
      if ( settings%intaverage.gt.0. ) then
        ! averaging interval extends up to jdateend
        jdateend = dnint((jdatestart+real(settings%intaverage/24.,kind=8))*1.e6)/1.e6
      else
        jdateend = dnint((jdatestart+real(freq/24.,kind=8))*1.e6)/1.e6
      endif
      ! if interval crosses month set to min of current month
      jdateend = min(jdateend,jdeom)
      ! check that not beyond end of run time
      jdateend = min(jdateend,jdatef)
!      print*, 'jdate: ',jdate
!      print*, 'jdatestart: ',jdatestart
!      print*, 'jdateend: ',jdateend
      cnt = 0
      buffer(:,:) = 0.
      ! loop over observations in averaging interval (fill buffer)
      do while( (jdt.ge.jdatestart).and.(jdt.lt.jdateend) )
        call caldate(jdt, jjjjmmdd, hhmiss)
!        print*, 'jdt:',jdt
!        print*, 'jjjjmmdd, hhmiss:',jjjjmmdd,hhmiss
        if( settings%lselect.eq.1 ) then
          ! select obs
          hloc = hhmiss/10000 + int(lonobs(n)*24./360.)
!          print*, 'hh:',hhmiss/10000
          if( hloc.gt.24) hloc = hloc - 24
          if( hloc.lt.0)  hloc = hloc + 24
!          print*, 'hloc:',hloc
          if( altobs(n).lt.1000. ) then
            ! select daytime (12-16h)
            if( hloc.ge.12.and.hloc.le.16) then
              cnt = cnt + 1
              buffer(cnt,1) = jdobs(n)
              buffer(cnt,2) = latobs(n)
              buffer(cnt,3) = lonobs(n)
              buffer(cnt,4) = altobs(n)
              buffer(cnt,5) = concobs(n)
              buffer(cnt,6) = errobs(n)
            endif
          else
            ! select nighttime (0-4h)
            if( hloc.ge.0.and.hloc.le.4) then
              cnt = cnt + 1
              buffer(cnt,1) = jdobs(n)
              buffer(cnt,2) = latobs(n)
              buffer(cnt,3) = lonobs(n)
              buffer(cnt,4) = altobs(n)
              buffer(cnt,5) = concobs(n)
              buffer(cnt,6) = errobs(n)
            endif
          endif
        else
          ! no selection        
          cnt = cnt + 1
          buffer(cnt,1) = jdobs(n)
          buffer(cnt,2) = latobs(n)
          buffer(cnt,3) = lonobs(n)
          buffer(cnt,4) = altobs(n)
          buffer(cnt,5) = concobs(n)
          buffer(cnt,6) = errobs(n)
        endif
        nprev = n
        n = n + 1
        if ( n.gt.nobs ) go to 10
!        print*, 'jdt: ',jdt
!        jdt = jdobs(n)
        jdt = dnint(jdobs(n)*1.e6)/1.e6
!        print*, 'next jdt: ',jdt
      end do
      if( nprev.eq.0 ) nprev = 1
10    continue
!      print*, 'cnt:',cnt
!      print*, 'buffer:'
      do c = 1, cnt
!        print*, buffer(c,1), buffer(c,2), buffer(c,3), buffer(c,4), buffer(c,5), buffer(c,6)
      end do
      if( settings%laverage.eq.1 ) then
        ! average obs in buffer
        if( cnt.gt.0 ) then
          i = i + 1
          ! round jdi to lsync frequency to match flexpart
          call caldate(jdatestart, jjjjmmdd, hhmiss)
          hhmiss = int(real(hhmiss)/real((lsync/60)*100))*lsync*100/60
          obs%jdi(i) = juldate(jjjjmmdd, hhmiss)
          ! round jdf to lsync frequency to match flexpart
          call caldate(jdateend, jjjjmmdd, hhmiss)
          hhmiss = int(real(hhmiss)/real((lsync/60)*100))*lsync*100/60
          obs%jdf(i) = juldate(jjjjmmdd, hhmiss)
          ! median longitude
          work = buffer(:,3)
          call sort(cnt, work(1:cnt), ind(1:cnt))
          obs%lon(i) = real(work(cnt/2+1),kind=4)
          ! mean
          obs%lat(i) = sum(buffer(1:cnt,2))/real(cnt)
          obs%alt(i) = sum(buffer(1:cnt,4))/real(cnt)
          obs%con(i) = sum(buffer(1:cnt,5))/real(cnt)
          obs%err(i) = sum(buffer(1:cnt,6))/real(cnt)
          obs%num(i) = cnt
        endif
      else
        ! no average
        if( cnt.gt.0 ) then
          do c = 1, cnt
            i = i + 1
            ! round jdi to lsync frequency to match flexpart
            call caldate(buffer(c,1), jjjjmmdd, hhmiss)
            hhmiss = int(real(hhmiss)/real((lsync/60)*100))*lsync*100/60
            obs%jdi(i) = juldate(jjjjmmdd, hhmiss)
            obs%jdf(i) = obs%jdi(i) + real(freq,kind=8)/24d0
            obs%lat(i) = buffer(c,2)
            obs%lon(i) = buffer(c,3)
            obs%alt(i) = buffer(c,4)
            obs%con(i) = buffer(c,5)
            obs%err(i) = buffer(c,6)
            obs%num(i) = 1
          end do
        endif
      endif
      if ( n.gt.nobs ) go to 20
      jdate = jdt
      cnt = 0
      buffer(:,:) = 0.
    end do
20  continue
    nobs = i
    print*, 'Number observations after selection/averaging: ',nobs
  endif ! averaging and/or selection

  ! if no selection and no averaging
  if( settings%lselect.eq.0.and.settings%laverage.eq.0) then
     do n = 1, nobs
       ! round jdi to lsync frequency to match flexpart
       call caldate(jdobs(n), jjjjmmdd, hhmiss)
       hhmiss = int(real(hhmiss)/real((lsync/60)*100))*lsync*100/60
       obs%jdi(n) = juldate(jjjjmmdd, hhmiss)
       obs%jdf(n) = obs%jdi(n) + real(freq,kind=8)/24d0
       ! if interval crosses month set to min of current month
       eomday = calceomday(jjjjmmdd/100)
       jdsom = juldate((jjjjmmdd/100)*100+1, 0)
       jdeom = jdsom + real(eomday,kind=8)
       obs%jdf(n) = min(obs%jdf(n),jdeom)       
       ! check that not beyond end of run time
       obs%jdf(n) = min(obs%jdf(n),jdatef)
       obs%lat(n) = latobs(n)
       obs%lon(n) = lonobs(n)
       obs%alt(n) = altobs(n)
       obs%con(n) = concobs(n)
       obs%err(n) = errobs(n)
       obs%num(n) = 1
     end do
  endif

end subroutine process_obs



