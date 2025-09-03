!---------------------------------------------------------------------------------------
! PREP_FLEXPART: mod_dates
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
!> mod_dates
!!
!! Purpose:    Module of date functions.
!!
!---------------------------------------------------------------------------------------

module mod_dates

  implicit none

  contains

  ! --------------------------------------------------
  ! caldate
  ! --------------------------------------------------
  !> caldate
  !! 
  !! Purpose:  Converts a julian date number to date 
  !!           and time format YYYYMMDD, HHMISS
  !!           according to the gregorian calender
  !!
  ! --------------------------------------------------

  subroutine caldate(jdate, yyyymmdd, hhmiss)

    real(kind=8), intent(in) :: jdate
    integer, intent(out)     :: yyyymmdd, hhmiss
    integer                  :: yyyy, mm, dd, hh, mi, ss
    integer                  :: julday, ja, jb, jc, jd, je, jalpha
    integer, parameter       :: igreg = 2299161

    julday=int(jdate)
    if(julday.ge.igreg) then
      jalpha=int(((julday-1867216)-0.25)/36524.25)
      ja=julday+1+jalpha-int(0.25*jalpha)
    else
      ja=julday
    endif
    jb=ja+1524
    jc=int(6680.+((jb-2439870)-122.1)/365.25)
    jd=365*jc+int(0.25*jc)
    je=int((jb-jd)/30.6001)
    dd=jb-jd-int(30.6001*je)
    mm=je-1
    if(mm.gt.12) mm=mm-12
    yyyy=jc-4715
    if(mm.gt.2) yyyy=yyyy-1
    if(yyyy.le.0) yyyy=yyyy-1

    yyyymmdd=10000*yyyy+100*mm+dd
    hh=int(24.*(jdate-float(julday)))
    mi=int(1440.*(jdate-float(julday))-60.*float(hh))
    ss=nint(86400.*(jdate-float(julday))-3600.*float(hh))-60.*float(mi)
    if(ss.eq.60) then  ! 60 seconds = 1 minute
      ss=0
      mi=mi+1
    endif
    if(mi.eq.60) then
      mi=0
      hh=hh+1
    endif
    hhmiss=10000*hh+100*mi+ss

  end subroutine caldate

  ! --------------------------------------------------
  ! julday
  ! --------------------------------------------------
  !> julday
  !! 
  !! Purpose:  Converts the date and time format 
  !!           YYYYMMDD and HHMMSS to a julian date 
  !!           number.
  !!
  ! --------------------------------------------------

  real(kind=8) function juldate(yyyymmdd,hhmiss)

    integer, intent(in) :: yyyymmdd,hhmiss
    integer :: yyyy,mm,hh,dd,mi,ss
    integer :: julday,jy,jm,ja
    integer, parameter :: igreg=15+31*(10+12*1582)

    yyyy=yyyymmdd/10000
    mm=(yyyymmdd-10000*yyyy)/100
    dd=yyyymmdd-10000*yyyy-100*mm
    hh=hhmiss/10000
    mi=(hhmiss-10000*hh)/100
    ss=hhmiss-10000*hh-100*mi

    if(yyyy.eq.0) print*, 'ERROR: there is no year zero'
    if(yyyy.lt.0) yyyy=yyyy+1
    if(mm.gt.2) then
      jy=yyyy
      jm=mm+1
    else
      jy=yyyy-1
      jm=mm+13
    endif
    julday=int(365.25*jy)+int(30.6001*jm)+dd+1720995
    if (dd+31*(mm+12*yyyy).ge.igreg) then
      ja=int(0.01*jy)
      julday=julday+2-ja+int(0.25*ja)
    endif

    juldate=dble(float(julday))+dble(float(hh)/24.)&
             &+dble(float(mi)/1440.)+dble(float(ss)/86400.)

  end function juldate

  ! --------------------------------------------------
  ! calceomday
  ! --------------------------------------------------
  !> calceomday
  !! 
  !! Purpose:  Calculates number of days in a given 
  !!           year, month currently only considers 
  !!           years after 1900.
  !!
  ! --------------------------------------------------

  integer function calceomday(yyyymm)

    integer, intent(in) :: yyyymm
    integer :: yyyy,mm
    integer, dimension(12) :: leapdays,days
    integer :: eomday

    leapdays=(/31,29,31,30,31,30,31,31,30,31,30,31/)
    days=(/31,28,31,30,31,30,31,31,30,31,30,31/)

    yyyy=floor(yyyymm/100.)
    mm=yyyymm-yyyy*100

    if((float(yyyy)/100.).eq.float(yyyy/100)) then
      if((float(yyyy)/400.).eq.float(yyyy/400)) then
        eomday=leapdays(mm)
      else
        eomday=days(mm)
      endif
    else
      if((float(yyyy)/4.).eq.float(yyyy/4)) then
        eomday=leapdays(mm)
      else
        eomday=days(mm)
      endif
    endif

    calceomday=eomday

  end function calceomday

  ! --------------------------------------------------

end module mod_dates


