!---------------------------------------------------------------------------------------
! PREP_FLEXPART: prep_outgrid
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
!> prep_outgrid
!!
!! Purpose:    Prepares the options file OUTGRID
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
!---------------------------------------------------------------------------------------

subroutine prep_outgrid(settings, jd, nr)

  use mod_var
  use mod_settings
  use mod_dates
  use mod_tools

  implicit none

  type (settings_t), intent(in)      :: settings
  real(kind=8),      intent(in)      :: jd
  integer,           intent(in)      :: nr

  character(len=max_path_len)        :: filename
  character(len=6)                   :: adate
  integer                            :: jjjjmm, jjjjmmdd, hhmiss
  integer                            :: datei, datef, eomday
  integer                            :: ierr, n

  real                               :: outlon0, outlat0, outlon0n, outlat0n
  integer                            :: numxgrid, numygrid, numxgridn, numygridn
  real                               :: dxout, dyout, dxoutn, dyoutn
  real, dimension(settings%numzgrid) :: outheights

  namelist /outgrid/ &
    outlon0, &
    outlat0, &
    numxgrid, &
    numygrid, &
    dxout, &
    dyout, &
    outheights

  namelist /outgridn/ &
    outlon0n, &
    outlat0n, &
    numxgridn, &
    numygridn, &
    dxoutn, &
    dyoutn

  ! write OUTGRID

  ! preset namelist variables
  outlon0 = settings%outlonleft
  ! correction for FLEXPART windfields
  if ( settings%outlonleft.le.-180. ) then
    if ( settings%windfield.eq.'oper' ) then
      outlon0 = -179.
    else if ( settings%windfield.eq.'era5' ) then
      outlon0 = -179.5
    else
      write(*,*) 'ERROR: prep_outgrid: unknown windfield type'
      stop
    endif
  endif
  outlat0 = settings%outlatlower
  numxgrid = settings%numxgrid
  numygrid = settings%numygrid
  dxout = settings%dxout
  dyout = settings%dyout
  do n = 1, settings%numzgrid
    outheights(n) = settings%zlevel(n)
  end do

  call caldate(jd, jjjjmmdd, hhmiss)
  write(adate,fmt='(I6.6)') jjjjmmdd/100
  filename = trim(settings%path_options)//trim(recname(nr))//'/'//adate//'/options/OUTGRID'
  open(100,file=trim(filename),status='replace',action='write',iostat=ierr)

  if( settings%lnamelist.eq.1 ) then
    ! use namelist file format
    write(100,nml=outgrid)
  else
    ! use old file format
    write(100,fmt='(A)') '********************************************************************************'
    write(100,fmt='(A)') '*                                                                              *'
    write(100,fmt='(A)') '*      Input file for the Lagrangian particle dispersion model FLEXPART        *'
    write(100,fmt='(A)') '*                       Please specify your output grid                        *'
    write(100,fmt='(A)') '*                                                                              *'
    write(100,fmt='(A)') '********************************************************************************'
    call skiplines(100,2) 
    write(100,fmt='(4X,F11.4)') settings%outlonleft
    call skiplines(100,3)
    write(100,fmt='(4X,F11.4)') settings%outlatlower
    call skiplines(100,3)
    write(100,fmt='(4X,I5)')    settings%numxgrid
    call skiplines(100,3)
    write(100,fmt='(4X,I5)')    settings%numygrid
    call skiplines(100,3)
    write(100,fmt='(4X,F12.5)') settings%dxout
    call skiplines(100,3)
    write(100,fmt='(4X,F12.5)') settings%dyout
    call skiplines(100,3)
    do n = 1, settings%numzgrid 
      write(100,fmt='(4X,F7.1)') settings%zlevel(n)
      call skiplines(100,3)
    end do
  endif

  close(100)
  
  ! write OUTGRID_NEST

  if( settings%lnested.eq.1 ) then

    ! preset namelist variables
    outlon0n = settings%outlonnest
    outlat0n = settings%outlatnest
    numxgridn = settings%numxnest
    numygridn = settings%numynest
    dxoutn = settings%dxoutnest
    dyoutn = settings%dyoutnest

    call caldate(jd, jjjjmmdd, hhmiss)
    write(adate,fmt='(I6.6)') jjjjmmdd/100
    filename = trim(settings%path_options)//trim(recname(nr))//'/'//adate//'/options/OUTGRID_NEST'
    open(100,file=trim(filename),status='replace',action='write',iostat=ierr)

    if( settings%lnamelist.eq.1 ) then
      ! use namelist file format
      write(100,nml=outgridn)
    else
      ! use old file format
      write(100,*) '********************************************************************************'
      write(100,*) '*                                                                              *'
      write(100,*) '*      Input file for the Lagrangian particle dispersion model FLEXPART        *'
      write(100,*) '*                       Please specify your output grid                        *'
      write(100,*) '*                                                                              *'
      write(100,*) '********************************************************************************'
      call skiplines(100,2)
      write(100,fmt='(F11.4)') settings%outlonnest
      call skiplines(100,3)
      write(100,fmt='(F11.4)') settings%outlatnest
      call skiplines(100,3)
      write(100,fmt='(I5)')    settings%numxnest
      call skiplines(100,3)
      write(100,fmt='(I5)')    settings%numynest
      call skiplines(100,3)
      write(100,fmt='(F10.3)') settings%dxoutnest
      call skiplines(100,3)
      write(100,fmt='(F10.3)') settings%dyoutnest
      call skiplines(100,3)
      do n = 1, settings%numzgrid
        write(100,fmt='(F7.1)') settings%zlevel(n)
        call skiplines(100,3)
      end do
      close(100)    
    endif
  
  endif

end subroutine prep_outgrid

