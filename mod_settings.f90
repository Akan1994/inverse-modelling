!---------------------------------------------------------------------------------------
! PREP_FLEXPART: mod_settings
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
!> mod_settings
!!
!! Purpose:    Module for the construction of the settings data structure.
!!
!---------------------------------------------------------------------------------------

module mod_settings

  use mod_var

  implicit none
  private

  public :: settings_t, read_settings, parse_string

  ! settings_t contains all settings

  type :: settings_t

    character(len=max_path_len)     :: path_flexpart   
    character(len=max_path_len)     :: path_options    
    character(len=max_path_len)     :: path_output
    character(len=max_path_len)     :: path_ohfield
    character(len=max_path_len)     :: path_obs
    character(len=max_path_len)     :: path_obsout
    character(len=max_path_len)     :: file_recept
    character(len=max_path_len)     :: file_avail
    character(len=max_path_len)     :: file_availnest
    character(len=max_name_len)     :: windfield
    character(len=max_name_len)     :: obsformat
    character(len=max_name_len)     :: suffix
    integer                         :: lscratch
    integer                         :: FPversion

    integer                         :: nreagent
    character(len=max_path_len), dimension(:), allocatable :: path_reagents
    character(len=max_name_len), dimension(:), allocatable :: reagent_units
    integer, dimension(:), allocatable :: reagent_interp

    integer                         :: lselect
    integer                         :: lrelease
    integer                         :: lappendfile
    real                            :: relfreq
    integer                         :: laverage
    real                            :: intaverage
    integer                         :: lnamelist
    real                            :: measerr

    integer                         :: datei       
    integer                         :: datef          
    integer                         :: outrate          
    integer                         :: outaverage  
    integer                         :: outsample 
    integer                         :: ind_source 
    integer                         :: ind_receptor 
    integer                         :: lnested 
    integer                         :: linit_cond

    real                            :: outlonleft 
    real                            :: outlatlower 
    integer                         :: numxgrid 
    integer                         :: numygrid
    integer                         :: numzgrid
    real                            :: dxout
    real                            :: dyout
    real                            :: outlonnest
    real                            :: outlatnest  
    integer                         :: numxnest  
    integer                         :: numynest
    real                            :: dxoutnest
    real                            :: dyoutnest
    real, dimension(:), allocatable :: zlevel
 
    character(len=10)               :: species           
    integer                         :: reldatei
    integer                         :: reldatef
    integer                         :: npart
    integer                         :: zref
    character(len=max_path_len)     :: file_orog

    integer                         :: ageclass

  end type settings_t

  contains

    ! --------------------------------------------------
    ! check if input is numeric
    ! --------------------------------------------------

    logical function is_numeric(string)

      character(len=200), intent(in) :: string
      real :: var
      integer :: ierr

      read(string,*,iostat=ierr) var
      if(ierr.eq.0) then
        is_numeric=.true.
      else
        is_numeric=.false.
      endif

    end function is_numeric

    ! --------------------------------------------------
    ! check if input is logical
    ! --------------------------------------------------

    logical function is_logical(string)

      character(len=200), intent(in) :: string
      logical :: var
      integer :: ierr

      read(string,*,iostat=ierr) var
      if(ierr.eq.0) then
        is_logical=.true.
      else
        is_logical=.false.
      endif

    end function is_logical

    ! --------------------------------------------------
    ! split string
    ! --------------------------------------------------

    subroutine split(str,delims,before,sep)

    ! Routine finds the first instance of a character from 'delims' in the
    ! the string 'str'. The characters before the found delimiter are
    ! output in 'before'. The characters after the found delimiter are
    ! output in 'str'. The optional output character 'sep' contains the 
    ! found delimiter. A delimiter in 'str' is treated like an ordinary 
    ! character if it is preceded by a backslash (\). If the backslash 
    ! character is desired in 'str', then precede it with another backslash.

    character(len=*) :: str, delims, before
    character,optional :: sep
    logical :: pres
    character :: ch,cha
    integer :: i, k, ibsl, ipos, iposa, lenstr

    pres=present(sep)
    str=adjustl(str)
    lenstr=len_trim(str)
    if(lenstr == 0) return        ! string str is empty
    k=0
    ibsl=0                        ! backslash initially inactive
    before=' '
    do i=1,lenstr
      ch=str(i:i)
      if(ibsl == 1) then          ! backslash active
        k=k+1
        before(k:k)=ch
        ibsl=0
        cycle
      end if
      if(ch == '\') then          ! backslash with backslash inactive
        k=k+1
        before(k:k)=ch
        ibsl=1
        cycle
      end if
      ipos=index(delims,ch)
      if(ipos == 0) then          ! character is not a delimiter
        k=k+1
        before(k:k)=ch
        cycle
      end if
      if(ch /= ' ') then          ! character is a delimiter that is not a space
        str=str(i+1:)
        if(pres) sep=ch
        exit
      end if
      cha=str(i+1:i+1)            ! character is a space delimiter
      iposa=index(delims,cha)
      if(iposa > 0) then          ! next character is a delimiter
        str=str(i+2:)
        if(pres) sep=cha
        exit
      else
        str=str(i+1:)
        if(pres) sep=ch
        exit
      end if
    end do
    if(i >= lenstr) str=''
    str=trim(str)
    before=trim(before)
    return

    end subroutine split

    ! --------------------------------------------------
    ! routine to identify and read in content
    ! --------------------------------------------------

    subroutine read_content (line, identifier, cc, cn, cl, match)

      character (len=200), intent (in) :: line, identifier
      character (len=200), intent (out) :: cc ! character content
      real(kind=8),        intent (out) :: cn ! numeric content
      logical,             intent (out) :: cl ! logical content
      logical,             intent (out) :: match

      integer :: n

      n = len_trim (identifier)

      ! default: line does not match, cc="", cn=-9900.0
      cc    = ""
      cn    = -9999.9
      match = .false.
      if ( len_trim (line) >= n ) then
        if ( line(:n) == identifier(:n) ) then
          cc = adjustl (line(n+1:))
          if ( is_numeric(cc) ) then
            read (line(n+1:),*) cn
          else
            if( is_logical(cc) ) read (line(n+1:),*) cl
          endif
          match = .true.
        end if
      end if

    end subroutine read_content

    ! --------------------------------------------------
    ! routine to parse a string
    ! --------------------------------------------------

    subroutine parse_string (str, delims, args, nargs)

    ! Parses the string 'str' into arguments args(1), ..., args(nargs) based on
    ! the delimiters contained in the string 'delims'. Preceding a delimiter in
    ! 'str' by a backslash (\) makes this particular instance not a delimiter.
    ! The integer output variable nargs contains the number of arguments found.

    character(len=*) :: str, delims
    character(len=len_trim(str)) :: strsav
    character(len=*), dimension(:) :: args
    integer :: i, na, lenstr, nargs

    strsav=str
    na=size(args)

    do i=1,na
      args(i)=' '
    end do
    nargs=0
    lenstr=len_trim(str)
    if(lenstr==0) return

    do
      if(len_trim(str) == 0) exit
      nargs=nargs+1
      call split(str,delims,args(nargs))
    end do
    str=strsav

    end subroutine parse_string

    ! --------------------------------------------------
    ! read settings
    ! --------------------------------------------------

    subroutine read_settings(filename, settings)

      character(len=200), intent(in)     :: filename
      type(settings_t),   intent(in out) :: settings

      integer            :: ierr, i, n
      character(len=200) :: line, identifier, cc
      real(kind=8)       :: cn
      logical            :: match, cl
      character(len=100), dimension(100) :: temp

     ! open file
      open (100, file = trim (filename), status = 'old', iostat=ierr)
      if(ierr.gt.0) then
        write (*,*) 'ERROR: cannot find SETTINGS'
        stop
      endif

      ! default settings
      settings%lscratch = 1  ! use scratch drive

      ! loop over lines
      read_loop: do
        read (100, fmt='(A)', iostat=ierr) line

        ! first check if line exists or eof was reached
        if ( ierr.gt.0 ) then
          write (*,*) 'ERROR: check ', filename
          exit read_loop
        else if ( ierr.lt.0 ) then
          exit read_loop
        else

          ! omit comments ("#") and empty lines
          if ( index (trim (line), "#") .eq. 1 ) cycle read_loop
          if ( len   (trim (line))      .eq. 0 ) cycle read_loop

          ! Path and file settings
          identifier = "lscratch:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%lscratch = int(cn)
          identifier = "path_flexpart:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%path_flexpart = cc
          identifier = "path_options:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%path_options = cc
          identifier = "path_output:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%path_output = cc
          identifier = "path_ohfield:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%path_ohfield = cc
          identifier = "path_obs:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%path_obs = cc
          identifier = "path_obsout:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%path_obsout = cc
          identifier = "obsformat:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%obsformat = cc
          identifier = "suffix:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%suffix = cc
          identifier = "file_recept:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%file_recept = cc
          identifier = "file_avail:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%file_avail = cc
          identifier = "file_availnest:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%file_availnest = cc
          identifier = "windfield:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%windfield = cc
          identifier = "FPversion:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%FPversion = int(cn)

          ! Species settings
          identifier = "nreagent:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%nreagent = int(cn)
          identifier = "path_reagents:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) then
            call parse_string(cc, ',', temp, n)
            if ( n.ne.settings%nreagent ) then
              write(*,*) 'ERROR reading SETTINGS: number of reagent paths not equal to nreagent'
              stop
            endif
            allocate( settings%path_reagents(settings%nreagent) )
            do i=1,settings%nreagent
              settings%path_reagents(i) = trim(temp(i))
            end do
          endif
          identifier = "reagent_units:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) then
            call parse_string(cc, ',', temp, n)
            if ( n.ne.settings%nreagent ) then
              write(*,*) 'ERROR reading SETTINGS: number of reagent units not equal to nreagent'
              stop
            endif
            allocate( settings%reagent_units(settings%nreagent) )
            do i=1,settings%nreagent
              settings%reagent_units(i) = trim(temp(i))
            end do
          endif
          identifier = "reagent_interp:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) then
            call parse_string(cc, ',', temp, n)
            if ( n.ne.settings%nreagent ) then
              write(*,*) 'ERROR reading SETTINGS: number of flags for interpolation not equal to nreagent'
              stop
            endif
            allocate( settings%reagent_interp(settings%nreagent) )
            do i=1,settings%nreagent
              read(temp(i),*) settings%reagent_interp(i) 
            end do
          endif

          ! General settings
          identifier = "lselect:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%lselect = int(cn)
          identifier = "lrelease:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%lrelease = int(cn)
          identifier = "lappendfile:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%lappendfile = int(cn)
          identifier = "relfreq:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%relfreq = real(cn)
          identifier = "laverage:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%laverage = int(cn)
          identifier = "intaverage:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%intaverage = real(cn)
          identifier = "lnamelist:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%lnamelist = int(cn)
          identifier = "measerr:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%measerr = real(cn)

          ! COMMAND settings
          identifier = "datei:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%datei = int(cn)    
          identifier = "datef:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%datef = int(cn)
          identifier = "outrate:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%outrate = int(cn)
          identifier = "outaverage:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%outaverage = int(cn)
          identifier = "outsample:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%outsample = int(cn)
          identifier = "ind_source:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%ind_source = int(cn)
          identifier = "ind_receptor:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%ind_receptor = int(cn)
          identifier = "lnested:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%lnested = int(cn)
          identifier = "linit_cond:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%linit_cond = int(cn)

          ! OUTGRID settings 
          identifier = "outlonleft:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%outlonleft = real(cn)
          identifier = "outlatlower:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%outlatlower = real(cn)
          identifier = "numxgrid:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%numxgrid = real(cn)
          identifier = "numygrid:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%numygrid = real(cn)
          identifier = "dxout:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%dxout = real(cn)
          identifier = "dyout:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%dyout = real(cn)
          identifier = "outlonnest:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%outlonnest = real(cn)
          identifier = "outlatnest:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%outlatnest = real(cn)
          identifier = "numxnest:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%numxnest = real(cn)
          identifier = "numynest:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%numynest = real(cn)
          identifier = "dxoutnest:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%dxoutnest = real(cn)
          identifier = "dyoutnest:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%dyoutnest = real(cn)
          identifier = "zlevel:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) then
            call parse_string (cc, ",", temp(:), n)
            if( .not.allocated(settings%zlevel) ) allocate( settings%zlevel(n) )
            settings%numzgrid = n
            do i = 1, n
              read(temp(i),*) settings%zlevel(i)
            enddo
          end if

          ! RELEASES settings
          identifier = "species:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%species = cc          
          identifier = "reldatei:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%reldatei = int(cn)
          identifier = "reldatef:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%reldatef = int(cn)
          identifier = "npart:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%npart = real(cn)
          identifier = "zref:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%zref = real(cn)
          identifier = "file_orog:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%file_orog = cc

          ! AGECLASS settings
          identifier = "ageclass:"
          call read_content (line, identifier, cc, cn, cl, match)
          if ( match ) settings%ageclass = int(cn)

        endif

      end do read_loop

      print*, 'path_flexpart: ',trim(settings%path_flexpart)
      print*, 'path_options: ',trim(settings%path_options)

      ! Some checks
      if ( settings%FPversion.lt.10.or.settings%FPversion.gt.11 ) then
        write(*,*) 'ERROR: FPversion must be either 10 or 11'
        stop
      endif
      if ( settings%lnamelist.ne.1.and.settings%FPversion.eq.11 ) then
        write(*,*) 'WARNING: only namelist file format available for FPversion 11'
        settings%lnamelist = 1
      endif
      
      nreagent = settings%nreagent

    end subroutine read_settings

    ! --------------------------------------------------

end module mod_settings

