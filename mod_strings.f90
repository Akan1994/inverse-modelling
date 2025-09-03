!---------------------------------------------------------------------------------------
! PREP_FLEXPART: mod_strings
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
!> mod_strings
!!
!! Purpose:    Module of string functions.
!!
!---------------------------------------------------------------------------------------

module mod_strings

  use mod_var

  implicit none

  character(*), private, parameter :: lower_case = 'abcdefghijklmnopqrstuvwxyz' 
  character(*), private, parameter :: upper_case = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' 

  public :: to_upper, to_lower, str_replace

  contains

    ! --------------------------------------------------
    ! to uppercase
    ! --------------------------------------------------
    !> to_upper
    !! 
    !! Purpose:  Converts a string to upper case
    !!
    ! --------------------------------------------------

    function to_upper( string_in ) result ( string_out ) 
 
      implicit none

      character(*), intent(in)      :: string_in 
      character( len( string_in ) ) :: string_out 
      integer                       :: i, n 

      string_out = string_in 
      do i = 1, len( string_out ) 
        ! find location of letter in lower case constant string 
        n = index( lower_case, string_in(i:i) ) 
        ! if current substring is a lower case letter, make it upper case 
        if ( n.ne.0 ) string_out(i:i) = upper_case(n:n) 
      end do

    end function to_upper 

    ! --------------------------------------------------
    ! to lowercase
    ! --------------------------------------------------
    !> to_lower
    !! 
    !! Purpose:  Converts a string to lower case
    !!
    ! --------------------------------------------------

    function to_lower( string_in ) result ( string_out )         

      implicit none

      character(*), intent(in)      :: string_in      
      character( len( string_in ) ) :: string_out      
      integer                       :: i, n

      string_out = string_in        
      do i = 1, len( string_out )   
        ! find location of letter in upper case constant string 
        n = index( upper_case, string_in(i:i) )      
        ! if current substring is a upper case letter, make it upper case 
        if ( n.ne.0 ) string_out(i:i) = lower_case(n:n)        
      end do

    end function to_lower

    ! --------------------------------------------------
    ! replace substring in string
    ! --------------------------------------------------
    !> str_replace
    !!
    !! Purpose:  Replaces a string pattern in a string
    !!
    ! --------------------------------------------------

    function str_replace( string_in, substr, repstr ) result ( string_out )

      implicit none

      character(*), intent(in)      :: string_in
      character(*), intent(in)      :: substr
      character(*), intent(in)      :: repstr
      character(max_path_len)       :: string_out
      character(max_path_len)       :: strtmp1, strtmp2
      integer                       :: strlen, replen, n

      strlen = len_trim(string_in)
      replen = len_trim(repstr)
      ! find first position of substr in string_in 
      n = index( string_in, substr, back=.false. )
      strtmp1 = string_in(1:n-1)
      ! find last position of substr in string_in 
      n = index( string_in, substr, back=.true. )      
      strtmp2 = string_in(n+replen:strlen)
      string_out = trim(strtmp1)//trim(repstr)//trim(strtmp2) 

    end function str_replace

    ! --------------------------------------------------

end module mod_strings


