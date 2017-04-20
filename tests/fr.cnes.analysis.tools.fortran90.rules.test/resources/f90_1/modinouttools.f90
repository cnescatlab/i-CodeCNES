! modinouttools --
!
! modinouttools -- Module
!
! * Purpose
!   Module for type declaration.
!
module modinouttools
   use precision_type
!
   implicit none
!
   public :: open_file_r  &
            ,open_file_w  &
            ,open_file_rw &
            ,open_file_ad &
            ,read_field   &
            ,write_field  &
            ,close_file   &
            ,b1Type       &
            ,l1Type       &
            ,ch1Type      &
            ,ch2Type      &
            ,ch4Type      &
            ,ch8Type      &
            ,i2Type       &
            ,i4Type       &
            ,r4Type       &
            ,r8Type       &
            ,c8Type       &
            ,c16Type       
!
      external open_file_r   !$pragma C( open_file_r )
      external open_file_w   !$pragma C( open_file_w )
      external open_file_rw  !$pragma C( open_file_rw )
      external open_file_ad  !$pragma C( open_file_ad )
      external read_field    !$pragma C( read_field )
      external write_field   !$pragma C( write_field )
      external close_file    !$pragma C( close_file )
!        !$pragma C( open_file_r )
!        !$pragma C( open_file_w )
!        !$pragma C( open_file_rw )
!        !$pragma C( open_file_ad )
!        !$pragma C( read_field )
!        !$pragma C( write_field )
!        !$pragma C( close_file )

!     Valeur possible pour fieldtype
      integer(kind=LONG) , parameter :: b1Type  = 0
      integer(kind=LONG) , parameter :: l1Type  = 1
      integer(kind=LONG) , parameter :: ch1Type = 2
      integer(kind=LONG) , parameter :: ch2Type = 3
      integer(kind=LONG) , parameter :: ch4Type = 4
      integer(kind=LONG) , parameter :: ch8Type = 5
      integer(kind=LONG) , parameter :: i2Type  = 6
      integer(kind=LONG) , parameter :: i4Type  = 7
      integer(kind=LONG) , parameter :: r4Type  = 8
      integer(kind=LONG) , parameter :: r8Type  = 8
      integer(kind=LONG) , parameter :: c8Type  = 10
      integer(kind=LONG) , parameter :: c16Type = 11
!
end module modinouttools
