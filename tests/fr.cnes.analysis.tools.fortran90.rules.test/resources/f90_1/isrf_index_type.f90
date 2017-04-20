!!* isrf_index_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: july 2013
!!
!> spectrum_type -- Module
!!
!! * Purpose
!!
!!   Module for saf/srf/apf top level type declaration.
!!
!! * Description
!!      
!!     This module defines the top level saf/srf/apf type declaration
!!
!! * Sub-routines and functions
!!
!!     *  type_isrf_index  : type for declaration of saf/srf/apf top level fields
!!
!! * References
!!
!!      SPS ATBD
!!
module isrf_index_type
   use precision_type
   use header_type
!
   implicit none
!
!
   public :: type_isrf_index
!
   type :: type_isrf_index
     type(type_header)  :: header  ! < type header
     integer(kind=LONG) :: NcolFov ! < number of column of the FoV matrix
     integer(kind=LONG) :: NlinFov ! < number of line of the FoV matrix
     integer(kind=LONG) :: NsubFov ! < sub-pixel number
     integer(kind=LONG) :: NBand   ! < spectral band
     integer(kind=LONG) :: ColFov  ! < current column of the FoV matrix
     integer(kind=LONG) :: LinFov  ! < current line of the FoV matrix
     integer(kind=LONG), dimension(:), allocatable :: SubFov  ! < list of sub-pixel index
     integer(kind=LONG) :: Band    ! < current spectral band
   end type type_isrf_index
!
end module isrf_index_type
