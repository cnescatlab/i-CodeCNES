!!* spect_interf_index_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: july 2013
!!
!> spectrum_type -- Module
!!
!! * Purpose
!!
!!   Module for spectrum and interferogram top level type declaration.
!!
!! * Description
!!      
!!     This module defines the top level Spectrum and Interferogram type declaration
!!
!! * Sub-routines and functions
!!
!!     *  type_spect_interf_index  : type for declaration of Spectrum and inteferogram top level fields
!!
!! * References
!!     
!!      SPS ATBD
!!
module spect_interf_index_type
   use precision_type
   use header_type
!
   implicit none
!
!
   public :: type_spect_interf_index
!
   type :: type_spect_interf_index
     type(type_header)  :: header  ! < type header
     integer(kind=LONG) :: NcolFov ! < number of column of the FoV matrix
     integer(kind=LONG) :: NlinFov ! < number of line of the FoV matrix
     integer(kind=LONG) :: NsubFov ! < sub-pixel number
     integer(kind=LONG) :: Nband   ! < spectral band number
     integer(kind=LONG) :: NFor    ! < number of Field of Regard
     integer(kind=LONG) :: Nline   ! < number of lines of the product
     integer(kind=LONG) :: ColFov  ! < current column of the FoV matrix
     integer(kind=LONG) :: LinFov  ! < current line of the FoV matrix
     integer(kind=LONG) :: SubFov  ! < current sub-pixel number
     integer(kind=LONG) :: Band    ! < current spectral band
     integer(kind=LONG) :: For     ! < current Field of Regard
     integer(kind=LONG) :: Line    ! < current lines of the product
     character(len=2)   :: Target  !< spectrum target
   end type type_spect_interf_index
!
!
end module spect_interf_index_type
