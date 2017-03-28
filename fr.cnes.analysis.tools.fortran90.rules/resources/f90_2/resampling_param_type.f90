!!* resampling_param_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: october 2009
!!*           Version: $Revision: 1.4 $
!!* Last modification: $Date: 2011-11-02 14:57:53 $
!!
!> type_resampling_param -- Module
!!
!! * Purpose
!!
!!   Module for type resampling parameters declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the resampling parameters type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Resampling_Param   : type for declaration and allocation of Resampling_Param
!!     * alloc_Resampling_Param  : type_Resampling_Param allocation
!!     * dalloc_Resampling_Param : type_Resampling_Param deallocation
!!
!! * References
!!

module resampling_param_type
   use precision_type
   use error_type
!
   implicit none
!
!
   public :: type_Resampling_Param,  &
             alloc_Resampling_Param, &
             dalloc_Resampling_Param
!
   type :: type_Resampling_Param
     character(len=500)   :: filename !< resampling characterisation file name
     character(len=10)    :: Sampling_Mode !< sampling mode (reference spline lagrange)
     character(len=3)     :: Field_Compensation !< flag for applying or not the field compensation
     real(kind=DOUBLE)    :: FieldMeanAngle !< field mean angle
     real(kind=DOUBLE)    :: OpdMax !< maximum of optical path difference (m)
     real(kind=DOUBLE)    :: dWn !< wavenumber sampling 
     integer(kind=LONG)   :: Ns_Fft !< Fourier transform samples number
     integer(kind=LONG)   :: Ns_Margin !< Margins samples number
     integer(kind=LONG)   :: OSFactor !< scaling factor
     integer(kind=LONG)   :: N_Sample !<  sample number used for interpolation
     real(kind=DOUBLE)    :: dOpd !<  resampled Opd step
   end type type_Resampling_Param
!
   contains
!
!
   subroutine alloc_Resampling_Param( Resampling_Param )
   implicit none
     type(type_Resampling_Param), intent(inout)            :: Resampling_Param
     integer(kind=LONG)                                    :: ErrCode
!
     ErrCode = 0
     if (ErrCode > 0) then
        write(0,*) 'allocation Resampling_Param Error'
        write(0,*) 'Resampling_Param: fatal error' 
        call exit(1)
     end if
     return
   end subroutine alloc_Resampling_Param
!
!
   subroutine dalloc_Resampling_Param( Resampling_Param )
   implicit none
     type(type_Resampling_Param), intent(inout)               :: Resampling_Param
!
!
     return
   end subroutine dalloc_Resampling_Param
!
!
end module resampling_param_type
