!!* ils_perfo_param_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: october 2013
!!
!> ils_perfo_param_type -- Module
!!
!! * Purpose
!!
!!   Module for type spectral performance parameters declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the spectral performance parameters type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Ils_Perfo_param   : type for declaration and allocation of spectral performance parameters
!!
!! * References
!!


module ils_perfo_param_type
   use precision_type
   use error_type
!
   implicit none
!
!
   public :: type_Ils_Perfo_Param
!
   type :: type_Ils_Perfo_Param
     character(len=500)                               :: filename !< file name
     real(kind=DOUBLE)                                :: WnIls !< ILS wavenumber (m-1)
     integer(kind=LONG)                               :: Ip !< flag for barycenter computation (linear/quadratic)  
     integer(kind=LONG)                               :: Iteration !< barycenter iteration number  
     integer(kind=LONG)                               :: OSFactor !< oversampling factor
     real(kind=DOUBLE)                                :: SDWn !< Spectral domain wavenumber (m-1)  
   end type type_Ils_Perfo_Param
!
!
end module ils_perfo_param_type
!
