!!#  aggregation_module.f90 --
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: october 2009
!!#            Version: $Revision: 1.4 $
!!#  Last modification: $Date: 2011-06-22 10:06:26 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!> interferogram aggregation -- Module
!!
!! * Purpose
!!
!!    Module for interferogram aggregation
!!
!! * Description
!!      
!!   This module allows the aggregation of two interferograms
!!
!! * Sub-routines and functions
!!
!! -  interf_aggregation : aggregation of two interferograms
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.8.8
!!

module aggregation_module
   use precision_type
   use error_type
   use constantes_type
   use math_module
   use interferogram_type
!
   implicit none
!
!
   public :: interf_aggregation
!
   contains
!
!

!> interf_aggregation -- Public
!!
!! * Purpose
!!
!!     Aggregation of two interferograms
!!
!! * Description
!! 
!!     Two resampled interferograms with the same number of samples can be aggregated. 
!!
!! * Inputs
!!
!!     - Interf : type_Interferogram / type for declaration and allocation of interferogram
!! 
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Interf_agreg : type_Interferogram / type for declaration and allocation of interferogram
!!
!! * References
!!
!!     SPS ATBD
!!

   subroutine interf_aggregation( Interf,      &
                                  Interf_agreg )
   implicit none
     type(type_Interferogram)   , intent(in)              :: Interf
     type(type_Interferogram)   , intent(inout)           :: Interf_agreg
!
!    coherence control
     if( Interf_agreg%N_Sample /= Interf%N_Sample ) then
        write(*,*) ' N_Sample not coherent',  &
                     Interf_agreg%N_Sample,   &
                     Interf%N_Sample
        write(*,*) 'interf_aggregation Fatal Error'
        call exit(1)
     end if
     if( Interf_agreg%Type == 'R' ) then
        Interf_agreg%Real_Part(1:Interf_agreg%N_Sample) =             &
                       Interf_agreg%Real_Part(1:Interf_agreg%N_Sample)&
                     + Interf%Real_Part(1:Interf_agreg%N_Sample)
     else if( Interf_agreg%Type == 'C' ) then
        Interf_agreg%Complex(1:Interf_agreg%N_Sample) =             &
                       Interf_agreg%Complex(1:Interf_agreg%N_Sample)&
                     + Interf%Complex(1:Interf_agreg%N_Sample)
     else
        write(*,*) 'Interferogram Type ERROR'
        write(*,*) 'interf_aggregation Fatal Error'
        call exit(1)
     end if
!
     return
   end subroutine interf_aggregation
!
!
end module aggregation_module
