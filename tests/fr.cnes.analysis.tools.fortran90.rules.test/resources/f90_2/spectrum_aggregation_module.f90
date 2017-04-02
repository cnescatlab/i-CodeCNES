!!#  spectrum_aggregation_module.f90 --
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: april 2011
!!#            Version: $Revision: 1.2 $
!!#  Last modification: $Date: 2011-06-22 10:06:26 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!> spectrum aggregation -- Module
!!
!! * Purpose
!!
!!    Module for spectrum aggregation
!!
!! * Description
!!      
!!   This module allows the aggregation of two spectra
!!
!! * Sub-routines and functions
!!
!! -  spectrum_aggregation : aggregation of two spectra
!!
!! * References
!!
!!     SPS ATBD
!!

module spectrum_aggregation_module
   use precision_type
   use error_type
   use constantes_type
   use spectrum_type
!
   implicit none
!
!
   public :: spectrum_aggregation
!
   contains
!
!

!> spectrum_aggregation -- Public
!!
!! * Purpose
!!
!!     Aggregation of two spectra
!!
!! * Description
!! 
!!     Two spectra with the same number of samples can be aggregated. 
!!
!! * Inputs
!!
!!     - Spectrum : type_Spectrum / type for declaration and allocation of spectrum
!!     -  NSUB_PN : Number of sub-pixel to aggregate
!!
!! * Outputs
!!
!!     - Spectrum_agreg : type_Spectrum / type for declaration and allocation of spectrum
!!
!! * References
!!
!!     SPS ATBD 
!!

   subroutine spectrum_aggregation( &
        Spectrum,                   &
        NSUB_PN,                    &
        Spectrum_agreg            )
   implicit none
     type(type_Spectrum)   , intent(in)    :: Spectrum
     integer(kind=LONG)    , intent(in)    ::  NSUB_PN               
     type(type_Spectrum)   , intent(inout) :: Spectrum_agreg
!
!    coherence control
     if( Spectrum_agreg%N_Sample /= Spectrum%N_Sample ) then
        write(*,*) ' N_Sample not coherent',  &
                     Spectrum_agreg%N_Sample,   &
                     Spectrum%N_Sample
        write(*,*) 'spectrum_aggregation Fatal Error'
        call exit(1)
     end if
     if( Spectrum_agreg%Type(1:len_trim(Spectrum_agreg%Type)) == 'R' ) then
        Spectrum_agreg%Real_Part(1:Spectrum_agreg%N_Sample) =    &
             Spectrum_agreg%Real_Part(1:Spectrum_agreg%N_Sample) &
             + Spectrum%Real_Part(1:Spectrum_agreg%N_Sample)     &
             / real(NSUB_PN,kind=DOUBLE)
     else
        write(*,*) 'Spectrum Type ERROR'
        write(*,*) 'spectrum_aggregation Fatal Error'
        call exit(1)
     end if
!
     return
   end subroutine spectrum_aggregation
!
!
 end module spectrum_aggregation_module
