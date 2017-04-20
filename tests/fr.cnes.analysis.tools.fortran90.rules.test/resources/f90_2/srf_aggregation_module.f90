!!#  srf_aggregation_module.f90 --
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: april 2011
!!#            Version: $Revision: 1.1 $
!!#  Last modification: $Date: 2011-06-22 10:06:26 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!> srf aggregation -- Module
!!
!! * Purpose
!!
!!    Module for srf aggregation
!!
!! * Description
!!      
!!   This module allows the aggregation of two spectral response functions
!!
!! * Sub-routines and functions
!!
!! -  srf_aggregation : aggregation of two srf
!!
!! * References
!!
!!     SPS ATBD
!!

module srf_aggregation_module
   use precision_type
   use error_type
   use constantes_type
   use srf_type
!
   implicit none
!
!
   public :: srf_aggregation
!
   contains
!
!

!> srf_aggregation -- Public
!!
!! * Purpose
!!
!!     Aggregation of two srf
!!
!! * Description
!! 
!!     Two srf with the same number of samples can be aggregated. 
!!
!! * Inputs
!!
!!     - Srf : type_Srf / type for declaration and allocation of srf
!!     - NbSUB_PN : Number of sub-pixel to aggregate
!!
!! * Outputs
!!
!!     - Srf_agreg : type_Srf / type for declaration and allocation of srf
!!
!! * References
!!
!!     SPS ATBD 
!!

   subroutine srf_aggregation( Srf,  NbSUB_PN, Srf_agreg )
   implicit none
     type(type_Srf)        , intent(in)    :: Srf
     integer(kind=LONG)    , intent(in)    :: NbSUB_PN               
     type(type_Srf)        , intent(inout) :: Srf_agreg
!
!    coherence control
     if( (Srf_agreg%NsPixel /= Srf%NsPixel) .or.   &
         (Srf_agreg%NsWn0 /= Srf%NsWn0) .or.       &
         (Srf_agreg%NsSDWn1a /= Srf%NsSDWn1a) .or. &
         (Srf_agreg%NsSDWn1b /= Srf%NsSDWn1b) .or. &
         (Srf_agreg%NsSDWn1c /= Srf%NsSDWn1c) ) then
        write(*,*) ' N_Sample not coherent'
        write(*,*) 'srf_aggregation Fatal Error'
        call exit(1)
     end if
     Srf_agreg%L1a(:,:,:) = Srf_agreg%L1a(:,:,:)                      &
                          + (Srf%L1a(:,:,:)/real(NbSUB_PN,kind=DOUBLE))
     Srf_agreg%L1b(:,:,:) = Srf_agreg%L1b(:,:,:)                      &
                          + (Srf%L1b(:,:,:)/real(NbSUB_PN,kind=DOUBLE))
     Srf_agreg%L1c(:,:,:) = Srf_agreg%L1c(:,:,:)                      &
                          + (Srf%L1c(:,:,:)/real(NbSUB_PN,kind=DOUBLE))

     Srf_agreg%WnShift1a(:,:) = Srf_agreg%WnShift1a(:,:)                      &
                              + (Srf%WnShift1a(:,:)/real(NbSUB_PN,kind=DOUBLE))
     Srf_agreg%WnShift1b(:,:) = Srf_agreg%WnShift1b(:,:)                      &
                              + (Srf%WnShift1b(:,:)/real(NbSUB_PN,kind=DOUBLE))
     Srf_agreg%WnShift1c(:,:) = Srf_agreg%WnShift1c(:,:)                      &
                              + (Srf%WnShift1c(:,:)/real(NbSUB_PN,kind=DOUBLE))
!
     Srf_agreg%Fcs1a(:,:) = Srf_agreg%Fcs1a(:,:)                      &
                          + (Srf%Fcs1a(:,:)/real(NbSUB_PN,kind=DOUBLE))
     Srf_agreg%Fcs1b(:,:) = Srf_agreg%Fcs1b(:,:)                      &
                          + (Srf%Fcs1b(:,:)/real(NbSUB_PN,kind=DOUBLE))
     Srf_agreg%Fcs1c(:,:) = Srf_agreg%Fcs1c(:,:)                      &
                          + (Srf%Fcs1c(:,:)/real(NbSUB_PN,kind=DOUBLE))
!
     Srf_agreg%FWhm1a(:,:) = Srf_agreg%FWhm1a(:,:)                      &
                           + (Srf%FWhm1a(:,:)/real(NbSUB_PN,kind=DOUBLE))
     Srf_agreg%FWhm1b(:,:) = Srf_agreg%FWhm1b(:,:)                      &
                           + (Srf%FWhm1b(:,:)/real(NbSUB_PN,kind=DOUBLE))
     Srf_agreg%FWhm1c(:,:) = Srf_agreg%FWhm1c(:,:)                      &
                           + (Srf%FWhm1c(:,:)/real(NbSUB_PN,kind=DOUBLE))
!
     return
   end subroutine srf_aggregation
!
!
 end module srf_aggregation_module
