!!#   interf_oversamp_module.f90 --
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: september 2012
!!#            Version: $Revision: $
!!#  Last modification: $Date: $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!>  interf oversampling -- Module
!!
!! * Purpose
!!
!!   Module for Interferogram Oversampling.
!!
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.8.12
!!

module interf_oversamp_module
   use precision_type
   use error_type
   use constantes_type
   use spectrum_type
   use interferogram_type
   use fft_module
   use math_module
!
   implicit none
!
!
   public :: interf_os_zero_padding
!
   contains
!
!
   subroutine interf_os_zero_padding( OSFactor, &
                                      Interf,   &
                                      Interf_os )
!
     integer(kind=LONG)      ,intent(in)                 :: OSFactor
     type(type_Interferogram),intent(in)                 :: Interf
     type(type_Interferogram),intent(out)                :: Interf_os
     type(type_Spectrum)                                 :: Spectrum
     type(type_Spectrum)                                 :: Spectrum_zeropad
!
!    oversampled interferogram initialisation
     Interf_os%Type           = Interf%Type
     Interf_os%FieldMeanAngle = Interf%FieldMeanAngle
     Interf_os%OpdMax         = Interf%OpdMax
     Interf_os%TimeMax        = Interf%TimeMax
     Interf_os%dOpd           = Interf%dOpd / dble( OSFactor )
     Interf_os%N_Sample       = ( ( Interf%N_Sample-1 ) * OSFactor ) + 1
     Interf_os%dTime          = Interf%dTime / dble( OSFactor )
     Interf_os%N_Top          = Interf%N_Top
!
     call alloc_Interferogram( Interf_os )
     call Interferogram_Basis( Interf_os )
!
!    Spectrum initialisation
     Spectrum%Type     = 'C'
     Spectrum%dWn      = 1_DOUBLE / ( 2_DOUBLE * Interf%OpdMax )
     Spectrum%WnMax    = 1_DOUBLE / ( 2_DOUBLE * Interf%dOpd )
     Spectrum%Wn_First = 0_DOUBLE
     Spectrum%Wn_Last  = Spectrum%WnMax
     Spectrum%Ns_First = idnint( Spectrum%Wn_First / Spectrum%dWn ) + 1
     Spectrum%Ns_Last  = idnint( Spectrum%Wn_Last  / Spectrum%dWn ) + 1
     Spectrum%N_Sample = Spectrum%Ns_Last - Spectrum%Ns_First + 1
!
     call alloc_Spectrum( Spectrum )
     call Spectrum_Basis( Spectrum )
!
!    Spectrum computation
     call iftosp_open( Interf, Spectrum )
!
!    Spectrum zero_pad initialisation
     Spectrum_zeropad%Type     = 'C'
     Spectrum_zeropad%dWn      = 1_DOUBLE / ( 2_DOUBLE * Interf_os%OpdMax )
     Spectrum_zeropad%WnMax    = 1_DOUBLE / ( 2_DOUBLE * Interf_os%dOpd )
     Spectrum_zeropad%Wn_First = 0_DOUBLE
     Spectrum_zeropad%Wn_Last  = Spectrum_zeropad%WnMax
     Spectrum_zeropad%Ns_First = idnint( Spectrum_zeropad%Wn_First &
                                       / Spectrum_zeropad%dWn ) + 1
     Spectrum_zeropad%Ns_Last  = idnint( Spectrum_zeropad%Wn_Last  &
                                       / Spectrum_zeropad%dWn ) + 1
     Spectrum_zeropad%N_Sample = Spectrum_zeropad%Ns_Last    &
                               - Spectrum_zeropad%Ns_First + 1
!
     call alloc_Spectrum( Spectrum_zeropad )
     call Spectrum_Basis( Spectrum_zeropad )
!
!    Spectrum transfert
     Spectrum_zeropad%Complex(1:Spectrum_zeropad%N_Sample) = 0_DOUBLE
     Spectrum_zeropad%Complex(1:Spectrum%N_Sample) =                &
                                Spectrum%Complex(1:Spectrum%N_Sample)
!
!    Oversampled interferogram computation
     call sptoif( Spectrum_zeropad, Interf_os )
!
!    deallocation
     call dalloc_Spectrum( Spectrum )
     call dalloc_Spectrum( Spectrum_zeropad )
!
     return
   end subroutine interf_os_zero_padding
!
!
end module interf_oversamp_module
