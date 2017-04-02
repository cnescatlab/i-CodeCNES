!!#  compression_module.f90 --
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: march 2010
!!#            Version: $Revision: 1.2 $
!!#  Last modification: $Date: 2010-11-03 09:18:58 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!> compression -- Module
!!
!! * Purpose
!!
!!    Module for spectrum compression
!!
!! * Description
!!      
!!   This module allows the spectrum compression
!!
!! * Sub-routines and functions
!!
!! -  band_merging : spectra merging
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : AD
!!


module compression_module
   use precision_type
   use error_type
   use constantes_type
   use interf_param_type
   use spectrum_type
!
   implicit none
!
!
   public :: band_merging
!
   contains
!
!

!> band_merging -- Public
!!
!! * Purpose
!!
!!     Spectrum merging
!!
!! * Description
!! 
!!     This subroutine allows the merging of spectra which are spectral band dependant
!!
!! * Inputs
!!
!!     - Interf_Param : type_Interf_Param / type for declaration and allocation of interferogram
!!                      parameters
!!     - Spectrum     : type_Spectrum / type for declaration and allocation of spectrum
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Spectrum_Merged : type_Spectrum / type for declaration and allocation of spectrum
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : AD
!!

   subroutine band_merging( Interf_Param,   &
                            Spectrum,       &
                            Spectrum_Merged )
   implicit none
     type(type_Interf_Param),intent(in)                   :: Interf_Param
     type(type_Spectrum),intent(in) ,dimension(1:Interf_Param%Nband) &
                                                          :: Spectrum
     type(type_Spectrum),intent(out)                      :: Spectrum_Merged
     integer(kind=LONG)                                   :: SB
     integer(kind=LONG)                                   :: Ns0
!
     do SB = 1, Interf_Param%Nband-1
        if( Spectrum(SB)%Type /= Spectrum(SB+1)%Type ) then
           write(*,*) 'Spectrum Type Error', SB,  &
                       Spectrum(SB)%Type,Spectrum(SB+1)%Type
           write(*,*) 'band_merging Fatal Error'
           call exit(1)
        end if
        if( Spectrum(SB)%dWn /= Spectrum(SB+1)%dWn ) then
           write(*,*) 'Spectrum dWn Error', SB,  &
                       Spectrum(SB)%dWn,Spectrum(SB+1)%dWn
           write(*,*) 'band_merging Fatal Error'
           call exit(1)
        end if
     end do
     
     Spectrum_Merged%Type     = Spectrum(1)%Type
     Spectrum_Merged%Ns_First = Spectrum(1)%Ns_First
     Spectrum_Merged%Ns_Last  = Spectrum(Interf_Param%Nband)%Ns_Last
     Spectrum_Merged%Wn_First = Spectrum(1)%Wn_First
     Spectrum_Merged%Wn_Last  = Spectrum(Interf_Param%Nband)%Wn_Last
     Spectrum_Merged%N_Sample = Spectrum_Merged%Ns_Last  &
                              - Spectrum_Merged%Ns_First+1
     Spectrum_Merged%dWn      = Spectrum(1)%dWn
     Spectrum_Merged%WnMax    = Spectrum(Interf_Param%Nband)%WnMax
     call alloc_Spectrum( Spectrum_Merged )
     call Spectrum_Basis( Spectrum_Merged )
     Ns0 = Spectrum_Merged%Ns_First - 1
     do SB = 1, Interf_Param%Nband
        if( Spectrum_Merged%Type(1:len_trim(Spectrum_Merged%Type))     &
                                                          == 'C'  ) then
           Spectrum_Merged%Complex( Spectrum(SB)%Ns_First-Ns0:    &
                                    Spectrum(SB)%Ns_Last-Ns0  ) = &
                        Spectrum(SB)%Complex(1:Spectrum(SB)%N_Sample)
        else if( Spectrum_Merged%Type(1:len_trim(Spectrum_Merged%Type)) &
                                                           == 'RI' ) then
           Spectrum_Merged%Real_Part( Spectrum(SB)%Ns_First-Ns0:    &
                                      Spectrum(SB)%Ns_Last-Ns0  ) = &
                        Spectrum(SB)%Real_Part(1:Spectrum(SB)%N_Sample)
           Spectrum_Merged%Imag_Part( Spectrum(SB)%Ns_First-Ns0:    &
                                      Spectrum(SB)%Ns_Last-Ns0  ) = &
                        Spectrum(SB)%Imag_Part(1:Spectrum(SB)%N_Sample)
        else if( Spectrum_Merged%Type(1:len_trim(Spectrum_Merged%Type)) &
                                                           == 'MA' ) then
           Spectrum_Merged%Modulus( Spectrum(SB)%Ns_First-Ns0:    &
                                    Spectrum(SB)%Ns_Last-Ns0  ) = &
                        Spectrum(SB)%Modulus(1:Spectrum(SB)%N_Sample)
           Spectrum_Merged%Argument( Spectrum(SB)%Ns_First-Ns0:    &
                                     Spectrum(SB)%Ns_Last-Ns0  ) = &
                        Spectrum(SB)%Argument(1:Spectrum(SB)%N_Sample)
        else if( Spectrum_Merged%Type(1:len_trim(Spectrum_Merged%Type)) &
                                                           == 'R'  ) then
           Spectrum_Merged%Real_Part( Spectrum(SB)%Ns_First-Ns0:    &
                                      Spectrum(SB)%Ns_Last-Ns0  ) = &
                        Spectrum(SB)%Real_Part(1:Spectrum(SB)%N_Sample)
        else if( Spectrum_Merged%Type(1:len_trim(Spectrum_Merged%Type)) &
                                                           == 'I'  ) then
           Spectrum_Merged%Imag_Part( Spectrum(SB)%Ns_First-Ns0:    &
                                      Spectrum(SB)%Ns_Last-Ns0  ) = &
                        Spectrum(SB)%Imag_Part(1:Spectrum(SB)%N_Sample)
        else if( Spectrum_Merged%Type(1:len_trim(Spectrum_Merged%Type)) &
                                                           == 'M'  ) then
           Spectrum_Merged%Modulus( Spectrum(SB)%Ns_First-Ns0:    &
                                    Spectrum(SB)%Ns_Last-Ns0  ) = &
                        Spectrum(SB)%Modulus(1:Spectrum(SB)%N_Sample)
        else if( Spectrum_Merged%Type(1:len_trim(Spectrum_Merged%Type)) &
                                                           == 'A'  ) then
           Spectrum_Merged%Argument( Spectrum(SB)%Ns_First-Ns0:    &
                                     Spectrum(SB)%Ns_Last-Ns0  ) = &
                        Spectrum(SB)%Argument(1:Spectrum(SB)%N_Sample)
        else
           write(*,*) 'Spectrum_Merged%Type Error',Spectrum_Merged%Type
           write(*,*) 'alloc_Spectrum Fatal Error'
           call exit(1)
        end if
     end do
     return
   end subroutine band_merging
!
!
end module compression_module
