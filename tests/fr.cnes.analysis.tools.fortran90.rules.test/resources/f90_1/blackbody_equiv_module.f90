! blackbody_equiv_module.f90 --
!
!           Project: SPS_GENERIC
!           Authors: NOVELTIS/B.TOURNIER
!              Date: february 2010
!           Version: $Revision: 1.3 $
! Last modification: $Date: 2010-11-03 14:55:31 $
!
! Fourre tout -- Module
!
! * Purpose
!   Module for computation of the equivallent BlackBody
!
module blackbody_equiv_module
   use precision_type
   use error_type
   use constantes_type
   use spectrum_type
   use plancklib_module
   use math_module
!
   implicit none
!
!
   public :: bb_temp_equiv, &
             photon_bb_tab, &
             photon_number
!
   contains
!
!
   subroutine bb_temp_equiv( Spectrum,    &
                             N_Temp,      &
                             NPhoton_Tab, &
                             BB_Temp_Tab, &
                             BB_Temp  )
   implicit none
     type(type_Spectrum),intent(in)                             :: Spectrum
     integer(kind=LONG) ,intent(in)                             :: N_Temp
     real(kind=DOUBLE)  ,intent(in) ,dimension(1:N_Temp)        :: NPhoton_Tab
     real(kind=DOUBLE)  ,intent(in) ,dimension(1:N_Temp)        :: BB_Temp_Tab
     real(kind=DOUBLE)  ,intent(inout)                          :: BB_Temp
     real(kind=DOUBLE)                                          :: NPhoton
     integer(kind=LONG)                                         :: N1
     integer(kind=LONG)                                         :: N2
     real(kind=DOUBLE)                                          :: p
!
!    Photon number computation
     call photon_number( Spectrum, NPhoton )
!
!    interpolation in the BB tabulation
     N1 = 1
     N2 = N_Temp
     call dichotomd( NPhoton, NPhoton_Tab(N1:N2), N1, N2 )
     if( N1 == N2 ) then
        BB_Temp = BB_Temp_Tab(N1)
     else
        p = ( NPhoton-NPhoton_Tab(N1) ) &
          / ( NPhoton_Tab(N2)-NPhoton_Tab(N1) )
        BB_Temp = ( (1.d+00-p) * BB_Temp_Tab(N1) ) + ( p * BB_Temp_Tab(N2) )
     end if
     return
   end subroutine bb_temp_equiv
!
!
   subroutine photon_bb_tab( Spectrum,    &
                             Temp_Start,  &
                             Temp_Step,   &
                             N_Temp,      &
                             NPhoton_Tab, &
                             BB_Temp_Tab  )
   implicit none
     type(type_Spectrum) ,intent(in)                            :: Spectrum
     real(kind=DOUBLE)   ,intent(in)                            :: Temp_Start
     real(kind=DOUBLE)   ,intent(in)                            :: Temp_Step
     integer(kind=LONG)  ,intent(in)                            :: N_Temp
     real(kind=DOUBLE)  ,intent(inout) ,dimension(1:N_Temp)     :: NPhoton_Tab
     real(kind=DOUBLE)  ,intent(inout) ,dimension(1:N_Temp)     :: BB_Temp_Tab
     type(type_Spectrum)                                        :: Spectrum_BB
     integer(kind=LONG)                                         :: NT
     integer(kind=LONG)                                         :: Ns
!
!    Spectrum parameters transfer
     Spectrum_BB%Type     = 'R'
     Spectrum_BB%N_Sample = Spectrum%N_Sample
     Spectrum_BB%Ns_First = Spectrum%Ns_First
     Spectrum_BB%Ns_Last  = Spectrum%Ns_Last
     Spectrum_BB%dWn      = Spectrum%dWn
     write(*,*)'Spectrum%N_Sample',Spectrum%N_Sample
     write(*,*)'Spectrum%Ns_First',Spectrum%Ns_First
     write(*,*)'Spectrum%Ns_Last',Spectrum%Ns_Last
     write(*,*) 'Spectrum%dWn',Spectrum%dWn
     call alloc_Spectrum( Spectrum_BB )
     call Spectrum_Basis( Spectrum_BB )
!
!    Temperature loop
     do NT = 1, N_Temp
!
!       Blackdody computation
        BB_Temp_Tab(NT) = Temp_Start + dble(NT-1)*Temp_Step
        do Ns = 1, Spectrum_BB%N_Sample
           call plkdirect( BB_Temp_Tab(NT),          &
                           Spectrum_BB%Wn(Ns),       &
                           Spectrum_BB%Real_Part(Ns) )
        end do
!
!       Photon number computation
        call photon_number( Spectrum_BB, NPhoton_Tab(NT) )
     end do
     call dalloc_Spectrum( Spectrum_BB )
     return
   end subroutine photon_bb_tab
!
!
   subroutine photon_number( Spectrum, NPhoton )
   implicit none
     type(type_Spectrum)     ,intent(in)                :: Spectrum
     real(kind=DOUBLE)       ,intent(out)               :: NPhoton
     real(kind=DOUBLE)                                  :: EPhoton
     integer(kind=LONG)                                 :: Ns
!
!    spectral loop
     NPhoton = 0.d+00
     do Ns = 1, Spectrum%N_Sample
!       Photon Energy
        EPhoton = Cst_h * Cst_c * Spectrum%Wn(Ns)
!
!       Number of photons in the band
        NPhoton = NPhoton + Spectrum%Real_Part(Ns)/EPhoton &
                          * Spectrum%dWn
     end do
     return
   end subroutine photon_number
!
!
end module blackbody_equiv_module
