!!#   bilatere_module.f90 --
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: february 2010
!!#            Version: $Revision: 1.7 $
!!#  Last modification: $Date: 2011-11-02 14:57:49 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!>  interferogram bilatere -- Module
!!
!! * Purpose
!!
!!   Module for two-sides interferogram functions
!!
!! * Description
!!      
!!   The objective of this module is to compute a two-sides interferogram.
!!
!! * Sub-routines and functions
!!
!! -  interf_triplet_bilatere    : two-sides interferogram computation for the three targets
!!                                 Cold Splace, BlackBody and one Earth-View  
!! -  interf_quadruplet_bilatere : two-sides interferogram computation for the four targets
!!                                 Cold Splace, BlackBody and 2 Earth-Views  
!! -  interf_bilatere            : two-sides interferogram computation
!! -  interf_bi_prepa            : preparation of the computation of two-sides interferogram
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : AC
!!

module bilatere_module
   use precision_type
   use error_type
   use zpd_type
   use interferogram_type
   use resampling_param_type
!
   implicit none
!
!
   public ::                            &
             interf_triplet_bilatere,   &
             interf_bilatere,           &
             interf_bi_prepa
!
   contains
!
!

!> interf_triplet_bilatere -- Public
!!
!! * Purpose
!!
!!     Two-sides interferogram computation for the three targets Cold Splace, BlackBody
!!     and one Earth-View.  
!!
!! * Description
!! 
!!     This subroutine applies the interf_bilatere subroutine on the three different
!!     targets : the Cold Space (CS), the BlackBody (BB), and the Earth-View.
!!
!! * Inputs
!!                  
!!     - Resampling_Param  : type_Resampling_Param / type for resampling parameters declaration 
!!                           and allocation.
!!     - SB                : spectral band
!!     - Interf_ReSamp_CS  : type_Interferogram / type for declaration and allocation of 
!!                           interferogram
!!     - Interf_ReSamp_BB  : type_Interferogram / type for declaration and allocation of 
!!                           interferogram
!!     - Interf_ReSamp_EW  : type_Interferogram / type for declaration and allocation of 
!!                           interferogram
!!
!! * Inputs/outputs
!!
!!     - Zpd_CS : type_Zpd / type for zpd parameters declaration and allocation
!!     - Zpd_BB : type_Zpd / type for zpd parameters declaration and allocation
!!     - Zpd_EW : type_Zpd / type for zpd parameters declaration and allocation
!!
!! * Outputs
!!
!!     - Interf_BiLatere_CS : type_Interferogram / type for declaration and allocation of 
!!                            interferogram
!!     - Interf_BiLatere_BB : type_Interferogram / type for declaration and allocation of 
!!                            interferogram
!!     - Interf_BiLatere_EW : type_Interferogram / type for declaration and allocation of 
!!                            interferogram
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : AC
!!

   subroutine interf_triplet_bilatere( Resampling_Param,    &
                                       Zpd_CS,              &
                                       Zpd_BB,              &
                                       Zpd_EW,              &
                                       Interf_ReSamp_CS,    &
                                       Interf_ReSamp_BB,    &
                                       Interf_ReSamp_EW,    &
                                       Interf_BiLatere_CS,  &
                                       Interf_BiLatere_BB,  &
                                       Interf_BiLatere_EW   )
   implicit none
     type(type_Resampling_Param),intent(in)            :: Resampling_Param
     type(type_Zpd)             ,intent(inout)         :: Zpd_CS
     type(type_Zpd)             ,intent(inout)         :: Zpd_BB
     type(type_Zpd)             ,intent(inout)         :: Zpd_EW
     type(type_Interferogram)   ,intent(in)            :: Interf_ReSamp_CS
     type(type_Interferogram)   ,intent(in)            :: Interf_ReSamp_BB
     type(type_Interferogram)   ,intent(in)            :: Interf_ReSamp_EW
     type(type_Interferogram)   ,intent(out)           :: Interf_BiLatere_CS
     type(type_Interferogram)   ,intent(out)           :: Interf_BiLatere_BB
     type(type_Interferogram)   ,intent(out)           :: Interf_BiLatere_EW
!
!
     call interf_bilatere( Resampling_Param,    &
                           Zpd_CS,              &
                           Interf_ReSamp_CS,    &
                           Interf_BiLatere_CS   )
!
     call interf_bilatere( Resampling_Param,    &
                           Zpd_BB,              &
                           Interf_ReSamp_BB,    &
                           Interf_BiLatere_BB   )
!
     call interf_bilatere( Resampling_Param,    &
                           Zpd_EW,              &
                           Interf_ReSamp_EW,    &
                           Interf_BiLatere_EW   )
!
     return
   end subroutine interf_triplet_bilatere
!
!

!> interf_bilatere -- Public
!!
!! * Purpose
!!
!!     Two-sides interferogram computation
!!
!! * Description
!! 
!!     This subroutine computes the two-sides interferogram thanks to the call of the 
!!     interf_bi_prepa subroutine.
!!
!! * Inputs
!!                  
!!     - Resampling_Param  : type_Resampling_Param / type for resampling parameters declaration and 
!!                           allocation.
!!     - Interf_ReSamp     : type_Interferogram / type for declaration and allocation of 
!!                           interferogram
!!
!! * Inputs/outputs
!!
!!     - Zpd    : type_Zpd / type for zpd parameters declaration and allocation
!!
!! * Outputs
!!
!!     - Interf_BiLatere : type_Interferogram / type for declaration and allocation of 
!!                         interferogram
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : AC
!!

   subroutine interf_bilatere( Resampling_Param, &
                               Zpd,              &
                               Interf_ReSamp,    &
                               Interf_BiLat      )
   implicit none
     type(type_Resampling_Param),intent(in)            :: Resampling_Param
     type(type_Zpd)             ,intent(inout)         :: Zpd
     type(type_Interferogram)   ,intent(in)            :: Interf_ReSamp
     type(type_Interferogram)   ,intent(out)           :: Interf_BiLat
     integer(kind=LONG)                                :: N_Sample
!
!    Interferogram bilatere preparation
     N_Sample = Resampling_Param%Ns_Fft              &
              + ( 2 * Resampling_Param%Ns_Margin ) + 1
     call interf_bi_prepa( N_Sample,     &
                           Zpd,          &
                           Interf_ReSamp,&
                           Interf_BiLat  )
!    
     return
   end subroutine interf_bilatere
!
!

!> interf_bi_prepa -- Public
!!
!! * Purpose
!!
!!     Preparation of the computation of two-sides interferogram. 
!!
!! * Description
!! 
!!     This subroutine allows to prepare the computation of the two-sides interferogram. The
!!     required tables are allocated, parameters are initialized and direct current is removed
!!     from the real part of the interferogram.
!!
!! * Inputs
!!                  
!!     - Ns_Fft : Fourier transform samples number
!!     - Interf : type_Interferogram / type for declaration and allocation of interferogram                    
!!                           
!! * Inputs/outputs
!!
!!     - Zpd : type_Zpd / type for zpd parameters declaration and allocation
!!
!! * Outputs
!!
!!     - Interf_bi : type_Interferogram / type for declaration and allocation of interferogram
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : AC
!!

   subroutine interf_bi_prepa( N_Sample,         &
                               Zpd,              &
                               Interf,           &
                               Interf_bi         )
   implicit none
     integer(kind=LONG)          ,intent(in)            :: N_Sample
     type(type_Zpd)              ,intent(in)            :: Zpd
     type(type_Interferogram)    ,intent(in)            :: Interf
     type(type_Interferogram)    ,intent(out)           :: Interf_bi
     integer(kind=LONG)                                 :: Ns1
     integer(kind=LONG)                                 :: Ns2
     real(kind=DOUBLE)                                  :: DC_Component
     real(kind=DOUBLE)                                  :: Time0
     real(kind=DOUBLE)                                  :: Opd0
!
     Interf_bi%N_Sample = N_Sample
     Interf_bi%Type     = Interf%Type
     Interf_bi%dOpd     = Interf%dOpd
     Interf_bi%dTime    = Interf%dTime
     Interf_bi%OpdMax   = Interf%dOpd * dble(int(N_Sample/2))
     Interf_bi%TimeMax  = Interf%dTime * dble(int(N_Sample/2))
     call alloc_Interferogram( Interf_bi )
     Ns1 = Zpd%NZpd - int(N_Sample/2)
     Ns2 = Zpd%NZpd + int(N_Sample/2)
     write(*,*) 'Bilatere Setting Interf%N_Sample  ',Interf%N_Sample
     if( (Ns1 < 1) .or. (Interf%N_Sample < Ns2) ) then
        write(*,*) 'Not Enough Samples :'
        write(*,*) 'NZpd N_Sample      :',Zpd%NZpd,N_Sample
        write(*,*) 'Ns1 Ns2 NSample    :',Ns1,Ns2,Interf%N_Sample
        write(*,*) 'interf_bi_prepa Fatal Error'
        call exit(1)
     end if
!
!    Opd and Time centering
     Opd0  = Interf%Opd(Zpd%NZpd)
     Time0 = Interf%Time(Zpd%NZpd)
!
!    Interferogram transfer
     Interf_bi%Real_Part(1:Interf_bi%N_Sample) = &
                         Interf%Real_Part(Ns1:Ns2)
     Interf_bi%Opd(1:Interf_bi%N_Sample) = &
                  Interf%Opd(Ns1:Ns2) - Opd0
     Interf_bi%Time(1:Interf_bi%N_Sample) = &
                 Interf%Time(Ns1:Ns2) - Time0
!
!    remove DC component
     DC_Component = sum( Interf_bi%Real_Part(1:Interf_bi%N_Sample) ) &
                  / dble(Interf_bi%N_Sample)
     Interf_bi%Real_Part(1:Interf_bi%N_Sample) =                    &
             Interf_bi%Real_Part(1:Interf_bi%N_Sample) - DC_Component
     return
   end subroutine interf_bi_prepa
!
!
end module bilatere_module
