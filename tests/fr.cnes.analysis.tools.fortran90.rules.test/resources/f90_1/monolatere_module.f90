!!#   monolatere_module.f90 --
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: october 2009
!!#            Version: $Revision: 1.13 $
!!#  Last modification: $Date: 2011-11-02 14:57:50 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!>  single-side interferogram -- Module
!!
!! * Purpose
!!
!!   Module for single-side interferogram functions
!!
!! * Description
!!      
!!   The objective of the single-side interferogram  module is to compute a single-side
!!   interferogram by truncation of the full (two-side) interferogram. The first sample
!!   of the single-side interferogram uses a ratio of the full interferogram close to 
!!   the central fringe. So the single-side interferogram is equal to the full interferogram
!!   between Ns_start and Nsample that is the number of sample of the full interferogram.
!!   Finally, the interferogram baseline, average valued of the single-side interferogram
!!   is extracted to the real part to give the single-side interferogram.
!!
!! * Sub-routines and functions
!!
!! -  interf_triplet_monolatere    : single-side interferogram computation for the three targets
!!                                   Cold Splace, BlackBody and one Earth-View  
!! -  interf_quadruplet_monolatere : single-side interferogram computation for the four targets
!!                                   Cold Splace, BlackBody and 2 Earth-Views  
!! -  interf_monolatere            : single-side interferogram computation
!! -  interf_mono_prepa            : preparation of the computation of single-side interferogram
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.5.7
!!


module monolatere_module
   use precision_type
   use error_type
   use zpd_type
   use interferogram_type
   use forman_type
   use resampling_param_type
   use forman_module
   use phase_mertz_module
!
   implicit none
!
!
   public :: interf_triplet_monoforman,    &
             interf_triplet_forman_decim,  &
             interf_triplet_monomertz,     &
             interf_quadruplet_monoforman, &
             interf_monoforman,            &
             interf_forman_decim,          &
             interf_monomertz,             &
             interf_mono_prepa
!
   contains
!
!

!> interf_triplet_monoforman -- Public
!!
!! * Purpose
!!
!!     Single-side interferogram computation for the three targets Cold Splace, BlackBody
!!      and one Earth-View.  
!!
!! * Description
!! 
!!     This subroutine applies the interf_monoforman subroutine on the three different
!!     targets : the Cold Space (CS), the BlackBody (BB), and the Earth-View.
!!
!! * Inputs
!!                  
!!     - Resampling_Param  : type_Resampling_Param / type for resampling parameters declaration 
!!                           and allocation.
!!     - File_Forman_Param : character / input file with definition of interferogram Forman
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
!!     - Forman : type_Forman / type for Forman parameters declaration and allocation.
!!
!! * Outputs
!!
!!     - Interf_MonoForman_CS : type_Interferogram / type for declaration and allocation of 
!!                              interferogram
!!     - Interf_MonoForman_BB : type_Interferogram / type for declaration and allocation of 
!!                              interferogram
!!     - Interf_MonoForman_EW : type_Interferogram / type for declaration and allocation of 
!!                              interferogram
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.5.7
!!

   subroutine interf_triplet_monoforman( Ns_Fft,              &
                                         Zpd_CS,              &
                                         Zpd_BB,              &
                                         Zpd_EW,              &
                                         Interf_ReSamp_CS,    &
                                         Interf_ReSamp_BB,    &
                                         Interf_ReSamp_EW,    &
                                         Forman,              &
                                         Interf_MonoForman_CS,&
                                         Interf_MonoForman_BB,&
                                         Interf_MonoForman_EW )
   implicit none
     integer(kind=LONG)         ,intent(in)            :: Ns_Fft
     type(type_Zpd)             ,intent(inout)         :: Zpd_CS
     type(type_Zpd)             ,intent(inout)         :: Zpd_BB
     type(type_Zpd)             ,intent(inout)         :: Zpd_EW
     type(type_Interferogram)   ,intent(in)            :: Interf_ReSamp_CS
     type(type_Interferogram)   ,intent(in)            :: Interf_ReSamp_BB
     type(type_Interferogram)   ,intent(in)            :: Interf_ReSamp_EW
     type(type_Forman)          ,intent(inout)         :: Forman
     type(type_Interferogram)   ,intent(out)           :: Interf_MonoForman_CS
     type(type_Interferogram)   ,intent(out)           :: Interf_MonoForman_BB
     type(type_Interferogram)   ,intent(out)           :: Interf_MonoForman_EW
     character(len=2)                                  :: Interf_Target
!
!
     Interf_Target = 'BB'
     call interf_monoforman( Ns_Fft,              &
                             Interf_Target,       &
                             Zpd_BB,              &
                             Interf_ReSamp_BB,    &
                             Forman,              &
                             Interf_MonoForman_BB )
!
     Interf_Target = 'CS'
     call interf_monoforman( Ns_Fft,              &
                             Interf_Target,       &
                             Zpd_CS,              &
                             Interf_ReSamp_CS,    &
                             Forman,              &
                             Interf_MonoForman_CS )
!
     Interf_Target = 'EW'
     call interf_monoforman( Ns_Fft,              &
                             Interf_Target,       &
                             Zpd_EW,              &
                             Interf_ReSamp_EW,    &
                             Forman,              &
                             Interf_MonoForman_EW )
!
     return
   end subroutine interf_triplet_monoforman
!
!
   subroutine interf_triplet_forman_decim( Decim_One_fir,       &
                                           Zpd_CS,              &
                                           Zpd_BB,              &
                                           Zpd_EW,              &
                                           Interf_ReSamp_CS,    &
                                           Interf_ReSamp_BB,    &
                                           Interf_ReSamp_EW,    &
                                           Forman,              &
                                           Forman_Decim,        &
                                           Interf_MonoForman_CS,&
                                           Interf_MonoForman_BB,&
                                           Interf_MonoForman_EW )
   implicit none
     type(type_Decim_One_fir)   ,intent(inout)         :: Decim_One_fir
     type(type_Zpd)             ,intent(inout)         :: Zpd_CS
     type(type_Zpd)             ,intent(inout)         :: Zpd_BB
     type(type_Zpd)             ,intent(inout)         :: Zpd_EW
     type(type_Interferogram)   ,intent(in)            :: Interf_ReSamp_CS
     type(type_Interferogram)   ,intent(in)            :: Interf_ReSamp_BB
     type(type_Interferogram)   ,intent(in)            :: Interf_ReSamp_EW
     type(type_Forman)          ,intent(inout)         :: Forman
     type(type_Forman)          ,intent(inout)         :: Forman_Decim
     type(type_Interferogram)   ,intent(out)           :: Interf_MonoForman_CS
     type(type_Interferogram)   ,intent(out)           :: Interf_MonoForman_BB
     type(type_Interferogram)   ,intent(out)           :: Interf_MonoForman_EW
     character(len=2)                                  :: Interf_Target
!
!
     Interf_Target = 'BB'
     call interf_forman_decim( Decim_One_fir,       &
                               Interf_Target,       &
                               Zpd_BB,              &
                               Interf_ReSamp_BB,    &
                               Forman,              &
                               Forman_Decim,        &
                               Interf_MonoForman_BB )
!
     Interf_Target = 'CS'
     call interf_forman_decim( Decim_One_fir,       &
                               Interf_Target,       &
                               Zpd_CS,              &
                               Interf_ReSamp_CS,    &
                               Forman,              &
                               Forman_Decim,        &
                               Interf_MonoForman_CS )
!
     Interf_Target = 'EW'
     call interf_forman_decim( Decim_One_fir,       &
                               Interf_Target,       &
                               Zpd_EW,              &
                               Interf_ReSamp_EW,    &
                               Forman,              &
                               Forman_Decim,        &
                               Interf_MonoForman_EW )
!
     return
   end subroutine interf_triplet_forman_decim
!
!
   subroutine interf_triplet_monomertz( Interf_Param,    &
                                        Resampling_Param,&
                                        SB,              &
                                        Interf_ReSamp_CS,&
                                        Interf_ReSamp_BB,&
                                        Interf_ReSamp_EW,&
                                        Phase_Mertz,     &
                                        Spectrum_CS,     &
                                        Spectrum_BB,     &
                                        Spectrum_EW      )
   implicit none
     type(type_Interf_Param)    ,intent(in)            :: Interf_Param
     type(type_Resampling_Param),intent(in)            :: Resampling_Param
     integer(kind=LONG)         ,intent(in)            :: SB
     type(type_Interferogram)   ,intent(in)            :: Interf_ReSamp_CS
     type(type_Interferogram)   ,intent(in)            :: Interf_ReSamp_BB
     type(type_Interferogram)   ,intent(in)            :: Interf_ReSamp_EW
     type(type_Phase_Mertz)     ,intent(inout)         :: Phase_Mertz
     type(type_Spectrum)        ,intent(inout)         :: Spectrum_CS
     type(type_Spectrum)        ,intent(inout)         :: Spectrum_BB
     type(type_Spectrum)        ,intent(inout)         :: Spectrum_EW
     character(len=2)                                  :: Interf_Target
!
!    Phase computation
     Interf_Target = 'BB'
     call phase_mertz_calc( Interf_Target,   &
                            Interf_ReSamp_BB,&
                            Phase_Mertz      )
!
!
     Interf_Target = 'CS'
     call interf_monomertz( SB,              &
                            Interf_Param,    &
                            Resampling_Param,&
                            Interf_ReSamp_CS,&
                            Phase_Mertz,     &
                            Spectrum_CS      )
!
!
     Interf_Target = 'BB'
     call interf_monomertz( SB,              &
                            Interf_Param,    &
                            Resampling_Param,&
                            Interf_ReSamp_BB,&
                            Phase_Mertz,     &
                            Spectrum_BB      )
!
!
     Interf_Target = 'EW'
     call interf_monomertz( SB,              &
                            Interf_Param,    &
                            Resampling_Param,&
                            Interf_ReSamp_EW,&
                            Phase_Mertz,     &
                            Spectrum_EW      )
!
     return
   end subroutine interf_triplet_monomertz
!
!

!> interf_quadruplet_monoforman -- Public
!!
!! * Purpose
!!
!!     Single-side interferogram computation for the four targets Cold Splace, BlackBody
!!      and 2 Earth-Views.  
!!
!! * Description
!! 
!!     This subroutine applies the interf_monoforman subroutine on the four different
!!     targets : the Cold Space (CS), the BlackBody (BB), the Earth-View1 (EW_1) and the
!!     Earth-View2 (EW_2).
!!
!! * Inputs
!!                  
!!     - Resampling_Param  : type_Resampling_Param / type for resampling parameters declaration and 
!!                           allocation.
!!     - File_Forman_Param : character / input file with definition of interferogram Forman
!!     - SB                : spectral band
!!     - Interf_ReSamp_CS  : type_Interferogram / type for declaration and allocation of 
!!                          interferogram
!!     - Interf_ReSamp_BB  : type_Interferogram / type for declaration and allocation of 
!!                          interferogram
!!     - Interf_ReSamp_EW1 : type_Interferogram / type for declaration and allocation of 
!!                          interferogram
!!     - Interf_ReSamp_EW2 : type_Interferogram / type for declaration and allocation of 
!!                          interferogram
!!
!! * Inputs/outputs
!!
!!     - Zpd_CS  : type_Zpd / type for zpd parameters declaration and allocation
!!     - Zpd_BB  : type_Zpd / type for zpd parameters declaration and allocation
!!     - Zpd_EW1 : type_Zpd / type for zpd parameters declaration and allocation
!!     - Zpd_EW2 : type_Zpd / type for zpd parameters declaration and allocation
!!     - Forman  : type_Forman / type for Forman parameters declaration and allocation.
!!
!! * Outputs
!!
!!     - Interf_MonoForman_CS  : type_Interferogram / type for declaration and allocation of 
!!                          interferogram
!!     - Interf_MonoForman_BB  : type_Interferogram / type for declaration and allocation of 
!!                          interferogram
!!     - Interf_MonoForman_EW1 : type_Interferogram / type for declaration and allocation of 
!!                          interferogram
!!     - Interf_MonoForman_EW2 : type_Interferogram / type for declaration and allocation of 
!!                          interferogram
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.5.7
!!

   subroutine interf_quadruplet_monoforman( Ns_Fft,               &
                                            Zpd_CS,               &
                                            Zpd_BB,               &
                                            Zpd_EW1,              &
                                            Zpd_EW2,              &
                                            Interf_ReSamp_CS,     &
                                            Interf_ReSamp_BB,     &
                                            Interf_ReSamp_EW1,    &
                                            Interf_ReSamp_EW2,    &
                                            Forman,               &
                                            Interf_MonoForman_CS ,&
                                            Interf_MonoForman_BB ,&
                                            Interf_MonoForman_EW1,&
                                            Interf_MonoForman_EW2 )
   implicit none
     integer(kind=LONG)         ,intent(in)            :: Ns_Fft
     type(type_Zpd)             ,intent(inout)         :: Zpd_CS
     type(type_Zpd)             ,intent(inout)         :: Zpd_BB
     type(type_Zpd)             ,intent(inout)         :: Zpd_EW1
     type(type_Zpd)             ,intent(inout)         :: Zpd_EW2
     type(type_Interferogram)   ,intent(in)            :: Interf_ReSamp_CS
     type(type_Interferogram)   ,intent(in)            :: Interf_ReSamp_BB
     type(type_Interferogram)   ,intent(in)            :: Interf_ReSamp_EW1
     type(type_Interferogram)   ,intent(in)            :: Interf_ReSamp_EW2
     type(type_Forman)          ,intent(inout)         :: Forman
     type(type_Interferogram)   ,intent(out)           :: Interf_MonoForman_CS
     type(type_Interferogram)   ,intent(out)           :: Interf_MonoForman_BB
     type(type_Interferogram)   ,intent(out)           :: Interf_MonoForman_EW1
     type(type_Interferogram)   ,intent(out)           :: Interf_MonoForman_EW2
     character(len=2)                                  :: Interf_Target
!
!
     Interf_Target = 'BB'
     call interf_monoforman( Ns_Fft,              &
                             Interf_Target,       &
                             Zpd_BB,              &
                             Interf_ReSamp_BB,    &
                             Forman,              &
                             Interf_MonoForman_BB )
!
     Interf_Target = 'CS'
     call interf_monoforman( Ns_Fft,              &
                             Interf_Target,       &
                             Zpd_CS,              &
                             Interf_ReSamp_CS,    &
                             Forman,              &
                             Interf_MonoForman_CS )
!
     Interf_Target = 'EW'
     call interf_monoforman( Ns_Fft,               &
                             Interf_Target,        &
                             Zpd_EW1,              &
                             Interf_ReSamp_EW1,    &
                             Forman,               &
                             Interf_MonoForman_EW1 )
!
     Interf_Target = 'EW'
     call interf_monoforman( Ns_Fft,               &
                             Interf_Target,        &
                             Zpd_EW2,              &
                             Interf_ReSamp_EW2,    &
                             Forman,               &
                             Interf_MonoForman_EW2 )
!
     return
   end subroutine interf_quadruplet_monoforman
!
!

!> interf_monoforman -- Public
!!
!! * Purpose
!!
!!     Single-side interferogram computation
!!
!! * Description
!! 
!!     This subroutine computes the single-side interferogram thanks to the following steps:
!!      - the first step consits in the initialisation of the type defining the interferogram 
!!        forman parameters;
!!      - then, the preparation of the two-side interferogram as well as the single-side 
!!        interferogram is done;
!!      - if the target is blackbody, the Forman kernel computation is done
!!      - the last step consists in the convolution of the single-side interferogram by the
!!        Forman kernel.
!!
!! * Inputs
!!                  
!!     - Resampling_Param  : type_Resampling_Param / type for resampling parameters declaration and 
!!                           allocation.
!!     - File_Forman_Param : character / input file with definition of interferogram Forman
!!     - Interf_Target     : character / definition of the target type
!!     - SB                : spectral band
!!     - Interf_ReSamp     : type_Interferogram / type for declaration and allocation of 
!!                           interferogram
!!
!! * Inputs/outputs
!!
!!     - Zpd    : type_Zpd / type for zpd parameters declaration and allocation
!!     - Forman : type_Forman / type for Forman parameters declaration and allocation.
!!
!! * Outputs
!!
!!     - Interf_MonoSym  : type_Interferogram / type for declaration and allocation of 
!!                         interferogram 
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.5.7
!!

   subroutine interf_monoforman( Ns_Fft,        &
                                 Interf_Target, &
                                 Zpd,           &
                                 Interf_ReSamp, &
                                 Forman,        &
                                 Interf_MonoSym )
   implicit none
     integer(kind=LONG)         ,intent(in)            :: Ns_Fft
     character(len=2)           ,intent(in)            :: Interf_Target
     type(type_Zpd)             ,intent(inout)         :: Zpd
     type(type_Interferogram)   ,intent(in)            :: Interf_ReSamp
     type(type_Interferogram)   ,intent(out)           :: Interf_MonoSym
     type(type_Forman)          ,intent(inout)         :: Forman
     type(type_Interferogram)                          :: Interf_Mono_local
     if( Interf_Target == 'BB' ) then
        write(*,*) 'Forman%Wn_Usefull_First',Forman%Wn_Usefull_First
        write(*,*) 'Forman%Wn_Usefull_Last ',Forman%Wn_Usefull_Last
        write(*,*) 'Forman%Type            ',Forman%Type
     end if
!
!    Interferogram monolatere preparation
     call interf_mono_prepa( Forman,           &
                             Zpd,              &
                             Interf_ReSamp,    &
                             Interf_Mono_local )
     write(*,*) 'End interf_mono_prepa'
!
     if( Interf_Target == 'BB' ) then
!
!       Forman kernel computation
        call forman_kernel( Interf_Target,    &
                            Ns_Fft,           &
                            Interf_Mono_local,&
                            Zpd,              &
                            Forman            )
        write(*,*) 'End forman_kernel'
     end if
!
!    Interferogram convolution and symetrisation
     call forman_cnv( Interf_Target,    &
                      Ns_Fft,           &
                      Interf_Mono_local,&
                      Forman,           &
                      Interf_MonoSym    )
     write(*,*) 'End forman_cnv'
     call dalloc_Interferogram( Interf_Mono_local )
!    
     return
   end subroutine interf_monoforman
!
!
   subroutine interf_forman_decim( Decim_One_fir, &
                                   Interf_Target, &
                                   Zpd,           &
                                   Interf_ReSamp, &
                                   Forman,        &
                                   Forman_Decim,  &
                                   Interf_MonoSym )
   implicit none
     type(type_Decim_One_fir)   ,intent(inout)         :: Decim_One_fir
     character(len=2)           ,intent(in)            :: Interf_Target
     type(type_Zpd)             ,intent(inout)         :: Zpd
     type(type_Interferogram)   ,intent(in)            :: Interf_ReSamp
     type(type_Forman)          ,intent(inout)         :: Forman
     type(type_Forman)          ,intent(inout)         :: Forman_Decim
     type(type_Interferogram)   ,intent(out)           :: Interf_MonoSym
     type(type_Interferogram)                          :: Interf_Mono_local
     integer(kind=LONG)                                :: Ns_Fft
!
     if( Interf_Target == 'BB' ) then
        write(*,*) 'Forman%Wn_Usefull_First',Forman%Wn_Usefull_First
        write(*,*) 'Forman%Wn_Usefull_Last ',Forman%Wn_Usefull_Last
        write(*,*) 'Forman%Type            ',Forman%Type
     end if
     Ns_Fft = Decim_One_fir%Ns_Fft
!
!    Interferogram monolatere preparation
     call interf_mono_prepa( Forman,           &
                             Zpd,              &
                             Interf_ReSamp,    &
                             Interf_Mono_local )
     write(*,*) 'End interf_mono_prepa'
!
     if( Interf_Target == 'BB' ) then
!
!       Forman kernel decimated computation
        call forman_kernel_decim( Decim_One_fir,&
                                  Forman,       &
                                  Forman_Decim  )
        write(*,*) 'End forman_kernel_Decim'
     end if
!
!    Interferogram convolution and symetrisation
     call forman_cnv_cplx( Interf_Target,    &
                           Ns_Fft,           &
                           Interf_Mono_local,&
                           Forman_Decim,     &
                           Interf_MonoSym    )
     write(*,*) 'End forman_cnv_cplx'
     call dalloc_Interferogram( Interf_Mono_local )
!    
     return
   end subroutine interf_forman_decim
!
!
   subroutine interf_monomertz( SB,              &
                                Interf_Param,    &
                                Resampling_Param,&
                                Interf_ReSamp,   &
                                Phase_Mertz,     &
                                Spectrum         )
   implicit none
     integer(kind=LONG)         ,intent(in)            :: SB
     type(type_Interf_Param)    ,intent(in)            :: Interf_Param
     type(type_Resampling_Param),intent(in)            :: Resampling_Param
     type(type_Interferogram)   ,intent(in)            :: Interf_ReSamp
     type(type_Phase_Mertz)     ,intent(in   )         :: Phase_Mertz
     type(type_Spectrum)        ,intent(inout)         :: Spectrum
!
!    Interferogram apodisation
     call spectrum_mertz( Interf_Param,    &
                          Resampling_Param,&
                          SB,              &
                          Interf_ReSamp,   &
                          Phase_Mertz,     &
                          Spectrum         )
!
!    spectrum phase correction
     call phase_mertz_correction( Phase_Mertz,  &
                                  Spectrum      )
!    
     return
   end subroutine interf_monomertz
!
!

!> interf_mono_prepa -- Public
!!
!! * Purpose
!!
!!     Preparation of the computation of single-side interferogram. 
!!
!! * Description
!! 
!!     This subroutine allows to prepare the computation of the single-side interferogram. The
!!     required tables are allocated, parameters are initialized and direct current is removed
!!     from the real part of the interferogram.
!!
!! * Inputs
!!                  
!!     - Forman : type_Forman / type for Forman parameters declaration and allocation.
!!     - Interf : type_Interferogram / type for declaration and allocation of interferogram                    
!!                           
!! * Inputs/outputs
!!
!!     - Zpd : type_Zpd / type for zpd parameters declaration and allocation
!!
!! * Outputs
!!
!!     - Interf_mono : type_Interferogram / type for declaration and allocation of interferogram
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.5.7
!!

   subroutine interf_mono_prepa( Forman,        &
                                 Zpd,           &
                                 Interf,        &
                                 Interf_mono    )
   implicit none
     type(type_Forman)       , intent(in)                 :: Forman
     type(type_Zpd)          , intent(inout)              :: Zpd
     type(type_Interferogram), intent(in)                 :: Interf
     type(type_Interferogram), intent(out)                :: Interf_mono
     integer(kind=LONG)                                   :: NsFftp
     integer(kind=LONG)                                   :: NsBi
     integer(kind=LONG)                                   :: Ns1
     real(kind=DOUBLE)                                    :: Time0
     real(kind=DOUBLE)                                    :: Opd0
     real(kind=DOUBLE)                                    :: Interf_Avg
!
     NsFftp = int(Forman%Ns_Fft/2)
     NsBi   = int(Zpd%NZpd * Forman%Ratio_bilatere)
     Ns1    = Zpd%NZpd - NsBi + 1
     write(*,*) 'Ns1,NsBi', Ns1,NsBi
     write(*,*) 'Zpd%NZpd',Zpd%NZpd
     if( Ns1 < 0 .or. Ns1 > Zpd%NZpd ) then
        write(0,*) 'Not enough samples on the left part',Ns1
        call exit(1)
     end if
     Interf_mono%Type     = Interf%Type
     if( (Interf_mono%Type /= 'R') .and. (Interf_mono%Type /= 'C') ) then
        write(*,*) 'Interf%Type Error must be R or C'
        write(*,*) 'interf_mono_prepa Fatal Error'
        call exit(1)
     end if
!
     Interf_mono%N_Sample = Interf%N_Sample - Ns1 + 1
     call alloc_Interferogram( Interf_mono )
     Interf_mono%dTime    = Interf%dTime
     Interf_mono%OpdMax   = Interf%OpdMax
     Interf_mono%dOpd     = Interf%dOpd
     Interf_mono%TimeMax  = Interf%TimeMax
!
!    Opd and Time centering
     Opd0  = Interf%Opd(Zpd%NZpd)
     Time0 = Interf%Time(Zpd%NZpd)
!
!    Interferogram transfer
     Interf_mono%Time(1:Interf_mono%N_Sample) =      &
              Interf%Time(Ns1:Interf%N_Sample) - Time0
     Interf_mono%Opd(1:Interf_mono%N_Sample)  =      &
                Interf%Opd(Ns1:Interf%N_Sample) - Opd0
     if( Interf_mono%Type == 'R' ) then
        Interf_mono%Real_Part(1:Interf_mono%N_Sample) = &
                    Interf%Real_Part(Ns1:Interf%N_Sample)
     else if( Interf_mono%Type == 'C' ) then
        Interf_mono%Complex(1:Interf_mono%N_Sample) = &
                    Interf%Complex(Ns1:Interf%N_Sample)
     end if
     Zpd%NZpd = Zpd%NZpd - Ns1 + 1
     write(*,*) 'Interf_mono%N_Sample',Interf_mono%N_Sample
     write(*,*) 'Zpd%NZpd            ',Zpd%NZpd
!
!    Interferogram centering
     if( Interf_mono%Type == 'R' ) then
        Interf_Avg = sum(Interf%Real_Part(1:Interf%N_Sample))&
                   / dble(Interf%N_Sample)
        Interf_mono%Real_Part(1:Interf_mono%N_Sample) =             &
                       Interf_mono%Real_Part(1:Interf_mono%N_Sample)&
                     - Interf_Avg
     end if
!
     return
   end subroutine interf_mono_prepa
!
!
end module monolatere_module
