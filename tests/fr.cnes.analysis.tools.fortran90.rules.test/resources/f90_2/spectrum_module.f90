!!#  spectrum_module.f90 --
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: october 2009
!!#            Version: $Revision: 1.21 $
!!#  Last modification: $Date: 2012-02-08 10:19:16 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!> spectrum computation -- Module
!!
!! * Purpose
!!
!!    Module for spectrum computation.
!!
!! * Description
!!      
!!   The module simulates spectrum as the linear combination of the blackbody background 
!!   spectrum with target spectra (cold space, hot blackbody or earth view, or laser line). 
!!   For a laser line, the spectrum is reduced at its background because the laser signal 
!!   is introduced on the continuous interferogram directly. 
!!       - The blackbody spectral radiance is given by the Planck law for the reference 
!!         temperature.
!!       - The atmospheric high resolution spectra are read in an atmospheric data base.
!!       - A sampling reduction is applied on the high resolution spectrum in order to
!!         minimize the size of the FFT in the interferogram simulator module.
!!       - A radiometric response function is applied on the low resolution real spectra.
!!       - A phase function can be added to the low resolution complex spectrum. 
!!
!! * Sub-routines and functions
!!
!! -  spectrum_sim        : simulation of a realistic and sampled spectrum on a defined 
!!                          wavenumber domain.
!! -  spectrum_hr_sub     : reading of an atmospheric high resolution spectrum file
!! -  spectrum_gauss      : spectrum convolution by a gauss function 
!! -  spectrum_boxcar     : spectrum convolution by a box function 
!! -  spectrum_usefull    : extraction of spectrum usefull part
!! -  spectrum_extract    : extraction of an input spectrum part towards an output spectrum
!! -  spectrum_transfert  : equality between two spectra
!! -  phase_sim           : spectrum phase simulation 
!! -  add_spectrum_noise  : adding noise
!! -  spectrum_noise_init : initialisation of the spectrum noise
!!
!! * References
!!

module spectrum_module
   use precision_type
   use error_type
   use constantes_type
   use detection_type
   use interf_param_type
   use spectrum_type
   use emissivity_type
   use spectral_l1c_mb_type
   use math_module
   use plancklib_module
   use spectrum_hr_module
   use convolution_module
!
   implicit none
!
!
   public :: spectrum_target,   &
             target_simulation, &
             spectrum_sim,      &
             spectrum_hr_sub,   &
             spectrum_usefull,  &
             spectrum_extract,  &
             phase_sim,         &
             add_spectrum_noise,&
             spectrum_noise_init

!
   contains
!
!
   subroutine spectrum_target( Spectral_hr,     &
                               Interf_Param,    &
                               Emissivity,      &
                               File_Spectrum_hr,&
                               Target_Spectrum, &
                               Target_Type,     &
                               SB,              &
                               Spectrum_hr      )
   implicit none
     type(type_Spectral_L1c_mb),intent(in)               :: Spectral_hr
     type(type_Interf_Param)   ,intent(in)               :: Interf_Param
     type(type_Emissivity)     ,intent(in)               :: Emissivity
     character(len=*)          ,intent(in)               :: File_Spectrum_hr
     character(len=2)          ,intent(in)               :: Target_Spectrum
     character(len=2)          ,intent(in)               :: Target_Type
     integer(kind=LONG)        ,intent(in)               :: SB
     type(type_Spectrum)       ,intent(out)              :: Spectrum_hr
     real(kind=DOUBLE)         ,dimension(:),allocatable :: Emissivity_Target
     real(kind=DOUBLE)                                   :: T_CN
     real(kind=DOUBLE)                                   :: WnS
     real(kind=DOUBLE)                                   :: WnE
     real(kind=DOUBLE)                                   :: Margin
!
!    coherence control
     if( Interf_Param%Wn_First(SB) < Spectral_hr%WnS(SB) .or. &
         Spectral_hr%WnE(SB) < Interf_Param%Wn_Last(SB) ) then
        write(*,*) 'Spectral limits error'
        write(*,*) 'spectrum_target Fatal Error'
        call exit(1)
     end if
!
!    Spectral limits
     WnS = Spectral_hr%WnS(SB)
     WnE = Spectral_hr%WnE(SB)
     Margin = ( 2_DOUBLE*Spectral_hr%SigS(SB) + 1 ) * Spectral_hr%dWn(SB)
!
     if( Target_Spectrum == "AT" ) then
!
!       Atmospheric spectrum reading
        call spectrum_hr_bin_at( WnS, WnE, Margin, &
                                 File_Spectrum_hr, &
                                 Spectrum_hr       )
!
     else if( Target_Spectrum == "CN" ) then
        if(      Target_Type == 'CS' ) then
           T_CN = Interf_Param%T_CS
        else if( Target_Type == 'BB' ) then
           T_CN = Interf_Param%T_BB
        else if( Target_Type == 'EW' ) then
           T_CN = Interf_Param%T_EW
        else
           write(*,*) 'Target_Type Error',Target_Type
           write(*,*) 'spectrum_target Fatal Error'
           call exit(1)
        end if
        write(*,*) 'Target_Type     : ',Target_Type
        write(*,*) 'Target_Spectrum : ',Target_Spectrum
        write(*,*) 'T_CN            : ',T_CN
!
!       BlackBody spectrum computation
        call spectrum_hr_bin_bb( WnS, WnE, Margin, T_CN,       &
                                 File_Spectrum_hr, Spectrum_hr )
!
     else if( Target_Spectrum == "LZ" ) then
        write(*,*) 'Target_Type   : ',Target_Type
!
!       Laser spectrum computation
        call spectrum_hr_bin_lz( WnS, WnE, Margin,                &
                                 Interf_Param%WnLaser_EW(SB),     &
                                 Interf_Param%Laser_Intensity(SB),&
                                 File_Spectrum_hr, Spectrum_hr    )
!
     else if( Target_Spectrum == "FT" ) then
        write(*,*) 'Target_Type   : ',Target_Type
!
!       Flat spectrum computation
        call spectrum_hr_bin_ft( WnS, WnE, Margin,               &
                                 Interf_Param%Flat_Intensity(SB),&
                                 File_Spectrum_hr, Spectrum_hr   )
     else
        write(*,*) 'Target_Spectrum Error   : ',Target_Spectrum
        write(*,*) 'spectrum_target Fatal Error'
        call exit(1)
     end if
!
!    Interpolate target emissivity function
     write(*,*) 'Emissivity%NsWn0 Target ',Emissivity%NsWn0
     allocate( Emissivity_Target(Spectrum_hr%N_Sample) )
     if( (Target_Spectrum == "CN") .and. (Target_Type == 'EW') ) then
        call inttabdble( Emissivity%Fct_EW,   &
                         Emissivity%Wn0,      &
                         Emissivity%NsWn0,    &
                         Emissivity_Target,   &
                         Spectrum_hr%Wn,      &
                         Spectrum_hr%N_Sample )
     else
        Emissivity_Target(1:Spectrum_hr%N_Sample) = 1_DOUBLE
     end if
     Spectrum_hr%Real_Part(1:Spectrum_hr%N_Sample) =  &
         Spectrum_hr%Real_Part(1:Spectrum_hr%N_Sample)&
       * Emissivity_Target(1:Spectrum_hr%N_Sample)
!
!    set frequency max
     Spectrum_hr%WnMax = 1_DOUBLE / Interf_Param%LambdaLaserRpd
     write(*,*) 'Spectrum_hr%WnMax ',Spectrum_hr%WnMax
!
     return
!
   end subroutine spectrum_target
!
!
   subroutine target_simulation( Interf_Param,    &
                                 Emissivity,      &
                                 Detection,       &
                                 File_Spectrum_hr,&
                                 Target_Type,     &
                                 SB,              &
                                 Spectrum_bg,     &
                                 Spectrum         )
   implicit none
     type(type_Interf_Param) ,intent(in)               :: Interf_Param
     type(type_Emissivity)   ,intent(in)               :: Emissivity
     type(type_Detection)    ,intent(in)               :: Detection
     character(len=*)        ,intent(in)               :: File_Spectrum_hr
     character(len=2)        ,intent(in)               :: Target_Type
     integer(kind=LONG)      ,intent(in)               :: SB
     type(type_Spectrum)     ,intent(out)              :: Spectrum_bg
     type(type_Spectrum)     ,intent(out)              :: Spectrum
     integer(kind=LONG)                                :: Ns
     real(kind=DOUBLE)       ,dimension(:),allocatable :: Emissivity_Mod_BG
     real(kind=DOUBLE)       ,dimension(:),allocatable :: Emissivity_Dir_BG
     real(kind=DOUBLE)       ,dimension(:),allocatable :: Emissivity_Target
     real(kind=DOUBLE)       ,dimension(:),allocatable :: Transfert_Function
     real(kind=DOUBLE)       ,dimension(:),allocatable :: Rad_Response
     real(kind=DOUBLE)       ,dimension(:),allocatable :: E_Photon
     type(type_Spectrum)                               :: Spectrum_hr
     type(type_Spectrum)                               :: Spectrum_bg_modul
     type(type_Spectrum)                               :: Spectrum_bg_direct
     real(kind=DOUBLE)                                 :: WnS
     real(kind=DOUBLE)                                 :: WnE
     real(kind=DOUBLE)                                 :: Margin
     integer(kind=LONG)                                :: NsDel
     real(kind=DOUBLE)                                 :: Geom_Pix
!
!    Spectral limits
     WnS = Interf_Param%Wn_First(SB)
     WnE = Interf_Param%Wn_Last(SB)
     Margin = 0_DOUBLE
     Spectrum_hr%TopLevelSp%Band = SB
     ! NsubFov is an output from sgm module (fixed to 1)
     Spectrum_hr%TopLevelSp%NsubFov = 1
     Spectrum_hr%TopLevelSp%SubFov = 1
     Spectrum_bg_modul%TopLevelSp%Band = SB
     ! NsubFov is an output from sgm module (fixed to 1)
     Spectrum_bg_modul%TopLevelSp%NsubFov = 1
     Spectrum_bg_modul%TopLevelSp%SubFov = 1
     Spectrum_bg_direct%TopLevelSp%Band = SB
     ! NsubFov is an output from sgm module (fixed to 1)
     Spectrum_bg_direct%TopLevelSp%NsubFov = 1
     Spectrum_bg_direct%TopLevelSp%SubFov = 1
     if( Target_Type == 'EW' ) then
!
!       Atmospheric spectrum reading
        call spectrum_hr_bin_at( WnS, WnE, Margin, &
                                 File_Spectrum_hr, &
                                 Spectrum_hr       )
     else if( Target_Type == 'BB' ) then
!
!       Hot BlackBody spectrum reading
        call spectrum_hr_bin_bb( WnS, WnE, Margin, &
                                 Interf_Param%T_BB,&
                                 File_Spectrum_hr, &
                                 Spectrum_hr       )
     else if( Target_Type == 'C1' .or. Target_Type == 'C2' ) then
!
!       Cold BlackBody spectrum reading
        call spectrum_hr_bin_bb( WnS, WnE, Margin, &
                                 Interf_Param%T_CS,&
                                 File_Spectrum_hr, &
                                 Spectrum_hr       )
                                 write(6,*) 'target simu C1 ou C2'
     else
        write(*,*) 'Wrong Target_Type',Target_Type
        call exit(1)
     end if
     if( Target_Type == 'BB' .or. Target_Type == 'C2' ) then
!
!       compute modulated background 2 spectrum BB and CS2 path
        call spectrum_hr_bin_bb( WnS, WnE, Margin,        &
                                 Interf_Param%T_BG2_modul,&
                                 File_Spectrum_hr,        &
                                 Spectrum_bg_modul        )
     else
!
!       compute modulated background 1 spectrum EW and CS1 Path
        call spectrum_hr_bin_bb( WnS, WnE, Margin,        &
                                 Interf_Param%T_BG1_modul,&
                                 File_Spectrum_hr,        &
                                 Spectrum_bg_modul        )
     end if
!
!    compute non modulated background spectrum
     call spectrum_hr_bin_bb( WnS, WnE, Margin,        &
                              Interf_Param%T_BG_direct,&
                              File_Spectrum_hr,        &
                              Spectrum_bg_direct       )
!
!    Spectrum header
     Spectrum%Type     = 'C'
     Spectrum%dWn      = Spectrum_hr%dWn
     Spectrum%Wn_First = Interf_Param%Wn_First(SB)
     Spectrum%Wn_Last  = Interf_Param%Wn_Last(SB)
     Spectrum%Ns_First = idnint( Spectrum%Wn_First/Spectrum%dWn ) + 1
     Spectrum%Ns_Last  = idnint( Spectrum%Wn_Last/Spectrum%dWn ) + 1
     Spectrum%N_Sample = Spectrum%Ns_Last - Spectrum%Ns_First + 1
     Spectrum%Wn_First = dble(Spectrum%Ns_First-1) * Spectrum%dWn
     Spectrum%Wn_Last  = dble(Spectrum%Ns_Last-1)  * Spectrum%dWn
     Spectrum%WnMax    = Spectrum_hr%WnMax
     write(*,*) 'Spectrum%Type    ',Spectrum%Type
     write(*,*) 'Spectrum%N_Sample',Spectrum%N_Sample
     write(*,*) 'Spectrum%WnMax   ',Spectrum%WnMax
     write(*,*) 'Spectrum%Ns_First',Spectrum%Ns_First
     write(*,*) 'Spectrum%Ns_Last ',Spectrum%Ns_Last
     write(*,*) 'Spectrum%Wn_First',Spectrum%Wn_First
     write(*,*) 'Spectrum%Wn_Last ',Spectrum%Wn_Last
     write(*,*) 'Spectrum%dWn     ',Spectrum%dWn
     if( Spectrum%WnMax < &
         (Interf_Param%Wn_Last(Interf_Param%Nband) * 1.2d+00) ) then
        write(*,*) 'Inconsistency between LambdaLaserRpd/Wn_Last'
        write(*,*) 'target_simulation Fatal Error'
        call exit(1)
     end if
     call alloc_Spectrum( Spectrum )
     call Spectrum_Basis( Spectrum )
     call Spectrum_Header_Transfer( Spectrum, Spectrum_bg )
!
!    Interpolate modulated background emissivity function
     write(*,*) 'Emissivity%NsWn0 BG ',Emissivity%NsWn0
     allocate( Emissivity_Mod_BG(Spectrum%N_Sample) )
     if( Target_Type == 'BB' .or.  Target_Type == 'C2' ) then
        do Ns = 1, Emissivity%NsWn0
           write(*,*) 'Emissivity Modulated BG2 ',SB,&
                       Emissivity%Wn0(Ns),          &
                       Emissivity%Mod_BG2(Ns)
        end do
        call inttabdble( Emissivity%Mod_BG2,&
                         Emissivity%Wn0,    &
                         Emissivity%NsWn0,  &
                         Emissivity_Mod_BG, &
                         Spectrum%Wn,       &
                         Spectrum%N_Sample  )
     else
        do Ns = 1, Emissivity%NsWn0
           write(*,*) 'Emissivity Modulated BG1 ',SB,&
                       Emissivity%Wn0(Ns),          &
                       Emissivity%Mod_BG1(Ns)
        end do
        call inttabdble( Emissivity%Mod_BG1,&
                         Emissivity%Wn0,    &
                         Emissivity%NsWn0,  &
                         Emissivity_Mod_BG, &
                         Spectrum%Wn,       &
                         Spectrum%N_Sample  )
     end if
!
!    Interpolate direct background emissivity function
     write(*,*) 'Emissivity%NsWn0 BG ',Emissivity%NsWn0
     allocate( Emissivity_Dir_BG(Spectrum%N_Sample) )
     do Ns = 1, Emissivity%NsWn0
        write(*,*) 'Emissivity Direct BG ',SB,&
                    Emissivity%Wn0(Ns),       &
                    Emissivity%Dir_BG(Ns)
     end do
     call inttabdble( Emissivity%Dir_BG,&
                      Emissivity%Wn0,   &
                      Emissivity%NsWn0, &
                      Emissivity_Dir_BG,&
                      Spectrum%Wn,      &
                      Spectrum%N_Sample )
!
!    Interpolate target emissivity function
     write(*,*) 'Emissivity%NsWn0 Target ',Emissivity%NsWn0
     allocate( Emissivity_Target(Spectrum%N_Sample) )
     if( Target_Type == 'C1' .or. Target_Type == 'C2' ) then
        call inttabdble( Emissivity%Fct_CS, &
                         Emissivity%Wn0,    &
                         Emissivity%NsWn0,  &
                         Emissivity_Target, &
                         Spectrum%Wn,       &
                         Spectrum%N_Sample  )
     else if( Target_Type == 'BB' ) then
        call inttabdble( Emissivity%Fct_BB, &
                         Emissivity%Wn0,    &
                         Emissivity%NsWn0,  &
                         Emissivity_Target, &
                         Spectrum%Wn,       &
                         Spectrum%N_Sample  )
     else
        Emissivity_Target(1:Spectrum%N_Sample) = 1_DOUBLE
     end if
!
!    transfert hr spectrum and add background spectrum
     NsDel = idnint( (Spectrum%Wn_First - Spectrum_hr%Wn_First)&
                     /Spectrum_hr%dWn )
     write(*,*) 'NsDel            ',NsDel
     if( NsDel < 0 ) then
        write(*,*) 'Inconsistency between spectral limits',NsDel
        write(*,*) 'Spectrum%Wn_First Spectrum_hr%Wn_First',&
                    Spectrum%Wn_First,Spectrum_hr%Wn_First
        write(*,*) 'target_simulation Fatal Error'
        call exit(1)
     end if
!
!    Modulated spectrum
     if( (Spectrum_hr%Type == 'R') .and.      &
         (Spectrum_bg_modul%Type == 'R') ) then
        Spectrum%Complex(1:Spectrum%N_Sample) =                             &
           ( Emissivity_Target(1:Spectrum%N_Sample)                         &
           * dcmplx( Spectrum_hr%Real_Part(1:Spectrum%N_Sample),0_DOUBLE ) )&
         + ( Emissivity_Mod_BG(1:Spectrum%N_Sample)                         &
           * dcmplx( Spectrum_bg_modul%Real_Part(1:Spectrum%N_Sample)       &
                   , 0_DOUBLE ) )
     else if( (Spectrum_hr%Type == 'C') .and.      &
              (Spectrum_bg_modul%Type == 'R') ) then
        Spectrum%Complex(1:Spectrum%N_Sample) =                             &
           ( Emissivity_Target(1:Spectrum%N_Sample)                         &
           * Spectrum_hr%Complex(1:Spectrum%N_Sample) )                     &
         + ( Emissivity_Mod_BG(1:Spectrum%N_Sample)                         &
           * dcmplx( Spectrum_bg_modul%Real_Part(1:Spectrum%N_Sample)       &
                   , 0_DOUBLE ) )
     else if( (Spectrum_hr%Type == 'R') .and.      &
              (Spectrum_bg_modul%Type == 'C') ) then
        Spectrum%Complex(1:Spectrum%N_Sample) =                             &
           ( Emissivity_Target(1:Spectrum%N_Sample)                         &
           * dcmplx( Spectrum_hr%Real_Part(1:Spectrum%N_Sample),0_DOUBLE ) )&
         + ( Emissivity_Mod_BG(1:Spectrum%N_Sample)                         &
           * Spectrum_bg_modul%Complex(1:Spectrum%N_Sample) )
     else if( (Spectrum_hr%Type == 'C') .and.      &
              (Spectrum_bg_modul%Type == 'C') ) then
        Spectrum%Complex(1:Spectrum%N_Sample) =                             &
           ( Emissivity_Target(1:Spectrum%N_Sample)                         &
           * Spectrum_hr%Complex(1:Spectrum%N_Sample) )                     &
         + ( Emissivity_Mod_BG(1:Spectrum%N_Sample)                         &
           * Spectrum_bg_modul%Complex(1:Spectrum%N_Sample) )
     else
        write(*,*) 'Wrong spectrum_bg_modul_%type',Spectrum_bg_modul%Type
        write(*,*) 'target_simulation Fatal Error'
        call exit(1)
     end if
!
!    Not modulated spectrum
     if( Spectrum_bg_direct%Type == 'R' ) then
        Spectrum_bg%Complex(1:Spectrum%N_Sample) =                    &
             Emissivity_Dir_BG(1:Spectrum%N_Sample)                   &
           * dcmplx( Spectrum_bg_direct%Real_Part(1:Spectrum%N_Sample)&
                   , 0_DOUBLE )
     else if( Spectrum_bg_direct%Type == 'C' ) then
        Spectrum_bg%Complex(1:Spectrum%N_Sample) =         &
             Emissivity_Dir_BG(1:Spectrum%N_Sample)        &
           * Spectrum_bg_direct%Complex(1:Spectrum%N_Sample)
     else
        write(*,*) ' Wrong Spectrum_bg_direct%Type ',Spectrum_bg_direct%Type
        write(*,*) ' Target_simulation Fatal Error'
        call exit(1)
     end if
     deallocate( Emissivity_Mod_BG )
     deallocate( Emissivity_Dir_BG )
     deallocate( Emissivity_Target )
!
!    Interferometer transfert function
     write(*,*) 'Detection%NsWn0 ',Detection%NsWn0
     allocate( Transfert_Function(Spectrum%N_Sample) )
!
!    modulated optical transfert function
     do Ns = 1, Detection%NsWn0
        write(*,*) 'Transfert Function',SB,  &
                   Detection%Wn0(Ns),        &
                   Detection%Transfert_Mod(Ns)
     end do
     call inttabdble( Detection%Transfert_Mod,&
                      Detection%Wn0,          &
                      Detection%NsWn0,        &
                      Transfert_Function,     &
                      Spectrum%Wn,            &
                      Spectrum%N_Sample       )
!
!    Add interferometer transfert function
     Geom_Pix = ( Pi * Detection%Pixel_Diam**2 / 4_DOUBLE ) &
              * ( Pi * Detection%Pupill_Diam**2 / 4_DOUBLE )&
              / ( Detection%H_Sat**2 )
!
     Spectrum%Complex(1:Spectrum%N_Sample) =                   &
                        Spectrum%Complex(1:Spectrum%N_Sample)  &
                      * Transfert_Function(1:Spectrum%N_Sample)&
                      * Geom_Pix
!
!    direct optical transfert function
     do Ns = 1, Detection%NsWn0
        write(*,*) 'Transfert Function',SB,  &
                   Detection%Wn0(Ns),        &
                   Detection%Transfert_Dir(Ns)
     end do
     call inttabdble( Detection%Transfert_Dir,&
                      Detection%Wn0,          &
                      Detection%NsWn0,        &
                      Transfert_Function,     &
                      Spectrum%Wn,            &
                      Spectrum%N_Sample       )
!
     Spectrum_bg%Complex(1:Spectrum_bg%N_Sample) =                 &
                        Spectrum_bg%Complex(1:Spectrum_bg%N_Sample)&
                      * Transfert_Function(1:Spectrum_bg%N_Sample) &
                      * Geom_Pix
!
!    Band radiometric reponse
     allocate( Rad_Response(Spectrum%N_Sample) )
     do Ns = 1, Detection%NsWn0
        write(*,*) 'Rad_Response',SB,            &
                   Detection%Wn0(Ns),            &
                   Detection%Detector_Response(Ns)
     end do
     call inttabdble( Detection%Detector_Response,&
                      Detection%Wn0,              &
                      Detection%NsWn0,            &
                      Rad_Response,               &
                      Spectrum%Wn,                &
                      Spectrum%N_Sample           )
!
!    Photon Energy
     allocate( E_Photon(Spectrum%N_Sample) )
     E_Photon(1:Spectrum%N_Sample) = Cst_h * Cst_c                  &
                                   * Spectrum%Wn(1:Spectrum%N_Sample)
!
!    add radiometric response in Ampere per second
     Spectrum%Complex(1:Spectrum%N_Sample) =                 &
                        Spectrum%Complex(1:Spectrum%N_Sample)&
                      * Rad_Response(1:Spectrum%N_Sample)    &
                      / E_Photon(1:Spectrum%N_Sample)        &
                      * Cst_chel
!
     Spectrum_bg%Complex(1:Spectrum_bg%N_Sample) =                 &
                        Spectrum_bg%Complex(1:Spectrum_bg%N_Sample)&
                      * Rad_Response(1:Spectrum_bg%N_Sample)       &
                      / E_Photon(1:Spectrum_bg%N_Sample)           &
                      * Cst_chel
!
!    add phase function
     call phase_sim( Interf_Param,&
                     Spectrum     )
     call phase_sim( Interf_Param,&
                     Spectrum_bg  )
!
!    smooth optical filters
     if( Detection%SigS /= 0 ) then
        write(*,*) 'Smoothing spectrum edges',Detection%SigS
        call windowingcplx( Spectrum_bg%N_Sample,&
                            Spectrum_bg%Complex, &
                            Detection%SigS,      &
                            Spectrum_bg%dWn      )
        call windowingcplx( Spectrum%N_Sample,&
                            Spectrum%Complex, &
                            Detection%SigS,   &
                            Spectrum%dWn      )
     end if
                          
!
     deallocate( Transfert_Function )
     deallocate( Rad_Response )
     deallocate( E_Photon )
     call dalloc_Spectrum( Spectrum_hr )
     call dalloc_Spectrum( Spectrum_bg_modul )
     call dalloc_Spectrum( Spectrum_bg_direct )
!
     return
!
   end subroutine target_simulation
!
!
!> spectrum_sim -- Public
!!
!! * Purpose
!!
!!     Simulation of a realistic and sampled spectrum on a defined wavenumber domain.
!!
!! * Description
!! 
!!     The spectrum_sim subroutine allows to simulate a realistic and sampled spectrum on a 
!!     defined wavenumber domain.
!!     This simulation requires the following steps:
!!         - fist of all, if the target is defined as an atmospheric spectrum, the high 
!!           resolution spectrum which is given in binary format is read, and a radiance 
!!           spectrum is then available. If the target is a blackbody, the spectrum is 
!!           computed taking into account the blackbody temperature (defined in interf_param, 
!!           and in function of the target Cold Space, Hot Blackbody or Earth View)
!!         - the second step consists in the computation of the background spectrum (the
!!           background temperature is defined in interf_param)
!!         - then, the sampling reduction is applied to the both spectra (target and background);
!!           the sampling reduction should be done thanks gauss or boxcar integration; it aims 
!!           to minimize the size of the FFT in the interferogram simulator module.
!!         - The spectrum header is defined before combining the target and the  background 
!!           spectra. 
!!         - The band radiometric reponse is computed thanks to spline interpolation and is 
!!           added to the spectrum.
!!         - At last, the spectrum phase is simulated thanks to a polynomial function 
!!           or a sinuso���������dal function.
!!
!! * Inputs
!!
!!     - Interf_Param     : type_Interf_Param / type for declaration and allocation of 
!!                          interferogram parameters
!!     - File_Spectrum_hr : atmospheric high resolution spectrum binary filename
!!     - Interf_Target    : type of target (atmospheric or blackbody)
!!     - Target_Type      : type of blackbody target (cold space, hot blackbody or earth view)
!!     - SB               : spectral band
!! 
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Spectrum : type_Spectrum / type for declaration and allocation of spectrum
!!
!! * References
!!
!
!
   subroutine spectrum_sim( Interf_Param,    &
                            Emissivity,      &
                            Detection,       &
                            File_Spectrum_hr,&
                            Interf_Target,   &
                            Target_Type,     &
                            SB,              &
                            Spectrum_bg,     &
                            Spectrum         )
   implicit none
     type(type_Interf_Param) ,intent(in)               :: Interf_Param
     type(type_Emissivity)   ,intent(in)               :: Emissivity
     type(type_Detection)    ,intent(in)               :: Detection
     character(len=*)        ,intent(in)               :: File_Spectrum_hr
     character(len=2)        ,intent(in)               :: Interf_Target
     character(len=2)        ,intent(in)               :: Target_Type
     integer(kind=LONG)      ,intent(in)               :: SB
     type(type_Spectrum)     ,intent(out)              :: Spectrum_bg
     type(type_Spectrum)     ,intent(out)              :: Spectrum
     integer(kind=LONG)                                :: Ns
     real(kind=DOUBLE)       ,dimension(:),allocatable :: Emissivity_Mod_BG
     real(kind=DOUBLE)       ,dimension(:),allocatable :: Emissivity_Dir_BG
     real(kind=DOUBLE)       ,dimension(:),allocatable :: Emissivity_Target
     real(kind=DOUBLE)       ,dimension(:),allocatable :: Transfert_Function
     real(kind=DOUBLE)       ,dimension(:),allocatable :: Rad_Response
     real(kind=DOUBLE)       ,dimension(:),allocatable :: E_Photon
     real(kind=DOUBLE)                                 :: T_CN
     type(type_Spectrum)                               :: Spectrum_hr
     type(type_Spectrum)                               :: Spectrum_bg_modul
     type(type_Spectrum)                               :: Spectrum_bg_direct
     real(kind=DOUBLE)                                 :: WnS
     real(kind=DOUBLE)                                 :: WnE
     real(kind=DOUBLE)                                 :: Margin
     integer(kind=LONG)                                :: Reduction_Factor
     integer(kind=LONG)                                :: R_F
     integer(kind=LONG)                                :: NsDel
     integer(kind=LONG)                                :: Ms
     real(kind=DOUBLE)       ,dimension(:),allocatable :: Boxcar_Filter
     real(kind=DOUBLE)                                 :: Geom_Pix
!
!    Spectral limits
     WnS = Interf_Param%Wn_First(SB)
     WnE = Interf_Param%Wn_Last(SB)
     Margin = 0_DOUBLE
!
     if( Interf_Target == "AT" ) then
!
!       Atmospheric spectrum reading
        call spectrum_hr_bin_at( WnS, WnE, Margin, &
                                 File_Spectrum_hr, &
                                 Spectrum_hr       )
!
     else if( Interf_Target == "CN" ) then
        if( Target_Type == 'C1' .or. Target_Type == 'C2' ) then
           T_CN = Interf_Param%T_CS
        else if( Target_Type == 'BB' ) then
           T_CN = Interf_Param%T_BB
        else if( Target_Type == 'EW' ) then
           T_CN = Interf_Param%T_EW
        else
           write(*,*) 'Target_Type Error',Target_Type
           write(*,*) 'spectrum_sim Fatal Error'
           call exit(1)
        end if
        write(*,*) 'Target_Type   : ',Target_Type
        write(*,*) 'Interf_Target : ',Interf_Target
        write(*,*) 'T_CN          : ',T_CN
!
!       BlackBody spectrum computation
        call spectrum_hr_bin_bb( WnS, WnE, Margin, T_CN,       &
                                 File_Spectrum_hr, Spectrum_hr )
!
     else if( Interf_Target == "LZ" ) then
        write(*,*) 'Target_Type   : ',Target_Type
!
!       Laser spectrum computation
        call spectrum_hr_bin_lz( WnS, WnE, Margin,                &
                                 Interf_Param%WnLaser_EW(SB),     &
                                 Interf_Param%Laser_Intensity(SB),&
                                 File_Spectrum_hr, Spectrum_hr    )
!
     else if( Interf_Target == "FT" ) then
        write(*,*) 'Target_Type   : ',Target_Type
!
!       Flat spectrum computation
        call spectrum_hr_bin_ft( WnS, WnE, Margin,               &
                                 Interf_Param%Flat_Intensity(SB),&
                                 File_Spectrum_hr, Spectrum_hr   )
     else
        write(*,*) 'Interf_Target Error   : ',Interf_Target
        write(*,*) 'spectrum_sim Fatal Error'
        call exit(1)
     end if
!
!    compute modulated background spectrum
     call spectrum_hr_bin_bb( WnS, WnE, Margin,        &
                              Interf_Param%T_BG1_modul,&
                              File_Spectrum_hr,        &
                              Spectrum_bg_modul        )
!
!    compute non modulated background spectrum
     call spectrum_hr_bin_bb( WnS, WnE, Margin,        &
                              Interf_Param%T_BG_direct,&
                              File_Spectrum_hr,        &
                              Spectrum_bg_direct       )
!
!    Sampling Reduction
     if( Spectrum_hr%dWn == 0.05d+00 ) then
        Reduction_Factor = 10
     else if( Spectrum_hr%dWn == 0.1d+00 ) then
        Reduction_Factor = 5
     else if( Spectrum_hr%dWn >= 0.5d+00 ) then
        Reduction_Factor = 1
     else
        write(*,*) 'Reduction_Factor ERROR',Spectrum_hr%dWn
        call exit(1)
     end if
     R_F = int( Reduction_Factor/2 )
!
!    Boxcar Filter
     allocate( Boxcar_Filter(-R_F:+R_F) )
     if( R_F /= 1 ) then
        if( modulo(Reduction_Factor,2) == 0 ) then
           Boxcar_Filter(-R_F)          = 0.5d+00
           Boxcar_Filter(-R_F+1:+R_F-1) = 1.0d+00
           Boxcar_Filter(+R_F)          = 0.5d+00
        else
           Boxcar_Filter(-R_F:+R_F) = 1_DOUBLE
        end if
     else
        Boxcar_Filter(0)  = 1.0d+00
     end if
     Boxcar_Filter(-R_F:+R_F) = Boxcar_Filter(-R_F:+R_F)    &
                              / sum(Boxcar_Filter(-R_F:+R_F))
!
!    Spectrum header
     Spectrum%Type     = 'C'
     Spectrum%dWn      = Spectrum_hr%dWn * dble(Reduction_Factor)
     Spectrum%Wn_First = Interf_Param%Wn_First(SB)
     Spectrum%Wn_Last  = Interf_Param%Wn_Last(SB)
     Spectrum%Ns_First = idnint( Spectrum%Wn_First/Spectrum%dWn ) + 1
     Spectrum%Ns_Last  = idnint( Spectrum%Wn_Last/Spectrum%dWn ) + 1
     Spectrum%N_Sample = Spectrum%Ns_Last - Spectrum%Ns_First + 1
     Spectrum%Wn_First = dble(Spectrum%Ns_First-1) * Spectrum%dWn
     Spectrum%Wn_Last  = dble(Spectrum%Ns_Last-1)  * Spectrum%dWn
     Spectrum%WnMax    = 1.d+00                                        &
                       / ( 2.d+00 * Interf_Param%LambdaLaserRpd/2.d+00 )
!     Spectrum%WnMax    = 1.2d+00 * Interf_Param%Wn_Last(Interf_Param%Nband)
     Ns                = idnint( Spectrum%WnMax/Spectrum%dWn ) + 1
     Spectrum%WnMax    = dble(Ns-1) * Spectrum%dWn
     write(*,*) 'Spectrum%Type    ',Spectrum%Type
     write(*,*) 'Spectrum%N_Sample',Spectrum%N_Sample
     write(*,*) 'Spectrum%WnMax   ',Spectrum%WnMax
     write(*,*) 'Spectrum%Ns_First',Spectrum%Ns_First
     write(*,*) 'Spectrum%Ns_Last ',Spectrum%Ns_Last
     write(*,*) 'Spectrum%Wn_First',Spectrum%Wn_First
     write(*,*) 'Spectrum%Wn_Last ',Spectrum%Wn_Last
     write(*,*) 'Spectrum%dWn     ',Spectrum%dWn
     if( Spectrum%WnMax < &
         (Interf_Param%Wn_Last(Interf_Param%Nband) * 1.2d+00) ) then
        write(*,*) 'Inconsistency between LambdaLaserRpd/Wn_Last'
        write(*,*) 'spectrum_sim Fatal Error'
        call exit(1)
     end if
     call alloc_Spectrum( Spectrum )
     call Spectrum_Basis( Spectrum )
     call Spectrum_Header_Transfer( Spectrum, Spectrum_bg )
!
!    Interpolate modulated background emissivity function
     write(*,*) 'Emissivity%NsWn0 BG ',Emissivity%NsWn0
     allocate( Emissivity_Mod_BG(Spectrum%N_Sample) )
     do Ns = 1, Emissivity%NsWn0
        write(*,*) 'Emissivity Modulated BG ',SB,&
                    Emissivity%Wn0(Ns),          &
                    Emissivity%Mod_BG1(Ns)
     end do
     call inttabdble( Emissivity%Mod_BG1,&
                      Emissivity%Wn0,    &
                      Emissivity%NsWn0,  &
                      Emissivity_Mod_BG, &
                      Spectrum%Wn,       &
                      Spectrum%N_Sample  )
!
!    Interpolate direct background emissivity function
     write(*,*) 'Emissivity%NsWn0 BG ',Emissivity%NsWn0
     allocate( Emissivity_Dir_BG(Spectrum%N_Sample) )
     do Ns = 1, Emissivity%NsWn0
        write(*,*) 'Emissivity Direct BG ',SB,&
                    Emissivity%Wn0(Ns),       &
                    Emissivity%Dir_BG(Ns)
     end do
     call inttabdble( Emissivity%Dir_BG,&
                      Emissivity%Wn0,   &
                      Emissivity%NsWn0, &
                      Emissivity_Dir_BG,&
                      Spectrum%Wn,      &
                      Spectrum%N_Sample )
!
!    Interpolate target emissivity function
     write(*,*) 'Emissivity%NsWn0 Target ',Emissivity%NsWn0
     allocate( Emissivity_Target(Spectrum%N_Sample) )
     if( Target_Type == 'C1' .or. Target_Type == 'C2' ) then
        call inttabdble( Emissivity%Fct_CS, &
                         Emissivity%Wn0,    &
                         Emissivity%NsWn0,  &
                         Emissivity_Target, &
                         Spectrum%Wn,       &
                         Spectrum%N_Sample  )
     else if( Target_Type == 'BB' ) then
        call inttabdble( Emissivity%Fct_BB, &
                         Emissivity%Wn0,    &
                         Emissivity%NsWn0,  &
                         Emissivity_Target, &
                         Spectrum%Wn,       &
                         Spectrum%N_Sample  )
     else
        Emissivity_Target(1:Spectrum%N_Sample) = 1_DOUBLE
     end if
!
!    transfert hr spectrum and add background spectrum
     NsDel = idnint( (Spectrum%Wn_First - Spectrum_hr%Wn_First)&
                     /Spectrum_hr%dWn )
     write(*,*) 'NsDel            ',NsDel
     if( NsDel < 0 ) then
        write(*,*) 'Inconsistency between spectral limits',NsDel
        write(*,*) 'Spectrum%Wn_First Spectrum_hr%Wn_First',&
                    Spectrum%Wn_First,Spectrum_hr%Wn_First
        write(*,*) 'spectrum_sim Fatal Error'
        call exit(1)
     end if
!
!    Modulated spectrum
     if( (Spectrum_hr%Type == 'R') .and.      &
         (Spectrum_bg_modul%Type == 'R') ) then
        Ms = NsDel+1
        do Ns = 1, Spectrum%N_Sample
           Spectrum%Complex(Ns) = Emissivity_Target(Ns) * dcmplx(         &
                                  sum(Spectrum_hr%Real_Part(Ms-R_F:Ms+R_F)&
                                     *Boxcar_Filter(-R_F:+R_F)),0_DOUBLE )&
                                + Emissivity_Mod_BG(Ns) * dcmplx(         &
                                  sum(Spectrum_bg_modul%Real_Part(Ms-R_F: &
                                                                  Ms+R_F) &
                                     *Boxcar_Filter(-R_F:+R_F)),0_DOUBLE )
           Ms = Ms + Reduction_Factor
        end do
     else if( (Spectrum_hr%Type == 'C') .and.      &
              (Spectrum_bg_modul%Type == 'R') ) then
        Ms = NsDel+1
        do Ns = 1, Spectrum%N_Sample
           Spectrum%Complex(Ns) = Emissivity_Target(Ns) *                &
                                  sum(Spectrum_hr%Complex(Ms-R_F:Ms+R_F) &
                                     *Boxcar_Filter(-R_F:+R_F))          &
                                + Emissivity_Mod_BG(Ns) * dcmplx(        &
                                  sum(Spectrum_bg_modul%Real_Part(Ms-R_F:&
                                                                  Ms+R_F)&
                                     *Boxcar_Filter(-R_F:+R_F)),0_DOUBLE )
           Ms = Ms + Reduction_Factor
        end do
     else if( (Spectrum_hr%Type == 'R') .and.      &
              (Spectrum_bg_modul%Type == 'C') ) then
        Ms = NsDel+1
        do Ns = 1, Spectrum%N_Sample
           Spectrum%Complex(Ns) = Emissivity_Target(Ns) * dcmplx(         &
                                  sum(Spectrum_hr%Real_Part(Ms-R_F:Ms+R_F)&
                                     *Boxcar_Filter(-R_F:+R_F)),0_DOUBLE )&
                                + Emissivity_Mod_BG(Ns) *                 &
                                  sum(Spectrum_bg_modul%Complex(Ms-R_F:   &
                                                                Ms+R_F)   &
                                     *Boxcar_Filter(-R_F:+R_F))
           Ms = Ms + Reduction_Factor
        end do
     else if( (Spectrum_hr%Type == 'C') .and.      &
              (Spectrum_bg_modul%Type == 'C') ) then
        Ms = NsDel+1
        do Ns = 1, Spectrum%N_Sample
           Spectrum%Complex(Ns) = Emissivity_Target(Ns) *               &
                                  sum(Spectrum_hr%Complex(Ms-R_F:Ms+R_F)&
                                     *Boxcar_Filter(-R_F:+R_F))         &
                                + Emissivity_Mod_BG(Ns) *               &
                                  sum(Spectrum_bg_modul%Complex(Ms-R_F: &
                                                                Ms+R_F) &
                                     *Boxcar_Filter(-R_F:+R_F))
           Ms = Ms + Reduction_Factor
        end do
     else
     end if
!
!    Not modulated spectrum
     if( Spectrum_bg_direct%Type == 'R' ) then
        Ms = NsDel+1
        do Ns = 1, Spectrum%N_Sample
           Spectrum_bg%Complex(Ns) = Emissivity_Dir_BG(Ns) * dcmplx(         &
                                     sum(Spectrum_bg_direct%Real_Part(Ms-R_F:&
                                                                      Ms+R_F)&
                                        *Boxcar_Filter(-R_F:+R_F)), 0_DOUBLE )
           Ms = Ms + Reduction_Factor
        end do
     else if( Spectrum_bg_direct%Type == 'C' ) then
        Ms = NsDel+1
        do Ns = 1, Spectrum%N_Sample
           Spectrum_bg%Complex(Ns) = Emissivity_Dir_BG(Ns) *               &
                                     sum(Spectrum_bg_direct%Complex(Ms-R_F:&
                                                                    Ms+R_F)&
                                        *Boxcar_Filter(-R_F:+R_F))
           Ms = Ms + Reduction_Factor
        end do
     else
        write(*,*) ' Wrong Spectrum_bg_direct%Type ',Spectrum_bg_direct%Type
        write(*,*) ' Spectrum_Sim Fatal Error'
        call exit(1)
     end if
     deallocate( Emissivity_Mod_BG )
     deallocate( Emissivity_Dir_BG )
     deallocate( Emissivity_Target )
     deallocate( Boxcar_Filter )
!
!    Interferometer transfert function
     write(*,*) 'Detection%NsWn0 ',Detection%NsWn0
     allocate( Transfert_Function(Spectrum%N_Sample) )
!
!    modulated optical transfert function
     do Ns = 1, Detection%NsWn0
        write(*,*) 'Transfert Function',SB,  &
                   Detection%Wn0(Ns),        &
                   Detection%Transfert_Mod(Ns)
     end do
     call inttabdble( Detection%Transfert_Mod,&
                      Detection%Wn0,          &
                      Detection%NsWn0,        &
                      Transfert_Function,     &
                      Spectrum%Wn,            &
                      Spectrum%N_Sample       )
!
!    Add interferometer transfert function
     Geom_Pix = ( Pi * Detection%Pixel_Diam**2 / 4_DOUBLE ) &
              * ( Pi * Detection%Pupill_Diam**2 / 4_DOUBLE )&
              / ( Detection%H_Sat**2 )
!
     Spectrum%Complex(1:Spectrum%N_Sample) =                   &
                        Spectrum%Complex(1:Spectrum%N_Sample)  &
                      * Transfert_Function(1:Spectrum%N_Sample)&
                      * Geom_Pix
!
!    direct optical transfert function
     do Ns = 1, Detection%NsWn0
        write(*,*) 'Transfert Function',SB,  &
                   Detection%Wn0(Ns),        &
                   Detection%Transfert_Dir(Ns)
     end do
     call inttabdble( Detection%Transfert_Dir,&
                      Detection%Wn0,          &
                      Detection%NsWn0,        &
                      Transfert_Function,     &
                      Spectrum%Wn,            &
                      Spectrum%N_Sample       )
!
     Spectrum_bg%Complex(1:Spectrum_bg%N_Sample) =                 &
                        Spectrum_bg%Complex(1:Spectrum_bg%N_Sample)&
                      * Transfert_Function(1:Spectrum_bg%N_Sample) &
                      * Geom_Pix
!
!    Band radiometric reponse
     allocate( Rad_Response(Spectrum%N_Sample) )
     do Ns = 1, Detection%NsWn0
        write(*,*) 'Rad_Response',SB,            &
                   Detection%Wn0(Ns),            &
                   Detection%Detector_Response(Ns)
     end do
     call inttabdble( Detection%Detector_Response,&
                      Detection%Wn0,              &
                      Detection%NsWn0,            &
                      Rad_Response,               &
                      Spectrum%Wn,                &
                      Spectrum%N_Sample           )
!
!    Photon Energy
     allocate( E_Photon(Spectrum%N_Sample) )
     E_Photon(1:Spectrum%N_Sample) = Cst_h * Cst_c                  &
                                   * Spectrum%Wn(1:Spectrum%N_Sample)
!
!    add radiometric response in Ampere per second
     Spectrum%Complex(1:Spectrum%N_Sample) =                 &
                        Spectrum%Complex(1:Spectrum%N_Sample)&
                      * Rad_Response(1:Spectrum%N_Sample)    &
                      / E_Photon(1:Spectrum%N_Sample)        &
                      * Cst_chel
!
     Spectrum_bg%Complex(1:Spectrum_bg%N_Sample) =                 &
                        Spectrum_bg%Complex(1:Spectrum_bg%N_Sample)&
                      * Rad_Response(1:Spectrum_bg%N_Sample)       &
                      / E_Photon(1:Spectrum_bg%N_Sample)           &
                      * Cst_chel
!
!    add phase function
     call phase_sim( Interf_Param,&
                     Spectrum     )
     call phase_sim( Interf_Param,&
                     Spectrum_bg  )
!
!    smooth optical filters
     if( Detection%SigS /= 0 ) then
        write(*,*) 'Smoothing spectrum edges',Detection%SigS
        call windowingcplx( Spectrum_bg%N_Sample,&
                            Spectrum_bg%Complex, &
                            Detection%SigS,      &
                            Spectrum_bg%dWn      )
        call windowingcplx( Spectrum%N_Sample,&
                            Spectrum%Complex, &
                            Detection%SigS,   &
                            Spectrum%dWn      )
     end if
                          
!
     deallocate( Transfert_Function )
     deallocate( Rad_Response )
     deallocate( E_Photon )
     call dalloc_Spectrum( Spectrum_hr )
     call dalloc_Spectrum( Spectrum_bg_modul )
     call dalloc_Spectrum( Spectrum_bg_direct )
!
     return
!
   end subroutine spectrum_sim
!
!

!> spectrum_hr_sub -- Public
!!
!! * Purpose
!!
!!     Reading of an atmospheric high resolution spectrum file
!!
!! * Description
!! 
!!     This subroutine allows to read an atmospheric high resolution spectrum which is given in 
!!     binary format. Each spectral band limits are fixed by the interferogram parameters file.
!!
!! * Inputs
!!
!!     - Interf_Param     : type_Interf_Param / type for declaration and allocation of 
!!                          interferogram parameters
!!     - File_Spectrum_hr : atmospheric high resolution spectrum binary filename
!!     - SB               : spectral band
!! 
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Spectrum_hr : type_Spectrum / type for declaration and allocation of spectrum
!!
!! * References
!!

   subroutine spectrum_hr_sub( Interf_Param,    &
                               File_Spectrum_hr,&
                               SB,              &
                               Spectrum_hr      )
   implicit none
     type(type_Interf_Param) , intent(in)                :: Interf_Param
     character(len=*)        , intent(in)                :: File_Spectrum_hr
     integer(kind=LONG)      , intent(in)                :: SB
     type(type_Spectrum)     , intent(out)               :: Spectrum_hr
     real(kind=DOUBLE)                                   :: WnS
     real(kind=DOUBLE)                                   :: WnE
     real(kind=DOUBLE)                                   :: Margin
!
     WnS = Interf_Param%Wn_First(SB)
     WnE = Interf_Param%Wn_Last(SB)
     Margin = 200_DOUBLE
     call spectrum_hr_bin_at( WnS, WnE, Margin, &
                              File_Spectrum_hr, &
                              Spectrum_hr       )
!
     return
   end subroutine spectrum_hr_sub
!
!


!> spectrum_usefull -- Public
!!
!! * Purpose
!!
!!     Extraction of spectrum usefull part
!!
!! * Description
!! 
!!     The spectrum_usefull subroutine extracts the usefull part of the spectrum: the usefull 
!!     part is defined by a first and a last wavenumber.
!!
!! * Inputs
!!
!!     - WnUS :  first usefull wavenumber
!!     - WnUE :  last usefull wavenumber
!! 
!! * Inputs/outputs
!!
!!     - Spectrum : type_Spectrum / type for declaration and allocation of spectrum
!!
!! * Outputs
!!
!! * References
!!

   subroutine spectrum_usefull( WnUS, WnUE, Spectrum )
   implicit none
     real(kind=DOUBLE)       , intent(in)                :: WnUS
     real(kind=DOUBLE)       , intent(in)                :: WnUE
     type(type_Spectrum)     , intent(inout)             :: Spectrum
     integer(kind=LONG)                                  :: NsDel
     integer(kind=LONG)                                  :: NsMax
!
     write(*,*) 'Spectrum%Wn_First',Spectrum%Wn_First
     write(*,*) 'Spectrum%Wn_Last ',Spectrum%Wn_Last
     NsDel = Spectrum%Ns_First
     NsMax = NsDel
     Spectrum%Ns_First = idnint( WnUS / Spectrum%dWn ) + 1
     Spectrum%Ns_Last  = idnint( WnUE / Spectrum%dWn ) + 1
     Spectrum%Wn_First = dble( Spectrum%Ns_First-1) * Spectrum%dWn
     Spectrum%Wn_Last  = dble( Spectrum%Ns_Last-1 ) * Spectrum%dWn
     Spectrum%N_Sample = Spectrum%Ns_Last - Spectrum%Ns_First + 1
     write(*,*) 'Spectrum%Wn_First',Spectrum%Wn_First
     write(*,*) 'Spectrum%Wn_Last ',Spectrum%Wn_Last
     NsDel = Spectrum%Ns_First - NsDel
     if( (NsDel < 0) .or. (NsDel >= NsMax) ) then
        write(*,*) 'Spectral Udefull limits Error',NsDel,NsMax
        write(*,*) 'spectrum_usefull Fatal Error'
        call exit(1)
     end if
     Spectrum%Wn(1:Spectrum%N_Sample) = &
                   Spectrum%Wn(NsDel+1:NsDel+Spectrum%N_Sample)
     if( Spectrum%Type == 'C' ) then
        Spectrum%Complex(1:Spectrum%N_Sample) = &
                   Spectrum%Complex(NsDel+1:NsDel+Spectrum%N_Sample)
     else if( Spectrum%Type == 'RI' ) then
        Spectrum%Real_Part(1:Spectrum%N_Sample) = &
                   Spectrum%Real_Part(NsDel+1:NsDel+Spectrum%N_Sample)
        Spectrum%Imag_Part(1:Spectrum%N_Sample) = &
                   Spectrum%Imag_Part(NsDel+1:NsDel+Spectrum%N_Sample)
     else if( Spectrum%Type == 'MA' ) then
        Spectrum%Modulus(1:Spectrum%N_Sample) = &
                   Spectrum%Modulus(NsDel+1:NsDel+Spectrum%N_Sample)
        Spectrum%Argument(1:Spectrum%N_Sample) = &
                   Spectrum%Argument(NsDel+1:NsDel+Spectrum%N_Sample)
     else if( Spectrum%Type == 'R'  ) then
        Spectrum%Real_Part(1:Spectrum%N_Sample) = &
                   Spectrum%Real_Part(NsDel+1:NsDel+Spectrum%N_Sample)
     else if( Spectrum%Type == 'I'  ) then
        Spectrum%Imag_Part(1:Spectrum%N_Sample) = &
                   Spectrum%Imag_Part(NsDel+1:NsDel+Spectrum%N_Sample)
     else if( Spectrum%Type == 'M'  ) then
        Spectrum%Modulus(1:Spectrum%N_Sample) = &
                   Spectrum%Modulus(NsDel+1:NsDel+Spectrum%N_Sample)
     else if( Spectrum%Type == 'A'  ) then
        Spectrum%Argument(1:Spectrum%N_Sample) = &
                   Spectrum%Argument(NsDel+1:NsDel+Spectrum%N_Sample)
     else
        write(*,*) 'Spectrum%Type Error'
        write(*,*) 'spectrum_usefull Fatal Error'
        call exit(1)
     end if
     return
   end subroutine spectrum_usefull
!
!

!> spectrum_extract -- Public
!!
!! * Purpose
!!
!!     Extraction of an input spectrum part towards an output spectrum
!!
!! * Description
!! 
!!     The spectrum_extract subroutine extracts a part of an input spectrum towards an output 
!!     spectrum: the both spectra are defined by their first and a last wavenumbers, and the 
!!     subroutine checks the value of these wavenumber to accuratly extract the output spectrum.
!!
!! * Inputs
!!
!!     - WnS :  first usefull wavenumber
!!     - WnE :  last usefull wavenumber
!!     - Spectrum_In : type_Spectrum / type for declaration and allocation of spectrum
!! 
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Spectrum_Out : type_Spectrum / type for declaration and allocation of spectrum
!!
!! * References
!!

   subroutine spectrum_extract( WnS, WnE, Spectrum_In, Spectrum_Out )
   implicit none
     real(kind=DOUBLE)       , intent(in)                :: WnS
     real(kind=DOUBLE)       , intent(in)                :: WnE
     type(type_Spectrum)     , intent(in)                :: Spectrum_In
     type(type_Spectrum)     , intent(out)               :: Spectrum_Out
     integer(kind=LONG)                                  :: NsDel
     integer(kind=LONG)                                  :: NsFil
!
     write(*,*) 'Spectrum_In%Wn_First',Spectrum_In%Wn_First
     write(*,*) 'Spectrum_In%Wn_Last ',Spectrum_In%Wn_Last
     Spectrum_Out%Type     = Spectrum_In%Type
     Spectrum_Out%dWn      = Spectrum_In%dWn
     Spectrum_Out%Ns_First = idnint( WnS / Spectrum_Out%dWn ) + 1
     Spectrum_Out%Ns_Last  = idnint( WnE / Spectrum_Out%dWn ) + 1
     Spectrum_Out%Wn_First = dble( Spectrum_Out%Ns_First-1) * Spectrum_Out%dWn
     Spectrum_Out%Wn_Last  = dble( Spectrum_Out%Ns_Last-1 ) * Spectrum_Out%dWn
     Spectrum_Out%N_Sample = Spectrum_Out%Ns_Last - Spectrum_Out%Ns_First + 1
     write(*,*) 'Spectrum_Out%Type    ',Spectrum_Out%Type
     write(*,*) 'Spectrum_Out%Wn_First',Spectrum_Out%Wn_First
     write(*,*) 'Spectrum_Out%Wn_Last ',Spectrum_Out%Wn_Last
     write(*,*) 'Spectrum_Out%N_Sample',Spectrum_Out%N_Sample
     call alloc_Spectrum( Spectrum_Out )
     call Spectrum_Basis( Spectrum_Out )
     if( (Spectrum_In%Ns_First <= Spectrum_Out%Ns_First) .and. &
         (Spectrum_Out%Ns_Last <= Spectrum_In%Ns_Last) ) then
        NsDel = Spectrum_Out%Ns_First - Spectrum_In%Ns_First
        write(*,*) 'NsDel',NsDel
        if( Spectrum_Out%Type == 'C' ) then
           Spectrum_Out%Complex(1:Spectrum_Out%N_Sample) =   &
                   Spectrum_In%Complex(NsDel+1:NsDel+Spectrum_Out%N_Sample)
        else if( Spectrum_Out%Type == 'RI' ) then
           Spectrum_Out%Real_Part(1:Spectrum_Out%N_Sample) = &
                   Spectrum_In%Real_Part(NsDel+1:NsDel+Spectrum_Out%N_Sample)
           Spectrum_Out%Imag_Part(1:Spectrum_Out%N_Sample) = &
                   Spectrum_In%Imag_Part(NsDel+1:NsDel+Spectrum_Out%N_Sample)
        else if( Spectrum_Out%Type == 'MA' ) then
           Spectrum_Out%Modulus(1:Spectrum_Out%N_Sample) =   &
                   Spectrum_In%Modulus(NsDel+1:NsDel+Spectrum_Out%N_Sample)
           Spectrum_Out%Argument(1:Spectrum_Out%N_Sample) =  &
                   Spectrum_In%Argument(NsDel+1:NsDel+Spectrum_Out%N_Sample)
        else if( Spectrum_Out%Type == 'R'  ) then
           Spectrum_Out%Real_Part(1:Spectrum_Out%N_Sample) = &
                   Spectrum_In%Real_Part(NsDel+1:NsDel+Spectrum_Out%N_Sample)
        else if( Spectrum_Out%Type == 'I'  ) then
           Spectrum_Out%Imag_Part(1:Spectrum_Out%N_Sample) = &
                   Spectrum_In%Imag_Part(NsDel+1:NsDel+Spectrum_Out%N_Sample)
        else if( Spectrum_Out%Type == 'M'  ) then
           Spectrum_Out%Modulus(1:Spectrum_Out%N_Sample) =   &
                   Spectrum_In%Modulus(NsDel+1:NsDel+Spectrum_Out%N_Sample)
        else if( Spectrum_Out%Type == 'A'  ) then
           Spectrum_Out%Argument(1:Spectrum_Out%N_Sample) =  &
                   Spectrum_In%Argument(NsDel+1:NsDel+Spectrum_Out%N_Sample)
        else
           write(*,*) 'Spectrum_Out%Type Error'
           write(*,*) 'spectrum_extract Fatal Error'
           call exit(1)
        end if
     else if( (Spectrum_Out%Ns_First < Spectrum_In%Ns_First) .and. &
              (Spectrum_Out%Ns_Last <= Spectrum_In%Ns_Last) ) then
        NsDel = Spectrum_In%Ns_First - Spectrum_Out%Ns_First
        write(*,*) 'Filling begin',NsDel
        if( Spectrum_Out%Type == 'C' ) then
           Spectrum_Out%Complex(1:NsDel) =   &
                   Spectrum_In%Complex(1)
           Spectrum_Out%Complex(NsDel+1:Spectrum_Out%N_Sample) =    &
                   Spectrum_In%Complex(1:Spectrum_Out%N_Sample-NsDel)
        else if( Spectrum_Out%Type == 'RI' ) then
           Spectrum_Out%Real_Part(1:NsDel) =   &
                   Spectrum_In%Real_Part(1)
           Spectrum_Out%Real_Part(NsDel+1:Spectrum_Out%N_Sample) =    &
                   Spectrum_In%Real_Part(1:Spectrum_Out%N_Sample-NsDel)
           Spectrum_Out%Imag_Part(1:NsDel) =   &
                   Spectrum_In%Imag_Part(1)
           Spectrum_Out%Imag_Part(NsDel+1:Spectrum_Out%N_Sample) =    &
                   Spectrum_In%Imag_Part(1:Spectrum_Out%N_Sample-NsDel)
        else if( Spectrum_Out%Type == 'MA' ) then
           Spectrum_Out%Modulus(1:NsDel) =   &
                   Spectrum_In%Modulus(1)
           Spectrum_Out%Modulus(NsDel+1:Spectrum_Out%N_Sample) =    &
                   Spectrum_In%Modulus(1:Spectrum_Out%N_Sample-NsDel)
           Spectrum_Out%Argument(1:NsDel) =   &
                   Spectrum_In%Argument(1)
           Spectrum_Out%Argument(NsDel+1:Spectrum_Out%N_Sample) =    &
                   Spectrum_In%Argument(1:Spectrum_Out%N_Sample-NsDel)
        else if( Spectrum_Out%Type == 'R'  ) then
           Spectrum_Out%Real_Part(1:NsDel) =   &
                   Spectrum_In%Real_Part(1)
           Spectrum_Out%Real_Part(NsDel+1:Spectrum_Out%N_Sample) =    &
                   Spectrum_In%Real_Part(1:Spectrum_Out%N_Sample-NsDel)
        else if( Spectrum_Out%Type == 'I'  ) then
           Spectrum_Out%Imag_Part(1:NsDel) =   &
                   Spectrum_In%Imag_Part(1)
           Spectrum_Out%Imag_Part(NsDel+1:Spectrum_Out%N_Sample) =    &
                   Spectrum_In%Imag_Part(1:Spectrum_Out%N_Sample-NsDel)
        else if( Spectrum_Out%Type == 'M'  ) then
           Spectrum_Out%Modulus(1:NsDel) =   &
                   Spectrum_In%Modulus(1)
           Spectrum_Out%Modulus(NsDel+1:Spectrum_Out%N_Sample) =    &
                   Spectrum_In%Modulus(1:Spectrum_Out%N_Sample-NsDel)
        else if( Spectrum_Out%Type == 'A'  ) then
           Spectrum_Out%Argument(1:NsDel) =   &
                   Spectrum_In%Argument(1)
           Spectrum_Out%Argument(NsDel+1:Spectrum_Out%N_Sample) =    &
                   Spectrum_In%Argument(1:Spectrum_Out%N_Sample-NsDel)
        else
           write(*,*) 'Spectrum_Out%Type Error'
           write(*,*) 'spectrum_extract Fatal Error'
           call exit(1)
        end if
     else if( (Spectrum_In%Ns_First <= Spectrum_Out%Ns_First) .and. &
              (Spectrum_In%Ns_Last < Spectrum_Out%Ns_Last) ) then
        NsFil = Spectrum_Out%Ns_Last - Spectrum_In%Ns_Last
        NsDel = Spectrum_Out%Ns_First - Spectrum_In%Ns_First
        write(*,*) 'Filling End',NsDel,NsFil
        if( Spectrum_Out%Type == 'C' ) then
           Spectrum_Out%Complex(1:Spectrum_Out%N_Sample-NsFil) =   &
                   Spectrum_In%Complex(NsDel+1:Spectrum_In%N_Sample)
           Spectrum_Out%Complex(Spectrum_Out%N_Sample-NsFil+1: &
                                Spectrum_Out%N_Sample) =       &
                   Spectrum_In%Complex(Spectrum_In%N_Sample)
        else if( Spectrum_Out%Type == 'RI' ) then
           Spectrum_Out%Real_Part(1:Spectrum_Out%N_Sample-NsFil) =   &
                   Spectrum_In%Real_Part(NsDel+1:Spectrum_In%N_Sample)
           Spectrum_Out%Real_Part(Spectrum_Out%N_Sample-NsFil+1: &
                                Spectrum_Out%N_Sample) =         &
                   Spectrum_In%Real_Part(Spectrum_In%N_Sample)
           Spectrum_Out%Imag_Part(1:Spectrum_Out%N_Sample-NsFil) =   &
                   Spectrum_In%Imag_Part(NsDel+1:Spectrum_In%N_Sample)
           Spectrum_Out%Imag_Part(Spectrum_Out%N_Sample-NsFil+1: &
                                Spectrum_Out%N_Sample) =         &
                   Spectrum_In%Imag_Part(Spectrum_In%N_Sample)
        else if( Spectrum_Out%Type == 'MA' ) then
           Spectrum_Out%Modulus(1:Spectrum_Out%N_Sample-NsFil) =   &
                   Spectrum_In%Modulus(NsDel+1:Spectrum_In%N_Sample)
           Spectrum_Out%Modulus(Spectrum_Out%N_Sample-NsFil+1: &
                                Spectrum_Out%N_Sample) =       &
                   Spectrum_In%Modulus(Spectrum_In%N_Sample)
           Spectrum_Out%Argument(1:Spectrum_Out%N_Sample-NsFil) =   &
                   Spectrum_In%Argument(NsDel+1:Spectrum_In%N_Sample)
           Spectrum_Out%Argument(Spectrum_Out%N_Sample-NsFil+1: &
                                Spectrum_Out%N_Sample) =        &
                   Spectrum_In%Argument(Spectrum_In%N_Sample)
        else if( Spectrum_Out%Type == 'R'  ) then
           Spectrum_Out%Real_Part(1:Spectrum_Out%N_Sample-NsFil) =   &
                   Spectrum_In%Real_Part(NsDel+1:Spectrum_In%N_Sample)
           Spectrum_Out%Real_Part(Spectrum_Out%N_Sample-NsFil+1: &
                                Spectrum_Out%N_Sample) =         &
                   Spectrum_In%Real_Part(Spectrum_In%N_Sample)
        else if( Spectrum_Out%Type == 'I'  ) then
           Spectrum_Out%Imag_Part(1:Spectrum_Out%N_Sample-NsFil) =   &
                   Spectrum_In%Imag_Part(NsDel+1:Spectrum_In%N_Sample)
           Spectrum_Out%Imag_Part(Spectrum_Out%N_Sample-NsFil+1: &
                                Spectrum_Out%N_Sample) =         &
                   Spectrum_In%Imag_Part(Spectrum_In%N_Sample)
        else if( Spectrum_Out%Type == 'M'  ) then
           Spectrum_Out%Modulus(1:Spectrum_Out%N_Sample-NsFil) =   &
                   Spectrum_In%Modulus(NsDel+1:Spectrum_In%N_Sample)
           Spectrum_Out%Modulus(Spectrum_Out%N_Sample-NsFil+1: &
                                Spectrum_Out%N_Sample) =       &
                   Spectrum_In%Modulus(Spectrum_In%N_Sample)
        else if( Spectrum_Out%Type == 'A'  ) then
           Spectrum_Out%Argument(1:Spectrum_Out%N_Sample-NsFil) =   &
                   Spectrum_In%Argument(NsDel+1:Spectrum_In%N_Sample)
           Spectrum_Out%Argument(Spectrum_Out%N_Sample-NsFil+1: &
                                Spectrum_Out%N_Sample) =        &
                   Spectrum_In%Argument(Spectrum_In%N_Sample)
        else
           write(*,*) 'Spectrum_Out%Type Error'
           write(*,*) 'spectrum_extract Fatal Error'
           call exit(1)
        end if
     else if( (Spectrum_Out%Ns_First < Spectrum_In%Ns_First) .and. &
              (Spectrum_In%Ns_Last   < Spectrum_Out%Ns_Last) ) then
        NsDel = Spectrum_In%Ns_First - Spectrum_Out%Ns_First
        NsFil = Spectrum_Out%Ns_Last - Spectrum_In%Ns_Last
        write(*,*) 'Filling Begin and End',NsDel,NsFil
        if( Spectrum_Out%Type == 'C' ) then
           Spectrum_Out%Complex(1:NsDel) = Spectrum_In%Complex(1)
           Spectrum_Out%Complex(NsDel+1:NsDel+Spectrum_In%N_Sample) =   &
                   Spectrum_In%Complex(1:Spectrum_In%N_Sample)
           Spectrum_Out%Complex(Spectrum_Out%N_Sample-NsFil+1:          &
                                Spectrum_Out%N_Sample) =                &
                   Spectrum_In%Complex(Spectrum_In%N_Sample)
        else if( Spectrum_Out%Type == 'RI' ) then
           Spectrum_Out%Real_Part(1:NsDel) =                            &
                   Spectrum_In%Real_Part(1)
           Spectrum_Out%Real_Part(NsDel+1:NsDel+Spectrum_In%N_Sample) = &
                   Spectrum_In%Real_Part(1:Spectrum_In%N_Sample)
           Spectrum_Out%Real_Part(Spectrum_Out%N_Sample-NsFil+1:        &
                                Spectrum_Out%N_Sample) =                &
                   Spectrum_In%Real_Part(Spectrum_In%N_Sample)
           Spectrum_Out%Imag_Part(1:NsDel) = Spectrum_In%Imag_Part(1)
           Spectrum_Out%Imag_Part(NsDel+1:NsDel+Spectrum_In%N_Sample) = &
                   Spectrum_In%Imag_Part(1:Spectrum_In%N_Sample)
           Spectrum_Out%Imag_Part(Spectrum_Out%N_Sample-NsFil+1:        &
                                Spectrum_Out%N_Sample) =                &
                   Spectrum_In%Imag_Part(Spectrum_In%N_Sample)
        else if( Spectrum_Out%Type == 'MA' ) then
           Spectrum_Out%Modulus(1:NsDel) = Spectrum_In%Modulus(1)
           Spectrum_Out%Modulus(NsDel+1:NsDel+Spectrum_In%N_Sample) =   &
                   Spectrum_In%Modulus(1:Spectrum_In%N_Sample)
           Spectrum_Out%Modulus(Spectrum_Out%N_Sample-NsFil+1:          &
                                Spectrum_Out%N_Sample) =                &
                   Spectrum_In%Modulus(Spectrum_In%N_Sample)
           Spectrum_Out%Argument(1:NsDel) = Spectrum_In%Argument(1)
           Spectrum_Out%Argument(NsDel+1:NsDel+Spectrum_In%N_Sample) =  &
                   Spectrum_In%Argument(1:Spectrum_In%N_Sample)
           Spectrum_Out%Argument(Spectrum_Out%N_Sample-NsFil+1:         &
                                Spectrum_Out%N_Sample) =                &
                   Spectrum_In%Argument(Spectrum_In%N_Sample)
        else if( Spectrum_Out%Type == 'R'  ) then
           Spectrum_Out%Real_Part(1:NsDel) =                            &
                   Spectrum_In%Real_Part(1)
           Spectrum_Out%Real_Part(NsDel+1:NsDel+Spectrum_In%N_Sample) = &
                   Spectrum_In%Real_Part(1:Spectrum_In%N_Sample)
           Spectrum_Out%Real_Part(Spectrum_Out%N_Sample-NsFil+1:        &
                                Spectrum_Out%N_Sample) =                &
                   Spectrum_In%Real_Part(Spectrum_In%N_Sample)
        else if( Spectrum_Out%Type == 'I'  ) then
           Spectrum_Out%Imag_Part(1:NsDel) = Spectrum_In%Imag_Part(1)
           Spectrum_Out%Imag_Part(NsDel+1:NsDel+Spectrum_In%N_Sample) = &
                   Spectrum_In%Imag_Part(1:Spectrum_In%N_Sample)
           Spectrum_Out%Imag_Part(Spectrum_Out%N_Sample-NsFil+1:        &
                                Spectrum_Out%N_Sample) =                &
                   Spectrum_In%Imag_Part(Spectrum_In%N_Sample)
        else if( Spectrum_Out%Type == 'M'  ) then
           Spectrum_Out%Modulus(1:NsDel) = Spectrum_In%Modulus(1)
           Spectrum_Out%Modulus(NsDel+1:NsDel+Spectrum_In%N_Sample) =   &
                   Spectrum_In%Modulus(1:Spectrum_In%N_Sample)
           Spectrum_Out%Modulus(Spectrum_Out%N_Sample-NsFil+1:          &
                                Spectrum_Out%N_Sample) =                &
                   Spectrum_In%Modulus(Spectrum_In%N_Sample)
        else if( Spectrum_Out%Type == 'A'  ) then
           Spectrum_Out%Argument(1:NsDel) = Spectrum_In%Argument(1)
           Spectrum_Out%Argument(NsDel+1:NsDel+Spectrum_In%N_Sample) =  &
                   Spectrum_In%Argument(1:Spectrum_In%N_Sample)
           Spectrum_Out%Argument(Spectrum_Out%N_Sample-NsFil+1:         &
                                Spectrum_Out%N_Sample) =                &
                   Spectrum_In%Argument(Spectrum_In%N_Sample)
        else
           write(*,*) 'Spectrum_Out%Type Error'
           write(*,*) 'spectrum_extract Fatal Error'
           call exit(1)
        end if
     else
        write(*,*) 'Spectrum_Out Limits Error'
        write(*,*) 'spectrum_extract Fatal Error'
        call exit(1)
     end if
     return
   end subroutine spectrum_extract
!
!

!> phase_sim -- Public
!!
!! * Purpose
!!
!!     Spectrum phase simulation 
!!
!! * Description
!! 
!!     The phase_sim subroutine allows the simulation of the spectrum complex part thanks to a 
!!     polynomial function or a sinuso�dal function. It depends on the wave number.
!!
!! * Inputs
!!
!!     - Interf_Param     : type_Interf_Param / type for declaration and allocation of 
!!                          interferogram parameters
!! 
!! * Inputs/outputs
!!
!!     - Spectrum : type_Spectrum / type for declaration and allocation of spectrum
!!
!! * Outputs
!!
!! * References
!!

   subroutine phase_sim( Interf_Param, &
                         Spectrum      )
   implicit none
     type(type_Interf_Param) , intent(in)                :: Interf_Param
     type(type_Spectrum)     , intent(inout)             :: Spectrum
     integer(kind=LONG)                                  :: Ns
     real(kind=DOUBLE)    , dimension(:), allocatable    :: Phase
!
     allocate( Phase(1) )
     if( Interf_Param%Phase_Poly_Deg > 1 ) then
!       Polynomial Phase
        write(*,*) ' Spectrum Phase : Polynomial'
        do Ns = 1, Spectrum%N_Sample
           call polyappli( 1,                            &
                           Spectrum%Wn(Ns),              &
                           Interf_Param%Phase_Poly_Coeff,&
                           Interf_Param%Phase_Poly_Deg,  &
                           Phase(1)                      )
           Spectrum%Complex(Ns) = Spectrum%Complex(Ns)   &
                                * dcmplx( dcos(Phase(1)) &
                                         ,dsin(Phase(1)) )
        end do
     else if( Interf_Param%Phase_Poly_Coeff(0) /= 0 ) then
!       Sine Phase
        write(*,*) ' Spectrum Phase : Sinusoidal'
        do Ns = 1, Spectrum%N_Sample
           Phase(1) = Interf_Param%Phase_Poly_Coeff(0)       &
                    * dsin( twoPi * Spectrum%Wn(Ns)          &
                          / Interf_Param%Phase_Poly_Coeff(1) )
           Spectrum%Complex(Ns) = Spectrum%Complex(Ns)   &
                                * dcmplx( dcos(Phase(1)) &
                                         ,dsin(Phase(1)) )
        end do
     else
        write(*,*) ' Spectrum Phase : None'
     end if
     deallocate( Phase )
!
     return
!
   end subroutine phase_sim
!
!

!> add_spectrum_noise -- Public
!!
!! * Purpose
!!
!!     Adding noise
!!
!! * Description
!! 
!!     The add_spectrum_noise subroutine allows to add noise in function of the spectrum type
!!
!! * Inputs
!!
!!     - Spectrum_Noise : type_Spectrum / type for declaration and allocation of spectrum
!!     - OSFactor       : noise amplification factor
!! 
!! * Inputs/outputs
!!
!!     - Spectrum : type_Spectrum / type for declaration and allocation of spectrum
!!
!! * Outputs
!!
!! * References
!!

   subroutine add_spectrum_noise( Spectrum_Noise, OSFactor, Spectrum )
   implicit none
     type(type_Spectrum)     , intent(in)                :: Spectrum_Noise
     integer(kind=LONG)      , intent(in)                :: OSFactor
     type(type_Spectrum)     , intent(inout)             :: Spectrum
     integer(kind=LONG)                                  :: n
     integer(kind=LONG)                                  :: ks
     integer(kind=LONG)                                  :: kk
     real(kind=DOUBLE)                                   :: br
     real(kind=DOUBLE)                                   :: rmsbr
!
     if( Spectrum%Type == 'R' ) then
        rmsbr = 0.d+00
        do n = 1, Spectrum%N_Sample
           ks = idnint(mod(abs(Spectrum%Real_Part(n))*1.d+08,128.d+00))
           do kk = 1, ks
              br = prnd()
           end do
           br = dsqrt(dble(OSFactor))*Spectrum_Noise%Real_Part(n)*grnd(0)
           Spectrum%Real_Part(n) = Spectrum%Real_Part(n) + br
           rmsbr = rmsbr + (br)**2
        end do
      else if( Spectrum%Type == 'I' ) then
        rmsbr = 0.d+00
        do n = 1, Spectrum%N_Sample
           ks = idnint(mod(abs(Spectrum%Imag_Part(n))*1.d+08,128.d+00))
           do kk = 1, ks
              br = prnd()
           end do
           br = dsqrt(dble(OSFactor))*Spectrum_Noise%Imag_Part(n)*grnd(0)
           Spectrum%Imag_Part(n) = Spectrum%Imag_Part(n) + br
           rmsbr = rmsbr + (br)**2
        end do
      else if( Spectrum%Type == 'C' ) then
        rmsbr = 0.d+00
        do n = 1, Spectrum%N_Sample
           ks = idnint(mod(abs(Spectrum%Complex(n))*1.d+08,128.d+00))
           do kk = 1, ks
              br = prnd()
           end do
           br = dsqrt(dble(OSFactor))*Spectrum_Noise%Complex(n)*grnd(0)
           Spectrum%Complex(n) = Spectrum%Complex(n) + br
           rmsbr = rmsbr + (br)**2
        end do
      else if( Spectrum%Type == 'M' ) then
        rmsbr = 0.d+00
        do n = 1, Spectrum%N_Sample
           ks = idnint(mod(abs(Spectrum%Modulus(n))*1.d+08,128.d+00))
           do kk = 1, ks
              br = prnd()
           end do
           br = dsqrt(dble(OSFactor))*Spectrum_Noise%Modulus(n)*grnd(0)
           Spectrum%Modulus(n) = Spectrum%Modulus(n) + br
           rmsbr = rmsbr + (br)**2
        end do
      else if( Spectrum%Type == 'A' ) then
        rmsbr = 0.d+00
        do n = 1, Spectrum%N_Sample
           ks = idnint(mod(abs(Spectrum%Argument(n))*1.d+08,128.d+00))
           do kk = 1, ks
              br = prnd()
           end do
           br = dsqrt(dble(OSFactor))*Spectrum_Noise%Argument(n)*grnd(0)
           Spectrum%Argument(n) = Spectrum%Argument(n) + br
           rmsbr = rmsbr + (br)**2
        end do
      else if( Spectrum%Type == 'MA' ) then
        rmsbr = 0.d+00
        do n = 1, Spectrum%N_Sample
           ks = idnint(mod(abs(Spectrum%Modulus(n))*1.d+08,128.d+00))
           do kk = 1, ks
              br = prnd()
           end do
           br = dsqrt(dble(OSFactor))*Spectrum_Noise%Modulus(n)*grnd(0)
           Spectrum%Modulus(n) = Spectrum%Modulus(n) + br
           rmsbr = rmsbr + (br)**2
        end do
        rmsbr = 0.d+00
        do n = 1, Spectrum%N_Sample
           ks = idnint(mod(abs(Spectrum%Argument(n))*1.d+08,128.d+00))
           do kk = 1, ks
              br = prnd()
           end do
           br = dsqrt(dble(OSFactor))*Spectrum_Noise%Argument(n)*grnd(0)
           Spectrum%Argument(n) = Spectrum%Argument(n) + br
           rmsbr = rmsbr + (br)**2
        end do
      else if( Spectrum%Type == 'RI' ) then
        rmsbr = 0.d+00
        do n = 1, Spectrum%N_Sample
           ks = idnint(mod(abs(Spectrum%Real_Part(n))*1.d+08,128.d+00))
           do kk = 1, ks
              br = prnd()
           end do
           br = dsqrt(dble(OSFactor))*Spectrum_Noise%Real_Part(n)*grnd(0)
           Spectrum%Real_Part(n) = Spectrum%Real_Part(n) + br
           rmsbr = rmsbr + (br)**2
        end do
        rmsbr = 0.d+00
        do n = 1, Spectrum%N_Sample
           ks = idnint(mod(abs(Spectrum%Imag_Part(n))*1.d+08,128.d+00))
           do kk = 1, ks
              br = prnd()
           end do
           br = dsqrt(dble(OSFactor))*Spectrum_Noise%Imag_Part(n)*grnd(0)
           Spectrum%Imag_Part(n) = Spectrum%Imag_Part(n) + br
           rmsbr = rmsbr + (br)**2
        end do
     else
        write(*,*) 'Spectrum%Type Error',Spectrum%Type
        write(*,*) 'add_spectrum_noise Fatal Error'
        call exit(1)
     end if
     rmsbr = dsqrt( rmsbr/dble(Spectrum%N_Sample))
     write(*,*) 'add_spectrum_noise rmsbr ',rmsbr
!
     return
   end subroutine add_spectrum_noise
!
!

!> spectrum_noise_init -- Public
!!
!! * Purpose
!!
!!     Initialisation of the spectrum noise
!!
!! * Description
!! 
!!     This subroutine allows the initialisation of the spectrum noise. First, a dedicated file 
!!     is read and required tables are allocated. Then units are checked and if it is required, 
!!     units conversion is done:
!!         -  wavenumbers have to be in m-1, and 
!!         -  noise has to be in watt/m**2/stradian/m-1, or in NedT at the reference temperature.
!!     At last, in function of the spectrum type, noise spline interpolation is done.
!!
!! * Inputs
!!
!!     - File_Noise     : noise filename
!!     - Spectrum_Noise : type_Spectrum / type for declaration and allocation of spectrum
!! 
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Spectrum_Noise : type_Spectrum / type for declaration and allocation of spectrum
!!
!! * References
!!

   subroutine spectrum_noise_init( File_Noise, Spectrum, Spectrum_Noise )
   implicit none
     character(len=*)        , intent(in)                :: File_Noise
     type(type_Spectrum)     , intent(in)                :: Spectrum
     type(type_Spectrum)     , intent(inout)             :: Spectrum_Noise
     integer(kind=LONG)                                  :: iFile
     integer(kind=LONG)                                  :: iPos
     integer(kind=LONG)                                  :: ErrCode
     integer(kind=LONG)                                  :: ioalloc
     character(len=20)                                   :: Units_Wn
     character(len=20)                                   :: Units_Noise
     character(len=20)                                   :: Label
     integer(kind=LONG)                                  :: Ns
     integer(kind=LONG)                                  :: N_Sample
     real(kind=DOUBLE)   ,dimension(:) ,allocatable      :: Wn
     real(kind=DOUBLE)   ,dimension(:) ,allocatable      :: Noise
     real(kind=DOUBLE)   ,dimension(:) ,allocatable      :: Real_Part
     real(kind=DOUBLE)   ,dimension(:) ,allocatable      :: Imag_Part
     real(kind=DOUBLE)                                   :: T_Ref
     real(kind=DOUBLE)                                   :: w
     real(kind=DOUBLE)                                   :: dwsdt
!
     iFile   = 10
     iPos    = 1
     ioalloc = 0
!
!    reading radiometric noise
     open(unit=iFile, file=File_Noise, status='old',err=999)
     iPos  = 2
     read(iFile,'(a)',err=999) Label
     write(*,*) 'Label : ',Label
     iPos  = 3
     read(iFile,'(a)',err=999) Units_Wn
     write(*,*) 'Units_Wn : ',Units_Wn
     iPos  = 4
     read(iFile,'(a)',err=999) Units_Noise
     write(*,*) 'Units_Noise : ',Units_Noise
     iPos  = 5
     read(iFile,'(f3.0)',err=999) T_Ref
     write(*,*) 'T_Ref : ',T_Ref
     iPos  = 6
     read(iFile,'(a)',err=999) Label
     write(*,*) 'Label : ',Label
     iPos  = 7
     read(iFile,*,err=999)     N_Sample
     write(*,*) 'N_Sample : ',N_Sample
     iPos  = 8
     allocate( Wn(N_Sample), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Noise(N_Sample), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     do Ns = 1, N_Sample
        read(iFile,*,err=999) Wn(Ns),Noise(Ns)
        iPos  = 8 + Ns
     end do
     close(unit=iFile)
     if( Units_Wn(1:len_trim(Units_Wn)) == 'cm-1' ) then
        write(*,*) 'Conversion Units_Wn : ',Units_Wn
        do Ns = 1, N_Sample
           Wn(Ns) = Wn(Ns) * 100.
        end do
     end if
     if( Units_Noise(1:len_trim(Units_Noise)) == 'nW_cm2_sr_cm' ) then
        write(*,*) 'Conversion Units_Noise : ',Units_Noise
        do Ns = 1, N_Sample
           Noise(Ns) = Noise(Ns) * 1.d-07
        end do
     else if( Units_Noise(1:len_trim(Units_Noise)) == 'mW_m2_sr_cm' ) then
        write(*,*) 'Conversion Units_Noise : ',Units_Noise
        do Ns = 1, N_Sample
           Noise(Ns) = Noise(Ns) * 1.d-05
        end do
     else if( Units_Noise(1:len_trim(Units_Noise)) == 'Kelvin' ) then
        write(*,*) 'Conversion Units_Noise : ',Units_Noise
        do Ns = 1, N_Sample
           call plkderive( T_Ref, Wn(Ns), w ,dwsdt )
           Noise(Ns) = Noise(Ns) * dwsdt
        end do
     else
        write(*,*) 'Conversion Units_Noise : None '
     end if
!
     call Spectrum_Header_Transfer( Spectrum, Spectrum_Noise )
     if( Spectrum_Noise%Type == 'R' ) then
        call inttabdble( Noise, Wn, N_Sample, Spectrum_Noise%Real_Part,&
                         Spectrum_Noise%Wn, Spectrum_Noise%N_Sample    )
     else if( Spectrum_Noise%Type == 'I' ) then
        call inttabdble( Noise, Wn, N_Sample, Spectrum_Noise%Imag_Part,&
                         Spectrum_Noise%Wn, Spectrum_Noise%N_Sample    )
     else if( Spectrum_Noise%Type == 'M' ) then
        call inttabdble( Noise, Wn, N_Sample, Spectrum_Noise%Modulus,&
                         Spectrum_Noise%Wn, Spectrum_Noise%N_Sample  )
     else if( Spectrum_Noise%Type == 'A' ) then
        call inttabdble( Noise, Wn, N_Sample, Spectrum_Noise%Argument,&
                         Spectrum_Noise%Wn, Spectrum_Noise%N_Sample   )
     else if( Spectrum_Noise%Type == 'MA' ) then
        call inttabdble( Noise, Wn, N_Sample, Spectrum_Noise%Modulus,&
                         Spectrum_Noise%Wn, Spectrum_Noise%N_Sample  )
        call inttabdble( Noise, Wn, N_Sample, Spectrum_Noise%Argument,&
                         Spectrum_Noise%Wn, Spectrum_Noise%N_Sample   )
     else if( Spectrum_Noise%Type == 'RI' ) then
        call inttabdble( Noise, Wn, N_Sample, Spectrum_Noise%Real_Part,&
                         Spectrum_Noise%Wn, Spectrum_Noise%N_Sample    )
        call inttabdble( Noise, Wn, N_Sample, Spectrum_Noise%Imag_Part,&
                         Spectrum_Noise%Wn, Spectrum_Noise%N_Sample    )
     else if( Spectrum_Noise%Type == 'C' ) then
        allocate( Real_Part(Spectrum_Noise%N_Sample), stat=ErrCode )
        if( ErrCode /= 0 ) ioalloc = ioalloc + 1
        allocate( Imag_Part(Spectrum_Noise%N_Sample), stat=ErrCode )
        if( ErrCode /= 0 ) ioalloc = ioalloc + 1
        call inttabdble( Noise, Wn, N_Sample, Real_Part,            &
                         Spectrum_Noise%Wn, Spectrum_Noise%N_Sample )
        call inttabdble( Noise, Wn, N_Sample, Imag_Part,            &
                         Spectrum_Noise%Wn, Spectrum_Noise%N_Sample )
        Spectrum_Noise%Complex(1:Spectrum_Noise%N_Sample) = &
                dcmplx( Real_Part(1:Spectrum_Noise%N_Sample),&
                        Imag_Part(1:Spectrum_Noise%N_Sample) )
       deallocate( Real_Part )
       deallocate( Imag_Part )
     else
        write(*,*) 'Spectrum%Type Error',Spectrum%Type
        write(*,*) 'spectrum_noise_init Fatal Error'
        call exit(1)
     end if
     if( ioalloc /= 0 ) then
        write(*,*) 'Allocation Error ',ioalloc
        write(*,*) 'spectrum_noise_init Fatal Error'
        call exit(1)
     end if
     deallocate( Wn )
     deallocate( Noise )
     return
!
999  write(*,*) 'parameters reading error',iPos
     write(*,*) 'spectrum_noise_init Fatal Error'
     call exit(1)
   end subroutine spectrum_noise_init
!
!
end module spectrum_module
