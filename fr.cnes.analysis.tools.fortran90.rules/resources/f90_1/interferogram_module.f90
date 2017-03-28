!!#  interferogram_module.f90 --
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: october 2009
!!#            Version: $Revision: 1.15 $
!!#  Last modification: $Date: 2011-11-02 14:57:50 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!> interferogram_module -- Module
!!
!! * Purpose
!!
!!   Module for interferogram computation.
!!
!! * Description
!!      
!!   This module simulates and samples RPD and target interferograms. The laser Rpd interferogram 
!!   orders the sampling of the measured interferograms (whatever the targets). The interferogram 
!!   is simulated as a continuous signal on which cube corner speed variation, delay and sampling 
!!   jitter are introduced. Then, the signal is sampled by a clock or the laser RPD tops, finally 
!!   the detection chain noise is applied. 
!!
!! * Sub-routines and functions
!!
!! - interferogram_sim    : laser RPD and target interferograms simulation (target: CS/BB/EW)
!! - interferogram_scene  : scenes interferograms simulation (target: CS/BB/EV).
!! - interferogram_los    : laser RPD and target interferograms simulation (target: CS/BB/2EW)  
!! - interf_param_rpd_def : initialisation of the type defining the continuous RPD interferogram
!! - interf_param_def     : initialisation of the types defining the theoretical interferogram
!!                          and the usefull continuous interferogram 
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.4.2
!!

module interferogram_module
   use precision_type
   use error_type
   use interf_param_type
   use scene_param_type
   use interferogram_type
   use spectrum_type
   use gain_type
   use detection_type
   use amplification_type
   use spike_injection_type
   use adc_convert_type
   use non_linearity_type
   use interferogram_type
   use interf_continu_module
   use interf_sampling_module
   use gain_module
   use detection_module
   use amplification_module
   use spike_injection_module
   use adc_convert_module
   use non_linearity_module
   use scene_mixing_module
   use math_module
   use fft_module
!
   implicit none
!
!
   public ::                      &
             interferogram_scene, &
             interf_param_rpd_def,&
             interf_param_def
!
!
   contains
!
!

!> interferogram_scene -- Public
!!
!! * Purpose
!!
!!     Scenes interferograms simulation (target: CS/BB/EV).  
!!
!! * Description
!!
!!     The interferogram_scene subroutine consists in simulation and sampling of laser RPD and 
!!     target interferograms. The laser Rpd interferogram orders the sampling of the measured 
!!     interferograms (calibration and Earth View targets). 
!!     Interferograms are simulated as a continuous signal on which cube corner speed 
!!     variation, delay and sampling jitter are introduced. Then, the signal is sampled by a 
!!     clock or the laser RPD tops. 
!!     It is important to note that this subroutine allows not only the simulation of one scene,
!!     but it allows to simulate many scenes: the scene number to be simulate is define in the 
!!     Scene_Param type.
!!     
!!     More precisely, interferograms computation requires the following steps:
!!       - The first one is the definition of the simulation RPD parameters and the simulation 
!!         interferogram parameters 
!!       - Whatever the form (real, real-imaginary, or complex...) of the Rpd interferogram 
!!         computation, the laser RPD wavelength instabilities are simulated as a Gaussian 
!!         random function and a cube corner speed variation is introduced.
!!       - Then the RPD Interferogram is sampled by a clock, the temporal basis includes the 
!!         RPD sampling jitter and delay. The OPD is deduced for the sampled interferogram by 
!!         calculating the zero-crossings (RPD tops) of the sampled RPD interferogram: RPD tops
!!         at each zero crossing for all samples.
!!       - The Cold Space and Black Body continuous interferograms are computed and sampled by a 
!!         clock or by the laser RPD tops. Interferograms are amplified and converted in digital
!!         counts.
!!       - At last, each Earth View Interferogram is also computated as a continuous 
!!         interferogram and sampled by a clock or by the laser RPD tops. Interferograms are 
!!         amplified and converted in digital counts.
!!
!! * Inputs
!!
!!     - Interf_Param : type_Interf_Param / type for declaration and allocation of interferogram
!!                      parameters
!!     - Scene_Param  : type_Scene_Param / type for scene parameters declaration and allocation
!!     - Gain         : type_Gain / type for detectors gain declaration and allocation.
!!     - Detection    : type_Detection / type for declaration and allocation of detection  
!!                      parameters
!!     - Amplification : type_Amplification / type for declaration and allocation of  
!!                       amplification parameters 
!!     - Adc_Convert  :  type_Adc_Convert / type for declaration and allocation of adc 
!!                       conversion parameters
!!     - Spectrum_CS  : type_Spectrum / type for declaration and allocation of spectrum 
!!     - Spectrum_BB  : type_Spectrum / type for declaration and allocation of spectrum
!!     - Spectrum_EW  : type_Spectrum / type for declaration and allocation of spcetrum
!!     - Saf_Rpd      : type_Saf / type for declaration and allocation of saf
!!     - Saf          : type_Saf / type for declaration and allocation of saf
!!     - SUB_PN       : sub-pixel number
!!     - SB           : spectral band 
!!
!! * Inputs/outputs
!!
!!     - Interf_cont_Rpd : type_Interferogram / type for declaration and allocation of 
!!                         interferogram
!!     - Interf_Samp_Rpd : type_Interferogram / type for declaration and allocation of 
!!                         interferogram
!!
!! * Outputs
!!
!!     - Interf_Samp_CS : type_Interferogram / type for declaration and allocation of 
!!                        interferogram
!!     - Interf_Samp_BB : type_Interferogram / type for declaration and allocation of 
!!                        interferogram
!!     - Interf_Samp_EW : type_Interferogram / type for declaration and allocation of 
!!                        interferogram
!!
!! * References
!!

   subroutine interferogram_scene( Interf_Param,      &
                                   Scene_Param,       &
                                   Gain,              &
                                   Detection,         &
                                   Non_Linearity,     &
                                   Amplification,     &
                                   Spike_Injection_CS,&
                                   Spike_Injection_BB,&
                                   Spike_Injection_EW,&
                                   Adc_Convert_Nadc,  &
                                   Spectrum_BG,       &
                                   Spectrum_CS,       &
                                   Spectrum_BB,       &
                                   Spectrum_EW,       &
                                   Saf_Rpd,           &
                                   Saf,               &
                                   SUB_PN,            &
                                   SB,                &
                                   Interf_cont_Rpd,   &
                                   Interf_Samp_Rpd,   &
                                   Interf_Samp_CS,    &
                                   Interf_Samp_BB,    &
                                   Interf_Samp_EW     )
   implicit none
     type(type_Interf_Param) ,intent(in)                 :: Interf_Param
     type(type_Scene_Param)  ,intent(in)                 :: Scene_Param
     type(type_Gain)         ,intent(in)                 :: Gain
     type(type_Detection)    ,intent(in)                 :: Detection
     type(type_non_linearity),intent(in)                 :: Non_Linearity
     type(type_Amplification),intent(in)                 :: Amplification
     type(type_Spike_Injection),intent(in)               :: Spike_Injection_CS
     type(type_Spike_Injection),intent(in)               :: Spike_Injection_BB
     type(type_Spike_Injection),intent(in)               :: Spike_Injection_EW
     type(type_Adc_Convert_Nadc),intent(in)              :: Adc_Convert_Nadc
     type(type_Spectrum)     ,intent(in)                 :: Spectrum_BG
     type(type_Spectrum)     ,intent(in)                 :: Spectrum_CS
     type(type_Spectrum)     ,intent(in)                 :: Spectrum_BB
     type(type_Spectrum)     ,intent(in)                    &
                         ,dimension(Scene_Param%NbScene) :: Spectrum_EW
     type(type_Saf)          ,intent(in)                 :: Saf_Rpd
     type(type_Saf)          ,intent(in)                 :: Saf
     integer(kind=LONG)      ,intent(in)                 :: SUB_PN
     integer(kind=LONG)      ,intent(in)                 :: SB
     type(type_Interferogram),intent(inout)              :: Interf_cont_Rpd
     type(type_Interferogram),intent(inout)              :: Interf_Samp_Rpd
     type(type_Interferogram),intent(out)                :: Interf_Samp_CS
     type(type_Interferogram),intent(out)                :: Interf_Samp_BB
     type(type_Interferogram),intent(out)                   &
                         ,dimension(Scene_Param%NbScene) :: Interf_Samp_EW
     type(type_Interferogram)                            :: Interf_Diff
     type(type_Interferogram)                            :: Interferogram
     type(type_Interferogram)                            :: Interf_cont
     type(type_Interferogram)                            :: Interf_cont_dvcc
     character(len=2)                                    :: Interf_Target
     character(LEN=3)                                    :: Path
     integer(kind=LONG)                                  :: SC
!
!    simulation rpd parameters definition
     if( (SUB_PN == 1) .and. (SB == 1) ) then
        call interf_param_rpd_def( Interf_Param,   &
                                   Spectrum_EW(1), &
                                   Interf_cont_Rpd )
        write(0,*) 'end interf_param_rpd_def'
     end if
!
!    simulation parameters definition
     call interf_param_def( Interf_Param,  &
                            Spectrum_EW(1),&
                            Interferogram, &
                            Interf_cont    )
     write(0,*) 'end interf_param_def'
!
!    Rpd Interferogram computation
!    Cube Corner speed variation introduction
     if( (SUB_PN == 1) .and. (SB == 1) ) then
        call interf_cont_rpd_sim( Interf_Param,   &
                                  Saf_Rpd,        &
                                  Interf_cont_Rpd )
        write(0,*) 'end interf_cont_rpd_sim'
!
!       Rpd Interferogram sampling
        Path = 'RPD'
        call sampling_clock( Path,            &
                             Interf_Param,    &
                             Interf_cont_Rpd, &
                             Interf_Samp_Rpd  )
        write(0,*) 'end sampling_clock Rpd'
        if( Interf_Param%Rpd_Type == 'RI' ) then
!
!          OPD computation including speed variation
           call opd_samp( Interf_Param,   &
                          Interf_Samp_Rpd )
           write(0,*) 'end opd_samp'
        else if( Interf_Param%Rpd_Type == 'R' ) then
!
!          Zero crossing computation
           call rpd_zero_crossing( Interf_Param,   &
                                   Interf_Samp_Rpd )
           write(0,*) 'end rpd_zero_crossing'
        else
           write(*,*) 'Interf_Param%Rpd_Type Error',Interf_Param%Rpd_Type
           write(*,*) 'interferogram_sim Fatal Error'
           call exit(1)
        end if
     end if
!
!    Cold Space Interferogram computation
     Interf_Target = 'CS'
     write(*,*) 'Spectrum_CS%N_Sample',Spectrum_CS%N_Sample
     call interf_cont_sim( Interf_Target,   &
                           Interf_Param,    &
                           Gain,            &
                           Detection,       &
                           Spectrum_BG,     &
                           Spectrum_CS,     &
                           Interferogram,   &
                           Interf_cont,     &
                           Interf_cont_Rpd, &
                           Saf,             &
                           SUB_PN,          & 
                           SB,              & 
                           Interf_cont_dvcc )
!
     write(*,*) 'CS signal_Continu Min',&
          minval(Interf_cont_dvcc%Real_Part(1:Interf_cont_dvcc%N_Sample))
     write(*,*) 'CS signal_continu Max',&
          maxval(Interf_cont_dvcc%Real_Part(1:Interf_cont_dvcc%N_Sample))
!
     write(0,*) 'end interf_cont_sim CS'
     if(      Interf_Param%Sampling_Mode == 'CLOCK' ) then
        Path = 'MES'
        write(*,*) 'sampling_clock CS'
        select case( Interf_Param%Integration_Mode       &
             (1:len_trim(Interf_Param%Integration_Mode)) )
        case ( 'Differe' )
           call sampling_clock( Path,             &
                                Interf_Param,     &
                                Interf_cont_dvcc, &
                                Interf_Diff       )
           call recombine_differential( Interf_Param,  &
                                        Interf_Diff,   &
                                        Interf_Samp_CS )
           call dalloc_Interferogram( Interf_Diff )
        case default
           call sampling_clock( Path,             &
                                Interf_Param,     &
                                Interf_cont_dvcc, &
                                Interf_Samp_CS    )
        end select
        write(0,*) 'end sampling_clock CS'
     else if( Interf_Param%Sampling_Mode == 'LZRPD' ) then
        write(*,*) 'sampling_lzrpd CS'
        call sampling_lzrpd( Interf_Param,     &
                             Interf_Samp_Rpd,  &
                             Interf_cont_dvcc, &
                             Interf_Samp_CS    )
        write(0,*) 'end sampling_lzrpd CS'
     else
        write(*,*) 'Sampling mode Error',Interf_Param%Sampling_Mode
        write(*,*) 'interferogram_sim Fatal Error'
        call exit(1)
     end if
     call dalloc_Interferogram( Interf_cont_dvcc )
!
!    interferogram analog non-linearity
     call analog_non_linearity( Non_Linearity, &
                                Interf_Samp_CS )
!
!    interferogram amplification
     call interf_amplification( Detection,     &
                                Amplification, &
                                Interf_Samp_CS )
!
!    interferogram spike injection
     call spike_injection( Spike_Injection_CS, &
                           Amplification,      &
                           Interf_Samp_CS      )
!
!    interferogram digital non-linearity
     call digital_non_linearity( Non_Linearity, &
                                 Interf_Samp_CS )
!
!    interferogram adc conversion
     call interf_adc_convert( Amplification,    &
                              Adc_Convert_Nadc, &
                              Interf_Samp_CS )
!
!    Black Body Interferogram computation
     Interf_Target = 'BB'
     write(*,*) 'Spectrum_BB%N_Sample',Spectrum_BB%N_Sample
     call interf_cont_sim( Interf_Target,   &
                           Interf_Param,    &
                           Gain,            &
                           Detection,       &
                           Spectrum_BG,     &
                           Spectrum_BB,     &
                           Interferogram,   &
                           Interf_cont,     &
                           Interf_cont_Rpd, &
                           Saf,             &
                           SUB_PN,          & 
                           SB,              & 
                           Interf_cont_dvcc )
!
     write(*,*) 'BB signal_Continu Min',&
          minval(Interf_cont_dvcc%Real_Part(1:Interf_cont_dvcc%N_Sample))
     write(*,*) 'BB signal_continu Max',&
          maxval(Interf_cont_dvcc%Real_Part(1:Interf_cont_dvcc%N_Sample))
!
     write(0,*) 'end interf_cont_sim BB'
     if(      Interf_Param%Sampling_Mode == 'CLOCK' ) then
        Path = 'MES'
        write(*,*) 'sampling_clock BB'
        select case( Interf_Param%Integration_Mode       &
             (1:len_trim(Interf_Param%Integration_Mode)) )
        case ( 'Differe' )
           call sampling_clock( Path,             &
                                Interf_Param,     &
                                Interf_cont_dvcc, &
                                Interf_Diff       )
           call recombine_differential( Interf_Param,  &
                                        Interf_Diff,   &
                                        Interf_Samp_BB )
           call dalloc_Interferogram( Interf_Diff )
        case default
           call sampling_clock( Path,             &
                                Interf_Param,     &
                                Interf_cont_dvcc, &
                                Interf_Samp_BB    )
        end select
        write(0,*) 'end sampling_clock BB'
     else if( Interf_Param%Sampling_Mode == 'LZRPD' ) then
        write(*,*) 'sampling_lzrpd BB'
        call sampling_lzrpd( Interf_Param,     &
                             Interf_Samp_Rpd,  &
                             Interf_cont_dvcc, &
                             Interf_Samp_BB    )
        write(0,*) 'end sampling_lzrpd BB'
     else
        write(*,*) 'Sampling mode Error',Interf_Param%Sampling_Mode
        write(*,*) 'interferogram_sim Fatal Error'
        call exit(1)
     end if
     call dalloc_Interferogram( Interf_cont_dvcc )
!
!    interferogram analog non-linearity
     call analog_non_linearity( Non_Linearity, &
                                Interf_Samp_BB )
!
!    interferogram amplification
     call interf_amplification( Detection,     &
                                Amplification, &
                                Interf_Samp_BB )
!
!    interferogram spike injection
     call spike_injection( Spike_Injection_BB, &
                           Amplification,      &
                           Interf_Samp_BB      )
!
!    interferogram digital non-linearity
     call digital_non_linearity( Non_Linearity, &
                                 Interf_Samp_BB )
!
!    interferogral adc conversion
     call interf_adc_convert( Amplification, &
                              Adc_Convert_Nadc, &
                              Interf_Samp_BB )
!
!    Scene loop
     do SC = 1, Scene_Param%NbScene
!
!       Earth View Interferogram computation
        Interf_Target = 'EW'
        call interf_cont_sim( Interf_Target,   &
                              Interf_Param,    &
                              Gain,            &
                              Detection,       &
                              Spectrum_BG,     &
                              Spectrum_EW(SC), &
                              Interferogram,   &
                              Interf_cont,     &
                              Interf_cont_Rpd, &
                              Saf,             &
                              SUB_PN,          & 
                              SB,              & 
                              Interf_cont_dvcc )
!
        write(*,*) 'EW signal_Continu Min',&
          minval(Interf_cont_dvcc%Real_Part(1:Interf_cont_dvcc%N_Sample))
        write(*,*) 'EW signal_continu Max',&
          maxval(Interf_cont_dvcc%Real_Part(1:Interf_cont_dvcc%N_Sample))
!
        write(0,*) 'end interf_cont_sim EW SC',SC
        if(      Interf_Param%Sampling_Mode == 'CLOCK' ) then
           Path = 'MES'
           write(*,*) 'sampling_clock EW SC',SC
           select case( Interf_Param%Integration_Mode       &
                (1:len_trim(Interf_Param%Integration_Mode)) )
           case ( 'Differe' )
              call sampling_clock( Path,             &
                                   Interf_Param,     &
                                   Interf_cont_dvcc, &
                                   Interf_Diff       )
              call recombine_differential( Interf_Param,      &
                                           Interf_Diff,       &
                                           Interf_Samp_EW(SC) )
              call dalloc_Interferogram( Interf_Diff )
           case default
              call sampling_clock( Path,              &
                                   Interf_Param,      &
                                   Interf_cont_dvcc,  &
                                   Interf_Samp_EW(SC) )
           end select
           write(0,*) 'end sampling_clock EW SC',SC
        else if( Interf_Param%Sampling_Mode == 'LZRPD' ) then
           write(*,*) 'sampling_lzrpd EW SC',SC
           call sampling_lzrpd( Interf_Param,      &
                                Interf_Samp_Rpd,   &
                                Interf_cont_dvcc,  &
                                Interf_Samp_EW(SC) )
           write(0,*) 'end sampling_lzrpd EW SC',SC
        else
           write(*,*) 'Sampling mode Error',Interf_Param%Sampling_Mode
           write(*,*) 'interferogram_sim Fatal Error'
           call exit(1)
        end if
        call dalloc_Interferogram( Interf_cont_dvcc )
!
!       interferogram analog non-linearity
        call analog_non_linearity( Non_Linearity,     &
                                   Interf_Samp_EW(SC) )
!
!       interferogram amplification
        call interf_amplification( Detection,         &
                                   Amplification,     &
                                   Interf_Samp_EW(SC) )
!
!       interferogram spike injection
        call spike_injection( Spike_Injection_EW, &
                              Amplification,      &
                              Interf_Samp_EW(SC)  )
!
!       interferogram digital non-linearity
        call digital_non_linearity( Non_Linearity,     &
                                    Interf_Samp_EW(SC) )
!
!       interferogral adc conversion
        call interf_adc_convert( Amplification,     &
                                 Adc_Convert_Nadc,  &
                                 Interf_Samp_EW(SC) )
     end do ! end scene loop
!
!    de allocation
     call dalloc_Interferogram( Interferogram )
     call dalloc_Interferogram( Interf_cont )
     return
!
   end subroutine interferogram_scene
!
!

!> interf_param_def -- Public
!!
!! * Purpose
!!
!!     Initialisation of the types defining the theoretical interferogram and the usefull 
!!     continuous interferogram
!!
!! * Description
!!
!!     This subroutine defines: 
!!         - the FFT size computation including oversampling
!!         - the theoretical interferogram scaling factors
!!         - the usefull continuous interferogram extraction parameters
!!
!! * Inputs
!!
!!     - Interf_Param : type_Interf_Param / type for interferogram parameters declaration 
!!                      and allocation.
!!     - Spectrum     : type_Spectrum / type for spectrum declaration and allocation.
!!
!! * Inputs/outputs
!!
!!     - Interferogram : type_Interferogram / type for interferogram declaration and allocation.
!!     - Interf_cont   : type_Interferogram / type for interferogram declaration and allocation.
!!
!! * Outputs
!!
!! * References
!!

   subroutine interf_param_def( Interf_Param, &
                                Spectrum,     &
                                Interferogram,&
                                Interf_cont   )
   implicit none
     type(type_Interf_Param) ,intent(in)                 :: Interf_Param
     type(type_Spectrum)     ,intent(in)                 :: Spectrum
     type(type_Interferogram),intent(out)                :: Interferogram
     type(type_Interferogram),intent(out)                :: Interf_cont
     integer(kind=LONG)                                  :: N_Sample
     integer(kind=LONG)                                  :: NsFft
     integer(kind=LONG)                                  :: NsFftp
     integer(kind=LONG)                                  :: ios
!
!    FFT size computation including oversampling
     ios = 0
     N_Sample  = 2 * idnint( Spectrum%WnMax/Spectrum%dWn )
     call tabule_NsFft( N_Sample, NsFft, ios )
     if( ios /= 0 ) then
        write(*,*) 'tabule_NsFFT error'
        write(*,*) 'interf_param_def error'
        call exit(1)
     end if
     NsFft = NsFft * Interf_Param%OSFactor
     NsFftp = int(NsFft/2)
     write(*,*) 'interf_param_def'
     write(*,*) 'NsFft',NsFft
!
!    theoretical interferogram scaling factors computation
     Interferogram%Type     = Interf_Param%Type
     Interferogram%N_Sample = NsFft+1
     Interferogram%OpdMax   = 1.d+00 / ( 2.d+00*Spectrum%dWn )
     Interferogram%dOpd     = 1.d+00 / ( 2.d+00*Spectrum%dWn*dble(NsFftp) )
     Interferogram%dTime    = Interferogram%dOpd &
                            / ( 2.d+00*Interf_Param%VCC )
     Interferogram%TimeMax  = Interferogram%dTime &
                            * int(Interferogram%N_Sample/2)
     Interferogram%N_Top    = 0
!
!    usefull continuous interferogram extraction
     Interf_cont%Type     = Interferogram%Type
     Interf_cont%dOpd     = Interferogram%dOpd
     Interf_cont%dTime    = Interf_cont%dOpd &
                          / ( 2.d+00*Interf_Param%VCC )
     Interf_cont%N_Sample =                       &
               2 * idnint( Interf_Param%Ratio_Opd &
                         * Interf_Param%OpdMax    &
                         / Interf_cont%dOpd ) + 1
     Interf_cont%OpdMax   = Interf_cont%dOpd &
                          * int(Interf_cont%N_Sample/2)
     Interf_cont%TimeMax  = Interf_cont%dTime &
                          * int(Interf_cont%N_Sample/2)
     Interf_cont%N_Top    = 0
     write(*,*) 'Interferogram%N_Sample',Interferogram%N_Sample
     write(*,*) 'Interf_cont%N_Sample  ',Interf_cont%N_Sample
     write(*,*) 'Interf_cont%Type      ',Interf_cont%Type
     write(*,*) 'Interf_cont%dOpd      ',Interf_cont%dOpd
     write(*,*) 'Interf_cont%dTime     ',Interf_cont%dTime
     write(*,*) 'Interf_cont%OpdMax    ',Interf_cont%OpdMax
     write(*,*) 'Interf_cont%TimeMax   ',Interf_cont%TimeMax
     if( Interf_cont%dTime > Interf_Param%dT_Mes_Samp ) then
        write(*,*) 'Inconsistency between OSFactor and dT_Mes_Samp'
        write(*,*) 'interf_param_def Fatal Error'
        call exit(1)
     end if
     call alloc_Interferogram( Interferogram )
     call alloc_Interferogram( Interf_cont )
     call Interferogram_Basis( Interferogram )
     call Interferogram_Basis( Interf_cont )
     write(*,*) 'Interf_cont%Time(1)       ',&
                 Interf_cont%Time(1)
     write(*,*) 'Interf_cont%Time(N_Sample)',&
                 Interf_cont%Time(Interf_cont%N_Sample)
     write(*,*) 'Interf_cont%Opd(1)        ',&
                 Interf_cont%Opd(1)
     write(*,*) 'Interf_cont%Opd(N_Sample) ',&
                 Interf_cont%Opd(Interf_cont%N_Sample)
     return
!
   end subroutine interf_param_def
!
!

!> interf_param_rpd_def -- Public
!!
!! * Purpose
!!
!!     Initialisation of the type defining the continuous RPD interferogram
!!
!! * Description
!!
!!     This subroutine defines: 
!!         - the FFT size computation including oversampling
!!         - the continuous RPD interferogram parameters  
!!
!! * Inputs
!!
!!     - Interf_Param : type_Interf_Param / type for interferogram parameters declaration 
!!                      and allocation.
!!     - Spectrum     : type_Spectrum / type for spectrum declaration and allocation.
!!
!! * Inputs/outputs
!!
!!     - Interf_cont_Rpd : type_Interferogram / type for interferogram declaration and 
!!                         allocation.
!!
!! * Outputs
!!
!! * References
!!

   subroutine interf_param_rpd_def( Interf_Param,   &
                                    Spectrum,       &
                                    Interf_cont_Rpd )
   implicit none
     type(type_Interf_Param) ,intent(in)                 :: Interf_Param
     type(type_Spectrum)     ,intent(in)                 :: Spectrum
     type(type_Interferogram),intent(inout)              :: Interf_cont_Rpd
     integer(kind=LONG)                                  :: N_Sample
     integer(kind=LONG)                                  :: NsFft
     integer(kind=LONG)                                  :: NsFftp
     integer(kind=LONG)                                  :: ios
!
!    FFT size computation including oversampling
     ios = 0
     N_Sample  = 2 * idnint( Spectrum%WnMax/Spectrum%dWn )
     call tabule_NsFft( N_Sample, NsFft, ios )
     if( ios /= 0 ) then
        write(*,*) 'tabule_NsFFT error'
        write(*,*) 'interf_param_def error'
        call exit(1)
     end if
     NsFft = NsFft * Interf_Param%OSFactor
     NsFftp = int(NsFft/2)
     write(*,*) 'interf_param_rpd_def'
     write(*,*) 'NsFft',NsFft
!
!    continuous RPD interferogram parameters
     Interf_cont_Rpd%Type     = Interf_Param%Rpd_Type
     Interf_cont_Rpd%dOpd     = 1.d+00 / ( 2.d+00*Spectrum%dWn*dble(NsFftp) )
     Interf_cont_Rpd%dTime    = Interf_cont_Rpd%dOpd &
                              / ( 2.d+00*Interf_Param%VCC )
     Interf_cont_Rpd%N_Sample =                     &
               2 * idnint( Interf_Param%Ratio_Opd   &
                         * Interf_Param%OpdMax      &
                         / Interf_cont_Rpd%dOpd ) + 1
     Interf_cont_Rpd%OpdMax   = Interf_cont_Rpd%dOpd &
                              * int(Interf_cont_Rpd%N_Sample/2)
     Interf_cont_Rpd%TimeMax  = Interf_cont_Rpd%dTime &
                              * int(Interf_cont_Rpd%N_Sample/2)
     Interf_cont_Rpd%N_Top    = 0
     write(*,*) 'Interf_cont_Rpd%N_Sample',Interf_cont_Rpd%N_Sample
     write(*,*) 'Interf_cont_Rpd%Type    ',Interf_cont_Rpd%Type
     write(*,*) 'Interf_cont_Rpd%dOpd    ',Interf_cont_Rpd%dOpd
     write(*,*) 'Interf_cont_Rpd%dTime   ',Interf_cont_Rpd%dTime
     write(*,*) 'Interf_cont_Rpd%OpdMax  ',Interf_cont_Rpd%OpdMax
     write(*,*) 'Interf_cont_Rpd%TimeMax ',Interf_cont_Rpd%TimeMax
     if( Interf_cont_Rpd%dTime > Interf_Param%dT_Rpd_Samp ) then
        write(*,*) 'Inconsistency between OSFactor and dT_Rpd_Samp'
        write(*,*) 'interf_param_rpd_def Fatal Error'
        call exit(1)
     end if
     call alloc_Interferogram( Interf_cont_Rpd )
     call Interferogram_Basis( Interf_cont_Rpd )
     write(*,*) 'Interf_cont_Rpd%Time(1)       ',&
                 Interf_cont_Rpd%Time(1)
     write(*,*) 'Interf_cont_Rpd%Time(N_Sample)',&
                 Interf_cont_Rpd%Time(Interf_cont_Rpd%N_Sample)
     write(*,*) 'Interf_cont_Rpd%Opd(1)        ',&
                 Interf_cont_Rpd%Opd(1)
     write(*,*) 'Interf_cont_Rpd%Opd(N_Sample) ',&
                 Interf_cont_Rpd%Opd(Interf_cont_Rpd%N_Sample)
     return
!
   end subroutine interf_param_rpd_def
!
!
end module interferogram_module
