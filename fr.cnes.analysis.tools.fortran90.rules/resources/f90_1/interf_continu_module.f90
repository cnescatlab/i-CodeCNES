!!#  interf_continu_module.f90 -- 
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
!> interf_continu_module -- Module
!!
!! * Purpose
!!
!!   Module for continuous interferogram computation
!!
!! * Description
!!      
!!   The module simulates and samples RPD and target interferograms. The laser Rpd 
!!   interferogram orders the sampling of the measured interferograms (calibration and 
!!   Earth View targets). The interferogram is simulated as a continuous signal on which 
!!   cube corner speed variation, delay and sampling jitter are introduced. 
!!   Then, the signal is sampled by a clock or the laser RPD tops, finally the detection 
!!   chain noise is applied. 
!!
!! * Sub-routines and functions
!!
!! -  interf_cont_sim       : continuous interferogram simulation 
!! -  interf_cont_laser_sim : laser perfect theoretical interferogram simulation 
!! -  interf_cont_cnv_ft    : interferogram convolved by the instrument functions 
!! -  interf_cont_rpd_sim   : continuous laser RPD interferogram simulation
!! -  vcc_variation         : computation of corner cube speed variation 
!! -  interf_cont_dvcc      : interferogram computation taking into account corner cube 
!!                            speed variation
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : AD
!!

module interf_continu_module
   use precision_type
   use error_type
   use constantes_type
   use interf_param_type
   use interferogram_type
   use detection_type
   use spectrum_type
   use fft_module
   use saf_type
   use gain_type
   use math_module
   use gain_module
   use detection_module
   use statistique_module
!
   implicit none
!
!
   public :: interf_cont_sim,       &
             interf_cont_laser_sim, &
             interf_cont_rpd_sim,   &
             interf_cont_dvcc,      &
             vcc_variation,         &
             interf_cont_cnv_ft

!
   contains
!
!

!> interf_cont_sim -- Public
!!
!! * Purpose
!!
!!     Continuous interferogram simulation 
!!
!! * Description
!! 
!!     The continuous interferogram simulation requires the following steps:
!!     - For a calibration or Earth View target, continuous interferogram is computed 
!!       as the direct Fourier Transform of the target spectrum after symmetrisation. 
!!     - The sampling and the OPD are identical at those of the continuous laser RPD 
!!       interferogram. The laser line interferogram can be added to the continuous 
!!       interferogram in the case of the target EW.
!!     - The self apodisation functions are applied on the target continuous interferogram to 
!!       obtain interferograms convolved by the instrument functions. 
!!     - The continuous interferogram usefull part is extracted.
!!     - Then the cube corner speed variation is introduced on convolved continuous 
!!       interferograms.
!!
!! * Inputs
!!
!!     - Interf_Target   : 
!!     - Interf_Param    : type_Interf_Param / type for declaration and allocation of 
!!                         interferogram parameters
!!     - Spectrum        : type_Spectrum / type for declaration and allocation of spectrum
!!     - Interf_cont_Rpd : type_Interferogram / type for declaration and allocation of 
!                          interferogram
!!     - Saf             : type_Saf / type for declaration and allocation of saf parameters
!!     - SUB_PN          : sub-pixel 
!!     - SB              : spectral band
!! 
!! * Inputs/outputs
!!
!!     - Interferogram : type_Interferogram / type for declaration and allocation of 
!!                       interferogram
!!     - Interf_cont   : type_Interferogram / type for declaration and allocation of 
!!                       interferogram
!!
!! * Outputs
!!
!!     - Interf_cont_vcc : type_Interferogram / type for declaration and allocation 
!!                            of interferogram
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : AD
!!

   subroutine interf_cont_sim( Interf_Target,     &
                               Interf_Param,      &
                               Gain,              &
                               Detection,         &
                               Spectrum_bg,       &
                               Spectrum,          &
                               Interferogram,     &
                               Interf_cont,       &
                               Interf_cont_Rpd,   &
                               Saf,               &
                               SUB_PN,            & 
                               SB,                & 
                               Interf_cont_vcc   )
   implicit none
     character(len=*)        , intent(in)                :: Interf_Target
     type(type_Interf_Param) , intent(in)                :: Interf_Param
     type(type_Gain)         , intent(in)                :: Gain
     type(type_Detection)    , intent(in)                :: Detection
     type(type_Spectrum)     , intent(in)                :: Spectrum
     type(type_Spectrum)     , intent(in)                :: Spectrum_bg
     type(type_Interferogram), intent(inout)             :: Interferogram
     type(type_Interferogram), intent(inout)             :: Interf_cont
     type(type_Interferogram), intent(in)                :: Interf_cont_Rpd
     type(type_Saf)          , intent(in)                :: Saf
     integer(kind=LONG)      , intent(in)                :: SUB_PN
     integer(kind=LONG)      , intent(in)                :: SB
     type(type_Interferogram), intent(out)               :: Interf_cont_vcc
     integer(kind=LONG)                                  :: NsDel
     type(type_Interferogram)                            :: Interf_cont_Cnv
     real(kind=DOUBLE)                                   :: Wn_First
     real(kind=DOUBLE)                                   :: Wn_Last
!
!    theoretical interferogram computation
     write(*,*) 'interf_cont_sim'
     write(*,*) 'Interferogram%N_Sample',Interferogram%N_Sample
     write(*,*) 'Spectrum%N_Sample     ',Spectrum%N_Sample
     write(*,*) 'Interferogram%Type    ',Interferogram%Type
!
!    Interferogram computation
     call sptoif( Spectrum, Interferogram )
!
!    laser emission computation
     if( (Interf_Target == 'EW') .and. (Interf_Param%Target_EW == 'LZ')&
          .and. Interf_Param%Laser_Intensity(SB) <= 0.d+00 ) then
        call interf_cont_laser_sim( Interf_Param, SB, Interferogram )
     end if
!
!    interferogram convolution
     Wn_First = dble( Spectrum%Ns_First-1 ) * Spectrum%dWn
     Wn_Last  = dble( Spectrum%Ns_Last-1  ) * Spectrum%dWn
     call interf_cont_cnv_ft( Saf, SUB_PN, Wn_First, Wn_Last,  &
                              Interferogram, Interf_cont_Cnv )
!
!    usefull continuous interferogram extraction
     NsDel = int(Interferogram%N_Sample/2)&
           - int(Interf_cont%N_Sample/2)
     if( NsDel < 0 ) then
        write(*,*) 'NsDel',NsDel
        write(*,*) 'interf_cont_sim Fatal Error'
        call exit(1)
     end if
     Interf_cont%Real_Part(1:Interf_cont%N_Sample) =        &
                         Interf_cont_Cnv%Real_Part        &
                         (NsDel+1:NsDel+Interf_cont%N_Sample)
     Interf_cont%FieldMeanAngle = Interf_cont_Cnv%FieldMeanAngle
!
!    Interferogram detection
     call interf_detection( Interf_Target,&
                            Detection,    &
                            Interf_Param, &
                            Spectrum_bg,  &
                            Spectrum,     &
                            Interf_cont   )
!
!    Detection gain
     call interf_gainappli( Gain, SUB_PN, Interf_cont )
!
!    Cube Corner speed variation introduction
     call interf_cont_dvcc( Interf_Param,    &
                            Interf_cont_Rpd, &
                            Interf_cont,     &
                            Interf_cont_vcc )
     call dalloc_Interferogram( Interf_cont_Cnv )
     return
!
   end subroutine interf_cont_sim
!
!

!> interf_cont_laser_sim -- Public
!!
!! * Purpose
!!
!!     Laser perfect theoretical interferogram simulation 
!!
!! * Description
!!
!!     The continuous theoretical interferogram is simulated as cosine depending of the laser 
!!     wave-number and the laser self apodisation function.
!!
!! * Inputs
!!
!!     - Interf_Param : type_Interf_Param / type for declaration and allocation of 
!!                      interferogram parameters
!!     - SB           : spectral band
!!
!! * Inputs/outputs
!!
!!     - Interferogram : type_Interferogram / type for declaration and allocation of 
!!                       interferogram
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : AD
!!

   subroutine interf_cont_laser_sim( Interf_Param,  &
                                     SB,            &
                                     Interferogram  )
   implicit none
     type(type_Interf_Param) , intent(in)                :: Interf_Param
     integer(kind=LONG)      , intent(in)                :: SB
     type(type_Interferogram), intent(inout)             :: Interferogram
     real(kind=DOUBLE)                                   :: Interf_Max
!
     Interf_Max = maxval( dabs(Interferogram%Real_Part) )
!
     Interferogram%Real_Part(1:Interferogram%N_Sample) =              &
             Interferogram%Real_Part(1:Interferogram%N_Sample)        &
             + ( 10.d+00 * Interf_Max                                 &
                * dcos( twoPi * Interf_Param%WnLaser_EW(SB)           &
                      * Interferogram%Opd(1:Interferogram%N_Sample) ) )
!
     return
!
   end subroutine interf_cont_laser_sim
!
!

!> interf_cont_rpd_sim -- Public
!!
!! * Purpose
!!
!!     Continuous laser RPD interferogram simulation
!!
!! * Description
!!
!!     The laser RPD interferogram is simulated as a continuous signal on which cube corner 
!!     speed variation is introduced.
!!     
!!     More precisely, the continuous RPD interferogram simulation requires the following steps:
!!       - The first step consists in the allocation of all required tables and a spline 
!!         interpolation in the computation discretisation;
!!       - Then, the laser RPD wavelength instabilities are simulated as a Gaussian random 
!!         function;
!!       - The continuous laser RPD interferogram is simulated as the real part of a cosine 
!!         depending of the laser wave-number and the laser self apodisation function;
!!       - At last, the cube corner speed variation is introduced.
!!
!! * Inputs
!!
!!     - Interf_Param : type_Interf_Param / type for declaration and allocation of interferogram
!!                      parameters
!!     - Saf_Rpd      : type_Saf / type for declaration and allocation of saf parameters
!!
!! * Inputs/outputs
!!
!!     - Interf_cont_Rpd : type_Interferogram / type for declaration and allocation of 
!!                         interferogram
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.4.2.1
!!

   subroutine interf_cont_rpd_sim( Interf_Param,   &
                                   Saf_Rpd,        &
                                   Interf_cont_Rpd )
   implicit none
     type(type_Interf_Param) , intent(in)                :: Interf_Param
     type(type_Saf)          , intent(in)                :: Saf_Rpd
     type(type_Interferogram), intent(inout)             :: Interf_cont_Rpd
     real(kind=DOUBLE)       , dimension(:), allocatable :: Opd
     real(kind=DOUBLE)                                   :: Opd_Pivot
     real(kind=DOUBLE)       , dimension(:), allocatable :: Saf_Mod
     real(kind=DOUBLE)       , dimension(:), allocatable :: Saf_Arg
     complex(kind=DOUBLE)    , dimension(:), allocatable :: Saf_cplx
     real(kind=DOUBLE)       , dimension(:), allocatable :: WnLaserRpd
     real(kind=DOUBLE)       , dimension(:), allocatable :: Interf
     real(kind=DOUBLE)                                   :: LambdaLaserRpd
     integer(kind=LONG)                                  :: Ns
     real(kind=DOUBLE)                                   :: XAvg
     real(kind=DOUBLE)                                   :: XStd
     real(kind=DOUBLE)                                   :: XMin
     real(kind=DOUBLE)                                   :: XMax
     integer(kind=LONG)                                  :: NsMin
     integer(kind=LONG)                                  :: NsMax
!   
!    allocation
     allocate( Opd(Interf_cont_Rpd%N_Sample) )
     allocate( Saf_Mod(Interf_cont_Rpd%N_Sample) )
     allocate( Saf_Arg(Interf_cont_Rpd%N_Sample) )
     allocate( Saf_cplx(Interf_cont_Rpd%N_Sample) )
     allocate( WnLaserRpd(Interf_cont_Rpd%N_Sample) )
!   
!    interpolation in the computation discretisation
     Opd_Pivot = Interf_cont_Rpd%Opd(Interf_cont_Rpd%N_Sample/2+1)
     Opd(1:Interf_cont_Rpd%N_Sample) =                               &
           Interf_cont_Rpd%Opd(1:Interf_cont_Rpd%N_Sample) - Opd_Pivot
     call intspline( Saf_Rpd%NsOpd           &
                    ,Saf_Rpd%Opd             &
                    ,Saf_Rpd%Mod(1,1,1)      &
                    ,Interf_cont_Rpd%N_Sample&
                    ,Opd                     &
                    ,Saf_Mod                 )
     call intspline( Saf_Rpd%NsOpd           &
                    ,Saf_Rpd%Opd             &
                    ,Saf_Rpd%Arg(1,1,1)      &
                    ,Interf_cont_Rpd%N_Sample&
                    ,Opd                     &
                    ,Saf_Arg                 )
     call modargtocplx( Saf_cplx, Saf_Mod, Saf_Arg, &
                        Interf_cont_Rpd%N_Sample    )
!
!    deallocation
     deallocate( Opd )
!
     write(*,*) 'interf_cont_rpd_sim'
     write(*,*) 'Interf_cont_Rpd%N_Sample',Interf_cont_Rpd%N_Sample
     write(*,*) 'Interf_Param%Rpd_Type',Interf_Param%Rpd_Type
!
!    laser wavelength instabilities
     do Ns = 1, Interf_cont_Rpd%N_Sample
        LambdaLaserRpd = Interf_Param%LambdaLaserRpd &
                       * (1.d+00 + Interf_Param%LambdaLaserOffset         &
                                 + Interf_Param%LambdaLaserJitter*grnd(0) )
        WnLaserRpd(Ns) = 1.d+00 / LambdaLaserRpd
     end do
     call statistics( Interf_cont_Rpd%N_Sample, &
                      WnLaserRpd, &
                      XAvg,       &
                      XStd,       &
                      XMin,       &
                      XMax,       &
                      NsMin,      &
                      NsMax       )
     write(*,'(60a)') &
            'WnLaserRpd_Vect : XAvg, XStd, XMin, XMax, NsMin, NsMax'
     write(*,'(4f20.6,2i10)') &
                               XAvg, XStd, XMin, XMax, NsMin, NsMax
     write(*,*) 'compute cosine'
     Interf_cont_Rpd%Real_Part(1:Interf_cont_Rpd%N_Sample) =          &
        dreal( dcos( twoPi*WnLaserRpd(1:Interf_cont_Rpd%N_Sample)     &
                    *Interf_cont_Rpd%Opd(1:Interf_cont_Rpd%N_Sample) )&
              * Saf_cplx(1:Interf_cont_Rpd%N_Sample) )
     if( Interf_Param%Rpd_Type == 'RI' ) then
        write(*,*) 'compute sine'
        Interf_cont_Rpd%Imag_Part(1:Interf_cont_Rpd%N_Sample) =          &
           dreal( dsin( twoPi*WnLaserRpd(1:Interf_cont_Rpd%N_Sample)     &
                       *Interf_cont_Rpd%Opd(1:Interf_cont_Rpd%N_Sample) )&
                 * Saf_cplx(1:Interf_cont_Rpd%N_Sample) )
     end if
     Interf_cont_Rpd%FieldMeanAngle = Saf_Rpd%FieldMeanAngle(1,1)
!
!    Cube Corner speed variation introduction
     allocate( Opd(Interf_cont_Rpd%N_Sample) )
     allocate( Interf(Interf_cont_Rpd%N_Sample) )
     Opd(1:Interf_cont_Rpd%N_Sample) = &
               Interf_cont_Rpd%Opd(1:Interf_cont_Rpd%N_Sample)
     Interf(1:Interf_cont_Rpd%N_Sample) = &
               Interf_cont_Rpd%Real_Part(1:Interf_cont_Rpd%N_Sample)
!
     call vcc_variation( Interf_Param,   &
                         Interf_cont_Rpd )
     call intspline( Interf_cont_Rpd%N_Sample, &
                     Opd,                      &
                     Interf,                   &
                     Interf_cont_Rpd%N_Sample, &
                     Interf_cont_Rpd%Opd,      &
                     Interf_cont_Rpd%Real_Part )
     if( Interf_Param%Rpd_Type == 'RI' ) then
        Interf(1:Interf_cont_Rpd%N_Sample) = &
               Interf_cont_Rpd%Imag_Part(1:Interf_cont_Rpd%N_Sample)
        call intspline( Interf_cont_Rpd%N_Sample, &
                        Opd,                      &
                        Interf,                   &
                        Interf_cont_Rpd%N_Sample, &
                        Interf_cont_Rpd%Opd,      &
                        Interf_cont_Rpd%Imag_Part )
     end if
!
!    deallocation
     deallocate( Opd )
     deallocate( Interf )
     deallocate( Saf_Mod )
     deallocate( Saf_Arg )
     deallocate( Saf_cplx )
     deallocate( WnLaserRpd )
     return
!
   end subroutine interf_cont_rpd_sim
!
!

!> vcc_variation -- Public
!!
!! * Purpose
!!
!!     Computation of corner cube speed variation 
!!
!! * Description
!!
!!     The cube corner speed variation is simulated as a sine function defined by its amplitude,
!!     its frequency and its phase. It takes also into account the time of the central fringe of
!!     the interferogram.
!!
!! * Inputs
!!
!!     - Interf_Param : type_Interf_Param / type for declaration and allocation of interferogram
!!                      parameters
!!
!! * Inputs/outputs
!!
!!     - Interferogram : type_Interferogram / type for declaration and allocation of 
!!                       interferogram
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.8.4
!!

   subroutine vcc_variation( Interf_Param, &
                             Interferogram )
   implicit none
     type(type_Interf_Param) , intent(in)             :: Interf_Param
     type(type_Interferogram), intent(inout)          :: Interferogram
     integer(kind=LONG)                               :: Ns
     integer(kind=LONG)                               :: Nf
     real(kind=DOUBLE)                                :: dVCCAvg
     real(kind=DOUBLE)                                :: dVCCStd
     real(kind=DOUBLE)                                :: Time_Zpd
     real(kind=DOUBLE)                                :: dVCC
     real(kind=DOUBLE)                                :: VCCCos
     real(kind=DOUBLE)                                :: OpdMax
     integer(kind=LONG)                               :: Seg
     integer(kind=LONG)                               :: n1
     integer(kind=LONG)                               :: n2
     integer(kind=LONG)  ,dimension(:),allocatable    :: Ns_Start
     integer(kind=LONG)  ,dimension(:),allocatable    :: Ns_Stop
!
!    computation of cube corner speed variation 
     if( Interf_Param%VCC_Nb_Freq /= 0 ) then
!
!       cosine variation
        write(*,*) ' Corner cube speed cosine variation'
        dVCCAvg = 0.d+00
        dVCCStd = 0.d+00
        Time_Zpd = Interferogram%Time(int(Interferogram%N_Sample/2)+1)
        do Ns = 1, Interferogram%N_Sample
           dVCC = 0.d+00
           do Nf = 1, Interf_Param%VCC_Nb_Freq
              VCCCos  = Interf_Param%VCC * Interf_Param%VCC_Amp_muvib(Nf) &
                      * dsin( (twoPi * Interf_Param%VCC_Freq_muvib(Nf)    &
                               *(Interferogram%Time(Ns)-Time_Zpd))    &
                             + Interf_Param%VCC_Phase_muvib(Nf) )
              dVCC    = dVCC + VCCCos
           end do
           dVCCAvg = dVCCAvg + dVCC
           dVCCStd = dVCCStd + dVCC**2
           Interferogram%Opd(Ns) = ( Interferogram%Time(Ns)    &
                                    *2.d+00*Interf_Param%VCC ) &
                                 + ( Interferogram%dTime       &
                                    *2.d+00*dVCC )
        end do
        dVCCAvg = dVCCAvg / dble(Interferogram%N_Sample)
        dVCCStd = dsqrt( (dVCCStd/dble(Interferogram%N_Sample)) - dVCCAvg**2 )
        write(*,'(a,2f12.9,a)') ' Cube Corner speed variation Avg Std :' &
                           , dVCCAvg, dVCCStd,' m/s'
     else if( Interf_Param%VCC_Nb_Seg /= 0 ) then
!
!       linear variation per segment
        write(*,*) 'Corner cube speed variation linear per segment'
        allocate(  Ns_Start(Interf_Param%VCC_Nb_Seg) )
        allocate(  Ns_Stop(Interf_Param%VCC_Nb_Seg) )
        dVCCAvg = 0.d+00
        dVCCStd = 0.d+00
        OpdMax = 0.d+00
        do Seg = 1, Interf_Param%VCC_Nb_Seg
           n1 = 1
           n2 = Interferogram%N_Sample
           OpdMax = OpdMax &
                  + 2*Interferogram%OpdMax*Interf_Param%VCC_Course(Seg)
           call dichotomd( OpdMax, Interferogram%Opd(n1:n2),n1,n2 )
           if( Seg == 1 ) then
              Ns_Start(Seg) = 1
              Ns_Stop(Seg)  = n2
           else if( (Seg > 1) .and. (Seg == Interf_Param%VCC_Nb_Seg) ) then
              Ns_Start(Seg) = Ns_Stop(Seg-1) + 1
              Ns_Stop(Seg)  = Interferogram%N_Sample
           else
              Ns_Start(Seg) = Ns_Stop(Seg-1) + 1
              Ns_Stop(Seg)  = n2
           end if
           write(*,*) 'Ns Start Stop',Ns_Start(Seg), Ns_Stop(Seg)
           do Ns = Ns_Start(Seg), Ns_Stop(Seg)
              dVCC = Interf_Param%VCC_A0(Seg) &
                   + (Interf_Param%VCC_A1(Seg) * Interferogram%Opd(Ns))
              dVCCAvg = dVCCAvg + dVCC
              dVCCStd = dVCCStd + dVCC**2
              Interferogram%Opd(Ns) = 2.d+00 * ( Interf_Param%VCC+dVCC )&
                                    * Interferogram%Time(Ns)
              if( Ns == Ns_Start(Seg) ) then
                 write(*,*) 'dVCC Start    ',Seg, dVCC, Interf_Param%VCC+dVCC
              else if( Ns == Ns_Stop(Seg) ) then
                 write(*,*) 'dVCC Stop     ',Seg, dVCC, Interf_Param%VCC+dVCC
              end if
           end do
        end do
        deallocate( Ns_Start )
        deallocate( Ns_Stop )
        dVCCAvg = dVCCAvg / dble(Interferogram%N_Sample)
        dVCCStd = dsqrt( (dVCCStd/dble(Interferogram%N_Sample)) - dVCCAvg**2 )
        write(*,'(a,2f12.9,a)') ' Cube Corner speed variation Avg Std :' &
                           , dVCCAvg, dVCCStd,' m/s'
     else
        write(*,*) 'No corner cube speed variation'
     end if
     return
!
   end subroutine vcc_variation
!
!

!> interf_cont_dvcc -- Public
!!
!! * Purpose
!!
!!     Interferogram computation taking into account corner cube speed variation 
!!
!! * Description
!!
!!     The interf_cont_dvcc subroutine computes the interferogram in taking into account corner
!!     cube speed variation: after required tables allocation, a splin interpolation is done 
!!     considering the interferogram type (R/RI/C/Im...).
!!
!! * Inputs
!!
!!     - Interf_Param    : type_Interf_Param / type for declaration and allocation of 
!!                         interferogram parameters
!!     - Interf_cont_Rpd : type_Interferogram / type for declaration and allocation of 
!!                         interferogram
!!     - Interf_cont     : type_Interferogram / type for declaration and allocation of 
!!                         interferogram
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Interf_cont_vcc : type_Interferogram / type for declaration and allocation of 
!!                            interferogram
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : AD
!!

   subroutine interf_cont_dvcc( Interf_Param,   &
                                Interf_cont_Rpd,&
                                Interf_cont,    &
                                Interf_cont_vcc )
   implicit none
     type(type_Interf_Param) , intent(in)             :: Interf_Param
     type(type_Interferogram), intent(in)             :: Interf_cont_Rpd
     type(type_Interferogram), intent(in)             :: Interf_cont
     type(type_Interferogram), intent(out)            :: Interf_cont_vcc
     integer(kind=LONG)                               :: N_Calc
     real(kind=DOUBLE)    , dimension(:), allocatable :: Input_1
     real(kind=DOUBLE)    , dimension(:), allocatable :: Input_2
     real(kind=DOUBLE)    , dimension(:), allocatable :: Output_1
     real(kind=DOUBLE)    , dimension(:), allocatable :: Output_2
!
!    allocation
     if( Interf_cont_Rpd%N_Sample /=       &
         Interf_cont%N_Sample         ) then
        write(*,*) 'N_Sample Error',Interf_cont_Rpd%N_Sample,&
                                    Interf_cont%N_Sample
        write(*,*) 'interf_cont_dvcc Fatal Error'
        call exit(1)
     end if
     Interf_cont_vcc%N_Sample = Interf_cont%N_Sample
     Interf_cont_vcc%Type     = Interf_cont%Type
     Interf_cont_vcc%FieldMeanAngle = Interf_cont%FieldMeanAngle
     write(*,*) 'interf_cont_dvcc'
     write(*,*) 'Interf_cont_vcc%N_Sample   ',Interf_cont_vcc%N_Sample
     write(*,*) 'Interf_cont_Rpd%N_Sample      ',Interf_cont_Rpd%N_Sample
     write(*,*) 'Interf_Param%Rpd_Samp_OSFactor',Interf_Param%Rpd_Samp_OSFactor
     call alloc_Interferogram( Interf_cont_vcc )
     Interf_cont_vcc = Interf_cont
     Interf_cont_vcc%Opd(1:Interf_cont_vcc%N_Sample) =   &
                Interf_cont_Rpd%Opd(1:Interf_cont_Rpd%N_Sample )
     Interf_cont_vcc%Time(1:Interf_cont_vcc%N_Sample) =   &
                Interf_cont_Rpd%Time(1:Interf_cont_Rpd%N_Sample )
!
!    speed variation introduction
     allocate( Input_1(Interf_cont%N_Sample) )
     allocate( Input_2(Interf_cont%N_Sample) )
     allocate( Output_1(Interf_cont_vcc%N_Sample) )
     allocate( Output_2(Interf_cont_vcc%N_Sample) )
     if( Interf_cont%Type(1:len_trim(Interf_cont%Type))      == 'C'  ) then
        N_Calc = 2
        Input_1(1:Interf_cont%N_Sample) =                            &
            dreal( Interf_cont%Complex(1:Interf_cont%N_Sample) )
        Input_2(1:Interf_cont%N_Sample) =                            &
            dimag( Interf_cont%Complex(1:Interf_cont%N_Sample) )
     else if( Interf_cont%Type(1:len_trim(Interf_cont%Type)) == 'RI' ) then
        N_Calc = 2
        Input_1(1:Interf_cont%N_Sample) =                            &
            Interf_cont%Real_Part(1:Interf_cont%N_Sample) 
        Input_2(1:Interf_cont%N_Sample) =                            &
            Interf_cont%Imag_Part(1:Interf_cont%N_Sample) 
     else if( Interf_cont%Type(1:len_trim(Interf_cont%Type)) == 'MA' ) then
        N_Calc = 2
        Input_1(1:Interf_cont%N_Sample) =                            &
            Interf_cont%Modulus(1:Interf_cont%N_Sample) 
        Input_2(1:Interf_cont%N_Sample) =                            &
            Interf_cont%Argument(1:Interf_cont%N_Sample) 
     else if( Interf_cont%Type(1:len_trim(Interf_cont%Type)) == 'R'  ) then
        N_Calc = 1
        Input_1(1:Interf_cont%N_Sample) =                            &
            Interf_cont%Real_Part(1:Interf_cont%N_Sample) 
     else if( Interf_cont%Type(1:len_trim(Interf_cont%Type)) == 'I'  ) then
        N_Calc = 1
        Input_1(1:Interf_cont%N_Sample) =                            &
            Interf_cont%Imag_Part(1:Interf_cont%N_Sample) 
     else if( Interf_cont%Type(1:len_trim(Interf_cont%Type)) == 'M'  ) then
        N_Calc = 1
        Input_1(1:Interf_cont%N_Sample) =                            &
            Interf_cont%Modulus(1:Interf_cont%N_Sample) 
     else if( Interf_cont%Type(1:len_trim(Interf_cont%Type)) == 'A'  ) then
        N_Calc = 1
        Input_1(1:Interf_cont%N_Sample) =                            &
            Interf_cont%Argument(1:Interf_cont%N_Sample) 
     end if
     call intspline( Interf_cont%N_Sample,    &
                     Interf_cont%Opd,         &
                     Input_1,                 &
                     Interf_cont_vcc%N_Sample,&
                     Interf_cont_vcc%Opd,     &
                     Output_1                 )
     if( N_Calc == 2 ) &
     call intspline( Interf_cont%N_Sample,    &
                     Interf_cont%Opd,         &
                     Input_2,                 &
                     Interf_cont_vcc%N_Sample,&
                     Interf_cont_vcc%Opd,     &
                     Output_2                 )
     if( Interf_cont%Type(1:len_trim(Interf_cont%Type))      == 'C'  ) then
        Interf_cont_vcc%Complex(1:Interf_cont_vcc%N_Sample) =   &
                  dcmplx( Output_1(1:Interf_cont_vcc%N_Sample), &
                          Output_2(1:Interf_cont_vcc%N_Sample) )
     else if( Interf_cont%Type(1:len_trim(Interf_cont%Type)) == 'RI' ) then
        Interf_cont_vcc%Real_Part(1:Interf_cont_vcc%N_Sample) = &
                            Output_1(1:Interf_cont_vcc%N_Sample)
        Interf_cont_vcc%Imag_Part(1:Interf_cont_vcc%N_Sample) = &
                            Output_2(1:Interf_cont_vcc%N_Sample)
     else if( Interf_cont%Type(1:len_trim(Interf_cont%Type)) == 'MA' ) then
        Interf_cont_vcc%Modulus(1:Interf_cont_vcc%N_Sample)  =  &
                          Output_1(1:Interf_cont_vcc%N_Sample)
        Interf_cont_vcc%Argument(1:Interf_cont_vcc%N_Sample) =  &
                           Output_2(1:Interf_cont_vcc%N_Sample)
     else if( Interf_cont%Type(1:len_trim(Interf_cont%Type)) == 'R'  ) then
        Interf_cont_vcc%Real_Part(1:Interf_cont_vcc%N_Sample) = &
                            Output_1(1:Interf_cont_vcc%N_Sample)
     else if( Interf_cont%Type(1:len_trim(Interf_cont%Type)) == 'I'  ) then
        Interf_cont_vcc%Imag_Part(1:Interf_cont_vcc%N_Sample) = &
                            Output_1(1:Interf_cont_vcc%N_Sample)
     else if( Interf_cont%Type(1:len_trim(Interf_cont%Type)) == 'M'  ) then
        Interf_cont_vcc%Modulus(1:Interf_cont_vcc%N_Sample)  =  &
                          Output_1(1:Interf_cont_vcc%N_Sample)
     else if( Interf_cont%Type(1:len_trim(Interf_cont%Type)) == 'A'  ) then
        Interf_cont_vcc%Argument(1:Interf_cont_vcc%N_Sample) =  &
                           Output_1(1:Interf_cont_vcc%N_Sample)
     end if
     deallocate( Input_1 )
     deallocate( Input_2 )
     deallocate( Output_1 )
     deallocate( Output_2 )
     return
!
   end subroutine interf_cont_dvcc
!
!

!> interf_cont_cnv_ft -- Public
!!
!! * Purpose
!!
!!     Interferogram convolved by the instrument functions 
!!
!! * Description
!!
!!     The self apodisation functions are applied on the laser RPD and targets continuous 
!!     interferograms to obtain interferograms convolved by the instrument functions. In 
!!     practice, the convolution is done in the Fourier space for a few wave numbers for 
!!     which the SAF has been calculated, then the true convolved interferogram is obtained 
!!     by linear interpolation in the spectrum space.
!!
!!     More precisely, the interferogram convolution requires the following steps:
!!      - The first steps consist in the required tables allocation and the definition of the 
!!        spectrum limits, the limits in the TF space and the convolution limits.
!!      - Then, a loop is done on tabulated self apodisation function:  
!!              - the complex saf is interpolated on the continuous interferogram samples,
!!              - the continuous interferogram is multiplied by the interpolated saf to obtain 
!!                the interferogram convolved by the SAF,
!!              - thanks to an inverse fourier transform of the complex SAF interferogram, the 
!!                spectrum is computed
!!              - The complex spectral samples between two different wave numbers are calculated
!!                by linear interpolation between two spectra corresponding to the tabulated 
!!                self-apodisation functions.
!!      - Thanks to an fourier transform, we come back into the interferogram space: indeed, 
!!        the convolved interferogram is given by the real part of the direct fourier transform
!!        of the previous spectrum.
!!
!! * Inputs
!!
!!     - Saf           : type_Saf / type for declaration and allocation of saf parameters
!!     - SUB_PN        : sub-pixel
!!     - Wn_First      : first wave number of complex spectral samples 
!!     - Wn_Last       : last wave number of complex spectral samples 
!!     - Interferogram : type_Interferogram / type for declaration and allocation of 
!!                       interferogram
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Interf_cnv  : type_Interferogram / type for declaration and allocation of interferogram
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : AD
!!

   subroutine interf_cont_cnv_ft( Saf,           &
                                  SUB_PN,        &
                                  Wn_First,      &
                                  Wn_Last,       &
                                  Interferogram, &
                                  Interf_Cnv     )
   implicit none
     type(type_Saf)          , intent(in)                :: Saf
     integer(kind=LONG)      , intent(in)                :: SUB_PN
     real(kind=DOUBLE)       , intent(in)                :: Wn_First
     real(kind=DOUBLE)       , intent(in)                :: Wn_Last
     type(type_Interferogram), intent(in)                :: Interferogram
     type(type_Interferogram), intent(out)               :: Interf_Cnv
     type(type_Spectrum)                                 :: Spectrum_1
     type(type_Spectrum)                                 :: Spectrum_2
     type(type_Spectrum)                                 :: Spectrum
     integer(kind=LONG)                                  :: Ns_First
     integer(kind=LONG)                                  :: Ns_Last
     integer(kind=LONG)                                  :: n1
     integer(kind=LONG)                                  :: n2
     integer(kind=LONG)                                  :: NSafStart
     integer(kind=LONG)                                  :: NSafEnd
     real(kind=DOUBLE)       , dimension(:), allocatable :: Opd
     real(kind=DOUBLE)                                   :: Opd_Pivot
     integer(kind=LONG)                                  :: NSaf
     real(kind=DOUBLE)                                   :: WnSafStart
     real(kind=DOUBLE)                                   :: WnSafEnd
     real(kind=DOUBLE)       , dimension(:), allocatable :: Saf_Mod
     real(kind=DOUBLE)       , dimension(:), allocatable :: Saf_Arg
     complex(kind=DOUBLE)    , dimension(:), allocatable :: Saf_cplx
     type(type_Interferogram)                            :: Interf_Saf
     integer(kind=LONG)                                  :: Ns
     integer(kind=LONG)                                  :: NsStart
     integer(kind=LONG)                                  :: NsEnd
     integer(kind=LONG)                                  :: n
     real(kind=DOUBLE)                                   :: alpha
     real(kind=DOUBLE)                                   :: beta
!
!    spectrum limits
     Spectrum%dWn        = 1.d+00 / ( 2.d+00*Interferogram%OpdMax )
     Spectrum%Ns_First   = idnint( Wn_First/Spectrum%dWn ) + 1
     Spectrum%Ns_Last    = idnint( Wn_Last /Spectrum%dWn ) + 1
     Spectrum%N_Sample   = Spectrum%Ns_Last-Spectrum%Ns_First+1
     Spectrum%Type       = 'C'
     Spectrum%Wn_First   = dble( Spectrum%Ns_First-1) * Spectrum%dWn
     Spectrum%Wn_Last    = dble( Spectrum%Ns_Last-1)  * Spectrum%dWn
     Spectrum%WnMax      = Spectrum%Wn_Last
     call alloc_Spectrum( Spectrum )
     call Spectrum_Basis( Spectrum )
     Spectrum_1%N_Sample = Spectrum%Ns_Last-Spectrum%Ns_First+1
     Spectrum_2%N_Sample = Spectrum%Ns_Last-Spectrum%Ns_First+1
     Spectrum_1%Type     = 'C'
     Spectrum_2%Type     = 'C'
     Spectrum_1%dWn      = Spectrum%dWn
     Spectrum_2%dWn      = Spectrum%dWn
     Spectrum_1%Ns_First = Spectrum%Ns_First
     Spectrum_2%Ns_First = Spectrum%Ns_First
     Spectrum_1%Ns_Last  = Spectrum%Ns_Last
     Spectrum_2%Ns_Last  = Spectrum%Ns_Last
     Spectrum_1%Wn_First = Spectrum%Wn_First
     Spectrum_2%Wn_First = Spectrum%Wn_First
     Spectrum_1%Wn_Last  = Spectrum%Wn_Last
     Spectrum_2%Wn_Last  = Spectrum%Wn_Last
     Spectrum_1%WnMax    = Spectrum%WnMax
     Spectrum_2%WnMax    = Spectrum%WnMax
     call alloc_Spectrum( Spectrum_1 )
     call alloc_Spectrum( Spectrum_2 )
!
!    limits in the TF space
     Ns_First = Spectrum%Ns_First + int(Interferogram%N_Sample/2)
     Ns_Last  = Spectrum%Ns_Last  + int(Interferogram%N_Sample/2)
!
!    convolution limits
     n1 = 1
     n2 = Saf%NsWn0
     call dichotomd( Wn_First, Saf%Wn0, n1, n2 ) 
     NSafStart = n1
     n1 = 1
     n2 = Saf%NsWn0
     call dichotomd( Wn_Last, Saf%Wn0, n1, n2 )
     NSafEnd = n2
     Opd_Pivot = Interferogram%Opd(Interferogram%N_Sample/2+1)
     allocate( Opd(Interferogram%N_Sample) )
     Opd(1:Interferogram%N_Sample) =                             &
           Interferogram%Opd(1:Interferogram%N_Sample) - Opd_Pivot
     allocate( Saf_Mod(Interferogram%N_Sample) )
     allocate( Saf_Arg(Interferogram%N_Sample) )
     allocate( Saf_cplx(Interferogram%N_Sample) )
     Interf_Saf%N_Sample = Interferogram%N_Sample
     Interf_Saf%Type = 'C'
     call alloc_Interferogram( Interf_Saf )
     Interf_Saf%dOpd = Interferogram%dOpd
!
!    tabulated self apodisation loop
     WnSafEnd = Saf%Wn0( NSafStart )
     do NSaf = NSafStart, NSafEnd
!
        WnSafStart = WnSafEnd
        WnSafEnd   = Saf%Wn0( NSaf )
!   
!       interpolation in the computation discretisation
        call intspline_open( Saf%NsOpd             &
                            ,Saf%Opd               &
                            ,Saf%Mod(1,NSaf,SUB_PN)&
                            ,Interferogram%N_Sample&
                            ,Opd                   &
                            ,Saf_Mod               )
        call intspline_open( Saf%NsOpd             &
                            ,Saf%Opd               &
                            ,Saf%Arg(1,NSaf,SUB_PN)&
                            ,Interferogram%N_Sample&
                            ,Opd                   &
                            ,Saf_Arg               )
        call modargtocplx( Saf_cplx, Saf_Mod, Saf_Arg, &
                           Interferogram%N_Sample      )
        Interf_Saf%Complex(1:Interferogram%N_Sample) =            &
                  dconjg(Saf_cplx(1:Interferogram%N_Sample))      &
                * Interferogram%Real_Part(1:Interferogram%N_Sample)
!
!       interferogram conversion into the spectrum space
        call iftosp_open( Interf_Saf, Spectrum_2 )
!
!       interpolation between WnSafStart and WnSafEnd
        if( NSaf .ne. NSafStart ) then
           NsStart = Ns_First
           NsEnd   = Ns_Last
           if( WnSafStart .gt. Wn_First ) &
              NsStart = Ns_First + idnint( (WnSafStart-Wn_First)/Spectrum%dWn )
           if( WnSafEnd .lt. Wn_Last )    &
              NsEnd   = Ns_First + idnint( (WnSafEnd  -Wn_First)/Spectrum%dWn )
           do Ns = NsStart, NsEnd
              n = Ns - Ns_First + 1
              beta  = (Spectrum%Wn(n)-WnSafStart)/(WnSafEnd-WnSafStart)
              alpha = 1.d+00-beta
              Spectrum%Complex(n) = ( alpha*Spectrum_1%Complex(n) &
                                     + beta*Spectrum_2%Complex(n) )
           end do
        end if
!
!       transfert sp2 into sp1
        Spectrum_1%Complex(1:Spectrum%N_Sample) =             &
                        Spectrum_2%Complex(1:Spectrum%N_Sample) 
     end do ! end self apodisation loop
!
!    convolved interferogram initialisation
     call Interf_Header_Transfer( Interferogram, Interf_Cnv )
     Interf_Cnv%FieldMeanAngle =                               &
                 sum( Saf%FieldMeanAngle(1:Saf%NsWn0,SUB_PN) ) &
               / Saf%NsWn0
     write(*,*) 'Interf_Cnv Type          ',Interf_Cnv%Type
     write(*,*) 'Interf_Cnv N_Sample      ',Interf_Cnv%N_Sample
     write(*,*) 'Interf_Cnv dOpd          ',Interf_Cnv%dOpd
     write(*,*) 'Interf_Cnv dTime         ',Interf_Cnv%dTime
     write(*,*) 'Interf_Cnv OpdMax        ',Interf_Cnv%OpdMax
     write(*,*) 'Interf_Cnv TimeMax       ',Interf_Cnv%TimeMax
     write(*,*) 'Interf_Cnv N_Top         ',Interf_Cnv%N_Top
     write(*,*) 'Interf_Cnv FieldMeanAngle',Interf_Cnv%FieldMeanAngle
!
     write(*,*) 'Spectrum Type    ',Spectrum%Type
     write(*,*) 'Spectrum N_Sample',Spectrum%N_Sample
     write(*,*) 'Spectrum Ns_First',Spectrum%Ns_First
     write(*,*) 'Spectrum Ns_Last ',Spectrum%Ns_Last
     write(*,*) 'Spectrum Wn_First',Spectrum%Wn_First
     write(*,*) 'Spectrum Wn_Last ',Spectrum%Wn_Last
     write(*,*) 'Spectrum WnMax   ',Spectrum%WnMax
     write(*,*) 'Spectrum dWn     ',Spectrum%dWn
     write(*,*) 'Spectrum Wn(1)   ',Spectrum%Wn(1)
     write(*,*) 'Spectrum Wn(N)   ',Spectrum%Wn(Spectrum%N_Sample)
     write(*,*) 'Spectrum Cplx(1) ',Spectrum%Complex(1)
     write(*,*) 'Spectrum Cplx(N) ',Spectrum%Complex(Spectrum%N_Sample)
!
!    back into the interferogram space
     call sptoif( Spectrum, Interf_Cnv )
!
!    de allocation
     deallocate( Opd )
     deallocate( Saf_cplx )
     deallocate( Saf_Mod )
     deallocate( Saf_Arg )
     call dalloc_Interferogram( Interf_Saf )
     call dalloc_Spectrum( Spectrum_1 )
     call dalloc_Spectrum( Spectrum_2 )
     call dalloc_Spectrum( Spectrum )
     return
!
   end subroutine interf_cont_cnv_ft
!
!
end module interf_continu_module
