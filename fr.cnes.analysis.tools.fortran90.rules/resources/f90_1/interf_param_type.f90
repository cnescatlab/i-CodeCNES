!!* interf_param_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: october 2009
!!*           Version: $Revision: 1.8 $
!!* Last modification: $Date: 2011-11-02 14:57:53 $
!!
!> type_interf_param -- Module
!!
!! * Purpose
!!
!!   Module for type interf parameters declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the interf parameters type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Interf_Param   : type for declaration and allocation of Interf_Param
!!     * alloc_Interf_Param  : type_Interf_Param allocation
!!     * dalloc_Interf_Param : type_Interf_Param deallocation
!!
!! * References
!!

module interf_param_type
   use precision_type
   use error_type
   use modinouttools
!
   implicit none
!
!
   public :: type_Interf_Param,  &
             alloc_Interf_Param, &
             dalloc_Interf_Param  
!
   type :: type_Interf_Param
     character(len=500)                               :: filename !< interferogram parameters characterisation file name
     character(len=2)                                 :: Type !< Interferogram type (R/RI/C/etc...)
     character(len=2)                                 :: Rpd_Type !< Rpd type (RI/R)
     character(len=5)                                 :: Sampling_Mode !< sampling mode (CLOCK/LZRPD)
     character(len=8)                                 :: Integration_Mode !< Integration mode (Time_Cst/Opd_Cst/None)
     character(len=6)                                 :: option_cnv !< convolution option (GAUSS/BOXCAR)
     real(kind=DOUBLE)                                :: LambdaLaserRpd !< Rpd laser wavelength (m)
     real(kind=DOUBLE)                                :: LambdaLaserOffset !< Offset laser wavelength (relative)
     real(kind=DOUBLE)                                :: LambdaLaserJitter !< Jitter laser wavelength (relative)
     real(kind=DOUBLE)                                :: WnLaserRpd !< Rpd laser wavenumber (m-1)
     real(kind=DOUBLE)                                :: VCC !< cornercube velocity (m.s-1)
     real(kind=DOUBLE)                                :: Ratio_Opd !< OPD marging : OpdMax*Ratio_Opd 
     real(kind=DOUBLE)                                :: OpdMax !< maximum optical path difference (m)
     integer(kind=LONG)                               :: OSFactor !< oversampling scaling factor
     integer(kind=LONG)                               :: Phase_Poly_Deg !< polynom degree for phase interpolation - Phase_Poly_Coeff(0:Phase_Poly_Deg)
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: Phase_Poly_Coeff !< polynom coefficients for phase interpolation 
     integer(kind=LONG)                               :: VCC_Nb_Freq !< frequencies number
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: VCC_Amp_muvib !< VCC micro vibration amplitude (m) - Amp_muvib(1:Nb_Freq)
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: VCC_Freq_muvib !< VCC micro vibration frequencies (Hz) - Freq_muvib(1:Nb_Freq) 
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: VCC_Phase_muvib !< VCC micro vibration phase at Zpd (degree) - Phase_muvib(1:Nb_Freq)
     integer(kind=LONG)                               :: VCC_Nb_Seg !< Course segmentation number
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: VCC_Course !< Course fraction covered  - Course(1:Nb_Seg)
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: VCC_A0 !< VCC linear offset (m/s) - A0(1:Nb_Seg)
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: VCC_A1 !< VCC linear slope (m/s) - A1(1:Nb_Seg)
     integer(kind=LONG)                               :: Rpd_Samp_OSFactor !< oversampling scaling factor for Rpd sampling
     real(kind=DOUBLE)                                :: dT_Rpd_Samp !< Rpd time sampling 
     real(kind=DOUBLE)                                :: dT_Mes_Samp !< Measurement time sampling
     real(kind=DOUBLE)                                :: Noise_Rpd_Std !< Noise standart deviation
     real(kind=DOUBLE)                                :: Rpd_Delay !< Rpd delay
     real(kind=DOUBLE)                                :: Rpd_Samp_Jitter !< Rpd sampling jitter
     real(kind=DOUBLE)                                :: Mes_Delay !< Measurement delay
     real(kind=DOUBLE)                                :: Mes_Samp_Jitter !< Measurement sampling jitter
     real(kind=DOUBLE)                                :: dT_Integ !< Integration time sampling
     real(kind=DOUBLE)                                :: dT_Integ_Jitter !< Jitter on integration time sampling
     real(kind=DOUBLE)                                :: dOpd_Integ !< Integration OPD sampling
     character(len=2)                                 :: Target_CS !< flag for cold space choice 
     character(len=2)                                 :: Target_BB !< flag for blackbody choice 
     character(len=2)                                 :: Target_EW !< flag for Earth-View choice 
     real(kind=DOUBLE)                                :: T_BG1_modul !< modulated background temperature (K)
     real(kind=DOUBLE)                                :: T_BG2_modul !< modulated background temperature (K)
     real(kind=DOUBLE)                                :: T_BG_direct !< direct background temperature (K)
     real(kind=DOUBLE)                                :: T_CS !< cold space temperature (K)
     real(kind=DOUBLE)                                :: T_BB !< blackbody temperature (K)
     real(kind=DOUBLE)                                :: T_EW !< Earth-View temperature (K)
     real(kind=DOUBLE)                                :: TempRef !< reference temperature (K)
     real(kind=DOUBLE)                                :: dWn !< wavenumber sampling
     integer(kind=LONG)                               :: Nband !< spectral band number
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: Wn_First !< first wavenumber - Wn_First(1:Nband)
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: Wn_Last !< last wavenumber - Wn_Last(1:Nband)
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: Wn_Usefull_First !< first usefull wavenumber - Wn_Usefull_First(1:Nband)
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: Wn_Usefull_Last !< last usefull wavenumber - Wn_Usefull_Last(1:Nband)
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: WnLaser_EW !< Wavenumber ILS WnLaser_EW(1:Nband)
     real(kind=DOUBLE)  ,dimension(:)    ,allocatable :: Laser_Intensity !< Laser Intensity
     real(kind=DOUBLE)  ,dimension(:)    ,allocatable :: Flat_Intensity !< Flat spectrum intensity
   end type type_Interf_Param
!
   contains
!
!
   subroutine alloc_Interf_Param( Interf_Param )
   implicit none
     type(type_Interf_Param), intent(inout)      :: Interf_Param
     integer(kind=LONG)                          :: ErrCode
!
     allocate( Interf_Param%Phase_Poly_Coeff(0:Interf_Param%Phase_Poly_Deg),&
                                                               stat=ErrCode )
     allocate( Interf_Param%VCC_Amp_muvib(Interf_Param%VCC_Nb_Freq),        &
                                                               stat=ErrCode )
     allocate( Interf_Param%VCC_Freq_muvib(Interf_Param%VCC_Nb_Freq),       &
                                                               stat=ErrCode )
     allocate( Interf_Param%VCC_Phase_muvib(Interf_Param%VCC_Nb_Freq),      &
                                                               stat=ErrCode )
     allocate( Interf_Param%VCC_Course(Interf_Param%VCC_Nb_Seg),            &
                                                               stat=ErrCode )
     allocate( Interf_Param%VCC_A0(Interf_Param%VCC_Nb_Seg),                &
                                                               stat=ErrCode )
     allocate( Interf_Param%VCC_A1(Interf_Param%VCC_Nb_Seg),                &
                                                               stat=ErrCode )
     allocate( Interf_Param%Wn_First(Interf_Param%Nband),      stat=ErrCode )
     allocate( Interf_Param%Wn_Last(Interf_Param%Nband),       stat=ErrCode )
     allocate( Interf_Param%Wn_Usefull_First(Interf_Param%Nband),           &
                                                               stat=ErrCode )
     allocate( Interf_Param%Wn_Usefull_Last(Interf_Param%Nband),            &
                                                               stat=ErrCode )
     allocate( Interf_Param%WnLaser_EW(Interf_Param%Nband),    stat=ErrCode )
     allocate( Interf_Param%Laser_Intensity(Interf_Param%Nband),            &
                                                               stat=ErrCode )
     allocate( Interf_Param%Flat_Intensity(Interf_Param%Nband),            &
                                                               stat=ErrCode )
!
     if (ErrCode > 0) then
        write(0,*) 'allocation Interf_Param Error'
        write(0,*) 'Interf_Param: fatal error'
        call Err_outputErrorMessage(0)
        call exit(1)
     end if
     return
   end subroutine alloc_Interf_Param
!
!
   subroutine dalloc_Interf_Param( Interf_Param )
   implicit none
     type(type_Interf_Param), intent(inout)         :: Interf_Param
!
     deallocate( Interf_Param%Phase_Poly_Coeff )
     deallocate( Interf_Param%VCC_Amp_muvib )
     deallocate( Interf_Param%VCC_Freq_muvib )
     deallocate( Interf_Param%VCC_Phase_muvib )
     deallocate( Interf_Param%VCC_Course )
     deallocate( Interf_Param%VCC_A0 )
     deallocate( Interf_Param%VCC_A1 )
     deallocate( Interf_Param%Wn_First )
     deallocate( Interf_Param%Wn_Last )
     deallocate( Interf_Param%Wn_Usefull_First )
     deallocate( Interf_Param%Wn_Usefull_Last )
     deallocate( Interf_Param%WnLaser_EW )
     deallocate( Interf_Param%Laser_Intensity )
     deallocate( Interf_Param%Flat_Intensity )
     return
   end subroutine dalloc_Interf_Param
!
!
end module interf_param_type
