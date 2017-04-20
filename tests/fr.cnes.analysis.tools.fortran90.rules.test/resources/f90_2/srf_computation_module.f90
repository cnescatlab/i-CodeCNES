!!# isrf_model_computation_module.f90 --
!!#
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#              Date: october 2009 
!!#           Version: $Revision: 1.8 $
!!# 
!!#
!!# Language:  F90
!!# Standards: Noveltis
!!# Last modification: $Date: 2012-02-08 10:19:17 $
!!#
!!# --
!!#
!! 
!> srf_model_computation_module -- Module
!!
!! * Purpose
!!
!!     Module for srf computation.
!!
!! * Description
!!      
!!     This module defines the analytic model to calculate the Spectral Response 
!!     Function for levels 1a, 1b and 1c. The real spectral response function is 
!!     provide for measurement targets at specified wavenumbers in spectral sampling.  
!!     More precisely, Spectral Response Functions are the results of the inverse 
!!     Fourier Transform of the Self Apodisation Functions. The result is separated 
!!     in two components : 
!!        -  the normalised spectral shape SRF 
!!        -  and the spectral shift. 
!!     The spectral shape is re-centred on its barycentre. In order to produce the 
!!     calibrated SRF, its spectral abscissa are calibrated
!!
!! * Sub-routines and functions
!!
!!     - srf_init        :  Initialisation of the Spectral Response Function 
!!     - srf_L1a         :  Computation of the Spectral Response Function for level 1a
!!     - srf_L1b         :  Computation of the Spectral Response Function for level 1b
!!     - srf_L1c_Gauss   :  Computation of the Srf for level 1c - use of gauss function 
!!     - srf_L1c_Coscar  :  Computation of the Srf for level 1c - use of square cosine 
!!                          function  
!!     - srf_L1c_Triangl :  Computation of the Srf for level 1c - use of triangular 
!!                          function 
!!     - srf_L1c_Perturb :  Computation of the Srf for level 1c perturbated
!!     - srf_L1d_Perturb :  Computation of the Srf for level 1d perturbated 
!!
!! * References
!!
!!     ATBD
!!

module srf_computation_module
   use precision_type
   use error_type
   use constantes_type
   use ccm_type
   use chrom_type
   use srf_param_type
   use general_type
   use saf_type
   use srf_type
   use math_module
   use fft_module
   use convolution_module
!
   implicit none
!
!
   public ::                 &
             srf_L1a         &
            ,srf_L1b         &
            ,srf_L1c_Gauss   &
            ,srf_L1c_Coscar  &
            ,srf_L1c_Triangl &
            ,srf_L1c_Perturb &
            ,srf_L1d_Perturb &
            ,srf_init        
!
   contains

!!
!!
!> srf_init -- Public
!!
!! * Purpose
!!
!!     Initialisation of the Spectral Response Function 
!!
!! * Description
!!
!!     This subroutine allows the initialisation of type_Srf parameters in function of 
!!     parameters read in the input file
!!
!! * Inputs
!!
!!    - Srf_Param : type_Srf_Param / type for declaration and allocation of srf parameters
!!    - SB        : spectral band    
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!    - Srf : type_Srf / type for declaration and allocation of srf
!!
!! * References
!!

   subroutine srf_init( Srf_Param, SB, Srf )
   implicit none
!
   type(type_Srf_Param) ,intent(in)                  :: Srf_Param
   integer(kind=LONG)   ,intent(in)                  :: SB
   type(type_Srf)       ,intent(out)                 :: Srf
   integer(kind=LONG)                                :: NF
!
     Srf%NsWn0     = Srf_Param%NsWn0(SB)
     Srf%SigI      = Srf_Param%SigI
     Srf%NsPixel   = Srf_Param%Mes_NsPsf
     Srf%NsSDWn1a  = Srf_Param%NsSDWn1a
     Srf%NsSDWn1b  = Srf_Param%NsSDWn1b
     Srf%NsSDWn1c  = Srf_Param%NsSDWn1c
     Srf%SDWnMax1a = Srf_Param%SDWnMax1a
     Srf%SDWnMax1b = Srf_Param%SDWnMax1b
     Srf%SDWnMax1c = Srf_Param%SDWnMax1c
     Srf%dWn1a     = 2.d+00 * Srf%SDWnMax1a / dble( Srf%NsSDWn1a-1 )
     Srf%dWn1b     = 2.d+00 * Srf%SDWnMax1b / dble( Srf%NsSDWn1b-1 )
     Srf%dWn1c     = 2.d+00 * Srf%SDWnMax1c / dble( Srf%NsSDWn1c-1 )
     call alloc_Srf( Srf )
     do NF = 1, Srf%NsWn0
        Srf%Wn0(NF) = Srf_Param%Wn0(NF,SB)
     end do
     call Srf_Base_1a( Srf )
     call Srf_Base_1b( Srf )
     call Srf_Base_1c( Srf )
     return
   end subroutine srf_init
!!
!!
!> srf_L1a -- Public
!!
!! * Purpose
!!
!!     Computation of the Spectral Response Function for level 1a
!!
!! * Description
!!
!!   This subroutine allows the computation of the Spectral Response Function for level 1a.
!!   The Srf L1a is interpolated by a cubic spline from the wave numbers that take into
!!   account the level 1a spectral shift to the nominal level 1a calibrated wave numbers.
!! 
!!   So, the computation requires the following steps:
!!    - The first one is the definition of the ISRFs oversampling factor
!!    - then, for the definition of the sampling basis of the Saf interpolated before the 
!!      Fourier Transform
!!            - the maximum OPD used for the TF before level L1a is computed
!!            - the FFT dimension is defined
!!            - all required tables are allocated
!!            - and opd value are defined for each of all interpolation points 
!!    - The preparation of the Saf before the TF consists in 
!!            - the initialisation of the interpolated Saf (complex form)
!!            - the determination of the position of the last zero
!!            - a cubic splin interpolation of Saf
!!            - the initialisation of the Saf imaginary part required for the fft
!!    - Then, the Isrf sampling step is computed and used in the Fourier Transform 
!!    - In order to define the Isrf sampling basis, the next step consist in the definition 
!!      of calibrated wavenumbers for the Isrf samples after the TF, from 1 to NsFft+1
!!    - After the determination of the Isrf quadratic barycenter, the Isrf is refocused 
!!      on its barycenter and the wavenumbers for the refocused samples are computed ; 
!!    - Finally, before all tables deallocation, the Spectral Response Function for level 1a
!!      is computed as the complex normalisation of the centered Isrf by splin cubic 
!!      interpolation 
!!
!! * Inputs
!!
!!    - L1aNsFft : samples number for the Level 1a FFT
!!    - NF       : frequencies number
!!    - PN       : pixel
!!    - Sas      : type_Saf / type for declaration and allocation of saf parameters
!!
!! * Inputs/outputs
!!
!!    - Srf : type_Srf / type for declaration and allocation of srf
!!
!! * Outputs
!!
!! * References
!!
   subroutine srf_L1a( Srf_Param, NF, PN, Sas, Srf )
   implicit none
     type(type_Srf_Param), intent(in)                :: Srf_Param
     integer(kind=LONG)  ,intent(in)                 :: NF
     integer(kind=LONG)  ,intent(in)                 :: PN
     type(type_Saf)      ,intent(inout)              :: Sas
     type(type_Srf)      ,intent(inout)              :: Srf
     integer(kind=LONG)                              :: OSFactor
     real(kind=DOUBLE)                               :: dOpd
     integer(kind=LONG)                              :: NsOpd
     integer(kind=LONG)                              :: NsFft
     integer(kind=LONG)                              :: NZpd
     real(kind=DOUBLE)   ,dimension(:) ,allocatable  :: Opd
     real(kind=DOUBLE)   ,dimension(:) ,allocatable  :: SafIMod
     real(kind=DOUBLE)   ,dimension(:) ,allocatable  :: SafIArg
     complex(kind=DOUBLE),dimension(:) ,allocatable  :: SafI
     integer(kind=LONG)                              :: Nizero
     integer(kind=LONG)                              :: Sens
     real(kind=DOUBLE)                               :: dWnIsrfC
     real(kind=DOUBLE)   ,dimension(:) ,allocatable  :: WnIsrfC
     integer(kind=LONG)                              :: Ns
     complex(kind=DOUBLE),dimension(:) ,allocatable  :: Isrf
     real(kind=DOUBLE)   ,dimension(:) ,allocatable  :: WnIsrfR
     real(kind=DOUBLE)   ,dimension(:) ,allocatable  :: IsrfReal
     real(kind=DOUBLE)   ,dimension(:) ,allocatable  :: IsrfRReal
     real(kind=DOUBLE)                               :: NormR
     integer(kind=LONG)                              :: Ip
     integer(kind=LONG)                              :: ErrCode
     integer(kind=LONG)                              :: ioalloc
     integer(kind=LONG)                              :: Iteration
     type(type_Saf)                                  :: Sas_tmp
!
!    definition of the ISRFs oversampling factor
     OSFactor = 8
     NsOpd = Srf_Param%L1aNsFft + 1
!    FFT Dimension
     NsFft = (NsOpd -1) * OSFactor
!
!    Sas apodisation
     call Saf_Transfer( Sas, Sas_tmp )
     if( trim(Srf_Param%Apod_Option) == 'Yes' ) then
        write(*,*) 'srf_L1a apodisation'
        Sas_tmp%Mod(1:Sas_tmp%NsOpd,NF,PN) =             &
                      Sas_tmp%Mod(1:Sas_tmp%NsOpd,NF,PN) &
                    * Srf%L1cTF(1:Sas_tmp%NsOpd)
     else
        write(*,*) 'srf_L1a no apodisation'
     end if 
!
!    allocation
     ioalloc = 0
     allocate( Opd(Srf_Param%L1aNsFft+1),stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( SafIMod(NsFft+1),  stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( SafIArg(NsFft+1),  stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( SafI(NsFft+1),     stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( WnIsrfC(NsFft+1),  stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( WnIsrfR(NsFft+1),  stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Isrf(NsFft+1),     stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( IsrfReal(NsFft+1), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( IsrfRReal(NsFft+1), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if( ioalloc > 0 ) then
        write(*,*) 'Allocation Error'
        write(*,*) 'srf_L1a Fatal Error'
        call exit(1)
     end if
!
!    definition of the sampling basis of the Saf interpolated 
!    before the Fourier Transform
!    maximum Optical Path Difference use for the TF before level L1a
     dOpd = Srf_Param%OpdMax / dble(Srf_Param%L1aNsFft/2)
!
!    opd value defined for each of all interpolation points 
     NZpd = int(Srf_Param%L1aNsFft/2)+1
     Opd(1:Srf_Param%L1aNsFft+1) = &
        (/ (dble(Ns-NZpd),Ns=1,Srf_Param%L1aNsFft+1) /) * dOpd

!    initialisation of the interpolated Saf (complex form)
     SafIMod(1:NsFft+1) = 0.d+00
     SafIArg(1:NsFft+1) = 0.d+00
!
!    last zero position
     Nizero = int(NsFft/2) - int(NsOpd/2)
     if( Nizero .lt. 0 ) then
        write(0,'(a40,i10)') ' error nb points ISRF to zero',Nizero
        call exit(1)
     end if
!
!    cubic spline interpolation of the Saf 
     call intspline( Sas_tmp%NsOpd, Sas_tmp%Opd, Sas_tmp%Mod(1,NF,PN)&
                    ,NsOpd,                 Opd,   SafIMod(Nizero+1) )
     call intspline( Sas_tmp%NsOpd, Sas_tmp%Opd, Sas_tmp%Arg(1,NF,PN)&
                    ,NsOpd,                 Opd,   SafIArg(Nizero+1) )
!
     call modargtocplx( SafI, SafIMod, SafIArg, NsFft+1 )
!
!    initialisation of the Saf imaginary part required for the fft
     SafI(Nizero+NsOpd) = dcmplx(0.d+00,0.d+00)
!
!    Srf local sampling step
     dWnIsrfC = 1.d+00 / ( 2.d+00 * Srf_Param%OpdMax * OSFactor )
!
!    Fourier Transform
     Sens = -1
     call fft_c2c( NsFft, Sens, SafI, dOpd, dWnIsrfC, Isrf )
!
!    real part extraction
     IsrfReal(1:NsFft+1) = dreal( Isrf(1:NsFft+1) )
!
!    srf local sampling basis 
!    after the TF, from 1 to NsFft+1
     NZpd = int(NsFft/2)+1
     WnIsrfC(1:NsFft+1) = (/ (dble(Ns-NZpd),Ns=1,NsFft+1) /) * dWnIsrfC
!
!    Srf sampling step
     Srf%dWn1a = 2.d+00 * Srf%SDWnMax1a  / dble( Srf%NsSDWn1a -1 )
     if( Srf%NsSDWn1a .gt. NsFft+1 ) then
        write(0,'(a40,i10)') ' error nb points SRF',Srf%NsSDWn1a
        call exit(1)
     end if
     call Srf_Base_1a( Srf )
!
     if( Srf_Param%Calib_Option == 15 ) then
!
!       Barycentre from phase slope at OPD=0
        call extract_poly_barycentre( Sas_tmp%NsOpd,         &
                                      Sas_tmp%Opd,           &
                                      Srf_Param%Calib_Option,&
                                      Sas_tmp%Mod(1,NF,PN),  &
                                      Sas_tmp%Arg(1,NF,PN),  &
                                      Srf%WnShift1a(NF,PN)   )
     else if( Srf_Param%Calib_Option >= 2 ) then
!
!       Barycentre from phase slope at OPD=0
        call extract_lin_barycentre( Sas_tmp%NsOpd,         &
                                     Sas_tmp%Opd,           &
                                     Srf_Param%Calib_Option,&
                                     Sas_tmp%Mod(1,NF,PN),  &
                                     Sas_tmp%Arg(1,NF,PN),  &
                                     Srf%WnShift1a(NF,PN)   )
     else if( Srf_Param%Calib_Option == 1 ) then
!
!       Isrf linear barycentre
        Ip = 1
        Iteration = 3
        call barycen_iter_real( NsFft+1, dWnIsrfC, WnIsrfC, IsrfReal,&
                                Ip, Iteration, Srf%WnShift1a(NF,PN)  )
     else if( Srf_Param%Calib_Option == 0 ) then
!
!       Isrf quadratic barycentre
        Ip = 2
        Iteration = 3
        call barycen_iter_real( NsFft+1, dWnIsrfC, WnIsrfC, IsrfReal,&
                                Ip, Iteration, Srf%WnShift1a(NF,PN)  )
     else
        write(*,*) 'Srf_Param%Calib_Option Error',Srf_Param%Calib_Option
        call exit(1)
     end if
     Srf%Fcs1a(NF,PN) = Srf%WnShift1a(NF,PN) / Srf%Wn0(NF)
!
!    Isrf re-focused on its barycentre
!    Computation of the wavenumbers for the refocused samples 
     WnIsrfR(1:NsFft+1) = WnIsrfC(1:NsFft+1) - Srf%WnShift1a(NF,PN)
!
!    Spectral Response Function for level 1a :
!    computation of the centered Isrf by cubic spline interpolation 
     call intspline( NsFft+1,      WnIsrfR,    IsrfReal, &
                     Srf%NsSDWn1a, Srf%SDWn1a, IsrfRReal )
!
!    real normalisation of the centered Isrf 
     NormR = 0.d+00
     NormR = NormR + IsrfRReal(1)            * Srf%dWn1a / 2.d+00
     NormR = NormR + IsrfRReal(Srf%NsSDWn1a) * Srf%dWn1a / 2.d+00
     NormR = NormR + sum( IsrfRReal(2:Srf%NsSDWn1a-1) ) * Srf%dWn1a
!
     Srf%L1a(1:Srf%NsSDWn1a,NF,PN) = IsrfRReal(1:Srf%NsSDWn1a) / NormR
!
     call fwhm_real( Srf%NsSDWn1a, Srf%L1a(1,NF,PN),&
                     Srf%dWn1a, Srf%FWhm1a(NF,PN)   )
!
!    deallocation
     call dalloc_Saf( Sas_tmp )
     deallocate(Opd)
     deallocate(SafIMod)
     deallocate(SafIArg)
     deallocate(SafI)
     deallocate(WnIsrfC)
     deallocate(WnIsrfR)
     deallocate(Isrf)
     deallocate(IsrfReal)
     deallocate(IsrfRReal)
     return
   end subroutine srf_L1a
!!
!!
!> srf_L1b -- Public
!!
!! * Purpose
!!
!!     Computation of the Spectral Response Function for level 1b
!!
!! * Description
!!
!!   This subroutine allows the computation of the Spectral Response Function for level 1b.
!!
!!   The SRF L1b is calculated by the same algorithm than the SRF 1a with two differences:
!! 	- The algorithm of windowing is applied to the complex SAF if the SigI factor 
!!        is not equal to zero. 
!!      - The cubic spline interpolation of the SRF 1b, calculated on the shifted wave 
!!        numbers is performed on the nominal constant level 1b/1c wave numbers .
!!
!!   So, the SRF L1b computation requires the following steps:
!!    - The first one is the definition of the ISRFs oversampling factor
!!    - then, for the definition of the sampling basis of the Saf interpolated before the 
!!      Fourier Transform
!!            - the maximum Optical Path Difference use for the TF before level L1b
!!              is computed
!!            - the FFT dimension is defined
!!            - all required tables are allocated
!!            - and opd value are defined for each of all interpolation points 
!!    - The preparation of the Saf before the TF consists in 
!!            - the initialisation of the interpolated Saf (complex form)
!!            - the determination of the position of the last zero
!!            - a cubic splin interpolation of Saf
!!            - the interferogram both ends are dampened 
!!            - the initialisation of the Saf imaginary part required for the fft
!!    - Then, the Isrf sampling step is computed and used in the Fourier Transform 
!!    - In order to define the Isrf sampling basis, the next step consist in the definition 
!!      of calibrated wavenumbers for the Isrf samples after the TF, from 1 to NsIsrf
!!    - After the determination of the Isrf quadratic barycenter, the Isrf is refocused 
!!      on its barycenter and the wavenumbers for the refocused samples are computed ; 
!!    - Finally, before all tables deallocation, the Spectral Response Function for level 1 
!!      is computed as the complex normalisation of the centered Isrf by splin cubic 
!!      interpolation 
!!
!! * Inputs
!!
!!    - L1bNsFft : samples number for the Level 1a FFT
!!    - NF       : frequencies number
!!    - PN       : pixel
!!    - Sas      : type_Saf / type for declaration and allocation of saf parameters
!!
!! * Inputs/outputs
!!
!!    - Srf : type_Srf / type for declaration and allocation of srf
!!
!! * Outputs
!!
!! * References
!!

   subroutine srf_L1b( Srf_Param, NF, PN, Sas, Srf )
   implicit none
     type(type_Srf_Param), intent(in)                :: Srf_Param
     integer(kind=LONG)  ,intent(in)                 :: NF
     integer(kind=LONG)  ,intent(in)                 :: PN
     type(type_Saf)      ,intent(in)                 :: Sas
     type(type_Srf)      ,intent(inout)              :: Srf
     integer(kind=LONG)                              :: OSFactor
     real(kind=DOUBLE)                               :: dOpd
     integer(kind=LONG)                              :: NsOpd
     integer(kind=LONG)                              :: NsFft
     integer(kind=LONG)                              :: NZpd
     real(kind=DOUBLE)   ,dimension(:) ,allocatable  :: Opd
     real(kind=DOUBLE)   ,dimension(:) ,allocatable  :: SafIMod
     real(kind=DOUBLE)   ,dimension(:) ,allocatable  :: SafIArg
     complex(kind=DOUBLE),dimension(:) ,allocatable  :: SafI
     integer(kind=LONG)                              :: Nizero
     integer(kind=LONG)                              :: Sens
     real(kind=DOUBLE)                               :: dWnIsrfC
     real(kind=DOUBLE)   ,dimension(:) ,allocatable  :: WnIsrfC
     integer(kind=LONG)                              :: Ns
     complex(kind=DOUBLE),dimension(:) ,allocatable  :: Isrf
     real(kind=DOUBLE)   ,dimension(:) ,allocatable  :: WnIsrfR
     real(kind=DOUBLE)   ,dimension(:) ,allocatable  :: IsrfReal
     real(kind=DOUBLE)   ,dimension(:) ,allocatable  :: IsrfRReal
     real(kind=DOUBLE)                               :: NormR
     integer(kind=LONG)                              :: NsWin
     integer(kind=LONG)                              :: Ip
     integer(kind=LONG)                              :: ErrCode
     integer(kind=LONG)                              :: ioalloc
     integer(kind=LONG)                              :: Iteration
     type(type_Saf)                                  :: Sas_tmp
!
!    definition of the ISRFs oversampling factor
     OSFactor = 8
     NsOpd = Srf_Param%L1bNsFft + 1
!    FFT Dimension
     NsFft = (NsOpd -1) * OSFactor
!
!    Sas apodisation
     call Saf_Transfer( Sas, Sas_tmp )
     if( trim(Srf_Param%Apod_Option) == 'Yes' ) then
        write(*,*) 'srf_L1b apodisation'
        Sas_tmp%Mod(1:Sas_tmp%NsOpd,NF,PN) =             &
                      Sas_tmp%Mod(1:Sas_tmp%NsOpd,NF,PN) &
                    * Srf%L1cTF(1:Sas_tmp%NsOpd)
     else
        write(*,*) 'srf_L1b no apodisation'
     end if 
!
!    allocation
     ioalloc = 0
     allocate( Opd(Srf_Param%L1bNsFft+1),stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( SafIMod(NsFft+1),  stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( SafIArg(NsFft+1),  stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( SafI(NsFft+1),     stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( WnIsrfC(NsFft+1),  stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( WnIsrfR(NsFft+1),  stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( Isrf(NsFft+1),     stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( IsrfReal(NsFft+1), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( IsrfRReal(NsFft+1),stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if( ioalloc > 0 ) then
        write(*,*) 'Allocation Error'
        write(*,*) 'srf_L1b Fatal Error'
        call exit(1)
     end if
!
!    definition of the sampling basis of the Saf interpolated 
!    before the Fourier Transform
!    maximum Optical Path Difference use for the TF before level L1b
     dOpd = Srf_Param%OpdMax / dble(Srf_Param%L1bNsFft/2)
!
!    opd value defined for each of all interpolation points 
     NZpd = int(Srf_Param%L1bNsFft/2)+1
     Opd(1:Srf_Param%L1bNsFft+1) = &
        (/ (dble(Ns-NZpd),Ns=1,Srf_Param%L1bNsFft+1) /) * dOpd

!    initialisation of the interpolated Saf (complex form)
     SafIMod(1:NsFft+1) = 0.d+00
     SafIArg(1:NsFft+1) = 0.d+00
!
!    last zero position
     Nizero = int(NsFft/2) - int(NsOpd/2)
     if( Nizero .lt. 0 ) then
        write(0,'(a40,i10)') ' error nb points ISRF to zero',Nizero
        call exit(1)
     end if
!
!    cubic spline interpolation of the Saf 
     call intspline( Sas_tmp%NsOpd, Sas_tmp%Opd, Sas_tmp%Mod(1,NF,PN)&
                    ,NsOpd,         Opd,           SafIMod(Nizero+1) )
     call intspline( Sas_tmp%NsOpd, Sas_tmp%Opd, Sas_tmp%Arg(1,NF,PN)&
                    ,NsOpd,         Opd,           SafIArg(Nizero+1) )
!
     call modargtocplx( SafI, SafIMod, SafIArg, NsFft+1 )
!
!    interferogram both ends are dampened
     if( Srf%SigI /= 0 ) then
        NsWin = ((Srf%SigI-1)*OSFactor) +1
        call windowingcplx( NsOpd, SafI(Nizero+1), NsWin, dOpd )
     end if
!
!    initialisation of the Saf imaginary part required for the fft
     SafI(Nizero+NsOpd) = dcmplx(0.d+00,0.d+00)
!
!    Srf local sampling step
     dWnIsrfC = 1.d+00 / ( 2.d+00 * Srf_Param%OpdMax * OSFactor )
!
!    Fourier Transform
     Sens = -1
     call fft_c2c( NsFft, Sens, SafI, dOpd, dWnIsrfC, Isrf )
!
!    real part extraction
     IsrfReal(1:NsFft+1) = dreal( Isrf(1:NsFft+1) )
!
!    srf local sampling basis 
!    after the TF, from 1 to NsFFT+1
     NZpd = int(NsFft/2)+1
     WnIsrfC(1:NsFft+1) = (/ (dble(Ns-NZpd),Ns=1,NsFft+1) /) * dWnIsrfC
!
!    Srf sampling step
     Srf%dWn1b = 2.d+00 * Srf%SDWnMax1b  / dble( Srf%NsSDWn1b -1 )
     if( Srf%NsSDWn1b .gt. NsFft+1 ) then
        write(0,'(a40,i10)') ' error nb points SRF',Srf%NsSDWn1b
        call exit(1)
     end if
     call Srf_Base_1b( Srf )
!
     if( Srf_Param%Calib_Option == 15 ) then
!
!       Barycentre from phase slope at OPD=0
        call extract_poly_barycentre( Sas_tmp%NsOpd,         &
                                      Sas_tmp%Opd,           &
                                      Srf_Param%Calib_Option,&
                                      Sas_tmp%Mod(1,NF,PN),  &
                                      Sas_tmp%Arg(1,NF,PN),  &
                                      Srf%WnShift1b(NF,PN)   )
     else if( Srf_Param%Calib_Option >= 2 ) then
!
!       Barycentre from phase slope at OPD=0
        call extract_lin_barycentre( Sas_tmp%NsOpd,         &
                                     Sas_tmp%Opd,           &
                                     Srf_Param%Calib_Option,&
                                     Sas_tmp%Mod(1,NF,PN),  &
                                     Sas_tmp%Arg(1,NF,PN),  &
                                     Srf%WnShift1b(NF,PN)   )
     else if( Srf_Param%Calib_Option == 1 ) then
!
!       Isrf linear barycentre
        Ip = 1
        Iteration = 3
        call barycen_iter_real( NsFft+1, dWnIsrfC, WnIsrfC, IsrfReal,&
                                Ip, Iteration, Srf%WnShift1b(NF,PN)  )
     else if( Srf_Param%Calib_Option == 0 ) then
!
!       Isrf quadratic barycentre
        Ip = 2
        Iteration = 4
        call barycen_iter_real( NsFft+1, dWnIsrfC, WnIsrfC, IsrfReal,&
                                Ip, Iteration, Srf%WnShift1b(NF,PN)  )
     else
        write(*,*) 'Calib_Option Error',Srf_Param%Calib_Option
        call exit(1)
     end if
     Srf%Fcs1b(NF,PN) = Srf%WnShift1b(NF,PN) / Srf%Wn0(NF)
!
!    Isrf re-focused on its barycentre
!    Computation of the wavenumbers for the refocused samples 
     WnIsrfR(1:NsFft+1) = WnIsrfC(1:NsFft+1) - Srf%WnShift1b(NF,PN)
!
!    Spectral Response Function for level 1b :
!    computation of the centered Isrf by spline cubic interpolation
     call intspline( NsFft+1,      WnIsrfR,    IsrfReal  &
                    ,Srf%NsSDWn1b, Srf%SDWn1b, IsrfRReal )
!
!    complex normalisation of the centered Isrf
     NormR = 0.d+00
     NormR = NormR + IsrfRReal(1)            * Srf%dWn1b / 2.d+00
     NormR = NormR + IsrfRReal(Srf%NsSDWn1b) * Srf%dWn1b / 2.d+00
     NormR = NormR + sum( IsrfRReal(2:Srf%NsSDWn1b-1) ) * Srf%dWn1b
!
     Srf%L1b(1:Srf%NsSDWn1b,NF,PN) = IsrfRReal(1:Srf%NsSDWn1b) / NormR
!
     call fwhm_real( Srf%NsSDWn1b, Srf%L1b(1,NF,PN),&
                     Srf%dWn1b, Srf%FWhm1b(NF,PN)   )
!
!    deallocation
     call dalloc_Saf( Sas_tmp )
     deallocate(Opd)
     deallocate(SafIMod)
     deallocate(SafIArg)
     deallocate(SafI)
     deallocate(WnIsrfC)
     deallocate(WnIsrfR)
     deallocate(Isrf)
     deallocate(IsrfReal)
     deallocate(IsrfRReal)
     return
   end subroutine srf_L1b
!!
!!
!> srf_L1c_Gauss -- Public
!!
!! * Purpose
!!
!!     Computation of the Spectral Response Function for level 1c - use of gauss function 
!!
!! * Description
!!
!!   This subroutine allows the computation of the Spectral Response Function for level 1c.
!!   
!!   The atmospheric level 1C spectra correspond to interferograms that are observed by a 
!!   perfect instrument and on which we apply a given Apodisation Function. 
!!   A transformation is introduced from level 1B to level 1c to eliminate the spectral 
!!   dependency and the temporal dependency of the instrument response functions. 
!!   The level 1c spectrum is the convolution of a high resolution spectrum by a theoretical
!!   SRF. The level 1c SRF is the real inverse Fourier transform of a theoretical function.
!!   In this subroutine the theoretical function wich is used is a Gauss function.
!! 
!!   The computation requires the following steps:
!!    - Before the computation of the gauss function, required tables allocation is done 
!!      and constant parameters are initialized.      
!!    - Then, thanks to an inverse Fourier Transform, the level 1c Isrf is computed ; only 
!!      the usefull part is extracted and normalized ; At last, the function is computed 
!!      for the Apf sampling
!!
!! * Inputs
!!
!!    - FWHM    : Full Width at Half Maximum value
!!    - NsFft   : FFT sample number 
!!    - PN      : pixel
!!    - Sas     : type_Saf / type for declaration and allocation of saf parameters
!!
!! * Inputs/outputs
!!
!!    - Srf : type_Srf / type for declaration and allocation of srf
!!
!! * Outputs
!!
!! * References
!!
!!      ATBD 
!!

   subroutine srf_L1c_Gauss( Srf_Param, Sas, Srf )
   implicit none
     type(type_Srf_Param),intent(in)                 :: Srf_Param
     type(type_Saf)     ,intent(in)                  :: Sas
     type(type_Srf)     ,intent(inout)               :: Srf
     type(type_Cnv_Fct)                              :: Cnv_Fct
!
!    constants initialisation 
     Srf%Opd_effective = Srf_Param%OpdMax * dcos( Srf_Param%FieldMeanAngle )
!
!    Gaussian truncated function computation
     Cnv_Fct%FWHM     = Srf_Param%FWHM
     Cnv_Fct%FullSDWn = 2_DOUBLE * Srf%SDWnMax1c
     Cnv_Fct%Ns_Opd   = Sas%NsOpd
     Cnv_Fct%OpdMax   = Srf_Param%OpdMax
     Cnv_Fct%dOpd     = Sas%dOpd
     Cnv_Fct%WnShift  = 0_DOUBLE
     Cnv_Fct%Opd_effective = Srf%Opd_effective
     call calc_gauss_tronc( Srf_Param%L1cNsFft, Cnv_Fct )
!
!    transfert Fct to Srf
     Srf%FWhm1c = Cnv_Fct%FWHM
     Srf%WnShift1c = Cnv_Fct%WnShift
     Srf%Opd_effective = Cnv_Fct%Opd_effective
     call intspline( Cnv_Fct%N_Sample,&
                     Cnv_Fct%Wn,      &
                     Cnv_Fct%Fct,     &
                     Srf%NsSDWn1c,    &
                     Srf%SDWn1c,      &
                     Srf%L1c(1,1,1)   )
     call intspline( Cnv_Fct%Ns_Opd,&
                     Cnv_Fct%Opd,   &
                     Cnv_Fct%Fct_TF,&
                     Sas%NsOpd,     &
                     Sas%Opd,       &
                     Srf%L1cTF      )
!
!    deallocation
     call dalloc_Cnv_Fct( Cnv_Fct )
     return
   end subroutine srf_L1c_Gauss


!!
!!
!> srf_L1c_Coscar -- Public
!!
!! * Purpose
!!
!!     Computation of the Spectral Response Function for level 1c - use of square 
!!     cosine function 
!!
!! * Description
!!
!!   This subroutine allows the computation of the Spectral Response Function for level 1c.
!!   
!!   The atmospheric level 1C spectra correspond to interferograms that are observed by a 
!!   perfect instrument and on which we apply a given Apodisation Function. 
!!   A transformation is introduced from level 1B to level 1c to eliminate the spectral 
!!   dependency and the temporal dependency of the instrument response functions. 
!!   The level 1c spectrum is the convolution of a high resolution spectrum by a theoretical
!!   SRF. The level 1c SRF is the real inverse Fourier transform of a theoretical function.
!!   In this subroutine the theoretical function wich is used is a square cosine function.
!! 
!!   The computation requires the following steps:
!!    - The first step consists in required tables allocation, and constant parameters 
!!      initialisation. 
!!    - Then, the optical difference path basis is defined and the Saf modulus is 
!!      interpolated by splin.
!!    - The square cosine function is also defined.     
!!    - And, thanks to an inverse Fourier Transform, the level 1c Isrf is computed ; only 
!!      the usefull part is extracted and normalized ; At last, the function is computed 
!!      for the Apf sampling
!!
!! * Inputs
!!
!!    - NF       : frequencies number
!!    - PN       : pixel
!!    - Fraction : fraction value of effective opd
!!    - NsFft    : FFT sample number 
!!    - Sas      : type_Saf / type for declaration and allocation of saf parameters
!!
!! * Inputs/outputs
!!
!!    - Srf : type_Srf / type for declaration and allocation of srf
!!
!! * Outputs
!!
!! * References
!!
!!      ATBD 
!!

   subroutine srf_L1c_Coscar( Srf_Param, Sas, Srf )
   implicit none
     type(type_Srf_Param),intent(in)                 :: Srf_Param
     type(type_Saf)     ,intent(in)                  :: Sas
     type(type_Srf)     ,intent(inout)               :: Srf
     type(type_Cnv_Fct)                              :: Cnv_Fct
!
!    constants initialisation 
     Srf%Opd_effective = Srf_Param%OpdMax * dcos( Srf_Param%FieldMeanAngle )
!
!    Coscar function computation
     Cnv_Fct%FWHM     = 0_DOUBLE
     Cnv_Fct%FullSDWn = 2_DOUBLE * Srf%SDWnMax1c
     Cnv_Fct%Ns_Opd   = Sas%NsOpd
     Cnv_Fct%OpdMax   = Srf_Param%OpdMax
     Cnv_Fct%dOpd     = Sas%dOpd
     Cnv_Fct%WnShift  = 0_DOUBLE
     Cnv_Fct%Opd_effective = Srf%Opd_effective
     call calc_coscar( Srf_Param%L1cNsFft, Srf_Param%OpdFraction, Cnv_Fct )
!
!    transfert Fct to Srf
     Srf%FWhm1c = Cnv_Fct%FWHM
     Srf%WnShift1c = Cnv_Fct%WnShift
     Srf%Opd_effective = Cnv_Fct%Opd_effective
     call intspline( Cnv_Fct%N_Sample,&
                     Cnv_Fct%Wn,      &
                     Cnv_Fct%Fct,     &
                     Srf%NsSDWn1c,    &
                     Srf%SDWn1c,      &
                     Srf%L1c(1,1,1)   )
     call intspline( Cnv_Fct%Ns_Opd,&
                     Cnv_Fct%Opd,   &
                     Cnv_Fct%Fct_TF,&
                     Sas%NsOpd,     &
                     Sas%Opd,       &
                     Srf%L1cTF      )
!
!    deallocation
     call dalloc_Cnv_Fct( Cnv_Fct )
     return
   end subroutine srf_L1c_Coscar


!!
!!
!> srf_L1c_Triangl -- Public
!!
!! * Purpose
!!
!!     Computation of the Spectral Response Function for level 1c - use of 
!!     triangular function 
!!
!! * Description
!!
!!   This subroutine allows the computation of the Spectral Response Function for level 1c.
!!   
!!   The atmospheric level 1C spectra correspond to interferograms that are observed by a 
!!   perfect instrument and on which we apply a given Apodisation Function. 
!!   A transformation is introduced from level 1B to level 1c to eliminate the spectral 
!!   dependency and the temporal dependency of the instrument response functions. 
!!   The level 1c spectrum is the convolution of a high resolution spectrum by a theoretical
!!   SRF. The level 1c SRF is the real inverse Fourier transform of a theoretical function.
!!   In this subroutine the theoretical function wich is used is a triangular function.
!! 
!!   The computation requires the following steps:
!!    - The first step consists in required tables allocation, and constant parameters 
!!      initialisation. Then, the optical difference path basis is defined.
!!    - The trianglular function is then defined.     
!!    - And, thanks to an inverse Fourier Transform, the level 1c Isrf is computed ; only 
!!      the usefull part is extracted and normalized ; At last, the function is computed 
!!      for the Apf sampling
!!
!! * Inputs
!!
!!    - NsFft    : FFT sample number 
!!    - PN       : pixel
!!    - Sas      : type_Saf / type for declaration and allocation of saf parameters
!!
!! * Inputs/outputs
!!
!!    - Srf : type_Srf / type for declaration and allocation of srf
!!
!! * Outputs
!!
!! * References
!!
!!      ATBD 
!!

   subroutine srf_L1c_Triangl( Srf_Param, Sas, Srf )
   implicit none
     type(type_Srf_Param),intent(in)                 :: Srf_Param
     type(type_Saf)     ,intent(in)                  :: Sas
     type(type_Srf)     ,intent(inout)               :: Srf
     type(type_Cnv_Fct)                              :: Cnv_Fct
!
!    constants initialisation 
     Srf%Opd_effective = Srf_Param%OpdMax * dcos( Srf_Param%FieldMeanAngle )
!
!    Coscar function computation
     Cnv_Fct%FWHM     = 0_DOUBLE
     Cnv_Fct%FullSDWn = 2_DOUBLE * Srf%SDWnMax1c
     Cnv_Fct%Ns_Opd   = Sas%NsOpd
     Cnv_Fct%OpdMax   = Srf_Param%OpdMax
     Cnv_Fct%dOpd     = Sas%dOpd
     Cnv_Fct%WnShift  = 0_DOUBLE
     Cnv_Fct%Opd_effective = Srf%Opd_effective
     call calc_triangl( Srf_Param%L1cNsFft, Cnv_Fct )
!
!    transfert Fct to Srf
     Srf%FWhm1c = Cnv_Fct%FWHM
     Srf%WnShift1c = Cnv_Fct%WnShift
     Srf%Opd_effective = Cnv_Fct%Opd_effective
     call intspline( Cnv_Fct%N_Sample,&
                     Cnv_Fct%Wn,      &
                     Cnv_Fct%Fct,     &
                     Srf%NsSDWn1c,    &
                     Srf%SDWn1c,      &
                     Srf%L1c(1,1,1)   )
     call intspline( Cnv_Fct%Ns_Opd,&
                     Cnv_Fct%Opd,   &
                     Cnv_Fct%Fct_TF,&
                     Sas%NsOpd,     &
                     Sas%Opd,       &
                     Srf%L1cTF      )
!
!    deallocation
     call dalloc_Cnv_Fct( Cnv_Fct )
     return
   end subroutine srf_L1c_Triangl


!!
!!
!> srf_L1c_Perturb -- Public
!!
!! * Purpose
!!
!!     Computation of the Spectral Response Function for level 1c perturbated
!!
!! * Description
!!
!!     The IASI Level 1c ISRF is the inverse Fourier transform of a truncated 
!!     Gaussian function. This truncated Gaussian function is the truncation of 
!!     a direct Fourier transform of a Gaussian function with a FWHM given. 
!!     The ISRF 1c affected by a perturbation is computed as the real part of 
!!     the invers Fourier transform of the product of the truncated Gaussian 
!!     function and the ratio between the auto apodisation functions with 
!!     perturbation and without.     
!!
!!     This computation requires the following steps:
!!     - After SAS coherence control, the required tables allocation, and 
!!       constants initialisation, the OPD basis for the SAF is computed 
!!     - Then, the perturb L1c function computation consist in the following steps: 
!!             - the ratio between the two SAF estimations (modulus and argument)
!!             - a spline interpolation on the result
!!             - Isrf1c computation thanks to an invers Fourier transform
!!             - the usefull part extraction
!!             - the ISRF quadratic barycentre computation and a suface normalisation
!!     - at last, table deallocation is done
!!
!! * Inputs
!!
!!    - NsFft : samples number for the FFT
!!    - NF       : frequencies number
!!    - PN       : pixel
!!    - Sas      : type_Saf / type for declaration and allocation of saf parameters
!!
!! * Inputs/outputs
!!
!!    - Srf : type_Srf / type for declaration and allocation of srf
!!
!! * Outputs
!!
!! * References
!!
   subroutine srf_L1c_Perturb( NsFft, PN,   &
                               Sas_nominal, &
                               Sas_perturb, &
                               Srf          )
   implicit none
     integer(kind=LONG)  ,intent(in)                 :: NsFft
     integer(kind=LONG)  ,intent(in)                 :: PN
     type(type_Saf)      ,intent(in)                 :: Sas_nominal
     type(type_Saf)      ,intent(in)                 :: Sas_perturb
     type(type_Srf)      ,intent(inout)              :: Srf
     real(kind=DOUBLE)   ,dimension(:) ,allocatable  :: Modulus
     real(kind=DOUBLE)   ,dimension(:) ,allocatable  :: Argument
     real(kind=DOUBLE)   ,dimension(:) ,allocatable  :: Opd
     real(kind=DOUBLE)   ,dimension(:) ,allocatable  :: SMod
     real(kind=DOUBLE)   ,dimension(:) ,allocatable  :: SArg
     complex(kind=DOUBLE),dimension(:) ,allocatable  :: G
     complex(kind=DOUBLE),dimension(:) ,allocatable  :: Isrf
     real(kind=DOUBLE)                               :: dOpd
     integer(kind=LONG)                              :: NsFftp
     integer(kind=LONG)                              :: NF
     integer(kind=LONG)                              :: Ns
     integer(kind=LONG)                              :: Nsfirst
     integer(kind=LONG)                              :: Nslast
     integer(kind=LONG)                              :: Sens
     real(kind=DOUBLE)                               :: Norm
     integer(kind=LONG)                              :: Ip
     integer(kind=LONG)                              :: ErrCode
!
!    coherence control
     if( Sas_nominal%NsOpd /= Sas_perturb%NsOpd ) then
        write(*,*) 'NsOpd Error',Sas_nominal%NsOpd,Sas_perturb%NsOpd
        write(*,*) 'srf_L1c_Perturb Fatal Error'
        call exit(1)
     end if
     if( Sas_nominal%NsWn0 /= Sas_perturb%NsWn0 ) then
        write(*,*) 'NsWn0 Error',Sas_nominal%NsWn0,Sas_perturb%NsWn0
        write(*,*) 'srf_L1c_Perturb Fatal Error'
        call exit(1)
     end if
!
!    allocation
     allocate(Modulus(Sas_nominal%NsOpd)  ,stat=ErrCode )
     allocate(Argument(Sas_nominal%NsOpd) ,stat=ErrCode )
     allocate(Opd(NsFft+1) ,stat=ErrCode )
     allocate(SMod(NsFft+1) ,stat=ErrCode )
     allocate(SArg(NsFft+1) ,stat=ErrCode )
     allocate(G(NsFft+1)   ,stat=ErrCode )
     allocate(Isrf(NsFft+1),stat=ErrCode )
!
     if (ErrCode > 0) then
        write(0,*) 'allocation srf_L1c_Perturb Error'
        write(0,*) 'srf_L1c_Perturb : fatal error'
        call exit(1)
     end if
!
!    initialisation des constantes
     NsFftp = int( NsFft/2 )
     dOpd = 1.d+00 / ( Srf%dWn1c * dble(NsFft))
!
!    base opd du module de Saf interpole sur le FFT
     Opd(1:NsFft+1) = (/ (dble(Ns-NsFftp-1),Ns=1,NsFft+1) /) * dOpd
!
!    Perturb L1c function computation
     do NF = 1, Srf%NsWn0
!
!       Ratio between the two SAF estimations
        Modulus(1:Sas_nominal%NsOpd) =                     &
                Sas_perturb%Mod(1:Sas_perturb%NsOpd,NF,PN) &
              / Sas_nominal%Mod(1:Sas_nominal%NsOpd,NF,PN) &
              * Srf%L1cTF(1:Sas_nominal%NsOpd)
        Argument(1:Sas_nominal%NsOpd) =                    &
                Sas_perturb%Arg(1:Sas_perturb%NsOpd,NF,PN) &
              - Sas_nominal%Arg(1:Sas_nominal%NsOpd,NF,PN) 
!
!       Spline interpolation
        call intspline( Sas_nominal%NsOpd, Sas_nominal%Opd, Modulus, &
                        NsFft+1, Opd, SMod )
        call intspline( Sas_nominal%NsOpd, Sas_nominal%Opd, Argument, &
                        NsFft+1, Opd, SArg )
        call modargtocplx( G, SMod, SArg, NsFft+1 )
        do Ns = 1, NsFft
           if( dabs(Opd(Ns)) .gt. Srf%Opd_effective ) then
              G(Ns) = dcmplx(0.d+00,0.d+00)
           end if
        end do
!       Isrf1c computation
        Sens = -1
        call fft_c2c( NsFft, Sens, G, dOpd, Srf%dWn1c, Isrf )
!
!       Usefull part extraction
        Nsfirst = ( NsFftp+1 ) - int( Srf%NsSDWn1c/2 )
        Nslast  = ( NsFftp+1 ) + int( Srf%NsSDWn1c/2 )
        Srf%L1c(1:Srf%NsSDWn1c,NF,1) = dreal(Isrf(Nsfirst:Nslast))
!
        call Srf_Base_1c( Srf )
!
!       ISRF quadratic barycentre
        Ip = 2
        call barycentre_cplx( Srf%NsSDWn1c, Srf%dWn1c,          &
                              Srf%SDWn1c, Isrf(Nsfirst:Nslast), &
                              Ip, Srf%WnShift1c(NF,1)           )
        write(*,*) 'Srf%WnShift1c(NF,1)',NF,Srf%WnShift1c(NF,1)
        Srf%Fcs1c(NF,1) = Srf%WnShift1c(NF,1) / Srf%Wn0(NF)
!
!       Suface Normalisation
        Norm = sum( Srf%L1c(1:Srf%NsSDWn1c,NF,1) ) * Srf%dWn1c
        Srf%L1c(1:Srf%NsSDWn1c,NF,1) = Srf%L1c(1:Srf%NsSDWn1c,NF,1) / Norm
!
        call fwhm_real( Srf%NsSDWn1c, Srf%L1c(1,NF,1), &
                        Srf%dWn1c ,Srf%FWhm1c(NF,1)    )
        write(*,*) 'Srf%FWhm1c(NF,1)',Srf%FWhm1c(NF,1)
     end do ! End wave numbers loop
!
!    deallocation
     deallocate(Modulus)
     deallocate(Argument)
     deallocate(Opd)
     deallocate(SMod)
     deallocate(SArg)
     deallocate(G)
     deallocate(Isrf)
     return
   end subroutine srf_L1c_Perturb


!!
!!
!> srf_L1d_Perturb -- Public
!!
!! * Purpose
!!
!!     Computation of the Spectral Response Function for level 1d perturbated
!!
!! * Description
!!
!!     The IASI Level 1d ISRF is the inverse Fourier transform of a truncated 
!!     Gaussian function. This truncated Gaussian function is the truncation of 
!!     a direct Fourier transform of a Gaussian function with a FWHM given. 
!!     The ISRF 1c affected by a perturbation is computed as the real part of 
!!     the inverse Fourier transform of the product of the truncated Gaussian 
!!     function and the ratio between the self apodisation functions with 
!!     perturbation and without : for level 1d, on self apodisation functions 
!!     a spectral calibration is done.     
!!
!!     This computation requires the following steps:
!!     - After SAS coherence control, the required tables allocation, and 
!!       constants initialisation, the OPD basis for the SAF is computed 
!!     - Then, the perturb L1c function computation consist in the following steps: 
!!             - the ratio between the two SAF estimations (modulus and argument)
!!             - before a spline interpolation, a spectral calibration is applied 
!!               on the previous result
!!             - Isrf1c computation thanks to an invers Fourier transform
!!             - the usefull part extraction
!!             - the ISRF quadratic barycentre computation and a suface normalisation
!!     - at last, table deallocation is done
!!
!! * Inputs
!!
!!    - NsFft : samples number for the Level 1a FFT
!!    - NF       : frequencies number
!!    - PN       : pixel
!!    - Sas      : type_Saf / type for declaration and allocation of saf parameters
!!
!! * Inputs/outputs
!!
!!    - Srf : type_Srf / type for declaration and allocation of srf
!!
!! * Outputs
!!
!! * References
!!
   subroutine srf_L1d_Perturb( NsFft, PN,   &
                               Sas_nominal, &
                               Sas_perturb, &
                               Srf          )
   implicit none
     integer(kind=LONG)  ,intent(in)                 :: NsFft
     integer(kind=LONG)  ,intent(in)                 :: PN
     type(type_Saf)      ,intent(in)                 :: Sas_nominal
     type(type_Saf)      ,intent(in)                 :: Sas_perturb
     type(type_Srf)      ,intent(inout)              :: Srf
     real(kind=DOUBLE)   ,dimension(:) ,allocatable  :: Modulus
     real(kind=DOUBLE)   ,dimension(:) ,allocatable  :: Argument
     real(kind=DOUBLE)   ,dimension(:) ,allocatable  :: Opd
     real(kind=DOUBLE)   ,dimension(:) ,allocatable  :: SMod
     real(kind=DOUBLE)   ,dimension(:) ,allocatable  :: SArg
     complex(kind=DOUBLE),dimension(:) ,allocatable  :: G
     complex(kind=DOUBLE),dimension(:) ,allocatable  :: Isrf
     real(kind=DOUBLE)                               :: dOpd
     integer(kind=LONG)                              :: NsFftp
     integer(kind=LONG)                              :: NF
     integer(kind=LONG)                              :: Ns
     integer(kind=LONG)                              :: Nsfirst
     integer(kind=LONG)                              :: Nslast
     integer(kind=LONG)                              :: Sens
     real(kind=DOUBLE)                               :: Norm
     integer(kind=LONG)                              :: NsFit
     integer(kind=LONG)                              :: Ip
     integer(kind=LONG)                              :: ErrCode
!
!    coherence control
     if( Sas_nominal%NsOpd /= Sas_perturb%NsOpd ) then
        write(*,*) 'NsOpd Error',Sas_nominal%NsOpd,Sas_perturb%NsOpd
        write(*,*) 'srf_L1d_Perturb Fatal Error'
        call exit(1)
     end if
     if( Sas_nominal%NsWn0 /= Sas_perturb%NsWn0 ) then
        write(*,*) 'NsWn0 Error',Sas_nominal%NsWn0,Sas_perturb%NsWn0
        write(*,*) 'srf_L1d_Perturb Fatal Error'
        call exit(1)
     end if
!
!    allocation
     write(*,*) 'Sas_nominal%NsOpd',Sas_nominal%NsOpd
     allocate(Modulus(Sas_nominal%NsOpd)  ,stat=ErrCode )
     allocate(Argument(Sas_nominal%NsOpd) ,stat=ErrCode )
     if (ErrCode > 0) then
        write(0,*) 'allocation srf_L1d_Perturb Error'
        write(0,*) 'srf_L1d_Perturb : fatal error'
        call exit(1)
     end if
     allocate(Opd(NsFft+1) ,stat=ErrCode )
     allocate(SMod(NsFft+1) ,stat=ErrCode )
     allocate(SArg(NsFft+1) ,stat=ErrCode )
     allocate(G(NsFft+1)   ,stat=ErrCode )
     allocate(Isrf(NsFft+1),stat=ErrCode )
!
     if (ErrCode > 0) then
        write(0,*) 'allocation srf_L1d_Perturb Error'
        write(0,*) 'srf_L1d_Perturb : fatal error'
        call exit(1)
     end if
!
!    initialisation des constantes
     NsFftp = int( NsFft/2 )
     dOpd = 1.d+00 / ( Srf%dWn1c * dble(NsFft))
!
!    base opd du module de Saf interpole sur le FFT
     Opd(1:NsFft+1) = (/ (dble(Ns-NsFftp-1),Ns=1,NsFft+1) /) * dOpd
!
!    Perturb L1d function computation
     do NF = 1, Srf%NsWn0
!
!       Ratio between the two SAF estimations
        Modulus(1:Sas_nominal%NsOpd) =                     &
                Sas_perturb%Mod(1:Sas_perturb%NsOpd,NF,PN) &
              / Sas_nominal%Mod(1:Sas_nominal%NsOpd,NF,PN) &
              * Srf%L1cTF(1:Sas_nominal%NsOpd)
        Argument(1:Sas_nominal%NsOpd) =                    &
                Sas_perturb%Arg(1:Sas_perturb%NsOpd,NF,PN) &
              - Sas_nominal%Arg(1:Sas_nominal%NsOpd,NF,PN)
!
!       Spectral calibration
        NsFit = 50
        call extract_linear_argument( Sas_nominal%NsOpd, &
                                      Sas_nominal%Opd,   &
                                      NsFit,             &
                                      Modulus,           &
                                      Argument           )
        
!
!       Spline interpolation
        call intspline( Sas_nominal%NsOpd, Sas_nominal%Opd, Modulus, &
                        NsFft+1, Opd, SMod )
        call intspline( Sas_nominal%NsOpd, Sas_nominal%Opd, Argument, &
                        NsFft+1, Opd, SArg )
        call modargtocplx( G, SMod, SArg, NsFft+1 )
        do Ns = 1, NsFft+1
           if( dabs(Opd(Ns)) .gt. Srf%Opd_effective ) then
              G(Ns) = dcmplx(0.d+00,0.d+00)
           end if
        end do
!       Isrf1c computation
        Sens = -1
        call fft_c2c( NsFft, Sens, G, dOpd, Srf%dWn1c, Isrf )
!
!       Usefull part extraction
        Nsfirst = ( NsFftp+1 ) - int( Srf%NsSDWn1c/2 )
        Nslast  = ( NsFftp+1 ) + int( Srf%NsSDWn1c/2 )
        Srf%L1c(1:Srf%NsSDWn1c,NF,1) = dreal(Isrf(Nsfirst:Nslast))
!
        call Srf_Base_1c( Srf )
!
!       ISRF quadratic barycentre
        Ip = 2
        call barycentre_cplx( Srf%NsSDWn1c, Srf%dWn1c,          &
                              Srf%SDWn1c, Isrf(Nsfirst:Nslast), &
                              Ip, Srf%WnShift1c(NF,1)           )
        write(*,*) 'Srf%WnShift1c(NF,1)',NF,Srf%WnShift1c(NF,1)
        Srf%Fcs1c(NF,1) = Srf%WnShift1c(NF,1) / Srf%Wn0(NF)
!
!       Suface Normalisation
        Norm = sum( Srf%L1c(1:Srf%NsSDWn1c,NF,1) ) * Srf%dWn1c
        Srf%L1c(1:Srf%NsSDWn1c,NF,1) = Srf%L1c(1:Srf%NsSDWn1c,NF,1) / Norm
!
        call fwhm_real( Srf%NsSDWn1c, Srf%L1c(1,NF,1), &
                        Srf%dWn1c ,Srf%FWhm1c(NF,1)    )
        write(*,*) 'Srf%FWhm1c(NF,1)',Srf%FWhm1c(NF,1)
     end do ! End wave numbers loop
!
!    deallocation
     deallocate(Modulus)
     deallocate(Argument)
     deallocate(Opd)
     deallocate(SMod)
     deallocate(SArg)
     deallocate(G)
     deallocate(Isrf)
     return
   end subroutine srf_L1d_Perturb
!
!
end module srf_computation_module
