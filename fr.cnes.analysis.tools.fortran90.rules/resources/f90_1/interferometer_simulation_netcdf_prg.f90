!!# interferometer_simulation_netcdf_prg.f90 --
!!#
!!#           Project: SPS_GENERIC
!!#           Authors: NOVELTIS/JD
!!#              Date: july 2013
!
!!#
!!
!> interferometer simulation -- Module
!!
!! * Purpose
!!
!!   Program for interferometer simulation with netCDF interfaces.
!!
!! * Description
!!        
!!   This principal program allows interferograms simulation; it provides an interferograms
!!   collection. This computation requires the following steps:    
!!     - the first one is the read of inputs parameters; 
!!     - then, initialisation of noise generator is required, as well as 
!!       SRF parameters initialisation;
!!     - then, spectral RPD function and spectral pixel function are read;
!!     - gain file parameters, detection file parameters, amplification file parameters
!!       and adc conversion parameters are also read;
!!     - before a spectral bands loop, all required tables are allocated; then inside the loop:
!!              - in order to compute spectra for the cold space, the hot blackbody and 
!!                the earth view, detection gain is initialised as well as detection 
!!                parameters, amplification parameters and ADC conversion parameters.
!!              - for each of CS, BB and EWs target, spectrum are simulated.
!!              - thanks to a sub pixels loop, a interferogram triplet simulation is realised, 
!!                and for each target interferogram are written.
!!     - at last, all tables are deallocated
!!
!! * References
!!
!!

program interferometer_simulation_netcdf_prg
   use precision_type
   use error_type
   use interf_param_type
   use scene_param_type
   use saf_type
   use gain_type
   use emissivity_type
   use detection_type
   use amplification_type
   use spike_injection_type
   use adc_convert_type
   use non_linearity_type
   use interferogram_type
   use spectrum_type
   use math_module
   use interferometer_init_module
   use spectrum_module
   use interferogram_module
   use gain_module
   use detection_module
   use emissivity_module
   use amplification_module
   use spike_injection_module
   use adc_convert_module
   use non_linearity_module
   use math_module
   use fft_module
   use plot_module
!
   implicit none
     type(type_Interf_Param)                            :: Interf_Param
     type(type_Gain)           ,dimension(:),allocatable:: Gain
     type(type_Emissivity)     ,dimension(:),allocatable:: Emissivity
     type(type_Detection)      ,dimension(:),allocatable:: Detection
     type(type_non_linearity)  ,dimension(:),allocatable:: Non_Linearity
     type(type_Adc_Convert_SB)                          :: Adc_Convert_SB
     type(type_Amplification)  ,dimension(:),allocatable:: Amplification
     type(type_Spike_Injection),dimension(:,:),allocatable::Spike_Injection_CS
     type(type_Spike_Injection),dimension(:),allocatable:: Spike_Injection_BB
     type(type_Spike_Injection),dimension(:),allocatable:: Spike_Injection_EW
     type(type_Scene_Param)                             :: Scene_Param
     type(type_Spectrum)                                :: Spectrum_BG
     type(type_Spectrum)       ,dimension(:),allocatable:: Spectrum_CS
     type(type_Spectrum)                                :: Spectrum_BB
     type(type_Spectrum)       ,dimension(:),allocatable:: Spectrum_EW
     type(type_Saf)                                     :: Saf_Rpd
     type(type_Saf)            ,dimension(:),allocatable:: Saf
     type(type_spect_interf_index)                      :: InfoIn    ! general fields
     type(type_spect_interf_index)                      :: InfoInCS1 ! top level fields for interf CS1
     type(type_spect_interf_index)                      :: InfoInCS2 ! top level fields for interf CS2
     type(type_spect_interf_index)                      :: InfoInBB  ! top level fields for interf BB
     type(type_spect_interf_index) ,dimension(:),allocatable :: InfoInEW ! top level fields for interf EW
     type(type_spect_interf_index)                      :: InfoInRPD  ! top level fields for interf RPD
     type(type_isrf_index)                              :: InfoSaf    ! top level fields for SAF
     integer(kind=LONG)                                 :: N_ColdSpace
     integer(kind=LONG)                                 :: CS
     integer(kind=LONG)                                 :: SC
     integer(kind=LONG)                                 :: SB
     integer(kind=LONG)        ,dimension(:),allocatable:: SB_List
     integer(kind=LONG)                                 :: SUB_PN
     integer(kind=LONG)                                 :: NADC    
     integer(kind=LONG)                                 :: ErrCode
     integer(kind=LONG)                                 :: ioalloc
     integer(kind=LONG)                                 :: iPos
     integer(kind=LONG)                                 :: iFile
     integer(kind=LONG)                                 :: PN_Mes
     type(type_Interferogram)                           :: Interf_Cont_Rpd
     type(type_Interferogram)                           :: Interf_Samp_Rpd
     type(type_Interferogram),dimension(:,:,:),allocatable :: Interf_Samp_CS
     type(type_Interferogram),dimension(:,:),allocatable   :: Interf_Samp_BB
     type(type_Interferogram),dimension(:,:,:),allocatable :: Interf_Samp_EW
     character(len=2)                                  :: Interf_Target
     character(len=2)                                  :: Target_Type
     character(len=60)                                 :: Date
     character(len=2)                                  :: SubPixel
     character(len=4)                                  :: Pixel
     character(len=1)                                  :: Band
     character(len=2)                                  :: Scene
!
     character(len=500)                                :: Files_Srf
     character(len=500)                                :: File_Saf_Rpd
     character(len=500)                                :: File_Saf
     character(len=500)                                :: Files_Gain
     character(len=500)  ,dimension(:) ,allocatable    :: File_Gain
     character(len=500)                                :: Files_Interf_Param
     character(len=500)                                :: Files_Detection_Param
     character(len=500)  ,dimension(:) ,allocatable    :: File_Detection_Param
     character(len=500)                                :: Files_Emissivity_Param
     character(len=500)  ,dimension(:) ,allocatable    :: File_Emissivity_Param
     character(len=500)                                :: Files_Non_Linearity_Param
     character(len=500)  ,dimension(:) ,allocatable    :: File_Non_Linearity_Param
     character(len=500)                                :: Files_Amplification_Param
     character(len=500)  ,dimension(:) ,allocatable    :: File_Amplification_Param
     character(len=500)                                :: Files_Spike_Injection_Param
     character(len=500)  ,dimension(:,:) ,allocatable  :: File_Spike_Injection_CS
     character(len=500)  ,dimension(:) ,allocatable    :: File_Spike_Injection_BB
     character(len=500)  ,dimension(:) ,allocatable    :: File_Spike_Injection_EW
     character(len=500)                                :: Files_Adc_Convert_Param
     character(len=500)                                :: Files_Scene_Param
     character(len=500)                                :: File_Scene_Param
     character(len=500)                                :: File_OutPut_
     character(len=500)                                :: File_Spectrum_hr
     character(len=500)                                :: File_Header
     integer(kind=SHORT)                               :: init_interf_CS1, init_interf_CS2, init_interf_BB
     integer(kind=SHORT)                               :: init_interf_EW, init_interf_RPD
!
!    read inputs parameters
     read(*,fmt='(a)',err=999) Files_Interf_Param
     read(*,fmt='(a)',err=999) Files_Gain
     read(*,fmt='(a)',err=999) Files_Emissivity_Param
     read(*,fmt='(a)',err=999) Files_Detection_Param
     read(*,fmt='(a)',err=999) Files_Non_Linearity_Param
     read(*,fmt='(a)',err=999) Files_Amplification_Param
     read(*,fmt='(a)',err=999) Files_Spike_Injection_Param
     read(*,fmt='(a)',err=999) Files_Adc_Convert_Param
     read(*,fmt='(a)',err=999) Files_Scene_Param
     read(*,fmt='(a)',err=999) Files_Srf
     read(*,fmt='(a)',err=999) File_OutPut_
     read(*,fmt='(a)',err=999) File_Header
     read(*,fmt='(a)',err=999) Date
     write(*,'(a)') 'Files_Interf_Param',&
         Files_Interf_Param(1:len_trim(Files_Interf_Param))
     write(*,'(a)') 'Files_Gain',&
         Files_Gain(1:len_trim(Files_Gain))
     write(*,'(a)') 'Files_Emissivity_Param',&
         Files_Emissivity_Param(1:len_trim(Files_Emissivity_Param))
     write(*,'(a)') 'Files_Detection_Param',&
         Files_Detection_Param(1:len_trim(Files_Detection_Param))
     write(*,'(a)') 'Files_Non_Linearity_Param',&
         Files_Non_Linearity_Param(1:len_trim(Files_Non_Linearity_Param))
     write(*,'(a)') 'Files_Amplification_Param',&
         Files_Amplification_Param(1:len_trim(Files_Amplification_Param))
     write(*,'(a)') 'Files_Spike_Injection_Param',&
         Files_Spike_Injection_Param(1:len_trim(Files_Spike_Injection_Param))
     write(*,'(a)') 'Files_Adc_Convert_Param',&
         Files_Adc_Convert_Param(1:len_trim(Files_Adc_Convert_Param))
     write(*,'(a)') 'Files_Scene_Param',&
         Files_Scene_Param(1:len_trim(Files_Scene_Param))
     write(*,'(a)') 'Files_Srf',&
         Files_Srf(1:len_trim(Files_Srf))
     write(*,'(a)') 'File_Header',&
         File_Header(1:len_trim(File_Header))
!
!    noise generator initialisation
     write(*,'(a)') Date
     call grnd_init(Date)
!
     iFile = 10
     ioalloc = 0
     init_interf_CS1 = 0
     init_interf_CS2 = 0
     init_interf_BB = 0
     init_interf_EW = 0
     init_interf_RPD = 0
!
!
     ! InfoIn initialization (header)
     call read_header ( File_Header, InfoIn%header)
     InfoIn%header%Operator = "INS"
     InfoIn%header%ProcessingStep = "interferometer simulation"
!
!    Interferogram parameters initialisation
     call interferometer_def( Files_Interf_Param, Interf_Param )
!
!    Scene parameters initialisation
     iPos = 3
     open(unit=iFile, file=Files_Scene_Param, status='old', err=999)
     iPos = 4
     read(iFile,*,err=999) N_ColdSpace
     read(iFile,'(a)',err=999) File_Scene_Param
     close(unit=iFile)
     call scene_mixing_def( File_Scene_Param,&
                            Scene_Param      )
!
!    Srf parameters initialisation
     iPos = 3
     open(unit=iFile, file=Files_Srf, status='old', err=999)
     iPos = 4
     allocate(SB_List(Interf_Param%Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     read(iFile,*,err=999) PN_Mes
     read(iFile,'(a)',err=999) File_Saf_Rpd
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     read(iFile,*,err=999) SB_List(1:Interf_Param%Nband)
     read(iFile,'(a)',err=999) File_Saf
     close(unit=iFile)
!
!    Spectral Rpd function reading
     ! Saf_Rpd initialization
     Saf_Rpd%TopLevelSaf%header%filename = File_Saf_Rpd
     write(*,'(a)') 'Saf_Rpd', trim(Saf_Rpd%TopLevelSaf%header%filename)
     Saf_Rpd%TopLevelSaf%Band = 1
     Saf_Rpd%TopLevelSaf%NcolFov = 1
     Saf_Rpd%TopLevelSaf%NlinFov = 1
     Saf_Rpd%TopLevelSaf%ColFov  = 1
     Saf_Rpd%TopLevelSaf%LinFov  = 1
     call readsaf_netcdf( Saf_Rpd, ErrCode )
     if( ErrCode /= 0 ) then
        write(*,*) 'Read Saf_Rpd ERROR'
        call exit(1)
     end if
!
!    Spectral Pixel function reading
     ! Saf initialization
     allocate( Saf(Interf_Param%Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     InfoSaf%header%filename = File_Saf

     do SB = 1, Interf_Param%Nband
        Saf(SB)%TopLevelSaf%Band = SB_List(SB)
        Saf(SB)%TopLevelSaf%header%filename = InfoSaf%header%filename
        Saf(SB)%TopLevelSaf%NcolFov = 1
        Saf(SB)%TopLevelSaf%NlinFov = 1
        Saf(SB)%TopLevelSaf%ColFov  = 1
        Saf(SB)%TopLevelSaf%LinFov  = 1
        write(*,'(a)') 'Saf', trim(Saf(SB)%TopLevelSaf%header%filename)
        call readsaf_netcdf( Saf(SB), ErrCode )
        if( ErrCode /= 0 ) then
           write(*,*) 'Read Saf ERROR SB :',SB
           call exit(1)
        end if
     end do
!
!    gain file parameters reading
     iPos = 5
     allocate(File_Gain(Interf_Param%Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     allocate(Gain(Interf_Param%Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     open(iFile, file=Files_Gain, status='old', err=999)
     do SB = 1, Interf_Param%Nband
        read(iFile,'(a)',err=999) File_Gain(SB)
        Gain(SB)%filename = File_Gain(SB)
        call readgain( Gain(SB), ErrCode )
        if( ErrCode /= 0 ) then
           write(*,*) ' readgain Fatal Error',&
                        File_Gain(SB)(1:len_trim(File_Gain(SB)))
           go to 999
        end if
     end do
     close(iFile)
     deallocate( File_Gain )
!
!    emissivity file parameters reading
     iPos = 6
     allocate(File_Emissivity_Param(Interf_Param%Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     allocate(Emissivity(Interf_Param%Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     open(iFile, file=Files_Emissivity_Param, status='old', err=999)
     do SB = 1, Interf_Param%Nband
        read(iFile,'(a)',err=999) File_Emissivity_Param(SB)
     end do
     close(iFile)
!
!    detection file parameters reading
     iPos = 7
     allocate(File_Detection_Param(Interf_Param%Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     allocate(Detection(Interf_Param%Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     open(iFile, file=Files_Detection_Param, status='old', err=999)
     do SB = 1, Interf_Param%Nband
        read(iFile,'(a)',err=999) File_Detection_Param(SB)
     end do
     close(iFile)
!
!    amplification file parameters reading
     iPos = 8
     allocate(File_Amplification_Param(Interf_Param%Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     allocate(Amplification(Interf_Param%Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     open(iFile, file=Files_Amplification_Param, status='old', err=999)
     do SB = 1, Interf_Param%Nband
        read(iFile,'(a)',err=999) File_Amplification_Param(SB)
     end do
     close(iFile)
!
!    spike injection file parameters reading
     iPos = 9
     allocate(File_Spike_Injection_CS(Interf_Param%Nband,N_ColdSpace), &
                                                          stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     allocate(File_Spike_Injection_BB(Interf_Param%Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     allocate(File_Spike_Injection_EW(Interf_Param%Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     allocate(Spike_Injection_CS(Interf_Param%Nband,N_ColdSpace), &
                                                     stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     allocate(Spike_Injection_BB(Interf_Param%Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     allocate(Spike_Injection_EW(Interf_Param%Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     open(iFile, file=Files_Spike_Injection_Param, status='old', err=999)
     do SB = 1, Interf_Param%Nband
        do CS = 1, N_ColdSpace
           read(iFile,'(a)',err=999) File_Spike_Injection_CS(SB,CS)
        end do
        read(iFile,'(a)',err=999) File_Spike_Injection_BB(SB)
        read(iFile,'(a)',err=999) File_Spike_Injection_EW(SB)
     end do
     close(iFile)
!
!    adc conversion parameters reading
     iPos = 10
     open(iFile, file=Files_Adc_Convert_Param, status='old', err=999)
     Adc_Convert_SB%Nband = Interf_Param%Nband
     call alloc_Adc_Convert_SB( Adc_Convert_SB )
     do SB = 1, Interf_Param%Nband
        read(iFile,*,err=999) Adc_Convert_SB%Adc_Convert_Nadc(SB)%Nadc
        write(*,*) 'Nadc ',Adc_Convert_SB%Adc_Convert_Nadc(SB)%Nadc
        call alloc_Adc_Convert_Nadc(Adc_Convert_SB%Adc_Convert_Nadc(SB))      
        do NADC = 1, Adc_Convert_SB%Adc_Convert_Nadc(SB)%Nadc
           read(iFile,'(a)',err=999) Adc_Convert_SB%Adc_Convert_Nadc(SB)&
                                              %Adc_Convert(NADC)%filename
           write(*,'(a)') Adc_Convert_SB%Adc_Convert_Nadc(SB)    &
                          %Adc_Convert(NADC)%filename(1:len_trim(&
                          Adc_Convert_SB%Adc_Convert_Nadc(SB)    &
                          %Adc_Convert(NADC)%filename))
        end do
     end do
     close(iFile)
!
!    Non linearity parameters reading
     iPos = 11
     allocate(File_Non_Linearity_Param(Interf_Param%Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     allocate(Non_Linearity(Interf_Param%Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     open(iFile, file=Files_Non_Linearity_Param, status='old', err=999)
     do SB = 1, Interf_Param%Nband
        read(iFile,'(a)',err=999) File_Non_Linearity_Param(SB)
     end do
     close(iFile)

!
!    allocation
     allocate( Interf_Samp_CS(Saf(1)%NsPixel,Interf_Param%Nband, &
                                      N_ColdSpace), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     allocate( Interf_Samp_BB(Saf(1)%NsPixel,Interf_Param%Nband), &
                                                     stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     allocate( Interf_Samp_EW(Scene_Param%NbScene,Saf(1)%NsPixel,&
                               Interf_Param%Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     allocate( Spectrum_CS(N_ColdSpace), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     allocate( Spectrum_EW(Scene_Param%NbScene), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
!
     if( ioalloc /= 0 ) then
        write(*,*) 'Allocation Error',ioalloc
        call exit(1)
     end if
!
!    Spectral bands loop
     do SB = 1, Interf_Param%Nband
!
!       Emissivity parameters initialisation
        call emissivity_init( File_Emissivity_Param(SB),&
                              Emissivity(SB)            )
!
!       Detection parameters initialisation
        call detection_init( File_Detection_Param(SB),&
                             Detection(SB)            )
!
!       Non-linearity parameters initialisation
        call non_linearity_init( File_Non_Linearity_Param(SB),&
                                 Non_Linearity(SB)            )
!
!       Amplification parameters initialisation
        call amplification_init( File_Amplification_Param(SB),&
                                 Amplification(SB)            )
!
!       Spike injection parameters initialisation
        do CS = 1, N_ColdSpace
           call spike_injection_init( File_Spike_Injection_CS(SB,CS), &
                                      Spike_Injection_CS(SB,CS)       )
        end do
        call spike_injection_init( File_Spike_Injection_BB(SB), &
                                   Spike_Injection_BB(SB)       )
        call spike_injection_init( File_Spike_Injection_EW(SB), &
                                   Spike_Injection_EW(SB)       )
!
!       ADC conversion parameters initialisation
        do NADC = 1, Adc_Convert_SB%Adc_Convert_Nadc(SB)%Nadc
           call adc_convert_init( Adc_Convert_SB%Adc_Convert_Nadc(SB)&
                                         %Adc_Convert(NADC)%filename,&
                Adc_Convert_SB%Adc_Convert_Nadc(SB)%Adc_Convert(NADC) )
        end do
    end do

    write(Pixel,'(i4.4)') PN_Mes

    ! InfoIn initialization (NBand,NcolFov,NlinFov,ColFov,LinFov)
    InfoIn%NBand = Interf_Param%Nband
    InfoIn%NcolFov = 1
    InfoIn%NlinFov = 1
    InfoIn%ColFov = 1
    InfoIn%LinFov = PN_Mes

   ! Target_CS1 output
   InfoInCS1%header%filename = File_OutPut_(1:len_trim(File_OutPut_))&
      //'Target_CS1'//'_PN'//Pixel//'.nc'
   InfoInCS1%Target = Interf_Param%Target_CS
   if ( N_ColdSpace == 2 ) then
     InfoInCS2%header%filename = File_OutPut_(1:len_trim(File_OutPut_))&
        //'Target_CS2'//'_PN'//Pixel//'.nc'
     InfoInCS2%Target = Interf_Param%Target_CS
   end if
   InfoInBB%header%filename = File_OutPut_(1:len_trim(File_OutPut_))&
       //'Target_BB'//'_PN'//Pixel//'.nc'
   InfoInBB%Target = Interf_Param%Target_BB
   InfoInRPD%header%filename = File_OutPut_(1:len_trim(File_OutPut_))&
       //'Target_RPD'//'_PN'//Pixel//'.nc'
   ! MT = metrology
   InfoInRPD%Target = "MT"
   allocate( InfoInEW(Scene_Param%NbScene), stat=ErrCode )
   if ( ErrCode /= 0 ) then
      write(*,*) 'Allocation Error InfoInEW'
      call exit(1)
   end if
   do SC = 1, Scene_Param%NbScene
      write(Scene,'(i2.2)') SC
      InfoInEW(SC)%Target = Interf_Param%Target_EW
      if ( Interf_Param%Target_EW == 'AT' ) then
         InfoInEW(SC)%header%filename = File_OutPut_(1:len_trim(File_OutPut_))&
            //'Target_EW_SC'//Scene//'_PN'//Pixel//'.nc'
      else if( Interf_Param%Target_EW == 'LZ' ) then
         InfoInEW(SC)%header%filename = File_OutPut_(1:len_trim(File_OutPut_))&
           //'Target_EW_LZ'//Scene//'_PN'//Pixel//'.nc'
      else if( Interf_Param%Target_EW == 'CN' ) then
         InfoInEW(SC)%header%filename = File_OutPut_(1:len_trim(File_OutPut_))&
           //'Target_EW_CN'//Scene//'_PN'//Pixel//'.nc'
      else if( Interf_Param%Target_EW == 'FT' ) then
         InfoInEW(SC)%header%filename = File_OutPut_(1:len_trim(File_OutPut_))&
           //'Target_EW_FT'//Scene//'_PN'//Pixel//'.nc'
      else
         write(*,*) 'Target_EW Error',Interf_Param%Target_EW
         call exit(1)
      end if
   end do
!
!    Spectral bands loop
     do SB = 1, Interf_Param%Nband
        write(Band,'(i1.1)') SB_List(SB)
        ! InfoIn initialization (Band,NSubFov)
        InfoIn%Band = SB_List(SB)
        InfoIn%NsubFov = Saf(SB)%NsPixel
!        case calibration target
        File_Spectrum_hr = Scene_Param%hr_filename(1)
!
!       Cold space spectrum
        Interf_Target = Interf_Param%Target_CS
        Target_Type   = 'C1'
        call target_simulation( Interf_Param,    &
                                Emissivity(SB),  &
                                Detection(SB),   &
                                File_Spectrum_hr,&
                                Target_Type,     &
                                SB,              &
                                Spectrum_BG,     &
                                Spectrum_CS(1)   )
!
!       Hot blackbody spectrum
        Interf_Target = Interf_Param%Target_BB
        Target_Type   = 'BB'
        call target_simulation( Interf_Param,    &
                                Emissivity(SB),  &
                                Detection(SB),   &
                                File_Spectrum_hr,&
                                Target_Type,     &
                                SB,              &
                                Spectrum_BG,     &
                                Spectrum_BB      )
!
!       Earth view spectrum
        do SC = 1, Scene_Param%NbScene
           Interf_Target = Interf_Param%Target_EW
           Target_Type   = 'EW'
           File_Spectrum_hr = Scene_Param%hr_filename(SC)
           call target_simulation( Interf_Param,    &
                                   Emissivity(SB),  &
                                   Detection(SB),   &
                                   File_Spectrum_hr,&
                                   Target_Type,     &
                                   SB,              &
                                   Spectrum_BG,     &
                                   Spectrum_EW(SC)  )
        end do
!
!       Sub Pixels loop
        do SUB_PN = 1, Saf(SB)%NsPixel
           write(0,*) '**************SB*******SUB_PN : ',SB,SB_List(SB),SUB_PN
           write(SubPixel,'(i2.2)') SUB_PN
           ! InfoIn initialization (SubFov)
           InfoIn%SubFov = SUB_PN
!
!          Interferogram triplet simulation
           select case( N_ColdSpace )
           case ( 1 )
              call interferogram_scene( Interf_Param,               &
                                        Scene_Param,                &
                                        Gain(SB),                   &
                                        Detection(SB),              &
                                        Non_Linearity(SB),          &
                                        Amplification(SB),          &
                                        Spike_Injection_CS(SB,1),   &
                                        Spike_Injection_BB(SB),     &
                                        Spike_Injection_EW(SB),     &
                                        Adc_Convert_SB%Adc_Convert_Nadc(SB), &
                                        Spectrum_BG,                &
                                        Spectrum_CS(1),             &
                                        Spectrum_BB,                &
                                        Spectrum_EW,                &
                                        Saf_Rpd,                    &
                                        Saf(SB),                    &
                                        SUB_PN,                     &
                                        SB,                         &
                                        Interf_Cont_Rpd,            &
                                        Interf_Samp_Rpd,            &
                                        Interf_Samp_CS(SUB_PN,SB,1),&
                                        Interf_Samp_BB(SUB_PN,SB),  &
                                        Interf_Samp_EW(1,SUB_PN,SB) )
           case default
              write(*,*) ' Wrong number of Cold Space',N_ColdSpace
              call exit(1)
           end select
!
!          write CS1 binary Interferogram
           Interf_Samp_CS(SUB_PN,SB,1)%TopLevelIn = InfoIn
           Interf_Samp_CS(SUB_PN,SB,1)%TopLevelIn%header%filename = InfoInCS1%header%filename
           Interf_Samp_CS(SUB_PN,SB,1)%TopLevelIn%Target = InfoInCS1%Target
           write(0,'(a)') trim(Interf_Samp_CS(SUB_PN,SB,1)%TopLevelIn%header%filename)
           call writeinterferogram_netcdf( Interf_Samp_CS(SUB_PN,SB,1), ErrCode, init_interf_CS1 )
           if( ErrCode /= 0 ) then
              write(*,*) 'Write interf CS1 Error'
              call exit(1)
           end if
!
!
!          write BB binary Interferogram
           Interf_Samp_BB(SUB_PN,SB)%TopLevelIn = InfoIn
           Interf_Samp_BB(SUB_PN,SB)%TopLevelIn%header%filename = InfoInBB%header%filename
           Interf_Samp_BB(SUB_PN,SB)%TopLevelIn%Target = InfoInBB%Target
           write(0,'(a)') trim(Interf_Samp_BB(SUB_PN,SB)%TopLevelIn%header%filename)
           call writeinterferogram_netcdf( Interf_Samp_BB(SUB_PN,SB), ErrCode, init_interf_BB )
           if( ErrCode /= 0 ) then
              write(*,*) 'Write BB Error'
              call exit(1)
           end if
!
!          write EW binary Interferogram
           do SC = 1, Scene_Param%NbScene
              write(Scene,'(i2.2)') Scene_Param%Scene_List(SC)

              Interf_Samp_EW(SC,SUB_PN,SB)%TopLevelIn = InfoIn
              Interf_Samp_EW(SC,SUB_PN,SB)%TopLevelIn%header%filename = InfoInEW(SC)%header%filename
              Interf_Samp_EW(SC,SUB_PN,SB)%TopLevelIn%Target = InfoInEW(SC)%Target
              write(0,'(a)') trim(Interf_Samp_EW(SC,SUB_PN,SB)%TopLevelIn%header%filename)
              call writeinterferogram_netcdf( Interf_Samp_EW(SC,SUB_PN,SB), ErrCode, init_interf_EW )
              if( ErrCode /= 0 ) then
                 write(*,*) 'Write EW Error SC',SC
                 call exit(1)
              end if
              call dalloc_Interferogram( Interf_Samp_EW(SC,SUB_PN,SB) )
           end do
           call dalloc_Interferogram( Interf_Samp_CS(SUB_PN,SB,1) )
           call dalloc_Interferogram( Interf_Samp_BB(SUB_PN,SB) )
!
        end do ! end subpixels loop
        call dalloc_Spectrum( Spectrum_BG )
        call dalloc_Spectrum( Spectrum_CS(1) )
        call dalloc_Spectrum( Spectrum_BB )
        do SC = 1, Scene_Param%NbScene
           call dalloc_Spectrum( Spectrum_EW(SC) )
        end do
!
     end do ! end spectral bands loop
!

!
     ! InfoIn initialization for Interf_Samp_Rpd
     InfoIn%Nband = 1
     InfoIn%Band = 1
     InfoIn%NSubFov = Saf_Rpd%NsPixel
     InfoIn%SubFov = 1
     Interf_Samp_Rpd%TopLevelIn = InfoIn
     Interf_Samp_Rpd%TopLevelIn%header%filename = InfoInRPD%header%filename
     Interf_Samp_Rpd%TopLevelIn%Target = InfoInRPD%Target
     write(0,'(a)') trim(Interf_Samp_Rpd%TopLevelIn%header%filename)
     call writeinterferogram_netcdf( Interf_Samp_Rpd, ErrCode, init_interf_RPD )
     if( ErrCode /= 0 ) then
        write(*,*) 'Write RPD Error'
        call exit(1)
     end if
!
     write(*,*) 'End interferometer simulation'
!
!    deallocation
     call dalloc_Interferogram( Interf_Cont_Rpd )
     call dalloc_Interferogram( Interf_Samp_Rpd )
     call dalloc_Saf( Saf_Rpd )
     do SB = 1, Interf_Param%Nband ! boucle sur les bandes
        call dalloc_Saf( Saf(SB) )
        call dalloc_Detection( Detection(SB) )
        call dalloc_Gain( Gain(SB) )
        if( Spike_Injection_CS(SB,1)%NSpike /= 0 ) then
           call dalloc_Spike_Injection( Spike_Injection_CS(SB,1) )
        end if
        if( Spike_Injection_BB(SB)%NSpike /= 0 ) then
           call dalloc_Spike_Injection( Spike_Injection_BB(SB) )
        end if
        if( Spike_Injection_EW(SB)%NSpike /= 0 ) then
           call dalloc_Spike_Injection( Spike_Injection_EW(SB) )
        end if
        call dalloc_non_linearity( Non_Linearity(SB) )
        call dalloc_Adc_Convert_Nadc( Adc_Convert_SB%Adc_Convert_Nadc(SB) )
     end do  ! fin de boucle sur les bandes
     deallocate( Saf )
     deallocate( Interf_Samp_CS )
     deallocate( Interf_Samp_BB )
     deallocate( Interf_Samp_EW )
     deallocate( Gain )
     deallocate( Detection )
     deallocate( Amplification )
     deallocate( Spike_Injection_CS )
     deallocate( Spike_Injection_BB )
     deallocate( Spike_Injection_EW )
     deallocate( Non_Linearity )
     call dalloc_Adc_Convert_SB( Adc_Convert_SB )
     deallocate( File_Spike_Injection_CS )
     deallocate( File_Spike_Injection_BB )
     deallocate( File_Spike_Injection_EW )
     call exit(0)
999 write(*,*) 'interferometer_simulation_netcdf_prg Fatal error',iPos
    call exit(1)
!
end program interferometer_simulation_netcdf_prg
