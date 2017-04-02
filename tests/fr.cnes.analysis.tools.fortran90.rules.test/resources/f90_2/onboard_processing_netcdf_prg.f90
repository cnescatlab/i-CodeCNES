!!# onboard_processing_prg.f90 --
!!#
!!#           Project: SPS_GENERIC
!!#           Authors: NOVELTIS/B.TOURNIER
!!#              Date: september 2011
!!#           Version: $Revision: 1.1 $
!!# Last modification: $Date: 2011-11-02 14:57:52 $
!!#
!!
!> onboard_processing_prg -- Program
!!
!! * Purpose
!!
!!
!! * References
!!


program onboard_processing_prg
   use precision_type
   use error_type
   use interf_param_type
   use amplification_type
   use adc_convert_type
   use non_linearity_type
   use scene_param_type
   use resampling_param_type
   use decim_one_fir_type
   use gain_type
   use interferogram_type
   use spectrum_type
   use zpd_type
   use dc_component_type
   use spike_detection_type
   use bit_trimming_type
   use math_module
   use interferometer_init_module
   use scene_mixing_module
   use gain_module
   use amplification_module
   use adc_convert_module
   use dc_component_module
   use spike_detection_module
   use non_linearity_module
   use non_linearity_correction_module
   use resampling_module
   use decimation_module
   use bilatere_module
   use aggregation_module
   use nzpd_module
   use bit_trimming_module
   use plot_module
!
   implicit none
     type(type_Interf_Param)                             :: Interf_Param
     type(type_Scene_Param)     ,dimension(:),allocatable:: Scene_Param
     type(type_Adc_Convert_SB)                           :: Adc_Convert_SB
     type(type_Gain)            ,dimension(:),allocatable:: Gain
     type(type_Amplification)   ,dimension(:),allocatable:: Amplification
     type(type_non_linearity)   ,dimension(:),allocatable:: Non_Linearity
     type(type_Spike_Detection) ,dimension(:),allocatable:: Spike_Detect
     type(type_Resampling_Param),dimension(:),allocatable:: ReSamp_Param
     type(type_Decim_One_fir)   ,dimension(:),allocatable:: Decim_One_fir
     type(type_Bit_Trimming)    ,dimension(:),allocatable:: Bit_Trimming
     integer(kind=LONG)                                  :: Nband
     integer(kind=LONG)                                  :: N_ColdSpace
     integer(kind=LONG)                                  :: CS
     integer(kind=LONG)                                  :: SB
     integer(kind=LONG)         ,dimension(:),allocatable:: SB_List
     integer(kind=LONG)         ,dimension(:),allocatable:: Scene_List
     integer(kind=LONG)                                  :: SC
     integer(kind=LONG)                                  :: Nb_SUB_PN
     integer(kind=LONG)                                  :: Nb_Scene
     integer(kind=LONG)                                  :: SUB_PN
     integer(kind=LONG)                                  :: NADC    
     integer(kind=LONG)                                  :: ErrCode
     integer(kind=LONG)                                  :: ioalloc
     integer(kind=LONG)                                  :: iPos
     integer(kind=LONG)                                  :: iFile
     integer(kind=LONG)                                  :: PN_Mes
     type(type_Interferogram)                            :: Interf_Samp_Rpd
     type(type_Interferogram),dimension(:,:,:),allocatable :: Interf_Samp_CS
     type(type_Interferogram),dimension(:,:),allocatable :: Interf_Samp_BB
     type(type_Interferogram),dimension(:,:),allocatable :: Interf_Samp_EW
     type(type_Interferogram),dimension(:,:,:),allocatable :: Interf_Filt_CS
     type(type_Interferogram),dimension(:,:),allocatable :: Interf_Filt_BB
     type(type_Interferogram),dimension(:,:),allocatable :: Interf_Filt_EW
     type(type_Interferogram),dimension(:,:,:),allocatable :: Interf_ReSamp_CS
     type(type_Interferogram),dimension(:,:),allocatable :: Interf_ReSamp_BB
     type(type_Interferogram),dimension(:,:),allocatable :: Interf_ReSamp_EW
     type(type_Interferogram),dimension(:,:,:),allocatable :: Interf_BiLatere_CS
     type(type_Interferogram),dimension(:,:),allocatable :: Interf_BiLatere_BB
     type(type_Interferogram),dimension(:,:),allocatable :: Interf_BiLatere_EW
     type(type_Interferogram),dimension(:,:),allocatable :: Interf_ReSagreg_CS
     type(type_Interferogram),dimension(:)  ,allocatable :: Interf_ReSagreg_BB
     type(type_Interferogram),dimension(:)  ,allocatable :: Interf_ReSagreg_EW
     type(type_Interferogram),dimension(:,:),allocatable :: Interf_Decim_CS
     type(type_Interferogram),dimension(:)  ,allocatable :: Interf_Decim_BB
     type(type_Interferogram),dimension(:)  ,allocatable :: Interf_Decim_EW
     type(type_Interferogram),dimension(:,:),allocatable :: Interf_Trim_CS
     type(type_Interferogram),dimension(:)  ,allocatable :: Interf_Trim_BB
     type(type_Interferogram),dimension(:)  ,allocatable :: Interf_Trim_EW
     type(type_spect_interf_index)                       :: Info ! Interferogram top level fields
     type(type_spect_interf_index)                       :: InfoCS1
     type(type_spect_interf_index)                       :: InfoCS2
     type(type_spect_interf_index)                       :: InfoBB
     type(type_spect_interf_index)                       :: InfoEW
     type(type_Zpd)          ,dimension(:,:,:),allocatable :: Zpd_Samp_CS
     type(type_Zpd)          ,dimension(:,:),allocatable :: Zpd_Samp_BB
     type(type_Zpd)          ,dimension(:,:),allocatable :: Zpd_Samp_EW
     type(type_Zpd)          ,dimension(:,:,:),allocatable :: Zpd_ReSamp_CS
     type(type_Zpd)          ,dimension(:,:),allocatable :: Zpd_ReSamp_BB
     type(type_Zpd)          ,dimension(:,:),allocatable :: Zpd_ReSamp_EW
     type(type_Dc_Component) ,dimension(:,:,:),allocatable :: Dc_Component_CS
     type(type_Dc_Component) ,dimension(:,:),allocatable :: Dc_Component_BB
     type(type_Dc_Component) ,dimension(:,:),allocatable :: Dc_Component_EW
     character(len=60)                                   :: Date
     character(len=4)                                    :: Pixel
     character(len=2)                                    :: Sub_Pixel
     character(len=1)                                    :: Band
!
     character(len=500)                                  :: Files_Interf_Param
     character(len=500)                                  :: Files_Interf
     character(len=500)                                  :: File_Interf_Rpd
     character(len=500)   ,dimension(:) ,allocatable     :: File_Interf_CS
     character(len=500)                                  :: File_Interf_BB
     character(len=500)   ,dimension(:) ,allocatable     :: File_Interf_EW
     character(len=500)                                  :: Files_Scene_Param
     character(len=500)    ,dimension(:) ,allocatable    :: File_Scene_Param
     character(len=500)                                  :: Files_Gain
     character(len=500)    ,dimension(:) ,allocatable    :: File_Gain
     character(len=500)                                  :: Files_Dps_Param
     character(len=500)    ,dimension(:) ,allocatable    :: File_Amplification
     character(len=500)    ,dimension(:) ,allocatable    :: File_Dc_Param
     character(len=500)    ,dimension(:) ,allocatable    :: File_Non_Linearity
     character(len=500)    ,dimension(:) ,allocatable    :: File_Spike_Detect
     character(len=500)    ,dimension(:) ,allocatable    :: File_Zpd_Param
     character(len=500)    ,dimension(:) ,allocatable    :: File_ReSamp_Param
     character(len=500)    ,dimension(:) ,allocatable    :: File_Decim_Param
     character(len=500)    ,dimension(:) ,allocatable    :: File_Trim_Param
     character(len=500)                                  :: File_OutPut_
     character(len=500)                                  :: File_OutPut
     character(len=500)                                  :: File_Header
     integer(kind=SHORT)                                 :: init_CS1, init_CS2, init_BB, init_EW
!
!    read inputs Parameters
     iPos = 1
     read(*,fmt='(a)',err=999) Files_Interf_Param
     read(*,fmt='(a)',err=999) Files_Interf
     read(*,fmt='(a)',err=999) Files_Scene_Param
     read(*,fmt='(a)',err=999) Files_Gain
     read(*,fmt='(a)',err=999) Files_Dps_Param
     read(*,fmt='(a)',err=999) File_OutPut_
     read(*,fmt='(a)',err=999) File_Header
     read(*,fmt='(a)',err=999) Date
!
!    noise generator initialisation
     write(*,'(a)') Date
     call grnd_init(Date)
!
     iFile = 10
     ioalloc = 0
     init_CS1 = 0
     init_CS2 = 0
     init_BB = 0
     init_EW = 0
!
!    Interferogram Parameters initialisation
     call interferometer_def( Files_Interf_Param,&
                              Interf_Param       )

!    reading input parameters
     ! Info initialization (header)
     call read_header ( File_Header, Info%header)
     Info%header%Operator = "OBP"
     Info%header%ProcessingStep = "on board processing"
!
!    Interferograms file names initialisation
     iPos = 1
     open(iFile, file=Files_Interf, status='old', err=999)
     iPos = 2
     read(iFile,*,err=999) PN_Mes
     read(iFile,*,err=999) Nband
     read(iFile,*,err=999) N_ColdSpace
     read(iFile,*,err=999) Nb_Scene
     read(iFile,*,err=999) Nb_SUB_PN

     allocate( Scene_List(Nb_Scene), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     read(iFile,*,err=999) Scene_List(1:Nb_Scene)
     if( (N_ColdSpace < 1) .or. (N_ColdSpace > 2) ) then
        write(*,*) 'Wrong Number of Cold Space Fatal Error', N_ColdSpace
        go to 999
     end if
     if( Nb_Scene /= 1 ) then
        write(*,'(a)') ' Nb_Scene Error must be 1'
        call exit(1)
     end if
     allocate(SB_List(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate(File_Interf_CS(N_ColdSpace), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate(File_Interf_EW(Nb_Scene), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     read(iFile,*,err=999) SB_List(1:Nband)
     read(iFile,'(a)',err=999) File_Interf_Rpd

     do CS = 1, N_ColdSpace
        read(iFile,'(a)',err=999) File_Interf_CS(CS)
     end do
     read(iFile,'(a)',err=999) File_Interf_BB

     do SC = 1, Nb_Scene
        read(iFile,'(a)',err=999) File_Interf_EW(SC)
     end do

     close(iFile)
!
!    Scene parameters initialisation
     iPos = 3
     open(unit=iFile, file=Files_Scene_Param, status='old', err=999)
     iPos = 4
     allocate(File_Scene_Param(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1     
     do SB = 1, Nband
        read(iFile,'(a)',err=999) File_Scene_Param(SB)
     end do
     close(unit=iFile)
     allocate(Scene_Param(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1     
     do SB = 1, Nband
        call scene_mixing_def( File_Scene_Param(SB),&
                               Scene_Param(SB)      )
     end do
!
!    gain file parameters reading
     iPos = 3
     allocate(File_Gain(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     allocate(Gain(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     open(iFile, file=Files_Gain, status='old', err=999)
     do SB = 1, Nband
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
!    Dps initialisation
     iPos = 4
     open(iFile, file=Files_Dps_Param, status='old', err=999)
     iPos = 5
     allocate(File_Amplification(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     allocate(File_Non_Linearity(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     allocate(File_Spike_Detect(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     allocate(File_Dc_Param(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate(File_Zpd_Param(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate(File_ReSamp_Param(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate(File_Decim_Param(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate(File_Trim_Param(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     Adc_Convert_SB%Nband = Nband
     call alloc_Adc_Convert_SB( Adc_Convert_SB )
     do SB = 1, Nband
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
        read(iFile,'(a)',err=999) File_Amplification(SB)
        read(iFile,'(a)',err=999) File_Non_Linearity(SB)
        read(iFile,'(a)',err=999) File_Spike_Detect(SB)
        read(iFile,'(a)',err=999) File_Dc_Param(SB)
        read(iFile,'(a)',err=999) File_Zpd_Param(SB)
        read(iFile,'(a)',err=999) File_ReSamp_Param(SB)
        read(iFile,'(a)',err=999) File_Decim_Param(SB)
        read(iFile,'(a)',err=999) File_Trim_Param(SB)
     end do
     close(iFile)
!
!    allocation
     allocate( Interf_Samp_CS(Nb_SUB_PN,Nband, N_ColdSpace),&
                                               stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Interf_Samp_BB(Nb_SUB_PN,Nband),stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Interf_Samp_EW(Nb_SUB_PN,Nband),stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Interf_Filt_CS(Nb_SUB_PN,Nband, N_ColdSpace),&
                                               stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Interf_Filt_BB(Nb_SUB_PN,Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Interf_Filt_EW(Nb_SUB_PN,Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Interf_ReSamp_CS(Nb_SUB_PN,Nband, N_ColdSpace),&
                                                 stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Interf_ReSamp_BB(Nb_SUB_PN,Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Interf_ReSamp_EW(Nb_SUB_PN,Nband),  stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Interf_BiLatere_CS(Nb_SUB_PN,Nband, N_ColdSpace),&
                                                   stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Interf_BiLatere_BB(Nb_SUB_PN,Nband),  stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Interf_BiLatere_EW(Nb_SUB_PN,Nband),  stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Interf_ReSagreg_CS(Nband, N_ColdSpace), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Interf_ReSagreg_BB(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Interf_ReSagreg_EW(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Interf_Decim_CS(Nband, N_ColdSpace), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Interf_Decim_BB(Nband),  stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Interf_Decim_EW(Nband),  stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Interf_Trim_CS(Nband, N_ColdSpace), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Interf_Trim_BB(Nband),  stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Interf_Trim_EW(Nband),  stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Zpd_Samp_CS(Nb_SUB_PN,Nband, N_ColdSpace), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Zpd_Samp_BB(Nb_SUB_PN,Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Zpd_Samp_EW(Nb_SUB_PN,Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Zpd_ReSamp_CS(Nb_SUB_PN,Nband, N_ColdSpace), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Zpd_ReSamp_BB(Nb_SUB_PN,Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Zpd_ReSamp_EW(Nb_SUB_PN,Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate(Amplification(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     allocate(Non_Linearity(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     allocate(Spike_Detect(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc+1
     allocate(Dc_Component_CS(Nb_SUB_PN,Nband, N_ColdSpace), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate(Dc_Component_BB(Nb_SUB_PN,Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate(Dc_Component_EW(Nb_SUB_PN,Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate(ReSamp_Param(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate(Decim_One_fir(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate(Bit_Trimming(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     if( ioalloc /= 0 ) then
        write(*,*) 'Allocation Error',ioalloc
        call exit(1)
     end if
!
!    NZPD initialisation
     do SB = 1, Nband
        do SUB_PN = 1, Nb_SUB_PN
           do CS = 1, N_ColdSpace
              call nzpd_init( File_Zpd_Param(SB),       &
                              Interf_Param,             &
                              SB,                       &
                              Zpd_Samp_CS(SUB_PN,SB,CS) )
           end do
           call nzpd_init( File_Zpd_Param(SB),    &
                           Interf_Param,          &
                           SB,                    &
                           Zpd_Samp_BB(SUB_PN,SB) )
           call nzpd_init( File_Zpd_Param(SB),    &
                           Interf_Param,          &
                           SB,                    &
                           Zpd_Samp_EW(SUB_PN,SB) )
           do CS = 1, N_ColdSpace
              call nzpd_init( File_Zpd_Param(SB),         &
                              Interf_Param,               &
                              SB,                         &
                              Zpd_ReSamp_CS(SUB_PN,SB,CS) )
           end do
           call nzpd_init( File_Zpd_Param(SB),      &
                           Interf_Param,            &
                           SB,                      &
                           Zpd_ReSamp_BB(SUB_PN,SB) )
           call nzpd_init( File_Zpd_Param(SB),      &
                           Interf_Param,            &
                           SB,                      &
                           Zpd_ReSamp_EW(SUB_PN,SB) )
        end do
     end do
     write(*,*) 'End nzpd_init'


     write(Pixel,'(i4.4)') PN_Mes

     InfoCS1%header%filename = File_OutPut_(1:len_trim(File_OutPut_))//'Interf_Trim_CS1_'//Interf_Param%Target_CS&
                       //'_PN'//Pixel//'.nc'
     ! BB output
     InfoBB%header%filename = File_OutPut_(1:len_trim(File_OutPut_))//'Interf_Trim_BB_'//Interf_Param%Target_BB&
                       //'_PN'//Pixel//'.nc'
     ! EW output
     InfoEW%header%filename = File_OutPut_(1:len_trim(File_OutPut_))//'Interf_Trim_EW_'//Interf_Param%Target_EW&
                       //'_PN'//Pixel//'.nc'

     if (N_ColdSpace == 2) then
        ! CS2 output
        InfoCS2%header%filename = File_OutPut_(1:len_trim(File_OutPut_))//'Interf_Trim_CS2_'//Interf_Param%Target_CS&
                       //'_PN'//Pixel//'.nc'
     end if

     ! Info initialization (Nband,NcolFov,NlinFov,ColFov,LinFov,NsubFov)
     Info%Nband   = Nband
     Info%NcolFov = 1
     Info%NlinFov = 1
     Info%ColFov  = 1
     Info%LinFov  = PN_Mes
     Info%NsubFov = Nb_SUB_PN
!
!    Spectral bands loop
     do SB = 1, Nband
!
!       Non-linearity parameters initialisation
        call non_linearity_init( File_Non_Linearity(SB),&
                                 Non_Linearity(SB)      )
!
!       Amplification parameters initialisation
        call amplification_init( File_Amplification(SB),&
                                 Amplification(SB)      )
!
!       ADC conversion parameters initialisation
        do NADC = 1, Adc_Convert_SB%Adc_Convert_Nadc(SB)%Nadc
           call adc_convert_init( Adc_Convert_SB%Adc_Convert_Nadc(SB)&
                                         %Adc_Convert(NADC)%filename,&
                Adc_Convert_SB%Adc_Convert_Nadc(SB)%Adc_Convert(NADC) )
        end do
!
!       Spike detection parameters initialisation
        call spike_detection_init( File_Spike_Detect(SB),&
                                   Spike_Detect(SB)      )
!
!       Resampling Parameters initialisation
        call interf_resampling_init( File_ReSamp_Param(SB),&
                                     ReSamp_Param(SB)      )
        write(*,*) 'End resampling_init'
!
!       Decimation parameters initialisation
        call decim_one_fir_init( File_Decim_Param(SB),&
                                 Decim_One_fir(SB)    )
        write(*,*) 'End decim_one_fir_init'
!
!       Dc Component parameters initialisation
        do SUB_PN = 1, Nb_SUB_PN
           do CS = 1, N_ColdSpace
              call dc_component_init( File_Dc_Param(SB),            &
                                      Dc_Component_CS(SUB_PN,SB,CS) )
              Dc_Component_CS(SUB_PN,SB,CS)%T = Interf_Param%T_CS
           end do
           call dc_component_init( File_Dc_Param(SB),         &
                                   Dc_Component_BB(SUB_PN,SB) )
           Dc_Component_BB(SUB_PN,SB)%T = Interf_Param%T_BB
           call dc_component_init( File_Dc_Param(SB),         &
                                   Dc_Component_EW(SUB_PN,SB) )
           Dc_Component_EW(SUB_PN,SB)%T = 0.d+00
        end do
!
!       Bit trimming initialisation
        call bit_trimming_init( File_Trim_Param(SB),&
                                Bit_Trimming(SB)    )
!
     end do
!
!    Interferogram RPD reading
     ! Interf_Samp_Rpd initialization
     Interf_Samp_Rpd%TopLevelIn%header%filename = File_Interf_Rpd
     Interf_Samp_Rpd%TopLevelIn%Band = 1
     Interf_Samp_Rpd%TopLevelIn%NsubFov = 1
     Interf_Samp_Rpd%TopLevelIn%SubFov = 1
     call readinterferogram_netcdf( Interf_Samp_Rpd ,ErrCode )
     if( ErrCode /= 0 ) then
        write(*,*) 'Interferogram reading Error RPD'
        write(*,'(a)') trim(Interf_Samp_Rpd%TopLevelIn%header%filename)
        call exit(1)
     end if
!
!    Spectral bands loop
     do SB = 1, Nband
        write(Band,'(i1.1)') SB_List(SB)
        ! Band
        Info%Band = SB_List(SB)
!
!       SubPixels loop
        do SUB_PN = 1, Nb_SUB_PN
           write(*,*) '**************SB*******SUB_PN : ',SB,SB_List(SB),SUB_PN
           write(Sub_Pixel,'(i2.2)') SUB_PN
           SC = Scene_Param(SB)%N_Spectre_1(SUB_PN)
           Info%SubFov = SUB_PN
!
!          Interferogram triplet reading
           do CS = 1, N_ColdSpace
              Interf_Samp_CS(SUB_PN,SB,CS)%TopLevelIn = Info
              Interf_Samp_CS(SUB_PN,SB,CS)%TopLevelIn%header%filename = &
                           File_Interf_CS(CS)
              call readinterferogram_netcdf( Interf_Samp_CS(SUB_PN,SB,CS) ,ErrCode )
              if( ErrCode /= 0 ) then
                 write(*,*) 'Interferogram reading Error CS',SUB_PN,SB,CS,&
                 Interf_Samp_CS(SUB_PN,SB,CS)%filename(1:&
                           len_trim(Interf_Samp_CS(SUB_PN,SB,CS)%TopLevelIn%header%filename))
                 call exit(1)
              end if
           end do
           Interf_Samp_BB(SUB_PN,SB)%TopLevelIn = Info
           Interf_Samp_BB(SUB_PN,SB)%TopLevelIn%header%filename = File_Interf_BB
           call readinterferogram_netcdf( Interf_Samp_BB(SUB_PN,SB) ,ErrCode )
           if( ErrCode /= 0 ) then
              write(*,*) 'Interferogram reading Error BB',SUB_PN,SB,&
              Interf_Samp_BB(SUB_PN,SB)%filename(1:&
                            len_trim(Interf_Samp_BB(SUB_PN,SB)%TopLevelIn%header%filename))
              call exit(1)
           end if
           Interf_Samp_EW(SUB_PN,SB)%TopLevelIn = Info
           Interf_Samp_EW(SUB_PN,SB)%TopLevelIn%header%filename = File_Interf_EW(SC)
           write(*,*) 'EW SC SUB_PN SB ',SC,SUB_PN,SB,&
           Interf_Samp_EW(SUB_PN,SB)%TopLevelIn%header%filename(1:&
                            len_trim(Interf_Samp_EW(SUB_PN,SB)%TopLevelIn%header%filename))
           call readinterferogram_netcdf( Interf_Samp_EW(SUB_PN,SB) ,ErrCode )
           if( ErrCode /= 0 ) then
              write(*,*) 'Interferogram reading Error EW SC',SC,SUB_PN,SB,&
              Interf_Samp_EW(SUB_PN,SB)%TopLevelIn%header%filename(1:&
                            len_trim(Interf_Samp_EW(SUB_PN,SB)%TopLevelIn%header%filename))
              call exit(1)
           end if

           select case ( N_ColdSpace )
           case ( 1 )
!
!             Non linearity correction
              call interf_triplet_nlc( Non_Linearity(SB),          &
                                       Adc_Convert_SB%Adc_Convert_Nadc(SB),&
                                       Amplification(SB),          &
                                       Interf_Samp_CS(SUB_PN,SB,1),&
                                       Interf_Samp_BB(SUB_PN,SB),  &
                                       Interf_Samp_EW(SUB_PN,SB)   )
           case default
              write(*,*) ' Wrong number of Cold Space',N_ColdSpace
              call exit(1)
           end select
!
!          NZPD computation
           call nzpd_connes( Interf_Param,               &
                             Zpd_Samp_CS(SUB_PN,SB,1),   &
                             Interf_Samp_CS(SUB_PN,SB,1) )
           call nzpd_connes( Interf_Param,             &
                             Zpd_Samp_BB(SUB_PN,SB),   &
                             Interf_Samp_BB(SUB_PN,SB) )
           call nzpd_connes( Interf_Param,             &
                             Zpd_Samp_EW(SUB_PN,SB),   &
                             Interf_Samp_EW(SUB_PN,SB) )
           write(*,*) 'end nzpd_connes SB',SB,SB_List(SB)
           call nzpd( Zpd_Samp_CS(SUB_PN,SB,1),   &
                      Zpd_Samp_BB(SUB_PN,SB),     &
                      Zpd_Samp_CS(SUB_PN,SB,1),   &
                      Interf_Samp_CS(SUB_PN,SB,1) )
           select case ( N_ColdSpace )
           case ( 1 )
              call nzpd( Zpd_Samp_CS(SUB_PN,SB,1), &
                         Zpd_Samp_BB(SUB_PN,SB),   &
                         Zpd_Samp_BB(SUB_PN,SB),   &
                         Interf_Samp_BB(SUB_PN,SB) )
              call nzpd( Zpd_Samp_CS(SUB_PN,SB,1), &
                         Zpd_Samp_BB(SUB_PN,SB),   &
                         Zpd_Samp_EW(SUB_PN,SB),   &
                         Interf_Samp_EW(SUB_PN,SB) )
           case default
              write(*,*) ' Wrong number of Cold Space',N_ColdSpace
              call exit(1)
           end select
           write(*,*) 'end nzpd EW SB',SB,SB_List(SB)
           if( Interf_Param%Target_EW == 'LZ' ) then
              Zpd_Samp_EW(SUB_PN,SB)%NZpd = Zpd_Samp_BB(SUB_PN,SB)%NZpd
           end if
!
!          Plots output
           write(*,*) 'Opd(1)       ',Interf_Samp_CS(SUB_PN,SB,1)%Time(1)
           write(*,*) 'Opd(N_Sample)',Interf_Samp_CS(SUB_PN,SB,1)&
                       %Time(Interf_Samp_CS(SUB_PN,SB,1)%N_Sample)
           File_OutPut = File_OutPut_(1:len_trim(File_OutPut_))    &
                       //'Interf_Samp_CS1_'//Interf_Param%Target_CS&
                       //'_PN'//Pixel//'_SUB_PN'//Sub_Pixel        &
                       //'_SB'//Band//'.plt'
           call plot_interf( File_OutPut,                &
                             Zpd_Samp_CS(SUB_PN,SB,1),   &
                             Interf_Samp_CS(SUB_PN,SB,1) )
           File_OutPut = File_OutPut_(1:len_trim(File_OutPut_))   &
                       //'Interf_Samp_BB_'//Interf_Param%Target_BB&
                       //'_PN'//Pixel//'_SUB_PN'//Sub_Pixel       &
                       //'_SB'//Band//'.plt'
           call plot_interf( File_OutPut,              &
                             Zpd_Samp_BB(SUB_PN,SB),   &
                             Interf_Samp_BB(SUB_PN,SB) )
           File_OutPut = File_OutPut_(1:len_trim(File_OutPut_))   &
                       //'Interf_Samp_EW_'//Interf_Param%Target_EW&
                       //'_PN'//Pixel//'_SUB_PN'//Sub_Pixel       &
                       //'_SB'//Band//'.plt'
           call plot_interf( File_OutPut,              &
                             Zpd_Samp_EW(SUB_PN,SB),   &
                             Interf_Samp_EW(SUB_PN,SB) )
!
!          DC component Computation
           do CS = 1, N_ColdSpace
              call calc_dc_component( Zpd_Samp_CS(SUB_PN,SB,CS),    &
                                      Interf_Samp_CS(SUB_PN,SB,CS), &
                                      Dc_Component_CS(SUB_PN,SB,CS) )
           end do
           call calc_dc_component( Zpd_Samp_BB(SUB_PN,SB),    &
                                   Interf_Samp_BB(SUB_PN,SB), &
                                   Dc_Component_BB(SUB_PN,SB) )
           call calc_dc_component( Zpd_Samp_EW(SUB_PN,SB),    &
                                   Interf_Samp_EW(SUB_PN,SB), &
                                   Dc_Component_EW(SUB_PN,SB) )
!
!          DC component extraction
           do CS = 1, N_ColdSpace
              call extract_dc_component( Interf_Samp_CS(SUB_PN,SB,CS), &
                                         Dc_Component_CS(SUB_PN,SB,CS) )
           end do
           call extract_dc_component( Interf_Samp_BB(SUB_PN,SB), &
                                      Dc_Component_BB(SUB_PN,SB) )
           call extract_dc_component( Interf_Samp_EW(SUB_PN,SB), &
                                      Dc_Component_EW(SUB_PN,SB) )
           select case ( N_ColdSpace )
           case ( 1 )
!
!             Spike detection
              call interf_triplet_spike( Spike_Detect(SB),           &
                                         Zpd_Samp_CS(SUB_PN,SB,1),   &
                                         Zpd_Samp_BB(SUB_PN,SB),     &
                                         Zpd_Samp_EW(SUB_PN,SB),     &
                                         Interf_Samp_CS(SUB_PN,SB,1),&
                                         Interf_Samp_BB(SUB_PN,SB),  &
                                         Interf_Samp_EW(SUB_PN,SB),  &
                                         Interf_Filt_CS(SUB_PN,SB,1),&
                                         Interf_Filt_BB(SUB_PN,SB),  &
                                         Interf_Filt_EW(SUB_PN,SB)   )
           case default
              write(*,*) ' Wrong number of Cold Space',N_ColdSpace
              call exit(1)
           end select
           write(*,*) 'End interf_triplet_spike detection'
!
!          Nominal Interferogram setting before radiometric calibration
           if( Interf_Param%Sampling_Mode == 'CLOCK' ) then
              write(*,*) 'Interferogram resampling Band SB',&
                                               SB,SB_List(SB)
              if( Interf_Param%Target_EW == 'LZ' ) then
                 Zpd_Samp_EW(SUB_PN,SB)%NZpd = Zpd_Samp_BB(SUB_PN,SB)%NZpd
              end if
              select case ( N_ColdSpace )
              case ( 1 )
!
!                Interferogram triplet resampling
                 call interf_triplet_resampling( ReSamp_Param(SB),           &
                                               Interf_Param,                 &
                                               Zpd_Samp_CS(SUB_PN,SB,1),     &
                                               Zpd_Samp_BB(SUB_PN,SB),       &
                                               Zpd_Samp_EW(SUB_PN,SB),       &
                                               Interf_Samp_Rpd,              &
                                               Interf_Samp_CS(SUB_PN,SB,1),  &
                                               Interf_Samp_BB(SUB_PN,SB),    &
                                               Interf_Samp_EW(SUB_PN,SB),    &
                                               Interf_ReSamp_CS(SUB_PN,SB,1),&
                                               Interf_ReSamp_BB(SUB_PN,SB),  &
                                               Interf_ReSamp_EW(SUB_PN,SB)   )
              case default
                 write(*,*) ' Wrong number of Cold Space',N_ColdSpace
                 call exit(1)
              end select
              write(*,*) 'End interf_resampling SB SUB_PN',&
                                                            SB,SUB_PN
!
!             NZPD computation
              call nzpd_connes( Interf_Param,                 &
                                Zpd_ReSamp_CS(SUB_PN,SB,1),   &
                                Interf_ReSamp_CS(SUB_PN,SB,1) )
              call nzpd_connes( Interf_Param,               &
                                Zpd_ReSamp_BB(SUB_PN,SB),   &
                                Interf_ReSamp_BB(SUB_PN,SB) )
              call nzpd_connes( Interf_Param,               &
                                Zpd_ReSamp_EW(SUB_PN,SB),   &
                                Interf_ReSamp_EW(SUB_PN,SB) )
              write(*,*) 'end nzpd_connes resamp ref SB',SB,SB_List(SB)
              call nzpd( Zpd_ReSamp_CS(SUB_PN,SB,1),   &
                         Zpd_ReSamp_BB(SUB_PN,SB),     &
                         Zpd_ReSamp_CS(SUB_PN,SB,1),   &
                         Interf_ReSamp_CS(SUB_PN,SB,1) )
              select case ( N_ColdSpace )
              case ( 1 )
                 call nzpd( Zpd_ReSamp_CS(SUB_PN,SB,1), &
                            Zpd_ReSamp_BB(SUB_PN,SB),   &
                            Zpd_ReSamp_BB(SUB_PN,SB),   &
                            Interf_ReSamp_BB(SUB_PN,SB) )
              case default
                 write(*,*) ' Wrong number of Cold Space',N_ColdSpace
                 call exit(1)
              end select
              call nzpd( Zpd_ReSamp_CS(SUB_PN,SB,1), &
                         Zpd_ReSamp_BB(SUB_PN,SB),   &
                         Zpd_ReSamp_EW(SUB_PN,SB),   &
                         Interf_ReSamp_EW(SUB_PN,SB) )
              write(*,*) 'end nzpd EW resamp ref SB',SB,SB_List(SB)
              if( Interf_Param%Target_EW == 'LZ' ) then
                 Zpd_ReSamp_EW(SUB_PN,SB)%NZpd = Zpd_ReSamp_BB(SUB_PN,SB)%NZpd
              end if
              select case ( N_ColdSpace )
              case ( 1 )
!
!                Interferogram triplet bilatere
                 call interf_triplet_bilatere( ReSamp_Param(SB),             &
                                             Zpd_ReSamp_CS(SUB_PN,SB,1),     &
                                             Zpd_ReSamp_BB(SUB_PN,SB),       &
                                             Zpd_ReSamp_EW(SUB_PN,SB),       &
                                             Interf_ReSamp_CS(SUB_PN,SB,1),  &
                                             Interf_ReSamp_BB(SUB_PN,SB),    &
                                             Interf_ReSamp_EW(SUB_PN,SB),    &
                                             Interf_BiLatere_CS(SUB_PN,SB,1),&
                                             Interf_BiLatere_BB(SUB_PN,SB),  &
                                             Interf_BiLatere_EW(SUB_PN,SB)   )
                 write(*,*) 'end interf_triplet_bilatere SB',&
                                                SB,SB_List(SB)
              case default
                 write(*,*) ' Wrong number of Cold Space',N_ColdSpace
                 call exit(1)
              end select
!
           else if( Interf_Param%Sampling_Mode == 'LZRPD' ) then
              select case ( N_ColdSpace )
              case ( 1 )
!
!                Interferogram triplet bilatere
                 call interf_triplet_bilatere( ReSamp_Param(SB),             &
                                             Zpd_Samp_CS(SUB_PN,SB,1),       &
                                             Zpd_Samp_BB(SUB_PN,SB),         &
                                             Zpd_Samp_EW(SUB_PN,SB),         &
                                             Interf_Samp_CS(SUB_PN,SB,1),    &
                                             Interf_Samp_BB(SUB_PN,SB),      &
                                             Interf_Samp_EW(SUB_PN,SB),      &
                                             Interf_BiLatere_CS(SUB_PN,SB,1),&
                                             Interf_BiLatere_BB(SUB_PN,SB),  &
                                             Interf_BiLatere_EW(SUB_PN,SB)   )
              case default
                 write(*,*) ' Wrong number of Cold Space',N_ColdSpace
                 call exit(1)
              end select
              write(*,*) 'end interf_triplet_bilatere nominal SB',&
                                                     SB,SB_List(SB)
           else
              write(*,*) 'Interf Sampling Mode ERROR',&
                          Interf_Param%Sampling_Mode
              call exit(1)
           end if
!
!          Interferogram aggregation
           if( SUB_PN == 1 ) then
              call Interf_Header_Transfer( Interf_BiLatere_CS(SUB_PN,SB,1),&
                                           Interf_ReSagreg_CS(SB,1) )
              if( N_ColdSpace == 2 ) then
                call Interf_Header_Transfer( Interf_BiLatere_CS(SUB_PN,SB,2),&
                                             Interf_ReSagreg_CS(SB,2) )
              end if
              call Interf_Header_Transfer( Interf_BiLatere_BB(SUB_PN,SB),&
                                           Interf_ReSagreg_BB(SB) )
              call Interf_Header_Transfer( Interf_BiLatere_EW(SUB_PN,SB),&
                                           Interf_ReSagreg_EW(SB) )
           end if
           write(*,*) 'Interferogram aggregation Band : ',SB
           call interf_aggregation( Interf_BiLatere_CS(SUB_PN,SB,1), &
                                    Interf_ReSagreg_CS(SB,1) )
           call interf_aggregation( Interf_BiLatere_BB(SUB_PN,SB), &
                                    Interf_ReSagreg_BB(SB) )
           call interf_aggregation( Interf_BiLatere_EW(SUB_PN,SB), &
                                    Interf_ReSagreg_EW(SB) )
           write(*,*) 'End interf_aggregation'
!
           if( Interf_Param%Sampling_Mode == 'CLOCK' ) then
              call dalloc_Interferogram( Interf_ReSamp_CS(SUB_PN,SB,1) )
              call dalloc_Interferogram( Interf_ReSamp_BB(SUB_PN,SB) )
              call dalloc_Interferogram( Interf_ReSamp_EW(SUB_PN,SB) )
           end if
           call dalloc_Interferogram( Interf_BiLatere_CS(SUB_PN,SB,1) )
           call dalloc_Interferogram( Interf_BiLatere_BB(SUB_PN,SB) )
           call dalloc_Interferogram( Interf_BiLatere_EW(SUB_PN,SB) )
           call dalloc_Interferogram( Interf_Samp_CS(SUB_PN,SB,1) )
           call dalloc_Interferogram( Interf_Samp_BB(SUB_PN,SB) )
           call dalloc_Interferogram( Interf_Samp_EW(SUB_PN,SB) )
           call dalloc_Zpd(Zpd_Samp_CS(SUB_PN,SB,1))
           call dalloc_Zpd(Zpd_Samp_BB(SUB_PN,SB))
           call dalloc_Zpd(Zpd_Samp_EW(SUB_PN,SB))
           if( Interf_Param%Sampling_Mode == 'CLOCK' ) then
              call dalloc_Zpd(Zpd_ReSamp_CS(SUB_PN,SB,1))
              call dalloc_Zpd(Zpd_ReSamp_BB(SUB_PN,SB))
              call dalloc_Zpd(Zpd_ReSamp_EW(SUB_PN,SB))
           end if
! 
        end do ! end Subpixels loop
!
        write(*,*) 'End interf_aggregation SB',SB
!
!       Gain compensation
        call interf_gaincompens( Gain(SB), Interf_ReSagreg_CS(SB,1) )
        call interf_gaincompens( Gain(SB), Interf_ReSagreg_BB(SB) )
        call interf_gaincompens( Gain(SB), Interf_ReSagreg_EW(SB) )
        call dalloc_Gain( Gain(SB) )
!
!       Interferogram double side decimation
        select case ( N_ColdSpace )
        case ( 1 )
           call interf_triplet_decim_one_fir( Decim_One_fir(SB),       &
                                              Interf_Param,            &
                                              Interf_ReSagreg_CS(SB,1),&
                                              Interf_ReSagreg_BB(SB),  &
                                              Interf_ReSagreg_EW(SB),  &
                                              Interf_Decim_CS(SB,1),   &
                                              Interf_Decim_BB(SB),     &
                                              Interf_Decim_EW(SB)      )
        case default
           write(*,*) ' Wrong number of Cold Space',N_ColdSpace
           call exit(1)
        end select
        write(*,*) 'end interf_triplet decimation SB',&
                                         SB,SB_List(SB)
        call dalloc_Interferogram( Interf_ReSagreg_CS(SB,1) )
        call dalloc_Interferogram( Interf_ReSagreg_BB(SB) )
        call dalloc_Interferogram( Interf_ReSagreg_EW(SB) )
!
!       Interferogram bit trimming
        select case ( N_ColdSpace )
        case ( 1 )
           call interf_triplet_bit_trimming( Bit_Trimming(SB),     &
                                             Interf_Decim_CS(SB,1),&
                                             Interf_Decim_BB(SB),  &
                                             Interf_Decim_EW(SB),  &
                                             Interf_Trim_CS(SB,1), &
                                             Interf_Trim_BB(SB),   &
                                             Interf_Trim_EW(SB)    )
        case default
           write(*,*) ' Wrong number of Cold Space',N_ColdSpace
           call exit(1)
        end select
        write(*,*) 'end interf_triplet bit trimmed SB',&
                                          SB,SB_List(SB)
!
        call dalloc_Interferogram( Interf_Decim_CS(SB,1) )
        if( N_ColdSpace == 2 ) then
           call dalloc_Interferogram( Interf_Decim_CS(SB,2) )
        end if
        call dalloc_Interferogram( Interf_Decim_BB(SB) )
        call dalloc_Interferogram( Interf_Decim_EW(SB) )

        ! write trimmed interferograms
        ! Info initialization (NsubFov=1,SubFov=1 after subpixels loop)
        Info%NsubFov = 1
        Info%SubFov = 1
        Interf_Trim_CS(SB,1)%TopLevelIn = Info
        Interf_Trim_CS(SB,1)%TopLevelIn%header%filename = InfoCS1%header%filename
        Interf_Trim_CS(SB,1)%TopLevelIn%Target = Interf_Param%Target_CS
        call writeinterferogram_netcdf( Interf_Trim_CS(SB,1), ErrCode, init_CS1 )
        if( ErrCode /= 0 ) then
           write(*,*) 'Interferogram writing Error CS1',SB,   &
                       trim(Interf_Trim_CS(SB,1)%TopLevelIn%header%filename)
           call exit(1)
        end if


        Interf_Trim_BB(SB)%TopLevelIn = Info
        Interf_Trim_BB(SB)%TopLevelIn%header%filename = InfoBB%header%filename
        Interf_Trim_BB(SB)%TopLevelIn%Target = Interf_Param%Target_BB
        write(*,'(a,a)') 'File_OutPut : ',               &
                 trim(Interf_Trim_BB(SB)%TopLevelIn%header%filename)
        call writeinterferogram_netcdf( Interf_Trim_BB(SB) ,ErrCode, init_BB )
        if( ErrCode /= 0 ) then
           write(*,*) 'Interferogram writing Error BB',SB,  &
                       trim(Interf_Trim_BB(SB)%TopLevelIn%header%filename)
           call exit(1)
        end if

        Interf_Trim_EW(SB)%TopLevelIn = Info
        Interf_Trim_EW(SB)%TopLevelIn%header%filename = InfoEW%header%filename
        Interf_Trim_EW(SB)%TopLevelIn%Target = Interf_Param%Target_EW
        write(*,'(a,a)') 'File_OutPut : ',               &
                 trim(Interf_Trim_EW(SB)%TopLevelIn%header%filename)
        call writeinterferogram_netcdf( Interf_Trim_EW(SB) ,ErrCode, init_EW )
        if( ErrCode /= 0 ) then
           write(*,*) 'Interferogram writing Error EW',SB,  &
                       trim(Interf_Trim_EW(SB)%TopLevelIn%header%filename)
           call exit(1)
        end if
        call dalloc_Interferogram( Interf_Trim_CS(SB,1) )
        call dalloc_Interferogram( Interf_Trim_BB(SB) )
        call dalloc_Interferogram( Interf_Trim_EW(SB) )
!
     end do ! end spectral bands loop
     write(*,*) 'End onboard_processing_prg'
!
!    deallocation
     call dalloc_Interferogram( Interf_Samp_Rpd )
     deallocate( Interf_Samp_CS )
     deallocate( Interf_Samp_BB )
     deallocate( Interf_Samp_EW )
     deallocate( Interf_ReSamp_CS )
     deallocate( Interf_ReSamp_BB )
     deallocate( Interf_ReSamp_EW )
     deallocate( Interf_BiLatere_CS )
     deallocate( Interf_BiLatere_BB )
     deallocate( Interf_BiLatere_EW )
     deallocate( Interf_ReSagreg_CS )
     deallocate( Interf_ReSagreg_BB )
     deallocate( Interf_ReSagreg_EW )
     deallocate( Interf_Decim_CS )
     deallocate( Interf_Decim_BB )
     deallocate( Interf_Decim_EW )
     deallocate( Interf_Trim_CS )
     deallocate( Interf_Trim_BB )
     deallocate( Interf_Trim_EW )
     deallocate( File_Interf_CS )
     deallocate( File_Interf_EW )
!
     call exit(0)
999 write(*,*) 'onboard_processing_prg Fatal error',iPos
    call exit(1)
!
end program onboard_processing_prg
