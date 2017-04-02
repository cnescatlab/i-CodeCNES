!!# onground_processing_prg.f90 --
!!#
!!#           Project: SPS_GENERIC
!!#           Authors: NOVELTIS/B.TOURNIER
!!#              Date: august 2011
!!#           Version: $Revision: 1.2 $
!!# Last modification: $Date: 2012/02/08 10:19:15 $
!!#
!!
!> onground_processing_prg -- Program
!!
!! * Purpose
!!
!!   Program for spectral calibration computation.
!!
!! * Description
!!        
!!   This main program allows the level 1b and 1c spectral calibrated spectra 
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.7.3
!!

program onground_processing_prg
   use precision_type
   use error_type
   use constantes_type
   use interf_param_type
   use resampling_param_type
   use decim_one_fir_type
   use interferogram_type
   use spectrum_type
   use srf_type
   use apf_type
   use spectrum_type
   use spectral_l1b_type
   use spectral_l1c_type
   use bit_trimming_type
   use math_module
   use interferometer_init_module
   use resampling_module
   use decimation_module
   use radiometric_calibration_module
   use spectrum_oversamp_module
   use spectral_l1bl1c_module
   use bit_trimming_module
   use plot_module
!
   implicit none
     type(type_Interf_Param)                             :: Interf_Param
     type(type_Resampling_Param),dimension(:),allocatable:: Resamp_Param
     type(type_Decim_One_fir),dimension(:),allocatable   :: Decim_One_fir
     type(type_Bit_Trimming) ,dimension(:),allocatable   :: Bit_Trimming
     type(type_Spectral_L1b)                             :: Spectral_L1b
     type(type_Spectral_L1c)                             :: Spectral_L1c
     type(type_Interferogram),dimension(:,:),allocatable :: Interf_Trim_CS
     type(type_Interferogram),dimension(:),allocatable   :: Interf_Trim_BB
     type(type_Interferogram),dimension(:),allocatable   :: Interf_Trim_EW
     type(type_Interferogram),dimension(:,:),allocatable :: Interf_Decim_CS
     type(type_Interferogram),dimension(:),allocatable   :: Interf_Decim_BB
     type(type_Interferogram),dimension(:),allocatable   :: Interf_Decim_EW
     type(type_spect_interf_index)                       :: Info ! Spectrum top level fields
     type(type_spect_interf_index)                       :: InfoL1a
     type(type_spect_interf_index)                       :: InfoL1b
     type(type_spect_interf_index)                       :: InfoL1c
     type(type_Spectrum)     ,dimension(:),allocatable   :: Spectrum_L1a
     type(type_Spectrum)     ,dimension(:),allocatable   :: Spectrum_L1b
     type(type_Spectrum)     ,dimension(:),allocatable   :: Spectrum_L1c
     type(type_Srf)          ,dimension(:),allocatable   :: Srf
     type(type_Apf)          ,dimension(:),allocatable   :: Apf
     integer(kind=LONG)                                  :: Nband
     integer(kind=LONG)                                  :: N_ColdSpace
     integer(kind=LONG)                                  :: CS
     integer(kind=LONG)                                  :: SB
     integer(kind=LONG)      ,dimension(:),allocatable   :: SB_List
     integer(kind=LONG)                                  :: PN
     integer(kind=LONG)                                  :: PN_Mes
     integer(kind=LONG)                                  :: ErrCode
     integer(kind=LONG)                                  :: ioalloc
     integer(kind=LONG)                                  :: iFile
     integer(kind=LONG)                                  :: iPos
     character(len=60)                                   :: Date
     character(len=4)                                    :: Pixel
     character(len=1)                                    :: Band
     character(len=3)                                    :: Aggregation_Mode
     character(len=3)                                    :: Post_Calibration
     integer(kind=LONG)                                  :: NF
     real(kind=DOUBLE)                                   :: V_Doppler
     real(kind=DOUBLE)                                   :: dV_Doppler
!
     character(len=500)                                  :: Files_Interf_Param
     character(len=500)                                  :: Files_Srf
     character(len=500)                                  :: File_Srf
     character(len=500)                                  :: File_Apf
     character(len=500)                                  :: Files_Level1_Param
     character(len=500)                                  :: File_Level1b_Param
     character(len=500)                                  :: File_Level1c_Param
     character(len=500)                                  :: Files_Interf
     character(len=500)  ,dimension(:) ,allocatable      :: File_Interf_CS
     character(len=500)                                  :: File_Interf_BB
     character(len=500)                                  :: File_Interf_EW
     character(len=500)                                  :: Files_Dps_Param
     character(len=500)  ,dimension(:) ,allocatable      :: File_ReSamp_Param
     character(len=500)  ,dimension(:) ,allocatable      :: File_Decim_Param
     character(len=500)  ,dimension(:) ,allocatable      :: File_Trim_Param
     character(len=500)                                  :: File_OutPut_
     character(len=500)                                  :: File_OutPut
     character(len=500)                                  :: File_Header
     integer(kind=SHORT)                                 :: init_L1a, init_L1b, init_L1c
!
!    read inputs parameters
     read(*,fmt='(a)',err=999) Files_Interf_Param
     read(*,fmt='(a)',err=999) Files_Srf
     read(*,fmt='(a)',err=999) Files_Level1_Param
     read(*,fmt='(a)',err=999) Files_Interf
     read(*,fmt='(a)',err=999) Files_Dps_Param
     read(*,fmt='(a)',err=999) File_OutPut_
     read(*,fmt='(a)',err=999) File_Header
     read(*,fmt='(a)',err=999) Date
     write(*,*) Files_Srf(1:len_trim(Files_Srf))
     write(*,*) Files_Level1_Param(1:len_trim(Files_Level1_Param))
     write(*,*) Files_Interf(1:len_trim(Files_Interf))
!
!    noise generator initialisation
     write(*,'(a)') Date
     call grnd_init(Date)
!
     iFile = 10
     ioalloc = 0
     init_L1a = 0
     init_L1b = 0
     init_L1c = 0
!
!    Interferogram Parameters initialisation
     call interferometer_def( Files_Interf_Param,&
                              Interf_Param       )
!    reading input parameters
     ! Info initialization (header)
     call read_header ( File_Header, Info%header)
     Info%header%Operator = "OGP"
     Info%header%ProcessingStep = "on ground processing"
!
!    Interferogram initialisation
     iPos = 7
     open(unit=iFile, file=Files_Interf, err=999)
     iPos = 8
     read(iFile,*,err=999) PN_Mes
     read(iFile,*,err=999) Nband
     read(iFile,*,err=999) N_ColdSpace
     allocate(SB_List(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate(File_Interf_CS(N_ColdSpace), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     read(iFile,*,err=999) SB_List(1:Nband)
     if( (N_ColdSpace < 1) .or. (N_ColdSpace > 2) ) then
        write(*,*) 'Wrong Number of Cold Space Fatal Error', N_ColdSpace
        go to 999
     end if

     do CS = 1, N_ColdSpace
        read(iFile,'(a)',err=999) File_Interf_CS(CS)
     end do

     read(iFile,'(a)',err=999) File_Interf_BB
     read(iFile,'(a)',err=999) File_Interf_EW

     close(iFile)
!
!    Dps initialisation
     iPos = 2
     open(iFile, file=Files_Dps_Param, status='old', err=999)
     iPos = 3
     allocate(File_ReSamp_Param(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate(File_Decim_Param(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate(File_Trim_Param(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     do SB = 1, Nband
        read(iFile,'(a)',err=999) File_ReSamp_Param(SB)
        read(iFile,'(a)',err=999) File_Decim_Param(SB)
        read(iFile,'(a)',err=999) File_Trim_Param(SB)
     end do
     close(iFile)
!
!    level 1 file names initialisation
     iPos = 4
     open(iFile,file=Files_Level1_Param, err=999)
     iPos = 5
     read(iFile,fmt='(a)',err=999) Post_Calibration
     read(iFile,fmt='(a)',err=999) File_Level1b_Param
     read(iFile,fmt='(a)',err=999) File_Level1c_Param
     close(unit=iFile)
!
!    Level 1b initialisation
     call spectral_l1b_init( File_Level1b_Param,&
                             Spectral_L1b       )
!
!    Level 1c initialisation
     call spectral_l1c_init( File_Level1c_Param,&
                             Spectral_L1c       )
     iPos = 6
     if( (Interf_Param%Nband /= Spectral_L1b%Nband) .or. &
         (Interf_Param%Nband /= Spectral_L1c%Nband) ) then
        write(*,'(a,3i2)') 'Nband Error',Interf_Param%Nband, &
                                         Spectral_L1b%Nband, &
                                         Spectral_L1c%Nband
        go to 999
     end if
!
!    allocation
     iPos = 9
     if( Spectral_L1c%Nband /= Spectral_L1b%Nband ) then
        write(*,*) 'Level 1b 1c Nband Error',&
                    Spectral_L1b%Nband,Spectral_L1c%Nband
        go to 999
     end if
     iPos = 10
     allocate( Interf_Trim_CS(Nband, N_ColdSpace), &
                                                   stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Interf_Trim_BB(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Interf_Trim_EW(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Interf_Decim_CS(Nband, N_ColdSpace), &
                                                    stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Interf_Decim_BB(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Interf_Decim_EW(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Srf(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Apf(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Spectrum_L1a(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Spectrum_L1b(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate( Spectrum_L1c(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate(ReSamp_Param(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate(Decim_One_fir(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     allocate(Bit_Trimming(Nband), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     if( ioalloc /= 0 ) then
        write(*,*) 'allocation Error'
        go to 999
     end if
!
!    Srf initialisation
     iPos = 11
     open(unit=iFile, file=Files_Srf, err=999)
     iPos = 12
     if( ErrCode /= 0 ) ioalloc = ioalloc +1
     read(iFile,*,err=999) PN_Mes
     read(iFile,'(a)',err=999) Aggregation_Mode
     read(iFile,'(a)',err=999) File_Srf
     read(iFile,'(a)',err=999) File_Apf
     close(unit=iFile)
!
!    Parameters initialisation
     iPos = 13
     do SB = 1, Nband
!
!       Resampling Parameters initialisation
        call interf_resampling_init( File_ReSamp_Param(SB),&
                                     Resamp_Param(SB)      )
        write(*,*) 'End resampling_init'
!
!       Decimation parameters initialisation
        call decim_one_fir_init( File_Decim_Param(SB),&
                                Decim_One_fir(SB)     )
        write(*,*) 'End decim_one_fir_init'
!
!       Bit trimming initialisation
        call bit_trimming_init( File_Trim_Param(SB),&
                                Bit_Trimming(SB)    )
     end do

     write(Pixel,'(i4.4)') PN_Mes
     ! Level 1a Post Calibrated Binary output
     InfoL1a%header%filename = File_OutPut_(1:len_trim(File_OutPut_))//'Spectrum_L1a_'//'PN'//Pixel//'.nc'
     ! Level 1b
     InfoL1b%header%filename = File_OutPut_(1:len_trim(File_OutPut_))//'Spectrum_L1b_'//'PN'//Pixel//'.nc'
     ! Level 1c
     InfoL1c%header%filename = File_OutPut_(1:len_trim(File_OutPut_))//'Spectrum_L1c_'//'PN'//Pixel//'.nc'

     ! Info initialization (Nband,NcolFov,NlinFov,ColFov,LinFov,NsubFov,SubFov)
     Info%Nband   = Nband
     Info%NcolFov = 1
     Info%NlinFov = 1
     Info%ColFov  = 1
     Info%LinFov  = PN_Mes
     Info%NsubFov = 1
     Info%SubFov = 1
!
!    Spectral bands loop
     iPos = 14
     do SB = 1, Nband
        write(Band,'(i1.1)') SB_List(SB)
        ! Band
        Info%Band = SB_List(SB)
        write(*,*) '**************SB******* : ',SB,SB_List(SB)
!
        ! Srf initialization
        Srf(SB)%TopLevelSrf%Band = SB_List(SB)
        Srf(SB)%TopLevelSrf%header%filename = File_Srf
        Srf(SB)%TopLevelSrf%NsubFov = 1
        call readsrf_netcdf( Srf(SB), ErrCode )
        if( ErrCode /= 0 ) then
           write(*,*) 'readsrf Error', SB
           write(*,'(a)') 'Srf', trim(Srf(SB)%TopLevelSrf%header%filename)
           go to 999
        end if
!
        ! Apf initialization
        Apf(SB)%TopLevelApf%Band = SB_List(SB)
        Apf(SB)%TopLevelApf%header%filename = File_Apf
        Apf(SB)%TopLevelApf%NsubFov = 1
        call readapf_netcdf( Apf(SB), ErrCode )
        if( ErrCode /= 0 ) then
           write(*,*) 'readapf Error', SB
           write(*,'(a)') 'Apf',trim(Apf(SB)%TopLevelApf%header%filename)
           go to 999
        end if
!
        PN = Srf(SB)%NsPixel+1
        do NF = 1, Srf(SB)%NsWn0
           write(*,*) NF,Srf(SB)%Wn0(NF),&
                         Srf(SB)%WnShift1b(NF,PN),&
                         Srf(SB)%Fcs1b(NF,PN)
        end do
!
!       Cold Space Interferograms reading
        do CS = 1, N_ColdSpace
           Interf_Trim_CS(SB,CS)%TopLevelIn = Info
           Interf_Trim_CS(SB,CS)%TopLevelIn%header%filename = File_Interf_CS(CS)
           call readinterferogram_netcdf( Interf_Trim_CS(SB,CS) ,ErrCode )
           if( ErrCode /= 0 ) then
              write(*,'(a,2i3,a)') &
              'Interferogram reading Error CS ',SB, CS,&
              trim(Interf_Trim_CS(SB,CS)%TopLevelIn%header%filename)
              call exit(1)
           end if
        end do
!
!       Black Body Interferograms reading
        Interf_Trim_BB(SB)%TopLevelIn = Info
        Interf_Trim_BB(SB)%TopLevelIn%header%filename = File_Interf_BB
        call readinterferogram_netcdf( Interf_Trim_BB(SB) ,ErrCode )
        if( ErrCode /= 0 ) then
           write(*,'(a,i3,a)') &
           'Interferogram reading Error BB',SB,&
           trim(Interf_Trim_BB(SB)%TopLevelIn%header%filename)
           call exit(1)
        end if
!
!       Earth View Interferograms reading
        Interf_Trim_EW(SB)%TopLevelIn = Info
        Interf_Trim_EW(SB)%TopLevelIn%header%filename = File_Interf_EW
        write(*,*) 'EW SB ',SB,&
                    trim(Interf_Trim_EW(SB)%TopLevelIn%header%filename)
        call readinterferogram_netcdf( Interf_Trim_EW(SB) ,ErrCode )
        if( ErrCode /= 0 ) then
           write(*,'(a,i3,a)') &
           'Interferogram reading Error EW',SB,&
           trim(Interf_Trim_EW(SB)%TopLevelIn%header%filename)
           call exit(1)
        end if
!
        select case ( N_ColdSpace )
        case ( 1 )
!
!          Interferogram un bit trimming
           call interf_triplet_un_bit_trimming( Bit_Trimming(SB),     &
                                                Interf_Trim_CS(SB,1), &
                                                Interf_Trim_BB(SB),   &
                                                Interf_Trim_EW(SB),   &
                                                Interf_Decim_CS(SB,1),&
                                                Interf_Decim_BB(SB),  &
                                                Interf_Decim_EW(SB)   )
           call dalloc_Interferogram( Interf_Trim_CS(SB,1) )
           write(*,*) 'end interf_triplet un bit trimmed SB',&
                                             SB,SB_List(SB)
!
!          Radiometric complex calibration
           call radio_cal_decim_1cs( Interf_Param,         &
                                     Decim_One_fir(SB),    &
                                     Spectral_L1c,         &
                                     Apf(SB),              &
                                     Interf_Decim_CS(SB,1),&
                                     Interf_Decim_BB(SB),  &
                                     Interf_Decim_EW(SB),  &
                                     SB,                   &
                                     Spectrum_L1a(SB)      )
           call dalloc_Interferogram( Interf_Decim_CS(SB,1) )
           write(*,*) 'end radio_cal_decim SB',SB
        case default
           write(*,*) ' Wrong number of Cold Space',N_ColdSpace
           call exit(1)
        end select
        call dalloc_Interferogram( Interf_Trim_BB(SB) )
        call dalloc_Interferogram( Interf_Trim_EW(SB) )
        call dalloc_Interferogram( Interf_Decim_BB(SB) )
        call dalloc_Interferogram( Interf_Decim_EW(SB) )
        call dalloc_Bit_Trimming( Bit_Trimming(SB) )
        call dalloc_Decim_One_fir( Decim_One_fir(SB) )
!
!       extract the spectrum real part if necessary
        if( Spectrum_L1a(SB)%Type == 'C' ) then
           allocate( Spectrum_L1a(SB)%Real_Part(Spectrum_L1a(SB)%N_Sample) )
           Spectrum_L1a(SB)%Real_Part(1:Spectrum_L1a(SB)%N_Sample) =    &
             dreal(Spectrum_L1a(SB)%Complex(1:Spectrum_L1a(SB)%N_Sample))
           deallocate( Spectrum_L1a(SB)%Complex )
           Spectrum_L1a(SB)%Type = 'R'
        end if
!
!       Spectrum level 1a radiometric post calibration
        V_Doppler  = 0.d+00
        dV_Doppler = 0.d+00
        if( Post_Calibration == 'YES' ) then
           call spectrum_post_calib( Interf_Param,    &
                                     PN,              &
                                     V_Doppler,       &
                                     dV_Doppler,      &
                                     Srf(SB),         &
                                     Spectrum_L1a(SB) )
        end if
!
!       Level 1a Post Calibrated Binary output
        Spectrum_L1a(SB)%TopLevelSp = Info
        Spectrum_L1a(SB)%TopLevelSp%header%filename = InfoL1a%header%filename
        Spectrum_L1a(SB)%TopLevelSp%Target = Interf_Param%Target_EW
        call writespectrum_netcdf( Spectrum_L1a(SB), ErrCode, init_L1a )
        if( ErrCode /= 0 ) then
           write(*,*) 'writespectrum Error', SB
           write(*,'(a)') 'Spectrum_L1a',trim(Spectrum_L1a(SB)%TopLevelSp%header%filename)
           go to 999
        end if
!
!       spectrum level 1b computation
        call spectrum_calib_l1b( Spectral_L1b,    &
                                 SB, PN,          &
                                 V_Doppler,       &
                                 dV_Doppler,      &
                                 Srf(SB),         &
                                 Spectrum_L1a(SB),&
                                 Spectrum_L1b(SB) )
        write(*,*) 'End spectrum_calib_l1b'
!
!       apodisation
        select case ( Spectral_L1c%Mode(1:len_trim(Spectral_L1c%Mode)) )
        case ( 'REF' )
!          apodisation Reference
           call spectrum_apodspa_ref_l1c( Spectral_L1c,    &
                                          SB, PN,          &
                                          Srf(SB),         &
                                          Apf(SB),         &
                                          Spectrum_L1b(SB),&
                                          Spectrum_L1c(SB) )
           write(*,*) 'End spectrum_apodspa_ref_l1c'
        case ( 'OPT' )
!          apodisation Operational
           call spectrum_apodspa_opt_l1c( Spectral_L1c,    &
                                          SB, PN,          &
                                          Srf(SB),         &
                                          Apf(SB),         &
                                          Spectrum_L1b(SB),&
                                          Spectrum_L1c(SB) )
           write(*,*) 'End spectrum_apodspa_opt_l1c'
        case ( 'L1A' )
!          apodisation from L1a
           call spectrum_apodspa_l1a_l1c( Spectral_L1c,     &
                                          SB, PN,           &
                                          Srf(SB),          &
                                          Apf(SB),          &
                                          Spectrum_L1a(SB), &
                                          Spectrum_L1c(SB)  )
           write(*,*) 'End spectrum_apodspa_l1a_l1c'
        case ( 'L1B' )
!          apodisation from L1b Ref
           call spectrum_apodspa_l1b_l1c( Spectral_L1c,     &
                                          SB, PN,           &
                                          Srf(SB),          &
                                          Apf(SB),          &
                                          Spectrum_L1b(SB), &
                                          Spectrum_L1c(SB)  )
           write(*,*) 'End spectrum_apodspa_l1b_l1c'
        case ( 'CALREF' )
!          despatialisation Reference
           call spectrum_apodspa_ref_l1c( Spectral_L1c,    &
                                          SB, PN,          &
                                          Srf(SB),         &
                                          Apf(SB),         &
                                          Spectrum_L1b(SB),&
                                          Spectrum_L1c(SB) )
           write(*,*) 'End spectrum_apodspa_ref_l1c '
        case ( 'CALOPT' )
!          despatialisation Operational
           call spectrum_apodspa_opt_l1c( Spectral_L1c,    &
                                          SB, PN,          &
                                          Srf(SB),         &
                                          Apf(SB),         &
                                          Spectrum_L1b(SB),&
                                          Spectrum_L1c(SB) )
           write(*,*) 'End spectrum_apodspa_opt_l1c '
        case ( 'CALL1A' )
!          despatialisation from L1a
           call spectrum_apodspa_l1a_l1c( Spectral_L1c,     &
                                          SB, PN,           &
                                          Srf(SB),          &
                                          Apf(SB),          &
                                          Spectrum_L1a(SB), &
                                          Spectrum_L1c(SB)  )
           write(*,*) 'End spectrum_apodspa_l1a_l1c '
        case ( 'CALL1B' )
!          despatialisation from L1b Ref
           call spectrum_apodspa_l1b_l1c( Spectral_L1c,     &
                                          SB, PN,           &
                                          Srf(SB),          &
                                          Apf(SB),          &
                                          Spectrum_L1b(SB), &
                                          Spectrum_L1c(SB)  )
           write(*,*) 'End spectrum_apodspa_l1b_l1c '
        case default
           write(*,*) 'Spectral_L1c%Mode Error'
           go to 999
        end select
!
!       Binary output
        Spectrum_L1b(SB)%TopLevelSp = Info
        Spectrum_L1b(SB)%TopLevelSp%header%filename = InfoL1b%header%filename
        Spectrum_L1b(SB)%TopLevelSp%Target = Interf_Param%Target_EW
        call writespectrum_netcdf( Spectrum_L1b(SB), ErrCode, init_L1b )
        if( ErrCode /= 0 ) then
           write(*,*) 'writespectrum Error', SB
           write(*,'(a)') 'Spectrum_L1b',trim(Spectrum_L1b(SB)%TopLevelSp%header%filename)
           go to 999
        end if
        Spectrum_L1c(SB)%TopLevelSp = Info
        Spectrum_L1c(SB)%TopLevelSp%header%filename = InfoL1c%header%filename
        Spectrum_L1c(SB)%TopLevelSp%Target = Interf_Param%Target_EW
        call writespectrum_netcdf( Spectrum_L1c(SB), ErrCode, init_L1c )
        if( ErrCode /= 0 ) then
           write(*,*) 'writespectrum Error', SB
           write(*,'(a)') 'Spectrum_L1c',trim(Spectrum_L1c(SB)%TopLevelSp%header%filename)
           go to 999
        end if
!
!       Plots output
        File_OutPut = File_OutPut_(1:len_trim(File_OutPut_))&
                    //'Spectrum_L1a_'//'PN'//Pixel//'_SB'//Band//'.plt'
        call plot_spectre( File_OutPut,     &
                           Spectrum_L1a(SB) )
        File_OutPut = File_OutPut_(1:len_trim(File_OutPut_))&
                    //'Spectrum_L1b_'//'PN'//Pixel//'_SB'//Band//'.plt'
        call plot_spectre( File_OutPut,     &
                           Spectrum_L1b(SB) )
        File_OutPut = File_OutPut_(1:len_trim(File_OutPut_))&
                    //'Spectrum_L1c_'//'PN'//Pixel//'_SB'//Band//'.plt'
        call plot_spectre( File_OutPut,     &
                           Spectrum_L1c(SB) )
     end do ! end spectral bands loop
!
!    deallocation
     do SB = 1, Spectral_L1b%Nband ! boucle sur les bandes
        call dalloc_Srf( Srf(SB) )
        call dalloc_Apf( Apf(SB) )
        call dalloc_Spectrum( Spectrum_L1a(SB) )
        call dalloc_Spectrum( Spectrum_L1b(SB) )
        call dalloc_Spectrum( Spectrum_L1c(SB) )
     end do  ! fin de boucle sur les bandes
     deallocate( Srf )
     deallocate( Apf )
     deallocate( Spectrum_L1a )
     deallocate( Spectrum_L1b )
     deallocate( Spectrum_L1c )
     deallocate( File_Interf_CS )
     deallocate( SB_List )
!
     write(*,*) 'End onground_processing_prg'
!
   call exit(0)
999 write(*,*) 'onground_processing_prg fatal error',iPos
   call exit(1)
!
end program onground_processing_prg
