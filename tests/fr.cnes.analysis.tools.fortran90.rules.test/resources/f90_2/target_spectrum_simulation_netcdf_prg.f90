!!# target_spectrum_simulation_netcdf_prg.f90 --
!!#
!!#           Project: SPS_GENERIC
!!#           Authors: NOVELTIS/B.TOURNIER
!!#              Date: june 2013
!!#           Version: $Revision:$
!!# Last modification: $Date:$
!!#
!!
!> target_spectrum_simulation_prg -- Program
!!
!! * Purpose
!!
!!   Program for multi targets spectrum computation.
!!
!! * Description
!!        
!!   This main program produce input spectrum for instrument simulator
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 
!!
!! modified 22.07.2013 (JD) netcdf interfaces
!! modified 25.07.2013 (JD) spectrum and interferogram shared top level type

program target_spectrum_simulation_netcdf_prg
   use precision_type
   use error_type
   use constantes_type
   use interf_param_type
   use spectrum_type
   use emissivity_type
   use spectral_l1c_mb_type
   use header_type
   use spectral_l1bl1c_module
   use spectrum_hr_module
   use spectrum_module
   use spectrum_compress_module
   use emissivity_module
   use math_module
   use plot_module
!
   implicit none
   type(type_Spectral_L1c_mb)                            :: Spectral_hr
   type(type_Spectral_L1c_mb)                            :: Spectral_lr
   type(type_Interf_Param)                               :: Interf_Param
   type(type_Emissivity)    ,dimension(:),allocatable    :: Emissivity
   type(type_Spectrum)                                   :: Spectrum_hr
   type(type_Interferogram)                              :: Interf_hr
   type(type_Spectrum)                                   :: Spectrum_lr
   type(type_Interferogram)                              :: Interf_lr
   type(type_spect_interf_index)                         :: SpInfo ! Spectrum top level fields
   !
   integer(kind=LONG)                                    :: iFile
   integer(kind=LONG)                                    :: iPos
   character(len=2)                                      :: Target_Spectrum
   character(len=2)                                      :: Target_Type
   character(len=1)                                      :: Band
   integer(kind=LONG)                                    :: Nband
   integer(kind=LONG)                                    :: SB
   integer(kind=LONG)  ,dimension(:) ,allocatable        :: SB_List
   integer(kind=LONG)                                    :: N_Lagrange
   character(len=3)                                      :: Noise
   integer(kind=LONG)                                    :: ErrCode
   character(len=60)                                     :: Date
!
   character(len=500)                                    :: File_Spectrum_Param
   character(len=500)                                    :: Files_Emissivity
   character(len=500)  ,dimension(:) ,allocatable        :: File_Emissivity
   character(len=500)                                    :: Files_Spectrum_hr
   character(len=500)  ,dimension(:) ,allocatable        :: File_Spectrum_hr
   character(len=500)                                    :: File_hr_Param
   character(len=500)                                    :: File_lr_Param
   character(len=500)  ,dimension(:) ,allocatable        :: File_Noise
   character(len=500)                                    :: File_Output_
   character(len=500)                                    :: File_Output
   character(len=500)                                    :: File_Header
   integer(kind=SHORT)                                   :: init_sp
!
   iFile = 10
   iPos = 1
   ErrCode = 0
   init_sp = 0
!
!  reading File_Spectrum_Param
   read(*,'(a)',err=999) File_Spectrum_Param
   read(*,'(a)',err=999) Files_Emissivity
   read(*,'(a)',err=999) Files_Spectrum_hr
   read(*,'(a)',err=999) File_Output_
   read(*,'(a)',err=999) File_Header
   read(*,'(a)',err=999) Date
   write(*,'(a)') File_Spectrum_Param(1:len_trim(File_Spectrum_Param))
   write(*,'(a)') Files_Emissivity(1:len_trim(Files_Emissivity))
   write(*,'(a)') Files_Spectrum_hr(1:len_trim(Files_Spectrum_hr))
   write(*,'(a)') File_Header(1:len_trim(File_Header))
   write(*,'(a)') File_Output_(1:len_trim(File_Output_))

!
!  noise generator initialisation
   write(*,'(a)') Date
   call grnd_init(Date)
!
!  Interf_Param initialisation
   Interf_Param%Phase_Poly_Deg = 1
   Interf_Param%VCC_Nb_Freq = 1
   Interf_Param%VCC_Nb_Seg = 1
   !  reading input parameters
   ! SpInfo initialization (header)
   call read_header ( File_Header, SpInfo%header)
   SpInfo%header%filename = File_Output_(1:len_trim(File_Output_))//'.nc'
   SpInfo%header%Operator = "SGM"
   SpInfo%header%ProcessingStep = "target spectrum simulation"
   !
   iPos = 2
   open(iFile, file=File_Spectrum_Param, status='old', err=999)
   iPos = 3
   read(iFile,'(a)',err=999) Target_Spectrum
   read(iFile,'(a)',err=999) Target_Type
   read(iFile,*,err=999) Nband
   Interf_Param%Nband = Nband
   call alloc_Interf_Param( Interf_Param )
   allocate( SB_List(Nband), stat=ErrCode )
   if (ErrCode /= 0) then
      write(*,*) 'Allocation Error'
      go to 999
   end if
   !
   ! SpInfo initialization (Target,Nband,NcolFov,NlinFov,ColFov,LinFov,NSubFov,SubFov)
   SpInfo%Target = Target_Spectrum
   SpInfo%Nband = Interf_Param%Nband
   SpInfo%NcolFov = 1
   SpInfo%NlinFov = 1
   SpInfo%ColFov = 1
   SpInfo%LinFov = 1
   SpInfo%NSubFov = 1
   SpInfo%SubFov = 1
   !
   read(iFile,*,err=999) (SB_List(SB),SB=1,Nband)
   do SB = 1, Nband
      read(iFile,*,err=999) Interf_Param%WnLaser_EW(SB)
      read(iFile,*,err=999) Interf_Param%Laser_Intensity(SB)
      read(iFile,*,err=999) Interf_Param%Flat_Intensity(SB)
   end do
   read(iFile,*,err=999) Interf_Param%LambdaLaserRpd
   read(iFile,*,err=999) Interf_Param%OSFactor
   read(iFile,*,err=999) Interf_Param%T_CS
   read(iFile,*,err=999) Interf_Param%T_BB
   read(iFile,*,err=999) Interf_Param%T_EW
   read(iFile,*,err=999) N_Lagrange
   read(iFile,'(a)',err=999) Noise
   iPos = 4
   allocate( File_Noise(Nband), stat=ErrCode )
   if (ErrCode /= 0) then
      write(*,*) 'Allocation Error'
      go to 999
   end if
   read(iFile,'(a)',err=999) File_hr_Param
   read(iFile,'(a)',err=999) File_lr_Param
   if( Noise == 'YES' ) then
      do SB = 1, Nband
         read(iFile,'(a)',err=999) File_Noise(SB)
      end do
   end if
   close(iFile)
!
   iPos = 10
   open(iFile, file=Files_Spectrum_hr, status='old', err=999)
   iPos = 11
   allocate( File_Spectrum_hr(Nband), stat=ErrCode )
   if (ErrCode /= 0) then
      write(*,*) 'Allocation Error'
      go to 999
   end if
   do SB = 1, Nband
      read(iFile,'(a)',err=999) File_Spectrum_hr(SB)
   end do
   close(iFile)
!
!  emissivity file parameters reading
   iPos = 12
   allocate(File_Emissivity(Nband), stat=ErrCode )
   if (ErrCode /= 0) then
      write(*,*) 'Allocation Error'
      go to 999
   end if
   allocate(Emissivity(Nband), stat=ErrCode )
   if (ErrCode /= 0) then
      write(*,*) 'Allocation Error'
      go to 999
   end if
   open(iFile, file=Files_Emissivity, status='old', err=999)
   do SB = 1, Nband
      read(iFile,'(a)',err=999) File_Emissivity(SB)
   end do
   close(iFile)
!
!  Spectral parameters initialisation
   call spectral_l1c_mb_init( File_hr_Param, &
                              Spectral_hr    )
   call spectral_l1c_mb_init( File_lr_Param, &
                              Spectral_lr    )
   do SB = 1, Nband
      Interf_Param%Wn_First(SB) = Spectral_lr%WnS(SB)
      Interf_Param%Wn_Last(SB)  = Spectral_lr%WnE(SB)
   end do
!
!  Spectral band separation
   if( (Nband /= Spectral_hr%Nband) .or.             &
       (Spectral_hr%Nband /= Spectral_lr%Nband) ) then
      write(*,*) 'Inconsistency between lr and hr Nband'
      call exit(1)
   end if

   do SB = 1, Spectral_hr%Nband
      iPos = 20 + SB
      write(Band,'(i1:i1)') SB_List(SB)
      ! Band
      SpInfo%Band = SB_List(SB)
!
!     Emissivity parameters initialisation
      call emissivity_init( File_Emissivity(SB),&
                            Emissivity(SB)      )
      call spectrum_target( Spectral_hr,         &
                            Interf_Param,        &
                            Emissivity(SB),      &
                            File_Spectrum_hr(SB),&
                            Target_Spectrum,     &
                            Target_Type,         &
                            SB,                  &
                            Spectrum_hr          )
!
!
      if( Spectral_hr%dWn(SB) < Spectral_lr%dWn(SB) ) then
!
!        Spectrum reduction sampling
         call spectrum_to_interf( Spectral_hr, &
                                  SB,          &
                                  Spectrum_hr, &
                                  Interf_hr    )
         call interf_to_spectrum( Spectral_lr,   &
                                  Interf_Param,  &
                                  N_Lagrange,    &
                                  SB, Noise,     &
                                  File_Noise(SB),&
                                  Interf_hr,     &
                                  Interf_lr,     &
                                  Spectrum_lr    )

         call dalloc_Interferogram( Interf_hr )
         call dalloc_Interferogram( Interf_lr )
      else if( Spectral_hr%dWn(SB) == Spectral_lr%dWn(SB) ) then
!
!        Transfert High resolution spectrum
         call spectrum_transfert( Spectrum_hr, Spectrum_lr )
      else
         write(*,*) 'Wrong spectral low resolution ',&
                     Spectral_hr%dWn(SB), Spectral_lr%dWn(SB)
         ErrCode = -1
      end if
      call dalloc_Spectrum( Spectrum_hr )
!
!     write spectrum netCDF
!     Transfert top level variables from SpInfo to Spectrum_lr
      Spectrum_lr%TopLevelSp = SpInfo

      call writespectrum_netcdf( Spectrum_lr, ErrCode, init_sp )
      if( ErrCode /= 0 ) then
         write(*,*) 'writespectrum_netcdf Fatal Error'
         go to 999
      end if
!
!     plot spectrum
      File_Output = File_Output_(1:len_trim(File_Output_))&
                    //'_SB'//Band//'.asc'
      call  plot_spectre( File_OutPut,&
                          Spectrum_lr )
      !
      call dalloc_Spectrum( Spectrum_lr )
!
   end do !End spectral band loop
   call dalloc_Interf_Param( Interf_Param )
   write(*,*) 'End target_spectrum_simulation_netcdf_prg'
!
   call exit(0)
999 write(*,*) 'target_spectrum_simulation_netcdf_prg fatal error',iPos
   call exit(1)
!
end program target_spectrum_simulation_netcdf_prg
