!!# isrf_model_netcdf_prg.f90 --
!!#
!!#           Project: SPS_GENERIC
!!#           Authors: NOVELTIS/B.TOURNIER
!!#              Date: october 2009
!!#           Version: $Revision: 1.2 $
!!# Last modification: $Date: 2011-06-22 10:06:27 $
!!#
!!
!> isrf_model_prg -- Module
!!
!! * Purpose
!!
!!   Program for ISRF simulation.
!!
!! * Description
!!        
!!   This principal program allows the ISRF simulation: this computation 
!!   requires the following steps:    
!!     - the first one is the reading of inputs parameters 
!!     - then, initialisation of noise generator is required, as well as 
!!       Srf parameters initialisation
!!     - before the SRF computation, all required tables are allocated and 
!!       detection gain is read
!!     - at last, before tables deallocation, outputs are written both 
!!       in ascii and in binary files deallocation
!!
!! * References
!!
!!   SPS ATBD
!! 26.07.2013 (JD) netCDF interfaces

program isrf_model_netcdf_prg
   use srf_param_type
   use psf_type
   use chrom_type
   use fcomp_type
   use ccm_type
   use saf_type
   use srf_type
   use apf_type
   use isrf_index_type
   use gain_type
   use refrac_index_type
   use pupil_type
   use mertz_type
   use isrf_model_computation_module
   use gain_module
   use math_module
   use plot_module
!
   implicit none
   type(type_Srf_Param)                             :: Srf_Param
   type(type_Psf)                                   :: Psf_Ref
   type(type_Psf)                                   :: Psf_Rpd
   type(type_Psf)                                   :: Psf_Mes
   type(type_Chrom)                                 :: Chrom
   type(type_Chrom)                                 :: Chrom_Rpd
   type(type_Fcomp)                                 :: Fcomp
   type(type_Ccm)                                   :: Ccm
   type(type_Ccm)                                   :: Ccm_Rpd
   type(type_Saf)                                   :: Saf_Rpd
   type(type_Saf)         ,dimension(:),allocatable :: Saf
   type(type_Saf)         ,dimension(:),allocatable :: Sas
   type(type_Srf)         ,dimension(:),allocatable :: Srf
   type(type_Apf)         ,dimension(:),allocatable :: Apf
   type(type_Gain)        ,dimension(:),allocatable :: Gain
   type(type_RefracIndex)                           :: RefracIndex_Rpd   
   type(type_RefracIndex)                           :: RefracIndex_Mes   
   type(type_Pupil)                                 :: Pupil
   type(type_Ccm)                                   :: Prism_Motion
   type(type_Mertz)                                 :: Mertz
   type(type_isrf_index)                            :: Info ! top level fields
   type(type_isrf_index)                            :: InfoSafRpd ! top level fields
   type(type_isrf_index)                            :: InfoSaf ! top level fields
   type(type_isrf_index)                            :: InfoSas ! top level fields
   type(type_isrf_index)                            :: InfoSrf ! top level fields
   type(type_isrf_index)                            :: InfoApf ! top level fields
   character(len=60)                                :: Date
!
   integer(kind=LONG)                               :: allocerror
   integer(kind=LONG)                               :: ErrCode
   integer(kind=LONG)                               :: SB
   integer(kind=LONG)                               :: iFile
!
   character(len=500)                               :: Files_param_Inputs
   character(len=500)                               :: Files_Gain
   character(len=500) ,dimension(:),allocatable     :: File_Gain
   character(len=500)                               :: Files_Results_
   character(len=500)                               :: Files_Results
   character(len=500)                               :: File_Header
   character(len=1)                                 :: Band
   integer(kind=SHORT)                              :: init_saf_rpd,init_saf,init_sas,init_srf,init_apf
!
     iFile = 10

     init_saf_rpd = 0
     init_saf = 0
     init_sas = 0
     init_srf = 0
     init_apf = 0
!
!    read inputs parameters
     read(*,fmt='(a)',err=999) Files_param_Inputs
     read(*,fmt='(a)',err=999) Files_Gain
     read(*,fmt='(a)',err=999) Files_Results_
     read(*,fmt='(a)',err=999) File_Header
     read(*,fmt='(a)',err=999) Date
!
!    reading input parameters
     ! Info initialization (header)
     call read_header ( File_Header, Info%header)
     Info%header%Operator = "SRF"
     Info%header%ProcessingStep = "isrf model"
     ! Saf_Rpd output
     InfoSafRpd%header%filename = Files_Results_(1:len_trim(Files_Results_))//'Saf_Rpd.nc'
     ! Saf output
     InfoSaf%header%filename = Files_Results_(1:len_trim(Files_Results_))//'Saf.nc'
     ! Sas output
     InfoSas%header%filename = Files_Results_(1:len_trim(Files_Results_))//'Sas.nc'
     ! Srf output
     InfoSrf%header%filename = Files_Results_(1:len_trim(Files_Results_))//'Srf.nc'
     ! Apf output
     InfoApf%header%filename = Files_Results_(1:len_trim(Files_Results_))//'Apf.nc'
!
!    noise generator initialisation
     write(*,'(a)') Date
     call grnd_init(Date)
!
!    Srf parameters initialisation
     call isrf_model_init( Files_param_Inputs, &
                           Srf_Param, Psf_Ref, &
                           Psf_Rpd, Psf_Mes,   &
                           Ccm_Rpd, Ccm,       &
                           Chrom_Rpd, Chrom,   &
                           RefracIndex_Rpd,    &
                           RefracIndex_Mes,    &
                           Pupil, Mertz,       &
                           Prism_Motion,       &
                           Fcomp  )
!
!    Psf in ascii files
     call plot_psf( Files_Results_,  &
                    Psf_Ref,         &
                    Psf_Rpd, Psf_Mes )
!
!
     ! Info initialization (Nband,NcolFov,NlinFov,ColFov,LinFov,NsubFov)
     Info%Nband   = Srf_Param%Nband
     Info%NcolFov = 1
     Info%NlinFov = 1
     Info%ColFov  = 1
     Info%LinFov  = Srf_Param%PN_Mes
     Info%NsubFov = Srf_Param%Mes_NsPsf
!
!    allocation
     allocerror = 0
     allocate( Saf(Srf_Param%Nband), stat=ErrCode )
     if( ErrCode /= 0 ) allocerror = allocerror + 1
     allocate( Sas(Srf_Param%Nband), stat=ErrCode )
     if( ErrCode /= 0 ) allocerror = allocerror + 1
     allocate( Srf(Srf_Param%Nband), stat=ErrCode )
     if( ErrCode /= 0 ) allocerror = allocerror + 1
     allocate( Apf(Srf_Param%Nband), stat=ErrCode )
     if( ErrCode /= 0 ) allocerror = allocerror + 1
     if( allocerror /= 0 ) then
        write(*,*) ' Allocation Error', allocerror
        call exit(1)
     end if
!
!    Detection gain initialisation
     allocate(File_Gain(Srf_Param%Nband), stat=ErrCode )
     if( ErrCode /= 0 ) allocerror = allocerror + 1
     allocate( Gain(Srf_Param%Nband), stat=ErrCode )
     if( ErrCode /= 0 ) allocerror = allocerror + 1
     open(iFile, file=Files_Gain, status='old', err=999)
     do SB = 1, Srf_Param%Nband
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
!    Srf computation
     call isrf_model_sub( Srf_Param, Psf_Ref,              &
                          Psf_Rpd, Psf_Mes,                &
                          Gain, Chrom_Rpd, Chrom,          &
                          Pupil, Prism_Motion, Mertz,      &
                          RefracIndex_Rpd, RefracIndex_Mes,&
                          Fcomp, Ccm_Rpd, Ccm, Saf_Rpd,    &
                          Saf, Sas, Srf, Apf               )
!
!    outputs in ascii files
     call plot_spectral( Files_Results_,    &
                         Srf_Param, Saf_Rpd,&
                         Saf, Sas, Apf, Srf )
     Files_Results = Files_Results_(1:len_trim(Files_Results_))&
                   //'Sas_'
     do SB = 1, Srf_Param%Nband
        call plot_saf( Files_Results,        &
                       Srf_Param%PN_Mes,     &
                       Srf_Param%SB_List(SB),&
                       Sas(SB) )
     end do
     Files_Results = Files_Results_(1:len_trim(Files_Results_))&
                   //'Saf_'
     do SB = 1, Srf_Param%Nband
        call plot_saf( Files_Results,        &
                       Srf_Param%PN_Mes,     &
                       Srf_Param%SB_List(SB),&
                       Saf(SB) )
     end do
!
!    Ccm in ascii files
     call plot_ccm( Files_Results_, &
                    Ccm, Ccm_Rpd    )
!
     call dalloc_Psf( Psf_Ref )
     call dalloc_Psf( Psf_Rpd )
     call dalloc_Psf( Psf_Mes )
     call dalloc_Ccm( Ccm )
     call dalloc_Ccm( Ccm_Rpd )
!
!    Output in binary files
     write(*,'(a)') Saf_Rpd%filename(1:len_trim(Saf_Rpd%filename))

     ! Saf_Rpd initialization
     Saf_Rpd%TopLevelSaf%header = Info%header
     Saf_Rpd%TopLevelSaf%Nband   = Srf_Param%Nband
     Saf_Rpd%TopLevelSaf%NcolFov = 1
     Saf_Rpd%TopLevelSaf%NlinFov = 1
     Saf_Rpd%TopLevelSaf%ColFov  = 1
     Saf_Rpd%TopLevelSaf%LinFov  = Srf_Param%PN_Mes
     Saf_Rpd%TopLevelSaf%NsubFov = Srf_Param%Rpd_NsPsf
     Saf_Rpd%TopLevelSaf%Band = 1
     Saf_Rpd%TopLevelSaf%header%filename = InfoSafRpd%header%filename

     call writesaf_netcdf( Saf_Rpd, ErrCode, init_saf_rpd )
     if( ErrCode /= 0 ) then
        write(*,*) 'writesaf_netcdf Fatal Error'
        go to 999
     end if

     write(0,*) 'end writesaf Rpd'
     do SB = 1, Srf_Param%Nband ! boucle sur les bandes
        write(Band,'(i1.1)') Srf_Param%SB_List(SB)
        ! Info initialization (Band)
        Info%Band = Srf_Param%SB_List(SB)

        write(*,'(a)') Saf(SB)%filename(1:len_trim(Saf(SB)%filename))

        ! Saf initialization
        Saf(SB)%TopLevelSaf%header = Info%header
        Saf(SB)%TopLevelSaf%header%filename = InfoSaf%header%filename
        Saf(SB)%TopLevelSaf%Nband   = Srf_Param%Nband
        Saf(SB)%TopLevelSaf%Band = Srf_Param%SB_List(SB)
        Saf(SB)%TopLevelSaf%NcolFov = 1
        Saf(SB)%TopLevelSaf%NlinFov = 1
        Saf(SB)%TopLevelSaf%ColFov  = 1
        Saf(SB)%TopLevelSaf%LinFov  = Srf_Param%PN_Mes
        Saf(SB)%TopLevelSaf%NsubFov =  Srf_Param%Mes_NsPsf

        call writesaf_netcdf( Saf(SB), ErrCode, init_saf )
        if( ErrCode /= 0 ) then
           write(*,*) 'writesaf_netcdf Fatal Error'
           go to 999
        end if

        write(0,*) 'end writesaf',Srf_Param%SB_List(SB)

        write(*,'(a)') Sas(SB)%filename(1:len_trim(Sas(SB)%filename))

        ! Sas initialization
        Sas(SB)%TopLevelSaf%header = Info%header
        Sas(SB)%TopLevelSaf%header%filename = InfoSas%header%filename
        Sas(SB)%TopLevelSaf%Nband   = Srf_Param%Nband
        Sas(SB)%TopLevelSaf%Band = Srf_Param%SB_List(SB)
        Sas(SB)%TopLevelSaf%NcolFov = 1
        Sas(SB)%TopLevelSaf%NlinFov = 1
        Sas(SB)%TopLevelSaf%ColFov  = 1
        Sas(SB)%TopLevelSaf%LinFov  = Srf_Param%PN_Mes
        Sas(SB)%TopLevelSaf%NsubFov =  Srf_Param%Mes_NsPsf

        call writesaf_netcdf( Sas(SB), ErrCode, init_sas )
        if( ErrCode /= 0 ) then
           write(*,*) 'writesaf_netcdf Fatal Error'
           go to 999
        end if

        write(0,*) 'end writesas',Srf_Param%SB_List(SB)

        write(*,'(a)') Srf(SB)%filename(1:len_trim(Srf(SB)%filename))

        ! Srf initialization
        Srf(SB)%TopLevelSrf%header = Info%header
        Srf(SB)%TopLevelSrf%header%filename = InfoSrf%header%filename
        Srf(SB)%TopLevelSrf%Nband   = Srf_Param%Nband
        Srf(SB)%TopLevelSrf%Band = Srf_Param%SB_List(SB)
        Srf(SB)%TopLevelSrf%NcolFov = 1
        Srf(SB)%TopLevelSrf%NlinFov = 1
        Srf(SB)%TopLevelSrf%ColFov  = 1
        Srf(SB)%TopLevelSrf%LinFov  = Srf_Param%PN_Mes
        Srf(SB)%TopLevelSrf%NsubFov =  Srf_Param%Mes_NsPsf

        call writesrf_netcdf( Srf(SB), ErrCode, init_srf )
        if( ErrCode /= 0 ) then
           write(*,*) 'writesrf_netcdf Fatal Error'
           go to 999
        end if

        write(*,*) 'WnShift1a',Srf(SB)%WnShift1a
        write(*,*) 'WnShift1b',Srf(SB)%WnShift1b
        write(*,*) 'WnShift1c',Srf(SB)%WnShift1c
        write(0,*) 'end writesrf',Srf_Param%SB_List(SB)

        write(*,'(a)') Apf(SB)%filename(1:len_trim(Apf(SB)%filename))

        ! Apf initialization
        Apf(SB)%TopLevelApf%header = Info%header
        Apf(SB)%TopLevelApf%header%filename = InfoApf%header%filename
        Apf(SB)%TopLevelApf%Nband   = Srf_Param%Nband
        Apf(SB)%TopLevelApf%Band = Srf_Param%SB_List(SB)
        Apf(SB)%TopLevelApf%NcolFov = 1
        Apf(SB)%TopLevelApf%NlinFov = 1
        Apf(SB)%TopLevelApf%ColFov  = 1
        Apf(SB)%TopLevelApf%LinFov  = Srf_Param%PN_Mes
        Apf(SB)%TopLevelApf%NsubFov =  Srf_Param%Mes_NsPsf

        call writeapf_netcdf( Apf(SB), ErrCode, init_apf )
        if( ErrCode /= 0 ) then
           write(*,*) 'writeapf_netcdf Fatal Error'
           go to 999
        end if

        write(*,*) 'end writeapf',Srf_Param%SB_List(SB)
        call dalloc_Gain( Gain(SB) )
        call dalloc_Saf( Saf(SB) )
        call dalloc_Saf( Sas(SB) )
        call dalloc_Srf( Srf(SB) )
        call dalloc_Apf( Apf(SB) )
     end do  ! fin de boucle sur les bandes
     call dalloc_Saf( Saf_Rpd )
!
!    deallocation
     deallocate( Gain )
     deallocate( Saf )
     deallocate( Sas )
     deallocate( Srf )
     deallocate( Apf )
     call dalloc_Srf_Param( Srf_Param )
     if( ErrCode /= 0 ) then
        write(0,*) 'Writing Result files Error'
        goto 999
     end if
   call exit(0)
999 write(*,*) 'isrf_model_netcdf_prg fatal error'
   call exit(1)
!
end program isrf_model_netcdf_prg
