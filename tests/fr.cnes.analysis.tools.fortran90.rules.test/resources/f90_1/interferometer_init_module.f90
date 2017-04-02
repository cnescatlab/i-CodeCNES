!!# interferometer_init_module.f90 --
!!#
!!#           Project: SPS_GENERIC
!!#           Authors: NOVELTIS/B.TOURNIER
!!#              Date: october 2009
!!#           Version: $Revision: 1.11 $
!!# Last modification: $Date: 2012-02-08 10:19:14 $
!!#
!!# Language:  F90
!!# Standards: Noveltis
!!#
!!# --
!!#
!!
!> interferometer_init_module -- Module
!!
!! * Purpose
!!
!!   Module for interferometer parameters definition
!!
!! * Description
!!      
!! This module defines 
!!
!! * Sub-routines and functions
!!
!! - interferometer_def : initialisation of the type defining the interferogram acquisition parameters 
!! - scene_mixing_def   : initialisation of the type defining the scene parameters
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.4.2
!!

module interferometer_init_module
   use precision_type
   use error_type
   use constantes_type
   use interf_param_type
!
   implicit none
!
!
   public :: interferometer_def
!
!
   contains


!> interferometer_def -- Public
!!
!! * Purpose
!!
!!     Initialisation of the type defining the interferogram acquisition parameters 
!!
!! * Description
!!
!!     The interferometer_def subroutine allows to update the type defining the interferogram 
!!     acquisition parameters thanks to the loading of the Files_param_Inputs file ; 
!!     it updates interferometer parameters, radiometric response and radiometric noise.
!!
!! * Inputs
!!
!!     - Files_param_Inputs : character / input file with definition of interferogram 
!!                            acquisition parameters
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Interf_Param : type_Interf_Param / type for interf parameters declaration and allocation.
!!
!! * References
!!

   subroutine interferometer_def( Files_param_Inputs, &
                                  Interf_Param        )
   implicit none
     character(len=500)      , intent(in)                :: Files_param_Inputs
     type(type_Interf_Param) , intent(out)               :: Interf_Param
     integer(kind=LONG)                                  :: iFile
     integer(kind=LONG)                                  :: iPos
     integer(kind=LONG)                                  :: ErrCode
     character(len=500)                                  :: Fileinterfparam
     integer(kind=LONG)                                  :: ND
     integer(kind=LONG)                                  :: NF
     integer(kind=LONG)                                  :: SB
!
     iFile = 10
!
     iPos = 1
     open(unit=iFile, file=Files_param_Inputs, status='old',err=999)
     iPos = 2
     read(iFile,fmt='(a)',err=999) Fileinterfparam
     close(unit=iFile)
     write(*,'(a)') Fileinterfparam(1:len_trim(Fileinterfparam))
!
!    interferometer parameters
     iPos = 3
     open(unit=iFile, file=Fileinterfparam, status='old',err=999)
     iPos = 4
     Interf_Param%filename = Fileinterfparam
     read(iFile,*,err=999) Interf_Param%Type
     read(iFile,*,err=999) Interf_Param%Rpd_Type
     read(iFile,*,err=999) Interf_Param%Sampling_Mode
     read(iFile,*,err=999) Interf_Param%Integration_Mode
     read(iFile,*,err=999) Interf_Param%option_cnv
     write(*,'(a)') Interf_Param%Type
     write(*,'(a)') Interf_Param%Rpd_Type
     write(*,'(a)') Interf_Param%Sampling_Mode
     write(*,'(a)') Interf_Param%Integration_Mode
     write(*,'(a)') Interf_Param%option_cnv
     read(iFile,*,err=999)     Interf_Param%LambdaLaserRpd
     read(iFile,*,err=999)     Interf_Param%LambdaLaserJitter
     read(iFile,*,err=999)     Interf_Param%LambdaLaserOffset
     Interf_Param%WnLaserRpd = 1.d+00 / Interf_Param%LambdaLaserRpd
     write(*,'(a,e12.6)') 'Interf_Param%WnLaserRpd'&
                          ,Interf_Param%WnLaserRpd
     write(*,'(a,e12.6)') 'Interf_Param%LambdaLaserRpd'&
                          ,Interf_Param%LambdaLaserRpd
     write(*,'(a,e12.6)') 'Interf_Param%LambdaLaserOffset'&
                          ,Interf_Param%LambdaLaserOffset
     write(*,'(a,e12.6)') 'Interf_Param%LambdaLaserJitter'&
                          ,Interf_Param%LambdaLaserJitter
     read(iFile,*,err=999)     Interf_Param%VCC
     write(*,'(a,f10.6)') 'Interf_Param%VCC',Interf_Param%VCC
     read(iFile,*,err=999)     Interf_Param%Ratio_Opd
     write(*,'(a,f10.6)') 'Interf_Param%Ratio_Opd',Interf_Param%Ratio_Opd
     read(iFile,*,err=999)     Interf_Param%OpdMax
     write(*,'(a,f10.6)') 'Interf_Param%OpdMax',Interf_Param%OpdMax
     read(iFile,*,err=999)     Interf_Param%OSFactor
     write(*,'(a,i10)') 'Interf_Param%OSFactor',Interf_Param%OSFactor
     read(iFile,*,err=999)     Interf_Param%Phase_Poly_Deg
     write(*,'(a,i10)') 'Interf_Param%Phase_Poly_Deg',Interf_Param%Phase_Poly_Deg
     allocate( Interf_Param%Phase_Poly_Coeff(0:Interf_Param%Phase_Poly_Deg),&
                                                               stat=ErrCode )
     do ND = 0, Interf_Param%Phase_Poly_Deg
       read(iFile,*,err=999)   Interf_Param%Phase_Poly_Coeff(ND)
       write(*,'(a,e14.6)') 'Interf_Param%Phase_Poly_Coeff(ND)',&
                             Interf_Param%Phase_Poly_Coeff(ND)
     end do
!
!    corner cube speed variation
     read(iFile,*,err=999)     Interf_Param%VCC_Nb_Freq
     write(*,'(a,i10)') 'Interf_Param%VCC_Nb_Freq',Interf_Param%VCC_Nb_Freq
     if( Interf_Param%VCC_Nb_Freq /= 0 ) then
        allocate( Interf_Param%VCC_Amp_muvib(Interf_Param%VCC_Nb_Freq), &
                                                           stat=ErrCode )
        allocate( Interf_Param%VCC_Freq_muvib(Interf_Param%VCC_Nb_Freq),&
                                                           stat=ErrCode )
        allocate( Interf_Param%VCC_Phase_muvib(Interf_Param%VCC_Nb_Freq),&
                                                            stat=ErrCode )
        read(iFile,*,err=999)    (Interf_Param%VCC_Amp_muvib(NF),&
                               NF=1,Interf_Param%VCC_Nb_Freq)
        write(*,'(a,f13.4)') 'Interf_Param%VCC_Amp_muvib(NF)',&
                 (Interf_Param%VCC_Amp_muvib(NF),NF=1,Interf_Param%VCC_Nb_Freq)
        read(iFile,*,err=999)    (Interf_Param%VCC_Freq_muvib(NF),&
                                  NF=1,Interf_Param%VCC_Nb_Freq)
        write(*,'(a,f13.9)') 'Interf_Param%VCC_Freq_muvib(NF)',&
                              (Interf_Param%VCC_Freq_muvib(NF),&
                                  NF=1,Interf_Param%VCC_Nb_Freq)
        read(iFile,*,err=999)    (Interf_Param%VCC_Phase_muvib(NF),&
                                  NF=1,Interf_Param%VCC_Nb_Freq)
        write(*,'(a,f13.9)') 'Interf_Param%VCC_Phase_muvib(NF)',&
                              (Interf_Param%VCC_Phase_muvib(NF),&
                                   NF=1,Interf_Param%VCC_Nb_Freq)
        read(iFile,*,err=999)
        read(iFile,*,err=999)
        read(iFile,*,err=999)
        read(iFile,*,err=999)
     else
        read(iFile,*,err=999)
        read(iFile,*,err=999)
        read(iFile,*,err=999)
        read(iFile,*,err=999)     Interf_Param%VCC_Nb_Seg
        write(*,'(a,i10)') 'Interf_Param%VCC_Nb_Seg ',Interf_Param%VCC_Nb_Seg
        if( Interf_Param%VCC_Nb_Seg /= 0 ) then
           allocate( Interf_Param%VCC_Course(Interf_Param%VCC_Nb_Seg), &
                                                          stat=ErrCode )
           allocate( Interf_Param%VCC_A0(Interf_Param%VCC_Nb_Seg), &
                                                      stat=ErrCode )
           allocate( Interf_Param%VCC_A1(Interf_Param%VCC_Nb_Seg), &
                                                      stat=ErrCode )
           do NF = 1, Interf_Param%VCC_Nb_Seg
              read(iFile,*,err=999)    Interf_Param%VCC_Course(NF)
              read(iFile,*,err=999)    Interf_Param%VCC_A0(NF)
              read(iFile,*,err=999)    Interf_Param%VCC_A1(NF)
              write(*,'(a,2f13.9,f10.4)') 'Interf_Param%VCC_A0 A1 Course',&
                         Interf_Param%VCC_A0(NF), Interf_Param%VCC_A1(NF),&
                         Interf_Param%VCC_Course(NF)
           end do
        else
           read(iFile,*,err=999)
           read(iFile,*,err=999)
           read(iFile,*,err=999)
        end if
     end if
!
!    Sampling parameters
     read(iFile,*,err=999)     Interf_Param%Rpd_Samp_OSFactor
     write(*,'(a,i10)') 'Interf_Param%Rpd_Samp_OSFactor',&
                         Interf_Param%Rpd_Samp_OSFactor
     read(iFile,*,err=999)     Interf_Param%dT_Mes_Samp
     Interf_Param%dT_Rpd_Samp = &
           Interf_Param%dT_Mes_Samp / dble(Interf_Param%Rpd_Samp_OSFactor)
     write(*,'(a,f13.9)') 'Interf_Param%dT_Mes_Samp',Interf_Param%dT_Mes_Samp
     write(*,'(a,f13.9)') 'Interf_Param%dT_Rpd_Samp',Interf_Param%dT_Rpd_Samp
     read(iFile,*,err=999)     Interf_Param%Noise_Rpd_Std
     write(*,'(a,f13.9)') 'Interf_Param%Noise_Rpd_Std',&
                           Interf_Param%Noise_Rpd_Std
     read(iFile,*,err=999)     Interf_Param%Rpd_Delay
     write(*,'(a,f13.9)') 'Interf_Param%Rpd_Delay',Interf_Param%Rpd_Delay
     read(iFile,*,err=999)     Interf_Param%Rpd_Samp_Jitter
     write(*,'(a,f13.9)') 'Interf_Param%Rpd_Samp_Jitter',&
                           Interf_Param%Rpd_Samp_Jitter
     read(iFile,*,err=999)     Interf_Param%Mes_Delay
     write(*,'(a,f13.9)') 'Interf_Param%Mes_Delay',Interf_Param%Mes_Delay
     read(iFile,*,err=999)     Interf_Param%Mes_Samp_Jitter
     write(*,'(a,f13.9)') 'Interf_Param%Mes_Samp_Jitter',&
                           Interf_Param%Mes_Samp_Jitter
     read(iFile,*,err=999)     Interf_Param%dT_Integ
     write(*,'(a,f13.9)') 'Interf_Param%dT_Integ',Interf_Param%dT_Integ
     read(iFile,*,err=999)     Interf_Param%dT_Integ_Jitter
     write(*,'(a,f13.9)') 'Interf_Param%dT_Integ_Jitter',&
                           Interf_Param%dT_Integ_Jitter
     read(iFile,*,err=999)     Interf_Param%dOpd_Integ
     write(*,'(a,f13.9)') 'Interf_Param%dOpd_Integ',Interf_Param%dOpd_Integ
     read(iFile,*,err=999) Interf_Param%Target_CS
     write(*,'(a,a)') 'Interf_Param%Target_CS',Interf_Param%Target_CS
     read(iFile,*,err=999) Interf_Param%Target_BB
     write(*,'(a,a)') 'Interf_Param%Target_BB',Interf_Param%Target_BB
     read(iFile,*,err=999) Interf_Param%Target_EW
     write(*,'(a,a)') 'Interf_Param%Target_EW',Interf_Param%Target_EW
     read(iFile,*,err=999)     Interf_Param%T_BG1_modul
     write(*,'(a,f13.9)') 'Interf_Param%T_BG1_modul',Interf_Param%T_BG1_modul
     read(iFile,*,err=999)     Interf_Param%T_BG2_modul
     write(*,'(a,f13.9)') 'Interf_Param%T_BG2_modul',Interf_Param%T_BG2_modul
     read(iFile,*,err=999)     Interf_Param%T_BG_direct
     write(*,'(a,f13.9)') 'Interf_Param%T_BG_direct',Interf_Param%T_BG_direct
     read(iFile,*,err=999)     Interf_Param%T_CS
     write(*,'(a,f13.9)') 'Interf_Param%T_CS',Interf_Param%T_CS
     read(iFile,*,err=999)     Interf_Param%T_BB
     write(*,'(a,f13.9)') 'Interf_Param%T_BB',Interf_Param%T_BB
     read(iFile,*,err=999)     Interf_Param%T_EW
     write(*,'(a,f13.9)') 'Interf_Param%T_EW',Interf_Param%T_EW
     read(iFile,*,err=999)     Interf_Param%TempRef
     write(*,'(a,f13.9)') 'Interf_Param%TempRef',Interf_Param%TempRef
     iPos = 10
     read(iFile,*,err=999)     Interf_Param%Nband
     write(*,'(a,i10)') 'Interf_Param%Nband',Interf_Param%Nband
     allocate( Interf_Param%Wn_First(Interf_Param%Nband) )
     allocate( Interf_Param%Wn_Last(Interf_Param%Nband) )
     allocate( Interf_Param%Wn_Usefull_First(Interf_Param%Nband) )
     allocate( Interf_Param%Wn_Usefull_Last(Interf_Param%Nband) )
     allocate( Interf_Param%WnLaser_EW(Interf_Param%Nband) )
     allocate( Interf_Param%Laser_Intensity(Interf_Param%Nband) )
     allocate( Interf_Param%Flat_Intensity(Interf_Param%Nband) )
     do SB = 1, Interf_Param%Nband
        read(iFile,*,err=999)     Interf_Param%Wn_First(SB)
        write(*,'(a,f10.0)') 'Interf_Param%Wn_First',Interf_Param%Wn_First(SB)
        read(iFile,*,err=999)     Interf_Param%Wn_Last(SB)
        write(*,'(a,f10.0)') 'Interf_Param%Wn_Last',Interf_Param%Wn_Last(SB)
        read(iFile,*,err=999)     Interf_Param%Wn_Usefull_First(SB)
        write(*,'(a,f10.0)') 'Interf_Param%Wn_Usefull_First'&
                             ,Interf_Param%Wn_Usefull_First(SB)
        read(iFile,*,err=999)     Interf_Param%Wn_Usefull_Last(SB)
        write(*,'(a,f10.0)') 'Interf_Param%Wn_Usefull_Last'&
                             ,Interf_Param%Wn_Usefull_Last(SB)
     end do
     do SB = 1, Interf_Param%Nband
        read(iFile,*,err=999)     Interf_Param%WnLaser_EW(SB)
        write(*,'(a,f10.0)') 'Interf_Param%WnLaser_EW',&
                              Interf_Param%WnLaser_EW(SB)
        read(iFile,*,err=999)     Interf_Param%Laser_Intensity(SB)
        write(*,'(a,f13.9)') 'Interf_Param%Laser_Intensity',&
                              Interf_Param%Laser_Intensity(SB)
        read(iFile,*,err=999)     Interf_Param%Flat_Intensity(SB)
        write(*,'(a,f13.9)') 'Interf_Param%Flat_Intensity',&
                              Interf_Param%Flat_Intensity(SB)
     end do
     Interf_Param%dWn = 1.d+00 &
                      / (2.d+00*Interf_Param%OpdMax*Interf_Param%Ratio_Opd)
     write(*,'(a,f10.4)') 'Interf_Param%dWn',Interf_Param%dWn
     close(unit=iFile)
!
!    Parameters control and forcing
     if( Interf_Param%Sampling_Mode == 'LZRPD' ) then
        Interf_Param%dT_Mes_Samp = Interf_Param%LambdaLaserRpd / 2.d+00 &
                                 / (2.d+00*Interf_Param%VCC)
        Interf_Param%dT_Integ    = Interf_Param%LambdaLaserRpd / 2.d+00 &
                                 / (2.d+00*Interf_Param%VCC)
        Interf_Param%dOpd_Integ  = 1.d+00
        write(*,*) 'Warning dT_Mes_Samp and dT_Integ forced '
        write(*,*) 'to be LambdaLaserRpd/2 ',Interf_Param%dT_Integ
     else if( Interf_Param%Sampling_Mode == 'CLOCK' ) then
        if( Interf_Param%dT_Integ > Interf_Param%dT_Mes_Samp ) then
           Interf_Param%dT_Integ = Interf_Param%dT_Mes_Samp
           write(*,*) 'Warning dT_Integ adjusted',Interf_Param%dT_Integ
        end if
     end if
!
     return
!
999  write(*,*) 'parameters reading error',iPos
     write(*,*) 'interferometer_def Fatal Error'
     call exit(1)
   end subroutine interferometer_def

!
!
end module interferometer_init_module
