!!# ccm_computation_module.f90 --
!!#
!!#           Project: SPS_GENERIC
!!#           Authors: NOVELTIS/B.TOURNIER
!!#              Date: october 2009
!!#
!!#           Version: $Revision: 1.4 $
!!# Last modification: $Date: 2011-03-07 15:54:59 $
!!#
!!# Language:  F90
!!# Standards: Noveltis
!!#
!!# --
!!#
!! 
!> ccm_computation_module -- Module
!!
!! * Purpose
!!
!!      Module for Corner Cube Motion computation.
!!
!! * Description
!!      
!! This module defines the cube corner motion law implemented in the functional 
!! model. The motion law is defined in metrics units in a reference frame (XYZ) 
!! where  
!! - (XY) plan defines the theoretical optical bench
!! - Z is perpendicular to the optical bench
!! - (YZ) plan is parallel to the theoretical focal plan  
!!
!! * Sub-routines and functions
!!
!! - ccm_define   : definition of the cube corner motion law
!! - ccm_linear   : linear cube corner motion law
!! - ccm_circular : circular cube corner motion law
!! - ccm_offset   : offset in cube corner motion law
!! - ccm_tilt     : tilt incube corner motion law
!! - ccm_parabol  : parabolic cube corner motion law
!! - ccm_muvib    : micro-vibration cube corner motion law
!! - ccm_focalvib : focal plane vibration 
!! - ccm_init     : initialisation in function of the interferometer configuration, 
!!                  Michelson/Genzel 
!!
!! * References
!!
!!    NOV-3820-NT-ATBD : 4.4.1.6
!!

module ccm_computation_module
   use precision_type
   use error_type
   use constantes_type
   use ccm_type
   use math_module
!
   implicit none
!
!
   public :: ccm_define   &
            ,ccm_linear   &
            ,ccm_offset   &
            ,ccm_tilt     &
            ,ccm_parabol  &
            ,ccm_muvib    &
            ,ccm_focalvib &
            ,ccm_init

!
   contains
!
!

!> ccm_define -- Public
!!
!! * Purpose
!!
!!     Define cube corner motion law
!!
!! * Description
!!
!!     Update cube corner motion law in function of the initialized type
!!
!! * Inputs
!!
!! * Inputs/outputs
!!
!!     - Ccm : type_Ccm / type for declaration and allocation of cube corner motion
!!
!! * Outputs
!!
!! * References
!!

   subroutine ccm_define( Ccm )
!
!
     implicit none
     type(type_Ccm)    , intent(inout)      :: Ccm
     integer(kind=LONG)                     :: ErrCode
     type(type_Ccm)                         :: Ccm_Temp
!
     Ccm_Temp%NsCcm         = Ccm%NsCcm
     Ccm_Temp%NSpeedvibfreq = Ccm%NSpeedvibfreq
     Ccm_Temp%NFocalvibfreq = Ccm%NFocalvibfreq
     Ccm_Temp%VCC_Nb_Seg    = Ccm%VCC_Nb_Seg
     call alloc_Ccm( Ccm_Temp )
     ErrCode = 0
!
     write(*,*) 'ccm_computation : ',Ccm%Law(1:len_trim(Ccm%Law))
     select case ( Ccm%Law(1:len_trim(Ccm%Law)) )
     case ( 'LINEAR' )
!
!       define cube corner motion law : linear
        call ccm_linear( Ccm )
     case ( 'LINGEN' )
!
!       define cube corner motion law : linear
        call ccm_linear( Ccm )
     case default
        write(*,*) 'ccm law error : ',Ccm%Law
        ErrCode = 1
     end select
     Ccm_Temp = Ccm
!
!    update cube corner motion law : offset
     call ccm_offset( Ccm )
!
!    update cube corner motion law : tilt
     call ccm_tilt( Ccm_Temp, Ccm )
!
!    update cube corner motion law : parabolic
     call ccm_parabol( Ccm_Temp, Ccm )
!
!    update cube corner motion law : micro_vibrations
     call ccm_muvib( Ccm )
!
!    update cube corner motion law : focal plane vibrations
     call ccm_focalvib( Ccm_Temp, Ccm )
!
     if (ErrCode > 0) then
        write(*,*) 'Ccm define: fatal error'
        call Err_outputErrorMessage(0)
        call exit(1)
     end if
     call dalloc_Ccm( Ccm_Temp )
     return
!
   end subroutine ccm_define
!
!

!> ccm_linear -- Public
!!
!! * Purpose
!!
!!     Define cube corner motion law : linear
!!
!! * Description
!!
!!     The linear cube corner motion law is define along the X axis 
!!     direction     
!!
!! * Inputs
!!
!! * Inputs/outputs
!!
!!     - Ccm : type_Ccm / type for declaration and allocation of cube 
!!             corner motion 
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.4.1.6.1
!!

   subroutine ccm_linear( Ccm )
     implicit none
     type(type_Ccm)    , intent(inout)      :: Ccm
     integer(kind=LONG)                     :: NsCcmp
     integer(kind=LONG)                     :: Ns
     real(kind=DOUBLE)                      :: dApex
!
     NsCcmp = int(Ccm%NsCcm/2)
     dApex = Ccm%OpdMax / dble( Ccm%NsCcm - 1 )
!
     Ccm%ApexX(1:Ccm%NsCcm) = (/ (dble(Ns-NsCcmp-1),Ns=1,Ccm%NsCcm) /) * dApex
!
     Ccm%ApexY(1:Ccm%NsCcm) = 0.d+00
     Ccm%ApexZ(1:Ccm%NsCcm) = 0.d+00
!
     return
   end subroutine ccm_linear
!

!> ccm_offset -- Public
!!
!! * Purpose
!!
!!     Define cube corner motion law : offset
!!
!! * Description
!! 
!!     The offset describes a misalignment of the fixed corner cube with 
!!     respect to the optical axis, independently in any of the three 
!!     dimensions. 
!!
!! * Inputs
!!
!! * Inputs/outputs
!!
!!     - Ccm : type_Ccm / type for declaration and allocation of cube corner motion 
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.4.1.6.2
!!

   subroutine ccm_offset( Ccm )
     implicit none
     type(type_Ccm)    , intent(inout)               :: Ccm
!
     Ccm%ApexX(1:Ccm%NsCcm) = Ccm%ApexX(1:Ccm%NsCcm) + Ccm%OffsetCCMx
     Ccm%ApexY(1:Ccm%NsCcm) = Ccm%ApexY(1:Ccm%NsCcm) + Ccm%OffsetCCMy
     Ccm%ApexZ(1:Ccm%NsCcm) = Ccm%ApexZ(1:Ccm%NsCcm) + Ccm%OffsetCCMz
!
     return
   end subroutine ccm_offset
!
!

!> ccm_tilt -- Public
!!
!! * Purpose
!!
!!     Define cube corner motion law : tilt
!!
!! * Description
!!
!!     The tilt/shear simulates a linear defect of the mobile cube corner 
!!     vector with respect to the optical axis. 
!!
!! * Inputs
!!
!!     - Ccm_Temp : type_Ccm / type for cube corner motion declaration and allocation 
!!
!! * Inputs/outputs
!!
!!     - Ccm : type_Ccm / type for declaration and allocation of cube corner motion 
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.4.1.6.3
!!

   subroutine ccm_tilt( Ccm_Temp, Ccm )
     implicit none
     type(type_Ccm)    , intent(in)         :: Ccm_Temp
     type(type_Ccm)    , intent(inout)      :: Ccm
!
     Ccm%ApexY(1:Ccm%NsCcm) = Ccm%ApexY(1:Ccm%NsCcm)                        &
                            + ( Ccm_Temp%ApexX(1:Ccm%NsCcm)                 &
                              * dtan( Ccm%TiltCCMy ) )
     Ccm%ApexZ(1:Ccm%NsCcm) = Ccm%ApexZ(1:Ccm%NsCcm)                        &
                            + ( Ccm_Temp%ApexX(1:Ccm%NsCcm)                 &
                              * dtan( Ccm%TiltCCMz ))
!
     return
   end subroutine ccm_tilt
!
!

!> ccm_parabol -- Public
!!
!! * Purpose
!!
!!     Define cube corner motion law : parabolic
!!
!! * Description
!!
!!     A parabolic motion defect can be caused by gravitational effects. 
!!     It is defined by two parameters: 
!!      - the amplitude (in meter) of the defect at the maximum cube corner 
!!        course Xmax (Opdmax/2) 
!!      - and the rotation angle in the (YZ) plane of the parabolic plane.
!!
!! * Inputs
!!
!!     - Ccm_Temp : type_Ccm / type for cube corner motion declaration and allocation
!!
!! * Inputs/outputs
!!
!!     - Ccm : type_Ccm / type for declaration and allocation of cube corner motion
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.4.1.2.5
!!

    subroutine ccm_parabol( Ccm_Temp, Ccm )
     implicit none
     type(type_Ccm)    , intent(in)         :: Ccm_Temp
     type(type_Ccm)    , intent(inout)      :: Ccm
!
     if( Ccm_Temp%ApexX(Ccm_Temp%NsCcm) == 0.d+00 .or. &
         Ccm_Temp%NsCcm /= Ccm%NsCcm                   ) then
        write(*,*) ' ccm_parabol error',Ccm_Temp%ApexX(Ccm_Temp%NsCcm)
        call exit(1)
     else
        Ccm%ApexY(1:Ccm%NsCcm) = Ccm%ApexY(1:Ccm%NsCcm)                   &
                               + Ccm%ParabolCCM * dcos( Ccm%ParabolPhi )  &
                               * ( Ccm_Temp%ApexX(1:Ccm%NsCcm)            &
                                 / Ccm_Temp%ApexX(Ccm%NsCcm) )**2
        Ccm%ApexZ(1:Ccm%NsCcm) = Ccm%ApexZ(1:Ccm%NsCcm)                   &
                               + Ccm%ParabolCCM * dsin( Ccm%ParabolPhi )  &
                               * ( Ccm_Temp%ApexX(1:Ccm%NsCcm)            &
                                 / Ccm_Temp%ApexX(Ccm%NsCcm) )**2
     end if
!
     return
   end subroutine ccm_parabol
!!
!!
!> ccm_muvib -- Public
!!
!! * Purpose
!!
!!     Define cube corner motion law : micro_vibrations 
!!
!! * Description
!!
!!     The micro vibration is simulated as a cosine function 
!!     using four parameters: 
!!       - the amplitude of the cosine (m),
!!       - the frequency of the vibration (Hz), 
!!       - the phase (rad) of the vibration at Zero Path Difference 
!!       - and the rotation angle (rad) in the (YZ) plane of the 
!!         micro-vibration plane.
!!
!! * Inputs
!!
!! * Inputs/outputs
!!
!!     - Ccm : type_Ccm / type for declaration and allocation of cube corner motion
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.4.1.6.5
!!

   subroutine ccm_muvib( Ccm )
     implicit none
     type(type_Ccm)    , intent(inout)             :: Ccm
     real(kind=DOUBLE) , dimension(:), allocatable :: Time
     integer(kind=LONG)                            :: Nf
     integer(kind=LONG)                            :: Ns
     integer(kind=LONG)                            :: NsCcmp
     real(kind=DOUBLE)                             :: dT
!
     if( Ccm%NSpeedvibfreq /= 0 ) then
       allocate( Time(Ccm%NsCcm) )
       NsCcmp = int(Ccm%NsCcm/2)
       dT = Ccm%T_Course / dble( Ccm%NsCcm - 1 )
       Time(1:Ccm%NsCcm) = (/ (dble(Ns-NsCcmp-1),Ns=1,Ccm%NsCcm) /) * dT
       do Nf = 1, Ccm%NSpeedvibfreq
          Ccm%ApexY(1:Ccm%NsCcm) = Ccm%ApexY(1:Ccm%NsCcm)                 &
                        + ( Ccm%SpeedvibAmp(Nf)                           &
                          * dcos( twoPi * Ccm%SpeedvibFreq(Nf)            &
                                        * Time(1:Ccm%NsCcm)               &
                                + Ccm%SpeedvibPhase(Nf) )                 &
                          * dcos( Ccm%SpeedvibPhi(Nf) ) )
          Ccm%ApexZ(1:Ccm%NsCcm) = Ccm%ApexZ(1:Ccm%NsCcm)                 &
                        + ( Ccm%SpeedvibAmp(Nf)                           &
                          * dcos( twoPi * Ccm%SpeedvibFreq(Nf)            &
                                        * Time(1:Ccm%NsCcm)               &
                                + Ccm%SpeedvibPhase(Nf) )                 &
                          * dsin( Ccm%SpeedvibPhi(Nf) ) )
       end do
       deallocate( Time )
     end if
     return
   end subroutine ccm_muvib
!!
!!
!> ccm_focalvib -- Public
!!
!! * Purpose
!!
!!     Define cube corner motion law : focal plane vibrations
!!
!! * Description
!!
!!     The focal plane vibration is simulated as a cosine weight by the 
!!     course along the ideal moving cube corner direction. 
!!     It is defined by four parameters: 
!!       - the amplitude of the vibration FVAmpl (m), 
!!       - the frequency of the vibration FVfreq (Hz), 
!!       - the phase (rad) at Zero Path Difference FVphase 
!!       - and the rotation angle (rad) in the (YZ) plane of the 
!!         micro-vibration plane
!!
!! * Inputs
!!
!!     - Ccm_Temp : type_Ccm / type for declaration and allocation of cube corner motion
!!
!! * Inputs/outputs
!!
!!     - Ccm : type_Ccm / type for declaration and allocation of cube corner motion
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.4.1.2.7
!!

   subroutine ccm_focalvib( Ccm_Temp, Ccm )
     implicit none
     type(type_Ccm)    , intent(in)                :: Ccm_Temp
     type(type_Ccm)    , intent(inout)             :: Ccm
     real(kind=DOUBLE) , dimension(:), allocatable :: Time
     integer(kind=LONG)                            :: Nf
     integer(kind=LONG)                            :: Ns
     integer(kind=LONG)                            :: NsCcmp
     real(kind=DOUBLE)                             :: dT
!
     if( Ccm%NFocalvibfreq /= 0 ) then
       allocate( Time(Ccm%NsCcm) )
       NsCcmp = int(Ccm%NsCcm/2)
       dT = Ccm%T_Course / dble( Ccm%NsCcm - 1 )
       Time(1:Ccm%NsCcm) = (/ (dble(Ns-NsCcmp-1),Ns=1,Ccm%NsCcm) /) * dT
       do Nf = 1, Ccm%NFocalvibfreq
          Ccm%ApexY(1:Ccm%NsCcm) = Ccm%ApexY(1:Ccm%NsCcm)                 &
                        + dabs(Ccm_Temp%ApexX(1:Ccm%NsCcm))               &
                          * ( Ccm%FocalvibAmp(Nf)                         &
                            * dcos( twoPi * Ccm%FocalvibFreq(Nf)          &
                                          * Time(1:Ccm%NsCcm)             &
                                  + Ccm%FocalvibPhase(Nf) )               &
                            * dcos( Ccm%FocalvibPhi(Nf) ) )
          Ccm%ApexZ(1:Ccm%NsCcm) = Ccm%ApexZ(1:Ccm%NsCcm)                 &
                        + dabs(Ccm_Temp%ApexX(1:Ccm%NsCcm))               &
                          * ( Ccm%FocalvibAmp(Nf)                         &
                            * dcos( twoPi * Ccm%FocalvibFreq(Nf)          &
                                          * Time(1:Ccm%NsCcm)             &
                                  + Ccm%FocalvibPhase(Nf) )               &
                            * dsin( Ccm%FocalvibPhi(Nf) ) )
       end do
       deallocate( Time )
     end if
     return
   end subroutine ccm_focalvib
!!
!!

!> ccm_init -- Public
!!
!! * Purpose
!!
!!     Initialisation of the type defining cube corner motion law (LINEAR/
!!     CIRCULAR/LINGEN) in function of the interferometer configuration : 
!!     Michelson, Genzel, double pendular  
!!
!! * Description
!!
!!     In the case of classical Michelson (IASI) configuration there is one 
!!     fixed CC and one mobile CC. 
!!     In the Genzel type configuration interferometers there is again one 
!!     single mechanism for a CC displacement (OPD generation). The nominal 
!!     displacement is again linear, and parallel to the optical interferometer 
!!     axis. The difference is that both CCs, mounted in a face-to-face fashion, 
!!     are moving.
!!
!! * Inputs 
!!
!!     - File_Ccm :  character / input file with definition of cube corner motion 
!!                   law parameters
!!
!! * Inputs/outputs
!!
!!     - Ccm : type_Ccm / type for declaration and allocation of cube corner motion 
!!
!! * Outputs
!!
!! * References
!!

   subroutine ccm_init( File_Ccm, &
                        Ccm       )
     implicit none
     character(len=*)  , intent(in)                  :: File_Ccm
     type(type_Ccm)    , intent(inout)               :: Ccm
     integer(kind=LONG)                              :: Nb
     integer(kind=LONG)                              :: iFile
     integer(kind=LONG)                              :: iPos
!
     iFile = 10
     iPos  = 1
     open(unit=iFile, file=File_Ccm, status='old', err=999 )
     Ccm%filename = File_Ccm
     iPos  = 2
     read(iFile,'(a)',err=999) Ccm%Law
     read(iFile,*,err=999) Ccm%VCC
     read(iFile,*,err=999) Ccm%Radius
     read(iFile,*,err=999) Ccm%d_Radius1
     read(iFile,*,err=999) Ccm%d_Radius2
     read(iFile,*,err=999) Ccm%GenDistCC
     Ccm%Omega = 4.d0*Pi*Ccm%Radius/Ccm%VCC
     read(iFile,*,err=999) Ccm%OpdMax
     select case ( Ccm%Law(1:len_trim(Ccm%Law)) )
     case default
        Ccm%T_Course = Ccm%OpdMax/Ccm%VCC
     end select
     read(iFile,*,err=999) Ccm%NsCcm
     read(iFile,*,err=999) Ccm%OffsetCCMx
     read(iFile,*,err=999) Ccm%OffsetCCMy
     read(iFile,*,err=999) Ccm%OffsetCCMz
     read(iFile,*,err=999) Ccm%TiltCCMy
     read(iFile,*,err=999) Ccm%TiltCCMz
     read(iFile,*,err=999) Ccm%ParabolCCM
     read(iFile,*,err=999) Ccm%ParabolPhi
     select case ( Ccm%Law(1:len_trim(Ccm%Law)) )
     case ( 'LINGEN' )
        Ccm%OffsetCCMy = Ccm%OffsetCCMy + Ccm%GenDistCC* &
                       dsin(datan2(Ccm%TiltCCMy,Ccm%OpdMax/4.d+00))
        Ccm%OffsetCCMz = Ccm%OffsetCCMz + Ccm%GenDistCC* &
                       dsin(datan2(Ccm%TiltCCMz,Ccm%OpdMax/4.d+00))
        Ccm%TiltCCMy = 4.d0*Ccm%ParabolCCM*Ccm%GenDistCC* &
                       dcos(Ccm%ParabolPhi/180.d+00*Pi)/Ccm%OpdMax
        Ccm%TiltCCMz = 4.d0*Ccm%ParabolCCM*Ccm%GenDistCC* &
                       dsin(Ccm%ParabolPhi/180.d+00*Pi)/Ccm%OpdMax
        Ccm%TiltCCMy   = datan2( Ccm%TiltCCMy, Ccm%OpdMax/4.d+00 )
        Ccm%TiltCCMz   = datan2( Ccm%TiltCCMz, Ccm%OpdMax/4.d+00 )
        Ccm%ParabolCCM = 0.d0
        Ccm%ParabolPhi = 0.d0
     case default
        Ccm%TiltCCMy   = datan2( Ccm%TiltCCMy, Ccm%OpdMax/2.d+00 )
        Ccm%TiltCCMz   = datan2( Ccm%TiltCCMz, Ccm%OpdMax/2.d+00 )
        Ccm%ParabolCCM = datan2( Ccm%ParabolCCM, Ccm%OpdMax/2.d+00 )
        Ccm%ParabolPhi = Ccm%ParabolPhi / 180.d+00 * Pi
     end select
     read(iFile,*,err=999) Ccm%NSpeedvibfreq
     read(iFile,*,err=999) Ccm%NFocalvibfreq
     read(iFile,*,err=999) Ccm%VCC_Nb_Seg
     call alloc_Ccm( Ccm )
!
!    Speed vibrations
     if( Ccm%NSpeedvibfreq /= 0 ) then
        do Nb = 1, Ccm%NSpeedvibfreq
           read(iFile,*,err=999) Ccm%SpeedvibFreq(Nb)
           read(iFile,*,err=999) Ccm%SpeedvibAmp(Nb)
           read(iFile,*,err=999) Ccm%SpeedvibPhase(Nb)
           read(iFile,*,err=999) Ccm%SpeedvibPhi(Nb)
           select case ( Ccm%Law(1:len_trim(Ccm%Law)) )
           case ( 'LINGEN' )
              Ccm%SpeedvibAmp(Nb) = Ccm%SpeedvibAmp(Nb)*Ccm%GenDistCC*2.d0&
                                  *Pi*Ccm%SpeedvibFreq(Nb)/Ccm%VCC
              Ccm%SpeedvibPhase(Nb) = (Ccm%SpeedvibPhase(Nb) + 90.d0) &
                                    / 180.d+00 * Pi
              Ccm%SpeedvibPhi(Nb)   = Ccm%SpeedvibPhi(Nb)   / 180.d+00 * Pi
           case default
              Ccm%SpeedvibPhase(Nb) = Ccm%SpeedvibPhase(Nb) / 180.d+00 * Pi
              Ccm%SpeedvibPhi(Nb)   = Ccm%SpeedvibPhi(Nb)   / 180.d+00 * Pi
           end select
        end do
     else
        read(iFile,*,err=999)
        read(iFile,*,err=999)
        read(iFile,*,err=999)
        read(iFile,*,err=999)
     end if
!
!    focal vibration
     if( Ccm%NFocalvibfreq /= 0 ) then
        do Nb = 1, Ccm%NFocalvibfreq
           read(iFile,*,err=999) Ccm%FocalvibFreq(Nb)
           read(iFile,*,err=999) Ccm%FocalvibAmp(Nb)
           read(iFile,*,err=999) Ccm%FocalvibPhase(Nb)
           read(iFile,*,err=999) Ccm%FocalvibPhi(Nb)
           Ccm%FocalvibPhase(Nb) = Ccm%FocalvibPhase(Nb) / 180.d+00 * Pi
           Ccm%FocalvibPhi(Nb)   = Ccm%FocalvibPhi(Nb)   / 180.d+00 * Pi
        end do
     else
        read(iFile,*,err=999)
        read(iFile,*,err=999)
        read(iFile,*,err=999)
        read(iFile,*,err=999)
     end if
!
!    corner cube speed variation
     if( Ccm%VCC_Nb_Seg /= 0 ) then
        write(*,'(a,i10)') 'Ccm%VCC_Nb_Seg ',Ccm%VCC_Nb_Seg
        if( Ccm%VCC_Nb_Seg /= 0 ) then
           do Nb = 1, Ccm%VCC_Nb_Seg
              read(iFile,*,err=999)    Ccm%VCC_Course(Nb)
              read(iFile,*,err=999)    Ccm%VCC_A0(Nb)
              read(iFile,*,err=999)    Ccm%VCC_A1(Nb)
              write(*,'(a,2f13.9,f10.4)') 'Ccm%VCC_A0 A1 Course',&
                         Ccm%VCC_A0(Nb), Ccm%VCC_A1(Nb),&
                         Ccm%VCC_Course(Nb)
           end do
        else
           read(iFile,*,err=999)
           read(iFile,*,err=999)
           read(iFile,*,err=999)
        end if
     end if
     close(unit=iFile)
!
     return
999  write(*,*) 'parameters reading error',iPos
     write(*,*) 'ccm_init Fatal Error'
     call exit(1)
   end subroutine ccm_init
!
!
end module ccm_computation_module
