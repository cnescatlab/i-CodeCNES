!!# mertz_computation_module.f90 --
!!#
!!#           Project: SPS_GENERIC
!!#           Authors: NOVELTIS/B.TOURNIER
!!#              Date: january 2011
!!#           Version: $Revision: 1.1 $
!!# Last modification: $Date: 2011-03-07 15:54:59 $
!!#
!!# Language:  F90
!!# Standards: Noveltis
!!#
!!# --
!!#
!! 
!> mertz_computation_module -- Module
!!
!! * Purpose
!!
!!     Module for Mertz system prism Motion computation.
!!
!! * Description
!!      
!!     This module defines laws for thickness defect in MERTZ configuration.
!!
!! * Sub-routines and functions
!!
!!     - mertz_define : Computation of prism motion law defect
!!     - mertz_init   : Initialisation of the type defining cube corner motion law in MERTZ configuration. 
!!     - mertz        : Computation of thickness defect in MERTZ configuration.  
!!
!! * References
!!

module mertz_computation_module
   use precision_type
   use error_type
   use constantes_type
   use ccm_type
   use math_module
   use mertz_type
   use fcomp_type
   use srf_param_type
   use psf_type
   use refrac_index_type
   use ccm_computation_module
   use chromatism_computation_module
!
   implicit none
!
!
   public :: mertz_define   &
            ,mertz_init     &
            ,mertz_calc

!
   contains
!
!-------------------------------------------------------------------------------

!!
!!
!> mertz_define -- Public
!!
!! * Purpose
!!
!!     Computation of prism motion law defect
!!     
!! * Description
!!
!!     The motion of the prism is driven by the motion law of the mobile cube corner by 
!!     definition. Moreover this subroutine simulates a defect of the proper prism motion 
!!     law. The error on the motion gives an error on the thickness of the material. The 
!!     model simulates defects on the prism motion law such as: offset, shear, parabolic 
!!     or oscillations. 
!!     In this subroutine, the followings steps are required:
!!         - first, required tables are allocated,
!!         - then, prism motion law is define as linear or circular.
!!         - afterwards, prism motion law is updated with offset, tilt, parabolic law and 
!!           micro_vibrations
!!         - at last, prism motion defectis computed. 
!!
!! * Inputs 
!!
!! * Inputs/outputs
!!
!!     - Prism_Motion : type_Ccm / type for declaration and allocation of cube corner motion 
!!
!! * Outputs
!!
!! * References
!!

   subroutine mertz_define( Prism_Motion )
!
! purpose: calculates prism motion law defect
! 
     implicit none
     type(type_Ccm)    , intent(inout)      :: Prism_Motion
     integer(kind=LONG)                     :: ErrCode
     type(type_Ccm)                         :: Prism_Motion_Temp
!
     Prism_Motion_Temp%NsCcm         = Prism_Motion%NsCcm
     Prism_Motion_Temp%NSpeedvibfreq = Prism_Motion%NSpeedvibfreq
     Prism_Motion_Temp%NFocalvibfreq = Prism_Motion%NFocalvibfreq
     call alloc_Ccm( Prism_Motion_Temp )
     ErrCode = 0
!
     write(*,*) 'mertz_motion_computation : ', &
                 Prism_Motion%Law(1:len_trim(Prism_Motion%Law))
     select case (Prism_Motion%Law(1:len_trim(Prism_Motion%Law))) 
        case ('LINEAR')
!
!          define prism motion law : linear
           call ccm_linear( Prism_Motion )
        case default     
           write(*,*) 'prism law error : ',Prism_Motion%Law
           ErrCode = 1
     end select

     Prism_Motion_Temp = Prism_Motion
!
!    update prism motion law : offset
     call ccm_offset( Prism_Motion )
!
!    update prism  motion law : tilt
     call ccm_tilt( Prism_Motion_Temp, Prism_Motion )
!
!    update prism motion law : parabolique
     call ccm_parabol( Prism_Motion_Temp, Prism_Motion )
!
!    update prism motion law : micro_vibrations
     call ccm_muvib( Prism_Motion )
!
     if (ErrCode > 0) then
        write(*,*) 'Prism_Motion define: fatal error'
        call Err_outputErrorMessage(0)
        call exit(1)
     end if
!    calculates prism motion defect 
     Prism_Motion%ApexX = Prism_Motion%ApexX-Prism_Motion_Temp%ApexX
     Prism_Motion%ApexY = Prism_Motion%ApexY-Prism_Motion_Temp%ApexY
     Prism_Motion%ApexZ = Prism_Motion%ApexZ-Prism_Motion_Temp%ApexZ

     call dalloc_Ccm( Prism_Motion_Temp )
     return
!
   end subroutine mertz_define
!
!-------------------------------------------------------------------------------

!!
!!
!> mertz_init -- Public
!!
!! * Purpose
!!
!!     Initialisation of the type defining cube corner motion law in MERTZ configuration.  
!!
!! * Description
!!
!!     The type for declaration and allocation of cube corner motion in MERTZ configuration 
!!     is initialised thanks the read of file with definition of cube corner motion law
!!     parameters
!!
!! * Inputs 
!!
!!     - File_Mertz : character / input file with definition of cube corner motion 
!!                    law parameters
!!
!! * Inputs/outputs
!!
!!     - Prism_Motion : type_Ccm / type for declaration and allocation of cube corner motion 
!!
!! * Outputs
!!
!! * References
!!

   subroutine mertz_init( File_Mertz,  &
                          Mertz,       &
                          Prism_Motion )
     implicit none
     character(len=*)  ,intent(in)                   :: File_Mertz
     type(type_Mertz)  ,intent(inout)                :: Mertz
     type(type_Ccm)    ,intent(inout)                :: Prism_Motion
     integer(kind=LONG)                              :: Nb
     integer(kind=LONG)                              :: iFile
     integer(kind=LONG)                              :: iPos
!
     iFile = 10
     iPos  = 1
     open(unit=iFile, file=File_Mertz, status='old', err=999 )
     write(*,'(a)') File_Mertz(1:len_trim(File_Mertz))
     Prism_Motion%filename = File_Mertz
     iPos  = 2
     read(iFile,'(a)',err=999) Prism_Motion%Law
     iPos  = 3
     read(iFile,*,err=999) Prism_Motion%VCC
     iPos  = 4
     read(iFile,*,err=999) Prism_Motion%Radius
     iPos  = 5
     read(iFile,*,err=999) Prism_Motion%d_Radius1
     iPos  = 6
     read(iFile,*,err=999) Prism_Motion%d_Radius2
     iPos  = 7
     read(iFile,*,err=999) 
     Prism_Motion%Omega = 4.d0*Pi*Prism_Motion%Radius/Prism_Motion%VCC
     iPos  = 8
     read(iFile,*,err=999) Prism_Motion%OpdMax
     if( Prism_Motion%Law(1:len_trim(Prism_Motion%Law)) == 'CIRCULAR' ) then
        Prism_Motion%T_Course = 4.d0 * Prism_Motion%Radius          &
                     / Prism_Motion%VCC * dasin(Prism_Motion%OpdMax &
                     / 4.d0 / Prism_Motion%Radius)
     else
        Prism_Motion%T_Course = Prism_Motion%OpdMax / Prism_Motion%VCC
     endif
     iPos  = 9
     read(iFile,*,err=999) Prism_Motion%NsCcm
     iPos  = 10
     read(iFile,*,err=999) Prism_Motion%OffsetCCMx
     iPos  = 11
     read(iFile,*,err=999) Prism_Motion%OffsetCCMy
     iPos  = 12
     read(iFile,*,err=999) Prism_Motion%OffsetCCMz
     iPos  = 13
     read(iFile,*,err=999) Prism_Motion%TiltCCMy
     iPos  = 14
     read(iFile,*,err=999) Prism_Motion%TiltCCMz
     iPos  = 15
     read(iFile,*,err=999) Prism_Motion%ParabolCCM
     iPos  = 16
     read(iFile,*,err=999) Prism_Motion%ParabolPhi   
     Prism_Motion%TiltCCMy   = datan2( Prism_Motion%TiltCCMy, &
                                       Prism_Motion%OpdMax/2.d+00 )
     Prism_Motion%TiltCCMz   = datan2( Prism_Motion%TiltCCMz, &
                                       Prism_Motion%OpdMax/2.d+00 )
     Prism_Motion%ParabolCCM = datan2( Prism_Motion%ParabolCCM, &
                                       Prism_Motion%OpdMax/2.d+00 )
     Prism_Motion%ParabolPhi = Prism_Motion%ParabolPhi / 180.d+00 * Pi
     iPos  = 17
     read(iFile,*,err=999) Prism_Motion%NSpeedvibfreq
     read(iFile,*,err=999) Prism_Motion%NFocalvibfreq
     iPos  = 18
     call alloc_Ccm( Prism_Motion )
     do Nb = 1, Prism_Motion%NSpeedvibfreq
        iPos  = 19+Nb
        read(iFile,*,err=999) Prism_Motion%SpeedvibFreq(Nb)
        iPos  = 20+Nb
        read(iFile,*,err=999) Prism_Motion%SpeedvibAmp(Nb)
        iPos  = 21+Nb
        read(iFile,*,err=999) Prism_Motion%SpeedvibPhase(Nb)
        iPos  = 22+Nb
        read(iFile,*,err=999) Prism_Motion%SpeedvibPhi(Nb)
        if( Prism_Motion%Law(1:len_trim(Prism_Motion%Law)) == 'LINGEN' ) then
           Prism_Motion%SpeedvibAmp(Nb) = Prism_Motion%SpeedvibAmp(Nb)&
                                        * Prism_Motion%GenDistCC*2.d0 &
                                  *Pi*Prism_Motion%SpeedvibFreq(Nb)   &
                                  /Prism_Motion%VCC
           Prism_Motion%SpeedvibPhase(Nb) = ( Prism_Motion%SpeedvibPhase(Nb) &
                                            + 90.d0 ) / 180.d+00 * Pi
           Prism_Motion%SpeedvibPhi(Nb)   = Prism_Motion%SpeedvibPhi(Nb)&
                                          / 180.d+00 * Pi
        else
           Prism_Motion%SpeedvibPhase(Nb) = Prism_Motion%SpeedvibPhase(Nb) &
                                          / 180.d+00 * Pi
           Prism_Motion%SpeedvibPhi(Nb)   = Prism_Motion%SpeedvibPhi(Nb) &
                                          / 180.d+00 * Pi
        end if
     end do
     close(unit=iFile)
!
     Mertz%NsCcm = Prism_Motion%NsCcm
     call alloc_Mertz( Mertz )
!
     return
999  write(*,*) 'parameters reading error',iPos
     write(*,*) 'mertz_init Fatal Error'
     call exit(1)
   end subroutine mertz_init
!
!
!-------------------------------------------------------------------------------
!!
!!
!> mertz_calc -- Public
!!
!! * Purpose
!!
!!     Computation of thickness defect in MERTZ configuration.  
!!
!! * Description
!!
!!     This subroutine allows the computation of thickness defect in MERTZ configuration.  
!!     Following steps are required:
!!      - first, the refractive index is computed for the given material at the given wave
!!        number;
!!      - then, the refractive index is integrated on the field;
!!      - at last, thickness defect is computed.
!!
!! * Inputs 
!!
!!     - Prism_Motion : type_Ccm / type for declaration and allocation of cube corner motion 
!!     - Fcomp     : type_Fcomp / type for declaration and allocation of field compensation
!!     - Srf_Param : type_Srf_Param / type for srf parameters declaration and allocation
!!     - Psf       : type_Psf / type for declaration and allocation of the psf 
!!
!! * Inputs/outputs
!!
!!     - RefracIndex : type_RefracIndex / type for declaration and allocation of refractive
!!                     index of materials
!!
!! * Outputs
!!
!!     - Mertz : Mertz / type for declaration and allocation of cube corner motion 
!!
!! * References
!!


   subroutine mertz_calc( Prism_Motion, Fcomp, RefracIndex, &
                          Psf, Wn, Mertz )

!  inputs
   type(type_Ccm)         , intent(in)    :: Prism_Motion
   type(type_Fcomp)       , intent(in)    :: Fcomp
   type(type_Psf)         , intent(in)    :: Psf
   real(kind=DOUBLE)                      :: Wn 
!  input/output 
   type(type_RefracIndex) , intent(inout)  :: RefracIndex
   type(type_Mertz)       , intent(inout)  :: Mertz
!
!  local
   integer(kind=LONG)          :: NC
   integer(kind=LONG)          :: NL
   !
   ! Refractive index of material at wave number
   RefracIndex%N = real(0,kind=DOUBLE)
   call CalcIndice( Fcomp%Material, RefracIndex, Wn, RefracIndex%N )
   !
   ! Field integration
   RefracIndex%MeanRefracIndex = real(0,kind=DOUBLE)
   do NL = 1, Psf%NbLin
      do NC = 1, Psf%NbCol
         RefracIndex%Nper(NC,NL) = RefracIndex%N*( real(1,kind=DOUBLE) &
                                 + RefracIndex%FracN(NC,NL) )
         RefracIndex%MeanRefracIndex = RefracIndex%MeanRefracIndex &
                                     + ( RefracIndex%Nper(NC,NL)   &
                                       * Psf%Wgt(NC,NL,1) )
      end do
   end do
   if( Mertz%NsCcm /= Prism_Motion%NsCcm ) then
      write(*,*) 'Mertz NsCcm not compatible with Prism Motion',&
                 Mertz%NsCcm, Prism_Motion%NsCcm
      write(*,*) 'mertz_calc Fatal Error'
      call exit(1)
   end if
   Mertz%Dthickness(1:Mertz%NsCcm) = Prism_Motion%ApexX(1:Mertz%NsCcm)

 end subroutine mertz_calc
!
!
end module mertz_computation_module
