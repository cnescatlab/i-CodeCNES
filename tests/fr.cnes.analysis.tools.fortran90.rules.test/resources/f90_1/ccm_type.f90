!!* ccm_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: october 2009
!!*           Version: $Revision: 1.6 $
!!* Last modification:  $Date: 2011-03-07 15:54:59 $
!!
!> ccm_type -- Module
!!
!! * Purpose
!!
!!   Module for type cube corner motion declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the cube corner motion type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Ccm   : type for declaration and allocation of cube corner motion
!!     * alloc_Ccm  : type_Ccm allocation
!!     * dalloc_Ccm : type_Ccm deallocation
!!     * readccm    : type_Ccm reading routine
!!     * writeccm   : type_Ccm writing routine
!!
!! * References
!!

module ccm_type
   use precision_type
   use error_type
   use modinouttools
!
   implicit none
!
!
   public :: type_Ccm   &
            ,alloc_Ccm  &
            ,dalloc_Ccm &
            ,readccm    &
            ,writeccm   
!
   type :: type_Ccm
     character(len=500)                               :: filename !< cube corner characterisation file name  
     character(len=8)                                 :: Law !< type of cube corner motion law (LINEAR/CIRCULAR/LINGEN)
     real(kind=DOUBLE)                                :: VCC !< mobile cube corner velocity (m.s-1) (CIRCULAR+LINGEN : specify twice the actual speed)
     real(kind=DOUBLE)                                :: Omega !<  cube corner radial frequency 
     real(kind=DOUBLE)                                :: T_Course !< time for the mobile cube corner to scan the whole optical path difference (s)
     real(kind=DOUBLE)                                :: OpdMax !< maximum optical path difference (m) 
     real(kind=DOUBLE)                                :: Radius !< CIRCULAR only: Radius (nominal pendulum radius in m)
     real(kind=DOUBLE)                                :: d_Radius1 !< CIRCULAR only: d_Radius1 (left arm radius defect in m)
     real(kind=DOUBLE)                                :: d_Radius2 !< CIRCULAR only: d_Radius2 (right arm radius defect in m)
     real(kind=DOUBLE)                                :: GenDistCC !< LINGEN only  : C1-C2 distance (=thickness of separation plate) (m)
     integer(kind=LONG)                               :: NsCcm !< number of OPD discretisation samples 
     real(kind=DOUBLE)                                :: OffsetCCMx !< cube corner offset in X direction (m) 
     real(kind=DOUBLE)                                :: OffsetCCMy !< cube corner offset in Y direction (m)
     real(kind=DOUBLE)                                :: OffsetCCMz !< cube corner offset in Z direction (m)
     real(kind=DOUBLE)                                :: TiltCCMy !< mobile cube corner tilt in Y direction at OPDMax (m) 
     real(kind=DOUBLE)                                :: TiltCCMz !< mobile cube corner tilt in Z direction at OPDMax (m) 
     real(kind=DOUBLE)                                :: ParabolCCM !< amplitude of the parabolic defect at OPDMax (m)
     real(kind=DOUBLE)                                :: ParabolPhi !< angle in the YZ plan of the parabolic defect (degree)
     integer(kind=LONG)                               :: NSpeedvibfreq !< Number of micro vibration frequencies
     integer(kind=LONG)                               :: NFocalvibfreq !< Number of micro vibration frequencies
     integer(kind=LONG)                               :: VCC_Nb_Seg !< Course segmentation number
     real(kind=DOUBLE) ,dimension(:),allocatable      :: SpeedvibFreq !< micro vibration frequency (Hz) - SpeedvibFreq(1:NSpeedvibfreq)
     real(kind=DOUBLE) ,dimension(:),allocatable      :: SpeedvibAmp !< micro vibration amplitude (m) - SpeedvibAmp(1:NSpeedvibfreq)
     real(kind=DOUBLE) ,dimension(:),allocatable      :: SpeedvibPhase !< micro vibration phase at ZPD (degree) - SpeedvibPhase(1:NSpeedvibfreq)
     real(kind=DOUBLE) ,dimension(:),allocatable      :: SpeedvibPhi !< micro vibration angle in the YZ plan (degree) - SpeedvibPhi(1:NSpeedvibfreq)
     real(kind=DOUBLE) ,dimension(:),allocatable      :: FocalvibFreq !< focal plan vibration frequency (Hz) - FocalvibFreq(1:NFocalvibfreq)
     real(kind=DOUBLE) ,dimension(:),allocatable      :: FocalvibAmp !< focal plan vibration amplitude (m) - FocalvibAmp(1:NFocalvibfreq)
     real(kind=DOUBLE) ,dimension(:),allocatable      :: FocalvibPhase !< focal plan vibration phase at ZPD (degree) - FocalvibPhase(1:NFocalvibfreq)
     real(kind=DOUBLE) ,dimension(:),allocatable      :: FocalvibPhi !< focal plan vibration angle in the YZ plan (degree) - FocalvibPhi(1:NFocalvibfreq)
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: VCC_Course !< Course fraction covered  - Course(1:Nb_Seg)
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: VCC_A0 !< VCC linear offset (m/s) - A0(1:Nb_Seg)
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: VCC_A1 !< VCC linear slope (m/s) - A1(1:Nb_Seg)
     real(kind=DOUBLE) ,dimension(:),allocatable      :: ApexX !< apex vector coordinate along X axis - ApexX(1:NsCcm)
     real(kind=DOUBLE) ,dimension(:),allocatable      :: ApexY !< apex vector coordinate along Y axis - ApexY(1:NsCcm)
     real(kind=DOUBLE) ,dimension(:),allocatable      :: ApexZ !< apex vector coordinate along Z axis - ApexZ(1:NsCcm)
   end type type_Ccm
!
   contains
!
!
   subroutine alloc_Ccm( Ccm )
     implicit none
     type(type_Ccm), intent(inout)      :: Ccm
     integer(kind=LONG)                 :: ErrCode
     if( Ccm%NSpeedvibfreq /= 0 ) then
       allocate( Ccm%SpeedvibFreq(Ccm%NSpeedvibfreq),      stat=ErrCode )
       allocate( Ccm%SpeedvibAmp(Ccm%NSpeedvibfreq),       stat=ErrCode )
       allocate( Ccm%SpeedvibPhase(Ccm%NSpeedvibfreq),     stat=ErrCode )
       allocate( Ccm%SpeedvibPhi(Ccm%NSpeedvibfreq),       stat=ErrCode )
     end if
     if( Ccm%NFocalvibfreq /= 0 ) then
       allocate( Ccm%FocalvibFreq(Ccm%NFocalvibfreq),      stat=ErrCode )
       allocate( Ccm%FocalvibAmp(Ccm%NFocalvibfreq),       stat=ErrCode )
       allocate( Ccm%FocalvibPhase(Ccm%NFocalvibfreq),     stat=ErrCode )
       allocate( Ccm%FocalvibPhi(Ccm%NFocalvibfreq),       stat=ErrCode )
     end if
     if( Ccm%VCC_Nb_Seg /= 0 ) then
       allocate( Ccm%VCC_Course(Ccm%VCC_Nb_Seg), stat=ErrCode )
       allocate( Ccm%VCC_A0(Ccm%VCC_Nb_Seg),     stat=ErrCode )
       allocate( Ccm%VCC_A1(Ccm%VCC_Nb_Seg),     stat=ErrCode )
     end if
     allocate( Ccm%ApexX(Ccm%NsCcm),                     stat=ErrCode )
     allocate( Ccm%ApexY(Ccm%NsCcm),                     stat=ErrCode )
     allocate( Ccm%ApexZ(Ccm%NsCcm),                     stat=ErrCode )
     if (ErrCode > 0) then
        write(0,*) 'allocation Ccm Error'
        write(0,*) 'Ccm: fatal error'
        call Err_outputErrorMessage(0)
        call exit(1)
     end if
     return
   end subroutine alloc_Ccm
!
!
   subroutine dalloc_Ccm( Ccm )
     implicit none
     type(type_Ccm), intent(inout)      :: Ccm
     if( Ccm%NSpeedvibfreq /= 0 ) then
       deallocate( Ccm%SpeedvibFreq   )
       deallocate( Ccm%SpeedvibAmp    )
       deallocate( Ccm%SpeedvibPhase  )
       deallocate( Ccm%SpeedvibPhi    )
     end if
     if( Ccm%NFocalvibfreq /= 0 ) then
       deallocate( Ccm%FocalvibFreq   )
       deallocate( Ccm%FocalvibAmp    )
       deallocate( Ccm%FocalvibPhase  )
       deallocate( Ccm%FocalvibPhi    )
     end if
     if( Ccm%VCC_Nb_Seg /= 0 ) then
       deallocate( Ccm%VCC_Course     )
       deallocate( Ccm%VCC_A0         )
       deallocate( Ccm%VCC_A1         )
     end if
     deallocate( Ccm%ApexX          )
     deallocate( Ccm%ApexY          )
     deallocate( Ccm%ApexZ          )
     return
   end subroutine dalloc_Ccm
!
!
   subroutine readccm( Ccm,iostatus )
   implicit none
     type(type_Ccm)   , intent(inout) :: Ccm
!
     integer(kind=LONG), intent(out)  :: iostatus
     integer(kind=DOUBLE)             :: ifile
     integer(kind=LONG)               :: offset
     integer(kind=LONG)               :: Type
     integer(kind=LONG)               :: Size
!
      iostatus = 0
      call open_file_r(Ccm%filename                    &
                       (1:len_trim(Ccm%filename))      &
                       // char(0), ifile)
      if ( ifile .eq. 0 ) then
         iostatus = 1
      else
         offset = 0
         Type = ch8Type
         Size = 8
         call read_field( ifile, Ccm%Law,                      &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call read_field( ifile, Ccm%VCC,                      &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ccm%Omega,                    &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ccm%T_Course,                 &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ccm%OpdMax,                   &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ccm%Radius,                   &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ccm%d_Radius1,                &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ccm%d_Radius2,                &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ccm%GenDistCC,                &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call read_field( ifile, Ccm%NsCcm,                    &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call read_field( ifile, Ccm%OffsetCCMx,               &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ccm%OffsetCCMy,               &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ccm%OffsetCCMz,               &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ccm%TiltCCMy,                 &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ccm%TiltCCMz,                 &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ccm%ParabolCCM,               &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ccm%ParabolPhi,               &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call read_field( ifile, Ccm%NSpeedvibfreq,            &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ccm%NFocalvibfreq,            &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ccm%VCC_Nb_Seg,               &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call alloc_Ccm( Ccm )
         if( Ccm%NSpeedvibfreq /= 0 ) then
           Type = r8Type
           Size = 8*Ccm%NSpeedvibfreq
           call read_field( ifile, Ccm%SpeedvibFreq,             &
                            %VAL(Type), %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Ccm%SpeedvibAmp,              &
                            %VAL(Type), %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Ccm%SpeedvibPhase,            &
                            %VAL(Type), %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Ccm%SpeedvibPhi,              &
                            %VAL(Type), %VAL(Size), %VAL(offset) )
           offset = offset + Size
         end if
         if( Ccm%NFocalvibfreq /= 0 ) then
           Type = r8Type
           Size = 8*Ccm%NFocalvibfreq
           call read_field( ifile, Ccm%FocalvibFreq,             &
                            %VAL(Type), %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Ccm%FocalvibAmp,              &
                            %VAL(Type), %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Ccm%FocalvibPhase,            &
                            %VAL(Type), %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Ccm%FocalvibPhi,              &
                            %VAL(Type), %VAL(Size), %VAL(offset) )
           offset = offset + Size
         end if
         if( Ccm%VCC_Nb_Seg /= 0 ) then
           Type = r8Type
           Size = 8*Ccm%VCC_Nb_Seg
           call read_field( ifile, Ccm%VCC_Course,               &
                            %VAL(Type), %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Ccm%VCC_A0,                   &
                            %VAL(Type), %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call read_field( ifile, Ccm%VCC_A1,                   &
                            %VAL(Type), %VAL(Size), %VAL(offset) )
           offset = offset + Size
         end if
         Type = r8Type
         Size = 8*Ccm%NsCcm
         call read_field( ifile, Ccm%ApexX,                    &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ccm%ApexY,                    &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call read_field( ifile, Ccm%ApexZ,                    &
                          %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call close_file(Ccm%filename              &
                         (1:len_trim(Ccm%filename))&
                         // char(0), ifile)
     end if
     return
  end subroutine readccm
!
!
   subroutine writeccm( Ccm,iostatus )
   implicit none
     type(type_Ccm), intent(in)      :: Ccm
!
     integer(kind=LONG), intent(out) :: iostatus
     integer(kind=DOUBLE)            :: ifile
     integer(kind=LONG)              :: offset
     integer(kind=LONG)              :: Type
     integer(kind=LONG)              :: Size
!
      iostatus = 0
      call open_file_w(Ccm%filename                    &
                       (1:len_trim(Ccm%filename))      &
                       // char(0), ifile)
      if ( ifile .eq. 0 ) then
         iostatus = 1
      else
         offset = 0
         Type = ch8Type
         Size = 8
         call write_field( ifile, Ccm%Law,                      &
                           %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call write_field( ifile, Ccm%VCC,                      &
                           %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ccm%Omega,                    &
                           %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ccm%T_Course,                 &
                           %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ccm%OpdMax,                   &
                           %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ccm%Radius,                   &
                           %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ccm%d_Radius1,                &
                           %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ccm%d_Radius2,                &
                           %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ccm%GenDistCC,                &
                           %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call write_field( ifile, Ccm%NsCcm,                    &
                           %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = r8Type
         Size = 8
         call write_field( ifile, Ccm%OffsetCCMx,               &
                           %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ccm%OffsetCCMy,               &
                           %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ccm%OffsetCCMz,               &
                           %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ccm%TiltCCMy,                 &
                           %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ccm%TiltCCMz,                 &
                           %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ccm%ParabolCCM,               &
                           %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ccm%ParabolPhi,               &
                           %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         Type = i4Type
         Size = 4
         call write_field( ifile, Ccm%NSpeedvibfreq,            &
                           %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ccm%NFocalvibfreq,            &
                           %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ccm%VCC_Nb_Seg,               &
                           %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         if( Ccm%NSpeedvibfreq /= 0 ) then
           Type = r8Type
           Size = 8*Ccm%NSpeedvibfreq
           call write_field( ifile, Ccm%SpeedvibFreq,             &
                             %VAL(Type), %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Ccm%SpeedvibAmp,              &
                             %VAL(Type), %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Ccm%SpeedvibPhase,            &
                             %VAL(Type), %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Ccm%SpeedvibPhi,              &
                             %VAL(Type), %VAL(Size), %VAL(offset) )
           offset = offset + Size
         end if
         if( Ccm%NFocalvibfreq /= 0 ) then
           Type = r8Type
           Size = 8*Ccm%NFocalvibfreq
           call write_field( ifile, Ccm%FocalvibFreq,             &
                             %VAL(Type), %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Ccm%FocalvibAmp,              &
                             %VAL(Type), %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Ccm%FocalvibPhase,            &
                             %VAL(Type), %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Ccm%FocalvibPhi,              &
                             %VAL(Type), %VAL(Size), %VAL(offset) )
           offset = offset + Size
         end if
         if( Ccm%VCC_Nb_Seg /= 0 ) then
           Type = r8Type
           Size = 8*Ccm%VCC_Nb_Seg
           call write_field( ifile, Ccm%VCC_Course,               &
                             %VAL(Type), %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Ccm%VCC_A0,                   &
                             %VAL(Type), %VAL(Size), %VAL(offset) )
           offset = offset + Size
           call write_field( ifile, Ccm%VCC_A1,                   &
                             %VAL(Type), %VAL(Size), %VAL(offset) )
           offset = offset + Size
         end if
         Type = r8Type
         Size = 8*Ccm%NsCcm
         call write_field( ifile, Ccm%ApexX,                    &
                           %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ccm%ApexY,                    &
                           %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call write_field( ifile, Ccm%ApexZ,                    &
                           %VAL(Type), %VAL(Size), %VAL(offset) )
         offset = offset + Size
         call close_file(Ccm%filename              &
                         (1:len_trim(Ccm%filename))&
                         // char(0), ifile)
     end if
     return
  end subroutine writeccm
!
!
end module ccm_type
