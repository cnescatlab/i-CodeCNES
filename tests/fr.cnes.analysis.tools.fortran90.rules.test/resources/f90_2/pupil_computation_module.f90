!!# pupil_computation_module.f90 --
!!#
!!#           Project: SPS_GENERIC
!!#           Authors: NOVELTIS/B.TOURNIER
!!#              Date: december 2010
!!#           Version: $Revision: 1.1 $
!!# Last modification: $Date: 2011-03-07 15:54:59 $
!!#
!!# Language:  F90
!!# Standards: Noveltis
!!#
!!# --
!!#
!! 
!> pupil_computation_module -- Module
!!
!! * Purpose
!!
!!     Module for pupil computation.
!!
!! * Description
!!      
!!     This module initialises and defines all the theoretical pupil parameters.
!!
!! * Sub-routines and functions
!!
!!     - pupil_init : Initialisation of the type defining the pupil
!!     - pupil_gen  : Computation of the theoretical pupil definition
!!
!! * References
!!

module pupil_computation_module
   use precision_type
   use error_type
   use constantes_type
   use srf_param_type
   use pupil_type
!
   implicit none
!
!
   public :: pupil_init, &
             pupil_gen
!
   contains
!
!

!!
!!
!> pupil_init -- Public
!!
!! * Purpose
!!
!!     Initialisation of the type defining the pupil
!!     
!!
!! * Description
!!
!!     In function the differents files defining pupil header parameters, pupil main 
!!     parameters, pupil wave front error parameters, pupil defect parameters and srf 
!!     parameters, the parameters defining the type_Pupil are initialised 
!!
!! * Inputs 
!!
!!     - File_Pupil_Header : character / name of input file defining pupil header parameters 
!!     - File_Pupil_Main   : character / name of input file defining pupil main parameters
!!     - File_Pupil_WFE    : character / name of input file defining pupil wave front error 
!!                           parameters
!!     - File_Pupil_Defect : character / name of input file defining pupil defect parameters 
!!     - Srf_Param : type_Srf_Param / type for declaration and allocation of srf parameters
!!
!! * Inputs/outputs
!!
!!     - Pupil : type_Pupil / type for declaration and allocation of Pupil
!!
!! * Outputs
!!
!! * References
!!

   subroutine pupil_init( File_Pupil_Header,&
                          File_Pupil_Main,  &
                          File_Pupil_WFE,   &
                          File_Pupil_Defect,&
                          Srf_Param,        &
                          Pupil             )
   implicit none
     character(len=*)    ,intent(in)                 :: File_Pupil_Header
     character(len=*)    ,intent(in)                 :: File_Pupil_Main
     character(len=*)    ,intent(in)                 :: File_Pupil_WFE
     character(len=*)    ,intent(in)                 :: File_Pupil_Defect
     type(type_Srf_Param),intent(in)                 :: Srf_Param
     type(type_Pupil)    ,intent(inout)              :: Pupil
     integer(kind=LONG)                              :: iFile
     integer(kind=LONG)                              :: iPos
     integer(kind=LONG)                              :: Nl
     integer(kind=LONG)                              :: Nc
     integer(kind=LONG)                              :: NbCol
     integer(kind=LONG)                              :: NbLin
     integer(kind=LONG)                              :: NsOpd
     integer(kind=LONG)                              :: ErrCode
     integer(kind=LONG)                              :: dummy1
     integer(kind=LONG)                              :: dummy2
     integer(kind=LONG)                              :: dummy3
     real(kind=DOUBLE)                               :: Y_Step
     real(kind=DOUBLE)                               :: Z_Step
     real(kind=DOUBLE)                               :: norm
     
     !
     iFile = 10
     iPos  = 1
     write(*,*) File_Pupil_Header
     open(unit=iFile, file=File_Pupil_Header, status='old', err=999 )
     Pupil%filename = File_Pupil_Header
     iPos  = 2
     read(iFile,*,err=999) 
     read(iFile,*,err=999) Pupil%NbCol
     read(iFile,*,err=999) Pupil%NbLin
     iPos  = 3
     call alloc_Pupil( Pupil )
     read(iFile,*,err=999) Y_Step ! in mm
     read(iFile,*,err=999) Z_Step
     close(unit=iFile)
     Pupil%Y(1:Pupil%NbCol) =                                         &
             (/ (dble(Nc-(int(Pupil%NbCol/2)+1)),Nc=1,Pupil%NbCol) /) &
             * Y_Step * real(0.001,kind=DOUBLE)
     Pupil%Z(1:Pupil%NbLin) =                                         &
             (/ (dble((int(Pupil%NbLin/2)+1)-Nl),Nl=1,Pupil%NbLin) /) &
             * Z_Step * real(0.001,kind=DOUBLE)
!
!    conversion in radians
     Pupil%Y(1:Pupil%NbCol) = datan2( Pupil%Y(1:Pupil%NbCol), Srf_Param%H_Sat )
     Pupil%Z(1:Pupil%NbLin) = datan2( Pupil%Z(1:Pupil%NbLin), Srf_Param%H_Sat )
     !
     iPos  = 4
     ! read pupil design
     write(*,*) File_Pupil_Main
     open(unit=iFile, file=File_Pupil_Main, status='old', err=999 )
     iPos  = 5
     read(iFile,*,err=999) 
     read(iFile,*,err=999) NbCol
     write(*,*) 'NbCol',NbCol
     read(iFile,*,err=999) NbLin
     write(*,*) 'NbLin',NbLin
     if( (NbCol /= Pupil%NbCol) .or. (NbLin /= Pupil%NbLin) ) then
        write(*,*) 'PUPIL NbCol or NbLin Error',NbCol,NbLin
        go to 999
     end if
     iPos  = 6 
     read(iFile,*,err=999)
     do Nl = 1, Pupil%NbLin
        do Nc = 1, Pupil%NbCol
           read(iFile,*,err=999) dummy1, dummy2, Pupil%Wgt(Nc,Nl)
        end do
     end do
     close(unit=iFile)
     ! verifies integral
     norm = sum( Pupil%Wgt(:,:))
     write(*,*) 'PUPIL norm', norm
     ! read wave front error
     write(*,*) File_Pupil_WFE
     open(unit=iFile, file=File_Pupil_WFE, status='old', err=999 )
     iPos  = 7
     Pupil%NsOpd = Srf_Param%NsOpd
     read(iFile,*,err=999) 
     read(iFile,*,err=999) NbCol
     write(*,*) 'NbCol',NbCol
     read(iFile,*,err=999) NbLin
     write(*,*) 'NbLin',NbLin
     read(iFile,*,err=999) NsOpd
     write(*,*) 'NsOpd', NsOpd
    
     if ( (NbCol /= Pupil%NbCol) .or. (NbLin /= Pupil%NbLin) .or. &
          (NsOpd /= Pupil%NsOpd) ) then
        write(*,*) 'PUPIL NbCol or NbLin or NsOpd error',NbCol,NbLin,NsOpd,&
            Pupil%NbCol,  Pupil%NbLin, Pupil%NsOpd 
        go to 999
     end if
     iPos  = 8
     allocate( Pupil%WFE(Pupil%NbCol,Pupil%NbLin,Pupil%NsOpd),  stat=ErrCode ) 
     if ( ErrCode /= 0 ) then
        write(*,*) 'Error while allocating WFE matrix'
     end if
     read(iFile,*,err=999) 
     do Nl = 1, Pupil%NbLin
        do Nc = 1, Pupil%NbCol
           do NsOpd = 1, Pupil%NsOpd
              read(iFile,*,err=999) dummy1, dummy2, dummy3, Pupil%WFE(Nc,Nl,NsOpd)
           end do
        end do
     end do
     close(unit=iFile)
     ! read pupil defect
     write(*,*) File_Pupil_Defect
     open(unit=iFile, file=File_Pupil_Defect, status='old', err=999 )
     iPos  = 9
     read(iFile,*,err=999) 
     read(iFile,*,err=999) NsOpd
     write(*,*) 'NsOpd', NsOpd
    
     if ( NsOpd /= Pupil%NsOpd ) then
        write(*,*) 'PUPIL NsOpd error',NsOpd, Pupil%NsOpd
        go to 999
     end if
     iPos  = 10
     allocate( Pupil%AlphaY(Pupil%NsOpd), stat=ErrCode )
     if ( ErrCode /= 0 ) then
        write(*,*) 'Error while allocating AlphaY matrix'
     end if
     allocate( Pupil%AlphaZ(Pupil%NsOpd),  stat=ErrCode )
     if ( ErrCode /= 0 ) then
        write(*,*) 'Error while allocating AlphaZ matrix'
     end if
     read(iFile,*,err=999)
     do NsOpd = 1,Pupil%NsOpd
        read(iFile,*,err=999) dummy1, Pupil%AlphaY(NsOpd)
     end do
     read(iFile,*,err=999)
     do NsOpd = 1, Pupil%NsOpd
        read(iFile,*,err=999) dummy1, Pupil%AlphaZ(NsOpd)
     end do
     close(unit=iFile)
     return

999  write(*,*) 'parameters reading error',iPos
     write(*,*) 'pupil_init Fatal Error'
     call exit(1)
   end subroutine pupil_init
!
!----------------------------------------------------
!!
!!
!> pupil_gen -- Public
!!
!! * Purpose
!!
!!    Computation of the theoretical pupil definition
!!
!! * Description
!!
!!     This subroutine computes the theoretical pupil definition. 
!!
!! * Inputs 
!!
!!     - File_Pupil_Header : character / name of input file defining pupil header parameters 
!!
!! * Inputs/outputs
!!
!!     - Pupil : type_Pupil / type for declaration and allocation of Pupil
!!
!! * Outputs
!!
!! * References
!!

   subroutine pupil_gen( File_Pupil_Header, Pupil )
   implicit none
   character(len=*)    ,intent(in)                 :: File_Pupil_Header   
   type(type_Pupil)    ,intent(inout)              :: Pupil
   integer(kind=LONG)                              :: Nl
   integer(kind=LONG)                              :: Nc
   integer(kind=LONG)                              :: radius
   integer(kind=LONG)                              :: circle
   integer(kind=LONG)                              :: pts
   integer(kind=LONG)                              :: iPos
   integer(kind=LONG)                              :: iFile
   real(kind=DOUBLE)                               :: norm
   
   !
    iFile = 10
    iPos  = 1
    write(*,*) File_Pupil_Header
    open(unit=iFile, file=File_Pupil_Header, status='old', err=999 )
    Pupil%filename = File_Pupil_Header
    iPos  = 2
    read(iFile,*,err=999) 
    read(iFile,*,err=999) Pupil%NbCol
    read(iFile,*,err=999) Pupil%NbLin
    iPos  = 3
    call alloc_Pupil( Pupil )
    !
    iPos  = 4
    if ( Pupil%NbCol > Pupil%NbLin ) then
       radius = Pupil%NbLin/2
    else
       radius = Pupil%NbCol/2
    end if
    ! theoretical pupil definition
    pts = 0
    Pupil%Wgt(:,:)= real(0,kind=DOUBLE)
    do Nl = 1, Pupil%NbLin
       do Nc = 1, Pupil%NbCol
          circle = (Nc-(int(Pupil%NbCol/2)+1))**2 + (Nl-(int(Pupil%NbLin/2)+1))**2
          if ( circle <= radius**2 ) then
             Pupil%Wgt(Nc,Nl)= real(1,kind=DOUBLE)
             pts = pts + 1
          end if
       end do
    end do
     Pupil%Wgt(:,:) = Pupil%Wgt(:,:)/real(pts,kind=DOUBLE)
     ! verifies integral
     norm = sum( Pupil%Wgt(:,:))
     write(*,*) 'PUPIL norm', norm
     ! writes results
     do Nl = 1, Pupil%NbLin
        do Nc = 1, Pupil%NbCol
           write(*,*) Nc, Nl, Pupil%Wgt(Nc,Nl)
        end do
     end do
     
     return
999  write(*,*) 'parameters reading error', iPos
     write(*,*) 'pupil_gen Fatal Error'
     call exit(1)
   end subroutine pupil_gen
!
!
   subroutine pupil_wfe( Pupil )
   implicit none
   type(type_Pupil)    ,intent(inout)                :: Pupil
     integer(kind=LONG)                              :: Ns
     integer(kind=LONG)                              :: Nl
     integer(kind=LONG)                              :: Nc
     do Nc = 1, Pupil%NbCol
        do Nl = 1, Pupil%NbLin
           do Ns = 1, Pupil%NsOpd
              Pupil%WFE(Nc,Nl,Ns) = 0_DOUBLE
           end do
        end do
     end do
     return
   end subroutine pupil_wfe
!
!
   subroutine pupil_defect( Pupil )
   implicit none
   type(type_Pupil)    ,intent(inout)                :: Pupil
     integer(kind=LONG)                              :: Ns
     do Ns = 1, Pupil%NsOpd
        Pupil%AlphaY(Ns) = 0_DOUBLE
     end do
     do Ns = 1, Pupil%NsOpd
        Pupil%AlphaZ(Ns) = 0_DOUBLE
     end do
     return
   end subroutine pupil_defect
!
!
 end module pupil_computation_module
