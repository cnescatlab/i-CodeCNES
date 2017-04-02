!!* non_linearity_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/J.DONNADILLE
!!*              Date: february 2011
!!*           Version: $Revision: 1.1 $
!!* Last modification: $Date: 2011-04-12 08:32:32 $
!!
!> type_non_linearity -- Module
!!
!! * Purpose
!!
!!   Module for type non-linearity parameters declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the non-linearity parameters type and allows 
!!     its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_non_linearity : type non-linearity declaration
!!       parameters  
!!     * alloc_non_linearity: allocation of non-linearity type
!!
!! * References
!! 

module non_linearity_type
  use precision_type
  use error_type
!
  implicit none
!
!
  public :: type_non_linearity, &
            alloc_non_linearity,&
            dalloc_non_linearity
!
!
  type :: type_non_linearity
     character(len=500)                          :: filename !< non-linearity parametres file name  
     integer(kind=LONG)                          :: Analog_Poly_Deg !< polynom degree for analogic non-linearity simulation
     integer(kind=LONG)                          :: Digital_Poly_Deg !< polynom degree for digital non-linearity simulation
     real(kind=DOUBLE) ,dimension(:),allocatable :: Analog_Poly_Coeff !< Analogic polynom coefficients for non-linearity simulation - Analog_Poly_Coeff(0:Analog_Poly_Deg)
     real(kind=DOUBLE) ,dimension(:),allocatable :: Digital_Poly_Coeff !< Digital polynom coefficients for non-linearity simulation - Digital_Poly_Coeff(0:Digital_Poly_Deg)
     integer(kind=LONG)                          :: N_X_Sample  !< X Sample Number for analogic non-linearity simulation
     integer(kind=LONG)                          :: N_U_Sample  !< U Sample Number for digital non-linearity simulation
     real(kind=DOUBLE)                           :: dX_Table    !< Sampling of analogic signal 
     real(kind=DOUBLE)                           :: dY_Table    !< Sampling of analogic signal after non-linearity simulation
     real(kind=DOUBLE)                           :: dU_Table    !< Sampling of digital signal
     real(kind=DOUBLE)                           :: dV_Table    !< Sampling of digital signal after non-linearity simulation
     real(kind=DOUBLE)                           :: X_Min_Table !< Min value of analogic signal 
     real(kind=DOUBLE)                           :: X_Max_Table !< Max value of analogic signal 
     real(kind=DOUBLE)                           :: Y_Min_Table !< Min value of analogic signal after non-linearity simulation
     real(kind=DOUBLE)                           :: Y_Max_Table !< Max value of analogic signal after non-linearity simulation
     real(kind=DOUBLE)                           :: U_Min_Table !< Min value of digital signal
     real(kind=DOUBLE)                           :: U_Max_Table !< Max value of digital signal
     real(kind=DOUBLE)                           :: V_Min_Table !< Min value of digital signal after non-linearity simulation
     real(kind=DOUBLE)                           :: V_Max_Table !< Max value of digital signal after non-linearity simulation
     real(kind=DOUBLE) ,dimension(:),allocatable :: X_Table     !< Analogic signal - X_Table(N_X_Sample)
     real(kind=DOUBLE) ,dimension(:),allocatable :: Y_Table     !< Analogic signal after non-linearity simulation - Y_Table(N_X_Sample)
     real(kind=DOUBLE) ,dimension(:),allocatable :: U_Table     !< Digital signal - U_Table(N_U_Sample)
     real(kind=DOUBLE) ,dimension(:),allocatable :: V_Table     !< Digital signal after non-linearity simulation - V_Table(N_U_Sample)
     real(kind=DOUBLE) ,dimension(:),allocatable :: X_Table_Inv !< Analogic signal with non-linearity - X_Table_Inv(N_X_Sample)
     real(kind=DOUBLE) ,dimension(:),allocatable :: Y_Table_Inv !< Analogic signal after non-linearity correction - Y_Table_Inv(N_X_Sample)
     real(kind=DOUBLE) ,dimension(:),allocatable :: U_Table_Inv !< Digital signal with non-linearity - U_Table_Inv(N_U_Sample) 
     real(kind=DOUBLE) ,dimension(:),allocatable :: V_Table_Inv !< Digital signal after non-linearity correction - V_Table_Inv(N_U_Sample)
  end type type_non_linearity
!
!   
contains
!
!
  subroutine alloc_non_linearity( Non_Linearity )
    implicit none
    type(type_non_linearity), intent(inout)     :: Non_Linearity
    integer(kind=LONG)                          :: ErrCode
    integer(kind=LONG)                          :: ioalloc
    !
    ioalloc = 0
    allocate( Non_Linearity%Analog_Poly_Coeff(0:Non_Linearity          &
         %Analog_Poly_Deg), stat=ErrCode )
    if( ErrCode /= 0 ) ioalloc = ioalloc + 1

    allocate( Non_Linearity%Digital_Poly_Coeff(0:Non_Linearity         &
         %Digital_Poly_Deg),stat=ErrCode )
    if( ErrCode /= 0 ) ioalloc = ioalloc + 1

    allocate( Non_Linearity%X_Table(Non_Linearity%N_X_Sample),         &
         stat=ErrCode )
    if( ErrCode /= 0 ) ioalloc = ioalloc + 1

    allocate( Non_Linearity%Y_Table(Non_Linearity%N_X_Sample),         &
         stat=ErrCode )
    if( ErrCode /= 0 ) ioalloc = ioalloc + 1

    allocate( Non_Linearity%U_Table(Non_Linearity%N_U_Sample),         &
         stat=ErrCode )
    if( ErrCode /= 0 ) ioalloc = ioalloc + 1

    allocate( Non_Linearity%V_Table(Non_Linearity%N_U_Sample),         &
         stat=ErrCode )
    if( ErrCode /= 0 ) ioalloc = ioalloc + 1
    
    allocate( Non_Linearity%X_Table_Inv(Non_Linearity%N_X_Sample),     &
         stat=ErrCode )
    if( ErrCode /= 0 ) ioalloc = ioalloc + 1

    allocate( Non_Linearity%Y_Table_Inv(Non_Linearity%N_X_Sample),     &
         stat=ErrCode )
    if( ErrCode /= 0 ) ioalloc = ioalloc + 1

    allocate( Non_Linearity%U_Table_Inv(Non_Linearity%N_U_Sample),     &
         stat=ErrCode )
    if( ErrCode /= 0 ) ioalloc = ioalloc + 1

    allocate( Non_Linearity%V_Table_Inv(Non_Linearity%N_U_Sample),     &
         stat=ErrCode )
    if( ErrCode /= 0 ) ioalloc = ioalloc + 1

    if (ErrCode > 0) then
       write(0,*) 'allocation non_linearity Error ', ioalloc
       write(0,*) 'Non_Linearity: fatal error'
       call Err_outputErrorMessage(0)
       call exit(1)
    end if

    return
  end subroutine alloc_non_linearity
!
!
  subroutine dalloc_non_linearity( Non_Linearity )
    implicit none
    type(type_non_linearity), intent(inout) :: Non_Linearity

    deallocate( Non_Linearity%Analog_Poly_Coeff )
    deallocate( Non_Linearity%Digital_Poly_Coeff )
    deallocate( Non_Linearity%X_Table )
    deallocate( Non_Linearity%Y_Table )
    deallocate( Non_Linearity%U_Table )
    deallocate( Non_Linearity%V_Table )
    deallocate( Non_Linearity%X_Table_Inv )
    deallocate( Non_Linearity%Y_Table_Inv )
    deallocate( Non_Linearity%U_Table_Inv )
    deallocate( Non_Linearity%V_Table_Inv )

    return
  end subroutine dalloc_non_linearity
!
!
end module non_linearity_type
