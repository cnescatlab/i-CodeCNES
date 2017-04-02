!!#  non_linearity_module.f90 -- 
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/J.DONNADILLE
!!#               Date: february 2011
!!#            Version: $Revision: 1.1 $
!!#  Last modification: $Date: 2011-04-12 08:32:31 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!> non_linearity_module -- Module
!!
!! * Purpose
!!
!!   Module for interferogram non linearity
!!
!! * Description
!!      
!!   This module simulates non-linearity of an Analog and Digital interferogram.
!!
!! * Sub-routines and functions
!!
!!     -  non_linearity_init   : initialisation of the type defining the non-linearity 
!!                               parameters 
!!     - analog_non_linearity  : apply analog non-linearity to the interferogram
!!     - digital_non_linearity : apply digital non-linearity to the interferogram
!!
!! * References
!!
!!     SPS ATBD
!!

module non_linearity_module
  use precision_type
  use error_type
  use constantes_type
  use amplification_type
  use non_linearity_type
  use interferogram_type
  use math_module
  !
  implicit none
  !
  !
  public :: non_linearity_init,    &
            analog_non_linearity,  &
            digital_non_linearity
  !
  !
  contains
  !
  !
  !> non_linearity_init -- Public
  !!
  !! * Purpose
  !!
  !!     Initialisation of the type defining the non linearity parameters 
  !!
  !! * Description
  !!
  !!     parameters thanks to the read of the File_Param file; 
  !!
  !!     The detection non-linearity: The interferogram measured by the detection sub-system
  !!     is slightly  diffrent from a theoretical interferogram that will be measured by a 
  !!     linear detection sub-system. The anlog non-linearity is simulated by a deg-order 
  !!     polynomial. The polynomial function is calculated on the range of the analogic 
  !!     signal at a given sampling and put in a lookup table. 
  !!     The analogic non-linearity correction lookup table is calculated by inverting the
  !!     analogic non-linearity lookup table
  !!
  !!     Digital non-linearity: ADCs suffer from non-linearity errors due to their physical 
  !!     imperfections inducing their output to deviate from a linear function of their input.   
  !!     The digital non-linearity is simulated by a deg-order polynomial. The polynomial function 
  !!     is calculated on the range of the digital signal at a given sampling and put 
  !!     in a lookup table. 
  !!     The digital non-linearity correction lookup tableis calculated by inverting the 
  !!     digital non-linearity lookup table.
  !!
  !! * Inputs
  !!
  !!     - File_Param : character / input file with definition of analogic and digital
  !!                    non-linearity parameters
  !!
  !! * Outputs
  !!
  !!     - Non_Linearity : type_non_linearity / type for declaration and allocation
  !!                     conversion parameters 
  !!
  !! * References
  !!     SPS ATBD

  subroutine non_linearity_init( File_Param,   &
                                 Non_Linearity )
    implicit none
    character(len=*)              ,intent(in)    :: File_Param
    type(type_non_linearity)      ,intent(inout) :: Non_Linearity
    !
    integer(kind=LONG)                           :: iFile
    integer(kind=LONG)                           :: iPos
    integer(kind=LONG)                           :: deg
    integer(kind=LONG)                           :: Ns
    integer(kind=LONG)                           :: N1
    integer(kind=LONG)                           :: N2
    !
    iFile = 10
    !
    iPos = 1
    write(*,'(a)') File_Param(1:len_trim(File_Param))
    open(unit=iFile, file=File_Param, status='old',err=999)
    Non_Linearity%filename(1:len_trim(File_Param)) = &
                    File_Param(1:len_trim(File_Param))
    iPos = 2
    read(iFile,*,err=999) 
    read(iFile,*,err=999) Non_Linearity%X_Min_Table
    read(iFile,*,err=999) Non_Linearity%X_Max_Table
    read(iFile,*,err=999) Non_Linearity%dX_Table
    read(iFile,*,err=999) Non_Linearity%Y_Min_Table
    read(iFile,*,err=999) Non_Linearity%Y_Max_Table
    read(iFile,*,err=999) Non_Linearity%dY_Table
    read(iFile,*,err=999) Non_Linearity%U_Min_Table
    read(iFile,*,err=999) Non_Linearity%U_Max_Table
    read(iFile,*,err=999) Non_Linearity%dU_Table
    read(iFile,*,err=999) Non_Linearity%V_Min_Table
    read(iFile,*,err=999) Non_Linearity%V_Max_Table
    read(iFile,*,err=999) Non_Linearity%dV_Table
    Non_Linearity%N_X_Sample = idnint( ( Non_Linearity%X_Max_Table  &
                                       - Non_Linearity%X_Min_Table )&
                                       / Non_Linearity%dX_Table ) + 1
    Non_Linearity%N_U_Sample = idnint( ( Non_Linearity%U_Max_Table  &
                                       - Non_Linearity%U_Min_Table )&
                                       / Non_Linearity%dU_Table ) + 1
    read(iFile,*,err=999) Non_Linearity%Analog_Poly_Deg
    read(iFile,*,err=999) Non_Linearity%Digital_Poly_Deg
    call alloc_non_linearity( Non_Linearity )
    iPos = 3
    do deg = 0, Non_Linearity%Analog_Poly_Deg
       read(iFile,*,err=999) Non_Linearity%Analog_Poly_Coeff(deg)
    end do
    iPos = 4
    do deg = 0, Non_Linearity%Digital_Poly_Deg
       read(iFile,*,err=999) Non_Linearity%Digital_Poly_Coeff(deg)
    end do
    close(unit=iFile)
!
    write(*,*) 'Non_Linearity%Analog_Poly_Deg  ',&
                Non_Linearity%Analog_Poly_Deg
    do deg = 0, Non_Linearity%Analog_Poly_Deg
       write(*,*) 'Deg, Non_Linearity%Analog_Poly_Coeff(Deg)  ', &
                   deg, Non_Linearity%Analog_Poly_Coeff(deg)
    end do
!
    write(*,*) 'Non_Linearity%Digital_Poly_Deg  ',&
                Non_Linearity%Digital_Poly_Deg
    do deg = 0, Non_Linearity%Digital_Poly_Deg
       write(*,*) 'Deg, Non_Linearity%Digital_Poly_Coeff(Deg)  ', &
                   deg, Non_Linearity%Digital_Poly_Coeff(deg)
    end do
!
!   non linearity table analog before amplification
    Non_Linearity%X_Table(1:Non_Linearity%N_X_Sample) =         &
               Non_Linearity%X_Min_Table                        &
             + (/ (dble(Ns-1), Ns=1,Non_Linearity%N_X_Sample) /)&
               * Non_Linearity%dX_Table
    call polyappli( Non_Linearity%N_X_Sample,       &
                    Non_Linearity%X_Table,          &
                    Non_Linearity%Analog_Poly_Coeff,&
                    Non_Linearity%Analog_Poly_Deg,  &
                    Non_Linearity%Y_Table           )
!
!   Inverse non linearity table analog before amplification
    Non_Linearity%Y_Min_Table = minval(Non_Linearity%Y_Table)
    Non_Linearity%Y_Max_Table = maxval(Non_Linearity%Y_Table)
    Non_Linearity%dY_Table = ( Non_Linearity%Y_Max_Table      &
                              -Non_Linearity%Y_Min_Table )    &
                             / dble(Non_Linearity%N_X_Sample-1)
    Non_Linearity%X_Table_Inv(1:Non_Linearity%N_X_Sample) =     &
                Non_Linearity%Y_Min_Table                       &
             + (/ (dble(Ns-1), Ns=1,Non_Linearity%N_X_Sample) /)&
               * Non_Linearity%dY_Table
    do Ns = 1, Non_Linearity%N_X_Sample
       N1 = 1
       N2 = Non_Linearity%N_X_Sample
       call dichotomd( Non_Linearity%X_Table_Inv(Ns),       &
                       Non_Linearity%Y_Table(N1:N2), N1, N2 )
       if( N1 == N2 ) then
          Non_Linearity%Y_Table_Inv(Ns) = Non_Linearity%X_Table(N1)
       else
          Non_Linearity%Y_Table_Inv(Ns) = Non_Linearity%X_Table(N1)        &
             + ( Non_Linearity%X_Table(N2) - Non_Linearity%X_Table(N1) )   &
             / ( Non_Linearity%Y_Table(N2) - Non_Linearity%Y_Table(N1) )   &
             * ( Non_Linearity%X_Table_Inv(Ns) - Non_Linearity%Y_Table(N1) )
       end if
    end do
!
!   non linearity table ADC conversion
    Non_Linearity%U_Table(1:Non_Linearity%N_U_Sample) =         &
               Non_Linearity%U_Min_Table                        &
             + (/ (dble(Ns-1), Ns=1,Non_Linearity%N_U_Sample) /)&
               * Non_Linearity%dU_Table
    call polyappli( Non_Linearity%N_U_Sample,        &
                    Non_Linearity%U_Table,           &
                    Non_Linearity%Digital_Poly_Coeff,&
                    Non_Linearity%Digital_Poly_Deg,  &
                    Non_Linearity%V_Table            )
!
!   Inverse non linearity table ADC conversion
    Non_Linearity%V_Min_Table = minval(Non_Linearity%V_Table)
    Non_Linearity%V_Max_Table = maxval(Non_Linearity%V_Table)
    Non_Linearity%dV_Table = ( Non_Linearity%V_Max_Table      &
                              -Non_Linearity%V_Min_Table )    &
                             / dble(Non_Linearity%N_U_Sample-1)
    Non_Linearity%U_Table_Inv(1:Non_Linearity%N_U_Sample) =     &
                Non_Linearity%V_Min_Table                       &
             + (/ (dble(Ns-1), Ns=1,Non_Linearity%N_U_Sample) /)&
               * Non_Linearity%dV_Table
    do Ns = 1, Non_Linearity%N_U_Sample
       N1 = 1
       N2 = Non_Linearity%N_U_Sample
       call dichotomd( Non_Linearity%U_Table_Inv(Ns),       &
                       Non_Linearity%V_Table(N1:N2), N1, N2 )
       if( N1 == N2 ) then
          Non_Linearity%V_Table_Inv(Ns) = Non_Linearity%U_Table(N1)
       else
          Non_Linearity%V_Table_Inv(Ns) = Non_Linearity%U_Table(N1)        &
             + ( Non_Linearity%U_Table(N2) - Non_Linearity%U_Table(N1) )   &
             / ( Non_Linearity%V_Table(N2) - Non_Linearity%V_Table(N1) )   &
             * ( Non_Linearity%U_Table_Inv(Ns) - Non_Linearity%V_Table(N1) )
       end if
    end do
    !
    return
999 write(*,*) 'parameters reading error', iPos
    write(*,*) 'non_linearity_init Fatal Error'
    call exit(1)
  end subroutine non_linearity_init
  !
  !
  !> analog_non_linearity -- Public
  !!
  !! * Purpose
  !!
  !!     Simulates analog non-linearity.
  !!
  !! * Description
  !!
  !!     The analog_non_linearity subroutine allows to simulates the analogic non-linearity. 
  !!       -  the analogic non-linearity is calculated by linear interpolation between 
  !!          two values of the analogic non-linearity lookup table.
  !!
  !! * Input
  !!
  !!     - Non-Linearity   : type_Non-Linearity / type for declaration and allocation
  !!                       conversion parameters  
  !!
  !! * Input/output
  !!
  !!     - Interf : type_Interferogram / type for declaration of interferogram
  !!
  !!
  !! * References
  !!
  !!   SPS ATBD


  subroutine analog_non_linearity( Non_Linearity,&
                                   Interf        )
    implicit none
    type(type_non_linearity)  ,intent(in)               :: Non_Linearity
    type(type_Interferogram)  ,intent(inout)            :: Interf
    !
    real(kind=DOUBLE)                                   :: Interf_tmp
    integer(kind=LONG)                                  :: Ns
    integer(kind=LONG)                                  :: N1
    integer(kind=LONG)                                  :: N2
    !
    ! apply non-linearity
    if ( Non_Linearity%Analog_Poly_Deg > 1 ) then
       do Ns = 1, Interf%N_Sample
          N1 = 1
          N2 = Non_Linearity%N_X_Sample
          call dichotomd( Interf%Real_Part(Ns),        &
                          Non_Linearity%X_Table(N1:N2),&
                          N1, N2 )
          if( N1 == N2 ) then
             Interf_tmp = Non_Linearity%Y_Table(N1)
          else
             Interf_tmp = Non_Linearity%Y_Table(N1)                        &
                + ( Non_Linearity%Y_Table(N2) - Non_Linearity%Y_Table(N1) )&
                / ( Non_Linearity%X_Table(N2) - Non_Linearity%X_Table(N1) )&
                * ( Interf%Real_Part(Ns)      - Non_Linearity%X_Table(N1) )
          end if
          Interf%Real_Part(Ns) = Interf_tmp
       end do
    else
       write(*,*) ' no apply non-linearity, Analog_Poly_Deg', &
            Non_Linearity%Analog_Poly_Deg 
    end if   
    !    
    !
    return
  end subroutine analog_non_linearity
  !
  !
  !> digital_non_linearity -- Public
  !!
  !! * Purpose
  !!
  !!     Simulates digital non-linearity.
  !!
  !! * Description
  !!
  !!     The digital_non_linearity subroutine allows to simulates the digital non-linearity. 
  !!       -  the digital non-linearity is calculated by linear interpolation between 
  !!          two values of the digital non-linearity lookup table.
  !!
  !! * Input
  !!
  !!     - Non-Linearity   : type_Non-Linearity / type for declaration and allocation
  !!                       conversion parameters  
  !!
  !! * Input/output
  !!
  !!     - Interf : type_Interferogram / type for declaration of interferogram
  !!
  !!
  !! * References
  !!
  !!   SPS ATBD
  subroutine digital_non_linearity( Non_Linearity,&
                                    Interf        )
    implicit none
    type(type_non_linearity)  ,intent(in)               :: Non_Linearity
    type(type_Interferogram)  ,intent(inout)            :: Interf
    !
    real(kind=DOUBLE)                                   :: Interf_tmp
    integer(kind=LONG)                                  :: Ns
    integer(kind=LONG)                                  :: N1
    integer(kind=LONG)                                  :: N2
    !
    ! apply non-linearity
    if ( Non_Linearity%Digital_Poly_Deg > 1 ) then
       do Ns = 1, Interf%N_Sample
          N1 = 1
          N2 = Non_Linearity%N_U_Sample
          call dichotomd( Interf%Real_Part(Ns),        &
                          Non_Linearity%U_Table(N1:N2),&
                          N1, N2 )
          if( N1 == N2 ) then
             Interf_tmp = Non_Linearity%V_Table(N1)
          else
             Interf_tmp = Non_Linearity%V_Table(N1)                        &
                + ( Non_Linearity%V_Table(N2) - Non_Linearity%V_Table(N1) )&
                / ( Non_Linearity%U_Table(N2) - Non_Linearity%U_Table(N1) )&
                * ( Interf%Real_Part(Ns)      - Non_Linearity%U_Table(N1) )
          end if
          Interf%Real_Part(Ns) = Interf_tmp
       end do
    else
       write(*,*) ' no apply non-linearity, Digital_Poly_Deg', &
            Non_Linearity%Digital_Poly_Deg 
    end if   
    !    
    !
    return
  end subroutine digital_non_linearity
  !
  !
end module non_linearity_module
