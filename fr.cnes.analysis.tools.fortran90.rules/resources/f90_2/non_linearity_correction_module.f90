!!#  non_linearity_correction_module.f90 -- 
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: march 2011
!!#            Version: $Revision: 1.2 $
!!#  Last modification: $Date: 2011-11-02 14:57:50 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!> non_linearity_correction_module -- Module
!!
!! * Purpose
!!
!!   Module for interferogram non linearity correction
!!
!! * Description
!!      
!!   This module correct for non-linearity of a digital interferogram.
!!
!! * Sub-routines and functions
!!
!!    -  non_linearity_correction : correct for non-linearity of a digital interferogram
!!
!! * References
!!
!!     SPS ATBD
!!
module non_linearity_correction_module
  use precision_type
  use error_type
  use constantes_type
  use amplification_type
  use adc_convert_type
  use non_linearity_type
  use interferogram_type
  use math_module
  !
  implicit none
  !
  !
  public ::                          &
            interf_triplet_nlc,      &
            non_linearity_correction
  !
  !
contains
!
!
!>   interf_triplet_nlc-- Public
!!
!! * Purpose
!!
!!      Non linearity correction of triplet target   
!!
!! * Description
!!
!!      The subroutine correct digital interferogram non-linearity of the 3 targets CS/BB/EW
!!
!! * Inputs 
!!
!!     - Non_Linearity : type non_linearity/type for declaration and allocation of non linearity
!!     - Adc_Convert   : type Adc_Convert/type for declaration and allocation of ADC conversion
!!     - Amplification : type Amplification/type for declaration and allocation of interferogram signal 
!!                       amplification
!!
!! * Inputs-Outputs
!!
!!     - Interf_CS : type Interferogram / type for declaration and allocation of CS interferogram
!!     - Interf_BB : type Interferogram / type for declaration and allocation of BB interferogram
!!     - Interf_EW : type Interferogram / type for declaration and allocation of EW interferogram
!!
!! * References
!!   SPS ATBD
!!
  subroutine interf_triplet_nlc( Non_Linearity, &
                                 Adc_Convert_Nadc,&
                                 Amplification, &
                                 Interf_CS,     &
                                 Interf_BB,     &
                                 Interf_EW      )
  implicit none
    type(type_Amplification) ,intent(in)                :: Amplification
    type(type_Adc_Convert_Nadc),intent(in)              :: Adc_Convert_Nadc
    type(type_non_linearity) ,intent(in)                :: Non_Linearity
    type(type_Interferogram) ,intent(inout)             :: Interf_CS
    type(type_Interferogram) ,intent(inout)             :: Interf_BB
    type(type_Interferogram) ,intent(inout)             :: Interf_EW
!
!   Cold Space linearisation
    call non_linearity_correction( Non_Linearity,    &
                                   Adc_Convert_Nadc, &
                                   Amplification,    &
                                   Interf_CS      )
!
!   Black Body linearisation
    call non_linearity_correction( Non_Linearity,    &
                                   Adc_Convert_Nadc, &
                                   Amplification,    &
                                   Interf_BB      )
!
!   Earth View linearisation
    call non_linearity_correction( Non_Linearity,    &
                                   Adc_Convert_Nadc, &
                                   Amplification,    &
                                   Interf_EW      )
    return
  end subroutine interf_triplet_nlc
!
!
!>  non_linearity_correction -- Public
!!
!! * Purpose
!!
!!      Non linearity correction    
!!
!! * Description
!!
!!      The subroutine correct digital interferogram non-linearity in 4 steps:
!!        Step 1 ADC output --> ADC input : inverts ADC encoding law
!!        Step 2 ADC input --> Amplificator output: apply digital non-linearity correction
!!                                                       lookup table
!!        Step 3 Amplificator output --> Amplificator input: inverts amplification law
!!        Step 4 Amplificator input --> Detector output: apply analogic non-linearity correction
!!                                                       lookup table
!!
!! * Inputs 
!!
!!     - Non_Linearity : type non_linearity/type for declaration and allocation of non linearity
!!     - Adc_Convert   : type Adc_Convert/type for declaration and allocation of ADC conversion
!!     - Amplification : type Amplification/type for declaration and allocation of interferogram signal 
!!                       amplification
!!
!! * Inputs-Outputs
!!
!!     - Interf : type Interferogram / type for declaration and allocation of interferogram
!!
!! * References
!!   SPS ATBD
!!
  subroutine non_linearity_correction( Non_Linearity,&
                                       Adc_Convert_Nadc,&
                                       Amplification,&
                                       Interf        )
    implicit none
    type(type_non_linearity) ,intent(in)                :: Non_Linearity
    type(type_Adc_Convert_Nadc),intent(in)              :: Adc_Convert_Nadc
    type(type_Amplification) ,intent(in)                :: Amplification
    type(type_Interferogram) ,intent(inout)             :: Interf
    real(kind=DOUBLE)                                   :: Offset
    real(kind=DOUBLE)                                   :: Gain
    real(kind=DOUBLE)                                   :: Interf_tmp
    integer(kind=LONG)                                  :: Ns
    integer(kind=LONG)                                  :: N1
    integer(kind=LONG)                                  :: N2
    integer(kind=LONG)                                  :: first
    integer(kind=LONG)                                  :: last
    integer(kind=LONG)                                  :: step
    integer(kind=LONG)                                  :: nadc
!
!   Step 1 ADC output --> ADC input
    do nadc = 1, Adc_Convert_Nadc%Nadc
       if( Adc_Convert_Nadc%Adc_Convert(nadc)%N_Bit < 64 ) then
          if ( Adc_Convert_Nadc%Nadc == 1 ) then
             first = 1
             last  = Interf%N_Sample
             step  = 1
          else
             if ( nadc == 1 ) then  ! odd numbers
                first = 1
                last  = Interf%N_Sample
                step  = 2             
             end if
             if ( nadc == 2 ) then ! even numbers
                first = 2
                last  = Interf%N_Sample
                step  = 2             
             end if
          end if
          Offset = Adc_Convert_Nadc%Adc_Convert(nadc)%Cod_Min
          Gain   = ( Adc_Convert_Nadc%Adc_Convert(nadc)%Cod_Max  &
                   - Adc_Convert_Nadc%Adc_Convert(nadc)%Cod_Min )&
                 / ( Amplification%V_Max - Amplification%V_Min )
          Interf%Real_Part(first:last:step) =  Amplification%V_Min  &
               + ( Interf%Real_Part(first:last:step) - Offset -0.5 )&
               / Gain
       end if
    end do
!
!   Step 2 ADC input --> Amplificator output
       do Ns = 1, Interf%N_Sample
          N1 = 1
          N2 = Non_Linearity%N_U_Sample
          call dichotomd( Interf%Real_Part(Ns),            &
                          Non_Linearity%U_Table_Inv(N1:N2),&
                          N1, N2 )
          if( N1 == N2 ) then
             Interf_tmp = Non_Linearity%V_Table_Inv(N1)
          else
             Interf_tmp = Non_Linearity%V_Table_Inv(N1)    &
                        + ( Non_Linearity%V_Table_Inv(N2)  &
                          - Non_Linearity%V_Table_Inv(N1) )&
                        / ( Non_Linearity%U_Table_Inv(N2)  &
                          - Non_Linearity%U_Table_Inv(N1) )&
                        * ( Interf%Real_Part(Ns)           &
                          - Non_Linearity%U_Table_Inv(N1) )
          end if
          Interf%Real_Part(Ns) = Interf_tmp
       end do
!
!   Step 3 Amplificator output --> Amplificator input
    Offset = Amplification%V_Min
    Gain   = ( Amplification%V_Max - Amplification%V_Min )           &
           / ( Non_Linearity%Y_Max_Table - Non_Linearity%Y_Min_Table )
    
    Interf%Real_Part(1:Interf%N_Sample) = Non_Linearity%Y_Min_Table &
                 + ( Interf%Real_Part(1:Interf%N_Sample) - Offset ) &
                 / Gain
!
!   Step 4 Amplificator input --> Detector output
       do Ns = 1, Interf%N_Sample
          N1 = 1
          N2 = Non_Linearity%N_X_Sample
          call dichotomd( Interf%Real_Part(Ns),            &
                          Non_Linearity%X_Table_Inv(N1:N2),&
                          N1, N2 )
          if( N1 == N2 ) then
             Interf_tmp = Non_Linearity%Y_Table_Inv(N1)
          else
             Interf_tmp = Non_Linearity%Y_Table_Inv(N1)    &
                        + ( Non_Linearity%Y_Table_Inv(N2)  &
                          - Non_Linearity%Y_Table_Inv(N1) )&
                        / ( Non_Linearity%X_Table_Inv(N2)  &
                          - Non_Linearity%X_Table_Inv(N1) )&
                        * ( Interf%Real_Part(Ns)           &
                          - Non_Linearity%X_Table_Inv(N1)  )
          end if
          Interf%Real_Part(Ns) = Interf_tmp
       end do
!
!
    return
  end subroutine non_linearity_correction
!
!
end module non_linearity_correction_module
