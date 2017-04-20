!!#  adc_convert_module.f90 -- 
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: november 2010
!!#            Version: $Revision: 1.5 $
!!#  Last modification: $Date: 2011-04-12 08:32:31 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!> adc_convert_module -- Module
!!
!! * Purpose
!!
!!   Module for interferogram adc conversion
!!
!! * Description
!!      
!!   This module simulates algorithms for an Analog to Digital Converter (ADC) system 
!!   which converts a continuous quantity to a discrete digital number. 
!!   An ADC is an electronic device that converts an input analog voltage to an output 
!!   digital number proportional to the amplitude of the voltage. The resolution of the 
!!   ADC is the number of discrete values it can produce over the range of analog values.
!!   The values are stored in binary form, so the resolution is expressed in bits. So 
!!   the number of discrete values available is usually a power of two.
!!
!! * Sub-routines and functions
!!
!! -  adc_convert_init   : initialisation of the type defining the ADC parameters 
!! -  interf_adc_convert : conversion of a continuous interferogram to a discrete digital 
!!                         number
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.4.2.9
!!

module adc_convert_module
   use precision_type
   use error_type
   use constantes_type
   use amplification_type
   use adc_convert_type
   use interferogram_type
!
   implicit none
!
!
   public :: adc_convert_init, &
             interf_adc_convert
!
!
   contains
!
!

!> adc_convert_init -- Public
!!
!! * Purpose
!!
!!     Initialisation of the type defining the ADC parameters 
!!
!! * Description
!!
!!     The adc_convert_init subroutine allows to update the type defining the ADC  
!!     parameters thanks to the read of the File_Param file; 
!!
!! * Inputs
!!
!!     - File_Param : character / input file with definition of ADC parameters
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Adc_Convert : type_Adc_Convert / type for declaration and allocation of adc 
!!                     conversion parameters 
!!
!! * References
!!

   subroutine adc_convert_init( File_Param, &
                                Adc_Convert )
   implicit none
     character(len=*)        ,intent(in)            :: File_Param
     type(type_Adc_Convert)  ,intent(out)           :: Adc_Convert
!
     integer(kind=LONG)                             :: iFile
     integer(kind=LONG)                             :: iPos
!
     iFile = 10
!
     iPos = 1
     write(*,'(a)') File_Param(1:len_trim(File_Param))
     open(unit=iFile, file=File_Param, status='old',err=999)
     !Adc_Convert%filename = File_Param
     iPos = 2
     read(iFile,*,err=999) Adc_Convert%N_Bit
     read(iFile,*,err=999) Adc_Convert%Gain_a0
     read(iFile,*,err=999) Adc_Convert%Gain_b0
     Adc_Convert%Cod_Min = -(2**(Adc_Convert%N_Bit-1))
     Adc_Convert%Cod_Max = +(2**(Adc_Convert%N_Bit-1)) - 1
     close(unit=iFile)
     write(*,*) 'Adc_Convert%N_Bit  ',Adc_Convert%N_Bit
     write(*,*) 'Adc_Convert%Cod_Min',Adc_Convert%Cod_Min
     write(*,*) 'Adc_Convert%Cod_Max',Adc_Convert%Cod_Max
     write(*,*) 'Adc_Convert%Gain_a0',Adc_Convert%Gain_a0
     write(*,*) 'Adc_Convert%Gain_b0',Adc_Convert%Gain_b0
!
     return
 999 write(*,*) 'adc_convert_init Fatal Error',iPos
     call exit(1)
   end subroutine adc_convert_init
!
!

!> interf_adc_convert -- Public
!!
!! * Purpose
!!
!!     Conversion of a continuous interferogram to a discrete digital number.
!!
!! * Description
!!
!!     The interf_adc_convert subroutine allows to convert the continuaous interferogram 
!!     to a discrete digital number. This conversion requires the following steps:  
!!       - after required table allocation, a temporal drift of the input signal is 
!!         simulated as a linear relation
!!       - Then the adc conversion algorithm converts the interferogram signal which 
!!         is between Vmin and Vmax into a coded interferogram for a number of bits 
!!         of coding <64bits.
!!
!! * Inputs
!!
!!     - Amplification : type_Amplification / type for amplification parameters 
!!                       declaration and allocation.
!!     - Adc_Convert   : type_Adc_Convert / type for declaration and allocation of adc 
!!                       conversion parameters 
!!     _ Nadc          : Number of ADC to convert the signal (case 1 or 2 taken into account)
!!
!! * Inputs/outputs
!!
!!     - Interf : type_Interferogram / type for declaration and allocation of interferogram
!!
!! * Outputs
!!
!! * References
!!

   subroutine interf_adc_convert( Amplification,   &
                                  Adc_Convert_Nadc,&
                                  Interf        )
   implicit none
     type(type_Adc_Convert_Nadc)  ,intent(in)            :: Adc_Convert_Nadc
     type(type_Amplification),intent(in)                 :: Amplification
     type(type_Interferogram),intent(inout)              :: Interf
!
     real(kind=DOUBLE)                                   :: Gain
     integer(kind=DOUBLE), dimension(:),allocatable      :: tmp_code
     integer(kind=LONG)                                  :: ErrCode
     integer(kind=LONG)                                  :: first
     integer(kind=LONG)                                  :: last
     integer(kind=LONG)                                  :: step
     integer(kind=LONG)                                  :: nadc
     
!
     allocate( tmp_code(Interf%N_Sample) , stat=ErrCode )
     if (ErrCode > 0) then
        write(0,*) 'allocation  Error'
        write(0,*) 'interf_adc_convert: fatal error'
        call exit(1)
     end if
!
!    input temporal linear derive
     do nadc = 1, Adc_Convert_Nadc%Nadc
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
        Interf%Real_Part(first:last:step) =              &
            Interf%Real_Part(first:last:step)            &
          + ( Adc_Convert_Nadc%Adc_Convert(nadc)%Gain_a0 &
            * Interf%Time(first:last:step) )             &
          + Adc_Convert_Nadc%Adc_Convert(nadc)%Gain_b0
        write(*,*) 'Adc_Convert%Gain_a0 ',&
                    Adc_Convert_Nadc%Adc_Convert(nadc)%Gain_a0
        write(*,*) 'Adc_Convert%Gain_b0 ',&
                    Adc_Convert_Nadc%Adc_Convert(nadc)%Gain_b0
     end do
!
!    adc conversion
     do nadc = 1, Adc_Convert_Nadc%Nadc
        if( Adc_Convert_Nadc%Adc_Convert(nadc)%N_Bit < 64 ) then
           write(*,*) ' ADC conversion'
           Gain = ( Adc_Convert_Nadc%Adc_Convert(nadc)%Cod_Max - &
                    Adc_Convert_Nadc%Adc_Convert(nadc)%Cod_Min ) &
                / ( Amplification%V_Max - Amplification%V_Min )
           write(*,*) 'ADC Gain :',Gain
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
           tmp_code(first:last:step) = ( Gain                      &
                * ( Interf%Real_Part(first:last:step)              &
                  - Amplification%V_Min ) )                        &
                + Adc_Convert_Nadc%Adc_Convert(nadc)%Cod_Min + 0.5
           Interf%Real_Part(first:last:step) =              &
                            dble( tmp_code(first:last:step) )
        else
           write(*,*) ' ADC No conversion'
        end if
     end do
     write(*,*) 'interf_adc_convert Min',&
                 minval(Interf%Real_Part(1:Interf%N_Sample))
     write(*,*) 'interf_adc_convert Max',&
                 maxval(Interf%Real_Part(1:Interf%N_Sample))
!
     deallocate( tmp_code , stat=ErrCode )
!
     if (ErrCode > 0) then
        write(0,*) 'deallocation  Error'
        write(0,*) 'interf_adc_convert: fatal error'
        call exit(1)
     end if
!
     return
   end subroutine interf_adc_convert
!
!
end module adc_convert_module
