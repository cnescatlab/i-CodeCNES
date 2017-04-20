!!* adc_convert_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: november 2010
!!*           Version: $Revision: 1.3 $
!!* Last modification: $Date: 2011-04-12 08:32:32 $
!!
!> type_adc_convert -- Module
!!
!! * Purpose
!!
!!   Module for type adc conversion parameters declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the adc conversion parameters type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Adc_Convert      : type for declaration of adc conversion parameters 
!!     * type_Adc_Convert_Nadc : type for declaration of N ADC conversion types
!!     * type_Adc_Convert_SB   : type for declaration of ADC conversion types per band
!!     * alloc_Adc_Convert_Nadc: allocation of N ADC conversion types
!!     * alloc_Adc_Convert_SB  : allocation of ADC conversion types per band
!!
!! * References
!!     SPS ATBD

module adc_convert_type
   use precision_type
   use error_type
!
   implicit none
!
!
   public :: type_Adc_Convert ,  &
        type_Adc_Convert_Nadc ,  &
        type_Adc_Convert_SB   ,  &
        alloc_Adc_Convert_Nadc,  &
        alloc_Adc_Convert_SB  ,  &
        dalloc_Adc_Convert_Nadc, &
        dalloc_Adc_Convert_SB
!
   type :: type_Adc_Convert
     character(len=500)   :: filename !< Adc_Convert characterisation file name  
     integer(kind=LONG)   :: N_Bit !< number of bits of coding
     real(kind=DOUBLE)    :: Cod_Min !< minimum value encodded
     real(kind=DOUBLE)    :: Cod_Max !< maximum value encodded
     real(kind=DOUBLE)    :: Gain_a0 !< first coefficient for the computation of the temporal drift of the input signal
     real(kind=DOUBLE)    :: Gain_b0 !< second coefficient for the computation of the temporal drift of the input signal
   end type type_Adc_Convert
!
!
   type :: type_Adc_Convert_Nadc
     integer(kind=LONG)                              :: Nadc          !< Number of ADC
     type(type_Adc_Convert),dimension(:),allocatable :: Adc_Convert   !< Adc_Convert type - Adc_Convert(Nadc)
  end type type_Adc_Convert_Nadc
!
!
   type :: type_Adc_Convert_SB
     integer(kind=LONG)                                   :: Nband       !< Number of spectral band
     type(type_Adc_Convert_Nadc),dimension(:),allocatable :: Adc_Convert_Nadc !< Adc_Convert_Nadc type - Adc_Convert_Nadc(Nband)
  end type type_Adc_Convert_SB
!
!
   contains
!
!
   subroutine alloc_Adc_Convert_Nadc( Adc_Convert_Nadc )
   implicit none
     type(type_Adc_Convert_Nadc), intent(inout)   :: Adc_Convert_Nadc
     integer(kind=LONG)                           :: ErrCode
!
     allocate( Adc_Convert_Nadc%Adc_Convert(Adc_Convert_Nadc%Nadc), &
          stat=ErrCode )
     if( ErrCode > 0 ) then
        write(*,*) 'allocation Adc_Convert_Nadc Error', ErrCode
        write(*,*) 'type_Adc_Convert_Nadc: fatal error'
        call exit(1)
     end if
     return
   end subroutine alloc_Adc_Convert_Nadc
!
!
   subroutine alloc_Adc_Convert_SB( Adc_Convert_SB )
   implicit none
     type(type_Adc_Convert_SB), intent(inout)     :: Adc_Convert_SB
     integer(kind=LONG)                           :: ErrCode
!
     allocate( Adc_Convert_SB%Adc_Convert_Nadc(Adc_Convert_SB%Nband), &
          stat=ErrCode )
     if( ErrCode > 0 ) then
        write(*,*) 'allocation Adc_Convert_Nadc Error', ErrCode
        write(*,*) 'type_Adc_Convert_Nadc: fatal error'
        call exit(1)
     end if
     return
   end subroutine alloc_Adc_Convert_SB
!
!
   subroutine dalloc_Adc_Convert_Nadc( Adc_Convert_Nadc )
   implicit none
     type(type_Adc_Convert_Nadc), intent(inout) :: Adc_Convert_Nadc
!
     deallocate(Adc_Convert_Nadc%Adc_Convert)
     return
   end subroutine dalloc_Adc_Convert_Nadc
!
!
   subroutine dalloc_Adc_Convert_SB( Adc_Convert_SB )
     implicit none
     type(type_Adc_Convert_SB), intent(inout) :: Adc_Convert_SB
!
     deallocate(Adc_Convert_SB%Adc_Convert_Nadc)
     return
   end subroutine dalloc_Adc_Convert_SB
!

!
 end module adc_convert_type
