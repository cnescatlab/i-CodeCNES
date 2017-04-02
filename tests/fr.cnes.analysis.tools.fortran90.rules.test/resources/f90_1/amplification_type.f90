!!* amplification_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: november 2010
!!*           Version: $Revision: 1.2 $
!!* Last modification: $Date: 2011-03-07 15:54:59 $
!!
!> amplification_type -- Module
!!
!! * Purpose
!!
!!   Module for type amplification parameters declaration and allocation.
!!
!! * Sub-routines and functions
!!
!!     * type_Amplification : type for declaration and allocation of amplification parameters 
!!
!! * References
!!

module amplification_type
   use precision_type
   use error_type
!
   implicit none
!
!
   public :: type_Amplification
!
   type :: type_Amplification
     character(len=500)  :: filename !< amplification characterisation file name
     real(kind=DOUBLE)   :: V_Min !< minimum value of the amplified signal, V_Min (Volts)
     real(kind=DOUBLE)   :: V_Max !< maximum value of the amplified signal, V_Max (Volts)
     real(kind=DOUBLE)   :: Gain_a0 !< not used
     real(kind=DOUBLE)   :: Gain_b0 !< not used
   end type type_Amplification
!
!
end module amplification_type
