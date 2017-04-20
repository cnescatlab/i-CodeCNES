!!* chrom_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: october 2009
!!*           Version: $Revision: 1.3 $
!!* Last modification: $Date: 2011-03-07 15:54:59 $
!!
!> type_chrom -- Module
!!
!! * Purpose
!!
!!      Module for type chromatism declaration and allocation.
!!
!! * Description
!!      
!!      This module defines the chromatism type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Chrom : type for declaration and allocation of chromatism computation
!!
!! * References
!!
module chrom_type
   use precision_type
   use error_type
   use modinouttools
!
   implicit none
!
!
   public :: type_Chrom   


   type :: type_Chrom
     character(len=500)                               :: filename !<  chromatism characterisation file name     
     real(kind=DOUBLE)                                :: Wn0 !< reference frequency (alignment laser) 
     character(len=4)                                 :: Material !< plate material for refractive index computation 
     real(kind=DOUBLE)                                :: Alphas !< rotation angle around the Z axis for mobil corner cube 
     real(kind=DOUBLE)                                :: Es !< thickness of the plate computed for mobil corner cube 
     real(kind=DOUBLE)                                :: Alphac !< rotation angle around the Z axis for fixed corner cube 
     real(kind=DOUBLE)                                :: Ec !< thickness of the plate computed for fixed corner cube 
     real(kind=DOUBLE) ,dimension(:,:,:) ,allocatable :: ABOpt !< Optical Aberation (Y,Z,X) matrix 
  end type type_Chrom
!
!
end module chrom_type
