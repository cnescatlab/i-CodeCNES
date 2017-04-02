!!* srf_param_type.f90 --
!!*
!!*           Project: SPS_GENERIC
!!*           Authors: NOVELTIS/B.TOURNIER
!!*              Date: october 2009
!!*           Version: $Revision: 1.8 $
!!* Last modification: $Date: 2011-11-02 14:57:54 $
!!
!> srf_param_type -- Module
!!
!! * Purpose
!!
!!   Module for srf (Spectral Response Function) parameters type declaration and allocation.
!!
!! * Description
!!      
!!     This module defines the srf parameters type and allows its allocation and declaration. 
!!
!! * Sub-routines and functions
!!
!!     * type_Srf_Param   : type for declaration and allocation of Srf parameters
!!     * alloc_Srf_Param  : type_Srf_Param allocation
!!     * dalloc_Srf_Param : type_Srf_Param deallocation
!!
!! * References
!!


module srf_param_type
   use precision_type
   use error_type
   use constantes_type
   use modinouttools
!
   implicit none
!
!
   public :: type_Srf_Param    &
            ,alloc_Srf_Param   &
            ,dalloc_Srf_Param  
!
   type :: type_Srf_Param
     character(len=500)                               :: filename !< srf parameters characterisation file name
     character(len=7)                                 :: L1c_Option !< L1c_Option for convolution (GAUSS/COSCAR/TRIANGL)
     character(len=7)                                 :: Apod_Option !< Apod_Option Yes No)
     character(len=12)                                :: Scan_Mode  !< Single/Double Sided
     real(kind=DOUBLE)                                :: Ratio_bilatere !< Ratio Bilatere
     character(len=10)                                :: Filter !< filter type
     character(len=4)                                 :: Mode_Agreg !< Aggregation mode (SOFT/HARD/DYN1/DYN2/THEO)
     integer(kind=LONG)                               :: Calib_Option !< Calibration_Option (0=shift x=Fit length of the phase slope)
     real(kind=DOUBLE)                                :: MagnificationY !< magnification along Y axis 
     real(kind=DOUBLE)                                :: MagnificationZ !< magnification along Z axis 
     integer(kind=LONG)                               :: Nband !< spectral bands number  
     integer(kind=LONG),dimension(:)    ,allocatable  :: SB_List !< list of spectral bands - SB_List(1:Nband)
     integer(kind=LONG)                               :: NsOpd !< OPD number 
     real(kind=DOUBLE)                                :: OpdMax !< OPD maximum (m)
     real(kind=DOUBLE)                                :: OpdFraction !< OpdFraction for COSCAR Option 
     real(kind=DOUBLE)                                :: FWHM !< FWHM Gaussian L1C Resolution (m) for GAUSS Option 
     integer(kind=LONG)                               :: L1aNsFft !< samples number for Fourier transform for level 1a
     integer(kind=LONG)                               :: L1bNsFft !< samples number for Fourier transform for level 1b 
     integer(kind=LONG)                               :: L1cNsFft !< samples number for Fourier transform for level 1c 
     integer(kind=LONG)                               :: SigI !< samples number of the smoothing function
     integer(kind=LONG)                               :: NsSDWn1a !< central wavenumber number for spectral domain level 1a  
     integer(kind=LONG)                               :: NsSDWn1b !< central wavenumber number for spectral domain level 1b  
     integer(kind=LONG)                               :: NsSDWn1c !< central wavenumber number for spectral domain level 1c  
     real(kind=DOUBLE)                                :: SDWnMax1a !< central wavenumber for spectral domain level 1a (m-1)  
     real(kind=DOUBLE)                                :: SDWnMax1b !< central wavenumber for spectral domain level 1b (m-1)  
     real(kind=DOUBLE)                                :: SDWnMax1c !< central wavenumber for spectral domain level 1c (m-1)  
     real(kind=DOUBLE)                                :: H_Sat !< satellite altitude (Km)
     real(kind=DOUBLE)                                :: FieldMeanAngle !< (rd)
     integer(kind=LONG)                               :: Rpd_NsPsf !< PSF number defined by RPD laser
     integer(kind=LONG)                               :: Mes_NsPsf !< PSF number defined by measurement 
     integer(kind=LONG)                               :: Ref_NsPixel !< Reference pixels number 
     integer(kind=LONG)                               :: Rpd_NsPixel !< RPD laser pixels number 
     integer(kind=LONG)                               :: Mes_NsPixel !< measurement pixels number
     integer(kind=LONG)                               :: PN_Ref !< reference pixel 
     integer(kind=LONG)                               :: PN_Rpd !< reference pixel for Rpd measurement 
     integer(kind=LONG)                               :: PN_Mes !< pixel for measurement 
     integer(kind=LONG)                               :: PN_Cen_Rpd !< central pixel for Rpd measurement    
     integer(kind=LONG)                               :: PN_Cen_Mes !< central pixel for measurement    
     real(kind=DOUBLE)                                :: AxeY !< interferometric axis shift along Y
     real(kind=DOUBLE)                                :: AxeZ !< interferometric axis shift along Z 
     real(kind=DOUBLE)                                :: Rpd_Wn0_Offset !< Offset of the RPD laser wavenumber (m-1)
     real(kind=DOUBLE)                                :: Rpd_Wn0_Jitter !< Jitter of the RPD laser wavenumber (m-1) 
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: Rpd_Psf_OffsetY !< Offset along Y axis of the PSF defined by RPD laser - Rpd_Psf_OffsetY(1:Rpd_NsPsf)
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: Rpd_Psf_OffsetZ !< Offset along Z axis of the PSF defined by RPD laser - Rpd_Psf_OffsetZ(1:Rpd_NsPsf)
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: Mes_Psf_OffsetY !< Offset along Y axis of the PSF defined by measurement - Mes_Psf_OffsetY(1:Mes_NsPsf)
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: Mes_Psf_OffsetZ !< Offset along Z axis of the PSF defined by measurement - Mes_Psf_OffsetZ(1:Mes_NsPsf) 
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: Ref_PosY !< Ref_PosY(1:Ref_NsPixel) 
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: Ref_PosZ !< Ref_PosZ(1:Ref_NsPixel) 
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: Rpd_PosY !< Rpd_PosY(1:Rpd_NsPixel) 
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: Rpd_PosZ !< Rpd_PosZ(1:Rpd_NsPixel) 
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: Mes_PosY !< Mes_PosY(1:Mes_NsPixel) 
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: Mes_PosZ !< Mes_PosZ(1:Mes_NsPixel) 
     integer(kind=LONG),dimension(:)    ,allocatable  :: NsWn0 !< central wavenumber number - NsWn0(1:Nband),1:Nband)
     real(kind=DOUBLE) ,dimension(:,:)  ,allocatable  :: Wn0 !< wavenumber base (m-1) - Wn0(1:maxval(NsWn0(1:Nband),1:Nband)
     real(kind=DOUBLE) ,dimension(:)    ,allocatable  :: Rpd_Wn0_Jitter_vect !<  Rpd_Wn0_Jitter_vect(1:NsOpd)
   end type type_Srf_Param
!
   contains
!
!
   subroutine alloc_Srf_Param( Srf_Param )
   implicit none
     type(type_Srf_Param), intent(inout)      :: Srf_Param
     integer(kind=LONG)                       :: ErrCode
     integer(kind=LONG)                       :: NsWn0_Max
!
     allocate( Srf_Param%SB_List(Srf_Param%Nband),             stat=ErrCode )
     allocate( Srf_Param%Rpd_Psf_OffsetY(Srf_Param%Rpd_NsPsf), stat=ErrCode )
     allocate( Srf_Param%Rpd_Psf_OffsetZ(Srf_Param%Rpd_NsPsf), stat=ErrCode )
     allocate( Srf_Param%Mes_Psf_OffsetY(Srf_Param%Mes_NsPsf), stat=ErrCode )
     allocate( Srf_Param%Mes_Psf_OffsetZ(Srf_Param%Mes_NsPsf), stat=ErrCode )
     allocate( Srf_Param%NsWn0(Srf_Param%Nband),               stat=ErrCode )
     NsWn0_Max = maxval(Srf_Param%NsWn0(1:Srf_Param%Nband))
     allocate( Srf_Param%Wn0(NsWn0_Max,Srf_Param%Nband),       stat=ErrCode )
     allocate( Srf_Param%Ref_PosY(Srf_Param%Ref_NsPixel),      stat=ErrCode )
     allocate( Srf_Param%Ref_PosZ(Srf_Param%Ref_NsPixel),      stat=ErrCode )
     allocate( Srf_Param%Rpd_PosY(Srf_Param%Rpd_NsPixel),      stat=ErrCode )
     allocate( Srf_Param%Rpd_PosZ(Srf_Param%Rpd_NsPixel),      stat=ErrCode )
     allocate( Srf_Param%Mes_PosY(Srf_Param%Mes_NsPixel),      stat=ErrCode )
     allocate( Srf_Param%Mes_PosZ(Srf_Param%Mes_NsPixel),      stat=ErrCode )
     allocate( Srf_Param%Rpd_Wn0_Jitter_vect(Srf_Param%NsOpd), stat=ErrCode )
!
     if (ErrCode > 0) then
        write(0,*) 'allocation Srf_Param Error'
        write(0,*) 'Srf_Param: fatal error'
        call Err_outputErrorMessage(0)
        call exit(1)
     end if
     return
   end subroutine alloc_Srf_Param
!
!
   subroutine dalloc_Srf_Param( Srf_Param )
   implicit none
     type(type_Srf_Param), intent(inout)         :: Srf_Param
!
     deallocate( Srf_Param%SB_List )
     deallocate( Srf_Param%Rpd_Psf_OffsetY )
     deallocate( Srf_Param%Rpd_Psf_OffsetZ )
     deallocate( Srf_Param%Mes_Psf_OffsetY )
     deallocate( Srf_Param%Mes_Psf_OffsetZ )
     deallocate( Srf_Param%NsWn0 )
     deallocate( Srf_Param%Wn0 )
     deallocate( Srf_Param%Ref_PosY )
     deallocate( Srf_Param%Ref_PosZ )
     deallocate( Srf_Param%Rpd_PosY )
     deallocate( Srf_Param%Rpd_PosZ )
     deallocate( Srf_Param%Mes_PosY )
     deallocate( Srf_Param%Mes_PosZ )
     return
   end subroutine dalloc_Srf_Param
!
!
end module srf_param_type
