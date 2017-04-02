!!# isrf_model_computation_module.f90 --
!!#
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#              Date: october 2009 
!!#           Version: $Revision: 1.13 $
!!# 
!!#
!!# Language:  F90
!!# Standards: Noveltis
!!# Last modification: $Date: 2012-02-08 10:19:17 $
!!#
!!# --
!!#
!! 
!> isrf_model_computation_module -- Module
!!
!! * Purpose
!!
!!     Module for isrf computation.
!!
!! * Description
!!      
!!     This module defines the analytic model to calculate the Instrument 
!!     Response Function for levels 1a, 1b and 1c.  
!!
!! * Sub-routines and functions
!!
!!     - isrf_model_sub  : Computation of the spectral response function 
!!                         for level 1a, 1b and 1c
!!     - isrf_model_init : Initialisation of all required parameters before            
!!                         computation of the spectral response function 
!!
!! * References
!!
!!     
!!


module isrf_model_computation_module
   use precision_type
   use error_type
   use constantes_type
   use srf_param_type
   use psf_type
   use ccm_type
   use chrom_type
   use fcomp_type
   use saf_type
   use srf_type
   use apf_type
   use gain_type
   use refrac_index_type
   use pupil_type
   use mertz_type
   use math_module
   use statistique_module
   use psf_computation_module
   use saf_computation_module
   use srf_computation_module
   use ccm_computation_module
   use chromatism_computation_module
   use fcompens_computation_module
   use refractive_index_module
   use pupil_computation_module
   use mertz_computation_module

!
   implicit none
!
!
   public ::                        &
             isrf_model_sub,        &
             isrf_model_init     
!
   contains
!
!
!!
!!
!> isrf_model_sub -- Public
!!
!! * Purpose
!!
!!   Computation of the spectral response function for level 1a, 1b and 1c
!!
!! * Description
!!
!!   This subroutine defines the isrf implemented in the functional model. This 
!!   computation requires the following steps:
!!      - first of all, initialisation of the tables of saf, srf, and
!!        apf and the initialisation of the corner cube motion law.
!!      - then, magnification application on Rpd
!!      - update of the interferometric axis
!!      - Psf computation for RPD position 
!!      - magnification application on Measurement Pixel
!!      - computation (for Path='Rpd') of the Rpd Self Apodisation Function
!!      - for Path = 'MES', computation of the srf level 1a and level 1b ; 
!!        taking into account a loop over spectral bands and one over wave-numbers:
!!             - spectral interpolation of the Psf
!!             - update of the interferometric axis
!!             - computation of subpixel psf at central pixel position
!!             - then, thanks to a loop over pixels:
!!                   - computation of subpixel Psf at actual subpixel position
!!                   - computation of measurement subpixel Self Apodisation Function
!!                   - use of Rpd Sampling
!!                   - then Spectral response function 1a and 1b computation
!!          - tables deallocation
!!          - Pixel aggregation for instrument function level 1a and level 1b
!!      - complex argument regularisation (monotone)
!!      - computation of the Spectral response function 1c, in function of the case 
!!        ('GAUSS', 'COSCAR' or 'TRIANGL')
!!      - and before computing the Apodisation Function, tables deallocation
!!
!! * Inputs 
!!
!!     - Srf_Param  :  type_Srf_Param / type for declaration and allocation of the srf 
!!                     parameters
!!     - Psf_Ref    :  type_Psf / type for declaration and allocation of psf
!!     - Psf_Rpd_In :  type_Psf / type for declaration and allocation of psf
!!     - Psf_In     :  type_Psf / type for declaration and allocation of psf
!!     - Gain       :  type_Gain / type for declaration and allocation of detectors gain
!!     - Pupil      :  type_Pupil / type for declaration and allocation of pupil
!!
!! * Inputs/outputs
!!
!!     - RefracIndex_Mes : type_RefracIndex / type for declaration and allocation of 
!!                         refractive index of materials
!!     - RefracIndex_Rpd : type_RefracIndex / type for declaration and allocation of 
!!                         refractive index of materials
!!     - Prism_Motion :  type_Ccm / type for declaration and allocation of cube corner motion 
!!     - Fcomp   :  type_Fcomp / type for declaration and allocation of field compensation
!!     - Chrom   :  type_Chrom / type for declaration and allocation of chromatism 
!!     - Chrom_Rpd :  type_Chrom / type for declaration and allocation of chromatism 
!!     - Ccm     :  type_Ccm / type for declaration and allocation of cube corner motion 
!!     - Ccm_Rpd :  type_Ccm / type for declaration and allocation of cube corner motion
!!     - Saf_Rpd :  type_Saf / type for declaration and allocation of saf  
!!     - Saf     :  type_Saf / type for declaration and allocation of saf  
!!     - Sas     :  type_Saf / type for declaration and allocation of saf
!!     - Srf     :  type_Srf / type for declaration and allocation of srf  
!!     - Apf     :  type_Apf / type for declaration and allocation of apodisation function
!!
!! * Outputs
!!
!! * References
!!

   subroutine isrf_model_sub( Srf_Param, Psf_Ref,              &
                              Psf_Rpd_In, Psf_Mes_In,          &
                              Gain, Chrom_Rpd, Chrom,          &
                              Pupil, Prism_Motion, Mertz,      &
                              RefracIndex_Rpd, RefracIndex_Mes,&
                              Fcomp, Ccm_Rpd, Ccm, Saf_Rpd,    &
                              Saf, Sas, Srf, Apf    )

   implicit none
!
!  inputs
   type(type_Srf_Param), intent(in)                    :: Srf_Param
   type(type_Psf)      , intent(in)                    :: Psf_Ref
   type(type_Psf)      , intent(in)                    :: Psf_Rpd_In
   type(type_Psf)      , intent(inout)                 :: Psf_Mes_In
   type(type_Gain)     , intent(in),dimension(Srf_Param%Nband):: Gain
   type(type_Pupil)    , intent(in)                    :: Pupil
!
!  inputs/outputs
   type(type_RefracIndex),intent(inout)                :: RefracIndex_Mes
   type(type_RefracIndex),intent(inout)                :: RefracIndex_Rpd
   type(type_Ccm)      , intent(inout)                 :: Prism_Motion
   type(type_Mertz)    , intent(inout)                 :: Mertz
   type(type_Fcomp)    , intent(inout)                 :: Fcomp
   type(type_Chrom)    , intent(inout)                 :: Chrom
   type(type_Chrom)    , intent(inout)                 :: Chrom_Rpd
   type(type_Ccm)      , intent(inout)                 :: Ccm
   type(type_Ccm)      , intent(inout)                 :: Ccm_Rpd
   type(type_Saf)      , intent(inout)                 :: Saf_Rpd
   type(type_Saf)      , intent(inout),dimension(Srf_Param%Nband):: Saf
   type(type_Saf)      , intent(inout),dimension(Srf_Param%Nband):: Sas
   type(type_Srf)      , intent(inout),dimension(Srf_Param%Nband):: Srf
   type(type_Apf)      , intent(inout),dimension(Srf_Param%Nband):: Apf
!
!  local
   type(type_Psf)                                  :: Psf_Rpd_Pixel
   type(type_Psf)                                  :: Psf_Rpd_Pixel_Mag
   type(type_Psf)                                  :: Psf_Rpd_Pixel_Mag_Psp
   type(type_Psf)                                  :: Psf_Mes_Interp
   type(type_Psf)                                  :: Psf_Mes_Interp_SubPixel
   type(type_Psf)                                  :: Psf_Mes_Interp_SubPixel_Mag
   type(type_Psf)                                  :: Psf_Mes_Interp_SubPixel_Mag_Psp
   type(type_Psf)      , allocatable ,dimension(:) :: Psf_Mes_Interp_SubPixel_Mag_Psp_Gain
   character(len=3)                                :: Path
   integer(kind=LONG)                              :: SB
   integer(kind=LONG)                              :: PN
   integer(kind=LONG)                              :: SUB_PN
   integer(kind=LONG)                              :: NF
!
!    Corner cube motion law initialisation 
     call ccm_define( Ccm )
     call ccm_define( Ccm_Rpd )
!
     Path = 'RPD'
     SB   = 0
     call saf_init( Srf_Param, Ccm_Rpd, SB, Path, Psf_Rpd_In, Saf_Rpd )
     Path = 'MES'
     do SB = 1, Srf_Param%Nband ! loop over spectral bands
        call saf_init( Srf_Param, Ccm, SB, Path, Psf_Mes_In, Saf(SB) )
        call saf_init( Srf_Param, Ccm, SB, Path, Psf_Mes_In, Sas(SB) )
        call srf_init( Srf_Param, SB, Srf(SB) )
        call apf_init( Srf_Param, SB, Apf(SB) )
!
!      Spectral response function 1c
       select case (Srf_Param%L1c_Option(1:len_trim(Srf_Param%L1c_Option)))
       case ('GAUSS')
         call srf_L1c_Gauss( Srf_Param, Sas(SB), Srf(SB) )
       case ('COSCAR')
         call srf_L1c_Coscar( Srf_Param, Sas(SB), Srf(SB) )
       case ('TRIANGL')
         call srf_L1c_Triangl( Srf_Param, Sas(SB), Srf(SB) )
       case default
         write(0,*) 'Srf_Param%L1c_Option Error',Srf_Param%L1c_Option
         go to 999
       end select
     end do  ! end of the loop over spectral bands
!
!    Psf RPD position
     call psf_pixel( Srf_Param%Rpd_PosY(Srf_Param%PN_Rpd), &
                     Srf_Param%Rpd_PosZ(Srf_Param%PN_Rpd), &
                     Psf_Rpd_In,                      &
                     Psf_Rpd_Pixel )
!
!    Magnification application on Rpd
     call psf_magnif( Srf_Param%MagnificationY, &
                      Srf_Param%MagnificationZ, &
                      Psf_Rpd_Pixel,            &
                      Psf_Rpd_Pixel_Mag )
!
!    update of interferometric axis
     call psf_psp( Srf_Param%AxeY,       &
                   Srf_Param%AxeZ,       &
                   Psf_Rpd_Pixel_Mag,    &
                   Psf_Rpd_Pixel_Mag_Psp )
     if ( Srf_Param%Mode_Agreg == "MER1" .or. &
          Srf_Param%Mode_Agreg == "MER2" ) then
        ! Prism motion Initialisation
        call mertz_define( Prism_Motion )  
     end if
!
!    Rpd Self Apodisation Function
     NF = 1
     PN = 1
     Fcomp%PN_Cen = Srf_Param%PN_Cen_Rpd
     Path = 'RPD'
     select case ( Srf_Param%Mode_Agreg(1:len_trim(Srf_Param%Mode_Agreg)) )
     case ( "MER1" )
        call saf_time_mertz1( Srf_Param, Path, Psf_Ref,      &
                              Psf_Rpd_Pixel_Mag_Psp,         &
                              Ccm_Rpd, Chrom_Rpd, Pupil,     &
                              RefracIndex_Rpd, Fcomp, NF, PN,&
                              Saf_Rpd      )
     case ( "MER2" )
        call mertz_calc( Prism_Motion, Fcomp, RefracIndex_Rpd,         &
                         Psf_Rpd_Pixel_Mag_Psp, Saf_Rpd%Wn0(NF), Mertz )
        call saf_time_mertz2( Srf_Param, Path, Psf_Ref,                &
                              Psf_Rpd_Pixel_Mag_Psp, Ccm_Rpd,          &
                              Chrom_Rpd, Pupil, RefracIndex_Rpd, Fcomp,&
                              Mertz, NF, PN, Saf_Rpd )
     case default
        call saf_time( Srf_Param, Path, Psf_Ref, Psf_Rpd_Pixel_Mag_Psp, &
                       Ccm_Rpd, Chrom, RefracIndex_Rpd, Fcomp, NF, PN,  &
                       Srf_Param%PN_Rpd, Saf_Rpd                        )
     end select
!
     Path = 'MES'
     do SB = 1, Srf_Param%Nband ! loop over spectral bands
       do NF = 1, Saf(SB)%NsWn0 ! loop over wavenumbers
!
!        Psf spectral interpolation
         call psf_interpol( NF, Saf(SB), Psf_Mes_In, Psf_Mes_Interp )
!
!
         allocate( Psf_Mes_Interp_SubPixel_Mag_Psp_Gain( Saf(SB)%NsPixel ) )
         do SUB_PN = 1, Saf(SB)%NsPixel ! loop over sub pixels
           write(0,'(a10,3i5,f10.2)') 'SB,NF,SUB_PN',             &
                                       SB,NF,SUB_PN,Saf(SB)%Wn0(NF)
!
!          subpixel psf at actual subpixel position
           call psf_pixel( Srf_Param%Mes_PosY(Srf_Param%PN_Mes)+ &
                           Srf_Param%Mes_Psf_OffsetY(SUB_PN),    &
                           Srf_Param%Mes_PosZ(Srf_Param%PN_Mes)+ &
                           Srf_Param%Mes_Psf_OffsetZ(SUB_PN),    &
                           Psf_Mes_Interp,                       &
                           Psf_Mes_Interp_SubPixel               )
!
!          Magnification application on Measurement Pixel
           call psf_magnif( Srf_Param%MagnificationY,   & 
                            Srf_Param%MagnificationZ,   &
                            Psf_Mes_Interp_SubPixel,    &
                            Psf_Mes_Interp_SubPixel_Mag )
!
!          update of interferometric axis
           call psf_psp( Srf_Param%AxeY,                 &
                         Srf_Param%AxeZ,                 &
                         Psf_Mes_Interp_SubPixel_Mag,    &
                         Psf_Mes_Interp_SubPixel_Mag_Psp )
!
!          subpixel psf Gain application
           call psf_gain( Gain(SB),                            &
                          SUB_PN,                              &
                          Psf_Mes_Interp_SubPixel_Mag_Psp,     &
                          Psf_Mes_Interp_SubPixel_Mag_Psp_Gain(SUB_PN) )
!
!          Measurement subpixel Self Apodisation Function
           Fcomp%PN_Cen = Srf_Param%PN_Cen_Mes
           select case ( Srf_Param%Mode_Agreg(1:&
                len_trim(Srf_Param%Mode_Agreg)) )
           case ( "MER1" )
              call saf_time_mertz1( Srf_Param, Path, Psf_Ref,            &
                                    Psf_Mes_Interp_SubPixel_Mag_Psp_Gain(SUB_PN),&
                                    Ccm, Chrom, Pupil, RefracIndex_Mes,  &
                                    Fcomp, NF, SUB_PN, Saf(SB)        )
           case ( "MER2" )
               call mertz_calc( Prism_Motion, Fcomp, RefracIndex_Mes,  &
                                Psf_Mes_Interp_SubPixel_Mag_Psp_Gain(SUB_PN),&
                                Saf(SB)%Wn0(NF), Mertz                 )
               call saf_time_mertz2( Srf_Param, Path, Psf_Ref,            &
                                     Psf_Mes_Interp_SubPixel_Mag_Psp_Gain(SUB_PN),&
                                     Ccm, Chrom, Pupil, RefracIndex_Mes,  &
                                     Fcomp, Mertz, NF, SUB_PN, Saf(SB)    )  
           case default
               call saf_time( Srf_Param, Path, Psf_Ref,                    &
                              Psf_Mes_Interp_SubPixel_Mag_Psp_Gain(SUB_PN),&
                              Ccm, Chrom, RefracIndex_Mes, Fcomp,          &
                              NF, SUB_PN, Srf_Param%PN_Mes,Saf(SB) )
           end select
!
!          Use of Rpd Sampling
           call saf_opd( Srf_Param, Ccm, Saf(SB), NF, SUB_PN, Saf_Rpd, Sas(SB) )
!
!          Spectral response function 1a
           call srf_L1a( Srf_Param,&
                         NF,       &
                         SUB_PN,   &
                         Sas(SB),  &
                         Srf(SB)   )
!
!          Spectral response function 1b
           call srf_L1b( Srf_Param,&
                         NF,       &
                         SUB_PN,   &
                         Sas(SB),  &
                         Srf(SB)   )
!
           call dalloc_Psf( Psf_Mes_Interp_SubPixel_Mag_Psp )
           call dalloc_Psf( Psf_Mes_Interp_SubPixel_Mag )
           call dalloc_Psf( Psf_Mes_Interp_SubPixel )
!
         end do  ! End sub pixels loop
         call dalloc_Psf( Psf_Mes_Interp )
!
!        Sub Pixels aggregation
         call saf_agf( Srf_Param,&
                       NF,       &
                       Srf(SB),  &
                       Psf_Mes_Interp_SubPixel_Mag_Psp_Gain,&
                       Sas(SB)   )
!
         do SUB_PN = 1, Saf(SB)%NsPixel
           Saf(SB)%FieldMeanAngle(NF,SUB_PN) =              &
                            Sas(SB)%FieldMeanAngle(NF,SUB_PN)
           call dalloc_Psf( Psf_Mes_Interp_SubPixel_Mag_Psp_Gain(SUB_PN) )
         end do
         Saf(SB)%FieldMeanAngle(NF, Saf(SB)%NsPixel+1) = 0_DOUBLE
         deallocate( Psf_Mes_Interp_SubPixel_Mag_Psp_Gain )
!
!        aggregated instrument function 1a
         call srf_L1a( Srf_Param,        &
                       NF,               &
                       Sas(SB)%NsPixel+1,&
                       Sas(SB),          &
                       Srf(SB)           )
!
!        aggregated instrument function 1b
         call srf_L1b( Srf_Param,        &
                       NF,               &
                       Sas(SB)%NsPixel+1,&
                       Sas(SB),          &
                       Srf(SB)           )
!
       end do  ! End Wave numbers loop
!
       if( Saf(SB)%NsWn0 /= 1 ) then
          call phasemono( Sas(SB)%Arg0(1,SUB_PN),Sas(SB)%NsWn0 )
       end if
!
     end do  ! End Spectral bands loop
     call dalloc_Psf( Psf_Rpd_Pixel_Mag_Psp )
     call dalloc_Psf( Psf_Rpd_Pixel_Mag )
     call dalloc_Psf( Psf_Rpd_Pixel )
!
!    Apodisation Function
     do SB = 1, Srf_Param%Nband ! loop over spectral bands
       do NF = 1, Saf(SB)%NsWn0 ! lopp over wavenumbers
         do SUB_PN = 1, Saf(SB)%NsPixel+1 ! loop over pixels
           call apf_calc( NF, SUB_PN, Sas(SB), Srf(SB), Apf(SB) )
         end do  ! End subpixels loop
       end do  ! End Wave Numbers loop
     end do  ! End Spectral bands loop
!
     return
 999 write(0,*) ' isrf_model_sub Fatal Error'
     call exit(1)
   end subroutine isrf_model_sub
!
!
!!
!!
!> isrf_model_init -- Public
!!
!! * Purpose
!!
!!   Initialisation of all required parameters before computation of the spectral 
!!   response function 
!!
!! * Description
!!
!!   This subroutine allows the initialisation of all the parameters required for the 
!!   computation of the srf.
!!   First of all, the input file which defines all the reading file names is read.
!!   The Srf parameters are initialised ; the folowing step is the pixels position 
!!   initialisation. 
!!   Then the PSF initialisation is done for the three types Psf_Ref, Psf_Rpd and Psf_Mes
!!   The chromatism initialisation is realized as well as the field compensation 
!!   initialisation.
!!   The Corner Cube Motion is initialisated even for Rpd detection vibration.
!!   The last initialisations deal with the Rpd frequency defect, the Pupil/Mirror
!!   initialisation, the Mertz Prism Motion initialisation, the material refractive 
!!   index initialisation both for the measurement and for the RPD. 
!!
!! * Inputs 
!!
!!     - Files_param_Inputs : character / input file with definition of reading file names
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Srf_Param :  type_Srf / type for declaration and allocation of srf 
!!     - Psf_Ref   :  type_Psf / type for declaration and allocation of psf 
!!     - Psf_Rpd   :  type_Psf / type for declaration and allocation of psf 
!!     - Psf_Mes   :  type_Psf / type for declaration and allocation of psf 
!!     - Chrom     :  type_Chrom / type for declaration and allocation of chromatism 
!!     - Fcomp     :  type_Fcomp / type for declaration and allocation of field 
!!                    compensation
!!     - Ccm       :  type_Ccm / type for declaration and allocation of cube corner 
!!                    motion  
!!     - Ccm_Rpd   :  type_Ccm / type for declaration and allocation of cube corner 
!!                    motion  
!!     - Pupil     :  type_Pupil / type for declaration and allocation of pupil
!!     - RefracIndex_Mes :  type_RefracIndex / type for declaration and allocation of 
!!                          refractive index of materials
!!     - Chrom_Rpd :  type_Chrom / type for declaration and allocation of chromatism 
!!     - Prism_Motion        :  type_Ccm / type for declaration and allocation of cube corner motion
!!     - Mertz : Mertz / type for declaration and allocation of prism defect
!!     - RefracIndex_Rpd :  type_RefracIndex / type for declaration and allocation of 
!!                          refractive index of materials
!!
!! * References
!!

   subroutine isrf_model_init( Files_param_Inputs, &
                               Srf_Param, Psf_Ref, &
                               Psf_Rpd, Psf_Mes,   &
                               Ccm_Rpd, Ccm,       &
                               Chrom_Rpd,  Chrom,  &
                               RefracIndex_Rpd,    &
                               RefracIndex_Mes,    &
                               Pupil, Mertz,       &
                               Prism_Motion,       &
                               Fcomp               )

     character(len=*)    , intent(in)                :: Files_param_Inputs
     type(type_Srf_Param), intent(out)               :: Srf_Param
     type(type_Psf)      , intent(out)               :: Psf_Ref
     type(type_Psf)      , intent(out)               :: Psf_Rpd
     type(type_Psf)      , intent(out)               :: Psf_Mes
     type(type_Chrom)    , intent(out)               :: Chrom
     type(type_Chrom)    , intent(out)               :: Chrom_Rpd
     type(type_Fcomp)    , intent(out)               :: Fcomp
     type(type_Ccm)      , intent(out)               :: Ccm
     type(type_Ccm)      , intent(out)               :: Ccm_Rpd
     type(type_RefracIndex)  ,intent(out)            :: RefracIndex_Mes
     type(type_RefracIndex)  ,intent(out)            :: RefracIndex_Rpd
     type(type_Pupil)    , intent(out)               :: Pupil
     type(type_Ccm)      , intent(out)               :: Prism_Motion
     type(type_Mertz)    , intent(out)               :: Mertz
     integer(kind=LONG)                              :: iFile
     character(len=500)                              :: File_Srf_General
     character(len=500)                              :: File_Detector_Matrix_Conf
     character(len=500)                              :: File_Pix_Ref_Position
     character(len=500)                              :: File_Pix_Rpd_Position
     character(len=500)                              :: File_Pix_Mes_Position
     character(len=500)                              :: File_Psf_Pix_Ref_Header
     character(len=500)                              :: File_Psf_Pix_Ref_Main
     character(len=500)                              :: File_Psf_Pix_Rpd_Header
     character(len=500)                              :: File_Psf_Pix_Rpd_Main
     character(len=500)                              :: File_Psf_Pix_Mes_Header
     character(len=500)                              :: File_Psf_Pix_Mes_Main
     character(len=500)                              :: File_Chrom
     character(len=500)                              :: File_Fcomp
     character(len=500)                              :: File_Ccm
     character(len=500)                              :: File_Ccm_Rpd
     character(len=500)                              :: File_Pupil_Header
     character(len=500)                              :: File_Pupil_Main
     character(len=500)                              :: File_Pupil_WFE
     character(len=500)                              :: File_Pupil_Defect
     character(len=500)                              :: File_Mertz
     character(len=500)                              :: File_Material_Mes
     character(len=500)                              :: File_Material_Rpd
     character(len=500)                              :: File_Scan_Mode

     integer(kind=LONG)                              :: SB
     integer(kind=LONG)                              :: Np
     integer(kind=LONG)                              :: Ns
     integer(kind=LONG)                              :: Nb
     integer(kind=LONG)                              :: NsWn0_Max
     integer(kind=LONG)                              :: ErrCode
     real(kind=DOUBLE)                               :: XAvg
     real(kind=DOUBLE)                               :: XStd
     real(kind=DOUBLE)                               :: XMin
     real(kind=DOUBLE)                               :: XMax
     integer(kind=LONG)                              :: NsMin
     integer(kind=LONG)                              :: NsMax
     integer(kind=LONG)                              :: iPos
!
     iFile = 10
!
!    reading file names
     iPos = 1
     open(unit=iFile, file=Files_param_Inputs, status='old', err=999)
     iPos = 2
     read(iFile,fmt='(a)',err=999) File_Srf_General
     iPos = 3
     read(iFile,fmt='(a)',err=999) File_Detector_Matrix_Conf
     iPos = 4
     read(iFile,fmt='(a)',err=999) File_Pix_Ref_Position
     iPos = 5
     read(iFile,fmt='(a)',err=999) File_Pix_Rpd_Position
     iPos = 6
     read(iFile,fmt='(a)',err=999) File_Pix_Mes_Position
     iPos = 7
     read(iFile,fmt='(a)',err=999) File_Psf_Pix_Ref_Header
     iPos = 8
     read(iFile,fmt='(a)',err=999) File_Psf_Pix_Rpd_Header
     iPos = 9
     read(iFile,fmt='(a)',err=999) File_Psf_Pix_Mes_Header
     iPos = 10
     read(iFile,fmt='(a)',err=999) File_Psf_Pix_Ref_Main
     iPos = 11
     read(iFile,fmt='(a)',err=999) File_Psf_Pix_Rpd_Main
     iPos = 12
     read(iFile,fmt='(a)',err=999) File_Psf_Pix_Mes_Main
     iPos = 13
     read(iFile,fmt='(a)',err=999) File_Chrom
     iPos = 14
     read(iFile,fmt='(a)',err=999) File_Fcomp
     iPos = 15
     read(iFile,fmt='(a)',err=999) File_Ccm
     iPos = 16
     read(iFile,fmt='(a)',err=999) File_Ccm_Rpd
     iPos = 17
     read(iFile,fmt='(a)',err=999) File_Pupil_Header
     iPos = 18
     read(iFile,fmt='(a)',err=999) File_Pupil_Main
     iPos = 19
     read(iFile,fmt='(a)',err=999) File_Pupil_WFE
     iPos = 20
     read(iFile,fmt='(a)',err=999) File_Pupil_Defect
     iPos = 21
     read(iFile,fmt='(a)',err=999) File_Mertz
     iPos = 22
     read(iFile,fmt='(a)',err=999) File_Material_Rpd
     iPos = 23
     read(iFile,fmt='(a)',err=999) File_Material_Mes
     iPos = 24
     read(iFile,fmt='(a)',err=999) File_Scan_Mode
     iPos = 25
     close(unit=iFile)
!
!    Srf param initialisation
     open(unit=iFile, file=File_Srf_General, status='old', err=999)
     iPos = 26
     Srf_Param%filename = File_Srf_General
     read(iFile,*,err=999) Srf_Param%L1c_Option
     write(*,*) 'Srf_Param%L1c_Option',Srf_Param%L1c_Option
     read(iFile,*,err=999) Srf_Param%Apod_Option
     write(*,*) 'Srf_Param%L1c_Option',Srf_Param%Apod_Option
     iPos = 27
     read(iFile,*,err=999) Srf_Param%OpdFraction
     write(*,*) 'Srf_Param%OpdFraction',Srf_Param%OpdFraction
     iPos = 28
     read(iFile,*,err=999) Srf_Param%FWHM
     write(*,*) 'Srf_Param%FWHM',Srf_Param%FWHM
     iPos = 29
     read(iFile,*,err=999) Srf_Param%Mode_Agreg
     write(*,*) 'Srf_Param%Mode_Agreg',Srf_Param%Mode_Agreg
     iPos = 30
     read(iFile,*,err=999) Srf_Param%Calib_Option
     write(*,*) 'Srf_Param%Calib_Option',Srf_Param%Calib_Option
     iPos = 31
     read(iFile,*,err=999) Srf_Param%MagnificationY
     write(*,*) 'Srf_Param%MagnificationY',Srf_Param%MagnificationY
     iPos = 32
     read(iFile,*,err=999) Srf_Param%MagnificationZ
     write(*,*) 'Srf_Param%MagnificationZ',Srf_Param%MagnificationZ
     iPos = 33
     read(iFile,*,err=999) Srf_Param%NsOpd
     write(*,*) 'Srf_Param%NsOpd',Srf_Param%NsOpd
     iPos = 34
     read(iFile,*,err=999) Srf_Param%OpdMax
     write(*,*) 'Srf_Param%OpdMax',Srf_Param%OpdMax
     iPos = 35
     read(iFile,*,err=999) Srf_Param%L1aNsFft
     write(*,*) 'Srf_Param%L1aNsFft',Srf_Param%L1aNsFft
     iPos = 36
     read(iFile,*,err=999) Srf_Param%L1bNsFft
     write(*,*) 'Srf_Param%L1bNsFft',Srf_Param%L1bNsFft
     iPos = 37
     read(iFile,*,err=999) Srf_Param%L1cNsFft
     write(*,*) 'Srf_Param%L1cNsFft',Srf_Param%L1cNsFft
     iPos = 38
     read(iFile,*,err=999) Srf_Param%SigI
     write(*,*) 'Srf_Param%SigI',Srf_Param%SigI
     iPos = 39
     read(iFile,*,err=999) Srf_Param%NsSDWn1a
     write(*,*) 'Srf_Param%NsSDWn1a',Srf_Param%NsSDWn1a
     iPos = 40
     read(iFile,*,err=999) Srf_Param%NsSDWn1b
     write(*,*) 'Srf_Param%NsSDWn1b',Srf_Param%NsSDWn1b
     iPos = 41
     read(iFile,*,err=999) Srf_Param%NsSDWn1c
     write(*,*) 'Srf_Param%NsSDWn1c',Srf_Param%NsSDWn1c
     iPos = 42
     read(iFile,*,err=999) Srf_Param%SDWnMax1a
     write(*,*) 'Srf_Param%SDWnMax1a',Srf_Param%SDWnMax1a
     iPos = 43
     read(iFile,*,err=999) Srf_Param%SDWnMax1b
     write(*,*) 'Srf_Param%SDWnMax1b',Srf_Param%SDWnMax1b
     iPos = 44
     read(iFile,*,err=999) Srf_Param%SDWnMax1c
     write(*,*) 'Srf_Param%SDWnMax1c',Srf_Param%SDWnMax1c
     iPos = 45
     read(iFile,*,err=999) Srf_Param%H_Sat
     write(*,*) 'Srf_Param%H_Sat',Srf_Param%H_Sat
     iPos = 45
     read(iFile,*,err=999) Srf_Param%FieldMeanAngle
     write(*,*) 'Srf_Param%FieldMeanAngle',Srf_Param%FieldMeanAngle
     iPos = 46
     read(iFile,*,err=999) Srf_Param%PN_Ref
     write(*,*) 'Srf_Param%PN_Ref',Srf_Param%PN_Ref
     iPos = 47
     read(iFile,*,err=999) Srf_Param%PN_Rpd
     write(*,*) 'Srf_Param%PN_Rpd',Srf_Param%PN_Rpd
     iPos = 48
     read(iFile,*,err=999) Srf_Param%PN_Mes
     write(*,*) 'Srf_Param%PN_Mes',Srf_Param%PN_Mes
     iPos = 49
     read(iFile,*,err=999) Srf_Param%PN_Cen_Rpd
     write(*,*) 'Srf_Param%PN_Cen_Rpd',Srf_Param%PN_Cen_Rpd
     iPos = 50
     read(iFile,*,err=999) Srf_Param%PN_Cen_Mes
     write(*,*) 'Srf_Param%PN_Cen_Mes',Srf_Param%PN_Cen_Mes
     iPos = 51
     read(iFile,*,err=999) Srf_Param%AxeY
     write(*,*) 'Srf_Param%AxeY',Srf_Param%AxeY
     iPos = 52
     read(iFile,*,err=999) Srf_Param%AxeZ
     write(*,*) 'Srf_Param%AxeZ',Srf_Param%AxeZ
     iPos = 53
     read(iFile,*,err=999) Srf_Param%Rpd_Wn0_Offset
     write(*,*) 'Srf_Param%Rpd_Wn0_Offset',Srf_Param%Rpd_Wn0_Offset
     iPos = 54
     read(iFile,*,err=999) Srf_Param%Rpd_Wn0_Jitter
     write(*,*) 'Srf_Param%Rpd_Wn0_Jitter',Srf_Param%Rpd_Wn0_Jitter
     iPos = 55
     read(iFile,*,err=999) Srf_Param%Nband
     write(*,*) 'Srf_Param%Nband',Srf_Param%Nband
     iPos = 56
     allocate( Srf_Param%NsWn0(Srf_Param%Nband),    stat=ErrCode )
     allocate( Srf_Param%SB_List(Srf_Param%Nband),  stat=ErrCode )
     write(*,*) 'Srf_Param%Nband',Srf_Param%Nband
     do SB = 1, Srf_Param%Nband
        read(iFile,*,err=999) Srf_Param%SB_List(SB)
        read(iFile,*,err=999) Srf_Param%NsWn0(SB)
        write(*,*) 'SB_List Srf_Param%NsWn0(SB)',Srf_Param%SB_List(SB)&
                                                ,Srf_Param%NsWn0(SB)
        iPos = 57
     end do
     NsWn0_Max = maxval(Srf_Param%NsWn0(1:Srf_Param%Nband))
     allocate( Srf_Param%Wn0(NsWn0_Max,Srf_Param%Nband),&
                                           stat=ErrCode )
     do SB = 1, Srf_Param%Nband
        do Nb = 1, Srf_Param%NsWn0(SB)
           iPos = 58
           read(iFile,*,err=999) Srf_Param%Wn0(Nb,SB)
           write(*,*) 'Srf_Param%Wn0(Nb,SB)',Srf_Param%Wn0(Nb,SB)
        end do
     end do
     close(unit=iFile)
!
!    Scanning configuration
     iPos = 59
     open(unit=iFile, file=File_Scan_Mode, status='old', err=999)
     write(*,*) File_Scan_Mode(1:len_trim(File_Scan_Mode))
     iPos = 60
     read(iFile,*,err=999) Srf_Param%Scan_Mode
     iPos = 61
     read(iFile,*,err=999) Srf_Param%Ratio_bilatere
     iPos = 62
     read(iFile,*,err=999) Srf_Param%Filter
     iPos = 63
     write(*,*) 'Srf_Param%Scan_Mode     ',Srf_Param%Scan_Mode
     write(*,*) 'Srf_Param%Ratio_bilatere',Srf_Param%Ratio_bilatere
     write(*,*) 'Srf_Param%Filter        ',Srf_Param%Filter
     close(unit=iFile)
!
!    Detector configuration
     iPos = 64
     open(unit=iFile, file=File_Detector_Matrix_Conf, status='old', err=999)
     iPos = 65
     Srf_Param%Rpd_NsPsf = 1
     allocate( Srf_Param%Rpd_Psf_OffsetY(Srf_Param%Rpd_NsPsf), stat=ErrCode )
     allocate( Srf_Param%Rpd_Psf_OffsetZ(Srf_Param%Rpd_NsPsf), stat=ErrCode )
     Srf_Param%Rpd_Psf_OffsetY(1:Srf_Param%Rpd_NsPsf) = 0.d+00
     Srf_Param%Rpd_Psf_OffsetZ(1:Srf_Param%Rpd_NsPsf) = 0.d+00
     read(iFile,*,err=999) Srf_Param%Mes_NsPsf
     iPos = 66
     allocate( Srf_Param%Mes_Psf_OffsetY(Srf_Param%Mes_NsPsf),  stat=ErrCode )
     allocate( Srf_Param%Mes_Psf_OffsetZ(Srf_Param%Mes_NsPsf),  stat=ErrCode )
     do Ns = 1, Srf_Param%Mes_NsPsf
        read(iFile,*,err=999) Srf_Param%Mes_Psf_OffsetY(Ns),&
                              Srf_Param%Mes_Psf_OffsetZ(Ns)
        Srf_Param%Mes_Psf_OffsetY(Ns) =                 &
                          Srf_Param%Mes_Psf_OffsetY(Ns) &
                        / Srf_Param%H_Sat     
        Srf_Param%Mes_Psf_OffsetZ(Ns) =                 & 
                          Srf_Param%Mes_Psf_OffsetZ(Ns) &
                        / Srf_Param%H_Sat     
        iPos = 67
     end do
     close(unit=iFile)
!
!    Pixels position initialisation
     iPos = 68
     open(unit=iFile, file=File_Pix_Ref_Position, status='old', err=999)
     iPos = 69
     read(iFile,*,err=999) 
     iPos = 70
     read(iFile,*,err=999) Srf_Param%Ref_NsPixel
     iPos = 71
     allocate( Srf_Param%Ref_PosY(Srf_Param%Ref_NsPixel),  stat=ErrCode )
     allocate( Srf_Param%Ref_PosZ(Srf_Param%Ref_NsPixel),  stat=ErrCode )
     do Np = 1, Srf_Param%Ref_NsPixel
        read(iFile,*,err=999) Srf_Param%Ref_PosY(Np), &
                              Srf_Param%Ref_PosZ(Np)
        Srf_Param%Ref_PosY(Np) = Srf_Param%Ref_PosY(Np) &
                               / Srf_Param%H_Sat
        Srf_Param%Ref_PosZ(Np) = Srf_Param%Ref_PosZ(Np) &
                               / Srf_Param%H_Sat
        iPos = 72
     end do
     close(unit=iFile)
     iPos = 73
     open(unit=iFile, file=File_Pix_Rpd_Position, status='old', err=999)
     read(iFile,*,err=999) 
     read(iFile,*,err=999) Srf_Param%Rpd_NsPixel
     allocate( Srf_Param%Rpd_PosY(Srf_Param%Rpd_NsPixel),  stat=ErrCode )
     allocate( Srf_Param%Rpd_PosZ(Srf_Param%Rpd_NsPixel),  stat=ErrCode )
     do Np = 1, Srf_Param%Rpd_NsPixel
        read(iFile,*,err=999) Srf_Param%Rpd_PosY(Np), &
                              Srf_Param%Rpd_PosZ(Np)
        Srf_Param%Rpd_PosY(Np) = Srf_Param%Rpd_PosY(Np) &
                               / Srf_Param%H_Sat
        Srf_Param%Rpd_PosZ(Np) = Srf_Param%Rpd_PosZ(Np) &
                               / Srf_Param%H_Sat
     end do
     close(unit=iFile)
     iPos = 74
     open(unit=iFile, file=File_Pix_Mes_Position, status='old', err=999)
     read(iFile,*,err=999) 
     read(iFile,*,err=999) Srf_Param%Mes_NsPixel
     allocate( Srf_Param%Mes_PosY(Srf_Param%Mes_NsPixel),  stat=ErrCode )
     allocate( Srf_Param%Mes_PosZ(Srf_Param%Mes_NsPixel),  stat=ErrCode )
     do Np = 1, Srf_Param%Mes_NsPixel
        read(iFile,*,err=999) Srf_Param%Mes_PosY(Np), &
                              Srf_Param%Mes_PosZ(Np)
        Srf_Param%Mes_PosY(Np) = Srf_Param%Mes_PosY(Np) &
                               / Srf_Param%H_Sat
        Srf_Param%Mes_PosZ(Np) = Srf_Param%Mes_PosZ(Np) &
                               / Srf_Param%H_Sat
     end do
     close(unit=iFile)
     iPos = 75
!
!    Psf_Ref initialisation
     Psf_Ref%H_Sat          = Srf_Param%H_Sat
     Psf_Ref%MagnificationY = Srf_Param%MagnificationY
     Psf_Ref%MagnificationZ = Srf_Param%MagnificationZ
     Psf_Ref%PixelPosY = Srf_Param%Ref_PosY(Srf_Param%PN_Ref)
     Psf_Ref%PixelPosZ = Srf_Param%Ref_PosZ(Srf_Param%PN_Ref)
     call psf_init( File_Psf_Pix_Ref_Header, &
                    File_Psf_Pix_Ref_Main,   &
                    Psf_Ref                  )
!
!    Psf_Rpd initialisation
     Psf_Rpd%H_Sat          = Srf_Param%H_Sat
     Psf_Rpd%MagnificationY = Srf_Param%MagnificationY
     Psf_Rpd%MagnificationZ = Srf_Param%MagnificationZ
     Psf_Rpd%PixelPosY = Srf_Param%Rpd_PosY(Srf_Param%PN_Rpd)
     Psf_Rpd%PixelPosZ = Srf_Param%Rpd_PosZ(Srf_Param%PN_Rpd)
     call psf_init( File_Psf_Pix_Rpd_Header, &
                    File_Psf_Pix_Rpd_Main,   &
                    Psf_Rpd                  )
!
!    Psf Pixel initialisation
     Psf_Mes%H_Sat          = Srf_Param%H_Sat
     Psf_Mes%MagnificationY = Srf_Param%MagnificationY
     Psf_Mes%MagnificationZ = Srf_Param%MagnificationZ
     Psf_Mes%PixelPosY = Srf_Param%Mes_PosY(Srf_Param%PN_Mes)
     Psf_Mes%PixelPosZ = Srf_Param%Mes_PosZ(Srf_Param%PN_Mes)
     call psf_init( File_Psf_Pix_Mes_Header, &
                    File_Psf_Pix_Mes_Main,   &
                    Psf_Mes                  )
!
!    chromatism initialisation and optical aberation
     Psf_Ref%NbCol = Psf_Mes%NbCol
     Psf_Ref%NbLin = Psf_Mes%NbLin
     call chromatism_init( File_Chrom, &
                           Psf_Ref,    &
                           Srf_Param,  &
                           Chrom       )
     Psf_Ref%NbCol = Psf_Rpd%NbCol
     Psf_Ref%NbLin = Psf_Rpd%NbLin    
!
!    chromatism initialisation
     call chromatism_init( File_Chrom, &
                           Psf_Rpd,    &
                           Srf_Param,  &
                           Chrom_Rpd   )    
!
!    field compensation initialisation
     call fcompensation_init( File_Fcomp,&
                              Srf_Param, &
                              Fcomp      )
!
!    Corner Cube Motion initialisation
     call ccm_init( File_Ccm, &
                    Ccm       )
!
!    Corner Cube Motion initialisation for Rpd detection vibration
     call ccm_init( File_Ccm_Rpd, &
                    Ccm_Rpd       )
!
!    Rpd frequency defect initialisation
     allocate( Srf_Param%Rpd_Wn0_Jitter_vect(Srf_Param%NsOpd), stat=ErrCode )
     if( ErrCode /= 0 ) then
        write(*,*) 'isrf_model_init allocation error'
        call exit(1)
     end if
     do Ns = 1, Srf_Param%NsOpd
        Srf_Param%Rpd_Wn0_Jitter_vect(Ns) =   &
               Srf_Param%Rpd_Wn0_Offset       &
             + Srf_Param%Rpd_Wn0_Jitter*grnd(0)
     end do
     call statistics( Srf_Param%NsOpd              &
                     ,Srf_Param%Rpd_Wn0_Jitter_vect&
!
                     ,XAvg       &
                     ,XStd       &
                     ,XMin       &
                     ,XMax       &
                     ,NsMin      &
                     ,NsMax      )
     write(*,'(60a)') &
               'Rpd_Wn0_Jitter_Vect : XAvg, XStd, XMin, XMax, NsMin, NsMax'
     write(*,'(4e20.6,2i10)') &
               XAvg, XStd, XMin, XMax, NsMin, NsMax
!
!    Pupil/Mirror initialisation
     call pupil_init( File_Pupil_Header, &
                      File_Pupil_Main,   &
                      File_Pupil_WFE,    &
                      File_Pupil_Defect, &
                      Srf_Param,         &
                      Pupil              )
!
!    Mertz Prism Motion initialisation
     call mertz_init( File_Mertz,  &
                      Mertz,       &
                      Prism_Motion )
!
!    Material refractive index initialisation Mes
     call refractive_index_init( Srf_Param, File_Material_Mes,&
                                 Psf_Mes, RefracIndex_Mes )
!    Material refractive index initialisation Rpd
     call refractive_index_init( Srf_Param, File_Material_Rpd,&
                                 Psf_Rpd, RefracIndex_Rpd )
!
     return
999  write(*,*) 'parameters reading error',iPos
     write(*,*) 'isrf_model_init Fatal Error'
     call exit(1)
   end subroutine isrf_model_init
!
!
end module isrf_model_computation_module
