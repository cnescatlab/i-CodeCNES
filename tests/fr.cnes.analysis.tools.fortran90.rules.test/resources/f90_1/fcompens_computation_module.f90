!!# fcompens_computation_module.f90 --
!!#
!!#           Project: SPS_GENERIC
!!#           Authors: NOVELTIS/B.TOURNIER
!!#              Date: January 2010
!!#           Version: $Revision: 1.3 $
!!# Last modification: $Date: 2011-03-07 15:54:59 $
!!#
!!# Language:  F90
!!# Standards: Noveltis
!!#
!!# --
!!#
!! 
!> fcompens_computation_module -- Module
!!
!! * Purpose
!!
!!      Module for field compensation computation.
!!
!! * Description
!!      
!!      This module defines the implementation of options for active field 
!!      compensation. Indeed 3 methods are candidates:
!!      - rotation of compensation mirrors around an axis (quasi-)perpendicular 
!!        to the field angle gradient in the image plane of each sample. 
!!      - Linear translation of spherical compensation mirrors parallel to the 
!!        direction towards the nominal optical axis in the image plane. 
!!      - Compensation mirror deformation in synchronisation with the cube corner 
!!        motion.
!!      For none of the candidate methods field compensation for the central 
!!      sample coincident with the nominal interferometer axis is expected. 
!!      The compensation approaches are adapted for a quasi-linear structure 
!!      of the interference pattern within a sample. In the central sample the 
!!      interference pattern is radial. Field angle compensation as for the other 
!!      samples is inefficient and, due to the small maximum field angle, not required.
!!
!!      A 'nominal' field compensation is implemented with typical perturbations 
!!
!!      Hypothesis: The nominal field compensation modifies the field angle dependent 
!!              OPD in each sample, by correcting for the OPD gradient in the 
!!              centre of the sample.
!!
!! * Sub-routines and functions
!!
!!     - FieldCompensation_sub : Define the field compensation value for each point of 
!!                               the PSF
!!     - fcompensation_init : Initialisation of the type defining the field compensation 
!!                            in function of the agregation mode chosen (THEO, DYN1, DYN2,
!!                            DEFAULT) 
!!     - FieldCompensation_sub_mertz :  Define the field compensation value for each point
!!                                      of the PSF for Mertz's System
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.4.1.1.2
!!

module fcompens_computation_module
   use precision_type
   use error_type
   use psf_type
   use srf_param_type
   use fcomp_type
   use math_module
   use statistique_module
   use refrac_index_type
   use chromatism_computation_module
!
   implicit none
!
!
   public ::                             &
             FieldCompensation_sub,      &
             FieldCompensation_sub_mertz,&
             fcompensation_init          
!
   contains
!
!

!!
!> FieldCompensation_sub -- Public
!!
!! * Purpose
!!
!!     Define the field compensation value for each point of the PSF
!!
!! * Description
!!
!!      For each point of the PSF, the computation of the field compensation value 
!!      requires the following steps:
!!      - allocation
!!      - determination of field compensation reference (central column, central 
!!        line and central field angle)
!!      - computation of the field compensation (slope for each point of the PSF)
!!            - in the theoretical perfect field compensation case ('THEO'):
!!           OPD_0 = OPD*(1-cos(theta)) = OPD (1 + 1/cos(theta) - 1)
!!            - in the theoretical field compensation according following equation ('DYN1'):
!!           OPD' = OPD (1 + sin(theta_central)*(theta-theta_central))
!!           around the (circular) axis with field angle theta_central
!!            - in the field compensation according following equation ('DYN2'):
!!           OPD" = OPD (1 + sin(theta_central)*theta")
!!           with theta" the angular difference to the compensation axis
!!            - in the default case, no compensation mode is implemented
!!
!! * Inputs
!!
!!     - Psf : type_Psf / type for declaration and allocation of the point spread function 
!!     - Fcomp : type_Fcomp / type for declaration and allocation of field compensation
!!     - Srf_Param : type_Srf_Param / type for declaration and allocation of srf 
!!
!! * Inputs/outputs
!!
!!     - FieldComp : field compensation value for each point of the PSF
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.4.1.1.2
!!

   subroutine FieldCompensation_sub( Psf, Srf_Param, FieldComp )
   implicit none
     type(type_Psf)    ,intent(in)                :: Psf
     type(type_Srf_Param), intent(in)             :: Srf_Param
     real(kind=DOUBLE) ,intent(inout)             &
          ,dimension(1:3,1:Psf%NbCol,1:Psf%NbLin) :: FieldComp
     integer(kind=LONG)                           :: NC
     integer(kind=LONG)                           :: NL
     real(kind=DOUBLE)                            :: theta
     integer(kind=LONG)                           :: NC_m
     integer(kind=LONG)                           :: NL_m
     real(kind=DOUBLE)                            :: theta_m
     integer(kind=LONG)                           :: ErrCode
     real(kind=DOUBLE) ,dimension(:) ,allocatable :: Y
     real(kind=DOUBLE) ,dimension(:) ,allocatable :: Z
!
!    allocate Y(Psf%NbCol) Z(Psf%NbLin)
     allocate ( Y(Psf%NbCol), stat=ErrCode )
     allocate ( Z(Psf%NbLin), stat=ErrCode )
     if (ErrCode > 0) then
        write(0,*) 'allocation Y Z Error'
        write(0,*) 'FieldCompensation_sub: fatal error'
        call exit(1)
     end if
     Y(1:Psf%NbCol) = Psf%Y(1:Psf%NbCol) + Srf_Param%AxeY
     Z(1:Psf%NbLin) = Psf%Z(1:Psf%NbLin) + Srf_Param%AxeZ
!
!    central column and line (Reference for field compensation)
     NC_m = int(Psf%NbCol/2) + 1
     NL_m = int(Psf%NbLin/2) + 1
     theta_m = dsqrt( Y(NC_m)*Y(NC_m) + Z(NL_m)*Z(NL_m) )

!    compute the field compensation (slope for each point of the PSF)
     select case (Srf_Param%Mode_Agreg)

     case ('THEO')
!    theoretical perfect field compensation OPD_0 = OPD*(1-cos(theta)) 
!    = OPD (1 + 1/cos(theta) - 1)
     FieldComp(2:3,:,:) = 0.d0
     do NL = 1, Psf%NbLin
        do NC = 1, Psf%NbCol
           theta = dsqrt( Y(NC)*Y(NC) + Z(NL)*Z(NL) )
           FieldComp(1,NC,NL) = ( 1.d0 - dcos(theta))  &
                              / dcos(theta)
        end do
     end do

     case ('DYN1')
!    theoretical field compensation OPD' 
!    = OPD (1 + sin(theta_central)*(theta-theta_central))
!    around the (circular) axis with field angle theta_central
     FieldComp(2:3,:,:) = 0.d0
     do NL = 1, Psf%NbLin
        do NC = 1, Psf%NbCol
           theta = dsqrt( Y(NC)*Y(NC) + Z(NL)*Z(NL) )
           FieldComp(1,NC,NL) = dsin(theta_m)*(theta-theta_m) &
                              / dcos(theta)
        end do
     end do

     case ('DYN2')
!    field compensation OPD" = OPD (1 + sin(theta_central)*theta")
!    with theta" the angular difference to the compensation axis
     FieldComp(2:3,:,:) = 0.d0
     do NL = 1, Psf%NbLin
        do NC = 1, Psf%NbCol
           theta = dsqrt( Y(NC)*Y(NC) + Z(NL)*Z(NL) )
           FieldComp(1,NC,NL) = dsin(theta_m)                 &
                              * dsqrt((Y(NC)-Y(NC_m))         &
                                     *(Y(NC)-Y(NC_m))         &
                                     +(Z(NL)-Z(NL_m))         &
                                     *(Z(NL)-Z(NL_m)))        &
                              * dcos(datan2(Z(NL)-Z(NL_m),    &
                                            Y(NC)-Y(NC_m))    &
                                    -datan2(Z(NL_m),Y(NC_m))) &
                             / dcos(theta)
        end do
     end do

!     case DEFAULT
     case default
       write(*,*) 'WARNING: Compensation mode not implemented!'
       write(*,*) 'No field compensation is applied!'
       FieldComp(:,:,:) = 0.d0
     end select
     deallocate( Y )
     deallocate( Z )
     return
   end subroutine FieldCompensation_sub
!
!------------------------------------------------------------------------------
!!
!> FieldCompensation_sub_mertz -- Public
!!
!! * Purpose
!!
!!     Define the field compensation value for each point of the PSF for Mertz's System
!!
!! * Description
!!
!!      For each point of the PSF, the computation of the field compensation value 
!!      requires the following steps:
!!      - allocation
!!      - computation of the field compensation (slope for each point of the PSF)
!!            - in the case of Mertz field compensation system without error ('MER1'), 
!!              the field compensation is computed after the refractive index of material 
!!              at wave number has been defined.:
!!            - in the case 'MER2', Mertz field compensation is computed taking into account 
!!              errors on refractive index and on prism thickness. 
!!            - in the default case, no compensation mode is implemented
!!
!! * Inputs
!!
!!     - Psf : type_Psf / type for declaration and allocation of the point spread function 
!!     - Fcomp : type_Fcomp / type for declaration and allocation of field compensation
!!     - RefracIndex : type_RefracIndex / type for declaration and allocation material 
!!                     refractive index initialisation
!!     - Srf_Param : type_Srf_Param / type for declaration and allocation of srf 
!!
!! * Inputs/outputs
!!
!!     - FieldComp : field compensation value for each point of the PSF
!!
!! * Outputs
!!
!! * References
!!

   subroutine FieldCompensation_sub_mertz( Psf ,Fcomp ,RefracIndex ,&
                                           Srf_Param ,Wn, FieldComp, N )
   implicit none
     type(type_Psf)          , intent(in)         :: Psf
     type(type_RefracIndex)  , intent(in)         :: RefracIndex
     type(type_Srf_Param)    , intent(in)         :: Srf_Param
     real(kind=DOUBLE)       , intent(in)         :: Wn
     type(type_Fcomp)        , intent(inout)      :: Fcomp
     real(kind=DOUBLE)     ,intent(inout)             &
          ,dimension(1:3,1:Psf%NbCol,1:Psf%NbLin) :: FieldComp
     real(kind=DOUBLE),       intent(out)         :: N
     integer(kind=LONG)                           :: NC
     integer(kind=LONG)                           :: NL
     real(kind=DOUBLE)                            :: theta
     integer(kind=LONG)                           :: ErrCode
     real(kind=DOUBLE) ,dimension(:) ,allocatable :: Y
     real(kind=DOUBLE) ,dimension(:) ,allocatable :: Z

!
!    
     allocate ( Y(Psf%NbCol), stat=ErrCode )
     allocate ( Z(Psf%NbLin), stat=ErrCode )
     if (ErrCode > 0) then
        write(0,*) 'allocation Y Z Error'
        write(0,*) 'FieldCompensation_sub_mertz: fatal error'
        call exit(1)
     end if
     Y(1:Psf%NbCol) = Psf%Y(1:Psf%NbCol) + Srf_Param%AxeY
     Z(1:Psf%NbLin) = Psf%Z(1:Psf%NbLin) + Srf_Param%AxeZ

!    compute the field compensation (slope for each point of the PSF)
     select case (Srf_Param%Mode_Agreg)

     case ('MER1')
!    
!    Mertz field compensation system without error
!
!    Refractive index of material at wave number 
     call CalcIndice( Fcomp%Material, RefracIndex, Wn, N )
     FieldComp(1:3,:,:) = real(0,kind=DOUBLE)
     theta= real(0,kind=DOUBLE)
     do NL = 1, Psf%NbLin
        do NC = 1, Psf%NbCol
           theta = dsqrt( Y(NC)*Y(NC) + Z(NL)*Z(NL) )
           FieldComp(1,NC,NL) = N/(N-real(1,kind=DOUBLE))* &
                ( dsqrt(N*N-dsin(theta)*dsin(theta)) - dcos(theta) ) &
                / dcos(theta)
        end do
     end do

     case ('MER2')
!    
!    Mertz field compensation system with error on
!         refractive index
!         prism thickness
!
        allocate( Fcomp%Theta(Psf%NbCol,Psf%NbLin), stat=ErrCode )
        if ( ErrCode /= 0 ) then
           write(0,*) 'Error while allocating Theta matrix'
        end if
        FieldComp(1:3,:,:) = real(0,kind=DOUBLE)
        Fcomp%Theta(:,:) = real(0,kind=DOUBLE)
        N = real(0,kind=DOUBLE) ! not used
        do NL = 1, Psf%NbLin
           do NC = 1, Psf%NbCol
              Fcomp%Theta(NC,NL) = dsqrt( Y(NC)*Y(NC) + Z(NL)*Z(NL) )
              FieldComp(1,NC,NL) =                                             &
                   ( dsqrt(RefracIndex%Nper(NC,NL)*RefracIndex%Nper(NC,NL) -   &
                   dsin(Fcomp%Theta(NC,NL))*dsin(Fcomp%Theta(NC,NL)))          &
                   - dcos(Fcomp%Theta(NC,NL)) )                                &
                   / dcos(Fcomp%Theta(NC,NL))
           end do
        end do

     case DEFAULT
        write(*,*) 'WARNING: Compensation mode not implemented!'
        write(*,*) 'No field compensation is applied!'
        FieldComp(:,:,:) = 0.d0
     end select
     deallocate( Y )
     deallocate( Z )
     return
   end subroutine FieldCompensation_sub_mertz
!-------------------------------------------------------------------------------
!

!!
!!
!> fcompensation_init -- Public
!!
!! * Purpose
!!
!!     Initialisation of the type defining the field compensation in 
!!     function of the agregation mode chosen (THEO, DYN1, DYN2, DEFAULT) 
!!
!! * Description
!!
!! * Inputs
!!
!!     - File_Fcomp : character / input file with definition of field compensation
!!                    parameters 
!!     - Srf_Param : type_Srf_Param / type for declaration and allocation of srf 
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Fcomp : type_Fcomp / type for declaration and allocation of field compensation 
!!
!! * References
!!

   subroutine fcompensation_init( File_Fcomp, &
                                  Srf_Param,  &
                                  Fcomp       )
     character(len=*)    , intent(in)                  :: File_Fcomp
     type(type_Srf_Param), intent(in)                  :: Srf_Param
     type(type_Fcomp)    , intent(out)                 :: Fcomp
     integer(kind=LONG)                                :: iFile
     integer(kind=LONG)                                :: iPos
     integer(kind=LONG)                                :: Ns
     real(kind=DOUBLE)                                 :: XAvg
     real(kind=DOUBLE)                                 :: XStd
     real(kind=DOUBLE)                                 :: XMin
     real(kind=DOUBLE)                                 :: XMax
     integer(kind=LONG)                                :: NsMin
     integer(kind=LONG)                                :: NsMax
!
     select case (Srf_Param%Mode_Agreg)
!
     case ( 'DYN1','DYN2','THEO' )
       iFile = 10
       iPos  = 1
       open(unit=iFile, file=File_Fcomp, status='old', err=999 )
       iPos  = 2
       Fcomp%filename = File_Fcomp
       read(iFile,*,err=999) Fcomp%DX
       write(*,*) 'Fcomp%DX',Fcomp%DX
       read(iFile,*,err=999) Fcomp%DA
       write(*,*) 'Fcomp%DA',Fcomp%DA
       read(iFile,*,err=999) Fcomp%DA_Jitter
       write(*,*) 'Fcomp%DA_Jitter',Fcomp%DA_Jitter
       Fcomp%NsOpd      = Srf_Param%NsOpd
       write(*,*) 'Fcomp%NsOpd',Fcomp%NsOpd
       Fcomp%PN_Cen_Rpd = Srf_Param%PN_Cen_Rpd
       write(*,*) 'Fcomp%PN_Cen_Rpd',Fcomp%PN_Cen_Rpd
       Fcomp%PN_Cen_Mes = Srf_Param%PN_Cen_Mes
       write(*,*) 'Fcomp%PN_Cen_Mes',Fcomp%PN_Cen_Mes
       read(iFile,*,err=999) Fcomp%Material 
       write(*,*) 'Fcomp%Material ',Fcomp%Material
       close(unit=iFile)
!
     case ( 'MER1','MER2' )
       iFile = 10
       iPos  = 1
       open(unit=iFile, file=File_Fcomp, status='old', err=999 )
       iPos  = 2
       Fcomp%filename = File_Fcomp
       read(iFile,*,err=999) Fcomp%DX
       write(*,*) 'Fcomp%DX',Fcomp%DX
       read(iFile,*,err=999) Fcomp%DA
       write(*,*) 'Fcomp%DA',Fcomp%DA
       read(iFile,*,err=999) Fcomp%DA_Jitter
       write(*,*) 'Fcomp%DA_Jitter',Fcomp%DA_Jitter
       Fcomp%NsOpd      = Srf_Param%NsOpd
       write(*,*) 'Fcomp%NsOpd',Fcomp%NsOpd
       Fcomp%PN_Cen_Rpd = Srf_Param%PN_Cen_Rpd
       write(*,*) 'Fcomp%PN_Cen_Rpd',Fcomp%PN_Cen_Rpd
       Fcomp%PN_Cen_Mes = Srf_Param%PN_Cen_Mes
       write(*,*) 'Fcomp%PN_Cen_Mes',Fcomp%PN_Cen_Mes
       read(iFile,*,err=999) Fcomp%Material 
       write(*,*) 'Fcomp%Material ',Fcomp%Material
       close(unit=iFile)
!
     case default
       Fcomp%filename = File_Fcomp
       Fcomp%DX         = 0.d0
       Fcomp%DA         = 0.d0
       Fcomp%DA_Jitter  = 0.d0
       Fcomp%NsOpd      = Srf_Param%NsOpd
       Fcomp%PN_Cen_Rpd = Srf_Param%PN_Cen_Rpd
       Fcomp%PN_Cen_Mes = Srf_Param%PN_Cen_Mes
       Fcomp%Material  = "ZnSe"
     end select
     write(*,*) 'Fcomp%NsOpd',Fcomp%NsOpd
     call alloc_Fcomp( Fcomp )
!
!    Compensation jitter
     do Ns = 1, Fcomp%NsOpd
        Fcomp%DA_Jitter_Vect(Ns) = Fcomp%DA*(1+Fcomp%DA_Jitter*grnd(0))
     end do
     call statistics( Fcomp%NsOpd    &
                     ,Fcomp%DA_Jitter_Vect&
!
                     ,XAvg       &
                     ,XStd       &
                     ,XMin       &
                     ,XMax       &
                     ,NsMin      &
                     ,NsMax      )
     write(*,'(60a)') &
               'DA_Jitter_Vect : XAvg, XStd, XMin, XMax, NsMin, NsMax'
     write(*,'(4f12.6,2i10)') &
               XAvg, XStd, XMin, XMax, NsMin, NsMax
!
     return
999  write(*,*) 'parameters reading error',iPos
     write(*,*) 'fcompensation_init Fatal Error'
     call exit(1)
   end subroutine fcompensation_init
!
!
 end module fcompens_computation_module
