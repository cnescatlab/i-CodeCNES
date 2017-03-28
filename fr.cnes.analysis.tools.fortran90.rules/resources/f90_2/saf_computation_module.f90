!!# saf_computation_module.f90 --
!!#
!!#           Project: SPS_GENERIC
!!#           Authors: NOVELTIS/B.TOURNIER
!!#              Date: october 2009
!!#           Version: $Revision: 1.9 $
!!# Last modification: $Date: 2012-02-08 10:19:17 $
!!#
!!# Language:  F90
!!# Standards: Noveltis
!!#
!!# --
!!#
!! 
!> saf_computation_module -- Module
!!
!! * Purpose
!!
!!     Module for self apodisation and apodisation functions computation.
!!
!! * Description
!!      
!!     This module defines the analytic model to calculate the Self 
!!     Apodisation Function (SAF) of the instrument which consists on 
!!     a monochromatic calculation of the envelope of the interferogram. 
!!     SAF for a given Optical Path Difference (OPD) is defined as the 
!!     ratio between the interferogram measured for a monochromatic light 
!!     and the theoretical interferogram for a field angle null and a 
!!     punctual detector. 
!!     For symmetry reasons, the SAF is sampled on a grid of an odd 
!!     number points.
!!
!! * Sub-routines and functions
!!
!!     - saf_init  : Initialisation of the type defining the saf in function of the path 
!!     - saf_time  : Computation of the self apodisation function
!!     - saf_time_mertz1 : Computation of each subpixel self apodisation function in the 
!!                         case of Merz with a mirror position error - in this case, the 
!!                         field is parallel to interferometric axis
!!     - saf_time_mertz2 : Computation of each subpixel self apodisation function in the
!!                         case of Merz with a mirror position error, a refractivity error,
!!                         and the prism thickness error - in this case, all field 
!!                         directions are took into account
!!     - saf_opd   : Computation of the envelope of the interferogram sampled by a clock
!!     - saf_agf   : Pixel self apodisation function aggregation
!!     - saf_cal   : Initialisation of complex expression of the saf spectral calibration 
!!     - apf_init  : Initialisation of the type defining apodisation function
!!     - apf_calc  : Computation of the apodisation function
!!     - anrf_calc : Initialisation of the noise reduction factor of Apf type
!!
!! * References
!!
!!     SPS ATBD
!!
! comments: saf_tas_read added 15.03.2012 (JD) read the TAS SAF
!           sas_tas_read added 19.03.2012 (JD) read the TAS SAS
!           saf_opd_tas added 31.05.2012 (JD)  SAS symmetrization and normalization
!           saf_opd_cnestas added 20.12.2012 (JD)  SAS symmetrization and normalization

module saf_computation_module
   use precision_type
   use error_type
   use constantes_type
   use psf_type
   use ccm_type
   use chrom_type
   use fcomp_type
   use srf_param_type
   use saf_type
   use srf_type
   use apf_type
   use pupil_type
   use mertz_type
   use refrac_index_type
   use math_module
   use chromatism_computation_module
   use fcompens_computation_module
   use psf_computation_module
   use fft_module
!
   implicit none
!
!
   public ::                   &
             saf_time          &
            ,saf_opd           &
            ,saf_agf           &
            ,saf_cal           &
            ,apf_calc          &
            ,apf_uncal         &
            ,anrf_calc         &
            ,saf_time_mertz1   &
            ,saf_time_mertz2   &
            ,saf_init          &
            ,apf_init          
!
   contains

!!
!!
!> saf_init -- Public
!!
!! * Purpose
!!
!!     Initialisation of the type defining the saf in function of the path. 
!!     
!!
!! * Description
!!
!!     In function of the path which must be "MES" or "RPD", the parameters
!!     defining the type_Saf are initialized 
!!
!! * Inputs 
!!
!!     - Srf_Param :  type_Srf_Param / type for srf parameters declaration and allocation
!!     - SB        :  spectral band
!!     - Path      :  character / path
!!     - Psf_In    :  type_Psf / type for declaration and allocation of psf
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Saf : type_Saf / type for declaration and allocation of saf
!!
!! * References
!!     SPS ATBD

   subroutine saf_init( Srf_Param, Ccm, SB, Path, Psf_In, Saf )
   implicit none
!
   type(type_Srf_Param), intent(in)                  :: Srf_Param
   type(type_Ccm)      , intent(in)                  :: Ccm
   integer(kind=LONG)  , intent(in)                  :: SB
   character(len=3)    , intent(in)                  :: Path
   type(type_Psf)      , intent(in)                  :: Psf_In
   type(type_Saf)      , intent(out)                 :: Saf
   integer(kind=LONG)                                :: NF
!
     if( Path == 'MES' ) then
        Saf%NsWn0   = Srf_Param%NsWn0(SB)
        Saf%NsPixel = Srf_Param%Mes_NsPsf
        Saf%NsOpd   = Srf_Param%NsOpd
        Saf%OpdMax  = Ccm%OpdMax
        Saf%dOpd    = 2.d+00 * Saf%OpdMax / dble( Saf%NsOpd-1 )
        call alloc_Saf( Saf )
        call Saf_Base( Saf )
        do NF = 1, Saf%NsWn0
           Saf%Wn0(NF) = Srf_Param%Wn0(NF,SB)
        end do
     else if( Path == 'RPD' ) then
        Saf%NsWn0   = Psf_In%NsWn
        Saf%NsPixel = Srf_Param%Rpd_NsPsf
        Saf%NsOpd   = Srf_Param%NsOpd
        Saf%OpdMax  = Ccm%OpdMax
        Saf%dOpd    = 2.d+00 * Saf%OpdMax / dble( Saf%NsOpd-1 )
        call alloc_Saf( Saf )
        call Saf_Base( Saf )
        do NF = 1, Saf%NsWn0
           Saf%Wn0(NF) = Psf_In%Wn(NF)
        end do
     else
        write(*,*) 'Path Error must be MES or RPD : ',Path
        write(*,*) 'saf_init Fatal Error'
        call exit(1)
     end if
!
!    Filter verification
     if( (Srf_Param%Filter /= 'NONE')   .and. &
         (Srf_Param%Filter /= 'LINEAR') .and. &
         (Srf_Param%Filter /= 'COSINE') .and. &
         (Srf_Param%Filter /= 'ERF')    ) then
        write(*,*) 'Srf_Param%Filter Error',Srf_Param%Filter
        write(*,*) 'saf_init Fatal Error'
        call exit(1)
     end if
     return
   end subroutine saf_init

!!
!!
!> saf_time -- Public
!!
!! * Purpose
!!
!!      Computation of each subpixel self apodisation function 
!!
!! * Description
!!
!!      The computation of the self apodisation function requires the following steps:
!!        - The first step is the vectors and matrix allocation and the self apodisation 
!!          function argument initialisation
!!        - The second step consists in the computation of the chromatic offset, if 
!!          required 
!!        - Then, except for the central pixel, the optical field compensation is applied 
!!          in function of active field compensation method which is chosen 
!!        - The norm of the field vector is computed
!!        - The cube corner motion is discretisated and determinated in meter geometric
!!        - the Apex Vector coordinates (x, y, z) are defined by spline interpolation
!!        - Then over the cube corner motion discretisation loop, 
!!               - the SAF real and imaginary parts are initialisated
!!               - the perfect interferogram is defined: it consists in the theoretical 
!!              interferogram for a field angle null and a ponctual detector
!!               - the interferogram measured for a monochromatic light is computed in 
!!                 complex components thanks to a field discretisation loop: 
!!                      - the APEX vector is corrected from chromatism effect and field 
!!                        compensation
!!                      - in field appearent optical path difference is took into account
!!                        in the SAF complex componants
!!        - The complex self apodisation function is computed as the ratio between the 
!!          measured interferogram and the theoretical one; it is formulated in modulus and
!!          argument, for which a regularisation is done (monotone argument) 
!!        - The last step is the vectors and matrix deallocation 
!!
!! * Inputs 
!!
!!     - Srf_Param :  type_Srf_Param / type for srf parameters declaration and allocation
!!     - Path      :  character / path
!!     - Psf_Ref   :  type_Psf / type for declaration and allocation of the psf
!!     - Psf       :  type_Psf / type for declaration and allocation of the psf
!!     - Ccm       :  type_Ccm / type for declaration and allocation of cube corner motion
!!     - Fcomp     :  type_Fcomp / type for declaration and allocation of field compensation
!!     - NF        :  frequencies number
!!     - PN        :  current pixel
!!     - PN_G      :  central pixel for which the field compensation is not required
!!     - RefracIndex : type_RefracIndex / type for declaration and allocation of refractive
!!                     index of materials
!!
!! * Inputs/outputs
!!
!!     - Chrom     :  type_Chrom / type for declaration and allocation of chromatism 
!!     - Saf       :  type_Saf / type for declaration and allocation of saf
!!
!!
!! * Outputs
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.4.1.1
!!

   subroutine saf_time( Srf_Param, Path, Psf_Ref, Psf, Ccm,&
                        Chrom, RefracIndex, Fcomp, NF, PN, PN_G, Saf )
   implicit none
     type(type_Srf_Param),intent(in)                   :: Srf_Param
     character(len=*)    ,intent(in)                   :: Path
     type(type_Psf)      ,intent(in)                   :: Psf_Ref
     type(type_Psf)      ,intent(in)                   :: Psf
     type(type_Ccm)      ,intent(in)                   :: Ccm
     type(type_Chrom)    ,intent(inout)                :: Chrom
     type(type_RefracIndex)  ,intent(in)               :: RefracIndex
     type(type_Fcomp)    ,intent(in)                   :: Fcomp
     integer(kind=LONG)  ,intent(in)                   :: NF
     integer(kind=LONG)  ,intent(in)                   :: PN
     integer(kind=LONG)  ,intent(in)                   :: PN_G
     type(type_Saf)      ,intent(inout)                :: Saf
     integer(kind=LONG)                                :: ErrCode
     integer(kind=LONG)                                :: ioalloc
     real(kind=DOUBLE)                                 :: Arg0
     real(kind=DOUBLE)   ,dimension(:)    ,allocatable :: Arg0_vect
     real(kind=DOUBLE)   ,dimension(:,:,:),allocatable :: OffsetChrom
     real(kind=DOUBLE)   ,dimension(:,:,:),allocatable :: FieldComp
     integer(kind=LONG)                                :: NC
     integer(kind=LONG)                                :: NL
     real(kind=DOUBLE)   ,dimension(:,:)  ,allocatable :: Vnorm
     real(kind=DOUBLE)                                 :: dx
     integer(kind=LONG)                                :: Ns
     integer(kind=LONG)                                :: NZpd
     real(kind=DOUBLE)   ,dimension(:)    ,allocatable :: x
     real(kind=DOUBLE)   ,dimension(:)    ,allocatable :: y
     real(kind=DOUBLE)   ,dimension(:)    ,allocatable :: z
     real(kind=DOUBLE)                                 :: RSaf
     real(kind=DOUBLE)                                 :: ISaf
     complex(kind=DOUBLE)                              :: I0
     real(kind=DOUBLE)                                 :: ApexChromX
     real(kind=DOUBLE)                                 :: ApexChromY
     real(kind=DOUBLE)                                 :: ApexChromZ
     real(kind=DOUBLE)                                 :: xApp
     complex(kind=DOUBLE),dimension(:) ,allocatable    :: Self_Apod
     real(kind=DOUBLE)                                 :: AngWgtS
!
!    allocation
     ioalloc = 0
     allocate( OffsetChrom(3,Psf%NbCol,Psf%NbLin), stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( FieldComp(3,Psf%NbCol,Psf%NbLin),   stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( Vnorm(Psf%NbCol,Psf%NbLin),         stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( x(Saf%NsOpd),                       stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( y(Saf%NsOpd),                       stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( z(Saf%NsOpd),                       stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( Self_Apod(Saf%NsOpd),               stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( Arg0_vect(Saf%NsOpd),               stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     if (ioalloc > 0) then
        write(*,*) 'allocation OffsetChrom Error',ioalloc
        write(*,*) 'saf_computation: fatal error'
        call exit(1)
     end if
     write(*,*) 'Saf%Wn0(NF)',Saf%Wn0(NF)
!
!    Self Apodisation Function Argument initialisation
     Arg0 = 2.d+00 * Pi * Saf%Wn0(NF) * 2.d+00
     if( Path == 'RPD' ) then
        Arg0_vect(1:Saf%NsOpd) =                                         &
              Arg0 * ( 1.d+00+Srf_Param%Rpd_Wn0_Jitter_vect(1:Saf%NsOpd) )
     else if( Path == 'MES' ) then
        Arg0_vect(1:Saf%NsOpd) = Arg0
     else
        write(*,*) 'Path Error must be RPD or MES : ',Path
        write(*,*) 'saf_computation: fatal error'
        call exit(1)
     end if
!
!    Offset chromatique
     if( (Chrom%Alphas /= Chrom%Alphac) .or. (Chrom%Es /= Chrom%Ec) ) then
       write(*,*) 'OffsetChrom'
       call OffsetChrom_sub( Psf_Ref, Psf, Chrom, RefracIndex, &
                             Saf%Wn0(NF), OffsetChrom )
     else
       write(*,*) 'No OffsetChrom'
       OffsetChrom(1:3,1:Psf%NbCol,1:Psf%NbLin) = 0.d+00
     end if
!
!    Optical field compensation (if applied), except for central pixel
     select case (Srf_Param%Mode_Agreg)
     case ('DYN1','DYN2','THEO')
       if (PN_G /= Fcomp%PN_Cen) then
         write(*,*) 'Field Compensation'
         call FieldCompensation_sub( Psf,       &
                                     Srf_Param, &
                                     FieldComp  )
       else
         write(*,*) 'No Field Compensation'
         FieldComp(1:3,1:Psf%NbCol,1:Psf%NbLin) = 0.d+00
       endif
!     case DEFAULT
     case default
       write(*,*) 'No Field Compensation'
       FieldComp(1:3,1:Psf%NbCol,1:Psf%NbLin) = 0.d+00
     end select
!
!    Field Vector Norm
     do NL = 1, Psf%NbLin
        do NC = 1, Psf%NbCol
          Vnorm(NC,NL) = dsqrt( 1.d0 + Psf%Y(NC)*Psf%Y(NC) &
                                     + Psf%Z(NL)*Psf%Z(NL) )
        end do
     end do
!
!    Cube Corner Motion discretisation (m geometric)
     dx = Saf%dOpd / 2.d+00
     NZpd = int(Saf%NsOpd/2)+1
     x(1:Saf%NsOpd) = (/ (dble(Ns-NZpd),Ns=1,Saf%NsOpd) /) * dx
!
!    Apex Vector coordinates (x, y, z)
     call intspline( Ccm%NsCcm,Ccm%ApexX,Ccm%ApexY,Saf%NsOpd,x,y )
     call intspline( Ccm%NsCcm,Ccm%ApexX,Ccm%ApexZ,Saf%NsOpd,x,z )
!
!    CC Motion discretisation loop
     do Ns = 1, Saf%NsOpd
!       SAF Real and Imaginary Parts initialisation
        AngWgtS = 0.d+00
        RSaf = 0.d+00
        ISaf = 0.d+00
!       Perfect interferogram
        I0 = dcmplx(dcos(Arg0*x(Ns)),-dsin(Arg0*x(Ns)))
!       Field discretisation loop
        do NL = 1, Psf%NbLin
           do NC = 1, Psf%NbCol
!             APEX vector correction (Chromatism and field compensation)
              ApexChromX = x(Ns) - OffsetChrom(1,NC,NL) &
                         + FieldComp(1,NC,NL)*(x(Ns)-Fcomp%DX) &
                           * Fcomp%DA_Jitter_Vect(Ns)
              ApexChromY = y(Ns) - OffsetChrom(2,NC,NL) &
                         + FieldComp(2,NC,NL)*(x(Ns)-Fcomp%DX) &
                           * Fcomp%DA_Jitter_Vect(Ns)
              ApexChromZ = z(Ns) - OffsetChrom(3,NC,NL) &
                         + FieldComp(3,NC,NL)*(x(Ns)-Fcomp%DX) &
                           * Fcomp%DA_Jitter_Vect(Ns)
!             In field Appearent Optical Path Difference
              xApp = ( ApexChromX + ApexChromY*Psf%Y(NC)  &
                                  + ApexChromZ*Psf%Z(NL) )&
                     / Vnorm(NC,NL)           
!             SAF Complex components
              RSaf = RSaf + dcos(Arg0_vect(Ns) * xApp) * Psf%AWgt(NC,NL,1)
              ISaf = ISaf + dsin(Arg0_vect(Ns) * xApp) * Psf%AWgt(NC,NL,1)
           end do
        end do  !       End Field discretisation loop
!       Self Apodisation Function complex
        Self_Apod(Ns) = dcmplx(RSaf,-ISaf) / I0
     end do  !    End CC Motion discretisation loop
!
!    modulus
     Saf%Mod(1:Saf%NsOpd,NF,PN) = dsqrt( dreal( Self_Apod(1:Saf%NsOpd) &
                                       * dconjg( Self_Apod(1:Saf%NsOpd) ) ) )
!    complex argument 
     Saf%Arg(1:Saf%NsOpd,NF,PN) = datan2( dimag(Self_Apod(1:Saf%NsOpd)) &
                                         ,dreal(Self_Apod(1:Saf%NsOpd)) )
!    Complex argument regularisation (monotone)
     call phasemono( Saf%Arg(1,NF,PN),Saf%NsOpd )
!
!!!!!!!attention il faut retirer l'axe interferometrique!!!!!!!!!!!!!!!!!!!!!!
     Saf%FieldMeanAngle(NF,PN) = Psf%FieldMeanAngle(1)
!
!    deallocation
     deallocate(OffsetChrom)
     deallocate(FieldComp)
     deallocate(Vnorm)
     deallocate(x)
     deallocate(y)
     deallocate(z)
     deallocate(Self_Apod)
     deallocate(Arg0_vect)
!
     return
   end subroutine saf_time
!
!-------------------------------------------------------------------
!!
!!
!>  saf_time_mertz1 -- Public
!!
!! * Purpose
!!
!!      Computation of each subpixel self apodisation function in the case of Merz with a
!!      mirror position error - in this case, the field is parallel to interferometric axis
!!
!! * Description
!!
!!      The computation of the self apodisation function requires the following steps:
!!        - The first step is the vectors and matrix allocation and the self apodisation 
!!          function argument initialisation
!!        - The second step consists in the computation of the chromatic offset, if 
!!          required 
!!        - Then, except for the central pixel, the optical field compensation is applied 
!!          for the mertz system 
!!        - The norm of the field vector is computed
!!        - The cube corner motion is discretisated and determinated in meter geometric
!!        - the Apex Vector coordinates (x, y, z) are defined by spline interpolation
!!        - Then over the cube corner motion discretisation loop, 
!!               - the SAF real and imaginary parts are initialisated
!!               - the perfect interferogram is defined: it consists in the theoretical 
!!              interferogram for a field angle null and a ponctual detector
!!               - the interferogram measured for a monochromatic light is computed in 
!!                 complex components thanks to a field discretisation loop: 
!!                      - the APEX vector is corrected from chromatism effect and field 
!!                        compensation
!!                      - in field appearent optical path difference is took into account
!!                        in the SAF complex componants
!!        - The complex self apodisation function is computed as the ratio between the 
!!          measured interferogram and the theoretical one; it is formulated in modulus and
!!          argument, for which a regularisation is done (monotone argument) 
!!        - The last step is the vectors and matrix deallocation 
!!
!! * Inputs 
!!
!!     - Srf_Param :  type_Srf_Param / type for srf parameters declaration and allocation
!!     - Path      :  character / path
!!     - Psf_Ref   :  type_Psf / type for declaration and allocation of the psf
!!     - Psf       :  type_Psf / type for declaration and allocation of the psf
!!     - Ccm       :  type_Ccm / type for declaration and allocation of cube corner motion
!!     - Pupil     :  type_Pupil / type for declaration and allocation of pupil
!!     - NF        :  frequencies number
!!     - PN        :  current pixel
!!     - PN_G      :  central pixel for which the field compensation is not required
!!     - RefracIndex : type_RefracIndex / type for declaration and allocation of refractive
!!                     index of materials
!!
!! * Inputs/outputs
!!
!!     - Chrom     :  type_Chrom / type for declaration and allocation of chromatism 
!!     - Fcomp     :  type_Fcomp / type for declaration and allocation of field compensation
!!     - Saf       :  type_Saf / type for declaration and allocation of saf
!!
!!
!! * Outputs
!!
!! * References
!!

   subroutine saf_time_mertz1( Srf_Param, Path, Psf_Ref, Psf, Ccm,&
        Chrom, Pupil, RefracIndex, Fcomp, NF, PN, Saf )
   implicit none
     type(type_Srf_Param),intent(in)                   :: Srf_Param
     character(len=*)    ,intent(in)                   :: Path
     type(type_Psf)      ,intent(in)                   :: Psf_Ref
     type(type_Psf)      ,intent(in)                   :: Psf
     type(type_Ccm)      ,intent(in)                   :: Ccm
     type(type_Chrom)    ,intent(inout)                :: Chrom
     type(type_Fcomp)    ,intent(inout)                :: Fcomp
     type(type_Pupil)    ,intent(in)                   :: Pupil
     type(type_RefracIndex)  ,intent(in)               :: RefracIndex
     integer(kind=LONG)  ,intent(in)                   :: NF
     integer(kind=LONG)  ,intent(in)                   :: PN
     type(type_Saf)      ,intent(inout)                :: Saf
     integer(kind=LONG)                                :: ErrCode
     integer(kind=LONG)                                :: ioalloc
     real(kind=DOUBLE)                                 :: Arg0
     real(kind=DOUBLE)   ,dimension(:)    ,allocatable :: Arg0_vect
     real(kind=DOUBLE)   ,dimension(:,:,:),allocatable :: OffsetChrom
     real(kind=DOUBLE)   ,dimension(:,:,:),allocatable :: FieldComp
     integer(kind=LONG)                                :: NC
     integer(kind=LONG)                                :: NL
     real(kind=DOUBLE)   ,dimension(:,:)  ,allocatable :: Vnorm
     real(kind=DOUBLE)                                 :: dx
     integer(kind=LONG)                                :: Ns
     integer(kind=LONG)                                :: NZpd
     real(kind=DOUBLE)   ,dimension(:)    ,allocatable :: x
     real(kind=DOUBLE)   ,dimension(:)    ,allocatable :: y
     real(kind=DOUBLE)   ,dimension(:)    ,allocatable :: z
     real(kind=DOUBLE)                                 :: RSaf
     real(kind=DOUBLE)                                 :: ISaf
     real(kind=DOUBLE)                                 :: RSaf_field
     real(kind=DOUBLE)                                 :: ISaf_field
     real(kind=DOUBLE)                                 :: RSaf_pupil
     real(kind=DOUBLE)                                 :: ISaf_pupil
     complex(kind=DOUBLE)                              :: I0
     real(kind=DOUBLE)                                 :: ApexChromX
     real(kind=DOUBLE)                                 :: ApexChromY
     real(kind=DOUBLE)                                 :: ApexChromZ
     real(kind=DOUBLE)                                 :: xApp
     complex(kind=DOUBLE),dimension(:) ,allocatable    :: Self_Apod
     real(kind=DOUBLE)                                 :: theta
     real(kind=DOUBLE)                                 :: radius
     real(kind=DOUBLE)                                 :: PupilX
     real(kind=DOUBLE)                                 :: PupilY
     real(kind=DOUBLE)                                 :: PupilZ
     real(kind=DOUBLE)                                 :: N
!
!    allocation
     ioalloc = 0
     allocate( OffsetChrom(3,Psf%NbCol,Psf%NbLin), stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( FieldComp(3,Psf%NbCol,Psf%NbLin),   stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( Vnorm(Psf%NbCol,Psf%NbLin),         stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( x(Saf%NsOpd),                       stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( y(Saf%NsOpd),                       stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( z(Saf%NsOpd),                       stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( Self_Apod(Saf%NsOpd),               stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( Arg0_vect(Saf%NsOpd),               stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     if (ioalloc > 0) then
        write(*,*) 'allocation OffsetChrom Error',ioalloc
        write(*,*) 'saf_computation: fatal error'
        call exit(1)
     end if
     write(*,*) 'Saf%Wn0(NF)',Saf%Wn0(NF)
!
!    Self Apodisation Function Argument initialisation
     Arg0 = 2.d+00 * Pi * Saf%Wn0(NF) * 2.d+00
     if( Path == 'RPD' ) then
        Arg0_vect(1:Saf%NsOpd) =                                         &
              Arg0 * ( 1.d+00+Srf_Param%Rpd_Wn0_Jitter_vect(1:Saf%NsOpd) )
     else if( Path == 'MES' ) then
        Arg0_vect(1:Saf%NsOpd) = Arg0
     else
        write(*,*) 'Path Error must be RPD or MES : ',Path
        write(*,*) 'saf_computation: fatal error'
        call exit(1)
     end if
!
!    Offset chromatique
     if( (Chrom%Alphas /= Chrom%Alphac) .or. (Chrom%Es /= Chrom%Ec) ) then
       write(*,*) 'OffsetChrom'
       call OffsetChrom_sub( Psf_Ref, Psf, Chrom, RefracIndex, &
                             Saf%Wn0(NF), OffsetChrom )
     else
       write(*,*) 'No OffsetChrom'
       OffsetChrom(1:3,1:Psf%NbCol,1:Psf%NbLin) = 0.d+00
     end if
!
!    Optical field compensation
     call FieldCompensation_sub_mertz( Psf ,Fcomp ,RefracIndex ,Srf_Param &
          ,Saf%Wn0(NF) ,FieldComp, N )
!
!    Field Vector Norm
     do NL = 1, Psf%NbLin
        do NC = 1, Psf%NbCol
          Vnorm(NC,NL) = dsqrt( 1.d0 + dsin(Psf%Y(NC))*dsin(Psf%Y(NC)) &
                                     + dsin(Psf%Z(NL))*dsin(Psf%Z(NL)) )
        end do
     end do
!
!    Cube Corner Motion discretisation (m geometric)
     dx = Saf%dOpd / 2.d+00
     NZpd = int(Saf%NsOpd/2)+1
     x(1:Saf%NsOpd) = (/ (dble(Ns-NZpd),Ns=1,Saf%NsOpd) /) * dx
!
!    Apex Vector coordinates (x, y, z)
     call intspline( Ccm%NsCcm,Ccm%ApexX,Ccm%ApexY,Saf%NsOpd,x,y )
     call intspline( Ccm%NsCcm,Ccm%ApexX,Ccm%ApexZ,Saf%NsOpd,x,z )
!
!    CC Motion discretisation loop
     do Ns = 1, Saf%NsOpd
!       SAF Real and Imaginary Parts initialisation
        RSaf_field = real(0,kind=DOUBLE)
        ISaf_field = real(0,kind=DOUBLE)
!       Perfect interferogram ( field + pupil for field null)
        I0 = dcmplx(dcos(Arg0*x(Ns)*(real(1,kind=DOUBLE)+N)),&
             -dsin(Arg0*x(Ns)*(real(1,kind=DOUBLE)+N)))
!       Field discretisation loop
        do NL = 1, Psf%NbLin
           do NC = 1, Psf%NbCol
!             APEX vector correction (Chromatism and field compensation)
              ApexChromX = x(Ns) - OffsetChrom(1,NC,NL)        &
                         + FieldComp(1,NC,NL)*(x(Ns)-Fcomp%DX) &
                           * Fcomp%DA_Jitter_Vect(Ns) + Chrom%ABOpt(NC,NL,Ns)
              ApexChromY = y(Ns) - OffsetChrom(2,NC,NL)        &
                         + FieldComp(2,NC,NL)*(x(Ns)-Fcomp%DX) &
                           * Fcomp%DA_Jitter_Vect(Ns) 
              ApexChromZ = z(Ns) - OffsetChrom(3,NC,NL)        &
                         + FieldComp(3,NC,NL)*(x(Ns)-Fcomp%DX) &
                           * Fcomp%DA_Jitter_Vect(Ns) 
!             In field Appearent Optical Path Difference
              xApp = ( ApexChromX + ApexChromY*dsin(Psf%Y(NC))  &
                                  + ApexChromZ*dsin(Psf%Z(NL)) )&
                     / Vnorm(NC,NL)           
!             SAF Complex componants
              RSaf_field = RSaf_field + ( dcos(Arg0_vect(Ns) * xApp) &
                                                  * Psf%Wgt(NC,NL,1) )
              ISaf_field = ISaf_field + ( dsin(Arg0_vect(Ns) * xApp) &
                                                  * Psf%Wgt(NC,NL,1) )
           end do
        end do  !       End Field discretisation loop
        RSaf_pupil = real(0,kind=DOUBLE)
        ISaf_pupil = real(0,kind=DOUBLE)
        ! Pupil discretisation loop
        do NL = 1, Pupil%NbLin
           do NC = 1, Pupil%NbCol
!             Pupil defects 
              radius = dsqrt( Pupil%Y(NC)*Pupil%Y(NC) &
                            + Pupil%Z(NL)*Pupil%Z(NL) )
              theta  = datan2(Pupil%Y(NC),Pupil%Z(NL))
             
              PupilX = radius * ( Pupil%AlphaY(Ns)*cos(theta)   &
                                - Pupil%AlphaZ(Ns)*sin(theta) ) &
                              + Pupil%WFE(NC,NL,Ns)
              PupilY = real(0,kind=DOUBLE)
              PupilZ = real(0,kind=DOUBLE)        
!             SAF Complex componants
              RSaf_pupil = RSaf_pupil + ( dcos(Arg0_vect(Ns) * PupilX) &
                                        * Pupil%Wgt(NC,NL) )
              ISaf_pupil = ISaf_pupil + ( dsin(Arg0_vect(Ns) * PupilX) &
                                        * Pupil%Wgt(NC,NL) )
           end do
        end do  !       End Pupil discretisation loop
        RSaf = RSaf_field*RSaf_pupil
        ISaf = ISaf_field
!       Self Apodisation Function complex
        Self_Apod(Ns) = dcmplx(RSaf,-ISaf) / I0
     end do  !    End CC Motion discretisation loop
!
!    modulus
     Saf%Mod(1:Saf%NsOpd,NF,PN) = dsqrt( dreal( Self_Apod(1:Saf%NsOpd) &
                                       * dconjg( Self_Apod(1:Saf%NsOpd) ) ) )
!    complex argument 
     Saf%Arg(1:Saf%NsOpd,NF,PN) = datan2( dimag(Self_Apod(1:Saf%NsOpd)) &
                                         ,dreal(Self_Apod(1:Saf%NsOpd)) )
!    Complex argument regularisation (monotone)
     call phasemono( Saf%Arg(1,NF,PN),Saf%NsOpd )
!
     Saf%FieldMeanAngle(NF,PN) = Psf%FieldMeanAngle(1)
!
!    deallocation
     deallocate(OffsetChrom)
     deallocate(FieldComp)
     deallocate(Vnorm)
     deallocate(x)
     deallocate(y)
     deallocate(z)
     deallocate(Self_Apod)
     deallocate(Arg0_vect)
!
     return
   end subroutine saf_time_mertz1
!-------------------------------------------------------------------
!!
!!
!>  saf_time_mertz2 -- Public
!!
!! * Purpose
!!
!!      Computation of each subpixel self apodisation function in the case of Merz with a
!!      mirror position error, a refractivity error, and the prism thickness error - in 
!!      this case, all field directions are took into account
!!
!! * Description
!!
!!      The computation of the self apodisation function requires the following steps:
!!        - The first step is the vectors and matrix allocation and the self apodisation 
!!          function argument initialisation
!!        - The second step consists in the computation of the chromatic offset, if 
!!          required 
!!        - Then, except for the central pixel, the optical field compensation is applied 
!!          for the mertz system 
!!        - The norm of the field vector is computed
!!        - The cube corner motion is discretisated and determinated in meter geometric
!!        - the Apex Vector coordinates (x, y, z) are defined by spline interpolation
!!        - Then over the cube corner motion discretisation loop, 
!!               - the SAF real and imaginary parts are initialisated
!!               - the perfect interferogram is defined: it consists in the theoretical 
!!              interferogram for a field angle null and a ponctual detector
!!               - the interferogram measured for a monochromatic light is computed in 
!!                 complex components thanks to a field discretisation loop: 
!!                      - the APEX vector is corrected from chromatism effect and field 
!!                        compensation
!!                      - in field appearent optical path difference is took into account
!!                        in the SAF complex componants
!!        - The complex self apodisation function is computed as the ratio between the 
!!          measured interferogram and the theoretical one; it is formulated in modulus and
!!          argument, for which a regularisation is done (monotone argument) 
!!        - The last step is the vectors and matrix deallocation 
!!
!! * Inputs 
!!
!!     - Srf_Param :  type_Srf_Param / type for srf parameters declaration and allocation
!!     - Path      :  character / path
!!     - Psf_Ref   :  type_Psf / type for declaration and allocation of the psf
!!     - Psf       :  type_Psf / type for declaration and allocation of the psf
!!     - Ccm       :  type_Ccm / type for declaration and allocation of cube corner motion
!!     - Pupil     :  type_Pupil / type for declaration and allocation of pupil
!!     - NF        :  frequencies number
!!     - PN        :  current pixel
!!     - PN_G      :  central pixel for which the field compensation is not required
!!     - RefracIndex : type_RefracIndex / type for declaration and allocation of refractive
!!                     index of materials
!!
!! * Inputs/outputs
!!
!!     - Chrom     :  type_Chrom / type for declaration and allocation of chromatism 
!!     - Fcomp     :  type_Fcomp / type for declaration and allocation of field compensation
!!     - Saf       :  type_Saf / type for declaration and allocation of saf
!!
!!
!! * Outputs
!!
!! * References
!!

   subroutine saf_time_mertz2( Srf_Param, Path, Psf_Ref, Psf, Ccm,&
                        Chrom, Pupil, RefracIndex, Fcomp, Mertz,  &
                        NF, PN, Saf )
   implicit none
     type(type_Srf_Param),intent(in)                   :: Srf_Param
     character(len=*)    ,intent(in)                   :: Path
     type(type_Psf)      ,intent(in)                   :: Psf_Ref
     type(type_Psf)      ,intent(in)                   :: Psf
     type(type_Ccm)      ,intent(in)                   :: Ccm
     type(type_Chrom)    ,intent(inout)                :: Chrom
     type(type_Fcomp)    ,intent(inout)                :: Fcomp
     type(type_Pupil)    ,intent(in)                   :: Pupil
     type(type_RefracIndex)  ,intent(in)               :: RefracIndex
     type(type_Mertz)    ,intent(in)                   :: Mertz
     integer(kind=LONG)  ,intent(in)                   :: NF
     integer(kind=LONG)  ,intent(in)                   :: PN
     type(type_Saf)      ,intent(inout)                :: Saf
     integer(kind=LONG)                                :: ErrCode
     integer(kind=LONG)                                :: ioalloc
     real(kind=DOUBLE)                                 :: Arg0
     real(kind=DOUBLE)   ,dimension(:)    ,allocatable :: Arg0_vect
     real(kind=DOUBLE)   ,dimension(:,:,:),allocatable :: OffsetChrom
     real(kind=DOUBLE)   ,dimension(:,:,:),allocatable :: FieldComp
     integer(kind=LONG)                                :: NC
     integer(kind=LONG)                                :: NL
     real(kind=DOUBLE)   ,dimension(:,:)  ,allocatable :: Vnorm
     real(kind=DOUBLE)                                 :: dx
     integer(kind=LONG)                                :: Ns
     integer(kind=LONG)                                :: NZpd
     real(kind=DOUBLE)   ,dimension(:)    ,allocatable :: x
     real(kind=DOUBLE)   ,dimension(:)    ,allocatable :: y
     real(kind=DOUBLE)   ,dimension(:)    ,allocatable :: z
     real(kind=DOUBLE)                                 :: RSaf
     real(kind=DOUBLE)                                 :: ISaf
     real(kind=DOUBLE)                                 :: RSaf_field
     real(kind=DOUBLE)                                 :: ISaf_field
     real(kind=DOUBLE)                                 :: RSaf_pupil
     real(kind=DOUBLE)                                 :: ISaf_pupil
     complex(kind=DOUBLE)                              :: I0
     real(kind=DOUBLE)                                 :: ApexChromX
     real(kind=DOUBLE)                                 :: ApexChromY
     real(kind=DOUBLE)                                 :: ApexChromZ
     real(kind=DOUBLE)                                 :: xApp
     complex(kind=DOUBLE),dimension(:) ,allocatable    :: Self_Apod
     real(kind=DOUBLE)                                 :: theta
     real(kind=DOUBLE)                                 :: radius
     real(kind=DOUBLE)                                 :: PupilX
     real(kind=DOUBLE)                                 :: PupilY
     real(kind=DOUBLE)                                 :: PupilZ
     real(kind=DOUBLE)                                 :: N
     real(kind=DOUBLE)                                 :: cos_field
!
!    allocation
     ioalloc = 0
     allocate( OffsetChrom(3,Psf%NbCol,Psf%NbLin), stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( FieldComp(3,Psf%NbCol,Psf%NbLin),   stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( Vnorm(Psf%NbCol,Psf%NbLin),         stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( x(Saf%NsOpd),                       stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( y(Saf%NsOpd),                       stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( z(Saf%NsOpd),                       stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( Self_Apod(Saf%NsOpd),               stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( Arg0_vect(Saf%NsOpd),               stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     if (ioalloc > 0) then
        write(*,*) 'allocation OffsetChrom Error',ioalloc
        write(*,*) 'saf_computation: fatal error'
        call exit(1)
     end if
     write(*,*) 'Saf%Wn0(NF)',Saf%Wn0(NF)
!
!    Self Apodisation Function Argument initialisation
     Arg0 = 2.d+00 * Pi * Saf%Wn0(NF) * 2.d+00
     if( Path == 'RPD' ) then
        Arg0_vect(1:Saf%NsOpd) =                                         &
              Arg0 * ( 1.d+00+Srf_Param%Rpd_Wn0_Jitter_vect(1:Saf%NsOpd) )
     else if( Path == 'MES' ) then
        Arg0_vect(1:Saf%NsOpd) = Arg0
     else
        write(*,*) 'Path Error must be RPD or MES : ',Path
        write(*,*) 'saf_computation: fatal error'
        call exit(1)
     end if
!
!    Offset chromatique
     if( (Chrom%Alphas /= Chrom%Alphac) .or. (Chrom%Es /= Chrom%Ec) ) then
       write(*,*) 'OffsetChrom'
       call OffsetChrom_sub( Psf_Ref, Psf, Chrom, RefracIndex, &
                             Saf%Wn0(NF), OffsetChrom )
     else
       write(*,*) 'No OffsetChrom'
       OffsetChrom(1:3,1:Psf%NbCol,1:Psf%NbLin) = 0.d+00
     end if
!
!    Optical field compensation mertz  system
     call FieldCompensation_sub_mertz( Psf ,Fcomp ,RefracIndex ,Srf_Param &
             ,Saf%Wn0(NF) ,FieldComp, N )
!
!    Field Vector Norm
     do NL = 1, Psf%NbLin
        do NC = 1, Psf%NbCol
          Vnorm(NC,NL) = dsqrt( 1.d0 + dsin(Psf%Y(NC))*dsin(Psf%Y(NC)) &
                                     + dsin(Psf%Z(NL))*dsin(Psf%Z(NL)) )
        end do
     end do
!
!    Cube Corner Motion discretisation (m geometric)
     dx = Saf%dOpd / 2.d+00
     NZpd = int(Saf%NsOpd/2)+1
     x(1:Saf%NsOpd) = (/ (dble(Ns-NZpd),Ns=1,Saf%NsOpd) /) * dx
!
!    Apex Vector coordinates (x, y, z)
     call intspline( Ccm%NsCcm,Ccm%ApexX,Ccm%ApexY,Saf%NsOpd,x,y )
     call intspline( Ccm%NsCcm,Ccm%ApexX,Ccm%ApexZ,Saf%NsOpd,x,z )
!
!    CC Motion discretisation loop
     do Ns = 1, Saf%NsOpd
!       SAF Real and Imaginary Parts initialisation
        RSaf_field = real(0,kind=DOUBLE)
        ISaf_field = real(0,kind=DOUBLE)
        cos_field = real(0,kind=DOUBLE)
!       Perfect interferogram ( field + pupil for field null)
        I0 = dcmplx(dcos(Arg0*x(Ns)*(real(1,kind=DOUBLE)+RefracIndex%N)),&
             -dsin(Arg0*x(Ns)*(real(1,kind=DOUBLE)+RefracIndex%N)))
!       Field discretisation loop
        do NL = 1, Psf%NbLin
           do NC = 1, Psf%NbCol
!             APEX vector correction (Chromatism and field compensation)
              ApexChromX = x(Ns) - OffsetChrom(1,NC,NL)        &
                         + FieldComp(1,NC,NL) *                &
                         RefracIndex%MeanRefracIndex /         &
                         ( RefracIndex%MeanRefracIndex         &
                         - real(1,kind=DOUBLE) ) * ( x(Ns) +   &
                         Mertz%Dthickness(Ns) )                &
                         + Chrom%ABOpt(NC,NL,Ns)
              ApexChromY = y(Ns) - OffsetChrom(2,NC,NL)        &
                         + FieldComp(2,NC,NL)*x(Ns)
              ApexChromZ = z(Ns) - OffsetChrom(3,NC,NL)        &
                         + FieldComp(3,NC,NL)*x(Ns) 
!             In field Appearent Optical Path Difference
              xApp = ( ApexChromX + ApexChromY*dsin(Psf%Y(NC))  &
                                  + ApexChromZ*dsin(Psf%Z(NL)) )&
                     / Vnorm(NC,NL)           
!             SAF Complex componants
              RSaf_field = RSaf_field + ( dcos(Arg0_vect(Ns) * xApp) &
                                                  * Psf%Wgt(NC,NL,1) )
              ISaf_field = ISaf_field + ( dsin(Arg0_vect(Ns) * xApp) &
                                                  * Psf%Wgt(NC,NL,1) )
              cos_field  = cos_field  + ( dcos(Fcomp%Theta(NC,NL)) &
                                                * Psf%Wgt(NC,NL,1) )
           end do
        end do  !       End Field discretisation loop
        RSaf_pupil = real(0,kind=DOUBLE)
        ISaf_pupil = real(0,kind=DOUBLE)
        ! Pupil discretisation loop
        do NL = 1, Pupil%NbLin
           do NC = 1, Pupil%NbCol
!             Pupil defects 
              radius = dsqrt( Pupil%Y(NC)*Pupil%Y(NC) &
                            + Pupil%Z(NL)*Pupil%Z(NL) )
              theta  = datan2(Pupil%Y(NC),Pupil%Z(NL))
             
              PupilX = radius * ( Pupil%AlphaY(Ns)*cos(theta)   &
                                - Pupil%AlphaZ(Ns)*sin(theta) ) &
                     * real(2,kind=DOUBLE)*cos_field + Pupil%WFE(NC,NL,Ns)
              PupilY = real(0,kind=DOUBLE)
              PupilZ = real(0,kind=DOUBLE)        
!             SAF Complex componants
              RSaf_pupil = RSaf_pupil + ( dcos(Arg0_vect(Ns) * PupilX) &
                                                    * Pupil%Wgt(NC,NL) )
              ISaf_pupil = ISaf_pupil + ( dsin(Arg0_vect(Ns) * PupilX) &
                                                    * Pupil%Wgt(NC,NL) )
           end do
        end do  !       End Pupil discretisation loop
        RSaf = RSaf_field*RSaf_pupil
        ISaf = ISaf_field
!       Self Apodisation Function complex
        Self_Apod(Ns) = dcmplx(RSaf,-ISaf) / I0
     end do  !    End CC Motion discretisation loop
!
!    modulus
     Saf%Mod(1:Saf%NsOpd,NF,PN) = dsqrt( dreal( Self_Apod(1:Saf%NsOpd) &
                                       * dconjg( Self_Apod(1:Saf%NsOpd) ) ) )
!    complex argument 
     Saf%Arg(1:Saf%NsOpd,NF,PN) = datan2( dimag(Self_Apod(1:Saf%NsOpd)) &
                                         ,dreal(Self_Apod(1:Saf%NsOpd)) )
!    Complex argument regularisation (monotone)
     call phasemono( Saf%Arg(1,NF,PN),Saf%NsOpd )
!
     Saf%FieldMeanAngle(NF,PN) = Psf%FieldMeanAngle(1)
!
!    deallocation
     deallocate(OffsetChrom)
     deallocate(FieldComp)
     deallocate(Vnorm)
     deallocate(x)
     deallocate(y)
     deallocate(z)
     deallocate(Self_Apod)
     deallocate(Arg0_vect)
     deallocate(Fcomp%Theta)
!     deallocate(RefracIndex%Nper)
!
     return
   end subroutine saf_time_mertz2
!-------------------------------------------------------------------------------
!

!!
!!
!> saf_opd -- Public
!!
!! * Purpose
!!
!!      Computation of the envelope of the interferogram sampled by a clock
!!
!! * Description
!!
!!      The computation of the self apodisation function requires the following steps:
!!       - vector and matrix allocation
!!       - computation of the optical path difference distorsion for the Rpd path
!!       - computation of the optical path difference distorsion for the pixel path
!!       - argument correction
!!       - definition of the value of the argument for optical path null 
!!       - argument normalisation
!!       - computation of the envelope of the interferogram : interferometer contrast 
!!         function 
!!       - deallocation
!!
!! * Inputs 
!!
!!     - Saf     :  type_Saf / type for declaration and allocation of saf
!!     - NF      :  frequencies number
!!     - PN      :  current pixel
!!     - Saf_Rpd :  type_Saf / type for declaration and allocation of saf
!!
!! * Inputs/outputs
!!
!!     - Sas     :  type_Saf / type for declaration and allocation of saf
!!
!! * Outputs
!!
!! * References
!!

   subroutine saf_opd( Srf_Param, Ccm, Saf ,NF ,PN ,Saf_Rpd ,Sas )
   implicit none
     type(type_Srf_Param),intent(in)                   :: Srf_Param
     type(type_Ccm)      ,intent(in)                   :: Ccm
     type(type_Saf)      ,intent(in)                   :: Saf
     integer(kind=LONG)  ,intent(in)                   :: NF
     integer(kind=LONG)  ,intent(in)                   :: PN
     type(type_Saf)      ,intent(in)                   :: Saf_Rpd
     type(type_Saf)      ,intent(inout)                :: Sas
     real(kind=DOUBLE)   ,dimension(:) ,allocatable    :: Opd
     complex(kind=DOUBLE),dimension(:) ,allocatable    :: SasSym
     real(kind=DOUBLE)   ,dimension(:) ,allocatable    :: SasReal
     real(kind=DOUBLE)   ,dimension(:) ,allocatable    :: SasImag
     real(kind=DOUBLE)   ,dimension(:) ,allocatable    :: DddmRpd
     real(kind=DOUBLE)   ,dimension(:) ,allocatable    :: Dddm
     real(kind=DOUBLE)   ,dimension(:) ,allocatable    :: X_tmp
     integer(kind=LONG)                                :: ErrCode
     integer(kind=LONG)                                :: ioalloc
     integer(kind=LONG)                                :: NZpd
     integer(kind=LONG)                                :: Ns
     integer(kind=LONG)                                :: n
     integer(kind=LONG)                                :: m
     integer(kind=LONG)                                :: Half_Central_Part
     character(len=6)                                  :: Edge
     integer(kind=LONG)                                :: NR1
     integer(kind=LONG)                                :: NR2
     real(kind=DOUBLE)                                 :: Arg0
     real(kind=DOUBLE)                                 :: Mod0
     complex(kind=DOUBLE),dimension(:) ,allocatable    :: Sas_C
     real(kind=DOUBLE)   ,dimension(:) ,allocatable    :: Isrf
     integer(kind=LONG)                                :: Sens
     real(kind=DOUBLE)                                 :: dWn
!
!    allocation
     ioalloc = 0
     allocate( X_tmp(Saf%NsOpd),    stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( Sas_C(Saf%NsOpd),    stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( Isrf(Saf%NsOpd),    stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( Opd(Saf%NsOpd),    stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( DddmRpd(Saf%NsOpd),    stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( Dddm(Saf%NsOpd),       stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( SasSym(Sas%NsOpd),     stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( SasReal(Sas%NsOpd),    stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
     allocate( SasImag(Sas%NsOpd),    stat=ErrCode )
     if( ErrCode /= 0 )  ioalloc = ioalloc + 1
!
     if (ioalloc > 0) then
        write(0,*) 'allocation Saf_opd Error'
        write(0,*) 'Saf_opd : fatal error'
        call exit(1)
     end if
!
     Sas%FieldMeanAngle(NF,PN) = Saf%FieldMeanAngle(NF,PN)
     NZpd = int(Saf%NsOpd/2) + 1
!
!    Opd speed variation
     call opd_dvcc( Ccm, Saf, Opd )
!
!    Modulus transfer
     Sas%Mod(1:Sas%NsOpd,NF,PN) = Saf%Mod(1:Saf%NsOpd,NF,PN)
!
!    computation of the optical path difference distorsion for the Rpd path
     DddmRpd(1:Saf%NsOpd) = Saf_Rpd%Arg(1:Saf%NsOpd,1,1) &
                          / ( twoPi*Saf_Rpd%Wn0(1) )
!
!    computation of the optical path difference distorsion for the pixel path
     Dddm(1:Saf%NsOpd) = Saf%Arg(1:Saf%NsOpd,NF,PN) &
                       / ( twoPi*Saf%Wn0(NF) )
     if( Ccm%VCC_Nb_Seg /= 0 ) then
!
!       Speed variation in saf argument
        call intspline( Saf%NsOpd,     Opd, DddmRpd,&
                        Saf%NsOpd, Saf%Opd, X_tmp )
        DddmRpd(1:Saf%NsOpd) = X_tmp(1:Saf%NsOpd)
        call intspline( Saf%NsOpd,     Opd, Dddm,&
                        Saf%NsOpd, Saf%Opd, X_tmp )
        Dddm(1:Saf%NsOpd) = X_tmp(1:Saf%NsOpd)
!
!       Speed variation in saf modulus
        call intspline( Sas%NsOpd,     Opd, Sas%Mod(1,NF,PN),&
                        Sas%NsOpd, Sas%Opd, X_tmp )
        Sas%Mod(1:Saf%NsOpd,NF,PN) = X_tmp(1:Saf%NsOpd)
     end if
!
!    argument correction
     Sas%Arg(1:Saf%NsOpd,NF,PN) =          &
                   ( twoPi * Saf%Wn0(NF) ) &
                 * ( Dddm(1:Saf%NsOpd) - DddmRpd(1:Saf%NsOpd) )
!
!    Complex argument regularisation (monotone)
     call phasemono( Sas%Arg(1,NF,PN),Sas%NsOpd )
!
!    argument at ddm=0
     Sas%Arg0(NF,PN) = Sas%Arg(NZpd,NF,PN)
!
!    interferometer contrast function 
     Sas%Contrast(NF,PN) = Sas%Mod(NZpd,NF,PN)
!
!    Modulus and argument normalisation
     Sas%Mod(1:Sas%NsOpd,NF,PN) = Sas%Mod(1:Sas%NsOpd,NF,PN) &
                                / Sas%Contrast(NF,PN)
     Sas%Arg(1:Sas%NsOpd,NF,PN) = Sas%Arg(1:Sas%NsOpd,NF,PN) &
                                - Sas%Arg0(NF,PN)
     if( PN == 1 .and. NF == 1 ) then
        do Ns = 1, Sas%NsOpd
           write(*,'(I4,3f10.6,e12.4)') &
           Ns, Opd(Ns), Sas%Opd(Ns), Sas%Mod(Ns,NF,PN), Sas%Arg(Ns,NF,PN)
        end do
     end if
!
!    Symetrisation function of Single/Double Side interferometer
     if( Srf_Param%Scan_Mode == 'DOUBLE_SIDED' ) then
!
!       Convert sas (mod arg) to complex
        call modargtocplx( Sas_C, Sas%Mod(1,NF,PN),    &
                           Sas%Arg(1,NF,PN), Sas%NsOpd )
        
!
!       Fourier Transform in the spectrum space
        Sens = -1
        dWn = 1_DOUBLE / (2_DOUBLE*Sas%OpdMax)
        call fft_c2r( Sas%NsOpd-1, Sens, Sas_C, Sas%dOpd, dWn, Isrf )
!
!       Back to the interferogram space
        Sens = 1
        call fft_r2c( Sas%NsOpd-1, Sens, Isrf, dWn, Sas%dOpd, Sas_C )
!
!       convert sas complex to mod arg
        call cplxtomodarg( Sas_C, Sas%Mod(1,NF,PN),    &
                           Sas%Arg(1,NF,PN), Sas%NsOpd )
        Sas%Arg(1:Sas%NsOpd,NF,PN) = - Sas%Arg(1:Sas%NsOpd,NF,PN)
!
!       Double side symetrisation
!        call modargtocplx( SasSym, Sas%Mod(1,NF,PN),   &
!                           Sas%Arg(1,NF,PN), Sas%NsOpd )
!        call cplxtorealimag( SasSym, SasReal, SasImag, Sas%NsOpd )
!        do Ns = 0, NZpd-1
!           n = NZpd + Ns
!           m = NZpd - Ns
!           SasSym(n) = dcmplx( (SasReal(n)+SasReal(m))/2.d+00,&
!                               (SasImag(n)-SasImag(m))/2.d+00 )
!           SasSym(m) = dconjg( SasSym(n) )
!        end do
!        call cplxtomodarg( SasSym, Sas%Mod(1,NF,PN),   &
!                           Sas%Arg(1,NF,PN), Sas%NsOpd )
!
!       Complex argument regularisation (monotone)
        call phasemono( Sas%Arg(1,NF,PN),Sas%NsOpd )
        Arg0 = Sas%Arg(NZpd,NF,PN)
        Sas%Arg(1:Sas%NsOpd,NF,PN) = Sas%Arg(1:Sas%NsOpd,NF,PN) &
                                   - Arg0
!
!       argument at ddm=0
!        Sas%Arg0(NF,PN) = Sas%Arg(NZpd,NF,PN)
!
!       interferometer contrast function 
!        Sas%Contrast(NF,PN) = Sas%Mod(NZpd,NF,PN)
!
!       Modulus and argument normalisation
!        Sas%Mod(1:Sas%NsOpd,NF,PN) = Sas%Mod(1:Sas%NsOpd,NF,PN) &
!                                   / Sas%Contrast(NF,PN)
!        Sas%Arg(1:Sas%NsOpd,NF,PN) = Sas%Arg(1:Sas%NsOpd,NF,PN) &
!                                   - Sas%Arg0(NF,PN)
!
     else if( Srf_Param%Scan_Mode == 'SINGLE_SIDED' ) then
!
!       Single side symetrisation
!
!       Mertz filtering
        Edge = 'LEFT'
        Half_Central_Part = idnint( (Sas%NsOpd-1)        &
                          * Srf_Param%Ratio_bilatere / 2 )
        NR1 = NZpd - Half_Central_Part
        NR2 = NZpd + Half_Central_Part
        if( Srf_Param%Filter == 'LINEAR' ) then
           call smoothlinear( NR2-NR1+1, Edge,       &
                              Sas%Opd(NR1:NR2),      &
                              Sas%Mod(NR1:NR2,NF,PN) )
        else if( Srf_Param%Filter == 'COSINE' ) then
           call smoothcosine( NR2-NR1+1, Edge,       &
                              Sas%Opd(NR1:NR2),      &
                              Sas%Mod(NR1:NR2,NF,PN) )
        else if( Srf_Param%Filter == 'ERF' ) then
           call smootherf( NR2-NR1+1, Edge,       &
                           Sas%dOpd,              &
                           Sas%Opd(NR1:NR2),      &
                           Sas%Mod(NR1:NR2,NF,PN) )
        end if
!
!       Complex argument regularisation (monotone)
!        call phasemono( Sas%Arg(1,NF,PN),Sas%NsOpd )
!
!       argument at ddm=0
!        Sas%Arg0(NF,PN) = Sas%Arg(NZpd,NF,PN)
!
!       interferometer contrast function 
!        Sas%Contrast(NF,PN) = Sas%Mod(NZpd,NF,PN)
!
!       Modulus and argument normalisation
!        Sas%Mod(1:Sas%NsOpd,NF,PN) = Sas%Mod(1:Sas%NsOpd,NF,PN) &
!                                   / Sas%Contrast(NF,PN)
!        Sas%Arg(1:Sas%NsOpd,NF,PN) = Sas%Arg(1:Sas%NsOpd,NF,PN) &
!                                   - Sas%Arg0(NF,PN)
!
!       SAS symetrisation
        Sas%Mod(1:NZpd,NF,PN) =   Sas%Mod(Sas%NsOpd:NZpd:-1,NF,PN)
        Sas%Arg(1:NZpd,NF,PN) = - Sas%Arg(Sas%NsOpd:NZpd:-1,NF,PN)
!
        call modargtocplx( SasSym, Sas%Mod(1,NF,PN),   &
                           Sas%Arg(1,NF,PN), Sas%NsOpd )
        call cplxtorealimag( SasSym, SasReal, SasImag, Sas%NsOpd )
        do Ns = 0, NZpd-1
           n = NZpd + Ns
           m = NZpd - Ns
           SasSym(n) = dcmplx( (SasReal(n)+SasReal(m))/2.d+00,&
                               (SasImag(n)-SasImag(m))/2.d+00 )
           SasSym(m) = dconjg( SasSym(n) )
        end do
        call cplxtomodarg( SasSym, Sas%Mod(1,NF,PN),   &
                           Sas%Arg(1,NF,PN), Sas%NsOpd )
!
!       Complex argument regularisation (monotone)
        call phasemono( Sas%Arg(1,NF,PN),Sas%NsOpd )
        Arg0 = Sas%Arg(NZpd,NF,PN)
        Sas%Arg(1:Sas%NsOpd,NF,PN) = Sas%Arg(1:Sas%NsOpd,NF,PN) &
                                   - Arg0
        Mod0 = Sas%Mod(NZpd,NF,PN)
        Sas%Mod(1:Sas%NsOpd,NF,PN) = Sas%Mod(1:Sas%NsOpd,NF,PN) &
                                   / Mod0
!
!       argument at ddm=0
!        Sas%Arg0(NF,PN) = Sas%Arg(NZpd,NF,PN)
!
!       interferometer contrast function 
!        Sas%Contrast(NF,PN) = Sas%Mod(NZpd,NF,PN)
!
!       Modulus and argument normalisation
!        Sas%Mod(1:Sas%NsOpd,NF,PN) = Sas%Mod(1:Sas%NsOpd,NF,PN) &
!                                   / Sas%Contrast(NF,PN)
!        Sas%Arg(1:Sas%NsOpd,NF,PN) = Sas%Arg(1:Sas%NsOpd,NF,PN) &
!                                   - Sas%Arg0(NF,PN)
     else
        write(*,*) 'Srf_Param%Scan_Mode Error :',Srf_Param%Scan_Mode
        write(*,*) 'Saf_opd : fatal error'
        call exit(1)
     end if
!
!    deallocation
     deallocate( SasReal )
     deallocate( SasImag )
     deallocate( SasSym )
     deallocate( DddmRpd )
     deallocate( Dddm )
     deallocate( Opd )
     deallocate( X_tmp )
     deallocate( Sas_C )
     deallocate( Isrf )
!
     return
   end subroutine saf_opd
!
!
   subroutine opd_dvcc( Ccm, Saf, Opd )
   implicit none
     type(type_Ccm)   ,intent(in)                           :: Ccm
     type(type_Saf)   ,intent(in)                           :: Saf
     real(kind=DOUBLE),intent(inout),dimension(1:Saf%NsOpd) :: Opd
     real(kind=DOUBLE)            ,dimension(:),allocatable :: Opd_tmp
     integer(kind=LONG)           ,dimension(:),allocatable :: Ns_Start
     integer(kind=LONG)           ,dimension(:),allocatable :: Ns_Stop
     real(kind=DOUBLE)                                      :: dVCCAvg
     real(kind=DOUBLE)                                      :: dVCCStd
     real(kind=DOUBLE)                                      :: OpdMax
     real(kind=DOUBLE)                                      :: OpdZpd
     integer(kind=LONG)                                     :: Seg
     integer(kind=LONG)                                     :: n1
     integer(kind=LONG)                                     :: n2
     integer(kind=LONG)                                     :: Ns
     real(kind=DOUBLE)                                      :: dVCC
!
     if( Ccm%VCC_Nb_Seg /= 0 ) then
!
!       linear variation per segment
        write(*,*) 'Corner cube speed variation linear per segment'
        allocate(  Ns_Start(Ccm%VCC_Nb_Seg) )
        allocate(  Ns_Stop(Ccm%VCC_Nb_Seg) )
        allocate(  Opd_tmp(Saf%NsOpd) )
        dVCCAvg = 0.d+00
        dVCCStd = 0.d+00
        OpdMax = 0.d+00
        Opd_tmp(1:Saf%NsOpd) = Saf%Opd(1:Saf%NsOpd) - Saf%Opd(1)
        do Seg = 1, Ccm%VCC_Nb_Seg
           n1 = 1
           n2 = Saf%NsOpd
           OpdMax = OpdMax &
                  + 2*Saf%OpdMax*Ccm%VCC_Course(Seg)
           call dichotomd( OpdMax, Opd_tmp(n1:n2),n1,n2 )
           if( Seg == 1 ) then
              Ns_Start(Seg) = 1
              Ns_Stop(Seg)  = n2
           else if( (Seg > 1) .and. (Seg == Ccm%VCC_Nb_Seg) ) then
              Ns_Start(Seg) = Ns_Stop(Seg-1) + 1
              Ns_Stop(Seg)  = Saf%NsOpd
           else
              Ns_Start(Seg) = Ns_Stop(Seg-1) + 1
              Ns_Stop(Seg)  = n2
           end if
           write(*,*) 'Ns Start Stop',Ns_Start(Seg), Ns_Stop(Seg)
           do Ns = Ns_Start(Seg), Ns_Stop(Seg)
              dVCC =   Ccm%VCC_A0(Seg) &
                   + ( Ccm%VCC_A1(Seg) * Opd_tmp(Ns) )
              dVCCAvg = dVCCAvg + dVCC
              dVCCStd = dVCCStd + dVCC**2
              Opd(Ns) = 2_DOUBLE * ( Ccm%VCC+dVCC )&
                                 * ( Opd_tmp(Ns)/(2_DOUBLE*Ccm%VCC) )
              if( Ns == Ns_Start(Seg) ) then
                 write(*,*) 'dVCC Start    ',Seg, dVCC, Ccm%VCC+dVCC
              else if( Ns == Ns_Stop(Seg) ) then
                 write(*,*) 'dVCC Stop     ',Seg, dVCC, Ccm%VCC+dVCC
              end if
           end do
        end do
!
!       Opd centered
!!!        OpdZpd = Opd(int(Saf%NsOpd/2)+1)
!!!        OpdZpd = sum(Opd(1:Saf%NsOpd))/ real(Saf%NsOpd,DOUBLE)
        OpdZpd = Opd(Saf%NsOpd) / 2_DOUBLE
        Opd(1:Saf%NsOpd) = Opd(1:Saf%NsOpd) - OpdZpd
        deallocate( Ns_Start )
        deallocate( Ns_Stop )
        deallocate( Opd_tmp )
        dVCCAvg = dVCCAvg / dble(Saf%NsOpd)
        dVCCStd = dsqrt( (dVCCStd/dble(Saf%NsOpd)) - dVCCAvg**2 )
        write(*,'(a,2f12.9,a)') ' Cube Corner speed variation Avg Std :' &
                           , dVCCAvg, dVCCStd,' m/s'
     else
        write(*,*) 'No corner cube speed variation'
        Opd(1:Saf%NsOpd) = Saf%Opd(1:Saf%NsOpd)
     end if
     return
   end subroutine opd_dvcc 

!!
!!
!> saf_agf -- Public
!!
!! * Purpose
!!
!!      Pixel self apodisation function aggregation
!!
!! * Description
!!
!!      The computation of the self apodisation function requires the following steps:
!!      - vectors and matrix allocation and initialisation 
!!      - in function of the aggregation mode which is selected 
!!            - a loop over the pixels for their aggregation
!!            - computaion of the field mean angle, the value of the argument for 
!!              optical path null and the contrast
!!            - the complex argument regularisation is done (monotone argument) 
!!
!! * Inputs 
!!
!!     - Mode_Agreg    : Aggregation mode (can be SOFT/HARD/DYN1/DYN2/THEO)
!!     - Calib_Option  : flag to choose for the saf spectral calibration the 
!!                       use of the spectral shift or the argument slope
!!     - NF    :  frequencies number 
!!     - Srf   :  type_Srf / type for declaration and allocation of srf       
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Sas   : type_Saf / type for declaration and allocation of saf
!!
!! * References
!!

   subroutine saf_agf( Srf_Param, NF, Srf, Psf_In, Sas )
   implicit none
     type(type_Srf_Param),intent(in)                  :: Srf_Param
     integer(kind=LONG)  ,intent(in)                  :: NF
     type(type_Srf)      ,intent(in)                  :: Srf
     type(type_Psf)      ,intent(in),dimension(Srf%NsPixel):: Psf_In
     type(type_Saf)      ,intent(inout)               :: Sas
     complex(kind=DOUBLE)                             :: czero
     complex(kind=DOUBLE),dimension(:) ,allocatable   :: SafC
     complex(kind=DOUBLE),dimension(:) ,allocatable   :: SafA
     integer(kind=LONG)                               :: SUB_PN
     integer(kind=LONG)                               :: PNA
     integer(kind=LONG)                               :: NZpd
     integer(kind=LONG)                               :: ErrCode
!
!
     czero = dcmplx(0.d00,0.d00)
     allocate( SafC(Sas%NsOpd),    stat=ErrCode  )
     allocate( SafA(Sas%NsOpd),    stat=ErrCode  )
     SafA(1:Sas%NsOpd) = czero
     PNA = Sas%NsPixel+1
     NZpd = int(Sas%NsOpd/2)+1
     Sas%FieldMeanAngle(NF,PNA) = 0_DOUBLE
     Sas%Arg0(NF,PNA)           = 0_DOUBLE
     Sas%Contrast(NF,PNA)       = 0_DOUBLE
!
     select case (Srf_Param%Mode_Agreg)
     case ('HARD','DYN1','DYN2','THEO','MER1','MER2')
!       pixel loop to be aggregated
        do SUB_PN = 1, Sas%NsPixel
           call modargtocplx( SafC,Sas%Mod(1,NF,SUB_PN)      &
                             ,Sas%Arg(1,NF,SUB_PN),Sas%NsOpd )
           SafA(1:Sas%NsOpd) = SafA(1:Sas%NsOpd) &
                             + ( SafC(1:Sas%NsOpd) / dble(Sas%NsPixel) )
!!!!           Sas%FieldMeanAngle(NF,SUB_PN) =                            &
!!!!                               dacos( 1_DOUBLE - Srf%Fcs1a(NF,SUB_PN) )
        end do
!       end sub pixel loop
!
!       cpnversion Modulus Argument
        call cplxtomodarg(SafA,Sas%Mod(1,NF,PNA),Sas%Arg(1,NF,PNA),Sas%NsOpd)
!
!       complex argument regularisation (monotone argument)
        call phasemono( Sas%Arg(1,NF,PNA),Sas%NsOpd )
!
!       computation of Sas%FieldMeanAngle, Arg0 and Contrast
        Sas%FieldMeanAngle(NF,PNA) = dacos( &
                           sum( dcos( Sas%FieldMeanAngle(NF,1:Sas%NsPixel) ) )&
                           / dble(Sas%NsPixel) )
        Sas%Contrast(NF,PNA)        = Sas%Mod(NZpd,NF,PNA)
        Sas%Arg0(NF,PNA)            = Sas%Arg(NZpd,NF,PNA)
        Sas%Mod(1:Sas%NsOpd,NF,PNA) = Sas%Mod(1:Sas%NsOpd,NF,PNA) &
                                    / Sas%Contrast(NF,PNA)
        Sas%Arg(1:Sas%NsOpd,NF,PNA) = Sas%Arg(1:Sas%NsOpd,NF,PNA) &
                                    - Sas%Arg0(NF,PNA)
!
     case ('SOFT')
!       sub pixel loop to be aggregated
        do SUB_PN = 1, Sas%NsPixel
           call saf_cal( Srf_Param, Psf_In(SUB_PN), NF, &
                         SUB_PN, Srf, Sas, SafC )
           SafA(1:Sas%NsOpd) = SafA(1:Sas%NsOpd) &
                             + ( SafC(1:Sas%NsOpd) / dble(Sas%NsPixel) )
!!!!           Sas%FieldMeanAngle(NF,SUB_PN) =                            &
!!!!                               dacos( 1_DOUBLE - Srf%Fcs1a(NF,SUB_PN) )
        end do
!       end sub pixel loop
!
!       cpnversion Modulus Argument
        call cplxtomodarg(SafA,Sas%Mod(1,NF,PNA),Sas%Arg(1,NF,PNA),Sas%NsOpd)
!
!       complex argument regularisation (monotone argument)
        call phasemono( Sas%Arg(1,NF,PNA),Sas%NsOpd )
!
!       Approximation for aggregated "fieldangle"
        Sas%FieldMeanAngle(NF,PNA)  = 0_DOUBLE
        Sas%Contrast(NF,PNA)        = Sas%Mod(NZpd,NF,PNA)
        Sas%Arg0(NF,PNA)            = Sas%Arg(NZpd,NF,PNA)
        Sas%Mod(1:Sas%NsOpd,NF,PNA) = Sas%Mod(1:Sas%NsOpd,NF,PNA) &
                                    / Sas%Contrast(NF,PNA)
        Sas%Arg(1:Sas%NsOpd,NF,PNA) = Sas%Arg(1:Sas%NsOpd,NF,PNA) &
                                    - Sas%Arg0(NF,PNA)
     case default
        write(*,*) 'Agregation Mode Error',Srf_Param%Mode_Agreg
        write(*,*) 'not defined'
        call exit(1)
     end select
!
     deallocate( SafC )
     deallocate( SafA )
     return
   end subroutine saf_agf
!!
!!
!> saf_cal -- Public
!!
!! * Purpose
!!
!!      Initialisation of complex expression of the saf spectral calibration 
!!
!! * Description
!!
!!      The computation of the self apodisation function requires the following steps:
!!       - determination of the sampling basis adapted to saf spectral calibration 
!!       - in function of the flag "Calib_Option"
!!             - use of spectral shift for the saf spectral calibration 
!!               and cubic spline interpolation
!!             - use of argument slope for the saf spectral calibration 
!!               and cubic spline interpolation
!!
!! * Inputs 
!!
!!     - Calib_Option  : flag to choose for the saf spectral calibration the 
!!                       use of the spectral shift or the argument slope
!!     - NF   :  frequencies number 
!!     - PN   :  current pixel  
!!     - Sas  :  type_Saf / type for declaration and allocation of saf
!!     - Srf  :  type_Srf / type for declaration and allocation of srf
!!
!! * Inputs/outputs
!!
!!     - SasC :  complex expression of saf
!!
!! * Outputs
!!
!! * References
!!

   subroutine saf_cal( Srf_Param, Psf_In, NF, PN, Srf, Sas, SasC )

   implicit none
     type(type_Srf_Param),intent(in)                           :: Srf_Param
     type(type_Psf)      ,intent(in)                           :: Psf_In
     integer(kind=LONG)  ,intent(in)                           :: NF
     integer(kind=LONG)  ,intent(in)                           :: PN
     type(type_Srf)      ,intent(in)                           :: Srf
     type(type_Saf)      ,intent(in)                           :: Sas
     type(type_Psf)                                            :: Psf_Out
     complex(kind=DOUBLE),intent(inout),dimension(1:Sas%NsOpd) :: SasC
     real(kind=DOUBLE)                                         :: WnShift
!
!    Psf recentering
!     call psf_psp( -Srf_Param%AxeY, -Srf_Param%AxeZ, Psf_In, Psf_Out )
!
!    Nominal Spectral Shift
!!     WnShift = Sas%Wn0(NF) * (1.d+00 - dcos( Psf_Out%FieldMeanAngle(1) ) )
     WnShift = Srf%WnShift1a(NF,PN)
!
!    use of spectral shift for the saf spectral calibration 
     call modargtocplx( SasC, Sas%Mod(1,NF,PN),     &
                        Sas%Arg(1,NF,PN), Sas%NsOpd )
     SasC(1:Sas%NsOpd) = SasC(1:Sas%NsOpd)                 &
        * dcmplx( dcos(twoPi*Sas%Opd(1:Sas%NsOpd)*WnShift),&
                 -dsin(twoPi*Sas%Opd(1:Sas%NsOpd)*WnShift) )
!
!     call dalloc_Psf( Psf_Out )
!
     return
   end subroutine saf_cal
!!
!!
!> apf_init -- Public
!!
!! * Purpose
!!
!!     Initialisation of the type defining apodisation function
!!
!! * Description
!!
!! * Inputs 
!!
!!     - Srf_Param :  type_Srf_Param / type for srf parameters declaration and allocation
!!     - SB        :  spectral band 
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Apf  :  type_Apf / type for declaration and allocation of apodisation function
!!
!! * References
!!

   subroutine apf_init( Srf_Param, SB, Apf )
   implicit none
!
   type(type_Srf_Param), intent(in)                  :: Srf_Param
   integer(kind=LONG)  , intent(in)                  :: SB
   type(type_Apf)      , intent(out)                 :: Apf
   integer(kind=LONG)                                :: NF
!
     Apf%NsWn0   = Srf_Param%NsWn0(SB)
     Apf%NsPixel = Srf_Param%Mes_NsPsf
     Apf%NsOpd   = Srf_Param%NsOpd
     Apf%OpdMax  = Srf_Param%OpdMax
     Apf%dOpd = 2.d+00 * Apf%OpdMax / dble( Apf%NsOpd-1 )
     call alloc_Apf( Apf )
     do NF = 1, Apf%NsWn0
        Apf%Wn0(NF) = Srf_Param%Wn0(NF,SB)
     end do
     call Apf_Base( Apf )
     return
   end subroutine apf_init
!!
!!
!> apf_calc -- Public
!!
!! * Purpose
!!
!!     Computation of the apodisation function
!!
!! * Description
!!
!!     The apodisation function is defined as the ratio between the Fourier Transform of
!!     the theoretical ISRF and the Fourier Transform of the ISRF centred on its barycenter.
!!     The apodisation function is computed for each spectral sample. Its computation 
!!     requires the following steps:
!!       - determination of the sampling basis, spectral calibration taken into account  
!!       - spectral calibration and normalization of self-apodisation function
!!       - cubic spline interpolation of spectral calibrated self-apodisation 
!!         function on the Apf opd basis
!!       - cubic spline interpolation of L1cTF on the Apf opd basis
!!       - apodisation function computation
!!       - computation of the Apf noise reduction factor
!!
!! * Inputs 
!!
!!     - NF   :  frequencies number
!!     - PN   :  pixel id
!!     - Sas  :  type_Saf / type for declaration and allocation of saf
!!     - Srf  :  type_Srf / type for declaration and allocation of srf
!!
!! * Inputs/outputs
!!
!!     - Apf  :  type_Apf / type for declaration and allocation of apodisation function
!!
!! * Outputs
!!
!! * References
!!

   subroutine apf_calc( NF, PN, Sas, Srf, Apf )
   implicit none
     integer(kind=LONG)  ,intent(in)                           :: NF
     integer(kind=LONG)  ,intent(in)                           :: PN
     type(type_Saf)      ,intent(in)                           :: Sas
     type(type_Srf)      ,intent(in)                           :: Srf
     type(type_Apf)      ,intent(inout)                        :: Apf
     integer(kind=LONG)                                        :: Ns
     integer(kind=LONG)                                        :: NZpd
     real(kind=DOUBLE)                                         :: dOpdC
     real(kind=DOUBLE)                                         :: Mod0
     real(kind=DOUBLE)                                         :: Arg0
     real(kind=DOUBLE)   ,dimension(:) ,allocatable            :: OpdCal
     complex(kind=DOUBLE),dimension(:) ,allocatable            :: SasCal
     real(kind=DOUBLE)   ,dimension(:) ,allocatable            :: SasCalMod
     real(kind=DOUBLE)   ,dimension(:) ,allocatable            :: SasCalArg
     real(kind=DOUBLE)   ,dimension(:) ,allocatable            :: SasMod
     real(kind=DOUBLE)   ,dimension(:) ,allocatable            :: SasArg
     integer(kind=LONG)                                        :: ErrCode
!
     allocate( OpdCal(Sas%NsOpd),     stat=ErrCode )
     allocate( SasCal(Sas%NsOpd),     stat=ErrCode )
     allocate( SasCalMod(Sas%NsOpd),  stat=ErrCode )
     allocate( SasCalArg(Sas%NsOpd),  stat=ErrCode )
     allocate( SasMod(Sas%NsOpd),     stat=ErrCode )
     allocate( SasArg(Sas%NsOpd),     stat=ErrCode )
!
     NZpd = int(Sas%NsOpd/2) + 1
!!!!!     dOpdC = Sas%dOpd * dcos(Sas%FieldMeanAngle(NF,PN))
     dOpdC = Sas%dOpd
     OpdCal(1:Sas%NsOpd) = (/ (dble(Ns-NZpd),Ns=1,Sas%NsOpd) /) * dOpdC 
!
!    spectral calibration and normalization of self-apodisation function (Sas) 
     call modargtocplx( SasCal, Sas%Mod(1,NF,PN), Sas%Arg(1,NF,PN), Sas%NsOpd )
     SasCal(1:Sas%NsOpd) = SasCal(1:Sas%NsOpd) &
           * dcmplx( dcos(twoPi*OpdCal(1:Sas%NsOpd)*Srf%WnShift1b(NF,PN)) &
                   ,-dsin(twoPi*OpdCal(1:Sas%NsOpd)*Srf%WnShift1b(NF,PN)) )
!
     call cplxtomodarg( SasCal, SasCalMod, SasCalArg, Sas%NsOpd )
     call phasemono( SasCalArg, Sas%NsOpd )
     Arg0 = SasCalArg(NZpd)
     SasCalArg(1:Sas%NsOpd) = SasCalArg(1:Sas%NsOpd) - Arg0
!
!    cubic spline interpolation of Sas and L1cTF on the Apf opd basis
     call intspline( Sas%NsOpd, OpdCal, SasCalMod,&
                     Apf%NsOpd, Apf%Opd, SasMod   )
     call intspline( Sas%NsOpd, OpdCal, SasCalArg,&
                     Apf%NsOpd, Apf%Opd, SasArg   )
     call intspline( Sas%NsOpd, OpdCal, Srf%L1cTF, &
                     Apf%NsOpd, Apf%Opd, Apf%L1cTF )
!
!    apodisation function computation
     NZpd = int( Apf%NsOpd/2 ) + 1
     Apf%Mod(1:Apf%NsOpd,NF,PN) = 1_DOUBLE          &
                                / SasMod(1:Apf%NsOpd)
     Apf%Arg(1:Apf%NsOpd,NF,PN) = SasArg(1:Apf%NsOpd)
     Mod0 = Apf%Mod(NZpd,NF,PN)
     Arg0 = Apf%Arg(NZpd,NF,PN)
     Apf%Mod(1:Apf%NsOpd,NF,PN) = Apf%Mod(1:Apf%NsOpd,NF,PN) / Mod0
     Apf%Arg(1:Apf%NsOpd,NF,PN) = Apf%Arg(1:Apf%NsOpd,NF,PN) - Arg0
!
!    Computation of the Apf noise reduction factor
     call anrf_calc( NF, PN, Apf )
!
!    deallocation
     deallocate( OpdCal )
     deallocate( SasCal )
     deallocate( SasCalMod )
     deallocate( SasCalArg )
     deallocate( SasMod )
     deallocate( SasArg )
!
     return
   end subroutine apf_calc
!
!
   subroutine apf_uncal( Srf, Apf, Apf_Uncalib )
   implicit none
     type(type_Srf)      ,intent(in)                :: Srf
     type(type_Apf)      ,intent(in)                :: Apf
     type(type_Apf)      ,intent(inout)             :: Apf_Uncalib
     integer(kind=LONG)                             :: NF
     integer(kind=LONG)                             :: PN
!
     do PN = 1, Apf%NsPixel+1
        do NF = 1, Apf%NsWn0
!
!       Spectral de calibration
           Apf_Uncalib%Arg(1:Apf%NsOpd,NF,PN) =                       &
                    Apf%Arg(1:Apf%NsOpd,NF,PN)                        &
                   +( twoPi*Apf%Opd(1:Apf%NsOpd)*Srf%WnShift1a(NF,PN) )
        end do
     end do
     return
   end subroutine apf_uncal
!!
!!
!> anrf_calc -- Public
!!
!! * Purpose
!!
!!     Initialisation of the noise reduction factor of Apf type
!!
!! * Description
!!
!!     Computation of the noise reduction factor of the apodisation process
!!
!! * Inputs 
!!
!!     - NF    :  frequencies number
!!     - PN    :  pixel id
!!
!! * Inputs/outputs
!!
!!     - Apf   :  type_Apf / type for declaration and allocation of Apf
!!
!! * Outputs
!!
!! * References
!!

   subroutine anrf_calc( NF, PN, Apf )
   implicit none
     integer(kind=LONG)  ,intent(in)                           :: NF
     integer(kind=LONG)  ,intent(in)                           :: PN
     type(type_Apf)      ,intent(inout)                        :: Apf
!
!    compute noise reduction factor of the apodisation process
     Apf%Nrf(NF,PN) = dsqrt( sum( ( Apf%Mod(1:Apf%NsOpd,NF,PN)    &
                                   *Apf%L1cTF(1:Apf%NsOpd) )**2 ) &
                             * Apf%dOpd / (2.d+00*Apf%OpdMax)     )
     return
   end subroutine anrf_calc
!
!
end module saf_computation_module
