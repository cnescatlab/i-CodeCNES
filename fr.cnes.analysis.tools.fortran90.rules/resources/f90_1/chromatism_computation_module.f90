!!# chromatism_computation_module.f90 --
!!#
!!#           Project: SPS_GENERIC
!!#           Authors: NOVELTIS/B.TOURNIER
!!#              Date: october 2009
!!#           Version: $Revision: 1.3 $
!!# Last modification: $Date: 2012-02-08 10:19:17 $
!!#
!!# Language:  F90
!!# Standards: Noveltis
!!#
!!# --
!!#
!! 
!> chromatism_computation_module -- Module
!!
!! * Purpose
!!
!!     Module for Chromatism computation.
!!
!! * Description
!!      
!!     The Apex vector displaying the interferometric axis for a given position of the
!!     moving cube corner is defined as the vector joining the two apexes of the cube 
!!     corner in the exit space of the interferometer. 
!!     In fact the two cube corners are separated by the separating and the compensating 
!!     plates. 
!!     In the OPD computation, the optical paths induced by the plates crossing in 
!!     function of their thickness, refractive index and the slope of the beam (field 
!!     angle) have to be taken into account.
!!     The refractive index is dependent of the wavelength so the self apodisation 
!!     function is also dependent of the wavelength, the transformation is called the 
!!     chromatic offset vector (chromatic deviation).  
!!     The chromatic offset is null at a reference wavelength.
!!
!! * Sub-routines and functions
!!
!!     - OffsetChrom_sub   : computation of the chromatic offset 
!!     - CalcIndice        : computation of ZnSe refractive index for a given frequency
!!     - CalcOffsetChrom   : computation of the chromatic offset from the fixed and mobil
!!                           cube corner image deviations
!!     - CalcCprim         : computation of the cube corner image deviation
!!     - Vrotation         : computation of the deviation vector obtained by rotation 
!!                           around the Z axis 
!!     - chromatism_init   : initialisation of the type defining the chromatism 
!!
!! * References
!!
!!     NOV-3820-NT-10018 : IASI-NG SPS ATBD
!!

module chromatism_computation_module
   use precision_type
   use error_type
   use psf_type
   use ccm_type
   use chrom_type
   use refrac_index_type  
   use srf_param_type

!
   implicit none
!
!
   public :: OffsetChrom_sub &
            ,CalcIndice      &
            ,CalcOffsetChrom &
            ,CalcCprim       &
            ,Vrotation       &
            ,chromatism_init
!
   contains
!!
!!
!> OffsetChrom_sub -- Public
!!
!! * Purpose
!!
!!      Computation of the chromatic offset
!!
!! * Description
!!
!!      The chromatic offset is computed as the deviation vector of the two apexes 
!!      (Offset) : 
!!       - the vector offset displays the cube corner images deviation induced by the OPD
!!         in the plates of the two arms of the interferometer. It gives an OPD equivalent
!!         to the OPD written in the interferometer space induced by the index of the 
!!         material in fonction of the wave number and the field.
!!       - this vector is cancelled for the wave number of reference (alignment laser) 
!!         and for the field direction null.
!!
!! * Inputs
!!
!!     - Psf_Ref : type_Psf  / type for declaration and allocation of Psf (point spread 
!!                 function)
!!     - Psf     : type_Psf  / type for declaration and allocation of Psf (point spread 
!!                 function)
!!     - RefracIndex : type_RefracIndex / type for declaration and allocation material 
!!                     refractive index initialisation
!!     - WnSaf   : self apodisation function wavenumber basis  
!!
!! * Inputs/outputs
!!
!!     - Chrom       : type_Chrom / type for declaration and allocation of chromatism 
!!     - OffsetChrom : value of the chromatic offset over the field discretisation points
!!
!! * Outputs
!!
!! * References
!!

   subroutine OffsetChrom_sub( Psf_Ref ,Psf ,Chrom ,RefracIndex, WnSaf ,OffsetChrom )

   implicit none
     type(type_Psf)    ,intent(in)                :: Psf_Ref
     type(type_Psf)    ,intent(in)                :: Psf
     type(type_Chrom)  ,intent(inout)             :: Chrom     
     type(type_RefracIndex)  ,intent(in)          :: RefracIndex
     real(kind=DOUBLE) ,intent(in)                :: WnSaf
     real(kind=DOUBLE) ,intent(inout)             &
          ,dimension(1:3,1:Psf%NbCol,1:Psf%NbLin) :: OffsetChrom
     integer(kind=LONG)                           :: NC
     integer(kind=LONG)                           :: NL
     real(kind=DOUBLE)                            :: N
     real(kind=DOUBLE) ,dimension(:) ,allocatable :: CCfmean
     real(kind=DOUBLE) ,dimension(:) ,allocatable :: CCmmean
     real(kind=DOUBLE) ,dimension(:) ,allocatable :: OffsetChromMeanRef
     real(kind=DOUBLE) ,dimension(:) ,allocatable :: OffsetChromMean
     real(kind=DOUBLE) ,dimension(:) ,allocatable :: OffChromRef
     real(kind=DOUBLE) ,dimension(:) ,allocatable :: OffChrom
     integer(kind=LONG)                           :: ErrCode
!
     allocate( CCfmean(3),            stat=ErrCode )
     allocate( CCmmean(3),            stat=ErrCode )
     allocate( OffsetChromMeanRef(3), stat=ErrCode )
     allocate( OffsetChromMean(3),    stat=ErrCode )
     allocate( OffChromRef(3),        stat=ErrCode )
     allocate( OffChrom(3),           stat=ErrCode )
     if (ErrCode > 0) then
        write(0,*) 'allocation OffsetChrom_sub Error'
        write(0,*) 'OffsetChrom_sub: fatal error'
        call Err_outputErrorMessage(0)
        call exit(1)
     end if
!
!    material refractive index at the reference frequency (alignement Laser)
     call CalcIndice( Chrom%Material, RefracIndex ,Chrom%Wn0 ,N )
!
!    offset chromatique de reference
     CCfmean(1:3) = 0.d+00
     CCmmean(1:3) = 0.d+00
     NC = Psf_Ref%NbCol
     NL = Psf_Ref%NbLin
     call CalcOffsetChrom( Psf_Ref     &
                          ,Chrom       &
                          ,N           &
                          ,NC          &
                          ,NL          &
!
                          ,CCfmean     &
                          ,CCmmean     &
                          ,OffChromRef )
     OffsetChromMeanRef(1:3) = CCmmean(1:3) - CCfmean(1:3)
!
!    material refractive index at the wave number of calculation
     call CalcIndice( Chrom%Material, RefracIndex, WnSaf ,N )
!
!    boucle sur les points de discretisation du champ de vue
     CCfmean(1:3) = 0.d+00
     CCmmean(1:3) = 0.d+00
     do NL = 1, Psf%NbLin
        do NC = 1, Psf%NbCol
!          offset chromatique
           call CalcOffsetChrom( Psf      &
                                ,Chrom    &
                                ,N        &
                                ,NC       &
                                ,NL       &
!
                                ,CCfmean  &
                                ,CCmmean  &
                                ,OffChrom )
           OffsetChrom(1:3,NC,NL) = OffChrom(1:3) - OffChromRef(1:3)
        end do
     end do
     OffsetChromMean(1:3) = CCmmean(1:3) - CCfmean(1:3) &
                          - OffsetChromMeanRef(1:3)
!
!    deallocation
     deallocate(CCfmean)
     deallocate(CCmmean)
     deallocate(OffsetChromMeanRef)
     deallocate(OffsetChromMean)
     deallocate(OffChromRef)
     deallocate(OffChrom)
     return
   end subroutine OffsetChrom_sub
!
!------------------------------------------------------------------------
! Purpose: Calculates refractive index in material at
!          wave length Wn 
!
   subroutine CalcIndice( Material, RefracIndex ,Wn ,N )
     implicit none
     character(len=4)        ,intent(in)       :: Material
     type(type_RefracIndex)  ,intent(in)       :: RefracIndex
     real(kind=DOUBLE)       ,intent(in)       :: Wn
     real(kind=DOUBLE)       ,intent(out)      :: N
     real(kind=DOUBLE)                         :: s
     real(kind=DOUBLE)                         :: sums
     real(kind=DOUBLE)                         :: lambda
     integer(kind=SIMPLE)                      :: NCoef
     integer(kind=SIMPLE)                      :: mat ! material ZnSe/KBr
     !
     select case (Material)

     case ('ZnSe') 
        mat = 1

     case ('KBr ')
        mat = 2

     end select
     lambda = 1.d+06 / Wn
     sums = 0.d+00
     do NCoef = 1, RefracIndex%NCoef(mat)
        s = ( RefracIndex%A(NCoef,mat) * lambda**2 )  &
          / ( lambda**2 - RefracIndex%B(NCoef,mat)**2 )
        sums = sums + s
     end do
     N = dsqrt( RefracIndex%C(mat)+sums )
!
     return
   end subroutine CalcIndice
!!
!!
!> CalcOffsetChrom -- Public
!!
!! * Purpose
!!
!!      Computation of the chromatic offset from the fixed cube corner image deviation 
!!      and the mobil cube corner image deviation
!!
!! * Description
!! 
!!      The chromatic offset for a monochromatic wave, for a field direction and for one
!!      position of the mobile cube corner 
!!      is computed as the difference between the deviation vectors of the plate of the 
!!      mobile and fixed cube corners images:
!!       - the first step is to define the field direction unit vector in the 
!!         interferometer frame, 
!!       - as second step, the fixed cube corner image deviation is computed,
!!       - thirdly, the mobil cube corner image deviation is computed, 
!!       - then the chromatic offset is computed from the knowledge of both the fixed and
!!         mobil cube corner images deviations.
!!
!! * Inputs
!!
!!     - Psf   : type_Psf  / type for declaration and allocation of Psf (point spread 
!!               function)
!!     - Chrom : type_Chrom / type for declaration and allocation of chromatism 
!!     - N     : refractive index of the material
!!     - NC    : PSF sample number along the Y direction
!!     - NL    : PSF sample number along the Z direction
!!
!! * Inputs/outputs
!!
!!     - CCfmean     : image deviation for the fixed cube corner weighted by the PSF 
!!                     (along the 3 directions) 
!!     - CCmmean     : image deviation for the mobil cube corner weighted by the PSF 
!!                     (along the 3 directions) 
!!     - OffsetChrom : chromatic offset  
!!
!! * Outputs
!!
!! * References
!!

   subroutine CalcOffsetChrom( Psf ,Chrom ,N ,NC ,NL         &
                              ,CCfmean ,CCmmean ,OffsetChrom )
   implicit none
     type(type_Psf)    ,intent(in)                  :: Psf
     type(type_Chrom)  ,intent(in)                  :: Chrom
     real(kind=DOUBLE) ,intent(in)                  :: N
     integer(kind=LONG),intent(in)                  :: NC
     integer(kind=LONG),intent(in)                  :: NL
     real(kind=DOUBLE) ,intent(inout) ,dimension(3) :: CCfmean
     real(kind=DOUBLE) ,intent(inout) ,dimension(3) :: CCmmean
     real(kind=DOUBLE) ,intent(inout) ,dimension(3) :: OffsetChrom
     real(kind=DOUBLE) ,dimension(:) ,allocatable   :: Vi
     real(kind=DOUBLE) ,dimension(:) ,allocatable   :: CCf
     real(kind=DOUBLE) ,dimension(:) ,allocatable   :: CCm
     integer(kind=LONG)                             :: ErrCode
!!
!!    field unit vector in the interferometer frame  
     allocate(Vi(3),  stat=ErrCode)
     allocate(CCf(3), stat=ErrCode)
     allocate(CCm(3), stat=ErrCode)
     if (ErrCode > 0) then
        write(0,*) 'allocation CalcOffsetChrom Error'
        write(0,*) 'CalcOffsetChrom: fatal error'
        call Err_outputErrorMessage(0)
        call exit(1)
     end if
!
     Vi(1) = dsqrt( 1-Psf%Y(NC)**2 - Psf%Z(NL)**2 )
     Vi(2) = Psf%Y(NC)
     Vi(3) = Psf%Z(NL)
!!
!!    fixed cube corner image deviation 
     call CalcCprim( Vi ,Chrom%Alphac ,N ,Chrom%Ec ,CCf )
!!
!!    mobil cube corner image deviation 
     call CalcCprim( Vi ,Chrom%Alphas ,N ,Chrom%Es ,CCm )
!!
!!    chromatic offset
     OffsetChrom(1:3) = CCm(1:3) - CCf(1:3)
     CCmmean(1:3) = CCm(1:3)*Psf%Wgt(NC,NL,1)
     CCfmean(1:3) = CCf(1:3)*Psf%Wgt(NC,NL,1)
!
!    deallocation
     deallocate(Vi)
     deallocate(CCf)
     deallocate(CCm)
    return
   end subroutine CalcOffsetChrom
!!
!!
!> CalcCprim -- Public
!!
!! * Purpose
!!
!!      Computation of the cube corner image deviation  
!!
!! * Description
!!
!!      The computation of the cube corner image deviation requires the following steps:
!!        - The computation of the field vector Vn in the coordinates system normal to 
!!          the plate: Vn is obtained by a rotation of mean field angle of the 
!!          interferometer all around the axis Z, 
!!        - The computation of the Vn deviation due to the material: the path into the 
!!          plate of the field vector is 
!!          modified by the refractive index of the material,
!!        - The computation of the glass crossing lenght which corresponds to the length 
!!          of the path into the material; it depends of the thickness of the plate and 
!!          the component along the interferometric axis X of the deviation vector,
!!        - The computation of the equivalent lenght corresponding to the optical path in 
!!          vacuum equivalent to the path 
!!          into the material in the coordinates system normal to the plate,
!!        - The computation of the path into the material in the coordinates system normal
!!          to the plate (glass crossing vector) which is function of the length of the 
!!          path into the material,
!!        - The computation of the equivalent vector to the glass crossing which displays
!!          the equivalent optical path in the coordinates system normal to the plate 
!!          toward the field direction; it is written as the multiplication of the field 
!!          direction vector by the equivalent length in vacuum of the path into the 
!!          material.
!!        - The computation of the cube corner image deviation vector in the coordinates
!!          system normal to the plate
!!        - Finally, the computation of the deviation vector in the interferometer frame.
!!
!! * Inputs
!!
!!     - Vi    : mean field angle of the interferometer
!!     - Alpha : rotation angle
!!     - N     : refractive index of the material
!!     - E     : thickness of the plate
!!
!! * Inputs/outputs
!!
!!     - CC    : deviation vector in the interferometer frame 
!!
!! * Outputs
!!
!! * References
!!

   subroutine CalcCprim( Vi ,Alpha ,N ,E ,CC )
   implicit none
     real(kind=DOUBLE) ,intent(in)    ,dimension(3) :: Vi
     real(kind=DOUBLE) ,intent(in)                  :: Alpha
     real(kind=DOUBLE) ,intent(in)                  :: N
     real(kind=DOUBLE) ,intent(in)                  :: E
     real(kind=DOUBLE) ,intent(inout) ,dimension(3) :: CC
     integer(kind=LONG)                             :: Axe
     real(kind=DOUBLE) ,dimension(:),allocatable    :: Vn
     real(kind=DOUBLE) ,dimension(:),allocatable    :: Un
     real(kind=DOUBLE)                              :: Eprim
     real(kind=DOUBLE)                              :: Esecond
     real(kind=DOUBLE) ,dimension(:),allocatable    :: Uprimn
     real(kind=DOUBLE) ,dimension(:),allocatable    :: Usecondn
     real(kind=DOUBLE) ,dimension(:),allocatable    :: CCprim
     integer(kind=LONG)                             :: ErrCode
!
     allocate(Vn(3)      ,  stat=ErrCode)
     allocate(Un(3)      ,  stat=ErrCode)
     allocate(Uprimn(3)  ,  stat=ErrCode)
     allocate(Usecondn(3),  stat=ErrCode)
     allocate(CCprim(3)  ,  stat=ErrCode)
     if (ErrCode > 0) then
        write(0,*) 'allocation CalcCprim Error'
        write(0,*) 'CalcCprim: fatal error'
        call Err_outputErrorMessage(0)
        call exit(1)
     end if
!!
!!    Vn = Vi transformation in the frame which is normal to the beamsplitter 
     Axe = 3
     call Vrotation( Vi ,Axe ,Alpha ,Vn )

!!    Un = Vn deviation due to the material 
     Un(2) = Vn(2) / N
     Un(3) = Vn(3) / N
     Un(1) = dsqrt( 1-Un(2)**2-Un(3)**2 )
!!
!!    glass crossing lenght  
     Eprim = E / Un(1)
!!
!!    equivalent lenght 
     Esecond = Eprim * N
!!
!!    glass crossing vector
     Uprimn(1:3) = Un(1:3) * Eprim
!!
!!    equivalent vector to the glass crossing
     Usecondn(1:3) = Vn(1:3) * Esecond
!!
!!    cube corner image deviation vector
     CCprim(1:3) = Usecondn(1:3) - Uprimn(1:3)
!!
!!    deviation vector in the interferometer frame
     Axe = 3
     call Vrotation( CCprim ,Axe ,-Alpha ,CC )
!
!    deallocation
     deallocate(Vn)
     deallocate(Un)
     deallocate(Uprimn)
     deallocate(Usecondn)
     deallocate(CCprim)
     return
   end subroutine CalcCprim
!!
!!

!> Vrotation -- Public
!!
!! * Purpose
!!
!!      Computation of the deviation vector obtained by rotation around the Z axis  
!!
!! * Description
!! 
!!      The deviation vector in the exit space of the interferometer is obtained by a 
!!      rotation Alpha of mean slope of the field on the plate around the axis Axe, of
!!      the deviation vector expressed in the coordinates system normal to the plate.
!!
!! * Inputs
!!
!!     - Vin   : deviation vector defined in coordinates system normal to the plate
!!     - Axe   : rotation axis
!!     - Alpha : rotation angle  
!!
!! * Inputs/outputs
!!
!!     - Vout  : deviation vector in the exit space of the interferometer obtained after
!!               rotation around Axe
!!
!! * Outputs
!!
!! * References
!!

   subroutine Vrotation( Vin ,Axe ,Alpha ,Vout )
   implicit none
     real(kind=DOUBLE) ,intent(in)    ,dimension(3) :: Vin
     integer(kind=LONG),intent(in)                  :: Axe
     real(kind=DOUBLE) ,intent(in)                  :: Alpha
     real(kind=DOUBLE) ,intent(inout) ,dimension(3) :: Vout
     real(kind=DOUBLE) ,dimension(:,:) ,allocatable :: matrot
     integer(kind=LONG)                             :: ErrCode
     real(kind=DOUBLE)                              :: sx
     real(kind=DOUBLE)                              :: cx
     integer(kind=LONG)                             :: n
     integer(kind=LONG)                             :: m
     real(kind=DOUBLE)                              :: v
!
!    allocation
     allocate( matrot(3,3), stat=ErrCode )
     matrot(1:3,1:3)=0.
!
     if (ErrCode > 0) then
        write(0,*) 'allocation Vrotation Error'
        write(0,*) 'Vrotation : fatal error'
        call Err_outputErrorMessage(0)
        call exit(1)
     end if
     sx=dsin(Alpha)
     cx=dcos(Alpha)
!!
!!    Axe vector
     matrot(Axe,Axe) = 1.d+00
!!
!!    Axe+1 vector
     n=mod(Axe,3)+1
     m=mod(Axe+1,3)+1
     matrot(n,n)   =  cx
     matrot(m,m)   =  cx
     matrot(n,m)   = -sx
     matrot(m,n)   =  sx
!
     do n = 1, 3
        v = 0.d+00
        do m = 1, 3
           v = v + matrot(n,m) * Vin(m)
        end do
        Vout(n) = v
      end do
!
!    deallocation
     deallocate(matrot)
     return
   end subroutine Vrotation
!!
!!
!> chromatism_init -- Public
!!
!! * Purpose
!!
!!      Initialisation of the type defining the chromatism 
!!
!! * Description
!!
!! * Inputs
!!
!!     - File_Chromatism : character / input file with definition of chromatism parameters
!!     - Psf_Ref   : type_Psf  / type for declaration and allocation of Psf (point 
!!                   spread function)
!!     - Srf_Param : type_Srf_Param / type for SRF parameters declaration and allocation
!!
!! * Inputs/outputs
!!
!!     - Chrom : type_Chrom / type for declaration and allocation of chromatism 
!!
!! * Outputs
!!
!! * References
!!

   subroutine chromatism_init( File_Chromatism, &
                               Psf_Ref,         &
                               Srf_Param,       &
                               Chrom           )
     character(len=*)  , intent(in)                  :: File_Chromatism
     type(type_Psf)    , intent(in)                  :: Psf_Ref
     type(type_Srf_Param), intent(in)                :: Srf_Param
     type(type_Chrom)  , intent(inout)               :: Chrom
     integer(kind=LONG)                              :: iFile
     integer(kind=LONG)                              :: iPos
     integer(kind=LONG)                              :: ErrCode
!
     iFile = 10
     iPos  = 1
     open(unit=iFile, file=File_Chromatism, status='old', err=999 )
     iPos  = 2
     Chrom%filename = File_Chromatism
     Chrom%Wn0 = Psf_Ref%Wn(1)
     read(iFile,*,err=999) Chrom%Material 
     write(*,*) 'Chrom%Material ',Chrom%Material
     read(iFile,*,err=999) Chrom%Alphas
     read(iFile,*,err=999) Chrom%Es
     read(iFile,*,err=999) Chrom%Alphac
     read(iFile,*,err=999) Chrom%Ec
     Chrom%Alphas = Chrom%Alphas / 180.d+00 * Pi
     Chrom%Alphac = Chrom%Alphac / 180.d+00 * Pi
     write(*,*) 'ABOpt matrix', Psf_Ref%NbCol,Psf_Ref%NbLin,Srf_Param%NsOpd
     allocate( Chrom%ABOpt(Psf_Ref%NbCol,Psf_Ref%NbLin,Srf_Param%NsOpd),&
                                                             stat=ErrCode )
     if (ErrCode /= 0) then
        write(0,*) 'allocation ABOpt Error'
        call exit(1)
     end if    
     Chrom%ABOpt(:,:,:) = real(0,kind=DOUBLE)
     close(unit=iFile)
!
     return
999  write(*,*) 'parameters reading error'
     write(*,*) 'chromatism_init Fatal Error'
     call exit(1)
   end subroutine chromatism_init
!
!
end module chromatism_computation_module
