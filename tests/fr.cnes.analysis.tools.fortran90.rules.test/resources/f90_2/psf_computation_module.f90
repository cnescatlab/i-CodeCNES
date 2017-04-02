!!# psf_computation_module.f90 --
!!#
!!#           Project: SPS_GENERIC
!!#           Authors: NOVELTIS/B.TOURNIER
!!#              Date: october 2009
!!#           Version: $Revision: 1.5 $
!!# Last modification: $Date: 2011-11-02 14:57:53 $
!!#
!!# Language:  F90
!!# Standards: Noveltis
!!#
!!# --
!!#
!! 
!> psf_computation_module -- Module
!!
!! * Purpose
!!
!!      Module for Point Spread Function (PSF) computation.
!!
!! * Description
!!      
!!      This module defines all the functions that allows to define and to
!!      update the Psf in function of different configuration of its use.
!!
!! * Sub-routines and functions
!!
!! - psf_magnif   :   Update of the Psf for a given magnification 
!! - psf_pixel    :   Update of the Psf for a given pixel 
!! - psf_gain     :   Detector gain took into account into the PSF computation
!! - psf_interpol :   Interpolation of the Point Spread Function 
!! - psf_psp      :   Update of the Psf with a interferometric axis shift
!! - psf_index    :   Computation of difference between two PSFs
!! - psf_init     :   Initialisation of the Point Spread Function
!!
!! * References
!!
!!     
!!
!!
module psf_computation_module
   use precision_type
   use error_type
   use constantes_type
   use srf_param_type
   use psf_type
   use ccm_type
   use saf_type
   use gain_type
   use math_module
!
   implicit none
!
!
   public :: psf_magnif         &
            ,psf_interpol       &
            ,psf_psp            &
            ,psf_pixel          &
            ,psf_gain           &
            ,psf_index          &
            ,fieldmeanangle_calc&
            ,psf_init
!
   contains
!
!
!!
!!
!> psf_magnif -- Public
!!
!! * Purpose
!!
!!     Update of the Point Spread Function for a given magnification 
!!
!! * Description
!!
!!     This subroutine updates the Psf parameters for given magnification along Y and Z axis
!!
!! * Inputs
!!
!!    - MagnificationY : Psf magnification along the Y axis 
!!    - MagnificationZ : Psf magnification along the Z axis 
!!    - Psf_In    : type_Psf / type for declaration and allocation of Psf
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!    - Psf_Out : type_Psf / type for declaration and allocation of Psf
!!
!! * References
!!
   subroutine psf_magnif( MagnificationY, MagnificationZ, Psf_In, Psf_Out )
   implicit none
     real(kind=DOUBLE) , intent(in)               :: MagnificationY
     real(kind=DOUBLE) , intent(in)               :: MagnificationZ
     type(type_Psf)    , intent(in)               :: Psf_In
     type(type_Psf)    , intent(out)              :: Psf_Out
     integer(kind=LONG)                           :: NF
!
     Psf_Out%NsWn   = Psf_In%NsWn
     Psf_Out%NbCol  = Psf_In%NbCol
     Psf_Out%NbLin  = Psf_In%NbLin
     call alloc_Psf( Psf_Out )
     Psf_Out = Psf_In
     Psf_Out%MagnificationY = MagnificationY
     Psf_Out%MagnificationZ = MagnificationZ
!
     Psf_Out%AY(1:Psf_Out%NbCol) = Psf_Out%AY(1:Psf_Out%NbCol)   &
                                 * Psf_Out%MagnificationY
     Psf_Out%AZ(1:Psf_Out%NbLin) = Psf_Out%AZ(1:Psf_Out%NbLin)   &
                                 * Psf_Out%MagnificationZ
!
     do NF = 1, Psf_Out%NsWn
        Psf_Out%BaryCentreY(NF)    = 0.d+00
        Psf_Out%BaryCentreZ(NF)    = 0.d+00
        Psf_Out%FieldMeanAngle(NF) = 0.d+00
     end do
!
     return
   end subroutine psf_magnif
!!
!!
!> psf_interpol -- Public
!!
!! * Purpose
!!
!!     Interpollation of the Point Spread Function 
!!
!! * Description
!!
!!    This subroutine allows to interpollate the Psf thanks to the following steps : 
!!    - The first step is the determination of the indices of wavenumbers on which Psf is 
!!      defined adjacent to the ISRF wave number
!!    - Then, dimensions of the undersampled Psf are defined.
!!    - When the Psf_Out tables are allocated, spectral interpolation of Psf and spatial 
!!      averaging according to the undersampling factor is done ; 
!!    - If ISRF wavenumber is identical to a PSF wavenumber (n1=n2) no spectral 
!!      interpolation is applied.
!!    - A last, the PSF coordinates are undersampled along Y and Z axis
!!
!! * Inputs
!!
!!    - NF  : spectral wavenumber numerous
!!    - Saf : type_Saf / type for declaration and allocation of Saf
!!
!! * Inputs/outputs
!!
!!    - Psf_In  : type_Psf / type for declaration and allocation of Psf
!!
!! * Outputs
!!
!!    - Psf_Out : type_Psf / type for declaration and allocation of Psf
!!
!! * References
!!
   subroutine psf_interpol( NF, Saf, Psf_In, Psf_Out )
   implicit none
     integer(kind=LONG)  ,intent(in)              :: NF
     type(type_Saf)      ,intent(in)              :: Saf
     type(type_Psf)      ,intent(inout)           :: Psf_In
     type(type_Psf)      ,intent(out)             :: Psf_Out
     integer(kind=LONG)                           :: n1
     integer(kind=LONG)                           :: n2
     integer(kind=LONG)                           :: NCI
     integer(kind=LONG)                           :: NLI
     integer(kind=LONG)                           :: NSamp2
     integer(kind=LONG)                           :: NC
     integer(kind=LONG)                           :: NL
     integer(kind=LONG)                           :: IC
     integer(kind=LONG)                           :: IL
     integer(kind=LONG)                           :: ILS
     integer(kind=LONG)                           :: ILE
     integer(kind=LONG)                           :: ICS
     integer(kind=LONG)                           :: ICE
     integer(kind=LONG)                           :: NFC
     integer(kind=LONG)                           :: NFL
     real(kind=DOUBLE)                            :: PsfNorm
     real(kind=DOUBLE)  ,dimension(:),allocatable :: PsfI
     real(kind=DOUBLE)  ,dimension(:),allocatable :: tab
!
!    Determine the indices of wavenumbers on which PSF is defined
!    adjacent to the ISRF wave number
     n1 = 1
     n2 = Psf_In%NsWn
     call dichotomd( Saf%Wn0(NF), Psf_In%Wn, n1, n2 )
!
!    Dimensions of the undersampled PSF
!    for NbCol=NbLin=301, only NSamp=3,5,or 15 are allowed for instance
     if( Psf_In%NbCol .ne. 301 .or. &
         Psf_In%NbLin .ne. 301 .or. &
        (Psf_In%NSamp .ne. 3 .and.  &
         Psf_In%NSamp .ne. 5 .and.  &
         Psf_In%NSamp .ne. 1) ) then
        Psf_In%NSamp = 1
     end if
     NCI = int( (Psf_In%NbCol-1)/Psf_In%NSamp ) + 1
     NLI = int( (Psf_In%NbLin-1)/Psf_In%NSamp ) + 1
!
     Psf_Out%NsWn = 1
     Psf_Out%NbCol  = NCI
     Psf_Out%NbLin  = NLI
     call alloc_Psf( Psf_Out )
!
!    Spectral interpolation of PSF and spatial averaging
!    according to the undersampling factor
!    if ISRF wavenumber is identical to a PSF wavenumber (n1=n2)
!    no spectral interpolation is applied
     NSamp2 = int( (Psf_In%NSamp+1)/2 )
     PsfNorm = 0.d0 
     allocate(tab(2))
     allocate(PsfI(1))
!   
     do NL = 1, Psf_In%NbLin, Psf_In%NSamp
        IL = int( (NL-1)/Psf_In%NSamp ) + 1
        ILS = max((1-NSamp2),(1-NL))
        ILE = min((Psf_In%NSamp-NSamp2),(Psf_In%NbLin-NL))

        do NC = 1, Psf_In%NbCol, Psf_In%NSamp
           IC = int( (NC-1)/Psf_In%NSamp ) + 1
           ICS = max((1-NSamp2),(1-NC))
           ICE = min((Psf_In%NSamp-NSamp2),(Psf_In%NbCol-NC))

           Psf_Out%Wgt(IC,IL,1) = 0.d0
           do NFL = ILS, ILE

              do NFC=ICS,ICE
                 if (n1 .eq. n2) then
                    PsfI(1) = Psf_In%Wgt(NC+NFC,NL+NFL,n1)
                 else 
                    tab(1) = Psf_In%Wgt(NC+NFC,NL+NFL,n1)
                    tab(2) = Psf_In%Wgt(NC+NFC,NL+NFL,n2)
                    call inttabdble( tab,Psf_In%Wn(n1),2  &
                                    ,PsfI(1),Saf%Wn0(NF),1 )
                 end if
                 Psf_Out%Wgt(IC,IL,1) = Psf_Out%Wgt(IC,IL,1) + PsfI(1)
              end do
           end do

           PsfNorm = PsfNorm + Psf_Out%Wgt(IC,IL,1)
        end do
     end do
     deallocate(tab)
     deallocate(PsfI)
!
     if (PsfNorm .ne. 1.d0) then
        do NL = 1, NLI
           do NC = 1, NCI
              Psf_Out%Wgt(NC,NL,1) = Psf_Out%Wgt(NC,NL,1)/PsfNorm
           end do
        end do
     end if
!
!    undersampling of PSF coordinates
     do NC = 1, Psf_In%NbCol, Psf_In%NSamp
        IC = int( (NC-1)/Psf_In%NSamp ) + 1
        Psf_Out%Y(IC) = Psf_In%Y(NC)
     end do
     do NL = 1, Psf_In%NbLin, Psf_In%NSamp
        IL = int( (NL-1)/Psf_In%NSamp ) + 1
        Psf_Out%Z(IL) = Psf_In%Z(NL)
     end do
!
     return
   end subroutine psf_interpol
!
!
!!
!!
!> psf_psp -- Public
!!
!! * Purpose
!!
!!     Update of the Point Spread Function with a interferometric axis shift
!!
!! * Description
!!
!!     This subroutine updates the Psf parameters by taking into account an interferometric
!!     axis shift.
!!
!! * Inputs
!!
!!    - AxeY : interferometric axis shift along Y
!!    - AxeZ : interferometric axis shift along Z
!!    - Psf_In  : type_Psf / type for declaration and allocation of psf
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!    - Psf_Out : type_Psf / type for declaration and allocation of psf
!!
!! * References
!!
   subroutine psf_psp( AxeY, AxeZ, Psf_In, Psf_Out )
   implicit none
     real(kind=DOUBLE)  ,intent(in)               :: AxeY
     real(kind=DOUBLE)  ,intent(in)               :: AxeZ
     type(type_Psf)     ,intent(in)               :: Psf_In
     type(type_Psf)     ,intent(out)              :: Psf_Out
!
     Psf_Out%NsWn   = Psf_In%NsWn
     Psf_Out%NbCol  = Psf_In%NbCol
     Psf_Out%NbLin  = Psf_In%NbLin
     call alloc_Psf( Psf_Out )
     Psf_Out = Psf_In

     Psf_Out%AY(1:Psf_Out%NbCol)  = Psf_Out%AY(1:Psf_Out%NbCol) - AxeY
     Psf_Out%AZ(1:Psf_Out%NbLin)  = Psf_Out%AZ(1:Psf_Out%NbLin) - AxeZ
     Psf_Out%Y(1:Psf_Out%NbCol)  = dtan(Psf_Out%AY(1:Psf_Out%NbCol))
     Psf_Out%Z(1:Psf_Out%NbLin)  = dtan(Psf_Out%AZ(1:Psf_Out%NbLin))
!
     call fieldmeanangle_calc( Psf_Out )
!
     return
   end subroutine psf_psp
!
!
!!
!!
!> psf_pixel -- Public
!!
!! * Purpose
!!
!!     Update of the Point Spread Function for a given pixel 
!!
!! * Description
!!
!!     This subroutine updates the Psf parameters for a given pixel in function of its position
!!
!! * Inputs
!!
!!    - PixelPosY : pixel position along the Y axis 
!!    - PixelPosZ : pixel position along the Y axis
!!    - Psf_In    : type_Psf / type for declaration and allocation of Psf
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!    - Psf_Out : type_Psf / type for declaration and allocation of Psf
!!
!! * References
!!
   subroutine psf_pixel( PixelPosY, PixelPosZ, Psf_In, Psf_Out )
   implicit none
     real(kind=DOUBLE) , intent(in)               :: PixelPosY
     real(kind=DOUBLE) , intent(in)               :: PixelPosZ
     type(type_Psf)    , intent(in)               :: Psf_In
     type(type_Psf)    , intent(out)              :: Psf_Out
     integer(kind=LONG)                           :: NF
!
     Psf_Out%NsWn   = Psf_In%NsWn
     Psf_Out%NbCol  = Psf_In%NbCol
     Psf_Out%NbLin  = Psf_In%NbLin
     call alloc_Psf( Psf_Out )
     Psf_Out = Psf_In
     Psf_Out%PixelPosY = PixelPosY
     Psf_Out%PixelPosZ = PixelPosZ
!
     Psf_Out%Y(1:Psf_Out%NbCol)  = Psf_In%Y(1:Psf_Out%NbCol) + PixelPosY
     Psf_Out%Z(1:Psf_Out%NbLin)  = Psf_In%Z(1:Psf_Out%NbLin) + PixelPosZ
     Psf_Out%AY(1:Psf_Out%NbCol) = datan(Psf_Out%Y(1:Psf_Out%NbCol))
     Psf_Out%AZ(1:Psf_Out%NbLin) = datan(Psf_Out%Z(1:Psf_Out%NbLin))
!
     do NF = 1, Psf_Out%NsWn
        Psf_Out%BaryCentreY(NF)    = 0.d+00
        Psf_Out%BaryCentreZ(NF)    = 0.d+00
        Psf_Out%FieldMeanAngle(NF) = 0.d+00
     end do
!
     return
   end subroutine psf_pixel
!
!
!!
!!
!> fieldmeanangle_calc -- Public
!!
!! * Purpose
!!
!!     Average field angle computation
!!
!! * Description
!!
!!     This subroutine integrates the PSF weighted cosine of the field mean angle.
!!     The weight includes the correction for an angular integration over the PSF defined on an equidistant grid.
!!     The weighting factor is by accident identical to the integration property: cos(Theta). It represents the 
!!     decrease of angular variation with respect to distance variation at increasing field angles.
!!     The mean field angle is defined as the arccos of the average field mean angle cosine.
!!
!! * Inputs/outputs
!!
!!    - Psf : type_Psf / type for declaration and allocation of Psf
!!
!! * References
!!
   subroutine fieldmeanangle_calc( Psf )
   implicit none
     type(type_Psf)    , intent(inout)            :: Psf
     integer(kind=LONG)                           :: NC
     integer(kind=LONG)                           :: NL
     integer(kind=LONG)                           :: NF
     real(kind=DOUBLE)                            :: CosMean
     real(kind=DOUBLE)                            :: Theta 
     real(kind=DOUBLE)                            :: AngWgt
     real(kind=DOUBLE)                            :: AngWgtS
!
     do NF = 1, Psf%NsWn
        Psf%BaryCentreY(NF) = 0.d+00
        Psf%BaryCentreZ(NF) = 0.d+00
        CosMean = 0.d+00
        AngWgtS = 0.d+00
        do NL = 1, Psf%NbLin
           do NC = 1, Psf%NbCol
              Psf%BaryCentreY(NF) = Psf%BaryCentreY(NF)        &
                                  + Psf%AY(NC)* Psf%Wgt(NC,NL,NF)
              Psf%BaryCentreZ(NF) = Psf%BaryCentreZ(NF)        &
                                  + Psf%AZ(NL)* Psf%Wgt(NC,NL,NF)
              Theta = datan(dsqrt( dtan(Psf%AY(NC))**2   &
                                  +dtan(Psf%AZ(NL))**2 ) ) 
              AngWgt = dcos(Theta) * Psf%Wgt(NC,NL,NF)
              CosMean = CosMean + (dcos(Theta) * AngWgt)
              AngWgtS = AngWgtS + AngWgt
              Psf%AWgt(NC,NL,NF) = AngWgt
           end do
        end do
        do NL = 1, Psf%NbLin
           do NC = 1, Psf%NbCol
              Psf%AWgt(NC,NL,NF) = Psf%AWgt(NC,NL,NF) / AngWgtS
           end do
        end do
        Psf%FieldMeanAngle(NF) = dacos( CosMean/AngWgtS )
     end do
!
     return
   end subroutine fieldmeanangle_calc
!
!
!!
!!
!> psf_gain -- Public
!!
!! * Purpose
!!
!!     Detector gain took into account into the Point Spread Function computation
!!
!! * Description
!!
!!    This subroutine takes into account the detector gain into the PSF computation.
!!
!! * Inputs
!!
!!    - Gain   : type_Gain / type for detectors gain declaration and allocation
!!    - SUB_PN : sub-pixel
!!    - Psf_In : type_Psf / type for declaration and allocation of Psf
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!    - Psf_Out : type_Psf / type for declaration and allocation of Psf
!!
!! * References
!!
   subroutine psf_gain( Gain, SUB_PN, Psf_In, Psf_Out )
   implicit none
     type(type_Gain)         ,intent(in)                :: Gain
     integer(kind=LONG)      ,intent(in)                :: SUB_PN
     type(type_Psf)          ,intent(in)                :: Psf_In
     type(type_Psf)          ,intent(out)               :: Psf_Out
     integer(kind=LONG)                                 :: NF
     real(kind=DOUBLE)                                  :: Norm
!
     Psf_Out%NsWn   = Psf_In%NsWn
     Psf_Out%NbCol  = Psf_In%NbCol
     Psf_Out%NbLin  = Psf_In%NbLin
     call alloc_Psf( Psf_Out )
     Psf_Out = Psf_In
     if( Psf_Out%NbLin /= Gain%NbLin_SUB_PN ) then
        write(*,*) 'NbLin Error',Psf_Out%NbLin,Gain%NbLin_SUB_PN
        write(*,*) 'psf_gain Fatal Error'
        call exit(1)
     end if
     if( Psf_Out%NbCol /= Gain%NbCol_SUB_PN ) then
        write(*,*) 'NbCol Error',Psf_Out%NbCol,Gain%NbCol_SUB_PN
        write(*,*) 'psf_gain Fatal Error'
        call exit(1)
     end if
!
     do NF = 1, Psf_Out%NsWn
        Psf_Out%Wgt(1:Psf_Out%NbCol,1:Psf_Out%NbLin,NF) =                 &
                   Psf_In%Wgt(1:Psf_Out%NbCol,1:Psf_Out%NbLin,NF)         &
                 * Gain%Gain_SUB_PN(1:Psf_Out%NbCol,1:Psf_Out%NbLin,SUB_PN)
        Norm = sum( Psf_Out%Wgt(1:Psf_Out%NbCol,1:Psf_Out%NbLin,NF) ) 
        Psf_Out%Wgt(1:Psf_Out%NbCol,1:Psf_Out%NbLin,NF) =       &
                Psf_Out%Wgt(1:Psf_Out%NbCol,1:Psf_Out%NbLin,NF) &
              / Norm
     end do
!
     return
   end subroutine psf_gain
!
!
!!
!!
!> psf_index -- Public
!!
!! * Purpose
!!
!!     Computation of difference between two PSFs
!!
!! * Description
!!
!!     This subroutine computes the difference between a Psf and a reference one.
!!
!! * Inputs
!!
!!    - Psf     : type_Psf / type for declaration and allocation of psf
!!    - Psf_Ref : type_Psf / type for declaration and allocation of psf
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!    - Psf_Index : real value of the difference between two PSFs
!!
!! * References
!!
   subroutine psf_index( Psf, Psf_Ref, Index )
     type(type_Psf)      ,intent(in)                  :: Psf
     type(type_Psf)      ,intent(in)                  :: Psf_Ref
     real(kind=DOUBLE)   ,intent(out)                 :: Index
!
     if( (Psf%NbLin /= Psf_Ref%NbLin) .or. &
         (Psf%NbCol /= Psf_Ref%NbCol) ) then
        write(*,*) 'psf_index : Psf Dimensions Error'
        Index = -999.99d+00
        return
     end if
!
     Index = sum( dabs( Psf%Wgt(1:Psf%NbCol,1:Psf%NbLin,1)&
                       -Psf_Ref%Wgt(1:Psf%NbCol,1:Psf%NbLin,1) ) )
!
     return
   end subroutine psf_index
!
!
!!
!!
!> psf_init -- Public
!!
!! * Purpose
!!
!!     Initialisation of the Point Spread Function for a interferometric axis centered 
!!     on zero.
!!
!! * Description
!!
!!     This subroutine allows the allocation of Psf tables, their initialisation 
!!     in function of parameters read in the input file, and after a radian conversion, 
!!     their writting in a Psf output file (File_Psf_Main)
!!
!! * Inputs
!!
!!    - File_Psf_Header : character / name of input file defining psf parameters 
!!    - File_Psf_Main   : character / name of output file with main definition of all 
!!                        psf parameters 
!!    - Srf_Param : type_Srf_Param / type for declaration and allocation of srf parameters
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!    - Psf : type_Psf / type for declaration and allocation of psf
!!
!! * References
!!
   subroutine psf_init( File_Psf_Header,&
                        File_Psf_Main,  &
                        Psf             )
   implicit none
     character(len=*)    ,intent(in)                 :: File_Psf_Header
     character(len=*)    ,intent(in)                 :: File_Psf_Main
     type(type_Psf)      ,intent(inout)              :: Psf
     integer(kind=LONG)                              :: iFile
     integer(kind=LONG)                              :: iPos
     integer(kind=LONG)                              :: NsWn
     integer(kind=LONG)                              :: Nl
     integer(kind=LONG)                              :: Nc
     integer(kind=LONG)                              :: NbCol
     integer(kind=LONG)                              :: NbLin
     integer(kind=LONG)                              :: ErrCode
     real(kind=DOUBLE)                               :: Y_Step
     real(kind=DOUBLE)                               :: Z_Step
!
     iFile = 10
     iPos  = 1
     write(*,*) File_Psf_Header
     open(unit=iFile, file=File_Psf_Header, status='old', err=999 )
     Psf%filename = File_Psf_Header
     iPos  = 2
     read(iFile,*,err=999) 
     read(iFile,*,err=999) Psf%NsWn
     allocate( Psf%Wn(Psf%NsWn),             stat=ErrCode )
     allocate( Psf%BaryCentreY(Psf%NsWn),    stat=ErrCode )
     allocate( Psf%BaryCentreZ(Psf%NsWn),    stat=ErrCode )
     allocate( Psf%FieldMeanAngle(Psf%NsWn), stat=ErrCode )
     iPos  = 3
     do NsWn = 1, Psf%NsWn
        read(iFile,*,err=999) Psf%Wn(NsWn)
        read(iFile,*,err=999) Psf%BaryCentreY(NsWn)
        read(iFile,*,err=999) Psf%BaryCentreZ(NsWn)
        read(iFile,*,err=999) Psf%FieldMeanAngle(NsWn)
        Psf%BaryCentreY(NsWn) = 0.d+00
        Psf%BaryCentreZ(NsWn) = 0.d+00
        Psf%FieldMeanAngle(NsWn) = 0.d+00
     end do
     iPos  = 4
     read(iFile,*,err=999) Psf%NSamp
     read(iFile,*,err=999) Psf%NbCol
     read(iFile,*,err=999) Psf%NbLin
     iPos  = 5
     allocate( Psf%Y(Psf%NbCol),  stat=ErrCode )
     allocate( Psf%Z(Psf%NbLin),  stat=ErrCode )
     allocate( Psf%AY(Psf%NbCol), stat=ErrCode )
     allocate( Psf%AZ(Psf%NbLin), stat=ErrCode )
     allocate( Psf%Wgt(Psf%NbCol,Psf%NbLin,Psf%NsWn),  stat=ErrCode )
     allocate( Psf%AWgt(Psf%NbCol,Psf%NbLin,Psf%NsWn), stat=ErrCode )
     read(iFile,*,err=999) Y_Step
     read(iFile,*,err=999) Z_Step
     close(unit=iFile)
     Psf%Y(1:Psf%NbCol) =                                         &
             (/ (dble(Nc-(int(Psf%NbCol/2)+1)),Nc=1,Psf%NbCol) /) &
             * Y_Step / Psf%H_Sat
     Psf%Z(1:Psf%NbLin) =                                         &
             (/ (dble((int(Psf%NbLin/2)+1)-Nl),Nl=1,Psf%NbLin) /) &
             * Z_Step / Psf%H_Sat
     Psf%AY(1:Psf%NbCol) = 0.d+00
     Psf%AZ(1:Psf%NbLin) = 0.d+00
     iPos  = 7
     write(*,*) File_Psf_Main
     open(unit=iFile, file=File_Psf_Main, status='old', err=999 )
     iPos  = 8
     read(iFile,*,err=999) 
     read(iFile,*,err=999) NsWn
     write(*,*) 'NsWn',NsWn
     read(iFile,*,err=999) NbCol
     write(*,*) 'NbCol',NbCol
     read(iFile,*,err=999) NbLin
     write(*,*) 'NbLin',NbLin
     if( (NbCol /= Psf%NbCol) .or. (NbLin /= Psf%NbLin) &
                              .or. (NsWn /= Psf%NsWn) ) then
        write(*,*) 'PSF NbCol or NbLin or NsWn Error',NbCol,NbLin,NsWn
        go to 999
     end if
     iPos  = 9
     do Nl = 1, Psf%NbLin
        do Nc = 1, Psf%NbCol
           read(iFile,*,err=999) (Psf%Wgt(Nc,Nl,NsWn),NsWn=1,Psf%NsWn)
           Psf%AWgt(Nc,Nl,1:Psf%NsWn) = 0.d+00
        end do
     end do
     close(unit=iFile)
     return
999  write(*,*) 'parameters reading error',iPos
     write(*,*) 'psf_init Fatal Error'
     call exit(1)
   end subroutine psf_init
!
!
end module psf_computation_module
