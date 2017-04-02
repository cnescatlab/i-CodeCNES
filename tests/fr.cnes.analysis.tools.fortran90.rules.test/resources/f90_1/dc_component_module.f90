!!#   dc_component_module.f90 --
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: april 2011
!!#            Version: $Revision: 1.3 $
!!#  Last modification: $Date: 2012-02-08 10:19:14 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!>  dc_component -- Module
!!
!! * Purpose
!!
!!   Module for DC component computation and extraction.
!!
!! * Description
!!      
module dc_component_module
   use precision_type
   use error_type
   use dc_component_type
   use interferogram_type
   use spectrum_type
   use zpd_type
   use plancklib_module
!
   implicit none
!
!
   public :: dc_component_init,    &
             calc_dc_component,    &
             extract_dc_component, &
             dc_component_planck,  &
             dc_component_calibration
!
   contains
!
!
   subroutine dc_component_init( File_Dc_Component, &
                                 Dc_Component       )
   implicit none
     character(len=*)       ,intent(in)      :: File_Dc_Component
     type(type_Dc_Component), intent(inout)  :: Dc_Component
     integer(kind=LONG)                      :: iFile
     integer(kind=LONG)                      :: iPos
!
     iFile = 10
     iPos = 1
     write(*,'(a)') 'File_Dc_Component',&
                     File_Dc_Component(1:len_trim(File_Dc_Component))
     open(unit=iFile, file=File_Dc_Component, status='old', err=999)
     iPos = 2
     Dc_Component%filename = File_Dc_Component
     read(iFile,*,err=999) Dc_Component%N_Sample
     close(unit=iFile)
!
     return
 999 write(*,*) 'dc_component_init Reading Error',iPos
     call exit(1)
   end subroutine dc_component_init
!
!
   subroutine calc_dc_component( Zpd,         &
                                 Interf,      &
                                 Dc_Component )
   implicit none
     type(type_Zpd)            ,intent(in)               :: Zpd
     type(type_Interferogram)  ,intent(in)               :: Interf
     type(type_Dc_Component)   ,intent(inout)            :: Dc_Component
     integer(kind=LONG)                                  :: Ns
!
!    Coherence control
     if( Interf%Type /= 'R' ) then
        write(*,*) 'Interf%Type Error',Interf%Type
        write(*,*) 'calc_dc_component Fatal Error'
        call exit(1)
     end if
!
!   Full DC component computation
    Dc_Component%DC = sum( Interf%Real_Part(1 :Interf%N_Sample) ) &
                    / dble( Interf%N_Sample )
!
!    DC component segmentation
     Dc_Component%N_Segment = int( Interf%N_Sample/Dc_Component%N_Sample )
     call alloc_Dc_Component( Dc_Component )
     do Ns = 1, Dc_Component%N_Segment
        Dc_Component%Segment(Ns) = &
              sum( Interf%Real_Part( (Ns-1)*Dc_Component%N_Sample+1&
                                    :(Ns)*Dc_Component%N_Sample) ) &
              / dble( Dc_Component%N_Sample )
        Dc_Component%Opd(Ns) = &
              sum( Interf%Opd( (Ns-1)*Dc_Component%N_Sample+1&
                              :(Ns)*Dc_Component%N_Sample) ) &
              / dble( Dc_Component%N_Sample )                &
              - Interf%Opd(Zpd%NZpd)
     end do
     return
   end subroutine calc_dc_component
!
!
!>   extract_dc_component -- Public
!!
!! * Purpose
!!
!!    extracts interferogram DC component
!!
!! * Description
!!    
!!    The algorithm calculate the DC component of the interferogram and removes it from
!!    the interferogram
!!
!! * Inputs-Outputs
!!
!!     - Interf  : type Interferogram 
!!     - Dc_Component type Dc_Component
!! -  extract_dc_component      : calculates the DC component of the 
!!                                interferogram and removes it from
!!                                the interferogram.
!!
!!
!! * References
!!   SPS ATBD
!!
  subroutine extract_dc_component( Interf,       &
                                   Dc_Component  )
  implicit none
    type(type_Interferogram)  ,intent(inout)                :: Interf
    type(type_Dc_Component)   ,intent(in)                   :: Dc_Component
!
!   DC component extraction
    Interf%Real_Part(1:Interf%N_Sample) =                     &
          Interf%Real_Part(1:Interf%N_Sample) - Dc_Component%DC
!
    return
  end subroutine extract_dc_component
!
!
  subroutine dc_component_planck( Spectrum,     &
                                  Dc_Component  )
  implicit none
    type(type_Spectrum)     ,intent(in)               :: Spectrum
    type(type_Dc_Component) ,intent(inout)            :: Dc_Component
    real(kind=DOUBLE)                                 :: Rad
    real(kind=DOUBLE)                                 :: Rad_Sum
    integer(kind=LONG)                                :: Ns
!
    if( Dc_Component%T /= 0.d+00 ) then
      Dc_Component%Wn = 0.d+00
      Rad_Sum = 0.d+00
      do Ns = 1, Spectrum%N_Sample
         call plkdirect( Dc_Component%T, Spectrum%Wn(Ns), Rad )
         Dc_Component%Wn = Dc_Component%Wn + Spectrum%Wn(Ns)* Rad
         Rad_Sum         = Rad_Sum + Rad
      end do
      Dc_Component%Wn = Dc_Component%Wn / Rad_Sum
      Dc_Component%B  = Rad_Sum /dble(Spectrum%N_Sample)
    else
         Dc_Component%Wn = sum( Spectrum%Wn(1:Spectrum%N_Sample)) &
                         / dble(Spectrum%N_Sample)
         Dc_Component%B  = 0.d+00
    end if
!
    return
  end subroutine dc_component_planck
!
!
  subroutine dc_component_calibration( Dc_Component_CS, &
                                       Dc_Component_BB, &
                                       Dc_Component_EW  )
  implicit none
    type(type_Dc_Component)   ,intent(in)             :: Dc_Component_CS
    type(type_Dc_Component)   ,intent(in)             :: Dc_Component_BB
    type(type_Dc_Component)   ,intent(inout)          :: Dc_Component_EW
    real(kind=DOUBLE)                                 :: Offset
    real(kind=DOUBLE)                                 :: Slope
    real(kind=DOUBLE)                                 :: B_BB
    real(kind=DOUBLE)                                 :: B_CS
    integer(kind=LONG)                                :: Seg
!
!   Planck integration over the band
    B_CS = Dc_Component_CS%B
    B_BB = Dc_Component_BB%B
!
!   calibration coefficients
    Offset = ( (B_BB * Dc_Component_CS%DC) - (B_CS * Dc_Component_BB%DC) )&
           / ( B_BB - B_CS )
    Slope  = ( B_BB - B_CS ) / (Dc_Component_BB%DC - Dc_Component_CS%DC)
!
    Dc_Component_EW%DC_Cal = Slope * (Dc_Component_EW%DC - Offset)
!
!   Segments calibration
    do Seg = 1, Dc_Component_EW%N_Segment
       Offset = ( ( B_BB * Dc_Component_CS%Segment(Seg) )  &
                - ( B_CS * Dc_Component_BB%Segment(Seg) ) )&
              / ( B_BB - B_CS )
       Slope  = ( B_BB - B_CS ) / ( Dc_Component_BB%Segment(Seg) &
                                  - Dc_Component_CS%Segment(Seg) )
!
       Dc_Component_EW%Segment_Cal(Seg) =                           &
                    Slope * ( Dc_Component_EW%Segment(Seg) - Offset )
    end do
!
    return
  end subroutine dc_component_calibration
!
!
end module dc_component_module
