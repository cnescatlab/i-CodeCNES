!!# refractive_index_module.f90 --
!!#
!!#           Project: SPS_GENERIC
!!#           Authors: NOVELTIS/B.TOURNIER
!!#              Date: january 2010
!!#           Version: $Revision: 1.1 $
!!# Last modification: $Date: 2011-03-07 15:54:59 $
!!#
!!# Language:  F90
!!# Standards: Noveltis
!!#
!!# --
!!#
!! 
!> refractive_index_module -- Module
!!
!! * Purpose
!!
!!     Module for material refractive index initialisation.
!!
!! * Description
!!      
!!     This module defines the refractive index for a given material
!!
!! * Sub-routines and functions
!!
!!     - refractive_index_init : Initialisation of the type defining the 
!!                               material refractive index
!!
!! * References
!!

module refractive_index_module
  use precision_type
  use error_type
  use srf_param_type
  use refrac_index_type
  use psf_type
  !
  implicit none
  !
  !
  public :: refractive_index_init
  !
contains
  !
  !


!!
!!
!> refractive_index_init -- Public
!!
!! * Purpose
!!
!!     Initialisation of the type defining the material refractive index
!!     
!!
!! * Description
!!
!!     This subroutine defines the refractive index for a given material: ZnSe or KBr
!!
!! * Inputs 
!!
!!     - File_Material : character / name of input file defining material parameters 
!!     - Psf           : type_Psf / type for declaration and allocation of psf
!!
!! * Inputs/outputs
!!
!!     - RefracIndex : type_RefracIndex / type for declaration and allocation material 
!!                     refractive index initialisation
!!
!! * Outputs
!!
!! * References
!!

  subroutine refractive_index_init( Srf_Param,     &
                                    File_Material, &
                                    Psf,           &
                                    RefracIndex     )
    !input
    type(type_Srf_Param)    ,intent(in)   :: Srf_Param
    character(len=*)        ,intent(in)   :: File_Material
    type(type_Psf)          ,intent(in)   :: Psf
    !output
    type(type_RefracIndex)  ,intent(out)  :: RefracIndex
    ! locals
    integer(kind=LONG)                    :: iFile
    integer(kind=LONG)                    :: iPos
    integer(kind=LONG)                    :: Nb
    integer(kind=LONG)                    :: Nl
    integer(kind=LONG)                    :: Nc
    integer(kind=LONG)                    :: index
    integer(kind=LONG)                    :: dummy1
    integer(kind=LONG)                    :: dummy2
    !
    !
    iFile = 10
    iPos  = 1
    write(*,*) File_Material
    open(unit=iFile, file=File_Material, status='old', err=999 )
    iPos  = 2
    RefracIndex%filename = File_Material
    ! read ZnSe parameters
    index = 1
    read(iFile,*,err=999)
    read(iFile,*,err=999) RefracIndex%C(index)
    read(iFile,*,err=999) RefracIndex%NCoef(index)
    read(iFile,*,err=999) ( RefracIndex%A(Nb,index),      &
                            Nb=1,RefracIndex%NCoef(index) )
    read(iFile,*,err=999) ( RefracIndex%B(Nb,index),      &
                            Nb=1,RefracIndex%NCoef(index) )
    ! read KBr parameters
    index = 2
    read(iFile,*,err=999)
    read(iFile,*,err=999) RefracIndex%C(index)
    read(iFile,*,err=999) RefracIndex%NCoef(index)
    read(iFile,*,err=999) ( RefracIndex%A(Nb,index),      &
                            Nb=1,RefracIndex%NCoef(index) )
    read(iFile,*,err=999) ( RefracIndex%B(Nb,index),      &
                            Nb=1,RefracIndex%NCoef(index) )
    ! defect: read FracN refractive index fraction
    if( (Srf_Param%Mode_Agreg == 'MER1') .or. &
        (Srf_Param%Mode_Agreg == 'MER2') ) then
      read(iFile,*,err=999) 
      read(iFile,*,err=999) RefracIndex%NbCol
      write(*,*) 'NbCol',RefracIndex%NbCol
      read(iFile,*,err=999) RefracIndex%NbLin
      write(*,*) 'NbLin',RefracIndex%NbLin
      if ( (RefracIndex%NbCol /= Psf%NbCol) .or. &
           (RefracIndex%NbLin /= Psf%NbLin) )then
         write(*,*) 'FracN NbCol or NbLin error', &
                     RefracIndex%NbCol, RefracIndex%NbLin,&
                     Psf%NbCol, Psf%NbLin
         go to 999
      end if
      call alloc_RefracIndex( RefracIndex )
      read(iFile,*,err=999) 
      do Nl = 1, RefracIndex%NbLin
        do Nc = 1, RefracIndex%NbCol
          read(iFile,*,err=999) dummy1, dummy2, RefracIndex%FracN(Nc,Nl)
        end do
      end do
    end if
    close(unit=iFile)
    !
    return
999 write(*,*) 'parameters reading error'
    write(*,*) 'refractive_index_init Fatal Error'
    call exit(1)
  end subroutine refractive_index_init
  !
  !
end module refractive_index_module
