!!#  scene_mixing_module.f90 --
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: october 2009
!!#            Version: $Revision: 1.5 $
!!#  Last modification: $Date: 2011-03-14 10:39:55 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!> scene_mixing_module.f90 -- Module
!!
!! * Purpose
!!
!!   Module for scene mixing heterogeneity and LOS variation
!!
!! * Description
!!      
!!   This module simulates a scene composition in assuming that a spatial sample can be 
!!   described at each time as a linear combination of two radiance spectra corresponding 
!!   to two elementary scenes with different geophysical conditions: the fraction of each 
!!   of the both elementary scene is took into account. 
!!   The module can also take into account scene motion and variation. 
!!
!! * Sub-routines and functions
!!
!! -  interf_comb_jitter : Combination of scene heterogeneity taking into account the 
!!    line of sight (LOS) variation
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : AD
!!

module scene_mixing_module
   use precision_type
   use error_type
   use constantes_type
   use interferogram_type
   use scene_param_type
   use zpd_type
!
   implicit none
!
!
   public ::                    &
             scene_mixing_def
!
!
   contains
!
!

!> scene_mixing_def -- Public
!!
!! * Purpose
!!
!!     Initialisation of the type defining the scene parameters 
!!
!! * Description
!!
!!     The scene_mixing_def subroutine allows to update the type defining the scene 
!!     parameters thanks to the loading of the Files_Scene_Param file. 
!!
!! * Inputs
!!
!!     - Files_Scene_Param : character / input file with definition of scene parameters
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Scene_Param : type_Scene_Param / type for scene parameters declaration and allocation
!!
!! * References
!!

   subroutine scene_mixing_def( Files_Scene_Param, &
                                Scene_Param        )
   implicit none
     character(len=500)      , intent(in)                :: Files_Scene_Param
     type(type_Scene_Param)  , intent(out)               :: Scene_Param
     integer(kind=LONG)                                  :: iFile
     integer(kind=LONG)                                  :: iPos
     integer(kind=LONG)                                  :: Ns
!
     iFile = 10
!
!    scene parameter
     iPos = 1
     write(*,'(a)') Files_Scene_Param(1:len_trim(Files_Scene_Param))
     open(unit=iFile, file=Files_Scene_Param, status='old',err=999)
     iPos = 2
     Scene_Param%filename = Files_Scene_Param
     read(iFile,*,err=999) Scene_Param%NbScene
     read(iFile,*,err=999) Scene_Param%NbSubPixel
     call alloc_Scene_Param( Scene_Param )
     iPos = 3
     read(iFile,*,err=999) Scene_Param%Scene_List(1:Scene_Param%NbScene)
     do Ns = 1, Scene_Param%NbScene
        read(iFile,fmt='(a)',err=999) Scene_Param%hr_filename(Ns)
     end do
     iPos = 4
     read(iFile,fmt='(a)',err=999) 
     do Ns = 1, Scene_Param%NbSubPixel
        read(iFile,*,err=999) Scene_Param%N_Spectre_1(Ns),&
                              Scene_Param%N_Spectre_2(Ns),&
                              Scene_Param%Frac_1(Ns),     &
                              Scene_Param%AmpFracJit(Ns), &
                              Scene_Param%FreqJit(Ns),    &
                              Scene_Param%PhasJit(Ns)
     end do
     close(unit=iFile)
     write(*,*) 'Scene_Param%NbScene   ',Scene_Param%NbScene
     write(*,*) 'Scene_Param%NbSubPixel',Scene_Param%NbSubPixel
     do Ns = 1, Scene_Param%NbScene
        write(*,'(a)') Scene_Param%hr_filename(Ns)&
                                 (1:len_trim(Scene_Param%hr_filename(Ns)))
     end do
     do Ns = 1, Scene_Param%NbSubPixel
        write(*,*) 'N_Spectre_1',Ns,Scene_Param%N_Spectre_1(Ns)
        write(*,*) 'N_Spectre_2',Ns,Scene_Param%N_Spectre_2(Ns)
        write(*,*) 'Frac_1     ',Ns,Scene_Param%Frac_1(Ns)
        write(*,*) 'AmpFracJit ',Ns,Scene_Param%AmpFracJit(Ns)
        write(*,*) 'FreqJit    ',Ns,Scene_Param%FreqJit(Ns)
        write(*,*) 'PhasJit    ',Ns,Scene_Param%PhasJit(Ns)
        Scene_Param%PhasJit(Ns) = Scene_Param%PhasJit(Ns) * Pi / 180.d+00
     end do
!
     return
!
999  write(*,*) 'parameters reading error',iPos
     write(*,*) 'scene_mixing_def Fatal Error'
     call exit(1)
   end subroutine scene_mixing_def
!
!
end module scene_mixing_module
