!!#  emissivity_module.f90 -- 
!!# 
!!#            Project: SPS_GENERIC
!!#            Authors: NOVELTIS/B.TOURNIER
!!#               Date: april 2011
!!#            Version: $Revision: 1.2 $
!!#  Last modification: $Date: 2011-11-02 14:57:50 $
!!#
!!#  Language:  F90
!!#  Standards: Noveltis
!!#
!!# --
!!#
!!
!> emissivity_module -- Module
!!
!! * Purpose
!!
!!   Module for target emissivity initialisation
!!
!! * Description
!!      
!!
!! * Sub-routines and functions
!!
!! -  emissivity_init   : initialisation of the type defining the emissivity
!!
!! * References
!!
!!     NOV-3820-NT-ATBD : 4.4.2.5
!!

module emissivity_module
   use precision_type
   use error_type
   use constantes_type
   use emissivity_type
   use math_module
!
   implicit none
!
!
   public :: emissivity_init
!
!
   contains
!
!

!> emissivity_init -- Public
!!
!! * Purpose
!!
!!     Initialisation of the type emissivity parameters 
!!
!! * Description
!!
!!     The emissivity_init subroutine allows to update the type emissivity  
!!     emissivity parameters thanks to the read of the File_Param file; 
!!
!! * Inputs
!!
!!     - File_Param : character / input file with definition of 
!!                    emissivity parameters
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!!     - Emissivity : type_Emissivity / type for emissivity parameters 
!!                   declaration and allocation.
!!
!! * References
!!

   subroutine emissivity_init( File_Param,&
                               Emissivity  )
   implicit none
     character(len=*)        ,intent(in)                 :: File_Param
     type(type_Emissivity)   ,intent(out)                :: Emissivity
!
     integer(kind=LONG)                                  :: iFile
     integer(kind=LONG)                                  :: iPos
     integer(kind=LONG)                                  :: Ns
!
     iFile = 10
!
     iPos = 1
     write(*,'(a)') File_Param(1:len_trim(File_Param))
     open(unit=iFile, file=File_Param, status='old',err=999)
     Emissivity%filename = File_Param
     iPos = 2
     read(iFile,*,err=999) Emissivity%NsWn0
     write(*,*) 'Emissivity%NsWn0 :',Emissivity%NsWn0
     call alloc_Emissivity( Emissivity )
     iPos = 3
     read(iFile,*,err=999) 
     do Ns = 1, Emissivity%NsWn0
        iPos = 3 + Ns
        read(iFile,*,err=999) Emissivity%Wn0(Ns),    &
                              Emissivity%Mod_BG1(Ns),&
                              Emissivity%Mod_BG2(Ns),&
                              Emissivity%Dir_BG(Ns), &
                              Emissivity%Fct_CS(Ns), &
                              Emissivity%Fct_BB(Ns), &
                              Emissivity%Fct_EW(Ns)
       write(*,'(a,f10.2,6e12.4)') 'Emissivity BG1 BG2 DIR CS BB EW :',&
                                     Emissivity%Wn0(Ns),     &
                                     Emissivity%Mod_BG1(Ns), &
                                     Emissivity%Mod_BG2(Ns), &
                                     Emissivity%Dir_BG(Ns),  &
                                     Emissivity%Fct_CS(Ns),  &
                                     Emissivity%Fct_BB(Ns),  &
                                     Emissivity%Fct_EW(Ns)
     end do
     close(unit=iFile)
!
     return
 999 write(*,*) 'emissivity_init Fatal Error',iPos
     call exit(1)
   end subroutine emissivity_init
!
!
end module emissivity_module
