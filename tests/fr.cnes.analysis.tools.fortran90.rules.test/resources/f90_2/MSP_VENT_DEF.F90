module msp_vent_def

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  msp_vent_def
!
!$Resume
!	Module permettant de calculer les composantes du vent
!
!$Description
!       Module permettant de calculer les composantes du vent
!
!$Auteur
!
!$Version
!  $Id: MSP_VENT_DEF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: MSP_VENT_DEF.F90,v $
!  Revision 1.11  2010/10/20 09:35:43  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.10  2008/08/08 15:09:11  gss
!  DM-ID 1058 : (portage g95) initialisation à 0 de variables locales dans la
!  fonction cal_vent.
!
!  Revision 1.9  2008/07/04 14:54:36  huec
!  DM-ID 1058 : Gestion memoire
!
!  Revision 1.8  2008/02/22 14:36:11  huec
!  FA-ID 868 : AQ, modification du nom de la variable epsilon car mot reserve par fortran
!
!  Revision 1.7  2008/02/22 14:34:57  huec
!  FA-ID 868 : AQ, modification du nom de la variable epsilon car mot reserve par fortran
!
!  Revision 1.6  2008/02/22 13:52:01  huec
!  FA-ID 968 : Utilisation d un epsilon de comparaison et des pm_reel
!
!  Revision 1.5  2007/01/25 17:08:52  vivaresf
!  Version 4.4a1 : pour eviter un core dump, en cas de mauvaise
!  utilisation, on prend les variables locale dans le cas GRAM (qui sont égale aux variables passées en paramètres ou a zero)
!
!  Revision 1.4  2006/06/02 11:21:56  vpg
!  DM-ID 232 : qualite. Nommage des arguments optionnels lors des appels de fonctions et de routines
!
!  Revision 1.3  2005/03/08 07:32:38  fabrec
!  DM-ID 111 : mise à jour des cartouches
!
!  Revision 1.2  2005/03/03 15:05:19  vivaresf
!  DM-ID 111 : mise au point
!  Revision 1.1  2005/01/21 16:28:21  rostan
!  ajout MSP_stat
!  Revision 1.1  2004/04/14 13:57:51  simbad
!  Ajout du vent et de ACTSOL
!  Revision 1.2  2004/01/22 18:27:30  simbad
!  Avant suppression de AMLIB et IOLIB
!
!$FinHistorique
!
!$Usage
!  use msp_vent_def
!
!$Structure
!
!: msp_vent : 
!>     type_vent        : <integer>                    type du vent (definition MECASPA) (MSP_ENUM_SANS_VENT,MSP_ENUM_VENT_MODATM,MSP_ENUM_VENT_CONST,MSP_ENUM_VENT_TAB_1,MSP_ENUM_VENT_TAB_2)
!>     vent_nord        : <PM_REEL>                    |
!>     vent_est         : <PM_REEL>                    Vent constant
!>     vent_vert        : <PM_REEL>                    |
!>     vent_dim1_nord   : <PM_REEL,DIM=(:),pointer>    |
!>     vent_dim1_est    : <PM_REEL,DIM=(:),pointer>    Vent tabule a 1 dimension
!>     vent_dim1_vert   : <PM_REEL,DIM=(:),pointer>    |
!>     vent_dim2_nord   : <PM_REEL,DIM=(:,:),pointer>  |
!>     vent_dim2_est    : <PM_REEL,DIM=(:,:),pointer>  Vent tabule a 2 dimensions
!>     vent_dim2_vert   : <PM_REEL,DIM=(:,:),pointer>  |
!>     par1_vent        : <PM_REEL,DIM=(:),pointer>    grille du parametre 1 du vent
!>     par2_vent        : <PM_REEL,DIM=(:),pointer>    grille du parametre 2 du vent
!>     iwind            : <integer>                    type de  vent    (definition MECASPA SIMBAD)
!>     zvent            : <PM_REEL,DIM=(:),pointer>    altitude
!>     wvent            : <PM_REEL,DIM=(:),pointer>    vitesse du vent (m/s)
!>     avent            : <PM_REEL,DIM=(:),pointer>    azimuth du vent
!>     flag_func        : <logical>                    
!
!$Global
!
!>  msp_enum_vent_non            : <integer,parameter>  
!>  msp_enum_vent_tab            : <integer,parameter>  
!>  msp_enum_vent_modele         : <integer,parameter>  
!>  msp_enum_vent_tout           : <integer,parameter>  
! types de vents  (definition MECASPA)
!>  MSP_ENUM_SANS_VENT           : <integer,parameter>  pas de vent
!>  MSP_ENUM_VENT_MODATM         : <integer,parameter>  vent fourni par le modele d'atmosphere
!>  MSP_ENUM_VENT_CONST          : <integer,parameter>  vent constant
!>  MSP_ENUM_VENT_TAB_1          : <integer,parameter>  vent tabule a 1 dimmension
!>  MSP_ENUM_VENT_TAB_2          : <integer,parameter>  vent tabule a 2 dimmension
!>  MSP_ENUM_VENT_MODATM_CONST   : <integer,parameter>  Vent coherents avec le modele d'atmosphere + vent constant 
!>  MSP_ENUM_VENT_MODATM_TAB_1   : <integer,parameter>  Vent coherents avec le modele d'atmosphere + tabule a une dimension 
!>  MSP_ENUM_VENT_MODATM_TAB_2   : <integer,parameter>  Vent coherents avec le modele d'atmosphere + tabule a 2 dimensions   
!$Common
!
!$Routines
!- egaler_vent
!- MSP_effacer_vent
!- MSP_modifier_vent
!- MSP_cal_vent
!- vent_tab
!
!$Fonctions
!- MSP_mode_vent_modele
!- MSP_mode_vent
!- MSP_creer_vent
!
!$Include
!
!$Module
!#V
!- MSLIB
!- MSP_GESTION_ERREUR
!#
!
!$Interface
!> assignment :  egaler_vent
!#V
!#
!
!$Remarques
!
!$Mots-cles
!   VENT
!
!$Voir-Aussi
!.  MSP_mode_vent_modele MSP_mode_vent MSP_creer_vent egaler_vent MSP_effacer_vent MSP_modifier_vent
!.  MSP_cal_vent vent_tab
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use MSLIB
  use MSP_GESTION_ERREUR

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_VENT_DEF.F90 69 2012-09-11 08:33:34Z ffsm $'

  type msp_vent
     integer :: type_vent

      ! Definition du vent  (definition MECASPA)
     real(kind=PM_REEL)::vent_nord,vent_est,vent_vert
     real(kind=PM_REEL),dimension(:),pointer :: vent_dim1_nord => NULL()
     real(kind=PM_REEL),dimension(:),pointer :: vent_dim1_est  => NULL()
     real(kind=PM_REEL),dimension(:),pointer :: vent_dim1_vert => NULL()
     real(kind=PM_REEL),dimension(:,:),pointer :: vent_dim2_nord => NULL()
     real(kind=PM_REEL),dimension(:,:),pointer :: vent_dim2_est  => NULL()
     real(kind=PM_REEL),dimension(:,:),pointer :: vent_dim2_vert => NULL()
     real(kind=PM_REEL),dimension(:),pointer :: par1_vent => NULL()
     real(kind=PM_REEL),dimension(:),pointer :: par2_vent => NULL()

      ! Definition du vent  (definition MECASPA SIMBAD)
     integer :: iwind
     real(kind=PM_REEL),dimension(:), pointer ::zvent => NULL()
     real(kind=PM_REEL),dimension(:), pointer ::wvent => NULL()
     real(kind=PM_REEL),dimension(:), pointer ::avent => NULL()
     logical :: flag_func
  end type msp_vent

   ! Definition des differents types de vents possibles  (definition MECASPA SIMBAD)

  integer,parameter ::msp_enum_vent_non  = 0 
  integer,parameter ::msp_enum_vent_tab  = 1
  integer,parameter ::msp_enum_vent_modele = 2
  integer,parameter ::msp_enum_vent_tout = 3
  ! Definition des differents types de vents possibles  (definition MECASPA)
  integer,parameter :: MSP_ENUM_SANS_VENT = 0
  integer,parameter :: MSP_ENUM_VENT_MODATM = 1 ! Vent coherents avec le modele d'atmosphere
  integer,parameter :: MSP_ENUM_VENT_CONST = 2 ! vent constant
  integer,parameter :: MSP_ENUM_VENT_TAB_1 = 3 ! Vent tabule a une dimension
  integer,parameter :: MSP_ENUM_VENT_TAB_2 = 4 ! Vent tabule a 2 dimensions
  
  integer,parameter :: MSP_ENUM_VENT_MODATM_CONST = 12 ! Vent coherents avec le modele d'atmosphere + vent constant 
  integer,parameter :: MSP_ENUM_VENT_MODATM_TAB_1 = 13 ! Vent coherents avec le modele d'atmosphere + tabule a une dimension 
  integer,parameter :: MSP_ENUM_VENT_MODATM_TAB_2 = 14 ! Vent coherents avec le modele d'atmosphere + tabule a 2 dimensions

   interface assignment (=)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  assignment
!
!$Resume
!  Cette routine permet d'égaler deux structures vent
!
!$Description
!  Cette routine permet d'égaler deux structures vent
!
!$Acces
!  PUBLIC
!
!$Usage
!  venta=ventb
!.    type(msp_vent) :: venta
!.    type(msp_vent) :: ventb
!
!$Procedures
!- egaler_vent
!
!$Remarques
!
!$Mots-cles
! EGALER VENT
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure egaler_vent
   end interface

CONTAINS

  function MSP_mode_vent_modele(vent) result (oui_non)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_mode_vent_modele
!
!$Resume
!
!$Description
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  oui_non = MSP_mode_vent_modele(vent)
!.    type(MSP_VENT) :: vent
!.    logical :: oui_non
!
!$Arguments
!>E     vent     :<MSP_VENT>   
!>S     oui_non  :<logical>    
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MODE VENT MODELE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type(MSP_VENT),intent(in)::vent
    logical :: oui_non

    oui_non = ((vent%iwind==msp_enum_vent_modele).or.(vent%iwind==msp_enum_vent_tout))
  end function MSP_mode_vent_modele

  function MSP_mode_vent(vent) result (mode)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_mode_vent
!
!$Resume
!
!$Description
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  mode = MSP_mode_vent(vent)
!.    type(MSP_VENT) :: vent
!.    integer :: mode
!
!$Arguments
!>E     vent  :<MSP_VENT>   
!>S     mode  :<integer>    
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! MODE VENTXS
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type(MSP_VENT),intent(in)::vent
    integer :: mode

    mode = vent%iwind
  end function MSP_mode_vent

  subroutine egaler_vent(venta,ventb)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  egaler_vent
!
!$Resume
!	Routine permettant d'égaler 2 vents
!
!$Description
!	Routine permettant d'égaler 2 vents
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call egaler_vent(venta,ventb)
!.    type(msp_vent) :: venta
!.    type(msp_vent) :: ventb
!
!$Arguments
!>E/S   venta  :<msp_vent>   
!>E     ventb  :<msp_vent>   
!
!$Common
!
!$Routines
!- MSP_effacer_vent
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  EGALER VENT
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    type(msp_vent),intent(inout)::venta
    type(msp_vent),intent(in)::ventb

    call MSP_effacer_vent(venta)

    venta%iwind=ventb%iwind

    if (associated(ventb%zvent)) then
       allocate(venta%zvent(size(ventb%zvent)))
       venta%zvent(:)=ventb%zvent(:)
    end if
    if (associated(ventb%wvent)) then
       allocate(venta%wvent(size(ventb%wvent)))
       venta%wvent(:)=ventb%wvent(:)
    end if
    if (associated(ventb%avent)) then
       allocate(venta%avent(size(ventb%avent)))
       venta%avent(:)=ventb%avent(:)
    end if

    venta%type_vent = ventb%type_vent
    venta%vent_nord = ventb%vent_nord
    venta%vent_est = ventb%vent_est
    venta%vent_vert = ventb%vent_vert

    if (ASSOCIATED(ventb%vent_dim1_nord)) then 
       ALLOCATE(venta%vent_dim1_nord(size(ventb%vent_dim1_nord)))
       venta%vent_dim1_nord(:) = ventb%vent_dim1_nord(:)
    end if
    
    if (ASSOCIATED(ventb%vent_dim1_est)) then 
       ALLOCATE(venta%vent_dim1_est(size(ventb%vent_dim1_est)))
       venta%vent_dim1_est(:) = ventb%vent_dim1_est(:)
    end if
    
    if (ASSOCIATED(ventb%vent_dim1_vert)) then 
       ALLOCATE(venta%vent_dim1_vert(size(ventb%vent_dim1_vert)))
       venta%vent_dim1_vert(:) = ventb%vent_dim1_vert(:)
    end if
    
    if (ASSOCIATED(ventb%par1_vent)) then 
       ALLOCATE(venta%par1_vent(size(ventb%par1_vent)))
       venta%par1_vent(:) = ventb%par1_vent(:)
    end if
    
    if (ASSOCIATED(ventb%par2_vent)) then 
       ALLOCATE(venta%par2_vent(size(ventb%par2_vent)))
       venta%par2_vent(:) = ventb%par2_vent(:)
    end if
    
    if (ASSOCIATED(ventb%vent_dim2_nord)) then 
       ALLOCATE(venta%vent_dim2_nord(size(ventb%vent_dim2_nord, DIM=1), size(ventb%vent_dim2_nord, DIM=2)))
       venta%vent_dim2_nord(:, :) = ventb%vent_dim2_nord(:, :)
    end if
    
    if (ASSOCIATED(ventb%vent_dim2_est)) then 
       ALLOCATE(venta%vent_dim2_est(size(ventb%vent_dim2_est, DIM=1), size(ventb%vent_dim2_est, DIM=2)))
       venta%vent_dim2_est(:, :) = ventb%vent_dim2_est(:, :)
    end if
    
    if (ASSOCIATED(ventb%vent_dim2_vert)) then 
       ALLOCATE(venta%vent_dim2_vert(size(ventb%vent_dim2_vert, DIM=1), size(ventb%vent_dim2_vert, DIM=2)))
       venta%vent_dim2_vert(:, :) = ventb%vent_dim2_vert(:, :)
    end if

!    desallocation ajoutee par S ROSTAN, a confirmer avec des tests

    if  ( ventb%flag_func) call MSP_effacer_vent(ventb)
    venta%flag_func=.false.

  end subroutine egaler_vent

  subroutine MSP_effacer_vent(vent,nul)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_effacer_vent
!
!$Resume
!	Routine permettant de désallouer proprement une structure vent
!
!$Description
!	Routine permettant de désallouer proprement une structure vent
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_effacer_vent(vent,[nul])
!.    type(msp_vent) :: vent
!.    logical :: nul
!
!$Arguments
!>E/S   vent  :<msp_vent>   
!>[E/S] nul   :<logical>    
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! EFFACER VENT
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type(msp_vent)::vent
    logical, optional :: nul

    logical :: nul_tmp

    integer :: MSP_iostat

    MSP_iostat = 0
       
    if ( present (nul) ) then
       nul_tmp = nul
    else
       nul_tmp = .false.
    endif

    vent%iwind = 0
    vent%type_vent=0
    vent%vent_nord=0._PM_REEL
    vent%vent_est=0._PM_REEL
    vent%vent_vert=0._PM_REEL
    

      if ( nul_tmp ) then
         ! On se contente d'enlever les liens sans désallouer
         nullify(vent%zvent)
         nullify(vent%wvent)
         nullify(vent%avent)
         nullify(vent%vent_dim1_nord)
         nullify(vent%vent_dim1_est)
         nullify(vent%vent_dim1_vert)
         nullify(vent%vent_dim2_nord)
         nullify(vent%vent_dim2_est)
         nullify(vent%vent_dim2_vert)
         nullify(vent%par1_vent)
         nullify(vent%par2_vent)

      else
       ! -- Le vent a effacer etait déja utilisée
         ! déallocation des pointer
         if (associated(vent%zvent)) then
            deallocate(vent%zvent,stat=MSP_iostat)
         end if
         if (associated(vent%wvent)) then
            deallocate(vent%wvent,stat=MSP_iostat)
         end if
         if (associated(vent%avent)) then
            deallocate(vent%avent,stat=MSP_iostat)
         end if

          ! -- Desallocation de vent_dim1_nord
          if (associated(vent%vent_dim1_nord)) then
             deallocate(vent%vent_dim1_nord,stat=MSP_iostat)
          end if

          ! -- Desallocation de vent_dim1_est
          if (associated(vent%vent_dim1_est)) then
             deallocate(vent%vent_dim1_est,stat=MSP_iostat)
          end if

          ! -- Desallocation de vent_dim1_vert
          if (associated(vent%vent_dim1_vert)) then
             deallocate(vent%vent_dim1_vert,stat=MSP_iostat)
          end if

          ! -- Desallocation de vent_dim2_nord
          if (associated(vent%vent_dim2_nord)) then
             deallocate(vent%vent_dim2_nord,stat=MSP_iostat)
          end if

          ! -- Desallocation de vent_dim2_est
          if (associated(vent%vent_dim2_est)) then
             deallocate(vent%vent_dim2_est,stat=MSP_iostat)
          end if

          ! -- Desallocation de vent_dim2_vert
          if (associated(vent%vent_dim2_vert)) then
             deallocate(vent%vent_dim2_vert,stat=MSP_iostat)
          end if

          ! -- Desallocation de par1_vent
          if (associated(vent%par1_vent)) then
             deallocate(vent%par1_vent,stat=MSP_iostat)
          end if

          ! -- Desallocation de par2_vent
          if (associated(vent%par2_vent)) then
             deallocate(vent%par2_vent,stat=MSP_iostat)
          end if

      end if
  end subroutine MSP_effacer_vent



  subroutine MSP_modifier_vent(vent,iwind,zvent,wvent,avent,vent_nord,vent_est,vent_vert,&
                                  vent_dim1_nord,vent_dim1_est,vent_dim1_vert,&
                                  vent_dim2_nord,vent_dim2_est,vent_dim2_vert,&
                                  par1_vent,par2_vent,type_vent) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_vent
!
!$Resume
!  Routine de modification d'une structure vent 
!
!$Description
!  Routine de modification d'une structure vent
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_vent(vent,[iwind],[zvent],[wvent],[avent],[vent_nord],[vent_est],[vent_vert],&
!.                                      [vent_dim1_nord],[vent_dim1_est],[vent_dim1_vert],&
!.                                      [vent_dim2_nord],[vent_dim2_est],[vent_dim2_vert],&
!.                                      [par1_vent],[par2_vent],[type_vent]) 
!.    type(msp_vent) :: vent
!.    integer :: iwind
!.    real(kind=pm_reel),dimension(:),pointer :: zvent,wvent,avent
!.    real(kind=PM_REEL) :: vent_nord,vent_est,vent_vert
!.    real(kind=PM_REEL),pointer,dimension(:) :: vent_dim1_nord,vent_dim1_est,vent_dim1_vert
!.    real(kind=PM_REEL),pointer,dimension(:,:) :: vent_dim2_nord,vent_dim2_est,vent_dim2_vert
!.    real(kind=PM_REEL),pointer,dimension(:) :: par1_vent,par2_vent
!.    integer :: type_vent
!
!$Arguments
!>E/S   vent            :<msp_vent>                    
!>[E]   iwind           :<integer>                     
!>[E/S] zvent           :<pm_reel,DIM=(:),pointer>     altitude
!>[E/S] wvent           :<pm_reel,DIM=(:),pointer>     vitesse du vent en m/s
!>[E/S] avent           :<pm_reel,DIM=(:),pointer>     azimuth du vent
!>[E]   vent_nord       :<PM_REEL>                     |
!>[E]   vent_est        :<PM_REEL>                     Vent constant
!>[E]   vent_vert       :<PM_REEL>                     |
!>[E/S] vent_dim1_nord  :<PM_REEL,DIM=(:),pointer>     |
!>[E/S] vent_dim1_est   :<PM_REEL,DIM=(:),pointer>     Vent tabule a 1 dimension
!>[E/S] vent_dim1_vert  :<PM_REEL,DIM=(:),pointer>     |
!>[E/S] vent_dim2_nord  :<PM_REEL,DIM=(:,:),pointer>   |
!>[E/S] vent_dim2_est   :<PM_REEL,DIM=(:,:),pointer>   Vent tabule a 2 dimensions
!>[E/S] vent_dim2_vert  :<PM_REEL,DIM=(:,:),pointer>   |
!>[E/S] par1_vent       :<PM_REEL,DIM=(:),pointer>     grille du parametre 1 du vent
!>[E/S] par2_vent       :<PM_REEL,DIM=(:),pointer>     grille du parametre 2 du vent
!>[E]   type_vent       :<integer>                     type du vent (MSP_ENUM_SANS_VENT,MSP_ENUM_VENT_MODATM,MSP_ENUM_VENT_CONST,MSP_ENUM_VENT_TAB_1,MSP_ENUM_VENT_TAB_2)
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! MODIFIER VENT
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    type(msp_vent)::vent
    integer,intent(in),optional::iwind
    real(kind=pm_reel),dimension(:),pointer,optional::zvent,wvent,avent

    real(kind=PM_REEL),intent(IN),optional ::vent_nord,vent_est,vent_vert
    real(kind=PM_REEL),optional,pointer,dimension(:) ::vent_dim1_nord,vent_dim1_est,vent_dim1_vert
    real(kind=PM_REEL),optional,pointer,dimension(:,:) ::vent_dim2_nord,vent_dim2_est,vent_dim2_vert
    real(kind=PM_REEL),optional,pointer,dimension(:) ::par1_vent,par2_vent
    integer,intent(IN),optional ::type_vent
    
    if (present(iwind)) then
       vent%iwind = iwind
    end if

    if ( present(type_vent))   then
       vent%type_vent = type_vent
    end if
    if ( present(vent_nord))    then
       vent%vent_nord = vent_nord
    end if
    if ( present(vent_est))     then
       vent%vent_est = vent_est
    end if
    if ( present(vent_vert))     then
       vent%vent_vert = vent_vert
    end if
    if ( present(type_vent) )     then
       vent%type_vent = type_vent
    end if

    if (present(zvent)) then
       if (associated(zvent)) then
          if (associated(vent%zvent)) deallocate(vent%zvent)
          allocate(vent%zvent(size(zvent)))
          vent%zvent(:)=zvent(:)
       end if
    end if
    if (present(wvent)) then
       if (associated(wvent)) then
          if (associated(vent%wvent)) deallocate(vent%wvent)
          allocate(vent%wvent(size(wvent)))
          vent%wvent(:)=wvent(:)
       end if
    end if
    if (present(avent)) then
       if (associated(avent)) then
          if (associated(vent%avent)) deallocate(vent%avent)
          allocate(vent%avent(size(avent)))
          vent%avent(:)=avent(:)
       end if
    end if

    if ( present(vent_dim1_nord)) then 
       if (ASSOCIATED(vent%vent_dim1_nord)) DEALLOCATE(vent%vent_dim1_nord)
       ALLOCATE(vent%vent_dim1_nord(size(vent_dim1_nord)))
       vent%vent_dim1_nord(:) = vent_dim1_nord(:)
    end if
    if ( present(vent_dim1_est)) then 
       if (ASSOCIATED(vent%vent_dim1_est)) DEALLOCATE(vent%vent_dim1_est)
       ALLOCATE(vent%vent_dim1_est(size(vent_dim1_est)))
       vent%vent_dim1_est(:) = vent_dim1_est(:)
    end if
    if ( present(vent_dim1_vert)) then 
       if (ASSOCIATED(vent%vent_dim1_vert)) DEALLOCATE(vent%vent_dim1_vert)
       ALLOCATE(vent%vent_dim1_vert(size(vent_dim1_vert)))
       vent%vent_dim1_vert(:) = vent_dim1_vert(:)
    end if
    
    if ( present(vent_dim2_nord)) then 
       if (ASSOCIATED(vent%vent_dim2_nord)) DEALLOCATE(vent%vent_dim2_nord)
       ALLOCATE(vent%vent_dim2_nord(size(vent_dim2_nord, DIM=1), size(vent_dim2_nord, DIM=2)))
       vent%vent_dim2_nord(:, :) = vent_dim2_nord(:, :)
    end if
    if ( present(vent_dim2_est)) then 
       if (ASSOCIATED(vent%vent_dim2_est)) DEALLOCATE(vent%vent_dim2_est)
       ALLOCATE(vent%vent_dim2_est(size(vent_dim2_est, DIM=1), size(vent_dim2_est, DIM=2)))
       vent%vent_dim2_est(:, :) = vent_dim2_est(:, :)
    end if
    if ( present(vent_dim2_vert)) then 
       if (ASSOCIATED(vent%vent_dim2_vert)) DEALLOCATE(vent%vent_dim2_vert)
       ALLOCATE(vent%vent_dim2_vert(size(vent_dim2_vert, DIM=1), size(vent_dim2_vert, DIM=2)))
       vent%vent_dim2_vert(:, :) = vent_dim2_vert(:, :)
    end if
    
    if ( present(par1_vent)) then 
       if (ASSOCIATED(vent%par1_vent)) DEALLOCATE(vent%par1_vent)
       ALLOCATE(vent%par1_vent(size(par1_vent)))
       vent%par1_vent(:) = par1_vent(:)
    end if
    if ( present(par2_vent)) then 
       if (ASSOCIATED(vent%par2_vent)) DEALLOCATE(vent%par2_vent)
       ALLOCATE(vent%par2_vent(size(par2_vent)))
       vent%par2_vent(:) = par2_vent(:)
    end if



  end subroutine MSP_modifier_vent

 function MSP_creer_vent(iwind,zvent,wvent,avent, vent_nord,vent_est,vent_vert,&
                                  vent_dim1_nord,vent_dim1_est,vent_dim1_vert,&
                                  vent_dim2_nord,vent_dim2_est,vent_dim2_vert,&
                                  par1_vent,par2_vent,type_vent) result (vent)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_vent
!
!$Resume
!  Fonction servant à créer une loi de type MSP_VENT.
!
!$Description
!  Fonction servant à créer une loi de type MSP_VENT.
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  vent = MSP_creer_vent([iwind],[zvent],[wvent],[avent], [vent_nord],[vent_est],[vent_vert],&
!.                                      [vent_dim1_nord],[vent_dim1_est],[vent_dim1_vert],&
!.                                      [vent_dim2_nord],[vent_dim2_est],[vent_dim2_vert],&
!.                                      [par1_vent],[par2_vent],[type_vent])
!.    integer :: iwind
!.    real(kind=pm_reel),dimension(:),pointer :: zvent,wvent,avent
!.    real(kind=PM_REEL) :: vent_nord,vent_est,vent_vert
!.    real(kind=PM_REEL),pointer,dimension(:) :: vent_dim1_nord,vent_dim1_est,vent_dim1_vert
!.    real(kind=PM_REEL),pointer,dimension(:,:) :: vent_dim2_nord,vent_dim2_est,vent_dim2_vert
!.    real(kind=PM_REEL),pointer,dimension(:) :: par1_vent,par2_vent
!.    integer :: type_vent
!.    type(msp_vent) :: vent
!
!$Arguments
!>[E]   iwind           :<integer>                     
!>[E/S] zvent           :<pm_reel,DIM=(:),pointer>     altitude
!>[E/S] wvent           :<pm_reel,DIM=(:),pointer>     vitesse du vent en m/s
!>[E/S] avent           :<pm_reel,DIM=(:),pointer>     azimuth du vent
!>[E]   vent_nord       :<PM_REEL>                     |
!>[E]   vent_est        :<PM_REEL>                     Vent constant
!>[E]   vent_vert       :<PM_REEL>                     |
!>[E/S] vent_dim1_nord  :<PM_REEL,DIM=(:),pointer>     |
!>[E/S] vent_dim1_est   :<PM_REEL,DIM=(:),pointer>     Vent tabule a 1 dimension
!>[E/S] vent_dim1_vert  :<PM_REEL,DIM=(:),pointer>     |
!>[E/S] vent_dim2_nord  :<PM_REEL,DIM=(:,:),pointer>   |
!>[E/S] vent_dim2_est   :<PM_REEL,DIM=(:,:),pointer>   Vent tabule a 2 dimensions
!>[E/S] vent_dim2_vert  :<PM_REEL,DIM=(:,:),pointer>   |
!>[E/S] par1_vent       :<PM_REEL,DIM=(:),pointer>     grille du parametre 1 du vent
!>[E/S] par2_vent       :<PM_REEL,DIM=(:),pointer>     grille du parametre 2 du vent
!>[E]   type_vent       :<integer>                     type du vent (MSP_ENUM_SANS_VENT,MSP_ENUM_VENT_MODATM,MSP_ENUM_VENT_CONST,MSP_ENUM_VENT_TAB_1,MSP_ENUM_VENT_TAB_2,
!.                                      MSP_ENUM_VENT_MODATM_CONST ,
!.                                      MSP_ENUM_VENT_MODATM_TAB_1 ,
!.                                      MSP_ENUM_VENT_MODATM_TAB_2  
!>S     vent            :<msp_vent>                    
!
!$Common
!
!$Routines
!- msp_effacer_vent
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  VENT CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    integer,intent(in),optional::iwind
    real(kind=pm_reel),dimension(:),pointer,optional::zvent,wvent,avent

      real(kind=PM_REEL),intent(IN),optional ::vent_nord,vent_est,vent_vert
      real(kind=PM_REEL),optional,pointer,dimension(:) ::vent_dim1_nord,vent_dim1_est,vent_dim1_vert
      real(kind=PM_REEL),optional,pointer,dimension(:,:) ::vent_dim2_nord,vent_dim2_est,vent_dim2_vert
      real(kind=PM_REEL),optional,pointer,dimension(:) ::par1_vent,par2_vent
      integer,intent(IN),optional ::type_vent

      !variables locales
      integer::ndim1_grille,ndim2_grille
      integer::ndim1_v_nord,ndim1_v_est,ndim1_v_vert
      integer::ndim2_v_nord,ndim2_v_est,ndim2_v_vert



    type(msp_vent)::vent

    call msp_effacer_vent(vent,nul=.true.)
    vent%flag_func=.true.

    if (present(iwind)) then
       vent%iwind = iwind
    end if

    if (present(zvent)) then
       if (associated(zvent)) then
          allocate(vent%zvent(size(zvent)))
          vent%zvent(:)=zvent(:)
       end if
    end if
    if (present(wvent)) then
       if (associated(wvent)) then
          allocate(vent%wvent(size(wvent)))
          vent%wvent(:)=wvent(:)
       end if
    end if
    if (present(avent)) then
       if (associated(avent)) then
          allocate(vent%avent(size(avent)))
          vent%avent(:)=avent(:)
       end if
    end if


      ! Lecture du type de vent
    if ( present(type_vent)) then
       
       select case (type_vent) 
          
          ! -- Cas du vent constant
       case (MSP_ENUM_VENT_CONST, MSP_ENUM_VENT_MODATM_CONST,MSP_ENUM_VENT_MODATM ) 
          
          vent%type_vent = type_vent
          
          if ( present(vent_nord) ) then 
             vent%vent_nord = vent_nord 
          else 
             vent%vent_nord = 0._PM_REEL 
          end if
          
          if ( present(vent_est) ) then 
             vent%vent_est = vent_est 
          else 
             vent%vent_est = 0._PM_REEL 
          end if
          
          if ( present(vent_vert) ) then 
             vent%vent_vert = vent_vert 
          else 
             vent%vent_vert = 0._PM_REEL 
          end if
          
          ! -- Cas du vent à 1 dimension
       case (MSP_ENUM_VENT_TAB_1, MSP_ENUM_VENT_MODATM_TAB_1) 
          
          vent%type_vent = type_vent
          
          ! Test sur la presence de la grille dim1_vent 
          if ( present(par1_vent) ) then 
             
             ! Allocation dynamique de la grille du modele d'atmosphere 
             ndim1_grille = size(par1_vent) 
             
             allocate(vent%par1_vent(ndim1_grille)) 
             vent%par1_vent(:) = par1_vent(:) 

          else 

             call MSP_signaler_message (cle_mes="MSP_creer_vent_001",routine="MSP_creer_vent") 
             return 
             
          end if
          
          ! -- Vent Nord à une dimension
          if ( present(vent_dim1_nord) ) then 

             ! test sur la coherence avec la grille par1_vent 
             ndim1_v_nord = size(vent_dim1_nord) 
             
             if ( ndim1_v_nord == ndim1_grille ) then 
                allocate(vent%vent_dim1_nord(ndim1_v_nord)) 
                vent%vent_dim1_nord(:) = vent_dim1_nord(:) 
             else 
                call MSP_signaler_message (cle_mes="MSP_creer_vent_002",routine="MSP_creer_vent") 
                return 
             end if

          else 

             ! Initialisation a 0 de la valeur du vent Nord
             allocate(vent%vent_dim1_nord(ndim1_grille)) 
             vent%vent_dim1_nord(:) = 0._PM_REEL 

          end if

          ! -- Vent Est à une dimension
          if ( present(vent_dim1_est) ) then 

             ! test sur la coherence avec la grille par1_vent 
             ndim1_v_est = size(vent_dim1_est) 

             if ( ndim1_v_est == ndim1_grille ) then 
                allocate(vent%vent_dim1_est(ndim1_v_est)) 
                vent%vent_dim1_est(:) = vent_dim1_est(:) 
             else 
                call MSP_signaler_message (cle_mes="MSP_creer_vent_002",routine="MSP_creer_vent") 
                return 
             end if

          else 

             ! Initialisation a 0 de la valeur du vent Est
             allocate(vent%vent_dim1_est(ndim1_grille)) 
             vent%vent_dim1_est(:) = 0._PM_REEL 

          end if

          ! -- Vent Vertical à une dimension
          if ( present(vent_dim1_vert) ) then 

             ! test sur la coherence avec la grille par1_vent 
             ndim1_v_vert = size(vent_dim1_vert) 

             if ( ndim1_v_vert == ndim1_grille ) then 
                allocate(vent%vent_dim1_vert(ndim1_v_vert)) 
                vent%vent_dim1_vert(:) = vent_dim1_vert(:) 
             else 
                call MSP_signaler_message (cle_mes="MSP_creer_vent_002",routine="MSP_creer_vent") 
                return 
             end if

          else 

             ! Initialisation a 0 de la valeur du vent Vertical 
             allocate(vent%vent_dim1_vert(ndim1_grille)) 
             vent%vent_dim1_vert(:) = 0._PM_REEL 

          end if

       ! -- Cas du vent à 2 dimensions
       case (MSP_ENUM_VENT_TAB_2, MSP_ENUM_VENT_MODATM_TAB_2) 

          vent%type_vent = type_vent

           ! Test sur la presence de la grille dim1_vent
            if ( present(par1_vent).and.present(par2_vent)) then
               
               ! Allocation dynamique de la grille 1 du modele d'atmosphere
               ndim1_grille=size(par1_vent)

               allocate(vent%par1_vent(ndim1_grille))
               vent%par1_vent(:) = par1_vent(:)

               ! Allocation dynamique de la grille 2 du modele d'atmosphere
               ndim2_grille=size(par2_vent)

               allocate(vent%par2_vent(ndim2_grille))
               vent%par2_vent(:) = par2_vent(:)

            else
               call MSP_signaler_message (cle_mes="MSP_creer_vent_001",routine="MSP_creer_vent")
               return
            end if

            ! -- Vent Nord à deux dimensions
            if (present(vent_dim2_nord)) then

               ! test sur la coherence avec la grille par1_vent
               ndim1_v_nord=size(vent_dim2_nord,dim=1)
               ndim2_v_nord=size(vent_dim2_nord,dim=2)

               if (( ndim1_v_nord == ndim1_grille).and.( ndim2_v_nord == ndim2_grille)) then

                  allocate(vent%vent_dim2_nord(ndim1_v_nord,ndim2_v_nord))
                  vent%vent_dim2_nord(:,:) = vent_dim2_nord(:,:)
               else
                  call MSP_signaler_message (cle_mes="MSP_creer_vent_002",routine="MSP_creer_vent")
                  return
               end if
            else

               ! Initialisation a 0 de la valeur du vent Nord
               
               allocate(vent%vent_dim2_nord(ndim1_grille,ndim2_grille))
               vent%vent_dim2_nord(:,:) = 0._PM_REEL

            end if

            ! -- Vent Est à deux dimensions
            if (present(vent_dim2_est)) then

               ! test sur la coherence avec la grille par1_vent
               ndim1_v_est=size(vent_dim2_est,dim=1)
               ndim2_v_est=size(vent_dim2_est,dim=2)

               if (( ndim1_v_est == ndim1_grille).and.( ndim2_v_est == ndim2_grille)) then

                  allocate(vent%vent_dim2_est(ndim1_v_est,ndim2_v_est))
                  vent%vent_dim2_est(:,:) = vent_dim2_est(:,:)
               else
                  call MSP_signaler_message (cle_mes="MSP_creer_vent_002",routine="MSP_creer_vent")
                  return
               end if
            else

               ! Initialisation a 0 de la valeur du vent Est
               
               allocate(vent%vent_dim2_est(ndim1_grille,ndim2_grille))
               vent%vent_dim2_est(:,:) = 0._PM_REEL

            end if
                  
            ! -- Vent Vertical à deux dimensions
            if (present(vent_dim2_vert)) then

               ! test sur la coherence avec la grille par1_vent
               ndim1_v_vert=size(vent_dim2_vert,dim=1)
               ndim2_v_vert=size(vent_dim2_vert,dim=2)

               if (( ndim1_v_vert == ndim1_grille).and.( ndim2_v_vert == ndim2_grille)) then

                  allocate(vent%vent_dim2_vert(ndim1_v_vert,ndim2_v_vert))
                  vent%vent_dim2_vert(:,:) = vent_dim2_vert(:,:)
               else
                  call MSP_signaler_message (cle_mes="MSP_creer_vent_002",routine="MSP_creer_vent")
                  return
               end if
            else

               ! Initialisation a 0 de la valeur du vent Vertical
               
               allocate(vent%vent_dim2_vert(ndim1_grille,ndim2_grille))
               vent%vent_dim2_vert(:,:) = 0._PM_REEL

            end if

       ! -- Type de vent inconnu
       case default
          call MSP_signaler_message (cle_mes="MSP_creer_vent_003",routine="MSP_creer_vent")
          return
       end select

     else
        ! Initialisation par defaut au modele d'atmosphere
        vent%type_vent=MSP_ENUM_VENT_MODATM
     end if




  end function MSP_creer_vent

  subroutine MSP_cal_vent(vent,z,wind,aziw,windm,aziwm)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_cal_vent
!
!$Resume
!    Calcul de la vitesse et de l'azimut du vent
!
!$Description
!    Calcul de la vitesse et de l'azimut du vent
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_cal_vent(vent,z,wind,aziw,[windm],[aziwm])
!.    type (msp_vent) :: vent
!.    real(kind=pm_reel) :: z
!.    real(kind=pm_reel) :: wind,aziw
!.    real(kind=pm_reel) :: windm,aziwm
!
!$Arguments
!>E     vent   :<msp_vent>   altitude geodesique en metres
!>E     z      :<pm_reel>    
!>S     wind   :<pm_reel>    vitesse du vent en m/s
!>S     aziw   :<pm_reel>    azimut du vent en radians
!>[E]   windm  :<pm_reel>    vitesse du vent en m/s
!>[E]   aziwm  :<pm_reel>    azimut du vent en radians
!
!$Common
!
!$Routines
!- vent_tab
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! CALCULER VENT
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    type (msp_vent),intent(in)::vent
    real(kind=pm_reel),intent(in)::z
    real(kind=pm_reel),intent(out)::wind,aziw
    real(kind=pm_reel),intent(in),optional::windm,aziwm


    ! variables locales
    real(kind=pm_reel)::wind2,aziw2
    real(kind=pm_reel)::wind1,aziw1
    real(kind=pm_reel)::windu,windv

    ! Epsilon utilise pour la comparaison de deux PM_REEL (en general)
    real(kind=pm_reel), parameter :: LOC_EPSILON = 1.e-15_pm_reel

    
    ! Initialisation
    wind1 = 0._PM_REEL
    aziw1 = 0._PM_REEL

    if (present(windm)) then
       wind2=windm
    else
       wind2=0._PM_REEL
    end if

    if (present(aziwm)) then
       aziw2=aziwm
    else
       aziw2=0._PM_REEL
    end if

    select case (vent%iwind)

       case (msp_enum_vent_tab)
          ! calcul de la partie tabulée du vent
          call vent_tab (z,vent,wind,aziw)
       case (msp_enum_vent_modele) 
          ! calcul du vent selon le modèle gram
          wind = wind2
          aziw = aziw2
       case (msp_enum_vent_tout)
          ! calcul de la partie tabulée du vent
          call vent_tab (z,vent,wind1,aziw1)

          ! calcul du vent selon le modèle gram
          windv = (wind1*COS(aziw1))+(wind2*COS(aziw2))
          windu = (wind1*SIN(aziw1))+(wind2*SIN(aziw2))

          wind = SQRT(windu**2+windv**2)
          if (ABS(windv) < LOC_EPSILON) then
             if (windu == 0.0_PM_REEL) then
                aziw = 0.0_PM_REEL
             else if (windu > 0.0_PM_REEL) then
                aziw = PM_PI_SUR2
             else
                aziw = -PM_PI_SUR2
             endif
          else
             aziw = ATAN(windu/windv)
          endif
          if ((windu == 0.0_PM_REEL) .and. (windv < 0.0_PM_REEL)) then
             aziw = PM_PI
          end if
          if ((windu > 0.0_PM_REEL) .and. (windv < 0.0_PM_REEL)) then
             aziw = aziw+PM_PI
          end if
          if ((windu < 0.0_PM_REEL) .and. (windv < 0.0_PM_REEL)) then
             aziw = aziw-PM_PI
          end if
       case default
             wind = 0._pm_reel
             aziw = 0._pm_reel
       end select
     end subroutine MSP_cal_vent

      subroutine vent_tab (z,vent,wind,aziw)


!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  vent_tab
!
!$Resume
!	Sous-programme determinant la partie tabulee de la vitesse du vent et son azimut
!
!$Description
!	Sous-programme determinant la partie tabulee de la vitesse du vent et son azimut
!
!$Mots-cles
!  VENT TABULE
!
!$Acces
!  PUBLIC
!
!$Usage
!  call vent_tab (z,vent,wind,aziw)
!.    real (KIND=pm_reel) :: z
!.    type(msp_vent) :: vent
!.    real (KIND=pm_reel) :: wind,aziw
!
!$Arguments
!>E/S   z     :<pm_reel>    altitude geodesique en metres
!>E     vent  :<msp_vent>   
!>E/S   wind  :<pm_reel>    vitesse du vent en m/s
!>E/S   aziw  :<pm_reel>    azimut du vent en radians
!
!$Include
!
!$Auteur
!	A.WAGNER,J.F. GOESTER
!
!$Historique
!	17/07/89  version initiale
!
!$FinHistorique
!
!$Remarques
!
!$<>
!***********************************************************************


      implicit none

!     Arguments

      real (KIND=pm_reel) :: z
      type(msp_vent),intent(in)::vent
      real (KIND=pm_reel) :: wind,aziw


!     Variables locales

      integer,dimension(1) :: i



      wind = 0._pm_reel
      aziw = 0._pm_reel

!   ********************************************************************
!   * Encadrement de r parmi les valeurs tabules dans le fichier vent  *
!   ********************************************************************

      if (z<=vent%zvent(1)) then
         wind=vent%wvent(1)
         aziw=vent%avent(1)
      elseif (z>=vent%zvent(size(vent%zvent))) then
         wind=vent%wvent(size(vent%zvent))
         aziw=vent%avent(size(vent%zvent))
      else
         
         i=maxloc(vent%zvent,MASK=vent%zvent<z) 
         !   ********************************************************************
         !   * Calcul de wind et de aziw par interpolation                      *
         !   ********************************************************************
         wind=((vent%zvent(i(1)+1)-z)*vent%wvent(i(1))+ &
              (z-vent%zvent(i(1)))*vent%wvent(i(1)+1))/(vent%zvent(i(1)+1)-vent%zvent(i(1)))
         aziw=((vent%zvent(i(1)+1)-z)*vent%avent(i(1))+ &
              (z-vent%zvent(i(1)))*vent%avent(i(1)+1))/(vent%zvent(i(1)+1)-vent%zvent(i(1)))
        
      end if
      

      !   ********************************************************************
      !   * Modification de aziw suivant la caracteristique du vent          *
      !   ********************************************************************
      
      if(abs(aziw) > PM_PI) then
         aziw=aziw-abs(aziw)*PM_DEUX_PI/aziw
      endif
      

    end subroutine vent_tab

   end module msp_vent_def
