module msp_modvent_def

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  msp_modvent_def
!
!$Resume
!	Module permettant de calculer les composantes du vent
!
!$Description
!       Module permettant de calculer les composantes du vent
!       (modèle conçu pour les modèles de vents dans PSIMU)
!       Le repère de référence est le topocentrique Nord
!
!$Auteur
!
!$Version
!  $Id: MSP_MODVENT_DEF.F90 365 2013-02-18 12:36:19Z aadt $
!
!$Historique
!  $Log: MSP_MODVENT_DEF.F90,v $
!  Revision 365  2013/02/18 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.21  2010/10/20 09:35:43  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.20  2008/11/19 13:32:27  mercadig
!  DM-ID 733 : Mise a jour cartouche
!
!  Revision 1.19  2008/08/08 14:51:51  gss
!  DM-ID 1058 : (portage g95) initialisation à NULL des pointeurs lors de leur
!  déclaration. Ajout d'une gestion d'erreur pour l'accès madona.
!  Revision 1.18  2008/07/04 14:57:49  huec
!  DM-ID 1058 : Gestion memoire
!  Revision 1.17  2008/05/22 07:00:22  huec
!  FA-ID 1044 : Correction de declaration de parametre
!  Revision 1.16  2008/04/25 11:57:46  huec
!  AQ : Correction d erreurs dans les formats d ecriture
!  Revision 1.15  2008/04/24 13:59:04  huec
!  DM-ID 553 : On impose les formats d ecriture
!  Revision 1.14  2008/02/26 08:04:25  huec
!  FA-ID 887 : Ajout de tests d erreur suite a appel COMPAS
!  Revision 1.13  2008/02/18 08:58:27  huec
!  DM-ID 11 : Ajout d une interface MSP_creer_modvent
!  Revision 1.12  2007/10/23 15:01:06  huec
!  FA-ID 776 : Variables locales non utilisees dans la MECASPA
!  Revision 1.11  2007/06/18 10:14:24  tanguyy
!  FA-ID 749 : nouvelle constante MSP_LONG_NOMFIC pour les longueurs des noms de fichiers
!  Revision 1.10  2007/02/02 10:46:31  vivaresf
!  DM-ID 643 : interpolation sur les tableaux d'origine
!  Revision 1.9  2007/02/01 18:08:30  vivaresf
!  DM-ID 643 : validation
!  Revision 1.8  2007/02/01 17:04:26  vivaresf
!  DM-ID 643 : gestion d'erreur
!  Revision 1.7  2007/01/30 16:49:28  vivaresf
!  DM-ID 643 : intégration PSIMU, vents vers le Nord et vers le Sud pour le topo Nord
!  Revision 1.6  2007/01/25 17:09:14  vivaresf
!  DM-ID 643 : repère topocentrique
!  Revision 1.5  2007/01/25 16:19:33  vivaresf
!  Version 4.4a1 : renommage des constantes pour eviter les
!  confusions avce MSP_VENT
!  Revision 1.4  2007/01/25 15:33:48  vivaresf
!  DM-ID 643 : repère topocentrique Nord pour PSIMU
!  Revision 1.3  2007/01/25 15:09:36  vivaresf
!  DM-ID 643 : validation
!  Revision 1.2  2007/01/25 14:35:47  vivaresf
!  DM-ID 643 : prise en compte du modèle ARPEGE
!  Revision 1.1  2007/01/25 10:48:31  mle
!  DM-ID 643 : modeles de vent dans PSIMU
!  Revision 1.4  2006/06/02 11:21:56  vpg
!
!$FinHistorique
!
!$Usage
!  use msp_modvent_def
!
!$Structure
!
!: msp_modvent : 
!#V
!>     nom          : <LEN=MSP_LONG_CHAINE,private>      nom du vent 
!>     altitude     : <PM_REEL,DIM=(:),pointer,private>  altitude (m)
!>     Vent_SN      : <PM_REEL,DIM=(:),pointer,private>  vent Sud-Nord (m/s)
!                     ou, si modèle ARPEGE, direction (rad)
!>     Vent_EO      : <PM_REEL,DIM=(:),pointer,private>  vent Ouest-Est (m/s)
!                     ou, si modèle ARPEGE, module de la vitesse (m/s)
!>     Vent_vert    : <PM_REEL,DIM=(:),pointer,private>  vent vertical (m/s)
!>     nbvents      : <integer,private>                  Dimension des tableaux ci-dessus 
!>     typevent     : <integer,private>                  Permet de savoir si le vent 
!>                                           est défini sur 2 ou 3 paramètres
!>     modelevent   : <integer,private>                  Type de modèle (1=ATM, 2=ARPEGE) 
!>     flag_func    : <logical,private>                  Voir MAM           
!#
!
!$Global
!
!>  msp_enum_modvent_2param   : <integer,parameter>  indique que le vent est
!                                     défini sur 2 paramètres
!>  msp_enum_modvent_3param   : <integer,parameter>  indique que le vent est 
!                                     défini sur 3 paramètres
!>  msp_enum_modvent_atm      : <integer,parameter>  Modèle ATM
!>  msp_enum_modvent_arpege   : <integer,parameter>  Modèle ARPEGE
!>  msp_enum_modvent_autre    : <integer,parameter>  Modèle autre
!$Routines
!- MSP_creer_modvent
!- MSP_effacer_modvent
!- MSP_modifier_modvent
!- MSP_consulter_modvent
!- MSP_afficher_modvent
!- MSP_calculer_modvent
!#V
!- egaler_vent
!- ordre_croissant
!- reordonner
!#
!
!$Fonctions
!- MSP_lire_modvent
!
!$Include
!
!$Module
!#V
!- MSLIB
!- MSP_GESTION_ERREUR
!- MSP_MECASPA_DEF
!- MSP_BULLETIN_DEF
!- cps_vent
!- cps_utilisateur
!- mspro
!#
!
!$Interface
!> assignment :         egaler_vent
!> msp_creer_modvent :  MSP_lire_modvent
!#V
!#
!
!$Remarques
!
!$Mots-cles
!   VENT
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use MSLIB
  use MSP_GESTION_ERREUR
  use MSP_MECASPA_DEF
  use MSP_BULLETIN_DEF

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_MODVENT_DEF.F90 365 2013-02-18 12:36:19Z aadt $'


  type msp_modvent
     private
     !DM 643 : définition d'une nouvelle structure de vent
     
     character(len=MSP_LONG_CHAINE) ::nom
     
     real(kind=PM_REEL),dimension(:),pointer::altitude => NULL()

     real(kind=PM_REEL),dimension(:),pointer::Vent_SN => NULL()
     real(kind=PM_REEL),dimension(:),pointer::Vent_EO => NULL()
     real(kind=PM_REEL),dimension(:),pointer::Vent_vert => NULL()

     integer :: nbvents    ! dimension des tableaux

     integer :: typevent   ! 2 ou 3 directions
     integer :: modelevent ! type de modèle ATM/ARPEGE/autre
     
     logical :: flag_func

  end type msp_modvent

  ! le vent peut être donnée sur 2 ou 3 paramètres
  !- 2 paramètres : NS + EO
  !- 3 paramètres :  NS + EO + vent vertical (inutilisé actuellement)
  integer,parameter ::msp_enum_modvent_2param  = 2 ! valeur par défaut de typevent
  integer,parameter ::msp_enum_modvent_3param  = 3 ! inutilisé actuellement

  integer,parameter ::msp_enum_modvent_atm  = 1 
  integer,parameter ::msp_enum_modvent_arpege  = 2
  integer,parameter ::msp_enum_modvent_autre  = 0

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
!.    type(msp_modvent) :: venta
!.    type(msp_modvent) :: ventb
!
!$Mots-cles
! EGALER VENT
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure egaler_vent
   end interface

   interface MSP_creer_modvent

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_modvent
!
!$Resume
!  Fonction servant à lire un vent à partir d'un modèle de COMPAS.
!
!$Description
!  Fonction servant à lire un vent à partir d'un modèle de COMPAS.
!  La fonction extrait de COMPAS le type de modèle (ATM ou ARPEGE), 
!  l'emplacement dui fichier associé, puis appèle la fonction de
!  lecture COMPAS ad hoc.
!  Pour le format ARPEGE, on stocke la direction dans ventSN et
!  le module de la vitesse dans ventEO
!  Pour le format ATM, on convertit la paire vent_OE/ventSN en
!  ventSN/ventEO (repère topocentrique Nord).
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  vent = MSP_creer_modvent(modele)
!.    character(len=*) :: modele
!.    type(msp_modvent) :: vent
!
!$Mots-cles
! LIRE VENT CR
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      module procedure MSP_lire_modvent
   end interface

  private ordre_croissant, reordonner, egaler_vent

CONTAINS


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
!  PRIVE
!
!$Usage
!  call egaler_vent(venta,ventb)
!.    type(msp_modvent) :: venta
!.    type(msp_modvent) :: ventb
!
!$Arguments
!>E/S   venta  :<msp_modvent>   Vent en sortie
!>E     ventb  :<msp_modvent>   Vent en entrée
!
!$Mots-cles
!  EGALER VENT
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(msp_modvent),intent(inout)::venta
    type(msp_modvent),intent(in)::ventb

    call MSP_effacer_modvent(venta)

    venta%nom=ventb%nom

    if (associated(ventb%altitude)) then
       allocate(venta%altitude(size(ventb%altitude)))
       venta%altitude(:)=ventb%altitude(:)
    end if
    
    if (associated(ventb%Vent_SN)) then
       allocate(venta%Vent_SN(size(ventb%Vent_SN)))
       venta%Vent_SN(:)=ventb%Vent_SN(:)
    end if
    if (associated(ventb%Vent_EO)) then
       allocate(venta%Vent_EO(size(ventb%Vent_EO)))
       venta%Vent_EO(:)=ventb%Vent_EO(:)
    end if
    if (associated(ventb%Vent_vert)) then
       allocate(venta%Vent_vert(size(ventb%Vent_vert)))
       venta%Vent_vert(:)=ventb%Vent_vert(:) 
    end if

    venta%typevent=ventb%typevent
    venta%modelevent=ventb%modelevent
    venta%nbvents=ventb%nbvents

    venta%flag_func=.false.

  end subroutine egaler_vent

  subroutine MSP_effacer_modvent(vent,nul)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_effacer_modvent
!
!$Resume
!	Routine permettant de désallouer proprement une structure modvent
!
!$Description
!	Routine permettant de désallouer proprement une structure modvent
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_effacer_modvent(vent,[nul])
!.    type(msp_modvent) :: vent
!.    logical :: nul
!
!$Arguments
!>E/S   vent  :<msp_modvent>   structure vent à effacer
!>[E]   nul   :<logical>       si nul on se contente d'initialiser les pointeurs
!
!$Mots-cles
! EFFACER VENT
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none

    type(msp_modvent), intent(inout) :: vent
    logical, optional, intent(in) :: nul

    ! Variables locales
    logical :: nul_tmp
    integer :: MSP_iostat

    ! Initialisations
    MSP_iostat = 0
       
    if ( present (nul) ) then
       nul_tmp = nul
    else
       nul_tmp = .false.
    endif

    vent%nom = ""
    vent%typevent = msp_enum_modvent_2param
    
    ! Mise à 0 des pointeurs
      if ( nul_tmp ) then
         ! On se contente d'enlever les liens sans désallouer
         nullify(vent%altitude)
         nullify(vent%Vent_EO)
         nullify(vent%Vent_SN)
         nullify(vent%Vent_vert)
      else
    ! Effacement
       ! -- Le vent a effacer était déjà utilisée
         ! déallocation des pointers
         if (associated(vent%altitude)) then
            deallocate(vent%altitude,stat=MSP_iostat)
         end if

         if (associated(vent%Vent_EO)) then
            deallocate(vent%Vent_EO,stat=MSP_iostat)
         end if
         if (associated(vent%Vent_SN)) then
            deallocate(vent%Vent_SN,stat=MSP_iostat)
         end if
         if (associated(vent%Vent_vert)) then
            deallocate(vent%Vent_vert,stat=MSP_iostat)
         end if

      end if
    end subroutine MSP_effacer_modvent


  subroutine MSP_modifier_modvent(vent,nom,alt,vent_EO,vent_SN,vent_vert,&
                                  type_vent, modelevent)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_modvent
!
!$Resume
!  Routine de modification d'un ou plusieurs paramètres d'une structure vent
!
!$Description
!  Routine de modification d'un ou plusieurs paramètres d'une structure vent
!  Le repère de référence est le topocentrique Nord
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_modvent(vent,[nom],[alt],[vent_eo],[vent_sn],[vent_vert],&
!.                                      [type_vent], [modelevent])
!.    type(msp_modvent) :: vent
!.    character(len=MSP_LONG_CHAINE) :: nom
!.    real(kind=PM_REEL),pointer,dimension(:) :: alt
!.    real(kind=PM_REEL),pointer,dimension(:) :: vent_EO,vent_SN,vent_vert
!.    integer :: type_vent, modelevent
!
!$Arguments
!>E/S   vent        :<msp_modvent>               Vents
!>[E]   nom         :<LEN=MSP_LONG_CHAINE>       nom du vent
!>[E/S] alt         :<PM_REEL,DIM=(:),pointer>   altitude
!>[E/S] vent_EO     :<PM_REEL,DIM=(:),pointer>   vent Est-Ouest (m/s)
!                     ou, si modèle ARPEGE, module de la vitesse (m/s)
!>[E/S] vent_SN     :<PM_REEL,DIM=(:),pointer>   vent Sud-Nord (m/s)
!                     ou, si modèle ARPEGE, direction (rad)
!>[E/S] vent_vert   :<PM_REEL,DIM=(:),pointer>   vent vertical
!>[E]   type_vent   :<integer>                   type du vent (msp_enum_modvent_2param, msp_enum_modvent_3param)
!>[E]   modelevent  :<integer>                   type de modèle (1=ATM, 2=ARPEGE)
!
!$Mots-cles
! MODIFIER VENT
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(msp_modvent),intent(inout) :: vent
    character(len=MSP_LONG_CHAINE),intent(in),optional::nom
    real(kind=PM_REEL),optional,pointer,dimension(:) ::alt
    real(kind=PM_REEL),optional,pointer,dimension(:) ::vent_EO,vent_SN,vent_vert
    integer,intent(in),optional ::type_vent, modelevent


    ! Variables locales
    integer :: iostat
    
    if (present(nom)) then
       vent%nom = nom
    end if

    if ( present(alt)) then 
       if (ASSOCIATED(vent%altitude)) DEALLOCATE(vent%altitude,stat=iostat)
       ALLOCATE(vent%altitude(size(alt)))
       vent%altitude(:) = alt(:)
    end if

    if ( present(vent_EO)) then 
       if (ASSOCIATED(vent%Vent_EO)) DEALLOCATE(vent%Vent_EO,stat=iostat)
       ALLOCATE(vent%Vent_EO(size(vent_EO)))
       vent%Vent_EO(:) = vent_EO(:)
    end if
    if ( present(vent_SN)) then 
       if (ASSOCIATED(vent%Vent_SN)) DEALLOCATE(vent%Vent_SN,stat=iostat)
       ALLOCATE(vent%Vent_SN(size(vent_SN)))
       vent%Vent_SN(:) = vent_SN(:)
    end if
    if ( present(vent_vert)) then 
       if (ASSOCIATED(vent%Vent_vert)) DEALLOCATE(vent%Vent_vert,stat=iostat)
       ALLOCATE(vent%Vent_vert(size(vent_vert)))
       vent%Vent_vert(:) = vent_vert(:)
    end if

    if (present(type_vent)) then
       vent%typevent = type_vent
    end if

    if (present(modelevent)) then
       vent%modelevent = modelevent
    end if

  end subroutine MSP_modifier_modvent



  subroutine MSP_consulter_modvent(vent,nom,alt,vent_EO,vent_SN,vent_vert,&
       type_vent, modelevent) 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_modvent
!
!$Resume
!  Routine de consultation d'un ou plusieurs paramètres d'une structure vent
!
!$Description
!  Routine de consultation d'un ou plusieurs paramètres d'une structure vent
!  Le repère de référence est le topocentrique Nord
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_modvent(vent,[nom],[alt],[vent_eo],[vent_sn],[vent_vert],&
!.           [type_vent], [modelevent]) 
!.    type(msp_modvent) :: vent
!.    character(len=MSP_LONG_CHAINE) :: nom
!.    real(kind=PM_REEL),pointer,dimension(:) :: alt
!.    real(kind=PM_REEL),pointer,dimension(:) :: vent_EO,vent_SN,vent_vert
!.    integer :: type_vent, modelevent
!
!$Arguments
!>E/S   vent        :<msp_modvent>               
!>[E/S] nom         :<LEN=MSP_LONG_CHAINE>       nom du vent
!>[E/S] alt         :<PM_REEL,DIM=(:),pointer>   altitude
!>[E/S] vent_EO     :<PM_REEL,DIM=(:),pointer>   vent Est-Ouest (m/s)
!                     ou, si modèle ARPEGE, module de la vitesse (m/s)
!>[E/S] vent_SN     :<PM_REEL,DIM=(:),pointer>   vent Sud-Nord (m/s)
!                     ou, si modèle ARPEGE, direction (rad)
!>[E/S] vent_vert   :<PM_REEL,DIM=(:),pointer>   vent vertical
!>[E/S] type_vent   :<integer>                   type du vent (msp_enum_modvent_2param, msp_enum_modvent_3param)
!>[E/S] modelevent  :<integer>                   type de modèle (1=ATM, 2=ARPEGE)
!
!$Mots-cles
! MODIFIER VENT
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(msp_modvent)::vent
    character(len=MSP_LONG_CHAINE),intent(inout),optional::nom
    real(kind=PM_REEL),optional,pointer,dimension(:) ::alt
    real(kind=PM_REEL),optional,pointer,dimension(:) ::vent_EO,vent_SN,vent_vert

    integer,intent(INout),optional ::type_vent,    modelevent

    ! Variables locales
    integer :: iostat

    ! Mises à jours

    if (present(nom)) then
       nom = vent%nom
    end if

    if ( present(alt)) then
       if (ASSOCIATED(alt)) deallocate(alt,stat=iostat)
       if (ASSOCIATED(vent%altitude)) then
          allocate(alt(size(vent%altitude)))
          alt(:) = vent%altitude(:)
       end if
    end if

    if ( present(vent_EO)) then
       if (ASSOCIATED(vent_EO)) deallocate(vent_EO,stat=iostat)
       if (ASSOCIATED(vent%Vent_EO)) then
          allocate(vent_EO(size(vent%Vent_EO)))
          vent_EO(:) = vent%Vent_EO(:)
       end if
    end if
    if ( present(vent_SN)) then
       if (ASSOCIATED(vent_SN)) deallocate(vent_SN,stat=iostat)
       if (ASSOCIATED(vent%Vent_SN)) then
          allocate(vent_SN(size(vent%Vent_SN)))
          vent_SN(:) = vent%Vent_SN(:)
       end if
    end if
    if ( present(vent_vert)) then
       if (ASSOCIATED(vent_vert)) deallocate(vent_vert,stat=iostat)
       if (ASSOCIATED(vent%Vent_vert)) then
          allocate(vent_vert(size(vent%Vent_vert)))
          vent_vert(:) = vent%Vent_vert(:)
       end if
    end if

    if (present(type_vent)) then
       type_vent = vent%typevent
    end if

    if (present(modelevent)) then
       modelevent = vent%modelevent
    end if

  end subroutine MSP_consulter_modvent


  subroutine MSP_afficher_modvent(vent, comm, ilog) 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_afficher_modvent
!
!$Resume
!  Routine d'affichage d'une structure vent
!
!$Description
!  Routine d'affichage d'une structure vent
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_afficher_modvent(vent, [comm], [ilog]) 
!.    type(msp_modvent) :: vent
!.    logical :: comm
!.    integer :: ilog
!
!$Arguments
!>E     vent  :<msp_modvent>   Vent à afficher
!>[E]   comm  :<logical>       affichage des commentaires oui/non 
!>[E]   ilog  :<integer>       Nuléro d'unnité logique où afficher
!
!$Mots-cles
! AFFICHER VENT
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(msp_modvent), intent(in)::vent
    logical, optional, intent(in) :: comm
    integer, optional, intent(in) :: ilog

    ! Variables locales
    integer :: num
    logical :: commentaire

    !initialisation
    commentaire = .true.
    if (present(comm)) commentaire = comm
    
    if ( present(ilog) ) then
       num = ilog
    else
       num = MSP_ENUM_ECRAN
    endif

    ! Affichage
    if (commentaire) write(num,'(a)') " nom :" 
    write(num,'(a)') trim(vent%nom)

    if (commentaire) write(num,'(a)') " altitude"
    if(associated(vent%altitude)) then
       write(num,1000) vent%altitude(:)
    end if

    if (commentaire.and.vent%modelevent.eq.msp_enum_modvent_arpege) then
       write(num,'(a)') " vitesse du vent fonction de l'altitude"
    else if (commentaire) then
       write(num,'(a)') " vent E-O fonction de l'altitude"
    endif
    if(associated(vent%Vent_EO)) then
       write(num,1000) vent%Vent_EO(:)
    end if

    if (commentaire.and.vent%modelevent.eq.msp_enum_modvent_arpege) then
       write(num,'(a)') " direction du vent à partir de l'axe Sud-Nord"
    else if (commentaire) then
       write(num,'(a)') " vent S-N fonction de l'altitude"
    endif

    if(associated(vent%Vent_SN)) then
       write(num,1000) vent%Vent_SN(:)
    end if
 
    if (commentaire) write(num,'(a)') " vent vertical fonction de l'altitude"   

    if(associated(vent%Vent_vert)) then
       write(num,1000) vent%Vent_vert(:)
    endif

    if (commentaire) write(num,'(a)') " type de vent"
    write(num,'(i9)') vent%typevent
    
    if (commentaire) write(num,'(a)') " modèle d'origine"
    
    select case (vent%modelevent)
    case(msp_enum_modvent_atm)
       write(num,'(a)') "  ATM"
    case (msp_enum_modvent_arpege)
       write(num,'(a)') "  ARPEGE"
    case default
       write(num,'(a)') "  Autre modèle"
    end select

1000 format(4(e15.6))

  end subroutine MSP_afficher_modvent


 function MSP_lire_modvent(modele) result (vent)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_lire_modvent
!
!$Resume
!  Fonction servant à lire un vent à partir d'un modèle de COMPAS.
!
!$Description
!  Fonction servant à lire un vent à partir d'un modèle de COMPAS.
!  La fonction extrait de COMPAS le type de modèle (ATM ou ARPEGE), 
!  l'emplacement dui fichier associé, puis appèle la fonction de
!  lecture COMPAS ad hoc.
!  Pour le format ARPEGE, on stocke la direction dans ventSN et
!  le module de la vitesse dans ventEO
!  Pour le format ATM, on convertit la paire vent_OE/ventSN en
!  ventSN/ventEO (repère topocentrique Nord).
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  vent = MSP_lire_modvent(modele)
!.    character(len=*) :: modele
!.    type(msp_modvent) :: vent
!
!$Arguments
!>E     modele  :<LEN=*>         modele de vent dans COMPAS
!>S     vent    :<msp_modvent>   Structure vent
!
!$Mots-cles
!  LIRE VENT CR
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use cps_vent
   use cps_utilisateur
   
   implicit none
   
   character(len=*),intent(in) :: modele
   type(msp_modvent)::vent
   
   ! variables locales
   integer :: nbvent, trouve, iostat
   real(kind=pm_reel), dimension(:), pointer :: alt_tmp => NULL()
   real(kind=pm_reel), dimension(:), pointer :: dir => NULL(), vit => NULL()
   character(LEN=MSP_LONG_NOMFIC):: tmp, fichier
   integer, dimension(:), pointer :: indices => NULL()
   
   ! initialisation
   call MSP_effacer_modvent(vent,nul=.true.)
   vent%typevent = msp_enum_modvent_2param   ! par défaut
      
   if (.not.cps_utilisateur_init) call cps_init_utilisateur ()
   if (MSP_gen_messages("MSP_lire_modvent")) return
      
   ! DETERMINATION DU TYPE DE MODELE
   !--------------------------------
   trouve = cps_requete("modeles_vent", trim(modele), "*",&
        "typemodele", tmp)
   if (trouve == CPS_ERR_DEF) call MSP_signaler_message(cle_mes="MSP_modvent_acces_mad")
   if(trim(tmp).eq."ATM") then
      vent%modelevent = msp_enum_modvent_atm
   elseif(trim(tmp).eq."ARPEGE") then
      vent%modelevent = msp_enum_modvent_arpege
   else
      vent%modelevent = msp_enum_modvent_autre
      ! Format inconnu
      call MSP_signaler_message(cle_mes="MSP_modvent_type")
   end if

   ! DETERMINATION DU NOM DE FICHIER correspondant au modèle
   !--------------------------------------------------------
   ! (.true. indique qu'on veut le chemin absolu du fichier)
   trouve = cps_getFichierModele("vent", trim(modele), fichier, &
        .true.)
   if (MSP_gen_messages("MSP_lire_modvent")) return

   ! LECTURE DU VENT A PARTIR DU FICHIER
   !------------------------------------

   vent%nom = trim(modele)
   ! CAS : vent ATM
   if (vent%modelevent .eq. msp_enum_modvent_atm) then
      call cps_lireVents_ATM(trim(fichier), nbvent, vent%altitude, &
           vent%vent_EO, vent%vent_SN)
      if (MSP_gen_messages("MSP_lire_modvent")) return

      ! Passage dans le repère topo Nord
      vent%vent_EO = vent%vent_EO * (-1)
      vent%vent_SN = vent%vent_SN
      vent%typevent = msp_enum_modvent_2param
   elseif (vent%modelevent .eq. msp_enum_modvent_arpege) then
      ! CAS : vent ARPEGE 
      vent%nom = trim(modele)
      call cps_lireVents_ARPEGE(trim(fichier), nbvent, vent%altitude, &
           vit, dir)
      if (MSP_gen_messages("MSP_lire_modvent")) return
      
      vent%typevent = msp_enum_modvent_2param

      ! Le modèle ARPEGE donne la direction (rad) et le module de la vitesse
      ! on calcule les vents EO et NS rééls à partir de cela.
      allocate(vent%vent_EO(nbvent))
      allocate(vent%vent_SN(nbvent))
      
      ! direction : angle à partir de l'axe orienté Sud/Nord en partant vers l'Est
      vent%vent_SN = dir
      ! Module de la vitesse 
      vent%vent_EO = vit
         
   endif
   vent%nbvents=nbvent
   if (MSP_gen_messages("MSP_lire_modvent")) return

   if (nbvent > 0) then
      allocate(vent%Vent_vert(nbvent))
      vent%Vent_vert(:) = 0._pm_reel
      ! RANGEMENT par ordre croissant de l'altitude
      ! (pour l'appel à mu_inter_dim1_lin dans MSP_calculer_modvent)
      ! altitude par ordre croissant :
      call ordre_croissant(vent%altitude,alt_tmp,indices)
      vent%altitude(:) = alt_tmp(:)
      ! vents en fonction de l'altitude
      call reordonner(vent%Vent_EO,indices)
      call reordonner(vent%Vent_SN,indices)
      if (MSP_gen_messages("MSP_lire_modvent")) return
  endif

  vent%flag_func = .true.
  !deallocation memmoire
  if(associated(alt_tmp))  deallocate(alt_tmp,stat=iostat)
  if(associated(dir))  deallocate(dir,stat=iostat)
  if(associated(vit))  deallocate(vit,stat=iostat)
  if(associated(indices))  deallocate(indices,stat=iostat)
  
end function  MSP_lire_modvent


subroutine MSP_calculer_modvent(vent, alt, vent_EO, vent_SN, ventvert)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_modvent
!
!$Resume
!    Calcul de les trois composantes de la vitesse du vent
!
!$Description
!    Calcul de les trois composantes de la vitesse du vent
!    dans le repère topocentrique Nord (dans le cas ARPEGE la paire
!    direction/vitesse est convertie en X/Y)
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calculer_modvent(vent, alt, vent_EO, vent_SN, [ventvert])
!.    type (msp_modvent) :: vent
!.    real(kind=pm_reel) :: alt
!.    real(kind=pm_reel) :: vent_EO, vent_SN
!.    real(kind=pm_reel) :: ventvert
!
!$Arguments
!>E/S   vent      :<msp_modvent>   vent
!>E     alt       :<pm_reel>       altitude (m)
!>S     vent_EO   :<pm_reel>       composante Est-Ouest de la vitess du vent (m/s)
!>S     vent_SN   :<pm_reel>       composante Sud-Nord de la vitesse du vent (m/s)
!>[S]   ventvert  :<pm_reel>       composante verticale de la vitesse du vent (m/s)
!
!$Mots-cles
! CALCULER VENT
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use mspro
  
  implicit none

  type (msp_modvent), intent(inout) :: vent
  real(kind=pm_reel), intent(in) :: alt
  real(kind=pm_reel), intent(out) :: vent_EO, vent_SN
  real(kind=pm_reel), intent(out), optional :: ventvert

  ! variables locales
  type(tm_code_retour)::code1, code2, code3
  integer :: i_x, nn
  real(kind=pm_reel) :: dir, vit

  ! initialisation

  i_x = 1
  vent_EO = 0_PM_REEL
  vent_SN = 0_PM_REEL
  if (present(ventvert)) ventvert = 0_PM_REEL
  nn=size(vent%altitude)

  ! Modèle non initialisé, vent nul
  if (nn == 0) then
      call MSP_signaler_message(cle_mes="MSP_modvent_nul")
     return
  endif
  
  ! CALCUL DES 3 COMPOSANTES DE LA VITESSE DU VENT
  !-----------------------------------------------
  ! LE VENT DEPEND DE L'ALTITUDE SEULEMENT

  ! On ne veut pas d'extrapolation au dessus du modèle (vent supprosé nul)
  if (alt > vent%altitude(nn)) return

  ! si altitude négative (vent supprosé nul)
  if (alt < 0_PM_REEL) return

  ! si altitude comprise entre 0 et premier palier : on
  ! laisse mu_inter_dim1_lin extrapoler
 
  ! calcul par interpolation
  call mu_inter_dim1_lin(size(vent%altitude), vent%altitude, vent%Vent_EO, &
       alt, i_x, vent_EO, code1)
  call mu_inter_dim1_lin(size(vent%altitude), vent%altitude, vent%Vent_SN, &
       alt, i_x, vent_SN, code2)
  call MSP_signaler_message (ier_mslib=code1)
  call MSP_signaler_message (ier_mslib=code2)

  ! Modèle ARPEGE : cas direction / vitesse
  if (vent%modelevent .eq. msp_enum_modvent_arpege) then
     ! On a stocké la vitesse dans vent%vent_EO
     vit = vent_EO
     ! Et la direction dans vent%vent_SN
     dir = vent_SN
     ! Contrôle sur la direction : si saut de plus de 180deg alors il faut
     ! inverser le sens du vent
     if (abs(vent%Vent_SN(i_x+1) - vent%Vent_SN(i_x)) > PM_PI) then
        dir = vent_SN + PM_PI
     endif

     ! Passage dans le repère topo Nord
     ! direction : angle à partir de l'axe orienté Sud/Nord
     vent_SN = vit * cos(dir) * (-1)
     ! en partant vers l'Est
     vent_EO = vit * sin(dir)
  endif

  if (present(ventvert)) then
     call mu_inter_dim1_lin(size(vent%altitude), vent%altitude, &
          vent%Vent_vert, alt, i_x, ventvert, code3)
     call MSP_signaler_message (ier_mslib=code3)
  endif
  if (MSP_gen_messages("MSP_calculer_modvent")) return


end subroutine MSP_calculer_modvent
  

  subroutine ordre_croissant(x,x_out,ind)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ordre_croissant
!
!$Resume
!	Routine permettant de mettre des réels dans l'ordre croissant
!
!$Description
!	Routine permettant de mettre des réels dans l'ordre croissant
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call ordre_croissant(x,x_out,ind)
!.    real(kind=pm_reel), dimension(:), pointer :: x
!.    real(kind=pm_reel), dimension(:), pointer :: x_out
!.    integer, dimension(:), pointer :: ind
!
!$Arguments
!>E/S   x      :<pm_reel,DIM=(:),pointer>   Tableau en entrée
!>E/S   x_out  :<pm_reel,DIM=(:),pointer>   éléments de x par ordre croissant
!>E/S   ind    :<integer,DIM=(:),pointer>   indices initiaux correspondant à x_out
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none

      real(kind=pm_reel), dimension(:), pointer :: x
      real(kind=pm_reel), dimension(:), pointer :: x_out
      integer, dimension(:), pointer :: ind

      ! Variables locales
      integer :: ii, jj, kk, iostat
      real(kind=pm_reel), dimension(:), pointer :: x_tmp => NULL()
      integer, dimension(:), pointer :: ind_tmp => NULL()
      logical :: insere

      ! Initialisations
      allocate(x_out(size(x)))
      allocate(ind(size(x)))
      allocate(x_tmp(size(x_out)))
      allocate(ind_tmp(size(ind)))

      ! Calculs
      x_out(1) = x(1)
      ind(1) = 1
      do ii = 2, size(x)
         ! parcourt les elements
         x_tmp(:) = x_out(:)
         ind_tmp(:) = ind(:)
         insere=.false.
         do jj = 1, ii-1
            ! parcours les elements deja ordonnes
            if ((x(ii) < x_tmp(jj)) .and. .not. insere) then
               ! insersion du nouvel element
               insere = .true.
               x_out(jj) = x(ii)
               ind(jj) = ii
               do kk = jj+1, ii
                  x_out(kk) = x_tmp(kk-1)
                  ind(kk) = ind_tmp(kk-1)
               end do
            end if
         end do
         if (.not. insere) then
            x_out(ii) = x(ii)
            ind(ii) = ii
         end if
      end do

      deallocate(x_tmp,stat=iostat)
      deallocate(ind_tmp,stat=iostat)

    end subroutine ordre_croissant

  subroutine reordonner(x,ind)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  reordonner
!
!$Resume
!	Routine permettant de reordonner des éléments en fonction des
!       indices donnés
!
!$Description
!	Routine permettant de reordonner des éléments en fonction des
!       indices donnés
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call reordonner(x,ind)
!.    real(kind=pm_reel), dimension(:), pointer :: x
!.    integer, dimension(:), pointer :: ind
!
!$Arguments
!>E/S   x    :<pm_reel,DIM=(:),pointer>   éléments
!>E/S   ind  :<integer,DIM=(:),pointer>   indices 
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none

    real(kind=pm_reel), dimension(:), pointer :: x
    integer, dimension(:), pointer :: ind
    
    ! Variables locales
    integer :: ii, iostat
    real(kind=pm_reel), dimension(:), pointer :: x_tmp => NULL()
     
    ! Initialisations
    allocate(x_tmp(size(x)))

    ! Calculs
    x_tmp(:) = x(:)
    do ii = 1, size(ind)
       x(ind(ii)) = x_tmp(ii)
    end do
    deallocate(x_tmp,stat=iostat)

  end subroutine reordonner

end module msp_modvent_def
