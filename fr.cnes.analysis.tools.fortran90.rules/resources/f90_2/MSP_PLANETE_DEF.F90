module MSP_PLANETE_DEF


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_PLANETE_DEF
!
!$Resume
!	Module définissant une planète
!
!$Description
!	Module définissant une planète:
!-	Une planète est défini par:
!.	 le mouvement de son pole et du premier méridien
!.	 son rayon équatorial et son aplatissemnt
!
!$Auteur
!	S. ROUSSEAU
!
!$Version
!	$Id: MSP_PLANETE_DEF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!	$Log: MSP_PLANETE_DEF.F90,v $
!	Revision 1.14  2010/10/20 09:35:43  mercadig
!	VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!	Revision 1.13  2008/11/19 13:32:34  mercadig
!	DM-ID 733 : Mise a jour cartouche
!
!	Revision 1.12  2008/07/04 14:57:39  huec
!	DM-ID 1058 : Gestion memoire
!	Revision 1.11  2007/06/15 13:45:00  vivaresf
!	FA-ID 746 : mise en inout de la structure pour éviter un écrasement mémoire dans la fonction affecter
!	Revision 1.10  2005/11/03 17:58:01  vivaresf
!	DM-ID 125 : cartouches
!	Revision 1.9  2005/10/28 17:14:12  vivaresf
!	DM-ID 125 : Cloture du FT (Ajout d un accés rapide à la sous structure MSP_POLE de MSP_PLANETE)
!	Revision 1.8.2.1  2005/10/28 17:09:46  vivaresf
!	DM-ID 125 : fonction d'acces rapide par pointeur
!	Revision 1.8  2005/03/08 07:32:36  fabrec
!	DM-ID 111 : mise à jour des cartouches
!	Revision 1.7  2005/01/20 13:56:26  pauh
!	FA_332
!	Revision 1.6.2.1  2005/01/19 10:25:55  pauh
!	FA 332 : Appels de DEALLOCATE avec l'argument stat=MSP_iostat
!	Revision 1.6  2004/11/05 16:27:04  vivaresf
!	coquilles
!	Revision 1.5  2004/10/25 10:15:02  vivaresf
!	FA-ID 228 : sortie des routines
!	egaler (surcharges de l'operateur =) en inout pour pouvoir desallouer les pointeurs
!	et eviter les fuites memoires
!	Revision 1.4  2002/12/03 17:21:03  adm_ipsi
!	 Ajout de implicit none
!	Revision 1.3  2002/10/25 16:21:20  adm_ipsi
!	Les pointeurs requa et apla de la structure MSP_PLANETE sont remplacés par des réels
!	Revision 1.2  2002/10/01 14:13:45  adm_ipsi
!	Ajout du parametre nul=.true. dans l'appel à MSP_effacer_planete de MSP_creer_planete
!	Revision 1.1.1.1  2002/09/30 14:09:35  adm_ipsi
!	Industrialisation de la MECASPA sans les modules de gestion d'erreurs
!	Revision 1.5  2002/05/03 07:51:35  util_am
!	Modifications dues au passage avec le compilateur 6.2 (=> NULL() et argument nul dans effacer_)
!	Revision 1.4  2000/07/04 13:55:39  util_am
!	Ajout du texte des sections Resume et DEscription des cartouches de
!	  MSP_consulter_planete et MSP_modifier_planete
!	Revision 1.3  2000/06/14 16:17:44  util_am
!	- Ajout du champ flag_func dans la structure MSP_PLANETE pour la gestion des fuites mémoires
!	- Privatisation du contenu de la structure MSP_PLANETE
!	- Ajout des routines MSP_consulter_planete, MSP_modifier_planete
!	- Ajout des intefaces anglaises
!	- Mise à jour des cartouches
!	Revision 1.2  2000/02/10 16:17:54  rousseau
!	ajout de la routine MSP_affecter_planete
!	Revision 1.1.1.1  1999/07/13 08:37:58  util_am
!	Version 1.0 de MECASPA mise sous CVS
!
!$FinHistorique
!
!$Usage
!  use MSP_PLANETE_DEF
!
!$Structure
!
!: MSP_PLANETE : planète
!#V
!>     pole        : <MSP_pole,pointer,private>  
!>     requa       : <PM_REEL,private>           
!>     apla        : <PM_REEL,private>           
!>     flag_func   : <logical,private>           Booléen indiquant si la structure est créée par une fonction
!#
!
!$Global
!
!$Common
!
!$Routines
!- MSP_clear_planet
!- MSP_assign_planet
!- MSP_create_planet
!- MSP_get_planet_data
!- MSP_set_planet_data
!- MSP_effacer_planete
!- MSP_affecter_planete
!- MSP_consulter_planete
!- MSP_modifier_planete
!#V
!- egaler_planete
!#
!
!$Fonctions
!- MSP_creer_planete
!- MSP_pt_planete_pole
!- MSP_existe_pole
!
!$Include
!
!$Module
!#V
!- MSLIB
!- MSP_POLE_DEF
!#
!
!$Interface
!> msp_clear_planet :     MSP_effacer_planete
!> assignment :           egaler_planete
!> msp_create_planet :    MSP_creer_planete
!> msp_set_planet_data :  MSP_modifier_planete
!> msp_get_planet_data :  MSP_consulter_planete
!> msp_assign_planet :    MSP_affecter_planete
!#V
!#
!
!$Remarques
!
!$Mots-cles
! PLANETE
!
!$Voir-Aussi
!#V
!.  egaler_planete
!#
!.  MSP_creer_planete MSP_pt_planete_pole MSP_existe_pole MSP_clear_planet MSP_assign_planet
!.  MSP_create_planet MSP_get_planet_data MSP_set_planet_data MSP_effacer_planete MSP_affecter_planete
!.  MSP_consulter_planete MSP_modifier_planete
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
use MSLIB ,only:PM_REEL
use MSP_POLE_DEF

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_PLANETE_DEF.F90 69 2012-09-11 08:33:34Z ffsm $'


  type MSP_PLANETE
     private
     type(MSP_pole),pointer     :: pole  => NULL()
     real(kind=PM_REEL)         :: requa 
     real(kind=PM_REEL)         :: apla  
     logical :: flag_func
  end type MSP_PLANETE

  interface assignment (=)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  assignment
!
!$Resume
!	Routine permettant d'égaler 2 planètes
!
!$Description
!	Routine permettant d'égaler 2 planètes
!
!$Acces
!  PUBLIC
!
!$Usage
!  planetea=planeteb
!.    type(MSP_PLANETE) :: planetea
!.    type(MSP_PLANETE) :: planeteb
!
!$Procedures
!#V
!- egaler_planete
!#
!
!$Remarques
!
!$Mots-cles
! PLANETE EGALER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure egaler_planete
  end interface

  interface MSP_clear_planet

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_clear_planet
!
!$Resume
!  This routine clears the planet structure
!
!$Description
!  This routine clears the planet structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_clear_planet (planete, [nul])
!.    type(MSP_PLANETE) :: planete
!.    logical :: nul
!
!$Procedures
!- MSP_effacer_planete
!
!$Remarques
!
!$Mots-cles
! PLANETE EFFACER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_effacer_planete
  end interface
  
  interface MSP_assign_planet

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_assign_planet
!
!$Resume
!  This routine assign a planet structure to another one
!
!$Description
!  This routine assign a planet structure to another one
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_assign_planet(planetea,planeteb)
!.    type(MSP_PLANETE) :: planetea
!.    type(MSP_PLANETE) :: planeteb
!
!$Procedures
!- MSP_affecter_planete
!
!$Remarques
!
!$Mots-cles
! PLANETE AFFECTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_affecter_planete
  end interface
  
  interface MSP_create_planet

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_create_planet
!
!$Resume
!  This routine creates a planet structure
!
!$Description
!  This routine creates a planet structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  planete = MSP_create_planet([pole],[requa],[aplatissement])
!.    type(MSP_POLE) :: pole
!.    real(kind=PM_REEL) :: requa
!.    real(kind=PM_REEL) :: aplatissement
!.    type(MSP_PLANETE) :: planete
!
!$Procedures
!- MSP_creer_planete
!
!$Remarques
!
!$Mots-cles
! PLANETE CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_creer_planete
  end interface

  interface MSP_get_planet_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_planet_data
!
!$Resume
!  Get planet charactersitics
!
!$Description
!  Get planet charactersitics
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_get_planet_data(planete, [pole], [requa], [apla])
!.    type(MSP_PLANETE) :: planete
!.    type(MSP_POLE) :: pole
!.    real(kind=PM_REEL) :: requa
!.    real(kind=PM_REEL) :: apla
!
!$Procedures
!- MSP_consulter_planete
!
!$Remarques
!
!$Mots-cles
! PLANETE CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_consulter_planete
  end interface

  interface MSP_set_planet_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_set_planet_data
!
!$Resume
!  Modify planet charactersitics
!
!$Description
!  Modify planet charactersitics
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_set_planet_data(planete, [pole], [requa], [apla])
!.    type(MSP_PLANETE) :: planete
!.    type(MSP_POLE) :: pole
!.    real(kind=PM_REEL) :: requa
!.    real(kind=PM_REEL) :: apla
!
!$Procedures
!- MSP_modifier_planete
!
!$Remarques
!
!$Mots-cles
! PLANETE MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_modifier_planete
  end interface


  private :: egaler_planete

CONTAINS

  subroutine MSP_effacer_planete (planete, nul)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_effacer_planete
!
!$Resume
!	Routine permettant d'initialiser "proprement" une variable de type MSP_planete
!
!$Description
!	Routine permettant d'initialiser "proprement" une variable de type MSP_planete
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_effacer_planete (planete, [nul])
!.    type(MSP_PLANETE) :: planete
!.    logical :: nul
!
!$Arguments
!>E/S   planete  :<MSP_PLANETE>   planéte
!>[E]   nul      :<logical>       si nul=.true., on se contente des instructions NULLIFY (par défaut .false.)
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
! PLANETE EFFACER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_PLANETE) :: planete
    logical,intent(in), optional :: nul
       
    logical :: nul_tmp
    integer :: MSP_iostat
    MSP_iostat = 0

    if ( present (nul) ) then
       nul_tmp = nul
    else
       nul_tmp = .false.
    endif

    planete%requa=0._pm_reel
    planete%apla=0._pm_reel

    if ( nul_tmp ) then
      ! On se contente d'enlever les liens sans désallouer
      nullify(planete%pole)

    else
       if (associated(planete%pole)) then
          deallocate(planete%pole,stat=MSP_iostat)
       end if

    endif

  end subroutine MSP_effacer_planete



  subroutine egaler_planete(planetea,planeteb)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  egaler_planete
!
!$Resume
!	Routine permettant d'égaler 2 planètes
!
!$Description
!	Routine permettant d'égaler 2 planètes
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PRIVE
!
!$Usage
!  call egaler_planete(planetea,planeteb)
!.    type(MSP_PLANETE) :: planetea
!.    type(MSP_PLANETE) :: planeteb
!
!$Arguments
!>E/S   planetea  :<MSP_PLANETE>   planetea
!>E     planeteb  :<MSP_PLANETE>   planeteb
!
!$Common
!
!$Routines
!- MSP_effacer_planete
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! PLANETE EGALER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    implicit none

    type(MSP_PLANETE),intent(inout) :: planetea
    type(MSP_PLANETE),intent(in) :: planeteb

    call MSP_effacer_planete (planetea,nul=.true.)
    planetea%flag_func = .false.

    if(associated(planeteb%pole)) then
       allocate(planetea%pole)
       planetea%pole=planeteb%pole
    end if


    planetea%requa=planeteb%requa

    planetea%apla=planeteb%apla
   
    if (planeteb%flag_func) call MSP_effacer_planete(planeteb)

  end subroutine egaler_planete

  subroutine MSP_affecter_planete(planetea,planeteb)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_affecter_planete
!
!$Resume
!	Routine realisant les operations suivantes :
!	-planetea = planeteb
!	-supprimer planeteb
!
!$Description
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_affecter_planete(planetea,planeteb)
!.    type(MSP_PLANETE) :: planetea
!.    type(MSP_PLANETE) :: planeteb
!
!$Arguments
!>E/S   planetea  :<MSP_PLANETE>   planete à gauche du signe égal
!>E/S   planeteb  :<MSP_PLANETE>   planete à droite du signe égal
!
!$Common
!
!$Routines
!- MSP_effacer_planete
!#V
!- egaler_planete
!#
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! PLANETE AFFECTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    implicit none

    type(MSP_PLANETE),intent(inout) :: planetea
    type(MSP_PLANETE) :: planeteb

    call egaler_planete(planetea,planeteb)
    call MSP_effacer_planete(planeteb)
  end subroutine MSP_affecter_planete

  function MSP_creer_planete(pole,requa,aplatissement) result (planete)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_planete
!
!$Resume
!	Fonction permettant de créer une planète
!
!$Description
!	Fonction permettant de créer une planète
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PUBLIC
!
!$Usage
!  planete = MSP_creer_planete([pole],[requa],[aplatissement])
!.    type(MSP_POLE) :: pole
!.    real(kind=PM_REEL) :: requa
!.    real(kind=PM_REEL) :: aplatissement
!.    type(MSP_PLANETE) :: planete
!
!$Arguments
!>[E]   pole           :<MSP_POLE>      Description du mouvement du pole et du premier méridien de la planète
!>[E]   requa          :<PM_REEL>       Rayon équtorial de la planète
!>[E]   aplatissement  :<PM_REEL>       Aplatissement (ou inverse de l'aplatissement de la planète)
!>S     planete        :<MSP_PLANETE>   planète
!
!$Common
!
!$Routines
!- MSP_effacer_planete
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
! PLANETE CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_POLE),optional,intent(in) :: pole
    real(kind=PM_REEL),optional,intent(in) :: requa
    real(kind=PM_REEL),optional,intent(in) :: aplatissement
    type(MSP_PLANETE):: planete
    call MSP_effacer_planete(planete, nul=.true.)

    if(present(pole)) then
       allocate(planete%pole)
       planete%pole=pole
    end if

    if (present(requa)) then
       planete%requa=requa
    end if

    if ( present(aplatissement)) then
       planete%apla=aplatissement
    end if
    
    planete%flag_func = .true.

  end function MSP_creer_planete


  subroutine MSP_consulter_planete(planete, pole, requa, apla)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_planete
!
!$Resume
!  Consultation des caractéristiques de la planète
!
!$Description
!  Consultation des caractéristiques de la planète
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_planete(planete, [pole], [requa], [apla])
!.    type(MSP_PLANETE) :: planete
!.    type(MSP_POLE) :: pole
!.    real(kind=PM_REEL) :: requa
!.    real(kind=PM_REEL) :: apla
!
!$Arguments
!>E     planete  :<MSP_PLANETE>   structure planète à consulter
!>[S]   pole     :<MSP_POLE>      structure pole récupérée
!>[S]   requa    :<PM_REEL>       rayon équatorial récupéré
!>[S]   apla     :<PM_REEL>       aplatissement récupéré
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
! PLANETE CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_PLANETE),intent(in):: planete
    type(MSP_POLE),optional,intent(out) :: pole
    real(kind=PM_REEL),optional,intent(out) :: requa
    real(kind=PM_REEL),optional,intent(out) :: apla
    
    if (present(pole))  pole  = planete%pole
    if (present(requa)) requa = planete%requa
    if (present(apla))  apla  = planete%apla

  end subroutine MSP_consulter_planete

  subroutine MSP_modifier_planete(planete, pole, requa, apla)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_planete
!
!$Resume
!  Modification des caractéristiques de la planète
!
!$Description
!  Modification des caractéristiques de la planète
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_planete(planete, [pole], [requa], [apla])
!.    type(MSP_PLANETE) :: planete
!.    type(MSP_POLE) :: pole
!.    real(kind=PM_REEL) :: requa
!.    real(kind=PM_REEL) :: apla
!
!$Arguments
!>E/S   planete  :<MSP_PLANETE>   Structure planète à modifier
!>[E]   pole     :<MSP_POLE>      structure pole à substituer
!>[E]   requa    :<PM_REEL>       rayon équatorial à substituer
!>[E]   apla     :<PM_REEL>       aplatissement à substituer
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
! PLANETE MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    type(MSP_PLANETE),intent(inout):: planete
    type(MSP_POLE),optional,intent(in) :: pole
    real(kind=PM_REEL),optional,intent(in) :: requa
    real(kind=PM_REEL),optional,intent(in) :: apla
    
    integer :: MSP_iostat
    
    MSP_iostat = 0
    
    if (present(pole))  then 
       if (ASSOCIATED(planete%pole)) DEALLOCATE(planete%pole,stat=MSP_iostat)
       ALLOCATE(planete%pole)
       planete%pole=pole
    end if

    if (present(requa)) then 
       planete%requa = requa
    end if

    if (present(apla))  then
        planete%apla = apla
     end if

   end subroutine MSP_modifier_planete

   function MSP_pt_planete_pole(planete) result (pole)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_pt_planete_pole
!
!$Resume
!    Accès rapide à la structure MSP_POLE de MSP_PLANETE
!
!$Description
!    Accès rapide par pointeur à la structure MSP_POLE de MSP_PLANETE
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  pole = MSP_pt_planete_pole(planete)
!.    type(MSP_PLANETE) :: planete
!.    type(MSP_POLE),pointer :: pole
!
!$Arguments
!>E     planete  :<MSP_PLANETE>        Structure regroupant les information de planète
!>S     pole     :<MSP_POLE,pointer>   Pointeur sur le champ pole
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
!       Attention pas de vérification si le pointeur est affecté ou non
!       L'utilisateur devra tester si celui-ci est alloué avec MSP_existe_pole
!       avant de l'utiliser
!       Attention a utiliser l'affectation de pointeur "=>" et non "=" 
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     type(MSP_PLANETE),intent(in):: planete
     type(MSP_POLE),pointer::pole

     pole => planete%pole
   end function MSP_pt_planete_pole

     function MSP_existe_pole(planete) result (ok)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_existe_pole
!
!$Resume
!	Fonction de consultation permettant de savoir si le pole
!       est défini dans une variable de type MSP_PLANETE
!
!$Description
!	Fonction de consultation permettant de savoir si le pole
!       est défini dans une variable de type MSP_PLANETE
!
!$Auteur
!	F. Vivarès
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = MSP_existe_pole(planete)
!.    type(MSP_PLANETE) :: planete
!.    logical :: ok
!
!$Arguments
!>E     planete  :<MSP_PLANETE>   variable de type MSP_PLANETE
!>S     ok       :<logical>       booléen de retour
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
!  MODELES 3CORPS EXISTENCE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

     type(MSP_PLANETE),intent(in):: planete
      logical :: ok

      ok = (associated(planete%pole))


    end function MSP_existe_pole

end module MSP_PLANETE_DEF
