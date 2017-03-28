module MSP_PRE_SOLAIRE_DEF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_PRE_SOLAIRE_DEF
!
!$Resume
!  Module gérant les modèles de pression de radiation solaire.
!
!$Description
!  Module gérant les modèles de pression de radiation solaire.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: MSP_PRE_SOLAIRE_DEF.F90 365 2013-02-18 12:36:19Z aadt $
!
!$Historique
!  $Log: MSP_PRE_SOLAIRE_DEF.F90,v $
!  Revision 365  2013/02/18 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.18  2010/10/20 09:35:43  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.17  2009/03/12 16:44:43  mercadig
!  FA-ID 1178: Utilisation de MSP_EPSILON_PRS pour le module de pression de radiation solaire
!
!  Revision 1.16  2009/02/16 14:19:33  huec
!  AQ : Suppression de variables inutilisees
!
!  Revision 1.15  2009/02/16 14:04:16  huec
!  FA-ID 1181 : Decoupage de la routine msp_calculer_pre_solaire
!
!  Revision 1.14  2008/11/20 12:46:02  cml
!  AQ : Suppression de variables inutilisees
!
!  Revision 1.13  2008/11/19 13:37:20  mercadig
!  DM-ID 733 : Mise a jour cartouche
!
!  Revision 1.12  2008/11/04 08:03:35  huec
!  DM-ID 733 : Correction d\'une coquille dans la description d\'un argument
!  Revision 1.11  2008/11/03 17:10:36  huec
!  DM-ID 733 : Changement de module pour les routines de calcul de pression de radiation solaire
!  Revision 1.10  2008/11/03 16:53:50  huec
!  DM-ID 733 : Changement de module pour les routines de calcul de pression de radiation solaire
!  Revision 1.9  2008/11/03 16:22:07  huec
!  DM-ID 733 : Suppressions des routines obsoletes
!  Revision 1.8  2008/08/08 15:00:07  gss
!  DM-ID 1058 : (portage g95) ajout du champs répertoire des fichiers éphémérides
!  dans la structure pre_solaire. Modification en conséquence des fonctions
!  d'initialisation, modification, consultation, etc de la strurcture.
!  Initialisation à 0 de la variable surfapp.
!  Revision 1.7  2007/10/03 12:00:52  huec
!  DM-ID 733 : Extraction du calcul de la force de pression de radiation solaire
!  Revision 1.6  2007/06/18 10:14:26  tanguyy
!  FA-ID 749 : nouvelle constante MSP_LONG_NOMFIC pour les longueurs des noms de fichiers
!  Revision 1.5  2005/03/08 07:32:36  fabrec
!  DM-ID 111 : mise à jour des cartouches
!  Revision 1.4  2002/12/09 16:45:37  adm_ipsi
!  Utilisation du parametre MSP_ENUM_ECRAN pour les sorties ecran
!  Revision 1.3  2002/12/04 18:08:25  adm_ipsi
!  Utilisation du parametre NB_LONG_CHAINE
!  Revision 1.2  2002/12/03 17:21:04  adm_ipsi
!   Ajout de implicit none
!  Revision 1.1.1.1  2002/09/30 14:09:35  adm_ipsi
!  Industrialisation de la MECASPA sans les modules de gestion d'erreurs
!  Revision 1.3  2000/06/14 16:24:41  util_am
!  !  - Ajout du champ flag_func dans la structure MSP_PRE_SOLAIRE pour la gestion des fuites mémoires
!  !  - Privatisation du contenu de la structure MSP_PRE_SOLAIRE
!  !  - Ajout des routines MSP_consulter_pre_solaire, MSP_modifier_pre_solaire
!  !  - Ajout des interfaces anglaises
!  !  - Mise à jour des cartouches
!  !
!  Revision 1.2  1999/10/19 11:36:16  util_am
!  Ajouts permattant de considérer des fichiers éphémérides pour le soleil
!  Revision 1.1.1.1  1999/07/13 08:37:58  util_am
!  Version 1.0 de MECASPA mise sous CVS
!
!$FinHistorique
!
!$Usage
!  use MSP_PRE_SOLAIRE_DEF
!
!$Structure
!
!: MSP_PRE_SOLAIRE : 
!#V
!>     nom_pre_sol    : <LEN=MSP_LONG_CHAINE,private>  nom du modèle
!>     type_pre_sol   : <integer,private>              type du modèle (MSP_ENUM_NON ou MSP_ENUM_OUI)
!>     ficept         : <LEN=MSP_LONG_NOMFIC,private>  nom du fichier éphémérides
!>     dirept         : <LEN=MSP_LONG_NOMFIC,private>  
!>     consol         : <pm_reel,private>              constante de pression de radiation
!#
!
!$Global
!
!$Common
!
!$Routines
!- MSP_create_radiation_pressure
!- MSP_get_radiation_pressure_data
!- MSP_set_radiation_pressure_data
!- MSP_consulter_pre_solaire
!- MSP_modifier_pre_solaire
!- MSP_calculer_pre_solaire
!- MSP_flux_solaire
!
!$Fonctions
!- MSP_creer_pre_solaire
!- exp_limit
!
!$Include
!
!$Module
!#V
!- MSLIB
!- MSP_GESTION_ERREUR
!- MSP_MECASPA_DEF
!- MSP_MCI_DEF
!- MSP_BULLETIN_DEF
!- MSP_PRAD_DEF
!#
!
!$Interface
!> msp_create_radiation_pressure :    MSP_creer_pre_solaire
!> msp_get_radiation_pressure_data :  MSP_consulter_pre_solaire
!> msp_set_radiation_pressure_data :  MSP_modifier_pre_solaire
!#V
!#
!
!$Remarques
!
!$Mots-cles
! RADIATION
!
!$Voir-Aussi
!.  MSP_creer_pre_solaire exp_limit MSP_create_radiation_pressure MSP_get_radiation_pressure_data
!.  MSP_set_radiation_pressure_data MSP_consulter_pre_solaire MSP_modifier_pre_solaire MSP_calculer_pre_solaire
!.  MSP_flux_solaire
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use MSLIB, only : pm_reel
   use MSP_GESTION_ERREUR
   use MSP_MECASPA_DEF

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_PRE_SOLAIRE_DEF.F90 365 2013-02-18 12:36:19Z aadt $'


   type MSP_PRE_SOLAIRE

      private

      character(LEN=MSP_LONG_CHAINE)  :: nom_pre_sol

      integer :: type_pre_sol

      character(LEN=MSP_LONG_NOMFIC) :: ficept
      character(LEN=MSP_LONG_NOMFIC) :: dirept

      real (KIND=pm_reel) :: consol

   end type MSP_PRE_SOLAIRE


   INTERFACE MSP_create_radiation_pressure

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_create_radiation_pressure
!
!$Resume
!  Creation of radiation pressure model structure
!
!$Description
!  Creation of radiation pressure model structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  pre_solaire = MSP_create_radiation_pressure ([type],[ficept],[dirept],[consol],[nom_pre_sol])
!.    type(MSP_PRE_SOLAIRE) :: pre_solaire
!.    integer :: type
!.    real(kind=pm_reel) :: consol
!.    character(LEN=*) :: ficept,dirept,nom_pre_sol
!
!$Procedures
!- MSP_creer_pre_solaire
!
!$Remarques
!
!$Mots-cles
! RADIATION CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_creer_pre_solaire
   end INTERFACE

   INTERFACE MSP_get_radiation_pressure_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_radiation_pressure_data
!
!$Resume
!  Get characteristics of a radiation pressure model structure
!
!$Description
!  Get characteristics of a radiation pressure model structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_get_radiation_pressure_data(pre_solaire,[type],[ficept],[dirept],[consol],[nom_pre_sol])
!.    type(MSP_PRE_SOLAIRE) :: pre_solaire
!.    integer :: type
!.    real(kind=pm_reel) :: consol
!.    character(LEN=*) :: ficept,dirept,nom_pre_sol
!
!$Procedures
!- MSP_consulter_pre_solaire
!
!$Remarques
!
!$Mots-cles
! RADIATION CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_consulter_pre_solaire
   end INTERFACE

   INTERFACE MSP_set_radiation_pressure_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_set_radiation_pressure_data
!
!$Resume
!  Modify characteristics of a radiation pressure model structure
!
!$Description
!  Modify characteristics of a radiation pressure model structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_set_radiation_pressure_data(pre_solaire,[type],[ficept],[dirept],[consol],[nom_pre_sol])
!.    type(MSP_PRE_SOLAIRE) :: pre_solaire
!.    integer :: type
!.    real(kind=pm_reel) :: consol
!.    character(LEN=*) :: ficept,dirept,nom_pre_sol
!
!$Procedures
!- MSP_modifier_pre_solaire
!
!$Remarques
!
!$Mots-cles
! RADIATION MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_modifier_pre_solaire
   end INTERFACE

   contains

   function MSP_creer_pre_solaire (type,ficept,dirept,consol,nom_pre_sol) result (pre_solaire)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_pre_solaire
!
!$Resume
!  Fonction créant un modèle de pression de radiation solaire.
!
!$Description
!  Fonction créant un modèle de pression de radiation solaire.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  pre_solaire = MSP_creer_pre_solaire ([type],[ficept],[dirept],[consol],[nom_pre_sol])
!.    type(MSP_PRE_SOLAIRE) :: pre_solaire
!.    integer :: type
!.    real(kind=pm_reel) :: consol
!.    character(LEN=*) :: ficept,dirept,nom_pre_sol
!
!$Arguments
!>[E]   type         :<integer>           type du modèle (MSP_ENUM_NON ou MSP_ENUM_OUI) [par défaut MSP_ENUM_NON]
!>[E]   ficept       :<LEN=*>             nom du fichier éphémérides 
!>[E]   dirept       :<LEN=*>             répertoire où se trouve le fichier éphémérides 
!>[E]   consol       :<pm_reel>           constante de pression de radiation [par défaut 0.00000465]
!>[E]   nom_pre_sol  :<LEN=*>             nom du modèle
!>S     pre_solaire  :<MSP_PRE_SOLAIRE>   type contenant les données du modèle
!
!$Common
!
!$Routines
!- MSP_signaler_message
!
!$Include
!
!$Module
!#V
!- MSP_MECASPA_DEF
!#
!
!$Remarques
!
!$Mots-cles
! RADIATION CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use MSP_MECASPA_DEF

      implicit none

      type(MSP_PRE_SOLAIRE) :: pre_solaire

      integer, intent(IN), optional :: type
      real(kind=pm_reel), intent(IN), optional :: consol
      character(LEN=*), intent(IN), optional :: ficept,dirept,nom_pre_sol

      integer :: lnom

      character(LEN=80),dimension(2)       :: tmessage_var


      if ( present(nom_pre_sol) ) then
         lnom = LEN_TRIM(nom_pre_sol)
         if ( lnom <= MSP_LONG_CHAINE ) then 
            pre_solaire%nom_pre_sol  = nom_pre_sol
         else
            pre_solaire%nom_pre_sol  = nom_pre_sol(1:MSP_LONG_CHAINE)
            tmessage_var(1) = 'Le nom du modèle de pression de radiation solaire'
            write(tmessage_var(2),'(I8)')   MSP_LONG_CHAINE

            call MSP_signaler_message (cle_mes="MSP_LONGUEUR_CHAINE", &
               routine="MSP_creer_pre_solaire",type=MSP_ENUM_WARNING, &
               partie_variable=tmessage_var)
         endif
      else
         pre_solaire%nom_pre_sol = ""
      endif

      if ( present(type) ) then
         pre_solaire%type_pre_sol = type
      else
         pre_solaire%type_pre_sol = MSP_ENUM_NON
      endif

      if ( present(consol) ) then
         pre_solaire%consol = consol
      else
         pre_solaire%consol = 0.00000465_pm_reel
      endif

      if ( present(dirept) ) then
         lnom = LEN_TRIM(dirept)
         if ( lnom <= MSP_LONG_NOMFIC ) then 
            pre_solaire%dirept = dirept
         else
            pre_solaire%dirept = dirept(1:MSP_LONG_NOMFIC)
            tmessage_var(1) = 'Le nom du repertoire'
            write(tmessage_var(2),'(I8)')   MSP_LONG_NOMFIC

            call MSP_signaler_message (cle_mes="MSP_LONGUEUR_CHAINE", &
               routine="MSP_creer_pre_solaire",type=MSP_ENUM_WARNING, &
               partie_variable=tmessage_var)
         endif
      else
         pre_solaire%dirept = "."
      endif

      if ( present(ficept) ) then
         lnom = LEN_TRIM(ficept)
         if ( lnom <= MSP_LONG_NOMFIC ) then 
            pre_solaire%ficept  = ficept
         else
            pre_solaire%ficept  = ficept(1:MSP_LONG_NOMFIC)
            tmessage_var(1) = 'Le nom du fichier éphémérides'
            write(tmessage_var(2),'(I8)')   MSP_LONG_NOMFIC

            call MSP_signaler_message (cle_mes="MSP_LONGUEUR_CHAINE", &
               routine="MSP_creer_pre_solaire",type=MSP_ENUM_WARNING, &
               partie_variable=tmessage_var)
         endif
      else
         pre_solaire%ficept = ""
      endif

   end function MSP_creer_pre_solaire


   subroutine MSP_consulter_pre_solaire(pre_solaire,type,ficept,dirept,consol,nom_pre_sol)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_pre_solaire
!
!$Resume
!  Consultation du contenu de la structure prression de radiation solaire
!
!$Description
!  Consultation du contenu de la structure prression de radiation solaire
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_pre_solaire(pre_solaire,[type],[ficept],[dirept],[consol],[nom_pre_sol])
!.    type(MSP_PRE_SOLAIRE) :: pre_solaire
!.    integer :: type
!.    real(kind=pm_reel) :: consol
!.    character(LEN=*) :: ficept,dirept,nom_pre_sol
!
!$Arguments
!>E     pre_solaire  :<MSP_PRE_SOLAIRE>   structure pression de radiation à consulter
!>[S]   type         :<integer>           type du modèle (MSP_ENUM_NON ou MSP_ENUM_OUI)
!>[S]   ficept       :<LEN=*>             nom du fichier éphémérides 
!>[S]   dirept       :<LEN=*>             
!>[S]   consol       :<pm_reel>           constante de pression de radiation
!>[S]   nom_pre_sol  :<LEN=*>             nom du modèle
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
! RADIATION CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     implicit none

      type(MSP_PRE_SOLAIRE), intent(IN) :: pre_solaire

      integer, intent(OUT), optional :: type
      real(kind=pm_reel), intent(OUT), optional :: consol
      character(LEN=*), intent(OUT), optional :: ficept,dirept,nom_pre_sol

      if (present(nom_pre_sol)) nom_pre_sol = pre_solaire%nom_pre_sol
      if (present(type))        type = pre_solaire%type_pre_sol
      if (present(ficept))      ficept = pre_solaire%ficept
      if (present(dirept))      dirept = pre_solaire%dirept
      if (present(consol))      consol = pre_solaire%consol

    end subroutine MSP_consulter_pre_solaire

   subroutine MSP_modifier_pre_solaire(pre_solaire,type,ficept,dirept,consol,nom_pre_sol)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_pre_solaire
!
!$Resume
!  Modification du contenu d'une structure pression de radiation solaire
!
!$Description
!  Modification du contenu d'une structure pression de radiation solaire
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_pre_solaire(pre_solaire,[type],[ficept],[dirept],[consol],[nom_pre_sol])
!.    type(MSP_PRE_SOLAIRE) :: pre_solaire
!.    integer :: type
!.    real(kind=pm_reel) :: consol
!.    character(LEN=*) :: ficept,dirept,nom_pre_sol
!
!$Arguments
!>E/S   pre_solaire  :<MSP_PRE_SOLAIRE>   structure pression de radiation à modifier
!>[E]   type         :<integer>           type du modèle (MSP_ENUM_NON ou MSP_ENUM_OUI)
!>[E]   ficept       :<LEN=*>             nom du fichier éphémérides 
!>[E]   dirept       :<LEN=*>             
!>[E]   consol       :<pm_reel>           constante de pression de radiation
!>[E]   nom_pre_sol  :<LEN=*>             nom du modèle
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
! RADIATION MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     implicit none

      type(MSP_PRE_SOLAIRE), intent(INOUT) :: pre_solaire

      integer, intent(IN), optional :: type
      real(kind=pm_reel), intent(IN), optional :: consol
      character(LEN=*), intent(IN), optional :: ficept,dirept,nom_pre_sol

      if (present(nom_pre_sol)) pre_solaire%nom_pre_sol = nom_pre_sol
      if (present(type))        pre_solaire%type_pre_sol = type
      if (present(ficept))      pre_solaire%ficept = ficept
      if (present(dirept))      pre_solaire%dirept = dirept
      if (present(consol))      pre_solaire%consol = consol

    end subroutine MSP_modifier_pre_solaire

    subroutine MSP_calculer_pre_solaire(prad, accel, mci, dirsol, mat_att_veh, &
         acc_prad, modatt, panneaux, mat_att_panneaux)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_pre_solaire
!
!$Resume
!  Calcul de l'accélération due à la pression solaire
!
!$Description
!  Calcul de l'accélération due à la pression solaire
!
!$Auteur
!  Camille Hue (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calculer_pre_solaire(prad, accel, mci, dirsol, mat_att_veh, &
!.             acc_prad, [modatt], [panneaux], [mat_att_panneaux])
!.    type (MSP_PRAD) :: prad
!.    real (kind=pm_reel) :: accel
!.    type (MSP_MCI) :: mci
!.    real(kind=pm_reel), dimension(3) :: dirsol
!.    real(kind=pm_reel),dimension(3,3) :: mat_att_veh
!.    real(kind=pm_reel), dimension(3) :: acc_prad
!.    integer :: modatt
!.    integer :: panneaux
!.    real(kind=pm_reel), dimension(3,3) :: mat_att_panneaux
!
!$Arguments
!>E     prad              :<MSP_PRAD>            Structure MSP_PRAD contenant les coefficients ka, ks et kd
!>E/S   accel             :<pm_reel>             Accélération de radiation (m/s^2)
!>E     mci               :<MSP_MCI>             Strucure MECASPA contenant forme et surfaces du véhicule
!>E     dirsol            :<pm_reel,DIM=(3)>     Vecteur direction du soleil (de norme 1)
!>E     mat_att_veh       :<pm_reel,DIM=(3,3)>   Matrice de passage du repere d'expression de la direction 
!                                         du Soleil au repère véhicule
!>S     acc_prad          :<pm_reel,DIM=(3)>     Vecteur accélération (m/s^2)
!>[E]   modatt            :<integer>             Mode d'attitude (par défaut 0) :
!                             0 => on ne considère pas de loi d'attitude
!                             1 => on considère des lois d'attitude
!>[E]   panneaux          :<integer>             Prise en compte de panneaux solaires (par défaut 0)
!                             0 => on ne prend pas en compte de panneaux solaires
!                             1 => on prend en compte les panneaux solaires
!>[E]   mat_att_panneaux :<pm_reel(3,3)   Matrice d'attitude des panneaux solaires
!>[E]   mat_att_panneaux  :<pm_reel,DIM=(3,3)>   
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- mu_norme
!- MSP_consulter_mci
!- MSP_consulter_prad
!- mu_prod_vect
!
!$Include
!
!$Module
!#V
!- MSP_MCI_DEF
!- MSP_BULLETIN_DEF
!- MSP_PRAD_DEF
!#
!
!$Remarques
!
!$Mots-cles
!  VEHICULE CALCULER PRESSION RADIATION SOLAIRE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use MSP_MCI_DEF
      use MSP_BULLETIN_DEF, only: MSP_EPSILON_PRS
      use MSP_PRAD_DEF

      implicit none

      ! Decalaration des arguments
      type (MSP_PRAD), intent(IN)                              :: prad
      real (kind=pm_reel)                                      :: accel
      type (MSP_MCI), intent(IN)                               :: mci
      real(kind=pm_reel), intent(IN), dimension(3)             :: dirsol
      real(kind=pm_reel), intent(IN),dimension(3,3)            :: mat_att_veh
      real(kind=pm_reel), intent(OUT), dimension(3)            :: acc_prad
      integer, intent(IN), optional                            :: modatt
      integer, intent(IN), optional                            :: panneaux
      real(kind=pm_reel), intent(IN), dimension(3,3), optional :: mat_att_panneaux

      ! Autres declarations
      integer                          :: modatt_util, panneaux_util
      real(kind=pm_reel)               :: norme_dirsol
      real(kind=pm_reel)               :: coeff_ka, coeff_ks, coeff_kd
      real(kind=pm_reel)               :: surfapp, cosx, cosy, cosz, cost
      real(kind=pm_reel)               :: surfapp_panneaux, cosx_panneaux, cosy_panneaux, cosz_panneaux, cost_panneaux
      type (tm_code_retour)            :: code_retour
      integer                          :: forme, ii
      real(kind=pm_reel)               :: sx, sy, sz, st
      real(kind=pm_reel)               :: spx, spy, spz
      real(kind=pm_reel), dimension(3) :: normale_t
      real(kind=pm_reel), dimension(3) :: acc_ka, acc_ks, acc_kd
      real(kind=pm_reel), dimension(3) :: acc_ka_panneaux, acc_ks_panneaux, acc_kd_panneaux

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Controle de la cohérence des arguments et intialisations !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      if (present(modatt)) then
         modatt_util = modatt
         if (.not.((modatt_util == 1).or.(modatt_util == 0))) then
            ! Erreur, code non accepté
            call MSP_signaler_message (cle_mes="MSP_ERREUR_ARGUMENTS", &
                 partie_variable='Code inconnu pour modatt', &
                 routine="MSP_calculer_pre_solaire",type=MSP_ENUM_ERREUR)
            return
         end if
      else
         ! Par défaut, pas de loi d'attitude
         modatt_util = 0
      end if

      if (present(panneaux)) then
         panneaux_util = panneaux
         if (panneaux_util == 1) then
            ! On prend en compte les panneaux solaires
            ! On a besoin de la matrice
            if (.not.present(mat_att_panneaux)) then
               call MSP_signaler_message (cle_mes="MSP_ERREUR_ARGUMENTS",&
                    partie_variable='Panneaux solaires demandés, mais manque argument', &
                    routine="MSP_calculer_pre_solaire",type=MSP_ENUM_ERREUR)
               return
            end if
         elseif (panneaux_util == 0) then
            ! On ne prend pas en compte les panneaux solaires
            if (present(mat_att_panneaux)) then
               call MSP_signaler_message (cle_mes="MSP_WARN_ARGUMENTS", &
                    partie_variable='Argument de panneaux solaires fourni mais ignoré', &
                    routine="MSP_calculer_pre_solaire",type=MSP_ENUM_WARNING)
            end if
         else
            ! Erreur, code non accepté
            call MSP_signaler_message (cle_mes="MSP_ERREUR_ARGUMENTS", &
                 partie_variable='Code inconnu pour panneaux', &
                 routine="MSP_calculer_pre_solaire",type=MSP_ENUM_ERREUR)
            return
         end if
      else
         ! Par défaut , pas de panneaux solaires
         panneaux_util = 0
         if (present(mat_att_panneaux)) then
            call MSP_signaler_message (cle_mes="MSP_WARN_ARGUMENTS", &
                 partie_variable='Argument de panneaux solaires fourni mais ignoré', &
                 routine="MSP_calculer_pre_solaire",type=MSP_ENUM_WARNING)
         end if
      end if

      ! Controle que le vecteur dirsol est bien normé
      call mu_norme(dirsol, norme_dirsol, code_retour)
      if (code_retour%valeur < 0) then
         call MSP_signaler_message (ier_mslib=code_retour)
         if (MSP_gen_messages("MSP_calculer_pre_solaire" )) return
      end if
      if (abs(norme_dirsol - 1._pm_reel) > MSP_EPSILON_PRS) then
         ! Erreur, code non accepté
         call MSP_signaler_message (cle_mes="MSP_ERREUR_ARGUMENTS",partie_variable='Le vecteur dirsol n est pas norme', &
              routine="MSP_calculer_pre_solaire",type=MSP_ENUM_ERREUR)
         return
      end if

      !! Initialisations des paramètres véhicule
      call MSP_consulter_mci(mci, forme=forme, sx=sx, sy=sy, sz=sz, st=st,spx=spx,spy=spy,spz=spz)

      !!!!!!!!!!
      ! CALCUL !
      !!!!!!!!!!

      ! Calcul des termes dus au véhicule
      call MSP_presol_surfveh(prad, accel, modatt_util, forme, sx, sy, sz, st, mat_att_veh, &
           dirsol, coeff_ka, coeff_ks, coeff_kd, cosx, cosy, cosz, cost, surfapp, normale_t)

      ! Calcul des surfaces des panneaux solaires 
      if (panneaux_util == 1) then
         call MSP_presol_surfpan(modatt_util, spx, spy, spz, mat_att_panneaux, &
              dirsol, cosx_panneaux, cosy_panneaux, cosz_panneaux, cost_panneaux, surfapp_panneaux)
      end if

      ! Calcul des accélérations du véhicule
      call MSP_presol_accveh(coeff_ka, coeff_ks, coeff_kd, surfapp, dirsol, cosx, cosy, cosz, cost, &
         sx, sy, sz, st, mat_att_veh, normale_t, acc_ka, acc_ks, acc_kd)
      ! Calcul des accélérations des panneaux solaires
      if (panneaux_util == 1) then
         call MSP_presol_accpan(coeff_ka, coeff_ks, coeff_kd, surfapp_panneaux, dirsol, &
         cosx_panneaux, cosy_panneaux, cosz_panneaux, mat_att_panneaux, &
         cosx, cosy, cosz, spx, spy, spz, acc_ka_panneaux, acc_ks_panneaux, acc_kd_panneaux)
      end if
      
      ! Somme de toutes les accélérations pour l'accélération finale
      do ii = 1,3
         acc_prad(ii) = acc_ka(ii) + acc_ks(ii) + acc_kd(ii)
      end do
      if (panneaux_util == 1) then
         do ii = 1,3
            acc_prad(ii) = acc_prad(ii) + acc_ka_panneaux(ii) + acc_ks_panneaux(ii) + &
                 acc_kd_panneaux(ii)
         end do
      end if

    end subroutine MSP_calculer_pre_solaire

    subroutine MSP_flux_solaire(pos_veh, dirsol, cp, xm, Requa, pkreg, &
         gkreg, consol, accel)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_flux_solaire
!
!$Resume
!  Calcul du flux d'accélération de radiation solaire
!
!$Description
!  Calcul du flux d'accélération de radiation solaire (paramètre demandé par MSP_calculer_pre_solaire
!
!$Auteur
!  Camille Hue (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_flux_solaire(pos_veh, dirsol, cp, xm, Requa, pkreg, &
!.             gkreg, consol, accel)
!.    real(kind=pm_reel), dimension(3) :: pos_veh
!.    real(kind=pm_reel), dimension(3) :: dirsol
!.    real(kind=pm_reel) :: cp
!.    real(kind=pm_reel) :: xm
!.    real(kind=pm_reel) :: Requa
!.    real(kind=pm_reel) :: pkreg
!.    real(kind=pm_reel) :: gkreg
!.    real(kind=pm_reel) :: consol
!.    real(kind=pm_reel) :: accel
!
!$Arguments
!>E     pos_veh  :<pm_reel,DIM=(3)>   Position du véhicule dans le repère de travail (m)
!>E     dirsol   :<pm_reel,DIM=(3)>   Vecteur direction du Soleil (normé !)
!>E     cp       :<pm_reel>           Coefficient multiplicatif Cp
!>E     xm       :<pm_reel>           Masse du véhicule (kg)
!>E     Requa    :<pm_reel>           Rayon équatoriale de la planète de travail (m)
!>E     pkreg    :<pm_reel>           Paramètre pkreg du modèle (historiquement vaut 90.0 dans PSIMU)
!>E     gkreg    :<pm_reel>           Paramètre gkreg du modèle (historiquement vaut 20.0 dans PSIMU)
!>E     consol   :<pm_reel>           Paramètre consol du modèle (historiquement vaut 0.465e-5 
!                                dans PSIMU mais c'est une valeur spécifique à la Terre)
!>S     accel    :<pm_reel>           Accélération de radiation solaire
!
!$Common
!
!$Routines
!- MSP_signaler_message
!
!$Include
!
!$Module
!#V
!- MSP_BULLETIN_DEF
!#
!
!$Remarques
!
!$Mots-cles
!  CALCULER ACCELERATION RADIATION SOLAIRE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use MSP_BULLETIN_DEF, only: MSP_EPSILON_PRS
      implicit none

      ! Déclaration des arguments
      real(kind=pm_reel), intent(IN), dimension(3) :: pos_veh
      real(kind=pm_reel), intent(IN), dimension(3) :: dirsol
      real(kind=pm_reel), intent(IN)               :: cp
      real(kind=pm_reel), intent(IN)               :: xm
      real(kind=pm_reel), intent(IN)               :: Requa
      real(kind=pm_reel), intent(IN)               :: pkreg
      real(kind=pm_reel), intent(IN)               :: gkreg
      real(kind=pm_reel), intent(IN)               :: consol
      real(kind=pm_reel), intent(OUT)              :: accel

      ! Autres déclarations
      real(kind=pm_reel)   :: ts2, emregu, norme_dirsol, d2, xregu
      real(kind=pm_reel)   :: pres, fonreg

      ! Contrôle de la cohérence des arguments
      ! Tous les arguments sont obligatoires
      ! dirsol doit être normé
      norme_dirsol = sqrt(dirsol(1)*dirsol(1) + dirsol(2)*dirsol(2) + &
           dirsol(3)*dirsol(3))
      if (abs(norme_dirsol - 1._pm_reel) > MSP_EPSILON_PRS) then
         ! Erreur, code non accepté
         call MSP_signaler_message (cle_mes="MSP_ERREUR_ARGUMENTS",partie_variable='Le vecteur dirsol n est pas norme', &
              routine="MSP_flux_solaire",type=MSP_ENUM_ERREUR)
         return
      end if

      !!!!!!!!!!
      ! Calcul !
      !!!!!!!!!!
      ts2=pos_veh(1)*pos_veh(1)+pos_veh(2)*pos_veh(2)+pos_veh(3)*pos_veh(3)
      emregu=-(pos_veh(1)*dirsol(1)+pos_veh(2)*dirsol(2)+pos_veh(3)*dirsol(3))/Requa
      d2=ts2/(Requa*Requa)-(emregu*emregu)
      ! Utilisation de la fonction exp_limit qui calcule une exponentielle limitée entre -600 et 600
      xregu=pkreg*(d2-1._pm_reel)+exp_limit(-gkreg*emregu)
      fonreg=1._pm_reel/(1._pm_reel+exp_limit(-xregu))

      ! Pression de radiation solaire
      pres = consol*fonreg*cp

      ! Calcul de l'accélération de radiation solaire
      ! Controle que masse > 0
      if (xm > 0._pm_reel ) then
         accel = pres / xm
      else
         call MSP_signaler_message (cle_mes="MSP_MASSE_PRAD",routine="MSP_flux_solaire")
         accel = 0._pm_reel
         return
      endif

    end subroutine MSP_flux_solaire

    subroutine MSP_presol_surfveh(prad, accel, modatt, forme, sx, sy, sz, st, mat_att_veh, &
         dirsol, coeff_ka, coeff_ks, coeff_kd, cosx, cosy, cosz, cost, surfapp, normale_t)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_pre_solaire_surfveh
!
!$Resume
!  Calcul de la surface du au véhicule
!
!$Description
!  Calcul de la surface du au véhicule
!
!$Auteur
!  Camille Hue (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calculer_presol_surfveh()
!.    type (MSP_PRAD)                :: prad
!.    real (PM_REEL)                 :: accel
!.    integer                        :: modatt
!.    integer                        :: forme
!.    real (PM_REEL)                 :: sx
!.    real (PM_REEL)                 :: sy
!.    real (PM_REEL)                 :: sz
!.    real (PM_REEL)                 :: st
!.    real (PM_REEL), dimension(3,3) :: mat_att_veh
!.    real (PM_REEL), dimension(3)   :: dirsol
!.    real (PM_REEL)                 :: coeff_ka
!.    real (PM_REEL)                 :: coeff_ks
!.    real (PM_REEL)                 :: coeff_kd
!.    real (PM_REEL)                 :: cosx
!.    real (PM_REEL)                 :: cosy
!.    real (PM_REEL)                 :: cosz
!.    real (PM_REEL)                 :: cost
!.    real (PM_REEL)                 :: surfapp
!.    real (PM_REEL), dimension(3)   :: normale_t
!
!$Arguments
!>E     prad              :<MSP_PRAD>            Structure MSP_PRAD contenant les coefficients ka, ks et kd
!>E     accel             :<PM_REEL>             Accélération de radiation (m/s^2)
!>E     modatt            :<INTEGER>             Mode d'attitude (par défaut 0) :
!                             0 => on ne considère pas de loi d'attitude
!                             1 => on considère des lois d'attitude
!>E     forme             :<INTEGER>             Code de la forme du véhicule
!>E     sx                :<PM_REEL>             Surface du véhicule (selon x)
!>E     sy                :<PM_REEL>             Surface du véhicule (selon y)
!>E     sz                :<PM_REEL>             Surface du véhicule (selon z)
!>E     st                :<PM_REEL>             Surface du véhicule (transverse)
!>E     mat_att_veh       :<PM_REEL, DIM=(3,3)>  Matrice d'attitude du véhicule
!>E     dirsol            :<pm_reel, DIM=(3)>    Vecteur direction du soleil (de norme 1)
!>S     coeff_ka          :<PM_REEL>             Coefficient ka
!>S     coeff_ks          :<PM_REEL>             Coefficient ks
!>S     coeff_kd          :<PM_REEL>             Coefficient kd
!>S     cosx              :<PM_REEL>             Angle de la force sur le véhicule (axe x)
!>S     cosy              :<PM_REEL>             Angle de la force sur le véhicule (axe y)
!>S     cosz              :<PM_REEL>             Angle de la force sur le véhicule (axe z)
!>S     cost              :<PM_REEL>             Angle de la force sur le véhicule (transverse)
!>S     surfapp           :<PM_REEL>             Surface d'application de la force
!>S     normale_t         :<PM_REEL, DIM=(3)>    Normale à la surface
!
!$Common
!
!$Routines
!- MSP_consulter_prad
!
!$Include
!
!$Module
!#V
!- 
!#
!
!$Remarques
!
!$Mots-cles
!  VEHICULE CALCULER PRESSION RADIATION SOLAIRE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use MSP_PRAD_DEF
      use MSP_MCI_DEF
      use MSP_BULLETIN_DEF, only: MSP_EPSILON_PRS

      ! ARGUMENTS
      type(MSP_prad), intent(IN)                       :: prad
      real (kind=pm_reel), intent(IN)                  :: accel
      integer, intent(IN)                              :: modatt
      integer, intent(IN)                              :: forme
      real (kind=pm_reel), intent(IN)                  :: sx, sy, sz, st
      real (kind=pm_reel), dimension(3,3), intent(IN)  :: mat_att_veh
      real (kind=pm_reel), dimension(3), intent(IN)    :: dirsol
      real (kind=pm_reel), intent(OUT)                 :: coeff_ka
      real (kind=pm_reel), intent(OUT)                 :: coeff_ks
      real (kind=pm_reel), intent(OUT)                 :: coeff_kd
      real (kind=pm_reel), intent(OUT)                 :: cosx, cosy, cosz, cost
      real (kind=pm_reel), intent(OUT)                 :: surfapp
      real (kind=pm_reel), dimension(3), intent(OUT)   :: normale_t
      
      
      ! VARIABLES LOCALES
      real(kind=pm_reel)               :: ka, ks, kd, sin2, sinx
      real(kind=pm_reel), dimension(3) :: vtmp
      integer                          :: ii
      type (tm_code_retour)            :: code_retour

      !! Initialisation de normale_t
      do ii=1,3
         normale_t(ii) = 0._pm_reel
      end do

      ! Calcul des coefficients
      call MSP_consulter_prad(prad, ka=ka, ks=ks, kd=kd)
      coeff_ka = ka * accel
      coeff_ks = ks * accel
      coeff_kd = kd * accel
      ! Cas sans loi d'attitude
      if (modatt == 0) then
         select case (forme)

         case (MSP_ENUM_SPHERE)
            ! Paramètres
            cosx = 0._pm_reel
            cosy = 0._pm_reel
            cosz = 0._pm_reel
            cost = 1._pm_reel
            surfapp = st
            ! Calcul du vecteur normal à la surface transverse
            call mu_prod_vect (mat_att_veh(1,:),dirsol,vtmp,code_retour)
            if (code_retour%valeur < 0) then
               call MSP_signaler_message (ier_mslib=code_retour)
               if (MSP_gen_messages("MSP_calculer_pre_solaire" )) return
            end if
            call mu_prod_vect (vtmp,mat_att_veh(1,:),normale_t,code_retour)
            if (code_retour%valeur < 0) then
               call MSP_signaler_message (ier_mslib=code_retour)
               if (MSP_gen_messages("MSP_calculer_pre_solaire" )) return
            end if
            
         case default
            ! Paramètres
            surfapp = sx
            cosx = 1._pm_reel
            cosy = 0._pm_reel
            cosz = 0._pm_reel
            cost = 0._pm_reel
         end select !forme
      else
         select case (forme)

         case (MSP_ENUM_SPHERE)
            ! Paramètres
            cosx = 0._pm_reel
            cosy = 0._pm_reel
            cosz = 0._pm_reel
            cost = 1._pm_reel
            surfapp = st
            ! Calcul du vecteur normal à la surface transverse
            call mu_prod_vect (mat_att_veh(1,:),dirsol,vtmp,code_retour)
            if (code_retour%valeur < 0) then
               call MSP_signaler_message (ier_mslib=code_retour)
               if (MSP_gen_messages("MSP_calculer_pre_solaire" )) return
            end if
            call mu_prod_vect (vtmp,mat_att_veh(1,:),normale_t,code_retour)
            if (code_retour%valeur < 0) then
               call MSP_signaler_message (ier_mslib=code_retour)
               if (MSP_gen_messages("MSP_calculer_pre_solaire" )) return
            end if

         case (MSP_ENUM_PLAQUE)
            ! Paramètres
            cosx = (dirsol(1)*mat_att_veh(1,1)+dirsol(2)*mat_att_veh(1,2)+dirsol(3)*mat_att_veh(1,3))
            cosy = 0._pm_reel
            cosz = 0._pm_reel
            cost = 0._pm_reel
            surfapp = sx * abs(cosx)

         case (MSP_ENUM_CYLINDRE)
            ! Paramètres
            cosx = (dirsol(1)*mat_att_veh(1,1)+dirsol(2)*mat_att_veh(1,2)+dirsol(3)*mat_att_veh(1,3))
            cosy = (dirsol(1)*mat_att_veh(2,1)+dirsol(2)*mat_att_veh(2,2)+dirsol(3)*mat_att_veh(2,3))
            cosz = (dirsol(1)*mat_att_veh(3,1)+dirsol(2)*mat_att_veh(3,2)+dirsol(3)*mat_att_veh(3,3))
            sin2 = 1._pm_reel - cosx**2
            ! Calcul de sinx
            if ( sin2 < MSP_EPSILON_PRS ) sin2 = 0._pm_reel
            if ( (sin2 - 1._pm_reel) > MSP_EPSILON_PRS ) sin2 = 1._pm_reel
            sinx = sqrt ( sin2 )
            if ( cosx >= 0._pm_reel ) then
               cost = sinx
            else
               cost = -sinx
            endif
            surfapp = sx*abs(cosx) + sy*abs(cosy) + sz*abs(cosz) + st*sinx
            ! Calcul du vecteur normal à la surface transverse
            call mu_prod_vect (mat_att_veh(1,:),dirsol,vtmp,code_retour)
            if (code_retour%valeur < 0) then
               call MSP_signaler_message (ier_mslib=code_retour)
               if (MSP_gen_messages("MSP_calculer_pre_solaire" )) return
            end if
            call mu_prod_vect (vtmp,mat_att_veh(1,:),normale_t,code_retour)
            if (code_retour%valeur < 0) then
               call MSP_signaler_message (ier_mslib=code_retour)
               if (MSP_gen_messages("MSP_calculer_pre_solaire" )) return
            end if

         case (MSP_ENUM_PARALLEPIPEDE)
            ! Paramètres
            cosx = (dirsol(1)*mat_att_veh(1,1)+dirsol(2)*mat_att_veh(1,2)+dirsol(3)*mat_att_veh(1,3))
            cosy = (dirsol(1)*mat_att_veh(2,1)+dirsol(2)*mat_att_veh(2,2)+dirsol(3)*mat_att_veh(2,3))
            cosz = (dirsol(1)*mat_att_veh(3,1)+dirsol(2)*mat_att_veh(3,2)+dirsol(3)*mat_att_veh(3,3))
            cost = 0._pm_reel
            surfapp = sx*abs(cosx) + sy*abs(cosy) + sz*abs(cosz)

         case default
            ! Forme inconnue
            call MSP_signaler_message (cle_mes="MSP_ERREUR_FORME", &
                 routine="MSP_calculer_pre_solaire",type=MSP_ENUM_ERREUR)
            return

         end select !forme
      end if !modatt_util

    end subroutine MSP_presol_surfveh

    subroutine MSP_presol_surfpan(modatt, spx, spy, spz, mat_att_panneaux, &
         dirsol, cosx_panneaux, cosy_panneaux, cosz_panneaux, cost_panneaux, surfapp_panneaux)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_pre_solaire_surfpan
!
!$Resume
!  Calcul de la surface des panneaux solaires
!
!$Description
!  Calcul de la surface des panneaux solaires
!
!$Auteur
!  Camille Hue (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calculer_presol_surfpan()
!.    integer                        :: modatt
!.    real (PM_REEL)                 :: spx
!.    real (PM_REEL)                 :: spy
!.    real (PM_REEL)                 :: spz
!.    real (PM_REEL), dimension(3,3) :: mat_att_panneaux
!.    real (PM_REEL), dimension(3)   :: dirsol
!.    real (PM_REEL)                 :: cosx_panneaux
!.    real (PM_REEL)                 :: cosy_panneaux
!.    real (PM_REEL)                 :: cosz_panneaux
!.    real (PM_REEL)                 :: cost_panneaux
!.    real (PM_REEL)                 :: surfapp_panneaux
!
!$Arguments
!>E     modatt            :<INTEGER>             Mode d'attitude (par défaut 0) :
!                             0 => on ne considère pas de loi d'attitude
!                             1 => on considère des lois d'attitude
!>E     spx               :<PM_REEL>             Surface du panneau (selon x)
!>E     spy               :<PM_REEL>             Surface du panneau (selon y)
!>E     spz               :<PM_REEL>             Surface du panneau (selon z)
!>E     mat_att_panneaux  :<PM_REEL, DIM=(3,3)>  Matrice d'attitude des panneaux solaires
!>E     dirsol            :<pm_reel, DIM=(3)>    Vecteur direction du soleil (de norme 1)
!>S     cosx_panneaux     :<PM_REEL>             Angle de la force sur les panneaux (axe x)
!>S     cosy_panneaux     :<PM_REEL>             Angle de la force sur les panneaux (axe y)
!>S     cosz_panneaux     :<PM_REEL>             Angle de la force sur les panneaux (axe z)
!>S     cost_panneaux     :<PM_REEL>             Angle de la force sur les panneaux (transverse)
!>S     surfapp_panneaux  :<PM_REEL>             Surface d'application de la force
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- 
!#
!
!$Remarques
!
!$Mots-cles
!  VEHICULE CALCULER PRESSION RADIATION SOLAIRE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use MSP_MCI_DEF
      use MSP_BULLETIN_DEF, only: MSP_EPSILON_PRS

      ! ARGUMENTS
      integer, intent(IN)                              :: modatt
      real (kind=pm_reel), intent(IN)                  :: spx, spy, spz
      real (kind=pm_reel), dimension(3,3), intent(IN)  :: mat_att_panneaux
      real (kind=pm_reel), dimension(3), intent(IN)    :: dirsol
      real (kind=pm_reel), intent(OUT)                 :: cosx_panneaux, cosy_panneaux
      real (kind=pm_reel), intent(OUT)                 :: cosz_panneaux, cost_panneaux
      real (kind=pm_reel), intent(OUT)                 :: surfapp_panneaux

      ! Cas sans loi d'attitude
      if (modatt == 0) then
         surfapp_panneaux = spx
         cosx_panneaux = 1._pm_reel
         cosy_panneaux = 0._pm_reel
         cosz_panneaux = 0._pm_reel
         cost_panneaux = 0._pm_reel
      else
         ! Cas avec loi d'attitude
         ! On assimile les panneaux solaires à un parallélépipède
         cosx_panneaux = (dirsol(1)*mat_att_panneaux(1,1)+dirsol(2)*mat_att_panneaux(1,2)+ &
              dirsol(3)*mat_att_panneaux(1,3))
         cosy_panneaux = (dirsol(1)*mat_att_panneaux(2,1)+dirsol(2)*mat_att_panneaux(2,2)+ &
              dirsol(3)*mat_att_panneaux(2,3))
         cosz_panneaux = (dirsol(1)*mat_att_panneaux(3,1)+dirsol(2)*mat_att_panneaux(3,2)+ &
              dirsol(3)*mat_att_panneaux(3,3))
         cost_panneaux = 0._pm_reel
         surfapp_panneaux = spx*abs(cosx_panneaux) + spy*abs(cosy_panneaux) + spz*abs(cosz_panneaux)
      end if

    end subroutine MSP_presol_surfpan

    subroutine MSP_presol_accveh(coeff_ka, coeff_ks, coeff_kd, surfapp, dirsol, cosx, cosy, cosz, cost, &
         sx, sy, sz, st, mat_att_veh, normale_t, acc_ka, acc_ks, acc_kd)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_pre_solaire_accveh
!
!$Resume
!  Calcul de l'accélération de pression de radiation solaire du au véhicule
!
!$Description
!  Calcul de l'accélération de pression de radiation solaire du au véhicule
!
!$Auteur
!  Camille Hue (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calculer_presol_accveh()
!.    real (PM_REEL)                :: coeff_ka
!.    real (PM_REEL)                :: coeff_ks
!.    real (PM_REEL)                :: coeff_kd
!.    real (PM_REEL)                :: surfapp
!.    real (PM_REEL), dimension(3)  :: dirsol
!.    real (PM_REEL)                :: cosx
!.    real (PM_REEL)                :: cosy
!.    real (PM_REEL)                :: cosz
!.    real (PM_REEL)                :: cost
!.    real (PM_REEL)                :: sx
!.    real (PM_REEL)                :: sy
!.    real (PM_REEL)                :: sz
!.    real (PM_REEL)                :: st
!.    real (PM_REEL), dimension(3,3) :: mat_att_veh
!.    real (PM_REEL), dimension(3)  :: normale_t
!.    real (PM_REEL), dimension(3)  :: acc_ka
!.    real (PM_REEL), dimension(3)  :: acc_ks
!.    real (PM_REEL), dimension(3)  :: acc_kd
!
!$Arguments
!>E     coeff_ka              :<PM_REEL>            Coefficient d'absorption
!>E     coeff_ks              :<PM_REEL>            Coefficient de spécularité
!>E     coeff_kd              :<PM_REEL>            Coefficient de diffusion
!>E     surfapp               :<PM_REEL>            Surface d'application de la force
!>E     dirsol                :<pm_reel, DIM=(3)>   Vecteur direction du soleil (de norme 1)
!>E     cosx                  :<PM_REEL>            Angle d'application selon x
!>E     cosy                  :<PM_REEL>            Angle d'application selon y
!>E     cosz                  :<PM_REEL>            Angle d'application selon z
!>E     cost                  :<PM_REEL>            Angle d'application transverse
!>E     sx                    :<PM_REEL>            Surface d'application selon x
!>E     sy                    :<PM_REEL>            Surface d'application selon y
!>E     sz                    :<PM_REEL>            Surface d'application selon z
!>E     st                    :<PM_REEL>            Surface d'application transverse
!>E     mat_att_veh           :<PM_REEL, DIM=(3,3)>  Matrice d'attitude du véhicule
!>E     normale_t             :<PM_REEL, DIM=(3)>   Normale à la surface d'application
!>S     acc_ka                :<pm_reel, DIM=(3)>   Accélération due à la force d'absorption
!>S     acc_ks                :<pm_reel, DIM=(3)>   Accélération due à la force de spécularité
!>S     acc_kd                :<pm_reel, DIM=(3)>   Accélération due à la force de diffusion
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- 
!#
!
!$Remarques
!
!$Mots-cles
!  VEHICULE CALCULER PRESSION RADIATION SOLAIRE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ! ARGUMENTS
      real(kind=pm_reel), intent(in)                   :: coeff_ka, coeff_ks, coeff_kd
      real(kind=pm_reel), intent(in)                   :: surfapp
      real (kind=pm_reel), dimension(3), intent(IN)    :: dirsol
      real(kind=pm_reel), intent(in)                   :: cosx, cosy, cosz, cost
      real(kind=pm_reel), intent(in)                   :: sx, sy, sz, st
      real (kind=pm_reel), dimension(3,3), intent(IN)  :: mat_att_veh
      real (kind=pm_reel), dimension(3), intent(IN)    :: normale_t
      real (kind=pm_reel), dimension(3), intent(OUT)   :: acc_ka, acc_ks, acc_kd

      ! VARIABLES LOCALES
      real (kind=pm_reel) :: accx, accy, accz, acct
      real (kind=pm_reel), parameter   :: cfdif = 2._pm_reel/3._pm_reel

      ! Force d'absorbtion (sens opposé à dirsol)
      acc_ka(1) = -coeff_ka*surfapp*dirsol(1)
      acc_ka(2) = -coeff_ka*surfapp*dirsol(2)
      acc_ka(3) = -coeff_ka*surfapp*dirsol(3)

      ! Force de spécularité (dans le sens de la normale)
      accx = -sign(1._pm_reel,cosx)*2._pm_reel*coeff_ks*sx*cosx**2
      accy = -sign(1._pm_reel,cosy)*2._pm_reel*coeff_ks*sy*cosy**2
      accz = -sign(1._pm_reel,cosz)*2._pm_reel*coeff_ks*sz*cosz**2
      acct = -sign(1._pm_reel,cost)*2._pm_reel*coeff_ks*st*cost**2
      acc_ks(1) = accx*mat_att_veh(1,1) + accy*mat_att_veh(2,1) + &
           accz*mat_att_veh(3,1) + acct*normale_t(1)
      acc_ks(2) = accx*mat_att_veh(1,2) + accy*mat_att_veh(2,2) + &
           accz*mat_att_veh(3,2) + acct*normale_t(2)
      acc_ks(3) = accx*mat_att_veh(1,3) + accy*mat_att_veh(2,3) + &
           accz*mat_att_veh(3,3) + acct*normale_t(3)
      ! Force de diffusion (dans le sens de vdir et de la normale)
      accx = -sign(1._pm_reel,cosx)*sx*cosx
      accy = -sign(1._pm_reel,cosy)*sy*cosy
      accz = -sign(1._pm_reel,cosz)*sz*cosz
      acct = -sign(1._pm_reel,cost)*st*cost
      acc_kd(1) = coeff_kd * ( -surfapp*dirsol(1) + &
           cfdif*(accx*mat_att_veh(1,1) + accy*mat_att_veh(2,1) + accz*mat_att_veh(3,1)))
      acc_kd(2) = coeff_kd * ( -surfapp*dirsol(2) + &
           cfdif*(accx*mat_att_veh(1,2) + accy*mat_att_veh(2,2) + accz*mat_att_veh(3,2)))
      acc_kd(3) = coeff_kd * ( -surfapp*dirsol(3) + &
           cfdif*(accx*mat_att_veh(1,3) + accy*mat_att_veh(2,3) + accz*mat_att_veh(3,3)))

    end subroutine MSP_presol_accveh

    subroutine MSP_presol_accpan(coeff_ka, coeff_ks, coeff_kd, surfapp_panneaux, dirsol, &
         cosx_panneaux, cosy_panneaux, cosz_panneaux, mat_att_panneaux, &
         cosx, cosy, cosz, spx, spy, spz, acc_ka_panneaux, acc_ks_panneaux, acc_kd_panneaux)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_calculer_pre_solaire_accpan
!
!$Resume
!  Calcul de l'accélération de pression de radiation solaire du aux panneaux solaires
!
!$Description
!  Calcul de l'accélération de pression de radiation solaire du aux panneaux solaires
!
!$Auteur
!  Camille Hue (ATOS ORIGIN)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_calculer_presol_accveh()
!.    real (PM_REEL)                :: coeff_ka
!.    real (PM_REEL)                :: coeff_ks
!.    real (PM_REEL)                :: coeff_kd
!.    real (PM_REEL)                :: surfapp_panneaux
!.    real (PM_REEL), dimension(3)  :: dirsol
!.    real (PM_REEL)                :: cosx_panneaux
!.    real (PM_REEL)                :: cosy_panneaux
!.    real (PM_REEL)                :: cosz_panneaux
!.    real (PM_REEL), dimension(3,3) :: mat_att_panneaux
!.    real (PM_REEL)                :: cosx
!.    real (PM_REEL)                :: cosy
!.    real (PM_REEL)                :: cosz
!.    real (PM_REEL)                :: spx
!.    real (PM_REEL)                :: spy
!.    real (PM_REEL)                :: spz
!.    real (PM_REEL), dimension(3)  :: acc_ka_panneaux
!.    real (PM_REEL), dimension(3)  :: acc_ks_panneaux
!.    real (PM_REEL), dimension(3)  :: acc_kd_panneaux
!
!$Arguments
!>E     coeff_ka              :<PM_REEL>            Coefficient d'absorption
!>E     coeff_ks              :<PM_REEL>            Coefficient de spécularité
!>E     coeff_kd              :<PM_REEL>            Coefficient de diffusion
!>E     surfapp_panneaux      :<PM_REEL>            Surface d'application de la force
!>E     dirsol                :<pm_reel, DIM=(3)>   Vecteur direction du soleil (de norme 1)
!>E     cosx_panneaux         :<PM_REEL>            Angle d'application selon x
!>E     cosy_panneaux         :<PM_REEL>            Angle d'application selon y
!>E     cosz_panneaux         :<PM_REEL>            Angle d'application selon z
!>E     mat_att_panneaux      :<PM_REEL, DIM=(3,3)>  Matrice d'attitude du véhicule
!>E     cosx                  :<PM_REEL>            Angle d'application selon x (véhicule)
!>E     cosy                  :<PM_REEL>            Angle d'application selon y (véhicule)
!>E     cosz                  :<PM_REEL>            Angle d'application selon z (véhicule)
!>E     spx                    :<PM_REEL>            Surface d'application selon x
!>E     spy                    :<PM_REEL>            Surface d'application selon y
!>E     spz                    :<PM_REEL>            Surface d'application selon z
!>S     acc_ka_panneaux        :<pm_reel, DIM=(3)>   Accélération due à la force d'absorption
!>S     acc_ks_panneaux        :<pm_reel, DIM=(3)>   Accélération due à la force de spécularité
!>S     acc_kd_panneaux        :<pm_reel, DIM=(3)>   Accélération due à la force de diffusion
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- 
!#
!
!$Remarques
!
!$Mots-cles
!  VEHICULE CALCULER PRESSION RADIATION SOLAIRE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ! ARGUMENTS
      real(kind=pm_reel), intent(in)                   :: coeff_ka, coeff_ks, coeff_kd
      real(kind=pm_reel), intent(in)                   :: surfapp_panneaux
      real (kind=pm_reel), dimension(3), intent(IN)    :: dirsol
      real(kind=pm_reel), intent(in)                   :: cosx_panneaux, cosy_panneaux
      real(kind=pm_reel), intent(in)                   :: cosz_panneaux
      real (kind=pm_reel), dimension(3,3), intent(IN)  :: mat_att_panneaux
      real(kind=pm_reel), intent(in)                   :: cosx, cosy, cosz
      real(kind=pm_reel), intent(in)                   :: spx, spy, spz
      real (kind=pm_reel), dimension(3), intent(OUT)   :: acc_ka_panneaux, acc_ks_panneaux, acc_kd_panneaux

      ! VARIABLES LOCALES
      real (kind=pm_reel)              :: accx, accy, accz
      real (kind=pm_reel), parameter   :: cfdif = 2._pm_reel/3._pm_reel

      ! Force d'absorbtion (sens opposé à dirsol)
      acc_ka_panneaux(1) = -coeff_ka*surfapp_panneaux*dirsol(1)
      acc_ka_panneaux(2) = -coeff_ka*surfapp_panneaux*dirsol(2)
      acc_ka_panneaux(3) = -coeff_ka*surfapp_panneaux*dirsol(3)
      ! Force de spécularité (dans le sens de la normale)
      accx = -sign(1._pm_reel,cosx_panneaux)*2._pm_reel*coeff_ks*spx*cosx_panneaux**2
      accy = -sign(1._pm_reel,cosy_panneaux)*2._pm_reel*coeff_ks*spy*cosy_panneaux**2
      accz = -sign(1._pm_reel,cosz_panneaux)*2._pm_reel*coeff_ks*spz*cosz_panneaux**2
      acc_ks_panneaux(1) = accx*mat_att_panneaux(1,1) + accy*mat_att_panneaux(2,1) + &
           accz*mat_att_panneaux(3,1)
      acc_ks_panneaux(2) = accx*mat_att_panneaux(1,2) + accy*mat_att_panneaux(2,2) + &
           accz*mat_att_panneaux(3,2)
      acc_ks_panneaux(3) = accx*mat_att_panneaux(1,3) + accy*mat_att_panneaux(2,3) + &
           accz*mat_att_panneaux(3,3)
      ! Force de diffusion (dans le sens de vdir et de la normale)
      accx = -sign(1._pm_reel,cosx)*spx*cosx_panneaux
      accy = -sign(1._pm_reel,cosy)*spy*cosy_panneaux
      accz = -sign(1._pm_reel,cosz)*spz*cosz_panneaux
      acc_kd_panneaux(1) = coeff_kd * ( -surfapp_panneaux*dirsol(1) + &
           cfdif*(accx*mat_att_panneaux(1,1) + accy*mat_att_panneaux(2,1) + &
           accz*mat_att_panneaux(3,1)))
      acc_kd_panneaux(2) = coeff_kd * ( -surfapp_panneaux*dirsol(2) + &
           cfdif*(accx*mat_att_panneaux(1,2) + accy*mat_att_panneaux(2,2) + &
           accz*mat_att_panneaux(3,2)))
      acc_kd_panneaux(3) = coeff_kd * ( -surfapp_panneaux*dirsol(3) + &
           cfdif*(accx*mat_att_panneaux(1,3) + accy*mat_att_panneaux(2,3) + &
           accz*mat_att_panneaux(3,3)))

    end subroutine MSP_presol_accpan


    function exp_limit(u) result (reks)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  exp_limit
!
!$Resume
!  Fonction de régulation pour calcul d'une exponentielle.
!
!$Description
!  Fonction de régulation pour calcul d'une exponentielle.
!  L'exposant est borné entre un min (-600) et un max (+600)
!
!$Auteur
!  C. HUE d'après J.F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  reks = exp_limit(u)
!.    real(kind=pm_reel) :: u
!.    real(kind=pm_reel) :: reks
!
!$Arguments
!>E     u     :<pm_reel>   
!>S     reks  :<pm_reel>   
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
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none
      
      real(kind=pm_reel), intent(IN)  :: u
      real(kind=pm_reel) :: reks

      real(kind=pm_reel) :: expos

      ! L'exposant est borné par une borne max et une borne min
      ! de façon à ce que exp(expos) ne dépasse pas les capacités
      ! des réels double précision (la valeur 600 est choisie arbitrairement)
      expos = max(-600._pm_reel,u)
      expos = min(600._pm_reel,u)

      reks = exp(expos)

    end function exp_limit

end module MSP_PRE_SOLAIRE_DEF
