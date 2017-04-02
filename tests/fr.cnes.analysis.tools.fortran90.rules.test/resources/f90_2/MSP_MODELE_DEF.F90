module MSP_MODELE_DEF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_MODELE_DEF
!
!$Resume
!  Module gérant les modèles d'environnement planétaire.
!
!$Description
!  Module gérant les modèles d'environnement planétaire:
!-	
!
!$Auteur
!  J. F. GOESTER
! S. ROUSSEAU
!
!$Version
!  $Id: MSP_MODELE_DEF.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: MSP_MODELE_DEF.F90,v $
!  Revision 1.15  2010/10/20 09:35:43  mercadig
!  VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!  Revision 1.14  2008/11/19 13:32:20  mercadig
!  DM-ID 733 : Mise a jour cartouche
!
!  Revision 1.13  2008/07/04 14:58:11  huec
!  DM-ID 1058 : Gestion memoire
!  Revision 1.12  2008/06/03 09:39:44  huec
!  FA-ID 1060 : Initialisation des structures MECASPA nulles si elles ne sont pas fourines a MSP_creer_modele
!  Revision 1.11  2007/06/15 13:52:41  vivaresf
!  FA-ID 746 : mise en inout des structes suceptibles
!  d'être écrasées par une allocation pour permettre la désallocation propre
!  Revision 1.10  2005/10/28 17:13:47  vivaresf
!  DM-ID 116 : Cloture du FT (ajout de fonctions dans le module MSP_MODELE_DEF)
!  Revision 1.9.2.1  2005/10/28 17:09:21  vivaresf
!  DM-ID 116 : fonction d'acces rapide par pointeur
!  Revision 1.9  2005/03/08 07:32:35  fabrec
!  DM-ID 111 : mise à jour des cartouches
!  Revision 1.8  2005/01/20 13:56:20  pauh
!  FA_332
!  Revision 1.7.2.1  2005/01/19 10:20:17  pauh
!  FA 332 : Appels de DEALLOCATE avec l'argument stat=MSP_iostat
!  Revision 1.7  2005/01/10 12:48:06  vivaresf
!  FA_321
!  Revision 1.6.2.1  2005/01/07 10:53:10  vivaresf
!  FA-ID 321 : effacement propre des modeles planetes, potentiel et atmosphere avant re_affectation
!  Revision 1.6  2004/11/05 16:27:04  vivaresf
!  coquilles
!  Revision 1.5  2004/10/25 10:15:02  vivaresf
!  FA-ID 228 : sortie des routines
!  egaler (surcharges de l'operateur =) en inout pour pouvoir desallouer les pointeurs
!  et eviter les fuites memoires
!  Revision 1.4  2002/12/04 18:08:25  adm_ipsi
!  Utilisation du parametre NB_LONG_CHAINE
!  Revision 1.3  2002/12/03 17:21:02  adm_ipsi
!   Ajout de implicit none
!  Revision 1.2  2002/11/04 18:21:04  adm_ipsi
!  MSP_consulter_modele, test de la présence des éléments consultés
!  Revision 1.1.1.1  2002/09/30 14:09:35  adm_ipsi
!  Industrialisation de la MECASPA sans les modules de gestion d'erreurs
!  Revision 1.5  2002/05/03 07:50:54  util_am
!  Modifications dues au passage avec le compilateur 6.2 (=> NULL())
!  Revision 1.4  2000/07/04 13:53:33  util_am
!  - Ajout de nom_mod à MSP_creer_modele, MSP_consulter_modele et MSP_modifier_modele
!  Revision 1.3  2000/06/14 16:16:27  util_am
!  - Ajout du champ flag_func dans la structure MSP_MODELE pour la gestion des fuites mémoires
!  - Privatisation du contenu de la structure MSP_MODELE
!  - Ajout des routines MSP_consulter_modele, MSP_modifier_modele
!  - Ajout des intefaces anglaises
!  - Mise à jour des cartouches
!  Revision 1.2  2000/02/10 16:15:31  rousseau
!  Definition de l'opercreer_moateur =
!  Definition d'une routine affecter qui permet de realiser une affectation sans fuite memoire (utile dans le cas ou l'on veut utiliser une fonction pour initialiser une structure)
!  Revision 1.1.1.1  1999/07/13 08:37:58  util_am
!  Version 1.0 de MECASPA mise sous CVS
!
!$FinHistorique
!
!$Usage
!  use MSP_MODELE_DEF
!
!$Structure
!
!: MSP_MODELE : 
!#V
!>     flag_func     : <logical,private>                  
!>     nom_mod       : <LEN=MSP_LONG_CHAINE,private>      nom du modèle
!>     planete       : <MSP_PLANETE,pointer,private>      type dérivé contenant les données d'un modèle de planète
!>     potentiel     : <MSP_POTENTIEL,pointer,private>    type dérivé contenant les données d'un modèle de potentiel
!>     atmosphere    : <MSP_ATMOSPHERE,pointer,private>   type dérivé contenant les données d'un modèle d'atmosphère
!>     pre_solaire   : <MSP_PRE_SOLAIRE,pointer,private>  type dérivé contenant les données d'un modèle de pression de radiation solaire
!>     trois_corps   : <MSP_TROIS_CORPS,pointer,private>  type dérivé contenant les données d'un modèle de troisième corps
!#
!
!$Global
!
!$Common
!
!$Routines
!- MSP_clear_model
!- MSP_assign_model
!- MSP_create_model
!- MSP_get_model_data
!- MSP_set_model_data
!- MSP_exist_planet
!- MSP_exist_potential
!- MSP_exist_atmosphere
!- MSP_exist_radiation_pressure
!- MSP_exist_three_bodies
!- MSP_effacer_modele
!- MSP_affecter_modele
!- MSP_consulter_modele
!- MSP_modifier_modele
!#V
!- egaler_modele
!#
!
!$Fonctions
!- MSP_creer_modele
!- MSP_existe_planete
!- MSP_existe_potentiel
!- MSP_existe_atmosphere
!- MSP_existe_pre_solaire
!- MSP_existe_trois_corps
!- MSP_pt_modele_planete
!- MSP_pt_modele_potentiel
!- MSP_pt_modele_atmosphere
!- MSP_pt_modele_pre_solaire
!- MSP_pt_modele_trois_corps
!
!$Include
!
!$Module
!#V
!- MSLIB
!- MSP_GESTION_ERREUR
!- MSP_PLANETE_DEF
!- MSP_POTENTIEL_DEF
!- MSP_ATMOSPHERE_DEF
!- MSP_PRE_SOLAIRE_DEF
!- MSP_TROIS_CORPS_DEF
!#
!
!$Interface
!> msp_create_model :              MSP_creer_modele
!> msp_assign_model :              MSP_affecter_modele
!> assignment :                    egaler_modele
!> msp_exist_three_bodies :        MSP_existe_trois_corps
!> msp_set_model_data :            MSP_modifier_modele
!> msp_exist_planet :              MSP_existe_planete
!> msp_exist_atmosphere :          MSP_existe_atmosphere
!> msp_clear_model :               MSP_effacer_modele
!> msp_exist_potential :           MSP_existe_potentiel
!> msp_exist_radiation_pressure :  MSP_existe_pre_solaire
!> msp_get_model_data :            MSP_consulter_modele
!#V
!#
!
!$Remarques
!
!$Mots-cles
!  MODELES
!
!$Voir-Aussi
!#V
!.  egaler_modele
!#
!.  MSP_creer_modele MSP_existe_planete MSP_existe_potentiel MSP_existe_atmosphere MSP_existe_pre_solaire
!.  MSP_existe_trois_corps MSP_pt_modele_planete MSP_pt_modele_potentiel MSP_pt_modele_atmosphere
!.  MSP_pt_modele_pre_solaire MSP_pt_modele_trois_corps MSP_clear_model MSP_assign_model
!.  MSP_create_model MSP_get_model_data MSP_set_model_data MSP_exist_planet MSP_exist_potential
!.  MSP_exist_atmosphere MSP_exist_radiation_pressure MSP_exist_three_bodies MSP_effacer_modele
!.  MSP_affecter_modele MSP_consulter_modele MSP_modifier_modele
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use MSLIB
   use MSP_GESTION_ERREUR
   use MSP_PLANETE_DEF
   use MSP_POTENTIEL_DEF
   use MSP_ATMOSPHERE_DEF
   use MSP_PRE_SOLAIRE_DEF
   use MSP_TROIS_CORPS_DEF


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!!   Constante temporaires
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_MODELE_DEF.F90 69 2012-09-11 08:33:34Z ffsm $'




   type MSP_MODELE

      private
      logical :: flag_func
      character(LEN=MSP_LONG_CHAINE) :: nom_mod

      type(MSP_PLANETE),pointer     :: planete => NULL()
      type(MSP_POTENTIEL),pointer   :: potentiel => NULL()
      type(MSP_ATMOSPHERE),pointer  :: atmosphere => NULL()
      type(MSP_PRE_SOLAIRE),pointer :: pre_solaire => NULL()
      type(MSP_TROIS_CORPS),pointer :: trois_corps => NULL()

   end type MSP_MODELE


   interface assignment (=)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  assignment
!
!$Resume
!
!$Description
!
!$Acces
!  PUBLIC
!
!$Usage
!  modelea=modeleb
!.    type(MSP_modele) :: modelea
!.    type(MSP_modele) :: modeleb
!
!$Procedures
!#V
!- egaler_modele
!#
!
!$Remarques
!
!$Mots-cles
! MODELES EGALER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure egaler_modele
   end interface

   interface MSP_clear_model

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_clear_model
!
!$Resume
!  Clear a model structure
!
!$Description
!  Clear a model structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_clear_model (modele, [nul])
!.    type(MSP_modele) :: modele
!.    logical :: nul
!
!$Procedures
!- MSP_effacer_modele
!
!$Remarques
!
!$Mots-cles
! MODELES EFFACER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_effacer_modele
   end interface

   interface MSP_assign_model

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_assign_model
!
!$Resume
!  Assign a model structure to another
!
!$Description
!  Assign a model structure to another
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_assign_model(modelea,modeleb)
!.    type(MSP_modele) :: modelea
!.    type(MSP_modele) :: modeleb
!
!$Procedures
!- MSP_affecter_modele
!
!$Remarques
!
!$Mots-cles
! MODELES AFFECTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_affecter_modele
   end interface

   interface MSP_create_model

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_create_model
!
!$Resume
!  Create a model structure
!
!$Description
!  Create a model structure
!
!$Acces
!  PUBLIC
!
!$Usage
!  modele = MSP_create_model(potentiel,[planete],[atmosphere],[pre_solaire],[trois_corps],[nom_mod])
!.    type(MSP_potentiel) :: potentiel
!.    type(MSP_planete) :: planete
!.    type(MSP_atmosphere) :: atmosphere
!.    type(MSP_PRE_SOLAIRE) :: pre_solaire
!.    type(MSP_TROIS_CORPS) :: trois_corps
!.    character(LEN=*) :: nom_mod
!.    type (MSP_modele) :: modele
!
!$Procedures
!- MSP_creer_modele
!
!$Remarques
!
!$Mots-cles
! MODELES CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_creer_modele
   end interface

   interface MSP_get_model_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_get_model_data
!
!$Resume
!  Get models characteristics
!
!$Description
!  Get models characteristics
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_get_model_data(modele, [planete],[potentiel],[atmosphere],[pre_solaire],[trois_corps],[nom_mod])
!.    type(MSP_modele) :: modele
!.    type(MSP_planete) :: planete
!.    type(MSP_potentiel) :: potentiel
!.    type(MSP_atmosphere) :: atmosphere
!.    type(MSP_PRE_SOLAIRE) :: pre_solaire
!.    type(MSP_TROIS_CORPS) :: trois_corps
!.    character(LEN=*) :: nom_mod
!
!$Procedures
!- MSP_consulter_modele
!
!$Remarques
!
!$Mots-cles
! MODELES CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_consulter_modele
   end interface

   interface MSP_set_model_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_set_model_data
!
!$Resume
!  Change models characteristics
!
!$Description
!  Change models characteristics
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_set_model_data(modele, [planete],[potentiel],[atmosphere],[pre_solaire],[trois_corps],[nom_mod])
!.    type(MSP_modele) :: modele
!.    type(MSP_planete) :: planete
!.    type(MSP_potentiel) :: potentiel
!.    type(MSP_atmosphere) :: atmosphere
!.    type(MSP_PRE_SOLAIRE) :: pre_solaire
!.    type(MSP_TROIS_CORPS) :: trois_corps
!.    character(LEN=*) :: nom_mod
!
!$Procedures
!- MSP_modifier_modele
!
!$Remarques
!
!$Mots-cles
! MODELES MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_modifier_modele
   end interface

   interface MSP_exist_planet

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_exist_planet
!
!$Resume
!  Test the existence of a planet information
!
!$Description
!  Test the existence of a planet information
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = MSP_exist_planet(modele)
!.    type(MSP_MODELE) :: modele
!.    logical :: ok
!
!$Procedures
!- MSP_existe_planete
!
!$Remarques
!
!$Mots-cles
! MODELES PLANETE EXISTENCE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_existe_planete
   end interface
   
   interface MSP_exist_potential

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_exist_potential
!
!$Resume
!  Test the existence of a gravity potential information
!
!$Description
!  Test the existence of a gravity potential information
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = MSP_exist_potential(modele)
!.    type(MSP_MODELE) :: modele
!.    logical :: ok
!
!$Procedures
!- MSP_existe_potentiel
!
!$Remarques
!
!$Mots-cles
! MODELES POTENTIEL EXISTENCE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_existe_potentiel
   end interface
   
   interface MSP_exist_atmosphere

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_exist_atmosphere
!
!$Resume
!  Test the existence of an atmosphere information
!
!$Description
!  Test the existence of an atmosphere information
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = MSP_exist_atmosphere(modele)
!.    type(MSP_MODELE) :: modele
!.    logical :: ok
!
!$Procedures
!- MSP_existe_atmosphere
!
!$Remarques
!
!$Mots-cles
! MODELES ATMOSPHERE EXISTENCE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_existe_atmosphere
   end interface
   
   interface MSP_exist_radiation_pressure

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_exist_radiation_pressure
!
!$Resume
!  Test the existence of a radiation pressure information
!
!$Description
!  Test the existence of a radiation pressure information
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = MSP_exist_radiation_pressure(modele)
!.    type(MSP_MODELE) :: modele
!.    logical :: ok
!
!$Procedures
!- MSP_existe_pre_solaire
!
!$Remarques
!
!$Mots-cles
! MODELES RADIATION EXISTENCE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_existe_pre_solaire
   end interface
   
   interface MSP_exist_three_bodies

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_exist_three_bodies
!
!$Resume
!  Test the existence of a third body information
!
!$Description
!  Test the existence of a third body information
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = MSP_exist_three_bodies(modele)
!.    type(MSP_MODELE) :: modele
!.    logical :: ok
!
!$Procedures
!- MSP_existe_trois_corps
!
!$Remarques
!
!$Mots-cles
! MODELES 3CORPS EXISTENCE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module procedure MSP_existe_trois_corps
   end interface
   
   
   private :: egaler_modele

 contains
   
   subroutine egaler_modele(modelea,modeleb)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  egaler_modele
!
!$Resume
!	Routine interne au module definissant une affectation propres de 2 structures modeles
!
!$Description
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call egaler_modele(modelea,modeleb)
!.    type(MSP_modele) :: modelea
!.    type(MSP_modele) :: modeleb
!
!$Arguments
!>E/S   modelea  :<MSP_modele>   
!>E     modeleb  :<MSP_modele>   
!
!$Common
!
!$Routines
!- MSP_effacer_modele
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MODELES EGALER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
       implicit none

       type(MSP_modele),intent(inout) :: modelea
       type(MSP_modele),intent(in) :: modeleb


       call MSP_effacer_modele (modelea)
       modelea%flag_func = .false.


       modelea%nom_mod = modeleb%nom_mod

       if ( associated(modeleb%planete)) then
          allocate(modelea%planete)
          modelea%planete = modeleb%planete
       end if
       if ( associated(modeleb%potentiel)) then
          allocate(modelea%potentiel)
          modelea%potentiel = modeleb%potentiel
       end if
       if ( associated(modeleb%atmosphere)) then
          allocate(modelea%atmosphere)
          modelea%atmosphere = modeleb%atmosphere
       end if
       if ( associated(modeleb%pre_solaire)) then
          allocate(modelea%pre_solaire)
          modelea%pre_solaire = modeleb%pre_solaire
       end if
       if ( associated(modeleb%trois_corps)) then
          allocate(modelea%trois_corps)
          modelea%trois_corps = modeleb%trois_corps
       end if

       ! modeleb doit etre desalloue puisque c'est un clone cree par l'interface
       if (modeleb%flag_func) call MSP_effacer_modele(modeleb)

     end subroutine egaler_modele

     subroutine MSP_effacer_modele (modele, nul)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_effacer_modele
!
!$Resume
!	Routine permettant d'intialiser une variable de type modèle
!
!$Description
!	Routine permettant d'intialiser une variable de type modèle
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_effacer_modele (modele, [nul])
!.    type(MSP_modele) :: modele
!.    logical :: nul
!
!$Arguments
!>E/S   modele  :<MSP_modele>   Variable de type MSP_MODELE  
!>[E/S] nul     :<logical>      si nul=.true., on se contente des instructions NULLIFY (par défaut .false.)
!
!$Common
!
!$Routines
!- MSP_effacer_planete
!- MSP_effacer_potentiel
!- MSP_effacer_atmosphere
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MODELES EFFACER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
 
    ! Argument
    type(MSP_modele) :: modele
    logical, optional :: nul

    logical :: nul_tmp
    integer :: MSP_iostat
    
    MSP_iostat = 0

    if ( present (nul) ) then
       nul_tmp = nul
    else
       nul_tmp = .false.
    endif

    modele%nom_mod =""

    if ( nul_tmp ) then

      ! On se contente d'enlever les liens sans désallouer
      nullify(modele%planete)
      nullify(modele%potentiel)
      nullify(modele%atmosphere)
      nullify(modele%pre_solaire)
      nullify(modele%trois_corps)

    else

       ! Initialisation de la planete
       if ( associated(modele%planete)) then
          call MSP_effacer_planete(modele%planete)
          deallocate(modele%planete,stat=MSP_iostat)
       end if

       ! Initialisation du potentiel
       if ( associated(modele%potentiel)) then
          call MSP_effacer_potentiel(modele%potentiel)
          deallocate(modele%potentiel,stat=MSP_iostat)
       end if

      ! initialisation du modele d'atmosphere
       if ( associated(modele%atmosphere)) then
          call MSP_effacer_atmosphere(modele%atmosphere)
          deallocate(modele%atmosphere,stat=MSP_iostat)
       end if


       ! Initialisation de la pression de radiation solaire
       if ( associated(modele%pre_solaire)) then
          deallocate(modele%pre_solaire,stat=MSP_iostat)
       end if

      ! Initialisation du 3corps
       if ( associated(modele%trois_corps)) then
          deallocate(modele%trois_corps,stat=MSP_iostat)
       end if

    endif

    end subroutine MSP_effacer_modele


     subroutine MSP_affecter_modele(modelea,modeleb)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_affecter_modele
!
!$Resume
!	Routine permettant de realiser les operation suivante :
!	modelea = modeleb
!	supprimer modeleb
!
!$Description
!	Routine permettant de realiser les operation suivante :
!	modelea = modeleb
!	supprimer modeleb
!	Cette routine a ete cree pour eviter des fuites memoires
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_affecter_modele(modelea,modeleb)
!.    type(MSP_modele) :: modelea
!.    type(MSP_modele) :: modeleb
!
!$Arguments
!>E/S   modelea  :<MSP_modele>   modèle à gauche du signe égal
!>E/S   modeleb  :<MSP_modele>   modèle à droite du signe égal
!
!$Common
!
!$Routines
!- MSP_effacer_modele
!#V
!- egaler_modele
!#
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MODELES AFFECTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       
       implicit none

       type(MSP_modele),intent(inout) :: modelea
       type(MSP_modele) :: modeleb

       call egaler_modele(modelea,modeleb)
       call MSP_effacer_modele(modeleb)
     end subroutine MSP_affecter_modele

    function MSP_creer_modele(potentiel,planete,atmosphere,pre_solaire,trois_corps,nom_mod) result (modele)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_creer_modele
!
!$Resume
!	Routine permettant de créer une variable de type MSP_MODELE
!
!$Description
!	Routine permettant de créer une variable de type MSP_MODELE à partir de différents sous-type:
!.      potentiel (MSP_potentiel)   
!.	planete (MSP_planete)  
!.      atmosphere (MSP_atmosphere)    
!.      pre_solaire (MSP_PRE_SOLAIRE)   
!.      trois_corps (MSP_TROIS_CORPS)   
!
!$Auteur
!	J.F. GOESTER
!	S. ROUSSEAU
!
!$Acces
!  PUBLIC
!
!$Usage
!  modele = MSP_creer_modele(potentiel,[planete],[atmosphere],[pre_solaire],[trois_corps],[nom_mod])
!.    type(MSP_potentiel) :: potentiel
!.    type(MSP_planete) :: planete
!.    type(MSP_atmosphere) :: atmosphere
!.    type(MSP_PRE_SOLAIRE) :: pre_solaire
!.    type(MSP_TROIS_CORPS) :: trois_corps
!.    character(LEN=*) :: nom_mod
!.    type (MSP_modele) :: modele
!
!$Arguments
!>E     potentiel    :<MSP_potentiel>     type dérivé contenant les données d'un modèle de potentiel
!>[E]   planete      :<MSP_planete>       type dérivé contenant les données d'un modèle de planète
!>[E]   atmosphere   :<MSP_atmosphere>    type dérivé contenant les données d'un modèle d'atmosphère
!>[E]   pre_solaire  :<MSP_PRE_SOLAIRE>   type dérivé contenant les données d'un modèle de pression de radiation solaire
!>[E]   trois_corps  :<MSP_TROIS_CORPS>   type dérivé contenant les données d'un modèle de troisième corps
!>[E]   nom_mod      :<LEN=*>             
!>S     modele       :<MSP_modele>        Structure modèle
!
!$Common
!
!$Routines
!- MSP_effacer_modele
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MODELES CREER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       implicit none

      ! Arguments
      type(MSP_potentiel),intent(in)            :: potentiel
      type(MSP_planete),intent(in),optional     :: planete
      type(MSP_atmosphere),intent(in),optional  ::atmosphere
      type(MSP_PRE_SOLAIRE),intent(in),optional :: pre_solaire
      type(MSP_TROIS_CORPS),intent(in),optional :: trois_corps
      character(LEN=*), intent(IN), optional    :: nom_mod
      type (MSP_modele):: modele


      ! initialisation du modele
      call MSP_effacer_modele(modele, nul=.true.)
      modele%flag_func = .true.

      if (present(nom_mod)) then 
         modele%nom_mod = nom_mod
      else
         modele%nom_mod = ""
      end if

      ! Affectation des différents sous éléments

      if ( present(planete)) then
         allocate(modele%planete)
         modele%planete = planete
      end if

      ! Un modele de potentiel est obligatoire
      allocate(modele%potentiel)
      modele%potentiel = potentiel

      if (present(atmosphere)) then
         allocate(modele%atmosphere)
         modele%atmosphere = atmosphere
      end if

      if(present(pre_solaire)) then
         allocate(modele%pre_solaire)
         modele%pre_solaire = pre_solaire
      end if

      if (present(trois_corps)) then
         allocate(modele%trois_corps)
         modele%trois_corps = trois_corps
      end if

    end function MSP_creer_modele


    subroutine MSP_consulter_modele(modele, planete,potentiel,atmosphere,pre_solaire,trois_corps,nom_mod)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consulter_modele
!
!$Resume
!  Consultation des champs de la structure modèle
!
!$Description
!  Consultation des champs de la structure modèle
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_consulter_modele(modele, [planete],[potentiel],[atmosphere],[pre_solaire],[trois_corps],[nom_mod])
!.    type(MSP_modele) :: modele
!.    type(MSP_planete) :: planete
!.    type(MSP_potentiel) :: potentiel
!.    type(MSP_atmosphere) :: atmosphere
!.    type(MSP_PRE_SOLAIRE) :: pre_solaire
!.    type(MSP_TROIS_CORPS) :: trois_corps
!.    character(LEN=*) :: nom_mod
!
!$Arguments
!>E     modele       :<MSP_modele>        Structure modèles à consulter
!>[E/S] planete      :<MSP_planete>       structure planète récupérée
!>[E/S] potentiel    :<MSP_potentiel>     structure potentiel récupérée
!>[E/S] atmosphere   :<MSP_atmosphere>    structure atmosphere récupérée
!>[S]   pre_solaire  :<MSP_PRE_SOLAIRE>   structure pre_solaire récupérée
!>[E/S] trois_corps  :<MSP_TROIS_CORPS>   structure trois_corps récupérée
!>[S]   nom_mod      :<LEN=*>             
!
!$Common
!
!$Routines
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MODELES CONSULTER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_modele), intent(IN) :: modele
      type(MSP_planete),intent(INOUT),optional :: planete
      type(MSP_potentiel),intent(INOUT),optional :: potentiel
      type(MSP_atmosphere),intent(INOUT),optional::atmosphere
      type(MSP_PRE_SOLAIRE),intent(OUT),optional :: pre_solaire
      type(MSP_TROIS_CORPS),intent(INOUT),optional :: trois_corps
      character(LEN=*), intent(OUT), optional :: nom_mod

      if (present(planete))   then
         if (MSP_existe_planete(modele)) then
            planete  = modele%planete
          else
             call MSP_signaler_message (message="pas de # dans le modele", &
                  routine="MSP_consulter_planete", type=MSP_ENUM_WARNING, &
                  partie_variable='"planete "', code="MSP_consulter_modele_1")
          end if
      end if

      if (present(potentiel))   then
         if (MSP_existe_potentiel(modele)) then
            potentiel  = modele%potentiel
          else
             call MSP_signaler_message (message="pas de # dans le modele", &
                  routine="MSP_consulter_planete", type=MSP_ENUM_WARNING, &
                  partie_variable='"potentiel "', code="MSP_consulter_modele_1")
          end if
      end if


      if (present(atmosphere ))   then
         if (MSP_existe_atmosphere (modele)) then
            atmosphere  = modele%atmosphere
          else
             call MSP_signaler_message (message="pas d' # dans le modele", &
                  routine="MSP_consulter_planete", type=MSP_ENUM_WARNING, &
                  partie_variable='"atmosphere  "', code="MSP_consulter_modele_1")
          end if
      end if

      if (present(pre_solaire ))   then
         if (MSP_existe_pre_solaire (modele)) then
             pre_solaire = modele%pre_solaire
          else
             call MSP_signaler_message (message="pas de # dans le modele", &
                  routine="MSP_consulter_planete", type=MSP_ENUM_WARNING, &
                  partie_variable='"pre_solaire  "', code="MSP_consulter_modele_1")
          end if
      end if

      if (present(trois_corps ))   then
         if (MSP_existe_trois_corps (modele)) then
             trois_corps = modele%trois_corps
          else
             call MSP_signaler_message (message="pas de # dans le modele", &
                  routine="MSP_consulter_planete", type=MSP_ENUM_WARNING, &
                  partie_variable='"trois_corps  "', code="MSP_consulter_modele_1")
          end if
      end if


      if (present(nom_mod))     nom_mod     = modele%nom_mod

    end subroutine MSP_consulter_modele


    subroutine MSP_modifier_modele(modele, planete,potentiel,atmosphere,pre_solaire,trois_corps,nom_mod)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_modifier_modele
!
!$Resume
!  Consultation des champs de la structure modèle
!
!$Description
!  Consultation des champs de la structure modèle
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_modifier_modele(modele, [planete],[potentiel],[atmosphere],[pre_solaire],[trois_corps],[nom_mod])
!.    type(MSP_modele) :: modele
!.    type(MSP_planete) :: planete
!.    type(MSP_potentiel) :: potentiel
!.    type(MSP_atmosphere) :: atmosphere
!.    type(MSP_PRE_SOLAIRE) :: pre_solaire
!.    type(MSP_TROIS_CORPS) :: trois_corps
!.    character(LEN=*) :: nom_mod
!
!$Arguments
!>E/S   modele       :<MSP_modele>        Structure modèles à modifier
!>[E]   planete      :<MSP_planete>       Structure planete à substituer
!>[E]   potentiel    :<MSP_potentiel>     Structure potentiel à substituer
!>[E]   atmosphere   :<MSP_atmosphere>    Structure atmosphere à substituer
!>[E]   pre_solaire  :<MSP_PRE_SOLAIRE>   Structure pression de radiation solaire à substituer
!>[E]   trois_corps  :<MSP_TROIS_CORPS>   Structure troisième corps à substituer
!>[E]   nom_mod      : char(LEN=*)        Nom du modèle à substituer
!>[E]   nom_mod      :<LEN=*>             
!
!$Common
!
!$Routines
!- MSP_effacer_planete
!- MSP_effacer_potentiel
!- MSP_effacer_atmosphere
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MODELES MODIFIER
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_modele), intent(INOUT) :: modele
      type(MSP_planete),intent(IN),optional :: planete
      type(MSP_potentiel),intent(IN),optional :: potentiel
      type(MSP_atmosphere),intent(IN),optional::atmosphere
      type(MSP_PRE_SOLAIRE),intent(IN),optional :: pre_solaire
      type(MSP_TROIS_CORPS),intent(IN),optional :: trois_corps
      character(LEN=*), intent(IN), optional :: nom_mod

      integer :: lnom
      character(LEN=80),dimension(2)       :: tmessage_var

      if (present(planete)) then
         if ( associated(modele%planete)) then
            call MSP_effacer_planete(modele%planete)
         end if
         modele%planete     = planete
      endif
      if (present(potentiel)) then
         if ( associated(modele%potentiel)) then
            call MSP_effacer_potentiel(modele%potentiel)
         end if
         modele%potentiel   = potentiel
      endif
      if (present(atmosphere)) then
         if ( associated(modele%atmosphere)) then
            call MSP_effacer_atmosphere(modele%atmosphere)
         end if
          modele%atmosphere  = atmosphere
      endif
      if (present(pre_solaire)) then
         modele%pre_solaire = pre_solaire
      endif
      if (present(trois_corps)) then
         modele%trois_corps = trois_corps
      endif

      if (present(nom_mod))     modele%nom_mod     = nom_mod
      if ( PRESENT(nom_mod) ) then
         lnom = LEN_TRIM(nom_mod)
         if ( lnom <= MSP_LONG_CHAINE ) then
            modele%nom_mod  = nom_mod
         else
            modele%nom_mod  = nom_mod(1:MSP_LONG_CHAINE)
            tmessage_var(1) = 'Le nom du modèle'
            write(tmessage_var(2),'(I8)')   MSP_LONG_CHAINE

            call MSP_signaler_message (cle_mes="MSP_LONGUEUR_CHAINE", &
                 routine="MSP_modifier_modele", type=MSP_ENUM_WARNING, &
                 partie_variable=tmessage_var)
         endif
      else
         modele%nom_mod = ""
      endif

  
    end subroutine MSP_modifier_modele


    function MSP_existe_planete(modele) result (ok)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_existe_planete
!
!$Resume
!	Fonction de consultation permettant de savoir si la planète est définie dans une variable de type MSP_MODELE
!
!$Description
!	Fonction de consultation permettant de savoir si la planète est définie dans une variable de type MSP_MODELE
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = MSP_existe_planete(modele)
!.    type(MSP_MODELE) :: modele
!.    logical :: ok
!
!$Arguments
!>E     modele  :<MSP_MODELE>   variable de type MSP_MODELE
!>S     ok      :<logical>      booléen de retour
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
!  MODELES PLANETE EXISTENCE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_MODELE),intent(in):: modele
      logical :: ok
      ok = (associated(modele%planete))

    end function MSP_existe_planete


     function MSP_existe_potentiel(modele) result (ok)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_existe_potentiel
!
!$Resume
!	Fonction de consultation permettant de savoir si le potentiel est défini dans une variable de type MSP_MODELE
!
!$Description
!	Fonction de consultation permettant de savoir si le potentiel est défini dans une variable de type MSP_MODELE
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = MSP_existe_potentiel(modele)
!.    type(MSP_MODELE) :: modele
!.    logical :: ok
!
!$Arguments
!>E     modele  :<MSP_MODELE>   variable de type MSP_MODELE 
!>S     ok      :<logical>      booléen de retour
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
!  MODELES POTENTIEL EXISTENCE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_MODELE),intent(in):: modele
      logical :: ok
      ok = (associated(modele%potentiel))

   end function MSP_existe_potentiel


     function MSP_existe_atmosphere(modele) result (ok)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_existe_atmosphere
!
!$Resume
!	Fonction de consultation permettant de savoir si l'atmosphére est définie dans une variable de type MSP_MODELE
!
!$Description
!	Fonction de consultation permettant de savoir si l'atmosphére est définie dans une variable de type MSP_MODELE
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = MSP_existe_atmosphere(modele)
!.    type(MSP_MODELE) :: modele
!.    logical :: ok
!
!$Arguments
!>E     modele  :<MSP_MODELE>   variable de type MSP_MODELE
!>S     ok      :<logical>      booléen de retour
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
!  MODELES ATMOSPHERE EXISTENCE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       implicit none

       type(MSP_MODELE),intent(in):: modele
       logical :: ok

      ok = (associated(modele%atmosphere))

    end function MSP_existe_atmosphere

    function MSP_existe_pre_solaire(modele) result (ok)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_existe_pre_solaire
!
!$Resume
!	Fonction de consultation permettant de savoir si la pression de radiation solaire  est définie dans une variable de type MSP_MODELE
!
!$Description
!	Fonction de consultation permettant de savoir si la pression de radiation solaire  est définie dans une variable de type MSP_MODELE
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = MSP_existe_pre_solaire(modele)
!.    type(MSP_MODELE) :: modele
!.    logical :: ok
!
!$Arguments
!>E     modele  :<MSP_MODELE>   variable de type MSP_MODELE
!>S     ok      :<logical>      booléen de retour
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
!  MODELES RADIATION EXISTENCE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      type(MSP_MODELE),intent(in):: modele
      logical :: ok

      ok = (associated(modele%pre_solaire))


    end function MSP_existe_pre_solaire

     function MSP_existe_trois_corps(modele) result (ok)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_existe_trois_corps
!
!$Resume
!	Fonction de consultation permettant de savoir si les troisiémes corps  sont définis dans une variable de type MSP_MODELE
!
!$Description
!	Fonction de consultation permettant de savoir si les troisiémes corps  sont définis dans une variable de type MSP_MODELE
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PUBLIC
!
!$Usage
!  ok = MSP_existe_trois_corps(modele)
!.    type(MSP_MODELE) :: modele
!.    logical :: ok
!
!$Arguments
!>E     modele  :<MSP_MODELE>   variable de type MSP_MODELE
!>S     ok      :<logical>      booléen de retour
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

      type(MSP_MODELE),intent(in):: modele
      logical :: ok

      ok = (associated(modele%trois_corps))


    end function MSP_existe_trois_corps

!! DM 116 : fonctions de retour de pointeurs

    function MSP_pt_modele_planete(modele) result (planete)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_pt_modele_planete
!
!$Resume
!    Accès rapide à la structure MSP_PLANETE de MSP_MODELE
!
!$Description
!    Accès rapide par pointeur à la structure MSP_PLANETE de MSP_MODELE
!
!$Auteur
!    F. Vivarès (ATOS Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  planete = MSP_pt_modele_planete(modele)
!.    type(MSP_MODELE) :: modele
!.    type (MSP_PLANETE),pointer :: planete
!
!$Arguments
!>E     modele   :<MSP_MODELE>            Structure regroupant les modèles
!>S     planete  :<MSP_PLANETE,pointer>   Pointeur sur le champ planete
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
!       L'utilisateur devra tester si celui-ci est alloué avec MSP_existe_planete
!        avant de l'utiliser
!       Attention a utiliser l'affectation de pointeur "=>" et non "=" 
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      type(MSP_MODELE),intent(in):: modele
      type (MSP_PLANETE),pointer:: planete
      
      planete => modele%planete
    end function MSP_pt_modele_planete

    function MSP_pt_modele_potentiel(modele) result (potentiel)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_pt_modele_potentiel
!
!$Resume
!    Accès rapide à la structure MSP_POTENTIEL de MSP_MODELE
!
!$Description
!    Accès rapide par pointeur à la structure MSP_POTENTIEL de MSP_MODELE
!
!$Auteur
!    F. Vivarès (ATOS Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  potentiel = MSP_pt_modele_potentiel(modele)
!.    type(MSP_MODELE) :: modele
!.    type (MSP_POTENTIEL),pointer :: potentiel
!
!$Arguments
!>E     modele     :<MSP_MODELE>              Structure regroupant les modèles
!>S     potentiel  :<MSP_POTENTIEL,pointer>   Pointeur sur le champ potentiel
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
!       L'utilisateur devra tester si celui-ci est alloué avec MSP_existe_potentiel
!       avant de l'utiliser
!       Attention a utiliser l'affectation de pointeur "=>" et non "=" 
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      type(MSP_MODELE),intent(in):: modele
      type (MSP_POTENTIEL),pointer:: potentiel
      
      potentiel => modele%potentiel
    end function MSP_pt_modele_potentiel

    function MSP_pt_modele_atmosphere(modele) result (atmosphere)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_pt_modele_atmosphere
!
!$Resume
!    Accès rapide à la structure MSP_ATMOSPHERE de MSP_MODELE
!
!$Description
!    Accès rapide par pointeur à la structure MSP_ATMOSPHERE de MSP_MODELE
!
!$Auteur
!    F. Vivarès (ATOS Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  atmosphere = MSP_pt_modele_atmosphere(modele)
!.    type(MSP_MODELE) :: modele
!.    type (MSP_ATMOSPHERE),pointer :: atmosphere
!
!$Arguments
!>E     modele      :<MSP_MODELE>               Structure regroupant les modèles
!>S     atmosphere  :<MSP_ATMOSPHERE,pointer>   Pointeur sur le champ atmosphere
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
!       L'utilisateur devra tester si celui-ci est alloué avec MSP_existe_atmosphere
!       avant de l'utiliser
!       Attention a utiliser l'affectation de pointeur "=>" et non "=" 
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      type(MSP_MODELE),intent(in):: modele
      type (MSP_ATMOSPHERE),pointer:: atmosphere
      
      atmosphere => modele%atmosphere
    end function MSP_pt_modele_atmosphere

    function MSP_pt_modele_pre_solaire(modele) result (pre_solaire)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_pt_modele_pre_solaire
!
!$Resume
!    Accès rapide à la structure MSP_PRE_SOLAIRE de MSP_MODELE
!
!$Description
!    Accès rapide par pointeur à la structure MSP_PRE_SOLAIRE de MSP_MODELE
!
!$Auteur
!    F. Vivarès (ATOS Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  pre_solaire = MSP_pt_modele_pre_solaire(modele)
!.    type(MSP_MODELE) :: modele
!.    type (MSP_PRE_SOLAIRE),pointer :: pre_solaire
!
!$Arguments
!>E     modele       :<MSP_MODELE>                Structure regroupant les modèles
!>S     pre_solaire  :<MSP_PRE_SOLAIRE,pointer>   Pointeur sur le champ pre_solaire
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
!       L'utilisateur devra tester si celui-ci est alloué avec MSP_existe_pre_solaire
!       avant de l'utiliser
!       Attention a utiliser l'affectation de pointeur "=>" et non "=" 
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      type(MSP_MODELE),intent(in):: modele
      type (MSP_PRE_SOLAIRE),pointer:: pre_solaire
      
      pre_solaire => modele%pre_solaire
    end function MSP_pt_modele_pre_solaire

    function MSP_pt_modele_trois_corps(modele) result (trois_corps)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_pt_modele_trois_corps
!
!$Resume
!    Accès rapide à la structure MSP_TROIS_CORPS de MSP_MODELE
!
!$Description
!    Accès rapide par pointeur à la structure MSP_TROIS_CORPS de MSP_MODELE
!
!$Auteur
!    F. Vivarès (ATOS Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  trois_corps = MSP_pt_modele_trois_corps(modele)
!.    type(MSP_MODELE) :: modele
!.    type (MSP_TROIS_CORPS),pointer :: trois_corps
!
!$Arguments
!>E     modele       :<MSP_MODELE>                Structure regroupant les modèles
!>S     trois_corps  :<MSP_TROIS_CORPS,pointer>   Pointeur sur le champ trois_corps
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
!       L'utilisateur devra tester si celui-ci est alloué avec MSP_existe_trois_corps
!       avant de l'utiliser
!       Attention a utiliser l'affectation de pointeur "=>" et non "=" 
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      type(MSP_MODELE),intent(in):: modele
      type (MSP_TROIS_CORPS),pointer:: trois_corps
      
      trois_corps => modele%trois_corps
    end function MSP_pt_modele_trois_corps

      
  end module MSP_MODELE_DEF
 
