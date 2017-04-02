MODULE PS_UTILITAIRES

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  PS_UTILITAIRES
!
!$Resume
!  Module contenant des routines relatives à la consultation, génération, modification de scénario de lois
!
!$Description
!  Module contenant des routines relatives à la consultation, génération, modification de scénario de lois
!
!$Auteur
!  Jean-Jacques Wasbauer
!
!$Version
!  $Id: ps_utilitaires.F90 368 2013-02-19 14:43:59Z aadt $
!
!$Historique
!  $Log: ps_utilitaires.F90,v $
!  Revision 368  2013/02/19 aadt
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 1.41  2010/10/25 13:04:28  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.40  2010/03/01 13:45:55  mercadig
!  VERSION:9.6:DM-ID:1350:01/03/2010: Initialisation de la variable locale iprop_tmp dans ps_charger_fichier_propu
!
!  Revision 1.39  2009/12/03 13:24:34  mercadig
!  AQ: Suppression de variables inutilisees
!
!  Revision 1.38  2009/11/13 14:25:22  mercadig
!  DM-ID 842 : Dans ps_creer_modele_file suppression de la ressource data_actsol et mise a jour pour gestion du type activite reelle COMPAS
!
!  Revision 1.37  2009/09/04 15:16:30  mercadig
!  DM-ID 1218: Recherche des fichiers ephemerides dans la base locale COMPAS, suppression de la ressource data_3corps
!
!  Revision 1.36  2009/08/27 13:54:10  cmartel
!  DM-ID 1120 : Prise en compte de la modification de signature des modèles exp.
!
!  Revision 1.35  2009/03/20 08:40:13  tanguyy
!  Integration du nouveau parametre de GS_PS_MODELES (FA-ID 1179)
!
!  Revision 1.34  2008/12/08 08:46:32  tanguyy
!  AQ : amelioration des liberations memoires
!
!  Revision 1.33  2008/12/03 17:01:20  mercadig
!  DM-ID 733 : Correction creation structure mci (masse) dans ps_creer_vehicule_file
!
!  Revision 1.32  2008/12/02 13:27:10  mercadig
!  DM-ID 733: Ajout structure emcd pour creation structure atmosphere
!
!  Revision 1.31  2008/10/20 14:42:30  mercadig
!  FA-ID 1129 : Correction dimension des tableaux forme_sep et tymasse_sep dans la routine ps_init_scenario_separ
!
!  Revision 1.30  2008/10/14 15:35:36  tanguyy
!  DM-ID 1058 : suppression des warnings MSPRO
!
!  Revision 1.29  2008/05/02 14:38:32  tanguyy
!  FA-ID 865 : suppression de modprec (obsolete dans GSLIB et inutilise dans PSIMU)
!
!  Revision 1.28  2008/04/29 17:22:40  tanguyy
!  AQ : utilisation de msp_effacer_scenario dans toutes les routines ps_charger_scenario..
!
!  Revision 1.27  2008/04/22 08:19:39  tanguyy
!  Intégration GSLIB V6.9 (dont impact lié à la FA-ID 865 sur modprec)
!
!  Revision 1.26  2008/04/03 16:09:49  ttn
!  FA-ID 658 : suppression des variables inutilisees
!
!  Revision 1.24  2007/10/31 15:24:55  tanguyy
!  FA-ID 823 : initialisation du scenario avec mise à NULL des pointeurs
!
!  Revision 1.23  2007/07/09 13:04:21  tanguyy
!  DM-ID 702 / PSIMU V9-0
!
!  Revision 1.22  2007/06/21 13:16:05  vivaresf
!  FA-ID 746 : validation
!
!  Revision 1.21  2007/02/06 16:14:30  vivaresf
!  Validation version V8.7a1
!
!  Revision 1.20  2007/02/02 13:32:56  tanguyy
!  DM-ID 659
!
!  Revision 1.19  2007/02/01 17:53:48  vivaresf
!  FA-ID 695 : fuites mémoire
!
!  Revision 1.18  2007/01/26 15:56:02  vivaresf
!  DM-ID 642, ps_charger_fichier_separ : tablmeaus de dimension ps_nsepa
!  DM-ID 643 ps_creer_modele_file : lecture de str%modvent
!  DM-ID 659 ps_creer_vehicule_file : lecture de str%typcf_alt
!  DM-ID 659 ps_charger_fichier_separ : lecture de typcf_alt
!
!  Revision 1.17  2006/12/05 15:10:05  fabrec
!  suppression de variables redondantes
!
!  Revision 1.16  2006/12/01 13:39:13  tanguyy
!  PSIMU V8-6 / Integration finale
!
!  Revision 1.15  2006/11/07 13:09:58  tanguyy
!  Modif d'un appel MECASPA (DM-ID 560)
!
!  Revision 1.14  2006/11/06 17:22:36  tanguyy
!  DM-ID 560 : 1ere etape : reste a faire la traduction correcte d'un nom de fichier vers le nom de modele
!
!  Revision 1.13  2006/03/16 13:14:51  tanguyy
!  FA-ID 496 : correction de la conversion PSIMU -> MECASPA pour les types de coefs de frottement
!
!  Revision 1.12  2005/02/24 15:45:47  fabrec
!  DM-ID 235 : Couverture de test des utilitaires PSIMU
!
!  Revision 1.11  2005/01/31 16:41:25  fabrec
!  DM-ID 235 : Couverture de test des utilitaires PSIMU
!
!  Revision 1.10  2004/12/08 08:08:20  vivaresf
!  Code 2 pour propulsion tabulees
!
!  Revision 1.9  2004/11/19 11:23:09  vivaresf
!  FA-ID 245 : gestion des 3 types de dates (absolues, relatives ou absolues-relatives)
!  et des dates en jour/secondes
!
!  Revision 1.8  2004/07/05 16:38:36  ole
!  FA_159
!
!  Revision 1.7.2.1  2004/07/02 14:54:23  ole
!  Correction de la FA 159
!
!  Revision 1.7  2004/06/18 11:01:47  vivaresf
!  Mise a jour des entetes
!
!  Revision 1.5.2.2  2004/06/18 10:04:48  vivaresf
!  DM_ID 133 : nouvelles variables pour dans la separation
!
!  Revision 1.5  2003/12/09 17:09:32  adm_ipsi
!  FA-ID 94, Ajout du MSP_effacer_vehicule
!
!  Revision 1.4  2003/11/28 16:55:48  adm_ipsi
!  DM-ID 9, Ajout du Choix du fichier d'activite solaire
!
!  Revision 1.3  2003/10/27 11:26:09  adm_ipsi
!  DM-ID 65, Compatibilité des utilitaires
!
!  Revision 1.2  2003/08/08 13:36:04  adm_ipsi
!  Nouvelle version
!
!  Revision 1.8  2003/08/07 16:01:33  util_am
!  SLB - version industrialisee : mise a jour livraison du 4/08/2003
!
!  Revision 1.6  2003/03/28 09:56:48  util_am
!  SLB - Version industrialisée
!
!  Revision 1.1.1.1  2002/09/30 14:59:35  laurent
!  Industrialisation PSIMU
!
!  Revision 1.5  2002/09/16 11:06:57  util_am
!  Introduction d'une direction de poussée indépendante de l'attitude
!
!  Revision 1.4  2000/08/31 14:56:16  util_am
!  Ajout de l'argument dirept à la fonction ps_creer_modele_file.
!  Modification du traitement du nom du directory potentiel pour être en accord
!  avec la nouvelle GS_LIB
!
!  Revision 1.3  2000/06/22 11:10:00  util_am
!  Correction d'un bug sur le passage des arguments lors de la création du
!  scenario d'attitude.
!  Mise au norme de la syntaxe et des acces à structures privees vis-à-vis de
!  la nouvelle MECASPA V1-5
!
!  Revision 1.1  2000/02/24 16:59:12  util_am
!  ajout temporaire d'une correction du fichier ps_utilitaire de PSIMU
!  Revision 1.2  1999/11/09 09:46:25  util_am
!  Mise à jour de l'appel de read_gs_ps_modeles
!  Revision 1.1  1999/10/26 07:58:55  util_am
!  Ajout d'utilitaires contenus dans le module ps_utilitaires
!
!$FinHistorique
!
!$Usage
!  use PS_UTILITAIRES
!
!$Structure
!
!$Global
!
!>  gsun   : <integer>  
!$Common
!#V
!- common /unites/
!#
!
!$Routines
!- ps_init_scenario_propu
!- ps_init_scenario_attit
!- ps_init_scenario_separ
!- ps_charger_fichier_propu
!- ps_charger_fichier_attit
!- ps_charger_fichier_separ
!
!$Fonctions
!- ps_creer_scenario_file
!- ps_creer_vehicule_file
!- ps_creer_modele_file
!- ps_creer_integration_file
!
!$Include
!
!$Module
!#V
!- MECASPA
!- INTERFACE_PSIMU
!#
!
!$Interface
!#V
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  USE MECASPA
  use MSP_INTEGRATOR_DEF, only : msp_integrator, msp_creer_integrator, msp_consulter_integrator, msp_modifier_integrator
  USE INTERFACE_PSIMU, only: PS_STR_INTEGRATION, PS_STR_PROPULSION, PS_STR_ATTITUDE, &
       PS_STR_SEPARATIONS, PS_STR_MODELES, PS_STR_CARACTERISTIQUES, &
       ps_nloi_att, ps_npts_att, ps_nloi_propu, ps_npts_propu, ps_nsepa
  USE PS_CALCUL_FORCES, only : PSI_METHODE_EPH_TCHEMAD
  common /unites/ gsun
  integer :: gsun


! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ps_utilitaires.F90 368 2013-02-19 14:43:59Z aadt $'

CONTAINS

!+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
   type(MSP_SCENARIO_LOI) function ps_creer_scenario_file(type, date_ref, file, &
        nomdom, section, type_section) result (scenario)
!+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_creer_scenario_file
!
!$Resume
!  Routine de création d'un scenario de lois (encapsule MSP_creer_scenario)
!
!$Description
!  Routine de création d'un scenario de lois (encapsule MSP_creer_scenario)
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  scenario = ps_creer_scenario_file(type, date_ref,file, &
!.            nomdom, [section], [type_section])
!.    integer :: type
!.    real(KIND=PM_REEL) :: date_ref
!.    character(LEN=*) :: nomdom
!.    character(LEN=*) :: file
!.    character(LEN=*), dimension(:) :: section
!.    integer, dimension(:) :: type_section
!
!$Arguments
!>E     type          :<integer>           Type de scenario:
!.                                                 MSP_ENUM_PROPULSION => scenario de lois de propulsion
!.                                                 MSP_ENUM_ATTITUDE   => scenario de lois d'attitude
!.                                                 MSP_ENUM_SEPARATION => scenario de lois de séparation
!>E     date_ref      :<PM_REEL>           Date de référence
!>E     file          :<LEN=*>             Fichier contenant la description des lois au format MADONA
!>E     nomdom        :<LEN=*>             Nom du domaine de langage utilisé
!>[E]   section       :<LEN=*,DIM=(:)>     Tableau des sections MADONA à connecter avant d'accéder aux données
!>[E]   type_section  :<integer,DIM=(:)>   Tableau des types de sections MADONA à connecter avant d'accéder aux données
!>S     scenario      :<MSP_SCENARIO_LOI>  Scénario créé
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- MSP_annuler_probleme
!- ps_charger_fichier_propu
!- ps_charger_fichier_attit
!- ps_charger_fichier_separ
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

      ! Arguments obligatoires
      integer, intent(IN)             :: type
      real(KIND=PM_REEL), intent(IN) :: date_ref
      character(LEN=*), intent(IN) :: file
      character(LEN=*), intent(IN)    :: nomdom

      character(LEN=*), dimension(:), intent(IN), optional :: section
      integer, dimension(:), intent(IN), optional :: type_section

      ! Initialisations
      SELECT CASE(type)
         
      CASE(MSP_ENUM_PROPULSION)
         
         call ps_charger_fichier_propu(file, scenario, date_ref, nomdom, &
              section=section, type_section=type_section)
         if ( MSP_gen_messages("ps_creer_scenario_file") ) return 

      CASE(MSP_ENUM_ATTITUDE)

         call ps_charger_fichier_attit(file, scenario, date_ref, nomdom, &
              section=section, type_section=type_section)
         if ( MSP_gen_messages("ps_creer_scenario_file") ) return 
         
         
      CASE(MSP_ENUM_SEPARATION)

         call ps_charger_fichier_separ(file, scenario, date_ref, nomdom, &
              section=section, type_section=type_section)
         if ( MSP_gen_messages("ps_creer_scenario_file") ) return 
         
      end SELECT

    end function ps_creer_scenario_file


!+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
    SUBROUTINE ps_init_scenario_propu(nb_pro, &
          deltav, merg_pro, ntab_pro, isp, pas, dates_pro, dirref, omega, omegap, poussee, &
          numpla, echd, param, type_pro, typdatimp, typrep_pro, iorb, echt, obli, &
          polu, polv, requa_r, apla_r, scenario, jjsecpro)
!+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_init_scenario_propu
!
!$Resume
!  Routine initialisant un scenario de lois de propulsions à partir des tableaux de 
!  valeurs.
!
!$Description
!  Routine initialisant un scenario de lois de propulsions à partir des tableaux de 
!  valeurs.
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_init_scenario_propu(nb_pro, &
!.              deltav, merg_pro, ntab_pro, isp, pas, dates_pro, dirref, omega, omegap, poussee, &
!.              numpla, echd, param, type_pro, typdatimp, typrep_pro, iorb, echt, obli, &
!               polu, polv, requa_r, apla_r, scenario, jjsecpro)
!.    integer :: nb_pro
!.    integer, dimension(:) :: type_pro, iorb, ntab_pro
!.    integer, dimension(:) :: dirref, numpla, echd, echt
!.    real(KIND=PM_REEL), dimension(:) :: deltav, merg_pro, isp, pas
!.    real(KIND=PM_REEL), dimension(:, :) :: typrep_pro, dates_pro, omega
!.    real(KIND=PM_REEL), dimension(:, :) :: omegap, poussee, param
!.    real(KIND=PM_REEL), dimension(:) :: obli, polu, polv, requa_r, apla_r
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     nb_pro      :<integer>              
!>E     deltav      :<PM_REEL,DIM=(:)>      
!>E     merg_pro    :<PM_REEL,DIM=(:)>      
!>E     ntab_pro    :<integer,DIM=(:)>      
!>E     isp         :<PM_REEL,DIM=(:)>      
!>E     pas         :<PM_REEL,DIM=(:)>      
!>E     dates_pro   :<PM_REEL,DIM=(:, :)>   
!>E     dirref      :<integer,DIM=(:)>   
!>E     omega       :<PM_REEL,DIM=(:, :)>   
!>E     omegap      :<PM_REEL,DIM=(:, :)>   
!>E     poussee     :<PM_REEL,DIM=(:, :)>   
!>E     numpla      :<integer,DIM=(:)>      
!>E     echd        :<integer,DIM=(:)>      
!>E     param       :<PM_REEL,DIM=(:, :)>   
!>E     type_pro    :<integer,DIM=(:)>      
!>E     typrep_pro  :<PM_REEL,DIM=(:, :)>   
!>E     iorb        :<integer,DIM=(:)>      
!>E     echt        :<integer,DIM=(:)>      
!>E     obli        :<PM_REEL,DIM=(:)>      
!>E     polu        :<PM_REEL,DIM=(:)>      
!>E     polv        :<PM_REEL,DIM=(:)>      
!>E     requa_r     :<PM_REEL,DIM=(:)>      
!>E     apla_r      :<PM_REEL,DIM=(:)>      
!>S     scenario    :<MSP_SCENARIO_LOI>     
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- MSP_annuler_probleme
!- MSP_ajouter_loi
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

      integer, intent(IN)                             :: nb_pro
      integer, dimension(:), intent(IN)               :: type_pro, iorb, ntab_pro,typdatimp
      integer, dimension(:), intent(IN)               :: dirref, numpla, echd, echt
      real(KIND=PM_REEL), dimension(:), intent(IN)    :: deltav, merg_pro, isp, pas
      real(KIND=PM_REEL), dimension(:, :), intent(IN) :: typrep_pro, dates_pro, omega
      real(KIND=PM_REEL), dimension(:, :), intent(IN) :: omegap,  poussee, param
      real(KIND=PM_REEL), dimension(:), intent(IN)    :: obli, polu, polv, requa_r, apla_r
      type(tm_jour_sec), dimension(:, :), intent(IN), optional :: jjsecpro

      type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario

      integer :: i
      type(MSP_IMPULSION) :: loi_imp
      type(MSP_POUSSEE_CONTINUE) :: loi_cont
      type(MSP_POUSSEE_BULLETIN) :: loi_bull
      type(MSP_BULLETIN) :: bul
      character(LEN=80) :: titre
      character(LEN=2) :: car

      
      do i = 1, nb_pro
         ! Variables en cas d'erreur
         write(car, '(I2)') i

         write(titre, '(''Loi No '',I2,'' :'')') i

         SELECT CASE (type_pro(i))

         CASE(MSP_ENUM_LOI_IMP) ! Impulsion

            loi_imp = MSP_creer_impulsion (typdatimp(i), dates_pro(i, 1), &
                 deltav(i), dirref(i), omega(i, 1), omegap(i, 1), merg_pro(i), &
                 date_js=jjsecpro(i,1))
            if ( MSP_gen_messages("ps_init_scenario_propu") ) return 

            call MSP_ajouter_loi(scenario, TRIM(titre)//"impulsion", loi_imp)
            if ( MSP_gen_messages("ps_init_scenario_propu") ) return 

         CASE(MSP_ENUM_LOI_CONT) ! Continue

            loi_cont = MSP_creer_poussee_continue (ntab_pro(i), typdatimp(i), &
                 dates_pro(i, 1:ntab_pro(i)), poussee(i, 1:ntab_pro(i)), isp(i), &
                 dirref(i), omega(i, 1:ntab_pro(i)), omegap(i, 1:ntab_pro(i)), &
                 merg_pro(i), pas(i), dates_js=jjsecpro(i,1:ntab_pro(i)))
            if ( MSP_gen_messages("ps_init_scenario_propu") ) return 

            call MSP_ajouter_loi(scenario, TRIM(titre)//"continue", loi_cont)
            if ( MSP_gen_messages("ps_init_scenario_propu") ) return 

         CASE(MSP_ENUM_LOI_BUL) ! Bulletin

            bul = MSP_creer_bulletin (dates_pro(i, 1), iorb=iorb(i), &
                 param=param(i, :), typrep=int(typrep_pro(i, 1)), &
                 planete=numpla(i), cle_date=echd(i), ech_temps_rep=echt(i), &
                 requa_r=requa_r(i), apla_r=apla_r(i), pole_u=polu(i), pole_v=polv(i), &
                 obli=obli(i), datbul_js=jjsecpro(i,1))
            if ( MSP_gen_messages("ps_init_scenario_propu") ) return 

            loi_bull = MSP_creer_poussee_bulletin(typdatimp(i), dates_pro(i, 1), bul, merg_pro(i))
            call MSP_ajouter_loi(scenario, TRIM(titre)//"bulletin", loi_bull)
            if ( MSP_gen_messages("ps_init_scenario_propu") ) return 

         END SELECT

      end do

    end SUBROUTINE ps_init_scenario_propu




!+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
    SUBROUTINE ps_init_scenario_attit(nb_att, &
         psip, tetap, phip, typdatatt,typdatvrot,dates_js, dates_att, &
         psi0, teta0, phi0, type_att, typrep_att, ntab, scenario)
!+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_init_scenario_attit
!
!$Resume
!  Routine initialisant un scenario de lois d'attitude à partir des tableaux de 
!  valeurs.
!
!$Description
!  Routine initialisant un scenario de lois d'attitude à partir des tableaux de 
!  valeurs.
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_init_scenario_attit(nb_att, &
!.             psip, tetap, phip, dates_att, psi0, teta0, phi0, &
!.             type_att, typrep_att, ntab, scenario)
!.    integer :: nb_att
!.    integer, dimension(:) :: type_att, typrep_att, ntab
!.    real(KIND=PM_REEL), dimension(:) :: psip, tetap, phip
!.    real(KIND=PM_REEL), dimension(:,:) :: dates_att, psi0, teta0, phi0
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     nb_att      :<integer>             
!>E     psip        :<PM_REEL,DIM=(:)>     
!>E     tetap       :<PM_REEL,DIM=(:)>     
!>E     phip        :<PM_REEL,DIM=(:)>     
!>E     dates_att   :<PM_REEL,DIM=(:,:)>   
!>E     psi0        :<PM_REEL,DIM=(:,:)>   
!>E     teta0       :<PM_REEL,DIM=(:,:)>   
!>E     phi0        :<PM_REEL,DIM=(:,:)>   
!>E     type_att    :<integer,DIM=(:)>     
!>E     typrep_att  :<integer,DIM=(:)>     
!>E     ntab        :<integer,DIM=(:)>     
!>S     scenario    :<MSP_SCENARIO_LOI>    
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- MSP_annuler_probleme
!- MSP_ajouter_loi
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

      integer, intent(IN)                            :: nb_att
      integer, dimension(:), intent(IN)              :: type_att, typrep_att, ntab
      real(KIND=PM_REEL), dimension(:), intent(IN)   :: psip, tetap, phip
      real(KIND=PM_REEL), dimension(:,:), intent(IN) :: dates_att, psi0, teta0, phi0
      integer, dimension(:), intent(IN)              :: typdatatt, typdatvrot
      type(tm_jour_sec), dimension(:,:), intent(IN)  :: dates_js

      type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario

      integer :: i
      type(MSP_ATTITUDE_TABULEE) :: loi_tab
      type(MSP_ATTITUDE_SPINNEE) :: loi_spin
      character(LEN=80) :: titre
      character(LEN=2) :: car

      do i = 1, nb_att

! Variables en cas d'erreur
         write(car, '(I2)') i

         write(titre, '(''Loi No '',I2,'' :'')') i

         SELECT CASE (type_att(i))

         CASE(1) ! Loi tabulee

            loi_tab = MSP_creer_attitude_tabulee(ntab(i), typdatatt(i), dates_att(i, 1:ntab(i)), &
                 typrep_att(i), psi0(i, 1:ntab(i)), teta0(i, 1:ntab(i)), phi0(i, 1:ntab(i)), &
                 dates_js=dates_js(i,1:ntab(i)))
            if ( MSP_gen_messages("ps_init_scenario_attit ") ) return 

            call MSP_ajouter_loi(scenario, TRIM(titre)//"Tabulée", loi_tab)
            if ( MSP_gen_messages("ps_init_scenario_attit") ) return 

        CASE(2) ! Loi Spinnée

           loi_spin = MSP_creer_attitude_spinnee(typdatvrot(i), dates_att(i,1), dates_att(i,2), &
                typrep_att(i), psi0(i, 1), teta0(i, 1), phi0(i, 1), psip(i), tetap(i), phip(i),&
                datedeb_js=dates_js(i,1), datefin_js=dates_js(i,2))
           if ( MSP_gen_messages("ps_init_scenario_attit") ) return 

           call MSP_ajouter_loi(scenario, TRIM(titre)//"Spinnée", loi_spin)
           if ( MSP_gen_messages("ps_init_scenario_attit") ) return 

        END SELECT

     end do

   end SUBROUTINE ps_init_scenario_attit




!+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
   SUBROUTINE ps_init_scenario_separ(nb_sep, typdat, jjsep, secsep, &
        dates_sep, deltav_sep, omega_sep, omegap_sep, forme_sep, sx, sy, sz, st, spx, spy, spz, &
        tymasse_sep, merg_sep, typcf,scenario)
!+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_init_scenario_separ
!
!$Resume
!  Routine initialisant un scenario de lois de séparation à partir des tableaux de 
!  valeurs.
!
!$Description
!  Routine initialisant un scenario de lois de séparation à partir des tableaux de 
!  valeurs.
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_init_scenario_separ(nb_sep, &
!.            dates_sep, deltav_sep, omega_sep, omegap_sep, forme_sep, sx, sy, sz, st, spx, spy, spz, merg_sep, &
!.            scenario)
!.    integer :: nb_sep
!.    integer, dimension(:) :: forme_sep
!.    real(KIND=PM_REEL), dimension(:) :: dates_sep, sx, sy, sz, st, spx, spy, spz
!.    real(KIND=PM_REEL), dimension(:) :: merg_sep
!.    real(KIND=PM_REEL), dimension(:) :: deltav_sep, omega_sep, omegap_sep
!.    type(MSP_SCENARIO_LOI) :: scenario
!
!$Arguments
!>E     nb_sep      :<integer>            
!>E     dates_sep   :<PM_REEL,DIM=(:)>    
!>E     deltav_sep  :<PM_REEL,DIM=(:)>    
!>E     omega_sep   :<PM_REEL,DIM=(:)>    
!>E     omegap_sep  :<PM_REEL,DIM=(:)>    
!>E     forme_sep   :<integer,DIM=(:)>    
!>E     sx          :<PM_REEL,DIM=(:)>    
!>E     sy          :<PM_REEL,DIM=(:)>    
!>E     sz          :<PM_REEL,DIM=(:)>    
!>E     st          :<PM_REEL,DIM=(:)>    
!>E     spx         :<PM_REEL,DIM=(:)>    
!>E     spy         :<PM_REEL,DIM=(:)>    
!>E     spz         :<PM_REEL,DIM=(:)>    
!>E     merg_sep    :<PM_REEL,DIM=(:)>    
!>S     scenario    :<MSP_SCENARIO_LOI>   
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- MSP_annuler_probleme
!- MSP_ajouter_loi
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

     integer, intent(IN)                          :: nb_sep
     integer, dimension(:), intent(IN)            ::  forme_sep, tymasse_sep
     real(KIND=PM_REEL), dimension(:), intent(IN) :: dates_sep, secsep
     real(KIND=PM_REEL), dimension(:), intent(IN) :: merg_sep, sx, sy, sz, st, spx, spy, spz
     real(KIND=PM_REEL), dimension(:), intent(IN) :: deltav_sep, omega_sep, omegap_sep
     integer, dimension(:), intent(IN)            :: typdat, jjsep, typcf
     type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario

     type(MSP_SEPARATION) :: loi_sep

     integer :: i
     character(LEN=80) :: titre
     character(LEN=2) :: car
     type(tm_jour_sec) :: date_js


    do i = 1, nb_sep

       ! Variables en cas d'erreur
       write(car, '(I2)') i

       write(titre, '(''Loi No '',I2,'' :'')') i
       date_js%jour=jjsep(i)
       date_js%sec=secsep(i)

       loi_sep = MSP_creer_separation (typdat(i), dates_sep(i), &
            deltav_sep(i), omega_sep(i), omegap_sep(i), &
            forme_sep(i), sx(i), sy(i), sz(i), st(i), spx(i), spy(i), spz(i), &
            tymasse_sep(i),typcf(i), merg_sep(i), date_js=date_js)
       if ( MSP_gen_messages("ps_init_scenario_separ") ) return 

       call MSP_ajouter_loi(scenario, TRIM(titre)//"Séparation", loi_sep)
       if ( MSP_gen_messages("ps_init_scenario_separ") ) return 

    end do


   end SUBROUTINE ps_init_scenario_separ


!+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
   SUBROUTINE ps_charger_fichier_propu(file, scenario, date_ref, nomdom, section, type_section)
!+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_charger_fichier_propu
!
!$Resume
!  Création d'une structure scenario de propulsion à partir d'un fichier MADONA
!
!$Description
!  Création d'une structure scenario de propulsion à partir d'un fichier MADONA
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_charger_fichier_propu(file, scenario, date_ref, nomdom, [section], [type_section])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    real(KIND=PM_REEL) :: date_ref
!.    character(LEN=*) :: file
!.    character(LEN=*) :: nomdom
!.    character(LEN=*), dimension(:) :: section
!.    integer, dimension(:) :: type_section
!
!$Arguments
!>E     file          :<LEN=*>              Fichier MADONA contenant les lois de propulsion
!>S     scenario      :<MSP_SCENARIO_LOI>   Scenario de propulsion
!>E     date_ref      :<PM_REEL>            Date de référence du scenario créé
!>E     nomdom        :<LEN=*>              Nom du domaine de langage
!>[E]   section       :<LEN=*,DIM=(:)>      Tableau des sections MADONA à sélectionner pour atteindre 
!                                           la structure MADONA contenant les lois de propulsion
!>[E]   type_section  :<integer,DIM=(:)>    Type de ces sections
!
!$Common
!
!$Routines
!- gsrwnotauto
!- MSP_signaler_message
!- MSP_annuler_probleme
!- gssetaccin
!- read_GS_PS_PROPULSION
!- ps_init_scenario_propu
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

     type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
     real(KIND=PM_REEL), intent(IN) :: date_ref
     character(LEN=*), intent(IN) :: file
     character(LEN=*), intent(IN) :: nomdom
     character(LEN=*), dimension(:), intent(IN), optional :: section
     integer, dimension(:), intent(IN), optional :: type_section

     real (KIND=pm_reel) :: timp(ps_nloi_propu,ps_npts_propu),sectimp(ps_nloi_propu,ps_npts_propu)
     real (KIND=pm_reel) :: deltav(ps_nloi_propu)
     real (KIND=pm_reel) :: omtab(ps_nloi_propu,ps_npts_propu)
     real (KIND=pm_reel) :: omptab(ps_nloi_propu,ps_npts_propu) 
     real (KIND=pm_reel) :: fptab(ps_nloi_propu,ps_npts_propu)
     real (KIND=pm_reel) :: xisp(ps_nloi_propu)
     real (KIND=pm_reel) :: paspro(ps_nloi_propu)
     real (KIND=pm_reel) :: xmerg(ps_nloi_propu)
     real (KIND=pm_reel) :: parman(ps_nloi_propu,6)
     real (KIND=pm_reel) :: brep(ps_nloi_propu,10)
     real (KIND=pm_reel) :: obli(ps_nloi_propu),polu(ps_nloi_propu),polv(ps_nloi_propu)
     real (KIND=pm_reel) :: requa_r(ps_nloi_propu),apla_r(ps_nloi_propu)

     integer :: dirref(ps_nloi_propu), iprop_tmp(ps_nloi_propu)
     integer :: jjtimp(ps_nloi_propu,ps_npts_propu)
     integer :: bibul(ps_nloi_propu)
     integer :: bnumpla(ps_nloi_propu)
     integer :: bech_temps(ps_nloi_propu)
     integer :: bech_date(ps_nloi_propu)
     integer :: npo
     integer :: iprop(ps_nloi_propu)
     integer :: ntab(ps_nloi_propu), typdatimp(ps_nloi_propu), typdatcont(ps_nloi_propu)
     type(tm_jour_sec) :: jjsectimp(ps_nloi_propu,ps_npts_propu)

     integer :: ier, iloi
     integer :: lentab, nacc, kpro
     real(KIND=PM_REEL) :: vitrot
     integer :: jjref
     real(KIND=PM_REEL) :: secref
     character(LEN=256):: cdirfic
     character(LEN=80), dimension(ps_nloi_propu) ::  cficpro
     real (KIND=pm_reel), dimension(ps_nloi_propu) :: requa ,apla, gmu
    character(LEN=256) :: rctmp
    character(len=256) :: ficlistecorps, repficcorps
    integer :: longueur

     ier = AMv_rc_get ('data_psimu','psimu','','data_psimu',rctmp,longueur)
     cdirfic = rctmp(1:longueur)

     call gsrwnotauto()

     ier = MSP_acc_charger_donnees(file, nacc, "r")
     if ( MSP_gen_messages("ps_charger_fichier_propu") ) return 

     call gssetaccin(nacc)

     ! Connexion AMIGAU au fichier choisi
     if (PRESENT(section).and.PRESENT(type_section)) then 

        lentab = size(section)
        ier = MSP_acc_select(nacc, section, type_section, lentab)
        if ( MSP_gen_messages("ps_charger_fichier_propu") ) return 

     else if ( (PRESENT(section).and..not.PRESENT(type_section)) .or. &
          (.not.PRESENT(section).and.PRESENT(type_section)) ) then 

           call MSP_signaler_message (cle_mes="MSP_erreur_arguments_001", &
                routine="ps_charger_fichier_propu", type=MSP_ENUM_ERREUR, &
                partie_variable="section et type_section")
           return

     end if

     ! Dechargement du fichier unite puisque recharge dans read_GS_PS_PROPULSION
     if (gsun == 0) ier = AMv_unit_unload()

     jjref = int(date_ref)
     secref = (date_ref - jjref) * 86400
     iprop_tmp(:) = 0
 
     call read_GS_PS_PROPULSION (jjref, secref, kpro, &
          npo, iprop_tmp, ntab, typdatimp,typdatcont, jjtimp,sectimp, timp, &
          deltav, omtab, omptab, fptab, &
          xisp, paspro, xmerg, bnumpla, vitrot, bibul, bnumpla, &
          bech_date, brep, parman, bech_temps, &
          obli, polu, polv, requa_r, apla_r, requa,apla,gmu,nomdom, ps_nloi_propu, &
          cdirfic,cficpro,repficcorps,ficlistecorps)

     do iloi = 1 , npo
        dirref(iloi) = iprop_tmp(iloi)/100
        iprop(iloi)  = iprop_tmp(iloi) - dirref(iloi)*100
        dirref(iloi) = dirref(iloi) - 1
        if (iprop(iloi).eq.2) typdatimp(iloi) = typdatcont(iloi)
        jjsectimp(iloi, :)%jour = jjtimp(iloi, :)
        jjsectimp(iloi, :)%sec  = sectimp(iloi, :)
     enddo

     ! Initialisation des structures
     call MSP_effacer_scenario(scenario,nul=.true.)

     scenario = MSP_creer_scenario(MSP_ENUM_PROPULSION, date_ref, trim(file))
     
     call ps_init_scenario_propu(npo, deltav, xmerg, ntab, &
          xisp, paspro, timp, dirref, omtab, omptab, &
          fptab, bnumpla, bech_date, parman, iprop, typdatimp, &
          brep, bibul, bech_temps, obli, polu, polv, requa_r, apla_r, scenario, &
          jjsecpro=jjsectimp)
     if ( MSP_gen_messages("ps_charger_fichier_propu") ) return 

     if (nacc /= 0) then 
        ier = acc_deconnect (nacc, ACC_R)
        if (ier < 0) then 
           call MSP_signaler_message (cle_mes="MSP_probleme_close", &
             routine="ps_charger_fichier_propu", type=MSP_ENUM_ERREUR)
           return
        end if
        ier = acc_close(nacc)
        if (ier < 0) then 
           call MSP_signaler_message (cle_mes="MSP_probleme_close", &
             routine="ps_charger_fichier_propu", type=MSP_ENUM_ERREUR)
           return
        end if
     end if

   end SUBROUTINE ps_charger_fichier_propu



!+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
   SUBROUTINE ps_charger_fichier_attit(file, scenario, date_ref, nomdom, section, type_section)
!+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_charger_fichier_attit
!
!$Resume
!  Création d'une structure scenario d'attitude à partir d'un fichier MADONA
!
!$Description
!  Création d'une structure scenario d'attitude à partir d'un fichier MADONA
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_charger_fichier_attit(file, scenario, date_ref, nomdom, [section], [type_section])
!.    character(LEN=*) :: file
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    real(KIND=PM_REEL) :: date_ref
!.    character(LEN=*) :: nomdom
!.    character(LEN=*), dimension(:) :: section
!.    integer, dimension(:) :: type_section
!
!$Arguments
!>E     file          :<LEN=*>              Fichier MADONA contenant les lois d'attitude
!>S     scenario      :<MSP_SCENARIO_LOI>   Scenario d'attitude
!>E     date_ref      :<PM_REEL>            Date de référence du scenario créé
!>E     nomdom        :<LEN=*>              Nom du domaine de traduction
!>[E]   section       :<LEN=*,DIM=(:)>      Tableau des sections MADONA à sélectionner pour atteindre 
!                                           la structure MADONA contenant les lois d'attitude
!>[E]   type_section  :<integer,DIM=(:)>    Type des sections
!
!$Common
!
!$Routines
!- gsrwnotauto
!- MSP_signaler_message
!- MSP_annuler_probleme
!- gssetaccin
!- read_GS_PS_ATTITUDE
!- ps_init_scenario_attit
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

     character(LEN=*), intent(IN) :: file
     real(KIND=PM_REEL), intent(IN) :: date_ref
     type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
     character(LEN=*), intent(IN) :: nomdom
     character(LEN=*), dimension(:), intent(IN), optional :: section
     integer, dimension(:), intent(IN), optional :: type_section

     integer :: ier
     integer :: lentab, nacc, kati, iloi
     
     real (KIND=pm_reel) :: datt(ps_nloi_att,ps_npts_att)
     real (KIND=pm_reel) :: secatt(ps_nloi_att,ps_npts_att)
     real (KIND=pm_reel) :: ang1(ps_nloi_att,ps_npts_att)
     real (KIND=pm_reel) :: ang2(ps_nloi_att,ps_npts_att)
     real (KIND=pm_reel) :: ang3(ps_nloi_att,ps_npts_att)
     real (KIND=pm_reel) :: angp1(ps_nloi_att)
     real (KIND=pm_reel) :: angp2(ps_nloi_att)
     real (KIND=pm_reel) :: angp3(ps_nloi_att)
     integer             :: jjatt(ps_nloi_att,ps_npts_att)
     integer             :: itypdatatt(ps_nloi_att)
     integer             :: itypdatvrot(ps_nloi_att)

     integer             :: natt
     integer             :: iangle(ps_nloi_att)
     integer             :: itypa(ps_nloi_att)
     integer             :: irepa(ps_nloi_att)
     integer             :: npatt(ps_nloi_att)
     type(tm_jour_sec)   :: dates_js(ps_nloi_att,ps_npts_att)
     character(LEN=256):: cdirfic
     character(LEN=80), dimension(ps_nloi_propu) ::  cficatt
     character(LEN=256) :: rctmp
     integer :: longueur

     ier = AMv_rc_get ('data_psimu','psimu','','data_psimu',rctmp,longueur)
     cdirfic = rctmp(1:longueur)

     call gsrwnotauto()

     ier = MSP_acc_charger_donnees(file, nacc, "r")
     if ( MSP_gen_messages("ps_charger_fichier_attit") ) return 

     call gssetaccin(nacc)

     ! Connexion AMIGAU au fichier choisi
     if (PRESENT(section).and.PRESENT(type_section)) then 

        lentab = size(section)
        ier = MSP_acc_select(nacc, section, type_section, lentab)
        if ( MSP_gen_messages("ps_charger_fichier_attit") ) return 

     else if ( (PRESENT(section).and..not.PRESENT(type_section)) .or. &
          (.not.PRESENT(section).and.PRESENT(type_section)) ) then 

           call MSP_signaler_message (cle_mes="MSP_erreur_arguments_001", &
                routine="ps_charger_fichier_attit", type=MSP_ENUM_ERREUR, &
                partie_variable="section et type_section")
           return

     end if

     ! Dechargement du fichier unite puisque recharge dans read_GS_PS_ATTITUDE
     if (gsun == 0) ier = AMv_unit_unload()

     call read_GS_PS_ATTITUDE (kati, natt, itypa, iangle, irepa, npatt, &
          itypdatatt,itypdatvrot,jjatt,secatt, datt, &
          ang1, ang2, ang3, angp1, angp2, angp3, nomdom, ps_nloi_att, cdirfic,cficatt)

     do iloi = 1, natt
        dates_js(iloi,1:ps_npts_att)%jour=jjatt(iloi,1:ps_npts_att)
        dates_js(iloi,1:ps_npts_att)%sec=secatt(iloi,1:ps_npts_att)
     enddo

     ! Initialisation des structures
     call MSP_effacer_scenario(scenario,nul=.true.)

     scenario = MSP_creer_scenario(MSP_ENUM_ATTITUDE, date_ref, trim(file))
     call ps_init_scenario_attit(natt, angp1, angp2, angp3, &
          itypdatatt,itypdatvrot,dates_js, datt, ang1, ang2, ang3, &
          itypa, irepa, npatt, scenario)
     if ( MSP_gen_messages("ps_charger_fichier_attit") ) return 

     ier = acc_close(nacc)
     if (ier < 0) then 
        call MSP_signaler_message (cle_mes="MSP_probleme_close", &
             routine="ps_charger_fichier_attit", type=MSP_ENUM_ERREUR)
        return
     end if

   end SUBROUTINE ps_charger_fichier_attit


!+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
   SUBROUTINE ps_charger_fichier_separ(file, scenario, date_ref, nomdom, section, type_section)
!+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_charger_fichier_separ
!
!$Resume
!  Création d'une structure scenario de séparation à partir d'un fichier MADONA
!
!$Description
!  Création d'une structure scenario de séparation à partir d'un fichier MADONA
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_charger_fichier_separ(file, scenario, date_ref, nomdom, [section], [type_section])
!.    type(MSP_SCENARIO_LOI) :: scenario
!.    real(KIND=PM_REEL) :: date_ref
!.    character(LEN=*) :: file
!.    character(LEN=*) :: nomdom
!.    character(LEN=*), dimension(:) :: section
!.    integer, dimension(:) :: type_section
!
!$Arguments
!>E     file          :<LEN=*>              Fichier MADONA contenant les lois de séparation
!>S     scenario      :<MSP_SCENARIO_LOI>   Scenario de séparation
!>E     date_ref      :<PM_REEL>            Date de référence du scenario créé
!>E     nomdom        :<LEN=*>              Nom du domaine de traduction
!>[E]   section       :<LEN=*,DIM=(:)>      Tableau des sections MADONA à sélectionner pour atteindre 
!                                           la structure MADONA contenant les lois de séparation
!>[E]   type_section  :<integer,DIM=(:)>    Type des sections
!
!$Common
!
!$Routines
!- gsrwnotauto
!- MSP_signaler_message
!- MSP_annuler_probleme
!- gssetaccin
!- read_GS_PS_SEPARATIONS
!- ps_init_scenario_separ
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

     type(MSP_SCENARIO_LOI), intent(INOUT) :: scenario
     real(KIND=PM_REEL), intent(IN) :: date_ref

     character(LEN=*), intent(IN) :: file
     character(LEN=*), intent(IN) :: nomdom
     character(LEN=*), dimension(:), intent(IN), optional :: section
     integer, dimension(:), intent(IN), optional :: type_section

     ! Variables locales
     ! déclaration avec ps_nsepa et non dimension 2
     integer :: ier, lentab, nacc
     integer :: ksep, nsep
     integer, dimension(ps_nsepa) :: typdat
     integer, dimension(ps_nsepa) :: jjsep
     integer, dimension(ps_nsepa) :: forme, choixmasse, typcf, typcf_alt

     real (KIND=pm_reel), dimension(ps_nsepa) :: tsep, secsep
     real (KIND=pm_reel), dimension(ps_nsepa) :: dvsep, omsep, ompsep
     real (KIND=pm_reel), dimension(ps_nsepa) :: sx, sy, sz,st,spx, spy, spz
     real (KIND=pm_reel), dimension(ps_nsepa) :: xma, cp
     real (KIND=pm_reel), dimension(ps_nsepa) :: ka,ks, kd, cf, cd, cl

     character(LEN=80), dimension(ps_nsepa) ::  ficaero
     character(LEN=256):: cdiraero
     character(LEN=256) :: rctmp
     integer :: longueur
     integer :: i

     ier = AMv_rc_get ('data_psimu','psimu','','data_psimu',rctmp,longueur)
     cdiraero = rctmp(1:longueur)

     call gsrwnotauto()

     ier = MSP_acc_charger_donnees(file, nacc, "r")
     if ( MSP_gen_messages("ps_charger_fichier_separ") ) return 

     call gssetaccin(nacc)

     ! Connexion AMIGAU au fichier choisi
     if (PRESENT(section).and.PRESENT(type_section)) then 

        lentab = size(section)
        ier = MSP_acc_select(nacc, section, type_section, lentab)
        if ( MSP_gen_messages("ps_charger_fichier_separ") ) return 

     else if ( (PRESENT(section).and..not.PRESENT(type_section)) .or. &
          (.not.PRESENT(section).and.PRESENT(type_section)) ) then 

        call MSP_signaler_message (cle_mes="MSP_erreur_arguments_001", &
             routine="ps_charger_fichier_separ", type=MSP_ENUM_ERREUR, &
             partie_variable="section et type_section")
        return

     end if

     ! Dechargement du fichier unite puisque recharge dans read_GS_PS_SEPARATIONS
     if (gsun == 0) ier = AMv_unit_unload()

     call read_GS_PS_SEPARATIONS (ksep, nsep, typdat, jjsep, secsep, tsep, &
          dvsep, omsep, ompsep, forme, sx, sy, sz, st, &
          spx, spy, spz, choixmasse, xma, cp, ka, ks, kd, &
          cf, typcf, typcf_alt, cd, cl, ficaero, cdiraero, nomdom)

     ! Conversion PSIMU -> MECASPA pour les types de coefficients de frottement (FA-ID 496)
     ! PSIMU : 1 -> coeff tabules (MSP_ENUM_CF_TABULE_ALT_MOY)
     ! PSIMU : 2 -> coefs constants, exprimes dans rep aero (MSP_ENUM_COEFF_AERO_VITESSE)
     ! PSIMU : 3 -> coefs en fonction incidence et mach, exprimes dans rep aero
     !                         (MSP_ENUM_COEFF_AERO_VITESSE)
     do i=1,nsep
        if(typcf(i) /= 1) then
           typcf(i) = MSP_ENUM_COEFF_AERO_VITESSE
        else
           if (typcf_alt(i) == 1) then
              typcf(i) = MSP_ENUM_COEFF_AERO_VITESSE
           else
              typcf(i) = MSP_ENUM_CF_TABULE_ALT_MOY
           end if
        end if
     end do
     
     ! Initialisation des structures
     call MSP_effacer_scenario(scenario,nul=.true.)

     scenario = MSP_creer_scenario(MSP_ENUM_SEPARATION, date_ref, trim(file))
     call ps_init_scenario_separ(nsep,  typdat, jjsep, secsep, &
          tsep, dvsep, omsep, ompsep, &
          forme, sx, sy, sz, st, spx, spy, spz, &
          choixmasse, xma,typcf,scenario) 
     if ( MSP_gen_messages("ps_charger_fichier_separ") ) return 

     ier = acc_close(nacc)
     if (ier < 0) then 
        call MSP_signaler_message (cle_mes="MSP_probleme_close", &
             routine="ps_charger_fichier_separ", type=MSP_ENUM_ERREUR)
        return
     end if

   end SUBROUTINE ps_charger_fichier_separ


!+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
  function ps_creer_vehicule_file (file, dir_aero, nomdom,  section, type_section, nom_vehicule) result (vehicule)
!+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_creer_vehicule_file
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
!  vehicule = ps_creer_vehicule_file (file, dir_aero, nomdom,  [section], [type_section], [nom_vehicule])
!.    character(LEN=*) :: file, dir_aero, nomdom
!.    integer, dimension(:) :: type_section
!.    character(LEN=*), dimension(:) :: section
!.    type(MSP_VEHICULE) :: vehicule
!.    character(LEN=*) :: nom_vehicule
!
!$Arguments
!>E     file          :<LEN=*>             
!>E     dir_aero      :<LEN=*>             
!>E     nomdom        :<LEN=*>             
!>[E]   section       :<LEN=*,DIM=(:)>     
!>[E]   type_section  :<integer,DIM=(:)>   
!>[E]   nom_vehicule  :<LEN=*>             
!>S     vehicule      :<MSP_VEHICULE>      
!
!$Common
!
!$Routines
!- gsrwnotauto
!- MSP_signaler_message
!- MSP_annuler_probleme
!- gssetaccin
!- read_GS_PS_CARACTERISTIQUES
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

    character(LEN=*), intent(IN) :: file, dir_aero, nomdom
    integer, dimension(:), intent(IN), optional :: type_section
    character(LEN=*), dimension(:), intent(IN), optional :: section
    type(MSP_VEHICULE) :: vehicule
    character(LEN=*), intent(IN), optional :: nom_vehicule

    ! Variables locales
    integer :: ier, kcar
    integer :: lentab, nacc
    type(PS_STR_CARACTERISTIQUES) :: str
    type(MSP_MCI)  :: mci
    type(MSP_AERO) :: aero
    type(MSP_prad) :: prad

    type(MSP_COEF) :: cxa,czn
    real (KIND=pm_reel), pointer , dimension(:) :: coef_1d
    real (KIND=pm_reel), pointer , dimension(:) :: par1_coef

    nullify(coef_1d)
    nullify(par1_coef)

    ! Lecture
    call gsrwnotauto()

    ier = MSP_acc_charger_donnees(file, nacc, "r")
    if (MSP_ERREUR) then 
       call MSP_signaler_message (cle_mes="MSP_PROPAGATION_PROBLEME", &
            routine="ps_creer_vehicule_file", type=MSP_ENUM_PROPAGATION)
       return
    end if

    call gssetaccin(nacc)

    ! Connexion AMIGAU au fichier choisi
    if (PRESENT(section).and.PRESENT(type_section)) then 

       lentab = size(section)
       ier = MSP_acc_select(nacc, section, type_section, lentab)
       if (MSP_PROBLEME) then 
          call MSP_signaler_message (cle_mes="MSP_PROPAGATION_PROBLEME", &
               routine="ps_creer_vehicule_file", type=MSP_ENUM_PROPAGATION)
          return
       end if

    else if ( (PRESENT(section).and..not.PRESENT(type_section)) .or. &
         (.not.PRESENT(section).and.PRESENT(type_section)) ) then 

       call MSP_signaler_message (cle_mes="MSP_erreur_arguments_001", &
            routine="ps_creer_vehicule_file", type=MSP_ENUM_ERREUR, &
            partie_variable="section et type_section")
       return

    end if

    ! Dechargement du fichier unite puisque recharge dans read_GS_PS_CARACTERISTIQUES
    if (gsun == 0) ier = AMv_unit_unload()

    call read_GS_PS_CARACTERISTIQUES(kcar, &
         str%forme, str%sx, str%sy, str%sz, str%st, str%spx, str%spy, str%spz, &
         str%xm, str%cp, str%ka, str%ks, str%kd, str%cf, str%typcf, str%typcf_alt,&
         str%cd(:,:), str%cl(:,:), str%txcd(:), str%tycd(:), str%txcl(:), str%tycl(:), &
         str%nxcd, str%nycd, str%nxcl, str%nycl, str%ficaero, &
         dir_aero, nomdom)
       

    ! Création des structures MECASPA
    mci = MSP_creer_mci(forme=str%forme, sx=str%sx, sy=str%sy, sz=str%sz, st=str%st, &
                        spx=str%spx, spy=str%spy, spz=str%spz, mstr=str%xm)
    prad = MSP_creer_prad(cmp=str%cp, ka=str%ka, ks=str%ks, kd=str%kd)


! DM 659 : 2 cas de coef tabulés en fonction de l'altitude suivant str%typcf_alt
! Par défaut ou  par fichier

    SELECT CASE (str%typcf)
    CASE (1)
       if (str%typcf_alt == 0) then

          aero = MSP_creer_aero(type_coef=MSP_ENUM_CF_TABULE_ALT_MOY,type_variation=MSP_ENUM_ALTITUDE,cmf=str%cf)
       else

          aero = MSP_creer_aero(type_coef=MSP_ENUM_COEFF_AERO_VITESSE,&
            type_variation=MSP_ENUM_ALTITUDE,ficaero=str%ficaero)

       end if
! Coef constants
    CASE (2)
       call MSP_effacer_coef(cxa,nul=.true.)
       call MSP_effacer_coef(czn,nul=.true.)
       call MSP_effacer_aero(aero,nul=.true.)

       allocate(coef_1d(1))
       allocate(par1_coef(1))
       par1_coef(1)= 0._pm_reel
       ! Creation de la structure MSP_COEF pour les coefficient de frottement
       coef_1d = str%cd(1,1)
       cxa = MSP_creer_coef(coef_1d=coef_1d,par1_coef=par1_coef)
       ! Creation de la structure MSP_COEF pour les coefficients de portance
       coef_1d = str%cl(1,1)
       czn = MSP_creer_coef(coef_1d=coef_1d,par1_coef=par1_coef)
       if ( ASSOCIATED (coef_1d) )  DEALLOCATE (coef_1d)
       if ( ASSOCIATED (par1_coef) )  DEALLOCATE (par1_coef)
       aero = MSP_creer_aero(type_coef=MSP_ENUM_COEFF_AERO_VITESSE,&
            type_variation=MSP_ENUM_CONSTANT,cmf=str%cf,cxa=cxa,czn=czn)

       call MSP_effacer_coef(cxa)
       call MSP_effacer_coef(czn)

! Fichier AERO incidence/mach

    CASE (3)
       aero = MSP_creer_aero(type_coef=MSP_ENUM_COEFF_AERO_VITESSE,&
            type_variation=MSP_ENUM_INCIDENCE_MACH,ficaero=str%ficaero, cmf=str%cf)

    END SELECT

    call MSP_effacer_vehicule (vehicule, nul=.true.)
    vehicule = MSP_creer_vehicule (mci=mci,aero=aero,prad=prad, nom_vehicule=nom_vehicule)
    if (MSP_PROBLEME) then 
          call MSP_signaler_message (cle_mes="MSP_PROPAGATION_PROBLEME", &
               routine="ps_creer_vehicule_file", type=MSP_ENUM_PROPAGATION)
    end if

    ier = acc_close(nacc)
    if (ier < 0) then 
       call MSP_signaler_message (cle_mes="MSP_probleme_close", &
            routine="ps_creer_vehicule_file", type=MSP_ENUM_ERREUR)
       return
    end if

    call MSP_effacer_aero(aero)

  end function ps_creer_vehicule_file


!+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
  type(MSP_MODELE) FUNCTION ps_creer_modele_file (file, nomdom, section, type_section, &
       planete, dirpot, dirept, nom_mod) result (modele)
!+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_creer_modele_file
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
!  modele = ps_creer_modele_file (file, nomdom, [section], [type_section], &
!.           [planete], [dirpot], [dirept], [diract],[nom_mod])
!.    character(LEN=*) :: file, nomdom
!.    integer, dimension(:) :: type_section
!.    character(LEN=*), dimension(:) :: section
!.    character(LEN=*) :: planete, dirpot, dirept, diract
!.    character(LEN=*) :: nom_mod
!
!$Arguments
!>E     file          :<LEN=*>             
!>E     nomdom        :<LEN=*>             
!>[E]   section       :<LEN=*,DIM=(:)>     
!>[E]   type_section  :<integer,DIM=(:)>   
!>[E]   planete       :<LEN=*>             
!>[E]   dirpot        :<LEN=*>
!>[E]   dirept        :<LEN=*>
!>[E]   nom_mod       :<LEN=*>             
!>S     modele        :<>                  
!
!$Common
!
!$Routines
!- gsrwnotauto
!- MSP_signaler_message
!- MSP_annuler_probleme
!- gssetaccin
!- read_GS_PS_MODELES
!
!$Include
!
!$Module
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use cps_acces

    implicit none
    
    ! Arguments
    !==========
    character(LEN=*), intent(IN) :: file, nomdom
    integer, dimension(:), intent(IN), optional :: type_section
    character(LEN=*), dimension(:), intent(IN), optional :: section
    character(LEN=*), intent(IN), optional ::  dirpot, dirept
    character(LEN=*), intent(IN), optional  :: nom_mod
    integer, intent(IN), optional :: planete
    

    ! Variables locales
    !==================
    integer :: ier
    integer :: lentab, nacc
    integer :: kmod, ipot, nmax, iept
    character(LEN=132) :: direph_w
    character(LEN=256) :: fic_pot,rep_atm_emcd
    integer :: trouve
    integer :: base_cps
    integer :: iplanet
    integer :: actsol
    logical :: loc = .false.
    type(MSP_ATM_EMCD) :: atm_emcd
    type(MSP_ATM_EXP) :: atm_exp
    

    type(PS_STR_MODELES) :: str

    call MSP_effacer_modele (modele, nul = .true.)

    call gsrwnotauto()

    ier = MSP_acc_charger_donnees(file, nacc, "r")
    if (MSP_PROBLEME) then 
          call MSP_signaler_message (cle_mes="MSP_PROPAGATION_PROBLEME", &
               routine="ps_creer_modele_file", type=MSP_ENUM_PROPAGATION)
          return
    end if

    call gssetaccin(nacc)

    ! Connexion AMIGAU au fichier choisi
    if (PRESENT(section).and.PRESENT(type_section)) then 

       lentab = size(section)
       ier = MSP_acc_select(nacc, section, type_section, lentab)
       if (MSP_PROBLEME) then 
          call MSP_signaler_message (cle_mes="MSP_PROPAGATION_PROBLEME", &
               routine="ps_creer_modele_file", type=MSP_ENUM_PROPAGATION)
          return
       end if

    else if ( (PRESENT(section).and..not.PRESENT(type_section)) .or. &
         (.not.PRESENT(section).and.PRESENT(type_section)) ) then 
       
       call MSP_signaler_message (cle_mes="MSP_erreur_arguments_001", &
            routine="PSM_creer_integration", type=MSP_ENUM_ERREUR, &
            partie_variable="section et type_section")
       return

    end if

    if (PRESENT(planete))then 
       iplanet=planete
    else
       iplanet=eph_terre
    end if

    if (PRESENT(dirpot))then 
       write (*,*) "Attention, PSIMU n'utilise plus de répertoire pour les fichiers potentiels"
       write (*,*) "Les modèles de potentiels sont récupérés dans COMPAS"
    end if

    if (PRESENT(dirept))then 
       direph_w=dirept
    else
    
       ! Récupération des informations "éphémérides" de la base locale
       ! Recherche du répertoire où sont stockés les fichiers Tchebytchev
       call eph_infogetLocal(PSI_METHODE_EPH_TCHEMAD, loc, repertoire=direph_w)
       if (.not.loc) then
       	 call eph_infoget(PSI_METHODE_EPH_TCHEMAD, repertoire=direph_w)
       end if
       if ( MSP_ERREUR ) then
 	 call MSP_signaler_message (cle_mes="PS_ERR_FIC_EPH", routine="ps_creer_modele_file")
	 return
       endif

    end if

    ! Dechargement du fichier unite puisque recharge dans read_GS_PS_MODELES
    if (gsun == 0) ier = AMv_unit_unload()

    base_cps = 1

! DM 643 : modèles de vent
    call read_GS_PS_MODELES (kmod, ipot, nmax, &
         str%modpot, str%nzo, str%nte, str%ikle(:), str%ficactsol, iept, str%ficept, &
         str%modatm, str%flu, str%app, str%deltat(:), str%param_atm_exp(:), &
         str%typemod, str%scena, str%typper(:), str%lambda_gw, str%modvent, &
         str%dateref_actsol, str%mode_avance_actsol, base_cps, iplanet, direph_w, nomdom)

    ! DM-ID 560 : recherche dans COMPAS du nom du fichier potentiel, d'après le nom du modèle

    trouve = cps_getFichierModele("potentiel", trim(str%modpot), fic_pot, .true.)
    if (trouve /= CPS_OK) then 
       call MSP_signaler_message (cle_mes="MSP_PROPAGATION_PROBLEME", &
            routine="ps_creer_modele_file", type=MSP_ENUM_PROPAGATION)
       return
    end if
    
    rep_atm_emcd=""
    
    if(str%modatm == MSP_MODATM_MARS_EMCD_31 .or. &
       str%modatm == MSP_MODATM_MARS_EMCD_42 .or. &
       str%modatm == MSP_MODATM_MARS_EMCD_43) then
       !/ Recherche du répertoire de données EMCD 3.1, 4.2 ou 4.3(nommé "MCD" dans GS_LIB) dans COMPAS 
       !/ Le ".true." permet de récupérer le chemin absolu 
       !/ ie : répertoire des modèles + ssrép du modèle EMCD 3.1, 4.2 ou 4.3
	  
       trouve =  cps_getFichierModele("atmosphere", str%modatm, rep_atm_emcd, .true.)
       if (trouve /= CPS_OK) then 
          call MSP_signaler_message (cle_mes="MSP_PROPAGATION_PROBLEME", &
               routine="ps_creer_modele_file", type=MSP_ENUM_PROPAGATION)
          return
       end if

    end if

! ### DM 643 : stockage de str%modvent à valider
    atm_emcd = MSP_creer_atm_emcd(mars_emcd_sce=str%scena,mars_emcd_per=str%typper,&
         dir_emcd=trim(rep_atm_emcd))
    atm_exp = MSP_creer_atm_exp(hscale = str%param_atm_exp(6), &
         h0 = str%param_atm_exp(2), &
         ro0 = str%param_atm_exp(1),&
         beta = str%param_atm_exp(7), &
         tscale = int(str%param_atm_exp(5)), &
         altmin = str%param_atm_exp(3), &
         altmax = str%param_atm_exp(4) )

    ! Conversion du type d'activité solaire (pour compatibilité GSLIB/MECASPA: DM842)
    if (str%ikle(3) == 1) then
       ! ikle(3)=1 => activité réelle COMPAS
       actsol = MSP_ENUM_ACTSOL_COMPAS
    else if (str%ikle(3) == 3) then  
       ! ikle(3)=3 => activité réelle sur fichier
       actsol = MSP_ENUM_ACTSOL_REELLE
    else if (str%ikle(3) == 2) then 
       actsol = MSP_ENUM_ACTSOL_STD
    else
       actsol = 0
    endif 
       
    modele = MSP_creer_modele ( nom_mod=nom_mod, &
         potentiel  = MSP_creer_potentiel(nomfic=fic_pot,nzo=str%nzo,nte=str%nte,nom_pot=str%modpot), &
         atmosphere = MSP_creer_atmosphere(modele=str%modatm,actsol=actsol, &
         ficactsol=str%ficactsol,dateref_actsol=str%dateref_actsol, &
         flux=str%flu, ap=str%app,us76_deltat=str%deltat, &
         mars_russe_mod=str%scena,&
         emcd=atm_emcd, exp = atm_exp),&
         pre_solaire = MSP_creer_pre_solaire(type=str%ikle(4),ficept=str%ficept), &
         trois_corps = MSP_creer_trois_corps(soleil=str%ikle(1),lune=str%ikle(2),&
         ficept=str%ficept) )
    if (MSP_PROBLEME) then 
          call MSP_signaler_message (cle_mes="MSP_PROPAGATION_PROBLEME", &
               routine="ps_creer_modele_file", type=MSP_ENUM_PROPAGATION)
          return
    end if

    ier = acc_deconnect (nacc, ACC_R)
    if (ier < 0) then 
       call MSP_signaler_message (cle_mes="MSP_probleme_close", &
            routine="ps_creer_modele_file", type=MSP_ENUM_ERREUR)
       return
    end if

    ier = acc_close(nacc)
    if (ier < 0) then 
       call MSP_signaler_message (cle_mes="MSP_probleme_close", &
            routine="ps_creer_modele_file", type=MSP_ENUM_ERREUR)
       return
    end if


end FUNCTION ps_creer_modele_file



!+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=
  type(PS_STR_INTEGRATION) FUNCTION ps_creer_integration_file(file, nomdom, &
       section, type_section, datbul) result(integration)
!+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_creer_integration_file
!
!$Resume
!  Création d'une structure intégration
!
!$Description
!  Création d'une structure intégration
!
!$Auteur
!  26/07/1999 - Jean-Jacques Wasbauer
!
!$Acces
!  PUBLIC
!
!$Usage
!  integration = ps_creer_integration_file(file, nomdom, [section], [type_section], [datbul])
!.    character(LEN=*) :: file, nomdom
!.    integer, dimension(:) :: type_section
!.    character(LEN=*), dimension(:) :: section
!.    type(PS_STR_INTEGRATION) :: integration
!.    real(KIND=PM_REEL) :: datbul
!
!$Arguments
!>E     file          :<LEN=*>                Fichier contenant une structure intégration MADONA
!>E     nomdom        :<LEN=*>                Nom du domaine de traduction
!>[E]   section       :<LEN=*,DIM=(:)>        Sections à sélectionner pour atteindre la structure intégration
!>[E]   type_section  :<integer,DIM=(:)>      Type des sections ci-dessus
!>[E]   datbul        :<PM_REEL>              Date du bulletin de référence
!>S     integration   :<PS_STR_INTEGRATION>   Structure intégration
!
!$Common
!
!$Routines
!- gsrwnotauto
!- MSP_signaler_message
!- MSP_annuler_probleme
!- gssetaccin
!- read_GS_PS_INTEGRATION
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

    character(LEN=*), intent(in) :: file, nomdom
    integer, dimension(:), intent(IN), optional :: type_section
    character(LEN=*), dimension(:), intent(IN), optional :: section
    type(PS_STR_INTEGRATION) :: str
    real(KIND=PM_REEL), intent(IN), optional :: datbul

    integer :: jjref
    integer :: ier
    integer :: kint
    integer :: lentab, nacc
    real(KIND=PM_REEL) :: date_ref
    
    call gsrwnotauto()

    ier = MSP_acc_charger_donnees(file, nacc, "r")
    if (MSP_PROBLEME) then 
          call MSP_signaler_message (cle_mes="MSP_PROPAGATION_PROBLEME", &
               routine="PSM_creer_integration", type=MSP_ENUM_PROPAGATION)
          return
    end if

    call gssetaccin(nacc)

    ! Connexion AMIGAU au fichier choisi
    if (PRESENT(section).and.PRESENT(type_section)) then 

       lentab = size(section)
       ier = MSP_acc_select(nacc, section, type_section, lentab)
       if (MSP_PROBLEME) then 
          call MSP_signaler_message (cle_mes="MSP_PROPAGATION_PROBLEME", &
               routine="PSM_creer_integration", type=MSP_ENUM_PROPAGATION)
          return
       end if

    else if ( (PRESENT(section).and..not.PRESENT(type_section)) .or. &
         (.not.PRESENT(section).and.PRESENT(type_section)) ) then 

       call MSP_signaler_message (cle_mes="MSP_erreur_arguments_001", &
            routine="PSM_creer_integration", type=MSP_ENUM_ERREUR, &
            partie_variable="section et type_section")
       return

    end if

    if (.not.PRESENT(datbul)) then 

       call MSP_signaler_message (cle_mes="MSP_erreur_arguments_002", &
            routine="PSM_creer_integration", type=MSP_ENUM_WARNING, &
            partie_variable="datbul")
       date_ref = 0._pm_reel
       jjref = 0
    else
       jjref = int(datbul)
       date_ref = (datbul - jjref) * 86400
    end if
    
    ! Dechargement du fichier unite puisque recharge dans read_GS_PS_INTEGRATION
    if (gsun == 0) ier = AMv_unit_unload()

    call read_GS_PS_INTEGRATION (kint, str%h1, str%h2, str%hstop, str%h3, &
         str%pinteg0, str%pinteg1, str%pinteg2, str%pinteg3, str%type_alt, str%idatmax, str%rdatmax,     &
         str%tmax, str%typdat, jjref, date_ref, nomdom)          

    integration = str

    ier = acc_close(nacc)
    if (ier < 0) then 
       call MSP_signaler_message (cle_mes="MSP_probleme_close", &
            routine="PSM_creer_integration", type=MSP_ENUM_ERREUR)
       return
    end if

  end function ps_creer_integration_file

end MODULE PS_UTILITAIRES
