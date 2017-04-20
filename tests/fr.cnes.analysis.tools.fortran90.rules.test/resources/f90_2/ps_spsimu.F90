module ps_spsimu

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ps_spsimu
!
!$Resume
!  Module contenant le sous-programme d'appel à PSIMU en Fortran90.
!
!$Description
!  Module contenant le sous-programme d'appel à PSIMU en Fortran90.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: ps_spsimu.F90 368 2013-02-19 14:43:59Z aadt $
!
!$Historique
!  $Log: ps_spsimu.F90,v $
!  Revision 368  2013/02/19 aadt
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 1.42  2011/09/26 13:02:49  mercadig
!  VERSION::FA-ID:1490:20/09/2011:Initialisation de delta_date
!
!  Revision 1.41  2010/10/25 13:10:59  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.40  2009/11/19 15:55:52  mercadig
!  DM-ID 1019: Mise a jour cartouche modout
!
!  Revision 1.39  2009/10/06 08:35:26  cmartel
!  DM-ID 1164 : Le coordonnees cartesiennes doivent toujours etre mises a jour (pb algo)
!
!  Revision 1.38  2009/09/04 12:52:12  tanguyy
!  DM-ID 1113 : le flag kkcar prend la valeur 2 lorsqu'une structure MECASPA est passÃ©e Ã  la routine psimu()
!
!  Revision 1.37  2009/07/16 08:02:55  cml
!  DM-ID 1164 : Les attitudes doivent etre calculees par calsim=1 et 2
!
!  Revision 1.36  2009/07/15 10:20:47  cml
!  DM-ID 1164 : Ajout d'une routine d'analyse du code calsim en entree
!
!  Revision 1.35  2009/06/16 08:26:28  tanguyy
!  FA-ID 1306 : mise a NULL des differents pointeurs, lors du 1er appel avec ce numero de vehicule
!  Revision 1.34  2009/05/05 10:01:17  tanguyy
!  AQ : correction du code mettant a jour iloi_scenar_pro(iveh) pour tenir compte du sens d'integration.
!  Revision 1.33  2009/04/15 15:56:24  tanguyy
!  FA-ID 1275 : ypas doit etre mis a jour dans ps_spsimu, quand on reinitialise le bulletin. Cela doit etre fait avant d'utiliser ypas pour determiner le sens de propagation
!  Revision 1.32  2009/04/14 16:16:26  tanguyy
!  DM-ID 1274 : PSIMU n'est pas forcement reinitialise lorsque l'on passe un scenario d'attitude
!  Revision 1.31  2009/03/17 16:02:23  tanguyy
!  DM-ID 1227 : 2nde phase / utilisation des flags reinit_bulletin et reinit_calcul
!  Revision 1.30  2009/03/06 08:40:35  tanguyy
!  DM-ID 1227 (phase 1 : prototypage) + FA-ID 1186 (nommage des rep fcf)
!  Revision 1.29  2008/12/02 16:55:10  tanguyy
!  AQ : mise à jour des cartouches avant livraison de PSIMU V9.3
!  Revision 1.28  2008/12/02 10:46:25  huec
!  DM-ID 1058 : Suppression de la redeclaration en save de ini_var
!  Revision 1.27  2008/12/02 08:15:33  tanguyy
!  DM-ID 1058 : Modification de la declaration des variables locales de ps_spsimu suite à l'intégration dans ELECTRA / g95. Les "data" sont supprimés.
!  Revision 1.26  2008/11/18 13:39:29  tanguyy
!  DM-ID 733 : reorganisation des modules : donnees deplacées dans str_mod
!  Revision 1.25  2008/09/04 07:53:18  tanguyy
!  DM-ID 1058 : phase 1 du portage / suppression des warnings - initialisations
!  Revision 1.24  2008/02/15 16:36:39  huec
!  DM-ID 11 : Suppression de l utilisation d un fichier de saut du TUC hors base COMPAS
!  Revision 1.23  2007/10/30 08:44:08  huec
!  DM-ID 744 : Modele d atmosphere de Venus a implementer dans PSIMU
!  Revision 1.22  2007/07/09 12:36:11  tanguyy
!  PSIMU V9.0 - DM-ID 688 : code NAIF (ex : 399) pour indiquer le mode d'utilisation PSIMU
!  Revision 1.21  2007/02/06 16:14:31  vivaresf
!  Validation version V8.7a1
!  Revision 1.20  2007/02/05 17:27:05  tanguyy
!  DM-ID 643 : modifs des interfaces pour le mode sous-programme
!  Revision 1.19  2006/10/19 15:10:07  tanguyy
!  DM-ID 478 : Cloture du FT (Integrateur de Cowell : modification de l interface)
!  Revision 1.18.2.1  2006/10/13 07:55:25  tanguyy
!  DM-ID 478 : utilisation jj/sec. 1ere version fonctionnelle
!  Revision 1.18  2006/03/15 13:25:21  tanguyy
!  Livraison PSIMU V8-4 ; relecture code / suppression code mort / maj cartouches
!  Revision 1.17  2005/12/14 19:08:06  tanguyy
!  PSIMU V8-3
!  Revision 1.16  2005/11/10 18:37:10  vivaresf
!  Mise à jour des cartouches
!  Revision 1.15  2005/02/14 11:11:43  fabrec
!  DM-ID 4 : allocation dynamique pour l'activite solaire
!  Revision 1.14  2005/02/04 15:35:37  fabrec
!  DM-ID 4 : allocation dynamique pour l'activite solaire
!  Revision 1.13  2005/01/28 10:42:26  fabrec
!  DM-ID 175 : desallocations memoire
!  Revision 1.12  2005/01/17 15:29:48  fabrec
!  DM-ID 175 : utilisation des scenarios mecaspa
!  Revision 1.10  2004/07/07 08:55:35  adm_ipsi
!  DM_89
!  Revision 1.9.2.1  2004/07/02 07:41:43  ole
!  DM_89, version initiale
!  Revision 1.9  2004/06/24 08:58:16  adm_ipsi
!  DM_88
!  Revision 1.8.2.1  2004/06/23 15:58:55  adm_ipsi
!  DM-ID 88 : Utilisation du fichier de ressources associé à la version
!  Revision 1.8  2003/09/11 15:10:09  adm_ipsi
!  FA-ID 53, Correction sur le bug lors de l'utilisation en multi-véhicule
!  Revision 1.7  2003/02/14 15:52:30  rodier
!  PhB - Lecture ressource data_saut_TUC
!  Revision 1.6  2003/01/29 14:51:19  boschett
!  A Deramecourt : modif des appels pour madona
!  Revision 1.5  2002/12/04 14:27:08  boschett
!  Suppression des instructions return inutiles en fin de routine
!  Revision 1.4  2002/12/02 17:05:51  boschett
!  Suppression des variables locales déclarées et non utilisées
!  Revision 1.3  2002/11/26 17:05:29  boschett
!  Ajout de implicit none
!  Revision 1.2  2002/11/22 12:08:17  boschett
!  Passage de l'argument de scénario en INOUT pour l'appel à MSP_consulter_scenario car cette fonction repositionne le pointeur loi_courante du scénario
!  Revision 1.1.1.1  2002/09/30 14:59:35  laurent
!  Industrialisation PSIMU
!  Revision 1.13  2001/11/07 13:11:20  util_am
!  Possibilité de choisir entre G50 CNES et J2000 pour le RIS en mode Terre
!  Revision 1.12  2001/01/09 12:41:15  util_am
!  Prise en compte de la précision numérique sur une date d'entrée: ajout de l'argument datprec
!  Revision 1.11  2000/09/08 16:54:48  util_am
!  Erreur dans un chemin d'accès
!  Revision 1.10  2000/06/21 15:24:24  util_am
!  Gestion des ressources (fichiers de donnees)
!  Revision 1.9  2000/05/29 09:12:07  util_am
!  Utilisation de Domtraduire
!  Revision 1.8  2000/04/17 10:58:18  util_am
!  Version multi_satellite en Fortran90
!  Revision 1.7  2000/03/02 16:03:23  util_am
!  Bug dans le calcul du numéro de la prochaine poussée quand on change de sens.
!  Revision 1.6  2000/02/08 09:51:47  util_am
!  Modification pour tenir compte d'un delta de date sur les lois lors d'une réinitialisation de bulletin
!  (=> Ajout du paramètre datbul pour les utilitaires de conversions)
!  Revision 1.5  1999/12/09 10:19:57  util_am
!  Ajout d'un mode de calcul simplifié avec sorties dans Greenwich
!  Revision 1.4  1999/11/09 10:19:35  util_am
!  test sur l'altitude sans utiliser vetat(27)
!  Revision 1.3  1999/10/26 11:00:13  util_am
!  Mise à jour des cartouches
!  Revision 1.2  1999/08/04 11:28:19  util_am
!  Prise en compte de la gestion des erreurs de MECASPA
!
!$FinHistorique
!
!$Usage
!  use ps_spsimu
!
!$Structure
!
!$Global
!>  PS_SORTIES_COMPLETES     : <integer,parameter>  Code commandant le calcul de toutes les variables
!>  PS_SORTIES_POSVIT_RIS    : <integer,parameter>  Calcul des positions vitesses et attitudes
!                                                   dans le repère d'intégration de sortie
!>  PS_SORTIES_RIS_ET_RTS    : <integer,parameter>  Calcul des positions vitesses et attitudes
!                                                   dans le RIS et dans le repère tournant de sortie
!>  PS_SORTIES_RIS_RTS_GEO   : <integer,parameter>  Calcul des positions vitesses dans le RIS, le RTS 
!                                                   et calcul des positions vitesses géodésiques                                                     
!>  PS_SORTIES_RIS_KEP       : <integer,parameter>  Calcul des positions vitesses dans le RIS 
!                                                   et des paramètres orbitaux
!$Common
!
!$Routines
!- psimu90
!- psi_maj_flags_calcul_vetat
!
!$Fonctions
!
!$Include
!
!$Module
!#V
!- MECASPA
!- ps_interface_psimu_don
!- ps_generalites
!- ps_initialisations
!- ps_integration
!- ps_integration_cowell
!- ps_integration_rkutta
!- ps_evenements
!- ps_propulsion
!- ps_separations
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
   use MECASPA
   use ps_interface_psimu_don
   use ps_generalites

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ps_spsimu.F90 368 2013-02-19 14:43:59Z aadt $'


   integer, parameter :: PS_SORTIES_COMPLETES   = 0
   integer, parameter :: PS_SORTIES_POSVIT_RIS  = 1
   integer, parameter :: PS_SORTIES_RIS_ET_RTS  = 2
   integer, parameter :: PS_SORTIES_RIS_RTS_GEO = 3
   integer, parameter :: PS_SORTIES_RIS_KEP     = 4


   contains

      subroutine psimu90 (ifinal,tfinal,vetat,jflag,calsim,planete,pbul,pcar,pmod,pvent,pint,pati,psep,ppro,numveh,datprec,modout)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psimu90
!
!$Resume
!  Sous-programme appelant PSIMU en Fortran 90.
!
!$Description
!  Sous-programme appelant PSIMU en Fortran 90.
!!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psimu90 (ifinal,tfinal,vetat,jflag,[calsim],[planete],[pbul],[pcar],[pmod],[pvent],[pint],[pati],[psep],[ppro],[numveh],[datprec],[modout])
!.    integer :: ifinal
!.    real (KIND=pm_reel) :: tfinal
!.    real (KIND=pm_reel) :: vetat(nvmax)
!.    integer :: jflag
!.    integer :: calsim
!.    integer :: planete
!.    type(MSP_BULLETIN) :: pbul
!.    type(MSP_VEHICULE) :: pcar
!.    type(MSP_MODELE) :: pmod
!.    type(MSP_MODVENT) :: pvent
!.    type(PS_STR_INTEGRATION) :: pint
!.    type(MSP_SCENARIO_LOI) :: pati
!.    type(MSP_SCENARIO_LOI) :: psep
!.    type(MSP_SCENARIO_LOI) :: ppro
!.    integer :: numveh
!.    real (KIND=pm_reel) :: datprec
!.    integer :: modout
!
!$Arguments
!>E     ifinal   :<integer>               indice pour savoir si tfinal correspond à une durée (sec) ou une date (JJ CNES)
!.                                         ifinal = 1 -> date de fin
!.                                         ifinal = 2 -> durée
!>E     tfinal   :<pm_reel>               date de fin ou durée de l'extrapolation
!>S     vetat    :<pm_reel,DIM=(nvmax)>   tableau des variables de sortie (t=tfinal)
!>S     jflag    :<integer>               indice permettant de connaitre le mode de sortie de psimu:
!.                                         jflag = 1 -> date finale atteinte
!.                                         jflag = 2 -> altitude finale atteinte
!>[E]   calsim   :<integer>               indice pour savoir si on sort toutes les variables ou non
!                                          (argument optionnel, par défaut 0):
!.                                         icalsim = 0 -> on sort toutes les variables
!.                                         icalsim = 1 => calcul simplifié avec éphémérides du véhicule dans le repère d'intégration
!.                                         icalsim = 2 => calcul simplifié avec éphémérides du véhicule dans le repère d'intégration et dans Greenwich
!>[E]   planete  :<integer>               code NAIF de la planète : argument optionnel, par défaut,eph_terre = 399 ("Terre")
!>[E]   pbul     :<MSP_BULLETIN>          données sur le bulletin (argument optionnel)
!>[E]   pcar     :<MSP_VEHICULE>          données sur les caractéristiques véhicule (argument optionnel)
!>[E]   pmod     :<MSP_MODELE>            données sur les modèles (argument optionnel)
!>[E]   pvent    :<MSP_MODVENT>           données du modèle de vent (argument optionnel)
!>[E]   pint     :<PS_STR_INTEGRATION>    données sur les paramètres d'intégration (argument optionnel) 
!>[E/S] pati     :<MSP_SCENARIO_LOI>      données sur les lois d'attitude (argument optionnel)
!>[E/S] psep     :<MSP_SCENARIO_LOI>      données sur les lois de séparation (argument optionnel)
!>[E/S] ppro     :<MSP_SCENARIO_LOI>      données sur les lois de propulsion (argument optionnel)
!>[E]   numveh   :<integer>               numéro du véhicule utilisé (par défaut 1)
!>[E]   datprec  :<pm_reel>               précision sur les dates (sec) (par défaut la milli-seconde à savoir 0.001)
!>[E]   modout   :<integer>               J2000, GAMMA 50 ou GAMMA VRAI de la date pour la Terre
!.                                        sur Mars, toujours Planetocentrique Equatorial de la date ou EME2000
!
!$Common
!
!$Routines
!- getenv
!- MSP_signaler_message
!- MSP_ajouter_fichier_message
!- MSP_effacer_vehicule
!- MSP_effacer_scenario
!- MSP_effacer_modvent
!- MSP_effacer_atmosphere
!- MSP_effacer_potentiel
!- psi_maj_flags_calcul_vetat
!- psconv_msp_vers_psimu
!- MSP_consulter_bulletin
!- ps_analyse_scenario_attitude
!- ps_init_conditions_initiales
!- MSP_consulter_scenario
!- MSP_consulter_poussee_bulletin
!- ps_initialiser_simulation
!- MSP_consulter_poussee_continue
!- psbcowe
!- psbgill
!- psarret
!- MSP_effacer_poussee_continue
!
!$Include
!
!$Module
!#V
!- ps_initialisations
!- ps_integration
!- ps_integration_cowell
!- ps_integration_rkutta
!- ps_evenements
!- ps_propulsion
!- ps_separations
!- ps_interface_psimu_don
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use ps_initialisations
      use ps_integration
      use ps_integration_cowell
      use ps_integration_rkutta
      use ps_evenements
      use ps_propulsion
      use ps_separations
      use ps_interface_psimu_don

      implicit none

      ! Arguments
      !==========
      
      integer, intent(IN)              :: ifinal
      real (KIND=pm_reel), intent(IN)  :: tfinal

      real (KIND=pm_reel), intent(OUT) :: vetat(nvmax)
      integer, intent(OUT)             :: jflag

      integer, intent(IN), OPTIONAL                       :: calsim
      integer, intent(IN), OPTIONAL                       :: planete

      type(MSP_BULLETIN), intent(IN), OPTIONAL            :: pbul
      type(MSP_VEHICULE), intent(IN), OPTIONAL            :: pcar
      type(MSP_MODELE), intent(IN), OPTIONAL              :: pmod
      type(MSP_MODVENT), intent(IN), OPTIONAL             :: pvent
      type(PS_STR_INTEGRATION), intent(IN), OPTIONAL      :: pint
      type(MSP_SCENARIO_LOI), intent(INOUT), OPTIONAL     :: pati
      type(MSP_SCENARIO_LOI), intent(INOUT), OPTIONAL     :: psep
      type(MSP_SCENARIO_LOI), intent(INOUT), OPTIONAL     :: ppro

      integer, intent(IN), OPTIONAL :: numveh
      real (KIND=pm_reel), intent(IN), OPTIONAL :: datprec
      integer, intent(IN), OPTIONAL :: modout

      ! Variables locales
      !==================

      integer :: init
      ! Les variables du module d'interface PSIMU sont save pour garder leur valeur d'un appel sur l'autre.
      ! 
      integer             ,save       :: i,iter0(PS_NVMAX),iter(PS_NVMAX),jter,iterw(PS_NVMAX),inicow(PS_NVMAX),itsgnold(PS_NVMAX)
      real (KIND=pm_reel) ,save       :: ypas(PS_NVMAX),parex(6,PS_NVMAX)
      type (tm_jour_sec)  ,save       :: datex(PS_NVMAX)
      real (KIND=pm_reel) ,save       :: ralt,datbul_ref(PS_NVMAX),datbul_ref_old(PS_NVMAX),delta_date(PS_NVMAX)
      character(LEN=2)    ,save       :: madona_lang
      integer             ,save       :: typloi, ntab
      real (KIND=pm_reel) ,save       :: datbul,datedeb, date_ref, tbul
      type(MSP_POUSSEE_BULLETIN) :: loi_bul
      type(MSP_POUSSEE_CONTINUE) :: loi_cont
      real (KIND=pm_reel), pointer :: timp(:)

      integer :: longueur,ier,lnblnk
      character(LEN=256) :: rctmp
      character(LEN=12) :: psnum_version


      integer ::kkbul=0  
      integer ::kkcar=0 
      integer ::kkmod=0
      integer ::kkint=0
      integer ::kkati=0
      integer ::kksep=0
      integer ::kkpro=0
      integer ::kkecr=0
      integer ::kkeve=0

      ! flags de ré-initialisation du bulletin ou du calcul
      integer,save :: premiere_init_loi_att = 1
      integer,save :: premiere_init_bul(PS_NVMAX) = 1
      integer :: reinit_bulletin
      integer :: reinit_calcul 
      integer :: reinit_attitude

      logical, save :: ini_var = .true.

      nullify(timp)

      if ( ini_var ) then

         ! On récupère la langue par défaut de l'environnement:

         call getenv ("MADONA_LANG",madona_lang)
         if ( madona_lang .eq. "fr" ) then
            clangue = "fr"
         else if ( madona_lang .eq. "en" ) then
            clangue = "en"
         else
            clangue = "en"
         endif

         ! Récupération des données incluses dans les fichiers de ressource


         ! Le nom du répertoire fcf_psimu est lu dans le fichier de ressources associé à la version et non modifiable
         ier = AMv_rc_get ('fcf_psimu','psimu_'//trim(psnum_version()),'','fcf',rctmp,longueur)
         if ( ier < 0 ) then
            call MSP_signaler_message (cle_mes="PS_LECT_FIC_CONF",partie_variable="fcf_psimu",routine="psinit1")
            return
         else
            dirfcf_psimu = rctmp(1:longueur)
         endif

         ! Le nom du répertoire fcf_mecaspa est lu dans le fichier de ressources associé à la version et non modifiable
         ier = AMv_rc_get ('fcf_mecaspa','psimu_'//trim(psnum_version()),'','fcf',rctmp,longueur)
         if ( ier < 0 ) then
            call MSP_signaler_message (cle_mes="PS_LECT_FIC_CONF",partie_variable="fcf_mecaspa",routine="psinit1")
            return
         else
            dirfcf_mecaspa = rctmp(1:longueur)
         endif

         ! Ajout des fichiers de messages d'erreurs
         call MSP_ajouter_fichier_message (dirfcf_mecaspa(1:lnblnk(dirfcf_mecaspa))//"/MSP_MESSAGES")
         if ( MSP_gen_messages("psimu") ) return
         call MSP_ajouter_fichier_message (dirfcf_psimu(1:lnblnk(dirfcf_psimu))//"/PS_ERREURS_"//clangue)
         if ( MSP_gen_messages("psimu") ) return

         ini_var = .false.

      endif

      reinit_calcul = 0
      reinit_bulletin = 0

      ! Tests sur la présence des arguments:
    
      if ( PRESENT(numveh) )  then
          if ( numveh < 0 .or. numveh > PS_NVMAX ) then
             call MSP_signaler_message (routine="psimu90")
             return
          else
             iveh = numveh
          endif
       else
          iveh = 1
       endif
    
       if (tab_veh_init(iveh) == 0) then
          ! Structure véhicule
          call MSP_effacer_vehicule(str_car(iveh)%vehicule,nul=.true.)
          ! Scénarios de propulsion, attitude, séparations
          call MSP_effacer_scenario(scenar_pro(iveh),nul=.true.)
          call MSP_effacer_scenario(scenar_ati(iveh),nul=.true.)
          call MSP_effacer_scenario(scenar_sep(iveh),nul=.true.)
          ! Modèles & éphémérides
          call MSP_effacer_modvent(str_mod(iveh)%vent,nul=.true.)
          call MSP_effacer_atmosphere(str_mod(iveh)%atm,nul=.true.)
          call MSP_effacer_potentiel(str_mod(iveh)%pot,nul=.true.)

          nullify(str_mod(iveh)%vj)
          nullify(str_ecr(iveh)%ypas_sorties)

          ! flag à 1, pour que la routine ps_terminer_session efface les données de ce véhicule
          tab_veh_init(iveh) = 1
       end if

      ! Mise a jour des flags controlant la maj du vecteur d'etat
      ! en fonction du calsim
      if ( PRESENT(calsim) ) then
         call psi_maj_flags_calcul_vetat(calsim)
      else
         call psi_maj_flags_calcul_vetat(0)
      endif
      if ( MSP_gen_messages("psimu") ) return

      if ( PRESENT(planete) ) then
         str_gen(iveh)%planet = planete
      else
         str_gen(iveh)%planet = eph_terre
      endif
      ! Le mode de PSIMU se fait sur le corps central
      iplanet = str_gen(iveh)%planet

      if ( PRESENT(datprec) ) then
         PS_PREC_DATE = datprec
      else
         PS_PREC_DATE = 0.0001_pm_reel
      endif

      if ( PRESENT(modout) ) then
         PS_MODOUT = modout
      else
         PS_MODOUT = 1
      endif

      if ( PRESENT(pbul) ) then
         kkbul = 1
         call psconv_msp_vers_psimu (pbul,ps_don_bul)
         if ( MSP_gen_messages("psimu") ) return
         call MSP_consulter_bulletin(pbul,datbul=datbul_ref(iveh))
         if ( MSP_gen_messages("psimu") ) return
         if (premiere_init_bul(iveh) == 1) then
	    ! Première initialisation du bulletin
	    delta_date(iveh) = 0._pm_reel
	    premiere_init_bul(iveh) = 0
	 else	 
	    delta_date(iveh) = (datbul_ref_old(iveh) - datbul_ref(iveh)) * 86400._pm_reel
            ! Arrondi à la milli-seconde:
	    delta_date(iveh) = int(delta_date(iveh)) + nint((delta_date(iveh)-int(delta_date(iveh)))/PS_PREC_DATE)*PS_PREC_DATE
	 endif
	 datbul_ref_old(iveh) = datbul_ref(iveh)
         kkati = 2
         kksep = 2
         kkpro = 2
      endif
      if ( PRESENT(pcar) ) then
         kkcar = 2
         call psconv_msp_vers_psimu (pcar,ps_don_car)
         if ( MSP_gen_messages("psimu") ) return
      endif
      if ( PRESENT(pmod) )  then
         kkmod = 1
         call psconv_msp_vers_psimu (pmod,ps_don_mod)
         if ( MSP_gen_messages("psimu") ) return
      endif
      if ( PRESENT(pvent) )  then
         ! la présence du modèle de vent a une influence sur le bloc modèle de PSIMU
         kkmod = 1
         
         ! si une structure MSP est présente pour le vent, on positionne
         ! un flag dans ps_don_mod, et on indique le nom du modèle
         call psconv_msp_vers_psimu (pvent,ps_don_mod)
         if ( MSP_gen_messages("psimu") ) return
      endif
     
      if ( PRESENT(pint) )  then
         kkint = 1
         ps_don_int = pint
         if ( MSP_gen_messages("psimu") ) return
      endif
      if ( PRESENT(pati) )  then
         kkati = 1
         if (premiere_init_loi_att == 1) then
            ! C'est la 1ère fois qu'on initialise le scénario d'attitude, 
            ! il y aura réinitialisation de PSIMU
            kkati = 1
            premiere_init_loi_att = 0
         else
            reinit_attitude = 1
            
            ! Un scénario a déjà été initialisé
            ! Celui-ci remplace le précédent
            ! -> si le nouveau scénario n'a pas d'impact à la date courante
            ! alors on ne réinitialise pas
            ! sinon, kkati vaudra toujours 1 et le calcul (intégrateur) sera réinitialisé
            call ps_analyse_scenario_attitude(pati,reinit_attitude)
            if (MSP_gen_messages("psimu")) return
            if (reinit_attitude == 0) then
               kkati = 3
            end if
            
         end if

         call psconv_msp_vers_psimu (pati,datbul_ref(iveh),ps_don_ati)
         if ( MSP_gen_messages("psimu") ) return
         scenar_ati(iveh)=pati
      endif
      if ( PRESENT(psep) )  then
         kksep = 1
         call psconv_msp_vers_psimu (psep,datbul_ref(iveh),ps_don_sep)
         if ( MSP_gen_messages("psimu") ) return
         scenar_sep(iveh)=psep
      endif
      if ( PRESENT(ppro) )  then
         kkpro = 1
         call psconv_msp_vers_psimu (ppro,datbul_ref(iveh),ps_don_pro)
         if ( MSP_gen_messages("psimu") ) return
         scenar_pro(iveh)=ppro
      endif
     
    ! Initialisation de la boucle:

      init = kkbul+kkcar+kkmod+kkint+kkati+kksep+kkpro
      if ( init /= 0 ) then
         call ps_init_conditions_initiales (0,delta_date(iveh),kkbul,kkcar,kkmod,kkint,&
              kkati,kksep,kkpro,kkecr,kkeve,reinit_bulletin,reinit_calcul)
         if ( MSP_gen_messages("psimu") ) return
      endif

      if ( ifinal == 1 ) then
         str_int(iveh)%tmax = ( tfinal - datbul_ref(iveh) ) * 86400._pm_reel
       ! Arrondi à la milli-seconde:
         str_int(iveh)%tmax = int(str_int(iveh)%tmax) + nint((str_int(iveh)%tmax- &
                              int(str_int(iveh)%tmax))/PS_PREC_DATE)*PS_PREC_DATE
      else
         str_int(iveh)%tmax = tfinal
      endif

      str_eve(iveh)%ndeve = 0

!/  Lecture de la structure MSP_SCENARIO_LOI
      call MSP_consulter_scenario (scenar_pro(iveh), nloi=nloi_scenar_pro(iveh))
      if ( MSP_gen_messages("psimu") ) return
      
      do i = 1 , nloi_scenar_pro(iveh)
    !/  Extraction du type de la loi
         typloi = MSP_type_loi (scenar_pro(iveh), id=i)
         if ( MSP_gen_messages("psimu") ) return
         if (typloi == MSP_ENUM_LOI_BUL) then
     
!/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
            call MSP_consulter_scenario (scenar_pro(iveh), date_ref=date_ref)
            if ( MSP_gen_messages("psimu") ) return
            call MSP_consulter_scenario (scenar_pro(iveh), loi_bul=loi_bul, id=i)
            if ( MSP_gen_messages("psimu") ) return
            
            call MSP_consulter_poussee_bulletin (loi_bul, &
                 datedeb=datedeb, datbul=datbul)
            if ( MSP_gen_messages("psimu") ) return
                 
            tbul = (datbul - date_ref)*86400._pm_reel
            ! Arrondi à la milli-seconde:
            tbul = real(int(tbul),KIND=pm_reel) + &
                 real(nint((tbul-int(tbul))/PS_PREC_DATE),KIND=pm_reel)*PS_PREC_DATE
          
            if ((datedeb < str_int(iveh)%tmax) .and. (str_int(iveh)%tmax < tbul)) then
               call MSP_signaler_message (cle_mes="PS_INIT_PRO_002")
               return
            endif
         endif
      enddo

            
      if (reinit_bulletin == 1) then
         ! Le compteur "ypas" stocke la date courante, relative au bulletin initial.
         ! Ce compteur est remis à 0 lorsque l'on reinitialise le bulletin.
         ypas(iveh) = 0._pm_reel
      end if

      ! Détermination du sens d'intégration (posigrade ou rétrograde):
      if ( str_int(iveh)%tmax < ypas(iveh) ) then
         str_int(iveh)%itsgn = -1
         str_int(iveh)%tsign = -1._pm_reel
      else
         str_int(iveh)%itsgn = 1
         str_int(iveh)%tsign = 1._pm_reel
      endif

      if ( init /= 0 ) then
         call ps_initialiser_simulation(reinit_bulletin,reinit_calcul,jflag,iter0(iveh),&
              iter(iveh),iterw(iveh),inicow(iveh),&
	 itsgnold(iveh),ypas(iveh),datex(iveh),parex(1:6,iveh))
         if ( MSP_gen_messages("psimu") ) return
      else
         ralt = sqrt(parex(1,iveh)**2+parex(2,iveh)**2+parex(3,iveh)**2) - str_mod(iveh)%requa
         if ( ralt > str_int(iveh)%h2) then
            jflag=3
         else
            jflag=4
         endif
      endif

!/  Lecture de la structure MSP_SCENARIO_LOI
      call MSP_consulter_scenario (scenar_sep(iveh), nloi=nloi_scenar_sep(iveh))
      if ( MSP_gen_messages("psimu") ) return

!/  Lecture de la structure MSP_SCENARIO_LOI
      call MSP_consulter_scenario (scenar_pro(iveh))
      if ( MSP_gen_messages("psimu") ) return
      
    ! Redéfinition des prochaines dates à prendre en compte:

      if ( str_int(iveh)%itsgn /= itsgnold(iveh) ) then

         ! On incrémente les compteurs d'événements : 
         ! - événements explicites (str_eve(ive)%ndeve)
         ! - séparations
         ! - propulsions

         if ( str_eve(iveh)%ndeve /= 0 ) str_eve(iveh)%neve = str_eve(iveh)%neve + str_int(iveh)%itsgn
         if ( nloi_scenar_sep(iveh)  /= 0 ) iloi_scenar_sep(iveh) = iloi_scenar_sep(iveh) + str_int(iveh)%itsgn
         if ( nloi_scenar_pro(iveh)  /= 0 ) then
            if ( (iloi_scenar_pro(iveh) == (nloi_scenar_pro(iveh) + 1)) .or. (iloi_scenar_pro(iveh) == 0) ) then
               iloi_scenar_pro(iveh) = iloi_scenar_pro(iveh) + str_int(iveh)%itsgn
            else
               !/  Extraction du type de la loi
               typloi = MSP_type_loi (scenar_pro(iveh), id=iloi_scenar_pro(iveh))
               if ( MSP_gen_messages("psimu") ) return
               if (typloi /= 2) then
                  iloi_scenar_pro(iveh) = iloi_scenar_pro(iveh) + str_int(iveh)%itsgn
               else if (typloi == 2) then
                  call MSP_consulter_scenario (scenar_pro(iveh), loi_cont=loi_cont, id=iloi_scenar_pro(iveh))
                  if ( MSP_gen_messages("psimu") ) return
                  call MSP_consulter_poussee_continue (loi_cont, ntab=ntab,  dates=timp)
                  if ( MSP_gen_messages("psimu") ) return

                  ! Est-ce que on est en dehors de la loi courante ?
                  ! -> cas 1 : ypas < debut de la manoeuvre, et on intègre en rétrograde (ie : ypas diminue)
                  !            Dans ce cas, on ne "va" pas vers la manoeuvre courante, il faudra décrémenter l'indice
                  ! -> cas 2 : ypas > fin de la manoeuvre, et on intègre en posigrade (ie : ypas augmente)
                  !            Dans ce cas également, la manoeuvre courante est "passée", il faut incrémenter l'indice
                  !
                  if ( (ypas(iveh) <= timp(1) .and.&
                       str_int(iveh)%itsgn < 0) .or. &
                       (ypas(iveh) >= timp(ntab) .and.&
                       str_int(iveh)%itsgn > 0)) then

                     iloi_scenar_pro(iveh) = iloi_scenar_pro(iveh) + str_int(iveh)%itsgn

                  endif



               endif
            endif
         endif
      endif

    ! Simulation ...

      do while ( (jflag == 3) .or. (jflag == 4) )
         if ( jflag == 3 ) then
          ! Boucle sur l'intégrateur de Cowell:
            call psbcowe(iter0(iveh),iter(iveh),iterw(iveh),inicow(iveh),&
                 ypas(iveh),datex(iveh),parex(1:6,iveh),jflag)	
            if ( MSP_gen_messages("psimu") ) return
         else
          ! Boucle sur l'intégrateur Gill:
            call psbgill (iter0(iveh),iter(iveh),jter,iterw(iveh),ypas(iveh),datex(iveh),parex(1:6,iveh),jflag)
            if ( MSP_gen_messages("psimu") ) return
         endif
      enddo

    ! Messages d'arret du programme:

      call psarret (jflag,iter(iveh),jter,ypas(iveh),datex(iveh),parex(1:6,iveh))
      if ( MSP_gen_messages("psimu") ) return

      vetat(:) = str_gen(iveh)%etat(:)

      itsgnold(iveh) = str_int(iveh)%itsgn

      ! Libération de la mémoire
      call MSP_effacer_poussee_continue (loi_cont)
      if ( MSP_gen_messages("psimu") ) return
      if (associated(timp)) deallocate (timp)

   end subroutine psimu90


   subroutine psi_maj_flags_calcul_vetat(calsim)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psi_maj_flags_calcul_vetat
!
!$Resume
!  Routine de mise à jour des flags internes commandant la mise à jour 
!  du vecteur d'état.
!
!$Description
!  Cette routine analyse le code calsim (calcul simplifié) et met à jour
!  les flags en fonction
!
!$Auteur
!  Cédric Martel (Atos Origin)
!
!$Acces
!  PRIVE
!
!$Usage
!  call psi_maj_flags_calcul_vetat(calsim)
!.    integer :: calsim
!
!$Arguments
!>E     calsim  :<integer>   Code calsim compris entre 0 et 4
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
!- ps_interface_psimu_don
!#
!
!$Remarques
!  Génère une erreur si le code calsim n'est pas compris entre 0 et 4
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        use ps_interface_psimu_don

        implicit none
        
        integer, intent(in) :: calsim

        ! Mise a jour du flag minimum pour les positions cartésiennes dans le RIS
        ! Le cas échéant, cela pose des problèmes lorsque l'on ne spécifia aucune variable
        str_gen(iveh)%maj_vect_etat%calcul_car_ris = .true.

        ! Inits des flags
        str_gen(iveh)%maj_vect_etat%calcul_att_ris = .false.
        str_gen(iveh)%maj_vect_etat%calcul_car_rts = .false.
        str_gen(iveh)%maj_vect_etat%calcul_att_rts = .false.
        str_gen(iveh)%maj_vect_etat%calcul_pos_geo = .false.
        str_gen(iveh)%maj_vect_etat%calcul_vit_geo = .false.
        str_gen(iveh)%maj_vect_etat%calcul_kep     = .false.
        str_gen(iveh)%maj_vect_etat%calcul_forces  = .false.
        str_gen(iveh)%maj_vect_etat%calcul_force_prop = .false.
        str_gen(iveh)%maj_vect_etat%calcul_kep_moy = .false.
        str_gen(iveh)%maj_vect_etat%calcul_autres  = .false.

        select case (calsim)
           case (PS_SORTIES_COMPLETES)
               ! Calcul de l'ensemble des variables
               str_gen(iveh)%maj_vect_etat%calcul_car_ris    = .true.
               str_gen(iveh)%maj_vect_etat%calcul_att_ris    = .true.
               str_gen(iveh)%maj_vect_etat%calcul_car_rts    = .true.
               str_gen(iveh)%maj_vect_etat%calcul_att_rts    = .true.
               str_gen(iveh)%maj_vect_etat%calcul_pos_geo    = .true.
               str_gen(iveh)%maj_vect_etat%calcul_vit_geo    = .true.
               str_gen(iveh)%maj_vect_etat%calcul_kep        = .true.
               str_gen(iveh)%maj_vect_etat%calcul_forces     = .true.
               str_gen(iveh)%maj_vect_etat%calcul_force_prop = .true.
               str_gen(iveh)%maj_vect_etat%calcul_kep_moy    = .true.
               str_gen(iveh)%maj_vect_etat%calcul_autres     = .true.
           case (PS_SORTIES_POSVIT_RIS)
               str_gen(iveh)%maj_vect_etat%calcul_car_ris    = .true.
               str_gen(iveh)%maj_vect_etat%calcul_att_ris    = .true.
           case (PS_SORTIES_RIS_ET_RTS)
               str_gen(iveh)%maj_vect_etat%calcul_car_ris    = .true.
               str_gen(iveh)%maj_vect_etat%calcul_car_rts    = .true.
               str_gen(iveh)%maj_vect_etat%calcul_att_ris    = .true.
               str_gen(iveh)%maj_vect_etat%calcul_att_rts    = .true.
           case (PS_SORTIES_RIS_RTS_GEO)
               str_gen(iveh)%maj_vect_etat%calcul_car_ris    = .true.
               str_gen(iveh)%maj_vect_etat%calcul_car_rts    = .true.
               str_gen(iveh)%maj_vect_etat%calcul_pos_geo    = .true.
               str_gen(iveh)%maj_vect_etat%calcul_vit_geo    = .true.
           case (PS_SORTIES_RIS_KEP)
               str_gen(iveh)%maj_vect_etat%calcul_car_ris    = .true.
               str_gen(iveh)%maj_vect_etat%calcul_kep        = .true.
               str_gen(iveh)%maj_vect_etat%calcul_kep_moy    = .true.
           case default
              ! Génération d'une erreur
              call MSP_signaler_message (cle_mes="PS_CALSIM_INCONNU", &
                   routine="psi_maj_flags_calcul_vetat")
         end select

   end subroutine psi_maj_flags_calcul_vetat

end module ps_spsimu
