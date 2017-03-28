   program psimu

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  psimu
!
!$Resume
!  Programme de calcul de trajectoire d'un satellite.
!
!$Description
!  Programme de calcul de trajectoire d'un satellite en tenant compte:
!. de la planète d'origine
!. de divers modèles de potentiel
!. de la pression de radiation solaire
!. du potentiel lunaire
!. du potentiel solaire
!. du frottement atmosphérique
!. de lois d'attitude
!. de poussées (impulsionnelles, continues, bulletin)
!. de séparations
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: psimu.F90 368 2013-02-19 14:43:59Z aadt $
!
!$Historique
!  $Log: psimu.F90,v $
!  Revision 368  2013/02/19 aadt
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 1.48  2010/10/25 13:06:23  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.47  2009/11/18 19:22:01  kvernelo
!  AQ : correction liberations memoire pour mieux detecter les fuites
!
!  Revision 1.46  2009/10/26 14:02:47  kvernelo
!  VERSION:9.5:DM-ID:1299:26/10/2009:Noms absolus des fichiers si c'est une arboresence
!
!  Revision 1.45  2009/10/05 13:13:49  mercadig
!  FA-ID 1324: Appel a gs_init_bulletin apres le chargement des fichiers fcf
!
!  Revision 1.44  2009/08/31 12:21:43  cmartel
!  DM-ID 1164 : Ajout du calcul d'altitude quand on demande un affichage écran
!
!  Revision 1.43  2009/07/15 10:24:18  cml
!  DM-ID 1164 : Ajout de l'analyse des variables de sortie
!
!  Revision 1.42  2009/03/17 16:03:09  tanguyy
!  DM-ID 1227 : 2nde phase / utilisation des flags reinit_bulletin et reinit_calcul
!
!  Revision 1.41  2009/03/06 08:40:34  tanguyy
!  DM-ID 1227 (phase 1 : prototypage) + FA-ID 1186 (nommage des rep fcf)
!
!  Revision 1.40  2008/11/18 13:27:06  tanguyy
!  FA-ID 883 : PSIMU utilise le mode "historique" de GSLIB ( 9 planètes de repère ; "terrien", "martien", etc.)
!
!  Revision 1.39  2008/10/17 14:25:17  tanguyy
!  DM-ID 1058 : controles memoire ; nouvelle routine ps_terminer_session_PSIMU
!
!  Revision 1.38  2008/10/15 13:11:31  tanguyy
!  DM-ID 1058 : le fichier EPHEM est ouvert dans les routines d'initialisation / les liberations memoire sont faites via psarret()
!
!  Revision 1.37  2008/10/09 12:06:21  mercadig
!  FA-ID 883 Modification du nom de la routine qui initialise le bulletin GSLIB
!
!  Revision 1.36  2008/10/08 13:44:26  mercadig
!  FA-ID 883 Appel a la routine d initialisation du bulletin GSLIB
!
!  Revision 1.35  2008/04/22 08:18:18  tanguyy
!  AQ : vérif des cartouches pour livraison PSIMU V9.2
!
!  Revision 1.34  2008/04/21 16:04:24  ttn
!  FA-ID 981 : Mise a jour du message d'erreur si pb dans initialisation de base compas
!
!  Revision 1.33  2008/04/16 16:11:11  ttn
!  FA-ID 981 : Ajout d'un message d'erreur lorsque le fichier compas.rc est introuvable ou non conforme aux regles MADONA (suite cps_init_utilisateur)
!
!  Revision 1.31  2008/04/11 15:07:54  tanguyy
!  FA-ID 981 : gestion d'erreur sur l'initialisation COMPAS / FA-ID 798 utilisation de psnum_version
!
!  Revision 1.30  2008/04/03 14:35:16  ttn
!  FA-ID 658 : suppression des variables inutilisees
!
!  Revision 1.29  2008/02/15 16:34:51  huec
!  DM-ID 11 : Suppression de l utilisation d un fichier de saut du TUC hors base COMPAS
!
!  Revision 1.28  2007/12/05 16:06:02  huec
!  DM-ID 822 : Organisation des repertoires
!
!  Revision 1.27  2007/10/30 08:39:33  huec
!  DM-ID 744 : Modele d atmosphere de Venus a implementer dans PSIMU
!
!  Revision 1.26  2007/07/12 10:37:12  tanguyy
!  PSIMU 9.0 / Appel a la liberation des integrateurs
!
!  Revision 1.25  2007/07/12 10:35:55  tanguyy
!  PSIMU 9.0 / Appel a la liberation des integrateurs
!
!  Revision 1.24  2007/07/09 12:24:11  tanguyy
!  PSIMU V9.0 - intégration DM-ID 688
!
!  Revision 1.23  2007/03/19 14:49:40  mle
!  DM-ID 600 : permettre de traiter les ephemerides PSIMU par SORTILEGE
!
!  Revision 1.22  2007/02/02 09:15:02  mle
!  DM-ID 643 : modeles de vent dans PSIMU
!
!  Revision 1.21  2007/01/29 16:33:36  vivaresf
!  FA-ID 687 : terre en minuscule pour COMPAS
!
!  Revision 1.20  2006/12/05 15:10:18  tanguyy
!  Verif avant livraison
!
!  Revision 1.19  2006/12/01 13:39:12  tanguyy
!  PSIMU V8-6 / Integration finale / Correction FA-ID 628
!
!  Revision 1.18  2006/10/17 09:54:30  tanguyy
!  Finalisation DM-ID 478 (AQ : suppression var inutilisees, commentaires)
!
!  Revision 1.17  2006/03/15 13:25:23  tanguyy
!  Livraison PSIMU V8-4 ; relecture code / suppression code mort / maj cartouches
!  Revision 1.16  2005/11/10 18:37:11  vivaresf
!  Mise à jour des cartouches
!  Revision 1.15  2005/02/04 15:35:44  fabrec
!  DM-ID 4 : allocation dynamique pour l'activite solaire
!  Revision 1.14  2005/01/28 10:39:42  fabrec
!  maj cartouches
!  Revision 1.13  2004/06/24 08:59:55  adm_ipsi
!  DM-ID 88 : intégration
!  Revision 1.12.2.2  2004/06/23 16:00:00  adm_ipsi
!  DM-ID 88 : Utilisation du fichier de ressources associé à la version
!  Revision 1.12.2.1  2004/06/17 15:50:03  adm_ipsi
!  DM_88, version initiale
!  Revision 1.12  2003/11/25 11:58:53  adm_ipsi
!  FA-ID 75, nmax ,le degré max. du potentiel, est passé par common
!  Revision 1.11  2003/11/06 10:02:56  adm_ipsi
!  FA-ID 75, ajout du paramètre nmax dans l'appel à read_données
!  Revision 1.10  2003/07/11 09:37:52  adm_ipsi
!  DM-ID 21 : Suppresion des routines psfermefichiers et Initialiser_FichierXXX
!  Revision 1.12  2003/03/28 09:56:47  util_am
!  SLB - Version industrialisée
!  Revision 1.9  2003/02/18 16:59:27  rodier
!  PhB - Initialisation du mode développeur
!  Revision 1.8  2003/02/14 15:46:56  rodier
!  PhB - Répertoire de sortie concaténé avec fichier de sortie
!  Revision 1.7  2003/01/16 17:51:38  boschett
!  A Deramecourt : modif ouverture des fichiers EVENT, EPHEM
!                  ajout femeture pour ces mêmes fichiers
!  Revision 1.6  2002/12/20 16:41:30  boschett
!  Utilisation du traitement d'erreur de la MECASPA
!  Revision 1.5  2002/12/04 14:27:25  boschett
!  Suppression des instructions return inutiles en fin de routine ps_variables.F90
!  Revision 1.4  2002/12/04 11:29:56  boschett
!  Substitution de la sytaxe CHARACTER*N par CHARACTER(LEN=N)
!  Revision 1.3  2002/12/03 15:56:08  boschett
!  Initialisation de la variable jter
!  Revision 1.2  2002/12/02 17:07:09  boschett
!  Suppression des variables locales déclarées et non utilisées
!  Revision 1.1.1.1  2002/09/30 14:59:35  laurent
!  Industrialisation PSIMU
!  Revision 1.11  2000/09/04 08:02:52  util_am
!  Meiileure prise en compte de la redéfinition variable MADONA_DATA_PATH
!  Revision 1.10  2000/06/21 15:22:38  util_am
!  Gestion des ressources (fichiers de donnees)
!  Revision 1.9  2000/05/29 09:12:07  util_am
!  Utilisation de Domtraduire
!  Revision 1.8  2000/04/17 10:58:18  util_am
!  Version multi_satellite en Fortran90
!  Revision 1.7  2000/02/08 09:52:39  util_am
!  Ajout de l'argument delta_date dans l'appel à psinit0
!  Revision 1.6  2000/01/12 09:45:11  util_am
!  Modif provisoire pour gérer les variables d'environnement MADONA
!  Revision 1.5  1999/10/26 11:00:13  util_am
!  Mise à jour des cartouches
!  Revision 1.4  1999/08/04 11:28:20  util_am
!  Prise en compte de la gestion des erreurs de MECASPA
!  Revision 1.3  1999/07/01 08:02:27  util_io
!  Insertion de champs CVS pour la version et historique
!  Revision 1.2  1999/07/01 07:49:30  util_io
!  Insertion de champs CVS pour la version et historique
!
!$FinHistorique
!
!$Common
!
!$Routines
!- psinit1
!- psmess
!- psinit2
!- psbcowe
!- psbgill
!- psarret
!- psdesalsol
!- MSP_fermeture_MADONA
!- psinit1
!
!$Include
!
!$Module
!#V
!- MECASPA
!- ps_generalites
!- ps_initialisations
!- ps_evenements
!- ps_integration
!- ps_integration_cowell
!- ps_integration_rkutta
!- ps_actsol
!- ps_interface_psimu_don
!- ps_integration_don
!- ps_modeles
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use MECASPA

      use ps_generalites
      use ps_ecriture
      use ps_initialisations
      use ps_evenements
      use ps_integration
      use ps_integration_cowell
      use ps_integration_rkutta

      implicit none 

! SVN Source File Id
  character(len=256), parameter :: SVN_VER = &
   '$Id: psimu.F90 368 2013-02-19 14:43:59Z aadt $'


      integer :: jflag,iter0,iter,jter,iterw,inicow,itsgnold
      integer :: reinit_bulletin,reinit_calcul
      real (KIND=pm_reel) :: ypas,parex(6)

      type(tm_jour_sec) :: datex
      
!   ********************************************************************
!   * Initialisations                                                  *
!   ********************************************************************

      jter = 0
      ypas = 0._pm_reel


!     Numéro du satellite
!     ===================

      iveh = 1

!     Lecture de fichiers ...
!     =======================
      reinit_bulletin = 1
      reinit_calcul = 1

      call psinit1(reinit_bulletin,reinit_calcul)
      call psmess (nom_sp_propag="psinit1")

      tab_veh_init(iveh) = 1
      str_eve(iveh)%ndeve = str_eve(iveh)%ndeve - 1

!     Détermination du sens d'intégration (posigrade ou rétrograde)
!     =============================================================

      if ( str_int(iveh)%tmax < 0._pm_reel ) then
         str_int(iveh)%itsgn = -1
         str_int(iveh)%tsign = -1._pm_reel
      else
         str_int(iveh)%itsgn = 1
         str_int(iveh)%tsign = 1._pm_reel
      endif

!     Initialisation de la boucle et simulation si début de poussée a t=0:
!     ===================================================================

      call ps_initialiser_simulation (reinit_bulletin,reinit_calcul,jflag,iter0,iter,iterw,inicow,itsgnold,ypas,datex,parex)
      call psmess (nom_sp_propag="ps_initialiser_simulation")

!   ********************************************************************
!   * Simulation ...                                                   *
!   ********************************************************************

      do while ( (jflag == 3) .or. (jflag == 4) )
         if ( jflag == 3 ) then
            ! Boucle sur l'intégrateur de Cowell:
            ! ==================================
            call psbcowe (iter0,iter,iterw,inicow,ypas,datex,parex,jflag)
            call psmess (nom_sp_propag="psbcowe")
         else
            ! Boucle sur l'intégrateur Gill:
            ! =============================
            call psbgill (iter0,iter,jter,iterw,ypas,datex,parex,jflag)
            call psmess (nom_sp_propag="psbgill")
         endif
      enddo

!   ********************************************************************
!   * Messages d'arrêt du programme                                    *
!   ********************************************************************
      
      ! Messages d'arrêt selon le type d'arret
      call psarret (jflag,iter,jter,ypas,datex,parex)
      
      call ps_terminer_session_PSIMU
      
      call MSP_fermeture_MADONA(ilogeph)
      call MSP_fermeture_MADONA(ilogeve)
      call psmess (affichage_warning=flagw,nom_sp_propag="psarret")

   end program psimu



   subroutine psinit1 (reinit_bulletin,reinit_calcul)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psinit1
!
!$Resume
!  Initialisation de PSIMU en mode programme (arguments, ouverture des fichiers).
!
!$Description
!  Initialisation de PSIMU en mode programme (arguments, ouverture des fichiers).
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psinit1 ()
!
!$Arguments
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- MSP_mode_developpeur
!- getenv
!- getarg
!- MSP_ajouter_fichier_message
!- MSP_annuler_probleme
!- pslcons
!- gssetenv
!- gssetfic
!- read_donnees
!- psinit0
!- MSP_ouverture_MADONA_col
!
!$Include
!
!$Module
!#V
!- MECASPA
!- ps_interface_psimu_don
!- ps_generalites
!- ps_initialisations
!- ps_integration_don
!- ps_modeles
!- ps_ecriture
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use MECASPA

      use ps_interface_psimu_don
      use ps_generalites
      use ps_initialisations
      use ps_integration_don
      use ps_modeles
      use ps_ecriture

      implicit none

#include "parameter_gslib.h"

      ! Arguments
      !===========
      integer, intent(inout) :: reinit_bulletin,reinit_calcul

      character(LEN=2) :: copt, madona_lang

      integer :: iarg,lnblnk,ier,longueur,acces
      integer :: kkbul,kkcar,kkmod,kkint,kkati,kksep,kkpro,kkecr,kkeve
      integer :: init_bulletin

      logical :: flag_err_langue = .false.
      logical :: flag_err_arguments = .false.

      character(LEN=256)  :: rctmp,dirunit
      character(LEN=1024) :: madona_data_path

      character(LEN=12)   :: psnum_version

      data kkbul,kkint,kkcar,kkmod,kkati,kksep,kkpro,kkecr,kkeve / 1,1,1,1,1,1,1,1,1 /

      ! Initialisations diverses:

      ! Récupération des données incluses dans les fichiers de ressource

      ! Le répertoire data_psimu est lu dans le fichier de ressources modifiable
      ier = AMv_rc_get ('data_psimu','psimu','','data_psimu',rctmp,longueur)
      if ( ier < 0 ) then
         call MSP_signaler_message (cle_mes="PS_LECT_FIC_CONF",partie_variable="data_psimu",routine="psinit1")
         return
      else
         dirdat = rctmp(1:longueur)
      endif

      ! Le répertoire resul_psimu est lu dans le fichier de ressources modifiable
      ier = AMv_rc_get ('resul_psimu','psimu','','resul_psimu',rctmp,longueur)
      if ( ier < 0 ) then
         call MSP_signaler_message (cle_mes="PS_LECT_FIC_CONF",partie_variable="resul_psimu",routine="psinit1")
         return
      else
         dirres = rctmp(1:longueur)
      endif

      ! Le nom du fichier unités est lu dans le fichier de ressources associé à la version et non modifiable
      ier = AMv_rc_get ('unites','psimu_'//trim(psnum_version()),'','.',rctmp,longueur)
      if ( ier < 0 ) then
         call MSP_signaler_message (cle_mes="PS_LECT_FIC_CONF",partie_variable="unites",routine="psinit1")
         return
      else
         dirunit = rctmp(1:longueur)
      endif

      ! Le nom du répertoire fcf_psimu est lu dans le fichier de ressources associé à la version et non modifiable
      ier = AMv_rc_get ('fcf_psimu','psimu_'//trim(psnum_version()),'','fcf',rctmp,longueur)

      if ( ier < 0 ) then
         call MSP_signaler_message (cle_mes="PS_LECT_FIC_CONF",partie_variable="fcf_psimu",routine="psinit1")
         return
      else
         dirfcf_psimu = rctmp(1:longueur)
      endif

      ! Le nom du répertoire fcf_gslib est lu dans le fichier de ressources associé à la version et non modifiable
      ier = AMv_rc_get ('fcf_gslib','psimu_'//trim(psnum_version()),'','fcf',rctmp,longueur)
      if ( ier < 0 ) then
         call MSP_signaler_message (cle_mes="PS_LECT_FIC_CONF",partie_variable="fcf_gslib",routine="psinit1")
         return
      else
         dirfcf_gslib = rctmp(1:longueur)
      endif

      ! Le nom du répertoire fcf_mecaspa est lu dans le fichier de ressources associé à la version et non modifiable
      ier = AMv_rc_get ('fcf_mecaspa','psimu_'//trim(psnum_version()),'','fcf',rctmp,longueur)
      if ( ier < 0 ) then
         call MSP_signaler_message (cle_mes="PS_LECT_FIC_CONF",partie_variable="fcf_mecaspa",routine="psinit1")
         return
      else
         dirfcf_mecaspa = rctmp(1:longueur)
      endif

      ! Initialisation du mode développeur pour traiter
      ! les codes retour MSPRO
      call MSP_mode_developpeur()

      cficin   = 'OPTIONS'
      cficout1 = 'EPHEM'
      cficout2 = 'EVENT'

      iscreen  = 0
      flagw    = .false.

      iplanet  = 0

      ! On récupère la langue par défaut de l'environnement

      call getenv ("MADONA_LANG",madona_lang)
      if ( madona_lang .eq. "fr" ) then
         clangue = "fr"
      else if ( madona_lang .eq. "en" ) then
         clangue = "en"
      else
         flag_err_langue = .true.
         clangue = "en"
      endif

      ! Lecture des arguments:

      iarg = 1
      call getarg (iarg,copt)

      boucle_arg : do while ( copt(1:1) /= ' ' )

         call getarg (iarg,copt)

         if ( copt(1:1) /= '-' ) then

            flag_err_arguments = .true.
            exit boucle_arg

         else

            select case ( copt(2:2) )

               ! Changement du nom du fichier OPTIONS:
               case ('i')
                  call getarg (iarg+1,cficin)
               ! Changement du nom du fichier EPHEM:
               case ('o')
                  call getarg (iarg+1,cficout1)
               ! Changement du nom du fichier EVENT:
               case ('e')
                  call getarg (iarg+1,cficout2)
               ! Ecriture des données a l'ecran:
               case ('s')
                  iscreen = 1
                  iarg = iarg - 1
               ! Ecriture des warning à l'ecran:
               case ('w')
                  flagw = .true.
                  iarg = iarg - 1
               case default
                     flag_err_arguments = .true.
                     exit boucle_arg
            end select

         end if

         iarg = iarg+2
         call getarg (iarg,copt)

      enddo boucle_arg

      ! Ajout des fichiers de messages d'erreurs
      acces = acc_open()
      ier = acc_connect(acces,dirfcf_mecaspa(1:lnblnk(dirfcf_mecaspa))//"/MSP_MESSAGES",ACC_R)
      if (ier < 0) then
         ! Erreur : le fichier MSP_MESSAGE n'existe pas
         stop
      endif
      ier = acc_deconnect(acces,ACC_R)
      call MSP_ajouter_fichier_message (dirfcf_mecaspa(1:lnblnk(dirfcf_mecaspa))//"/MSP_MESSAGES")
      if ( MSP_gen_messages("psimu") ) return

      ier = acc_connect(acces,dirfcf_psimu(1:lnblnk(dirfcf_psimu))//"/PS_ERREURS_"//clangue,ACC_R)
      if (ier < 0) then
         ! Erreur : le fichier MSP_MESSAGE n'existe pas
         stop
      endif
      ier = acc_deconnect(acces,ACC_R)
      ier = acc_close(acces)
      call MSP_ajouter_fichier_message (dirfcf_psimu(1:lnblnk(dirfcf_psimu))//"/PS_ERREURS_"//clangue)
      if ( MSP_gen_messages("psinit1") ) return

      if ( flag_err_langue ) then
          call MSP_signaler_message (cle_mes="PS_LANGUE",routine="psinit1")
         call MSP_annuler_probleme ()
      endif
      if ( flag_err_arguments ) then
         call MSP_signaler_message (cle_mes="PS_ARGUMENTS")
         return
      endif

      ! Initialisations de constantes:

      call pslcons
      if ( MSP_gen_messages("psinit1") ) return

      ! Positionnement de MADONA_DATA_PATH pour le fichier unités */
      call getenv ("MADONA_DATA_PATH",madona_data_path)
      call gssetenv ('MADONA_DATA_PATH='//dirunit(1:lnblnk(dirunit))//':'//madona_data_path(1:lnblnk(madona_data_path)))

      ! Initialisation COMPAS
      ! FA-ID 981 : Si le fichier compas.rc ne respecte pas les règles MADONA, l'impossibilité d'ouvrir le fichier
      ! sera signaler.
      call cps_init_utilisateur()

!     Lecture des données dans les commons d'entrée:

      ier = DOMchargeDomaine(dirfcf_psimu(1:lnblnk(dirfcf_psimu))//&
                             "/PS_MESSAGES_"//clangue//".conf","psimu")
      ier = DOMchargeDomaine(dirfcf_gslib(1:lnblnk(dirfcf_gslib))//&
                             "/GS_MESSAGES_fr.conf","gslib_fr")
      ier = DOMchargeDomaine(dirfcf_gslib(1:lnblnk(dirfcf_gslib))//&
                             "/GS_MESSAGES_en.conf","gslib_en")
      if( index(cficin,"/") /= 0 ) then
         call  gssetfic (cficin)
      else
         call gssetfic (dirdat(1:lnblnk(dirdat))//"/"//cficin)
      endif
      ! Appel à la routine d'initialisation du bulletin GSLIB : 
      ! 0 = mode "historique" utilisant la théorie mixte Lieske-Whar-Aoki / UAI1994
      ! -> la liste de planètes de définition de repères est sous la forme "Terrien, Martien, etc"
      ! init_bulletin passera à 1 si l'init a eu lieu
      init_bulletin = 0

      call gs_init_bulletin("gslib_"//clangue,0,trim(dirfcf_psimu)//"/liste_corps",init_bulletin)      
      if ( MSP_gen_messages("psinit1") )  then
         call MSP_signaler_message (cle_mes="PS_LECT_FIC_COMPAS",routine="psinit1") 
         return
      endif
     
      call read_donnees ()
      if ( MSP_gen_messages("psinit1") ) return
 
!     Transfert des données des commons d'entrée
!     dans les commons internes à psimu

      call ps_init_conditions_initiales (1,0._pm_reel,kkbul,kkcar,kkmod,kkint,&
           kkati,kksep,kkpro,kkecr,kkeve,reinit_bulletin,reinit_calcul)
      if ( MSP_gen_messages("psinit1") ) return

!     Mise a jour des flags commandant la mise à jour du vecteur d'état
      call ps_analyser_maj_vetat(ps_don_ecr, str_gen(iveh)%maj_vect_etat)
      if ( MSP_gen_messages("psinit1") ) return 

!     Correction pour l'affichage ecran :
!     L'affichage écran nécessite le calcul des coordonnes géodésique
      if ( iscreen == 1 ) then
         ! Pour calculer l'altitude géodésique, il faut calculer
         ! les positions dans les repères géodésiques et RTS 
         str_gen(iveh)%maj_vect_etat%calcul_car_rts = .true.
         str_gen(iveh)%maj_vect_etat%calcul_pos_geo = .true.         
      endif

   end subroutine psinit1


