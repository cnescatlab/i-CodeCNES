module ps_propulsion

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ps_propulsion
!
!$Resume
!  Module contenant les sous-programmes nécessaires au calcul de poussées.
!
!$Description
!  Module contenant les sous-programmes nécessaires au calcul de poussées.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: ps_propulsion.F90 368 2013-02-19 14:43:59Z aadt $
!
!$Historique
!  $Log: ps_propulsion.F90,v $
!  Revision 368  2013/02/19 aadt
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 1.51  2010/10/25 13:10:59  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.50  2010/03/01 13:41:43  mercadig
!  VERSION:9.6:DM-ID:1350:01/03/2010: Variables locales passees en save dans pspcont
!
!  Revision 1.49  2009/11/30 13:15:16  mercadig
!  FA-ID 1129: Utilisation de l'epsilon eps et remplacement des valeurs en dur (1.e-10) dans les tests de comparaison des reels
!
!  Revision 1.48  2009/11/24 17:06:06  mercadig
!  AQ: Modification du test sur la masse dans pspcont (ajout de l epsilon masse utilise pour xmerg)
!
!  Revision 1.47  2009/10/28 16:17:51  mercadig
!  DM-ID 1018: Renommage de la variable locale dv et suppression de commentaires incorrects
!
!  Revision 1.46  2008/12/04 15:41:49  tanguyy
!  FA-ID 1100 : maj des tests pour la creation / suppression de l'integrateur pour la propulsion
!
!  Revision 1.45  2008/12/02 16:53:06  tanguyy
!  AQ : mise à jour des cartouches avant livraison de PSIMU V9.3
!
!  Revision 1.44  2008/11/26 08:35:48  tanguyy
!  DM-ID 733 : caracteristiques véhicules "lues" dans la structure MSP_VEHICULE
!  Revision 1.43  2008/11/18 13:38:56  tanguyy
!  DM-ID 733 : reorganisation des modules / utilisation de ps_force_modele_complet et ps_force_modele_simplifie
!  Revision 1.42  2008/10/24 09:41:13  huec
!  DM-ID 1058 : Gestion memoire
!  Revision 1.41  2008/10/15 13:06:25  tanguyy
!  DM-ID 1058 : rajout du flag d'initialisation de l'intégrateur pour les propulsions
!  Revision 1.40  2008/09/22 09:44:15  mercadig
!  FA-ID 1017 Correction appel de acc_close
!  Revision 1.39  2008/09/11 16:03:03  tanguyy
!  DM-ID 1058 : controle des desallocations d'integrateurs pour les propulsions
!  Revision 1.38  2008/09/04 07:53:13  tanguyy
!  DM-ID 1058 : phase 1 du portage / suppression des warnings - initialisations
!  Revision 1.37  2008/07/15 14:05:58  tanguyy
!  FA-ID 1100 : simplification des initialisations de l'integrateur lors des propulsions, pour permettre les poussées de durées nulles, sans emettre de message d'erreur
!  Revision 1.36  2008/04/04 13:36:39  ttn
!  FA-ID 940 : Comparaison de deux entites physiques reelles
!  Revision 1.35  2007/12/06 15:14:47  huec
!  DM-ID 733 : Annulation de la DM
!  Revision 1.34  2007/10/02 08:02:00  tanguyy
!  DM-ID 733 - utilisationd du calcul du potentiel de la MECASPA
!  Revision 1.33  2007/06/20 12:33:33  vivaresf
!  Intégration FA 725 patch V8.7a3
!  Revision 1.32.2.2  2007/04/18 06:27:11  vivaresf
!  FA-IF 725 : présentation des codes
!  Revision 1.32.2.1  2007/04/16 09:42:54  vivaresf
!  FA-ID 725 : optimisation et robustesse du patch V8.7a3
!  meilleure gestion des allocate (désallocation + status)
!  desallocation du COWELL si ré-initialisation de l'intégration
!  Revision 1.32  2007/02/02 12:20:41  vivaresf
!  FA-ID 695 : effacement des lois temporaires
!  Revision 1.31  2006/12/05 15:09:46  fabrec
!  suppression de variables redondantes
!  Revision 1.30  2006/10/19 15:09:28  tanguyy
!  DM-ID 478 : Cloture du FT (Integrateur de Cowell : modification de l interface)
!  Revision 1.29.2.2  2006/10/17 09:54:27  tanguyy
!  Finalisation DM-ID 478 (AQ : suppression var inutilisees, commentaires)
!  Revision 1.29.2.1  2006/10/13 07:55:23  tanguyy
!  DM-ID 478 : utilisation jj/sec. 1ere version fonctionnelle
!  Revision 1.29  2006/05/30 09:33:18  tanguyy
!  DM-ID 232 : Cloture du FT (Nommage des arguments lors de l appel d une routine ou d une fonction en Fortran 90 ou d un objet GENESIS)
!  Revision 1.28.2.1  2006/05/30 09:32:21  tanguyy
!  DM-ID 232 : nommage des parametres optionnels
!  Revision 1.28  2006/04/21 08:12:19  tanguyy
!  DM-ID 400 : Cloture du FT (Performances en temps de calcul sur les scnarios MECASPA)
!  Revision 1.27.2.1  2006/04/20 13:23:17  tanguyy
!  DM-ID 400 : utilisation de routines MSP rendant un pointeur sur la structure encapsulee dans les scenarios de lois
!  Revision 1.27  2006/03/15 13:16:55  tanguyy
!  FA-ID 498 : Cloture du FT (Bug dans le calcul des parametres orbitaux arpes une manoeuvre impulsionnelle)
!  Revision 1.26.2.1  2006/03/13 14:23:28  tanguyy
!  FA-ID 498 : Version initiale du FT (Bug dans le calcul des parametres orbitaux arpes une manoeuvre impulsionnelle)
!  Revision 1.26  2005/12/14 19:08:06  tanguyy
!  PSIMU V8-3
!  Revision 1.25  2005/12/14 13:22:44  vivaresf
!  DM-ID 397 : accepter les altitudes negatives si pas trop
!  Revision 1.24  2005/12/14 11:31:55  tanguyy
!  DM-ID 397 : Cloture du FT (Utiliser les intégrateurs MSPRO dans PSIMU)
!  Revision 1.23.2.3  2005/12/14 11:18:36  tanguyy
!  Tests sur les dates modifies
!  Revision 1.23.2.2  2005/12/08 09:16:39  tanguyy
!  Utilisation de psscemp pour le calcul des forces durant une poussee
!  Revision 1.23.2.1  2005/12/08 08:59:27  tanguyy
!  Version intermediaire avec integrateurs MSPRO (GILL seulement)
!  Revision 1.23  2005/11/10 18:37:09  vivaresf
!  Mise à jour des cartouches
!  Revision 1.22  2005/02/17 13:57:34  fabrec
!  DM-ID 98 : pas des ephemerides
!  Revision 1.21  2005/02/01 09:01:02  fabrec
!  DM-ID 175 : gestion de la memoire
!  Revision 1.20  2005/01/28 10:42:12  fabrec
!  DM-ID 175 : desallocations memoire
!  Revision 1.19  2005/01/25 16:20:29  fabrec
!  DM-ID 175 : date_ref
!  Revision 1.18  2005/01/21 16:04:10  fabrec
!  DM-ID 175 : dates
!  Revision 1.17  2005/01/17 15:28:32  fabrec
!  DM-ID 175 : utilisation des scenarios mecaspa
!  Revision 1.16  2004/11/23 10:45:42  vivaresf
!  DM-ID 200 : transfert des mu, vitrot, requa et apla dans le cas de la reorganisation des manoeuvres
!  Revision 1.15  2003/08/08 14:17:36  adm_ipsi
!  suppression de la routine psapous
!  Revision 1.14  2003/07/10 10:09:00  adm_ipsi
!  FA-ID 13 : Controle que la masse >0
!  Revision 1.13  2003/03/19 15:26:42  laurent
!   remplacement de mugil4 par psgil4
!  Revision 1.12  2003/03/14 15:25:27  adm_ipsi
!  Utilisation de ps_nloi_att, ps_nloi_propu, ps_npts_att, ps_npts_propu, ps_nsepa
!  Revision 1.11  2003/02/18 17:01:22  rodier
!  PhB - Suppression initialisation ibref + init. brequa_r et bapla_r
!  Revision 1.10  2003/02/14 16:20:06  rodier
!  PhB - MAJ identation
!  Revision 1.9  2002/12/20 16:40:32  boschett
!  Utilisation du traitement d'erreur de la MECASPA
!  Revision 1.8  2002/12/09 09:53:13  boschett
!  Modification de certais paramètres de mode intent(OUT) à intent(INOUT)
!  Revision 1.7  2002/12/05 17:40:18  boschett
!  Utilisation de les operateurs MECASPA .egal. et .different. pour tester l'égalité (inégalité) entre réels
!  Revision 1.6  2002/12/04 14:26:24  boschett
!  Suppression des instructions return inutiles en fin de routine
!  Revision 1.5  2002/12/03 18:13:18  boschett
!  Utilisation de la constante symbolique PS_ECRAN dans les appels d'écriture à l'écran
!  Revision 1.4  2002/12/03 14:59:21  boschett
!  Initialisation de la variable debit dans pssecmp
!  Revision 1.3  2002/12/02 17:05:18  boschett
!  Suppression des variables locales déclarées et non utilisées
!  Revision 1.2  2002/11/26 17:04:09  boschett
!  Ajout de implicit none
!  Revision 1.1.1.1  2002/09/30 14:59:34  laurent
!  Industrialisation PSIMU
!  Revision 1.13  2002/09/16 11:06:02  util_am
!  Introduction d'une direction de poussée indépendante de l'attitude
!  Revision 1.12  2000/06/27 11:42:40  util_am
!  Ajout des modes d'attitude LVLH et Yaw Steering
!  Revision 1.11  2000/06/21 15:25:11  util_am
!  retrait de l'include domtrad_F.h
!  Revision 1.10  2000/05/29 09:12:07  util_am
!  Utilisation de Domtraduire
!  Revision 1.9  2000/04/17 10:58:17  util_am
!  Version multi_satellite en Fortran90
!  Revision 1.8  2000/02/08 09:49:14  util_am
!  Modification sur xmerg(21) au lieu de xmerg(11) +
!  meilleure prise en compte d'une poussée bulletin dans le cas d'une réactualisation du bulletin initial
!  Revision 1.7  2000/02/02 14:46:38  util_am
!  Problème sur le choix de la loi d'attitude si la durée de poussée est <= au pas d'intégration initialement prévu => nouveau pas = duréee / 2
!  Revision 1.6  2000/01/14 12:17:36  util_am
!  Correction sur le choix de la loi d'attitude quand la date de fin de la loi i-1 est égale à la date de début de la loi i+1 (spécialement pour les poussées et les séparations)
!  Revision 1.5  1999/10/26 11:00:12  util_am
!  Mise à jour des cartouches
!  Revision 1.4  1999/08/31 14:58:06  util_am
!  Meilleure prise en compte des sorties dans le cas hyperbolique
!  Revision 1.3  1999/08/31 11:56:12  util_am
!  Prise en compte des nouvelles échelles de date et de temps
!  Revision 1.2  1999/08/04 11:28:17  util_am
!  Prise en compte de la gestion des erreurs de MECASPA
!
!$FinHistorique
!
!$Usage
!  use ps_propulsion
!
!$Structure
!
!$Global
!
!>  choix_loi   : <integer>  principe de choix de la loi d'attitude quand deux lois i/i+1 se jouxtent:
!                                      si 1: on prend la loi i, si 2: on prend la loi i+1
!$Common
!
!$Routines
!- pspimpu
!- pspcont
!- pssecmp
!- psnewbl
!- psinipc
!- pslecpro
!
!$Fonctions
!
!$Include
!
!$Module
!#V
!- MECASPA
!- ps_generalites
!- ps_bulletin
!- ps_caracteristiques
!- ps_ecriture
!- ps_propulsion_don
!- ps_variables
!- ps_attitude
!- ps_evenements
!- ps_modeles
!- MSPRO
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
   use ps_generalites
   use ps_bulletin
   use ps_caracteristiques
   use ps_ecriture

!   use ps_integration


!   use ps_variables

   use ps_propulsion_don

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ps_propulsion.F90 368 2013-02-19 14:43:59Z aadt $'


   integer :: choix_loi

   contains

      subroutine pspimpu (itsgn,tsign,t,date,par,iter)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  pspimpu
!
!$Resume
!  Sous-programme calculant un changement de trajectoire du a une impulsion.
!
!$Description
!  Sous-programme calculant un changement de trajectoire du a une impulsion.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call pspimpu (itsgn,tsign,t,date,par,iter)
!.    integer :: iter,itsgn
!.    real (KIND=pm_reel) :: tsign,t
!.    type(tm_jour_sec) :: date
!.    real (KIND=pm_reel) :: par(6)
!
!$Arguments
!>E     itsgn  :<integer>            sens de l'intégration:
!.                                    1 => posigrade
!.                                   -1 => rétrograde
!>E     tsign  :<pm_reel>            sens de l'intégration:
!.                                    1 => posigrade
!.                                   -1 => rétrograde   
!>E     t      :<pm_reel,DIM=(IN)>   instant de poussée (s)
!>E     date   :<tm_jour_sec>        date de poussée (Jours Juliens CNES)
!>E/S   par    :<pm_reel,DIM=(6)>    position/vitesse après poussée (m-m/s)
!>E     iter   :<integer>            numéro de l'itération
!
!$Common
!
!$Routines
!- ps_maj_vecteur_etat
!- pswresu
!- flush
!- MSP_consulter_scenario
!- MSP_consulter_impulsion
!- psattit
!- MSP_consulter_vehicule
!- MSP_consulter_mci
!- MSP_modifier_mci
!- MSP_modifier_vehicule
!- MSP_signaler_message
!
!$Include
!
!$Module
!#V
!- ps_variables
!- ps_attitude
!- ps_evenements
!- ps_modeles
!- ps_caracteristiques
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      use ps_variables
      use ps_attitude
      use ps_evenements
      use ps_modeles
      use ps_caracteristiques

      implicit none

      ! Arguments
      !==========

      integer, intent(IN)                :: iter,itsgn
      real (KIND=pm_reel), intent(IN)    :: tsign,t
      type(tm_jour_sec), intent(in)      :: date
      real (KIND=pm_reel), intent(INOUT) :: par(6)

      ! Variables locales
      !==================

      type (MSP_MCI) :: mci
      real (KIND=pm_reel) :: delv,omega,omegap,dv(3),dv_RI(3),pgamav(3,3)
      real (KIND=pm_reel) :: angle1,angle2,angle3,angi1,angi2,angi3
      real (KIND=pm_reel) :: xmerg, masse
      real (kind=pm_reel) :: r2,v2,ray,unsura
      integer :: typa,repa,dirref
      type(MSP_IMPULSION) :: loi_imp
      
      ! Début du code
      !==============

!     Calcul et ecriture des variables selectionnees:
!     ----------------------------------------------
      nimp(iloi_scenar_pro(iveh)) = 1
      if ( ilogeve /= 0 ) then
         if (itsgn > 0) then
            choix_loi = 1
         else
            choix_loi = 2
         endif

         call ps_maj_vecteur_etat (t,date,par,choix_loi_atti=choix_loi)
         if ( MSP_gen_messages("pspimpu") ) return 
         call pswresu (1,ilogeph,0,10,iter,t,str_gen(iveh)%etat,str_ecr(iveh)%nvarw, &
                       str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,str_eve(iveh)%ifore,&
                       str_eve(iveh)%neve,str_eve(iveh)%aeve)
         if ( MSP_gen_messages("pspimpu") ) return 
         if (itsgn > 0) then
            call pswresu (2,ilogeve,0,10,iter,t,str_gen(iveh)%etat,str_ecr(iveh)%nvarw,&
                          str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,str_eve(iveh)%ifore,&
                          str_eve(iveh)%neve,str_eve(iveh)%aeve)
         else
            call pswresu (2,ilogeve,0,11,iter,t,str_gen(iveh)%etat,str_ecr(iveh)%nvarw,&
                          str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,str_eve(iveh)%ifore,&
                          str_eve(iveh)%neve,str_eve(iveh)%aeve)
         endif
         if ( MSP_gen_messages("pspimpu") ) return 
      endif

      if ( iscreen == 1 ) then
         write (PS_ECRAN,Domtraduire(nomdomaine,"PS_PRO_01")) iloi_scenar_pro(iveh)
         call flush (PS_ECRAN)
      endif
     
!/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
      call MSP_consulter_scenario (scenar_pro(iveh), loi_imp=loi_imp)
      if ( MSP_gen_messages("pspimpu") ) return
     
      call MSP_consulter_impulsion (loi_imp,  deltav=delv, dirref=dirref, &
           omega=omega, omegap=omegap, merg=xmerg)
      if ( MSP_gen_messages("pspimpu") ) return

!   * Composantes 3 axes du Deltav dans le repère Véhicule:

      dv(1)=delv*cos(omegap)*cos(omega)
      dv(2)=delv*sin(omegap)
      dv(3)=delv*cos(omegap)*sin(omega)

!   * Calcul de l'attitude:      

      if ( dirref == - 1 ) then

         ! La direction de poussée est donnée par rapport au repère véhicule
         if (itsgn > 0) then
            choix_loi = 2
         else
            choix_loi = 1
         endif
	 
         call psattit (date,t,par(1),par(4),angle1,angle2,angle3,angi1,angi2,angi3,pgamav,typa,repa,choix_loi=choix_loi)
         if ( MSP_gen_messages("pspimpu") ) return 
      else
         ! La direction de poussée est donnée par rapport à un autre repère
         choix_loi = -dirref
	 
         call psattit (date,t,par(1),par(4),angle1,angle2,angle3,angi1,angi2,angi3,pgamav,typa,repa,choix_loi=choix_loi)
         if ( MSP_gen_messages("pspimpu") ) return 
      endif

!   * Deltav dans le repere RI de PSIMU     

      dv_RI(1)=pgamav(1,1)*dv(1)+pgamav(2,1)*dv(2)+pgamav(3,1)*dv(3)
      dv_RI(2)=pgamav(1,2)*dv(1)+pgamav(2,2)*dv(2)+pgamav(3,2)*dv(3)
      dv_RI(3)=pgamav(1,3)*dv(1)+pgamav(2,3)*dv(2)+pgamav(3,3)*dv(3)

!   * Nouveau vecteur vitesse dans Planeto:

      par(4)=par(4)+tsign*dv_RI(1)
      par(5)=par(5)+tsign*dv_RI(2)
      par(6)=par(6)+tsign*dv_RI(3)

      call MSP_consulter_vehicule(str_car(iveh)%vehicule,mci=mci)
      call MSP_consulter_mci(mci,mstr=masse)

      ! - Contrôle que la masse > 0
      if (masse > tsign*xmerg) then
         masse=masse-tsign*xmerg
         
         call MSP_modifier_mci(mci,mstr=masse)
         call MSP_modifier_vehicule(str_car(iveh)%vehicule,mci=mci)

      else
         call MSP_signaler_message (cle_mes="PS_MASSE_001",routine="psimpu")
         return
      endif

      ! Positionnement de l'indicateur d'orbite hyperbolique (FA-ID 498)

      r2 = par(1)**2+par(2)**2+par(3)**2
      v2 = par(4)**2+par(5)**2+par(6)**2
      ray = sqrt(r2)
      unsura = 2._pm_reel/ray - v2/str_mod(iveh)%gmu

      if ( unsura >= 0 ) then
         str_gen(iveh)%hyp=0
      else
         str_gen(iveh)%hyp=1
      endif


      nimp(iloi_scenar_pro(iveh)) = 0
      if ( ilogeve /= 0 ) then
      
         call ps_maj_vecteur_etat (t,date,par,choix_loi_atti=choix_loi)
         if ( MSP_gen_messages("pspimpu") ) return 
         call pswresu (1,ilogeph,iscreen,11,iter,t,str_gen(iveh)%etat,&
              str_ecr(iveh)%nvarw,&
              str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,str_eve(iveh)%ifore,&
              str_eve(iveh)%neve,str_eve(iveh)%aeve)
         if ( MSP_gen_messages("pspimpu") ) return 
         if (itsgn > 0) then
            call pswresu (2,ilogeve,0,11,iter,t,str_gen(iveh)%etat,str_ecr(iveh)%nvarw,&
                          str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,str_eve(iveh)%ifore,&
                          str_eve(iveh)%neve,str_eve(iveh)%aeve)
         else
            call pswresu (2,ilogeve,0,10,iter,t,str_gen(iveh)%etat,str_ecr(iveh)%nvarw,&
                          str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,str_eve(iveh)%ifore,&
                          str_eve(iveh)%neve,str_eve(iveh)%aeve)
         endif
          if ( MSP_gen_messages("pspimpu") ) return 
      endif

      end subroutine pspimpu

      subroutine pspcont (itsgn,tsign,hstop,tmax,t,date,par,iter,kflag)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  pspcont
!
!$Resume
!  Sous-programme calculant une trajectoire propulsée.
!
!$Description
!  Sous-programme calculant une trajectoire propulsée.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call pspcont (itsgn,tsign,hstop,tmax,t,date,par,iter,kflag)
!.    integer :: itsgn
!.    real (KIND=pm_reel) :: tsign
!.    integer :: iter
!.    real (KIND=pm_reel) :: t,par(6),hstop,tmax
!.    type(tm_jour_sec) :: date
!.    integer :: kflag
!
!$Arguments
!>E     itsgn  :<integer>               sens de l'intégration:
!.                                       1 => posigrade
!.                                      -1 => rétrograde
!>E     tsign  :<pm_reel>               sens de l'intégration:
!.                                       1 => posigrade
!.                                      -1 => rétrograde   
!>E/S   hstop  :<pm_reel>               altitude de fin de simulation (m)
!>E/S   tmax   :<pm_reel>               instant de fin de simulation (s)
!>E/S   t      :<pm_reel,DIM=(INOUT)>   instant de début puis fin de poussée (s)
!>E/S   date   :<tm_jour_sec>           date de début puis fin de poussée (Jours Juliens CNES)
!>E/S   par    :<pm_reel,DIM=(6)>       position/vitesse en début puis fin de poussée (m-m/s)
!>E/S   iter   :<integer>               numéro d'itération en début puis fin de poussée
!>S     kflag  :<integer>               indicateur de sortie du programme pspcont:
!.                                      0 -> nominal
!.                                      1 -> tmax atteint
!.                                      2 -> hstop atteint
!
!$Common
!
!$Routines
!- MSP_consulter_scenario
!- MSP_consulter_poussee_continue
!- MSP_consulter_vehicule
!- MSP_consulter_mci
!- flush
!- ps_maj_vecteur_etat
!- pswresu
!- mu_liberer_integ
!- MSP_Signaler_Message
!- mu_creer_integrateur
!- mu_integrer
!- MSP_modifier_mci
!- MSP_modifier_vehicule
!- MSP_signaler_message
!- MSP_modifier_poussee_continue
!- MSP_modifier_scenario
!- MSP_effacer_poussee_continue
!
!$Include
!
!$Module
!#V
!- ps_variables
!- ps_modeles
!- ps_evenements
!- ps_generalites
!- ps_caracteristiques
!- MSPRO
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      use ps_variables
      use ps_modeles
      use ps_evenements
      use ps_generalites
      use ps_caracteristiques

      use MSPRO

      implicit none

      ! Arguments
      !==========
      integer, intent(IN)                :: itsgn
      real (KIND=pm_reel), intent(IN)    :: tsign
      integer, intent(INOUT)             :: iter
      real (KIND=pm_reel), intent(INOUT) :: t,par(6),hstop,tmax
      type(tm_jour_sec), intent(inout)   :: date
      integer, intent(OUT)               :: kflag

      ! Variables locales
      !==================
      logical :: lboucl
      integer :: iterw,npf,ier

      real (KIND=pm_reel) :: xpas,xpasint,dtint,tnext,ralt
      real (KIND=pm_reel), dimension(8) :: ve, ve_out
      real (kind=pm_reel) :: r2,v2,ray,unsura

      real (KIND=pm_reel), pointer :: timp(:)
      real (KIND=pm_reel), pointer :: fptab(:)
      real (KIND=pm_reel), pointer :: omtab(:)
      real (KIND=pm_reel), pointer :: omptab(:)
      real (KIND=pm_reel) :: xmerg, masse
      real (KIND=pm_reel) :: paspro
      integer :: ntab
      type(MSP_POUSSEE_CONTINUE) :: loi_pro
      type(MSP_MCI) :: mci

      integer :: allocstat ! status du deallocate

      !-- Variables locales liées à l'intégrateur
      

      real (kind=PM_REEL) :: date_sortie_integ, y_fsub
      type(tm_code_retour) :: code_retour

      save lboucl,iterw,npf,ier,xpas,xpasint,dtint,tnext,ralt,ve
      save r2,v2,ray,unsura,xmerg, masse,paspro,ntab
      save date_sortie_integ, y_fsub     

!   ********************************************************************
!   * Initialisations                                                  *
!   ********************************************************************

      kflag=0
      iterw=0
      nullify(timp)
      nullify(fptab)
      nullify(omtab)
      nullify(omptab)

!/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
      call MSP_consulter_scenario (scenar_pro(iveh), loi_cont=loi_pro)
      if ( MSP_gen_messages("pspcont") ) return
         
      call MSP_consulter_poussee_continue (loi_pro,  ntab=ntab, &
           dates=timp, &
           poussee=fptab,  &
           omega=omtab, omegap=omptab,&
           merg=xmerg, pas=paspro)
      if ( MSP_gen_messages("pspcont") ) return

      xpas=tsign*paspro
      xmerg_21=xmerg

      call MSP_consulter_vehicule(str_car(iveh)%vehicule,mci=mci)
      call MSP_consulter_mci(mci,mstr=masse)

      ve(1)=par(1)
      ve(2)=par(2)
      ve(3)=par(3)
      ve(4)=par(4)
      ve(5)=par(5)
      ve(6)=par(6)
      ve(7)=xmerg_21
      ve(8)=masse

      if ( iscreen == 1 ) then
         if ( (itsgn > 0) .and. (t .egal. timp(1)) ) then
            write (PS_ECRAN,Domtraduire(nomdomaine,"PS_PRO_02")) iloi_scenar_pro(iveh)
         else if ( (itsgn < 0) .and. (t .egal. timp(ntab)) ) then
            if (xmerg_21 < eps) write (6,Domtraduire(nomdomaine,"PS_PRO_03"))
            write (PS_ECRAN,Domtraduire(nomdomaine,"PS_PRO_04")) iloi_scenar_pro(iveh)
         endif
         call flush (PS_ECRAN)
      endif

      if (itsgn > 0) then
         choix_loi = 1
      else
         choix_loi = 2
      endif

      nimp(iloi_scenar_pro(iveh)) = 1
      if (ilogeve /= 0) then
         if (iter > 0) then
            nimp(iloi_scenar_pro(iveh)) = 0

            call ps_maj_vecteur_etat (t,date,ve,choix_loi_atti=choix_loi)
            if ( MSP_gen_messages("pspcont") ) return 
            call pswresu (1,ilogeph,iscreen,12,iter,t,str_gen(iveh)%etat,str_ecr(iveh)%nvarw,&
                          str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,&
                          str_eve(iveh)%ifore,str_eve(iveh)%neve,str_eve(iveh)%aeve)
           
         endif
         nimp(iloi_scenar_pro(iveh)) = 1
         if (itsgn > 0) then
            choix_loi = 2
         else
            choix_loi = 1
         endif
	 
         call ps_maj_vecteur_etat (t,date,ve,choix_loi_atti=choix_loi)
         if ( MSP_gen_messages("pspcont") ) return 
         call pswresu (1,ilogeph,0,12,iter,t,str_gen(iveh)%etat,str_ecr(iveh)%nvarw,&
                       str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,str_eve(iveh)%ifore,&
                       str_eve(iveh)%neve,str_eve(iveh)%aeve)
         if ( MSP_gen_messages("pspcont") ) return 
         if ( (itsgn > 0) .and. (t .egal. timp(1)) ) then
            call pswresu (2,ilogeve,0,12,iter,t,str_gen(iveh)%etat,str_ecr(iveh)%nvarw,&
                          str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,str_eve(iveh)%ifore,&
                          str_eve(iveh)%neve,str_eve(iveh)%aeve)
         else if ( (itsgn < 0) .and. (t .egal. timp(ntab)) ) then
            if (xmerg_21 < eps) then
               call pswresu (2,ilogeve,0,14,iter,t,str_gen(iveh)%etat,str_ecr(iveh)%nvarw,&
                             str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,str_eve(iveh)%ifore,&
                             str_eve(iveh)%neve,str_eve(iveh)%aeve)
            else
               call pswresu (2,ilogeve,0,13,iter,t,str_gen(iveh)%etat,str_ecr(iveh)%nvarw,&
                             str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,str_eve(iveh)%ifore,&
                             str_eve(iveh)%neve,str_eve(iveh)%aeve)
            endif
         endif
         if ( MSP_gen_messages("pspcont") ) return 
      endif

!   ********************************************************************
!   * Boucle sur la poussee                                            *
!   ********************************************************************

      if (itsgn > 0) then
         choix_loi = 2
         npf = ntab
      else
         choix_loi = 1
         npf = 1
      endif
      lboucl = .true.

      do while (lboucl)
         dtint = tsign*xpas
         if ((str_eve(iveh)%neve /= 0) .and. (str_eve(iveh)%neve /= (str_eve(iveh)%ndeve + 1))) then
            if (tsign*(str_eve(iveh)%deve(str_eve(iveh)%neve) - t) <= dtint) then
               ! Conditionnelle avec le test de la V8-2. 
               ! .and. &
               !   abs(str_eve(iveh)%deve(str_eve(iveh)%neve) - t) > MSP_EPSILON_DATE) then
               
               dtint = tsign*(str_eve(iveh)%deve(str_eve(iveh)%neve) - t)
               !!! tnext = str_eve(iveh)%deve(str_eve(iveh)%neve)
            endif
         endif
         if (tsign*(tmax - t) <= dtint .and. &
              abs(tmax - t) > MSP_EPSILON_DATE) then
            dtint = tsign*(tmax - t)
            !!! tnext = tmax
         endif
         if (tsign*(timp(npf) - t) <= dtint .and. &
              abs(timp(npf) - t) > MSP_EPSILON_DATE) then
            dtint = tsign*(timp(npf) - t)
            !!! tnext = timp(np,npf)
            if ( itsgn > 0 ) then
               choix_loi = 1
            else
               choix_loi = 2
            endif
         endif
         tnext = t + tsign*dtint
         xpasint = tsign*dtint
         
         !/ On (re)initialise l'intégrateur si : 
         !/  1) on a un pas différent du pas précédent
         !/  2) on a un nouveau pas > 0 ..
         !/  3) .. et on intègre sur une durée non nulle
         !/
         !/ Note : le cas avec durée nulle ne nécessite pas d'intégration,
         !/ et aucun warning ne sera émis (cf FA-ID 1100)
         if (.not.(pas_integ_propulsion(iveh) .egal. xpasint) .and. &
              .not.(xpasint .egal. 0._pm_reel) &
              .and. (abs(tnext - t) > MSP_EPSILON_DATE)) then
            
            if (init_integ_propulsion(iveh) /= 0) then

               call mu_liberer_integ(integ_propulsion(iveh), code_retour)
               if(code_retour%valeur < 0) then
                  call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_ERR_LIBERATION_MSPRO", &
                       partie_variable = "Gill / Runge-Kutta")
                  return
               end if
               if(code_retour%valeur > 0) then 
                  call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_WARN_LIBERATION_MSPRO", &
                       partie_variable = "Gill / Runge-Kutta")
               end if
               init_integ_propulsion(iveh) = 0
            end if
            
         end if
         
         if ( init_integ_propulsion(iveh) == 0 .and. .not.(xpasint .egal. 0._pm_reel)) then
            
            pas_integ_propulsion(iveh) = abs(xpasint)

            call mu_creer_integrateur(type_integrateur=pm_gill, n=8, &
                 pas=pas_integ_propulsion(iveh), integrateur=integ_propulsion(iveh), &
                 code_retour=code_retour)
           
            if(code_retour%valeur < 0) then
               call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_ERR_INIT_MSPRO", &
                    partie_variable = "Gill / Runge-Kutta")
               return
            end if
            if(code_retour%valeur > 0) then 
               call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_WARN_INIT_MSPRO", &
                    partie_variable = "Gill / Runge-Kutta")
            end if

            init_integ_propulsion(iveh) = 1

         end if
         
         !/ Intégration par la méthode de Runge-Kutta (Gill)
         !/ avec l'objet intégrateur spécifique au module propulsion
         if(abs(tnext-t) > MSP_EPSILON_DATE) then
            if (init_integ_propulsion(iveh) == 0) then
               ! Emission d'une erreur si l'intégrateur n'est pas initialisé
               call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_ERR_INTEG_MSPRO", &
                    partie_variable = "Gill / Runge-Kutta")
               return
            end if

            call mu_integrer(pssecmp, integ_propulsion(iveh), t, &
                 ve, tnext, ve_out, date_sortie_integ, ier, &
                 y_fsub, code_retour)
                 ve = ve_out
            if(code_retour%valeur < 0) then
               call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_ERR_INTEG_MSPRO", &
                    partie_variable = "Gill / Runge-Kutta")
               return
            end if
            if(code_retour%valeur > 0) then 
               call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_WARN_INTEG_MSPRO", &
                    partie_variable = "Gill / Runge-Kutta")
            end if

         end if
         !/ La date courante est la date de sortie d'intégration..
         !/ Note : si tout se passe bien, date_sortie_integ == t +/- xpasint = tnext
         !/ On teste l'écart entre date théorique et sortie de l'intégrateur.
         !/ Au delà de la précision sur les dates, on affecte la valeur sortie par l'intégrateur.
         !/ Sinon, on garde la date "théorique" et on évite ainsi des différences numériques non
         !/ significatives sur les dates.
         if( abs(date_sortie_integ - tnext) > MSP_EPSILON_DATE) then
            t = date_sortie_integ
         else
            t = tnext
         end if
         
         r2 = ve(1)**2+ve(2)**2+ve(3)**2
         v2 = ve(4)**2+ve(5)**2+ve(6)**2
         ray = sqrt(r2)
         unsura = 2._pm_reel/ray - v2/str_mod(iveh)%gmu

!        POSITIONNEMENT DE L'INDICATEUR D'ORBITE HYPERBOLIQUE
         if ( unsura >= 0 ) then
            str_gen(iveh)%hyp=0
         else
            str_gen(iveh)%hyp=1
         endif

         xmerg_21=ve(7)

         ! - Contrôle que la masse > 0
         if (ve(8) > eps) then
	    masse=ve(8)
            ! Modification de la structure véhicule
            call MSP_modifier_mci(mci,mstr=masse)
            call MSP_modifier_vehicule(str_car(iveh)%vehicule,mci=mci)
         else
            call MSP_signaler_message (cle_mes="PS_MASSE_001",routine="pspcont")
            return
         endif


         date = str_bul(iveh)%datbul0_js + t
         iter=iter+1
         if ((str_eve(iveh)%neve /= 0) .and. (str_eve(iveh)%neve /= (str_eve(iveh)%ndeve + 1))) then
            if (tnext .egal. str_eve(iveh)%deve(str_eve(iveh)%neve)) then
               !Cas ou on atteint un evenement:
               iterw = iterw - 1
               iter  = iter  - 1
               if (ilogeve /= 0) then
	       
                  call ps_maj_vecteur_etat(t,date,ve,choix_loi_atti=choix_loi)
                  if ( MSP_gen_messages("pspcont") ) return 
                  call pswresu (2,ilogeve,0,40,iter,t,str_gen(iveh)%etat,str_ecr(iveh)%nvarw,&
                                str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,&
                                str_eve(iveh)%ifore,str_eve(iveh)%neve,str_eve(iveh)%aeve)
                  if ( MSP_gen_messages("pspcont") ) return 
               endif
               str_eve(iveh)%neve = str_eve(iveh)%neve + itsgn
            endif
         endif
         if (abs(tnext - timp(npf)) < MSP_EPSILON_DATE) then
            !Cas ou on atteint la fin de la pousee:
            lboucl = .false.
            if (ilogeve /= 0) then
	    
               call ps_maj_vecteur_etat(t,date,ve,choix_loi_atti=choix_loi)
               if ( MSP_gen_messages("pspcont") ) return 
               call pswresu (1,ilogeph,iscreen,13,iter,t,str_gen(iveh)%etat,&
                             str_ecr(iveh)%nvarw,str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,&
                             str_eve(iveh)%ifore,str_eve(iveh)%neve,str_eve(iveh)%aeve)
               if ( MSP_gen_messages("pspcont") ) return 
               if (itsgn > 0) then
                  if (xmerg_21 < eps) then
                     call pswresu (2,ilogeve,0,14,iter,t,str_gen(iveh)%etat,&
                                   str_ecr(iveh)%nvarw,str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,&
                                   str_eve(iveh)%ifore,str_eve(iveh)%neve,str_eve(iveh)%aeve)
                  else
                     call pswresu (2,ilogeve,0,13,iter,t,str_gen(iveh)%etat,&
                                   str_ecr(iveh)%nvarw,str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,&
                                   str_eve(iveh)%ifore,str_eve(iveh)%neve,str_eve(iveh)%aeve)
                  endif
               else
                  call pswresu (2,ilogeve,0,12,iter,t,str_gen(iveh)%etat,&
                                str_ecr(iveh)%nvarw,str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,&
                                str_eve(iveh)%ifore,str_eve(iveh)%neve,str_eve(iveh)%aeve)
               endif
               if ( MSP_gen_messages("pspcont") ) return 
            endif
            if (iscreen == 1) then
               if (itsgn > 0) then
                  if (xmerg_21 < eps) write (PS_ECRAN,Domtraduire(nomdomaine,"PS_PRO_03"))
                  write (PS_ECRAN,Domtraduire(nomdomaine,"PS_PRO_04")) iloi_scenar_pro(iveh)
               else
                  write (PS_ECRAN,Domtraduire(nomdomaine,"PS_PRO_02")) iloi_scenar_pro(iveh)
               endif
               call flush (PS_ECRAN)
            endif
            nimp(iloi_scenar_pro(iveh)) = 0
            if (ilogeve /= 0) then
	    
               call ps_maj_vecteur_etat (t,date,ve)
               if ( MSP_gen_messages("pspcont") ) return 

               call pswresu (1,ilogeph,0,13,iter,t,str_gen(iveh)%etat,str_ecr(iveh)%nvarw,&
                             str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,&
                             str_eve(iveh)%ifore,str_eve(iveh)%neve,str_eve(iveh)%aeve)
               if ( MSP_gen_messages("pspcont") ) return 
            endif
         else if (abs(tnext - tmax) < MSP_EPSILON_DATE)then
            !Cas ou on atteint tmax:
            lboucl = .false.
            kflag  = 1
         else
            iterw = iterw + 1
            if (ilogeph /= 0) then
               if (str_ecr(iveh)%ipas == 2 ) then
	       
                  call ps_maj_vecteur_etat (t,date,ve,choix_loi_atti=choix_loi)
                  if ( MSP_gen_messages("pspcont") ) return    
                  call pswresu (1,ilogeph,iscreen,0,iter,t,str_gen(iveh)%etat,&
                       str_ecr(iveh)%nvarw,str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,&
                       str_eve(iveh)%ifore,str_eve(iveh)%neve,str_eve(iveh)%aeve)
                  if ( MSP_gen_messages("pspcont") ) return   
               else
                  if (mod(iterw,str_ecr(iveh)%indw) == 0) then

                     call ps_maj_vecteur_etat (t,date,ve,choix_loi_atti=choix_loi)
                     if ( MSP_gen_messages("pspcont") ) return    
                     call pswresu (1,ilogeph,iscreen,0,iter,t,str_gen(iveh)%etat,&
                          str_ecr(iveh)%nvarw,str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,&
                          str_eve(iveh)%ifore,str_eve(iveh)%neve,str_eve(iveh)%aeve)
                     if ( MSP_gen_messages("pspcont") ) return
                  endif
               endif
            endif
         endif
         ralt = sqrt(ve(1)**2 + ve(2)**2 + ve(3)**2) - str_mod(iveh)%requa
         if (ralt <= hstop) then
            !Cas ou  on atteint hstop:
            kflag  = 2
            lboucl = .false.
         end if
      enddo

!   ********************************************************************
!   * Fin de traitement de la poussee continue                         *
!   ********************************************************************

      par(1:6)=ve(1:6)

      call MSP_modifier_poussee_continue (loi_pro, merg=xmerg_21)
      if ( MSP_gen_messages("pspcont") ) return
      ! Modification du scenario en remplacant l'ancienne loi par la nouvelle
      call MSP_modifier_scenario(scenar_pro(iveh), loi_cont=loi_pro, id=iloi_scenar_pro(iveh))
      if ( MSP_gen_messages("pspcont") ) return


      ! Libération de l'intégrateur
      call mu_liberer_integ(integ_propulsion(iveh), code_retour)
      if(code_retour%valeur < 0) then
         call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_ERR_LIBERATION_MSPRO", &
              partie_variable = "Gill / Runge-Kutta")
         return
      end if
      if(code_retour%valeur > 0) then 
         call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_WARN_LIBERATION_MSPRO", &
              partie_variable = "Gill / Runge-Kutta")
      end if

      ! remise à 0 du flag d'initialisation
      init_integ_propulsion(iveh) = 0

      ! Libération de la mémoire
      call MSP_effacer_poussee_continue (loi_pro)
      if ( MSP_gen_messages("pspcont") ) return

      if (associated(timp)) deallocate (timp, stat=allocstat)
      if (associated(fptab)) deallocate (fptab, stat=allocstat)
      if (associated(omtab)) deallocate (omtab, stat=allocstat)
      if (associated(omptab)) deallocate (omptab, stat=allocstat)

      end subroutine pspcont

      subroutine pssecmp (t,x,xp,ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  pssecmp
!
!$Resume
!  Sous-programme calculant le second membre de (Xp,Xpp)=(vit,acc) avec poussée.
!
!$Description
!  Sous-programme calculant le second membre de (Xp,Xpp)=(vit,acc) avec poussée.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call pssecmp (t,x,xp,ier)
!.    real (KIND=pm_reel) :: t
!.    real (KIND=pm_reel), dimension(:) :: x
!.    real (KIND=pm_reel), dimension(:) :: xp
!.    integer :: ier
!
!$Arguments
!>E     t    :<pm_reel,DIM=(IN)>   variable indépendante (temps)
!>E     x    :<pm_reel,DIM=(:)>    vecteur d'état
!>S     xp   :<pm_reel,DIM=(:)>    dérivée du vecteur d'état
!>S     ier  :<integer>            code retour (0: OK, -1 : erreur psattit, -2 erreur psforce, -3 :erreur dans le calcul de la force de poussée)
!
!$Common
!
!$Routines
!- MSP_consulter_vehicule
!- MSP_modifier_mci
!- MSP_modifier_vehicule
!- MSP_consulter_scenario
!- MSP_consulter_poussee_continue
!- MSP_effacer_poussee_continue
!- psattit
!- ps_force_modele_complet
!- psapous
!
!$Include
!
!$Module
!#V
!- ps_attitude
!- ps_modeles
!- ps_caracteristiques
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use ps_attitude
      use ps_modeles
      use ps_caracteristiques

      implicit none

      !-- Arguments compatibles avec les intégrateurs MSPRO
      !----------------------------------------------------
      real (KIND=pm_reel), intent(IN)                :: t
      real (KIND=pm_reel), dimension(:), intent(IN)  :: x
      real (KIND=pm_reel), dimension(:), intent(OUT) :: xp
      integer, intent(OUT)                           :: ier

      !-- Variables locales
      !----------------------------------------------------
      integer :: j
      type(tm_jour_sec)   :: date
      real (KIND=pm_reel) :: pos(3),vit(3),acc(3),accp(3)
      real (KIND=pm_reel) :: angle1,angle2,angle3,angi1,angi2,angi3,pgamav(3,3),pgamavp(3,3)
      real (KIND=pm_reel) :: fp,debit,omega,omegap,dt

      real (KIND=pm_reel), pointer :: timp(:)
      real (KIND=pm_reel), pointer :: fptab(:)
      real (KIND=pm_reel), pointer :: omtab(:)
      real (KIND=pm_reel), pointer :: omptab(:)
      real (KIND=pm_reel), pointer :: debtab(:)
      real (KIND=pm_reel) :: xmerg,masse
      real (KIND=pm_reel) :: paspro
      integer :: dirref
      type(MSP_POUSSEE_CONTINUE) :: loi_pro
      type(MSP_MCI) :: mci
      
      integer :: typa,repa,ntab
      integer :: choix_dir

      !-- Débit du code
      !-----------------------------------------------------
      debit = 0._pm_reel
      nullify(timp)
      nullify(fptab)
      nullify(omtab)
      nullify(omptab)
      nullify(debtab)

      pos(:)    = x(1:3)
      vit(:)    = x(4:6)
      xmerg_21 = x(7)
      
      masse = x(8)
      ! Modification de la structure véhicule
      call MSP_consulter_vehicule(str_car(iveh)%vehicule,mci=mci)
      call MSP_modifier_mci(mci,mstr=masse)
      call MSP_modifier_vehicule(str_car(iveh)%vehicule,mci=mci)


!/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
      call MSP_consulter_scenario (scenar_pro(iveh), loi_cont=loi_pro)
      if ( MSP_gen_messages("pssecmp") ) return
         
      call MSP_consulter_poussee_continue (loi_pro,  ntab=ntab, &
           dates=timp, &
           poussee=fptab, dirref=dirref,&
           omega=omtab, omegap=omptab,&
           merg=xmerg, pas=paspro, debit=debtab)
      if ( MSP_gen_messages("pssecmp") ) return

      ! Libération de la mémoire
      call MSP_effacer_poussee_continue (loi_pro)
      if ( MSP_gen_messages("pssecmp") ) return

      date = str_bul(iveh)%datbul0_js + t

!   * Calcul de l'attitude:

      call psattit (date,t,pos,vit,angle1,angle2,angle3,angi1,angi2,angi3,&
           pgamav,typa,repa,choix_loi=choix_loi)
      if ( MSP_gen_messages("pssecmp")) then
         ier = -1
         return    
      end if
!   * Calcul des accelerations "naturelles":

      call ps_force_modele_complet (date,pos,vit,pgamav,repa,acc)
      if ( MSP_gen_messages("pssecmp")) then
         ier = -2
         return    
      end if

!   * Calcul des accelerations dues a la poussee des moteurs:

      num_poussee: do j = (ntab-1),1,-1
         if (t .egal. timp(j)) then
            fp=fptab(j)
            omega=omtab(j)
            omegap=omptab(j)
            debit=debtab(j)
            exit num_poussee
         else if (t > timp(j)) then
            dt=(t-timp(j))/(timp(j+1)-timp(j))
            fp=fptab(j)+(fptab(j+1)-fptab(j))*dt
            omega=omtab(j)+(omtab(j+1)-omtab(j))*dt
            omegap=omptab(j)+(omptab(j+1)-omptab(j))*dt
            debit=debtab(j)+(debtab(j+1)-debtab(j))*dt
            exit num_poussee
         end if
      enddo num_poussee

      if ( dirref >= 0 ) then

         ! La direction de poussée est donnée par rapport à un autre repère
         choix_dir = -dirref
         call psattit (date,t,pos,vit,angle1,angle2,angle3,angi1,angi2,angi3,pgamavp,typa,repa,choix_loi=choix_dir)
         if ( MSP_gen_messages("pssecmp") ) return

         call psapous (pgamavp,fp,omega,omegap,accp)

      else
         ! La direction de poussée est donnée par rapport au repère véhicule
         call psapous (pgamav,fp,omega,omegap,accp)

      endif
         
      if ( MSP_gen_messages("pssecmp") .and. MSP_ERREUR) then 
         ier = -3
         return
      end if

      xp(1:3) = vit(:)
      xp(4:6) = acc(:) + accp(:)
      xp(7)   = -debit
      xp(8)   = xp(7)

      ier = 0
      
      ! Libération de la mémoire
      if (associated(timp)) deallocate(timp)
      if (associated(fptab)) deallocate(fptab)
      if (associated(omtab)) deallocate(omtab)
      if (associated(omptab)) deallocate(omptab)
      if (associated(debtab)) deallocate(debtab)

      end subroutine pssecmp

      subroutine psnewbl (itsgn,tsign,t,date,par,iter)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psnewbl
!
!$Resume
!  Sous-programme calculant un changement de trajectoire du a une manoeuvre de type "bulletin".
!
!$Description
!  Sous-programme calculant un changement de trajectoire du a une manoeuvre de type "bulletin".
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psnewbl (itsgn,tsign,t,date,par,iter)
!.    integer :: itsgn
!.    real (KIND=pm_reel) :: tsign
!.    real (KIND=pm_reel) :: t,par(6)
!.    type (tm_jour_sec) :: date
!.    integer :: iter
!
!$Arguments
!>E     itsgn  :<integer>               sens de l'intégration:
!.                                     1 => posigrade
!.                                    -1 => rétrograde
!>E     tsign  :<pm_reel>               sens de l'intégration:
!.                                     1 => posigrade
!.                                    -1 => rétrograde   
!>E/S   t      :<pm_reel,DIM=(INOUT)>   instant de poussée (s)
!>E/S   date   :<tm_jour_sec>           date de poussee (Jours Juliens CNES) 
!>E/S   par    :<pm_reel,DIM=(6)>       position/vitesse en fin de poussée
!>E/S   iter   :<integer>               numéro de l'itération
!
!$Common
!
!$Routines
!- MSP_consulter_scenario
!- MSP_consulter_poussee_bulletin
!- ps_maj_vecteur_etat
!- pswresu
!- flush
!- MSP_consulter_vehicule
!- MSP_consulter_mci
!- MSP_modifier_mci
!- MSP_modifier_vehicule
!- MSP_signaler_message
!
!$Include
!
!$Module
!#V
!- ps_variables
!- ps_evenements
!- ps_caracteristiques
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      use ps_variables
      use ps_evenements
      use ps_caracteristiques

      implicit none
      
      ! Arguments
      !==========

      integer, intent(IN) :: itsgn
      real (KIND=pm_reel), intent(IN)  :: tsign
      real (KIND=pm_reel), intent(INOUT) :: t,par(6)
      type (tm_jour_sec) , intent(inout) :: date
      integer, intent(INOUT) :: iter

      ! Variables locales
      !==================

      integer :: k
      integer :: np
      type(tm_jour_sec)   :: date_ref,date_bul_js
      real(kind=pm_reel)  :: t_deb  ! durée depuis le début de la loi de poussée bulletin
      real (kind=pm_reel) :: xmerg, datprec_tmp, masse
      type (MSP_MCI) :: mci

      type(MSP_POUSSEE_BULLETIN) :: loi_bul

      ! Début du code
      !==============


!/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
      call MSP_consulter_scenario (scenar_pro(iveh), date_js=date_ref)
      if ( MSP_gen_messages("psnewbl") ) return
      call MSP_consulter_scenario (scenar_pro(iveh), loi_bul=loi_bul)
      if ( MSP_gen_messages("psnewbl") ) return
         
      !!! datbul correspond à la date du bulletin, datedeb à une durée 
      !!! correspondant au début de la poussée
      call MSP_consulter_poussee_bulletin (loi_bul, &
           datedeb=t_deb, datbul_js=date_bul_js, merg=xmerg)
      if ( MSP_gen_messages("psnewbl") ) return


      np = iloi_scenar_pro(iveh)

!     Calcul et ecriture des variables selectionnees:
!     ----------------------------------------------
      nimp(iloi_scenar_pro(iveh)) = 1
      if ( ilogeve /= 0 ) then

         call ps_maj_vecteur_etat (t,date,par)
         if ( MSP_gen_messages("psnewbl") ) return
         call pswresu (1,ilogeph,0,15,iter,t,str_gen(iveh)%etat,str_ecr(iveh)%nvarw,&
                       str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,str_eve(iveh)%ifore,&
                       str_eve(iveh)%neve,str_eve(iveh)%aeve)
         if ( MSP_gen_messages("psnewbl") ) return
         if (itsgn > 0) then
            call pswresu (2,ilogeve,0,15,iter,t,str_gen(iveh)%etat,str_ecr(iveh)%nvarw,&
                          str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,str_eve(iveh)%ifore,&
                          str_eve(iveh)%neve,str_eve(iveh)%aeve)
         else
            call pswresu (2,ilogeve,0,16,iter,t,str_gen(iveh)%etat,str_ecr(iveh)%nvarw,&
                          str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,str_eve(iveh)%ifore,&
                          str_eve(iveh)%neve,str_eve(iveh)%aeve)
         endif
         if ( MSP_gen_messages("psnewbl") ) return
      endif

      if ( iscreen == 1 ) then
         write (PS_ECRAN,Domtraduire(nomdomaine,"PS_PRO_06")) iloi_scenar_pro(iveh)
         call flush (PS_ECRAN)
      endif

!   * Stockage du bulletin courant si besoin

      if ( itsgn > 0 ) then
            parman_car(iloi_scenar_pro(iveh),:,1)=par(:)
      else
            parman_car(iloi_scenar_pro(iveh),:,2)=par(:)
      endif

!   * Nouveau vecteur vitesse dans Planeto:
      datprec_tmp = 0.001_pm_reel

      if (itsgn > 0) then
         k = 2
         t = (date_bul_js - date_ref)
         ! Arrondi à la milli-seconde:
         t = real(int(t),KIND=pm_reel) + &
              real(nint((t-int(t))/datprec_tmp),KIND=pm_reel)*datprec_tmp
      else
         k = 1
         t = t_deb
      endif
      ! date_bul_js et date_ref sont des dates jj/sec
      ! Leur soustraction donne une quantité en sec, et date jj/s + sec -> date jj/s
      date = date + tsign*((date_bul_js - date_ref) - t_deb)
      par(:)=parman_car(np,:,k)

      ! - Contrôle que la masse > 0
      call MSP_consulter_vehicule(str_car(iveh)%vehicule,mci=mci)
      call MSP_consulter_mci(mci,mstr=masse)


      if (masse > tsign*xmerg ) then
         masse=masse-tsign*xmerg

         ! Modification de la structure véhicule
         call MSP_modifier_mci(mci,mstr=masse)
         call MSP_modifier_vehicule(str_car(iveh)%vehicule,mci=mci)

      else
         call MSP_signaler_message (cle_mes="PS_MASSE_001",routine="psnewbl")
         return
      endif

      nimp(np) = 0
      if ( ilogeve /= 0 ) then

         call ps_maj_vecteur_etat (t,date,par)
         if ( MSP_gen_messages("psnewbl") ) return
         call pswresu (1,ilogeph,iscreen,16,iter,t,str_gen(iveh)%etat,str_ecr(iveh)%nvarw,&
                       str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,&
                       str_eve(iveh)%ifore,str_eve(iveh)%neve,str_eve(iveh)%aeve)
         if ( MSP_gen_messages("psnewbl") ) return
         if (itsgn > 0) then
            call pswresu (2,ilogeve,0,16,iter,t,str_gen(iveh)%etat,str_ecr(iveh)%nvarw,&
                          str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,str_eve(iveh)%ifore,&
                          str_eve(iveh)%neve,str_eve(iveh)%aeve)
         else
            call pswresu (2,ilogeve,0,15,iter,t,str_gen(iveh)%etat,str_ecr(iveh)%nvarw,&
                          str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,str_eve(iveh)%ifore,&
                          str_eve(iveh)%neve,str_eve(iveh)%aeve)
         endif
         if ( MSP_gen_messages("psnewbl") ) return
      endif

      end subroutine psnewbl

      subroutine psinipc ()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psinipc
!
!$Resume
!  Réordonnancement des lois de poussée.
!
!$Description
!  Réordonnancement des lois de poussée.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psinipc ()
!
!$Arguments
!
!$Common
!
!$Routines
!- MSP_consulter_scenario
!- MSP_signaler_message
!- MSP_consulter_poussee_continue
!- MSP_modifier_poussee_continue
!- MSP_modifier_scenario
!- flush
!- MSP_effacer_poussee_continue
!- MSP_ajouter_impulsion
!- MSP_ajouter_poussee_continue
!- MSP_ajouter_poussee_bulletin
!- MSP_supprimer_liste_loi
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

      ! Variables locales
      !==================

      logical :: jfin,nop(ps_nloi_propu)
      integer :: i,j,k
      real (KIND=pm_reel) :: ac,b,b2,ddeb,de,dt,ddt,t0,t11,t12
      integer :: sc_type, sc_nloi, typloi, ntab, allocstat
      real (KIND=pm_reel), pointer :: timp(:)
      real (KIND=pm_reel), pointer :: fptab(:)
      real (KIND=pm_reel), pointer :: omtab(:)
      real (KIND=pm_reel), pointer :: omptab(:)
      real (KIND=pm_reel), pointer :: debtab(:)
      real (KIND=pm_reel), pointer :: xmerg0(:)
      real (KIND=pm_reel) :: xmerg
      real (KIND=pm_reel) :: paspro
      type(MSP_SCENARIO_LOI)    :: scenar_pro_reorg
      type(MSP_IMPULSION) :: loi_imp
      type(MSP_POUSSEE_CONTINUE) :: loi_cont
      type(MSP_POUSSEE_BULLETIN) :: loi_bul

!   ********************************************************************
!   * Calcul de l'evolution de xmerg pour les lois de poussee continue *
!   ********************************************************************

    nullify(timp)
    nullify(fptab)
    nullify(omtab)
    nullify(omptab)
    nullify(debtab)
    nullify(xmerg0)

!/  Lecture de la structure MSP_SCENARIO_LOI
      call MSP_consulter_scenario (scenar_pro(iveh), type=sc_type, nloi=sc_nloi)
      if ( MSP_gen_messages("psinipc") ) return
      
      if ( sc_type /= MSP_ENUM_PROPULSION ) then
!/  Si le scenario n'est pas de type propulsion
         call MSP_signaler_message (cle_mes="PS_CONV_003")
         return
      endif

      do i = 1 , sc_nloi
    
         nop(i) = .true.
         !/  Extraction du type de la loi courante
         typloi = MSP_type_loi (scenar_pro(iveh), id=i)
         if ( MSP_gen_messages("psinipc") ) return
         if (typloi == MSP_ENUM_LOI_CONT) then
     
!/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
            call MSP_consulter_scenario (scenar_pro(iveh), loi_cont=loi_cont, id=i)
            if ( MSP_gen_messages("psinipc") ) return
         
            call MSP_consulter_poussee_continue (loi_cont,  ntab=ntab, &
                 dates=timp, &
                 poussee=fptab,  &
                 omega=omtab, omegap=omptab,&
                 merg=xmerg, pas=paspro, debit=debtab)
            if ( MSP_gen_messages("psinipc") ) return

            if (ASSOCIATED(xmerg0)) deallocate(xmerg0, stat=allocstat)
            ALLOCATE(xmerg0(ntab))
                
            if (xmerg > 0._pm_reel) then
               xmerg0(1) = xmerg
               j = 1
               jfin = .false.
               do while (.not. jfin)
                  j = j+1
                  dt = timp(j)-timp(j-1)
                  xmerg0(j)=(debtab(j)+debtab(j-1))*dt/2._pm_reel
                  xmerg0(j)=xmerg0(j-1)-xmerg0(j)
                  if (xmerg0(j) < 0._pm_reel) then
                     ddeb = debtab(j) - debtab(j-1)
                     if (fptab(j) .egal. fptab(j-1)) then
                        t0 = timp(j-1) + xmerg0(j-1)/debtab(j-1)
                     else
                        b  = debtab(j-1)*dt
                        b2 = b*b
                        ac = ddeb*2._pm_reel*dt*xmerg0(j-1)
                        de = b2+ac
                        if ( de < 0._pm_reel ) then
                           call MSP_signaler_message (cle_mes="PS_INIPC_001")
                           return
                        else
                           t11=timp(j-1)-(b+sqrt(de))/ddeb
                           t12=timp(j-1)-(b-sqrt(de))/ddeb
                           if ( (timp(j-1) <= t11) .and. (t11 <= timp(j))  ) then
                              t0 = t11
                           else if ( (timp(j-1) <= t12) .and. (t12 <= timp(j))  ) then
                              t0 = t12
                           else
                              call MSP_signaler_message (cle_mes="PS_INIPC_002")
                              return
                           endif
                        endif
                     endif
                     ddt = (t0-timp(j-1))/dt
                     debtab(j) = debtab(j-1)+ddeb*ddt
                     ddeb = fptab(j) - fptab(j-1)
                     fptab(j) = fptab(j-1)+ddeb*ddt
                     ddeb = omtab(j) - omtab(j-1)
                     omtab(j) = omtab(j-1)+ddeb*ddt
                     ddeb = omptab(j) - omptab(j-1)
                     omptab(j) = omptab(j-1)+ddeb*ddt
                     timp(j) = t0
                     ntab = j
                     xmerg0(j) = 0._pm_reel
                     call MSP_modifier_poussee_continue(loi_cont, &
                          dates=timp, &
                          poussee=fptab,  &
                          omega=omtab, omegap=omptab, debit=debtab)
                     if ( MSP_gen_messages("psinipc") ) return
                     ! Modification du scenario en remplacant l'ancienne loi par la nouvelle
                     call MSP_modifier_scenario(scenar_pro(iveh), loi_cont=loi_cont, id=i)
                     if ( MSP_gen_messages("psinipc") ) return
                  endif
                  if (j == ntab) then
                     jfin = .true.
                     dt = abs(timp(ntab) - timp(1))
                     if (paspro >= dt) then
                        paspro = dt / 2._pm_reel         
                        call MSP_modifier_poussee_continue(loi_cont, pas=paspro)
                        if ( MSP_gen_messages("psinipc") ) return
                        ! Modification du scenario en remplacant l'ancienne loi par la nouvelle
                        call MSP_modifier_scenario(scenar_pro(iveh), loi_cont=loi_cont, id=i)
                        if ( MSP_gen_messages("psinipc") ) return
                     endif
                     !/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
                     if (i < sc_nloi) then
                        !/  Extraction du type de la loi
                        typloi = MSP_type_loi (scenar_pro(iveh), id=i+1)
                        if ( MSP_gen_messages("psinipc") ) return
                        if (typloi == MSP_ENUM_LOI_CONT) then
     
!/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
                           call MSP_consulter_scenario (scenar_pro(iveh), loi_cont=loi_cont, id=i+1)
                           if ( MSP_gen_messages("psinipc") ) return
         
                           call MSP_consulter_poussee_continue (loi_cont, merg=xmerg)
                           if ( MSP_gen_messages("psinipc") ) return

                           if ( xmerg .egal. 0._pm_reel) then
                              xmerg=xmerg0(j)
                              call MSP_modifier_poussee_continue(loi_cont, merg=xmerg)
                              if ( MSP_gen_messages("psinipc") ) return
                        
                              ! Modification du scenario en remplacant l'ancienne loi par la nouvelle
                              call MSP_modifier_scenario(scenar_pro(iveh), loi_cont=loi_cont, id=i+1)
                              if ( MSP_gen_messages("psinipc") ) return
                           endif
                        endif
                     endif
                     !!! if ( timp(i,ntab(i)) <= 0._pm_reel ) xmerg(i)=xmerg0(j)
                  endif
               enddo
            else
               nop(i) = .false.

               write (PS_ECRAN,Domtraduire(nomdomaine,"PS_PRO_05")) i
               call flush (PS_ECRAN)
               
            endif
            DEALLOCATE(xmerg0)

         endif

      enddo 

!   ********************************************************************
!   * Reorganisation des donnees                                       *
!   ********************************************************************

!/  Creation d'un scenario de type propulsion
      scenar_pro_reorg = MSP_creer_scenario (MSP_ENUM_PROPULSION,date_ref=str_bul(iveh)%datbul0,nom="")
      if ( MSP_gen_messages("psconv_pspro_pro") ) return

      ! Deallocation
      call MSP_effacer_poussee_continue(loi_cont)

      k=0
      do i = 1,sc_nloi
         if (nop(i)) then
            k=k+1
            !/  Extraction du type de la loi courante
            typloi = MSP_type_loi (scenar_pro(iveh), id=i)
            if ( MSP_gen_messages("psinipc") ) return
            if (typloi == MSP_ENUM_LOI_IMP) then

               !/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
               call MSP_consulter_scenario(scenar_pro(iveh), loi_imp=loi_imp, id=i)
               if ( MSP_gen_messages("psinipc") ) return

               call MSP_ajouter_impulsion(scenar_pro_reorg, "", loi=loi_imp)
               if ( MSP_gen_messages("psinipc") ) return

            else if (typloi == MSP_ENUM_LOI_CONT) then
               !/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
               call MSP_consulter_scenario(scenar_pro(iveh), loi_cont=loi_cont, id=i)
               if ( MSP_gen_messages("psinipc") ) return
            
               call MSP_ajouter_poussee_continue(scenar_pro_reorg, "", loi=loi_cont)
               if ( MSP_gen_messages("psinipc") ) return

            else

               !/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
               call MSP_consulter_scenario(scenar_pro(iveh), loi_bul=loi_bul, id=i)
               if ( MSP_gen_messages("psinipc") ) return
 
               call MSP_ajouter_poussee_bulletin(scenar_pro_reorg, "", loi=loi_bul)
               if ( MSP_gen_messages("psinipc") ) return
            endif
         endif
         
      enddo

      nloi_scenar_pro = k
      scenar_pro(iveh) = scenar_pro_reorg

      ! Libération de la mémoire
      if (associated(timp))   deallocate (timp)
      if (associated(fptab))  deallocate (fptab)
      if (associated(omtab))  deallocate (omtab)
      if (associated(omptab)) deallocate (omptab)
      if (associated(debtab)) deallocate (debtab)
      if (associated(xmerg0)) deallocate (xmerg0)

      call MSP_supprimer_liste_loi(scenar_pro_reorg)
      if ( MSP_gen_messages("psinipc") ) return
      call MSP_effacer_poussee_continue (loi_cont)
      if ( MSP_gen_messages("psinipc") ) return


      end subroutine psinipc


      subroutine pslecpro(ficpro,paspro,pro)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  pslecpro
!
!$Resume
!  Sous-programme de lecture du fichier de propulsion et de mise à jour
!  du scenario MECASPA avec la loi de poussee continue lue
!
!$Description
!  Sous-programme de lecture du fichier de propulsion et de mise à jour
!  du scenario MECASPA avec la loi de poussee continue lue
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call pslecpro(ficpro,paspro,pro)
!.    character(LEN=80) :: ficpro
!.    real (KIND=pm_reel) :: paspro
!.    type(MSP_SCENARIO_LOI) :: pro
!
!$Arguments
!>E     ficpro  :<LEN=80>             
!>E     paspro  :<pm_reel>            
!>E/S   pro     :<MSP_SCENARIO_LOI>   
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- MSP_consulter_scenario
!- MSP_ajouter_poussee_continue
!
!$Include
!
!$Module
!#V
!- ps_generalites
!- ps_bulletin
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use ps_generalites
      use ps_bulletin

      implicit none

      character(LEN=80), intent(IN)  :: ficpro
      real (KIND=pm_reel), intent(IN)  :: paspro
      type(MSP_SCENARIO_LOI), intent(INOUT) :: pro

      real (KIND=pm_reel), dimension(:), pointer :: timp
      real (KIND=pm_reel), dimension(:), pointer :: fptab
      real (KIND=pm_reel) :: xisp
      integer   :: dirref
      real (KIND=pm_reel), dimension(:), pointer :: omtab
      real (KIND=pm_reel), dimension(:), pointer :: omptab
      real (KIND=pm_reel) :: xmerg

      integer :: ier,longueur,acces
      integer :: ipts_pro=0, i=0
      integer :: npts_pro

      character(LEN=256) :: rctmp
      integer :: itypdat
      integer :: jjtimpori
      real (KIND=pm_reel) :: sectimpori, timp1
      integer, dimension(:), pointer :: jjtimp
      real (KIND=pm_reel), dimension(:), pointer :: sectimp
      real (KIND=pm_reel) :: date_ref

      real (KIND=pm_reel) :: g0
      data g0 / 9.80665_pm_reel /

      ! Récupération des données incluses dans les fichiers de ressource
            
      ! Le répertoire data_psimu est lu dans le fichier de ressources modifiable
      ier = AMv_rc_get ('data_psimu','psimu','','data_psimu',rctmp,longueur)
      if ( ier < 0 ) then
         call MSP_signaler_message (cle_mes="PS_LECT_FIC_CONF",&
              partie_variable="data_psimu",routine="pslecpro")
         return
      else
         dirdat = rctmp(1:longueur)
      endif

      ! Ouverture du fichier de propulsion
      acces = acc_open()
      ier = acc_connect (acces, dirdat(1:LEN_TRIM(dirdat))//"/"//ficpro(1:LEN_TRIM(ficpro)), ACC_R)

      ier = acc_read(acces, ACC_HEADER)

      ! Lecture du type de date
      ier = acc_geti (acces,"typdat",itypdat)
      if ( ier /= 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='du type de date', &
              routine="pslecpro",type=MSP_ENUM_ERREUR)
         return
      endif

      ! Lecture de JJ date dorigine
      ier = acc_geti (acces,"jjtimpori",jjtimpori)
      if ( ier /= 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='de date origine', &
              routine="pslecpro",type=MSP_ENUM_ERREUR)
         return
      endif

      ! Lecture de sec date origine
      ier = acc_getd (acces,"sectimpori",sectimpori,"s")
      if ( ier /= 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='de date origine', &
              routine="pslecpro",type=MSP_ENUM_ERREUR)
         return
      endif

      ! Lecture de la référence pour la direction de poussée
      ier = acc_geti (acces,"dirref",dirref)
      if ( ier /= 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='de la ref dir poussee', &
              routine="pslecpro",type=MSP_ENUM_ERREUR)
         return
      endif
      
      ! Lecture de l'impulsion specifique
      ier = acc_getd (acces,"xisp",xisp,"s")
      if ( ier /= 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='de l impulsion specifique', &
              routine="pslecpro",type=MSP_ENUM_ERREUR)
         return
      endif
      
      ! Lecture de la masse d ergol
      ier = acc_getd (acces,"xmerg",xmerg,"kg")
      if ( ier /= 0 ) then
         call MSP_signaler_message (cle_mes="MSP_ERREUR_LECTURE",partie_variable='de la masse d ergol', &
              routine="pslecpro",type=MSP_ENUM_ERREUR)
         return
      endif

      ! Lecture du fichier pour connaitre le nombre de lignes
       do while(acc_read(acces,ACC_NEXT) .ne. ACC_EOF)
         ipts_pro = ipts_pro+ 1
      enddo

      npts_pro = ipts_pro

      ! allocation des tableaux de donnees
      allocate(timp(npts_pro))
      allocate(fptab(npts_pro))
      allocate(omtab(npts_pro))
      allocate(omptab(npts_pro))
      allocate(jjtimp(npts_pro))
      allocate(sectimp(npts_pro))

      ! Retour en debut de fichier
      ier = acc_deconnect(acces, ACC_R)
      ier = acc_close(acces)

      acces = acc_open()
      ier = acc_connect (acces, dirdat(1:LEN_TRIM(dirdat))//"/"//ficpro(1:LEN_TRIM(ficpro)), ACC_R)

      ier = acc_read(acces, ACC_HEADER)

     ! Lecture des colonnes jjtimp/sectimp/fptab/omtab/omptab pour typdat=1
     ! Lecture des colonnes timp/fptab/omtab/omptab pour typdat=2 ou 3

      do while(acc_read(acces,ACC_NEXT) .ne. ACC_EOF)
         i = i+ 1
         if (itypdat .eq. 1) then !date absolue
            ier = acc_geti(acces,"jjtimp",jjtimp(i))
            ier = acc_getd(acces,"sectimp",sectimp(i) , "s")
         else
            ier = acc_getd(acces,"timp",timp(i) , "s")
         endif
         ier = acc_getd(acces,"fptab",fptab(i) , "N")
         ier = acc_getd(acces,"omtab",omtab(i) , "rad")
         ier = acc_getd(acces,"omptab",omptab(i) , "rad")
      enddo

      ! recuperation de la date de reference du scenario
      call MSP_consulter_scenario (pro, date_ref=date_ref)
      if ( MSP_gen_messages("pslecpro") ) return
     
      ! test du type de date
      if (itypdat.eq.1) then  !absolu
         timp(1:npts_pro) = (jjtimp(1:npts_pro)-date_ref)*86400._pm_reel &
              +sectimp(1:npts_pro)
         
      else if (itypdat.eq.3) then !absolu relatif
         timp1 = (jjtimpori-date_ref)*86400._pm_reel&
              + sectimpori
         
         timp(2:npts_pro) = timp1 + timp(1:(npts_pro-1))     
         timp(1) = timp1

      end if
 
!/  Ajout d'une loi de propulsion sur fichier dans la structure MSP_SCENARIO_LOI
      call MSP_ajouter_poussee_continue (pro,"",ntab=npts_pro,&
           typdat=MSP_ENUM_LOI_RELATIVE, &
           dates=timp(1:npts_pro),&
           poussee=fptab(1:npts_pro), &
           isp=xisp,omega=omtab(1:npts_pro), &
           dirref=dirref, omegap=omptab(1:npts_pro), &
           merg=xmerg, pas=paspro, &
           debit=fptab(1:npts_pro)/(g0*xisp))
      deallocate(timp)
      deallocate(fptab)
      deallocate(omtab)
      deallocate(omptab)
      deallocate(jjtimp)
      deallocate(sectimp)

      ier = acc_deconnect(acces, ACC_R)
      ier = acc_close(acces)
 
    end subroutine pslecpro

end module ps_propulsion
