module ps_modeles

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ps_modeles
!
!$Resume
!  Module "chapeau" gérant le calcul des forces non propulsives.
!
!$Description
!  Module "chapeau" gérant le calcul des forces non propulsives.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: ps_modeles.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ps_modeles.F90,v $
!  Revision 1.35  2010/10/25 13:10:59  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.34  2009/09/04 15:19:00  mercadig
!  DM-ID 1218: Mise a jour dans les appels a ps_propage
!
!  Revision 1.33  2008/12/04 13:56:10  tanguyy
!  DM-ID 733 : correction de la fonction de 2nd membre simplifie
!
!  Revision 1.32  2008/12/02 16:51:49  tanguyy
!  AQ : mise à jour des cartouches avant livraison de PSIMU V9.3
!
!  Revision 1.31  2008/12/02 10:47:55  huec
!  DM-ID 1058 : Suppression de variables inutilisees
!  Revision 1.30  2008/12/02 08:17:42  tanguyy
!  DM-ID 733 : correction mineures du code suite aux remarques de la relecture de code, mise en forme des cartouches
!  Revision 1.29  2008/11/26 09:04:14  tanguyy
!  DM-ID 733 : version initiale des nouvelles routines ps_force_modele_complet et ps_force_modele_simplifie
!  Revision 1.28  2008/09/04 07:53:11  tanguyy
!  DM-ID 1058 : phase 1 du portage / suppression des warnings - initialisations
!  Revision 1.27  2008/04/16 15:53:17  huec
!  DM-ID 859 : Utilisation de boucles explicites
!  Revision 1.26  2008/03/31 15:29:33  ttn
!  correction dans pspoter de la place de l'argument du SMC
!  Revision 1.25  2008/03/19 15:20:33  ttn
!  DM-ID 983 : Second membre simplifie
!  Revision 1.24  2008/03/07 09:58:41  huec
!  DM-ID 859 : Utilisation de matmul3, mulvecttrans3
!  Revision 1.23  2008/02/15 16:36:50  huec
!  DM-ID 11 : Suppression de l utilisation d un fichier de saut du TUC hors base COMPAS
!  Revision 1.22  2007/12/06 15:16:21  huec
!  DM-ID 733 : Annulation de la DM
!  Revision 1.21  2007/11/29 14:42:43  jpi
!  DM-ID 733 : annulation de la DM donc des modifs
!  Revision 1.20  2007/10/03 12:27:48  huec
!  DM-ID 733 : Extraction du calcul de la force de pression de radiation solaire
!  Revision 1.19  2007/10/02 08:01:59  tanguyy
!  DM-ID 733 - utilisationd du calcul du potentiel de la MECASPA
!  Revision 1.18  2007/07/09 12:22:21  tanguyy
!  DM-ID 688 : mode Terre/Mars géré par un entier (= code NAIF)
!  Revision 1.17  2007/06/20 12:33:32  vivaresf
!  Intégration FA 725 patch V8.7a3
!  Revision 1.16.2.2  2007/04/17 08:23:27  vivaresf
!  FA-ID 725 : optimisation des changements de repères des éphémérides
!  Revision 1.16.2.1  2007/04/16 09:42:53  vivaresf
!  FA-ID 725 : optimisation et robustesse du patch V8.7a3
!  meilleure gestion des allocate (désallocation + status)
!  desallocation du COWELL si ré-initialisation de l'intégration
!  Revision 1.16  2006/10/19 15:08:02  tanguyy
!  DM-ID 478 : Cloture du FT (Integrateur de Cowell : modification de l interface)
!  Revision 1.15.2.2  2006/10/17 09:54:26  tanguyy
!  Finalisation DM-ID 478 (AQ : suppression var inutilisees, commentaires)
!  Revision 1.15.2.1  2006/10/13 07:55:16  tanguyy
!  DM-ID 478 : utilisation jj/sec. 1ere version fonctionnelle
!  Revision 1.15  2006/03/15 13:25:20  tanguyy
!  Livraison PSIMU V8-4 ; relecture code / suppression code mort / maj cartouches
!  Revision 1.14  2005/11/10 18:37:08  vivaresf
!  Mise à jour des cartouches
!  Revision 1.13  2005/11/09 13:23:10  vivaresf
!  DM-ID 6 : remplacement de MSP_posvit_3corps par ps_propage
!  Revision 1.12  2005/01/28 10:38:56  fabrec
!  maj cartouches
!  Revision 1.11  2004/03/31 14:32:10  adm_ipsi
!  FA-ID 117, Utilisation de la sous-matrice (3,3) de Mat_RI_ME2000
!  Revision 1.10  2004/01/15 16:33:24  adm_ipsi
!  DM-ID 10, les calculs internes de PSIMU se font en date TE
!  Revision 1.9  2003/02/19 10:44:21  rodier
!  PhB - fic_tuc dans MSP_posvit_3corps
!  Revision 1.8  2003/01/31 12:33:18  boschett
!  A Deramecourt : suppression appels mumat3, muvec3, muvectr3 de la amlib
!  Revision 1.7  2002/12/20 16:40:00  boschett
!  Utilisation du traitement d'erreur de la MECASPA
!  Revision 1.6  2002/12/12 15:05:25  boschett
!  Livraison Intermediaire 16/12/2002
!  Revision 1.5  2002/12/10 15:25:28  boschett
!  Ajout du traitement par défaut dans la structure if/elseif/else de la subroutine psforce
!  Revision 1.4  2002/12/04 14:25:45  boschett
!  Suppression des instructions return inutiles en fin de routine
!  Revision 1.3  2002/12/02 17:05:05  boschett
!  Suppression des variables locales déclarées et non utilisées
!  Revision 1.2  2002/11/26 17:00:39  boschett
!  Ajout de implicit none
!  Revision 1.1.1.1  2002/09/30 14:59:34  laurent
!  Industrialisation PSIMU
!  Revision 1.8  2001/11/09 08:58:21  util_am
!  Appel de la routine MSP_posvit_3corps avec l'argument echt pour donner une date en TE
!  Revision 1.7  2000/06/27 11:42:40  util_am
!  Ajout des modes d'attitude LVLH et Yaw Steering
!  Revision 1.6  2000/04/19 13:06:40  util_am
!  Bug dû à un copier/coller sur phobos et deimos
!  Revision 1.5  2000/04/17 10:58:17  util_am
!  Version multi_satellite en Fortran90
!  Revision 1.4  1999/10/26 10:56:08  util_am
!  Prise en compte du 3ème corps
!  Revision 1.3  1999/08/31 11:56:12  util_am
!  Prise en compte des nouvelles échelles de date et de temps
!  Revision 1.2  1999/08/04 11:28:16  util_am
!  Prise en compte de la gestion des erreurs de MECASPA
!
!$FinHistorique
!
!$Usage
!  use ps_modeles
!
!$Structure
!
!$Global
!
!$Common
!
!$Routines
!- ps_force_modele_complet
!- ps_force_modele_simplifie
!
!$Fonctions
!
!$Include
!
!$Module
!#V
!- MECASPA
!- ps_troiscorps
!- ps_atmosphere
!- ps_psolaire
!- ps_calcul_forces
!- ps_generalites
!- ps_bulletin
!- ps_integration_don
!- ps_caracteristiques
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

   use ps_troiscorps
   use ps_atmosphere
   use ps_psolaire
   use ps_calcul_forces

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ps_modeles.F90 69 2012-09-11 08:33:34Z ffsm $'

   
   contains

      subroutine ps_force_modele_complet (date,Pos,Vit,Pgamav,irepa,Acc)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_force_modele_complet
!
!$Resume
!  Calcul de l'accélération produite par le modèle de forces utilisé.
!
!$Description
!  Calcul de l'accélération produite par le modèle de forces utilisé.
!  Cette routine gère un modèle de forces complet, pouvant inclure en 
!  plus du potentiel du corps central : 
!  - le potentiel des 3è corps, 
!  - une accélération due au frottement atmosphérique
!  - une accélération due à la pression solaire
!
!$Auteur
!  Y.TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_force_modele_complet (date,Pos,Vit,Pgamav,irepa,Acc)
!.    type(tm_jour_sec) :: date
!.    real (KIND=pm_reel),dimension(3) :: Pos,Vit
!.    real (KIND=pm_reel),dimension(3,3) :: pgamav
!.    integer :: irepa
!.    real (KIND=pm_reel),dimension(3) :: Acc
!
!$Arguments
!>E     date    :<tm_jour_sec>         Date en jj 1950 TE (jours/secondes)
!>E     Pos     :<pm_reel,DIM=(3)>     position dans le repère d'intégration (m)
!>E     Vit     :<pm_reel,DIM=(3)>     vitesse dans le repère d'intégration (m/s)
!>E     Pgamav  :<pm_reel,DIM=(3,3)>   matrice de passage du repère d'intégration au repère véhicule
!>E     irepa   :<integer>             type de repère "local" pour la définition de l'attitude
!>S     Acc     :<pm_reel,DIM=(3)>     accélération produite dans le repère d'intégration (m/s^2)
!
!$Common
!
!$Routines
!- ps_calcul_tsid_RI_Rapp
!- ps_rotation_tsid
!- ps_mat_rotation_tsid
!- mu_matmul3
!- ps_propage
!- psi_rep_veis_ri
!- ps_calcul_potentiel
!- ps_acc3corps
!- psfratm
!- psprsol
!
!$Include
!
!$Module
!#V
!- ps_generalites
!- ps_bulletin
!- ps_integration_don
!- ps_caracteristiques
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use ps_generalites
      use ps_bulletin
      use ps_integration_don
      use ps_caracteristiques

      implicit none
      
      ! Arguments
      !==========

      type(tm_jour_sec), intent(in)                 :: date
      real (KIND=pm_reel),dimension(3), intent(IN)  :: Pos,Vit
      real (KIND=pm_reel),dimension(3,3), intent(IN):: pgamav
      integer, intent(IN)                           :: irepa
      real (KIND=pm_reel),dimension(3), intent(OUT) :: Acc

      ! Variables locales
      !==================
      integer :: ii
      type(tm_code_retour) :: code_erreur
      real(kind=pm_reel) :: tsid
      

      ! accélérations de chaque force dans le repère d'intégration
      real (kind=pm_reel), dimension(3) :: acc_pot, acc_3corps, acc_prs, acc_fratm

      ! position et vitesse du véhicule dans le repère d'application de la force
      real (kind=pm_reel), dimension(3) :: Pos_Rapp, Vit_Rapp
      
      ! accélération dans le repère d'application de la force (planéto à date courante)
      real (kind=pm_reel), dimension(3) :: acc_pot_Rapp, acc_fratm_rapp

      ! matrice de passage du repère d'intégration au repère d'application de la force
      ! et du repère d'application de la force au repère véhicule
      !
      ! Conventions : RI   = repère d'intégration (planéto à date initiale)
      !               Rapp = repère d'application de la force (planéto à date courante)
      !               Rveh = repère véhicule, calculé par psattit (calcul d'attitude)
      real (kind=pm_reel), dimension(3,3) :: mat_RI_Rapp, mat_Rapp_RI
      real (kind=pm_reel), dimension(3,3) :: mat_Rapp_Rveh

      real(kind=pm_reel), dimension(3) :: pos_soleil_RI, pos_soleil_veis

      real(kind=pm_reel) :: mu_soleil, norme_pos_soleil, us_RI, vs_RI, ws_RI
  
      real (kind=pm_reel) :: ro,mach
      real(KIND=pm_reel),dimension(3)   :: vitatm
      real(KIND=pm_reel),dimension(3)   :: vit_vent
      real(kind=pm_reel) :: xcd,xcl
      

      ! Début du code
      !==============

      ! 0) Calculs préliminaires aux appels à chaque calcul de forces
      !==============================================================

      ! Calcul des positions vitesses dans le repère d'application de 
      ! la force, c'est à dire dans un repère planétocentrique à date courante
      ! -> a) calcul du temps sidéral
      ! -> b) rotation du vecteur position
      ! -> c) calcul de la matrice de rotation associée
      !
      ! Remarque générale : les fonctions ps_*_tsid et mu_matmul (ou mu_transpose) 
      ! ne peuvent pas sortir en erreur.

      ! a) calcul du temps sidéral
      call ps_calcul_tsid_RI_Rapp(date,tsid)

      ! b) Conversion de la position dans le repère d'application de la force
      call ps_rotation_tsid(pos,tsid,pos_rapp)
      call ps_rotation_tsid(vit,tsid,vit_rapp)

      ! c) Calcul de la matrice de rotation de passage de Rapp à RI
      !    puis de la matrice Rapp -> Rveh 
      call ps_mat_rotation_tsid(tsid,mat_RI_Rapp)
      call ps_mat_rotation_tsid(-tsid,mat_Rapp_RI)
      
      call mu_matmul3(pgamav,mat_Rapp_RI,mat_Rapp_Rveh,code_erreur) 

      ! Calcul des éphémérides du soleil dans le repère d'intégration
      ! -> nécessaire pour le 3è corps "Soleil", et en mode Terre
      ! pour les frottements atmosphériques et les frottements de pression solaire
      if ( (str_mod(iveh)%ikle(2) /= 0) .or. (str_mod(iveh)%ikle(4) /= 0) .or. &
           (str_mod(iveh)%ikle(3) /= 0)) then

         call ps_propage(str_3co(iveh)%typephem, date,MSP_ENUM_SOLEIL,&
              pos_soleil_veis,mu_soleil,repveis=1)
         if ( MSP_gen_messages("ps_force_modele_complet") ) return                            

         call psi_rep_veis_ri(date,  pos_soleil_veis, pos_soleil_RI)
         ! Note : pas de retour en erreur possible      

      else
         ! Initialisations des vecteurs positions du Soleil (et Mu)
         ! à zéro s'ils ne servent pas.
         do ii=1,3
            pos_soleil_RI(ii)   = 0._pm_reel
            pos_soleil_veis(ii) = 0._pm_reel
         end do
         mu_soleil = 0._pm_reel
      end if

 
      ! 1) Force due au potentiel du corps attracteur
      ! grâce à un potentiels GRGS -> calcul via la MECASPA
      ! 2nd membre simplifie = 0 : on garde les degrés zonaux et tesseraux 
      ! choisis par l'utilisateur
      !=====================================================
      call ps_calcul_potentiel(Pos_rapp,0,acc_pot_rapp)
      if (MSP_gen_messages("ps_force_modele_complet")) return

      ! Rotation inverse pour se replacer dans le repère d'intégration
      call ps_rotation_tsid(acc_pot_rapp,-tsid,acc_pot)


      ! 2) Force due à l'attraction des "troisièmes corps"
      ! 
      ! -> cette fonction propage éventuellement la position d'une (ou des) lune(s)
      ! et calcule l'accélération due au soleil (si demandé)
      ! ou à la lune (phobos et deimos en mode Mars)
      !
      ! -> l'accélération du 3è corps est directement calculée
      ! dans le repère d'intégration, car il n'y a pas lieu
      ! de se mettre dans un repère planéto à date courante.
      !
      ! note : les éphémérides du Soleil (et le mu) sont calculés auparavant, 
      ! car ils peuvent servir pour d'autres calculs (ex : pression solaire ou activité solaire)
      !
      !===================================================
      if (str_mod(iveh)%ikle(1) /= 0 .or. str_mod(iveh)%ikle(2) /= 0) then
         call ps_acc3corps(date,pos,pos_soleil_RI,mu_soleil,acc_3corps)
         if (MSP_gen_messages("ps_force_modele_complet")) return
      else
         do ii=1,3
            acc_3corps(ii) = 0._pm_reel
         end do
      end if
   
      ! 3) Force due au frottement atmosphérique
      !=========================================
      if ( str_mod(iveh)%ikle(3) /= 0 ) then
         call psfratm(date,pos_rapp,vit_rapp,mat_Rapp_Rveh,pos_soleil_veis,irepa,acc_fratm_rapp,ro,mach,vitatm,vit_vent,xcd,xcl)
         if (MSP_gen_messages("ps_force_modele_complet")) return

         ! Rotation inverse pour se replacer dans le repère d'intégration
         call ps_rotation_tsid(acc_fratm_rapp,-tsid,acc_fratm)
         
      else
         do ii=1,3
            acc_fratm(ii) = 0._pm_reel
         end do
      end if

      ! 4) Force due au frottement de pression de radiation solaire
      !
      ! Note : les éphémérides du soleil sont calculées dans le repère d'intégration 
      ! directement : il n'y a pas besoin d'effectuer de rotation pour se placer 
      ! dans un repère planéto à date courante.
      !============================================================
      if ( str_mod(iveh)%ikle(4) /= 0 ) then
         
         ! Normalisation du vecteur position du soleil dans RI
         norme_pos_soleil = sqrt(pos_soleil_RI(1)**2 + pos_soleil_RI(2)**2 + pos_soleil_RI(3)**2)
         us_RI = pos_soleil_RI(1) / norme_pos_soleil
         vs_RI = pos_soleil_RI(2) / norme_pos_soleil
         ws_RI = pos_soleil_RI(3) / norme_pos_soleil

         call psprsol (pos,us_RI,vs_RI,ws_RI,pgamav,irepa,acc_prs)
         if ( MSP_gen_messages("ps_force_modele_complet") ) return                                  
      else
         do ii=1,3
            acc_prs(ii) = 0._pm_reel
         end do
      end if
      
      ! 5) Somme des forces exprimées dans le repère d'intégration
      !===========================================================
      do ii=1,3
         Acc(ii) = acc_pot(ii) + acc_3corps(ii) + acc_prs(ii) + acc_fratm(ii)
      end do
      
    end subroutine ps_force_modele_complet

    subroutine ps_force_modele_simplifie(date,pos,acc)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_force_modele_simplifie
!
!$Resume
!  Calcule l'accélération produite par un modèle de forces simplifié.
!
!$Description
!  Calcule l'accélération produite par un modèle de forces simplifié.
!  Le modèle est limité au potentiel du corps central, dont on limite
!  volontairement les degrés zonaux et tesseraux à min(6, degré utilisateur)
!
!$Auteur
!  Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_force_modele_simplifie(date,pos,acc)
!.    type(tm_jour_sec) :: date 
!.    real(pm_reel),dimension(3) :: Pos 
!.    real(pm_reel),dimension(3) :: Acc 
!
!$Arguments
!>E     date  :<tm_jour_sec>       date en jj 1950 TE (jours/secondes)
!>E     pos   :<pm_reel,DIM=(3)>   position dans le repère d'intégration
!>S     acc   :<pm_reel,DIM=(3)>   accélération dans le repère d'intégration
!
!$Common
!
!$Routines
!- ps_calcul_tsid_RI_Rapp
!- ps_rotation_tsid
!- ps_calcul_potentiel
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      implicit none

      ! Arguments
      !==========
      type(tm_jour_sec),intent(in)                  ::  date     ! date TE
      real(pm_reel),dimension(3),intent(in)         ::  Pos      ! position
      real(pm_reel),dimension(3),intent(out)        ::  Acc      ! accélération
           
      ! Variables locales
      !==================
      ! Position et accélération dans le repère d'application
      ! de la force (planéto à date courante)
      real(kind=pm_reel),dimension(3) :: pos_rapp, acc_rapp
      real(kind=pm_reel) :: tsid

      ! Force due au potentiel du corps attracteur
      ! (potentiels GRGS -> calcul via la MECASPA)
      ! 2nd membre simplifie = 1 : on limite l'ordre du potentiel (degrés zonaux et tesseraux)
      !==============================================
      ! a) calcul du temps sidéral
      call ps_calcul_tsid_RI_Rapp(date,tsid)

      ! b) Conversion de la position dans le repère d'application de la force
      call ps_rotation_tsid(pos,tsid,pos_rapp)
      
      ! c) Appel au modèle avec second membre simplifie
      call ps_calcul_potentiel(pos_rapp,1,Acc_rapp)
      if (MSP_gen_messages("ps_force_modele_simplifie")) return
      
      ! d) Rotation inverse pour récupérer l'accélération dans RI
      call ps_rotation_tsid(Acc_rapp,-tsid,Acc)

    end subroutine ps_force_modele_simplifie

end module ps_modeles
