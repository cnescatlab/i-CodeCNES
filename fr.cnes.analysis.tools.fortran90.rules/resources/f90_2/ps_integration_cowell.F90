module ps_integration_cowell

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ps_integration_cowell
!
!$Resume
!  Module gérant l'appel à l'intégrateur de type Cowell
!
!$Description
!  Module gérant l'appel à l'intégrateur de type Cowell
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: ps_integration_cowell.F90 368 2013-02-19 14:43:59Z aadt $
!
!$Historique
!  $Log: ps_integration_cowell.F90,v $
!  Revision 368  2013/02/19 aadt
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 1.55  2010/10/25 13:10:59  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.54  2009/08/19 09:17:15  tanguyy
!  AQ : rajout d'un implicit none manquant
!
!  Revision 1.53  2009/06/16 08:22:00  tanguyy
!  FA-ID 1306 : corrections de fuites memoires potentielles (structure separation et structure integrateur)
!
!  Revision 1.52  2009/05/05 09:54:55  tanguyy
!  FA-ID 1283 : suppression du test sur la date 'jour /= 0' (non pertinent) / FA-ID 1292 : mise a jour systematique de date_prec_cowell, lors de la creation de l'integrateur, et pas seulement quand la date initiale differe de la precedente date initiale d'integration
!
!  Revision 1.51  2009/03/13 07:53:22  tanguyy
!  FA-ID 1174 et FA-ID 1175 : le Cowell n'est plus ré-initialisé lors des manoeuvres continues de durée nulle, et impulsionnelles avec un deltaV nul
!
!  Revision 1.50  2008/12/02 16:48:34  tanguyy
!  AQ : mise à jour des cartouches avant livraison de PSIMU V9.3
!
!  Revision 1.49  2008/11/18 13:38:09  tanguyy
!  DM-ID 733 : reorganisation des modules / utilisation de ps_force_modele_complet et ps_force_modele_simplifie
!  Revision 1.48  2008/10/15 13:10:27  tanguyy
!  DM-ID 1058 : controles lors des desallocations memoire
!  Revision 1.47  2008/10/15 12:55:19  tanguyy
!  DM-ID 1058 : Controles des desallocations memoire
!  Revision 1.46  2008/09/04 07:53:05  tanguyy
!  DM-ID 1058 : phase 1 du portage / suppression des warnings - initialisations
!  Revision 1.45  2008/04/29 17:22:10  tanguyy
!  DM-ID 964 et  FA-ID 886 : le test se fait desormais sur l'ecart entre la date precedente et la date courante : s'il est inferieur a 10-6 sec, l'integrateur n'est pas appele
!  Revision 1.44  2008/04/22 09:15:29  tanguyy
!  DM-ID 964 : il ne faut pas empecher un appel au Cowell de t0 à t0 -> suppression du test dans ps_integre_cowell
!  Revision 1.43  2008/04/03 16:08:52  ttn
!  FA-ID 658 : suppression des variables inutilisees
!  Revision 1.41  2008/04/02 13:47:29  tanguyy
!  DM-ID 983 : utilisation d'un epsilon 10+4 fois plus grand pour la variable d'integration
!  Revision 1.40  2008/03/31 15:30:25  ttn
!  correction dans pspoter de la place de l'argument du SMS
!  Revision 1.39  2008/03/19 15:20:16  ttn
!  DM-ID 983 : Second membre simplifie
!  Revision 1.38  2008/03/07 09:59:02  huec
!  AQ : Correction d une coquille
!  Revision 1.37  2008/03/06 12:46:08  ttn
!  DM-ID 959 : Modification routine ps_calcul_altitude_pente + prise en compte dans ps_init_boucle_cowell
!  Revision 1.36  2008/01/30 09:31:49  tanguyy
!  FA-ID 886 : suppression du warning lors de l'integration sur une duree nulle
!  Revision 1.35  2008/01/29 16:18:51  tanguyy
!  Annulation des tests pour la FA 855 -> en attente de realisation de DM 964 pour obtenir une solution viable au probleme de la reinitialisation de l'integrateur lors d'un aller-retour du Cowell
!  Revision 1.33  2007/12/06 15:16:54  huec
!  DM-ID 733 : Annulation de la DM
!  Revision 1.32  2007/10/02 08:01:58  tanguyy
!  DM-ID 733 - utilisationd du calcul du potentiel de la MECASPA
!  Revision 1.31  2007/09/24 15:06:17  tanguyy
!  FA-ID 787 ; suppression des variables inutilisees
!  Revision 1.30  2007/08/27 10:36:19  tanguyy
!  AQ : correction d'un commentaire dans ps_force_cowell
!  Revision 1.29  2007/07/10 09:05:37  tanguyy
!  Quand on change de sens mais pas de pas, seul xpas est modifie, mais l'integrateur n'est pas reinitialise
!  Revision 1.28  2007/07/09 12:09:17  tanguyy
!  Intégration des DM-ID 702, DM-ID 748 et DM-ID 692 sur les paramètres d'intégration
!  Revision 1.27  2007/06/20 12:33:30  vivaresf
!  Intégration FA 725 patch V8.7a3
!  Revision 1.26  2007/02/20 13:41:34  tanguyy
!  FA-ID 711 : desactivation de la liberation systematique des integrateurs /\rm -rf * version intermediaire
!  Revision 1.25.2.5  2007/06/15 17:17:16  vivaresf
!  PSIMU V8.7a4 : essais pour fuites mémoire
!  Revision 1.25.2.4  2007/04/18 06:29:42  vivaresf
!  FA-ID 725 : suppression initialisation de la date initiale
!  corrections dans les commentaires
!  présentation du code
!  gestion cohérente du %init_cowell avec la structure intégrateur (avec ps_libere_cowell())
!  Revision 1.25.2.3  2007/04/16 12:27:01  vivaresf
!  FA-ID 725 : ré-initialisation de l'intégrateur (sinon
!  plantage)
!  Revision 1.25.2.2  2007/04/16 09:42:51  vivaresf
!  FA-ID 725 : optimisation et robustesse du patch V8.7a3
!  meilleure gestion des allocate (désallocation + status)
!  desallocation du COWELL si ré-initialisation de l'intégration
!  Revision 1.25.2.1  2007/03/12 08:43:18  couturis
!  FA-ID 715 & FA-ID 711, ordre du Cowell et critere de covergence
!  Revision 1.25  2006/10/19 15:07:43  tanguyy
!  DM-ID 478 : Cloture du FT (Integrateur de Cowell : modification de l interface)
!  Revision 1.24.2.1  2006/10/13 07:55:14  tanguyy
!  DM-ID 478 : utilisation jj/sec. 1ere version fonctionnelle
!  Revision 1.24  2006/09/25 10:23:42  tanguyy
!  AQ : Rajout de commentaires pour la compréhension de l'algo
!  Revision 1.23  2006/05/30 09:33:03  tanguyy
!  DM-ID 232 : Cloture du FT (Nommage des arguments lors de l appel d une routine ou d une fonction en Fortran 90 ou d un objet GENESIS)
!  Revision 1.22.2.1  2006/05/30 09:32:19  tanguyy
!  DM-ID 232 : nommage des parametres optionnels
!  Revision 1.22  2006/04/21 08:12:38  tanguyy
!  DM-ID 400 : Cloture du FT (Performances en temps de calcul sur les scnarios MECASPA)
!  Revision 1.21.4.1  2006/04/20 13:21:32  tanguyy
!  Correction d'une anomalie sur le n° d'itération en mode pas fixes
!  Revision 1.21  2006/03/17 14:46:32  tanguyy
!  DM-ID 492 (utilisation critere de convergence) / livraison PSIMU V8-4
!  Revision 1.20  2006/03/01 13:34:44  tanguyy
!  FA-ID 480 : amélioration de la gestion des pas fixes
!  Revision 1.19  2006/02/27 16:07:19  tanguyy
!  FA-ID 480 : correction dans la gestion des pas fixes
!  Revision 1.18.2.1  2006/02/09 08:44:26  tanguyy
!  FA-ID 480 : Version initiale du FT (Erreur dans la gestion des pas fixes)
!  Revision 1.18  2005/12/14 19:08:05  tanguyy
!  PSIMU V8-3
!  Revision 1.17  2005/12/14 11:31:45  tanguyy
!  DM-ID 397 : Cloture du FT (Utiliser les intégrateurs MSPRO dans PSIMU)
!  Revision 1.16.2.1  2005/12/14 11:17:20  tanguyy
!  Utilisation de l integrateur COWELL de MSPRO. Version intermediaire
!  Revision 1.16  2005/11/10 18:37:07  vivaresf
!  Mise à jour des cartouches
!  Revision 1.15  2005/02/24 15:44:52  fabrec
!  DM-ID 98 : pas des ephemerides
!  Revision 1.14  2005/02/17 13:57:33  fabrec
!  DM-ID 98 : pas des ephemerides
!  Revision 1.13  2005/02/01 09:01:01  fabrec
!  DM-ID 175 : gestion de la memoire
!  Revision 1.12  2005/01/28 10:42:11  fabrec
!  DM-ID 175 : desallocations memoire
!  Revision 1.11  2005/01/17 15:26:28  fabrec
!  DM-ID 175 : utilisation des scenarios mecaspa
!  Revision 1.10  2004/12/10 16:58:29  fabrec
!  DM-ID 175 : utilisation des scenarios mecaspa
!  Revision 1.9  2002/12/20 16:39:06  boschett
!  Utilisation du traitement d'erreur de la MECASPA
!  Revision 1.8  2002/12/12 15:05:24  boschett
!  Livraison Intermediaire 16/12/2002
!  Revision 1.7  2002/12/10 10:42:30  boschett
!   Ajout du traitement par défaut dans les structures if/elseif/else de la subroutine psbcowe 
!  Revision 1.6  2002/12/05 17:32:31  boschett
!  Utilisation de les operateurs MECASPA .egal. et .different. pour tester l'égalité (inégalité) entre réels
!  Revision 1.5  2002/12/04 14:25:03  boschett
!  Suppression des instructions return inutiles en fin de routine
!  Revision 1.4  2002/12/03 13:35:08  boschett
!  Initialisation de la variable xpas dans psbcowe
!  Revision 1.3  2002/12/02 17:04:16  boschett
!  Suppression des variables locales déclarées et non utilisées
!  Revision 1.2  2002/11/26 16:59:23  boschett
!  Ajout de implicit none
!  Revision 1.1.1.1  2002/09/30 14:59:34  laurent
!  Industrialisation PSIMU
!  Revision 1.4  2000/04/17 10:58:17  util_am
!  Version multi_satellite en Fortran90
!  Revision 1.3  1999/10/26 11:00:12  util_am
!  Mise à jour des cartouches
!  Revision 1.2  1999/08/04 11:28:13  util_am
!  Prise en compte de la gestion des erreurs de MECASPA
!
!$FinHistorique
!
!$Usage
!  use ps_integration_cowell
!
!$Structure
!
!$Global
!
!$Common
!
!$Routines
!- psbcowe
!- psinpre
!- ps_integre_cowell
!- ps_force_cowell
!- psi_fcowell_simp
!- ps_libere_cowell
!- ps_reinit_integ_cowell
!#V
!- ps_init_boucle_cowell
!#
!
!$Fonctions
!
!$Include
!
!$Module
!#V
!- MECASPA
!- ps_generalites
!- ps_modeles
!- ps_propulsion
!- ps_separations
!- ps_ecriture
!- ps_evenements
!- ps_variables
!- ps_integration
!- ps_integration_don
!- MSPRO
!- ps_bulletin
!- ps_attitude
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
   use ps_modeles
   use ps_propulsion
   use ps_separations
   use ps_ecriture
   use ps_evenements
   use ps_variables
   use ps_integration
   use ps_integration_don

   use MSPRO
   
   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ps_integration_cowell.F90 368 2013-02-19 14:43:59Z aadt $'

   private :: ps_init_boucle_cowell

   contains

      subroutine psbcowe (iter0,iter,iterw,inicow,ypas,datex,parex,jflag)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psbcowe
!
!$Resume
!  Boucle sur l'intégrateur de Cowell.
!
!$Description
!  Boucle sur l'intégrateur de Cowell.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psbcowe (iter0,iter,iterw,inicow,ypas,datex,parex,jflag)
!.    integer :: iter0,iter,iterw,inicow,jflag
!.    real (KIND=pm_reel) :: ypas,parex(6)
!.    type(tm_jour_sec) :: datex
!
!$Arguments
!>E/S   iter0   :<integer>           numéro de l'itération à l'entrée dans le sous-programme
!>E/S   iter    :<integer>           numéro de l'itération courante
!>E/S   iterw   :<integer>           compteur servant à l'écriture des résultats
!>E/S   inicow  :<integer>           indicateur de l'état de l'initialisation
!>E/S   ypas    :<pm_reel>           temps écoulé par rapport au début de la simulation (sec)
!>E/S   datex   :<tm_jour_sec>       date (Jours Juliens CNES)
!>E/S   parex   :<pm_reel,DIM=(6)>   position-vitesse courantes (m-m/s) 
!>E/S   jflag   :<integer>           indicateur de sortie du sous-programme:
!.                                   0 si le nombre d'iterations est > npas
!.                                   1 si t >= tmax
!.                                   2 si h < hstop
!.                                   3 si h >  h2 ... utilisation de Cowell
!.                                   4 si h <= h2 ... utilisation de Gill
!
!$Common
!
!$Routines
!- pstsuiv
!- MSP_consulter_scenario
!- MSP_consulter_poussee_continue
!- MSP_effacer_poussee_continue
!- MSP_consulter_impulsion
!- MSP_signaler_message
!- MSP_consulter_poussee_bulletin
!- ps_integre_cowell
!- ps_libere_cowell
!- pspimpu
!- pspcont
!- psnewbl
!- MSP_consulter_separation
!- pseject
!- psnetat
!- pswresu
!#V
!- ps_init_boucle_cowell
!#
!
!$Include
!
!$Module
!#V
!- MSPRO
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use MSPRO


      implicit none

      ! Arguments
      !==========

      integer, intent(INOUT)             :: iter0,iter,iterw,inicow,jflag
      real (KIND=pm_reel), intent(INOUT) :: ypas,parex(6)
      type(tm_jour_sec), intent(inout)   :: datex

      ! Variables locales
      !==================

      integer :: iplus,kd,kf,kflag
      real (KIND=pm_reel),dimension(PS_NVMAX) :: xpas=0
      character (LEN=8) :: pvar
      integer ::  sc_type, sc_nloi, typloi
      real(KIND=pm_reel) ::  tsep
      type(MSP_SEPARATION) :: loi_sep
      type(MSP_IMPULSION) :: loi_imp
      type(MSP_POUSSEE_CONTINUE) :: loi_cont
      type(MSP_POUSSEE_BULLETIN) :: loi_bul
      integer :: ntab, allocstat
      real (KIND=pm_reel), dimension(:), pointer :: timp
      real (KIND=pm_reel) :: datbul,datedeb,datetimp, tbul
      real (KIND=pm_reel) :: datprec_tmp, date_ref
      real (KIND=pm_reel) :: duree_integ, deltav
      integer :: code_ret
      logical :: libere_cowell

      save xpas
      


!***********************************************************************
!*    Debut du code                                                    *
!***********************************************************************
      
      ! Initialisation
      nullify(timp)
      allocstat = 0
      datprec_tmp = 0.001_pm_reel

!***********************************************************************
!*    Debut de la boucle                                               *
!***********************************************************************
      
      iter = iter0

      do while ( iter <= npas )

         ! Routine interne d'initialisation du Cowell
         ! -> selon l'altitude, et les paramètres choisis par l'utilisateur
         ! on ré-initialise ou pas le Cowell..
         call  ps_init_boucle_cowell(datex,parex,inicow,iter,jflag,xpas)

         ! Sortie si l'altitude est trop basse et nécessite le Runge-Kutta, ou 
         ! d'arrêter le calcul
         if (jflag == 2 .or. jflag == 4) goto 999

         iterw=iter
         call pstsuiv (iplus)
         if ( MSP_gen_messages("psbcowe") ) return              
    
         if ( iplus == 1 ) then
            
            libere_cowell = .true.

            !!! 17/08/06 Explications sur l'algo :
            ! consultation du type de loi, et récupération dans timp des dates de poussées
            ! (soit date de début et de fin, soit date de poussée, soit date de poussée bulletin
            ! et nouvelle date)
            
             typloi = MSP_type_loi (scenar_pro(iveh), id=iloi_scenar_pro(iveh))
             if ( MSP_gen_messages("psbcowe") ) return

             if (typloi == MSP_ENUM_LOI_CONT) then
                !/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
                call MSP_consulter_scenario (scenar_pro(iveh), loi_cont=loi_cont, &
                     id=iloi_scenar_pro(iveh))
                if ( MSP_gen_messages("psbcowe") ) return
                
                call MSP_consulter_poussee_continue (loi_cont, ntab=ntab, dates=timp)
                if ( MSP_gen_messages("psbcowe") ) return

                ! Est-ce que la poussée à une durée nulle ?
                ! -> si c'est le cas, alors il ne faut pas ré-initialiser le Cowell 
                ! après la propulsion, car celle-ci est sans effet 
                ! et ne modifie pas les conditions initiales.
                if (abs(timp(1) - timp(ntab)) < MSP_EPSILON_APLA) then
                   libere_cowell = .false.
                end if

                ! Libération de la mémoire
                call MSP_effacer_poussee_continue (loi_cont)
                if ( MSP_gen_messages("psbcowe") ) return

             elseif (typloi == MSP_ENUM_LOI_IMP) then
                !/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
                call MSP_consulter_scenario (scenar_pro(iveh), loi_imp=loi_imp, id=iloi_scenar_pro(iveh))
                if ( MSP_gen_messages("psbcowe") ) return

                call MSP_consulter_impulsion (loi_imp, date=datetimp, deltav=deltav)
                if ( MSP_gen_messages("psbcowe") ) return

                ! Est-ce que la poussée est nulle (delta V = 0 m/s) ?
                ! -> si c'est le cas, alors il ne faut pas ré-initialiser le Cowell 
                ! après la propulsion, car celle-ci est sans effet 
                ! et ne modifie pas les conditions initiales.
                if (deltav < MSP_EPSILON_APLA) then
                   libere_cowell = .false.
                end if
                
                ntab = 1
                if (ASSOCIATED(timp)) then
                   deallocate(timp, stat=allocstat)
                   if (allocstat < 0) then
                      call MSP_signaler_message(cle_mes="PS_ERR_DESALLOC",partie_variable="psbcowe")
                      return
                   end if
                end if
                ALLOCATE(timp(1))
                timp(1) = datetimp

             elseif (typloi == MSP_ENUM_LOI_BUL) then
                !/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
                call MSP_consulter_scenario (scenar_pro(iveh), date_ref=date_ref)
                if ( MSP_gen_messages("psbcowe") ) return

                call MSP_consulter_scenario (scenar_pro(iveh), loi_bul=loi_bul, id=iloi_scenar_pro(iveh))
                if ( MSP_gen_messages("psbcowe") ) return

                call MSP_consulter_poussee_bulletin (loi_bul, datedeb=datedeb, datbul = datbul)
                if ( MSP_gen_messages("psbcowe") ) return

                if (ASSOCIATED(timp)) then
                   deallocate(timp, stat=allocstat)
                   if (allocstat < 0) then
                      call MSP_signaler_message(cle_mes="PS_ERR_DESALLOC",partie_variable="psbcowe")
                      return
                   end if
                end if
                ALLOCATE(timp(2))
                timp(1:2) = 0._PM_REEL

                timp(1) = datedeb
                tbul = (datbul - date_ref)*86400._pm_reel
                ! Arrondi à la milli-seconde:
                timp(2) = real(int(tbul),KIND=pm_reel) + &
                    real(nint((tbul-int(tbul))/datprec_tmp),KIND=pm_reel)*datprec_tmp
                ntab = 2
                
             endif

            !!! 17/08/06 Explications sur l'algo :
            ! Si la durée d'intégration risque de dépasser la date de poussée
             ! alors on intègre jusqu'à la date de poussée, puis on applique la 
             ! poussée

!           Appel a Cowell et Gill dans le cas d'une poussee:
!           ------------------------------------------------
            if (str_int(iveh)%itsgn > 0) then
               kd = 1
               kf = ntab
            else
               kd = ntab
               kf = 1
            endif

            !/ Avant d'appliquer la poussée, il faut s'assurer d'arriver jusqu'à cette date
            !/ de début de poussée.
            if (str_ecr(iveh)%ipas == 2) then
               duree_integ = str_ecr(iveh)%pas_sortie
            else
               duree_integ = str_int(iveh)%h
            end if


            if (str_int(iveh)%tsign*(ypas+duree_integ) >= (str_int(iveh)%tsign* timp(kd))) then
               if ( (str_int(iveh)%tsign*ypas) < (str_int(iveh)%tsign* timp(kd)) ) then
                  ypas=timp(kd)
                  datex=str_bul(iveh)%datbul0_js+ypas

                  call ps_integre_cowell(datex, str_int(iveh)%h, parex, code_ret)
                  if((code_ret < 0) .or. MSP_gen_messages("psbcowe")) return
                  
               endif

               if (libere_cowell) then
                  !/ On libere l'intégrateur de Cowell avant la manoeuvre
                  ! car les conditions du calcul après manoeuvres (P/V, date, etc.)
                  ! auront changé.
                  !
                  ! /!\ Seuls deux cas imposent de ne pas ré-initialiser l'intégrateur :
                  ! - poussée impulsionnelle nulle (le bulletin reste inchangé)
                  ! - poussée continue de durée nulle (le bulletin reste inchangé)
                  inicow = 0 
                  call ps_libere_cowell()

               end if

               if (typloi == MSP_ENUM_LOI_IMP) then
                  call pspimpu (str_int(iveh)%itsgn,str_int(iveh)%tsign,ypas,datex,parex,iter)
                  if ( MSP_gen_messages("psbcowe") ) return              
               else if (typloi == MSP_ENUM_LOI_CONT) then
                  call pspcont (str_int(iveh)%itsgn,str_int(iveh)%tsign,str_int(iveh)%hstop,&
                       str_int(iveh)%tmax,ypas,datex,parex,iter,kflag)
                  if ( MSP_gen_messages("psbcowe") ) return              
                  if ( kflag /= 0 ) then
                     if (ypas .egal. timp(kf)) then
                        iloi_scenar_pro(iveh) = iloi_scenar_pro(iveh) + str_int(iveh)%itsgn
                     endif
                     jflag=kflag
                     goto 999
                  endif
               else
                  call psnewbl (str_int(iveh)%itsgn,str_int(iveh)%tsign,ypas,datex,parex,iter)
                  if ( MSP_gen_messages("psbcowe") ) return              
               end if
               iloi_scenar_pro(iveh) = iloi_scenar_pro(iveh) + str_int(iveh)%itsgn
               goto 220
            end if
            
         else if ( iplus == 2 ) then

            !!! Explication sur l'algo : relativement similaire à l'algo pour les poussées :
            ! recherche de date, puis integration éventuelle, puis separation

!           Appel a Cowell et Gill dans le cas d'une separation:
!           ---------------------------------------------------
            !  Lecture de la structure MSP_SCENARIO_LOI
            call MSP_consulter_scenario (scenar_sep(iveh), type=sc_type, nloi=sc_nloi)
            if ( MSP_gen_messages("psbcowe") ) return

            !   Si le scenario est de type separation      
            if ( sc_type == MSP_ENUM_SEPARATION ) then
               if (sc_nloi > 0) then
                  call MSP_effacer_separation(loi_sep,nul=.true.)

                  call MSP_consulter_scenario (scenar_sep(iveh), loi_sep=loi_sep)
                  if ( MSP_gen_messages("psbcowe") ) return
                  
                  call MSP_consulter_separation (loi_sep, date=tsep)
                  if ( MSP_gen_messages("psbcowe") ) return

                  call MSP_effacer_separation (loi_sep)
                  !/ Avant d'appliquer la séparation, il faut s'assurer d'arriver jusqu'à cette date
                  !/ de début de poussée.
                  if (str_ecr(iveh)%ipas == 2) then
                     duree_integ = str_ecr(iveh)%pas_sortie
                  else
                     duree_integ = str_int(iveh)%h
                  end if

                  if (str_int(iveh)%tsign*(ypas+duree_integ) >= (str_int(iveh)%tsign*tsep)) then
                     if ( (str_int(iveh)%tsign*ypas) < (str_int(iveh)%tsign*tsep) ) then
                        ypas=tsep
                        datex=str_bul(iveh)%datbul0_js+ypas
                        call ps_integre_cowell(datex, str_int(iveh)%h, parex, code_ret)
                        if((code_ret < 0) .or. (MSP_gen_messages() .and. MSP_ERREUR)) return
             
                     endif
                     !/ Apres la separation, l'integrateur devra être reinitialise
                     !/ -> on le libere au prealable
                     inicow = 0
                     call ps_libere_cowell()

                     call pseject (str_int(iveh)%itsgn,str_int(iveh)%tsign,ypas,datex,parex,iter)
                     if ( MSP_gen_messages("psbcowe") ) return              
                     iloi_scenar_sep(iveh) = iloi_scenar_sep(iveh) + str_int(iveh)%itsgn
                     if ( ( iloi_scenar_sep(iveh) <=  sc_nloi) .and. ( iloi_scenar_sep(iveh) >= 1) ) then
                        ! -- Positionnement de la loi_courante sur la loi iloi_scenar_sep(iveh)
                        call MSP_consulter_scenario(scenar_sep(iveh), loi_sep, id=iloi_scenar_sep(iveh))
                        if ( MSP_gen_messages("psbcowe") ) return
                        call MSP_effacer_separation (loi_sep)
                     endif
                     goto 220
                  endif
               endif
            end if
            
         else if ( iplus == 3 ) then
            
!           Appel a Cowell juste a la date de fin:
!           -------------------------------------
            if (str_int(iveh)%tsign*(ypas+xpas(iveh)) >= (str_int(iveh)%tsign*str_int(iveh)%tmax)) then
               if ( ypas .different. str_int(iveh)%tmax ) then
                  ypas=str_int(iveh)%tmax
                  datex=str_bul(iveh)%datbul0_js+ypas

                  call ps_integre_cowell(datex, str_int(iveh)%h, parex, code_ret)
                  if((code_ret < 0) .or. (MSP_gen_messages() .and. MSP_ERREUR)) return

               endif
               jflag=1
! FA-ID 711 : désactivation de la liberation du Cowell ( /!\ modif provisoire )
! Pour la V8-7, il faudra prévoir une routine de liberation des integrateurs,
! utilisable après le dernier "call psimu"
!               inicow = 0
               goto 999
            end if

         else if ( iplus == 4 ) then

!           Appel a Cowell juste a l'evenement suivant:
!           ------------------------------------------
            
            !/ Avant d'écrire l'evt, il faut s'assurer de ne pas le
            !/ dépasser avec le prochain pas d'intégration
            if (str_ecr(iveh)%ipas == 2) then
               duree_integ = str_ecr(iveh)%pas_sortie
            else
               duree_integ = xpas(iveh)
            end if

            if (str_int(iveh)%tsign*(ypas+duree_integ) >= (str_int(iveh)%tsign*str_eve(iveh)%deve(str_eve(iveh)%neve))) then
               if(str_int(iveh)%tsign*(ypas) < str_int(iveh)%tsign*str_eve(iveh)%deve(str_eve(iveh)%neve)) then

                  ypas=str_eve(iveh)%deve(str_eve(iveh)%neve)
                  datex=str_bul(iveh)%datbul0_js+ypas

                  call ps_integre_cowell(datex, str_int(iveh)%h, parex, code_ret)
                  if((code_ret < 0) .or. (MSP_gen_messages() .and. MSP_ERREUR)) return

               end if
               if ( ilogeve /= 0 ) then
                  call psnetat (ypas,datex,parex)
                  if ( MSP_gen_messages("psbcowe") ) return              
                  call pswresu (2,ilogeve,0,40,iter,ypas,str_gen(iveh)%etat,&
                       str_ecr(iveh)%nvarw,str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,&
                       str_eve(iveh)%ifore,str_eve(iveh)%neve,str_eve(iveh)%aeve)
                  if ( MSP_gen_messages("psbcowe") ) return              
               endif
               str_eve(iveh)%neve = str_eve(iveh)%neve + str_int(iveh)%itsgn
               iter = iter - 1
               goto 220
            end if

         else if ( iplus == 5 ) then

!           Appel a Cowell jusqu'a la date de fin:
!           dans le cas de sorties regulieres
!           -------------------------------------
            if (str_int(iveh)%tsign*(ypas+str_ecr(iveh)%pas_sortie) >= (str_int(iveh)%tsign*str_int(iveh)%tmax)) then
               if ( ypas .different. str_int(iveh)%tmax ) then
                  ypas=str_int(iveh)%tmax
                  datex=str_bul(iveh)%datbul0_js+ypas

                  call ps_integre_cowell(datex, str_int(iveh)%h, parex, code_ret)
                  if((code_ret < 0) .or. (MSP_gen_messages() .and. MSP_ERREUR)) return
               endif
               jflag=1
! FA-ID 711 : désactivation de la liberation du Cowell ( /!\ modif provisoire )
! Pour la V8-7, il faudra prévoir une routine de liberation des integrateurs,
! utilisable après le dernier "call psimu"
!               inicow = 0
               goto 999
            end if

!           Appel a Cowell jusqu'a la sortie suivante:
!           ------------------------------------------
            if (str_int(iveh)%tsign*(ypas+str_ecr(iveh)%pas_sortie) >= &
                 str_int(iveh)%tsign*str_ecr(iveh)%ypas_sorties(str_ecr(iveh)%nsor)) then
               
               if(str_int(iveh)%tsign*(ypas) > &
                    str_int(iveh)%tsign*str_ecr(iveh)%ypas_sorties(str_ecr(iveh)%nsor)) then
                  ! La date courante est ultérieure à la date de sortie demandée :
                  ! il y a eu un saut dans le temps (ex : avec une manoeuvre bulletin)
                  ! et il faut retrouver une date de sortie correcte
                  do while (str_int(iveh)%tsign*(ypas) > &
                       str_int(iveh)%tsign*str_ecr(iveh)%ypas_sorties(str_ecr(iveh)%nsor) .and. &
                       str_ecr(iveh)%nsor < str_ecr(iveh)%nb_sorties)
                     str_ecr(iveh)%nsor = str_ecr(iveh)%nsor + 1
                  end do
               end if

	       ypas=str_ecr(iveh)%ypas_sorties(str_ecr(iveh)%nsor)
               datex=str_bul(iveh)%datbul0_js+ypas

               call ps_integre_cowell(datex, str_int(iveh)%h, parex, code_ret)
               if((code_ret < 0) .or. (MSP_gen_messages() .and. MSP_ERREUR)) return

               if ( ilogeph /= 0 ) then
                  call psnetat (ypas,datex,parex)
                  if ( MSP_gen_messages("psbcowe") ) return              
                  call pswresu (1,ilogeph,iscreen,0,iter,ypas,str_gen(iveh)%etat,&
                       str_ecr(iveh)%nvarw,str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,&
                       str_eve(iveh)%ifore,str_eve(iveh)%neve,str_eve(iveh)%aeve)
                  if ( MSP_gen_messages("psbcowe") ) return              
                  goto 220
               endif
            end if

         else
!           Type d'evenement à suivre inconnu
!           =================================
            write (pvar,'(I2)') iplus
            call MSP_signaler_message (cle_mes="PS_INTEGRATION_COWELL_002",partie_variable=pvar)
         end if

         if ( str_ecr(iveh)%ipas == 2 ) then

!           Appel a Cowell sans poussees, separations ...
!           =========================================

            ypas=ypas+str_ecr(iveh)%pas_sortie
            datex=str_bul(iveh)%datbul0_js+ypas

            call ps_integre_cowell(datex, str_int(iveh)%h, parex, code_ret)
            if((code_ret < 0) .or. (MSP_gen_messages() .and. MSP_ERREUR)) return

            if ( ilogeph /= 0 ) then

               if (str_int(iveh)%tsign*(ypas) .egal. &
                    str_int(iveh)%tsign*str_ecr(iveh)%ypas_sorties(str_ecr(iveh)%nsor)) then
                  call psnetat (ypas,datex,parex)
                  if ( MSP_gen_messages("psbcowe") ) return              
                  call pswresu (1,ilogeph,iscreen,0,iter,ypas,str_gen(iveh)%etat,&
                       str_ecr(iveh)%nvarw,str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,&
                       str_eve(iveh)%ifore,str_eve(iveh)%neve,str_eve(iveh)%aeve)
                  if ( MSP_gen_messages("psbcowe") ) return              
               endif
            endif
            
         else

!           Appel a Cowell sans poussees, separations ...
!           =========================================

            ypas=ypas+xpas(iveh)
            datex=str_bul(iveh)%datbul0_js+ypas

            call ps_integre_cowell(datex, str_int(iveh)%h, parex, code_ret)
            if((code_ret < 0) .or. (MSP_gen_messages() .and. MSP_ERREUR)) return

            if ( ilogeph /= 0 ) then
               if ((mod(iterw,str_ecr(iveh)%indw) == 0).and.(inicow /= 0)) then
                  call psnetat (ypas,datex,parex)
                  if ( MSP_gen_messages("psbcowe") ) return              
                  call pswresu (1,ilogeph,iscreen,0,iter,ypas,str_gen(iveh)%etat,&
                       str_ecr(iveh)%nvarw,str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,&
                       str_eve(iveh)%ifore,str_eve(iveh)%neve,str_eve(iveh)%aeve)
                  if ( MSP_gen_messages("psbcowe") ) return              
               endif
            endif
            
         endif
         
 220     iter = iter + 1

      enddo

!***********************************************************************
!*    Fin de la boucle                                                 *
!***********************************************************************

      jflag = 0

      ! Deallocations memoire
999   if (associated(timp)) deallocate(timp, stat=allocstat)

      if(inicow == 0) then
         call ps_libere_cowell()
      end if

     end subroutine psbcowe

     subroutine psinpre (xpas,datex)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psinpre
!
!$Resume
!  Lancement de l'initialisation de Cowell
!
!$Description
!  Lancement de l'initialisation de Cowell
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psinpre (xpas,datex)
!.    real (KIND=pm_reel) :: xpas
!.    type(tm_jour_sec) :: datex
!
!$Arguments
!>E     xpas   :<pm_reel>       pas d'intégration (s)
!>E     datex  :<tm_jour_sec>   date courante (Jours Juliens CNES)
!
!$Common
!
!$Routines
!- ps_reinit_integ_cowell
!
!$Include
!
!$Module
!#V
!- ps_bulletin
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use ps_bulletin

      implicit none
      
      ! arguments
      real (KIND=pm_reel), intent(IN) :: xpas
      type(tm_jour_sec), intent(in)   :: datex


      ! code
      str_int(iveh)%h=xpas
      str_bul(iveh)%datbul_js=datex

      !/ On reinitialise le COWELL : 
      !/    si le pas est changé
      !/ ou
      !/    si la date initiale d'intégration a changé
      !/ NB : la réinitialisation effective sera faite lors du 
      !/ prochain appel à ps_integre_cowell
      call ps_reinit_integ_cowell(str_int(iveh)%h, str_bul(iveh)%datbul_js)

    end subroutine psinpre

      subroutine ps_integre_cowell(date_fin, pas_integ, param, code_ret)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_integre_cowell
!
!$Resume
!  Routine chapeau d'appel à l'intégrateur COWELL de la MSPRO
!
!$Description
!  Routine chapeau d'appel à l'intégrateur COWELL de la MSPRO
!
!$Auteur
!  Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_integre_cowell(date_fin, pas_integ, param, code_ret)
!.    type(tm_jour_sec) :: date_fin 
!.    real(kind=PM_REEL) :: pas_integ 
!.    real(kind=PM_REEL), dimension(6) :: param 
!.    integer :: code_ret 
!
!$Arguments
!>E/S   date_fin   :<tm_jour_sec>       date finale demandée (jj50)
!>E     pas_integ  :<PM_REEL>           0 ou < 0 selon
!> que l'on intègre en avant ou en arrière. Mais l'intégrateur utilisera abs(pas_integ)        
!>E/S   param      :<PM_REEL,DIM=(6)>   paramètres P/V 
!>S     code_ret   :<integer>           code retour (0 si OK, < 0 si Erreur)
!
!$Common
!
!$Routines
!- ps_reinit_integ_cowell
!- mu_integrer
!- MSP_Signaler_Message
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

        !-- Arguments
        !------------
        type(tm_jour_sec), intent(inout) :: date_fin                    !/ Date finale (demandee/effective) de l'integration en jj/sec. Note : la date initiale est datbul0_js
        real(kind=PM_REEL), intent(in) :: pas_integ                      !/ Pas d'integration en sec
        real(kind=PM_REEL), dimension(6), intent(inout) :: param         !/ Parametre P/V
        integer, intent(out) :: code_ret                                 !/ Code retour < 0 ssi Erreur

        !-- Variables locales
        !--------------------

        !/ date_arret = date_fin si l'integration s'est terminée sans evt "bloquant" rencontré
        !/ date_arret = date_evt si un evt provoque un arret
        type(tm_jour_sec) :: date_arret, date_evt
        
        !/ Param_out : parametres calcules par l'integrateur
        real(kind=PM_REEL), dimension(6) :: param_out
        !/ Code de retour de la fonction de calcul de forces
        integer :: ier
        type(tm_code_retour) :: code_retour
        character(len=40) :: message_erreur
        
        !-- Debut du code
        !-- reinitialisation eventuelle de l'integrateur (seulement si le pas a changé)
        call ps_reinit_integ_cowell(pas_integ,str_int(iveh)%date_init_cowell)
        if(MSP_gen_messages("ps_integre_cowell")) return

       ! Si l'écart de dates est inférieur à 10-6 sec, on n'appelle pas 
        ! l'intégrateur, et on émet un warning
        if (abs(str_int(iveh)%date_prec_cowell - date_fin) > MSP_EPSILON_DATE) then
        
        !/ /!\ date_initiale doit être réinitialisée à chaque changement de phase d'intégration
        !/ Ex : après une poussée (intégration avec Gill)
        
        ! DM-ID 983 : Utilisation du second membre simplifié
        ! Si le flag du second membre simplifie est initialisé dans ps_reinit_integ_cowell a 1, 
        ! alors on intègre avec le second membre simplifié
        
        if(str_int(iveh)%second_membre_simplifie == 1) then
           call mu_integrer(ps_force_cowell, str_int(iveh)%integrateur_cowell, &
                str_int(iveh)%date_init_cowell, param, date_fin, param_out, &
                date_arret, ier, date_evt, code_retour, fcowell_simp=psi_fcowell_simp)
        else
           call mu_integrer(ps_force_cowell, str_int(iveh)%integrateur_cowell, &
                str_int(iveh)%date_init_cowell, param, date_fin, param_out, &
                date_arret, ier, date_evt, code_retour)
        end if

        if(code_retour%valeur < 0) then
           write (message_erreur,*) "Cowell - ",code_retour%valeur," -"
           call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_ERR_INTEG_MSPRO", &
                partie_variable = message_erreur)
           code_ret = code_retour%valeur
           return
        else 
           ! l'intégration avec COWELL s'est bien déroulée (ou warning)
           !
           ! code_retour%valeur >= 0
           if(code_retour%valeur > 0) then
              write (message_erreur,*) "Cowell - ",code_retour%valeur," -"
              call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_WARN_INTEG_MSPRO", &
                   partie_variable = message_erreur)
           end if

           !-- traitement des sorties de mu_integrer, traitement des erreurs
           param(:) = param_out(:)

           !-- Si la date d'arret est très proche de la date de fin demandée, on
           !   garde comme date de fin la date de fin demandée
           !   Ceci permet d'éviter des problèmes de différences numériques non 
           !   significatives sur les dates rendues
           if(abs(date_arret - date_fin) > MSP_EPSILON_DATE) then
              date_fin = date_arret
           end if

           !--- Pour le moment, pas de gestion d'evenements qui necessite des codes retours
           !    différents
           code_ret = 0

           str_int(iveh)%date_prec_cowell = date_fin

        end if !/ si code_retour 
     else
        !/ Intégration impossible, car date_fin = date_init
        !/ /!\ On ne sort pas pour autant en erreur
        ! Remarque : On n'émet pas pour autant de Warning,
        ! car il peut être utile d'intégrer sur une durée nulle,
        ! afin de récupérer simplement le vecteur d'état (FA-ID 886)
        
        param_out(:) = param(:)
           
     end if


   end subroutine ps_integre_cowell


  subroutine ps_force_cowell(date,x,xp,ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_force_cowell
!
!$Resume
!  Sous-programme calculant l'accélération du véhicule, en fonction de pos,vit
!  pour un appel à l'integrateur MSPRO
!
!$Description
!  Sous-programme calculant l'accélération du véhicule, en fonction de pos,vit
!  pour un appel à l'intégrateur MSPRO (routine passée en paramètre
!  de l'intégrateur.
!  /!\ Le calcul complet du second membre (ie : pos,vit -> vit,acc) est fait dans
!  la MSPRO. Cette routine se contente de rendre l'accélération, contrairement à la
!  routine ps_force_rkutta (pour l'intégrateur Gill) qui rend le vecteur dérivé (ie : vit,acc)
!
!$Auteur
!  Y. TANGUY (d'après pssecmb et ps_force_rkutta)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_force_cowell(date,x,xp,ier)
!.    type(tm_jour_sec) :: date
!.    real (KIND=pm_reel), dimension(:) :: x
!.    real (KIND=pm_reel), dimension(:) :: xp
!.    integer :: ier
!
!$Arguments
!>E     date  :<tm_jour_sec>       date en JJ50 CNES
!>E     x     :<pm_reel,DIM=(:)>   vecteur d'état à intégrer (position/vitesse en m-m/s)
!>S     xp    :<pm_reel,DIM=(:)>   accélération à la date demandée
!>S     ier   :<integer>           code retour (0 (ok) ou -1 : erreur dans psattit ; -2 : erreur dans psforce)
!
!$Common
!
!$Routines
!- psattit
!- Msp_annuler_probleme
!- ps_force_modele_complet
!
!$Include
!
!$Module
!#V
!- ps_bulletin
!- ps_attitude
!- ps_modeles
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use ps_bulletin
      use ps_attitude
      use ps_modeles

      implicit none

      !-- Arguments
      !------------
      type(tm_jour_sec), intent(IN)  :: date
      real (KIND=pm_reel), dimension(:), intent(IN)  :: x
      real (KIND=pm_reel), dimension(:), intent(OUT) :: xp
      integer, intent(OUT)             :: ier
      
      !-- Variables locales
      !--------------------
      real (KIND=pm_reel) :: t                         !/ durée en sec depuis t0
      real (KIND=pm_reel), dimension(3) :: pos,vit,acc 
      !/ Variables pour l'attitude
      real (KIND=pm_reel) :: angle1,angle2,angle3,angi1,angi2,angi3
      real (KIND=pm_reel), dimension(3,3) :: pgamav

      integer :: typa,repa
      
      ! Début du code
      !--------------

      pos(:) = x(1:3)
      vit(:) = x(4:6)

      ! Calcul de la durée (en sec) depuis le bulletin initial
      ! /!\ "t" sert ici dans ps_attit pour comparer la date courante aux dates relatives 
      ! des scenarios de lois d'attitude --> il est donc normal que ce soit une date 
      ! relative à la date bulletin, et non une date relative à la date de début d'intégration
      ! (= str_int%date_init_integ_cowell)
      !----------------------------------
      t = date - str_bul(iveh)%datbul0_js

      ! Init des parametres de sortie de psattit :
      pgamav(:,:) = 0._PM_REEL
      angle1 = 0._PM_REEL
      angle2 = 0._PM_REEL
      angle3 = 0._PM_REEL
      angi1 = 0._PM_REEL
      angi2 = 0._PM_REEL
      angi3 = 0._PM_REEL
      
!   * Calcul de l'attitude:
      call psattit (date,t,pos,vit,angle1,angle2,angle3,angi1,&
           angi2,angi3,pgamav,typa,repa)
      if ( MSP_gen_messages("ps_force_cowell")) then
         if(MSP_ERREUR) then
            ier = -1
            return     
         else
            call Msp_annuler_probleme()
         end if
      end if
         
!   * Calcul des accelerations:
      call ps_force_modele_complet (date,pos,vit,pgamav,repa,acc)
      if ( MSP_gen_messages("ps_force_cowell")) then
         if(MSP_ERREUR) then
            ier = -2
            return
         else
            call Msp_annuler_probleme()
         end if
      end if
      
      ! /!\ Adaptation de la routine pour le COWELL de MSPRO
      !     Les composantes 1 à 3 sont utilisées (accélération)
      xp(1:3) = acc(:)
      xp(4:6) = 0.0_PM_REEL

      ! Gestion du ier. fin normale : ier = 0
      ier = 0

    end subroutine ps_force_cowell


    subroutine psi_fcowell_simp (date,x,xp,ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psi_fcowell_simp
!
!$Resume
!  Calcule le 2nd membre simplifié pour le Cowell.
!
!$Description
!  Calcule le 2nd membre simplifié pour le Cowell, c'est à dire uniquement 
!  la force due au potentiel, avec un potentiel utilisant des degrés tesseraux et zonaux
!  à un ordre réduit : min (6, ordre choisi par l'utilisateur)
!
!$Auteur
!  Y.TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psi_fcowell_simp (date,x,xp,ier)
!.    type(tm_jour_sec) :: date 
!.    real(pm_reel),dimension(6) :: x 
!.    real(pm_reel),dimension(6) :: xp 
!.    integer :: ier 
!
!$Arguments
!>E     date  :<tm_jour_sec>       ! abscisse
!>E     x     :<pm_reel,DIM=(6)>   ! vecteur d'etat
!>S     xp    :<pm_reel,DIM=(6)>   ! derivee en t
!>S     ier   :<integer>           ! la fonction ps_force_modele_simplifie retourne un entier
!
!$Common
!
!$Routines
!- ps_force_modele_simplifie
!
!$Include
!
!$Module
!#V
!- ps_modeles
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use ps_modeles

      implicit none

      ! Arguments
      !==========
      type(tm_jour_sec),intent(in)                    ::  date     ! abscisse
      real(pm_reel),dimension(*),intent(in)           ::  x        ! vecteur d'etat
      real(pm_reel),dimension(*),intent(out)          ::  xp       ! derivee en t
      integer,intent(out)                             ::  ier      ! la fonction ps_force_modele_simplifie retourne un entier

      ! Début du code
      !==============


      ! DM-ID 983 : Second membre simpifié
      ! L'avant dernier argument permet d'utiliser la fonction pspoter avec second membre simplifié
      ! C'est a dire d'intégrer avec des termes zonaux et tesséraux ayant un ordre minimal (min(ordre choisi,6))
      call ps_force_modele_simplifie (date,x(1:3),xp(1:3))
      ier = 0

    end subroutine psi_fcowell_simp


    subroutine ps_libere_cowell()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_libere_cowell
!
!$Resume
!  Libération mémoire de l'intégrateur de COWELL.
!
!$Description
!  Libération mémoire de l'intégrateur de COWELL. Routine chapeau de 
!  mu_liberer_integrateur, qui gère les "flags" des structures PSIMU.
!
!$Auteur
!  Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_libere_cowell()
!
!$Arguments
!
!$Common
!
!$Routines
!- mu_liberer_integ
!- MSP_Signaler_Message
!
!$Include
!
!$Module
!#V
!- MSPRO
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      use MSPRO

      implicit none

      
      ! Variable locale
      type(tm_code_retour) :: code_retour
      
      ! Code

      ! Libération de l'intégrateur, si il était utilisé auparavant
      if(str_int(iveh)%init_cowell /= 0) then
         call mu_liberer_integ(str_int(iveh)%integrateur_cowell, code_retour)
         if(code_retour%valeur < 0) then
            call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_ERR_LIBERATION_MSPRO", &
                 partie_variable = "Cowell")
            return
         end if
         if(code_retour%valeur > 0) then 
            call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_WARN_LIBERATION_MSPRO", &
                 partie_variable = "Cowell")
         end if
      end if

      ! Mise à 0 du flag
      str_int(iveh)%init_cowell = 0

    end subroutine ps_libere_cowell



  subroutine ps_reinit_integ_cowell(nouveau_pas, nouvelle_date_init)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_reinit_integ_cowell
!
!$Resume
!  Initialisation ou réinitialisation de l'intégrateur de COWELL
!
!$Description
!  Initialisation ou réinitialisation de l'intégrateur de COWELL
!
!$Auteur
!  Y. TANGUY
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_reinit_integ_cowell(nouveau_pas, nouvelle_date_init)
!.    real(kind=PM_REEL) :: nouveau_pas
!.    type(tm_jour_sec) :: nouvelle_date_init
!
!$Arguments
!>E     nouveau_pas         :<PM_REEL>       nouveau pas (en secondes)
!>E     nouvelle_date_init  :<tm_jour_sec>   nouvelle date initiale (jj50)
!
!$Common
!
!$Routines
!- ps_libere_cowell
!- MSP_consulter_integrator
!- mu_creer_integrateur
!- MSP_Signaler_Message
!
!$Include
!
!$Module
!#V
!- MSPRO
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use MSPRO


    implicit none

    !-- Arguments
    !---------------------------
    real(kind=PM_REEL), intent(in) :: nouveau_pas
    type(tm_jour_sec),  intent(in) :: nouvelle_date_init


    !-- Variables locales
    !--------------------------
    type(tm_code_retour) :: code_retour
    real(kind=pm_reel) :: epsilon_init, epsilon_prog
    integer :: critere_convergence, circularisation,second_membre_simp,ordre_cowell
    logical :: mode_iteratif
    
    if ( ((abs(nouveau_pas) .different. str_int(iveh)%pas_cowell) .or.               &
         (nouvelle_date_init /= str_int(iveh)%date_init_cowell)) .and.   &
         (str_int(iveh)%init_cowell == 1) ) then
       
       ! (Le pas a change, 
       ! OU il y a une discontinuité, et on veut réinitialiser l'intégrateur)
       ! ET l'intégrateur était déjà initialisé
       
       !/ Réinitialisation de l'integrateur necessaire
       !/ 1) liberation memoire de l'integrateur
       !/ 2) positionne le flag "init_cowell" à 0
       call ps_libere_cowell()

    end if
     
    if(str_int(iveh)%init_cowell == 0) then
       
       !/ 2) creation nouvel integrateur
       !/ Note : les parametres integ_cowell%pas_integ
       !/ sont stockes dans ce module (ps_integration_cowell)
       !/
       !/ Le pas est toujours positif, c'est la date de sortie demandée
       !/ qui commande si on intègre en posigrade ou rétrograde.
       if(nouveau_pas /= 0.0) then
          str_int(iveh)%pas_cowell = abs(nouveau_pas)
       end if

       if(nouvelle_date_init /= str_int(iveh)%date_init_cowell) then
          str_int(iveh)%date_init_cowell = nouvelle_date_init
       end if
       
       

       !/ Récupération des paramètres personnalisés
       call MSP_consulter_integrator(str_int(iveh)%pinteg_courant,ordre=ordre_cowell,circularisation=circularisation,&
            second_membre_simp=second_membre_simp,convergence=critere_convergence,epsilon_init=epsilon_init,&
            epsilon_prog=epsilon_prog)
       if (MSP_gen_messages("ps_reinit_integ_cowell")) return

       !/ Critère de convergence (la MSPRO prend un logical en entrée)
       if (critere_convergence == 1) then
          mode_iteratif = .true.
       else 
          mode_iteratif = .false.
       end if
     
       ! DM-ID 983 : Second membre simplifié
       if (second_membre_simp == 1) then
          str_int(iveh)%second_membre_simplifie = 1
       else
          str_int(iveh)%second_membre_simplifie = 0
       end if

       !/ Explications par rapport à l'intégrateur de Cowell dans PSIMU
       !/
       !/ Note : même pour une intégration à rebours, on appelle l'intégrateur avec un pas positif
       !/        
       !/  mode "itératif" activé : recherche de la convergence dans la phase d'itération
       !/                  désactivé : le COWELL trouvera une solution, même si insatisfaisante.
       !/ Pour des trajectoires aux limites, il faut parfois désactiver le mode itératif
       !/ 
       !/ circularisation : il vaut mieux l'activer, quelque soit la trajectoire. Cela améliore la précision
       !/ sur les trajectoires "circulaires"
       !/ ireg = 1 -> régularisation en anomalie excentrique
       !/ 
       !/ epsilon / epsilon var. integ : Le Cowell manipule 2 x deux epsilons : 
       !/ - epsilon pour l'initialisation (phase de remplissage du "vecteur" du Cowell)
       !/ - epsilon pour la phase de progression 
       !/ -> ces deux epsilons affinent la précision sur les variables intégrées (accélération)
       !/ et il existe deux autres epsilons similaires, relatifs à la variable d'intégration (temps, ano vraie, ano excentrique)
       !/ - Un coefficient de 10e4 est utilisé pour les écarts entre les epsilons.
       !/ --> les epsilons sur la variable d'intégration sont 10e4 fois plus grands que ceux saisis par l'utilisateur.
       call mu_creer_integrateur(pm_cowell, 6, &
            str_int(iveh)%pas_cowell, str_int(iveh)%integrateur_cowell, code_retour, ordre=ordre_cowell, &
            icirc=circularisation, ireg=1, rxmu = str_mod(iveh)%gmu, mode_iteratif=mode_iteratif, &
            eps_init=epsilon_init, eps_prog=epsilon_prog, eps_init_var_integ=PS_COEFF_MULTI*epsilon_init,&
            eps_prog_var_integ=PS_COEFF_MULTI*epsilon_prog)

       if(code_retour%valeur < 0) then
          call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_ERR_INIT_MSPRO", &
               partie_variable = "Cowell")
          return
       end if
       if(code_retour%valeur > 0) then 
          call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_WARN_INIT_MSPRO", &
               partie_variable = "Cowell")
       end if
       
       str_int(iveh)%init_cowell = 1

       str_int(iveh)%date_prec_cowell = str_int(iveh)%date_init_cowell

    end if
    

  end subroutine ps_reinit_integ_cowell

  subroutine ps_init_boucle_cowell(datex,parex,inicow,iter,jflag,xpas)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_init_boucle_cowell
!
!$Resume
!
!$Description
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_init_boucle_cowell(datex,parex,inicow,iter,jflag,xpas)
!.    type(tm_jour_sec) :: datex
!.    real (KIND=pm_reel),dimension(6) :: parex
!.    integer :: inicow
!.    integer :: iter
!.    integer :: jflag
!.    real (KIND=pm_reel),dimension(PS_NVMAX) :: xpas
!
!$Arguments
!>E     datex   :<tm_jour_sec>              
!>E     parex   :<pm_reel,DIM=(6)>          
!>E/S   inicow  :<integer>                  
!>E/S   iter    :<integer>                  
!>E/S   jflag   :<integer>                  
!>S     xpas    :<pm_reel,DIM=(PS_NVMAX)>   
!
!$Common
!
!$Routines
!- ps_calcul_altitude_pente
!- ps_libere_cowell
!- MSP_consulter_integrator
!- psinpre
!- MSP_signaler_message
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
    type(tm_jour_sec), intent(in)                        :: datex
    real (KIND=pm_reel),dimension(6),      intent(in)    :: parex
    integer,                               intent(inout) :: inicow
    integer,                               intent(inout) :: iter
    integer,                               intent(inout) :: jflag
    real (KIND=pm_reel),dimension(PS_NVMAX), intent(out) :: xpas

    ! Variables locales
    !==================
    real(kind=pm_reel) :: ralt
    real(kind=pm_reel) :: xpash0,xpash1
    real(kind=pm_reel) :: pente

    ! Début du code
    !==============
    

    ! Calcul de l'altitude qui peut être géodésique 
    ! pour le calcul du critère d'arrêt
    ! DM-ID 959 : Calcul de la pente de la vitesse du véhicule
    ! Ce calcul permet de savoir si la trajectoire du véhicule
    ! est montante ou descendante dans le cas de h<=hstop.
    ! Le critère d'arret doit prendre en compte la valeur de cette pente
    !===============================================
    call ps_calcul_altitude_pente(parex,ralt,pente)
    
    ! Differents cas pour intégration
    !
    ! Altitude  
    !/\
    ! 
    !    4-)
    !
    ! -------------- h1
    !
    !    3-)
    !
    ! -------------- h2
    ! 
    !    2-)
    !
    ! -------------- h3
    !
    !    2-)
    !
    ! -------------- hstop
    !
    !    1-) si la pente de la vitesse du véhicule < 0 (arret - phase desccendante)  
    !    2-) si la pente de la vitesse du véhicule > = 0 (phase montante)
    !
    ! -------------- 0
    
    !1-)En dessous de hstop, le véhicule est en descente (pente de sa vitesse < 0) on arrete la simulation
    !============================================
    if ( ralt <= str_int(iveh)%hstop .and. pente < 0) then
       inicow = 0
       jflag  = 2
    
    !2-) En dessous de h2, on ne sert plus de Cowell mais de Gill
    !Traite le cas où en dessous de hstop mais que le véhicule est en montée (pente de sa vitesse >= 0) 
    !on continue la simulation      
    !===========================================
    else if ( ralt<= str_int(iveh)%h2) then
       iter = iter - 1
       inicow = 0
       jflag  = 4
    
    !3-) On initialise Cowell
    ! -> cas des altitudes moyennes, cas posigrade (inicow=2) ou rétrograde ( = -2)
    !====================
    else if (ralt <= str_int(iveh)%h1) then 
      
       if(inicow == 0) then
          ! Si cowell n'a pas été initialisé, on cree l'intégrateur
          ! Sinon, il sera libéré au préalable...
          call ps_libere_cowell()
       end if
       
       call MSP_consulter_integrator(str_int(iveh)%pinteg1,pas=xpash1)
       if(MSP_gen_messages("ps_init_boucle_cowell")) return

       
       if (abs(inicow) == abs(str_int(iveh)%itsgn*2)) then
          ! Cf explications ci-dessous ; si on change de sens d'intégration,
          ! on ne change pas forcément de pas, mais la variable xpas(iveh) doit changer
          inicow = str_int(iveh)%itsgn*2
          
          xpas(iveh) = str_int(iveh)%itsgn*xpash1
       else
          ! Par contre si l'intégrateur n'était pas initialisé (ie : inicow /= 2 et -2)
          ! alors on initialise l'intégrateur
          inicow = str_int(iveh)%itsgn*2
          
          xpas(iveh) = str_int(iveh)%itsgn*xpash1
          ! on pointe sur la structure contenant les paramètres courants
          str_int(iveh)%pinteg_courant = str_int(iveh)%pinteg1
          
          call psinpre (xpas(iveh),datex)
          if ( MSP_gen_messages("ps_init_boucle_cowell") ) return              
       end if
    
    !4-)On initialise Cowell
    ! -> cas des altitudes hautes, sens posigrade ou rétrograde
    !====================   
    else if (ralt > str_int(iveh)%h1) then 

       if(inicow == 0) then
          ! Si cowell n'a pas été initialisé, on cree l'intégrateur
          ! Sinon, il sera libéré au préalable...
          call ps_libere_cowell()
       end if

           
       call MSP_consulter_integrator(str_int(iveh)%pinteg0,pas=xpash0)
       if(MSP_gen_messages("ps_init_boucle_cowell")) return
     
       ! Est-ce que l'on a déjà intégrait déjà dans cette "plage" d'altitudes
       ! auparavant ?
       if (abs(inicow) == abs(str_int(iveh)%itsgn*1)) then
          ! Si on intégrait déjà avec le même pas de temps..
          ! .. alors, on reaffecte juste le pas xpas(iveh), qui doit 
          ! être positif ou négatif selon le sens d'intégration (car il sert à 
          ! calculer ypas(iveh)
          ! /!\ ON NE TOUCHE PAS A L'INITIALISATION DU COWELL (date initiale)
          
          inicow = str_int(iveh)%itsgn*1
          
          xpas(iveh) = str_int(iveh)%itsgn*xpash0
       else
          ! Auparavant, soit on n'utilisait pas le Cowell avec ce paramétrage
          ! --> on affecte le pas, 
          ! --> puis on initialise l'intégrateur (datex devient la date initiale du Cowell)

          inicow = str_int(iveh)%itsgn*1
          
          xpas(iveh) = str_int(iveh)%itsgn*xpash0
          ! on pointe sur la structure contenant les paramètres courants
          str_int(iveh)%pinteg_courant = str_int(iveh)%pinteg0

          call psinpre (xpas(iveh),datex)
          if ( MSP_gen_messages("ps_init_boucle_cowell") ) return              
       end if   
    else
       !Initialisation en fonction de l'altitude impossible
       !===================================================
       if (inicow == 0) then
          call MSP_signaler_message (cle_mes="PS_INTEGRATION_COWELL_001")
       end if
    end if

  end subroutine ps_init_boucle_cowell



end module ps_integration_cowell


