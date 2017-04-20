module ps_integration_rkutta

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ps_integration_rkutta
!
!$Resume
!  Module gérant l'appel à l'intégrateur de type Runge Kutta.
!
!$Description
!  Module gérant l'appel à l'intégrateur de type Runge Kutta.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: ps_integration_rkutta.F90 368 2013-02-19 14:43:59Z aadt $
!
!$Historique
!  $Log: ps_integration_rkutta.F90,v $
!  Revision 368  2013/02/19 aadt
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 1.44  2010/10/25 13:10:59  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.43  2009/08/19 09:17:15  tanguyy
!  AQ : rajout d'un implicit none manquant
!
!  Revision 1.42  2009/06/16 08:22:00  tanguyy
!  FA-ID 1306 : corrections de fuites memoires potentielles (structure separation et structure integrateur)
!
!  Revision 1.41  2009/04/14 16:09:46  tanguyy
!  DM-ID 1274 : pinteg_courant pointe sur la bonne structure, et pourra etre analyse lors des reinitialisations
!
!  Revision 1.40  2008/12/02 16:51:54  tanguyy
!  AQ : mise à jour des cartouches avant livraison de PSIMU V9.3
!
!  Revision 1.39  2008/11/18 13:38:07  tanguyy
!  DM-ID 733 : reorganisation des modules / utilisation de ps_force_modele_complet et ps_force_modele_simplifie
!  Revision 1.38  2008/10/17 10:07:12  mercadig
!  DM-ID 1058 Suppression double declaration allocstat
!  Revision 1.37  2008/10/15 13:10:25  tanguyy
!  DM-ID 1058 : controles lors des desallocations memoire
!  Revision 1.36  2008/09/04 07:53:06  tanguyy
!  DM-ID 1058 : phase 1 du portage / suppression des warnings - initialisations
!  Revision 1.35  2008/04/04 13:36:14  ttn
!  FA-ID 940 : Comparaison de deux entites physiques reelles
!  Revision 1.34  2008/04/03 14:30:18  ttn
!  FA-ID 658 : suppression des variables inutilisees
!  Revision 1.33  2008/03/31 15:31:29  ttn
!  correction MSP_gen_messages de ps_init_boucle_khutta
!  Revision 1.32  2008/03/06 15:50:13  huec
!  FA-ID 992 : Suppression du warning pour une tres petite duree d integration
!  Revision 1.31  2008/03/06 12:46:20  ttn
!  DM-ID 959 : Modification routine ps_calcul_altitude_pente + prise en compte dans ps_init_boucle_rkutta
!  Revision 1.30  2007/12/06 15:16:41  huec
!  DM-ID 733 : Annulation de la DM
!  Revision 1.29  2007/12/03 10:21:38  tanguyy
!  FA-ID 844 : correction du code du RK pour les separations en mode programme
!  Revision 1.28  2007/10/02 08:01:58  tanguyy
!  DM-ID 733 - utilisationd du calcul du potentiel de la MECASPA
!  Revision 1.27  2007/09/24 15:06:19  tanguyy
!  FA-ID 787 ; suppression des variables inutilisees
!  Revision 1.26  2007/07/09 12:09:59  tanguyy
!  Intégration des DM-ID 702, DM-ID 748 et DM-ID 692 sur les paramètres d'intégration
!  Revision 1.25  2007/06/20 12:33:31  vivaresf
!  Intégration FA 725 patch V8.7a3
!  Revision 1.24.2.2  2007/06/15 17:17:17  vivaresf
!  PSIMU V8.7a4 : essais pour fuites mémoire
!  Revision 1.24.2.1  2007/04/16 09:42:52  vivaresf
!  FA-ID 725 : optimisation et robustesse du patch V8.7a3
!  meilleure gestion des allocate (désallocation + status)
!  desallocation du COWELL si ré-initialisation de l'intégration
!  Revision 1.24  2006/10/19 15:09:17  tanguyy
!  DM-ID 478 : Cloture du FT (Integrateur de Cowell : modification de l interface)
!  Revision 1.23.2.2  2006/10/17 09:54:24  tanguyy
!  Finalisation DM-ID 478 (AQ : suppression var inutilisees, commentaires)
!  Revision 1.23.2.1  2006/10/13 07:55:22  tanguyy
!  DM-ID 478 : utilisation jj/sec. 1ere version fonctionnelle
!  Revision 1.23  2006/09/25 10:24:19  tanguyy
!  Correction d'une anomalie sur le RK : anomalie sur les pas fixes (le pas d'intégration était mal réglé)
!  Revision 1.22  2006/05/30 09:32:19  tanguyy
!  DM-ID 232 : nommage des parametres optionnels
!  Revision 1.21  2006/04/21 08:12:43  tanguyy
!  DM-ID 400 : Cloture du FT (Performances en temps de calcul sur les scnarios MECASPA)
!  Revision 1.20.4.1  2006/04/20 13:21:33  tanguyy
!  Correction d'une anomalie sur le n° d'itération en mode pas fixes
!  Revision 1.20  2006/03/01 13:34:59  tanguyy
!  FA-ID 480 : Cloture du FT (Erreur dans la gestion des pas fixes)
!  Revision 1.19.2.2  2006/03/01 13:34:45  tanguyy
!  FA-ID 480 : amélioration de la gestion des pas fixes
!  Revision 1.19.2.1  2006/02/27 16:07:18  tanguyy
!  FA-ID 480 : correction dans la gestion des pas fixes
!  Revision 1.19  2005/12/14 19:08:05  tanguyy
!  PSIMU V8-3
!  Revision 1.18  2005/12/14 13:22:40  vivaresf
!  DM-ID 397 : accepter les altitudes negatives si pas trop
!  Revision 1.17  2005/12/14 11:31:50  tanguyy
!  DM-ID 397 : Cloture du FT (Utiliser les intégrateurs MSPRO dans PSIMU)
!  Revision 1.16.2.2  2005/12/14 11:14:53  tanguyy
!  Test sur la date d arret d integration
!  Revision 1.16.2.1  2005/12/08 08:59:26  tanguyy
!  Version intermediaire avec integrateurs MSPRO (GILL seulement)
!  Revision 1.16  2005/11/10 18:37:07  vivaresf
!  Mise à jour des cartouches
!  Revision 1.15  2005/02/17 13:57:33  fabrec
!  DM-ID 98 : pas des ephemerides
!  Revision 1.14  2005/02/01 09:01:01  fabrec
!  DM-ID 175 : gestion de la memoire
!  Revision 1.13  2005/01/28 10:42:12  fabrec
!  DM-ID 175 : desallocations memoire
!  Revision 1.12  2005/01/17 15:26:54  fabrec
!  DM-ID 175 : utilisation des scenarios mecaspa
!  Revision 1.11  2004/12/10 16:58:37  fabrec
!  DM-ID 175 : utilisation des scenarios mecaspa
!  Revision 1.10  2003/03/19 15:26:31  laurent
!   remplacement de mugil4 par psgil4
!  Revision 1.9  2002/12/20 16:39:25  boschett
!  Utilisation du traitement d'erreur de la MECASPA
!  Revision 1.8  2002/12/12 15:05:24  boschett
!  Livraison Intermediaire 16/12/2002
!  Revision 1.7  2002/12/10 11:01:13  boschett
!   Ajout du traitement par défaut dans les structures if/elseif/else de la subroutine psbgill
!  Revision 1.6  2002/12/05 17:35:52  boschett
!  Utilisation de les operateurs MECASPA .egal. et .different. pour tester l'égalité (inégalité) entre réels
!  Revision 1.5  2002/12/04 14:25:24  boschett
!  Suppression des instructions return inutiles en fin de routine
!  Revision 1.4  2002/12/02 17:04:44  boschett
!  Suppression des variables locales déclarées et non utilisées
!  Revision 1.3  2002/11/26 16:59:52  boschett
!  Ajout de implicit none
!  Revision 1.2  2002/10/30 11:16:12  laurent
!  Ajout de commentaire fonctionnel
!  Revision 1.1.1.1  2002/09/30 14:59:34  laurent
!  Industrialisation PSIMU
!  Revision 1.5  2000/06/27 11:42:40  util_am
!  Ajout des modes d'attitude LVLH et Yaw Steering
!  Revision 1.4  2000/04/17 10:58:17  util_am
!  Version multi_satellite en Fortran90
!  Revision 1.3  1999/10/26 11:00:12  util_am
!  Mise à jour des cartouches
!  Revision 1.2  1999/08/04 11:28:14  util_am
!  Prise en compte de la gestion des erreurs de MECASPA
!
!$FinHistorique
!
!$Usage
!  use ps_integration_rkutta
!
!$Structure
!
!$Global
!
!$Common
!
!$Routines
!- psbgill
!- ps_force_rkutta
!- ps_integre_rkutta
!- ps_reinit_integ_rkutta
!#V
!- ps_init_boucle_rkutta
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
  character(len=256), private :: SVN_VER =  '$Id: ps_integration_rkutta.F90 368 2013-02-19 14:43:59Z aadt $'


   private :: ps_init_boucle_rkutta

   contains

      subroutine psbgill (iter0,iter,jter,iterw,ypas,datex,parex,jflag)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psbgill
!
!$Resume
!  Boucle sur l'intégrateur de Runge Kutta.
!
!$Description
!  Boucle sur l'intégrateur de Runge Kutta.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psbgill (iter0,iter,jter,iterw,ypas,datex,parex,jflag)
!.    integer :: iter0,iter,jter,iterw,jflag
!.    real (KIND=pm_reel) :: ypas,parex(6)
!.    type(tm_jour_sec) :: datex
!
!$Arguments
!>E/S   iter0  :<integer>           numéro de l'itération à la sortie du sous-programme
!>E/S   iter   :<integer>           numéro de l'itération courante (Cowell)
!>E/S   jter   :<integer>           numéro de l'itération courante (Runge Kutta)
!>E/S   iterw  :<integer>           compteur servant à l'écriture des résultats
!>E/S   ypas   :<pm_reel>           temps écoulé par rapport au début de la simulation (sec)
!>E/S   datex  :<tm_jour_sec>       date (Jours Juliens CNES)
!>E/S   parex  :<pm_reel,DIM=(6)>   position-vitesse courantes (m-m/s) 
!>E/S   jflag  :<integer>           indicateur de sortie du sous-programme:
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
!- ps_integre_rkutta
!- pspimpu
!- pspcont
!- psnewbl
!- MSP_consulter_separation
!- pseject
!- ps_maj_vecteur_etat
!- pswresu
!- psnetat
!- mu_liberer_integ
!- MSP_Signaler_Message
!#V
!- ps_init_boucle_rkutta
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

      integer, intent(INOUT)             :: iter0,iter,jter,iterw,jflag
      real (KIND=pm_reel), intent(INOUT) :: ypas,parex(6)
      type(tm_jour_sec), intent(inout)   :: datex

      ! Variables locales
      !==================

      integer :: iplus,kd,kf,kflag,ier,allocstat
      real (kind=pm_reel), dimension(PS_NVMAX) :: xpas
      character (LEN=8) :: pvar
      integer :: sc_type, sc_nloi, typloi
      real(KIND=pm_reel) :: tsep
      type(MSP_SEPARATION) :: loi_sep
      type(MSP_IMPULSION) :: loi_imp
      type(MSP_POUSSEE_CONTINUE) :: loi_cont
      type(MSP_POUSSEE_BULLETIN) :: loi_bul
      integer :: ntab
      real (KIND=pm_reel), pointer :: timp(:)
      real (KIND=pm_reel) :: datbul,datedeb,datetimp, tbul, datprec_tmp, date_ref
      
      type(tm_code_retour) :: code_retour
      real (KIND=pm_reel) :: duree_integ
      real (KIND=pm_reel) :: date_fin


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

      jter = iter + 1

      do while ( jter <= npas )
         

         ! Routine d'initialisation de la boucle de calcul du Runge-Kutta
         !========== DM-ID 748, DM-ID 702 ===============================
         call ps_init_boucle_rkutta(parex,jflag,jter,iter0,xpas)

         ! Sortie de la routine si on repart sur le Cowell, ou si l'altitude est 
         ! trop basse
         if (jflag == 2 .or. jflag == 3) goto 999
                  
         iterw=jter

!/   Determination de l'evenement du pas suivant (iplus) :
!/   1 => poussée; 2 => séparation; 3 => fin d'extrapolation; 4 => événement  
         call pstsuiv (iplus)
         if ( MSP_gen_messages("psbgill") ) return                       

         if ( iplus == 1 ) then

             typloi = MSP_type_loi (scenar_pro(iveh), id=iloi_scenar_pro(iveh))
             if ( MSP_gen_messages("psbgill") ) return

             if (typloi == MSP_ENUM_LOI_CONT) then
                !/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
                call MSP_consulter_scenario (scenar_pro(iveh), loi_cont=loi_cont, id=iloi_scenar_pro(iveh))
                if ( MSP_gen_messages("psbgill") ) return
                
                call MSP_consulter_poussee_continue (loi_cont, ntab=ntab, dates=timp)
                if ( MSP_gen_messages("psbgill") ) return
                
                ! Libération de la mémoire
                call MSP_effacer_poussee_continue (loi_cont)
                if ( MSP_gen_messages("psbgill") ) return

             elseif (typloi == MSP_ENUM_LOI_IMP) then
                !/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
                call MSP_consulter_scenario (scenar_pro(iveh), loi_imp=loi_imp, id=iloi_scenar_pro(iveh))
                if ( MSP_gen_messages("psbgill") ) return
                
                call MSP_consulter_impulsion (loi_imp, date=datetimp)
                if ( MSP_gen_messages("psbgill") ) return
                ntab = 1
                if (associated(timp)) then
                   deallocate(timp, stat=allocstat)
                   if (allocstat < 0) then
                      call MSP_signaler_message(cle_mes="PS_ERR_DESALLOC",partie_variable="psbgill")
                      return
                   end if
                end if
                allocate(timp(1))
                timp(1) = datetimp

             elseif (typloi == MSP_ENUM_LOI_BUL) then
                !/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
                call MSP_consulter_scenario (scenar_pro(iveh), date_ref=date_ref)
                if ( MSP_gen_messages("psbgill") ) return
                call MSP_consulter_scenario (scenar_pro(iveh), loi_bul=loi_bul, id=iloi_scenar_pro(iveh))
                if ( MSP_gen_messages("psbgill") ) return
                
                call MSP_consulter_poussee_bulletin (loi_bul, datedeb=datedeb, datbul = datbul)
                if ( MSP_gen_messages("psbgill") ) return
                
                if (associated(timp)) then
                   deallocate(timp, stat=allocstat)
                   if (allocstat < 0) then
                      call MSP_signaler_message(cle_mes="PS_ERR_DESALLOC",partie_variable="psbgill")
                      return
                   end if
                end if
                allocate(timp(2))
                timp(1:2) = 0._PM_REEL

                timp(1) = datedeb
                tbul = (datbul - date_ref)*86400._pm_reel
                ! Arrondi à la milli-seconde:
                timp(2) = real(int(tbul),KIND=pm_reel) + &
                    real(nint((tbul-int(tbul))/datprec_tmp),KIND=pm_reel)*datprec_tmp
                ntab = 2
                
             endif

!           Appel a Gill dans le cas d'une poussee:
!           --------------------------------------
            if (str_int(iveh)%itsgn > 0) then
!/     sens d'integration posigrade 
               kd = 1
               kf = ntab 
            else
!/     sens d'intgration retrograde
               kd = ntab
               kf = 1
            endif

            !/ Avant d'appliquer la poussée, il faut s'assurer d'arriver jusqu'à cette date
            !/ de début de poussée.
            if (str_ecr(iveh)%ipas == 2) then
               duree_integ = str_ecr(iveh)%pas_sortie
            else
               duree_integ = xpas(iveh)
            end if

            if (str_int(iveh)%tsign*(ypas+duree_integ) >= (str_int(iveh)%tsign*timp(kd))) then
               if ( (str_int(iveh)%tsign*ypas) < (str_int(iveh)%tsign*timp(kd)) ) then
!/   Si (la date depuis le debut de la simulation) < (date de poussee courante) =< (la date depuis 
!/       le debut de la simulation + pas de temps courant)  

                  date_fin=timp(kd)

                  call ps_integre_rkutta(ypas, date_fin, xpas(iveh), parex, ier)                       

                  if(ier < 0) return
                  ypas=date_fin
                  
                  datex=str_bul(iveh)%datbul0_js+ypas
               endif

               if (typloi == MSP_ENUM_LOI_IMP) then
!/    Si le type de loi de pousse courante est impulsionnel--> calcul du changement de trajectoire due a cet 
!/    impulsion
                  call pspimpu (str_int(iveh)%itsgn,str_int(iveh)%tsign,ypas,datex,parex,jter)
                  if ( MSP_gen_messages("psbgill") ) return                      
               else if (typloi == MSP_ENUM_LOI_CONT) then
!/    Si le type de loi de poussee courante est continu --> calcul de la nouvelle trajectoire propulsee 
                  call pspcont (str_int(iveh)%itsgn,str_int(iveh)%tsign,str_int(iveh)%hstop,&
                       str_int(iveh)%tmax,ypas,datex,parex,jter,kflag)
                  if ( MSP_gen_messages("psbgill") ) return                             
                  
                  if ( kflag /= 0 ) then
!/   Si la sortie de pspcont est non nominale (due a un tmax atteint (1) ou une altitude d'arret atteinte (2)) 
!/                      --> sortie de la procedure 
                     if (ypas .egal. timp(kf)) then
                        iloi_scenar_pro(iveh) = iloi_scenar_pro(iveh) + str_int(iveh)%itsgn
                     endif
                     jflag = kflag
                     goto 999
                  endif
               else

!/   Si la loi de poussee est donnee par un bulletin --> calcul de la tajectoire due a cette manoeuvre
                  call psnewbl (str_int(iveh)%itsgn,str_int(iveh)%tsign,ypas,datex,parex,jter)
                  if ( MSP_gen_messages("psbgill") ) return                       
               endif
!/   Incrementation du numero de la poussee courante
               iloi_scenar_pro(iveh) = iloi_scenar_pro(iveh) + str_int(iveh)%itsgn
               goto 420
            end if

         else if ( iplus == 2 ) then
            
!           Appel a Gill dans le cas d'une separation:
!           -----------------------------------------
            !  Lecture de la structure MSP_SCENARIO_LOI
            call MSP_consulter_scenario (scenar_sep(iveh), type=sc_type, nloi=sc_nloi)
            if ( MSP_gen_messages("psbgill") ) return

            !   Si le scenario est de type separation      
            if ( sc_type == MSP_ENUM_SEPARATION ) then
               if (sc_nloi > 0) then

                  call MSP_effacer_separation (loi_sep, nul=.true.)
                  
                  call MSP_consulter_scenario (scenar_sep(iveh), loi_sep=loi_sep)
                  if ( MSP_gen_messages("psbgill") ) return
                  
                  call MSP_consulter_separation (loi_sep, date=tsep)
                  if ( MSP_gen_messages("psbgill") ) return
                                      
                  call MSP_effacer_separation (loi_sep)
                  
                  !/ Avant d'appliquer la séparation, il faut s'assurer d'arriver jusqu'à cette date
                  !/ de début de poussée.
                  if (str_ecr(iveh)%ipas == 2) then
                     duree_integ = str_ecr(iveh)%pas_sortie
                  else
                     duree_integ = xpas(iveh)
                  end if

                  if (str_int(iveh)%tsign*(ypas+duree_integ) >= (str_int(iveh)%tsign*tsep)) then
                     if ( (str_int(iveh)%tsign*ypas) < (str_int(iveh)%tsign*tsep) ) then
!/   Si (la date depuis le debut de la simulation) < (date de la separation courante) =< (la date depuis 
!/       le debut de la simulation + pas de temps courant)  

                        date_fin=tsep
                        call ps_integre_rkutta(ypas, date_fin, xpas(iveh), parex, ier)                       
                        if(ier < 0) return
      
                        ypas=date_fin
                        datex=str_bul(iveh)%datbul0_js+ypas
                     endif

!/   Calcul du changement de trajectoire du a la separation
                     call pseject (str_int(iveh)%itsgn,str_int(iveh)%tsign,ypas,datex,parex,jter)
                     if ( MSP_gen_messages("psbgill") ) return                            
                     iloi_scenar_sep(iveh) = iloi_scenar_sep(iveh) + str_int(iveh)%itsgn
                     if ( ( iloi_scenar_sep(iveh) <=  sc_nloi) .and. ( iloi_scenar_sep(iveh) >= 1) ) then
                        ! -- Positionnement de la loi_courante sur la loi iloi_scenar_sep(iveh)
                        call MSP_consulter_scenario(scenar_sep(iveh), loi_sep, id=iloi_scenar_sep(iveh))
                        if ( MSP_gen_messages("psbgill") ) return
                        call MSP_effacer_separation(loi_sep)
                     endif
                     goto 420
                  endif
               endif
            end if

         else if ( iplus == 3 ) then

!           Appel a Gill juste a la date de fin:
!           -----------------------------------
            if (str_int(iveh)%tsign*(ypas+xpas(iveh)) >= (str_int(iveh)%tsign*str_int(iveh)%tmax)) then
               if ( ypas .different. str_int(iveh)%tmax ) then
!/   Si (la date depuis le debut de la simulation + pas de temps courant) >= (date maximale de calcul) et
!/    la date depuis le debut de la simulation est differente de la date maximale de calcul

!/   modification du pas d'integration et integration du systeme non lineaire grace aux outils de la mslib 
                  date_fin = str_int(iveh)%tmax
                  call ps_integre_rkutta(ypas, date_fin, xpas(iveh), parex, ier)                       
                  if(ier < 0) return
                  ypas=date_fin
                  datex=str_bul(iveh)%datbul0_js+ypas
               endif
               jflag = 1
               goto 999
            end if

         else if ( iplus == 4 ) then

!           Appel a Gill juste a l'evenement suivant:
!           ----------------------------------------

            !/ Avant d'écrire l'evt, il faut s'assurer de ne pas le
            !/ dépasser avec le prochain pas d'intégration
            if (str_ecr(iveh)%ipas == 2) then
               duree_integ = str_ecr(iveh)%pas_sortie
            else
               duree_integ = xpas(iveh)
            end if
            
            !/   Si (la date depuis le debut de la simulation +duree integ théorique) >= (date de l'evenement courant)
            !/   Modification du pas d'integration et integration du systeme non lineaire grace aux outils de la mslib
            if (str_int(iveh)%tsign*(ypas+duree_integ) >= (str_int(iveh)%tsign*str_eve(iveh)%deve(str_eve(iveh)%neve))) then

               if(str_int(iveh)%tsign*(ypas) < str_int(iveh)%tsign*str_eve(iveh)%deve(str_eve(iveh)%neve)) then

                  date_fin = str_eve(iveh)%deve(str_eve(iveh)%neve)

                  call ps_integre_rkutta(ypas, date_fin, xpas(iveh), parex, ier)                       
                  if(ier < 0) return
                  ypas=date_fin
                  datex=str_bul(iveh)%datbul0_js+ypas

               end if

               if ( ilogeve /= 0 ) then
                  !/   Si le numero logique du fichier EVENT (evenement) est different de 0 (var. globale) -->

                  !/      Mise a jour des variables a la date courante 
                  call ps_maj_vecteur_etat (ypas,datex,parex)
                  if ( MSP_gen_messages("psbgill") ) return                            

                  !/   ecriture des donnees dans le fichier EVENT (evenement)
                  call pswresu (2,ilogeve,0,40,jter,ypas,str_gen(iveh)%etat,&
                       str_ecr(iveh)%nvarw,str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,str_eve(iveh)%ifore,&
                       str_eve(iveh)%neve,str_eve(iveh)%aeve)
                  if ( MSP_gen_messages("psbgill") ) return                            
               endif

!/  Incrementation de l'evenement courant
               str_eve(iveh)%neve = str_eve(iveh)%neve + str_int(iveh)%itsgn
               jter = jter - 1
               goto 420
            end if

          else if ( iplus == 5 ) then

!           Appel a Gill juste a la date de fin:
!           -------------------------------------
            if (str_int(iveh)%tsign*(ypas+str_ecr(iveh)%pas_sortie) >= (str_int(iveh)%tsign*str_int(iveh)%tmax)) then
               if ( ypas .different. str_int(iveh)%tmax ) then
                  date_fin =  str_int(iveh)%tmax
                  call ps_integre_rkutta(ypas, date_fin, xpas(iveh), parex, ier)                       
                  if(ier < 0) return
                  ypas=date_fin
                  datex=str_bul(iveh)%datbul0_js+ypas
               endif
               jflag = 1
               goto 999
            end if

!           Appel a Gill juste a la sortie suivante:
!           ------------------------------------------
            if (str_int(iveh)%tsign*(ypas+str_ecr(iveh)%pas_sortie) >= &
                 str_int(iveh)%tsign*str_ecr(iveh)%ypas_sorties(str_ecr(iveh)%nsor)) then

               date_fin = str_int(iveh)%tsign*str_ecr(iveh)%ypas_sorties(str_ecr(iveh)%nsor)
               call ps_integre_rkutta(ypas, date_fin, xpas(iveh), parex, ier)                       
               if(ier < 0) return
               ypas=date_fin
               datex=str_bul(iveh)%datbul0_js+ypas
  
               if ( ilogeph /= 0 ) then
                  call ps_maj_vecteur_etat (ypas,datex,parex)
                  if ( MSP_gen_messages("psbgill") ) return              
                  call pswresu (1,ilogeph,iscreen,0,iter,ypas,str_gen(iveh)%etat,&
                       str_ecr(iveh)%nvarw,str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,&
                       str_eve(iveh)%ifore,str_eve(iveh)%neve,str_eve(iveh)%aeve)
                  if ( MSP_gen_messages("psbgill") ) return              
                  goto 420
               endif
            end if

         else
!           Type d'evenement à suivre inconnu
!           ---------------------------------
            write (pvar,'(I2)') iplus
            call MSP_signaler_message (cle_mes="PS_INTEGRATION_RKUTTA_001",partie_variable=pvar)
         end if


         if ( str_ecr(iveh)%ipas == 2 ) then

!           Appel a Gill sans poussees, separations ...
!           =========================================
            

            date_fin = ypas + str_ecr(iveh)%pas_sortie
            call ps_integre_rkutta(ypas, date_fin, xpas(iveh), parex, ier)                       
            if(ier < 0) return
            ypas=date_fin
            datex=str_bul(iveh)%datbul0_js + ypas
            
            if ( ilogeph /= 0 ) then
               call ps_maj_vecteur_etat (ypas,datex,parex)
               if ( MSP_gen_messages("psbgill") ) return                                
               call pswresu (1,ilogeph,iscreen,0,jter,ypas,str_gen(iveh)%etat,&
                    str_ecr(iveh)%nvarw,str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,str_eve(iveh)%ifore,&
                    str_eve(iveh)%neve,str_eve(iveh)%aeve)
               if ( MSP_gen_messages("psbgill") ) return
            endif
         else

!           Appel a Gill sans poussees, separations ...
!           =======================================

            date_fin = ypas + xpas(iveh)
            call ps_integre_rkutta(ypas, date_fin, xpas(iveh), parex, ier)                       
            if(ier < 0) return
            ypas=date_fin
            datex=str_bul(iveh)%datbul0_js + ypas

            if ( ilogeph /= 0 ) then
!/  Si le numero logique du fichier EPHEM est different de 0 (var. globale)
               if ( mod(iterw,str_ecr(iveh)%indw) == 0 ) then
!/  Mise a jour des variables 
                  call psnetat (ypas,datex,parex)
                  if ( MSP_gen_messages("psbgill") ) return                                

!/  Ecriture des donnees dans le fichier EPHEM
                  call pswresu (1,ilogeph,iscreen,0,jter,ypas,str_gen(iveh)%etat,&
                       str_ecr(iveh)%nvarw,str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,str_eve(iveh)%ifore,&
                       str_eve(iveh)%neve,str_eve(iveh)%aeve)
                  if ( MSP_gen_messages("psbgill") ) return                            
              
               endif
            endif
         endif


420      jter = jter + 1
 
      enddo

!***********************************************************************
!*    Fin de la boucle                                                 *
!***********************************************************************

      jflag = 0

      ! Deallocations memoire
999   if (associated(timp)) deallocate(timp, stat=allocstat)

      
      ! DM 397 
      if(str_int(iveh)%init_rkutta == 1) then
         call mu_liberer_integ(str_int(iveh)%integrateur_rkutta, code_retour)
         if(code_retour%valeur < 0) then
            call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_ERR_LIBERATION_MSPRO", &
                 partie_variable = "Gill / Runge-Kutta")
            return
         end if
         if(code_retour%valeur > 0) then 
            call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_WARN_LIBERATION_MSPRO", &
                 partie_variable = "Gill / Runge-Kutta")
         end if
      end if
      
      str_int(iveh)%init_rkutta = 0
      
      end subroutine psbgill


      subroutine ps_force_rkutta (t,x,xp,ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_force_rkutta
!
!$Resume
!  Sous-programme calculant le second membre de (Xp,Xpp)=(vit,acc) 
!  pour un appel à l'integrateur MSPRO
!
!$Description
!  Sous-programme calculant le second membre de (Xp,Xpp)=(vit,acc) 
!  pour un appel à l'intégrateur MSPRO (routine passée en paramètre
!  de l'intégrateur
!
!$Auteur
!  Y. TANGUY (d'après pssecmb)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_force_rkutta (t,x,xp,ier)
!.    real (KIND=pm_reel) :: t
!.    real (KIND=pm_reel), dimension(:) :: x
!.    real (KIND=pm_reel), dimension(:) :: xp
!.    integer :: ier
!
!$Arguments
!>E     t    :<pm_reel,DIM=(IN)>   variable indépendante (temps en secondes)
!>E     x    :<pm_reel,DIM=(:)>    vecteur d'état à intégrer (position/vitesse en m-m/s)
!>S     xp   :<pm_reel,DIM=(:)>    dérivée du vecteur d'état
!>S     ier  :<integer>            code retour  (0: OK, -1 : erreur psattit, -2 erreur psforce)
!
!$Common
!
!$Routines
!- psattit
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

      !/ Arguments
      real (KIND=pm_reel), intent(IN)  :: t
      real (KIND=pm_reel), dimension(:), intent(IN)  :: x
      real (KIND=pm_reel), dimension(:), intent(OUT) :: xp
      integer, intent(OUT)             :: ier

      !/ Variables locales
      type(tm_jour_sec) :: date
      real (KIND=pm_reel), dimension(3) :: pos,vit,acc
      real (KIND=pm_reel) :: angle1,angle2,angle3,angi1,angi2,angi3
      real (KIND=pm_reel), dimension(3,3) :: pgamav

      integer :: typa,repa

      !/ Code
      pos(:) = x(1:3)
      vit(:) = x(4:6)

      date = str_bul(iveh)%datbul0_js + t

!   * Calcul de l'attitude:
      call psattit (date,t,pos,vit,angle1,angle2,angle3,angi1,angi2,angi3,&
           pgamav,typa,repa)
      if ( MSP_gen_messages("ps_force_rkutta") .and. MSP_ERREUR) then
         ier = -1
         return                            
      end if

!   * Calcul des accelerations:
      call ps_force_modele_complet (date,pos,vit,pgamav,repa,acc)
      if ( MSP_gen_messages("ps_force_rkutta") .and. MSP_ERREUR) then
         ier = -2
         return                            
      end if
      
      xp(1:3) = vit(:)
      xp(4:6) = acc(:)

      ! Gestion du ier
      ier = 0

    end subroutine ps_force_rkutta


    subroutine ps_integre_rkutta(t_init, t_fin, pas_integ, param, code_ret)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_integre_rkutta
!
!$Resume
!  Routine chapeau de l'intégrateur Runge-Kutta
!
!$Description
!  Routine chapeau de l'intégrateur Runge-Kutta (appel à mu_integrer de la MSPRO)
!
!$Auteur
!  Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_integre_rkutta(t_init, t_fin, pas_integ, param, code_ret)
!.    real(kind=PM_REEL) :: t_init
!.    real(kind=PM_REEL) :: t_fin
!.    real(kind=PM_REEL) :: pas_integ 
!.    real(kind=PM_REEL), dimension(6) :: param
!.    integer :: code_ret
!
!$Arguments
!>E     t_init     :<PM_REEL>           date initiale (date relative à datbul0 en secondes)
!>E/S   t_fin      :<PM_REEL>           date finale (théorique/effective) en secondes
!>E     pas_integ  :<PM_REEL>           pas d'intégration en secondes
!>E/S   param      :<PM_REEL,DIM=(6)>   paramètres initiaux/finaux
!>S     code_ret   :<integer>           code retour (-1 si erreur dans mu_integrer)
!
!$Common
!
!$Routines
!- ps_reinit_integ_rkutta
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
      
      !-- Parametres
      real(kind=PM_REEL), intent(in) :: t_init
      real(kind=PM_REEL), intent(inout) :: t_fin
      real(kind=PM_REEL), intent(in) :: pas_integ      
      real(kind=PM_REEL), dimension(6), intent(inout) :: param
      integer, intent(out) :: code_ret
      
      !-- Variables locales
      
      !/ t_arret = t_fin si l'integration s'est terminée sans evt "bloquant" rencontré
      !/ t_arret = t_evt si un evt provoque un arret
      real(kind=PM_REEL) :: t_arret 
      real(kind=PM_REEL) :: t_evt

      !/ Param_out : parametres calcules par l'integrateur
      real(kind=PM_REEL), dimension(6) :: param_out
      !/ Code de retour de la fonction de calcul de forces
      integer :: ier

      type(tm_code_retour) :: code_retour

      !-- Debut du code
      !-- reinitialisation eventuelle de l'integrateur
      call ps_reinit_integ_rkutta(pas_integ)
      if(MSP_gen_messages("ps_integre_rkutta")) return

      if(abs(t_fin - t_init) > MSP_EPSILON_DATE) then
         !-- integration : appel a mu_integrer
         call mu_integrer(ps_force_rkutta, str_int(iveh)%integrateur_rkutta, &
              t_init, param, t_fin, param_out, t_arret, ier, &
              t_evt, code_retour)
         
         if(code_retour%valeur < 0) then
            call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_ERR_INTEG_MSPRO", &
                 partie_variable = "Gill / Runge-Kutta")
            code_ret = -1
            return
         end if
         if(code_retour%valeur > 0) then 
            call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_WARN_INTEG_MSPRO", &
                 partie_variable = "Gill / Runge-Kutta")
         end if

         !-- traitement des sorties de mu_integrer, traitement des erreurs
         param(:) = param_out(:)
         

         !-- Si la date d'arret est très proche de la date de fin demandée, on
         !   garde comme date de fin la date de fin demandée
         !   Ceci permet d'éviter des problèmes de différences numériques non 
         !   significatives sur les dates rendues
         if(abs(t_arret - t_fin) > MSP_EPSILON_DATE) then
            t_fin = t_arret
         end if
      else
         ! L'écart entre la date d'appel et la date de fin d'intégration 
         ! est inférieur à 10-6 sec : on copie les paramètres.
         ! Remarque : On n'émet pas pour autant de Warning,
         ! car il peut être utile d'intégrer sur une durée nulle,
         ! afin de récupérer simplement le vecteur d'état (FA-ID 886)
         param_out(:) = param(:)
      end if

      code_ret = 0

    end subroutine ps_integre_rkutta

    
    subroutine ps_reinit_integ_rkutta(nouveau_pas)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_reinit_integ_rkutta
!
!$Resume
!  Création ou réinitialisation de l'intégrateur de Runge-Kutta
!
!$Description
!  Création ou réinitialisation de l'intégrateur de Runge-Kutta
!
!$Auteur
!  Y. TANGUY
!
!$Usage
!  call ps_reinit_integ_rkutta(nouveau_pas)
!.    real(kind=PM_REEL) :: nouveau_pas
!
!$Arguments
!>E     nouveau_pas  :<PM_REEL>   Pas d'intégration en secondes
!
!$Common
!
!$Routines
!- mu_liberer_integ
!- MSP_Signaler_Message
!- mu_creer_integrateur
!
!$Include
!
!$Module
!#V
!- ps_generalites
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
      
      use ps_generalites
      use MSPRO

      implicit none

      !/ Argument
      real(kind=PM_REEL), intent(in) :: nouveau_pas
      
      !/ Variable locale
      type(tm_code_retour) :: code_retour

      !/ Code
      
      !/ Libération de l'integrateur necessaire
      !/ si le pas a changé, et que l'intégrateur n'avait pas été libéré
      if (abs(nouveau_pas-str_int(iveh)%pas_rkutta) > PS_EPSILON .and. &
           str_int(iveh)%init_rkutta == 1 ) then
         
         !/ Réinitisation de l'integrateur necessaire
         !/ 1) liberation memoire de l'integrateur
         call mu_liberer_integ(str_int(iveh)%integrateur_rkutta, code_retour)
         if(code_retour%valeur < 0) then
            call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_ERR_LIBERATION_MSPRO", &
                 partie_variable = "Gill / Runge-Kutta")
            return
         end if
         if(code_retour%valeur > 0) then 
            call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_WARN_LIBERATION_MSPRO", &
                 partie_variable = "Gill / Runge-Kutta")
         end if

         str_int(iveh)%init_rkutta = 0

      end if

      ! création de l'intégrateur
      if (str_int(iveh)%init_rkutta == 0) then

         !/ 2) creation nouvel integrateur
         !/ Note : les parametres dimension_integration et pas_integ_rkutta
         !/ sont stockes dans ce module (ps_integration_rkutta)
         if(nouveau_pas /= 0.0) then
            str_int(iveh)%pas_rkutta = abs(nouveau_pas)
         end if
         call mu_creer_integrateur(type_integrateur=pm_gill, n=6, &
              pas=str_int(iveh)%pas_rkutta, &
              integrateur=str_int(iveh)%integrateur_rkutta, &
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

         str_int(iveh)%init_rkutta = 1
      end if

    end subroutine ps_reinit_integ_rkutta
    
    
    subroutine ps_init_boucle_rkutta(parex,jflag,jter,iter0,xpas)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_init_boucle_rkutta
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
!  call ps_init_boucle_rkutta(parex,jflag,jter,iter0,xpas)
!.    real (KIND=pm_reel),dimension(6) :: parex
!.    integer :: jflag
!.    integer :: jter
!.    integer :: iter0
!.    real (KIND=pm_reel),dimension(PS_NVMAX) :: xpas
!
!$Arguments
!>E     parex  :<pm_reel,DIM=(6)>          
!>E/S   jflag  :<integer>                  
!>E/S   jter   :<integer>                  
!>S     iter0  :<integer>                  
!>S     xpas   :<pm_reel,DIM=(PS_NVMAX)>   
!
!$Common
!
!$Routines
!- ps_calcul_altitude_pente
!- mu_liberer_integ
!- MSP_Signaler_Message
!- MSP_consulter_integrator
!- ps_reinit_integ_rkutta
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
      real (KIND=pm_reel),dimension(6),      intent(in)    :: parex
      integer,                               intent(inout) :: jflag
      integer,                               intent(inout) :: jter
      integer,                               intent(out)   :: iter0
      real (KIND=pm_reel),dimension(PS_NVMAX), intent(out) :: xpas
    

      ! Variables locales
      !==================
      real(kind=pm_reel) :: ralt
      real(kind=pm_reel) :: xpash2,xpash3
      real(kind=pm_reel) :: pente
      type(tm_code_retour) :: code_retour

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
     !    3-)
     !
     ! -------------- h1
     !
     !    3-)
     !
     ! -------------- h2
     ! 
     !    4-)
     !
     ! -------------- h3
     !
     !    2-)
     !
     ! -------------- hstop
     !
     !    1-) si la pente de la vitesse du véhicule < 0  (arret - pahse descendante)
     !    2-) si la pente de la vitesse du véhicule > = 0 (phase montante)
     !
     ! -------------- 0

      ! 1-)En dessous de hstop, le véhicule est en descente (pente de sa vitesse < 0) on arrete la simulation
      ! ============================================
      if ( ralt <= str_int(iveh)%hstop .and. pente < 0) then
         jflag = 2       
         
         ! DM 397 
         call mu_liberer_integ(str_int(iveh)%integrateur_rkutta, code_retour)
         if(code_retour%valeur < 0) then
            call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_ERR_LIBERATION_MSPRO", &
                 partie_variable = "Gill / Runge-Kutta")
            return
         end if
         if(code_retour%valeur > 0) then 
            call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_WARN_LIBERATION_MSPRO", &
                 partie_variable = "Gill / Runge-Kutta")
         end if

      ! 2-)En dessous de h3, xpas=xpash3 (pas de fin d'integration de RK)
      ! Traite le cas où en dessous de hstop mais que le véhicule est en montée (pente de sa vitesse >= 0) 
      ! on continue la simulation
      ! ===================================================================================================	   
      else if (ralt <= str_int(iveh)%h3) then
         call MSP_consulter_integrator(str_int(iveh)%pinteg3,pas=xpash3)
         if(MSP_gen_messages("ps_init_boucle_rkutta")) return

         xpas(iveh) = str_int(iveh)%tsign * xpash3
         
         ! on pointe sur la structure contenant les paramètres courants
         str_int(iveh)%pinteg_courant = str_int(iveh)%pinteg3

         !/ Si besoin, on reinitialise le pas d'integration
         call ps_reinit_integ_rkutta(abs(xpas(iveh)))
         if(MSP_gen_messages("ps_init_boucle_rkutta")) return
      
      ! 3-)Au dessus de h2, on revient au Cowell
      ! ====================================
      else if ( ralt > str_int(iveh)%h2 ) then
         iter0 = jter - 1
         jflag = 3

         ! DM 397 
         call mu_liberer_integ(str_int(iveh)%integrateur_rkutta, code_retour)
         if(code_retour%valeur < 0) then
            call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_ERR_LIBERATION_MSPRO", &
                 partie_variable = "Gill / Runge-Kutta")
            return
         end if
         if(code_retour%valeur > 0) then 
            call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_WARN_LIBERATION_MSPRO", &
                 partie_variable = "Gill / Runge-Kutta")
         end if

         str_int(iveh)%init_rkutta = 0
      
      ! 4-) (entre h2 et h3)
      else
         !Sinon, xpas=xpash2 (pas d'integration de base de RK)
         !===================================
         
         call MSP_consulter_integrator(str_int(iveh)%pinteg2,pas=xpash2)
         if(MSP_gen_messages("ps_init_boucle_rkutta")) return

         xpas(iveh) = str_int(iveh)%tsign * xpash2
     
         ! on pointe sur la structure contenant les paramètres courants
         str_int(iveh)%pinteg_courant = str_int(iveh)%pinteg2
         
         !/ Si besoin, on reinitialise le pas d'integration
         call ps_reinit_integ_rkutta(abs(xpas(iveh)))
         if(MSP_gen_messages("ps_init_boucle_rkutta")) return
      end if

    end subroutine ps_init_boucle_rkutta

end module ps_integration_rkutta
