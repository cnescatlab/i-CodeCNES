module ps_integration

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ps_integration
!
!$Resume
!  Module regroupant les sous-programmes pstsuiv et psarret.
!
!$Description
!  Module regroupant les sous-programmes permettant de déterminer quel est l'événement
!  à suivre (pstsuiv) et d'arrêter l'extrapolation (psarret) .
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: ps_integration.F90 368 2013-02-19 14:43:59Z aadt $
!
!$Historique
!  $Log: ps_integration.F90,v $
!  Revision 368  2013/02/19 aadt
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 1.33  2010/10/25 13:10:59  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.32  2009/06/16 08:21:59  tanguyy
!  FA-ID 1306 : corrections de fuites memoires potentielles (structure separation et structure integrateur)
!
!  Revision 1.31  2008/12/02 16:48:09  tanguyy
!  AQ : mise à jour des cartouches avant livraison de PSIMU V9.3
!
!  Revision 1.30  2008/11/18 13:38:10  tanguyy
!  DM-ID 733 : reorganisation des modules / utilisation de ps_force_modele_complet et ps_force_modele_simplifie
!  Revision 1.29  2008/10/24 09:40:24  huec
!  AQ : Simplification de la gestion des modules
!  Revision 1.28  2008/10/17 14:25:16  tanguyy
!  DM-ID 1058 : controles memoire ; nouvelle routine ps_terminer_session_PSIMU
!  Revision 1.27  2008/10/15 12:53:51  tanguyy
!  DM-ID 1058 : Rajout de liberations mémoire
!  Revision 1.26  2008/09/04 07:53:03  tanguyy
!  DM-ID 1058 : phase 1 du portage / suppression des warnings - initialisations
!  Revision 1.25  2008/04/29 17:22:11  tanguyy
!  DM-ID 964 et  FA-ID 886 : le test se fait desormais sur l'ecart entre la date precedente et la date courante : s'il est inferieur a 10-6 sec, l'integrateur n'est pas appele
!  Revision 1.24  2008/04/02 13:45:33  tanguyy
!  DM-ID 959 : correction de l'aplatissement pour le calcul de coordonnees geodesiques
!  Revision 1.23  2008/03/06 12:45:37  ttn
!  DM-ID 959 : Modification routine ps_calcul_altitude_pente + prise en compte dans ps_init_boucle_cowell et ps_init_boucle_rkutta
!  Revision 1.22  2008/01/17 13:59:28  huec
!  AQ : Correction d une coquille dans commentaire
!  Revision 1.21  2007/07/09 12:06:12  tanguyy
!  DM-ID 692 : routine de calcul de l'altitude, traitant l'altitude géodésique
!  Revision 1.20  2007/06/20 12:33:30  vivaresf
!  Intégration FA 725 patch V8.7a3
!  Revision 1.19  2007/02/20 13:41:33  tanguyy
!  FA-ID 711 : desactivation de la liberation systematique des integrateurs
!  Revision 1.18.2.2  2007/06/15 17:17:15  vivaresf
!  PSIMU V8.7a4 : essais pour fuites mémoire
!  Revision 1.18.2.1  2007/04/16 09:42:50  vivaresf
!  FA-ID 725 : optimisation et robustesse du patch V8.7a3
!  meilleure gestion des allocate (désallocation + status)
!  desallocation du COWELL si ré-initialisation de l'intégration
!  Revision 1.18  2006/10/19 15:09:51  tanguyy
!  DM-ID 478 : Cloture du FT (Integrateur de Cowell : modification de l interface)
!  Revision 1.17.4.1  2006/10/13 07:55:25  tanguyy
!  DM-ID 478 : utilisation jj/sec. 1ere version fonctionnelle
!  Revision 1.17  2006/03/15 13:25:19  tanguyy
!  Livraison PSIMU V8-4 ; relecture code / suppression code mort / maj cartouches
!  Revision 1.16  2006/02/27 15:15:44  tanguyy
!  FA-ID 482 : allocation dynamique du tableau des dates de sortie
!  Revision 1.15  2005/11/10 18:37:06  vivaresf
!  Mise à jour des cartouches
!  Revision 1.14  2005/02/17 13:57:33  fabrec
!  DM-ID 98 : pas des ephemerides
!  Revision 1.13  2005/01/28 10:42:11  fabrec
!  DM-ID 175 : desallocations memoire
!  Revision 1.12  2005/01/17 15:26:15  fabrec
!  DM-ID 175 : utilisation des scenarios mecaspa
!  Revision 1.11  2004/12/10 16:53:07  fabrec
!  DM-ID 175 : nouveau type de loi fichier
!  Revision 1.10  2004/05/26 15:26:11  adm_ipsi
!  Corrections commentaire cartouche
!  Revision 1.9  2002/12/20 16:38:50  boschett
!  Utilisation du traitement d'erreur de la MECASPA
!  Revision 1.8  2002/12/12 15:05:24  boschett
!  Livraison Intermediaire 16/12/2002
!  Revision 1.7  2002/12/09 17:08:07  boschett
!  Ajout du traitement par défaut dans la structure if/elseif/else de la subroutine 'psarret'
!  Revision 1.6  2002/12/06 10:20:14  boschett
!  Suppression des assignations inutiles
!  Revision 1.5  2002/12/04 14:24:35  boschett
!  Suppression des instructions return inutiles en fin de routine
!  Revision 1.4  2002/12/03 17:39:21  boschett
!  Utilisation de la constante symbolique PS_ECRAN dans les appels d'écriture à l'écran
!  Revision 1.3  2002/11/26 16:59:02  boschett
!  Ajout de implicit none
!  Revision 1.2  2002/10/14 16:18:09  laurent
!  Inversion du mot clé quand altitude minimale atteinte dans PS_HMIN
!  Revision 1.1.1.1  2002/09/30 14:59:34  laurent
!  Industrialisation PSIMU
!  Revision 1.6  2000/06/21 15:30:12  util_am
!  retrait de l'include domtrad_F.h
!  Revision 1.5  2000/05/29 09:12:38  util_am
!  Utilisation de Domtraduire
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
!  use ps_integration
!
!$Structure
!
!$Global
!
!$Common
!
!$Routines
!- pstsuiv
!- psarret
!- ps_libere_integrateurs
!- ps_calcul_altitude_pente
!
!$Fonctions
!
!$Include
!#V
!- parameter_gslib.h
!#
!
!$Module
!#V
!- MECASPA
!- ps_generalites
!- ps_evenements
!- ps_variables
!- ps_integration_don
!- ps_propulsion
!- ps_separations
!- ps_ecriture
!- cps_utilisateur
!- ephem
!- ps_propulsion_don
!- ps_modeles
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
  use ps_evenements
  use ps_variables
  
  use ps_integration_don
  
  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ps_integration.F90 368 2013-02-19 14:43:59Z aadt $'

  
contains
  
  subroutine pstsuiv (iplus)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  pstsuiv
!
!$Resume
!  Détermination du test à effectuer sur le pas de calcul suivant.
!
!$Description
!  Détermination du test à effectuer sur le pas de calcul suivant.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call pstsuiv (iplus)
!.    integer :: iplus
!
!$Arguments
!>S     iplus  :<integer>   type d'événement à suivre:
!.                          1 => poussée
!.                          2 => séparation
!.                          3 => fin d'extrapolation
!.                          4 => événement
!.                          5 => sortie régulière
!
!$Common
!
!$Routines
!- MSP_consulter_scenario
!- MSP_consulter_separation
!- MSP_effacer_separation
!- MSP_consulter_poussee_continue
!- MSP_effacer_poussee_continue
!- MSP_consulter_impulsion
!- MSP_consulter_poussee_bulletin
!
!$Include
!
!$Module
!#V
!- ps_propulsion
!- ps_separations
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use ps_propulsion
    use ps_separations
    
    implicit none
    
    integer, intent(OUT) :: iplus
    
    real (KIND=pm_reel) :: tplus
    integer :: kdeb, allocstat
    integer ::  sc_type, sc_nloi, typloi
    real(KIND=pm_reel) :: tsep
    type(MSP_SEPARATION) :: loi_sep
    type(MSP_IMPULSION) :: loi_imp
    type(MSP_POUSSEE_CONTINUE) :: loi_cont
    type(MSP_POUSSEE_BULLETIN) :: loi_bul
    real (KIND=pm_reel), pointer :: timp(:)
    real (KIND=pm_reel) :: datbul,datedeb, datetimp, tbul
    real (KIND=pm_reel) :: datprec_tmp, date_ref
    integer :: ntab

!   ********************************************************************
!   * Initialisations                                                  *
!   ********************************************************************
    nullify(timp)
    datprec_tmp = 0.001_pm_reel

!   ********************************************************************
!   * Calcul de la date de fin                                         *
!   ********************************************************************
    iplus = 3
    tplus = str_int(iveh)%tmax


!   ********************************************************************
!   * Sorties regulieres                                               *
!   ********************************************************************

    if ( str_ecr(iveh)%ipas == 2 ) then
       if ( (str_ecr(iveh)%nsor <= str_ecr(iveh)%nb_sorties) .and. (str_ecr(iveh)%nb_sorties >= 1) ) then
          if ((str_int(iveh)%tsign*tplus) >=(str_int(iveh)%tsign*str_ecr(iveh)%ypas_sorties(str_ecr(iveh)%nsor))) then
             iplus = 5
             tplus = str_ecr(iveh)%ypas_sorties(str_ecr(iveh)%nsor)
          end if
       end if
    end if

!   ********************************************************************
!   * Calcul d'une date evenement                                      *
!   ********************************************************************

    if ( str_eve(iveh)%ndeve /= 0 ) then
       if ( (str_eve(iveh)%neve <= str_eve(iveh)%ndeve) .and. (str_eve(iveh)%neve >= 1) ) then
          if ((str_int(iveh)%tsign*tplus) >=(str_int(iveh)%tsign*str_eve(iveh)%deve(str_eve(iveh)%neve))) then
             iplus = 4
             tplus = str_eve(iveh)%deve(str_eve(iveh)%neve)
          end if
       end if
    end if

!   ********************************************************************
!   * Calcul de la date de la premiere separation                      *
!   ********************************************************************
    
    !  Lecture de la structure MSP_SCENARIO_LOI
    call MSP_consulter_scenario (scenar_sep(iveh), type=sc_type, nloi=sc_nloi)
    if ( MSP_gen_messages("pstsuiv") ) return

    !   Si le scenario est de type separation      
    if ( sc_type == MSP_ENUM_SEPARATION ) then
       if (sc_nloi > 0) then
          if ( ( iloi_scenar_sep(iveh) <=  sc_nloi) .and. ( iloi_scenar_sep(iveh) >= 1) ) then
             
             call MSP_consulter_scenario (scenar_sep(iveh), loi_sep=loi_sep)
             if ( MSP_gen_messages("pstsuiv") ) return
             
             call MSP_consulter_separation (loi_sep, date=tsep)
             if ( MSP_gen_messages("pstsuiv") ) return

             if ((str_int(iveh)%tsign*tplus) > (str_int(iveh)%tsign* tsep)) then
                iplus = 2
                tplus = tsep
             endif
             call MSP_effacer_separation(loi_sep)
          endif
       endif
    endif
   
!   ********************************************************************
!   * Calcul de la date de la premiere poussee                         *
!   ********************************************************************

    !  Lecture de la structure MSP_SCENARIO_LOI
    call MSP_consulter_scenario (scenar_pro(iveh), type=sc_type, nloi=sc_nloi)
    if ( MSP_gen_messages("pstsuiv") ) return

    !   Si le scenario est de type propulsion
    if ( sc_type == MSP_ENUM_PROPULSION ) then
       if (sc_nloi > 0) then
          if ( ( iloi_scenar_pro(iveh) <=  sc_nloi) .and. ( iloi_scenar_pro(iveh) >= 1) ) then
             
             typloi = MSP_type_loi (scenar_pro(iveh), id=iloi_scenar_pro(iveh))
             if ( MSP_gen_messages("pstsuiv") ) return

             if (typloi == MSP_ENUM_LOI_CONT) then
                !/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
                call MSP_consulter_scenario (scenar_pro(iveh), loi_cont=loi_cont, id=iloi_scenar_pro(iveh))
                if ( MSP_gen_messages("pstsuiv") ) return
                
                call MSP_consulter_poussee_continue (loi_cont, ntab=ntab, dates=timp)
                if ( MSP_gen_messages("pstsuiv") ) return

               ! Libération de la mémoire
               call MSP_effacer_poussee_continue (loi_cont)
               if ( MSP_gen_messages("pstsuiv") ) return

                
             elseif (typloi == MSP_ENUM_LOI_IMP) then
                !/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
                call MSP_consulter_scenario (scenar_pro(iveh), loi_imp=loi_imp, id=iloi_scenar_pro(iveh))
                if ( MSP_gen_messages("pstsuiv") ) return
                
                call MSP_consulter_impulsion (loi_imp, date=datetimp)
                if ( MSP_gen_messages("pstsuiv") ) return
                ntab = 1
                if (ASSOCIATED(timp)) deallocate(timp, stat=allocstat)
                ALLOCATE(timp(1))
                timp(1) = datetimp

             elseif (typloi == MSP_ENUM_LOI_BUL) then
                !/  extraction de la structure loi de propulsion dans la structure MSP_SCENARIO_LOI
                call MSP_consulter_scenario (scenar_pro(iveh), date_ref=date_ref)
                if ( MSP_gen_messages("pstsuiv") ) return
                call MSP_consulter_scenario (scenar_pro(iveh), loi_bul=loi_bul, id=iloi_scenar_pro(iveh))
                if ( MSP_gen_messages("pstsuiv") ) return
                
                call MSP_consulter_poussee_bulletin (loi_bul, datedeb=datedeb, datbul = datbul)
                if ( MSP_gen_messages("pstsuiv") ) return
                
                if (ASSOCIATED(timp)) deallocate(timp, stat=allocstat)
                ALLOCATE(timp(2))

                timp(1) = datedeb
                tbul = (datbul - date_ref)*86400._pm_reel
                ! Arrondi à la milli-seconde:
                timp(2) = real(int(tbul),KIND=pm_reel) + &
                    real(nint((tbul-int(tbul))/datprec_tmp),KIND=pm_reel)*datprec_tmp
                ntab = 2
               
             endif

             if (str_int(iveh)%itsgn > 0) then
                kdeb = 1
             else
                kdeb = ntab
             endif
             if ((str_int(iveh)%tsign*tplus) > (str_int(iveh)%tsign* timp(kdeb))) then
                iplus = 1
             end if
          end if
       end if
    endif


    ! Deallocations memoire
    if (associated(timp)) deallocate(timp)

  end subroutine pstsuiv

  subroutine psarret (jflag,iter,jter,ypas,datex,parex)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psarret
!
!$Resume
!   Messages d'arrêt du programme.
!
!$Description
!   Messages d'arrêt du programme.
!
!$Auteur
!  J. F. GOESTER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psarret (jflag,iter,jter,ypas,datex,parex)
!.    integer :: jflag,jter
!.    real (KIND=pm_reel) :: ypas,parex(6)
!.    type(tm_jour_sec) :: datex
!.    integer :: iter
!
!$Arguments
!>E     jflag  :<integer>           indique la façon dont on s'arrête
!>E/S   iter   :<integer>           numéro de l'itération courante (Cowell)
!>E     jter   :<integer>           numéro de l'itération courante (Runge Kutta)
!>E     ypas   :<pm_reel>           temps écoulé par rapport au début de la simulation (sec)
!>E     datex  :<tm_jour_sec>       date (Jours Juliens CNES)
!>E     parex  :<pm_reel,DIM=(6)>   position-vitesse courantes (m-m/s)
!
!$Common
!
!$Routines
!- psnetat
!- pswresu
!- MSP_signaler_message
!- flush
!
!$Include
!
!$Module
!#V
!- ps_ecriture
!- cps_utilisateur
!- ephem
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use ps_ecriture
    use cps_utilisateur
    use ephem, only : eph_closeposcor
    
    implicit none
    
    integer, intent(IN)             :: jflag,jter
    real (KIND=pm_reel), intent(IN) :: ypas,parex(6)
    type(tm_jour_sec), intent(in)   :: datex
    integer, intent(INOUT)          :: iter
    character(LEN=8)                :: pvar
  
!     Arret a cause d'un nombre trop grand d'iterations:
!     =================================================
    if ( jflag == 0 ) then
       
       if ( iscreen >= 0 ) then
          write (PS_ECRAN,Domtraduire(nomdomaine,"PS_ITERMAX")) npas
       endif
       
!     Arret car ypas=tmax:
!     ===================
       
    else if ( jflag == 1 ) then
       if ( (iter+jter) /= 0 ) then
          if ( jter > iter ) iter = jter
          call psnetat (ypas,datex,parex)
          if ( MSP_gen_messages("psarret") ) return         
          call pswresu (1,ilogeph,iscreen,30,iter,ypas,str_gen(iveh)%etat,&
               str_ecr(iveh)%nvarw,str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,&
               str_eve(iveh)%ifore,str_eve(iveh)%neve,str_eve(iveh)%aeve)
          if ( MSP_gen_messages("psarret") ) return         
          call pswresu (2,ilogeve,0,30,iter,ypas,str_gen(iveh)%etat,str_ecr(iveh)%nvarw,&
               str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,str_eve(iveh)%ifore,&
               str_eve(iveh)%neve,str_eve(iveh)%aeve)
          if ( MSP_gen_messages("psarret") ) return         
       endif
       if ( iscreen >= 0 ) then
          write(PS_ECRAN,Domtraduire(nomdomaine,"PS_DATEFIN"))
       endif
       
!     Arret car str_gen(iveh)%etat(29)<=hstop:
!     =========================

    else if ( jflag== 2  ) then
       
       if ( jter > iter ) iter = jter
       call psnetat (ypas,datex,parex)
       if ( MSP_gen_messages("psarret") ) return         
       call pswresu (1,ilogeph,iscreen,31,iter,ypas,str_gen(iveh)%etat,&
            str_ecr(iveh)%nvarw,str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,&
            str_eve(iveh)%ifore,str_eve(iveh)%neve,str_eve(iveh)%aeve)
       if ( MSP_gen_messages("psarret") ) return         
       call pswresu (2,ilogeve,0,31,iter,ypas,str_gen(iveh)%etat,str_ecr(iveh)%nvarw,&
            str_ecr(iveh)%iresw,str_ecr(iveh)%iforw,str_eve(iveh)%ifore,&
            str_eve(iveh)%neve,str_eve(iveh)%aeve)
       if ( MSP_gen_messages("psarret") ) return           
       if ( iscreen >= 0 ) then
          if ( str_int(iveh)%hstop >= 100000._pm_reel ) then
             write(PS_ECRAN,Domtraduire(nomdomaine,"PS_HMIN_03")) (str_int(iveh)%hstop/1000._pm_reel)
          else if ( str_int(iveh)%hstop >= 10000._pm_reel ) then
             write(PS_ECRAN,Domtraduire(nomdomaine,"PS_HMIN_02")) (str_int(iveh)%hstop/1000._pm_reel)
          else
             write(PS_ECRAN,Domtraduire(nomdomaine,"PS_HMIN_01")) (str_int(iveh)%hstop/1000._pm_reel)
          end if
       end if

!     Arret sur cause non prévue:
!     ===========================
    else
       write (pvar,'(I2)') jflag
       call MSP_signaler_message (cle_mes="PS_INTEGRATION_001",partie_variable=pvar)
    endif

    call flush (PS_ECRAN)
    
    !/ Desallocation memoire des structures de donnees
    if(associated(str_ecr(iveh)%ypas_sorties)) then
       deallocate(str_ecr(iveh)%ypas_sorties)
    end if

  end subroutine psarret

  subroutine ps_libere_integrateurs()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_libere_integrateurs
!
!$Resume
!  Routine de libération des intégrateurs, à utiliser à la fin de l'utilisation de 
!  PSIMU
!
!$Description
!  Routine de libération des intégrateurs, à utiliser à la fin de l'utilisation de 
!  PSIMU
!
!$Auteur
!  Y. TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_libere_integrateurs()
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
!- ps_propulsion_don
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
    use ps_propulsion_don

    implicit none
    
    
    !/ Variables locales
    type (tm_code_retour) :: code_retour
    integer :: ii

    ! Début du code
    !--------------
    do ii=1, PS_NVMAX

       !/ Désallocation mémoire des intégrateurs de Cowell et de Runge-Kutta
       if(str_int(ii)%init_cowell /= 0) then
          call mu_liberer_integ(str_int(ii)%integrateur_cowell, code_retour)
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
       str_int(ii)%init_cowell = 0

       if(str_int(ii)%init_rkutta /= 0) then
          call mu_liberer_integ(str_int(ii)%integrateur_rkutta, code_retour)
          if(code_retour%valeur < 0) then
             call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_ERR_LIBERATION_MSPRO", &
                  partie_variable = "Rkutta")
             return
          end if
          if(code_retour%valeur > 0) then 
             call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_WARN_LIBERATION_MSPRO", &
                  partie_variable = "Rkutta")
          end if
       end if

       ! Mise à 0 du flag
       str_int(ii)%init_rkutta = 0


       if (init_integ_propulsion(ii) /= 0) then
          call mu_liberer_integ(integ_propulsion(ii), code_retour)
          if(code_retour%valeur < 0) then
             call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_ERR_LIBERATION_MSPRO", &
                  partie_variable = "Rkutta - Propulsion")
             return
          end if
          if(code_retour%valeur > 0) then 
             call MSP_Signaler_Message(cle_mes="PS_INTEGRATION_WARN_LIBERATION_MSPRO", &
                  partie_variable = "Rkutta - Propulsion")
             return
          end if
          init_integ_propulsion(ii) = 0
       end if
    end do

  end subroutine ps_libere_integrateurs

  subroutine ps_calcul_altitude_pente(param,alt,pente)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_calcul_altitude_pente
!
!$Resume
!  Cette routine calcule l'altitude (géodésique ou géocentrique) et la pente (géodésique ou géocentrique)
! de la vitesse du véhicule
!
!$Description
!  Cette routine calcule l'altitude (géodésique ou géocentrique) et la pente (géodésique ou géocentrique)
! de la vitesse du véhicule
!  Altitude géocentrique par défaut
!
!$Auteur
! Yannick TANGUY / Thi-Anh TAN
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_calcul_altitude_pente(param,alt,pente)
!.    real(kind=pm_reel), dimension(6) :: param
!.    real(kind=pm_reel) :: alt
!.    real(kind=pm_reel) :: pente
!
!$Arguments
!>E     param  :<pm_reel,DIM=(6)>   Position/Vitesse du véhicule (m, m/s)
!>S     alt    :<pm_reel>           Altitude en m
!>S     pente  :<pm_reel>           pente sans unité 
!
!$Common
!
!$Routines
!- mt_car_meca_vol
!- MSP_signaler_message
!
!$Include
!#V
!- parameter_gslib.h
!#
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
    real(kind=pm_reel), dimension(6), intent(in) :: param
    real(kind=pm_reel),               intent(out):: alt
    real(kind=pm_reel),               intent(out):: pente

    ! Include : définition de constantes GS_PS_ALT_GEO..
    !==========================================================
#include "parameter_gslib.h"

    ! Variables locales
    !==================
    type(tm_geodesique)  :: corgeod
    type(tm_code_retour) :: code_retour
    type(tm_vit_meca_vol):: vit_meca_vol  ! vitesse mecanique du vol

    real(kind=pm_reel) :: apla_r ! valeur d'aplatissement recalculée.
    
    ! Début du code
    !==============

    ! L'altitude géocentrique est utilisée pour tous les changements d'intégrateurs.
    ! Si l'utilisateur le demande, l'altitude géodésique est utilisée pour l'arrêt du calcul
    !=======================================================================================
    ! Schéma du type d'altitude 
    !
    !
    ! Altitude  
    !/\
    ! ----------------------------------------------------- h1
    !
    !    altitude géocentrique
    !
    ! ----------------------------------------------------- h2
    ! 
    !    altitude géocentrique
    !
    ! ----------------------------------------------------- h3
    !
    !    altitude géodésique
    !
    ! ----------------------------------------------------- hstop
    !
    !    altitude géodésique pour cas ou pente de la vitesse > 0 sinon arret
    !
    ! ----------------------------------------------------- 0
    
    
    
    ! 1) On calcule l'altitude géocentrique - Si la sonde se situe en dessus de min(h2,h3) l'altitude
    ! calculée est géocentrique
    alt = sqrt(param(1)**2+param(2)**2+param(3)**2) - str_mod(iveh)%requa

    ! 2) Si la sonde se situe sous max(h3,hstop) et que type_alt = geodesique,
    !   alors on calcule l'altitude géodésique
    if (alt < max(str_int(iveh)%hstop,str_int(iveh)%h3) .and. str_int(iveh)%type_alt == GS_PS_ALT_GEODESIQUE) then

       ! Rappel : la MSLIB utilise l'inverse de l'applatissement (-> valeur très proche de 0. Environ 10e-3 pour la Terre)
       if (str_mod(iveh)%apla .different. 0._pm_reel) then
          apla_r = 1/str_mod(iveh)%apla
       else
          apla_r = 0._pm_reel
       end if

       call mt_car_meca_vol(str_mod(iveh)%requa,apla_r,&
            param(1:3), param(4:6), corgeod, vit_meca_vol, code_retour)
       if (code_retour%valeur < 0) then
          call MSP_signaler_message (ier_mslib=code_retour)
       end if

       alt = corgeod%haut

       ! 3) Si la sonde se situe au dessus min(h2,h3) alors on calcule l'altitude géocentrique
       ! L'aplatissement est nul
    else
       
       apla_r = 0._pm_reel

       call mt_car_meca_vol(str_mod(iveh)%requa,apla_r, &
            param(1:3), param(4:6), corgeod, vit_meca_vol, &
            code_retour)
       if (code_retour%valeur < 0) then
          call MSP_signaler_message (ier_mslib=code_retour)
       end if

    end if

    pente = vit_meca_vol%pente

  end subroutine ps_calcul_altitude_pente
end module ps_integration

