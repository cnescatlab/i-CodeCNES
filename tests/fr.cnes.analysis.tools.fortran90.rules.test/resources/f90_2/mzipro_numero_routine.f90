subroutine mzipro_numero_routine ( routine, nom, identification, retour )

! (C) Copyright CNES - MSPRO - 2002-2005
!************************************************************************
!
! But:  cette routine donne le nom et le role d'une routine, identifiee par son numero.
! ===   (pour la MSPRO uniquement)
!
! Note d'utilisation:  
! ==================
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 2.0 : creation a partir de mzpro_code_retour
!                         (Date: 05/2002 - Realisation: Guylaine Prat)
!   + Version 3.0 (DE globale 2) : suppression des routines passant dans la MSLIGHT
!                         (Date: 10/2002 - Realisation: Guylaine Prat)
!   + Version 3.0 (sans DE) : ajout de routines
!                         (Date: 10/2002 - Realisation: Michel Lagreca et Bruno Revelin)
!   + Version 4.0 (DE globale 8): suppression des routines passant dans la MSLIB90
!                         (Date: 10/2003 - Realisation: Veronique Lepine)
!   + Version 5.0 (DE globale 11): suppression des routines passant dans la MSLIB90
!                         (Date: 05/2004 - Realisation: Guylaine Prat)
!   + Version 5.1 (sans DE): ajout de nouvelles routines
!                         (Date: 09/2004 - Realisation: Bruno Revelin)
!   + Version 5.2 (sans DE) : ajout de routines
!                         (Date: 01/2005 - Realisation: Bruno Revelin)
!   + Version 5.5 : DM-ID 413 : Calcul du systeme de parametres Vinfini d arrivee
!                         (Date: 05/2006 - Realisation: Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                         (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.6 : DM-ID 473 : inhibition de la détection d'événements
!                         (Date: 09/2006 - Réalisation: Atos Origin
!   + Version 5.10: DM-ID 1058 : Correction des warnings levés par g95
!                   (Date: 8/2008 - Realisation: Atos origin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use numero_routine_mspro
use valeur_code_retour_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                                  :: routine        ! numero de la routine
character(len=pm_nom_routine),intent(out)            :: nom            ! nom de la routine
character(len=pm_identification_routine),intent(out) :: identification ! role succinct de la routine
integer, intent(out)                                 :: retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

character(len=31) :: chaine_nom  ! Chaine intermediaire contenant le nom de la routine.
! Elle est surdimensionnee pour ne pas avoir de probleme.
! Sa longueur reelle est testee par rapport a pm_nom_routine.

character(len=2*pm_identification_routine) :: chaine_identification  ! Chaine intermediaire 
! contenant l'identification de la routine.
! Elle est surdimensionnee pour ne pas avoir de probleme.
! Sa longueur reelle est testee par rapport a pm_identification_routine.

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mzipro_numero_routine.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! ============================================
! Affectation suivant le numero de la routine
! ============================================

select case (routine)

!..........
! Theme A
!..........

case (pm_num_ma_avion_sol)
   chaine_nom = 'ma_avion_sol'
   chaine_identification = &
        'Position du triedre avion par rapport au triedre normal lie au sol et porte par l''avion.'

case (pm_num_ma_avion_vit)
   chaine_nom = 'ma_avion_vit'
   chaine_identification = &
        'Position du triedre lie a la vitesse par rapport au triedre avion et&
        & calcul de la gite.'

!..........
! Theme D
!..........

case (pm_num_md_anjour_calend)
   chaine_nom = 'md_anjour_calend'
   chaine_identification = &
        'Conversion d''une date exprimee sous la forme annee et numero du jour dans l''annee&
        & en une date calendaire.'

case (pm_num_md_calend_anjour)
   chaine_nom = 'md_calend_anjour'
   chaine_identification = &
        'Conversion d''une date calendaire en une date exprimee sous la forme annee et numero du jour dans l''annee.'

case (pm_num_md_lire_saut_tuc)
   chaine_nom = 'md_lire_saut_tuc'
   chaine_identification = &
        'Lire le fichier de sauts TUC.'

case (pm_num_md_ech_temps)
   chaine_nom = 'md_ech_temps'
   chaine_identification = &
        'Changement d''echelle de temps.'

!..........
! Theme M
!..........

!..........
! Theme P
!..........

case (pm_num_mp_atm_cira)
   chaine_nom = 'mp_atm_cira'
   chaine_identification = &
        'Modele d''atmosphere terrestre CIRA.'

case (pm_num_mp_atm_cira_msis86)
   chaine_nom = 'mp_atm_cira_msis86'
   chaine_identification = &
        'Modele mixte d''atmosphere terrestre CIRA/MSIS86.'

case (pm_num_mp_atm_dtm78)
   chaine_nom = 'mp_atm_dtm78'
   chaine_identification = &
        'Modele d''atmosphere terrestre DTM78.'

case (pm_num_mp_atm_msis86)
   chaine_nom = 'mp_atm_msis86'
   chaine_identification = &
        'Modele d''atmosphere terrestre MSIS86.'

case (pm_num_mp_atm_us76d)
   chaine_nom = 'mp_atm_us76d'
   chaine_identification = &
        'Modele d''atmosphere terrestre US 76 avec dispersions possibles en temperature ou en densite.'

case (pm_num_mp_mag_ap_kp)
   chaine_nom = 'mp_mag_ap_kp'
   chaine_identification = &
        'Transformation de l''indice d''activite geomagnetique ap en kp.'

case (pm_num_mp_mag_kp_ap)
   chaine_nom = 'mp_mag_kp_ap'
   chaine_identification = &
        'Transformation de l''indice d''activite geomagnetique kp en ap.'

!..........
! Theme T
!..........

case (pm_num_mt_car_meca_vol)
   chaine_nom = 'mt_car_meca_vol'
   chaine_identification = &
        'Passage des position-vitesse en coordonnees cartesiennes dans le repere de reference&
        & en position-vitesse en coordonnees mecanique du vol.'
case (pm_num_mt_meca_vol_car)
   chaine_nom = 'mt_meca_vol_car'
   chaine_identification = &
        'Passage des position-vitesse en coordonnees mecanique du vol aux position-vitesse&
        & en coordonnees cartesiennes dans le repere de reference.'
case (pm_num_mt_car_gps_ard)
   chaine_nom = 'mt_car_gps_ard'
   chaine_identification = &
        'Passage des position-vitesse en  coordonnees cartesiennes dans le repere de reference en point GPS ARD.'
case (pm_num_mt_gps_ard_car)
   chaine_nom = 'mt_gps_ard_car'
   chaine_identification = &
        'Passage d''un point GPS ARD aux position-vitesse en coordonnees cartesiennes dans le repere de reference.'
!..........
! Theme U
!..........

case (pm_num_mu_ajouter_evenement)
   chaine_nom = 'mu_ajouter_evenement'
   chaine_identification = &
        'Ajouter, dans un integrateur, une subroutine de commutation dont les zeros marquent&
        & l''occurrence d un evenement.'

case (pm_num_mu_supprimer_evenement)
   chaine_nom = 'mu_supprimer_evenement'
   chaine_identification = &
        'Supprimer, dans un integrateur, une subroutine de commutation dont les zeros marquent&
        & l''occurrence d un evenement.'

case (pm_num_mu_car_spher)
   chaine_nom = 'mu_car_spher'
   chaine_identification = &
        'Passage des coordonnees cartesiennes aux coordonnees spheriques.'

case (pm_num_mu_creer_gest_pas)
   chaine_nom = 'mu_creer_gest_pas'
   chaine_identification = &
        'Creer un gestionnaire de pas dans l''integrateur.'

case (pm_num_mu_creer_integrateur)
   chaine_nom = 'mu_creer_integrateur'
   chaine_identification = &
        'Creer un integrateur a pas fixe ou a pas variable.'

case (pm_num_mu_det_mat)
   chaine_nom = 'mu_det_mat'
   chaine_identification = &
        'Calcul du determinant d''une matrice carree A donnee sous sa forme factorisee PA = LU.'

case (pm_num_mu_eq2degre_reel)
   chaine_nom = 'mu_eq2degre_reel'
   chaine_identification = &
        'Calcul des racines reelles de l''equation du second degre: ax2 + bx + c = 0.'

case (pm_num_mu_factor_LU)
   chaine_nom = 'mu_factor_LU'
   chaine_identification = &
        'Factorisation PA = LU d''une matrice carree A en deux matrices&
        & triangulaires inferieure L et superieure U.'

case (pm_num_mu_integrer)
   chaine_nom = 'mu_integrer'
   chaine_identification = &
        "Integrer l'equation differentielle y'=f(t, y) au point (t, y(t))."

case (pm_num_mu_inter_dim1_lin)
   chaine_nom = 'mu_inter_dim1_lin'
   chaine_identification = &
        'Interpolation lineaire (degre 1) sur une variable (dimension 1).'

case (pm_num_mu_inter_dim2_deg2)
   chaine_nom = 'mu_inter_dim2_deg2'
   chaine_identification = &
        'Interpolation de degre 2 sur deux variables (dimension 2).'

case (pm_num_mu_inter_dim3_deg3)
   chaine_nom = 'mu_inter_dim3_deg3'
   chaine_identification = &
        'Interpolation de degre 3 sur trois variables (dimension 3).'

case (pm_num_mu_inter_ind)
   chaine_nom = 'mu_inter_ind'
   chaine_identification = &
        'Dans le cadre d''une interpolation, recherche de l''indice d''un point appartenant a un vecteur de points ordonnes.'

case (pm_num_mu_inv_mat)
   chaine_nom = 'mu_inv_mat'
   chaine_identification = &
        'Inversion d''une matrice A donnee sous sa forme factorisee PA = LU.'

case (pm_num_mu_lagrange)
   chaine_nom = 'mu_lagrange'
   chaine_identification = &
        'Calcul d''un point interpole par le polynome de Lagrange d''ordre n.'

case (pm_num_mu_liberer_integ)
   chaine_nom = 'mu_liberer_integ'
   chaine_identification = &
        'Liberer la memoire allouee pour un integrateur donne.'

case (pm_num_mu_racine)
   chaine_nom = 'mu_racine'
   chaine_identification = &
        'Recherche d''une racine de l''equation f(x) = h.'

case (pm_num_mu_resol_sys_lin)
   chaine_nom = 'mu_resol_sys_lin'
   chaine_identification = &
        'Resolution d''un systeme lineaire AX = B ou A est donnee sous sa forme factorisee PA = LU.'

case (pm_num_mu_spher_car)
   chaine_nom = 'mu_spher_car'
   chaine_identification = &
        'Passage des coordonnees spheriques aux coordonnees cartesiennes.'

case (pm_num_mu_spline_cub_eval)
   chaine_nom = 'mu_spline_cub_eval'
   chaine_identification = &
        'Interpolation par spline cubique : evaluation.'

case (pm_num_mu_spline_cub_init)
   chaine_nom = 'mu_spline_cub_init'
   chaine_identification = &
        'Interpolation par spline cubique : initialisation.'

case (pm_num_mu_statis)
   chaine_nom = 'mu_statis'
   chaine_identification = &
        'Calcul statistiques sur un ensemble de points.'

!..........
! Theme V
!..........

case (pm_num_mv_Bplane_kep)
   chaine_nom = 'mv_Bplane_kep'
   chaine_identification = &
        'Passage des parametres de B-plane et anomalie moyenne aux parametres kepleriens.'

case (pm_num_mv_kep_Vinf)
   chaine_nom = 'mv_kep_Vinf'
   chaine_identification = &
        'Passage des parametres kepleriens aux parametres orbitaux hyperboliques a vitesse infinie depart.'

case (pm_num_mv_kep_Vinfarr)
   chaine_nom = 'mv_kep_Vinfarr'
   chaine_identification = &
        'Passage des parametres kepleriens aux parametres orbitaux hyperboliques a vitesse infinie d''arrivee.'

case (pm_num_mv_kep_Bplane)
   chaine_nom = 'mv_kep_Bplane'
   chaine_identification = &
        'Passage des parametres kepleriens aux parametres de B-plane.'

case (pm_num_mv_Vinf_kep)
   chaine_nom = 'mv_Vinf_kep'
   chaine_identification = &
        'Passage des parametres orbitaux hyperboliques a vitesse infinie depart aux parametres kepleriens.'

case (pm_num_mv_Vinfarr_kep)
   chaine_nom = 'mv_Vinfarr_kep'
   chaine_identification = &
        'Passage des parametres orbitaux hyperboliques a vitesse infinie d''arrivee aux parametres kepleriens.'

!..........
! Theme X
!..........

case (pm_num_mx_rep)
   chaine_nom = 'mx_rep'
   chaine_identification = &
        'Routine chapeau de changement de reperes.'

case (pm_num_mx_var)
   chaine_nom = 'mx_var'
   chaine_identification = &
        'Routine chapeau de changement de variables.'

!..........
! Theme Z
!..........

case (pm_num_mzpro_code_retour)
   chaine_nom = 'mzpro_code_retour'
   chaine_identification = &
        'Cette routine donne la signification complete du code retour d''une routine&
        & MSPRO ou de la MSLIB Fortran 90.'

case (pm_num_mzpro_traiter_retour)
   chaine_nom = 'mzpro_traiter_retour'
   chaine_identification = 'Routine permettant a l''utilisateur de traiter les&
        & codes retour de la MSPRO, et ceux de la MSLIB Fortran 90.'

!........................................
! Numero de routine inconnu pour la MSPRO
!........................................

case default 

      retour = pm_err_numero_routine_inconnu
      chaine_nom = ''
      chaine_identification = ''

end select

! test si les chaines donnant le nom de la routine et l'identification tiennent dans la longueur impartie
! =======================================================================================================

if ((len_trim(chaine_nom) <= pm_nom_routine) .and. (len_trim(chaine_identification) <= pm_identification_routine)) then

   nom = trim(chaine_nom)
   identification = trim(chaine_identification)

else ! Pour l'utilisateur: il n'est pas possible de passer dans ce cas.
     ! Ce test est prevu pour verifier que la chaine ne depasse pas la longueur autorisee
     ! car ni la compilation, ni l'execution ne permettent de le detecter. Du coup la chaine
     ! nom ou la chaine identification serait tronquee ...

   retour = pm_err_valid
   nom = ''
   identification = ''

end if

end subroutine mzipro_numero_routine
