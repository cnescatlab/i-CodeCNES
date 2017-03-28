subroutine mz_numero_routine (routine, nom, identification, code_retour)

! (C) Copyright CNES - MSLIB - 1998-2004

!************************************************************************
!
! But:  cette routine donne le nom et le role d'une routine, identifiee par son numero.
! ===
!
! Note d'utilisation:
! ==================
!
!$Historique
! ==========
!   + Version 0.1 (SP 147 ed01 rev00): creation
!                         (Date: 01/1998 - Realisation: Guylaine Prat)
!   + Version 0.1.1 (DE 188 ed01 rev00 + SP 147 ed02 rev00 + DE globale 182 ed01 rev00):
!                          prise en compte de regles de codage
!                         (Date: 02/1998 - Realisation: Guylaine Prat)
!   + Version 1.0 (sans DE): ajout de nouvelles routines
!                         (Date: 08/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (sans DE): ajout de nouvelles routines
!                         (Date: 09/1999 - Realisation: Sylvain Vresk)
!   + Version 3.0 (sans DE): ajout de nouvelles routines
!                         (Date: 09/2000 - Realisation: Veronique Lepine)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.0 (sans DE): ajout de nouvelles routines
!                         (Date: 01/2003 - Realisation: Bruno Revelin)
!   + Version 5.0 (sans DE): ajout de nouvelles routines
!                         (Date: 10/2003 - Realisation: Bruno Revelin et Veronique Lepine)
!   + Version 6.0 (sans DE): ajout de nouvelles routines
!                         (Date: 04/2004 - Realisation: Veronique Lepine)
!   + Version 6.1 (sans DE): ajout de nouvelles routines
!                         (Date: 09/2004 - Realisation: Guylaine Prat)
!   + Version 6.2 (sans DE): ajout de nouvelles routines
!                         (Date: 10/2004 - Realisation: Veronique Lepine)
!   + Version 6.3 : DM-ID 381 : Integrer des routines du theme trajectoires interplanetaires
!                           ajout de nouvelles routines du theme I
!                           (Date: 09/2005 - Realisation: Claire Fabre)
!   + version 6.3 : DM-ID 162 : introduction d'une routine de propagation d'orbite prenant en compte le 
!                               terme central du potentiel gravitationnel et les effets seculaires du J2
!                   creation a partir de la routine AM_pro_exorj2 de l' AMLIB
!                   (date: 11/2005 - realisation: Julien Bouillant - Atos Origin)
!   + Version 6.4 : DM-ID 426 : routine de calcul des angles de precession
!                 (Date: 04/2006 - Realisation: Claire Fabre - Atos Origin)
!                   DM-ID 424 : Modification des arguments de la nutation luni-solaire
!                         (Date: 05/2006 - Realisation : Atos Origin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.8 : DM-ID 859 : ajout des descriptions des nouvelles fonctions (calcul optimise)
!                   (Date: 03/2008 - Realisation: Atos origin)
!   + Version 6.9 : DM-ID 1092 : Ajout des nouvelles routines IERS 2003
!                   (Date: 09/2008 - Realisation: Atos origin)
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/02/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use numero_routine_mslib
use valeur_code_retour_mslib
use type_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                                       :: routine        ! numero de la routine
character(len=pm_nom_routine),intent(out)                 :: nom            ! nom de la routine
character(len=pm_identification_routine),intent(out)      :: identification ! role succinct de la routine
type(tm_code_retour), intent(out)                         :: code_retour

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
                     '@(#) Fichier MSLIB mz_numero_routine.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mz_numero_routine.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! Affectation suivant la numero de routine
! ========================================

select case (routine)
!..........
! Theme C
!..........
case (pm_num_mc_GRS1980)
     chaine_nom = 'mc_GRS1980'
     chaine_identification = &
     'Definition des constantes geodesiques du GRS1980.'

case (pm_num_mc_math)
     chaine_nom = 'mc_math'
     chaine_identification = &
     'Definition de constantes MATHematiques.'

case (pm_num_mc_phys)
     chaine_nom = 'mc_phys'
     chaine_identification = &
     'Definition de constantes PHYSiques.'

case (pm_num_mc_positionner_wahr5)
     chaine_nom = 'mc_positionner_wahr5'
     chaine_identification = &
     'Positionnement du flag associe au modele de nutation.'

case (pm_num_mc_recuperer_wahr5)
     chaine_nom = 'mc_recuperer_wahr5'
     chaine_identification = &
     'Recuperation du flag associe au modele de nutation.'

case (pm_num_mc_test)
     chaine_nom = 'mc_test'
     chaine_identification = &
     'Definition de constantes de TESTs utilisees par les routines de la MSLIB.'

!..........
! Theme D
!..........

case (pm_num_md_calend_julien)
     chaine_nom = 'md_calend_julien'
     chaine_identification = &
     'Calcul d''une date JULIENne CNES en fonction d''une date CALENDaire.'

case (pm_num_md_duree_jhms)
     chaine_nom = 'md_duree_jhms'
     chaine_identification = &
     'Conversion d''une DUREE en Jours/Heures/Minutes/Secondes.'

case (pm_num_md_julien_calend)
     chaine_nom = 'md_julien_calend'
     chaine_identification = &
     'Calcul d''une date CALENDaire en fonction d''une date JULIENne CNES.'

case (pm_num_md_comp_joursec)
     chaine_nom = 'md_comp_joursec'
     chaine_identification = &
     'COMParaison de deux quantites exprimees en JOURs et SECondes.'

case (pm_num_md_jourfrac_joursec)
     chaine_nom = 'md_jourfrac_joursec'
     chaine_identification = &
     'Conversion d''une quantite exprimee en JOURs  FRACtionnaires en une quantite exprimee&
     & en JOURs et SECondes dans le jour.'

case (pm_num_md_joursec_jourfrac)
     chaine_nom = 'md_joursec_jourfrac'
     chaine_identification = &
     'Conversion d''une quantite exprimee en JOURs et SECondes dans le jour en une quantite&
     & exprimee en JOURs FRACtionnaire.'

case (pm_num_md_joursec_norme)
     chaine_nom = 'md_joursec_norme'
     chaine_identification = &
     'NORMalisation d''une quantite en JOUR et SECondes dans le jour.'

!..........
! Theme E
!..........
case (pm_num_me_brouwer)
     chaine_nom = 'me_brouwer'
     chaine_identification = &
     'Modele analytique d''extrapolation d''orbite de Brouwer.'

case (pm_num_me_brouwer_moy)
     chaine_nom = 'me_brouwer_moy'
     chaine_identification = &
     'Calcul des parametres MOYens du modele d''extrapolation d''orbite de BROUWER.'

case (pm_num_me_deriv_secul_j2)
     chaine_nom = 'me_deriv_secul_j2'
     chaine_identification = &
     'Calcul des DERives SECULaires des elements Kepleriens, dues au J2.'

case (pm_num_me_eck_hech)
     chaine_nom = 'me_eck_hech'
     chaine_identification = &
     'Modele analytique d''extrapolation d''orbite de ECKSTEIN-HECHLER.'

case (pm_num_me_eck_hech_moy)
     chaine_nom = 'me_eck_hech_moy'
     chaine_identification = &
     'Calcul des parametres MOYens du modele d''extrapolation d''orbite de ECKSTEIN-HECHLER.'

case (pm_num_me_lyddane)
     chaine_nom = 'me_lyddane'
     chaine_identification = &
     'Modele analytique d''extrapolation d''orbite de LYDDANE.'

case (pm_num_me_lyddane_moy)
     chaine_nom = 'me_lyddane_moy'
     chaine_identification = &
     'Calcul des parametres MOYens du modele d''extrapolation d''orbite de LYDDANE.'

!..........
! Theme I
!..........
case (pm_num_mi_pb_deux_corps)
     chaine_nom = 'mi_pb_deux_corps'
     chaine_identification = &
     'ProBleme de DEUX CORPS.'

case (pm_num_mi_pb_lambert0)
     chaine_nom = 'mi_pb_lambert0'
     chaine_identification = &
     'Resolution du ProBleme de LAMBERT.'

case (pm_num_mi_pb_lambert1)
     chaine_nom = 'mi_pb_lambert0'
     chaine_identification = &
     'Resolution du ProBleme de LAMBERT a plus d'' 1 tour.'

case (pm_num_mi_pro_j2)
     chaine_nom = 'mi_pro_j2'
     chaine_identification = &
     'Propagation analytique prenant en compte le j2'

!..........
! Theme M
!..........
case (pm_num_mm_impul_car)
     chaine_nom = 'mm_impul_car'
     chaine_identification = &
     'Pour un satellite, calcul du bulletin d''orbite en coordonnee CARtesiennes, suite a la realisation d''une manoeuvre&
      & orbitale modelisee par une mono-IMPULsion quelconque.'

case (pm_num_mm_impul_kep)
     chaine_nom = 'mm_impul_kep'
     chaine_identification = &
     'Pour un satellite, calcul du bulletin d''orbite en parametres KEPleriens, suite a la realisation d''une manoeuvre&
      & orbitale modelisee par une mono-IMPULsion quelconque.'

!..........
! Theme O
!..........
case (pm_num_mo_geo_qsw)
     chaine_nom = 'mo_geo_qsw'
     chaine_identification = &
     'Passage dans le repere orbital local (Q,S,W) d''un vecteur exprime dans un repere GEOcentrique inertiel.'

case (pm_num_mo_geo_tnw)
     chaine_nom = 'mo_geo_tnw'
     chaine_identification = &
     'Passage dans le repere orbital local(T,N,W) d''un vecteur exprime dans un repere GEOcentrique inertiel.'

case (pm_num_mo_qsw_geo)
     chaine_nom = 'mo_qsw_geo'
     chaine_identification = &
     'Passage dans un repere GEOcentrique inertiel d''un vecteur exprime dans le repere orbital local (Q,S,W).'

case (pm_num_mo_tnw_geo)
     chaine_nom = 'mo_tnw_geo'
     chaine_identification = &
     'Passage dans un repere GEOcentrique inertiel d''un vecteur exprime dans le repere orbital local (T,N,W).'

case (pm_num_mo_def_qsw)
     chaine_nom = 'mo_def_qsw'
     chaine_identification = &
     'Calcul des coordonnees des vecteurs directeurs du repere orbital local (Q,S,W) dans un repere geocentrique inertiel.'

case (pm_num_mo_def_tnw)
     chaine_nom = 'mo_def_tnw'
     chaine_identification = &
     'Calcul des coordonnees des vecteurs directeurs du repere orbital local (T,N,W) dans un repere geocentrique inertiel.'

!..........
! Theme P
!..........
case (pm_num_mp_atm_US76)
     chaine_nom = 'mp_atm_US76'
     chaine_identification =  'Modele d''ATMosphere US76.'

!..........
! Theme R
!..........
case (pm_num_mr_EcliJ2000_J2000)
     chaine_nom = 'mr_EcliJ2000_J2000'
     chaine_identification = &
     'Passage du repere ecliptique moyen a la date J2000 au repere EME2000.'

case (pm_num_mr_EquaMoy_EquaVrai)
     chaine_nom = 'mr_EquaMoy_EquaVrai'
     chaine_identification = &
     'Passage du repere EQUAtorial MOYen a la date t au repere EQUAtorial VRAI a la meme date t.'

case (pm_num_mr_EquaMoy_J2000)
     chaine_nom = 'mr_EquaMoy_J2000'
     chaine_identification = &
     'Passage du repere EQUAtorial MOYen a la date t au repere equatorial moyen J2000.'

case (pm_num_mr_EquaUAI_J2000)
     chaine_nom = 'mr_EquaUAI_J2000'
     chaine_identification = &
     'Passage du repere equatorial planetaire UAI a la date t au repere EME2000.'

case (pm_num_mr_EquaUAI_PlanetVrai)
     chaine_nom = 'mr_EquaUAI_PlanetVrai'
     chaine_identification = &
     'Passage du repere equatorial planetaire UAI au repere planetocentrique vrai a la date t.'

case (pm_num_mr_EquaVrai_EquaMoy)
     chaine_nom = 'mr_EquaVrai_EquaMoy'
     chaine_identification = &
     'Passage du repere EQUAtorial VRAI a la date t au repere EQUAtorial MOYen a la meme date t.'

case (pm_num_mr_EquaVrai_TerVrai)
     chaine_nom = 'mr_EquaVrai_TerVrai'
     chaine_identification = &
     'Passage du repere EQUAtorial VRAI a la date t au repere TERrestre VRAI a la meme date t.'

case (pm_num_mr_EquaVrai_veis)
     chaine_nom = 'mr_EquaVrai_veis'
     chaine_identification = &
     'Passage du repere EQUAtorial VRAI a la date t au repere de VEIS a la meme date t.'

case (pm_num_mr_J2000_BBR)
     chaine_nom = 'mr_J2000_BBR'
     chaine_identification = &
     'Passage du repere equatorial moyen J2000 (EME2000) au repere Body Body Rotating (BBR).'

case (pm_num_mr_J2000_EcliJ2000)
     chaine_nom = 'mr_J2000_EcliJ2000'
     chaine_identification = &
     'Passage du repere EME2000 au repere ecliptique moyen a la date J2000.'

case (pm_num_mr_J2000_EquaMoy)
     chaine_nom = 'mr_J2000_EquaMoy'
     chaine_identification = &
     'Passage du repere equatorial moyen J2000 au repere EQUAtorial MOYen a la date t.'

case (pm_num_mr_J2000_EquaUAI)
     chaine_nom = 'mr_J2000_EquaUAI'
     chaine_identification = &
     'Passage du repere EME2000 au repere equatorial planetaire UAI a la date t.'

case (pm_num_mr_J2000_TerVrai)
     chaine_nom = 'mr_J2000_TerVrai'
     chaine_identification = &
     'Passage du repere equatorial moyen J2000 au repere TERrestre VRAI a la date t.'

case (pm_num_mr_J2000_veis)
     chaine_nom = 'mr_J2000_veis'
     chaine_identification = &
     'Passage du repere equatorial moyen J2000 au repere de VEIS a la date t.'

case (pm_num_mr_mat_J2000_BBR)
     chaine_nom = 'mr_mat_J2000_BBR'
     chaine_identification = &
     'Calcul de la MATrice de passage du repere equatorial moyen J2000 (EME2000) &
      & au repere Body Body Rotating (BBR).'

case (pm_num_mr_mat_nuta)
     chaine_nom = 'mr_mat_nuta'
     chaine_identification = &
     'Calcul de la MATrice de NUTAtion pour le passage du repere equatorial moyen au repere&
      & equatorial vrai pour la meme epoque.'

case (pm_num_mr_nuta)
     chaine_nom = 'mr_nuta'
     chaine_identification = &
     'Calcul des NUTations en longitude et en obliquite.'

case (pm_num_mr_obli_moy)
     chaine_nom = 'mr_obli_moy'
     chaine_identification = &
     'Calcul de l''OBLIquite MOYenne.'

case (pm_num_mr_prec)
     chaine_nom = 'mr_prec'
     chaine_identification = &
     'Calcul de la PRECession equatoriale.'

case (pm_num_mr_PlaIner_PlaVrai)
     chaine_nom = 'mr_PlaIner_PlaVrai'
     chaine_identification = &
     'Passage du repere planetocentrique inertiel du type «H0-n» a n=0 au repere planetocentrique vrai.'

case (pm_num_mr_PlanetVrai_EquaUAI)
     chaine_nom = 'mr_PlanetVrai_EquaUAI'
     chaine_identification = &
     'Passage du repere planetocentrique vrai au repere equatorial planetaire UAI a la date t.'

case (pm_num_mr_PlaVrai_PlaIner)
     chaine_nom = 'mr_PlaVrai_PlaIner'
     chaine_identification = &
     'Passage du repere planetocentrique vrai au repere planetocentrique inertiel du type «H0-n» a n=0.'

case (pm_num_mr_rep_fon)
     chaine_nom = 'mr_rep_fon'
     chaine_identification = &
     'Calcul de la matrice de passage entre deux REPeres FONdamentaux.'

case (pm_num_mr_TerRef_TerVrai)
     chaine_nom = 'mr_TerRef_TerVrai'
     chaine_identification = &
     'Passage du repere TERrestre de REFerence au repere TERrestre VRAI a la date t.'

case (pm_num_mr_TerVrai_EquaVrai)
     chaine_nom = 'mr_TerVrai_EquaVrai'
     chaine_identification = &
     'Passage du repere TERrestre VRAI a la date t au repere EQUAtorial VRAI a la meme date t.'

case (pm_num_mr_TerVrai_J2000)
     chaine_nom = 'mr_TerVrai_J2000'
     chaine_identification = &
     'Passage du repere TERrestre VRAI a la date t au repere equatorial moyen J2000.'

case (pm_num_mr_TerVrai_TerRef)
     chaine_nom = 'mr_TerVrai_TerRef'
     chaine_identification = &
     'Passage du repere TERrestre VRAI a la date t au repere TERrestre de REFerence.'

case (pm_num_mr_TerVrai_veis)
     chaine_nom = 'mr_TerVrai_veis'
     chaine_identification = &
     'Passage du repere TERrestre VRAI a la date t au repere de VEIS a la meme date t.'

case (pm_num_mr_tsid_aoki)
     chaine_nom = 'mr_tsid_aoki'
     chaine_identification = &
     'Calcul du Temps SIDeral dans le repere de reference defini par l''IAU en 1980 (AOKI).'

case (pm_num_mr_tsid_veis)
     chaine_nom = 'mr_tsid_veis'
     chaine_identification = &
     'Calcul du Temps SIDeral dans le repere de reference VEIS (Gamma50 CNES).'

case (pm_num_mr_tsid_vrai)
     chaine_nom = 'mr_tsid_vrai'
     chaine_identification = &
     'Calcul du Temps SIDeral vrai.'

case (pm_num_mr_veis_EquaVrai)
     chaine_nom = 'mr_veis_EquaVrai'
     chaine_identification = &
     'Passage du repere de VEIS a la date t au repere EQUAtorial VRAI a la meme date t.'

case (pm_num_mr_veis_J2000)
     chaine_nom = 'mr_veis_J2000'
     chaine_identification = &
     'Passage du repere de VEIS a la date t au repere equatorial moyen J2000.'

 case (pm_num_mr_veis_TerVrai)
     chaine_nom = 'mr_veis_TerVrai'
     chaine_identification = &
     'Passage du repere de VEIS a la date t au repere TERrestre VRAI a la meme date t.'

 case (pm_num_mr_s_prime_iers2003)
     chaine_nom = 'mr_s_prime_iers2003'
     chaine_identification = &
     'Calcul du paramètre s prime de l iers.'

 case (pm_num_mr_TerRef_TerVrai_iers)
     chaine_nom = 'mr_TerRef_TerVrai_iers2003'
     chaine_identification = &
     'Passage du repère Terrestre de Référence au repère Terrestre Vrai de la date, définition IERS 2003.'

 case (pm_num_mr_TerVrai_TerRef_iers)
     chaine_nom = 'mr_TerVrai_TerRef_iers2003'
     chaine_identification = &
     'Passage du repère Terrestre vrai au repère Terrestre de reference de la date, définition IERS 2003.'

 case (pm_num_mr_TerVrai_CIP_iers)
     chaine_nom = 'mr_TerVrai_CIP_iers2003'
     chaine_identification = &
     'Passage du repère Terrestre vrai au repère céleste intermédiaire CIP, définition IERS 2003.'

 case (pm_num_mr_CIP_TerVrai_iers)
     chaine_nom = 'mr_CIP_TerVrai_iers2003'
     chaine_identification = &
     'Passage du repère céleste intermédiaire CIP au repère terrestre vrai, définition IERS 2003.'

 case (pm_num_mr_CIP_EME2000_iers)
     chaine_nom = 'mr_CIP_EME2000_iers2003'
     chaine_identification = &
     'Passage du repère céleste intermédiaire CIP au repère EME2000, définition IERS 2003.'

 case (pm_num_mr_EME2000_CIP_iers)
     chaine_nom = 'mr_EME2000_CIP_iers2003'
     chaine_identification = &
     'Passage du repère EME2000 au repère céleste intermédiaire CIP, définition IERS 2003.'

 case (pm_num_mr_TerRef_EME2000_iers)
     chaine_nom = 'mr_TerRef_EME2000_iers2003'
     chaine_identification = &
     'Passage du repère terrestre de référence (ITRF) au repère EME2000, définition IERS 2003.'

 case (pm_num_mr_EME2000_TerRef_iers)
     chaine_nom = 'mr_EME2000_TerRef_iers2003'
     chaine_identification = &
     'Passage du repère EME2000 au repère terrestre de référence (ITRF), définition IERS 2003.'

!..........
! Theme S
!..........
case (pm_num_ms_pos_soleil_lune)
     chaine_nom = 'ms_pos_soleil_lune'
     chaine_identification = 'Calcul des POSitions du SOLEIL et de la LUNE dans le repere de Veis a une date donnee.'

!..........
! Theme T
!..........
case (pm_num_mt_car_geoc)
     chaine_nom = 'mt_car_geoc'
     chaine_identification = 'Passage des coordonnnees CARtesiennes aux coordonnees GEOCentriques.'

case (pm_num_mt_car_geod)
     chaine_nom = 'mt_car_geod'
     chaine_identification = 'Passage des coordonnnees CARtesiennes aux coordonnees GEODesiques.'

case (pm_num_mt_def_topo_N)
     chaine_nom = 'mt_def_topo_N'
     chaine_identification = 'DEFinition des cosinus directeurs d''un repere TOPOcentrique Nord &
                             &(convention axe Ox vers le Nord).'
case (pm_num_mt_geoc_car)
     chaine_nom = 'mt_geoc_car'
     chaine_identification = 'Passage des coordonnnees GEOCentriques aux coordonnees CARtesiennes.'

case (pm_num_mt_geod_car)
     chaine_nom = 'mt_geod_car'
     chaine_identification = 'Passage des coordonnnees GEODesiques  aux coordonnees CARtesiennes.'

case (pm_num_mt_iner_ref)
     chaine_nom = 'mt_iner_ref'
     chaine_identification = &
     'Passage du repere geocentrique INERtiel lie a un mobile (du type "H0-9") au repere terrestre de REFerence.'

case (pm_num_mt_ref_iner)
     chaine_nom = 'mt_ref_iner'
     chaine_identification = &
     'Passage du repere terrestre de REFerence a un repere geocentrique INERtiel lie a un mobile (du type "H0-9").'

case (pm_num_mt_ref_topo_N)
     chaine_nom = 'mt_ref_topo_N'
     chaine_identification = 'Passage du repere terrestre de REFerence a un repere TOPOcentrique Nord &
                             &(convention axe Ox vers le Nord).'

case (pm_num_mt_topo_car_E_N)
     chaine_nom = 'mt_topo_car_E_N'
     chaine_identification = &
     'Passage d''un repere TOPOcentrique Est (convention axe Ox vers l''Est) au repere topocentrique Nord associe &
      &(convention axe Ox vers le Nord) en coordonnees CARtesiennes.'

case (pm_num_mt_topo_car_N_E)
     chaine_nom = 'mt_topo_car_N_E'
     chaine_identification = &
     'Passage d''un repere TOPOcentrique Nord (convention axe Ox vers le Nord) au repere topocentrique Est associe &
      &(convention axe Ox vers l''Est) en coordonnees CARtesiennes.'

case (pm_num_mt_topo_E_car_sgd)
     chaine_nom = 'mt_topo_E_car_sgd'
     chaine_identification = 'Dans un repere TOPOcentrique Est (convention axe Ox vers l''Est),&
         & passage des coordonnees CARtesiennes aux coordonnees Site/Gisement/Distance.'

case (pm_num_mt_topo_E_sgd_car)
     chaine_nom = 'mt_topo_E_sgd_car'
     chaine_identification = 'Dans un repere TOPOcentrique Est (convention axe Ox vers l''Est),&
         & passage des coordonnees Site/Gisement/Distance aux coordonnees CARtesiennes .'

case (pm_num_mt_topo_N_car_sgd)
     chaine_nom = 'mt_topo_N_car_sgd'
     chaine_identification = 'Dans un repere TOPOcentrique Nord (convention axe Ox vers le Nord),&
         & passage des coordonnees CARtesiennes aux coordonnees Site/Gisement/Distance.'

case (pm_num_mt_topo_N_ref)
     chaine_nom = 'mt_topo_N_ref'
     chaine_identification = 'Passage du repere TOPOcentrique Nord (convention axe Ox vers le Nord) &
                              &au repere terrestre de REFerence.'

case (pm_num_mt_topo_N_sgd_car)
     chaine_nom = 'mt_topo_N_sgd_car'
     chaine_identification = 'Dans un repere TOPOcentrique Nord (convention axe Ox vers le Nord),&
         & passage des coordonnees Site/Gisement/Distance aux coordonnees CARtesiennes .'

case (pm_num_mt_topo_sgd_E_N)
     chaine_nom = 'mt_topo_sgd_E_N'
     chaine_identification = &
     'Passage d''un repere TOPOcentrique Est (convention axe Ox vers l''Est) au repere topocentrique Nord associe &
      &(convention axe Ox vers le Nord) en coordonnees Site/Gisement/Distance.'

case (pm_num_mt_topo_sgd_N_E)
     chaine_nom = 'mt_topo_sgd_N_E'
     chaine_identification = &
     'Passage d''un repere TOPOcentrique Nord (convention axe Ox vers le Nord) au repere topocentrique Est associe &
      &(convention axe Ox vers l''Est) en coordonnees Site/Gisement/Distance.'

!..........
! Theme U
!..........
case (pm_num_mu_3rot_quat)
   chaine_nom = 'mu_3rot_quat'
   chaine_identification = &
     'Calcul du QUATernion associe a une ROTation definie par 3 angles de Cardan ou d''Euler.'

case (pm_num_mu_angle2)
     chaine_nom = 'mu_angle2'
     chaine_identification = &
     'Dans le plan R2, calcul de l''ANGLE entre un vecteur et l''axe des abscisses.'

case (pm_num_mu_angle3)
     chaine_nom = 'mu_angle3'
     chaine_identification = &
     'Dans l''espace R3, calcul de l'' ANGLE non oriente entre deux vecteurs.'

case (pm_num_mu_axe_angle_quat)
     chaine_nom = 'mu_axe_angle_quat'
     chaine_identification = &
     'Conversion d''une rotation definie par son AXE et son ANGLE en un QUATernion.'

case (pm_num_mu_compar_rot_quat)
     chaine_nom = 'mu_compar_rot_quat'
     chaine_identification = &
     'COMPARaison de ROTations definies a l''aide de QUATernions.'

case (pm_num_mu_mat_quat)
   chaine_nom = 'mu_mat_quat'
   chaine_identification = &
     'Calcul du QUATernion associe a une MATrice de rotation.'

case (pm_num_mu_norme)
     chaine_nom = 'mu_norme'
     chaine_identification = &
     'Calcul de la NORME euclidienne d''un vecteur dans R3.'

case (pm_num_mu_prod_quat)
     chaine_nom = 'mu_prod_quat'
     chaine_identification = &
     'Calcul du PRODuit de deux QUATernions.'

case (pm_num_mu_prod_vect)
     chaine_nom = 'mu_prod_vect'
     chaine_identification = &
     'Calcul du PRODuit VECToriel de deux vecteurs dans R3.'

case (pm_num_mu_quat_3rot)
   chaine_nom = 'mu_quat_3rot'
   chaine_identification = &
     'Calcul des 3 angles de Cardan ou d''Euler associes a une ROTation definie par un QUATernion.'

case (pm_num_mu_quat_axe_angle)
     chaine_nom = 'mu_quat_axe_angle'
     chaine_identification = &
     'Conversion d''un QUATernion en une rotation definie par son AXE et son ANGLE.'

case (pm_num_mu_quat_conjug)
     chaine_nom = 'mu_quat_conjug'
     chaine_identification = &
     'Calcul du QUATernion CONJUGue d''un quaternion donne.'

case (pm_num_mu_quat_mat)
   chaine_nom = 'mu_quat_mat'
   chaine_identification = &
     'Calcul de la MATrice de rotation associee a un QUATernion.'

case (pm_num_mu_quat_norme)
     chaine_nom = 'mu_quat_norme'
     chaine_identification = &
     'Normalisation d''un QUATernion apres calcul de sa NORME.'

case (pm_num_mu_quat_rep)
     chaine_nom = 'mu_quat_rep'
     chaine_identification = &
     'A l''aide d''un QUATernion, calcul de changement de REPere.'

case (pm_num_mu_racine)
     chaine_nom = 'mu_racine'
     chaine_identification = &
     'Recherche d''une racine de l''equation f(x) = h.'

case (pm_num_mu_matmul3)
     chaine_nom = 'mu_matmul3'
     chaine_identification = &
     'MULtiplication de MATrices 3x3.'

case (pm_num_mu_matmul6)
     chaine_nom = 'mu_matmul6'
     chaine_identification = &
     'MULtiplication de MATrices 6x6.'

case (pm_num_mu_transpose3)
     chaine_nom = 'mu_transpose3'
     chaine_identification = &
     'TRANSPOSEe de matrice 3x3'

case (pm_num_mu_mulvect3)
     chaine_nom = 'mu_mulvect3'
     chaine_identification = &
     'MULtiplication matrice VECTeur (3,3)*3'

case (pm_num_mu_mulvecttrans3)
     chaine_nom = 'mu_mulvecttrans3'
     chaine_identification = &
     'MULtiplication VECTeur matrice TRANSposee T3*(3,3)'

!..........
! Theme V
!..........

case (pm_num_mv_car_cir)
     chaine_nom = 'mv_car_cir'
     chaine_identification = &
     'Passage des parametres CARtesiens aux parametres orbitaux dits adaptes aux orbites CIRculaires non equatoriales.'

case (pm_num_mv_car_cir_equa)
     chaine_nom = 'mv_car_cir_equa'
     chaine_identification = &
     'Passage des parametres CARtesiens aux parametres orbitaux dits adaptes aux orbites CIRculaires EQUAtoriales.'

case (pm_num_mv_car_equa)
     chaine_nom = 'mv_car_equa'
     chaine_identification = &
     'Passage des parametres CARtesiens aux parametres dits adaptes aux orbites EQUAtoriales non circulaires.'

case (pm_num_mv_car_kep)
     chaine_nom = 'mv_car_kep'
     chaine_identification = 'Passage des parametres CARtesiens aux parametres KEPleriens.'

case (pm_num_mv_cir_car)
     chaine_nom = 'mv_cir_car'
     chaine_identification = &
     'Passage des parametres orbitaux dits adaptes aux orbites CIRculaires non equatoriales aux parametres CARtesiens .'

case (pm_num_mv_cir_equa_car)
     chaine_nom = 'mv_cir_equa_car'
     chaine_identification = &
     'Passage des parametres orbitaux dits adaptes aux orbites CIRculaires EQUAtoriales aux parametres CARtesiens.'

case (pm_num_mv_cir_equa_kep)
     chaine_nom = 'mv_cir_equa_kep'
     chaine_identification = &
     'Passage des parametres orbitaux dits adaptes aux orbites CIRculaires EQUAtoriales aux parametres KEPleriens.'

case (pm_num_mv_cir_kep)
     chaine_nom = 'mv_cir_kep'
     chaine_identification = &
     'Passage des parametres orbitaux dits adaptes aux orbites CIRculaires non equatoriales aux parametres KEPleriens.'

case (pm_num_mv_conv_anom)
     chaine_nom = 'mv_conv_anom'
     chaine_identification = &
     'CONVersions d''ANOMalies excentrique, moyenne et vraie dans les cas elliptique, hyperbolique et parabolique.'

case (pm_num_mv_equa_car)
     chaine_nom = 'mv_equa_car'
     chaine_identification = &
     'Passage des parametres orbitaux dits adaptes aux orbites EQUAtoriales non circulaires aux parametres CARtesiens.'

case (pm_num_mv_equa_kep)
     chaine_nom = 'mv_equa_kep'
     chaine_identification = &
     'Passage des parametres orbitaux dits adaptes aux orbites EQUAtoriales non circulaires aux parametres KEPleriens.'

case (pm_num_mv_kep_car)
     chaine_nom = 'mv_kep_car'
     chaine_identification = 'Passage des parametres KEPleriens aux parametres CARtesiens .'

case (pm_num_mv_kep_cir)
     chaine_nom = 'mv_kep_cir'
     chaine_identification = 'Passage des parametres KEPleriens aux parametres orbitaux dits adaptes aux orbites &
                              &CIRculaires non equatoriales.'

case (pm_num_mv_kep_cir_equa)
     chaine_nom = 'mv_kep_cir_equa'
     chaine_identification = 'Passage des parametres KEPleriens aux parametres orbitaux dits adaptes aux orbites &
                              &CIRculaires EQUAtoriales.'
case (pm_num_mv_kep_equa)
     chaine_nom = 'mv_kep_equa'
     chaine_identification = 'Passage des parametres KEPleriens aux parametres orbitaux dits adaptes aux orbites &
                              &EQUAtoriales non circulaires.'

case (pm_num_mv_kepler_bar)
     chaine_nom = 'mv_kepler_bar'
     chaine_identification = 'Resolution des equations de KEPLER et de BARker.'

case (pm_num_mv_kepler_gene)
     chaine_nom = 'mv_kepler_gene'
     chaine_identification = 'Resolution de l''equation de KEPLER GENEralisee.'

case (pm_num_mv_kepler_std)
     chaine_nom = 'mv_kepler_std'
     chaine_identification = 'Resolution de l''equation de KEPLER STandarD (orbite elliptique).'

!..........
! Theme Z
!..........

case (pm_num_mz_numero_routine)
     chaine_nom = 'mz_numero_routine'
     chaine_identification = &
     'Cette routine donne le nom et le role d''une routine, identifiee par son numero d''identification (ce numero est contenu&
      & dans le champ %routine du code retour).'

case (pm_num_mz_val_code_retour)
     chaine_nom = 'mz_val_code_retour'
     chaine_identification = 'Cette routine donne la signification du champ %valeur d''un code retour.'

!..................................
! Numero de routine retour inconnu
!..................................

case default
     code_retour%valeur = pm_err_numero_routine_inconnu
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
     ! nom ou la chaine identiification serait tronquee ...

   code_retour%valeur = pm_err_valid
   nom = ''
   identification = ''

end if

code_retour%routine = pm_num_mz_numero_routine
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mz_numero_routine
