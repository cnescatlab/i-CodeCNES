subroutine mzipro_val_retour ( valeur, signification, retour )

! (C) Copyright CNES - MSPRO - 2002-2005

!************************************************************************
!
! But:  cette routine donne la signification du champ %valeur d'un code retour
! ===   (pour la MSPRO uniquement)
!
! Note d'utilisation:  l'entree valeur est un entier 
! ==================  et correspond a un champ %valeur d'un code retour
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 2.0 : creation a partir de mzpro_code_retour
!                         (Date: 05/2002 - Realisation: Guylaine Prat)
!   + Version 3.0 (DE globale 2) : suppression des codes retour lies aux routines 
!                          passant dans la MSLIGHT
!                         (Date: 10/2002 - Realisation: Guylaine Prat)
!   + Version 3.0 (sans DE) : ajout de codes retour
!                         (Date: 12/2002 - Realisation: Michel Lagreca et Bruno Revelin)
!   + Version 3.1 (DE 1) : suppression de codes retour suite a l'evolution d'algorithmes
!                          (dont passage dans la mslib90)
!                         (Date: 09/2003 - Realisation: Bruno Revelin et Guylaine Prat)
!   + Version 3.1 (sans DE) : ajout de codes retour
!                         (Date: 09/2003 - Realisation: Guylaine Prat)
!   + Version 4.0 (DE globale 8): suppression des codes retour specifiques aux routines
!                         passant dans la MSLIB90
!                         (Date: 10/2003 - Realisation: Veronique Lepine)
!   + Version 5.0 (DE globale 11): suppression des codes retour specifiques aux routines
!                         passant dans la MSLIB90
!                         (Date: 03/2004 - Realisation: Veronique Lepine)
!   + Version 5.1 (sans DE) : ajout de codes retour
!                         (Date: 09/2004 - Realisation: Bruno Revelin)
!   + Version 5.2 (sans DE) : ajout de codes retour
!                         (Date: 01/2005 - Realisation: Bruno Revelin)
!   + Version 5.4 : FA-ID 475 : Division par zero
!                   FA-ID 474 : Anomalie pas min
!                         (Date: 02/02/2006 - Realisation: Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.7 : DM-ID 738 : Evolution du cowell
!                   (Date: 06/2007 - Realisation: Atos origin)
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

use valeur_code_retour_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                                      :: valeur        ! valeur a traiter
character(len=pm_signification_code_retour), intent(out) :: signification ! chaine indiquant la signification du code retour

integer, intent(out)  :: retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

character(len=2*pm_signification_code_retour) :: chaine  ! Chaine intermediaire
                               ! contenant la signification du code retour.
                               ! Elle est surdimensionnee pour ne pas avoir de probleme.
                               ! Sa longueur reelle est testee par rapport a pm_signification_code_retour.

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mzipro_val_retour.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! ============================================================
! Affectation suivant la valeur du code retour
! ============================================================

! Rappel:
! Code retour propre a la MSPRO dans [ +/- 4001 , +/- 5999 ]

select case (valeur)

!.............
! Cas nominal
!.............
   
case (pm_OK)
   chaine = 'Retour normal.'

!..........................................................................................................
! Probleme sur la valeur d'une constante physique [ +/- 4001 , +/- 4099 ]
!..........................................................................................................

case (pm_warn_apla_nul)
   chaine = 'L''aplatissement (f) est nul.'

!..........................................................................................................
! Probleme sur la valeur du demi grand axe a de la conique [ +/- 4101 , +/- 4199 ]
!..........................................................................................................

!...................................................................
! Probleme sur la valeur de l'excentricite e [ +/- 4201 , +/- 4299 ]
!...................................................................

case (pm_err_e_non_hyperb)
   chaine = 'L''excentricite (e) est inferieure ou egale a 1 : l''orbite n''est pas hyperbolique.'

!........................................................................................................
! Probleme sur la valeur de l'inclinaison i ou sur le vecteur inclinaison (ix,iy) [ +/- 4301 , +/- 4399 ]
!........................................................................................................

!.........................................................................
! Probleme sur une date, une duree, un temps [ +/- 4401 , +/- 4499 ]
!.........................................................................

case (pm_err_duree_nul)
   chaine = 'La duree est nulle.'
case (pm_err_mois)
   chaine = 'Valeur incorrecte du mois.'
case (pm_err_jour_interval_an)
   chaine = 'Le jour n''appartient pas a l''intervalle [1,n]&
        &avec n=365 ou 366 selon l''annee.'
case (pm_err_nb_saut_tuc_max)
   chaine = 'Le nombre de sauts du TUC lu depasse la valeur maximale (pm_nb_max_saut_tuc).'
case (pm_err_fic_saut_tuc)
   chaine = 'Probleme dans le fichier de sauts du TUC, une date de fin est posterieure a la date de debut d intervalle.'
case (pm_err_ind_ech_temps)
   chaine = 'Mauvais indicateur pour l''echelle de temps.'
case (pm_err_date_inf_1saut_tuc)
   chaine = 'La date est anterieure a la date du premier saut du TUC.'
case (pm_err_nb_saut_tuc_nul)
   chaine = 'Le nombre de sauts du TUC fourni est nul.'

!.........................................................................
! Probleme sur la position et/ou la vitesse [ +/- 4501 , +/- 4599 ]
!.........................................................................

case (pm_warn_lamb_pi)  
   chaine = 'Le transfert est a 180 degres, il y a une infinite de plans de transfert possibles;&
        & arbitrairement, on a retenu le plan de l''orbite initiale.'
case (pm_warn_lat_cira) ! probleme specifique a la routine mp_atm_cira
   chaine = 'La latitude n''est pas comprise entre -80 degres et +80 degres; le modele atmospherique&
        & CIRA ne fonctionne que pour des latitudes dans l''intervalle ]-80 deg, +80 deg[.'

case (pm_warn_vit_vertic)   ! probleme specifique a la routine mt_car_meca_vol
   chaine = 'La composante horizontale de la vitesse est nulle, l''azimut de la vitesse n''est pas defini.&
        & Arbitrairement nous lui donnons la valeur zero.'
case (pm_warn_fuse_vertic)   ! probleme specifique a la routine ma_avion_sol
   chaine = 'L''axe du fuselage est vertical: l''azimut de l''avion est indefini.&
        & Arbitrairement nous lui donnons la valeur zero.'
case (pm_warn_pos_Oz_ref_meca_vol)   ! probleme specifique a la routine mt_car_meca_vol
   chaine = 'Position sur l''axe Oz du repere de reference.&
             & Il existe donc une infinite de solution pour la valeur de la longitude,&
             & et de l''azimut de la vitesse,&
             & et on leur a donne arbitrairement la valeur zero.'

case (pm_err_alt_sup120km)   ! probleme specifique a la routine mp_atm_cira
   chaine = 'L''altitude est superieure a 120 km; or le modele d''atmosphere CIRA n''est pas applicable&
        & a des altitudes superieures a 120 km.'
case (pm_err_alt_inf90km)   ! probleme specifique a la routine mp_atm_msis86
   chaine = 'L''altitude est inferieure a 90 km.'
case (pm_err_alt_inf120km)   ! probleme specifique a la routine mp_atm_dtm78
   chaine = 'L''altitude est inferieure a 120 km.'
case (pm_err_jac_non_calc_vit_vertic)   ! probleme specifique aux routine du theme T 
   chaine = 'La vitesse est verticale, il en resulte que la jacobienne n''est pas calculable.'
case (pm_err_vit_plan_sym_avion_ortho)  ! probleme specifique aux routines du theme A
   chaine = 'La vitesse et le plan de symetrie de l''avion sont orthogonaux.'
case (pm_err_jac_non_calc_decl_pisur2)   ! probleme specifique aux routine du theme V 
   chaine = 'Pour le vecteur vitesse infinie,&
     & la declinaison = + ou - pi/2 : il en resulte que la jacobienne n''est pas calculable.'
case (pm_err_Bplane_decli_pisur2)   ! probleme specifique aux routine du theme V 
   chaine = 'Pour le vecteur vitesse infinie arrivee dans le repere courant,&
     & la declinaison = + ou - pi/2 et l''ascension droite est differente de + pi/2 :&
     & convention du B-plane non respectee.'
case (pm_err_Bplane_BT_nulle)   ! probleme specifique aux routine du theme V 
   chaine = 'La coordonnee BT du point d''impact sur le B-plane est nulle.'

case (pm_err_C3_negatif_ou_nul)  ! probleme specifique aux routines du theme V
   chaine = 'Le carre de la vitesse infinie depart est negatif ou nul.'
case (pm_err_Rp_negatif_ou_nul)  ! probleme specifique aux routines du theme V
   chaine = 'Le rayon du periastre est negatif ou nul.'
case (pm_err_jac_non_calc_sin_theta)  ! probleme specifique aux routines du theme V
   chaine = 'L''angle theta vaut 0 ou pi: il en resulte que la jacobienne n''est pas calculable.'
case (pm_err_non_ellip)  ! probleme specifique aux routines du theme U
   chaine = 'Mouvement non elliptique detecte par l integrateur de Cowell.'

!..........................................................................................................
! Probleme sur la valeur d'une variable physique [ +/- 4601 , +/- 4699 ]
!..........................................................................................................

case (pm_err_flux_sol_negatif)
   chaine = 'Le flux solaire est negatif.'

case (pm_err_ap_negatif)
   chaine = 'L''indice d''activite geomagnetique ap est negatif.'

!.........................................................................
! Probleme de codage dans le code de l'utilisateur [ +/- 4801 , +/- 4898 ]
!.........................................................................

case (pm_warn_sub)
   chaine = 'Probleme (avertissement) lors de l''appel de la subroutine passee en argument&
        & dans la sequence d''appel d''une routine de la MSPRO.'
case (pm_warn_para_opt_trop)
   chaine = 'Compte tenu de la transformation demandee, un (ou plusieurs) parametre(s) optionnel(s)&
        & ont ete fournis inutilement (verifier la sequence d''appel).'
case (pm_warn_trsf_atypique)
   chaine = 'Transformation atypique : les resultats n''ont pas forcement un sens physique.'
case (pm_warn_commutation)
   chaine = 'Il y a eu commutation, avec action STOP.La date de fin d''integration est la date de commutation.'

case (pm_err_sub)
   chaine = 'Probleme (erreur) lors de l''appel de la subroutine passee en argument dans la&
        & sequence d''appel d''une routine de la MSPRO.'
case (pm_err_option)
   chaine = 'Option de calcul incorrecte.'
case (pm_err_biblio_inconnu)
   chaine = 'Cette valeur de numero de bibliotheque est inconnue.'
case (pm_err_pt_double)
   chaine = 'Utilisation d''une mauvaise valeur pour le parametre permettant&
        & de traiter le probleme des points doubles.'

case (pm_err_val_para)
   chaine = 'Un parametre contient une valeur erronee dans l''appel d''une routine MSPRO.'
case (pm_err_transfo)
   chaine = 'La transformation demandee dans cette routine MSPRO n''est pas prise en compte actuellement.'
case (pm_err_para_opt_abs)
   chaine = 'Compte tenu de la transformation demandee, il manque un (ou plusieurs) parametre(s) optionnel(s).'
case (pm_err_para_incoherents)
   chaine = 'Il apparait une incoherence entre 2 parametres dans l''appel d''une routine MSPRO.'
case (pm_err_type_integ)
   chaine = 'Le type d''integrateur choisi est inconnu, ou ne correspond pas aux autres entrees.'
case (pm_err_dans_sub_commut)
   chaine = 'Une subroutine de commutation a retourne une erreur.'
case (pm_err_integ_ordre_1)
   chaine = 'L''ordre est obligatoire pour Cowell.'
case (pm_err_integ_ordre_2)
   chaine = 'L''ordre doit etre superieur ou egal a 2.'
case (pm_err_integ_ordre_3)
   chaine = 'L''ordre doit etre inferieur ou egal a 16.'
case (pm_err_integ_ordre_4)
   chaine = 'L''ordre doit etre un entier pair.'
case (pm_err_integ_ireg)
   chaine = 'La cle de regularisation doit etre egale a 0, 1 ou 2.'
case (pm_err_integ_rxmu)
   chaine = 'La valeur de mu est obligatoire pour ce cas.'
case (pm_err_integ_hyp)
   chaine = 'Cas hyperbolique : la circularisation est impossible.'

!.........................................................................
! Probleme de codage dans le code MSPRO [ - 4899 ]
!.........................................................................

!.........................................................................
! Probleme de convergence dans les routines de mecanique spatiale [ +/- 4901, +/- 4997 ]
!.........................................................................
case (pm_err_conv_cowell)
   chaine = 'L''algorithme iteratif utilise pour l''integration numerique de Cowell n''a pas &
              &reussi a converger vers la bonne solution. '
case (pm_err_dim_etat_cowell)
   chaine = 'Non cohérence entre la dimension de l''etat et le nombre de paramètres à libérer.'
!..................................................................................................
! Probleme du a une routine issue de l'IOLIB ou de ZOOM [+/- 4998,+/-4999]
!..................................................................................................
case (pm_warn_IOLIB)
     chaine = 'Warning en retour d''une routine IOLIB.'

case (pm_err_IOLIB)
     chaine = 'Erreur en retour d''une routine IOLIB.'
case (pm_err_ZOOM)
     chaine = 'Erreur en retour d''une routine ZOOM.'

!..............................................
! Probleme mathematique [ +/- 5001 , +/- 5900 ]
!..............................................

case (pm_warn_long_indef)
   chaine = 'Les coordonnees x et y en entree sont proches de 0 : la longitude est indefinie&
        & et arbitrairement mise a 0.'
case (pm_warn_lat_long_indef)
   chaine = 'La distance en entree est proche de 0 : latitude et longitude sont indefinies&
        & et arbitrairement mises a 0.'
case (pm_warn_int_hors_domaine)
   chaine = 'La date d''interpolation n''est pas dans [t1+3h,t1+4h].'
case (pm_warn_extrapol)
   chaine = 'Fonctionnement en mode extrapolation: la precision est susceptible d''etre degradee.'
case (pm_warn_extrapol_borne_double)
   chaine = 'Fonctionnement en mode extrapolation au niveau d''une borne qui est un point double:&
        & la precision est probablement degradee.'
case (pm_warn_integ_pas_min)
   chaine = 'Le pas minimum d''integration est depasse.' 

case (pm_err_nb_pt_inf2)
   chaine = 'Le nombre de points d''interpolation en entree est inferieur a 2.'
case (pm_err_nb_pt_max)
   chaine = 'Le nombre de points en entree est superieur a la taille&
        & du vecteur en entree.'
case (pm_err_dim_y_inf1)
   chaine = 'La dimension des ordonnees est inferieure a 1.'
case (pm_err_x_non_diff)
   chaine = 'Les abscisses x d''interpolation ne sont pas toutes differentes.'
case (pm_err_x_non_ord_croi)
   chaine = 'Les abscisses x d''interpolation ne sont pas ordonnees de maniere croissante.'
case (pm_err_int_hors_domaine)
   chaine = 'Le point a interpoler se situe hors du domaine valide pour l''interpolation.'
case (pm_err_pas_tabul_negatif)
   chaine = 'Le pas de tabulation est negatif.'
case (pm_err_pas_tabul_nul)
   chaine = 'Le pas de tabulation est nul.'
case (pm_err_dim_mat)
   chaine = 'La dimension de la matrice est incorrecte.'
case (pm_err_nb_pt_inf1)
   chaine = 'Le nombre de points en entree est inferieur a 1.'   
case (pm_err_dim_inf2)
   chaine = 'La dimension du vecteur est inferieure a 2.'
case (pm_err_valeur_repetee)
   chaine = 'Interpolation dans une table n''ayant qu''une valeur repetee.'
case (pm_err_pivot_max_trop_faible)
   chaine = 'La matrice est singuliere.'  
case (pm_err_dimension)
   chaine = 'La dimension du vecteur est incorrecte.' 
case (pm_err_cherche_racine)
   chaine = 'La recherche du zero de la fonction a echoue.' 
case (pm_err_integ_dates)
   chaine = 'Division par zéro : les dates de debut et de fin d''integration sont égales.' 

!....................................................................................
! Probleme systeme [ +/- 5901 , +/- 5999 ]
!....................................................................................

case (pm_err_allocate)
   chaine = ' Probleme a l''allocation dynamique d''un tableau.'
case (pm_err_deallocate)
   chaine = 'Probleme a la desallocation d''un tableau.'
case (pm_err_fichier_absent)
   chaine = 'Le fichier est absent.'
case (pm_err_fichier_ouvert)
   chaine = 'Le fichier est deja ouvert.'
case (pm_err_alc_unite_logique)
   chaine = 'Probleme a l''allocation dynamique d''une unite logique.'
case (pm_err_ouvrir_fichier)
   chaine = 'Probleme a l''ouverture d''un fichier.'
case (pm_err_lire_don)
   chaine = 'Probleme a la lecture d''un fichier.'
case (pm_err_fermer_fichier)
   chaine = 'Probleme a la fermeture d''un fichier.'

! ..............................................
! Valeur inconnue du code retour pour la MSPRO
! ..............................................

case default 

     retour = pm_err_val_code_retour_inconnu
     chaine = ''

end select

! test si la chaine donnant la signification du code retour tient dans la longueur impartie
! =========================================================================================

if (len_trim(chaine) <= pm_signification_code_retour) then

   signification = trim(chaine)

else ! Pour l'utilisateur: il n'est pas possible de passer dans ce cas.
     ! Ce test est prevu pour verifier que la chaine ne depasse pas la longueur autorisee
     ! car ni la compilation, ni l'execution ne permettent de le detecter. Du coup la chaine
     ! signification serait tronquee ...

   retour = pm_err_valid
   signification = ''

end if

end subroutine mzipro_val_retour
