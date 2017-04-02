subroutine mz_val_code_retour (valeur, signification, code_retour)

! (C) Copyright CNES - MSLIB - 1998-2005

!************************************************************************
!
! But:  cette routine donne la signification du champ %valeur d'un code retour
! ===
!
! Note d'utilisation: l'entree valeur est un entier 
! ==================  et correspond a un champ %valeur d'un code retour
!
!$Historique
! ==========
!   + Version 0.1 (SP 146 ed01 rev00): creation
!                         (Date: 01/1998 - Realisation: Guylaine Prat)
!   + Version 0.1.1 (DE 187 ed01 rev00 + SP 146 ed02 rev00 + DE globale 182 ed01 rev00): 
!                         prise en compte de regles de codage
!                         (Date: 02/1998 - Realisation: Guylaine Prat)
!   + Version 1.0 (sans DE): ajout de nouveaux codes retour
!                         (Date: 08/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (sans DE): ajout de nouveaux codes retour
!                         (Date: 08/1999 - Realisation: Sylvain Vresk)
!   + Version 3.0 (sans DE): ajout de nouveaux codes retour
!                         (Date: 09/2000 - Realisation: Veronique Lepine)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 3.2 (DE 452 ed01 rev00): ajout d'une nouvelle plage et revision des libelles de
!                           pm_err_val_code_retour_inconnu et pm_err_numero_routine_inconnu
!                         (Date: 06/2002 - Realisation: Guylaine Prat)
!   + Version 4.0 (sans DE) : modif de la ligne rcs_id, et ajout de nouveaux codes retour
!                         (Date: 10/2002 - Realisation: Bruno Revelin)
!   + Version 4.1 (sans DE) : ajout de nouveaux codes retour
!                         (Date: 10/2003 - Realisation: Bruno Revelin)
!   + Version 5.0 (sans DE) : ajout de nouveaux codes retour
!                         (Date: 11/2003 - Realisation: Bruno Revelin et Veronique Lepine)
!   + Version 5.0 (DE 600 ed01 rev00) : revision libelle de pm_err_ind_model
!                         (Date: 11/2003 - Realisation: Bruno Revelin)
!   + Version 6.0 (sans DE): ajout de nouveaux codes retour
!                         (Date: 02/2004 - Realisation: Veronique Lepine)
!   + Version 6.2 (sans DE): ajout de nouveaux codes retour
!                         (Date: 01/2005 - Realisation: Guylaine Prat et Veronique Lepine)
!   + Version 6.3 : ajout de nouveaux codes retour
!                   DM-ID 381 : Integrer des routines du theme trajectoires interplanetaires
!                   DM-ID 162 : Rajout d une routine de propagation en j2
!                   (Date: 11/2005 - Realisation: Claire Fabre et Julien Bouillant)
!   + Version 6.4 : DM-ID 422 : Integrer l'ancienne procedure MPDLAM dans le 
!                   nouveau theme interplanetaire
!                   (Date: 04/2006 - Realisation: Claire Fabre - Atos origin)
!                   FA-ID 544 : Plantage lors de la resolution du probleme de Lambert
!                   (Date: 06/2006 - Realisation: Claire Fabre - Atos origin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : (Date : 02/05/2007 - Realisation: Sandrine Avril - Atos origin)
!                   DM-ID 636 : contrôle de la latitude pour la fonction mt_geoc_car
!   + Version 6.9 : (Date : 09/200 - Realisation: Atos origin)
!                   DM-ID 1092 : Ajout de nouveaux codes pour routines IERS 2003
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/03/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use valeur_code_retour_mslib
use numero_routine_mslib
use type_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
integer, intent(in)                                      :: valeur        ! valeur a traiter
character(len=pm_signification_code_retour), intent(out) :: signification ! chaine indiquant la signification du code retour
type(tm_code_retour), intent(out)                        :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

character(len=2*pm_signification_code_retour) :: chaine  ! Chaine intermediaire
                               ! contenant la signification du code retour.
                               ! Elle est surdimensionnee pour ne pas avoir de probleme.
                               ! Sa longueur reelle est testee par rapport a pm_signification_code_retour.

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mz_val_code_retour.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id = &
          ' $Id: mz_val_code_retour.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! Affectation suivant la valeur
! =============================

select case (valeur)

!.............
! Cas nominal
!.............

case (pm_OK)
     chaine = 'Retour normal.'

!........................................................................
! Probleme sur la valeur d'une constante physique [ +/- 1001 , +/- 1099 ]
!........................................................................

case (pm_err_mu_negatif)
     chaine = 'La constante gravitationnelle est negative.'
case (pm_err_mu_nul)
     chaine = 'La constante gravitationnelle est proche de 0.'
case (pm_err_apla_sup1)
     chaine = 'L''aplatissement (f) est superieur ou egal a 1.'
case (pm_err_apla_negatif)
     chaine = 'L''aplatissement (f) est negatif.'
case (pm_err_cn0_nul)
     chaine = 'Un des coefficients zonaux (CN0) est proche de 0.'
case (pm_err_r_equa_negatif)
     chaine = 'Le rayon equatorial de la planète est negatif'
case (pm_err_j2_negatif)
     chaine = 'Le terme en J2 du potentiel gravitationnel est negatif'
case (pm_err_r_equa_inf_egal1)
     chaine = 'Le rayon equatorial est inferieur ou egal a 1.'

!..................................................................................
! Probleme sur la valeur du demi grand-axe de la conique: a [ +/- 1101 , +/- 1199 ]
!..................................................................................

case (pm_err_a_negatif)
     chaine = 'Le demi grand axe (a) ou le parametre (p) de la parabole est negatif.'
case (pm_err_a_nul)
     chaine = 'Le demi grand axe (a) ou le parametre (p) de la parabole est proche de 0.'
case (pm_err_a_infini)
     chaine = 'L''inverse du demi grand axe (1/a) est proche de 0 (le demi grand axe est donc infini).'

!...................................................................
! Probleme sur la valeur de l'excentricite e [ +/- 1201 , +/- 1299 ]
!...................................................................

case (pm_warn_e_faible_brouwer)
     chaine = 'L''excentricite (e) est inferieure a 0,01; la precision est degradee dans les routines de calcul &
               &liees au modele d''extrapolation de BROUWER.'
case (pm_warn_e_grand_eck_hech)
     chaine = 'L''excentricite (e) est superieure a 0,005; la precision est degradee dans les routines de calcul &
               &liees au modele d''extrapolation de ECKSTEIN-HECHLER.'
case (pm_warn_e_parab)
     chaine = 'L''excentricite (e) est proche de 1 : les calculs ont ete faits en considerant que l''orbite etait &
              & parabolique.'
case (pm_warn_e_circul)
     chaine = 'L''excentricite (e) est proche de 0; l''orbite est circulaire.'
case (pm_warn_e_circul_i_equa)
     chaine = 'L''excentricite (e) est proche de 0 et sin(i) est proche de 0;&
        & l''orbite est circulaire et equatoriale (i=0 ou i=pi).'

case (pm_err_e_negatif)
     chaine = 'L''excentricite (e) est negative.'
case (pm_err_e_circul)
     chaine = 'L''excentricite (e) est proche de 0; l''orbite est circulaire.'
case (pm_err_e_grand_brouwer)
     chaine = 'L''excentricite (e) est superieure a 0.9; les routines de calcul liees au modele d''extrapolation de BROUWER&
              & n''autorisent pas ces valeurs de l''excentricite.'
case (pm_err_e_parab)
     chaine = 'L''excentricite (e) est proche de 1; l''orbite est parabolique.'
case (pm_err_e_hyperb)
     chaine = 'L''excentricite (e) est superieure a 1; l''orbite est hyperbolique.'
case (pm_err_e_non_ellip)
     chaine = 'L''excentricite(e) n''appartient pas a l''intervalle [0,1[; l''orbite n''est pas elliptique.'
case (pm_err_e_grand_eck_hech)
     chaine = 'L''excentricite (e) est superieure a 0.1; les routines de calcul liees au modele d''extrapolation de &
              & ECKSTEIN-HECHLER n''autorisent pas ces valeurs de l''excentricite.'
case (pm_err_e_faible_brouwer)
     chaine = 'L''excentricite (e) est inferieure a 0.0001; les routines de calcul liees au modele d''extrapolation de BROUWER&
              & n''autorisent pas ces valeurs de l''excentricite.'
case (pm_err_e_faible)
     chaine = 'L''excentricite (e) est inferieure a 1e-7; les routines de changement de variable car -> kep et car -> equa&
              & n''autorisent pas ces valeurs de l''excentricite.'
case (pm_err_jac_non_calc_e_circul)
     chaine = 'L''orbite est circulaire (e=0). Il en resulte que la jacobienne n''est pas calculable.'
case (pm_err_anom_v_incompatible_e)
     chaine = 'L''anomalie vraie v est incompatible avec l''excentricite de l''hyperbole:&
              & cos(v) < -1/e.'

!........................................................................................................
! Probleme sur la valeur de l'inclinaison i ou sur le vecteur inclinaison (ix,iy) [ +/- 1301 , +/- 1399 ]
!........................................................................................................

case (pm_warn_i_faible_brouwer)
     chaine = 'L''inclinaison (i) est inferieure a 0.018 radian; la precision est degradee dans les routines de calcul&
              & liees au modele d''extrapolation de BROUWER.'
case (pm_warn_i_equa)
   chaine = 'sin(i) est proche de 0; l''orbite est equatoriale (i=0 ou i=pi).'
case (pm_err_i_negatif)
     chaine = 'L''inclinaison (i) est negative.'
case (pm_err_i_equa)
     chaine = 'sin(i) est proche de 0; l''orbite est equatoriale (i=0 ou pi).'
case (pm_err_i_critique)
     chaine = 'L''inclinaison (i) a une valeur proche d''une des deux valeurs de l''inclinaison critique&
               & ( pour plus d''informations voir la routine mc_phys du theme Constantes ).'
case (pm_err_i_sup_pi)
     chaine = 'L''inclinaison (i) est superieure a pi.'
case (pm_err_ix_iy_sup2)
     chaine = 'La norme du vecteur inclinaison est trop grande (superieure a 2).'
case (pm_err_i_equa_retro)
     chaine = 'L''inclinaison (i) est egale a pi.'
case (pm_err_jac_non_calc_i_equa)
     chaine = 'L''orbite est equatoriale (i=0 ou pi). Il en resulte que la jacobienne n''est pas calculable.'

!...................................................................
! Probleme sur une date, une duree, un temps [ +/- 1401 , +/- 1499 ]
!...................................................................

case (pm_err_an_inf1950)
     chaine = 'L''annee est anterieure a 1950.'
case (pm_err_mois_interval)
     chaine = 'Le mois n''appartient pas a l''intervalle [1,12].'
case (pm_err_jour_interval)
     chaine = 'Le jour n''appartient pas a l''intervalle [1,n] avec n=28, 29, 30 ou 31 selon le mois de l''annee.'
case (pm_err_jul1950_negatif)
     chaine = 'Le nombre de jours juliens 1950 est negatif'
case (pm_err_duree_negatif)
     chaine = 'La duree est negative.'
case (pm_err_sec_interval_jour)
     chaine = 'Le nombre de secondes dans le jour n''appartient pas a l''intervalle [0.86400.[.'
case (pm_err_heure_interval)
     chaine = 'Le nombre d''heures dans le jour n''appartient pas a l''intervalle [0,24[.'
case (pm_err_min_interval)
     chaine = 'Le nombre de minutes dans l''heure n''appartient pas a l''intervalle [0,60[.'
case (pm_err_sec_interval_min)
     chaine = 'Le nombre de secondes dans la minute n''appartient pas a l''intervalle [0,60[.'
case (pm_err_an_sup2099)
     chaine = 'L''annee est posterieure a 2099.'
case (pm_err_jul1950_sup2099)
     chaine = 'Le nombre de jours juliens 1950 est trop grand ( date posterieure au 31 decembre 2099).'
case (pm_err_sec_negatif)
     chaine = 'Le nombre de secondes est negatif.'

!..................................................................
! Probleme sur la position et/ou la vitesse [ +/- 1501 , +/- 1599 ]
!..................................................................

case (pm_warn_pos_Oz_topo)
     chaine = 'Position sur l''axe Oz du repere topocentrique.                &
             & Il existe donc une infinite de solution pour la valeur du gisement,&
             & et on lui a donne arbitrairement la valeur zero.'
case (pm_warn_alt_negatif)
     chaine = 'L''altitude est negative. Les calculs ont ete effectues en prenant une altitude nulle.'
case (pm_warn_pos_Oz_ref)
     chaine = 'Position sur l''axe Oz du repere de reference.                &
             & Il existe donc une infinite de solution pour la valeur de la longitude,&
             & et on lui a donne arbitrairement une valeur.'

case (pm_err_pos_nul)
     chaine = 'La norme du vecteur position est proche de 0.'
case (pm_err_vit_nul)
     chaine = 'La norme du vecteur vitesse est proche de 0.'
case (pm_err_pos_vit_colineaire)
     chaine = 'Le produit vectoriel position-vitesse est pratiquement nul (ce qui signifie &
             &: position nulle ou vitesse nulle ou vecteurs position et vitesse colineaires).'
case (pm_err_pos_Oz_topo)
     chaine = 'Position sur l''axe Oz du repere topocentrique.                &
             & Il existe donc une infinite de solution pour la valeur du gisement. &
             & Les composantes de la vitesse ainsi que la jacobienne ne sont pas definies.'
case (pm_err_pos_orig_topo)
     chaine = 'La position est confondue avec l''origine du repere topocentrique.'
case (pm_err_alt_sup1000km)
     chaine = 'L''altitude est superieure a 1000km; or le modele d''atmosphere US76 n''est pas applicable&
              & a des altitudes superieures a 1000km.'
case (pm_err_orig_topo_centre_terre)
     chaine = 'L''origine du repere topocentrique est au centre de la Terre.'
case (pm_err_pos_Oz_ref)
     chaine = 'Position sur l''axe Oz du repere de reference.                &
             & Il existe donc une infinite de solution pour la valeur de la longitude. &
             & Les composantes de la vitesse ainsi que la jacobienne ne sont pas definies.'
case (pm_err_jac_non_calc_poles)
   chaine = 'La position de reference se situe aux poles, il en resulte que la jacobienne n''est pas calculable.'
case (pm_err_jac_non_calc_alt_neg)
   chaine = 'L''altitude est negative, il en resulte que la jacobienne n''est pas calculable.'
case (pm_err_meme_planete)
   chaine = 'Les planetes sont confondues, le repere BBR est donc indefini.'

!........................................................................
! Probleme sur la valeur d'une variable physique [ +/- 1601 , +/- 1699 ]
!........................................................................

case (pm_err_p_negatif)
     chaine = 'Le parametre p de la conique est negatif.'
case (pm_err_p_infini)
     chaine = 'Le parametre p de la conique est infini.'
case (pm_err_mlat_sup_pisur2)
     chaine = 'La latitude n''appartient pas a l''intervale [-pi/2,pi/2]'
case (pm_err_long_interval_0_2pi)
     chaine = 'La longitude n''appartient pas a l''intervale [0,2pi[' 

!.........................................................................
! Probleme de codage dans le code de l'utilisateur [ +/- 1801 , +/- 1898 ]
!.........................................................................

case (pm_warn_para_option)
     chaine = 'Manque de coherence entre les entrees optionnelles fournies et les sorties optionnelles demandees:&
              & soit il manque une ou plusieurs sorties optionnelles compte tenu des entrees optionnelles fournies, &
              & soit une entree optionnelle a ete fournie inutilement. Verifier votre sequence d''appel.'
case (pm_warn_conv_identite)
     chaine = 'Conversion identite: les types d''anomalie en entree et en sortie sont les memes.'
case (pm_warn_sub_ms)
     chaine = 'Probleme (avertissement) lors de l''appel de la subroutine passee en argument &
          & dans la séquence d''appel d''une routine de la MSLIB90.'

case (pm_err_para_option)
     chaine = 'Compte tenu des sorties optionnelles demandees, il manque des entrees optionnelles.'
case (pm_err_ind_nuta)
     chaine = 'La valeur donnee pour l''indicateur du modele de nutation est incorrecte.'
case (pm_err_ind_prec)
     chaine = 'La valeur donnee pour l''indicateur du modele de precession est incorrecte.'
case (pm_err_ind_model)
     chaine = 'La valeur donnee pour l''indicateur du modele est incorrecte.'
case (pm_err_ind_trsf)
     chaine = 'La valeur donnee pour l''indicateur de la transformation est incorrecte.'
case (pm_err_val_code_retour_inconnu)
     chaine = 'Cette valeur de code retour est inconnue dans la bibliotheque.'
case (pm_err_numero_routine_inconnu)
     chaine = 'Cette valeur de numero de routine est inconnue dans la bibliotheque.'
case (pm_err_ind_rep)
     chaine = 'La valeur donnee pour l''indicateur du repere est incorrecte.'
case (pm_err_planete)
     chaine = 'La valeur donnee pour l''indicateur de l''astre/planete est incorrecte.'
case (pm_err_clef_rot)
     chaine = 'La clef de rotation ne correspond pas a une rotation de Cardan ou d''Euler.'
case (pm_err_type_anom)
     chaine = 'La valeur donnee pour le type d''anomalie est incorrecte.'
case (pm_err_sens)
     chaine = 'La valeur donnee pour le sens de parcours est incorrecte.'
case (pm_err_nb_tours)
     chaine = 'La valeur donnee pour le nombre de tours est incorrecte.'
case (pm_err_sub_ms)
     chaine = 'Probleme (erreur) lors de l''appel de la subroutine passee en argument &
          & dans la séquence d''appel d''une routine de la MSLIB90.'

!.................................................
! Probleme de codage dans le code MSLIB [ - 1899 ]
!.................................................

case (pm_err_valid)
     chaine = 'Le code source de la MSLIB fortran 90 presente une anomalie. Contacter l''assistance utilisateur MSLIB pour &
               &correction de l''anomalie.'

!........................................................................................
! Probleme de convergence dans des routines de mecanique spatiale [ +/- 1901 , +/- 1998 ]
!........................................................................................

case (pm_err_conv_car_geod)
     chaine = 'L''algorithme iteratif utilise pour le calcul des coordonnees geodesiques a partir des coordonnees cartesiennes &
              &n''a pas reussi a converger vers la bonne solution. Contacter l''assistance utilisateur MSLIB.'
case (pm_err_conv_kepler_ellip)
     chaine = 'L''algorithme iteratif utilise pour la resolution de l''equation de KEPLER (orbite elliptique) n''a pas reussi a &
               &converger vers la bonne solution. Contacter l''assistance utilisateur MSLIB.'
case (pm_err_conv_kepler_hyperb)
     chaine = 'L''algorithme iteratif utilise pour la resolution de l''equation de KEPLER (orbite hyperbolique) n''a pas reussi a &
               &converger vers la bonne solution. Contacter l''assistance utilisateur MSLIB.'
case (pm_err_conv_kepler_gene)
     chaine = 'L''algorithme iteratif utilise pour la resolution de l''equation de KEPLER generalisee (equation avec le vecteur &
               &excentricite) n''a pas reussi a converger vers la bonne solution. Contacter l''assistance utilisateur MSLIB.'
case (pm_err_conv_brouwer)
     chaine = 'L''algorithme iteratif utilise pour le calcul des parametres moyens pour le modele de BROUWER n''a pas &
              &reussi a converger vers la bonne solution. Vous pouvez essayer d''autres valeurs pour les ecarts admissibles, &
              &ou bien contacter l''assistance utilisateur MSLIB.'
case (pm_err_conv_eck_hech)
     chaine = 'L''algorithme iteratif utilise pour le calcul des parametres moyens pour le modele de ECKSTEIN-HECHLER n''a pas &
              &reussi a converger vers la bonne solution. Vous pouvez essayer d''autres valeurs pour les ecarts admissibles, &
              &ou bien contacter l''assistance utilisateur MSLIB.'
case (pm_err_conv_lyddane)
     chaine = 'L''algorithme iteratif utilise pour le calcul des parametres moyens pour le modele de LYDDANE n''a pas &
              &reussi a converger vers la bonne solution. Contactez l''assistance utilisateur MSLIB.'
case (pm_err_conv_lambert)
     chaine = 'L''algorithme de recherche de la fonction TL(x) n''a pas &
              &reussi a converger vers la bonne solution. Contactez l''assistance utilisateur MSLIB.'

!..................................................................................................
! Probleme ayant une cause non identifiee a ce jour dans une routine de mecanique spatiale [- 1999]
!..................................................................................................

case (pm_err_cni)
     chaine = 'Probleme de calculs. Les causes possibles sont: probleme numerique lie a la representation en machine et a &
     &l''arithmetique virgule flottante; les donnees d''entree de la routine n''ont pas de sens physique; le cas physique &
     &correspondant aux donnees d''entree n''a pas ete pris en compte au cours du developpement de la routine; sur un ou &
     &plusieurs criteres, les donnees d''entree correspondent a un cas limite qui n''a pas pu etre detecte par les tests &
     &utilises habituellement dans la MSLIB. Contacter l assistance utilisateur MSLIB.'

!..............................................
! Probleme mathematique [ +/- 2001 , +/- 2999 ]
!..............................................

case (pm_warn_angle1_ou_3_indef)
     chaine = 'Infinite de solutions pour le premier et le troisieme angle. Arbitrairement nous donnons la valeur&
        & 0 au premier angle pour le cas de Cardan, ou au troisieme angle pour le cas d''Euler.'

case (pm_err_vect_nul)
     chaine = 'Vecteur nul.'
case (pm_err_eps_negatif)
     chaine = 'La valeur de l''epsilon en entree est negative.'
case (pm_err_eps_nul)
     chaine = 'La valeur de l''epsilon en entree est nulle.'
case (pm_err_axe_rot_nul)
     chaine = 'La norme de l''axe de rotation est nulle.'
case (pm_err_quat_nul)
     chaine = 'La norme du quaternion est nulle.'
case (pm_err_axe_rot_indef)
     chaine = 'La premiere composante du quaternion norme vaut 1 ou -1: l''axe de rotation est indefini.'
case (pm_err_mat_non_rot)
     chaine = 'La matrice n''est pas une matrice de rotation.'
case (pm_err_tdiv_nul)
     chaine = 'L''ecart entre les 2 valeurs de la fonction de Lambert est nul.'
case (pm_err_points_confondus)
     chaine = 'Les 2 points sont confondus.'
case (pm_err_transfert)
     chaine = 'Produit vectoriel nul. Transfert impossible.'
case (pm_err_hyperb)
     chaine = 'Cas hyperbolique.'
case (pm_err_det_nul)
     chaine = 'La matrice a un determinant nul.'
case (pm_err_calc_mat)
     chaine = 'Problème lors de produit de matrices'
case (pm_err_calc_transfo)
     chaine = 'Problème lors d une rotation'
case (pm_err_paramXYS)
     chaine = 'Problème lors du calcul des paramètres X, Y et S'

!...............................
! Numero de code retour inconnu
!...............................

case default
     code_retour%valeur = pm_err_val_code_retour_inconnu
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

   code_retour%valeur = pm_err_valid
   signification = ''

end if

code_retour%routine = pm_num_mz_val_code_retour
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mz_val_code_retour
