MODULE MSP_MATH

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  MSP_MATH
!
!$Resume
!	Module contenant des utilitaires mathematiques
!
!$Description
!	Module contenant des utilitaires mathematiques
!
!$Auteur
!       J.-J. WASBAUER
!
!$Version
!       $Id: MSP_MATH.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!       $Log: MSP_MATH.F90,v $
!       Revision 1.26  2010/10/20 09:35:43  mercadig
!       VERSION::AQ::20/10/2010:Ajout du marqueur de fin historique dans le cartouche
!
!       Revision 1.25  2010/06/11 09:07:33  mercadig
!       VERSION::FA-ID:1405:11/06/2010: Modification test sur demi-grand axe conformement a la FA
!
!       Revision 1.24  2010/06/10 15:20:23  mercadig
!       VERSION::FA-ID:1405:10/06/2010: Gestion d erreur pour la routine MSP_periode
!
!       Revision 1.23  2008/11/19 13:32:07  mercadig
!       DM-ID 733 : Mise a jour cartouche
!
!       Revision 1.22  2008/08/08 14:48:13  gss
!       DM-ID 1058 : (portage g95) initialisation à 0 des sorties avant le premier
!       return des fonctions.
!       Revision 1.21  2008/07/04 14:59:30  huec
!       DM-ID 1058 : Suppression de warnings G95
!       Revision 1.20  2008/06/03 08:02:18  huec
!       FA-ID 1015 : Initialisation de ier
!       Revision 1.19  2008/02/05 16:02:00  huec
!       DM-ID 11 : Suppression de la routine MSP_math_tchapp
!       Revision 1.18  2007/10/23 15:01:16  huec
!       FA-ID 776 : Variables locales non utilisees dans la MECASPA
!       Revision 1.17  2005/03/08 07:32:35  fabrec
!       DM-ID 111 : mise à jour des cartouches
!       Revision 1.16  2004/10/25 10:15:01  vivaresf
!       A-ID 228 : sortie des routines
!       egaler (surcharges de l'operateur =) en inout pour pouvoir desallouer les pointeurs
!       et eviter les fuites memoires
!       Revision 1.15  2004/10/12 13:50:31  vivaresf
!       Version 1-4
!       Revision 1.12.2.2  2004/05/03 14:37:08  vivaresf
!       DM-ID 83, MSP_jj_to_jcd : traitement des dates jour/secondes et/ou à J2000
!       Revision 1.12.2.1  2004/05/03 14:28:32  vivaresf
!       DM_83, version initiale
!       Revision 1.12  2003/03/20 17:42:45  adm_ipsi
!       MSP_excentrique_to_vraie, traitement des hyperboles
!       Revision 1.11  2003/02/24 17:53:30  adm_ipsi
!       *** empty log message ***
!       Revision 1.10  2003/02/10 17:26:26  adm_ipsi
!       MSP_pvit_absolu_vers_relatif, suppression d'un bloc non exécutable - cas (repere == MSP_ENUM_REPERE_CW)
!       Revision 1.9  2003/02/06 10:48:47  adm_ipsi
!       Ajout des fonctions MSP_math_tchapp et MSP_factorielle
!       Revision 1.8  2003/02/05 15:47:08  adm_ipsi
!       MSP_moyenne_to_excentrique, Remplacement de mvrxkn par mv_kepler_std
!       Revision 1.7  2003/01/08 15:12:39  adm_ipsi
!        Explicitation des conversions de type implicites
!       Revision 1.6  2003/01/07 18:11:40  adm_ipsi
!        suppression des variables non utilisées
!       Revision 1.5  2002/12/06 18:01:23  adm_ipsi
!       Utilisation des  fonctions .different.  et .egal. pour les comparaisons entre reel
!       Revision 1.4  2002/12/05 16:03:09  boschett
!       Ajout des opérateurs .egal. et .different. pour tester les égalités entre réels
!       Revision 1.3  2002/12/03 17:21:02  adm_ipsi
!        Ajout de implicit none
!       Revision 1.2  2002/10/02 15:25:54  adm_ipsi
!       Remplacement des subroutines MSP_pvit_absolu_vers_relatif et MSP_pvit_relatif_vers_absolu par celle fournies par le CNES
!       Revision 1.1.1.1  2002/09/30 14:09:35  adm_ipsi
!       Industrialisation de la MECASPA sans les modules de gestion d'erreurs
!       Revision 1.5  2001/11/08 09:26:27  util_am
!       Correction erreur sur la routine MSP_dureejj_to_jcd
!       Revision 1.4  2001/09/28 07:43:03  util_am
!       Modification de la routine MSP_moyenne_to_excentrique pour tenir compte des cas ou ecc=0 et sin M voisin de 0
!       Revision 1.3  2000/06/14 16:12:54  util_am
!       - Transfert de MSP_argument_latitude dans MSP_BULLETIN_DEF.F90
!       - Correction du calcul du dephasage entre deux bulletins
!       - Correction de la routine MSP_dureejj_to_jcd (pb des millisecondes)
!       - Ajout d'interfaces anglaises
!       - Mise à jour des cartouches
!       Revision 1.2  1999/09/22 15:09:17  util_am
!       Correction de la generation de certains messages d'erreurs
!       Correction bug dans MSP_jj_to_jcd
!       Mise a jour des cartouches
!       Revision 1.1  1999/09/03 16:42:40  util_am
!       MSP_ACCES.F90 :Ajout de routine pour la connexion et la deconnexion de section MADONA
!       MSP_BULLETIN_DEF.F90 : Ajout de MSP_modifier_bulletin, MSP_lire_BULLETIN
!       MECASPA.F90 : Ajout des use aux nouveaux modules
!
!$FinHistorique
!
!$Usage
!  use MSP_MATH
!
!$Structure
!
!$Global
!
!>  MSP_ENUM_REPERE_TNW    : <integer,parameter>          Repère orbital local TNW
!>  MSP_ENUM_REPERE_QSW    : <integer,parameter>          Repère orbital local TNW
!>  MSP_ENUM_REPERE_CW     : <integer,parameter>          Repère orbital local de Clohessy Wiltshire
!>  MSP_ENUM_CIBLE         : <integer,parameter>          
!>  MSP_ENUM_CHASSEUR      : <integer,parameter>          
!>  MSP_ENUM_G0            : <PM_REEL>                    Valeur de la gravité à altitude 0.
!>  MSP_ENUM_GRAVITATION   : <PM_REEL>                    
!#V
!>  MSP_2000jour           : <integer,parameter,private>  
!>  MSP_2000sec            : <pm_reel,parameter,private>  
!#
!$Common
!
!$Routines
!- MSP_rotate
!- MSP_rotation_matrix
!- MSP_convert_posvit_abs_to_rel
!- MSP_convert_posvit_rel_to_abs
!- MSP_convert_julian_to_calendar
!- MSP_convert_duration_to_jhms
!- MSP_convert_pos_cart_to_curv
!- MSP_convert_vel_cart_to_curv
!- MSP_convert_pos_curv_to_cart
!- MSP_convert_vel_curv_to_cart
!- MSP_compute_semi_major_axis
!- MSP_compute_period
!- MSP_compute_mean_motion
!- MSP_mean_to_excentric
!- MSP_excentric_to_true
!- MSP_mean_to_true
!- MSP_orbit_phasing_angle
!- MSP_kinetic_momentum
!- MSP_circular_velocity
!- MSP_thrust_duration
!- MSP_impulse_to_duration
!- MSP_impulse_consumption
!- MSP_spread_thrust_consumption
!- MSP_rotation
!- MSP_rotation_matrice
!- MSP_pvit_absolu_vers_relatif
!- MSP_pvit_relatif_vers_absolu
!- MSP_jj_to_jcd
!- MSP_dureejj_to_jcd
!
!$Fonctions
!- MSP_egal
!- MSP_different
!- MSP_normalise_vect
!- MSP_norme_vect
!- MSP_prod_scal
!- MSP_prod_vect
!- MSP_CW_to_QSW
!- MSP_QSW_to_CW
!- MSP_TNW_to_CW
!- MSP_CW_to_TNW
!- MSP_poscart_to_curv
!- MSP_vitcart_to_curv
!- MSP_poscurv_to_cart
!- MSP_vitcurv_to_cart
!- MSP_demi_grand_axe
!- MSP_periode
!- MSP_mouvement_moyen
!- MSP_moyenne_to_excentrique
!- MSP_excentrique_to_vraie
!- MSP_moyenne_to_vraie
!- MSP_dephasage_bulletin
!- MSP_moment_cinetique
!- MSP_vitesse_circulaire
!- MSP_duree_poussee
!- MSP_deltav_duree
!- MSP_consommation
!- MSP_consommation_continue
!- MSP_factorielle
!
!$Include
!
!$Module
!#V
!- MSLIB
!- MSP_GESTION_ERREUR
!#
!
!$Interface
!> msp_convert_pos_cart_to_curv :    MSP_poscart_to_curv
!> operator(.norm.) :                MSP_norme_vect
!> operator(.normalize.) :           MSP_normalise_vect
!> operator(.different.) :           MSP_different
!> msp_convert_posvit_rel_to_abs :   MSP_pvit_relatif_vers_absolu
!> msp_impulse_consumption :         MSP_consommation
!> msp_thrust_duration :             MSP_duree_poussee
!> operator(.ps.) :                  MSP_prod_scal
!> msp_mean_to_true :                MSP_moyenne_to_vraie
!> msp_kinetic_momentum :            MSP_moment_cinetique
!> msp_convert_julian_to_calendar :  MSP_jj_to_jcd
!> msp_circular_velocity :           MSP_vitesse_circulaire
!> operator(.pv.) :                  MSP_prod_vect
!> msp_rotate :                      MSP_rotation
!> msp_convert_vel_cart_to_curv :    MSP_vitcart_to_curv
!> msp_convert_vel_curv_to_cart :    MSP_vitcurv_to_cart
!> msp_mean_to_excentric :           MSP_moyenne_to_excentrique
!> msp_impulse_to_duration :         MSP_deltav_duree
!> msp_compute_mean_motion :         MSP_mouvement_moyen
!> msp_orbit_phasing_angle :         MSP_dephasage_bulletin
!> msp_excentric_to_true :           MSP_excentrique_to_vraie
!> operator(.egal.) :                MSP_egal
!> msp_convert_duration_to_jhms :    MSP_dureejj_to_jcd
!> msp_rotation_matrix :             MSP_rotation_matrice
!> msp_compute_period :              MSP_periode
!> msp_spread_thrust_consumption :   MSP_consommation_continue
!> msp_convert_pos_curv_to_cart :    MSP_poscurv_to_cart
!> msp_convert_posvit_abs_to_rel :   MSP_pvit_absolu_vers_relatif
!> msp_compute_semi_major_axis :     MSP_demi_grand_axe
!#V
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!#V
!.  MSP_egal MSP_different MSP_normalise_vect MSP_norme_vect MSP_prod_scal MSP_prod_vect
!#
!.  MSP_CW_to_QSW MSP_QSW_to_CW MSP_TNW_to_CW MSP_CW_to_TNW MSP_poscart_to_curv MSP_vitcart_to_curv
!.  MSP_poscurv_to_cart MSP_vitcurv_to_cart MSP_demi_grand_axe MSP_periode MSP_mouvement_moyen
!.  MSP_moyenne_to_excentrique MSP_excentrique_to_vraie MSP_moyenne_to_vraie MSP_dephasage_bulletin
!.  MSP_moment_cinetique MSP_vitesse_circulaire MSP_duree_poussee MSP_deltav_duree MSP_consommation
!.  MSP_consommation_continue MSP_factorielle MSP_rotate MSP_rotation_matrix MSP_convert_posvit_abs_to_rel
!.  MSP_convert_posvit_rel_to_abs MSP_convert_julian_to_calendar MSP_convert_duration_to_jhms
!.  MSP_convert_pos_cart_to_curv MSP_convert_vel_cart_to_curv MSP_convert_pos_curv_to_cart
!.  MSP_convert_vel_curv_to_cart MSP_compute_semi_major_axis MSP_compute_period MSP_compute_mean_motion
!.  MSP_mean_to_excentric MSP_excentric_to_true MSP_mean_to_true MSP_orbit_phasing_angle
!.  MSP_kinetic_momentum MSP_circular_velocity MSP_thrust_duration MSP_impulse_to_duration
!.  MSP_impulse_consumption MSP_spread_thrust_consumption MSP_rotation MSP_rotation_matrice
!.  MSP_pvit_absolu_vers_relatif MSP_pvit_relatif_vers_absolu MSP_jj_to_jcd MSP_dureejj_to_jcd
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  USE MSLIB, only : PM_REEL, PM_DEUX_PI, TM_jour_sec, &
       MU_NORME, MU_PROD_VECT, MO_QSW_GEO, MO_GEO_QSW, md_jourfrac_joursec, &
       MO_GEO_TNW, MO_GEO_TNW, md_julien_calend, md_duree_jhms
  USE MSP_GESTION_ERREUR
  USE MSP_MECASPA_DEF, only : MSP_EPSILON

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: MSP_MATH.F90 69 2012-09-11 08:33:34Z ffsm $'


  integer, parameter :: MSP_ENUM_REPERE_TNW = 1
  integer, parameter :: MSP_ENUM_REPERE_QSW = 2
  integer, parameter :: MSP_ENUM_REPERE_CW  = 3

  integer, parameter :: MSP_ENUM_CIBLE = 1
  integer, parameter :: MSP_ENUM_CHASSEUR = 2

  integer, parameter,private :: MSP_2000jour = 18262
  real(kind=pm_reel), parameter,private :: MSP_2000sec = 0.


  real(KIND=PM_REEL) :: MSP_ENUM_G0 = 9.80665_PM_REEL
  real(KIND=PM_REEL) :: MSP_ENUM_GRAVITATION = 3.9860047e+14_PM_REEL

  interface OPERATOR (.egal.)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  operator(.egal.)
!
!$Resume
!  Egalité entre réels
!
!$Description
!  Operateur permettant tester l'égalité entre deux réels   
!
!$Acces
!  PUBLIC
!
!$Usage
!  egal = reel1.egal.reel2
!.    real(KIND=PM_REEL) :: reel1, reel2
!.    logical :: egal
!
!$Procedures
!#V
!- MSP_egal
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
      module procedure MSP_egal
   end interface

   interface OPERATOR (.different.)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  operator(.different.)
!
!$Resume
!  Inégalité entre réels
!
!$Description
!  Operateur permettant tester l'inégalité entre deux réels   
!
!$Acces
!  PUBLIC
!
!$Usage
!  different = reel1.different.reel2
!.    real(KIND=PM_REEL) :: reel1, reel2
!.    logical :: different
!
!$Procedures
!#V
!- MSP_different
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
      module procedure MSP_different
   end interface




  interface OPERATOR (.PS.)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  operator(.PS.)
!
!$Resume
!  Produit scalaire
!
!$Description
!  Produit scalaire
!
!$Acces
!  PUBLIC
!
!$Usage
!  scal = vect1.PS.vect2
!.    real(KIND=PM_REEL), dimension(3) :: vect1, vect2
!.    real(KIND=PM_REEL) :: scal
!
!$Procedures
!#V
!- MSP_prod_scal
!#
!
!$Remarques
!
!$Mots-cles
! MATH VECTEUR
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     MODULE PROCEDURE MSP_prod_scal
  END interface

  interface OPERATOR (.PV.)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  operator(.PV.)
!
!$Resume
!  Produit vectoriel
!
!$Description
!  Produit vectoriel
!
!$Acces
!  PUBLIC
!
!$Usage
!  vect3 = vect1.PV.vect2
!.    real(KIND=PM_REEL), dimension(3) :: vect1, vect2
!.    real(KIND=PM_REEL), dimension(3) :: vect3
!
!$Procedures
!#V
!- MSP_prod_vect
!#
!
!$Remarques
!
!$Mots-cles
! MATH VECTEUR
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     MODULE PROCEDURE MSP_prod_vect
  END interface

  interface OPERATOR (.NORMALIZE.)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  operator(.NORMALIZE.)
!
!$Resume
!  Cet operateur permet de rendre un vecteur de norme 1
!
!$Description
!  Cet operateur permet de rendre un vecteur de norme 1
!
!$Acces
!  PUBLIC
!
!$Usage
!  vect2 = .NORMALIZE.vect1
!.    real(KIND=PM_REEL), dimension(3) :: vect1
!.    real(KIND=PM_REEL), dimension(3) :: vect2
!
!$Procedures
!#V
!- MSP_normalise_vect
!#
!
!$Remarques
!
!$Mots-cles
! MATH VECTEUR
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     MODULE PROCEDURE MSP_normalise_vect
  END interface

  interface OPERATOR (.NORM.)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  operator(.NORM.)
!
!$Resume
!  Cet operateur permet de retourner la norme d'un vecteur
!
!$Description
!  Cet operateur permet de retourner la norme d'un vecteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  norme = .NORM.vect1
!.    real(KIND=PM_REEL), dimension(3) :: vect1
!.    real(KIND=PM_REEL) :: norme
!
!$Procedures
!#V
!- MSP_norme_vect
!#
!
!$Remarques
!
!$Mots-cles
! MATH VECTEUR
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     MODULE PROCEDURE MSP_norme_vect
  END interface

  interface MSP_rotate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_rotate
!
!$Resume
!  This routine computes the rotation of a vector about another vector
!
!$Description
!  This routine computes the rotation of a vector about another vector 
!  from an angle defined by the sine and cosine 
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_rotate (vect_in, vect_axe, cangle, sangle, vect_out)
!.    real(KIND=PM_REEL) :: vect_in(3), vect_axe(3), cangle, sangle
!.    real(KIND=PM_REEL) :: vect_out(3)
!
!$Procedures
!- MSP_rotation
!
!$Remarques
!
!$Mots-cles
! MATH VECTEUR ROTATION
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_rotation
  end interface

  interface MSP_rotation_matrix

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_rotation_matrix
!
!$Resume
!  Computation of the matrix that transforms x into V
!
!$Description
!  This routine computes the transformation matrix to go from the (I,J,K) reference frame to
!  the (I',J',K') reference frame which has the given V vector as I'.
!  This routine computes the transformation matrix to go from the (I,J,K) reference frame to
!  the (I',J',K') reference frame which has the given V vector as I'.
!  This routine computes the transformation matrix to go from the (I,J,K) reference frame to
!  the (I',J',K') reference frame which has the given V vector as I'.
!  This routine computes the transformation matrix to go from the (I,J,K) reference frame to
!  the (I',J',K') reference frame which has the given V vector as I'.
!  This routine computes the transformation matrix to go from the (I,J,K) reference frame to
!  the (I',J',K') reference frame which has the given V vector as I'.
!  This routine computes the transformation matrix to go from the (I,J,K) reference frame to
!  the (I',J',K') reference frame which has the given V vector as I'.
!  This routine computes the transformation matrix to go from the (I,J,K) reference frame to
!  the (I',J',K') reference frame which has the given V vector as I'.
!  This routine computes the transformation matrix to go from the (I,J,K) reference frame to
!  the (I',J',K') reference frame which has the given V vector as I'.
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_rotation_matrix (vect_in, matrice)
!.    real(KIND=PM_REEL) :: vect_in(3)
!.    real(KIND=PM_REEL) :: matrice(3, 3)
!
!$Procedures
!- MSP_rotation_matrice
!
!$Remarques
!
!$Mots-cles
! MATH VECTEUR ROTATION
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_rotation_matrice
  end interface
   
  interface MSP_convert_posvit_abs_to_rel

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_convert_posvit_abs_to_rel
!
!$Resume
!  Conversion of two absolute positions into a relative one
!
!$Description
!  Conversion of two absolute positions into a relative one
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_convert_posvit_abs_to_rel(pos_ci_cart, vit_ci_cart, &
!.           pos_ch_cart, vit_ch_cart, posrel, vitrel, repere, origine)
!.    real(KIND=PM_REEL), dimension(3) :: pos_ci_cart, vit_ci_cart
!.    real(KIND=PM_REEL), dimension(3) :: pos_ch_cart, vit_ch_cart
!.    real(KIND=PM_REEL), dimension(3) :: posrel, vitrel
!.    integer :: repere, origine
!
!$Procedures
!- MSP_pvit_absolu_vers_relatif
!
!$Remarques
!
!$Mots-cles
! MATH TRANSFORMATION
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_pvit_absolu_vers_relatif
  end interface
  
  interface MSP_convert_posvit_rel_to_abs

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_convert_posvit_rel_to_abs
!
!$Resume
!  Conversion of a relative position and an absolute one into the composed absolute one
!
!$Description
!  Conversion of a relative position and an absolute one into the composed absolute one
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_convert_posvit_rel_to_abs(posrel, vitrel, pos_ci_cart, vit_ci_cart, &
!.           pos_ch_cart, vit_ch_cart, repere, bull_origine)
!.    real(KIND=PM_REEL), dimension(:) :: pos_ci_cart, vit_ci_cart
!.    real(KIND=PM_REEL), dimension(:) :: posrel, vitrel
!.    real(KIND=PM_REEL), dimension(:) :: bull_origine
!.    integer :: repere
!.    real(KIND=PM_REEL), dimension(:) :: pos_ch_cart, vit_ch_cart
!
!$Procedures
!- MSP_pvit_relatif_vers_absolu
!
!$Remarques
!
!$Mots-cles
! MATH TRANSFORMATION
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_pvit_relatif_vers_absolu
  end interface
  
  interface MSP_convert_julian_to_calendar

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_convert_julian_to_calendar
!
!$Resume
!  Conversion of a julian day into a calendar day
!
!$Description
!  Conversion of a julian day into a calendar day
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_convert_julian_to_calendar(jjul, ian, imois, ijour, iheure, imin, isec, &
!.           imils, ier, jjsec, origdat)
!.    real(KIND=PM_REEL) :: jjul
!.    integer :: ian, imois, ijour, iheure, imin, isec, imils, ier
!.    type(tm_jour_sec) :: jjsec
!.    integer :: origdat
!
!$Procedures
!- MSP_jj_to_jcd
!
!$Remarques
!
!$Mots-cles
! MATH DATE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_jj_to_jcd
  end interface
  
  interface MSP_convert_duration_to_jhms

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_convert_duration_to_jhms
!
!$Resume
!  Conversion of a duration into DHMS
!
!$Description
!  Conversion of a duration into DHMS
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_convert_duration_to_jhms(duree, ijour, iheure, imin, isec, imils, ier)
!.    real(KIND=PM_REEL) :: duree
!.    integer :: ijour, iheure, imin, isec, imils, ier
!
!$Procedures
!- MSP_dureejj_to_jcd
!
!$Remarques
!
!$Mots-cles
! MATH DATE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_dureejj_to_jcd
  end interface
  
  interface MSP_convert_pos_cart_to_curv

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_convert_pos_cart_to_curv
!
!$Resume
!  Conversion of cartesian position into a curved position on a circle of radius R
!
!$Description
!  Conversion of cartesian position into a curved position on a circle of radius R
!
!$Acces
!  PUBLIC
!
!$Usage
!  pos_curv = MSP_convert_pos_cart_to_curv(rayon, pos_cart)
!.    real(KIND=PM_REEL) :: rayon
!.    real(KIND=PM_REEL), dimension(3) :: pos_cart
!.    real(KIND=PM_REEL), dimension(3) :: pos_curv
!
!$Procedures
!- MSP_poscart_to_curv
!
!$Remarques
!
!$Mots-cles
! MATH TRANSFORMATION
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_poscart_to_curv
  end interface
  
  interface MSP_convert_vel_cart_to_curv

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_convert_vel_cart_to_curv
!
!$Resume
!  Conversion of cartesian velocity into a curved velocity  on a circle of radius R
!
!$Description
!  Conversion of cartesian velocity into a curved velocity  on a circle of radius R
!
!$Acces
!  PUBLIC
!
!$Usage
!  vit_curv = MSP_convert_vel_cart_to_curv(rayon, pos_cart, vit_cart)
!.    real(KIND=PM_REEL) :: rayon
!.    real(KIND=PM_REEL), dimension(3) :: pos_cart, vit_cart
!.    real(KIND=PM_REEL), dimension(3) :: vit_curv
!
!$Procedures
!- MSP_vitcart_to_curv
!
!$Remarques
!
!$Mots-cles
! MATH TRANSFORMATION
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_vitcart_to_curv
  end interface
  
  interface MSP_convert_pos_curv_to_cart

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_convert_pos_curv_to_cart
!
!$Resume
!  Conversion of a curved position on a circle of radius R into cartesian coordinates
!
!$Description
!  Conversion of a curved position on a circle of radius R into cartesian coordinates
!
!$Acces
!  PUBLIC
!
!$Usage
!  pos_cart = MSP_convert_pos_curv_to_cart(rayon, pos_curv)
!.    real(KIND=PM_REEL) :: rayon
!.    real(KIND=PM_REEL), dimension(3) :: pos_curv
!.    real(KIND=PM_REEL), dimension(3) :: pos_cart
!
!$Procedures
!- MSP_poscurv_to_cart
!
!$Remarques
!
!$Mots-cles
! MATH TRANSFORMATION
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_poscurv_to_cart
  end interface
  
  interface MSP_convert_vel_curv_to_cart

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_convert_vel_curv_to_cart
!
!$Resume
!  Conversion of a curved velocity on a circle of radius R into cartesian coordinates
!
!$Description
!  Conversion of a curved velocity on a circle of radius R into cartesian coordinates
!
!$Acces
!  PUBLIC
!
!$Usage
!  vit_cart = MSP_convert_vel_curv_to_cart(rayon, pos_curv, vit_curv)
!.    real(KIND=PM_REEL) :: rayon
!.    real(KIND=PM_REEL), dimension(3) :: pos_curv, vit_curv
!.    real(KIND=PM_REEL), dimension(3) :: vit_cart
!
!$Procedures
!- MSP_vitcurv_to_cart
!
!$Remarques
!
!$Mots-cles
! MATH TRANSFORMATION
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_vitcurv_to_cart
  end interface
  
  interface MSP_compute_semi_major_axis

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_compute_semi_major_axis
!
!$Resume
!  Compute the semi-major axis from the 6 cartesian coordinates
!
!$Description
!  Compute the semi-major axis from the 6 cartesian coordinates
!
!$Acces
!  PUBLIC
!
!$Usage
!  semia = MSP_compute_semi_major_axis(pvit, [mu])
!.    real(kind = PM_REEL), dimension(6) :: pvit
!.    real(kind = PM_REEL) :: mu
!
!$Procedures
!- MSP_demi_grand_axe
!
!$Remarques
!
!$Mots-cles
! MATH ORBITE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_demi_grand_axe
  end interface
  
  interface MSP_compute_period

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_compute_period
!
!$Resume
!  Compute the semi-major axis from the 6 cartesian coordinates or the semi-major axis
!
!$Description
!  Compute the semi-major axis from the 6 cartesian coordinates or the semi-major axis
!
!$Acces
!  PUBLIC
!
!$Usage
!  periode = MSP_compute_period([pvit], [a], [mu])
!.    real(kind = PM_REEL), dimension(6) :: pvit
!.    real(kind = PM_REEL) :: mu, a
!
!$Procedures
!- MSP_periode
!
!$Remarques
!
!$Mots-cles
! MATH ORBITE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_periode
  end interface
  
  interface MSP_compute_mean_motion

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_compute_mean_motion
!
!$Resume
!  Compute the mean motion from the 6 cartesian coordinates or the semi-major axis
!
!$Description
!  Compute the mean motion from the 6 cartesian coordinates or the semi-major axis
!
!$Acces
!  PUBLIC
!
!$Usage
!  w = MSP_compute_mean_motion([pvit], [a], [mu])
!.    real(kind = PM_REEL), dimension(6) :: pvit
!.    real(kind = PM_REEL) :: a, mu
!
!$Procedures
!- MSP_mouvement_moyen
!
!$Remarques
!
!$Mots-cles
! MATH ORBITE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_mouvement_moyen
  end interface
  
  interface MSP_mean_to_excentric

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_mean_to_excentric
!
!$Resume
!  Conversion of the mean anomaly into excentric anomaly
!
!$Description
!  Conversion of the mean anomaly into excentric anomaly
!
!$Acces
!  PUBLIC
!
!$Usage
!  E = MSP_mean_to_excentric(m, ecc)
!.    real(kind = PM_REEL) :: m, ecc
!
!$Procedures
!- MSP_moyenne_to_excentrique
!
!$Remarques
!
!$Mots-cles
! MATH ORBITE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_moyenne_to_excentrique
  end interface
  
  interface MSP_excentric_to_true

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_excentric_to_true
!
!$Resume
!  Conversion of the excentric anomaly into true anomaly
!
!$Description
!  Conversion of the excentric anomaly into true anomaly
!
!$Acces
!  PUBLIC
!
!$Usage
!  v = MSP_excentric_to_true(E, ecc)
!.    real(kind = PM_REEL) :: E, ecc
!
!$Procedures
!- MSP_excentrique_to_vraie
!
!$Remarques
!
!$Mots-cles
! MATH ORBITE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_excentrique_to_vraie
  end interface
  
  interface MSP_mean_to_true

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_mean_to_true
!
!$Resume
!  Conversion of the mean anomaly into true anomaly
!
!$Description
!  Conversion of the mean anomaly into true anomaly
!
!$Acces
!  PUBLIC
!
!$Usage
!  v = MSP_mean_to_true(m, ecc)
!.    real(kind = PM_REEL) :: m, ecc
!
!$Procedures
!- MSP_moyenne_to_vraie
!
!$Remarques
!
!$Mots-cles
! MATH ORBITE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_moyenne_to_vraie
  end interface
  
  interface MSP_orbit_phasing_angle

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_orbit_phasing_angle
!
!$Resume
!  Computation of the phasing angle between two orbits
!
!$Description
!  Computation of the phasing angle between two orbits
!
!$Acces
!  PUBLIC
!
!$Usage
!  delta = MSP_orbit_phasing_angle(param1, param2)
!.    real(kind = PM_REEL), dimension(6) :: param1, param2
!
!$Procedures
!- MSP_dephasage_bulletin
!
!$Remarques
!
!$Mots-cles
! MATH ORBITE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_dephasage_bulletin
  end interface
  
  interface  MSP_kinetic_momentum

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_kinetic_momentum
!
!$Resume
!  Computation of the kinetic momentum
!
!$Description
!  Computation of the kinetic momentum
!
!$Acces
!  PUBLIC
!
!$Usage
!  mom_cin = MSP_kinetic_momentum(pvit)
!.    real(kind = PM_REEL), dimension(6) :: pvit
!.    real(KIND=PM_REEL) , dimension(3) :: mom_cin
!
!$Procedures
!- MSP_moment_cinetique
!
!$Remarques
!
!$Mots-cles
! MATH ORBITE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_moment_cinetique
  end interface
  
  interface  MSP_circular_velocity

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_circular_velocity
!
!$Resume
!  Computation of the circular velocity 
!
!$Description
!  Computation of the circular velocity 
!
!$Acces
!  PUBLIC
!
!$Usage
!  vitcirc_g50 = MSP_circular_velocity(posg50, momcin_g50, [mu])
!.    real(kind = PM_REEL), dimension(3) :: posg50, momcin_g50
!.    real(kind = PM_REEL) :: mu
!.    real(kind = PM_REEL), dimension(3) :: vitcirc_g50
!
!$Procedures
!- MSP_vitesse_circulaire
!
!$Remarques
!
!$Mots-cles
! MATH ORBITE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_vitesse_circulaire
  end interface
  
  interface  MSP_thrust_duration

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_thrust_duration
!
!$Resume
!  Computation of the duration of the thrust from the impulse and the initial mass
!
!$Description
!  Computation of the duration of the thrust from the impulse and the initial mass
!
!$Acces
!  PUBLIC
!
!$Usage
!  dt = MSP_thrust_duration(dv, poussee, isp, masse_init)
!.    real(KIND=PM_REEL) :: dv, poussee, isp, masse_init
!
!$Procedures
!- MSP_duree_poussee
!
!$Remarques
!
!$Mots-cles
! MATH POUSSEE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_duree_poussee
  end interface
  
  interface  MSP_impulse_to_duration

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_impulse_to_duration
!
!$Resume
!  Computation the impulse from the duration of the thrust and the initial mass
!
!$Description
!  Computation the impulse from the duration of the thrust and the initial mass
!
!$Acces
!  PUBLIC
!
!$Usage
!  dv = MSP_impulse_to_duration(dt, poussee, isp, masse_init)
!.    real(KIND=PM_REEL) :: dt, poussee, isp, masse_init
!
!$Procedures
!- MSP_deltav_duree
!
!$Remarques
!
!$Mots-cles
! MATH POUSSEE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_deltav_duree
  end interface
  
  interface  MSP_impulse_consumption

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_impulse_consumption
!
!$Resume
!  Computation the mass loss during a maneuver 
!
!$Description
!  Computation the mass loss during a maneuver 
!
!$Acces
!  PUBLIC
!
!$Usage
!  masse = MSP_impulse_consumption(dv, isp, masse_init)
!.    real(KIND=PM_REEL) :: dv, isp, masse_init
!
!$Procedures
!- MSP_consommation
!
!$Remarques
!
!$Mots-cles
! MATH POUSSEE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_consommation
  end interface
  
  interface  MSP_spread_thrust_consumption

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_spread_thrust_consumption
!
!$Resume
!  Computation the mass loss during a spread thrust maneuver 
!
!$Description
!  Computation the mass loss during a spread thrust maneuver 
!
!$Acces
!  PUBLIC
!
!$Usage
!  masse = MSP_spread_thrust_consumption(isp, duree, poussee)
!.    real(KIND=PM_REEL) :: isp, duree, poussee
!
!$Procedures
!- MSP_consommation_continue
!
!$Remarques
!
!$Mots-cles
! MATH POUSSEE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure MSP_consommation_continue
  end interface

  PRIVATE MSP_normalise_vect, MSP_norme_vect, MSP_prod_scal, MSP_prod_vect, MSP_egal, MSP_different


CONTAINS

  FUNCTION  MSP_egal (reel1, reel2) result (egal)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_egal
!
!$Resume
!  Fonction permettant tester l'egalité entre deux réels         
!
!$Description
!  Fonction permettant tester l'egalité entre deux réels 
!
!$Acces
!  PRIVE
!
!$Usage
!  egal = MSP_egal (reel1, reel2)
!.    real(KIND=PM_REEL) :: reel1, reel2
!.    logical :: egal
!
!$Arguments
!>E     reel1  :<PM_REEL>   Numéro réel 1
!>E     reel2  :<PM_REEL>   Numéro réel 2
!>S     egal   :<logical>   Résultat de l'égalité
!
!$Common
!
!$Routines
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

        real(KIND=PM_REEL), intent(in) :: reel1, reel2
        logical :: egal
        real(KIND=PM_REEL) :: k

        ! Initialisation du facteur d'echelle k
        k=1._pm_reel
    
        if (abs(reel1-reel2) < k*SPACING(reel2)) then
           egal = .true.
        else
           egal = .false.
        endif

      end FUNCTION MSP_egal

      FUNCTION  MSP_different (reel1, reel2) result (different)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_different
!
!$Resume
!  Fonction permettant tester l'inégalité entre deux réels         
!
!$Description
!  Fonction permettant tester l'inégalité entre deux réels 
!
!$Acces
!  PRIVE
!
!$Usage
!  different = MSP_different (reel1, reel2)
!.    real(KIND=PM_REEL) :: reel1, reel2
!.    logical :: different
!
!$Arguments
!>E     reel1      :<PM_REEL>   Numéro réel 1
!>E     reel2      :<PM_REEL>   Numéro réel 2
!>S     different  :<logical>   Résultat de l'inégalité
!
!$Common
!
!$Routines
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

        real(KIND=PM_REEL), intent(in) :: reel1, reel2
        logical :: different
        real(KIND=PM_REEL) :: k

        ! Initialisation du facteur d'echelle k
        k=1._pm_reel
    
        if (abs(reel1-reel2) < k*SPACING(reel2)) then
           different = .false.
        else
           different = .true.
        endif

      end FUNCTION MSP_different


  FUNCTION MSP_normalise_vect (vect1) result (vect2)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_normalise_vect
!
!$Resume
!            Réalise la normalisation d'un vecteur
!
!$Description
!            Réalise la normalisation d'un vecteur
!
!$Auteur
!       J.-J. WASBAUER
!
!$Acces
!  PRIVE
!
!$Usage
!  vect2 = MSP_normalise_vect (vect1)
!.    real(KIND=PM_REEL), dimension(3) :: vect1
!.    real(KIND=PM_REEL), dimension(3) :: vect2
!
!$Arguments
!>E     vect1  :<PM_REEL,DIM=(3)>   vecteur d'entrŽée
!>S     vect2  :<PM_REEL,DIM=(3)>   vecteur normalisŽé
!
!$Common
!
!$Routines
!- mu_norme
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH VECTEUR
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(KIND=PM_REEL), intent(in), dimension(3) :: vect1
    real(KIND=PM_REEL), dimension(3) :: vect2
    real(KIND=PM_REEL), dimension(3) :: vect
    real(KIND=PM_REEL) :: norm

    type(tm_code_retour) :: code_erreur

    norm = MSP_norme_vect(vect1(:))
    if (norm .different. 0._pm_reel) then 
       call mu_norme(vect1(:), norm, code_erreur, vect_norme=vect(:))
       vect2(:)=vect(:)
       call MSP_signaler_message (ier_mslib=code_erreur)
       if (MSP_gen_messages()) return
    else
       call MSP_signaler_message (cle_mes="MSP_OPER_NORME",&
            routine=".NORMALIZE.",type=MSP_ENUM_ERREUR)
       return
    end if
    
  end FUNCTION MSP_normalise_vect


  FUNCTION MSP_norme_vect (vect1) result (norme)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_norme_vect
!
!$Resume
!            Calcule la norme d'un vecteur
!
!$Description
!            Calcule la norme d'un vecteur
!
!$Auteur
!       J.-J. WASBAUER
!
!$Acces
!  PRIVE
!
!$Usage
!  norme = MSP_norme_vect (vect1)
!.    real(KIND=PM_REEL), dimension(3) :: vect1
!.    real(KIND=PM_REEL) :: norme
!
!$Arguments
!>E     vect1  :<PM_REEL,DIM=(3)>   Vecteur d'entrŽée
!>S     norme  :<PM_REEL>           Norme du vecteur
!
!$Common
!
!$Routines
!- mu_norme
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH VECTEUR
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(KIND=PM_REEL), intent(in), dimension(3) :: vect1
    real(KIND=PM_REEL) :: norme
    type(tm_code_retour) :: code_erreur

!    norme = sqrt ( vect1(1)*vect1(1) + vect1(2)*vect1(2) + vect1(3)*vect1(3) )
    call mu_norme(vect1(:), norme, code_erreur)
    call MSP_signaler_message (ier_mslib=code_erreur)
    if (MSP_gen_messages()) return
    
  end FUNCTION MSP_norme_vect


    
  FUNCTION MSP_prod_scal (vect1, vect2) result (scal)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_prod_scal
!
!$Resume
!            Calcule le produit scalaire entre deux vecteurs de dimension 3
!
!$Description
!            Calcule le produit scalaire entre deux vecteurs de dimension 3
!
!$Auteur
!       J.-J. WASBAUER
!
!$Acces
!  PRIVE
!
!$Usage
!  scal = MSP_prod_scal (vect1, vect2)
!.    real(KIND=PM_REEL), dimension(3) :: vect1, vect2
!.    real(KIND=PM_REEL) :: scal
!
!$Arguments
!>E     vect1  :<PM_REEL,DIM=(3)>   Vecteur 1
!>E     vect2  :<PM_REEL,DIM=(3)>   Vecteur 2
!>S     scal   :<PM_REEL>           Produit scalaire des deux vecteurs
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH VECTEUR
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(KIND=PM_REEL), intent(in), dimension(3) :: vect1, vect2
    real(KIND=PM_REEL) :: scal
    
    scal = DOT_PRODUCT(vect1, vect2)

  end FUNCTION MSP_prod_scal
    
    
    
  FUNCTION MSP_prod_vect (vect1, vect2) result (vect3)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_prod_vect
!
!$Resume
!            Calcule le produit vectoriel entre deux vecteurs
!
!$Description
!            Calcule le produit vectoriel entre deux vecteurs
!
!$Auteur
!       J.-J. WASBAUER
!
!$Acces
!  PRIVE
!
!$Usage
!  vect3 = MSP_prod_vect (vect1, vect2)
!.    real(KIND=PM_REEL), dimension(3) :: vect1, vect2
!.    real(KIND=PM_REEL), dimension(3) :: vect3
!
!$Arguments
!>E     vect1  :<PM_REEL,DIM=(3)>   vecteur 1
!>E     vect2  :<PM_REEL,DIM=(3)>   vecteur 2
!>S     vect3  :<PM_REEL,DIM=(3)>   produit vectoriel: vecteur1^vecteur2
!
!$Common
!
!$Routines
!- mu_prod_vect
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH VECTEUR
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(KIND=PM_REEL), intent(in), dimension(3) :: vect1, vect2
    real(KIND=PM_REEL), dimension(3) :: vect3
    type(tm_code_retour) :: code_erreur
    
    call mu_prod_vect(vect1(:), vect2(:), vect3(:), code_erreur)
    call MSP_signaler_message (ier_mslib=code_erreur)
    if (MSP_gen_messages()) return

  end FUNCTION MSP_prod_vect


  SUBROUTINE MSP_rotation (vect_in, vect_axe, cangle, sangle, vect_out)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_rotation
!
!$Resume
!            Routine effectuant la rotation d'un vecteur d'un angle donne autour 
!            d'un autre vecteur
!
!$Description
!            Calcul des projections du vecteur à faire tourner sur l'axe et 
!            perpendiculairement à l'axe. Puis calcul des rotations des composantes 
!            et reconstitution du vecteur
!
!$Auteur
!  J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_rotation (vect_in, vect_axe, cangle, sangle, vect_out)
!.    real(KIND=PM_REEL) :: vect_in(3), vect_axe(3), cangle, sangle
!.    real(KIND=PM_REEL) :: vect_out(3)
!
!$Arguments
!>E     vect_in   :<PM_REEL,DIM=(3)>   vecteur a faire tourner
!>E     vect_axe  :<PM_REEL,DIM=(3)>   vecteur definissant la direction de l'axe oriente de rotation
!>E     cangle    :<PM_REEL>           cosinus de l'angle de rotation
!>E     sangle    :<PM_REEL>           sinus de l'angle de rotation
!>S     vect_out  :<PM_REEL,DIM=(3)>   vecteur resultat de la rotation
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH VECTEUR ROTATION
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(KIND=PM_REEL), intent(IN) :: vect_in(3), vect_axe(3), cangle, sangle
    real(KIND=PM_REEL), intent(OUT) :: vect_out(3)
    real(KIND=PM_REEL) :: vect_aux(3), scal

    scal = vect_in(:).PS.vect_axe(:)
    vect_aux(:) = vect_axe(:).PV.vect_in(:)
    if (MSP_gen_messages("MSP_rotation")) return

    vect_out(:) = cangle*vect_in(:) + (1._pm_reel - cangle)*scal*vect_axe(:) + sangle*vect_aux(:)

  end SUBROUTINE MSP_rotation




  SUBROUTINE MSP_rotation_matrice (vect_in, matrice)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_rotation_matrice
!
!$Resume
!  Calcul de la matrice de rotation transformant x en V
!
!$Description
!            Cette routine calcule la matrice de passage d'un repŽère (I,J,K) Žà 
!            un repŽère (I',J',K') ayant le vecteur V donnŽé pour axe unitaire I'.
!            - Normalisation du vecteur V
!            - Calcul de l'angle entre I et V.
!            - Calcul de l'axe de rotation pour faire tourner I vers I'.
!            - Calcul des images J' et K' de J et K
!            - Constitution de la matrice de passage Žà partir des coordonnŽées (I',J',K')
!
!$Auteur
!  J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_rotation_matrice (vect_in, matrice)
!.    real(KIND=PM_REEL) :: vect_in(3)
!.    real(KIND=PM_REEL) :: matrice(3, 3)
!
!$Arguments
!>E     vect_in  :<PM_REEL,DIM=(3)>      vecteur V
!>S     matrice  :<PM_REEL,DIM=(3, 3)>   matrice de passage
!
!$Common
!
!$Routines
!- MSP_rotation
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH VECTEUR ROTATION
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
    
    real(KIND=PM_REEL), intent(IN) :: vect_in(3)
    real(KIND=PM_REEL), intent(OUT) :: matrice(3, 3)
    real(KIND=PM_REEL), dimension(3) :: i_vect, j_vect, k_vect, j_rot_vect, k_rot_vect
    real(KIND=PM_REEL), dimension(3) :: vect_axe, vdir
    real(KIND=PM_REEL) :: cangle, sangle

    i_vect = 0._pm_reel
    j_vect = 0._pm_reel
    k_vect = 0._pm_reel
    i_vect(1) = 1._pm_reel
    j_vect(2) = 1._pm_reel
    k_vect(3) = 1._pm_reel

    vdir(:) = .NORMALIZE.vect_in(:)
    if (MSP_gen_messages("MSP_rotation_matrice")) return

    cangle = i_vect(:).PS.vdir(:)
    sangle = sqrt(1._pm_reel - cangle*cangle)

    if (sangle .egal. 0._PM_REEL) then 
       matrice(:, :) = 0._PM_REEL
       matrice(1, 1) = 1._PM_REEL
       matrice(2, 2) = 1._PM_REEL
       matrice(3, 3) = 1._PM_REEL
       return
    endif

    vect_axe(:) = .NORMALIZE.(i_vect(:).PV.vdir(:))
    if (MSP_gen_messages("MSP_rotation_matrice")) return

    call MSP_rotation(j_vect, vect_axe, cangle, sangle, j_rot_vect)
    if (MSP_gen_messages("MSP_rotation_matrice")) return

    call MSP_rotation( k_vect, vect_axe, cangle, sangle, k_rot_vect)
    if (MSP_gen_messages("MSP_rotation_matrice")) return

    matrice(1, :) = vdir(:)
    matrice(2, :) = j_rot_vect(:)
    matrice(3, :) = k_rot_vect(:)

  end SUBROUTINE MSP_rotation_matrice




  FUNCTION MSP_CW_to_QSW (vect_in) RESULT (vect_out)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_CW_to_QSW
!
!$Resume
!            Passage de CW a QSW
!
!$Description
!            Cette fonction transforme un vecteur exprimŽé dans le repŽère de 
!            Clohessy-Wiltshire dans le repŽère orbital local QSW (hypothŽèse 
!            circulaire oŽù les axes de ces repŽères sont alignŽés avec CW.
!
!$Auteur
!  J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  vect_out = MSP_CW_to_QSW (vect_in)
!.    real(KIND=PM_REEL) :: vect_in(3)
!.    real(KIND=PM_REEL) :: vect_out(3)
!
!$Arguments
!>E     vect_in   :<PM_REEL,DIM=(3)>   vecteur d'entree CW
!>S     vect_out  :<PM_REEL,DIM=(3)>   vecteur sortie dans QSW
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH REPERE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
    
    real(KIND=PM_REEL), intent(IN) :: vect_in(3)
    real(KIND=PM_REEL) :: vect_out(3)

    vect_out(1) = - vect_in(3)
    vect_out(2) = vect_in(1)
    vect_out(3) = -vect_in(2)

  end FUNCTION MSP_CW_to_QSW




  FUNCTION MSP_QSW_to_CW(vect_in) RESULT (vect_out)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_QSW_to_CW
!
!$Resume
!            Passage de QSW a CW.
!
!$Description
!            Cette fonction transforme un vecteur exprimŽé dans le repŽère de 
!            orbital local QSW dans le repŽère CW (hypothŽèse circulaire oŽù les 
!            axes de ces repŽères sont alignŽés avec QSW)
!
!$Auteur
!       J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  vect_out = MSP_QSW_to_CW(vect_in)
!.    real(KIND=PM_REEL) :: vect_in(3)
!.    real(KIND=PM_REEL) :: vect_out(3)
!
!$Arguments
!>E     vect_in   :<PM_REEL,DIM=(3)>   vecteur d'entree QSW
!>S     vect_out  :<PM_REEL,DIM=(3)>   vecteur sortie en CW
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH REPERE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
    
    real(KIND=PM_REEL), intent(IN) :: vect_in(3)
    real(KIND=PM_REEL) :: vect_out(3)

    vect_out(1) = vect_in(2)
    vect_out(2) = -vect_in(3)
    vect_out(3) = -vect_in(1)

  end FUNCTION MSP_QSW_to_CW




  FUNCTION MSP_TNW_to_CW(vect_in) RESULT (vect_out)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_TNW_to_CW
!
!$Resume
!            Passage de TNW a CW.
!
!$Description
!            Cette fonction transforme un vecteur exprimŽé dans le repŽère de
!            orbital local TNW dans le repŽère CW (hypothŽèse circulaire oŽù les
!            axes de ces repŽères sont alignŽés avec TNW)
!
!$Auteur
!       J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  vect_out = MSP_TNW_to_CW(vect_in)
!.    real(KIND=PM_REEL) :: vect_in(3)
!.    real(KIND=PM_REEL) :: vect_out(3)
!
!$Arguments
!>E     vect_in   :<PM_REEL,DIM=(3)>   vecteur d'entree TNW
!>S     vect_out  :<PM_REEL,DIM=(3)>   vecteur sortie CW
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH REPERE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
    
    real(KIND=PM_REEL), intent(IN) :: vect_in(3)
    real(KIND=PM_REEL) :: vect_out(3)

    vect_out(1) = vect_in(1)
    vect_out(2) = -vect_in(3)
    vect_out(3) = vect_in(2)

  end FUNCTION MSP_TNW_to_CW



  FUNCTION MSP_CW_to_TNW(vect_in) RESULT (vect_out)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_CW_to_TNW
!
!$Resume
!            Passage de CW a TNW.
!
!$Description
!            Cette fonction transforme un vecteur exprimŽé dans le repŽère de 
!            Clohessy-Wiltshire dans le repŽère orbital local TNW (hypothŽèse 
!            circulaire oŽù les axes de ces repŽères sont alignŽés avec CW)
!
!$Auteur
!       J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  vect_out = MSP_CW_to_TNW(vect_in)
!.    real(KIND=PM_REEL) :: vect_in(3)
!.    real(KIND=PM_REEL) :: vect_out(3)
!
!$Arguments
!>E     vect_in   :<PM_REEL,DIM=(3)>   vecteur entree CW
!>S     vect_out  :<PM_REEL,DIM=(3)>   vecteur sortie TNW
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH REPERE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
    
    real(KIND=PM_REEL), intent(IN) :: vect_in(3)
    real(KIND=PM_REEL) :: vect_out(3)

    vect_out(1) = vect_in(1)
    vect_out(2) = vect_in(3)
    vect_out(3) = -vect_in(2)

  end FUNCTION MSP_CW_to_TNW




  FUNCTION MSP_poscart_to_curv(rayon, pos_cart) result(pos_curv)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_poscart_to_curv
!
!$Resume
!  Transformation d'une position cartesienne vers une position curviligne
!
!$Description
!  Cette fonction permet de transformer une position en coordonnees cartesiennes
!  en une position curviligne sur un cercle de rayon R.
!
!$Auteur
!  J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  pos_curv = MSP_poscart_to_curv(rayon, pos_cart)
!.    real(KIND=PM_REEL) :: rayon
!.    real(KIND=PM_REEL), dimension(3) :: pos_cart
!.    real(KIND=PM_REEL), dimension(3) :: pos_curv
!
!$Arguments
!>E     rayon     :<PM_REEL>           Rayon de courbure du cercle
!>E     pos_cart  :<PM_REEL,DIM=(3)>   vecteur position en coordonnees cartesiennes
!>S     pos_curv  :<PM_REEL,DIM=(3)>   vecteur position en coordonnees curvilignes
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH TRANSFORMATION
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
    
    real(KIND=PM_REEL), intent(IN) :: rayon
    real(KIND=PM_REEL), dimension(3), intent(IN) :: pos_cart
    real(KIND=PM_REEL), dimension(3) :: pos_curv
    real(KIND=PM_REEL) :: rpos, ctheta, stheta, theta

    rpos = sqrt(pos_cart(1)**2 + (rayon - pos_cart(3))**2)
    ctheta = (rayon - pos_cart(3))/rpos
    stheta = pos_cart(1)/rpos
    theta = sign(1._PM_REEL, stheta)*acos(ctheta)

    pos_curv(1) = rayon*theta
    pos_curv(2) = pos_cart(2)
    pos_curv(3) = rayon - rpos

  end FUNCTION MSP_poscart_to_curv



  FUNCTION MSP_vitcart_to_curv(rayon, pos_cart, vit_cart) result(vit_curv)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_vitcart_to_curv
!
!$Resume
!  Transformation d'une vitesse cartesienne en vitesse curviligne
!
!$Description
!  Cette fonction permet de transformer des vitesses cartesiennes en vitesse 
!  dans un repere curviligne sur un cercle de rayon R.
!
!$Auteur
!  J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  vit_curv = MSP_vitcart_to_curv(rayon, pos_cart, vit_cart)
!.    real(KIND=PM_REEL) :: rayon
!.    real(KIND=PM_REEL), dimension(3) :: pos_cart, vit_cart
!.    real(KIND=PM_REEL), dimension(3) :: vit_curv
!
!$Arguments
!>E     rayon     :<PM_REEL>           Rayon de courbure du cercle
!>E     pos_cart  :<PM_REEL,DIM=(3)>   position cartesienne 
!>E     vit_cart  :<PM_REEL,DIM=(3)>   vitesse cartesienne
!>S     vit_curv  :<PM_REEL,DIM=(3)>   vitesse curviligne
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH TRANSFORMATION
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
    
    real(KIND=PM_REEL), intent(IN) :: rayon
    real(KIND=PM_REEL), dimension(3), intent(IN) :: pos_cart, vit_cart
    real(KIND=PM_REEL), dimension(3) :: vit_curv
    real(KIND=PM_REEL) :: rpos, ctheta, stheta

    rpos = sqrt(pos_cart(1)**2 + (rayon - pos_cart(3))**2)
    ctheta = (rayon - pos_cart(3))/rpos
    stheta = pos_cart(1)/rpos

    vit_curv(1) =  vit_cart(1)*ctheta + vit_cart(3)*stheta
    vit_curv(2) =  vit_cart(2)
    vit_curv(3) = -vit_cart(1)*stheta + vit_cart(3)*ctheta

  end FUNCTION MSP_vitcart_to_curv



  FUNCTION MSP_poscurv_to_cart(rayon, pos_curv) result(pos_cart)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_poscurv_to_cart
!
!$Resume
!  Transformation d'une position curviligne en position cartesienne
!
!$Description
!  Cette fonction permet de transformer une position curviligne sur un 
!  cercle de rayon R en une position cartesienne
!
!$Auteur
!  J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  pos_cart = MSP_poscurv_to_cart(rayon, pos_curv)
!.    real(KIND=PM_REEL) :: rayon
!.    real(KIND=PM_REEL), dimension(3) :: pos_curv
!.    real(KIND=PM_REEL), dimension(3) :: pos_cart
!
!$Arguments
!>E     rayon     :<PM_REEL>           rayon de courbure
!>E     pos_curv  :<PM_REEL,DIM=(3)>   position curviligne
!>S     pos_cart  :<PM_REEL,DIM=(3)>   position cartesienne
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH TRANSFORMATION
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
    
    real(KIND=PM_REEL), intent(IN) :: rayon
    real(KIND=PM_REEL), dimension(3), intent(IN) :: pos_curv
    real(KIND=PM_REEL), dimension(3) :: pos_cart
    real(KIND=PM_REEL) :: theta, ctheta, stheta

    theta = pos_curv(1)/rayon
    stheta = sin(theta)
    ctheta = cos(theta)

    pos_cart(1) = (rayon - pos_curv(3))*stheta
    pos_cart(2) = pos_curv(2)
    pos_cart(3) = rayon*(1._PM_REEL - ctheta) + pos_curv(3)*ctheta

  end FUNCTION MSP_poscurv_to_cart


  FUNCTION MSP_vitcurv_to_cart(rayon, pos_curv, vit_curv) result(vit_cart)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_vitcurv_to_cart
!
!$Resume
!  Transformation d'une vitesse cartesienne en vitesse curviligne
!
!$Description
!  Cette fonction permet de transformer une vitesse 
!  curviligne sur un cercle de rayon R en une vitesse cartesienne.
!
!$Auteur
!  J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  vit_cart = MSP_vitcurv_to_cart(rayon, pos_curv, vit_curv)
!.    real(KIND=PM_REEL) :: rayon
!.    real(KIND=PM_REEL), dimension(3) :: pos_curv, vit_curv
!.    real(KIND=PM_REEL), dimension(3) :: vit_cart
!
!$Arguments
!>E     rayon     :<PM_REEL>           Rayon de courbure
!>E     pos_curv  :<PM_REEL,DIM=(3)>   position curviligne
!>E     vit_curv  :<PM_REEL,DIM=(3)>   vitesse curviligne
!>S     vit_cart  :<PM_REEL,DIM=(3)>   vitesse cartesienne
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH TRANSFORMATION
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
    
    real(KIND=PM_REEL), intent(IN) :: rayon
    real(KIND=PM_REEL), dimension(3), intent(IN) :: pos_curv, vit_curv
    real(KIND=PM_REEL), dimension(3) :: vit_cart
    real(KIND=PM_REEL) :: theta, ctheta, stheta

    theta = pos_curv(1)/rayon
    stheta = sin(theta)
    ctheta = cos(theta)

    vit_cart(1) = vit_curv(1)*ctheta - vit_curv(3)*stheta
    vit_cart(2) = vit_curv(2)
    vit_cart(3) = vit_curv(1)*stheta + vit_curv(3)*ctheta

  end FUNCTION MSP_vitcurv_to_cart





  real(KIND=PM_REEL) FUNCTION MSP_demi_grand_axe(pvit, mu) result(semia)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_demi_grand_axe
!
!$Resume
!            Calcul du demi-grand axe
!
!$Description
!            Cette fonction calcule le demi-grand axe Žà partir des paramŽètres
!            orbitaux exprimŽés en cartŽésien
!
!$Auteur
!       J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  semia = MSP_demi_grand_axe(pvit, [mu])
!.    real(kind = PM_REEL), dimension(6) :: pvit
!.    real(kind = PM_REEL) :: mu
!
!$Arguments
!>E     pvit   :<PM_REEL,DIM=(6)>   position-vitesse cartesienne
!>[E]   mu     :<PM_REEL>           
!>S     semia  :<PM_REEL>           demi-grand axe
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH ORBITE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments en entrée
    real(kind = PM_REEL), dimension(6), intent(IN) :: pvit
    real(kind = PM_REEL), intent(IN), optional :: mu

    ! Variables locales
    real(kind = PM_REEL) :: rdist, rvit, qq, mu_loc

    ! Initialisation de la sortie
    semia = 0._PM_REEL

    if (PRESENT(mu)) then 
       mu_loc = mu
    else
       mu_loc =MSP_ENUM_GRAVITATION
    end if

    rdist = .NORM.pvit(1:3)
    if (MSP_gen_messages("MSP_demi_grand_axe")) return

    rvit  = .NORM.pvit(4:6)
    if (MSP_gen_messages("MSP_demi_grand_axe")) return

    qq    = rdist * rvit * rvit / mu_loc
    semia = rdist/(2._PM_REEL - qq)

  end FUNCTION MSP_demi_grand_axe




  real(KIND=PM_REEL) FUNCTION MSP_periode(pvit, a, mu) result(periode)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_periode
!
!$Resume
!            Calcul de la periode orbitale
!
!$Description
!            Cette fonction calcule la pŽériode de l'orbite Žà partir des 
!            paramŽètres orbitaux exprimŽés en cartŽésien
!
!$Auteur
!       J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  periode = MSP_periode([pvit], [a], [mu])
!.    real(kind = PM_REEL), dimension(6) :: pvit
!.    real(kind = PM_REEL) :: mu, a
!
!$Arguments
!>[E]   pvit     :<PM_REEL,DIM=(6)>   position-vitesse en cartesien
!>[E]   a        :<PM_REEL>           Demi grand axe
!>[E]   mu       :<PM_REEL>           constante de garvitation ( par défautMSP_ENUM_GRAVITATION)
!>S     periode  :<PM_REEL>           période de l'orbite
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH ORBITE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments en entrée
    real(kind = PM_REEL), dimension(6), intent(IN), optional :: pvit
    real(kind = PM_REEL), intent(IN), optional :: mu, a
    ! Variables locales
    real(kind = PM_REEL) :: semia, w
    real(kind = PM_REEL) :: mu_loc

    ! Initialisation de la sortie
    periode = 0._PM_REEL
    
    if (PRESENT(mu)) then 
       mu_loc = mu
    else
       mu_loc =MSP_ENUM_GRAVITATION
    end if

    if (PRESENT(a)) then 
       semia = a
    else
       semia = MSP_demi_grand_axe(pvit, mu=mu)
       if (MSP_gen_messages("MSP_periode")) return
    end if
    
    ! Controle sur le demi-grand axe: si inférieur à 1 km on remonte une erreur
    if (semia < 1._PM_REEL) then
       call MSP_signaler_message (cle_mes="MSP_periode")
       return
    endif

    w = sqrt(mu_loc/semia**3)
    periode = pm_deux_pi/w

  end FUNCTION MSP_periode






  real(KIND=PM_REEL) FUNCTION MSP_mouvement_moyen(pvit, a, mu) result(w)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_mouvement_moyen
!
!$Resume
!            Calcul le mouvement moyen
!
!$Description
!            Cette fonction calcule le mouvement moyen de l'orbite Žà partir des 
!            paramŽètres orbitaux exprimŽés en cartŽésiens ou du demi-grand axe
!
!$Auteur
!       J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  w = MSP_mouvement_moyen([pvit], [a], [mu])
!.    real(kind = PM_REEL), dimension(6) :: pvit
!.    real(kind = PM_REEL) :: a, mu
!
!$Arguments
!>[E]   pvit  :<PM_REEL,DIM=(6)>   position-vitesse en cartesien
!>[E]   a     :<PM_REEL>           demi-grand axe en m
!>[E]   mu    :<PM_REEL>           constante de gravitation 
!>S     w     :<PM_REEL>           constante de garvitation ( par défautMSP_ENUM_GRAVITATION)
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH ORBITE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments en entrée
    real(kind = PM_REEL), dimension(6), intent(IN), optional :: pvit
    real(kind = PM_REEL), intent(IN), optional :: a, mu
    ! Variable locale
    real(kind = PM_REEL) :: periode
    
    ! Initialisation de la sortie
    w = 0._PM_REEL

    periode = MSP_periode(pvit=pvit,a=a,mu=mu)
    if (MSP_gen_messages("MSP_mouvement_moyen")) return

    w = pm_deux_pi/periode

  end FUNCTION MSP_mouvement_moyen






  real(KIND=PM_REEL) FUNCTION MSP_moyenne_to_excentrique(m, ecc) result(E)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_moyenne_to_excentrique
!
!$Resume
!            Calcul de l'anomalie excentrique en fonction de M et e
!
!$Description
!            Cette fonction calcule l'anomalie excentrique Žà partir 
!            de l'anomalie moyenne et de l'excentricitŽé
!
!$Auteur
!       J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  E = MSP_moyenne_to_excentrique(m, ecc)
!.    real(kind = PM_REEL) :: m, ecc
!
!$Arguments
!>E     m    :<PM_REEL>   Anomalie moyenne
!>E     ecc  :<PM_REEL>   Excentricite
!>S     E    :<PM_REEL>   Anomalie excentrique
!
!$Common
!
!$Routines
!- mv_kepler_std
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH ORBITE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind = PM_REEL), intent(IN) :: m, ecc

    real(KIND = PM_REEL) :: anoEx
    type(tm_code_retour)      :: code_retour


    call mv_kepler_std(m, ecc,anoEx,code_retour)
    call MSP_signaler_message (ier_mslib=code_retour)
    E = anoEx

    if (MSP_ERREUR) then
       call MSP_signaler_message (cle_mes="MSP_moyenne_to_excentrique")
       return
    endif


  end FUNCTION MSP_moyenne_to_excentrique




  real(KIND=PM_REEL) FUNCTION MSP_excentrique_to_vraie(E, ecc) result(v)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_excentrique_to_vraie
!
!$Resume
!            Calcul l'anomalie vraie a partir de l'anomalie excentrique
!
!$Description
!            Cette fonction calcule l'anomalie vraie Žà partir 
!            de l'anomalie excentrique et de l'excentricitŽé
!
!$Auteur
!       J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  v = MSP_excentrique_to_vraie(E, ecc)
!.    real(kind = PM_REEL) :: E, ecc
!
!$Arguments
!>E     E    :<PM_REEL>   Anomalie excentrique
!>E     ecc  :<PM_REEL>   Excentricité
!>S     v    :<PM_REEL>   Anomalie vraie.
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH ORBITE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind = PM_REEL), intent(IN) :: E, ecc
    real(KIND = PM_REEL) ::  beta, w
    real(kind=pm_reel),parameter :: epsilon = 1.e-08_PM_REEL

    !cv = (cos(E) - ecc)/(1._PM_REEL - ecc*cos(E))
    !v = sign(1._PM_REEL, sin(E))*acos(max(min(1._PM_REEL, cv), -1._PM_REEL))

    if (ecc < (1._PM_REEL-epsilon)) then
           
       ! Cas de l'ellipse
           
       beta = ecc/(1._PM_REEL+sqrt(1._PM_REEL-ecc*ecc))
       v = E +2._PM_REEL*atan(beta*sin(E)/(1._PM_REEL-beta*cos(E)))
           
    else
           
       ! Cas de l'hyperbole
           
       w=sqrt((ecc+1._PM_REEL)/(ecc-1._PM_REEL))*tanh(E/2._PM_REEL)
       v = 2._PM_REEL*atan(w)
           
    endif


  end FUNCTION MSP_excentrique_to_vraie




  real(KIND=PM_REEL) FUNCTION MSP_moyenne_to_vraie(m, ecc) result(v)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_moyenne_to_vraie
!
!$Resume
!            Passage de l'anomalie moyenne Žà l'anomalie vraie.
!
!$Description
!            Cette fonction calcule l'anomalie vraie Žà partir 
!            de l'anomalie excentrique et de l'excentricitŽé
!
!$Auteur
!       J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  v = MSP_moyenne_to_vraie(m, ecc)
!.    real(kind = PM_REEL) :: m, ecc
!
!$Arguments
!>E     m    :<PM_REEL>   Anomalie moyenne
!>E     ecc  :<PM_REEL>   ExcentricitŽé
!>S     v    :<PM_REEL>   Anomalie vraie
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH ORBITE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments en entrée
    real(kind = PM_REEL), intent(IN) :: m, ecc
    ! Varaible locale
    real(KIND = PM_REEL) :: E

    ! Initialisation de la sortie
    v = 0._PM_REEL

    E = MSP_moyenne_to_excentrique(m, ecc)
    if (MSP_gen_messages("MSP_moyenne_to_vraie")) return

    v = MSP_excentrique_to_vraie(E, ecc)

  end FUNCTION MSP_moyenne_to_vraie




  real(KIND=PM_REEL) FUNCTION MSP_dephasage_bulletin(param1, param2) result(delta)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_dephasage_bulletin
!
!$Resume
!            Calcul de l'Žécart entre les deux positions sur orbites [0; 2pi]
!
!$Description
!            Cette fonction calcule le dŽéphasage angulaire entre deux
!            positions orbitales exprimŽées en cartŽésien
!
!$Auteur
!       J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  delta = MSP_dephasage_bulletin(param1, param2)
!.    real(kind = PM_REEL), dimension(6) :: param1, param2
!
!$Arguments
!>E     param1  :<PM_REEL,DIM=(6)>   ParamŽètres orbitaux de la premiŽère position orbitale
!>E     param2  :<PM_REEL,DIM=(6)>   ParamŽètres orbitaux de la seconde position orbitale
!>S     delta   :<PM_REEL>           Ecart angulaire en radians entre 0 et 2pi
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH ORBITE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind = PM_REEL), dimension(6), intent(IN) :: param1, param2

    delta = param1(4) + MSP_moyenne_to_vraie(param1(6), param1(2)) &
          - param2(4) - MSP_moyenne_to_vraie(param2(6), param2(2))
    if (MSP_gen_messages("MSP_dephasage_bulletin")) return
       
    delta = MODULO(delta, pm_deux_pi)

  end FUNCTION MSP_dephasage_bulletin


  FUNCTION MSP_moment_cinetique(pvit) result(mom_cin)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_moment_cinetique
!
!$Resume
!            Calcul du moment cinetique orbital
!
!$Description
!            Cette fonction calcule la valeur du moment cinŽétique orbital 
!            Žà partir des paramŽètres orbitaux exprimŽés en cartŽésiens
!
!$Auteur
!       J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  mom_cin = MSP_moment_cinetique(pvit)
!.    real(kind = PM_REEL), dimension(6) :: pvit
!.    real(KIND=PM_REEL) , dimension(3) :: mom_cin
!
!$Arguments
!>E     pvit     :<PM_REEL,DIM=(6)>   ParamŽètres orbitaux cartŽésiens
!>S     mom_cin  :<PM_REEL,DIM=(3)>   moment cinŽétique orbital en cartŽésiens
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH ORBITE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind = PM_REEL), dimension(6), intent(IN) :: pvit
    real(KIND=PM_REEL) , dimension(3) :: mom_cin
    
    mom_cin(:) = pvit(1:3).PV.pvit(4:6)
    if (MSP_gen_messages("MSP_moment_cinetique")) return

  end FUNCTION MSP_moment_cinetique



  FUNCTION MSP_vitesse_circulaire(posg50, momcin_g50, mu) result(vitcirc_g50)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_vitesse_circulaire
!
!$Resume
!  Calcul de la vitesse circulaire locale.
!
!$Description
!            Cette fonction calcule le vecteur vitesse absolu pour une position
!            orbitale donnŽée et un moment cinŽétique donnŽé (definit le plan
!            orbital dans le quel ce vecteur vitesse doit Žêtre projetŽé). Les
!            deux vecteurs doivent Žêtre exprimŽés en absolu dans le repŽère
!            GAMMA50 CNES.
!            - Calcul de la direction dans laquelle le vecteur vitesse pointe
!            - Calcul du module de la vitesse orbitale locale circulaire
!
!$Auteur
!       J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  vitcirc_g50 = MSP_vitesse_circulaire(posg50, momcin_g50, [mu])
!.    real(kind = PM_REEL), dimension(3) :: posg50, momcin_g50
!.    real(kind = PM_REEL) :: mu
!.    real(kind = PM_REEL), dimension(3) :: vitcirc_g50
!
!$Arguments
!>E     posg50       :<PM_REEL,DIM=(3)>   Vecteur position absolue dans GAMMA50 CNES
!>E     momcin_g50   :<PM_REEL,DIM=(3)>   Vecteur mooment cinetique absolu dans GAMMA50 CNES
!>[E]   mu           :<PM_REEL>           Constante de gravitation (par defaut celle de la Terre)
!>S     vitcirc_g50  :<PM_REEL,DIM=(3)>   Vitesse circulaire locale
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH ORBITE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind = PM_REEL), dimension(3), intent(IN) :: posg50, momcin_g50
    real(kind = PM_REEL), intent(IN), optional :: mu

    real(kind = PM_REEL), dimension(3) :: vitcirc_g50
    real(kind = PM_REEL), dimension(3) :: QQ, WW, SS1, SS
    real(kind = PM_REEL) :: vitesse, mu_loc

    if (PRESENT(mu)) then 
       mu_loc = mu
    else
       mu_loc = MSP_ENUM_GRAVITATION
    end if

    QQ(:) = .NORMALIZE.posg50(:)
    if (MSP_gen_messages("MSP_vitesse_circulaire" )) return

    WW(:) = .NORMALIZE.momcin_g50(:)
    if (MSP_gen_messages("MSP_vitesse_circulaire" )) return

    SS1(:) = WW(:).PV.QQ(:)
    if (MSP_gen_messages("MSP_vitesse_circulaire" )) return

    SS(:) = .NORMALIZE.SS1(:)
    if (MSP_gen_messages("MSP_vitesse_circulaire" )) return

    vitesse = sqrt(mu_loc/.NORM.posg50(:))
    if (MSP_gen_messages("MSP_vitesse_circulaire" )) return

    vitcirc_g50(:) = vitesse*SS(:)

  end FUNCTION MSP_vitesse_circulaire
  


  real(KIND=PM_REEL) FUNCTION MSP_duree_poussee(dv, poussee, isp, masse_init) result(dt)
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_duree_poussee
!
!$Resume
!            Calcul de la duree de poussee.
!
!$Description
!            Cette fonction calcule la duree de poussee nŽécessaire pour
!            rŽéaliser un deltav donnŽé. Elle s'appuie en entrŽée sur une 
!            valeur de la masse initiale, de l'isp du moteur et de la
!            poussee dŽélivrŽée par le systŽème propulsif. 
!
!$Auteur
!       J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  dt = MSP_duree_poussee(dv, poussee, isp, masse_init)
!.    real(KIND=PM_REEL) :: dv, poussee, isp, masse_init
!
!$Arguments
!>E     dv          :<PM_REEL>   Valeur du deltav Žà viser (en m/s)
!>E     poussee     :<PM_REEL>   Niveau de poussŽée du systŽème propulsif (en N)
!>E     isp         :<PM_REEL>   valeur de l'ISP du systŽème propulsif (en s)
!>E     masse_init  :<PM_REEL>   masse initiale du vŽéhicule (en kg)
!>S     dt          :<PM_REEL>   Durée de la poussée
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH POUSSEE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(KIND=PM_REEL), intent(IN) :: dv, poussee, isp, masse_init
    real(kind=PM_REEL) :: debit

    debit = poussee/(MSP_ENUM_G0*isp)
    dt = (masse_init/debit)*(1._PM_REEL - exp(-dv/MSP_ENUM_G0/isp))

  end FUNCTION MSP_duree_poussee



  real(KIND=PM_REEL) FUNCTION MSP_deltav_duree(dt, poussee, isp, masse_init) result(dv)
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_deltav_duree
!
!$Resume
!  Calcul du deltav Žà partir de la duree de propulsion.
!
!$Description
!            Cette fonction calcule la valeur du DV atteint lors d'une 
!            d'une poussŽée continue en focntion de la durŽée effective 
!            de la poussŽée, de l'impulsion spŽécifique de la poussŽée et 
!            de la masse initiale du vŽéhicule.
!
!$Auteur
!       J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  dv = MSP_deltav_duree(dt, poussee, isp, masse_init)
!.    real(KIND=PM_REEL) :: dt, poussee, isp, masse_init
!
!$Arguments
!>E     dt          :<PM_REEL>   DurŽée de la poussŽée continue (en s)
!>E     poussee     :<PM_REEL>   Niveau de poussŽée du systŽème propulsif (en N)
!>E     isp         :<PM_REEL>   valeur de l'ISP du systŽème propulsif (en s)
!>E     masse_init  :<PM_REEL>   masse initiale du vŽéhicule (en kg)
!>S     dv          :<PM_REEL>   Impulsion
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH POUSSEE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(KIND=PM_REEL), intent(IN) :: dt, poussee, isp, masse_init
    real(KIND=PM_REEL) :: debit

    debit = poussee/(MSP_ENUM_G0*isp)
    dv = -MSP_ENUM_G0*isp*log(1._PM_REEL-debit*dt/masse_init)

  end FUNCTION MSP_deltav_duree



  real(KIND=PM_REEL) FUNCTION MSP_consommation(dv, isp, masse_init) result(masse)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consommation
!
!$Resume
!  Calcul de la consommation d'ergol en poussee impulsionnelle.
!
!$Description
!            Cette fonction calcule la masse d'ergol consommŽée par une 
!            une poussŽée impulsionnelle, connaissant l'isp du moteur, 
!            la massse initiale du vŽéhicule et le DV rŽéalisŽé. 
!
!$Auteur
!       J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  masse = MSP_consommation(dv, isp, masse_init)
!.    real(KIND=PM_REEL) :: dv, isp, masse_init
!
!$Arguments
!>E     dv          :<PM_REEL>   Valeur du DV impulsionnel (en m/s)
!>E     isp         :<PM_REEL>   Valeur de l'ISP du systŽème propulsif (en s)
!>E     masse_init  :<PM_REEL>   masse initiale du vŽéhicule (en kg)
!>S     masse       :<PM_REEL>   masse d'ergols consommée
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH POUSSEE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(KIND=PM_REEL), intent(IN) :: dv, isp, masse_init

    masse = masse_init*(1._PM_REEL - exp(-dv/MSP_ENUM_G0/isp))

  end FUNCTION MSP_consommation





  real(KIND=PM_REEL) FUNCTION MSP_consommation_continue(isp, duree, poussee) result(masse)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_consommation_continue
!
!$Resume
!  Calcul de la consommation d'ergols en poussee continue.
!
!$Description
!            Cette fonction calcule la masse d'ergols consommŽée pour une
!            poussŽée continue en fonction de la duree de l'isp et de la
!            poussŽée dŽélivrŽée
!
!$Auteur
!       J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  masse = MSP_consommation_continue(isp, duree, poussee)
!.    real(KIND=PM_REEL) :: isp, duree, poussee
!
!$Arguments
!>E     isp      :<PM_REEL>   valeur de l'ISP du systŽème propulsif (en s)
!>E     duree    :<PM_REEL>   DurŽée de la poussŽée (en s)
!>E     poussee  :<PM_REEL>   valeur de la poussŽée (en N)
!>S     masse    :<PM_REEL>   masse d'ergols consommée
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH POUSSEE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(KIND=PM_REEL), intent(IN) :: isp, duree, poussee

    masse = (duree*poussee)/(MSP_ENUM_G0*isp)

  end FUNCTION MSP_consommation_continue




  SUBROUTINE MSP_pvit_absolu_vers_relatif(pos_ci_cart, vit_ci_cart, &
       pos_ch_cart, vit_ch_cart, posrel, vitrel, repere, origine)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_pvit_absolu_vers_relatif
!
!$Resume
!  Calcul du vecteur position-vitesse relatif à partir de l'absolu.
!
!$Description
!            Cette routine calcule les vecturs positions et vitesses
!            d'un vecteur appelé chasseur dans un repère choisi par
!            l'utilisateur mais lié au véhicule cible.
!
!$Auteur
!       J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_pvit_absolu_vers_relatif(pos_ci_cart, vit_ci_cart, &
!.           pos_ch_cart, vit_ch_cart, posrel, vitrel, [repere], [origine])
!.    real(KIND=PM_REEL), dimension(3) :: pos_ci_cart, vit_ci_cart
!.    real(KIND=PM_REEL), dimension(3) :: pos_ch_cart, vit_ch_cart
!.    real(KIND=PM_REEL), dimension(3) :: posrel, vitrel
!.    integer :: repere, origine
!
!$Arguments
!>E     pos_ci_cart  :<PM_REEL,DIM=(3)>   position de la cible en coordonnées cartésiennes absolue
!>E     vit_ci_cart  :<PM_REEL,DIM=(3)>   vitesse de la cible dans le même repère 
!>E     pos_ch_cart  :<PM_REEL,DIM=(3)>   position du chasseur en coordonnées cartésiennes absolue
!>E     vit_ch_cart  :<PM_REEL,DIM=(3)>   vitesse du chasseur dans le même repère 
!>S     posrel       :<PM_REEL,DIM=(3)>   position relative du chasseur par rapport à la cible
!>S     vitrel       :<PM_REEL,DIM=(3)>   vitesse relative du chasseur par rapport à la cible
!>[E]   repere       :<integer>           repère dans lequel on souhaite exprimer cette position-vitesse relative
!>[E]   origine      :<integer>           
!
!$Common
!
!$Routines
!- mo_geo_qsw
!- MSP_signaler_message
!- mo_qsw_geo
!- mo_geo_tnw
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH TRANSFORMATION
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(KIND=PM_REEL), intent(IN), dimension(3) :: pos_ci_cart, vit_ci_cart
    real(KIND=PM_REEL), intent(IN), dimension(3) :: pos_ch_cart, vit_ch_cart
    real(KIND=PM_REEL), intent(OUT), dimension(3) :: posrel, vitrel

    integer, intent(IN), optional :: repere, origine

    integer ::  origine_loc
    real(KIND=PM_REEL) :: rcible
    real(KIND=PM_REEL), dimension(3) :: delta_pos, delta_posQSW, delta_vitQSW
    real(KIND=PM_REEL), dimension(3) :: vitciQSW, vitchQSW
    real(KIND=PM_REEL), dimension(3) :: posg50, vitg50
    real(KIND=PM_REEL), dimension(3) :: pos_tmp, vit_tmp
    type(tm_code_retour) :: code_erreur

    if (PRESENT(origine)) then 
       origine_loc = origine
    else
       origine_loc = MSP_ENUM_CIBLE  ! Origine Cible
    end if

    rcible = .NORM.pos_ci_cart

    ! >>>>>>> POSITION RELATIVE DU CHASSEUR DANS QSW Cible 

    ! Transformation : POS REL dans gamma50 => POS REL dans QSW
    delta_pos(:) = pos_ch_cart(:) - pos_ci_cart(:)
    call mo_geo_qsw(pos_ci_cart, vit_ci_cart, delta_pos(:), delta_posQSW(:), code_erreur)
    call MSP_signaler_message (ier_mslib=code_erreur)
    if (MSP_gen_messages( )) return


    ! >>>>>>> VITESSE RELATIVE DU CHASSEUR DANS QSW Cible 

    ! Transformation : VIT ABS Cible dans gamma50 => VIT ABS Cible dans QSW cible
    call mo_geo_qsw(pos_ci_cart, vit_ci_cart, vit_ci_cart(:), vitciQSW, code_erreur)
    call MSP_signaler_message (ier_mslib=code_erreur)
    if (MSP_gen_messages( )) return

    ! Transformation : VIT ABS Chasseur dans gamma50 => VIT ABS Chasseur dans QSW Cible
    call mo_geo_qsw(pos_ci_cart, vit_ci_cart, vit_ch_cart(:), vitchQSW(:), code_erreur)
    call MSP_signaler_message (ier_mslib=code_erreur)
    if (MSP_gen_messages( )) return

    ! composition des vitesses : VIT ABS dans QSW cible => VIT REL dans QSW cible
    delta_vitQSW(1) = vitchQSW(1) - vitciQSW(1) + vitciQSW(2)*delta_posQSW(2)/rcible
    delta_vitQSW(2) = vitchQSW(2) - vitciQSW(2) - vitciQSW(2)*delta_posQSW(1)/rcible
    delta_vitQSW(3) = vitchQSW(3) - vitciQSW(3)


    ! >>>>>>> TRAITEMENT DU REPERE DE SORTIE

    ! Passage des positions et vitesses relatives dans Gamma50
    call mo_qsw_geo(pos_ci_cart, vit_ci_cart, delta_posQSW(:), posg50(:), code_erreur)
    call MSP_signaler_message (ier_mslib=code_erreur)
    if (MSP_gen_messages( )) return
    
    call mo_qsw_geo(pos_ci_cart, vit_ci_cart, delta_vitQSW(:), vitg50(:), code_erreur)
    call MSP_signaler_message (ier_mslib=code_erreur)
    if (MSP_gen_messages( )) return

    if (PRESENT(repere)) then 

       SELECT CASE(repere)

       CASE(MSP_ENUM_REPERE_QSW)

          SELECT CASE(origine_loc)

          CASE(MSP_ENUM_CIBLE)

             posrel(:) = delta_posQSW(:)
             vitrel(:) = delta_vitQSW(:)

          CASE(MSP_ENUM_CHASSEUR)

             call mo_geo_qsw(pos_ch_cart, vit_ch_cart, posg50(:), posrel(:), code_erreur)
             call MSP_signaler_message (ier_mslib=code_erreur)
             if (MSP_gen_messages( )) return

             call mo_geo_qsw(pos_ch_cart, vit_ch_cart, vitg50(:), vitrel(:), code_erreur)
             call MSP_signaler_message (ier_mslib=code_erreur)
             if (MSP_gen_messages( )) return

          end SELECT

       CASE(MSP_ENUM_REPERE_TNW)

          SELECT CASE(origine_loc)

          CASE(MSP_ENUM_CIBLE)

             call mo_geo_tnw(pos_ci_cart, vit_ci_cart, posg50(:), posrel(:), code_erreur)
             call MSP_signaler_message (ier_mslib=code_erreur)
             if (MSP_gen_messages( )) return

             call mo_geo_tnw(pos_ci_cart, vit_ci_cart, vitg50(:), vitrel(:), code_erreur)
             call MSP_signaler_message (ier_mslib=code_erreur)
             if (MSP_gen_messages( )) return

          CASE(MSP_ENUM_CHASSEUR)

             call mo_geo_tnw(pos_ch_cart, vit_ch_cart, posg50(:), posrel(:), code_erreur)
             call MSP_signaler_message (ier_mslib=code_erreur)
             if (MSP_gen_messages( )) return

             call mo_geo_tnw(pos_ch_cart, vit_ch_cart, vitg50(:), vitrel(:), code_erreur)
             call MSP_signaler_message (ier_mslib=code_erreur)
             if (MSP_gen_messages( )) return

          end SELECT         

       CASE(MSP_ENUM_REPERE_CW)

          SELECT CASE(origine_loc)

          CASE(MSP_ENUM_CIBLE)

             posrel(:) = delta_posQSW(:)
             vitrel(:) = delta_vitQSW(:)

          CASE(MSP_ENUM_CHASSEUR)

             call mo_geo_qsw(pos_ch_cart, vit_ch_cart, posg50(:), posrel(:), code_erreur)
             call MSP_signaler_message (ier_mslib=code_erreur)
             if (MSP_gen_messages( )) return

             call mo_geo_qsw(pos_ch_cart, vit_ch_cart, vitg50(:), vitrel(:), code_erreur)
             call MSP_signaler_message (ier_mslib=code_erreur)
             if (MSP_gen_messages( )) return

          end SELECT

          ! coordonnees QSW --> coordonnees CW
          pos_tmp(:) = posrel(:)
          vit_tmp(:) = vitrel(:)
          posrel(:) = MSP_QSW_to_CW(pos_tmp(:))
          vitrel(:) = MSP_QSW_to_CW(vit_tmp(:))

       END SELECT

    else
       
       ! coordonnees QSW --> coordonnees CW
       posrel(:) = MSP_QSW_to_CW(delta_posQSW(:))
       vitrel(:) = MSP_QSW_to_CW(delta_vitQSW(:))

    end if
       
  end SUBROUTINE MSP_pvit_absolu_vers_relatif




  SUBROUTINE MSP_pvit_relatif_vers_absolu(posrel, vitrel, pos_ci_cart, vit_ci_cart, &
       pos_ch_cart, vit_ch_cart, repere, bull_origine)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_pvit_relatif_vers_absolu
!
!$Resume
!  Calcul du vecteur position-vitesse absolu à partir du vecteur relatif.
!
!$Description
!            Cette routine calcule les vecturs positions et vitesses 
!            d'un vecteur appelé chasseur dans un repère choisi par 
!            l'utilisateur mais lié au véhicule cible 
!
!$Auteur
!       J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_pvit_relatif_vers_absolu(posrel, vitrel, pos_ci_cart, vit_ci_cart, &
!.           pos_ch_cart, vit_ch_cart, [repere], [bull_origine])
!.    real(KIND=PM_REEL), dimension(:) :: pos_ci_cart, vit_ci_cart
!.    real(KIND=PM_REEL), dimension(:) :: posrel, vitrel
!.    real(KIND=PM_REEL), dimension(:) :: bull_origine
!.    integer :: repere
!.    real(KIND=PM_REEL), dimension(:) :: pos_ch_cart, vit_ch_cart
!
!$Arguments
!>E     posrel        :<PM_REEL,DIM=(:)>   Position relative du chasseur par rapport à la cible
!>E     vitrel        :<PM_REEL,DIM=(:)>   Vitesse relative du chasseur par rapport à la cible
!>E     pos_ci_cart   :<PM_REEL,DIM=(:)>   Position cartésienne absolue de la cible 
!>E     vit_ci_cart   :<PM_REEL,DIM=(:)>   Vitesse cartésienne absolue de la cible
!>S     pos_ch_cart   :<PM_REEL,DIM=(:)>   Position cartésienne absolue du chasseur
!>S     vit_ch_cart   :<PM_REEL,DIM=(:)>   vitesse cartésienne absolue du chasseur
!>[E]   repere        :<integer>           repère dans lequel on exprime la position relative d'entrée
!>[E]   bull_origine  :<PM_REEL,DIM=(:)>   
!
!$Common
!
!$Routines
!- mo_qsw_geo
!- MSP_signaler_message
!- mo_geo_qsw
!- mo_tnw_geo
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH TRANSFORMATION
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(KIND=PM_REEL), intent(IN), dimension(:) :: pos_ci_cart, vit_ci_cart
    real(KIND=PM_REEL), intent(IN), dimension(:) :: posrel, vitrel

    real(KIND=PM_REEL), intent(IN), dimension(:), optional :: bull_origine
    integer, intent(IN), optional :: repere

    real(KIND=PM_REEL), intent(OUT), dimension(:) :: pos_ch_cart, vit_ch_cart

    real(KIND=PM_REEL) :: rcible
    real(KIND=PM_REEL), dimension(3) :: delta_pos, delta_posQSW, delta_vitQSW
    real(KIND=PM_REEL), dimension(3) :: posg50, vitg50
    real(KIND=PM_REEL), dimension(3) :: pos_tmp, vit_tmp
    real(KIND=PM_REEL), dimension(3) :: vitciQSW, vitchQSW
    real(KIND=PM_REEL), dimension(6) :: bull_orig

    type(tm_code_retour) :: code_erreur


    ! >>>>>>> TRAITEMENT DE L'ORIGINE DU REPERE D'ENTREE

    if (PRESENT(bull_origine)) then 
       bull_orig(:) = bull_origine(:)
    else
       bull_orig(:) = (/ pos_ci_cart(:), vit_ci_cart(:) /)
    end if
    rcible = .NORM.pos_ci_cart(:)
    if (MSP_gen_messages("MSP_pvit_absolu_vers_relatif")) return


    ! >>>>>>> TRAITEMENT DU REPERE D'ENTREE

    if (PRESENT(repere)) then 

       SELECT CASE(repere)

       CASE(MSP_ENUM_REPERE_QSW, MSP_ENUM_REPERE_CW)

          ! Positions et vitesses relatives dans QSW origine vers Gamma50
          call mo_qsw_geo(bull_orig(1:3), bull_orig(4:6), posrel(:), posg50(:), code_erreur)
          call MSP_signaler_message (ier_mslib=code_erreur)
          if (MSP_gen_messages( )) return

          call mo_qsw_geo(bull_orig(1:3), bull_orig(4:6), vitrel(:), vitg50(:), code_erreur)
          call MSP_signaler_message (ier_mslib=code_erreur)
          if (MSP_gen_messages( )) return
          
          ! Positions et vitesses relatives de Gamma50 vers QSW cible
          call mo_geo_qsw(pos_ci_cart, vit_ci_cart, posg50(:), delta_posQSW(:), code_erreur)
          call MSP_signaler_message (ier_mslib=code_erreur)
          if (MSP_gen_messages( )) return

          call mo_geo_qsw(pos_ci_cart, vit_ci_cart, vitg50(:), delta_vitQSW(:), code_erreur)
          call MSP_signaler_message (ier_mslib=code_erreur)
          if (MSP_gen_messages( )) return

          if (repere == MSP_ENUM_REPERE_CW) then

             ! coordonnees CW cible --> coordonnees QSW cible
             pos_tmp(:) = delta_posQSW(:)
             vit_tmp(:) = delta_vitQSW(:)
             delta_posQSW(:) = MSP_CW_to_QSW(pos_tmp(:))
             delta_vitQSW(:) = MSP_CW_to_QSW(vit_tmp(:))

          end if

       CASE(MSP_ENUM_REPERE_TNW)

          ! Positions et vitesses relatives dans TNW origine vers Gamma50
          call mo_tnw_geo(bull_orig(1:3), bull_orig(4:6), posrel(:), posg50(:), code_erreur)
          call MSP_signaler_message (ier_mslib=code_erreur)
          if (MSP_gen_messages( )) return

          call mo_tnw_geo(bull_orig(1:3), bull_orig(4:6), vitrel(:), vitg50(:), code_erreur)
          call MSP_signaler_message (ier_mslib=code_erreur)
          if (MSP_gen_messages( )) return
          
          ! Positions et vitesses relatives de Gamma50 vers QSW cible
          call mo_geo_qsw(pos_ci_cart, vit_ci_cart, posg50(:), delta_posQSW(:), code_erreur)
          call MSP_signaler_message (ier_mslib=code_erreur)
          if (MSP_gen_messages( )) return

          call mo_geo_qsw(pos_ci_cart, vit_ci_cart, vitg50(:), delta_vitQSW(:), code_erreur)
          call MSP_signaler_message (ier_mslib=code_erreur)
          if (MSP_gen_messages( )) return
          

       END SELECT

    else
       
       ! Positions et vitesses relatives dans QSW origine vers Gamma50
       call mo_qsw_geo(bull_orig(1:3), bull_orig(4:6), posrel(:), posg50(:), code_erreur)
       call MSP_signaler_message (ier_mslib=code_erreur)
       if (MSP_gen_messages( )) return
       
       call mo_qsw_geo(bull_orig(1:3), bull_orig(4:6), vitrel(:), vitg50(:), code_erreur)
       call MSP_signaler_message (ier_mslib=code_erreur)
       if (MSP_gen_messages( )) return
       
       ! Positions et vitesses relatives de Gamma50 vers QSW cible
       call mo_geo_qsw(pos_ci_cart, vit_ci_cart, posg50(:), delta_posQSW(:), code_erreur)
       call MSP_signaler_message (ier_mslib=code_erreur)
       if (MSP_gen_messages( )) return
       
       call mo_geo_qsw(pos_ci_cart, vit_ci_cart, vitg50(:), delta_vitQSW(:), code_erreur)
       call MSP_signaler_message (ier_mslib=code_erreur)
       if (MSP_gen_messages( )) return
       
       ! coordonnees CW cible --> coordonnees QSW cible
       pos_tmp(:) = delta_posQSW(:)
       vit_tmp(:) = delta_vitQSW(:)
       delta_posQSW(:) = MSP_CW_to_QSW(posrel(:))
       delta_vitQSW(:) = MSP_CW_to_QSW(vitrel(:))

    end if
     

    ! >>>>>>> POSITION DU CHASSEUR ABSOLUE DANS GAMMA50 

    ! Transformation :  POS REL dans QSW cible => POS ABS dans gamma50
    call mo_qsw_geo(pos_ci_cart, vit_ci_cart, delta_posQSW, delta_pos(:), code_erreur)
    call MSP_signaler_message (ier_mslib=code_erreur)
    if (MSP_gen_messages( )) return

    pos_ch_cart(:) = pos_ci_cart(:) + delta_pos(:)



    ! >>>>>>> VITESSE DU CHASSEUR ABSOLUE DANS GAMMA50 

    ! Transformation : VIT ABS Cible dans gamma50 => VIT ABS Cible dans QSW cible
    call mo_geo_qsw(pos_ci_cart, vit_ci_cart, vit_ci_cart, vitciQSW(:), code_erreur)
    call MSP_signaler_message (ier_mslib=code_erreur)
    if (MSP_gen_messages( )) return

    ! composition des vitesses : VIT REL dans QSW cible => VIT ABS dans QSW cible
    vitchQSW(1) = delta_vitQSW(1) + vitciQSW(1) - vitciQSW(2)*delta_posQSW(2)/rcible
    vitchQSW(2) = delta_vitQSW(2) + vitciQSW(2) + vitciQSW(2)*delta_posQSW(1)/rcible
    vitchQSW(3) = delta_vitQSW(3) + vitciQSW(3)

    ! Transformation : VIT ABS Chasseur dans QSW cible => VIT ABS Chasseur dans Gamma50
    call mo_qsw_geo(pos_ci_cart, vit_ci_cart, vitchQSW, vit_ch_cart(:), code_erreur)
    call MSP_signaler_message (ier_mslib=code_erreur)
    if (MSP_gen_messages( )) return


  end SUBROUTINE MSP_pvit_relatif_vers_absolu


  SUBROUTINE MSP_jj_to_jcd(jjul, ian, imois, ijour, iheure, imin, isec, &
       imils, ier, jjsec, origdat)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_jj_to_jcd
!
!$Resume
!  Calcul de la date calendaire à partir de la date Julienne.
!
!$Description
!            Cette routine calcule une date calendaire à partir d'une 
!            date exprimée en jour Julien CNES. La date calendaire est 
!            de la forme AAAA/MM/JJ HH/MM/SS.ddd
!
!$Auteur
!       J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_jj_to_jcd(jjul, ian, imois, ijour, iheure, imin, isec, &
!.           imils, ier, [jjsec], [origdat])
!.    real(KIND=PM_REEL) :: jjul
!.    integer :: ian, imois, ijour, iheure, imin, isec, imils, ier
!.    type(tm_jour_sec) :: jjsec
!.    integer :: origdat
!
!$Arguments
!>E     jjul     :<PM_REEL>       date en jour julien CNES
!>S     ian      :<integer>       Année
!>S     imois    :<integer>       Mois
!>S     ijour    :<integer>       jour
!>S     iheure   :<integer>       heure (entre 0 et 23)
!>S     imin     :<integer>       minutes (entre 0 et 59)
!>S     isec     :<integer>       secondes (entre 0 et 59)
!>S     imils    :<integer>       millisecondes (entre 0 et 999)
!>S     ier      :<integer>       code d'erreur
!>[E]   jjsec    :<tm_jour_sec>   date en jour/secondes
!>[E]   origdat  :<integer>       
!
!$Common
!
!$Routines
!- md_jourfrac_joursec
!- MSP_signaler_message
!- md_julien_calend
!- MSP_dureejj_to_jcd
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH DATE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(KIND=PM_REEL), intent(IN) :: jjul
    integer, intent(OUT) :: ian, imois, ijour, iheure, imin, isec, imils, ier
    type(tm_jour_sec), intent(IN), optional :: jjsec
    integer, intent(IN), optional :: origdat

    integer :: jj, origdat_tmp
    real(KIND=PM_REEL) ::  sec, duree
    type(tm_code_retour) :: code_erreur
    type(tm_jour_sec) :: jul1950

    ! Initialisations
    ian=0
    imois=0
    ijour=0
    iheure=0
    imin=0
    isec=0
    imils=0
    ier=0
    origdat_tmp=0
    if (present(origdat)) origdat_tmp=origdat

! Recherche de la date calendaire du jour julien a 0h00
    if (present(jjsec)) then
       jul1950 = jjsec
    else
       call md_jourfrac_joursec(jjul, jul1950, code_erreur)
       call MSP_signaler_message (ier_mslib=code_erreur)
       if (MSP_gen_messages("MSP_jj_to_jcd")) return
    endif

! origine des dates a J2000 au lieu de 1950
    if (origdat_tmp == 1) then
       jul1950%jour = jul1950%jour + MSP_2000jour
       jul1950%sec = jul1950%sec + MSP_2000sec
    endif

    call md_julien_calend(jul1950, ian, imois, ijour, iheure, imin, sec, code_erreur)
    call MSP_signaler_message (ier_mslib=code_erreur)
    if (MSP_gen_messages("MSP_jj_to_jcd")) return

    duree = jjul - real(jul1950%jour, KIND=PM_REEL)
    call MSP_dureejj_to_jcd(duree, jj, iheure, imin, isec, imils, ier)
    if (MSP_gen_messages("MSP_jj_to_jcd" )) return

  end SUBROUTINE MSP_jj_to_jcd




  SUBROUTINE MSP_dureejj_to_jcd(duree, ijour, iheure, imin, isec, imils, ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_dureejj_to_jcd
!
!$Resume
!  Calcul de la durée calendaire à partir de la durée en secondes.
!
!$Description
!            Cette routine transforme une durée exprimée en jours décimaux,
!            en une durée calculée en jour calendaire (JJ HH/MM/SS.ddd)
!
!$Auteur
!       J.-J. WASBAUER
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_dureejj_to_jcd(duree, ijour, iheure, imin, isec, imils, ier)
!.    real(KIND=PM_REEL) :: duree
!.    integer :: ijour, iheure, imin, isec, imils, ier
!
!$Arguments
!>E     duree   :<PM_REEL>   durée en jour julien
!>S     ijour   :<integer>   nombre de jours
!>S     iheure  :<integer>   nombre d'heure
!>S     imin    :<integer>   nombre de minutes
!>S     isec    :<integer>   nombre de secondes
!>S     imils   :<integer>   nombre de millisecondes
!>S     ier     :<integer>   code d'erreur
!
!$Common
!
!$Routines
!- md_duree_jhms
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!  MATH DATE
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(KIND=PM_REEL), intent(IN) :: duree
    integer, intent(OUT) :: ijour, iheure, imin, isec, imils, ier

    real(KIND=PM_REEL) ::  duree_tmp, rsec, rmils
    type(tm_code_retour) :: code_erreur

    ! Transforme la duree en jour, heure, minutes, secondes et millisecondes la plus proche
    ier = 0
    rsec = (duree - real(int(duree),kind=pm_reel))*86400._PM_REEL
    rmils = (rsec - real(int(rsec),kind=pm_reel))*1000._PM_REEL
    imils = nint(rmils)
    if (imils == 1000._PM_REEL) then 
       rsec = rsec + 1._PM_REEL
       imils = 0
    endif
    duree_tmp = real(int(rsec),kind=pm_reel)+ imils/1000._PM_REEL
    call md_duree_jhms(duree_tmp, ijour, iheure, imin, rsec, code_erreur)
    isec = int(rsec)
    call MSP_signaler_message (ier_mslib=code_erreur)
    if (MSP_gen_messages( )) return

  end SUBROUTINE MSP_dureejj_to_jcd


  real(kind=pm_reel) recursive function MSP_factorielle (N) result (fact)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_factorielle
!
!$Resume
!
!$Description
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  fact = MSP_factorielle (N)
!.    integer :: N
!
!$Arguments
!>E     N     :<integer>   
!>S     fact  :<pm_reel>   
!
!$Common
!
!$Routines
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

    integer, intent(IN)  :: N
    if (N <=1) then
       fact = 1
    else
       fact = N *  MSP_factorielle(N-1)
    endif

  end function MSP_factorielle
 
END MODULE MSP_MATH
