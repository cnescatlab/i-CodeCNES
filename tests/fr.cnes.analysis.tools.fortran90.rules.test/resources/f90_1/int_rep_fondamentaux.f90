module int_rep_fondamentaux

! (C) Copyright CNES - MSLIB - 2007
!************************************************************************
!
! But:  Definition des constantes et interface des fonctions du thème R
! ===
!
! Note d'utilisation:
! ==================
!   Module en principe utilisé uniquement par l'intermédiaire du module global
!   "mslib90"
!
!$Historique
! ==========
!   
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!   + Version 6.6 : DM-ID 616 (option) : inclusion des constantes des modules
!     code_transfo_mslib, code_modeles_mslib, code_planetes_mslib
!     en vue de la suppression de ces modules
!                   (Date: 05/2007 - Realisation: Atos origin)
!
!   + Version 6.9 : DM-ID 1092 (option) : Ajout des nouvelles routines de
!                   changement de repères IERS 2003
!                   (Date: 09/2008 - Realisation: Atos origin)
!
!   + Version 6.10 : FA-ID 1217 : Suppression du parametre optionnel "inertiel"
!                        dans les routines de changement de repere IERS 2003 
!                        TerRef->EME2000 et EME2000->TerRef
!                   (Date: 02/2009 - Realisation: Atos origin)
!
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/02/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************

use longueur_chaine_mslib
implicit none

! SVN Source File Id
  character(len=256), private, parameter :: SVN_VER =  '$Id: int_rep_fondamentaux.f90 362 2013-02-15 18:01:28Z bbjc $'


! definition des indicateurs de passage d'un repere a un autre

! Valeurs associees aux indicateurs de changement de repere:
! =========================================================

! Convention adoptee : equatorial moyen -> 1
!                      equatorial vrai  -> 2
!                      ecliptique moyen -> 3
!                      ecliptique vrai  -> 4

integer, parameter :: pm_equa_moy_equa_moy = 11  
integer, parameter :: pm_equa_moy_equa_vrai = 12  
integer, parameter :: pm_equa_moy_ecli_moy = 13  
integer, parameter :: pm_equa_moy_ecli_vrai = 14  

integer, parameter :: pm_equa_vrai_equa_moy = 21  
integer, parameter :: pm_equa_vrai_equa_vrai = 22  
integer, parameter :: pm_equa_vrai_ecli_moy = 23 
integer, parameter :: pm_equa_vrai_ecli_vrai = 24  

integer, parameter :: pm_ecli_moy_equa_moy = 31  
integer, parameter :: pm_ecli_moy_equa_vrai = 32  
integer, parameter :: pm_ecli_moy_ecli_moy = 33 
integer, parameter :: pm_ecli_moy_ecli_vrai = 34  

integer, parameter :: pm_ecli_vrai_equa_moy = 41  
integer, parameter :: pm_ecli_vrai_equa_vrai = 42  
integer, parameter :: pm_ecli_vrai_ecli_moy = 43  
integer, parameter :: pm_ecli_vrai_ecli_vrai = 44  

! Parametres representant une rotation (a partir des 3 rotations elementaires):
! ============================================================================

! Convention adoptee : rotation autour de X -> 1
!                      rotation autour de Y -> 2
!                      rotation autour de Z -> 3

! angles d'Euler
integer, parameter :: pm_1x_2y_3x = 121
integer, parameter :: pm_1x_2z_3x = 131 
integer, parameter :: pm_1y_2x_3y = 212
integer, parameter :: pm_1y_2z_3y = 232
integer, parameter :: pm_1z_2x_3z = 313
integer, parameter :: pm_1z_2y_3z = 323

! angles de Cardan
integer, parameter :: pm_1x_2y_3z = 123
integer, parameter :: pm_1x_2z_3y = 132
integer, parameter :: pm_1y_2x_3z = 213
integer, parameter :: pm_1y_2z_3x = 231
integer, parameter :: pm_1z_2x_3y = 312
integer, parameter :: pm_1z_2y_3x = 321


! definition des modeles de la MSLIB

! Valeurs associees aux indicateurs de modeles:
! ============================================

! modeles de precession et nutation

integer, parameter :: pm_lieske_wahr = 17  ! modele en nutation et precession Wahr+Lieske 
integer, parameter :: pm_lieske      = 18  ! indicateur du modele de precession de Lieske
integer, parameter :: pm_wahr        = 19  ! indicateur du modele de nutation de Wahr

! modeles decrivant les axes de rotation des astres

integer, parameter :: pm_nb_modelesUAI = 2
integer, parameter :: pm_UAI_modeles_min = 20
integer, parameter :: pm_UAI_autre_modele = pm_UAI_modeles_min
integer, parameter :: pm_UAI1994 = pm_UAI_autre_modele + 1
integer, parameter :: pm_UAI2000 = pm_UAI1994 + 1
integer, parameter :: pm_UAI_modeles_max = pm_UAI2000


!  use code_planetes_mslib       ! definition des clefs definissant les planetes

! Parametres representant les planetes (nomenclature NASA - NAIF/SPICE)
! =====================================================================

integer, parameter :: pm_pla_mercure = 199
integer, parameter :: pm_pla_venus   = 299
integer, parameter :: pm_pla_terre   = 399
integer, parameter :: pm_pla_mars    = 499
integer, parameter :: pm_pla_jupiter = 599
integer, parameter :: pm_pla_saturne = 699
integer, parameter :: pm_pla_uranus  = 799
integer, parameter :: pm_pla_neptune = 899
integer, parameter :: pm_pla_pluton  = 999


public
interface
     subroutine mr_EcliJ2000_J2000 ( pos_EcliJ2000, pos_J2000, code_retour,  &
                                  obliquite, vit_EcliJ2000, vit_J2000, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


real(pm_reel), dimension(3),   intent(in)            ::  pos_EcliJ2000 ! position dans le repere ecliptique J2000
real(pm_reel), dimension(3),   intent(out)           ::  pos_J2000     ! position dans le repere EME2000
type(tm_code_retour),          intent(out)           ::  code_retour
real(pm_reel),                 intent(in) , optional ::  obliquite     ! obliquite (rad)
real(pm_reel), dimension(3),   intent(in) , optional ::  vit_EcliJ2000 ! vitesse dans le repere ecliptique J2000
real(pm_reel), dimension(3),   intent(out), optional ::  vit_J2000     ! vitesse dans le repere EME2000
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob         ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_EcliJ2000_J2000
     subroutine mr_EquaMoy_EquaVrai ( model, jul1950, delta_tai, pos_EquaMoy, pos_EquaVrai, code_retour,  &
                                       inertiel, vit_EquaMoy, vit_EquaVrai, jacob )

       use type_mslib
       use precision_mslib
       implicit none



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,                            intent(in)            :: model        ! indicateur des modeles de precession et de nutation
type(tm_jour_sec),                  intent(in)            :: jul1950      ! date julienne 1950 t (non normalisee) en jour et secondes
real(pm_reel),                      intent(in)            :: delta_tai    ! ecart entre l'echelle de temps TAI et l'echelle de temps utilisee pour la date t
real(pm_reel),        dimension(3), intent(in)            :: pos_EquaMoy  ! vecteur position dans le repere equatorial moyen a la date t
real(pm_reel),        dimension(3), intent(out)           :: pos_EquaVrai ! vecteur position dans le repere equatorial vrai a la date t
type(tm_code_retour),               intent(out)           :: code_retour
logical,                            intent(in),  optional :: inertiel     ! indicateur a vrai si calcul inertiel (sans vitesses d'entrainement)
real(pm_reel),        dimension(3), intent(in),  optional :: vit_EquaMoy  ! vecteur vitesse dans le repere equatorial moyen a la date t 
real(pm_reel),        dimension(3), intent(out), optional :: vit_EquaVrai ! vecteur vitesse dans le repere equatorial vrai a la date t
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob        ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_EquaMoy_EquaVrai
     subroutine mr_EquaMoy_J2000 ( model, jul1950, delta_tai, pos_EquaMoy, pos_J2000, code_retour, &
          inertiel, vit_EquaMoy, vit_J2000, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


integer,                            intent(in)            :: model       ! indicateur des modeles de precession et de nutation
type(tm_jour_sec),                  intent(in)            :: jul1950     ! date julienne 1950 t (non normalisee) en jour et secondes
real(pm_reel),                      intent(in)            :: delta_tai   ! ecart entre l'echelle de temps TAI et l'echelle de temps utilisee pour la date t
real(pm_reel),        dimension(3), intent(in)            :: pos_EquaMoy ! vecteur position dans le repere equatorial moyen
real(pm_reel),        dimension(3), intent(out)           :: pos_J2000   ! vecteur position dans le repere equatorial moyen J2000
type(tm_code_retour),               intent(out)           :: code_retour
logical,                            intent(in),  optional :: inertiel    ! indicateur a vrai si calcul inertiel (sans vitesses d'entrainement)
real(pm_reel),        dimension(3), intent(in),  optional :: vit_EquaMoy ! vecteur vitesse dans le repere equatorial moyen
real(pm_reel),        dimension(3), intent(out), optional :: vit_J2000   ! vecteur vitesse dans le repere equatorial moyen J2000
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob       ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_EquaMoy_J2000
     subroutine mr_EquaUAI_J2000 ( planete, modeleUAI, jul1950, pos_EquaUAI, pos_J2000, code_retour, &
          asc_droite, declinaison, vit_EquaUAI, vit_J2000, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


integer,                       intent(in)            ::  planete       ! planete
integer,                       intent(in)            ::  modeleUAI     ! modele UAI definissant le pole de rotation de l'astre
type(tm_jour_sec),             intent(in)            ::  jul1950       ! date julienne 1950 en jours secondes
real(pm_reel), dimension(3),   intent(in)            ::  pos_EquaUAI   ! position dans le repere equatorial planetaire de l'astre
real(pm_reel), dimension(3),   intent(out)           ::  pos_J2000     ! position dans le repere EME2000
type(tm_code_retour),          intent(out)           ::  code_retour
real(pm_reel),                 intent(in),  optional ::  asc_droite    ! ascension droite (alpha0) du pole
real(pm_reel),                 intent(in),  optional ::  declinaison   ! declinaison (delta0) du pole
real(pm_reel), dimension(3),   intent(in),  optional ::  vit_EquaUAI   ! vitesse dans le repere equatorial planetaire de l'astre
real(pm_reel), dimension(3),   intent(out), optional ::  vit_J2000     ! vitesse dans le repere EME2000
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob         ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_EquaUAI_J2000
     subroutine mr_EquaUAI_PlanetVrai ( planete, modeleUAI, jul1950, pos_EquaUAI, pos_PlanetVrai, code_retour, &
          tsid, deriv_tsid, vit_EquaUAI, vit_PlanetVrai, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


integer,                       intent(in)            ::  planete          ! planete
integer,                       intent(in)            ::  modeleUAI        ! modele UAI definissant le meridien origine
type(tm_jour_sec),             intent(in)            ::  jul1950          ! date julienne 1950 en jours secondes
real(pm_reel), dimension(3),   intent(in)            ::  pos_EquaUAI      ! position dans le repere equatorial planetaire de l'astre
real(pm_reel), dimension(3),   intent(out)           ::  pos_PlanetVrai   ! position dans le repere planetocentrique
type(tm_code_retour),          intent(out)           ::  code_retour
real(pm_reel),                 intent(in),  optional ::  tsid             ! position du meridien origine
real(pm_reel),                 intent(in),  optional ::  deriv_tsid       ! derivee du meridien origine = vitesse de rotation
real(pm_reel), dimension(3),   intent(in),  optional ::  vit_EquaUAI      ! vitesse dans le repere equatorial planetaire de l'astre
real(pm_reel), dimension(3),   intent(out), optional ::  vit_PlanetVrai   ! position dans le repere planetocentrique
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob            ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_EquaUAI_PlanetVrai
     subroutine mr_EquaVrai_EquaMoy ( model, jul1950, delta_tai, pos_EquaVrai, pos_EquaMoy, code_retour, &
                                  inertiel, vit_EquaVrai, vit_EquaMoy, jacob )

       use type_mslib
       use precision_mslib
       implicit none



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,                            intent(in)            :: model        ! indicateur des modeles de precession et de nutation
type(tm_jour_sec),                  intent(in)            :: jul1950      ! date julienne 1950 t (non normalisee) en jour et secondes
real(pm_reel),                      intent(in)            :: delta_tai    ! ecart entre l'echelle de temps TAI et l'echelle de temps utilisee pour la date t
real(pm_reel),        dimension(3), intent(in)            :: pos_EquaVrai ! vecteur position dans le repere equatorial vrai a la date t
real(pm_reel),        dimension(3), intent(out)           :: pos_EquaMoy  ! vecteur position dans le repere equatorial moyen a la date t 
type(tm_code_retour),               intent(out)           :: code_retour
logical,                            intent(in),  optional :: inertiel     ! indicateur a vrai si calcul inertiel (sans vitesses d'entrainement)
real(pm_reel),        dimension(3), intent(in),  optional :: vit_EquaVrai ! vecteur vitesse dans le repere equatorial vrai a la date t
real(pm_reel),        dimension(3), intent(out), optional :: vit_EquaMoy  ! vecteur vitesse dans le repere equatorial moyen a la date t 
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob        ! jacobienne de la transformation


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_EquaVrai_EquaMoy
     subroutine mr_EquaVrai_TerVrai ( model, jul1950, delta_tu1, delta_tai, pos_EquaVrai, pos_TerVrai, code_retour, &
                                      inertiel, vit_EquaVrai, vit_TerVrai, jacob )

       use type_mslib
       use precision_mslib
       implicit none



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,                            intent(in)            :: model        ! indicateur des modeles de precession et de nutation
type(tm_jour_sec),                  intent(in)            :: jul1950      ! date julienne 1950 t (non normalisee) en jour et secondes
real(pm_reel),                      intent(in)            :: delta_tu1    ! ecart entre l'echelle de temps TU1 et l'echelle de temps utilisee pour la date t
real(pm_reel),                      intent(in)            :: delta_tai    ! ecart entre l'echelle de temps TAI et l'echelle de temps utilisee pour la date t
real(pm_reel),        dimension(3), intent(in)            :: pos_EquaVrai ! vecteur position dans le repere equatorial vrai a la date t
real(pm_reel),        dimension(3), intent(out)           :: pos_TerVrai  ! vecteur position dans le repere terrestre vrai a la date t 
type(tm_code_retour),               intent(out)           :: code_retour

logical,                            intent(in),  optional :: inertiel     ! indicateur a vrai si calcul inertiel (sans vitesses d'entrainement)
real(pm_reel),        dimension(3), intent(in),  optional :: vit_EquaVrai ! vecteur vitesse dans le repere equatorial vrai a la date t
real(pm_reel),        dimension(3), intent(out), optional :: vit_TerVrai  ! vecteur vitesse dans le repere terrestre vrai a la date t 
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob        ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_EquaVrai_TerVrai
     subroutine mr_EquaVrai_veis ( model, jul1950, delta_tu1, delta_tai, pos_EquaVrai, pos_veis, code_retour, &
                                   vit_EquaVrai, vit_veis, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


integer,                            intent(in)            :: model        ! indicateur des modeles de precession et de nutation
type(tm_jour_sec),                  intent(in)            :: jul1950      ! date julienne 1950 t (non normalisee) en jour et secondes
real(pm_reel),                      intent(in)            :: delta_tu1    ! ecart entre l'echelle de temps TU1 et l'echelle de temps utilisee pour la date t
real(pm_reel),                      intent(in)            :: delta_tai    ! ecart entre l'echelle de temps TAI et l'echelle de temps utilisee pour la date t
real(pm_reel),        dimension(3), intent(in)            :: pos_EquaVrai ! vecteur position dans le repere equatorial vrai a la date t
real(pm_reel),        dimension(3), intent(out)           :: pos_veis     ! vecteur position dans le repere de Veis a la date t 
type(tm_code_retour),               intent(out)           :: code_retour
real(pm_reel),        dimension(3), intent(in),  optional :: vit_EquaVrai ! vecteur vitesse dans le repere equatorial vrai a la date t
real(pm_reel),        dimension(3), intent(out), optional :: vit_veis     ! vecteur vitesse dans le repere de Veis a la date t 
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob        ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_EquaVrai_veis
     subroutine mr_J2000_BBR (pos_J2000, pos_Pla1, vit_Pla1, pos_Pla2, vit_Pla2, pos_BBR, code_retour, vit_J2000, vit_BBR)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3), intent(in)            :: pos_J2000 ! position dans le J2000
real(pm_reel), dimension(3), intent(in)            :: pos_Pla1  ! position de la planete 1 dans le J2000
real(pm_reel), dimension(3), intent(in)            :: vit_Pla1  ! vitesse de la planete 1 dans le J2000
real(pm_reel), dimension(3), intent(in)            :: pos_Pla2  ! position de la planete 2 dans le J2000
real(pm_reel), dimension(3), intent(in)            :: vit_Pla2  ! vitesse de la planete 2 dans le J2000
real(pm_reel), dimension(3), intent(out)           :: pos_BBR   ! position dans le BBR
type(tm_code_retour), intent(out)                  :: code_retour
real(pm_reel), dimension(3), intent(in), optional  :: vit_J2000 ! vitesse dans le J2000
real(pm_reel), dimension(3), intent(out), optional :: vit_BBR   ! vitesse dans le BBR



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_J2000_BBR
     subroutine mr_J2000_EcliJ2000 ( pos_J2000, pos_EcliJ2000, code_retour,  &
                                  obliquite, vit_J2000, vit_EcliJ2000, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3),   intent(in)            ::  pos_J2000     ! position dans le repere EME2000
real(pm_reel), dimension(3),   intent(out)           ::  pos_EcliJ2000 ! position dans le repere ecliptique J2000
type(tm_code_retour),          intent(out)           ::  code_retour
real(pm_reel),                 intent(in) , optional ::  obliquite     ! obliquite (rad)
real(pm_reel), dimension(3),   intent(in) , optional ::  vit_J2000     ! vitesse dans le repere EME2000
real(pm_reel), dimension(3),   intent(out), optional ::  vit_EcliJ2000 ! vitesse dans le repere ecliptique J2000
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob         ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_J2000_EcliJ2000
     subroutine mr_J2000_EquaMoy ( model, jul1950, delta_tai, pos_J2000, pos_EquaMoy, code_retour, &
     inertiel, vit_J2000, vit_EquaMoy, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


integer,                            intent(in)            :: model       ! indicateur des modeles de precession et de nutation
type(tm_jour_sec),                  intent(in)            :: jul1950     ! date julienne 1950 t (non normalisee) en jour et secondes
real(pm_reel),                      intent(in)            :: delta_tai   ! ecart entre l'echelle de temps TAI et l'echelle de temps utilisee pour la date t
real(pm_reel),        dimension(3), intent(in)            :: pos_J2000   ! vecteur position dans le repere equatorial moyen J2000
real(pm_reel),        dimension(3), intent(out)           :: pos_EquaMoy ! vecteur position dans le repere equatorial moyen a la date t
type(tm_code_retour),               intent(out)           :: code_retour
logical,                            intent(in),  optional :: inertiel    ! indicateur a vrai si calcul inertiel (sans vitesses d'entrainement)
real(pm_reel),        dimension(3), intent(in),  optional :: vit_J2000   ! vecteur vitesse dans le repere equatorial moyen J2000
real(pm_reel),        dimension(3), intent(out), optional :: vit_EquaMoy ! vecteur vitesse dans le repere equatorial moyen a la date t 
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob       ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_J2000_EquaMoy
     subroutine mr_J2000_EquaUAI ( planete, modeleUAI, jul1950, pos_J2000, pos_EquaUAI, code_retour, &
          asc_droite, declinaison, vit_J2000, vit_EquaUAI, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


integer,                       intent(in)            ::  planete       ! planete
integer,                       intent(in)            ::  modeleUAI     ! modele UAI definissant le pole de rotation de l'astre
type(tm_jour_sec),             intent(in)            ::  jul1950       ! date julienne 1950 en jours secondes
real(pm_reel), dimension(3),   intent(in)            ::  pos_J2000     ! position dans le repere EME2000
real(pm_reel), dimension(3),   intent(out)           ::  pos_EquaUAI   ! position dans le repere equatorial planetaire de l'astre
type(tm_code_retour),          intent(out)           ::  code_retour
real(pm_reel),                 intent(in),  optional ::  asc_droite    ! ascension droite (alpha0) du pole
real(pm_reel),                 intent(in),  optional ::  declinaison   ! declinaison (delta0) du pole
real(pm_reel), dimension(3),   intent(in),  optional ::  vit_J2000     ! vitesse dans le repere EME2000
real(pm_reel), dimension(3),   intent(out), optional ::  vit_EquaUAI   ! vitesse dans le repere equatorial planetaire de l'astre
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob         ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_J2000_EquaUAI
     subroutine mr_J2000_TerVrai (  model, jul1950, delta_tu1, delta_tai, pos_J2000, pos_TerVrai, code_retour, &
                                  vit_J2000, vit_TerVrai, jacob )

       use type_mslib
       use precision_mslib
       implicit none



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,                            intent(in)            :: model       ! indicateur des modeles de precession et de nutation
type(tm_jour_sec),                  intent(in)            :: jul1950     ! date julienne 1950 t (non normalisee) en jour et secondes
real(pm_reel),                      intent(in)            :: delta_tu1   ! ecart entre l'echelle de temps TU1 et l'echelle de temps utilisee pour la date t
real(pm_reel),                      intent(in)            :: delta_tai   ! ecart entre l'echelle de temps TAI et l'echelle de temps utilisee pour la date t
real(pm_reel),        dimension(3), intent(in)            :: pos_J2000   ! vecteur position dans le repere equatorial moyen J2000
real(pm_reel),        dimension(3), intent(out)           :: pos_TerVrai ! vecteur position dans le repere Terrestre Vrai a la date t 
type(tm_code_retour),               intent(out)           :: code_retour
real(pm_reel),        dimension(3), intent(in),  optional :: vit_J2000   ! vecteur vitesse dans le repere equatorial moyen J2000
real(pm_reel),        dimension(3), intent(out), optional :: vit_TerVrai ! vecteur vitesse dans le repere Terrestre Vrai a la date t 
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob       ! jacobienne de la transformation


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_J2000_TerVrai
     subroutine mr_J2000_veis ( model, jul1950, delta_tu1, delta_tai, pos_J2000, pos_veis, code_retour, &
          vit_J2000, vit_veis, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


integer,                            intent(in)            :: model       ! indicateur des modeles de precession et de nutation
type(tm_jour_sec),                  intent(in)            :: jul1950     ! date julienne 1950 t (non normalisee) en jour et secondes
real(pm_reel),                      intent(in)            :: delta_tu1   ! ecart entre l'echelle de temps TU1 et l'echelle de temps utilisee pour la date t
real(pm_reel),                      intent(in)            :: delta_tai   ! ecart entre l'echelle de temps TAI et l'echelle de temps utilisee pour la date t
real(pm_reel),        dimension(3), intent(in)            :: pos_J2000   ! vecteur position dans le repere equatorial moyen J2000
real(pm_reel),        dimension(3), intent(out)           :: pos_veis    ! vecteur position dans le repere de Veis a la date t 
type(tm_code_retour),               intent(out)           :: code_retour
real(pm_reel),        dimension(3), intent(in),  optional :: vit_J2000   ! vecteur vitesse dans le repere equatorial moyen J2000
real(pm_reel),        dimension(3), intent(out), optional :: vit_veis    ! vecteur vitesse dans le repere de Veis a la date t 
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob       ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_J2000_veis
     subroutine mr_PlaIner_PlaVrai ( planete, modeleUAI, long, pos_PlaIner, pos_PlaVrai, code_retour, &
          vit_rot, jul1950, vit_PlaIner, vit_PlaVrai, jacob)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


integer,                       intent(in)            ::  planete          ! planete
integer,                       intent(in)            ::  modeleUAI        ! modele UAI definissant la vitesse de rotation
real(pm_reel),                 intent(in)            ::  long             ! longitude du repere
real(pm_reel), dimension(3),   intent(in)            ::  pos_PlaIner      ! position dans le repere planetocentrique Inertiel
real(pm_reel), dimension(3),   intent(out)           ::  pos_PlaVrai      ! position dans le repere planetocentrique Vrai
type(tm_code_retour),          intent(out)           ::  code_retour
real(pm_reel),                 intent(in),  optional ::  vit_rot          ! vitesse de rotation de la planete
type(tm_jour_sec),             intent(in),  optional ::  jul1950          ! date pour la vit de rotation si planete = Neptune
real(pm_reel), dimension(3),   intent(in),  optional ::  vit_PlaIner      ! vitesse dans le repere  planetocentrique Inertiel
real(pm_reel), dimension(3),   intent(out), optional ::  vit_PlaVrai      ! position dans le repere planetocentrique Vrai
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob            ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_PlaIner_PlaVrai
     subroutine mr_PlaVrai_PlaIner ( planete, modeleUAI, long, pos_PlaVrai, pos_PlaIner, code_retour, &
          vit_rot, jul1950, vit_PlaVrai, vit_PlaIner, jacob)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


integer,                       intent(in)            ::  planete          ! planete
integer,                       intent(in)            ::  modeleUAI        ! modele UAI definissant la vitesse de rotation
real(pm_reel),                 intent(in)            ::  long             ! longitude du repere
real(pm_reel), dimension(3),   intent(in)            ::  pos_PlaVrai      ! position dans le repere planetocentrique Vrai
real(pm_reel), dimension(3),   intent(out)           ::  pos_PlaIner      ! position dans le repere planetocentrique Inertiel
type(tm_code_retour),          intent(out)           ::  code_retour
real(pm_reel),                 intent(in),  optional ::  vit_rot          ! vitesse de rotation de la planete
type(tm_jour_sec),             intent(in),  optional ::  jul1950          ! date pour la vit de rotation si planete = Neptune
real(pm_reel), dimension(3),   intent(in),  optional ::  vit_PlaVrai      ! position dans le repere planetocentrique Vrai
real(pm_reel), dimension(3),   intent(out), optional ::  vit_PlaIner      ! vitesse dans le repere  planetocentrique Inertiel
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob            ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_PlaVrai_PlaIner
     subroutine mr_PlanetVrai_EquaUAI ( planete, modeleUAI, jul1950, pos_PlanetVrai, pos_EquaUAI, code_retour, &
          tsid, deriv_tsid, vit_PlanetVrai, vit_EquaUAI, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


integer,                       intent(in)            ::  planete          ! planete
integer,                       intent(in)            ::  modeleUAI        ! modele UAI definissant le meridien origine
type(tm_jour_sec),             intent(in)            ::  jul1950          ! date julienne 1950 en jours secondes
real(pm_reel), dimension(3),   intent(in)            ::  pos_PlanetVrai   ! position dans le repere planetocentrique
real(pm_reel), dimension(3),   intent(out)           ::  pos_EquaUAI      ! position dans le repere equatorial planetaire de l'astre
type(tm_code_retour),          intent(out)           ::  code_retour
real(pm_reel),                 intent(in),  optional ::  tsid             ! position du meridien origine
real(pm_reel),                 intent(in),  optional ::  deriv_tsid       ! derivee du meridien origine = vitesse de rotation
real(pm_reel), dimension(3),   intent(in),  optional ::  vit_PlanetVrai   ! position dans le repere planetocentrique
real(pm_reel), dimension(3),   intent(out), optional ::  vit_EquaUAI      ! vitesse dans le repere equatorial planetaire de l'astre
real(pm_reel), dimension(6,6), intent(out), optional ::  jacob            ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_PlanetVrai_EquaUAI
     subroutine mr_TerRef_TerVrai( U, V, pos_ref, pos_vrai, code_retour, vit_ref, vit_vrai, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


real(pm_reel),                      intent(in)            :: U           ! coordonnee U du pole vrai a la date t dans le repere R
real(pm_reel),                      intent(in)            :: V           ! coordonnee V du pole vrai a la date t dans le repere R
real(pm_reel),        dimension(3), intent(in)            :: pos_ref     ! vecteur position dans le repere terrestre de reference
real(pm_reel),        dimension(3), intent(out)           :: pos_vrai    ! vecteur position dans le repere terrestre vrai a la date t
type(tm_code_retour),               intent(out)           :: code_retour
real(pm_reel),        dimension(3), intent(in) , optional :: vit_ref     ! vecteur vitesse dans le repere terrestre de reference
real(pm_reel),        dimension(3), intent(out), optional :: vit_vrai    ! vecteur vitesse dans le repere terrestre vrai a la date t
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob       ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_TerRef_TerVrai
     subroutine mr_TerRef_TerVrai_iers2003(Xp, Yp, pos_ref, pos_vrai, code_retour, s_prime, vit_ref, vit_vrai, jacob)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


real(pm_reel),                      intent(in)            :: Xp          ! coordonnee du pole vrai a la date t dans le repere de référence
                                                                         ! colonne 6 du bulletin A
real(pm_reel),                      intent(in)            :: Yp          ! coordonnee du pole vrai a la date t dans le repere de référence
                                                                         ! colonne 8 du bulletin A
real(pm_reel),        dimension(3), intent(in)            :: pos_ref     ! vecteur position dans le repere terrestre de reference
real(pm_reel),        dimension(3), intent(out)           :: pos_vrai    ! vecteur position dans le repere terrestre vrai a la date t
type(tm_code_retour),               intent(out)           :: code_retour
real(pm_reel),                      intent(in),  optional :: s_prime     ! Parametre de correction s'
real(pm_reel),        dimension(3), intent(in) , optional :: vit_ref     ! vecteur vitesse dans le repere terrestre de reference
real(pm_reel),        dimension(3), intent(out), optional :: vit_vrai    ! vecteur vitesse dans le repere terrestre vrai a la date t
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob       ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_TerRef_TerVrai_iers2003
     subroutine mr_TerVrai_TerRef_iers2003(Xp, Yp, pos_vrai, pos_ref, code_retour, s_prime, vit_vrai, vit_ref, jacob)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


real(pm_reel),                      intent(in)            :: Xp          ! coordonnee du pole vrai a la date t dans le repere de référence
                                                                         ! colonne 6 du bulletin A
real(pm_reel),                      intent(in)            :: Yp          ! coordonnee du pole vrai a la date t dans le repere de référence
                                                                         ! colonne 8 du bulletin A
real(pm_reel),        dimension(3), intent(in)            :: pos_vrai    ! vecteur position dans le repere terrestre vrai
real(pm_reel),        dimension(3), intent(out)           :: pos_ref     ! vecteur position dans le repere terrestre de reference
type(tm_code_retour),               intent(out)           :: code_retour
real(pm_reel),                      intent(in),  optional :: s_prime     ! Parametre de correction s'
real(pm_reel),        dimension(3), intent(in) , optional :: vit_vrai    ! vecteur vitesse dans le repere terrestre vrai
real(pm_reel),        dimension(3), intent(out), optional :: vit_ref     ! vecteur vitesse dans le repere terrestre de reference
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob       ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_TerVrai_TerRef_iers2003
     subroutine mr_TerVrai_EquaVrai ( model, jul1950, delta_tu1, delta_tai, pos_TerVrai, pos_EquaVrai, code_retour, &
                                      inertiel, vit_TerVrai, vit_EquaVrai, jacob )

       use type_mslib
       use precision_mslib
       implicit none



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,                            intent(in)            :: model        ! indicateur des modeles de precession et de nutation
type(tm_jour_sec),                  intent(in)            :: jul1950      ! date julienne 1950 t (non normalisee) en jour et secondes
real(pm_reel),                      intent(in)            :: delta_tu1    ! ecart entre l'echelle de temps TU1 et l'echelle de temps utilisee pour la date t
real(pm_reel),                      intent(in)            :: delta_tai    ! ecart entre l'echelle de temps TAI et l'echelle de temps utilisee pour la date t
real(pm_reel),        dimension(3), intent(in)            :: pos_TerVrai  ! vecteur position dans le repere terrestre vrai a la date t 
real(pm_reel),        dimension(3), intent(out)           :: pos_EquaVrai ! vecteur position dans le repere equatorial vrai a la date t
type(tm_code_retour),               intent(out)           :: code_retour

logical,                            intent(in),  optional :: inertiel     ! indicateur a vrai si calcul inertiel (sans vitesses d'entrainement)
real(pm_reel),        dimension(3), intent(in),  optional :: vit_TerVrai  ! vecteur vitesse dans le repere terrestre vrai a la date t 
real(pm_reel),        dimension(3), intent(out), optional :: vit_EquaVrai ! vecteur vitesse dans le repere equatorial vrai a la date t
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob        ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_TerVrai_EquaVrai
     subroutine mr_TerVrai_J2000 ( model, jul1950, delta_tu1, delta_tai, pos_TerVrai, pos_J2000, code_retour, &
                              vit_TerVrai, vit_J2000, jacob )

       use type_mslib
       use precision_mslib
       implicit none



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,                            intent(in)            :: model       ! indicateur des modeles de precession et de nutation
type(tm_jour_sec),                  intent(in)            :: jul1950     ! date julienne 1950 t (non normalisee) en jour et secondes
real(pm_reel),                      intent(in)            :: delta_tu1   ! ecart entre l'echelle de temps TU1 et l'echelle de temps utilisee pour la date t
real(pm_reel),                      intent(in)            :: delta_tai   ! ecart entre l'echelle de temps TAI et l'echelle de temps utilisee pour la date t
real(pm_reel),        dimension(3), intent(in)            :: pos_TerVrai ! vecteur position dans le repere terrestre vrai a la date t 
real(pm_reel),        dimension(3), intent(out)           :: pos_J2000   ! vecteur position dans le repere equatorial moyen J2000
type(tm_code_retour),               intent(out)           :: code_retour
real(pm_reel),        dimension(3), intent(in),  optional :: vit_TerVrai ! vecteur vitesse dans le repere terrestre vrai a la date t 
real(pm_reel),        dimension(3), intent(out), optional :: vit_J2000   ! vecteur vitesse dans le repere equatorial moyen J2000
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob       ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_TerVrai_J2000
     subroutine mr_TerVrai_TerRef ( U, V, pos_vrai, pos_ref, code_retour, vit_vrai, vit_ref, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel),                     intent(in)            :: U           ! coordonnee U du pole vrai a la date t dans le repere R
real(pm_reel),                     intent(in)            :: V           ! coordonnee V du pole vrai a la date t dans le repere R
real(pm_reel),       dimension(3), intent(in)            :: pos_vrai    ! vecteur position dans le repere terrestre vrai a la date t
real(pm_reel),       dimension(3), intent(out)           :: pos_ref     ! vecteur position dans le repere terrestre de reference
type(tm_code_retour)             , intent(out)           :: code_retour
real(pm_reel),       dimension(3), intent(in),  optional :: vit_vrai    ! vecteur vitesse dans le repere terrestre vrai a la date t
real(pm_reel),       dimension(3), intent(out), optional :: vit_ref     ! vecteur vitesse dans le repere terrestre de reference
real(pm_reel),     dimension(6,6), intent(out), optional :: jacob       ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_TerVrai_TerRef
     subroutine mr_TerVrai_veis ( jul1950, delta_tu1, pos_TerVrai, pos_veis, code_retour, vit_TerVrai, vit_veis, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_jour_sec),                  intent(in)            :: jul1950     ! date julienne 1950 t (non normalisee) en jour et secondes
real(pm_reel),                      intent(in)            :: delta_tu1   ! ecart entre l'echelle de temps TU1 et l'echelle de temps utilisee pour la date t
real(pm_reel),        dimension(3), intent(in)            :: pos_TerVrai ! vecteur position dans le repere terrestre vrai a la date t
real(pm_reel),        dimension(3), intent(out)           :: pos_veis    ! vecteur position dans le repere de Veis a la date t 
type(tm_code_retour),               intent(out)           :: code_retour
real(pm_reel),        dimension(3), intent(in),  optional :: vit_TerVrai ! vecteur vitesse dans le repere terrestre vrai a la date t
real(pm_reel),        dimension(3), intent(out), optional :: vit_veis    ! vecteur vitesse dans le repere de Veis a la date t 
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob       ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_TerVrai_veis
     subroutine mr_mat_J2000_BBR (pos_Pla1, vit_Pla1, pos_Pla2, vit_Pla2, mat, code_retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3), intent(in)    :: pos_Pla1 ! position de la planete 1 dans le J2000
real(pm_reel), dimension(3), intent(in)    :: vit_Pla1 ! vitesse de la planete 1 dans le J2000
real(pm_reel), dimension(3), intent(in)    :: pos_Pla2 ! position de la planete 2 dans le J2000
real(pm_reel), dimension(3), intent(in)    :: vit_Pla2 ! vitesse de la planete 2 dans le J2000
real(pm_reel), dimension(6,6), intent(out) :: mat      ! matrice de passage de J2000 au BBR
type(tm_code_retour), intent(out)          :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_mat_J2000_BBR
     subroutine mr_mat_nuta(nuta, obli_moy, mat_nuta, code_retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


type(tm_nuta), intent(in)                            :: nuta    !    nutation en longitude et en obliquite
real(pm_reel), intent(in)                            :: obli_moy!    obliquite moyenne
real(pm_reel), dimension(3,3), intent(out)           :: mat_nuta!     matrice de nutation
type(tm_code_retour), intent(out)                    :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_mat_nuta
     subroutine mr_nuta ( model_nuta, jul1950, nuta,&
      code_retour, delta_tai, offset_nuta, deriv1_nuta, deriv2_nuta)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                 :: model_nuta      ! indicateur du modele de nutation 
type(tm_jour_sec), intent(in)       :: jul1950         ! date julienne


type(tm_nuta), intent(out)          :: nuta            ! nutation en longitude et en obliquite
type(tm_code_retour), intent(out)   :: code_retour
real(pm_reel), intent(in),  optional:: delta_tai       ! ecart entre l'echelle de temps TAI et l'echelle de temps utilisee pour la date jul1950
type(tm_nuta), intent(in),  optional:: offset_nuta     ! corrections en longitude et obliquite
type(tm_nuta), intent(out), optional:: deriv1_nuta     ! derivee premiere de la nutation en longitude et en obliquite
type(tm_nuta), intent(out), optional:: deriv2_nuta     ! derivee seconde de la nutation en longitude et en obliquite




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_nuta
     subroutine mr_obli_moy(model_prec, jul1950, obli_moy, code_retour, delta_tai, deriv1_obli, deriv2_obli)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer , intent(in)                             :: model_prec   !  indicateur du modele de precession
type(tm_jour_sec), intent(in)                    :: jul1950      !  date julienne (jour, secondes) 
real(pm_reel) , intent(out)                      :: obli_moy     !  obliquite moyenne
type(tm_code_retour), intent(out)                :: code_retour
real(pm_reel), intent(in),  optional             :: delta_tai    ! ecart entre l'echelle de temps TAI et l'echelle de temps utilisee pour la date jul1950
real(pm_reel), intent(out), optional             :: deriv1_obli  !  derivee premiere
real(pm_reel), intent(out), optional             :: deriv2_obli  !  derivee seconde


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_obli_moy
     subroutine mr_prec ( model_prec, jul1950, prec, code_retour, delta_tai, deriv1_prec, deriv2_prec)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                 :: model_prec      ! indicateur du modele de precession 
type(tm_jour_sec), intent(in)       :: jul1950         ! date julienne
type(tm_prec), intent(out)          :: prec            ! precession equatoriale
type(tm_code_retour), intent(out)   :: code_retour
real(pm_reel), intent(in),  optional:: delta_tai       ! ecart entre l'echelle de temps TAI et l'echelle de temps utilisee pour la date jul1950
type(tm_prec), intent(out), optional:: deriv1_prec     ! derivee premiere de la precession
type(tm_prec), intent(out), optional:: deriv2_prec     ! derivee seconde de la precession


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_prec
     subroutine mr_rep_fon(trsf, model, jul1950_t1, jul1950_t2, mat_pass, code_retour, delta_tai)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)    ::  trsf  ! numero de la transformation:
integer,                       intent(in)           :: model       ! indicateur du modele --> pm_lieske_wahr: LIESKE + WAHR
type(tm_jour_sec),             intent(in)           :: jul1950_t1  ! date du repere initial
type(tm_jour_sec),             intent(in)           :: jul1950_t2  ! date du repere final

real(pm_reel), dimension(3,3), intent(out)          :: mat_pass    ! matrice de passage entre les 2 reperes

type(tm_code_retour),          intent(out)          :: code_retour
real(pm_reel),                 intent(in), optional :: delta_tai   ! ecart entre l'echelle de temps TAI et l'echelle de temps utilisee pour les dates jul1950_t1 et jul1950_t2


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_rep_fon
     subroutine mr_s_prime_iers2003(jul1950,s_prime,code_retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_jour_sec),             intent(in)            ::  jul1950       ! Date du calcul (J2000)
real(pm_reel),                 intent(out)           ::  s_prime       ! Paramètre s'
type(tm_code_retour),          intent(out)           ::  code_retour


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_s_prime_iers2003
     subroutine mr_TerVrai_CIP_iers2003(jul1950,delta_tu1,pos_TerVrai,pos_CIP,code_retour,inertiel,vit_TerVrai,vit_CIP,jacob, &
          mat,vect_rot)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


type(tm_jour_sec),                  intent(in)            :: jul1950     ! date julienne en (jour,sec)                             
real(pm_reel),                      intent(in)            :: delta_tu1   ! ecart entre l'echelle de temps TU1 et l'echelle de temps de la
                                                                         ! date jul1950; en s
real(pm_reel),        dimension(3), intent(in)            :: pos_TerVrai ! vecteur position dans le repere terrestre vrai a la date t
real(pm_reel),        dimension(3), intent(out)           :: pos_CIP     ! vecteur position dans le repere celeste intermedaire a la date t
type(tm_code_retour),               intent(out)           :: code_retour
logical,                            intent(in),  optional :: inertiel    ! indicateur vrai si calcul inertiel (sans vitesse d'entrainement)
real(pm_reel),        dimension(3), intent(in) , optional :: vit_TerVrai ! vecteur vitesse dans le repere terrestre vrai a la date t
real(pm_reel),        dimension(3), intent(out), optional :: vit_CIP     ! vecteur vitesse dans le repere celeste intermediare a la date t
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob       ! jacobienne de la transformation
real(pm_reel),      dimension(3,3), intent(out), optional :: mat         ! matrice de rotation instantannee
real(pm_reel),      dimension(3),   intent(out), optional :: vect_rot    ! vecteur rotation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_TerVrai_CIP_iers2003
     subroutine mr_CIP_TerVrai_iers2003(jul1950,delta_tu1,pos_CIP,pos_TerVrai,code_retour,inertiel,vit_CIP,vit_TerVrai,jacob, &
          mat,vect_rot)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


type(tm_jour_sec),                  intent(in)            :: jul1950     ! date julienne en (jour,sec)                             
real(pm_reel),                      intent(in)            :: delta_tu1   ! ecart entre l'echelle de temps TU1 et l'echelle de temps de la
                                                                         ! date jul1950; en s
real(pm_reel),        dimension(3), intent(in)            :: pos_CIP     ! vecteur position dans le repere celeste intermedaire a la date t
real(pm_reel),        dimension(3), intent(out)           :: pos_TerVrai! vecteur position dans le repere terrestre vrai a la date t
type(tm_code_retour),               intent(out)           :: code_retour
logical,                            intent(in),  optional :: inertiel    ! indicateur vrai si calcul inertiel (sans vitesse d'entrainement)
real(pm_reel),        dimension(3), intent(in) , optional :: vit_CIP     ! vecteur vitesse dans le repere celeste intermediare a la date t
real(pm_reel),        dimension(3), intent(out), optional :: vit_TerVrai ! vecteur vitesse dans le repere terrestre vrai a la date t
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob       ! jacobienne de la transformation
real(pm_reel),      dimension(3,3), intent(out), optional :: mat         ! matrice de rotation instantannee
real(pm_reel),      dimension(3),   intent(out), optional :: vect_rot    ! vecteur rotation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_CIP_TerVrai_iers2003
     subroutine mr_CIP_EME2000_iers2003(jul1950, delta_te, dX, dY, pos_CIP, pos_EME2000,code_retour,&
                                   inertiel, vit_CIP, vit_EME2000, jacob, mat,vect_rot)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


type(tm_jour_sec),                  intent(in)            :: jul1950     ! date julienne en (jours,sec)
real(pm_reel),                      intent(in)            :: delta_te    ! ecart entre l'echelle de temps TE (ou TT) et l'echelle de temps de la
                                                                         ! date jul1950; en s
real(pm_reel),                      intent(in)            :: dX          ! nutation  convertie en radian
                                                                         ! avant derniere colonne du bulletin A                                      
real(pm_reel),                      intent(in)            :: dY          ! nutation  convertie en radian
                                                                         ! derniere colonne du bulletin A
real(pm_reel),        dimension(3), intent(in)            :: pos_CIP     ! vecteur position dans le repere celeste intermediaire
real(pm_reel),        dimension(3), intent(out)           :: pos_EME2000 ! vecteur position dans le repere equatorial moyen EME2000
type(tm_code_retour),               intent(out)           :: code_retour
logical,                            intent(in),  optional :: inertiel    ! indicateur vrai si calcul inertiel (sans vitesse d'entrainement)
real(pm_reel),        dimension(3), intent(in) , optional :: vit_CIP     ! vecteur vitesse dans le repere celeste intermediaire
real(pm_reel),        dimension(3), intent(out), optional :: vit_EME2000 ! vecteur vitesse dans le repere equatorial moyen EME2000
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob       ! jacobienne de la transformation
real(pm_reel),      dimension(3,3), intent(out), optional :: mat         ! matrice de rotation instantannee
real(pm_reel),      dimension(3),   intent(out), optional :: vect_rot    ! vecteur rotation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_CIP_EME2000_iers2003
     subroutine mr_EME2000_CIP_iers2003(jul1950, delta_te, dX, dY, pos_EME2000, pos_CIP,code_retour,&
                                   inertiel, vit_EME2000, vit_CIP, jacob, mat, vect_rot)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


type(tm_jour_sec),                  intent(in)            :: jul1950     ! date julienne en (jours,sec)
real(pm_reel),                      intent(in)            :: delta_te    ! ecart entre l'echelle de temps TE (ou TT) et l'echelle de temps de la
                                                                         ! date jul1950; en s
real(pm_reel),                      intent(in)            :: dX          ! nutation  convertie en radian
                                                                         ! avant derniere colonne du bulletin A                                      
real(pm_reel),                      intent(in)            :: dY          ! nutation  convertie en radian
                                                                         ! derniere colonne du bulletin A			 									 
real(pm_reel),        dimension(3), intent(in)            :: pos_EME2000 ! vecteur position dans le repere celeste intermediaire
real(pm_reel),        dimension(3), intent(out)           :: pos_CIP     ! vecteur position dans le repere equatorial moyen EME2000
type(tm_code_retour),               intent(out)           :: code_retour
logical,                            intent(in),  optional :: inertiel    ! indicateur vrai si calcul inertiel (sans vitesse d'entrainement)
real(pm_reel),        dimension(3), intent(in) , optional :: vit_EME2000 ! vecteur vitesse dans le repere celeste intermediaire
real(pm_reel),        dimension(3), intent(out), optional :: vit_CIP     ! vecteur vitesse dans le repere equatorial moyen EME2000
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob       ! jacobienne de la transformation
real(pm_reel),      dimension(3,3), intent(out), optional :: mat         ! matrice de rotation instantannee
real(pm_reel),      dimension(3),   intent(out), optional :: vect_rot    ! vecteur rotation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_EME2000_CIP_iers2003
     subroutine mr_tsid_aoki(jul1950, delta_tu1, tsid, code_retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_jour_sec), intent(in)      ::  jul1950     !  date julienne 1950 (jour, seconde)
real(pm_reel), intent(in)          ::  delta_tu1   !  ecart de datation entre TU1 et l'echelle de datation utilisee pour jul1950
real(pm_reel), intent(out)         ::  tsid        !  temps sideral
type(tm_code_retour), intent(out)  ::  code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_tsid_aoki
     subroutine mr_tsid_veis(jul1950, delta_tu1, tsid, code_retour)
       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_jour_sec), intent(in)             ::  jul1950   !  date julienne

real(pm_reel), intent(in)                 ::  delta_tu1 ! delta TU1

real(pm_reel), intent(out)                ::  tsid      !  temps sideral
type(tm_code_retour), intent(out)         ::  code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_tsid_veis
     subroutine mr_tsid_vrai ( model, jul1950, delta_tu1, delta_tai, tsid_vrai, code_retour, offset_nuta, deriv_tsid_vrai )

       use type_mslib
       use precision_mslib
       implicit none



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


integer,           intent(in)           :: model      ! indicateur pour les modeles de precession + nutation
type(tm_jour_sec), intent(in)           :: jul1950    ! date julienne 1950 t (non normalisee) en jour et secondes
real(pm_reel),     intent(in)           :: delta_tu1  ! ecart entre l'echelle de temps TU1 et l'echelle de temps utilisee pour la date t
real(pm_reel),     intent(in)           :: delta_tai  ! ecart entre l'echelle de temps TAI et l'echelle de temps utilisee pour la date t
real(pm_reel),     intent(out)          :: tsid_vrai  ! temps sideral vrai (radians)
type(tm_code_retour), intent(out)       :: code_retour
type(tm_nuta),     intent(in), optional :: offset_nuta     ! corrections en longitude et obliquite (radians)
real(pm_reel),     intent(out),optional :: deriv_tsid_vrai ! derivee du temps sideral vrai (radians/sec)



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_tsid_vrai
     subroutine mr_veis_EquaVrai ( model, jul1950, delta_tu1, delta_tai, pos_veis, pos_EquaVrai, code_retour, &
                                   vit_veis, vit_EquaVrai, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,                            intent(in)            :: model        ! indicateur des modeles de precession et de nutation
type(tm_jour_sec),                  intent(in)            :: jul1950      ! date julienne 1950 t (non normalisee) en jour et secondes
real(pm_reel),                      intent(in)            :: delta_tu1    ! ecart entre l'echelle de temps TU1 et l'echelle de temps utilisee pour la date t
real(pm_reel),                      intent(in)            :: delta_tai    ! ecart entre l'echelle de temps TAI et l'echelle de temps utilisee pour la date t
real(pm_reel),        dimension(3), intent(in)            :: pos_veis     ! vecteur position dans le repere de Veis a la date t 
real(pm_reel),        dimension(3), intent(out)           :: pos_EquaVrai ! vecteur position dans le repere equatorial vrai a la date t
type(tm_code_retour),               intent(out)           :: code_retour
real(pm_reel),        dimension(3), intent(in),  optional :: vit_veis     ! vecteur vitesse dans le repere de Veis a la date t 
real(pm_reel),        dimension(3), intent(out), optional :: vit_EquaVrai ! vecteur vitesse dans le repere equatorial vrai a la date t
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob        ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_veis_EquaVrai
     subroutine mr_veis_J2000 ( model, jul1950, delta_tu1, delta_tai, pos_veis, pos_J2000, code_retour, &
          vit_veis, vit_J2000, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


integer,                            intent(in)            :: model       ! indicateur des modeles de precession et de nutation
type(tm_jour_sec),                  intent(in)            :: jul1950     ! date julienne 1950 t (non normalisee) en jour et secondes
real(pm_reel),                      intent(in)            :: delta_tu1   ! ecart entre l'echelle de temps TU1 et l'echelle de temps utilisee pour la date t
real(pm_reel),                      intent(in)            :: delta_tai   ! ecart entre l'echelle de temps TAI et l'echelle de temps utilisee pour la date t
real(pm_reel),        dimension(3), intent(in)            :: pos_veis    ! vecteur position dans le repere de Veis a la date t 
real(pm_reel),        dimension(3), intent(out)           :: pos_J2000   ! vecteur position dans le repere equatorial moyen J2000
type(tm_code_retour),               intent(out)           :: code_retour
real(pm_reel),        dimension(3), intent(in),  optional :: vit_veis    ! vecteur vitesse dans le repere de Veis a la date t 
real(pm_reel),        dimension(3), intent(out), optional :: vit_J2000   ! vecteur vitesse dans le repere equatorial moyen J2000
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob       ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_veis_J2000
     subroutine mr_veis_TerVrai( jul1950, delta_tu1, pos_veis, pos_TerVrai, code_retour, vit_veis, vit_TerVrai, jacob )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_jour_sec),                  intent(in)            :: jul1950     ! date julienne 1950 t (non normalisee) en jour et secondes
real(pm_reel),                      intent(in)            :: delta_tu1   ! ecart entre l'echelle de temps TU1 et l'echelle de temps utilisee pour la date t
real(pm_reel),        dimension(3), intent(in)            :: pos_veis    ! vecteur position dans le repere de Veis a la date t 
real(pm_reel),        dimension(3), intent(out)           :: pos_TerVrai ! vecteur position dans le repere terrestre vrai a la date t
type(tm_code_retour),               intent(out)           :: code_retour
real(pm_reel),        dimension(3), intent(in),  optional :: vit_veis    ! vecteur vitesse dans le repere de Veis a la date t 
real(pm_reel),        dimension(3), intent(out), optional :: vit_TerVrai ! vecteur vitesse dans le repere terrestre vrai a la date t
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob       ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_veis_TerVrai
     subroutine mr_TerRef_EME2000_iers2003(jul1950, Xp, Yp, delta_tu1, delta_te, dX, dY, pos_Ref, pos_EME2000, &
          code_retour, s_prime, vit_Ref, vit_EME2000, jacob)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_jour_sec),                  intent(in)            :: jul1950     ! date julienne en (jours,sec)
real(pm_reel),                      intent(in)            :: Xp          ! coordonnee du pole vrai a la date t dans le repere de référence
                                                                         ! colonne 6 du bulletin A
real(pm_reel),                      intent(in)            :: Yp          ! coordonnee du pole vrai a la date t dans le repere de référence
                                                                         ! colonne 8 du bulletin A
real(pm_reel),                      intent(in)            :: delta_tu1   ! ecart entre l'echelle de temps TU1 et l'echelle de temps de la
real(pm_reel),                      intent(in)            :: delta_te    ! ecart entre l'echelle de temps TE (ou TT) et l'echelle de temps de la
                                                                         ! date jul1950; en s
real(pm_reel),                      intent(in)            :: dX          ! nutation  convertie en radian
                                                                         ! avant derniere colonne du bulletin A                                      
real(pm_reel),                      intent(in)            :: dY          ! nutation  convertie en radian
                                                                         ! derniere colonne du bulletin A									 									 
real(pm_reel),        dimension(3), intent(in)            :: pos_Ref     ! vecteur position dans le repere de référence
real(pm_reel),        dimension(3), intent(out)           :: pos_EME2000 ! vecteur position dans le repere equatorial moyen EME2000
type(tm_code_retour),               intent(out)           :: code_retour
real(pm_reel),                      intent(in),  optional :: s_prime     ! Parametre de correction s'
real(pm_reel),        dimension(3), intent(in) , optional :: vit_Ref     ! vecteur vitesse dans le repere de référence
real(pm_reel),        dimension(3), intent(out), optional :: vit_EME2000 ! vecteur vitesse dans le repere equatorial moyen EME2000
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob       ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_TerRef_EME2000_iers2003
     subroutine mr_EME2000_TerRef_iers2003(jul1950, Xp, Yp, delta_tu1, delta_te, dX, dY, pos_EME2000, pos_Ref, &
          code_retour, s_prime, vit_EME2000, vit_Ref, jacob)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_jour_sec),                  intent(in)            :: jul1950     ! date julienne en (jours,sec)
real(pm_reel),                      intent(in)            :: Xp          ! coordonnee du pole vrai a la date t dans le repere de référence
                                                                         ! colonne 6 du bulletin A
real(pm_reel),                      intent(in)            :: Yp          ! coordonnee du pole vrai a la date t dans le repere de référence
                                                                         ! colonne 8 du bulletin A
real(pm_reel),                      intent(in)            :: delta_tu1   ! ecart entre l'echelle de temps TU1 et l'echelle de temps de la
real(pm_reel),                      intent(in)            :: delta_te    ! ecart entre l'echelle de temps TE (ou TT) et l'echelle de temps de la
                                                                         ! date jul1950; en s
real(pm_reel),                      intent(in)            :: dX          ! nutation  convertie en radian
                                                                         ! avant derniere colonne du bulletin A                                      
real(pm_reel),                      intent(in)            :: dY          ! nutation  convertie en radian
                                                                         ! derniere colonne du bulletin A									 									 
real(pm_reel),        dimension(3), intent(in)            :: pos_EME2000 ! vecteur position dans le repere de référence
real(pm_reel),        dimension(3), intent(out)           :: pos_Ref     ! vecteur position dans le repere equatorial moyen EME2000
type(tm_code_retour),               intent(out)           :: code_retour
real(pm_reel),                      intent(in),  optional :: s_prime     ! Parametre de correction s'
real(pm_reel),        dimension(3), intent(in) , optional :: vit_EME2000 ! vecteur vitesse dans le repere de référence
real(pm_reel),        dimension(3), intent(out), optional :: vit_Ref     ! vecteur vitesse dans le repere equatorial moyen EME2000
real(pm_reel),      dimension(6,6), intent(out), optional :: jacob       ! jacobienne de la transformation



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mr_EME2000_TerRef_iers2003
end interface

  character(len=pm_longueur_rcs_id), private, parameter :: &
  rcs_id =' $Id: int_rep_fondamentaux.f90 362 2013-02-15 18:01:28Z bbjc $ '

end module int_rep_fondamentaux
