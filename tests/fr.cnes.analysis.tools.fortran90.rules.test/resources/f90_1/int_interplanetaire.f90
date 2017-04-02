module int_interplanetaire
! (C) Copyright CNES - MSLIB - 2007
!************************************************************************
!
! But:  Definition des constantes et interface des fonctions du thème I
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
!   + Version 6.6 : rajout du cartouche
!                   (Date: 05/2007 - Realisation: Atos origin)
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
  character(len=256), private, parameter :: SVN_VER =  '$Id: int_interplanetaire.f90 362 2013-02-15 18:01:28Z bbjc $'

public
interface
     subroutine mi_pb_deux_corps ( mu, pos_car_t0, vit_car_t0, &
          duree, sol_kep, pos_car_t1, vit_car_t1, code_retour, &
          encke, dist_t0, dist_t1, acc_t0, acc_t1, deriv_t0, deriv_t1, deriv_mu_t0, deriv_mu_t1)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)                            :: mu           ! constante d'attraction du corps central
real(pm_reel), dimension(3), intent(in)              :: pos_car_t0   ! vecteur position en coordonnées cartésiennes à t0 initial
real(pm_reel), dimension(3), intent(in)              :: vit_car_t0   ! vecteur vitesse en coordonnées cartésiennes à t0 initial
real(pm_reel), intent(in)                            :: duree        ! duree de transfert
real(pm_reel), intent(out)                           :: sol_kep      ! solution de l'equation de kepler
real(pm_reel), dimension(3), intent(out)             :: pos_car_t1   ! vecteur position en coordonnées cartésiennes à t1 final
real(pm_reel), dimension(3), intent(out)             :: vit_car_t1   ! vecteur vitesse en coordonnées cartésiennes à t1 final
type(tm_code_retour), intent(out)                    :: code_retour
real(pm_reel), intent(in), optional                  :: encke        ! approximation de solution de l'equation de kepler generalise a t ou parametre de encke
real(pm_reel), intent(out), optional                 :: dist_t0      ! distance initiale
real(pm_reel), intent(out), optional                 :: dist_t1      ! distance finale
real(pm_reel), dimension(3), intent(out), optional   :: acc_t0       ! vecteur acceleration initial
real(pm_reel), dimension(3), intent(out), optional   :: acc_t1       ! vecteur acceleration final
real(pm_reel), dimension(6,6), intent(out), optional :: deriv_t0     ! derivees partielles initiales
real(pm_reel), dimension(6,6), intent(out), optional :: deriv_t1     ! derivees partielles finales
real(pm_reel), dimension(6), intent(out), optional   :: deriv_mu_t0  ! derivees partielles initiales par rapport a mu
real(pm_reel), dimension(6), intent(out), optional   :: deriv_mu_t1  ! derivees partielles finales par rapport a mu


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mi_pb_deux_corps
     subroutine mi_pb_lambert0 ( mu, pos_car_t0, pos_car_t1, duree, ind_sens, &
          vit_car_t0, vit_car_t1, code_retour, coniq, teta, encke, jacob)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)                         :: mu           ! constante d'attraction du corps central
real(pm_reel), dimension(3), intent(in)           :: pos_car_t0   ! vecteur position en coordonnées cartésiennes à t0 initial
real(pm_reel), dimension(3), intent(in)           :: pos_car_t1   ! vecteur position en coordonnées cartésiennes à t1 final
real(pm_reel), intent(in)                         :: duree   ! duree de transfert
integer , intent(in)                              :: ind_sens   ! indicateur du sens du parcours
real(pm_reel), dimension(3), intent(out)          :: vit_car_t0   ! vecteur vitesse en coordonnées cartésiennes à t0 initial
real(pm_reel), dimension(3), intent(out)          :: vit_car_t1   ! vecteur vitesse en coordonnées cartésiennes à t1 final
type(tm_code_retour), intent(out)                 :: code_retour
type(tm_coniq), intent(out), optional             :: coniq     ! parametres de la conique
real(pm_reel), intent(out) , optional             :: teta      ! angle de transfert
real(pm_reel), intent(out) , optional             :: encke     ! approximation de solution de l'equation de kepler generalise a t ou parametre de encke
 real(pm_reel), dimension(6,8), intent(out), optional  :: jacob ! matrice (6,8) des derivees partielles de lambert



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mi_pb_lambert0
     subroutine mi_pb_lambert1 ( mu, pos_car_t0, pos_car_t1, nb_tours, duree, ind_sens, &
          vit_car_t0_orb1, vit_car_t0_orb2, vit_car_t1_orb1, vit_car_t1_orb2, &
          code_retour, nb_max_iter, precision, coniq, teta, encke, jacob_orb1, jacob_orb2)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)                         :: mu           ! constante d'attraction du corps central
real(pm_reel), dimension(3), intent(in)           :: pos_car_t0   ! vecteur position en coordonnées cartésiennes à t0 initial
real(pm_reel), dimension(3), intent(in)           :: pos_car_t1   ! vecteur position en coordonnées cartésiennes à t1 final
integer , intent(in)                              :: nb_tours     ! nombre de tours effectues par la sonde
real(pm_reel), intent(in)                         :: duree        ! duree de transfert
integer , intent(in)                              :: ind_sens      ! indicateur du sens du parcours
real(pm_reel), dimension(3), intent(out)          :: vit_car_t0_orb1   ! vecteur vitesse en coordonnées cartésiennes à t0 initial pour l orbite 1
real(pm_reel), dimension(3), intent(out)          :: vit_car_t0_orb2   ! vecteur vitesse en coordonnées cartésiennes à t0 initial pour l orbite 2
real(pm_reel), dimension(3), intent(out)          :: vit_car_t1_orb1   ! vecteur vitesse en coordonnées cartésiennes à t1 final pour l orbite 1
real(pm_reel), dimension(3), intent(out)          :: vit_car_t1_orb2   ! vecteur vitesse en coordonnées cartésiennes à t1 final pour l orbite 2
type(tm_code_retour), intent(out)                 :: code_retour
integer, intent(in), optional               :: nb_max_iter    ! nombre maximum d'iterations
real(pm_reel), intent(in), optional               :: precision      ! precision requise
type(tm_coniq), dimension(2), intent(out), optional :: coniq     ! parametres de la conique pour les 2 orbites
real(pm_reel), intent(out) , optional               :: teta      ! angle de transfert
real(pm_reel), dimension(2), intent(out) , optional :: encke     ! approximation de solution de l'equation de kepler generalise a t ou parametre de encke pour les 2 orbites
 real(pm_reel), dimension(6,8), intent(out), optional  :: jacob_orb1 ! matrice (6,8) des derivees partielles de lambert pour l'orbite initiale
 real(pm_reel), dimension(6,8), intent(out), optional  :: jacob_orb2 ! matrice (6,8) des derivees partielles de lambert pour l'orbite finale




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mi_pb_lambert1
     subroutine mi_pro_j2 ( mu, r_equa_pla, coeff_j2, duree, osc_t0, osc_t1, code_retour, mat_trans)

       use type_mslib
       use precision_mslib

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(kind=pm_reel), intent(in)                             :: mu         ! constante d'attraction de la planète
real(kind=pm_reel), intent(in)                             :: r_equa_pla ! rayon équatorial de la planète
real(kind=pm_reel), intent(in)                             :: coeff_j2   ! coefficient J2 du potentiel de la planète
real(kind=pm_reel), intent(in)                             :: duree      ! durée de transfert
type(tm_orb_kep), intent(in)                               :: osc_t0     ! paramètres orbitaux à la date initiale
type(tm_orb_kep), intent(out)                              :: osc_t1     ! paramètres orbitaux après la durée du transfert
type(tm_code_retour), intent(out)                         :: code_retour
real(kind=pm_reel), dimension(6,6), intent(out), optional :: mat_trans  ! matrice de transition



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     end subroutine mi_pro_j2
end interface

  character(len=pm_longueur_rcs_id), private, parameter :: &
  rcs_id =' $Id: int_interplanetaire.f90 362 2013-02-15 18:01:28Z bbjc $ '

end module int_interplanetaire
