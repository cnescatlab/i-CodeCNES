subroutine mv_Vinfarr_kep (mu, Vinfarr, kep, code_retour, jacob)

! (C) Copyright CNES - MSPRO - 2004

!************************************************************************
!
! But: Passage des parametres orbitaux hyperboliques a vitesse infinie 
! ===  d arrivee aux parametres kepleriens
!
! Note d'utilisation:  Applicable uniquement au cas hyperbolique
! ==================
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 5.5 : creation
!             DM-ID 413 : Calcul du systeme de parametres Vinfini d arrivee
!             (Date: 05/2006 - Realisation: Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use type_mspro

use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)                            ::  mu     ! constante de la gravitation universelle
type(tm_orb_Vinf),intent(in)                         ::  Vinfarr   ! parametres hyperboliques
type(tm_orb_kep),intent(out)                         ::  kep    ! parametres kepleriens
type(tm_code_retour), intent(out)                    ::  code_retour
real(pm_reel), dimension(6,6),intent(out),optional   ::  jacob  ! jacobien de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

real(pm_reel) :: e_tmp, i_tmp, w_tmp ! valeurs temporaires des parametres kepleriens

real(pm_reel) :: un_sur_mu, un_sur_e, carre, denom  ! variables intermediaires
real(pm_reel) :: cos_i_sin_theta, sin_i_sin_theta ! variables intermediaires
real(pm_reel) :: cos_theta, sin_theta, theta, theta_infini ! variables intermediaires
real(pm_reel) :: jac33, jac43 ! variables intermediaires

type(tm_code_retour) :: code_retour_local ! code retour intermediaire

real(pm_reel) :: eps_parab, eps_equa ! pour test orbite hyperbolique et equatoriale
real(pm_reel) :: eps100              ! variable epsilon machine * 100 

intrinsic cos,sin,sqrt,acos,abs

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSPRO mv_Vinfarr_kep.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

! autres initialisations
call mc_test(code_retour_local, eps_equa = eps_equa)  ! pas de code retour a tester
call mc_test(code_retour_local,eps_parab = eps_parab) ! pas de code retour a tester
eps100 = 100._pm_reel * epsilon(1._pm_reel)

! Verifications
! =============

if (mu <= eps100) then
   if (mu < 0._pm_reel) then  ! constante de gravitation negative
      code_retour%valeur = pm_err_mu_negatif
   else                       ! constante de gravitation proche de 0
      code_retour%valeur = pm_err_mu_nul 
   end if
   go to 6000
end if

if (Vinfarr%C3 <= eps100) then
   code_retour%valeur = pm_err_C3_negatif_ou_nul
   go to 6000
end if

if (Vinfarr%Rp <= eps100) then
   code_retour%valeur = pm_err_Rp_negatif_ou_nul
   go to 6000
end if

! Calcul des parametres
! =====================

! demi-grand-axe
! --------------

kep%a = mu/Vinfarr%C3

! excenticite
! -----------

e_tmp = Vinfarr%Rp * Vinfarr%C3 / mu + 1._pm_reel ! valeur > 1 a ce stade des calculs
kep%e = e_tmp

un_sur_e = 1._pm_reel / e_tmp

! inclinaison
! -----------

cos_i_sin_theta = cos(Vinfarr%DLA)*sin(Vinfarr%RLA - Vinfarr%gom)
sin_i_sin_theta = sin(Vinfarr%DLA)

call mu_angle2(cos_i_sin_theta,sin_i_sin_theta,i_tmp,code_retour_local)
if (code_retour_local%valeur /= pm_OK)  then
   
   ! traitement pour cas DLA = 0 ou pi et (RLA - gom) = 0 ou pi ==> i = pi/2
   if (code_retour_local%valeur == pm_err_vect_nul) then

     i_tmp = pm_pi_sur2 ! i est dans [0,pi]

   else
     code_retour%valeur = code_retour_local%valeur
     if (code_retour_local%valeur < pm_OK)  go to 6000
   end if
end if

! i_tmp est dans [0, 2pi[ en sortie de mu_angle2
! on doit ramener i entre 0 et pi
if (i_tmp > pm_pi) then 
   i_tmp = i_tmp - pm_pi
end if

kep%i = i_tmp

! argument du periastre
! ---------------------

! calcul de sin(theta) avec theta = theta infini + petit omega
if (sin(i_tmp) < eps_equa) then ! orbite equatoriale (sin(i) proche de 0)

  ! utilisation du cos(i) non nul
  sin_theta = cos_i_sin_theta/cos(i_tmp)

else ! orbite non equatoriale

  ! utilisation du sin(i) non nul
  sin_theta = sin_i_sin_theta/sin(i_tmp)

end if

! calcul de cos(theta)
cos_theta = cos(Vinfarr%RLA - Vinfarr%gom)*cos(Vinfarr%DLA)

! calul de theta
call mu_angle2(cos_theta,sin_theta,theta,code_retour_local)
if (code_retour_local%valeur /= pm_OK)  then
   code_retour%valeur = code_retour_local%valeur
   if (code_retour_local%valeur < pm_OK)  go to 6000
end if

! calcul de theta infini
theta_infini = acos(un_sur_e) ! valeur entre [pi/2, pi] grace a -1/e < 0

! calcul de l'argument du periastre
w_tmp = theta - theta_infini

! valeur ramenee dans [0,2pi]
kep%pom = modulo(w_tmp, pm_deux_pi)

! ascension droite du noeud ascendant
! -----------------------------------
kep%gom = Vinfarr%gom

! anomalie moyenne
!-----------------
kep%M = Vinfarr%M

!---------------------------------
!   Calcul de la jacobienne
!---------------------------------

if (present(jacob)) then

   ! test des formes indeterminees

   if (abs(sin_theta) < eps100) then ! equivalent a DLA = 0 ou pi et (RLA - gom) = 0 ou pi
      code_retour%valeur = pm_err_jac_non_calc_sin_theta
      go to 6000
   end if

   ! Calcul

   un_sur_mu = 1._pm_reel/mu

   jacob(:,:) = 0._pm_reel

   jacob(1,1) = - mu/(Vinfarr%C3**2)

   jacob(2,1) = Vinfarr%Rp * un_sur_mu

   jacob(2,4) = Vinfarr%C3 * un_sur_mu

   jacob(3,2) = sin(Vinfarr%RLA-Vinfarr%gom)/(sin_theta**2)

   jac33 = - sin(i_tmp)*cos(Vinfarr%RLA-Vinfarr%gom)*cos(Vinfarr%DLA)/sin_theta

   jacob(3,3) = jac33

   jacob(3,5) = - jac33

   carre = (Vinfarr%Rp*Vinfarr%C3*un_sur_mu + 1._pm_reel)**2 ! carre > 1
   denom = mu*carre*sqrt(1._pm_reel - (1._pm_reel/carre)) ! denom > 0, avec (1 - 1/carre) > 0

   jacob(4,1) = - Vinfarr%Rp/denom

   jacob(4,2) = cos(Vinfarr%RLA-Vinfarr%gom)*sin(Vinfarr%DLA)/sin_theta

   jac43 = cos_i_sin_theta/sin_theta

   jacob(4,3) = jac43

   jacob(4,4) = - Vinfarr%C3/denom

   jacob(4,5) = - jac43

   jacob(5,5) = 1._pm_reel

   jacob(6,6) = 1._pm_reel

end if

6000 continue

code_retour%routine = pm_num_mv_Vinfarr_kep
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mv_Vinfarr_kep
