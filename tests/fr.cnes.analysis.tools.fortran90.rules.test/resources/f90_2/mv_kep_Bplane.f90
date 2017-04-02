subroutine mv_kep_Bplane ( mu, date, kep, Bplane, code_retour, jacob, jacobJPL )

! (C) Copyright CNES - MSPRO - 2004

!************************************************************************
!
! But: Passage des parametres kepleriens aux parametres de B-plane  
! ===
!
! Note d'utilisation:  
! ==================
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 5.1 : creation
!                         (Date: 09/2004 - Realisation: Bruno Revelin)
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
type(tm_jour_sec), intent(in)                        ::  date   ! date du passage a la position kep (JJCNES)
type(tm_orb_kep),intent(in)                          ::  kep    ! parametres kepleriens
type(tm_orb_Bplane),intent(out)                      ::  Bplane ! parametres de B-plane
type(tm_code_retour), intent(out)                    ::  code_retour
real(pm_reel), dimension(6,6),intent(out),optional   ::  jacob     ! jacobien de la transformation
real(pm_reel), dimension(6,6),intent(out),optional   ::  jacobJPL  ! jacobien avec les parametres JPL

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

real(pm_reel) :: DECI_tmp, RAI_tmp                  ! valeurs temporaires des parametres de Bplane
real(pm_reel) :: beta,cosalpha,sinalpha,costheta,sintheta      ! angles intermediaires
real(pm_reel) :: un_sur_e, j44, j54, usphi, B, C    ! variables intermediaires
real(pm_reel),dimension(6,6) :: jac_tmp             ! jacobien temporaire
real(pm_reel) :: eps100                             ! variable epsilon machine * 100 
real(pm_reel) :: eps_parab                          ! variable de comparaison pour l'excentricite 
type(tm_code_retour) :: code_retour_local           ! code retour local

intrinsic acos,asin,sin,cos,sqrt

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSPRO mv_kep_Bplane.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

! autres initialisations
call mc_test(code_retour,eps_parab=eps_parab)  ! pas de code retour a tester
eps100 = 100._pm_reel * epsilon(1._pm_reel) 

! Verifications
! =============

if (mu <= eps100) then
   if (mu < 0._pm_reel) then                                     ! constante de gravitation negative
      code_retour%valeur = pm_err_mu_negatif
   else                                                          ! constante de gravitation proche de 0
      code_retour%valeur = pm_err_mu_nul 
   end if
   go to 6000
end if

if (kep%e < (1._pm_reel+ eps_parab)) then        ! ce n'est pas une hyperbole
   code_retour%valeur = pm_err_e_non_hyperb
   go to 6000
end if
un_sur_e = 1._pm_reel / kep%e

! Calcul des parametres
! =====================

! Module de Vinfini

Bplane%C3 = mu / kep%a

! Declinaison de Vinfini

beta = acos(un_sur_e)
DECI_tmp = asin(sin(beta+kep%pom)*sin(kep%i))
Bplane%DECI = DECI_tmp

! Ascension droite de Vinfini

if (abs(abs(DECI_tmp) - pm_pi_sur2) > eps100) then

   cosalpha = (cos(beta+kep%pom)*cos(kep%gom)-sin(beta+kep%pom)*cos(kep%i)*sin(kep%gom))/cos(DECI_tmp)

   sinalpha = (cos(beta+kep%pom)*sin(kep%gom)+sin(beta+kep%pom)*cos(kep%i)*cos(kep%gom))/cos(DECI_tmp)

   call mu_angle2 (cosalpha, sinalpha, RAI_tmp, code_retour_local)
   if (code_retour_local%valeur /= pm_OK)  then
      code_retour%valeur = code_retour_local%valeur
      if (code_retour_local%valeur < pm_OK)  go to 6000
   end if

else

   RAI_tmp = pm_pi_sur2 
   ! c'est un choix arbitraire car l'ascension droite dans ce cas n'est pas definie

end if

Bplane%RAI = RAI_tmp

! calcul des parametres du Bplane

if (abs(abs(DECI_tmp) - pm_pi_sur2) > eps100) then
   costheta = cos(kep%i)/sqrt(1._pm_reel-(sin(beta+kep%pom)*sin(kep%i))**2)
   sintheta = sin(kep%i)*cos(kep%pom+beta)/sqrt(1._pm_reel-(sin(beta+kep%pom)*sin(kep%i))**2)
else
   costheta = sin(DECI_tmp)*sin(kep%i)*sin(RAI_tmp-kep%gom)
   sintheta = sin(kep%i)*cos(kep%gom-RAI_tmp)
end if

Bplane%BR = kep%a*sqrt(kep%e**2-1._pm_reel)*sintheta
Bplane%BT = kep%a*sqrt(kep%e**2-1._pm_reel)*costheta

Bplane%LTF = real(date%jour,kind=pm_reel)*86400._pm_reel + date%sec &
     -(kep%a**(1.5_pm_reel))/sqrt(mu)*(log(kep%e)+kep%M)

!---------------------------------
!   Calcul de la jacobienne
!---------------------------------

if (present(jacob).or.present(jacobJPL)) then

   !  test des formes indeterminees

   if (abs(abs(DECI_tmp) - pm_pi_sur2) < eps100) then
      code_retour%valeur = pm_err_jac_non_calc_decl_pisur2
      go to 6000
   end if

   ! Calcul

   usphi = 1._pm_reel / sqrt(1._pm_reel-(sin(kep%pom+beta)*sin(kep%i))**2)
   B = kep%a*sqrt(kep%e**2-1._pm_reel)
   C = kep%e**2*sqrt(1._pm_reel-1._pm_reel/kep%e**2)

   jac_tmp(:,:) = 0._pm_reel

   jac_tmp(1,1) = sqrt(kep%e**2-1._pm_reel)*sintheta

   jac_tmp(1,2) = kep%a*kep%e*sintheta/sqrt(kep%e**2-1._pm_reel)     &
        - B/C*sin(kep%i)*sin(kep%pom+beta)*usphi     &
        + B/C*(sin(kep%i))**3*(cos(kep%pom+beta))**2*sin(kep%pom+beta)*usphi**3

   jac_tmp(1,3) = B*cos(kep%i)*cos(kep%pom+beta)*usphi + B*(sin(kep%i))**2*cos(kep%pom+beta)   &
        *(sin(kep%pom+beta))**2*cos(kep%i)*usphi**3

   jac_tmp(1,4) = -B*sin(kep%i)*sin(kep%pom+beta)*usphi + B*(sin(kep%i))**3*     &
        (cos(kep%pom+beta))**2*sin(kep%pom+beta)*usphi**3

   jac_tmp(2,1) = sqrt(kep%e**2-1._pm_reel)*costheta

   jac_tmp(2,2) = kep%a*kep%e*cos(kep%i)/sqrt(kep%e**2-1._pm_reel)*usphi     &
        + B/C*cos(kep%i)*sin(kep%pom+beta)*(sin(kep%i))**2*cos(kep%pom+beta)*usphi**3

   jac_tmp(2,3) = B*sin(kep%i)*usphi * ((cos(kep%i))**2*(sin(kep%pom+beta))**2*usphi**2 - 1._pm_reel)

   jac_tmp(2,4) = B*cos(kep%i)*sin(kep%pom+beta)*(sin(kep%i))**2*cos(kep%pom+beta)*usphi**3

   jac_tmp(3,1) = -1.5_pm_reel*sqrt(kep%a/mu)*(log(kep%e)+kep%M)

   jac_tmp(3,2) = -kep%a**(1.5_pm_reel)/(sqrt(mu)*kep%e)

   jac_tmp(3,6) = -kep%a**(1.5_pm_reel)/sqrt(mu)

   j44 = sin(RAI_tmp-kep%gom)/(sin(kep%pom+beta)*cos(DECI_tmp))

   jac_tmp(4,2) = j44/(kep%e*sqrt(kep%e**2-1._pm_reel))

   jac_tmp(4,3) = -sin(kep%pom+beta)*cos(RAI_tmp-kep%gom)*sin(kep%i)/cos(DECI_tmp)

   jac_tmp(4,4) = j44

   jac_tmp(4,5) = 1._pm_reel

   j54 = cos(kep%pom+beta)*sin(kep%i)/cos(DECI_tmp)

   jac_tmp(5,2) = j54/(kep%e*sqrt(kep%e**2-1._pm_reel))

   jac_tmp(5,3) = sin(RAI_tmp-kep%gom)

   jac_tmp(5,4) = j54

   jac_tmp(6,1) = -mu/kep%a**2

   if (present(jacob)) jacob(:,:) = jac_tmp(:,:)

   if (present(jacobJPL)) then
        
      jacobJPL(:,:) = jac_tmp(:,:)
      jacobJPL(4,1:6) = -jac_tmp(5,1:6)
      jacobJPL(5,1:6) = -cos(DECI_tmp)*jac_tmp(4,1:6)
     
   end if

end if

6000 continue

code_retour%routine = pm_num_mv_kep_Bplane
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mv_kep_Bplane
