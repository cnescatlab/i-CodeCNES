subroutine mv_Bplane_kep (mu, M, Bplane, kep, date, code_retour)

! (C) Copyright CNES - MSPRO - 2004

!************************************************************************
!
! But:  Passage des parametres de B-plane aux parametres kepleriens
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
real(pm_reel), intent(in)                            ::  M      ! anomalie moyenne
type(tm_orb_Bplane),intent(in)                       ::  Bplane ! parametres de B-plane
type(tm_orb_kep),intent(out)                         ::  kep    ! parametres kepleriens
type(tm_jour_sec), intent(out)                       ::  date   ! date du passage a la position (JJCNES)
type(tm_code_retour), intent(out)                    ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

real(pm_reel) :: a_tmp, e_tmp, i_tmp           ! valeurs temporaires des parametres kepleriens
real(pm_reel) :: un_sur_e, jfrac                      ! variables intermediaires
real(pm_reel) :: cosamo,sinamo,amo,sinopb,cosopb,opb  ! angles intermediaires
real(pm_reel) :: B                             ! point d'impact sur Bplane
real(pm_reel) :: eps100,  eps_parab            ! variable de comparaison
type(tm_code_retour) :: code_retour_local    

intrinsic cos,sin,sqrt,acos

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mv_Bplane_kep.f90: derniere modification V5.15 >'

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

! convention du B-plane pour le cas d'une declinaison a +-Pi/2
if ((abs(abs(Bplane%DECI) - pm_pi_sur2) < eps100).and.(abs(Bplane%RAI-pm_pi_sur2) > eps100))  then
   code_retour%valeur = pm_err_bplane_decli_pisur2
   go to 6000
end if

! verification du point d'impact
if (abs(Bplane%BT) < eps100) then
   code_retour%valeur = pm_err_bplane_BT_nulle
   go to 6000
end if
B = sqrt(Bplane%BR**2+Bplane%BT**2)
   
! Calcul des parametres
! =====================

! demi_grand_axe

a_tmp = mu/Bplane%C3
kep%a = a_tmp

! excentricite

e_tmp = sqrt((B**2*Bplane%C3**2/mu**2)+1._pm_reel)
kep%e = e_tmp
if (kep%e < (1._pm_reel+ eps_parab)) then        ! ce n'est pas une hyperbole
   code_retour%valeur = pm_err_e_non_hyperb
   go to 6000
end if
un_sur_e = 1._pm_reel / e_tmp

! inclinaison (dans [0,Pi])

i_tmp = acos(cos(Bplane%DECI)*Bplane%BT/B)
kep%i = i_tmp

! ascension droite

if ((abs(kep%i) < eps100).or.(abs(kep%i-pm_pi) < eps100)) then

   kep%gom = 0._pm_reel

else    ! Cas ou i/=0 et i/=Pi

   sinamo = sin(Bplane%DECI)/sin(i_tmp)*Bplane%BT/B
   cosamo = Bplane%BR/(B*sin(i_tmp))
   call mu_angle2 (cosamo, sinamo, amo, code_retour_local)
   if (code_retour_local%valeur /= pm_OK)  then
      code_retour%valeur = code_retour_local%valeur
      if (code_retour_local%valeur < pm_OK)  go to 6000
   end if
   
   kep%gom = Bplane%RAI - amo
   
end if

! argument du periastre

sinopb = sinamo*B/Bplane%BT
cosopb = cosamo*cos(Bplane%DECI)
call mu_angle2 (cosopb, sinopb, opb, code_retour_local)
if (code_retour_local%valeur /= pm_OK)  then
   code_retour%valeur = code_retour_local%valeur
   if (code_retour_local%valeur < pm_OK)  go to 6000
end if

kep%pom = opb - acos(un_sur_e)

! anomalie moyenne

kep%M = M

! calcul de la date de passage

jfrac = (Bplane%LTF + a_tmp**(1.5_pm_reel)/sqrt(mu)*(log(e_tmp)+M))/86400._pm_reel
call md_jourfrac_joursec(jfrac, date, code_retour_local)   ! pas d'erreur possible

6000 continue

code_retour%routine = pm_num_mv_Bplane_kep
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mv_Bplane_kep
