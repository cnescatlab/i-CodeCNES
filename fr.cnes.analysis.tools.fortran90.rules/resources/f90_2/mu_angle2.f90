subroutine mu_angle2 (x, y, angle, code_retour)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  dans le plan, calcul d'un angle connaissant son cosinus et son sinus
! ===
!
! Note d'utilisation:  l'entree x = cosinus(angle) et l'entree y = sinus(angle)
! ==================
!
!$Historique
! ==========
!   + Version 0.1 (SP 145 ed01 rev00): creation a partir de la routine MUANGL de la MSLIB f77
!                         (Date: 01/1998 - Realisation: Guylaine Prat)
!   + Version 0.1.1 (DE globale 182 ed01 rev00): prise en compte de regles de codage
!                         (Date: 02/1998 - Realisation: Guylaine Prat)
!   + Version 1.0 (DE globale 217 ed01 rev00): prise en compte des modifications de nommage des
!                                              parametres mathematiques
!                         (Date: 07/1998 - Realisation: Veronique Lepine)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement du module math_mslib
!     par une sélection de int_constantes
!                   (Date: 05/2007 - Realisation: Atos origin)
!
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
use int_constantes, only : pm_pi,pm_deux_pi,pm_pi_sur2,pm_deg_rad,pm_rad_deg

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), intent(in)         :: x      ! coordonnee x du vecteur / cosinus de l'angle
real(pm_reel), intent(in)         :: y      ! coordonnee y du vecteur / sinus de l'angle
real(pm_reel), intent(out)        :: angle  ! angle calcule dans [0,2.pi[
type(tm_code_retour), intent(out) :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel) :: presque_zero, arctg_angle, s ! variables intermediaires de calcul

intrinsic abs,atan

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mu_angle2.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mu_angle2.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! initialisation constante de test
! ................................
presque_zero = tiny(1._pm_reel)  ! recherche du plus petit reel positif non nul

! Controle des parametres d'entree (x=y=0)
! ========================================

if ((abs(x) <= presque_zero) .and. (abs(y) <= presque_zero)) then
   code_retour%valeur = pm_err_vect_nul
   angle = 0._pm_reel
   go to 6000
end if

! Calcul arctg(s) avec s = min( | x | , | y |) / max( | x | , | y |)
! =====================================================================

if (abs(x) < abs(y)) then  ! si   | x | < | y | :  s = | x | / | y |  et  angle = pi/2 - arctg(s)

   s = abs(x) / abs(y)
   arctg_angle = pm_pi_sur2 - atan(s)

else   ! si | x | > | y | :   s = | y | / | x |  et angle = arctg(s)

   s = abs(y) / abs(x)
   arctg_angle = atan(s)

end if

! Determination signe et quadrant de l'angle suivant les signes de x et y
! =======================================================================

if (y >= 0._pm_reel) then    ! y > ou = 0

   angle = arctg_angle           ! si y >= 0 et x >= 0: angle = angle

   if (x < 0._pm_reel) then
      angle = pm_pi - arctg_angle   ! si y >= 0 et x < 0: angle = pi - angle
   end if

else    !  y < 0

   if (x > 0._pm_reel) then ! y < 0 et x > 0

      angle = pm_deux_pi - arctg_angle ! si y < 0 et x > 0: angle = 2pi - angle

   else ! y < 0 et x <= 0

      angle = pm_pi + arctg_angle      ! si y < 0 et x <= 0: angle = pi + angle

   end if

end if

6000 continue

code_retour%routine = pm_num_mu_angle2
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_angle2
