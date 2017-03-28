subroutine mu_statis (nb_pt, nb_pt_max, pt, val_min, val_max, moy, moy_quadra, ecart_type, code_retour)

! (C) Copyright CNES - MSPRO - 2000-2004

!************************************************************************
!
! But:  Calcul STATIStiques sur un ensemble de points.
! ===
!
! Note d'utilisation:  . Le nombre de points nb_pt doit etre strictement positif
! ==================   . nb_pt_max > ou = nb_pt
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 1.0 : creation a partir de la routine MUSTATIS de la MSLIB f77
!                         (Date: 10/2000 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE globale 1) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 07/2001 - Realisation: Guylaine Prat)
!   + Version 3.1 (DE globale 4) : Modifications suite aux remarques qualite ATV
!                         (Date: 09/2003 - Realisation: Bruno Revelin)
!   + Version 5.2 (DE globale 13): suppression des dimensions implicites dans l'include arg_*.h 
!                         (Date: 12/2004 - Realisation: Bruno Revelin)
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
use valeur_code_retour_mspro
use numero_routine_mspro

use mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                              :: nb_pt     ! nombre de points
integer, intent(in)                              :: nb_pt_max ! taille du tableau de points
real(pm_reel), dimension(nb_pt_max), intent(in)  :: pt        ! tableau de points
real(pm_reel), intent(out)                 :: val_min   ! plus petit point du tableau
real(pm_reel), intent(out)                 :: val_max   ! plus grand point du tableau
real(pm_reel), intent(out)                 :: moy       ! moyenne arithmetique des points du tableau
real(pm_reel), intent(out)                 :: moy_quadra! moyenne quadratique des points du tableau
real(pm_reel), intent(out)                 :: ecart_type! ecart type
type(tm_code_retour), intent(out)          :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

real(pm_reel) :: rm, rmprec, rq, rqprec, rw, rwprec ! variables intermediaires de calcul
integer       :: ind

intrinsic sqrt, real

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSPRO mu_statis.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

! Verification sur les arguments
! ==============================

if (nb_pt < 1) then  !test sur le nombre de points
   code_retour%valeur = pm_err_nb_pt_inf1
   go to 6000
end if
if (nb_pt_max < nb_pt) then  !test sur la taille du tableau de points
   code_retour%valeur = pm_err_nb_pt_max
   go to 6000
end if

! initialisation des donnees

rm = pt(1)
rq = 0._pm_reel
rw = pt(1) * pt(1)
ind = 2
val_min = pt(1)
val_max = pt(1)

! boucle de calcul sur tous les {xi}

do while (ind <= nb_pt) 

   rmprec = rm
   rqprec = rq
   rwprec = rw
   rm = rmprec + (pt(ind) - rmprec) / real(ind,pm_reel)
   rq = rqprec + real((ind - 1),pm_reel) * (pt(ind) - rmprec)**2 / real(ind,pm_reel)
   rw = rwprec + pt(ind) * pt(ind)

   if (pt(ind) < val_min) then
      val_min = pt(ind)
   elseif (pt(ind) > val_max) then
      val_max = pt(ind)
   end if

   ind = ind + 1

end do

! calcul des moyennes et de l'ecart-type

moy= rm
ecart_type = sqrt(rq / real(nb_pt, pm_reel))
moy_quadra = sqrt(rw / real(nb_pt, pm_reel))

6000 continue

code_retour%routine = pm_num_mu_statis
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_statis
