subroutine mr_obli_moy (model_prec, jul1950, obli_moy, code_retour, delta_tai, deriv1_obli, deriv2_obli)

! (C) Copyright CNES - MSLIB - 1998

!************************************************************************
!
! But:  Calcul de l'OBLIquite MOYenne.
! ===
!
! Note d'utilisation:  Le modele de precession disponible est celui de Lieske (epoque de base a J2000)
! ==================
!
!$Historique
! ==========
!   + Version 1.0 (SP 285 ed01 rev00): creation a partir de la routine MROBLI de la MSLIB f77
!                         (Date: 10/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE 364 ed01 rev00): suppression des tests sur les dates - utilisation de md_joursec_jourfrac/md_joursec_norme - ajout du parametre delta_tai en entree optionnelle
!                         (Date: 11/1999 - Realisation: Sylvain Vresk)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin, Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement des modules de constantes *_mslib 
!       par le module global parametre_mslib
!                   (Date: 05/2007 - Realisation: Atos origin)
!   + Version 6.9 : DM-ID 1058 : Suppression des warnings G95
!                   (Date: 09/2008 - Realisation: Atos origin)
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

use type_mslib

use int_dates, only : md_joursec_jourfrac
use int_dates, only : md_joursec_norme

use parametre_mslib


! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer , intent(in)                             :: model_prec   !  indicateur du modele de precession
type(tm_jour_sec), intent(in)                    :: jul1950      !  date julienne (jour, secondes) 
real(pm_reel) , intent(out)                      :: obli_moy     !  obliquite moyenne
type(tm_code_retour), intent(out)                :: code_retour
real(pm_reel), intent(in),  optional             :: delta_tai    ! ecart entre l'echelle de temps TAI et l'echelle de temps utilisee pour la date jul1950
real(pm_reel), intent(out), optional             :: deriv1_obli  !  derivee premiere
real(pm_reel), intent(out), optional             :: deriv2_obli  !  derivee seconde

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel)                 ::    rtemps        !     date en jour julien fractionnaire
real(pm_reel)                 ::    robli1,robli2 !     variables de travail

real(pm_reel), parameter      ::    rcderp = 3.168808781403e-10_pm_reel !     ... constantes utilisees dans les 
real(pm_reel), parameter      ::    rcders = 1.00413490931e-19_pm_reel  !         systemes de references

real(pm_reel), parameter      ::    delta_te_tai = 32.184_pm_reel       !     ... difference entre le temps des ephemerides et le tai

real(pm_reel), dimension(4), parameter  ::   &      !     ... tables des coefficients de Lieske
  rcoefL = (/0.40909280422233_pm_reel,-0.22696552481143e-03_pm_reel, -0.28604007185463e-08_pm_reel, 0.87896720385159e-08_pm_reel/)

type(tm_jour_sec)    :: joursec_tai                    ! variable temporaire
type(tm_jour_sec)    :: joursec_norme                  ! variable temporaire pour l appel a md_joursec_norme
type(tm_jour_sec)    :: jul1950_temp                   ! variable temporaire pour l appel a md_joursec_jourfrac
type(tm_code_retour) :: code_retour_local

intrinsic present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mr_obli_moy.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mr_obli_moy.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! controle des donnees
! ....................

if (model_prec /= pm_lieske) then  ! modele de precession inconnu
   code_retour%valeur = pm_err_ind_prec

else                              ! les entrees sont correctes : calcul de l'obliquite.

   joursec_tai%jour = jul1950%jour

   if (.not. present(delta_tai)) then

      joursec_tai%sec  = jul1950%sec               ! delta_tai est fixe a 0._pm_reel par defaut

   else

      joursec_tai%sec  = jul1950%sec + delta_tai   ! si delta_tai est precise par l'appelant

   end if

   call md_joursec_norme ( joursec_tai, joursec_norme, code_retour_local) ! code retour non teste car toujours = a 0

   !==========================================================
   ! date en siecles juliens
   ! conversion secondes tai en temps dynamique terrestre (tdt)
   !==========================================================
   jul1950_temp%jour = joursec_norme%jour
   jul1950_temp%sec  = joursec_norme%sec + delta_te_tai
   call md_joursec_jourfrac ( jul1950_temp, rtemps, code_retour_local )   ! code_retour_local%valeur = 0 est le seul retour possible => non teste
   rtemps = rtemps / 36525._pm_reel
   rtemps = rtemps - 0.5_pm_reel     ! selon le modele de Lieske

   ! ==================
   ! obliquite moyenne
   ! ==================

   robli1   = rcoefL(4)*rtemps + rcoefL(3)
   robli2   = robli1   *rtemps + rcoefL(2)
   obli_moy = robli2   *rtemps + rcoefL(1)

   ! =========
   ! derivees
   ! =========

   if (present(deriv1_obli)) then      !  calcul de la derivee premiere

      robli1      = 3._pm_reel*rcoefL(4)*rtemps + 2._pm_reel*rcoefL(3)
      deriv1_obli = ( robli1*rtemps + rcoefL(2) ) * rcderp

   end if

   if (present(deriv2_obli)) then      !  calcul de la derivee seconde

      deriv2_obli  = (6._pm_reel*rcoefL(4)*rtemps + 2._pm_reel*rcoefL(3))* rcders

   end if

end if

code_retour%routine = pm_num_mr_obli_moy
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mr_obli_moy
