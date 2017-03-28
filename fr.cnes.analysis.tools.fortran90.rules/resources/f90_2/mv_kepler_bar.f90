subroutine mv_kepler_bar (anom_M, e, anom_E_D, code_retour)

! (C) Copyright CNES - MSLIB - 1998

!************************************************************************
!
! But:  Resolution des equations de KEPLER (ellipse et hyperbole) et de BARker (parabole).
! ===
!
!$Historique
! ==========
!   + Version 1.0 (SP 223 ed01 rev00): creation a partir de la routine MVRYKN de la MSLIB f77
!                         (Date: 07/1998 - Realisation: Veronique Lepine)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin, Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
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
use test_mslib

use int_chgmnt_variables, only : mv_kepler_std
use int_var_internes, only : mvi_kepler_hyperb
use int_var_internes, only : mvi_barker

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), intent(in)                 :: anom_M   ! anomalie moyenne (M)
real(pm_reel), intent(in)                 :: e        ! excentricite (e)
real(pm_reel), intent(out)                :: anom_E_D ! anomalie excentrique (E) ou D = tangente[anomalie vraie/2] selon 
type(tm_code_retour), intent(out)         :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ------------------

real(pm_reel)    ::   D   !  tangente (anomalie vraie/2)

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mv_kepler_bar.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mv_kepler_bar.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! controle du parametre excentricite (e>0) 
! ------------------------------------------

if (e < 0._pm_reel) then    !   excentricite negative
   anom_E_D = 0._pm_reel
   code_retour%valeur = pm_err_e_negatif
   go to 6000
end if

if (e <= (1._pm_reel- pm_eps_parab)) then  !  cas elliptique : resolution de  E - e * sin(E) = M par appel a mv_kepler_std 
                                           !  en retour de cette routine, seules valeurs possibles du code_retour:
                                           !  0 ou pm_conv_kepler_ellip

   call mv_kepler_std( anom_M, e, anom_E_D, code_retour)
   
else if (e >= (1._pm_reel+ pm_eps_parab))  then    ! cas hyperbolique : resolution de  e * sinh(E) - E = M
   
   call mvi_kepler_hyperb(anom_M, e, anom_E_D, code_retour%valeur)

else      !        (e > (1._pm_reel- pm_eps_parab) .and. e < (1._pm_reel+ pm_eps_parab))
          ! cas parabolique :  resolution de l'equation de BARKER:    6 * M = 3 * D + D**3

   call mvi_barker(anom_M, D, code_retour%valeur)
   if (code_retour%valeur == pm_OK) code_retour%valeur = pm_warn_e_parab
   anom_E_D = D

end if

6000 continue

code_retour%routine = pm_num_mv_kepler_bar
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mv_kepler_bar
