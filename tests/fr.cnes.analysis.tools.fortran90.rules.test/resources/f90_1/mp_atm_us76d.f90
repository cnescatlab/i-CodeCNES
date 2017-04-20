subroutine mp_atm_us76d (delta_t,alt,dens,code_retour,delta_dens,vit_son,temp,pres,visco)
! (C) Copyright CNES - MSLIB - 2001-2005

!************************************************************************
!
! But:  Modele d'atmosphere US 76 avec dispersions possibles en temperature ou en densite
! ===
!
! Note d'utilisation:  Ce modele d'atmosphere est limite a des:
! ==================            altitudes: 0<alt<1000 km  
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 2.0 : creation a partir de rien
!                         (Date: 09/2001 - Realisation: Mickael Hazak)
!   + Version 3.1 (DE globale 4) : Modifications suite aux remarques qualite ATV
!                         (Date: 07/2003 - Realisation: Bruno Revelin)
!   + Version 5.2 (FA 1) : probleme de save dans mpi_IO_e_us76
!                         (Date: 03/2005 - Realisation: Guylaine Prat)
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

use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel),dimension(0:7),intent(in)   ::delta_t ! valeur des deltaTM a ajouter aux 8 valeur de TM

real(pm_reel),intent(in)                   :: alt          ! altitude
real(pm_reel),intent(out)                  :: dens         ! densite atmospherique
type(tm_code_retour), intent(out)          :: code_retour 
real(pm_reel), intent(in),  optional       :: delta_dens   ! delta pour la variation de densite atmospherique
real(pm_reel), intent(out), optional       :: vit_son      ! vitesse du son
real(pm_reel), intent(out), optional       :: temp         ! temperature
real(pm_reel), intent(out), optional       :: pres         ! pression
real(pm_reel), intent(out), optional       :: visco        ! viscosite dynamique
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================
integer      :: i                ! indice 
real(pm_reel),dimension(0:7),save :: delta_t_ref=1.e6_pm_reel   ! ecart de reference

! Declarations pour l'appel a la routine IOLIB fortran 77 mpi_IO_e_us76
integer      :: init      ! entier indiquant si on reinitialise les tables de temperature/pression (oui si !=0)
real(pm_reel):: zgeo,temp_IO_e_us76,ro,vson   ! altitude, temperature, densite, vitesse du son
real(pm_reel):: dro,pres_IO_e_us76,xmu        ! diff de densite / au profil nominal, pression, viscosite dynamique
intrinsic present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
         '@(#) Fichier MSPRO mp_atm_us76d.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ===============

code_retour%valeur = pm_OK
zgeo = alt

! verification des arguments d'entree
! ===================================

if (zgeo > 1000.e3_pm_reel)  then ! altitude superieure a 1000 km

   code_retour%valeur = pm_err_alt_sup1000km
   go to 6000

end if

if (zgeo <0._pm_reel) then ! altitude negative

   code_retour%valeur = pm_warn_alt_negatif
   zgeo = 0._pm_reel

end if

! calcul du modele atmospherique us76d
! ======================================

! pour les unites des donnees en entre et en sortie: 
! utilisation des commentaires en debut de code de mpi_IO_e_us76

do i = 0,7
   if ( (abs(delta_t_ref(i)-delta_t(i)) < 1.e-3_pm_reel) ) then ! si ecart sur les delta_t inferieur au millieme, 
      init=0                                                    ! pas de reinitialisation des tables.  
   else
      init=1
      go to 100
   end if
end do

100 continue
delta_t_ref(0:7)=delta_t(0:7) ! mise en memoire pour l'appel suivant

if (.not.present(delta_dens))then
   dro=0._pm_reel
else 
   dro=delta_dens
end if

call mpi_IO_e_us76 (init,zgeo,delta_t,dro,temp_IO_e_us76,pres_IO_e_us76,ro,vson,xmu) ! pas de code retour en sortie

! affectation des sorties
! =======================

dens = ro
if (present(vit_son)) vit_son = vson
if (present(temp)) temp = TEMP_IO_e_us76
if (present(pres)) pres = PRES_IO_e_us76
if (present(visco)) visco= xmu

6000 continue

code_retour%routine = pm_num_mp_atm_us76d
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mp_atm_us76d
