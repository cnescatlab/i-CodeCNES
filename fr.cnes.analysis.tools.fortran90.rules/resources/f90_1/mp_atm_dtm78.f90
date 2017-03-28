subroutine mp_atm_dtm78 (date,flux_veille,flux_3rot,ap_3h,lat,alt,heure_sol, &
                          dens,code_retour,inv_haut_ech)
! (C) Copyright CNES - MSLIB - 2001-2003

!************************************************************************
!
! But:  Modele d'atmosphere DTM78.
! ===
!
! Note d'utilisation:  Ce modele d'atmosphere est limite a des:
! ==================             altitude > 120 km 
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 2.0 : creation a partir de rien
!                         (Date: 09/2001 - Realisation: Mickael Hazak)
!   + Version 3.0 (FA 1 ed01 rev00) : renommage de commons au niveau des sources 77
!                         (Date: 02/2003 - Realisation: Guylaine Prat)
!   + Version 3.1 (DE globale 4) : Modifications suite aux remarques qualite ATV
!                         (Date: 07/2003 - Realisation: Bruno Revelin)
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

type(tm_jour_sec), intent(in)           :: date         ! date julienne 1950
real(pm_reel), intent(in)               :: flux_veille  ! flux solaire du jour precedent
real(pm_reel), intent(in)               :: flux_3rot    ! flux solaire moyen sur les 3 dernieres rotations solaires
real(pm_reel), intent(in)               :: ap_3h        ! indice geomagnetique ap des 3 heures precedentes                                         
real(pm_reel), intent(in)               :: lat          ! latitude geodesique
real(pm_reel), intent(in)               :: alt          ! altitude geodesique
real(pm_reel), intent(in)               :: heure_sol    ! heure solaire locale
real(pm_reel), intent(out)              :: dens         ! densite atmospherique
type(tm_code_retour), intent(out)       :: code_retour
real(pm_reel), intent(out), optional    :: inv_haut_ech ! inverse de la hauteur d'echelle

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

! Declarations pour l'appel a la routine ZOOM fortran 77 mpi_zoom_MDTM
integer                     :: JJ                  ! date en jour julien CNES
real(pm_reel)               :: SEC                     ! et secondes dans le jour
real(pm_reel)               :: F,FBAR              ! flux solaire sur llambda=10.7cm , et moyen
real(pm_reel)               :: Z,TLOC,TZ           ! altitude, heure loc, temperature exospherique
real(pm_reel)               :: RHO,DM,ECH          ! masse volumique, moyenne, inverse de la hauteur d'echelle
real(pm_reel)               :: AP                  ! indice geomagnetique planetaire
logical                     :: erreur              ! code d'erreur

intrinsic present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
         '@(#) Fichier MSPRO mp_atm_dtm78.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ===============

code_retour%valeur = pm_OK

! verification des arguments d'entree
! ===================================

if ((flux_veille < 0._pm_reel) .OR. (flux_3rot < 0._pm_reel)) then ! flux solaire negatif

   code_retour%valeur = pm_err_flux_sol_negatif
   go to 6000

end if

if (alt < 120.e3_pm_reel) then ! altitude inferieure a 120 km

   code_retour%valeur = pm_err_alt_inf120km
   go to 6000

end if

! pas de normalisation de date faites ici 
! compte tenu de l'utilisation jusqu'a present de mpi_zoom_MDTM
if (date%jour < 0) then

   code_retour%valeur = pm_err_jul1950_negatif
   go to 6000

end if

if (date%jour > 54787) then ! 54787 = 01/01/2100 a 00:00:00

   code_retour%valeur = pm_err_jul1950_sup2099
   go to 6000

end if

! calcul du modele atmospherique DTM78
! ======================================

! pour les unites des donnees en entre et en sortie: 
! utilisation des commentaires en debut de code de mpi_zoom_MDTM

JJ = date%jour
SEC = date%sec
F = flux_veille
FBAR = flux_3rot
AP  = ap_3h
z = alt
TLOC = heure_sol

call mpi_zoom_MDTM (JJ,SEC,F,FBAR,AP,Z,LAT,TLOC,RHO,TZ,DM,ECH,ERREUR)

if (erreur) then ! erreur vaut .false. si tout est OK

! les valeurs non nulles possibles pour le code retour de mpi_zoom_MDTM sont 
! normalement deja traitees avant l'appel

   code_retour%valeur = pm_err_ZOOM
   go to 6000 ! pas d'affectation des sorties

end if

! affectation des sorties
! =======================

dens = RHO

if (present(inv_haut_ech)) inv_haut_ech = ECH

6000 continue

code_retour%routine = pm_num_mp_atm_dtm78
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mp_atm_dtm78
