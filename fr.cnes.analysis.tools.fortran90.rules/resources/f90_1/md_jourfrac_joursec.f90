subroutine md_jourfrac_joursec (jourfrac, joursec, code_retour)

! (C) Copyright CNES - MSLIB - 1999-2003
!************************************************************************
!
! But:  conversion d'une quantite exprimee en jour fractionnaire en une  
! ===   quantite exprimee en jour et secondes dans le jour.  
!
!$Historique
! ==========
!   + Version 2.0 (SP 331 ed01 rev00): creation a partir de la routine MUCONVJJFJJS de la MSLIB f77
!                         (Date: 07/1999 - Realisation: Sylvain Vresk)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
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

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel),        intent(in)        :: jourfrac        ! quantite exprimee en jours fractionnaires
type(tm_jour_sec),    intent(out)       :: joursec         ! quantite exprimee en jours et secondes dans le jour
type(tm_code_retour), intent(out)       :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Declaration des variables locales
! ---------------------------------
type(tm_jour_sec) :: joursec_tampon ! pour les calculs intermediaires

intrinsic floor, real

! Declaration des initialisations
! -------------------------------
character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB md_jourfrac_joursec.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: md_jourfrac_joursec.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************
! initialisation de la valeur du code retour 
! ..........................................
code_retour%valeur = pm_OK

! calcul de la duree en jours et secondes dans le jour
! ....................................................
joursec_tampon%jour = floor(jourfrac)
joursec_tampon%sec  = (jourfrac - real(joursec_tampon%jour, kind=pm_reel)) * 86400._pm_reel

joursec%jour = joursec_tampon%jour
joursec%sec  = joursec_tampon%sec

code_retour%routine = pm_num_md_jourfrac_joursec
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine md_jourfrac_joursec
