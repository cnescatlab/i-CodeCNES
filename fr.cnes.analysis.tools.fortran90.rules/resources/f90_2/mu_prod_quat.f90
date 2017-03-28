subroutine mu_prod_quat (quat1, quat2, quat_prod, code_retour)

! (C) Copyright CNES - MSLIB - 2000

!************************************************************************
!
! But: Calcul du PRODuit de deux QUATernions 
! ===
!
! Note d'utilisation: Sans objet 
! ==================
!
!$Historique
! ==========
!   + Version 3.0 (SP 426 ed01 rev00): creation a partir de la routine MUQPROD de la MSLIB f77
!                         (Date: 09/2000 - Realisation: Veronique Lepine)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 6.3 (DM-ID 239) : Performances en temps de calcul
!                 (Date: 10/2005 - Realisation: ATOS ORIGIN) 
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.8 : DM-ID 859 : remplacement des boucles implicites par des boucles explicites
!                   (Date: 03/2008 - Realisation: Atos origin)
!   + Version 6.9 : DM-ID 1058 : Suppression des warnings G95
!                   (Date: 09/2008 - Realisation: Atos origin)
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
use int_utilitaires, only : mu_prod_vect
use int_util_internes, only : mui_dot_product3

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_quat), intent(in)                            ::  quat1         ! quaternion q1
type(tm_quat), intent(in)                            ::  quat2         ! quaternion q2
type(tm_quat), intent(out)                           ::  quat_prod     ! quaternion produit q1*q2
type(tm_code_retour), intent(out)                    ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

real(pm_reel), dimension(3)   ::   produit_vectoriel ! produit vectoriel des champs q123 respectifs 
                                                     ! des deux quaternions

real (pm_reel)  ::  prod_scal   ! produit scalaire
real (pm_reel)  ::  retour
integer         ::  ii          ! indice de boucle

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mu_prod_quat.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mu_prod_quat.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

call mui_dot_product3 ( quat1%q123 , quat2%q123 , prod_scal , retour )

quat_prod%q0 = quat1%q0 * quat2%q0 - prod_scal

call mu_prod_vect(quat1%q123, quat2%q123, produit_vectoriel, code_retour)

! Le code retour est toujours OK
do ii = 1,3
   quat_prod%q123(ii) = produit_vectoriel(ii) + quat1%q0 * quat2%q123(ii) + quat2%q0 * quat1%q123(ii)
end do

code_retour%routine = pm_num_mu_prod_quat
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_prod_quat
