subroutine mu_quat_norme (quat, quat_norme, norme, code_retour)

  ! (C) Copyright CNES - MSLIB - 2000

  !************************************************************************
  !
  ! But:  Normalisation d'un QUATernion apres calcul de sa NORME  
  ! ===
  !
  ! Note d'utilisation: Sans objet
  ! ==================
  !
  !$Historique
  ! ==========
  !   + Version 3.0 (SP 424 ed01 rev00): creation a partir de la routine MUQNORME de la MSLIB f77
  !                         (Date: 09/2000 - Realisation: Veronique Lepine)
  !   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
  !                         (Date: 04/2001 - Realisation: Guylaine Prat)
  !   + Version 6.3 (DM-ID 239) : Performances en temps de calcul
  !                 (Date: 10/2005 - Realisation: ATOS ORIGIN) 
  !   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
  !                   (Date: 10/2006 - Realisation: Atos origin) 
  !   + Version 6.8 : DM-ID 859 : remplacement des boucles implicites par des boucles explicites
  !                   (Date: 03/2008 - Realisation: Atos origin)
  !   Revision 362 2013/02/15 bbjc
  !   DM-ID 1513: Suppression des warnings de compilation
  !************************************************************************

  ! Modules
  ! =======
use int_util_internes, only : mui_dot_product4
  
  use precision_mslib
  use type_mslib
  use valeur_code_retour_mslib
  use numero_routine_mslib
  use longueur_chaine_mslib

  ! Declarations
  ! ============
  implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_quat), intent(in)                            ::  quat        ! quaternion (q0, q1, q2, q3)
type(tm_quat), intent(out)                           ::  quat_norme  ! quaternion norme
real(pm_reel), intent(out)                           ::  norme       ! norme du quaternion
type(tm_code_retour), intent(out)                    ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Autres declarations
  ! ===================

  real(pm_reel), dimension(4)  :: vect_quat    ! affectation a un vecteur du quaternion en entree  
  real(pm_reel)                :: presque_zero ! plus petit nombre machine
  real(pm_reel)                :: norme_locale ! affectation intermediaire de la norme
  real(pm_reel)                :: prod_scal    ! produit scalaire
  real(pm_reel)                :: retour
  integer                      :: ii           ! indice de boucle

  intrinsic tiny, sqrt

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSLIB mu_quat_norme.f90: derniere modification V6.13 >'

  ! Ne pas toucher a la ligne suivante
  character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mu_quat_norme.f90 362 2013-02-15 18:01:28Z bbjc $ '

  !************************************************************************

  ! initialisations
  ! ===============

  ! initialisation de la valeur du code retour

  code_retour%valeur = pm_OK

  ! Autres initialisations:

  ! vecteur intermediaire
  vect_quat(1)  = quat%q0
  do ii = 1,3
     vect_quat(1+ii)= quat%q123(ii)
  end do

  presque_zero = tiny(1._pm_reel)         ! plus petit reel machine

  call mui_dot_product4 ( vect_quat , vect_quat , prod_scal , retour )

  norme_locale = sqrt ( prod_scal )       !calcul de la norme du quaternion

  !test du cas ou la norme est nulle

  if (norme_locale <= presque_zero) then
     code_retour%valeur = pm_err_quat_nul
     go to 6000
  end if

  !calcul du quaternion norme

  quat_norme%q0      = quat%q0/norme_locale
  do ii = 1,3
     quat_norme%q123(ii) = quat%q123(ii)/norme_locale
  end do

  ! affectation de la norme
  norme = norme_locale

6000 continue

  code_retour%routine = pm_num_mu_quat_norme
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_quat_norme
