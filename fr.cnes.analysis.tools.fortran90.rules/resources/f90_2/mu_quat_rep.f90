subroutine mu_quat_rep (vect1, quat, vect2, code_retour)

  ! (C) Copyright CNES - MSLIB - 2000

  !************************************************************************
  !
  ! But: A l'aide d'un QUATernion, calcul de changement de REPere.
  ! ===
  !
  ! Note d'utilisation: Sans objet 
  ! ==================
  !
  !$Historique
  ! ==========
  !   + Version 3.0 (SP 427 ed01 rev00): creation a partir de la routine MUQUATROT de la MSLIB f77
  !                         (Date: 09/2000 - Realisation: Veronique Lepine)
  !   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
  !                         (Date: 04/2001 - Realisation: Guylaine Prat)
  !   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
  !                   (Date: 10/2006 - Realisation: Atos origin)
  !   + Version 6.8 : DM-ID 859 : remplacement des boucles implicites par des boucles explicites
  !                   (Date: 03/2008 - Realisation: Atos origin)
  !   Revision 362 2013/02/15 bbjc
  !   DM-ID 1513: Suppression des warnings de compilation
  !
  !************************************************************************

  ! Modules
  ! =======
use int_utilitaires, only : mu_quat_norme
use int_utilitaires, only : mu_prod_quat
use int_utilitaires, only : mu_quat_conjug

  use precision_mslib
  use type_mslib
  use valeur_code_retour_mslib
  use numero_routine_mslib
  use longueur_chaine_mslib

  ! Declarations
  ! ============
  implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3), intent(in)    ::  vect1 ! coordonnees du vecteur dans le repere R1
type(tm_quat), intent(in)                  ::  quat  ! quaternion de la transformation R1 -> R2
real(pm_reel), dimension(3), intent(out)   ::  vect2 ! coordonnees du vecteur dans le repere R2
type(tm_code_retour), intent(out)          ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Autres declarations
  ! ===================

  real(pm_reel) :: norme                              ! norme du quaternion d'entree
  type(tm_quat) :: quat_norme, quat_vect1, quat_prod1 ! quaternion d'entree norme, quaternion associe au vecteur
                                                      ! en entree, et produit de ces deux quaternions
  type(tm_quat) :: quat_norme_conjug                  ! quaternion conjugue du quaternion d'entree norme
  type(tm_quat) :: quat_vect2                         ! quaternion associe au vecteur en sortie
  integer       :: ii                                 ! indice de boucle

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSLIB mu_quat_rep.f90: derniere modification V6.13 >'

  ! Ne pas toucher a la ligne suivante
  character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mu_quat_rep.f90 362 2013-02-15 18:01:28Z bbjc $ '

  !************************************************************************

  ! initialisations
  ! ===============

  ! initialisation de la valeur du code retour

  code_retour%valeur = pm_OK

  call mu_quat_norme(quat, quat_norme, norme, code_retour) ! normalisation du quaternion
  if (code_retour%valeur /= pm_OK) then       ! erreur dans le cas d'un quaternion a norme nulle
     if (code_retour%valeur /= pm_err_quat_nul ) then
        code_retour%valeur = pm_err_valid     ! probleme dans la MSLIB90
     end if
     go to 6000
  end if

  ! Affectation du quaternion associe au vecteur en entree
  quat_vect1%q0 = 0._pm_reel
  do ii = 1,3
     quat_vect1%q123(ii) = vect1(ii)
  end do

  ! Calcul du quaternion resultat
  call mu_prod_quat(quat_vect1, quat_norme, quat_prod1, code_retour) ! les codes retour sont toujours OK
  call mu_quat_conjug (quat_norme, quat_norme_conjug, code_retour)
  call mu_prod_quat(quat_norme_conjug, quat_prod1, quat_vect2, code_retour)

  do ii = 1,3
     vect2(ii) = quat_vect2%q123(ii) ! Calcul du vecteur issu du quaternion resultat
  end do

6000 continue

  code_retour%routine = pm_num_mu_quat_rep
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_quat_rep
