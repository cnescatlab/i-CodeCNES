subroutine mu_quat_conjug (quat, quat_conjug, code_retour)

  ! (C) Copyright CNES - MSLIB - 2000

  !************************************************************************
  !
  ! But:  Calcul du QUATernion CONJUGue d'un quaternion donne
  ! ===
  !
  ! Note d'utilisation: Sans objet 
  ! ==================
  !
  !$Historique
  ! ==========
  !   + Version 3.0 (SP 425 ed01 rev00): creation a partir de la routine MUQCONJUG de la MSLIB f77
  !                         (Date: 09/2000 - Realisation: Veronique Lepine)
  !   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
  !                         (Date: 04/2001 - Realisation: Guylaine Prat)
  !   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
  !                   (Date: 10/2006 - Realisation: Atos origin)
  !   + Version 6.8 : DM-ID 859 : remplacement des boucles implicites par des boucles explicites
  !                   (Date: 03/2008 - Realisation: Atos origin)
  !   + Version 6.9 : DM-ID 1058 : Suppression des warnings G95
  !                   (Date: 09/2008 - Realisation: Atos origin)
  !   Revision 362 2013/02/15 bbjc
  !   DM-ID 1513: Suppression des warnings de compilation
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

type(tm_quat), intent(in)                            ::  quat         ! quaternion en entree
type(tm_quat), intent(out)                           ::  quat_conjug  ! quaternion conjugue correspondant
type(tm_code_retour), intent(out)                    ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Autres declarations
  ! ===================

  integer :: ii ! indice de boucle

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSLIB mu_quat_conjug.f90: derniere modification V6.13 >'

  ! Ne pas toucher a la ligne suivante
  character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mu_quat_conjug.f90 362 2013-02-15 18:01:28Z bbjc $ '

  !************************************************************************

  ! initialisations
  ! ===============

  ! initialisation de la valeur du code retour

  code_retour%valeur = pm_OK

  quat_conjug%q0      =  quat%q0
  do ii = 1,3
     quat_conjug%q123(ii) = -quat%q123(ii)
  end do

  code_retour%routine = pm_num_mu_quat_conjug
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_quat_conjug
