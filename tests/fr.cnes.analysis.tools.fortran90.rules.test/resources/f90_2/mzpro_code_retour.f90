subroutine mzpro_code_retour (code_retour, nom_biblio, nom_routine, identification_routine, &
     signification_code_retour, retour_mzpro, message, version_MSLIB90, version_MSPRO)

! (C) Copyright CNES - MSLIB - 2000

!************************************************************************
!
! But:  Cette routine donne la signification complete du code retour  
! ===   d'une routine MSPRO ou de la MSLIB Fortran 90.
!
! Note d'utilisation:   voir la documentation utilisateur
! ================== 
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 0.1 : creation
!        (Date: 10/2000 - Realisation: Veronique Lepine et Guylaine Prat)
!   + Version 1.0 (sans DE): ajout de routines et de codes retour
!        (Date: 02/2001 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE 1) : ajout de la sortie optionnelle message,
!                          utilisation du champ %biblio,
!                          changement du parameter pm_version_MSLIB en pm_version_MSLIB90
!                          et ajout de nouveaux themes, de nouvelles plages de code retour,
!                          de routines et de codes retour.
!        (Date: 03/2002 - Realisation: Guylaine Prat et Mickael Hazak)
!   + Version 2.0 (DE 2) : decoupage de la routine (via appel a des sous-routines)
!                          et redefinition totale de l'algorithme.
!                          Suppression des code retour pm_err_numero_routine_ng
!                          et pm_err_val_code_retour_ng.
!        (Date: 05/2002 - Realisation: Guylaine Prat)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)

!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use int_codes_retour_internes, only : mzipro_val_retour
use int_codes_retour_internes, only : mzipro_numero_routine

use valeur_code_retour_mspro
use numero_routine_mspro

use parametre_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_code_retour), intent(in)          :: code_retour ! code retour a analyser

character(len=pm_nom_biblio), intent(out) :: nom_biblio  ! nom de la bibliotheque a laquelle appartient 

character(len=pm_nom_routine), intent(out):: nom_routine ! nom de la routine ayant emis le code retour a analyser

character(len=pm_identification_routine), intent(out)    :: identification_routine ! role succinct de cette routine

character(len=pm_signification_code_retour), intent(out) :: signification_code_retour ! signification du champ

type(tm_code_retour), intent(out)                        :: retour_mzpro    ! code retour de mzpro_code_retour

character(len=pm_message), intent(out), optional         :: message         ! signification du champ

character(len=pm_numero_version), intent(out), optional  :: version_MSLIB90 ! version de la bibliotheque 

character(len=pm_numero_version), intent(out), optional  :: version_MSPRO   ! version de la bibliotheque 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
integer  :: routine, valeur  ! champs routine et valeur du code retour en entree

character(len=pm_signification_code_retour) :: chaine  ! Chaine intermediaire contenant la signification du code retour.

character(len=pm_nom_routine) :: chaine_nom  ! Chaine intermediaire contenant le nom de la routine.

character(len=pm_identification_routine) :: chaine_identification  ! Chaine intermediaire contenant l'identification de la routine.

! declaration pour appel a mz_numero_routine ou mz_val_code_retour
type(tm_code_retour)                        :: code_retour_mz_mslib    ! code retour (MSLIB90)
character(len=pm_nom_routine)               :: nom_mz_mslib            ! nom de la routine
character(len=pm_identification_routine)    :: identification_mz_mslib ! role succinct de la routine
character(len=pm_signification_code_retour) :: signification_mz_mslib  ! chaine indiquant la signification du code retour MSLIB 90

! declaration pour appel a mzipro_numero_routine ou mzipro_val_retour
integer                                     :: retour_mzi_mspro       ! code retour intermediaires des routines internes
character(len=pm_signification_code_retour) :: signification_mz_mspro ! chaine indiquant la signification du code retour MSPRO
character(len=pm_nom_routine)               :: nom_mz_mspro           ! nom de la routine
character(len=pm_identification_routine)    :: identification_mz_mspro ! role succinct de la routine

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSPRO mzpro_code_retour.f90: derniere modification V5.15 >'

intrinsic trim, abs, present

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour_mzpro%valeur  = pm_OK

! initialisation en cas de problemes 
! (pour valeurs inconnues %biblio, %routine ou %valeur)
! ..........................................
nom_routine = ''
identification_routine = ''
signification_code_retour = ''

! recuperation des valeurs a traiter
! .................................

routine = code_retour%routine
valeur  = code_retour%valeur

! affectation des numeros de version si demandes
! ..............................................
if (present(version_MSLIB90)) version_MSLIB90 = pm_version_MSLIB90
if (present(version_MSPRO))   version_MSPRO   = pm_version_MSPRO

! affectation de la sortie message
! ................................
if (present(message)) message = trim(code_retour%message) ! l'utilisateur n'a pas pu passer une chaine plus longue que pm_message
! Quoiqu'il arrive on affecte cette sortie car independante des autres

! ============================================
! Traitement de la MSLIB Fortran 90
! ============================================
if (code_retour%biblio == pm_mslib90) then

   nom_biblio = 'MSLIB Fortran 90' ! affectation appropriee

   ! Recherche des chaines liees au numero de la routine
   ! ---------------------------------------------------

   call mz_numero_routine (routine, nom_mz_mslib, identification_mz_mslib, code_retour_mz_mslib)

   if (code_retour_mz_mslib%valeur /= pm_OK) then 
  
      ! permet de traiter :
      ! * des routines inconnues de la MSLIB 90 (pm_err_numero_routine_inconnu)
      ! * des chaines trop longues (pm_err_valid)

      retour_mzpro%valeur = code_retour_mz_mslib%valeur
      if (retour_mzpro%valeur < pm_OK) nom_biblio = '' ! reaffectation appropriee
      go to 6000

   end if

   ! passage oblige par des variables intermediaires pour appel a mz_numero_routine
   ! (sinon probleme de surdimensionnenment)
   chaine_nom = nom_mz_mslib
   chaine_identification = identification_mz_mslib

   ! Recherche des chaines liees au numero de code retour
   ! ----------------------------------------------------
   call mz_val_code_retour (valeur, signification_mz_mslib , code_retour_mz_mslib)

   if (code_retour_mz_mslib%valeur /= pm_OK) then

      ! permet de traiter :
      ! * des codes retour inconnus de la MSLIB 90 (pm_err_val_code_retour_inconnu)
      ! * des chaines trop longues (pm_err_valid)

      retour_mzpro%valeur = code_retour_mz_mslib%valeur
      go to 6000

   end if

   ! passage oblige par variable intermediaire pour appel a mz_val_code_retour
   ! (sinon probleme de surdimensionnenment)
   chaine = signification_mz_mslib

! ============================================
! Traitement de la MSPRO
! ============================================
else if (code_retour%biblio == pm_mspro) then

   nom_biblio = 'MSPRO' ! affectation appropriee

   ! Recherche des chaines liees au numero de la routine
   ! ---------------------------------------------------

   call mzipro_numero_routine (routine, nom_mz_mspro, identification_mz_mspro, retour_mzi_mspro)

   if (retour_mzi_mspro /= pm_OK) then 
  
      ! permet de traiter :
      ! * des routines inconnues de la MSPRO (pm_err_numero_routine_inconnu)
      ! * des chaines trop longues (pm_err_valid)

      retour_mzpro%valeur = retour_mzi_mspro
      if (retour_mzpro%valeur < pm_OK) nom_biblio = '' ! reaffectation appropriee
      go to 6000

   end if

   ! passage oblige par des variables intermediaires pour appel a mzipro_numero_routine
   ! (sinon probleme de surdimensionnenment)
   chaine_nom = nom_mz_mspro
   chaine_identification = identification_mz_mspro

   ! Recherche des chaines liees au numero de code retour
   ! ----------------------------------------------------

   call mzipro_val_retour(valeur, signification_mz_mspro, retour_mzi_mspro)

   if (retour_mzi_mspro /= pm_OK) then

      if (retour_mzi_mspro == pm_err_valid)  then
         retour_mzpro%valeur = pm_err_valid
         signification_code_retour = ''
         go to 6000 ! probleme de chaine trop longue
                    ! (l'utilisateur ne doit pas passer par ce cas)
      end if

      if ((abs(valeur) > 1000).and.(abs(valeur) < 3000)) then ! code retour de la MSLIB 90 
                                            ! (pouvant etre utilise par des routines MSPRO)

         call mz_val_code_retour (valeur, signification_mz_mslib , code_retour_mz_mslib)

         if (code_retour_mz_mslib%valeur /= pm_OK) then 

            ! permet de traiter :
            ! * des codes retour inconnus de la MSLIB 90 (pm_err_val_code_retour_inconnu)
            ! * des chaines trop longues (pm_err_valid - a priori impossible ici)

            retour_mzpro%valeur = code_retour_mz_mslib%valeur
            go to 6000

         end if

         ! passage oblige par variable intermediaire pour appel a mz_val_code_retour
         ! (sinon probleme de surdimensionnenment)
         chaine = signification_mz_mslib

      else ! code retour inconnu de la MSLIB 90 et de la MSPRO

         retour_mzpro%valeur = pm_err_val_code_retour_inconnu
         chaine = ''
         go to 6000

      end if

   else ! valeur bien trouvee dans la MSPRO (retour = 0)

      ! passage oblige par variable intermediaire pour appel a mzipro_val_code_retour
      ! (sinon probleme de surdimensionnenment)
      chaine = signification_mz_mspro

   end if

! ============================================
! Traitement: bibliotheque non reconnue (mauvais champ %biblio)
! ============================================
else
 
   ! sortie directe (choix CNES)
   retour_mzpro%valeur = pm_err_biblio_inconnu
   nom_biblio = ''
   nom_routine = ''
   identification_routine = ''
   signification_code_retour = ''
   if (present(message)) message = ''
   go to 6000

end if ! fin du test sur code_retour%biblio

! ============================================
! Affectation des sorties finales
! ============================================

! Pour les chaines liees au numero de la routine
! ----------------------------------------------

nom_routine = trim(chaine_nom)
identification_routine = trim(chaine_identification)

! Pour la chaine liee au numero de code retour
! --------------------------------------------

signification_code_retour = trim(chaine)

6000 continue

retour_mzpro%routine = pm_num_mzpro_code_retour
retour_mzpro%biblio  = pm_mspro
if (retour_mzpro%valeur /= pm_OK) retour_mzpro%message = ' '

end subroutine mzpro_code_retour
