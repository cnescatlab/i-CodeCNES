subroutine mzpro_traiter_retour (code_retour, chaine_libre, fichier_edition, message_warn, stop_err, stop_warn, stop_pb)

! (C) Copyright CNES - MSPRO - 2000-2004

!************************************************************************
!
! But: Routine permettant a l'utilisateur de traiter les codes retour de 
! ===  la MSPRO, et ceux de la MSLIB Fortran 90.
!      
!
! Note d'utilisation:
! ==================
!    Les valeurs par defaut sont:
!                      message_warn est TRUE par defaut
!                      stop_err est TRUE par defaut
!                      stop_warn est FALSE par defaut
!                      stop_pb est FALSE par defaut
!                      chaine_libre est une chaine non affichee par defaut
!                      fichier_edition est la sortie standard par defaut
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 0.1 : creation 
!        (Date: 10/2000 - Realisation: Veronique Lepine et Guylaine Prat)
!   + Version 2.0 (DE 1) : Evolution appel a mzpro_code_retour (sortie message)
!                          et affichages
!                          Passage au standard MSPRO 90.
!        (Date: 02/2002 - Realisation: Guylaine Prat)
!   + Version 3.0 (DE globale 2) : passage au standard pre-mslib90.
!        (Date: 01/2003 - Realisation: Guylaine Prat)
!   + Version 3.1 (DE globale 4) : Modifications suite aux remarques qualite ATV
!        (Date: 07/2003 - Realisation: Bruno Revelin)
!   + Version 5.2 (FA 1) : Remplacement des formats * par '(a)' pour les write
!        (Date: 12/2004 - Realisation: Bruno Revelin)
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
use valeur_code_retour_mspro
use numero_routine_mspro
use type_mspro
use parametre_mspro

use int_codes_retour, only : mzpro_code_retour

use mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_code_retour), intent(in)                       :: code_retour     ! code retour de la routine posant probleme

character(len = pm_chaine_libre), intent(in), optional :: chaine_libre    ! message utilisateur

integer, intent(in), optional                          :: fichier_edition ! numero d'unite logique du fichier d'impression des messages

logical, intent(in), optional                          :: message_warn    ! indicateur d'affichage des avertissements

logical, intent(in), optional                          :: stop_err        ! indicateur d'arret en cas d'erreur

logical, intent(in), optional                          :: stop_warn       ! indicateur d'arret en cas de warning

logical, intent(in), optional                          :: stop_pb         ! indicateur d'arret si la routine mzpro_traiter_retour connait elle-meme un probleme.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

! Arguments de mzpro_code_retour
! ..............................
! code_retour est une entree de la routine mzpro_traiter_retour

character(len=pm_nom_biblio)  :: nom_biblio   ! nom de la bibliotheque a  laquelle la routine appartient
character(len=pm_nom_routine) :: nom_routine  ! nom de la routine ayant emis le code retour
character(len=pm_identification_routine)   :: identification_routine    ! fonction de cette routine
character(len=pm_signification_code_retour):: signification_code_retour ! signification du code retour
type(tm_code_retour)                       :: retour_mzpro              ! code retour de mzpro_code_retour
character(len=pm_message)                  :: message                   ! signification du champ message
character(len=pm_numero_version)           :: version_MSLIB90 ! numero de version MSLIB90

character(len=pm_numero_version)           :: version_MSPRO   ! numero de version MSPRO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Variables locales
! .................

logical :: arret        ! indicateur general d'arret d'execution selon choix utilisateur
logical :: erreur_mzpro ! indicateur que l'erreur provient d'un decodage du code retour

! Pour second appel a mzpro_code_retour (si probleme lors du premier appel)
character(len=pm_nom_biblio) :: sansobjet1 ! variable inutile ici (sortie obligatoire)
character(len=pm_nom_routine):: sansobjet2 ! variable inutile ici (sortie obligatoire)
character(len=pm_identification_routine)::sansobjet3 ! variable inutile ici (sortie obligatoire)
character(len=pm_signification_code_retour) :: message_mzpro ! message affiche si probleme de decodage du code retour dans mzpro_code_retour
type(tm_code_retour) :: retour_mzpro_sansobjet ! variable inutile ici (sortie obligatoire)

logical :: affichage          ! indicateur d'affichage 
logical :: affichage_warning  ! indicateur d'affichage des warnings
logical :: arret_si_erreur    ! indicateur d'arret en cas d'erreur
logical :: arret_si_warning   ! indicateur d'arret en cas de warning
logical :: arret_sur_pb       ! indicateur d'arret si la routine elle meme connait un probleme
integer :: fichier_sortie     ! unite logique du fichier de sortie
integer :: status             ! indicateur pour le write

intrinsic trim, present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSPRO mzpro_traiter_retour.f90: derniere modification V5.15 >'

!************************************************************************

! traitement du cas pm_OK
! =======================

if (code_retour%valeur == pm_OK) go to 6000 ! sortie immediate, pour gain en temps calcul

! initialisations des valeurs par defaut
! ======================================

if (.not.present(fichier_edition)) then
   fichier_sortie = pm_sortie_standard
else 
   fichier_sortie = fichier_edition
end if

if (.not.present(message_warn)) then
   affichage_warning    = .true.
else
   affichage_warning    = message_warn
end if

if (.not.present(stop_err)) then
   arret_si_erreur   = .true.
else
   arret_si_erreur   = stop_err
end if

if (.not.present(stop_warn)) then
   arret_si_warning   = .false.
else
   arret_si_warning   = stop_warn
end if

if (.not.present(stop_pb)) then
   arret_sur_pb     = .false.
else
   arret_sur_pb     = stop_pb
end if

affichage = .true.
arret = .false. 

! verification en fonction de la valeur du code retour 
! et reaffectation si besoin de affichage et arret
! ....................................................

if (code_retour%valeur > pm_OK) then  ! Cas du warning
   if (.not.affichage_warning) affichage = .false.
   if (arret_si_warning)       arret = .true.
end if

if (code_retour%valeur < pm_OK) then  ! Cas de l'erreur
   if (arret_si_erreur)        arret = .true.
end if

! Traitement de l'affichage
! =========================

if (affichage) then

   ! Recuperation des chaines de caracteres contenant le nom de la routine, sa fonction 
   ! et la signification du code retour
   ! ...................................................................................

   call mzpro_code_retour(code_retour, nom_biblio, nom_routine, identification_routine, &
                          signification_code_retour, retour_mzpro, message = message, &
                          version_MSLIB90 = version_MSLIB90, version_MSPRO = version_MSPRO)

   if (retour_mzpro%valeur /= pm_OK) then ! probleme lors de l'appel a mzpro_code_retour

      if (arret_sur_pb) arret = .true.
      erreur_mzpro = .true.

      ! appel a mzpro_code_retour avec le code retour "retour_mzpro"
      call mzpro_code_retour(retour_mzpro, &
           nom_biblio = sansobjet1, &             ! sortie non utilisee dans ce cas de figure
           nom_routine = sansobjet2, &            ! sortie non utilisee dans ce cas de figure
           identification_routine = sansobjet3, & ! sortie non utilisee dans ce cas de figure
           signification_code_retour = message_mzpro, &
           retour_mzpro = retour_mzpro_sansobjet)
  
      ! on ne teste pas ici : retour_mzpro_sansobjet  car le code retour retour_mzpro est forcement connu

   else ! pas de probleme lors de l'appel a mzpro_code_retour

      erreur_mzpro = .false.

   end if

   ! Affichage proprement dit
   ! ........................

   write(fichier_sortie,FMT='(a,a)',IOSTAT=status,ERR=5000) &
        'Bibliotheque a laquelle la routine appartient : ',trim(nom_biblio)

   write(fichier_sortie,FMT='(a,a,a,i5)',IOSTAT=status,ERR=5000) &
        'Routine : ', trim(nom_routine),' , numero : ', code_retour%routine 

   write(fichier_sortie,FMT='(a,a)',IOSTAT=status,ERR=5000) &
        'Role de la routine : ', trim(identification_routine)

   write(fichier_sortie,FMT='(a,i5)',IOSTAT=status,ERR=5000) &
        'Valeur du code retour : ', code_retour%valeur

   write(fichier_sortie,FMT='(a,a)',IOSTAT=status,ERR=5000) &
        'Signification du code retour : ', trim(signification_code_retour)

   write(fichier_sortie,FMT='(a,a)',IOSTAT=status,ERR=5000) &       ! a ce stade %message est affecte car
        'Message produit par la routine : ', trim(message)    ! %valeur est different de pm_OK

   write(fichier_sortie,FMT='(a,a,a,a,a)',IOSTAT=status,ERR=5000) &
        '(Version des binaires utilises : ', &
        trim(version_MSLIB90),' et ' , trim(version_MSPRO), ' )'

   if (present(chaine_libre)) then ! affichage si chaine_libre presente uniquement

      write(fichier_sortie,FMT='(a)',IOSTAT=status,ERR=5000) trim(chaine_libre)

   end if

   if (erreur_mzpro) then ! affichage si probleme lors de l'appel a mzpro_code_retour

      write(fichier_sortie,FMT='(a)',IOSTAT=status,ERR=5000) &
           'Probleme lors de l''appel a la routine mzpro_code_retour, ',&
          &'avec le code retour dont la signification est la suivante:'

      write(fichier_sortie,FMT='(a)',IOSTAT=status,ERR=5000) trim(message_mzpro)

   end if ! fin test sur erreur_mzpro

   5000 continue  ! cas d'un probleme lors de l'affichage

   if (status /= 0) then ! erreur d'ecriture: affichage sur la sortie standard
                         ! En particulier: le fichier donne pour la sortie pose probleme.

      write(pm_sortie_standard) &
           'Erreur d'' ecriture dans la routine mzpro_traiter_retour de la bibliotheque MSPRO.'

   end if ! fin test sur status

end if ! fin test sur affichage

! Traitement du cas d'arret
! =========================

if (arret) stop

! Sortie de la routine
! ====================

6000 continue

end subroutine mzpro_traiter_retour
