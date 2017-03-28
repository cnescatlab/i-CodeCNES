subroutine mdi_lire_don_saut_tuc (unific, nb_saut_tuc, date_saut_tuc, delta_saut_tuc, retour)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But: Lire les donnees du fichier Sauts de TUC 
! === 
!
! Note d'utilisation:  L'entree unific est un entier correspondant a
! ==================   l'unite logique du fichier a lire
!                      La sortie nb_saut_tuc fournit le nombre de sauts
!                      La sortie date_saut_tuc fournit la date en jour 
!                      Julien CNES (echelle de temps TAI) du saut du TUC
!                      (tableau de pm_nb_max_saut_tuc types tm_jour_sec)
!                      La sortie delta_saut_tuc fournit l'ecart entre les
!                      echelles de temps TAI et TUC aux dates "date_saut_tuc"
!                      (tableau de pm_nb_max_saut_tuc types reel)
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 3.0 : creation
!                         (Date: 10/2002 - Realisation: Michel Lagreca/Bruno Revelin)
!   + Version 3.1 (DE globale 4) : Modifications suite aux remarques qualite ATV
!                         (Date: 09/2003 - Realisation: Bruno Revelin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.10: DM-ID 1058 : Correction des warnings levés par g95
!                   (Date: 8/2008 - Realisation: Atos origin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use parametre_mspro
use valeur_code_retour_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)  :: unific          ! Unite logique associee au fichier sauts de TUC
integer, intent(out) :: nb_saut_tuc     ! Nombre de sauts TUC                       
type(tm_jour_sec), dimension(pm_nb_max_saut_tuc), intent(out) :: date_saut_tuc  ! dates des sauts TUC
real(pm_reel),     dimension(pm_nb_max_saut_tuc), intent(out) :: delta_saut_tuc ! ecarts (TAI-TUC) pour ces dates
integer, intent(out) :: retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

character(len=*), parameter :: format_lect_ligne='(A)'  ! format de lecture d'une chaine de carateres
character(len=*), parameter :: format_lect_saut_tuc='(I5,1X,I5,1X,F8.3)'
                         ! formats de lecture des lignes du fichier sauts de TUC
character(len=*), parameter :: diese='#'     ! caractere en 1ere colonne identifiant une ligne a eliminer

character(len=pm_chaine_libre) :: ligne      ! chaine de caracteres contenant une ligne entiere lue
integer            :: cr_lect    ! compte rendu d'une lecture
integer(pm_entier) :: jjd        ! date de debut de validite du saut
integer(pm_entier) :: jjf        ! date de fin de validite
real(pm_reel)      :: ect        ! ecart constate entre les deux echelles de temps
integer            :: nsaut      ! nombre local de sauts de tuc lus

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mdi_lire_don_saut_tuc.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! initialisation du compte rendu du read
! .................................................
cr_lect = pm_OK

! initialisation du nombre de sauts avant lecture
! ...............................................
nsaut = 0

! initialisation du tableau des dates et des ecarts a 0 avant lecture
! ...................................................................
date_saut_tuc(:)%jour = 0
date_saut_tuc(:)%sec = 0._pm_reel
delta_saut_tuc(:) = 0._pm_reel

! lecture du fichier en ne tenant pas compte des lignes debutant par le
! caractere "#" en 1ere colonne
! Le champ secondes de la date tm_jour_sec est arbitrairement initialise
! a 0._pm_reel car le fichier ne contient que le jour Julien entier.
! .....................................................................
do while (cr_lect == pm_OK)
   read(UNIT=unific, FMT=format_lect_ligne, IOSTAT=cr_lect) ligne
   if (cr_lect == pm_OK .and. ligne(1:1) /= diese) then
      read(ligne, FMT=format_lect_saut_tuc, IOSTAT=cr_lect) jjd, jjf, ect
      if (cr_lect == pm_OK) then
         nsaut = nsaut + 1
         if (nsaut > pm_nb_max_saut_tuc) then
            retour = pm_err_nb_saut_tuc_max
            go to 6000
         end if
         ! Le jour de fin d'intervalle doit etre posterieur au jour de debut
         if (jjf < jjd) then
            retour = pm_err_fic_saut_tuc
            go to 6000
         end if
         date_saut_tuc(nsaut)%jour = jjd
         date_saut_tuc(nsaut)%sec = 0._pm_reel
         delta_saut_tuc(nsaut) = ect
      end if
   end if
end do

! traitement du cas d'erreur de lecture
! Les proprietes du parametre IOSTAT permettent de s'affranchir 
! du parametre END, dand la gestion de sortie du READ (cf MPM Fortran90)
! .....................................
if (cr_lect > 0) then
   retour = pm_err_lire_don 
   go to 6000
end if

! affectation du nombre de sauts de TUC
! .....................................
nb_saut_tuc = nsaut

6000 continue

end subroutine mdi_lire_don_saut_tuc
