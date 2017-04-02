subroutine md_lire_saut_tuc (fichier, nb_saut_tuc, date_saut_tuc, delta_saut_tuc, code_retour)

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But: Lire le fichier Saut de TUC 
! === 
!
! Note d'utilisation:  L'entree fichier est une chaine correspondant au
! ==================   nom complet (chemin+nom) du fichier a lire
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
!                         (Date: 10/2002 - Realisation: Michel Lagreca
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

use parametre_mspro
use valeur_code_retour_mspro
use numero_routine_mspro
use int_mwi_internes, only : mwi_chercher_fichier
use int_mwi_internes, only : mwi_alc_unite_logique
use int_mwi_internes, only : mwi_ouvrir_fichier
use int_mwi_internes, only : mwi_fermer_fichier
use int_dates_internes_mspro, only : mdi_lire_don_saut_tuc

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

character(len=*), intent(in)  :: fichier         ! nom du fichier saut de TUC (chemin+nom)
integer,          intent(out) :: nb_saut_tuc     ! nombre de sauts TUC                       
type(tm_jour_sec), dimension(pm_nb_max_saut_tuc), intent(out) :: date_saut_tuc  ! dates des sauts TUC
real(pm_reel),     dimension(pm_nb_max_saut_tuc), intent(out) :: delta_saut_tuc ! (TAI-TUC) pour ces dates
type(tm_code_retour), intent(out) :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

integer :: retour      ! code retour local
integer :: unific      ! unite logique du fichier

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO md_lire_saut_tuc.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! initialisation a 0 de l'unite logique avant affectation dynamique
! .................................................................
unific = 0

! rechercher la presence du fichier des sauts de TUC a partir de son nom
! ......................................................................
call mwi_chercher_fichier ( fichier, retour )
if(retour /= pm_OK) then
   code_retour%valeur = retour
   go to 6000
end if

! allouer une unite logique disponible pour ouvrir ce fichier
! ...........................................................
call mwi_alc_unite_logique (unific, retour )
if(retour /= pm_OK) then
   code_retour%valeur = retour
   go to 6000
end if

! ouvrir en lecture ce fichier pointeur au debut des donnees
! ..........................................................
call mwi_ouvrir_fichier ( fichier, unific, retour )
if(retour /= pm_OK) then
   code_retour%valeur = retour
   go to 6000
end if

! lire les donnees de ce fichier
! ..............................
call mdi_lire_don_saut_tuc( unific, nb_saut_tuc, &
     date_saut_tuc, delta_saut_tuc, retour )
if(retour /= pm_OK) then
   code_retour%valeur = retour
!  fermer (et desallouer l'unite) de ce fichier si la lecture a echoue
!  on ne retourne pas le compte rendu de la fermeture car le probleme
!  vient de la lecture des donnees.
!  ...................................................................
   call mwi_fermer_fichier ( unific, retour )
   go to 6000
end if

! fermer (et desallouer l'unite) de ce fichier apres lecture
! ..........................................................
call mwi_fermer_fichier ( unific, retour )
if(retour /= pm_OK) then
   code_retour%valeur = retour
   go to 6000
end if

6000 continue

code_retour%routine = pm_num_md_lire_saut_tuc
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine md_lire_saut_tuc
