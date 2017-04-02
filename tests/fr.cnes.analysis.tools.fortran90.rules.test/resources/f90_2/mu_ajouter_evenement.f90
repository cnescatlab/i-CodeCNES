subroutine mu_ajouter_evenement (integrateur, g_commut, max_deltat, eps_converg, &
     action, code_retour, ident_g_commut)
! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  Ajouter une subroutine signalant l'occurrence d'un evenement.
! ===
!
! Note d'utilisation:  
! ==================
!
! * L'architecture et le code ont ete recopies directement depuis l'architecture et le code
! de la bibliotheque Mantissa (http://www.spaceroots.org/software/mantissa/index.html), apres
! transcription du code Java en Fortran 90
! Mantissa est un produit libre developpe par Luc Maisonobe et diffuse
! sous une licence BSD modifiee autorisant cet emprunt et ces modifications.
!
!$Historique
! ==========
!  Revision 372  2013/02/20 ffsm
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 5.2 : creation
!                         (Date: 01/2005 - Realisation: Bruno Revelin)
!   + Version 5.4 : DM-ID 472 : Amelioration de la detection d evenement
!                         (Date: 02/2006 - Realisation: Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                         (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.6 : DM-ID 473 : Inhibition d'evenements
!                         (Date: 09/2006 - Realisation: Atos Origin)
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use type_mspro
use parametre_interne_mspro
use parametre_mspro

use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

interface
   subroutine g_commut(t,y,gval,retour)     ! subroutine de commutation
   
   use mslib
   
   real(pm_reel),intent(in)                        ::  t     ! abscisse a tester
   real(pm_reel),dimension(*),intent(in)           ::  y     ! vecteur d'etat a t
   real(pm_reel),intent(out)                       ::  gval  ! valeur de la fonction de commutation
   integer,      intent(out)                       ::  retour
   
   end subroutine g_commut
end interface
type(tm_integrateur), intent(inout)                  ::  integrateur         ! integrateur utilise
real(pm_reel),        intent(in)                     ::  max_deltat          ! intervalle max entre 2 tests de la fonction
real(pm_reel),        intent(in)                     ::  eps_converg         ! seuil de convergence
integer,              intent(in)                     ::  action              ! si commutation, action a effectuer
type(tm_code_retour), intent(out)                    ::  code_retour
integer,              intent(out), optional          ::  ident_g_commut       ! identificateur de la subroutine de commutation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

real(pm_reel) :: presque_zero  ! plus petit reel positif non nul
integer :: alloc_ok  ! retour de l'allocation dynamique
type(tm_g_commut),dimension(:),allocatable :: tab_tmp    ! tableau intermediaire de copie
!!$ DM_473
integer :: max_ident ! maximum des identifiants existants, pour rajouter une routine "a la suite"
integer :: i ! compteur

integer :: MsproConvertitPointeur          ! Adresse de la fonction sous forme d'entier
external MsproConvertitPointeur            ! Fonction fournissant cette adresse

intrinsic abs,tiny

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mu_ajouter_evenement.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK
max_ident = 0

! verifications
presque_zero = tiny(1._pm_reel)
if ((abs(max_deltat) < presque_zero).or.(abs(eps_converg) < presque_zero).or.&
     ((action < pm_STOP).and.(action > pm_CONTINUE)) ) then
   code_retour%valeur = pm_err_val_para
   go to 6000
end if

! DM_473 
! boucle sur toutes les routines existantes
! on récupère l'identifiant qui a la valeur maximale
do i=1, integrateur%nb_commut
   if (max_ident < integrateur%g_commut(i)%ident) max_ident = integrateur%g_commut(i)%ident
enddo


! conversion en entier de l'adresse de la subroutine 
! grace a une fonction C.

integrateur%nb_commut = integrateur%nb_commut +1
if (integrateur%nb_commut >= integrateur%size_commut) then 
   ! il faut (re)allouer le tableau
   if (integrateur%size_commut == 0) then
      ! 1ere fois
      integrateur%size_commut = pm_i_SIZE_COMMUT
      allocate(integrateur%g_commut(integrateur%size_commut), stat= alloc_ok)
      if (alloc_ok /= pm_OK) then ! l'allocation dynamique a echoue
         code_retour%valeur = pm_err_allocate
         go to 6000
      end if
   else
      ! copie, desallocation, puis reallocation
      integrateur%size_commut = integrateur%size_commut * 2
      allocate(tab_tmp(integrateur%nb_commut-1), stat= alloc_ok)
      if (alloc_ok /= pm_OK) then ! l'allocation dynamique a echoue
         code_retour%valeur = pm_err_allocate
         go to 6000
      end if
      tab_tmp(:) = integrateur%g_commut(1:integrateur%nb_commut-1)
      deallocate(integrateur%g_commut,stat=alloc_ok)
      if (alloc_ok /= pm_OK) then ! la desallocation dynamique a echoue
         code_retour%valeur = pm_err_deallocate
         go to 6000
      end if
      allocate(integrateur%g_commut(integrateur%size_commut), stat= alloc_ok)
      if (alloc_ok /= pm_OK) then ! l'allocation dynamique a echoue
         code_retour%valeur = pm_err_allocate
         go to 6000
      end if
      integrateur%g_commut(1:integrateur%nb_commut-1) = tab_tmp(:)
   end if   
end if
integrateur%g_commut(integrateur%nb_commut)%adresse = MsproConvertitPointeur(g_commut)
integrateur%g_commut(integrateur%nb_commut)%max_deltat = abs(max_deltat)
integrateur%g_commut(integrateur%nb_commut)%eps_converg = abs(eps_converg)
integrateur%g_commut(integrateur%nb_commut)%action = action
!!$ integrateur%g_commut(integrateur%nb_commut)%ident = integrateur%nb_commut
! DM 473 : on affecte à la nouvelle routine un identifiant de la valeur maximale (des identifiants existants) + 1
integrateur%g_commut(integrateur%nb_commut)%ident = max_ident + 1 

if (present(ident_g_commut)) then
   ident_g_commut = integrateur%g_commut(integrateur%nb_commut)%ident
endif

6000 continue

code_retour%routine = pm_num_mu_ajouter_evenement
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_ajouter_evenement
