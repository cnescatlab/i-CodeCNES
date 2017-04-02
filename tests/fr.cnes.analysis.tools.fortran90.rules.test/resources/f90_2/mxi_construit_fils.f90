subroutine mxi_construit_fils ( nb_noeuds, arbre, retour )

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  construction automatique des noeuds d'un arbre
! ===
!
! Note d'utilisation:  
! ==================
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 3.0 : creation
!                         (Date: 11/2002 - Realisation: Bruno Revelin)
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
use type_themeX_interne_mspro     ! inclus use type_mspro
use parametre_themeX_interne_mspro

use valeur_code_retour_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                     ::  nb_noeuds         ! nb_noeuds
type(tm_i_noeud), dimension(:), intent(inout)      ::  arbre  ! arbre
integer, intent(out)                    ::  retour            ! retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

integer :: i,j     ! compteurs

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mxi_construit_fils.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! Programme
! .........

do i=1,nb_noeuds 
   arbre(i)%min = i
enddo

do i=1,nb_noeuds                        ! boucle sur tous les peres

   arbre(i)%nb_fils = 0
   do j=1,nb_noeuds                     ! scan de tous les fils eventuels
      
      if(arbre(j)%pere == i) then      ! recherche du lien de parente
         arbre(i)%nb_fils = arbre(i)%nb_fils + 1
         if (arbre(i)%nb_fils > pm_i_taille_fils_max) then
            retour = pm_err_valid
            go to 6000
         end if
         arbre(i)%fils(arbre(i)%nb_fils) = j

         if (arbre(i)%min > arbre(j)%min) arbre(i)%min = arbre(j)%min ! calcul du + petit num de la descendance
      end if
   end do

end do

6000 continue

end subroutine mxi_construit_fils
