subroutine mxi_def_parcours ( noeud_initial, noeud_final, arbre, liste_noeuds, nombre_transfo, retour )

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Definir la liste des noeuds d'un arbre a parcourir d'un noeud initial a un noeud final
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

use parametre_interne_mspro
use type_themeX_interne_mspro

use valeur_code_retour_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,                        intent(in)      ::  noeud_initial   ! noeud_initial
integer,                        intent(in)      ::  noeud_final     ! noeud_final
type(tm_i_noeud), dimension(:), intent(in)      ::  arbre           ! arbre
integer,dimension(:),           intent(out)     ::  liste_noeuds    ! liste_noeuds
integer,                        intent(out)     ::  nombre_transfo  ! nombre_transfo
integer,                        intent(out)     ::  retour          ! retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

integer           :: noeud_courant       ! indice du noeud courant
integer           :: indice_liste        ! indice de parcours de la liste
integer           :: indice_fils         ! indice de parcours des fils du noeud courant

logical           :: continue_boucle     ! indicateur de poursuite de la boucle

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mxi_def_parcours.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ...............
retour = pm_OK  ! pas d'erreur possible dans cette routine
noeud_courant = noeud_initial
indice_liste = 1
liste_noeuds(indice_liste) = noeud_initial

! programme
! .........

do while (noeud_courant /= noeud_final)

   ! on cherche a savoir si l'on doit descendre ou monter dans l'arbre
   if ((noeud_courant<noeud_final).OR.(noeud_final<arbre(noeud_courant)%min)) then
   ! noeud final hors de la descendance de l'arbre
   ! il faut remonter l'arbre
      noeud_courant = arbre(noeud_courant)%pere
      indice_liste = indice_liste + 1
      liste_noeuds(indice_liste) = noeud_courant   
   else
   ! on descend, et il faut parcourir les fils associes
      continue_boucle = pm_i_oui
      indice_fils = 1
      do while ((indice_fils <= arbre(noeud_courant)%nb_fils).AND.(continue_boucle))
         if (noeud_final <= arbre(noeud_courant)%fils(indice_fils)) then
         ! il faut continuer a descendre
            noeud_courant = arbre(noeud_courant)%fils(indice_fils)
            continue_boucle = pm_i_non
            indice_liste = indice_liste + 1
            liste_noeuds(indice_liste) = noeud_courant
         end if
         indice_fils = indice_fils + 1         
      end do
   end if   ! noeud_courant < noeud final
end do   ! noeud_courant /= noeud final

! noeud_courant = noeud_final.
nombre_transfo = indice_liste - 1
                        

end subroutine mxi_def_parcours
