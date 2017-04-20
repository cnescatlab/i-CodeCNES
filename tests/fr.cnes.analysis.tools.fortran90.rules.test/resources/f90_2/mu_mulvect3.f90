subroutine mu_mulvect3 (mat33, vect3, vect3_res, code_retour)

! (C) Copyright CNES - MSLIB - 2008

!************************************************************************
!
! But:  Calcul du produit matrice-vecteur (3,3)*(3)
! ===   
!
! Note d'utilisation:  voir la documentation utilisateur et la note algorithmique
! ==================
!
!$Historique
! ==========
!   + Version 6.8 : DM-ID 859 : Optimisation des performances
!                   (Date: 03/2008 - Realisation: C. Hue Atos origin)
!
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/02/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************
! Modules
! =======
use parametre_mslib

use parametres_internes_mslib
use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in), dimension(3,3)  :: mat33     ! matrice en entree
real(pm_reel), intent(in), dimension(3)    :: vect3     ! vecteur
real(pm_reel), intent(out), dimension(3)   :: vect3_res ! matrice en sortie
type(tm_code_retour), intent(out)          ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mu_mulvect3.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mu_mulvect3.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour
code_retour%valeur = pm_OK

! Calcul optimisé
! ===============

vect3_res(1)= mat33(1,1)*vect3(1)+mat33(1,2)*vect3(2)+mat33(1,3)*vect3(3)
vect3_res(2)= mat33(2,1)*vect3(1)+mat33(2,2)*vect3(2)+mat33(2,3)*vect3(3)
vect3_res(3)= mat33(3,1)*vect3(1)+mat33(3,2)*vect3(2)+mat33(3,3)*vect3(3)

code_retour%routine = pm_num_mu_mulvect3
code_retour%biblio = pm_mslib90

end subroutine mu_mulvect3
