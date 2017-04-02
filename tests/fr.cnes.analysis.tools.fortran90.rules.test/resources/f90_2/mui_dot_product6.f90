subroutine mui_dot_product6 ( vect1 , vect2 , prod_scal , retour )

! (C) Copyright CNES - MSLIB - 2000
!
!***********************************************************************
!                                                                      *
! But: Produit scalaire de deux vecteurs de dimension 6                *
! ===  remplace la fonction fortran "dot_product"                      *
!                                                                      *
! Note d'utilisation: Routine interne                                  *
! ==================                                                   *
!                                                                      *
!$Historique                                                          *
! ==========                                                           *
!   + Version 6.3 (DM-ID 239) : Performances en temps de calcul        *
!                 (Date: 10/2005 - Realisation: ATOS ORIGIN)           *
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)         *
!   + Version 6.9 : FA-ID 1033 : Correction de type                    *
!                   (Date: 05/2008 - Realisation: Atos origin)         *
!                                                                      *
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/02/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!***********************************************************************
! Declaration des modules externes                                     *
!***********************************************************************
  use precision_mslib
  use type_mslib
  use valeur_code_retour_mslib
  use longueur_chaine_mslib
!***********************************************************************
! Declarations des parametres du module                                *
!***********************************************************************
  implicit none
!
!***********************************************************************
  real (pm_reel), dimension(6), intent(in)   :: vect1       ! vecteur
  real (pm_reel), dimension(6), intent(in)   :: vect2       ! vecteur
  real (pm_reel)              , intent(out)  :: prod_scal   ! produit scalaire
  integer                     , intent(out)  :: retour
!***********************************************************************

!***********************************************************************
! Autres declarations                                                  *
!***********************************************************************
!
  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSLIB mui_dot_product6.f90: derniere modification V6.13 >'
!
! Ne pas toucher a la ligne suivante
  character(len=pm_longueur_rcs_id), parameter :: rcs_id = &
       ' $Id: mui_dot_product6.f90 362 2013-02-15 18:01:28Z bbjc $ '
!
!************************************************************************
! Initialisation de la valeur du code retour                            *
!************************************************************************
!
  retour = pm_OK
!
!************************************************************************
! Calcul du produit scalaire                                            *
!************************************************************************
!
  prod_scal = vect1 (1) * vect2 (1) &
            + vect1 (2) * vect2 (2) &
            + vect1 (3) * vect2 (3) &
            + vect1 (4) * vect2 (4) &
            + vect1 (5) * vect2 (5) &
            + vect1 (6) * vect2 (6)
!
!************************************************************************
! Fin du module                                                         *
!************************************************************************
!
end subroutine mui_dot_product6
