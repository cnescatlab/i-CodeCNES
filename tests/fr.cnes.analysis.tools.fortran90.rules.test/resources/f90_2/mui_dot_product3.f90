subroutine mui_dot_product3 ( vect1 , vect2 , prod_scal , retour )

! (C) Copyright CNES - MSPRO - 2000
!
!***********************************************************************
!                                                                      *
! But: Produit scalaire de deux vecteurs de dimension 3                *
! ===  remplace la fonction fortran "dot_product"                      *
!                                                                      *
! Note d'utilisation: Routine interne                                  *
! ==================                                                   *
!                                                                      *
!$Historique                                                          *
! ==========                                                           *
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 5.3 (DM-ID 239) : Performances en temps de calcul        *
!                 (Date: 10/2005 - Realisation: ATOS ORIGIN)           *
!     + Version 5.4 : modification
!                     FA-ID 439 : remarques qualite
!                     (Date: 02/2006 - Realisation: Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!                                                                      *
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!***********************************************************************
! Declaration des modules externes                                     *
!***********************************************************************
  use type_mspro
  use valeur_code_retour_mspro
!***********************************************************************
! Declarations des parametres du module                                *
!***********************************************************************
  implicit none
!
!***********************************************************************
  real (pm_reel), dimension(3), intent(in)   :: vect1       ! vecteur
  real (pm_reel), dimension(3), intent(in)   :: vect2       ! vecteur
  real (pm_reel)              , intent(out)  :: prod_scal   ! produit scalaire
  integer                     , intent(out)  :: retour
!***********************************************************************

!***********************************************************************
! Autres declarations                                                  *
!***********************************************************************
!
  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSLIB mui_dot_product3.f90: derniere modification V5.15 >'
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
            + vect1 (3) * vect2 (3)
!
!************************************************************************
! Fin du module                                                         *
!************************************************************************
!
end subroutine mui_dot_product3
