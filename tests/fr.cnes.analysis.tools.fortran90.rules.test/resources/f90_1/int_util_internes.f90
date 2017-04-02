module int_util_internes
! (C) Copyright CNES - MSLIB - 2007
!************************************************************************
!
! But:  Definition des constantes et interface des routines internes du thème U
! ===
!
! Note d'utilisation:
! ==================
!   Module en principe utilisé uniquement en interne de la MSLIB90
!
!$Historique
! ==========
!   
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!   + Version 6.6 : rajout du cartouche
!                   (Date: 05/2007 - Realisation: Atos origin)
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/02/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************
use longueur_chaine_mslib
implicit none

! SVN Source File Id
  character(len=256), private, parameter :: SVN_VER =  '$Id: int_util_internes.f90 362 2013-02-15 18:01:28Z bbjc $'

public
interface
     function mui_axe_norme_quat ( axe_norme, angle )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3) , intent(in) ::  axe_norme            ! axe de rotation (norme)
real(pm_reel)               , intent(in) ::  angle                ! angle de rotation
type(tm_quat)                            ::  mui_axe_norme_quat   ! quaternion resultat



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end function mui_axe_norme_quat
     subroutine mui_dot_product3 ( vect1 , vect2 , prod_scal , retour )
       
       use type_mslib
       use precision_mslib

!***********************************************************************
!***********************************************************************
  real (pm_reel), dimension(3), intent(in)   :: vect1       ! vecteur
  real (pm_reel), dimension(3), intent(in)   :: vect2       ! vecteur
  real (pm_reel)              , intent(out)  :: prod_scal   ! produit scalaire
  real (pm_reel)              , intent(out)  :: retour
!***********************************************************************



     end subroutine mui_dot_product3
     subroutine mui_dot_product4 ( vect1 , vect2 , prod_scal , retour )
       
       use type_mslib
       use precision_mslib

!***********************************************************************
!***********************************************************************
  real (pm_reel), dimension(4), intent(in)   :: vect1       ! vecteur
  real (pm_reel), dimension(4), intent(in)   :: vect2       ! vecteur
  real (pm_reel)              , intent(out)  :: prod_scal   ! produit scalaire
  real (pm_reel)              , intent(out)  :: retour
!***********************************************************************



     end subroutine mui_dot_product4
     subroutine mui_dot_product5 ( vect1 , vect2 , prod_scal , retour )
       
       use type_mslib
       use precision_mslib

!***********************************************************************
!***********************************************************************
  real (pm_reel), dimension(5), intent(in)   :: vect1       ! vecteur
  real (pm_reel), dimension(5), intent(in)   :: vect2       ! vecteur
  real (pm_reel)              , intent(out)  :: prod_scal   ! produit scalaire
  real (pm_reel)              , intent(out)  :: retour
!***********************************************************************



     end subroutine mui_dot_product5
     subroutine mui_dot_product6 ( vect1 , vect2 , prod_scal , retour )
       
       use type_mslib
       use precision_mslib

!***********************************************************************
!***********************************************************************
  real (pm_reel), dimension(6), intent(in)   :: vect1       ! vecteur
  real (pm_reel), dimension(6), intent(in)   :: vect2       ! vecteur
  real (pm_reel)              , intent(out)  :: prod_scal   ! produit scalaire
  real (pm_reel)              , intent(out)  :: retour
!***********************************************************************



     end subroutine mui_dot_product6
     subroutine mui_inverse_matrice (mat_A, mat_B, retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3,3) , intent(in)  ::  mat_A   ! matrice 3*3 initiale
real(pm_reel), dimension(3,3) , intent(out) ::  mat_B   ! matrice 3*3 inversee
integer , intent(out)           :: retour


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_inverse_matrice
end interface

  character(len=pm_longueur_rcs_id), private, parameter :: &
  rcs_id =' $Id: int_util_internes.f90 362 2013-02-15 18:01:28Z bbjc $ '

end module int_util_internes
