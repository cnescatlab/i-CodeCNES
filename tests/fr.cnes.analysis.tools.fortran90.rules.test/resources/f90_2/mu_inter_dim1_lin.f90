subroutine mu_inter_dim1_lin (dim_x,vect_X,vect_fX,x_int,i_x,fx_int,code_retour, &
                              x_int_bis,pt_double)

! (C) Copyright CNES - MSPRO - 2001-2004

!************************************************************************
!
! But:  Interpolation lineaire sur une variable (dimension 1)
! ===
!
! Note d'utilisation:   se reporter a la documentation utilisateur
! ==================
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 2.0 : creation a partir de rien
!                         (Date: 09/2001 - Realisation: Guylaine Prat)
!   + Version 5.2 (DE globale 13): suppression des dimensions implicites dans l'include arg_*.h 
!                         (Date: 12/2004 - Realisation: Bruno Revelin)
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

use int_utilitaires_mspro, only : mu_inter_ind
use parametre_mspro

use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                         :: dim_x   ! dimension n du vecteur X
real(pm_reel), dimension(dim_x), intent(in) :: vect_X  ! vecteur X contenant x(1), ..., x(n)
real(pm_reel), dimension(dim_x), intent(in) :: vect_fX ! vecteur fX contenant fx(1), ..., fx(n)
real(pm_reel), intent(in)                   :: x_int   ! valeur de x pour laquelle on cherche a estimer f(x_int)
integer,       intent(inout)                :: i_x     ! indice associe a x_int 
real(pm_reel), intent(out)                 :: fx_int   ! valeur estimee de f(x_int)
type(tm_code_retour), intent(out)          :: code_retour
logical, intent(in),optional               :: x_int_bis ! si appel avec les memes conditions que l'appel precedent, 
integer, intent(in),optional               :: pt_double ! parametre pour gestion des points double

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

integer :: direction ! valeur de pt_double si fournit par l'utilisateur, sinon pm_avant
logical :: recherche_necessaire ! indicateur signalant s'il faut appeler mu_inter_ind

real(pm_reel), save :: x_i, x_ip1 ! points X d'indice i_x et i_x+1
real(pm_reel) :: alpha ! facteur multiplicatif pour l'interpolation

type(tm_code_retour) :: code_local

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mu_inter_dim1_lin.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ===============
code_retour%valeur = pm_OK

if (present(pt_double)) then
   direction = pt_double
else
   direction = pm_avant ! valeur par defaut
end if

! Verification si recherche de l'indice i_x est necessaire ou non
! ===============================================================

if (present(x_int_bis)) then ! l'utilisateur a donne une valeur a x_int_bis
   recherche_necessaire = (.NOT.(x_int_bis))  ! recherche necessaire si x_int_bis = false
else
   recherche_necessaire = .TRUE.
end if

if (recherche_necessaire) then ! la recherche est necessaire

   ! recherches de l'indice necessaire
   call mu_inter_ind(dim_x,vect_X,x_int,i_x,code_local,pt_double=direction)
   code_retour%valeur = code_local%valeur
   if (code_retour%valeur <  pm_OK) go to 6000 ! probleme dans la recherche d'indice
                                               ! En cas de warning: on continue

   ! initialisation des elements utiles
   ! Si la recherche n'est pas demandee: on utilise les memes elements
   x_i = vect_X(i_x)
   x_ip1 = vect_X(i_x + 1)

end if

! Evaluation (par interpolation ou extrapolation) de f(x_int)
! ===========================================================

! a ce stade les differences utilisees dans les divisions sont non nulles 
! (par definition de i_x)
alpha = (x_int - x_i)/(x_ip1 - x_i)

! Formulation brute pour l'interpolation ou l'extrapolation:
! ---------------------------------------------------------
! fx_int = vect_fX(i_x) * (1._pm_reel - alpha) + vect_fX(i_x+1) * alpha

! formulation contenant 2 multiplications et 2 additions/soustractions

! Formulation optimisee (diminution du nombre de multiplications):
! ---------------------------------------------------------------
! formulation contenant 1 multiplication et 2 additions/soustractions

! Nota:
! l'acces aux elements du tableau se fait de maniere optimise par le compilateur.
! Il n'est pas utile d'utiliser des variables intermediaires contenant les elements
! utilises plusieurs fois dans la formule.

fx_int = (vect_fX(i_x+1) - vect_fX(i_x)) * alpha + vect_fX(i_x)

6000 continue

code_retour%routine = pm_num_mu_inter_dim1_lin
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_inter_dim1_lin
