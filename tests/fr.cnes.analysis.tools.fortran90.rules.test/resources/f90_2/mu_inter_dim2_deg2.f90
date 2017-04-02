subroutine mu_inter_dim2_deg2 (dim_x,vect_X,dim_y,vect_Y,tab_fXY,x_int,y_int,i_x,i_y, &
                               fxy_int,code_retour, &
                               x_int_bis,y_int_bis,pt_double)

! (C) Copyright CNES - MSPRO - 2001-2004

!************************************************************************
!
! But:  Interpolation de degre 2 sur deux variables (dimension 2)
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

integer,                              intent(in) :: dim_x   ! dimension n du vecteur X
real(pm_reel), dimension(dim_x),      intent(in) :: vect_X  ! vecteur X contenant x(1), ..., x(n)
integer,                              intent(in) :: dim_y   ! dimension m du vecteur Y
real(pm_reel), dimension(dim_y),      intent(in) :: vect_Y  ! vecteur Y contenant y(1), ..., y(m)
real(pm_reel), dimension(dim_x,dim_y),intent(in) :: tab_fXY ! tableau fXY contenant f[x(i),y(j)]

real(pm_reel), intent(in)                 :: x_int   ! valeur de x pour laquelle on cherche a estimer f(x_int,y_int)
real(pm_reel), intent(in)                 :: y_int   ! valeur de y pour laquelle on cherche a estimer f(x_int,y_int)

integer, intent(inout)                    :: i_x     ! indice associe a x_int 
integer, intent(inout)                    :: i_y     ! indice associe a y_int 

real(pm_reel), intent(out)              :: fxy_int   ! valeur estimee de f(x_int,y_int)
type(tm_code_retour), intent(out)       :: code_retour

logical, intent(in),optional            :: x_int_bis ! si appel avec les memes conditions que l'appel precedent, 
logical, intent(in),optional            :: y_int_bis ! si appel avec les memes conditions que l'appel precedent, 
integer, intent(in),optional            :: pt_double ! parametre pour gestion des points double

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

integer :: direction ! valeur de pt_double si fournit par l'utilisateur, sinon pm_avant
logical :: recherche_necessaire ! indicateur signalant s'il faut appeler mu_inter_ind

real(pm_reel), save :: x_i, x_ip1  ! points X d'indice i_x et i_x+1
real(pm_reel), save :: y_i, y_ip1  ! points Y d'indice i_y et i_y+1
real(pm_reel) :: alpha, beta ! facteurs multiplicatifs pour l'interpolation

type(tm_code_retour) :: code_local

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mu_inter_dim2_deg2.f90: derniere modification V5.15 >'

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

! Verification si recherche de l'indice i_y est necessaire ou non
! ===============================================================

if (present(y_int_bis)) then ! l'utilisateur a donne une valeur a y_int_bis
   recherche_necessaire = (.NOT.(y_int_bis))  ! recherche necessaire si y_int_bis = false
else
   recherche_necessaire = .TRUE.
end if

if (recherche_necessaire) then ! la recherche est necessaire

   ! recherches de l'indice necessaire
   call mu_inter_ind(dim_y,vect_Y,y_int,i_y,code_local,pt_double=direction)

   ! A ce stade code_retour%valeur est > ou = pm_OK
   ! Il ne faut pas l'ecraser si code_local%valeur = pm_OK
   if (code_local%valeur /= pm_OK) code_retour%valeur = code_local%valeur
   if (code_retour%valeur <  pm_OK) go to 6000 ! probleme dans la recherche d'indice
                                               ! En cas de warning: on continue

   ! initialisation des elements utiles
   ! Si la recherche n'est pas demandee: on utilise les memes elements
   y_i = vect_Y(i_y)
   y_ip1 = vect_Y(i_y + 1)

end if

! Evaluation (par interpolation ou extrapolation) de f(x_int,y_int)
! =================================================================

! a ce stade les differences utilisees dans les divisions sont non nulles 
! (par definition de i_x et i_y)
alpha = (x_int - x_i)/(x_ip1 - x_i)

beta = (y_int - y_i)/(y_ip1 - y_i)

! Formulation brute pour l'interpolation ou l'extrapolation:
! ---------------------------------------------------------
! avec : un_moins_alpha = 1._pm_reel - alpha
!        un_moins_beta = 1._pm_reel - beta
! fxy_int = tab_fXY(i_x,i_y) * un_moins_alpha * un_moins_beta  + &
!           tab_fXY(i_x+1,i_y) * alpha * un_moins_beta  + &       
!           tab_fXY(i_x,i_y+1) * un_moins_alpha * beta  + &       
!           tab_fXY(i_x+1,i_y+1) * alpha * beta                   

! formulation contenant 8 multiplications et 7 additions/soustractions

! Formulation optimisee (diminution du nombre de multiplications):
! ---------------------------------------------------------------
! formulation contenant 4 multiplications et 6 additions/soustractions

! Nota:
! l'acces aux elements du tableau se fait de maniere optimise par le compilateur.
! Il n'est pas utile d'utiliser des variables intermediaires contenant les elements
! utilises plusieurs fois dans la formule.

fxy_int = ((tab_fXY(i_x+1,i_y)-tab_fXY(i_x,i_y)) * alpha + tab_fXY(i_x,i_y)) * (1._pm_reel - beta) + &
         ((tab_fXY(i_x+1,i_y+1)-tab_fXY(i_x,i_y+1)) * alpha + tab_fXY(i_x,i_y+1)) * beta

6000 continue

code_retour%routine = pm_num_mu_inter_dim2_deg2
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_inter_dim2_deg2
