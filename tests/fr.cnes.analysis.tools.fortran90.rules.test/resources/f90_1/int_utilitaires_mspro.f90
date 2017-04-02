module int_utilitaires_mspro
use mslib
implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: int_utilitaires_mspro.f90 372 2013-02-20 14:30:25Z ffsm $'

public
interface
     subroutine mu_ajouter_evenement (integrateur, g_commut, max_deltat, eps_converg, &
          action,code_retour,ident_g_commut)

       use type_mspro



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


     end subroutine mu_ajouter_evenement
     subroutine mu_car_spher ( car, spher, code_retour )

       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3),intent(in)               ::  car   ! coordonnees cartesiennes
type(tm_spher), intent(out)                          ::  spher ! coordonnees spheriques correspondantes
type(tm_code_retour), intent(out)                    ::  code_retour


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_car_spher
     subroutine mu_creer_gest_pas ( integrateur, gest_pas, code_retour )

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

interface
   subroutine gest_pas(integrateur,interpolateur,tdeb,tfin,dernier_pas,retour)     ! gestionnaire de pas

   use mslib
   use type_mspro

   interface
      subroutine interpolateur(integrateur,t,y,retour_interp)     ! routine d'interpolation
   
      use mslib
      use type_mspro
      
      type(tm_integrateur),        intent(in)           ::  integrateur   ! integrateur utilise
      real(pm_reel),               intent(in)           ::  t             ! abscisse
      real(pm_reel),dimension(integrateur%n),intent(out)::  y             ! vecteur d'etat interpole
      integer,                     intent(out)          ::  retour_interp
      
      end subroutine interpolateur
   end interface

   type(tm_integrateur), intent(in) ::  integrateur  ! integrateur utilise
   real(pm_reel),        intent(in) ::  tdeb  ! abscisse debut de l'interv. de validite de l'interpolateur
   real(pm_reel),        intent(in) ::  tfin  ! abscisse fin de l'interv. de validite de l'interpolateur
   logical,              intent(in) ::  dernier_pas  ! indique si l'on est au dernier pas
   integer,              intent(out)::  retour


   end subroutine gest_pas
end interface

type(tm_integrateur), intent(inout) ::  integrateur ! integrateur utilise
type(tm_code_retour), intent(out)   ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_creer_gest_pas

     subroutine mu_det_mat ( n,mat_LU,vect_P,det,code_retour )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer      ,                  intent(in)           :: n         ! dimension des matrices et vecteurs
real(pm_reel), dimension (n,n), intent(in)           :: mat_LU    ! matrice factorisee LU
integer      , dimension (n),   intent(in)           :: vect_P    ! vecteur de permutation
real(pm_reel),                  intent(out)          :: det       ! determinant
type(tm_code_retour), intent(out)                    :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_det_mat
     subroutine mu_eq2degre_reel (a, b, c, nb_racine, x1, x2, code_retour, type_eq )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)                            :: a ! coefficient du terme en x2
real(pm_reel), intent(in)                            :: b ! coefficient du terme en x
real(pm_reel), intent(in)                            :: c ! coefficient de degre 0
integer, intent(out)                                 :: nb_racine ! nombre de racines
real(pm_reel), intent(out)                           :: x1 ! premiere racine
real(pm_reel), intent(out)                           :: x2 ! seconde racine
type(tm_code_retour), intent(out)                    :: code_retour
integer, intent(out), optional                       :: type_eq ! type d'equation


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_eq2degre_reel
     subroutine mu_factor_LU ( n,mat_A,eps,mat_LU,vect_P,code_retour )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer      ,                  intent(in)           :: n         ! dimension des matrices et vecteurs
real(pm_reel), dimension (n,n), intent(in)           :: mat_A     ! matrice carree a factoriser
real(pm_reel),                  intent(in)           :: eps       ! pivot maximum pour les permutations
real(pm_reel), dimension (n,n), intent(out)          :: mat_LU    ! matrice factorisee LU
integer      , dimension (n),   intent(out)          :: vect_P    ! vecteur de permutation
type(tm_code_retour), intent(out)                    :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_factor_LU
     subroutine mu_inter_dim1_lin ( dim_x,vect_X,vect_fX,x_int,i_x,fx_int,code_retour, &
                                   x_int_bis,pt_double)

       use mslib



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


     end subroutine mu_inter_dim1_lin
     subroutine mu_inter_dim2_deg2 ( dim_x,vect_X,dim_y,vect_Y,tab_fXY,x_int,y_int,i_x,i_y, &
                                     fxy_int,code_retour, &
                                     x_int_bis,y_int_bis,pt_double )

       use mslib



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


     end subroutine mu_inter_dim2_deg2
     subroutine mu_inter_dim3_deg3 ( dim_x,vect_X,dim_y,vect_Y,dim_z,vect_Z,tab_fXYZ, &
                                     x_int,y_int,z_int,i_x,i_y,i_z, &
                                     fxyz_int,code_retour, &
                                     x_int_bis,y_int_bis,z_int_bis,pt_double )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,                         intent(in)   :: dim_x   ! dimension n du vecteur X
real(pm_reel), dimension(dim_x), intent(in)   :: vect_X  ! vecteur X contenant x(1), ..., x(n)
integer,                         intent(in)   :: dim_y   ! dimension m du vecteur Y
real(pm_reel), dimension(dim_y), intent(in)   :: vect_Y  ! vecteur Y contenant y(1), ..., y(m)
integer,                         intent(in)   :: dim_z   ! dimension n du vecteur Z
real(pm_reel), dimension(dim_z), intent(in)   :: vect_Z  ! vecteur Z contenant z(1), ..., z(n)

real(pm_reel), dimension(dim_x,dim_y,dim_z), intent(in) :: tab_fXYZ ! tableau fXYZ contenant f[x(i),y(j),z(k)]

real(pm_reel), intent(in)                 :: x_int   ! valeur de x pour laquelle on cherche a estimer f(x_int,y_int,z_int)
real(pm_reel), intent(in)                 :: y_int   ! valeur de y pour laquelle on cherche a estimer f(x_int,y_int,z_int)
real(pm_reel), intent(in)                 :: z_int   ! valeur de z pour laquelle on cherche a estimer f(x_int,y_int,z_int)

integer, intent(inout)                    :: i_x     ! indice associe a x_int 
integer, intent(inout)                    :: i_y     ! indice associe a y_int 
integer, intent(inout)                    :: i_z     ! indice associe a z_int 


real(pm_reel), intent(out)              :: fxyz_int   ! valeur estimee de f(x_int,y_int,z_int)
type(tm_code_retour), intent(out)       :: code_retour

logical, intent(in),optional            :: x_int_bis ! si appel avec les memes conditions que l'appel precedent, 
logical, intent(in),optional            :: y_int_bis ! si appel avec les memes conditions que l'appel precedent, 
logical, intent(in),optional            :: z_int_bis ! si appel avec les memes conditions que l'appel precedent, 
integer, intent(in),optional            :: pt_double ! parametre pour gestion des points double

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_inter_dim3_deg3
     subroutine mu_inter_ind ( dim_x,vect_X,x_int,i_x,code_retour,pt_double,interpol )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,                         intent(in) :: dim_x   ! dimension n du vecteur X
real(pm_reel), dimension(dim_x), intent(in) :: vect_X  ! vecteur X contenant x(1), ..., x(n)
real(pm_reel),                   intent(in) :: x_int   ! valeur pour laquelle on cherche l'indice i_x
integer,                      intent(inout) :: i_x     ! indice associe a x_int 
type(tm_code_retour), intent(out)       :: code_retour

integer, intent(in),optional            :: pt_double ! parametre pour gestion des points double
integer, intent(out),optional           :: interpol  ! type d'interpolation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_inter_ind
     subroutine mu_inv_mat (n,mat_LU,vect_P,mat_inv,code_retour)

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer      ,                  intent(in)           :: n         ! dimension des matrices et vecteurs
real(pm_reel), dimension (n,n), intent(in)           :: mat_LU    ! matrice factorisee LU
integer      , dimension (n),   intent(in)           :: vect_P    ! vecteur de permutation
real(pm_reel), dimension (n,n), intent(out)          :: mat_inv   ! matrice inverse
type(tm_code_retour), intent(out)                    :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_inv_mat
     subroutine mu_lagrange (nb_pt, nb_pt_max, vect_X, dim_y, mat_Y, x_int, y_int, code_retour )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                                  ::  nb_pt        ! nombre de points
integer, intent(in)                                  ::  nb_pt_max    ! taille du tableau
real(pm_reel), dimension(nb_pt_max), intent(in)      ::  vect_X       ! abscisses
integer, intent(in)                                  ::  dim_y        ! dimension des ordonnees
real(pm_reel), dimension(dim_y,nb_pt_max), intent(in)::  mat_Y        ! ordonnees
real(pm_reel), intent(in)                            ::  x_int        ! abscisse x du point dont on veut les ordonnees
real(pm_reel), dimension(dim_y),intent(out)          ::  y_int        ! ordonnees du point x               
type(tm_code_retour), intent(out)                    ::  code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_lagrange
     subroutine mu_liberer_integ ( integrateur, code_retour )

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_integrateur), intent(inout) ::  integrateur ! integrateur utilise
type(tm_code_retour), intent(out)   ::  code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_liberer_integ
     subroutine mu_resol_sys_lin ( n,mat_LU,vect_P,vect_B,vect_X,code_retour )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer      ,                  intent(in)           :: n         ! dimension des matrices et vecteurs
real(pm_reel), dimension (n,n), intent(in)           :: mat_LU    ! matrice 1er membre factorisee LU
integer      , dimension (n),   intent(in)           :: vect_P    ! vecteur de permutation
real(pm_reel), dimension (n),   intent(in)           :: vect_B    ! vecteur 2nd membre
real(pm_reel), dimension (n),   intent(out)          :: vect_X    ! vecteur inconnu
type(tm_code_retour), intent(out)                    :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_resol_sys_lin
     subroutine mu_spher_car ( spher, car, code_retour )

       use type_mspro
       use mslib




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_spher), intent(in)                           ::  spher    !  coordonnees spheriques
real(pm_reel), dimension(3), intent(out)             ::  car      !  coordonnees cartesiennes correspondantes
type(tm_code_retour), intent(out)                    ::  code_retour


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_spher_car
     subroutine mu_spline_cub_eval ( nb_pt, nb_pt_max, vect_X, dim_y, mat_Y, mat_d2Y,&
                                     x_int, y_int, code_retour, d1y_int, d2y_int )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,                                   intent(in):: nb_pt      ! nombre de points
integer,                                   intent(in):: nb_pt_max  ! taille de tableau
real(pm_reel), dimension(nb_pt_max),       intent(in):: vect_X     ! abscisses
integer,                                   intent(in):: dim_y      ! dimension des ordonnees
real(pm_reel), dimension(dim_y,nb_pt_max), intent(in):: mat_Y      ! ordonnees
real(pm_reel), dimension(dim_y,nb_pt_max), intent(in):: mat_d2Y    ! derivees secondes
real(pm_reel),                             intent(in):: x_int      ! abscisse du point d'interpolation
real(pm_reel), dimension(dim_y),          intent(out):: y_int      ! ordonnees au point d'interpolation
type(tm_code_retour),                     intent(out):: code_retour
real(pm_reel), dimension(dim_y), intent(out), optional   :: d1y_int    ! derivees premieres en y
real(pm_reel), dimension(dim_y), intent(out), optional   :: d2y_int    ! derivees secondes en y


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_spline_cub_eval
     subroutine mu_spline_cub_init ( nb_pt, nb_pt_max, vect_X, dim_y, mat_Y, mat_d2Y, code_retour, vect_d1y1,&
                                     vect_d1yn, d1y1_fixe, d1yn_fixe )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,                                   intent(in) :: nb_pt      ! nombre de points
integer,                                   intent(in) :: nb_pt_max  ! taille de tableau
real(pm_reel), dimension(nb_pt_max),       intent(in) :: vect_X     ! abscisses
integer,                                   intent(in) :: dim_y      ! dimension des ordonnees
real(pm_reel), dimension(dim_y,nb_pt_max), intent(in) :: mat_Y      ! ordonnees
real(pm_reel), dimension(dim_y,nb_pt_max), intent(out):: mat_d2Y    ! derivees secondes
type(tm_code_retour),                      intent(out):: code_retour
real(pm_reel), dimension(dim_y), intent(in), optional    :: vect_d1y1  ! derivees premieres en x(1)
real(pm_reel), dimension(dim_y), intent(in), optional    :: vect_d1yn  ! derivees premieres en x(n)
logical,       dimension(dim_y), intent(in), optional    :: d1y1_fixe  ! d1y1_fixe
logical,       dimension(dim_y), intent(in), optional    :: d1yn_fixe  ! d1yn_fixe


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_spline_cub_init
     subroutine mu_statis (nb_pt, nb_pt_max, pt, val_min, val_max, moy, moy_quadra, ecart_type, code_retour )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                              :: nb_pt     ! nombre de points
integer, intent(in)                              :: nb_pt_max ! taille du tableau de points
real(pm_reel), dimension(nb_pt_max), intent(in)  :: pt        ! tableau de points
real(pm_reel), intent(out)                 :: val_min   ! plus petit point du tableau
real(pm_reel), intent(out)                 :: val_max   ! plus grand point du tableau
real(pm_reel), intent(out)                 :: moy       ! moyenne arithmetique des points du tableau
real(pm_reel), intent(out)                 :: moy_quadra! moyenne quadratique des points du tableau
real(pm_reel), intent(out)                 :: ecart_type! ecart type
type(tm_code_retour), intent(out)          :: code_retour


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_statis
end interface

interface mu_creer_integrateur
     subroutine mu_creer_integrateur_pas_fixe ( type_integrateur, n, pas, integrateur, code_retour, &
          ordre, icirc, ireg, rxmu, mode_iteratif,eps_init,eps_prog,liber_param,eps_init_var_integ,eps_prog_var_integ)

       use mslib
       use type_mspro

integer,                              intent(in)     :: type_integrateur! type de l'integrateur
integer,                              intent(in)     :: n               ! dimension du vecteur d'etat
real(pm_reel),                        intent(in)     :: pas             ! pas d'integration (pas fixe)
type(tm_integrateur),                 intent(out)    :: integrateur     ! integrateur cree
type(tm_code_retour), intent(out)                    :: code_retour
integer, intent(in),  optional                       :: ordre		! ordre de l'integrateur (nombre pair de 2 a 16)
integer, intent(in),  optional                       :: icirc		! cle de circularisation
integer, intent(in),  optional                       :: ireg		! cle de regularisation (chgt de variable temps)
real(pm_reel), intent(in),  optional                 :: rxmu		! mu
logical, intent(in),  optional                       :: mode_iteratif	! mode iteratif
real(pm_reel), intent(in), optional                           :: eps_init        ! epsilon d'initialisation de l'integration (cowell)
real(pm_reel), intent(in), optional                           :: eps_prog        ! epsilon de progression de l'integration (cowell)
integer,intent(in), dimension(6), optional           :: liber_param     ! indicateur de liberation des paramètres
real(pm_reel), intent(in), optional                           :: eps_init_var_integ ! epsilon d'initialisation pour la variable d'intégration (cowell)
real(pm_reel), intent(in), optional                           :: eps_prog_var_integ ! epsilon de progression pour la variable d'intégration (cowell)

     end subroutine mu_creer_integrateur_pas_fixe
     subroutine mu_creer_integrateur_pas_var ( type_integrateur, n, pas_min, pas_max, &
          tol_rel, tol_abs, integrateur, code_retour )

       use mslib
       use type_mspro

integer,                              intent(in)     :: type_integrateur! type de l'integrateur
integer,                              intent(in)     :: n               ! dimension du vecteur d'etat
real(pm_reel),                        intent(in)     :: pas_min         ! pas d'integration minimum (pas variable)
real(pm_reel),                        intent(in)     :: pas_max         ! pas d'integration maximum (pas variable)
real(pm_reel),dimension(n),target,    intent(in)     :: tol_rel         ! tableau des tolerances relatives (pas variable)
real(pm_reel),dimension(n),target,    intent(in)     :: tol_abs         ! tableau des tolerances absolues (pas variable)
type(tm_integrateur),                 intent(out)    :: integrateur     ! integrateur cree
type(tm_code_retour), intent(out)                    :: code_retour

     end subroutine mu_creer_integrateur_pas_var

!!$DM 473
     subroutine mu_supprimer_evenement(integrateur, ident_g_commut, code_retour)

       use mslib

       use type_mspro

! Arguments
! ============
  type(tm_integrateur), intent(inout)                  ::  integrateur         ! intégrateur concerné
  integer,              intent(out)                    ::  ident_g_commut      ! identificateur de la subroutine à supprimer
  type(tm_code_retour), intent(out)                    ::  code_retour

       end subroutine mu_supprimer_evenement


  end interface

interface mu_integrer
     subroutine mu_integrer_jjfrac ( fsub, integrateur, t0, y0, t, y, t_fin, retour_fsub, pb_fsub, &
          code_retour, num_commut, nb_commut, evt_commut)

       use mslib
       use type_mspro

interface
   subroutine fsub(t,y,ydot,retour)     ! equation differentielle

   use mslib

   real(pm_reel),intent(in)                      ::  t     ! abscisse
   real(pm_reel),dimension(:),intent(in)         ::  y     ! vecteur d'etat
   real(pm_reel),dimension(:),intent(out)        ::  ydot  ! derivee en t
   integer,      intent(out)                     ::  retour
   
   
   end subroutine fsub
end interface

type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
real(pm_reel),                         intent(in)                     ::  t0           ! valeur initiale de t
real(pm_reel),dimension(integrateur%n),intent(in)                     ::  y0           ! valeur de la fonction a t0
real(pm_reel),                         intent(in)                     ::  t            ! abscisse du point demande
real(pm_reel),dimension(integrateur%n),intent(out)                    ::  y            ! valeur de la fonction en t
real(pm_reel),                         intent(out)                    ::  t_fin        ! abscisse effective du y calcule
integer,                               intent(out)                    ::  retour_fsub  ! code retour de fsub
real(pm_reel),                         intent(out)                    ::  pb_fsub      ! abscisse posant pb a fsub
type(tm_code_retour),                  intent(out)                    ::  code_retour
integer,                               intent(out), optional          ::  num_commut   ! identificateur de la derniere routine ayant commute
integer,                               intent(out), optional          ::  nb_commut    ! nombre total de commutations
type(tm_evt_commut),dimension(:),      pointer,     optional          ::  evt_commut   ! pointeurs sur les évènements de commutation 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     end subroutine mu_integrer_jjfrac

     subroutine mu_integrer_jjsec ( fsub_cowell, integrateur, t0, y0, t, y, t_fin, retour_fsub, pb_fsub, &
          code_retour, num_commut, nb_commut)

       use mslib
       use type_mspro

interface
   subroutine fsub_cowell(t,y,ydot,retour)     ! equation differentielle

   use mslib

   type(tm_jour_sec),intent(in)                  ::  t     ! abscisse
   real(pm_reel),dimension(:),intent(in)         ::  y     ! vecteur d'etat
   real(pm_reel),dimension(:),intent(out)        ::  ydot  ! derivee en t
   integer,      intent(out)                     ::  retour
   
   
   end subroutine fsub_cowell
end interface

type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
type(tm_jour_sec),                     intent(in)                     ::  t0           ! valeur initiale de t
real(pm_reel),dimension(integrateur%n),intent(in)                     ::  y0           ! valeur de la fonction a t0
type(tm_jour_sec),                     intent(in)                     ::  t            ! abscisse du point demande
real(pm_reel),dimension(integrateur%n),intent(out)                    ::  y            ! valeur de la fonction en t
type(tm_jour_sec),                     intent(out)                    ::  t_fin        ! abscisse effective du y calcule
integer,                               intent(out)                    ::  retour_fsub  ! code retour de fsub
type(tm_jour_sec),                     intent(out)                    ::  pb_fsub      ! abscisse posant pb a fsub
type(tm_code_retour),                  intent(out)                    ::  code_retour
integer,                               intent(out), optional          ::  num_commut   ! identificateur de la derniere routine ayant commute
integer,                               intent(out), optional          ::  nb_commut    ! nombre total de commutations

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     end subroutine mu_integrer_jjsec

     subroutine mu_integrer_jjsec_simp ( fsub_cowell,integrateur, t0, y0, t, y, t_fin, retour_fsub, pb_fsub, &
          code_retour, num_commut, nb_commut, fcowell_simp)

       use mslib
       use type_mspro

interface
   subroutine fsub_cowell(t,y,ydot,retour)     ! equation differentielle

   use mslib

   type(tm_jour_sec),intent(in)                  ::  t     ! abscisse
   real(pm_reel),dimension(:),intent(in)         ::  y     ! vecteur d'etat
   real(pm_reel),dimension(:),intent(out)        ::  ydot  ! derivee en t
   integer,      intent(out)                     ::  retour
   
   
   end subroutine fsub_cowell
end interface



type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
type(tm_jour_sec),                     intent(in)                     ::  t0           ! valeur initiale de t
real(pm_reel),dimension(integrateur%n),intent(in)                     ::  y0           ! valeur de la fonction a t0
type(tm_jour_sec),                     intent(in)                     ::  t            ! abscisse du point demande
real(pm_reel),dimension(integrateur%n),intent(out)                    ::  y            ! valeur de la fonction en t
type(tm_jour_sec),                     intent(out)                    ::  t_fin        ! abscisse effective du y calcule
integer,                               intent(out)                    ::  retour_fsub  ! code retour de fsub
type(tm_jour_sec),                     intent(out)                    ::  pb_fsub      ! abscisse posant pb a fsub
type(tm_code_retour),                  intent(out)                    ::  code_retour
integer,                               intent(out), optional          ::  num_commut   ! identificateur de la derniere routine ayant commute
integer,                               intent(out), optional          ::  nb_commut    ! nombre total de commutations
interface
   subroutine fcowell_simp(t,y,ydot,retour)     ! equation differentielle

   use mslib

   type(tm_jour_sec),intent(in)                  ::  t     ! abscisse
   real(pm_reel),dimension(*),intent(in)         ::  y     ! vecteur d'etat
   real(pm_reel),dimension(*),intent(out)        ::  ydot  ! derivee en t
   integer,      intent(out)                     ::  retour
   
   end subroutine fcowell_simp

end interface

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     end subroutine mu_integrer_jjsec_simp
     


  end interface




end module int_utilitaires_mspro

