module int_utilitaires
! (C) Copyright CNES - MSLIB - 2007
!************************************************************************
!
! But:  Definition des constantes et interface des fonctions du thème U
! ===
!
! Note d'utilisation:
! ==================
!   Module en principe utilisé uniquement par l'intermédiaire du module global
!   "mslib90"
!
!$Historique
! ==========
!   
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!   + Version 6.6 : DM-ID 616 : rajout des constantes du module code_racine_mslib
!                   en vue de la suppression de ce module
!                   (Date: 05/2007 - Realisation: Atos origin)
!
!   + Version 6.8 : DM-ID 859 : ajout des déclarations des nouvelles fonctions
!                              (calcul optimise)
!                   (Date: 03/2008 - Realisation: Atos origin)
!
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
  character(len=256), private, parameter :: SVN_VER =  '$Id: int_utilitaires.f90 362 2013-02-15 18:01:28Z bbjc $'


!   use code_racine_mslib         ! definition des clefs definissant le nombre de racines
! Parametres associes a mu_racine
integer, parameter :: pm_A_B_racine      = 2        ! A et B sont racines de l'equation f(x)=h sur [A,B]
integer, parameter :: pm_1racine         = 1        ! mu_racine a trouve 1 racine 
integer, parameter :: pm_0racine_nb_pair = 0        ! il y a soit 0 racine, soit un nombre pair de racines
integer, parameter :: pm_criter_arret_x  = 1        ! arret sur delta < seuil
integer, parameter :: pm_criter_arret_fx = 2        ! arret sur f(x) - h < ou = plus petit reel positif
integer, parameter :: pm_criter_arret_iter_max = 3  ! arret sur nombre max d'iterations depasse



public
interface
     subroutine mu_3rot_quat ( def_3rot,angle1,angle2,angle3,quat,code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                      :: def_3rot   ! definition et ordre d'enchainement des trois rotations
real(pm_reel), intent(in)                :: angle1     ! valeur de l'angle associe a la premiere rotation
real(pm_reel), intent(in)                :: angle2     ! valeur de l'angle associe a la deuxieme rotation
real(pm_reel), intent(in)                :: angle3     ! valeur de l'angle associe a la troisieme rotation
type(tm_quat), intent(out)               :: quat       ! quaternion
type(tm_code_retour), intent(out)                    ::  code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_3rot_quat
     subroutine mu_angle2 (x, y, angle, code_retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), intent(in)         :: x      ! coordonnee x du vecteur / cosinus de l'angle
real(pm_reel), intent(in)         :: y      ! coordonnee y du vecteur / sinus de l'angle
real(pm_reel), intent(out)        :: angle  ! angle calcule dans [0,2.pi[
type(tm_code_retour), intent(out) :: code_retour




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_angle2
     subroutine mu_angle3 (vect_a, vect_b, angle, code_retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
real(pm_reel), dimension(3), intent(in) :: vect_a ! vecteur a
real(pm_reel), dimension(3), intent(in) :: vect_b ! vecteur b

real(pm_reel), intent(out)              :: angle  ! angle non oriente entre les vecteurs
type(tm_code_retour), intent(out)       :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_angle3
     subroutine mu_axe_angle_quat ( axe, angle, quat, code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3), intent(in)              ::  axe         ! axe de rotation
real(pm_reel), intent(in)                            ::  angle       ! angle de rotation
type(tm_quat), intent(out)                           ::  quat        ! quaternion norme              
type(tm_code_retour), intent(out)                    ::  code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_axe_angle_quat
     subroutine mu_compar_rot_quat ( quat1, quat2, angle, code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_quat), intent(in)                           ::  quat1         ! quaternion a comparer
type(tm_quat), intent(in)                           ::  quat2         ! quaternion a comparer
real(pm_reel), intent(out)                          ::  angle         ! ecart angulaire entre les deux rotations associees a quat1 et quat2
type(tm_code_retour), intent(out)                   ::  code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_compar_rot_quat
     subroutine mu_mat_quat ( mat, quat, code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3,3), intent(in)           ::  mat     ! matrice 
type(tm_quat), intent(out)                          ::  quat    ! quaternion associe a la matrice
type(tm_code_retour), intent(out)                   ::  code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_mat_quat
     subroutine mu_norme( vect, norme, code_retour, vect_norme )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3), intent(in)              :: vect        ! vecteur a normer

real(pm_reel), intent(out)                           :: norme       ! norme du vecteur en entree
type(tm_code_retour), intent(out)                    :: code_retour
real(pm_reel),dimension(3),intent(out), optional     :: vect_norme  ! vecteur en entree norme                  



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_norme
     subroutine mu_prod_quat ( quat1, quat2, quat_prod, code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_quat), intent(in)                            ::  quat1         ! quaternion q1
type(tm_quat), intent(in)                            ::  quat2         ! quaternion q2
type(tm_quat), intent(out)                           ::  quat_prod     ! quaternion produit q1*q2
type(tm_code_retour), intent(out)                    ::  code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_prod_quat
     subroutine mu_prod_vect( vect_a, vect_b, vect_c, code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), dimension(3), intent(in)               :: vect_a  ! vecteur a
real(pm_reel), dimension(3), intent(in)               :: vect_b  ! vecteur b

real(pm_reel), dimension(3), intent(out)              :: vect_c  ! vecteur produit vectoriel c = a x b
type(tm_code_retour), intent(out)                     :: code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_prod_vect
     subroutine mu_quat_3rot ( def_3rot,quat,angle1,angle2,angle3,code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                       :: def_3rot   ! definition et ordre d'enchainement des trois rotations
type(tm_quat), intent(in)                 :: quat       ! quaternion

real(pm_reel), intent(out)                :: angle1     ! valeur de l'angle associe a la premiere rotation
real(pm_reel), intent(out)                :: angle2     ! valeur de l'angle associe a la deuxieme rotation
real(pm_reel), intent(out)                :: angle3     ! valeur de l'angle associe a la troisieme rotation
type(tm_code_retour), intent(out)         ::  code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_quat_3rot
     subroutine mu_quat_axe_angle ( quat, axe, angle, code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_quat), intent(in)                            ::  quat  ! quaternion
real(pm_reel), dimension(3), intent(out)             ::  axe   ! axe de rotation norme
real(pm_reel), intent(out)                           ::  angle ! angle de rotation
type(tm_code_retour), intent(out)                    ::  code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_quat_axe_angle
     subroutine mu_quat_conjug ( quat, quat_conjug, code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_quat), intent(in)                            ::  quat         ! quaternion en entree
type(tm_quat), intent(out)                           ::  quat_conjug  ! quaternion conjugue correspondant
type(tm_code_retour), intent(out)                    ::  code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_quat_conjug
     subroutine mu_quat_mat ( quat, mat, code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_quat), intent(in)                            ::  quat    ! quaternion
real(pm_reel), dimension(3,3), intent(out)           ::  mat     ! matrice associee au quaternion

type(tm_code_retour), intent(out)                    ::  code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_quat_mat
     subroutine mu_quat_norme ( quat, quat_norme, norme, code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_quat), intent(in)                            ::  quat        ! quaternion (q0, q1, q2, q3)
type(tm_quat), intent(out)                           ::  quat_norme  ! quaternion norme
real(pm_reel), intent(out)                           ::  norme       ! norme du quaternion
type(tm_code_retour), intent(out)                    ::  code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_quat_norme
     subroutine mu_quat_rep ( vect1, quat, vect2, code_retour )

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), dimension(3), intent(in)    ::  vect1 ! coordonnees du vecteur dans le repere R1
type(tm_quat), intent(in)                  ::  quat  ! quaternion de la transformation R1 -> R2
real(pm_reel), dimension(3), intent(out)   ::  vect2 ! coordonnees du vecteur dans le repere R2
type(tm_code_retour), intent(out)          ::  code_retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_quat_rep
     subroutine mu_racine ( sub, A, B, h, err_abs, iter_max, nb_racine, criter_arret, sol, code_retour,&
          fsolh, delta, iter, retour_sub, pb_sub )

       use type_mslib
       use precision_mslib
       implicit none

       interface
          subroutine sub(x,fx,retour_sub)     ! argument sub representant la subroutine d'evaluation

            use type_mslib
            use precision_mslib
            implicit none

            real(pm_reel),intent(in)                        ::  x  ! reel a evaluer par sub
            real(pm_reel),intent(out)                       ::  fx ! resultat de l'evaluation
            type(tm_code_retour), intent(out)               ::  retour_sub


          end subroutine sub
       end interface

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


real(pm_reel), intent(in)                            ::  A   ! borne inferieure de l'intervalle de recherche de solution
real(pm_reel), intent(in)                            ::  B   ! borne superieure de l'intervalle de recherche de solution
real(pm_reel), intent(in)                            ::  h   ! valeur du second membre de l'equation
real(pm_reel), intent(in)                            ::  err_abs    ! erreur absolue autorisee par l'utilisateur
integer, intent(in)                                  ::  iter_max   ! nombre maximum d'iterations
integer, intent(out)                                 ::  nb_racine  ! indication sur le nombre de racines
integer, intent(out)                                 ::  criter_arret  ! critere d'arret utilise
real(pm_reel), intent(out)                           ::  sol    ! valeur de la racine si la routine en a trouve une
type(tm_code_retour), intent(out)                    ::  code_retour
real(pm_reel), intent(out), optional                 ::  fsolh  ! valeur de f(sol) - h
real(pm_reel), intent(out), optional                 ::  delta  ! majorant de l'erreur sur sol
integer, intent(out), optional                       ::  iter   ! nombre d'iterations effectuees
type(tm_code_retour), intent(out), optional          ::  retour_sub ! code retour de la routine sub
real(pm_reel), intent(out), optional                 ::  pb_sub ! valeur de x posant probleme lors de l'appel a sub


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     end subroutine mu_racine

     subroutine mu_matmul3 (mat33_1, mat33_2, mat33_res, code_retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in), dimension(3,3)  :: mat33_1     ! matrice 1
real(pm_reel), intent(in), dimension(3,3)  :: mat33_2     ! matrice 2
real(pm_reel), intent(out), dimension(3,3) :: mat33_res   ! matrice en sortie
type(tm_code_retour), intent(out)          ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_matmul3

     subroutine mu_matmul6 (mat66_1, mat66_2, mat66_res, code_retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in), dimension(6,6)  :: mat66_1     ! matrice 1
real(pm_reel), intent(in), dimension(6,6)  :: mat66_2     ! matrice 2
real(pm_reel), intent(out), dimension(6,6) :: mat66_res   ! matrice en sortie
type(tm_code_retour), intent(out)          ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_matmul6

     subroutine mu_transpose3 (mat33, mat33_res, code_retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in), dimension(3,3)  :: mat33    ! matrice en entree
real(pm_reel), intent(out), dimension(3,3) :: mat33_res ! matrice en sortie
type(tm_code_retour), intent(out)          ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_transpose3

     subroutine mu_mulvect3 (mat33, vect3, vect3_res, code_retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in), dimension(3,3)  :: mat33    ! matrice en entree
real(pm_reel), intent(in), dimension(3)    :: vect3    ! vecteur en entree
real(pm_reel), intent(out), dimension(3)   :: vect3_res ! vecteur en sortie
type(tm_code_retour), intent(out)          ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_mulvect3


     subroutine mu_mulvecttrans3 (mat33, vect3, vect3_res, code_retour)

       use type_mslib
       use precision_mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in), dimension(3,3)  :: mat33    ! matrice en entree
real(pm_reel), intent(in), dimension(3)    :: vect3    ! vecteur en entree
real(pm_reel), intent(out), dimension(3)   :: vect3_res ! vecteur en sortie
type(tm_code_retour), intent(out)          ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mu_mulvecttrans3

end interface

  character(len=pm_longueur_rcs_id), private, parameter :: &
  rcs_id =' $Id: int_utilitaires.f90 362 2013-02-15 18:01:28Z bbjc $ '

end module int_utilitaires
