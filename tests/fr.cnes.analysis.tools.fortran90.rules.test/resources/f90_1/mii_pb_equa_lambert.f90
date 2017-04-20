subroutine mii_pb_equa_lambert(param_x, param_t, code_retour)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Calcul de la fonction de Lambert unifiee
! ===   
!
! Note d'utilisation: - Routine interne.
! ==================  
!             La variable  est declaree dans  
!             en save, donc sauvegardee au cours des appels successifs
!                     a cette routine
!$Historique
! ==========
!   + Version 6.3 : DM-ID 381 : Integrer des routines du theme trajectoires interplanetaires
!                   creation a partir de la routine MPELUB de la MSLIB f77
!                   (Date: 09/2005 - Realisation: Claire Fabre - Atos origin)
!   + Version 6.4 : FA-ID 544 : Plantage lors de la resolution du probleme de Lambert
!                   rajout d'une sortie en erreur
!                   (Date: 06/2006 - Realisation: Claire Fabre - Atos origin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement du module math_mslib par 
!     une sélection de int_constantes
!                   (Date: 05/2007 - Realisation: Atos origin)
!
!   + Version 6.8 : FA-ID 857 : Amelioration qualite, ajout de parentheses
!                   (Date: 04/2008 - Realisation : Atos origin)
!
!   + Version 6.9 : FA-ID 1033 : Affectation d entier a un reel avec fonction
!                                explicite
!                   (Date: 05/2008 - Realisation : Atos origin)
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

use module_param_mslib

use int_constantes, only : pm_pi,pm_deux_pi,pm_pi_sur2,pm_deg_rad,pm_rad_deg

use parametre_mslib

use parametres_internes_mslib
use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), intent(in)               ::  param_x  ! 
real(pm_reel), intent(out)              ::  param_t  ! 
type(tm_code_retour) ,intent(out)       ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
real (pm_reel) :: rm,rq,rk,ry,rz   !     parametres de la fonction explicites
real (pm_reel) :: E         ! variable intermediaire permettant de reconnaitre le type
! d orbite (elliptique, parabolique ou hyperbolique)
! par definition, E= X**2 - 1

real (pm_reel) :: A1,A2,A3,A4,A5,A6

real (pm_reel) :: var_f, var_g, var_h, xlamb   !  variables intermediaires
real(pm_reel)  ::  eps100         ! variable epsilon machine *100

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mii_pb_equa_lambert: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mii_pb_equa_lambert.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! autres initialisations

eps100 = 100._pm_reel * epsilon(1._pm_reel)

! initialisations

!     recuperation des valeurs en entree

rm = real(nbre_tours,kind=PM_REEL)
rq = param_q
rk = param_q*param_q

! initialisation des constantes an
! AN = 1*3*5....(2N-1)/ [2**(N-2)  (2N+3) N! ]
A1=2._pm_reel/5._pm_reel
A2=3._pm_reel/14._pm_reel
A3=5._pm_reel/36._pm_reel
A4=35._pm_reel/352._pm_reel
A5=315._pm_reel/4160._pm_reel
A6=3465._pm_reel/57600._pm_reel

!Calcul de E,Y et Z  :
! E = X**2 - 1
! Y = RACINE CARREE (ABS(E))
! Z = RACINE CARREE (1 + KE)
!     --------------------------

E = param_x*param_x-1._pm_reel
ry = sqrt(abs(E))
rz = sqrt(1._pm_reel+rk*E)

if (abs(E) < eps100) then

!    cas particulier de la parabole : e=0 ---> t=4/3 (1-q**3)
!        --------------------------------------------------------
   param_t = (4._pm_reel/3._pm_reel) * (1._pm_reel - rq*rq*rq)
   
else
   if (abs(param_x-1._pm_reel) < 1.e-06_pm_reel) then

!       cas ou e est faible : utilisation de la fonction sigma
!                             t= sigma(-e)-qk sigma(-ke)
!           ------------------------------------------------------
!      t = sig(-e) - rq*rk* sig(-rk*e)
! avec sig(u)=4._pm_reel/3._pm_reel+u*(a1+u*(a2+u*(a3+u*(a4+u*(a5+u*a6)))))
!     fonction sig(u) = fonction sigma de l'equation de lambert :
!                       elle est un developpement limite autour
!                       de zero, qui permet de calculer la focntion
!                       de lambert unifiee, meme si e est tres proche
!                       de zero.
!     pour ce module, seul les 6 premiers termes du developpement
!     limite de sig(u) sont utilises. ce sont les coefficients
!     an (variant theoriquement de 1 a l'infini).
!     les coefficients an valent :
!     an = 1*3*5...(2n-1)/[2**(n-2)  (2n+3) n! ]

      param_t = (4._pm_reel/3._pm_reel+(-E)*(A1+(-E)*         &
           (A2+(-E)*(A3+(-E)*(A4+(-E)*(A5+(-E)*A6)))))) -     &
           rq*rk* (4._pm_reel/3._pm_reel+(-rk*E)*(A1+(-rk*E)* &
           (A2+(-rk*E)*(A3+(-rk*E)*(A4+(-rk*E)*(A5+(-rk*E)*A6))))))
      
   else

!       autres cas : calcul de f = y (z-qx)
!                              g = xz - qe
!                              h = y (x-qz)
!           -----------------------------------

      var_f = ry * (rz - rq*param_x)
      var_g = param_x*rz - rq*E
      var_h = ry * (param_x - rq*rz)
      if (E > 0._pm_reel) then

!          cas hyperbolique : e>0 --> t= 2(h - ln(f+g)) / y**3
!              ---------------------------------------------------

         if (var_f+var_g < eps100) then      
            code_retour%valeur = pm_err_hyperb
            go to 6000
         end if

         param_t = 2._pm_reel* (var_h - log(var_f+var_g))/ (ry*ry*ry)

      else

!          cas elliptique : e<0 --> t = 2(mpi + lambda - h)/ y**3
!                                   avec lambda = arctg (f/g)
!              ------------------------------------------------------

         xlamb = atan(var_f/var_g)
         !              on ramene lambda dans [0,pi]
         if (xlamb < 0._pm_reel) xlamb = xlamb + pm_pi
         param_t = 2._pm_reel * (rm*pm_pi + xlamb - var_h) / (ry*ry*ry)

      endif
   endif
endif

6000 continue

end subroutine mii_pb_equa_lambert
