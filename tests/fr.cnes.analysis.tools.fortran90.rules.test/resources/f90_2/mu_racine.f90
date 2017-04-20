subroutine mu_racine (sub, A, B, h, err_abs, iter_max, nb_racine, criter_arret, sol, code_retour,&
                      fsolh, delta, iter, retour_sub, pb_sub)

  ! (C) Copyright CNES - MSLIB90 - 2000-2003

  !************************************************************************
  !
  ! But:  Recherche d'une racine de l'equation  f(x) = h
  ! ===
  !
  ! Note d'utilisation: 1) nb_racine = pm_0racine_nb_pair -> soit il n'y a pas de racine, 
  ! ==================                                       soit un nombre pair de racine
  !                        nb_racine = pm_1racine    -> la routine a trouve 1 racine
  !                        nb_racine = pm_A_B_racine -> A et B sont racines
  !
  !                     2) L'algorithme utilise est iteratif, il y a 3 criteres d'arret:
  !                        (classement par ordre de priorite decroissante)
  !                        . delta < ou = seuil (= erreur relative + erreur absolue)
  !                        . |f(sol)-h| < ou egal au plus petit reel machine
  !                        . nombre d'iterations > nombre maximum d'iterations
  !
  !                      3) Les sorties sol, criter_arret, fsolh, delta et iter
  !                         ne sont utilisables que si une racine est trouvee.
  !                         La sortie pb_sub n'est utilisable que si retour_sub est non nul.
  !
  !$Historique
  ! ==========
  !   + Version 1.0 : creation a partir de la routine MURACI2 de la MSLIB f77
  !                         (Date: 10/2000 - Realisation: Veronique Lepine)
  !   + Version 2.0 (DE globale 1) : ajout des champs %biblio et %message pour le code retour
  !                         (Date: 07/2001 - Realisation: Guylaine Prat)
  !   + Version 2.0 (FA 1) : correction liee a l'affectation de retour_sub
  !                         (Date: 04/2002 - Realisation: Guylaine Prat)
  !   + Version 3.0 (DE 1) : ajout de commentaires dans note d'utilisation
  !                         (Date: 01/2003 - Realisation: Guylaine Prat)
  !   + Version 3.1 (DE globale 4) : Modifications suite aux remarques qualite ATV (dont revision algorithme)
  !                         (Date: 07/2003 - Realisation: Bruno Revelin)
  !   + Version 3.1 (FA 2) : correction de bugs de transcription de code fortran 77 vers 90
  !                         (Date: 07/2003 - Realisation: Bruno Revelin)
  !   + Version 6.3 : DM-ID 381 : Integrer des routines du theme trajectoires interplanetaires
  !                   transfert de mu_racine de la MSPRO a la MSLIB90
  !                   (Date: 09/2005 - Realisation: Claire Fabre - Atos origin) 
  !   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
  !                   (Date: 10/2006 - Realisation: Atos origin)
  !   + Version 6.6 : DM-ID 616 suppression math_mslib déjà inclu par parametres_internes_mslib
  !                   (Date: 05/2007 - Realisation: Atos origin)
  !   + Version 6.9 : DM-ID 1058 : Suppression des warnings G95
  !                   (Date: 09/2008 - Realisation: Atos origin)
  !!Revision 362 2013/02/15 bbjc
  !DM-ID 1513: Suppression des warnings de compilation
  !
  !
  !************************************************************************

  ! Modules
  ! =======
  
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

  ! Autres declarations
  ! ===================

  real(pm_reel) :: tols         ! critere de convergence
  real(pm_reel) :: s1, s2       ! signes respectifs de f(A) et f(B)
  real(pm_reel) :: presque_zero ! plus petit reel positif
  real(pm_reel) :: eps100       ! constante eps pour les reels
  real(pm_reel) :: fa, fb       ! valeurs de f(A) et F(B)
  real(pm_reel) :: c, fc        ! c et f(c)
  real(pm_reel) :: d, e         ! reels d et e
  real(pm_reel) :: xm, s, p, q, r, s3 ! variables intermediaires
  real(pm_reel) :: AA, BB       ! copie des bornes

  real(pm_reel) ::  fsolh_l=0._pm_reel  , delta_l , pb_sub_l ! en local: valeur de f(sol) - h, majorant de l'erreur sur sol, x posant probleme lors de l'appel a sub
  integer ::  iter_l   ! en local: nombre d'iterations effectuees
  type(tm_code_retour) ::  retour_sub_l ! en local: code retour de la routine sub

  intrinsic abs, tiny, epsilon, sign

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSLIB mu_racine.f90: derniere modification V6.13 >'

  ! Ne pas toucher a la ligne suivante
  character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mu_racine.f90 362 2013-02-15 18:01:28Z bbjc $ '


  !************************************************************************

  ! initialisations
  ! ===============

  ! initialisation de la valeur du code retour

  code_retour%valeur = pm_OK

  ! autres initialisations

  presque_zero = tiny(1._pm_reel)             ! plus petit nombre machine
  eps100 = 100._pm_reel * epsilon(1._pm_reel) ! constante 100*eps machine

  AA = A
  BB = B
  
  iter_l = 0

  delta_l = 0._pm_reel
  pb_sub_l = 0._pm_reel
  tols = 0._pm_reel

 ! =========================
 ! calcul de f(a) et de f(b)
 ! =========================

  call sub(AA, fa, retour_sub_l) ! Calcul de f(A)

  if (retour_sub_l%valeur /= pm_OK) then
     pb_sub_l = AA
     if (retour_sub_l%valeur > 0) then ! Avertissement
        code_retour%valeur = pm_warn_sub_ms
     else                              ! Erreur
        code_retour%valeur = pm_err_sub_ms
        go to 6000
     end if
  end if

  fa = fa - h

  call sub(BB, fb, retour_sub_l) ! Calcul de f(B)
  if (retour_sub_l%valeur /= pm_OK) then
     pb_sub_l = BB
     if (retour_sub_l%valeur > 0) then ! Avertissement
        code_retour%valeur = pm_warn_sub_ms
     else                              ! Erreur
        code_retour%valeur = pm_err_sub_ms
        go to 6000
     end if
  end if

  fb = fb - h      

  ! ============================================
  ! si f(a) = h alors a est racine de l'equation
  ! ============================================
  if (abs(fa) < presque_zero) then ! A est racine de l'equation

     nb_racine = pm_1racine

  ! ===========================================================
  ! si f(a) = h et f(b) = h alors a et b racines de l'equation
  ! ===========================================================

     if (abs(fb) < presque_zero) nb_racine = pm_A_B_racine  ! A et B sont racines de l'equation
     sol = AA
     fsolh_l = fa
     criter_arret = pm_criter_arret_fx
     go to 6000
     
  ! ============================================
  ! si f(b) = h alors b est racine de l'equation
  ! ============================================

  else if (abs(fb) < presque_zero) then ! B est racine de l'equation

     nb_racine = pm_1racine
     sol = BB
     fsolh_l = fb
     criter_arret = pm_criter_arret_fx
     go to 6000

  end if
  

  ! ============================================
  ! si (f(a)-h)*(f(b)-h) > 0 : 0 ou 2*n racines
  ! ============================================

  s1 = sign(1._pm_reel, fa) ! signe de f(A)
  s2 = sign(1._pm_reel, fb) ! signe de f(B)
  
  if (s1*s2 > 0._pm_reel) then ! Il y a 0 racines

     sol= 0._pm_reel
     fsolh_l = 0._pm_reel
     nb_racine = pm_0racine_nb_pair
     criter_arret = pm_criter_arret_fx
     go to 6000

  end if
  

  ! ============================================
  ! Mise en oeuvre de l'algorithme de R.P. Brent
  ! ============================================

   c = AA
   fc = fa
   d = BB - AA
   e = d

   if (abs(fc) < abs(fb)) then 
      AA = BB
      BB = c
      c = AA
      fa = fb
      fb = fc
      fc = fa
   end if
  
  tols=2._pm_reel * eps100 * abs(BB) + 0.5_pm_reel * err_abs
  xm = 0.5_pm_reel * (c - BB)
  
  delta_l = 2._pm_reel*abs(xm)
  
  do while ((abs(xm) > tols).and.(abs(fb) >= presque_zero))  ! la bissection est-elle necessaire?
     
     if ((abs(e) < tols).or.(abs(fa) <= abs(fb))) then

        d = xm
        e = d

     else

        s = fb / fa
       
        if (abs(AA - c) < presque_zero) then  ! Methode de la secante (= interpolation lineaire)

           p = 2.0_pm_reel * xm * s
           q = 1.0_pm_reel - s

        else             ! Methode de la secante (= interpolation quadratique inverse)
          
           q = fa / fc
           r = fb / fc
           p = s * (2.0_pm_reel * xm * q * (q - r) - (BB - AA) * (r - 1.0_pm_reel))
           q = (q - 1.0_pm_reel) * (r - 1.0_pm_reel) * (s - 1.0_pm_reel)

        end if

        if (p > 0._pm_reel) then

           q = -q

        else

           p = -p

        end if

        s = e
        e = d

        if (((2.0_pm_reel*p) < (3.0_pm_reel*xm*q - abs(tols*q))).and.(p < abs(0.5_pm_reel*s*q))) then

           d = p / q

        else

           d = xm
           e = d

        end if
     end if

     AA = BB
     fa = fb
     
     if (abs(d) > tols) then    ! Une etape en tols est-elle necessaire ?

        BB = BB + d

     else if (xm > 0.0_pm_reel) then

        BB = BB + tols

     else

        BB = BB - tols

     end if

     call sub(BB , fb, retour_sub_l)

     if (retour_sub_l%valeur /= pm_OK) then

        code_retour%valeur = pm_err_sub_ms
        pb_sub_l =  BB
        go to 6000

     end if

     fb = fb - h
     iter_l = iter_l + 1

     if (iter_l > iter_max) then  ! nombre d'iterations max atteint

        criter_arret = pm_criter_arret_iter_max
        nb_racine = pm_1racine
        sol = BB 
        fsolh_l = fb
        go to 6000

     end if

     s1=sign(1._pm_reel, fb)
     s3=sign(1._pm_reel, fc)

     if (s1*s3 > 0._pm_reel) then
        c = AA
        fc = fa
        d = BB - AA
        e = d
     end if

     if (abs(fc) < abs(fb)) then 
        AA = BB
        BB = c
        c = AA
        fa = fb
        fb = fc
        fc = fa
     end if
     
     tols=2._pm_reel * eps100 * abs(BB) + 0.5_pm_reel * err_abs
     xm = 0.5_pm_reel * (c - BB)
     
     delta_l = 2._pm_reel*abs(xm)
  
  end do
 
  ! =======================================
  ! le critere de convergence est satisfait
  ! =======================================

  nb_racine = pm_1racine    
  sol = BB
  fsolh_l = fb
  criter_arret = pm_criter_arret_x

6000 continue

if (present(fsolh))      fsolh = fsolh_l
if (present(delta))      delta = delta_l 
if (present(iter))       iter  = iter_l
if (present(retour_sub)) then
   retour_sub%valeur  = retour_sub_l%valeur
   retour_sub%routine = retour_sub_l%routine
   retour_sub%biblio  = retour_sub_l%biblio
   retour_sub%message = retour_sub_l%message
end if
if (present(pb_sub))     pb_sub = pb_sub_l

code_retour%routine = pm_num_mu_racine
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_racine
