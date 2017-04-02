subroutine mi_pb_lambert0 ( mu, pos_car_t0, pos_car_t1, duree, ind_sens, vit_car_t0, vit_car_t1, code_retour, &
     coniq, teta, encke, jacob)

! (C) Copyright CNES - MSLIB - 2000

!************************************************************************
!
! But:  Resolution du probleme de Lambert : determination de la conique passant par
! ===   2 points et pour une donnee de transfert donnee (limitation a moins d'1 tour)
!
! Note d'utilisation: Sans objet 
! ==================
!
!$Historique
! ==========
!   + Version 6.3 : DM-ID 381 : Integrer des routines du theme trajectoires interplanetaires
!                   creation a partir de la routine MPLAM0 de la MSLIB f77
!                   (Date: 09/2005 - Realisation: Claire Fabre - Atos origin)
!   + Version 6.4 : DM-ID 422 : Integrer l'ancienne procedure MPDLAM dans le 
!                   nouveau theme interplanetaire
!                   (Date: 04/2006 - Realisation: Claire Fabre - Atos origin)
!                   FA-ID 544 : Plantage lors de la resolution du probleme de Lambert
!                   rajout d'une sortie en erreur
!                   (Date: 06/2006 - Realisation: Claire Fabre - Atos origin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   FA-ID 439 : prise en compte de remarques Foresys
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : FA-ID 731 : Problèmes graves de codes sur la MSLIB soulevés par g95
!                   (Date: 05/2007 - réalisation : Sandrine Avril - Atos origin)
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

use int_utilitaires, only : mu_norme
use int_utilitaires, only : mu_angle2
use int_utilitaires, only : mu_racine

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

use module_param_mslib
use int_interpla_internes, only : mii_pb_equa_lambert
use int_interpla_internes, only : mii_pb_parg0
use int_interpla_internes, only : mii_pb_lambert_jac

! Declarations
! ============
implicit none

intrinsic present, cos, sin, sqrt, acos, epsilon

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)                         :: mu           ! constante d'attraction du corps central
real(pm_reel), dimension(3), intent(in)           :: pos_car_t0   ! vecteur position en coordonnées cartésiennes à t0 initial
real(pm_reel), dimension(3), intent(in)           :: pos_car_t1   ! vecteur position en coordonnées cartésiennes à t1 final
real(pm_reel), intent(in)                         :: duree   ! duree de transfert
integer , intent(in)                              :: ind_sens   ! indicateur du sens du parcours
real(pm_reel), dimension(3), intent(out)          :: vit_car_t0   ! vecteur vitesse en coordonnées cartésiennes à t0 initial
real(pm_reel), dimension(3), intent(out)          :: vit_car_t1   ! vecteur vitesse en coordonnées cartésiennes à t1 final
type(tm_code_retour), intent(out)                 :: code_retour
type(tm_coniq), intent(out), optional             :: coniq     ! parametres de la conique
real(pm_reel), intent(out) , optional             :: teta      ! angle de transfert
real(pm_reel), intent(out) , optional             :: encke     ! approximation de solution de l'equation de kepler generalise a t ou parametre de encke
 real(pm_reel), dimension(6,8), intent(out), optional  :: jacob ! matrice (6,8) des derivees partielles de lambert

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

real (pm_reel)  :: r1,r2     ! normes des vecteurs des positions extremes
real (pm_reel), dimension(3)  :: xu1,xu2   ! vecteurs des positions extremes unitaires
real (pm_reel)  :: ctet, stet, cotet   ! valeurs pour le calcul de l angle de transfert
real (pm_reel)  :: const_c2,const_c,const_s   !  constantes dependant de la geometrie du probleme
real (pm_reel)  :: fact, ts, to, tn   ! duree de transfert
real (pm_reel)  :: aa,bb,xn,xs,xx     ! intervalle en cadrant la racine et racine
integer         :: nbr                ! nombre de racines
integer         :: iter_max           ! nombre max d iterations
integer         :: criter_arret       ! critere d arret utilise
real (pm_reel)  :: aux, rp1, rp2, vt1, vt2  ! vitesses radiales
real (pm_reel)  :: repsi              !  epsilon pour le calcul de la racine
real (pm_reel)  :: aux1, bux1, aux2, bux2  !  valeurs entrant dans le calcul des vitesses initiales et finales
real (pm_reel)  :: sa, rcux, rdux, e2, ga, gb, yy, rz !  valeurs pour le calcul des parametres d'orbite
real (pm_reel)  :: rteta              !  valeurs pour le calcul de teta
real (pm_reel)  :: rp                 !  parametre p
integer         :: i,n                !  indices de boucle et variable non significative
logical         :: trouve             ! logique pour les iterations
real(pm_reel)   ::  eps100            ! variable epsilon machine *100
integer         :: retour
type(tm_code_retour)  :: code_retour_local

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mi_pb_lambert0.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mi_pb_lambert0.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour
code_retour%valeur = pm_OK

! autres initialisations
repsi = 0.0_pm_reel
eps100 = 100._pm_reel * epsilon(1._pm_reel)

trouve = .false.
n = 0
iter_max = 20

! verification des parametres en entree
! ------------------------

! constante de la gravitation
if (mu <= eps100) then
   if (mu < 0._pm_reel) then              ! constante de la gravitation negative
      code_retour%valeur = pm_err_mu_negatif
   else
      code_retour%valeur = pm_err_mu_nul  ! constante de la gravitation nulle
   end if
   go to 6000
end if

if (duree < 0._pm_reel) then
   code_retour%valeur = pm_err_duree_negatif
   go to 6000
end if

if ((ind_sens /= -1) .and. (ind_sens /= 1)) then
   code_retour%valeur = pm_err_sens
   go to 6000
end if

! normalisation des positions extremes
! ------------------------
call mu_norme(pos_car_t0,r1,code_retour, vect_norme = xu1)
if (r1 < eps100)  then   ! vecteur position nul
   code_retour%valeur = pm_err_pos_nul
   go to 6000
end if

call mu_norme(pos_car_t1,r2,code_retour, vect_norme = xu2)
if (r2 < eps100)  then   ! vecteur position nul
   code_retour%valeur = pm_err_pos_nul
   go to 6000
end if

! calcul de l'angle de transfert teta
! ------------------------

ctet = xu1(1)*xu2(1)+xu1(2)*xu2(2)+xu1(3)*xu2(3)
stet = sqrt(1._pm_reel-ctet*ctet)*sign(1._pm_reel,xu1(1)*xu2(2)- &
     xu1(2)*xu2(1))

if (abs(stet) < eps100) then
   code_retour%valeur = pm_err_transfert
   go to 6000
end if

stet = stet*real(ind_sens,kind=pm_reel)
cotet = stet/ctet

call mu_angle2(ctet, stet, rteta, code_retour_local)
if (code_retour_local%valeur == pm_err_vect_nul) then
   code_retour%valeur = pm_err_cni
   go to 6000
end if

! constantes dependant de la geometrie du probleme et intervenant
! dans la fonction de lambert TL(X)
! ------------------------
 
const_c2  = r1*r1+r2*r2-2._pm_reel*r1*r2*ctet
const_c   = sqrt(const_c2)
const_s   = (r1+r2+const_c)*0.5_pm_reel
param_q   = sqrt(r1*r2)* cos(rteta/2._pm_reel)/const_s

nbre_tours = 0

! normalisation de la duree de transfert
! ------------------------

fact = sqrt(8._pm_reel*mu/const_s**3)
ts   = duree*fact

! Resolution de l'equation TL(X)=TS dans l'intervalle (-1,+INFINI)
! ------------------------

! Choix de l'intervalle (A,B) encadrant la racine X
! pour ce choix, on calcule TL(0) et selon que TS est inferieur ou superieur
! a TL(0), on determine l'intervalle (0,2**98) ou (-1,0)
! ------------------------

to = sin(2._pm_reel*(acos(param_q))) + 2._pm_reel*(acos(param_q))
xs = 0._pm_reel

if (abs(ts-to) < eps100) then
!        on a trouve une racine
   trouve = .true.
   
else if (ts < to) then
!        iteration sur mii_pb_equa_lambert dans l'intervalle [0,2**98]

   aa=0._pm_reel
   bb=1._pm_reel

   do while ((.not. trouve).and. (n<100))
      n = n+1
      call mii_pb_equa_lambert(bb, tn, code_retour)
      if (code_retour_local%valeur /= pm_OK) then
         code_retour%valeur = code_retour_local%valeur           ! affectation du code retour
         if (code_retour%valeur < pm_OK) go to 6000              ! sortie si erreur
      end if

      IF(tn < ts) then
         trouve = .true.

         ! determination de la racine de l'equation TL(X)=TS
         ! ------------------------
    
         call mu_racine(mii_pb_equa_lambert,aa,bb,ts,repsi,iter_max, nbr, criter_arret, xs, code_retour)
         if (code_retour_local%valeur /= pm_OK) then
            code_retour%valeur = code_retour_local%valeur           ! affectation du code retour
            if (code_retour%valeur < pm_OK) go to 6000              ! sortie si erreur
         end if

      endif
      if (.not. trouve) then
         aa = bb
         bb = bb*2._pm_reel
      endif
   enddo

   if (.not. trouve) xs = 1.e+100_pm_reel

else

!        iteration sur mii_pb_equa_lambert dans l intervalle [-1,0]

   bb = 0._pm_reel
   xn = 1._pm_reel

   do while ((.not. trouve) .and. (n<=40))
      n = n+1
      xn=xn/2._pm_reel
      aa=xn-1._pm_reel

      call mii_pb_equa_lambert(aa, tn, code_retour)
      if (code_retour_local%valeur /= pm_OK) then
         code_retour%valeur = code_retour_local%valeur           ! affectation du code retour
         if (code_retour%valeur < pm_OK) go to 6000              ! sortie si erreur
      end if

      if (tn > ts) then
         trouve = .true.

         ! determination de la racine de l'equation TL(X)=TS
         ! ------------------------
      
         call mu_racine(mii_pb_equa_lambert,aa,bb,ts,repsi,iter_max, nbr, criter_arret, xs, code_retour)
         if (code_retour_local%valeur /= pm_OK) then
            code_retour%valeur = code_retour_local%valeur           ! affectation du code retour
            if (code_retour%valeur < pm_OK) go to 6000              ! sortie si erreur
         end if

      endif
      if (.not. trouve) bb = aa
   enddo

   if (.not. trouve) xs = -1._pm_reel
endif

xx = xs

yy = sqrt(abs(xs*xs-1._pm_reel))
rz = sqrt(1._pm_reel+(param_q*param_q)*(xs*xs-1._pm_reel))

! Calcul des parametres de sortie obligatoires : vit_car_t0, vit_car_t1
! ------------------------

! vitesses radiales

aux = sqrt(2._pm_reel*mu*const_s)/const_c
rp1 = aux*(param_q*rz*(const_s-r1)-xx*(const_s-r2))/r1
rp2 = aux*(xx*(const_s-r1)-param_q*rz*(const_s-r2))/r2

! demi grand axe

sa = 2._pm_reel*yy*yy*sign(1._pm_reel,1._pm_reel-xx)/const_s

! parametre p

rp =2._pm_reel*r1-r1*r1*sa-r1*r1*rp1*rp1/mu

! vitesses radiales
if (rp < 0._pm_reel) then
   code_retour%valeur = pm_err_p_negatif
   go to 6000
end if

vt1 = sqrt(mu*rp)/r1
vt2 = vt1*r1/r2

! composantes cartesiennes des vitesses

aux1 = rp1 - vt1/cotet
bux1 = vt1/stet
aux2 = -vt2/stet
bux2 = rp2 + vt2/cotet
do i = 1,3
   vit_car_t0(i) = aux1*xu1(i)+ bux1*xu2(i)
   vit_car_t1(i) = aux2*xu1(i)+ bux2*xu2(i)
enddo

! Calcul des parametres de sortie optionnels : coniq, teta, encke
! ------------------------

! Parametres de la conique

if (present(coniq)) then

   ! parametre P
   coniq%p = rp

   ! rajout d'un test sur rp
   if (rp > 1.e+200_pm_reel) then
      code_retour%valeur=pm_err_p_infini
      goto 6000
   endif

   ! excentricite
   e2=1._pm_reel-coniq%p*sa
   coniq%e = sqrt(e2)

   ! longitude du noeud ascendant et inclinaison du plan de l'orbite
   call mii_pb_parg0(xu1,xu2,ind_sens,coniq%gom,coniq%i,ga,gb,retour)
   if (retour /= pm_OK) then
      code_retour%valeur = retour          ! affectation du code retour
      if (code_retour%valeur < pm_OK) go to 6000              ! sortie si erreur
   end if

   ! anomalie vraie du point de depart
   rcux = coniq%p/r1-1._pm_reel
   rdux = rp1* sqrt(coniq%p/mu)
   call mu_angle2(rcux, rdux, coniq%anom_v, code_retour)
   if (code_retour_local%valeur == pm_err_vect_nul) then
      code_retour%valeur = pm_err_cni
      go to 6000
   end if

  ! argument du perigee
   coniq%pom = ga - coniq%anom_v
endif

!Angle de transfert
if (present(teta)) then
   teta = rteta
endif

!     initialisation de encke
if (present(encke)) then
   encke = (yy*yy*ts*sign(1._pm_reel,1._pm_reel-xx)+2._pm_reel*(xx-param_q*rz))* &
        sqrt(const_s /(2._pm_reel*mu))
endif

!     initialisation de jacob
if (present(jacob)) then
   if (present(encke)) then
      call mii_pb_lambert_jac(mu, pos_car_t0, vit_car_t0, duree, jacob, retour, encke)
   else
      call mii_pb_lambert_jac(mu, pos_car_t0, vit_car_t0, duree, jacob, retour)
   endif
   if (retour /= pm_OK) then
      code_retour%valeur = retour           ! affectation du code retour
      if (code_retour%valeur < pm_OK) go to 6000              ! sortie si erreur
   end if
endif

6000 continue

code_retour%routine = pm_num_mi_pb_lambert0
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mi_pb_lambert0
