subroutine mi_pb_lambert1 ( mu, pos_car_t0, pos_car_t1, nb_tours, duree, ind_sens, &
     vit_car_t0_orb1, vit_car_t0_orb2, vit_car_t1_orb1, vit_car_t1_orb2, code_retour, &
     nb_max_iter, precision, coniq, teta, encke, jacob_orb1, jacob_orb2)

! (C) Copyright CNES - MSLIB - 2000

!************************************************************************
!
! But:  Resolution du probleme de Lambert a plus d'un tour : determination de
! ===   la conique passant par 2 points et pour une donnee de transfert donnee
!
! Note d'utilisation: Sans objet 
! ==================
!
!$Historique
! ==========
!   + Version 6.3 : DM-ID 381 : Integrer des routines du theme trajectoires interplanetaires
!                   creation a partir de la routine MPLAM1 de la MSLIB f77
!                   (Date: 09/2005 - Realisation: Claire Fabre - Atos Origin)
!   + Version 6.4 : DM-ID 422 : Integrer l'ancienne procedure MPDLAM dans le 
!                   nouveau theme interplanetaire
!                   (Date: 04/2006 - Realisation: Claire Fabre - Atos origin)
!                   FA-ID 544 : Plantage lors de la resolution du probleme de Lambert
!                   rajout d'une sortie en erreur
!                   (Date: 06/2006 - Realisation: Claire Fabre - Atos origin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   FA-ID 439 : prise en compte de remarques Foresys
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 729 : problème de réinitialisation de l'algorithme
!                   résolvant TL(x)=x
!                   (Date: 05/2007 - Realisation Sandrine Avril- Atos origin)
!   + Version 6.9 : DM-ID 1058 : Suppression des warnings G95
!                   (Date: 09/2008 - Realisation Atos origin)
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

intrinsic present, cos, sqrt

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in)                         :: mu           ! constante d'attraction du corps central
real(pm_reel), dimension(3), intent(in)           :: pos_car_t0   ! vecteur position en coordonnées cartésiennes à t0 initial
real(pm_reel), dimension(3), intent(in)           :: pos_car_t1   ! vecteur position en coordonnées cartésiennes à t1 final
integer , intent(in)                              :: nb_tours     ! nombre de tours effectues par la sonde
real(pm_reel), intent(in)                         :: duree        ! duree de transfert
integer , intent(in)                              :: ind_sens      ! indicateur du sens du parcours
real(pm_reel), dimension(3), intent(out)          :: vit_car_t0_orb1   ! vecteur vitesse en coordonnées cartésiennes à t0 initial pour l orbite 1
real(pm_reel), dimension(3), intent(out)          :: vit_car_t0_orb2   ! vecteur vitesse en coordonnées cartésiennes à t0 initial pour l orbite 2
real(pm_reel), dimension(3), intent(out)          :: vit_car_t1_orb1   ! vecteur vitesse en coordonnées cartésiennes à t1 final pour l orbite 1
real(pm_reel), dimension(3), intent(out)          :: vit_car_t1_orb2   ! vecteur vitesse en coordonnées cartésiennes à t1 final pour l orbite 2
type(tm_code_retour), intent(out)                 :: code_retour
integer, intent(in), optional               :: nb_max_iter    ! nombre maximum d'iterations
real(pm_reel), intent(in), optional               :: precision      ! precision requise
type(tm_coniq), dimension(2), intent(out), optional :: coniq     ! parametres de la conique pour les 2 orbites
real(pm_reel), intent(out) , optional               :: teta      ! angle de transfert
real(pm_reel), dimension(2), intent(out) , optional :: encke     ! approximation de solution de l'equation de kepler generalise a t ou parametre de encke pour les 2 orbites
 real(pm_reel), dimension(6,8), intent(out), optional  :: jacob_orb1 ! matrice (6,8) des derivees partielles de lambert pour l'orbite initiale
 real(pm_reel), dimension(6,8), intent(out), optional  :: jacob_orb2 ! matrice (6,8) des derivees partielles de lambert pour l'orbite finale

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

real (pm_reel)  :: r1,r2                   ! normes des vecteurs des positions extremes
real (pm_reel), dimension(3)  :: xu1,xu2   ! vecteurs des positions extremes unitaires
real (pm_reel)  :: ctet, stet, cotet       ! valeurs pour le calcul de l angle de transfert
real (pm_reel)  :: const_c2,const_c,const_s! constantes dependant de la geometrie du probleme
real (pm_reel) :: fact,ts                  ! duree de transfert
real (pm_reel) :: xx,xs                    ! racines de l equation de lambert
real (pm_reel), dimension(2) :: xsol       ! racines de l equation de lambert
real (pm_reel) :: x0, x1, repsi            ! intervalle pour une iteration, precision
real (pm_reel) :: tl0,tl1,tdiv             ! valeurs de TL(X), ecart entre 2 valeurs
integer :: imax, iter                      ! nombre max d'iterations, numero de l'iteration
real (pm_reel) :: aux,rp1,rp2,vt1,vt2      ! vitesses radiales
real (pm_reel) :: aux1,bux1,aux2,bux2      ! valeurs entrant dans le calcul des vitesses initiales et finales
real (pm_reel)  :: sa, rcux, rdux, e2, ga, gb, yy, rz !  valeurs pour le calcul des parametres d'orbite
real (pm_reel) :: rteta                    ! valeurs pour le calcul de teta
real (pm_reel), dimension(2) :: rp         ! parametre P
integer :: i,j                             ! indices de boucle

logical          :: trouve                 ! logique pour les iterations
real(pm_reel)    ::  eps100                ! variable epsilon machine *100
integer     :: retour
type(tm_code_retour)  :: code_retour_local

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mi_pb_lambert1.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mi_pb_lambert1.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

! autres initialisations

eps100 = 100._pm_reel * epsilon(1._pm_reel)
trouve = .false.

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

if (nb_tours <= 0) then
   code_retour%valeur = pm_err_nb_tours
   go to 6000
end if

if (present(nb_max_iter)) then
   imax = nb_max_iter
else
   imax = 20
end if

if (present(precision)) then
   repsi = precision
else
   repsi = 1.e-06_pm_reel
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
if (const_c <= eps100) then
   code_retour%valeur = pm_err_points_confondus
   go to 6000
endif
const_s   = (r1+r2+const_c)*0.5_pm_reel
param_q   = sqrt(r1*r2)* cos(rteta/2._pm_reel)/const_s

nbre_tours = nb_tours

! normalisation de la duree de transfert
! ------------------------

fact = sqrt(8._pm_reel*mu/const_s**3)
ts   = duree*fact

! Resolution de l'equation TL(X)=TS dans la cas de plus dun tour
! Calcul des 2 racines de l equation sur l'intervalle (-1,+INFINI)
! par un derive de la methode "regula falsi"
! ------------------------

!     ---- pour la 1ere racine, le point initial de l'algorithme est
!     ---- x0=+0.6 , pour la 2eme racine x0=-0.6
do j = 1,2
   if (j == 1) then
      x0=0.6_pm_reel
   else
      x0=-0.6_pm_reel
      ! DM-ID 729 : on réinitialise à faux le booléen trouve
      trouve = .false.
   endif

!        calcul par iterations sur les intervalles successifs [x0,x1]
   
   iter = 0
   do while (.not. trouve)
      iter = iter+1
      if (iter == 1) then
!         call mii_pb_pelub(X0, nb_tours, param_q, TL0, param_y, param_z, retour)
         call mii_pb_equa_lambert(x0, tl0, code_retour)
         if (code_retour_local%valeur /= pm_OK) then
            code_retour%valeur = code_retour_local%valeur           ! affectation du code retour
            if (code_retour%valeur < pm_OK) go to 6000              ! sortie si erreur
         end if

         x1 = x0 + 0.05_pm_reel

      endif

!      call mii_pb_pelub(X1, nb_tours, param_q, TL1, param_y, param_z, retour)
      call mii_pb_equa_lambert(x1, tl1, code_retour)
      if (code_retour_local%valeur /= pm_OK) then
         code_retour%valeur = code_retour_local%valeur           ! affectation du code retour
         if (code_retour%valeur < pm_OK) go to 6000              ! sortie si erreur
      end if

      tdiv = tl1 - tl0

!     determination de la racine xs sur [x0,x1]
      if (abs(tdiv) < eps100) then
         code_retour%valeur = pm_err_tdiv_nul
         go to 6000
      else
         xs = (ts-tl0)*(x1-x0)/tdiv+x0
      endif
      x0=x1
      tl0=tl1
      x1=xS
      if(abs(x1) >= 1._pm_reel) then
         x1=x1/abs(x1)
         x1=(x1+x0)/2._pm_reel
      endif
      if (iter >= imax) then
         code_retour%valeur = pm_err_conv_lambert
         go to 6000
      endif

!     test de convergence
      if ((abs(tl0-ts) > repsi) .or. (iter < 3)) then
!           l'algorithme ne converge pas: 2 possibilites
!           imax trop petit , ou bien
!           la courbe y=ts est tangente a la courbe y=tl(x)
         trouve = .false.
      else
          trouve = .true.
        xsol(j)=xs
      endif
   enddo
enddo

!  calcul des parametres en sortie pour chaque orbite

do  j =1,2
   xx = xsol(j)

   yy = sqrt(abs(xx*xx-1._pm_reel))
   rz = sqrt(1._pm_reel+(param_q*param_q)*(xx*xx-1._pm_reel))

!  vitesses radiales

   aux = sqrt(2._pm_reel*mu*const_s)/const_c
   rp1 = aux*(param_q*rz*(const_s-r1)-xx*(const_s-r2))/r1
   rp2 = aux*(xx*(const_s-r1)-param_q*rz*(const_s-r2))/r2

! demi grand axe

   sa = 2._pm_reel*yy*yy*sign(1._pm_reel,1._pm_reel-xx)/const_s

! parametre p

   rp(j) =2._pm_reel*r1-r1*r1*sa-r1*r1*rp1*rp1/mu

! vitesses radiales
   if (rp(j) < 0._pm_reel) then
      code_retour%valeur = pm_err_p_negatif
      go to 6000
   end if

   vt1 = sqrt(mu*rp(j))/r1
   vt2 = vt1*r1/r2

! composantes cartesiennes des vitesses

   aux1 = rp1 - vt1/cotet
   bux1 = vt1/stet
   aux2 = -vt2/stet
   bux2 = rp2 + vt2/cotet

   do i=1,3
      if (j == 1) then
         vit_car_t0_orb1(i) = aux1*xu1(I)+bux1*xu2(i)
         vit_car_t1_orb1(i) = aux2*xu1(I)+bux2*xu2(i)
      else if (j == 2) then
         vit_car_t0_orb2(i) = aux1*xu1(i)+bux1*xu2(i)
         vit_car_t1_orb2(i) = aux2*xu1(i)+bux2*xu2(i)
      endif
   enddo

! Calcul des parametres de sortie optionnels : coniq, teta, encke
! ------------------------

! Parametres de la conique

   if (present(coniq)) then

   ! parametre P
      coniq(j)%p = rp(j)

   ! rajout d'un test sur rp
      if (rp(j) > 1.e+200_pm_reel) then
         code_retour%valeur=pm_err_p_infini
         goto 6000
      endif

   ! excentricite
      e2=1._pm_reel-coniq(j)%p*sa
      coniq(j)%e=sqrt(e2)

   ! longitude du noeud ascendant et inclinaison du plan de l'orbite
      call mii_pb_parg0(xu1,xu2,ind_sens,coniq(j)%gom,coniq(j)%i,ga,gb,retour)
      if (retour /= pm_OK) then
         code_retour%valeur = retour          ! affectation du code retour
         if (code_retour%valeur < pm_OK) go to 6000              ! sortie si erreur
      end if

   ! anomalie vraie du point de depart
      rcux = coniq(j)%p/R1-1._pm_reel
      rdux = rp1*sqrt(coniq(j)%p/mu)
      call mu_angle2(rcux, rdux, coniq(j)%anom_v, code_retour)
      if (code_retour_local%valeur == pm_err_vect_nul) then
         code_retour%valeur = pm_err_cni
         go to 6000
      end if

  ! argument du perigee
      coniq(j)%pom = ga - coniq(j)%anom_v
   endif

!  Angle de transfert
   if (present(teta)) then
      teta = rteta
   endif

!  initialisation de encke
   if (present(encke)) then
      encke(j) = (yy*yy*ts*sign(1._pm_reel,1._pm_reel-xx)+2._pm_reel*(xx-param_q*rz))* &
           sqrt(const_s /(2._pm_reel*mu))
   endif

enddo

!     initialisation de jacob_orb1
if (present(jacob_orb1)) then
   if (present(encke)) then
      call mii_pb_lambert_jac(mu, pos_car_t0, vit_car_t0_orb1, duree, jacob_orb1, retour, encke(i))
   else
      call mii_pb_lambert_jac(mu, pos_car_t0, vit_car_t0_orb1, duree, jacob_orb1, retour)
   endif
   if (retour /= pm_OK) then
      code_retour%valeur = retour           ! affectation du code retour
      if (code_retour%valeur < pm_OK) go to 6000              ! sortie si erreur
   end if
endif

!     initialisation de jacob_orb2
if (present(jacob_orb2)) then
   if (present(encke)) then
      call mii_pb_lambert_jac(mu, pos_car_t0, vit_car_t0_orb2, duree, jacob_orb2, retour, encke(i))
   else
      call mii_pb_lambert_jac(mu, pos_car_t0, vit_car_t0_orb2, duree, jacob_orb2, retour)
   endif
   if (retour /= pm_OK) then
      code_retour%valeur = retour           ! affectation du code retour
      if (code_retour%valeur < pm_OK) go to 6000              ! sortie si erreur
   end if
endif

6000 continue

code_retour%routine = pm_num_mi_pb_lambert1
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mi_pb_lambert1
