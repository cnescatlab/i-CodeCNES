subroutine mii_pb_parg0(pos_car_t0, pos_car_t1, ind_sens, gom, inc, rga, rgb, retour)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  calcul d'elements du plan d'orbite joignant 2 pts
! ===   les elements calcules sont :
!         -  longitude du noeud ascendant et inclinaison
!         -  angles des 2 points avec le noeud ascendant

!
! Note d'utilisation: - Routine interne.
! ==================  
!
!$Historique
! ==========
!   + Version 6.3 : DM-ID 381 : Integrer des routines du theme trajectoires interplanetaires
!                   creation a partir de la routine MPARG0 de la MSLIB f77
!                   (Date: 09/2005 - Realisation: Claire Fabre - Atos Origin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   FA-ID 439 : prise en compte de remarques Foresys
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement du module math_mslib par 
!     une sélection de int_constantes
!                   (Date: 05/2007 - Realisation: Atos origin)
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

use int_utilitaires, only : mu_prod_vect
use int_utilitaires, only : mu_angle2
use int_utilitaires, only : mu_norme

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
                                                       
real(pm_reel), dimension(3), intent(in) ::  pos_car_t0  ! COMPOSANTES CARTESIENNES VECT UNITAIRE JOIGNANT 1ER PT
real(pm_reel), dimension(3), intent(in) ::  pos_car_t1  ! COMPOSANTES CARTESIENNES VECT UNITAIRE JOIGNANT 2EM PT
integer ,intent(in )                    ::  ind_sens    ! INDICATEUR DE SENS DU PARCOURS (1:DIRECT -1:RETROGRADE)
real(pm_reel), intent(out)              ::  gom         ! LONGITUDE DU NOEUD ASCENDANT
real(pm_reel), intent(out)              ::  inc         ! INCLINAISON DU PLAN DE L'ORBITE
real(pm_reel), intent(out)              ::  rga         ! ANGLE ENTRE LE NOEUD ASCENDANT ET LE PREMIER POINT
real(pm_reel), intent(out)              ::  rgb         ! ANGLE ENTRE LE NOEUD ASCENDANT ET LE SECOND POINT
integer ,intent(out)                    ::  retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------
real (pm_reel), dimension(3) :: vect_w
real (pm_reel), dimension(3) :: vect_wu   
real (pm_reel)   ::  norme_w  ! norme
real (pm_reel) :: rtamp1,rtamp2   !     variables tampons pour calcul
INTEGER :: i, ialign   !     indice de boucle et indicateur si a et b alignes
type(tm_code_retour)     :: code_retour_local
real(pm_reel)    ::  eps100         ! variable epsilon machine *100

intrinsic acos, sin

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mii_pb_parg0: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mii_pb_parg0.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! autres initialisations

gom = 0._pm_reel
inc = 0._pm_reel
rga = 0._pm_reel
rgb = 0._pm_reel
ialign = 0

eps100 = 100._pm_reel * epsilon(1._pm_reel)

!     *****************************************************************
!    calcul du vecteur unitaire perpendiculaire au plan de l'orbite et
!    oriente par le sens du mouvement (meme sens que moment cinetique)
!     *****************************************************************

call mu_prod_vect(pos_car_t0, pos_car_t1, vect_w, code_retour_local)
if (code_retour_local%valeur /= pm_OK) then
   retour = code_retour_local%valeur
   if (code_retour_local%valeur < pm_OK) go to 6000
end if

!     *********************************************************
!   calcul des elements du plan d'orbite
!   note : l'orbite polaire n'est pas consideree comme un cas
!        particulier
!     *********************************************************

!     ------------------
!   cas a et b alignes
!     ------------------

if (abs(vect_w(1)) < eps100 .and. abs(vect_w(2)) < eps100 .and. &
     abs(vect_w(3)) < eps100 ) then
!     si a et b sont alignes on choisit arbitrairement le plan
!     passant par a et l'axe x
   ialign = 1
   IF (pos_car_t0(2) < -eps100) then
      vect_w(1) = 0._pm_reel
      vect_w(2) = pos_car_t0(3) * real(ind_sens,kind=pm_reel)
      vect_w(3) = -pos_car_t0(2) * real(ind_sens,kind=pm_reel)
   ELSE
      vect_w(1) = 0._pm_reel
      vect_w(2) = -pos_car_t0(3) * real(ind_sens,kind=pm_reel)
      vect_w(3) =  pos_car_t0(2) * real(ind_sens,kind=pm_reel)
   ENDIF
!   norme du vecteur w = x*pos_car_t0
   call mu_norme(vect_w, norme_w, code_retour_local, vect_norme = vect_wu) 

!   si a et b sont alignes et a sur l'axe x i.e. norme_w=0 (ier<>0)
!   on se ramene au cas equatorial
   if (code_retour_local%valeur /=0) then
!     inclinaison
      if (ind_sens == -1) then
         inc = pm_pi
      else
         inc = 0._pm_reel
      endif
!       longitude du noeud ascendant
      gom = 0._pm_reel
!       angle entre le noeud ascendant et le point a
      rtamp1 = pos_car_t0(1)
      rtamp2 = pos_car_t0(2) * real(ind_sens,kind=pm_reel)

      call mu_angle2(rtamp1,rtamp2,rga,code_retour_local) 
      if (code_retour_local%valeur == pm_err_vect_nul) then
         retour = pm_err_cni
         go to 6000
      end if

!       angle entre le noeud ascendant et le point b
      rtamp1 = pos_car_t1(1)
      rtamp2 = pos_car_t1(2) * real(ind_sens,kind=pm_reel)

      call mu_angle2(rtamp1,rtamp2,rgb,code_retour_local)   
      if (code_retour_local%valeur == pm_err_vect_nul) then
         retour = pm_err_cni
         go to 6000
      end if
      retour = ialign
      go to 6000
   endif

!   inclinaison
   inc = acos(vect_wu(3))
!   si sin(i) < eps : a et b sont alignes et dans le plan(x,y)
!     (cas equatorial)
   if (sin(inc) < eps100) then
!       inclinaison
      if (ind_sens == -1) then
         inc = pm_pi
      else
         inc = 0._pm_reel
      endif
!       longitude du noeud ascendant
      gom = 0._pm_reel
!      angle entre le noeud ascendant et le point a
      rtamp1 = pos_car_t0(1)
      rtamp2 = pos_car_t0(2) * real(ind_sens,kind=pm_reel)

      call mu_angle2(rtamp1,rtamp2,rga,code_retour_local) 
      if (code_retour_local%valeur == pm_err_vect_nul) then
         retour = pm_err_cni
         go to 6000
      end if

!       angle entre le noeud ascendant et le point b
      rtamp1 = pos_car_t1(1)
      rtamp2 = pos_car_t1(2) * real(ind_sens,kind=pm_reel)

      call mu_angle2(rtamp1,rtamp2,rgb,code_retour_local) 
      if (code_retour_local%valeur == pm_err_vect_nul) then
         retour = pm_err_cni
         go to 6000
      end if
      retour = ialign
      go to 6000
   endif

!   longitude du noeud ascendant
!   on a initialise gom = 0 (valeur de gom pour ind_sens=1)
   if (ind_sens == -1) gom = pm_pi
!   angle entre le noeud ascendant et le point a
   rtamp1 = - vect_wu(2) * pos_car_t0(1)
   rtamp2 = pos_car_t0(3)

   call mu_angle2(rtamp1,rtamp2,rga,code_retour_local) 
   if (code_retour_local%valeur == pm_err_vect_nul) then
      retour = pm_err_cni
      go to 6000
   end if

!   angle entre le noeud ascendant et le point b
   rtamp1 = - vect_wu(2) * pos_car_t1(1)
   rtamp2 = pos_car_t1(3)

   call mu_angle2(rtamp1,rtamp2,rgb,code_retour_local) 
   if (code_retour_local%valeur == pm_err_vect_nul) then
      retour = pm_err_cni
      go to 6000
   end if
   retour = ialign
   go to 6000
endif

!     -------------------------------------
! traitement cas des points non alignes
!     -------------------------------------

if (vect_w(3) < -eps100) then
!     w(3) est negatif (l'orbite est non polaire),
!     il faut tenir compte du signe de w(3)
   do  i = 1,3
      vect_w(i) = -vect_w(i)*real(ind_sens,kind=pm_reel)
   enddo
else
   do i = 1,3
      vect_w(i) = vect_w(i)*real(ind_sens,kind=pm_reel)
   enddo
endif

! norme du vecteur

call mu_norme(vect_w, norme_w, code_retour_local, vect_norme = vect_wu) ! calcul du rayon vecteur r
if (norme_w < eps100) then                   ! vecteur position nul
   retour = pm_err_pos_nul
   go to 6000
end if

! inclinaison

inc=acos(vect_wu(3))

if (sin(inc) > eps100) then

!   ----------------------------------------
!    cas orbite non equatoriale (cas general)
!   ----------------------------------------

!    longitude du noeud ascendant

   rtamp1 = -vect_wu(2)
   rtamp2 =  vect_wu(1)

   call mu_angle2(rtamp1,rtamp2,gom,code_retour_local) 
   if (code_retour_local%valeur == pm_err_vect_nul) then
      retour = pm_err_cni
      go to 6000
   end if

!    angle entre le noeud ascendant et le point a

   rtamp1 = pos_car_t0(2)*vect_wu(1) - pos_car_t0(1)*vect_wu(2)
   rtamp2 = pos_car_t0(3)

   call mu_angle2(rtamp1,rtamp2,rga,code_retour_local) 
   if (code_retour_local%valeur == pm_err_vect_nul) then
      retour = pm_err_cni
      go to 6000
   end if

!    angle entre le noeud ascendant et le point b

   rtamp1 = pos_car_t1(2)*vect_wu(1)-pos_car_t1(1)*vect_wu(2)
   rtamp2 = pos_car_t1(3)

   call mu_angle2(rtamp1,rtamp2,rgb,code_retour_local) 
   if (code_retour_local%valeur == pm_err_vect_nul) then
      retour = pm_err_cni
      go to 6000
   end if

else
!   ----------------------
!    cas orbite equatoriale
!   ----------------------

!    inclinaison
   if (ind_sens == -1) then
      inc = pm_pi
   else
      inc = 0._pm_reel
   endif

! longitude du noeud ascendant nul
   gom = 0._pm_reel

!    angle entre le noeud ascendant et le point a
   rtamp1 = pos_car_t0(1)
   rtamp2 = pos_car_t0(2) * real(ind_sens,kind=pm_reel)

   call mu_angle2(rtamp1,rtamp2,rga,code_retour_local) 
   if (code_retour_local%valeur == pm_err_vect_nul) then
      retour = pm_err_cni
      go to 6000
   end if

!   angle entre le noeud ascendant et le point b

   rtamp1 = pos_car_t1(1)
   rtamp2 = pos_car_t1(2) * real(ind_sens,kind=pm_reel)

   call mu_angle2(rtamp1,rtamp2,rgb,code_retour_local)
   if (code_retour_local%valeur == pm_err_vect_nul) then
      retour = pm_err_cni
      go to 6000
   end if
   
endif

6000 continue

end subroutine mii_pb_parg0
