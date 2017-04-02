subroutine mt_car_geod (pos_car, r_equa, apla, pos_geod, code_retour, jacob)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Passage des coordonnees CARtesiennes aux coordonnees GEODesiques
! ===
!
! Note d'utilisation: 1) La transformation inverse peut etre effectuee par mt_geod_car.
! ==================  2) L'aplatissement doit appartenir a [0.,1.[
!                     3) Le rayon equatorial doit etre >0 : aucun test n'est effectue ici.
!                     
!
!$Historique
! ==========
!   + Version 1.0 (SP 247 ed01 rev00): creation a partir de la routine MVCAEL de la MSLIB f77
!                         (Date: 08/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE 403 ed01 rev00): ajout de l'utilisation du code retour pm_warn_pos_Oz_ref
!                         (Date: 01/2000 - Realisation: Sylvain Vresk)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 4.1 (DE 485 ed01 rev00) : calcul de la jacobienne
!                         (Date: 08/2003 - Realisation: Bruno Revelin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : (Date : 02/05/2007 - Realisation: Sandrine Avril - Atos origin)
!                   DM-ID 636 : contrôle de la latitude pour la fonction mt_geoc_car
!   + Version 6.6 : DM-ID 616 remplacement du module math_mslib
!     par une sélection de int_constantes
!                   (Date: 05/2007 - Realisation: Atos origin)
!   + Version 6.9 : FA-ID 1108 : Ajout de parenthèses dans expression ambigue
!                   (Date: 09/2008 - Realisation: Atos origin)
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
use int_utilitaires, only : mu_angle2

use int_constantes, only : pm_pi,pm_deux_pi,pm_pi_sur2,pm_deg_rad,pm_rad_deg


use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), dimension(3), intent(in)              ::  pos_car      ! position en coordonnees cartesiennes
real(pm_reel), intent(in)                            ::  r_equa       ! rayon equatorial terrestre
real(pm_reel), intent(in)                            ::  apla         ! aplatissement terrestre
type(tm_geodesique), intent(out)                     ::  pos_geod     ! latitude, longitude, hauteur geodesiques
type(tm_code_retour), intent(out)                    ::  code_retour
real(pm_reel), dimension(3,3), intent(out), optional ::  jacob        ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel)    :: eps          !  epsilon du module (max(1.e-09,1.0e+3*epsdp))
real(pm_reel)    :: eps100       !  epsilon de test pour les reels
real(pm_reel)    :: rlat0        !  valeur de la latitude d'initialisation
real(pm_reel)    :: corxy, corapl!  valeurs intermediaires representant des termes de correction
real(pm_reel)    :: glat0, reqcor, crapl2, requa2, rqcor2, coralt!     termes intermediaires de calcul
real(pm_reel)    :: epsreq       !  variable servant a faire des tests de distance = r_equa*eps
real(pm_reel)    :: dist2        !  distance au carre (pos_car(1)**2+pos_car(2)**2+pos_car(3)**2)
real(pm_reel)    :: disxy2, disxy!  distance pos_car(1) - pos_car(2):disxy2 = (pos_car(1)**2+pos_car(2)**2), disxy = sqrt(disxy2)
real(pm_reel)    :: rasb         !  rasb = a/b pour calcul de l'arctangente de a/b
real(pm_reel)    :: denomi, sinl0, tanl0                !     variables de calculs intermediaires
real(pm_reel)    :: Rphi=0._pm_reel, fonc, dfonc, alpha            !     rayon, fonction et derivee pour
                                                        !     Newton et coefficient alpha qui intervient dans le calcul de fonc 
                                                        !     et dfonc
real(pm_reel)    :: exc2=0._pm_reel         !  carre de l'excentricite i.e. 2*aplat - aplat**2
real(pm_reel)    :: rx1, ry1, rz1
real(pm_reel)    :: rlon, rlat, rhaut  ! longitude, latitude et hauteur intermediaires de calcul

logical ::   testcv, intell, cas3ra   !     variables logiques resp. pour le test de convergence, pour 
                                      !     determiner si on se trouve ou non a l'interieur de l'ellipsoide 
                                      !     et pour determiner si on se trouve dans un cas ou il peut y 
                                      !     avoir 3 racines
logical ::   oknewt                   !     variables de travail servant a verfier si la solution fournie par Newton
                                      !     est correcte ou pas
real(pm_reel):: K,KRphiplusH, RphiplusH, correc=1._pm_reel  !     variables de calcul pour la jacobienne

integer ::   iter                     !     numero de l'iteration courante
integer, parameter ::   niter=30      !     nombre d'iterations maximum pour la recherche de la latitude(schema de Newton)     
type(tm_code_retour) :: code_retour_interne ! code retour intermediaire

intrinsic max, epsilon, sqrt, abs, sign, atan, sin, cos

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mt_car_geod.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mt_car_geod.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! verification des donnees d'entree
!..................................
if (r_equa <= 1._pm_reel) then ! rayon équatorial inférieur ou egal à 1
   code_retour%valeur = pm_err_r_equa_inf_egal1
   go to 6000
end if

! initialisation constante de test
! ................................

eps100 = 100._pm_reel * epsilon(1._pm_reel)

eps = max(1.0e-09_pm_reel, 10._pm_reel*eps100)
epsreq = r_equa*eps

if (apla < 0._pm_reel) then
   code_retour%valeur = pm_err_apla_negatif
   go to 6000
else if ((apla - 1._pm_reel) >= 0._pm_reel ) then ! aplatissement superieur ou egal a 1
   code_retour%valeur = pm_err_apla_sup1
   go to 6000
end if

disxy2 = pos_car(1)*pos_car(1) + pos_car(2)*pos_car(2)
disxy = sqrt(disxy2)
dist2 = pos_car(3)*pos_car(3) + disxy2

if (dist2  <  (epsreq*epsreq)) then   ! coordonnees d'entree au centre de la Terre
   code_retour%valeur = pm_err_pos_nul
   go to 6000
end if

!calcul de la longitude
!======================

if (abs(pos_car(2))  <  epsreq) then  ! la longitude vaut 0 ou pi

   if (pos_car(1)  >=  0._pm_reel) rlon = 0._pm_reel
   if (pos_car(1)  <   0._pm_reel) rlon = pm_pi

else

   call mu_angle2 (pos_car(1), pos_car(2), rlon, code_retour_interne) ! pas de test a faire sur code retour car pos_car(2) different de 0

end if

!calcul de la latitude et de la hauteur
!======================================

if (disxy  <  epsreq) then   !    latitude = +- pi/2: situation sur l'axe des poles

   rlat = sign(1._pm_reel, pos_car(3))*pm_pi_sur2
   rhaut = abs(pos_car(3)) - r_equa*(1._pm_reel - apla)

   code_retour%valeur = pm_warn_pos_Oz_ref

   ! NB: le jacobien ne sera pas calculable dans ce cas

else ! en dehors de l'axe des poles

   if (abs(pos_car(3))  <  epsreq) then    ! situation dans le plan de l'equateur

      rlat = 0._pm_reel
      rhaut = disxy - r_equa

      if (present(jacob)) then             ! variables pour le calcul de la jacobienne
         exc2   = apla*(2._pm_reel - apla)
         correc = 1._pm_reel - exc2 * (sin(rlat)**2) ! correc est > 0.
                                                     ! correc pourrait etre nul si apla=1: impossible a ce stade des calculs
         Rphi = r_equa/sqrt(correc)                  ! rayon a la latitude calculee
      end if

   else      ! initialisation du schema de Newton

      iter = 0
      !
      !initialisation de diverses constantes de calcul
      !
      corapl = 1._pm_reel - apla    ! dans ]0.,1.]
      exc2   = apla*(2._pm_reel - apla)
      crapl2 = corapl*corapl
      reqcor = r_equa*corapl
      requa2 = r_equa*r_equa
      rqcor2 = reqcor*reqcor
      coralt = disxy2/requa2 + (pos_car(3)*pos_car(3))/rqcor2
      !
      rhaut = sqrt(dist2)*(1._pm_reel - 1._pm_reel/sqrt(coralt))             !  valeur approchee de la hauteur
      !
      corxy = crapl2*disxy
      rasb  = pos_car(3)/corxy
      glat0 = atan (rasb)
      !
      alpha = disxy
      intell = ((disxy2/requa2 + (pos_car(3)**2)/rqcor2)  <  1._pm_reel)       !  initialisation de la latitude
      !
      if (intell) then                                                       ! point a l'interieur de l'ellipsoide

         cas3ra = ((alpha - exc2*r_equa)  <  0._pm_reel)

         if (cas3ra) then            !   cas complexe a 3 racines
            rlat = sqrt((r_equa/alpha)**2 - 1._pm_reel)/corapl
            rlat = atan(rlat)
            if (pos_car(3)  <  0._pm_reel) rlat = - rlat
         else
            rlat = glat0 - apla/reqcor * sin(2._pm_reel*glat0)*rhaut
         end if

      else if ((rhaut  <=  r_equa) .and. (apla  <=  1._pm_reel/2._pm_reel)) then !  point a l'exterieur de l'ellipsoide

         rlat = glat0 - apla/reqcor * sin(2._pm_reel*glat0)* rhaut  !  hauteur <= r_equa et apla <= 1./2.

      else

         rlat = glat0                                               ! hauteur > r_equa ou apla > 1./2.

      end if

      if (rlat  >  pm_pi_sur2) then      !  recalage eventuel de la latitude initiale entre - pi/2 et pi/2
         rlat = pm_pi - rlat
      else if (rlat  <  - pm_pi_sur2) then
         rlat = - pm_pi - rlat
      end if

      !  iterations du schema de Newton
      !  ------------------------------
      testcv = .true.

      do while (testcv .and. (iter  <  niter))

         iter  = iter + 1
         rlat0 = rlat
         sinl0 = sin(rlat0)
         tanl0 = tan(rlat0)
         denomi = sqrt(1._pm_reel - exc2*sinl0**2)
         fonc  = - sinl0*exc2*r_equa/denomi + alpha*tanl0 - pos_car(3)
         dfonc = r_equa/denomi**3
         dfonc = - dfonc*exc2*cos(rlat0) + alpha*(1._pm_reel + tanl0**2)

         rlat  = rlat0 - fonc/dfonc      ! mise a jour de la latitude par Newton

         if (rlat  >  pm_pi_sur2) then   ! recalage eventuel de la latitude entre -pi/2 et pi/2
            rlat = 0.5_pm_reel*(pm_pi_sur2 + rlat0)
         else if (rlat  <  - pm_pi_sur2) then
            rlat = 0.5_pm_reel*(- pm_pi_sur2 + rlat0)
         end if

         testcv = (abs(rlat - rlat0)  >  eps)   !  test de convergence: testcv vaut .true. si on n'a pas la convergence,
                                                !  .false. sinon

      end do

      
      correc = 1._pm_reel - exc2 * (sin(rlat)**2) ! correc est > 0.
                                                  ! correc pourrait etre nul si apla=1: impossible a ce stade des calculs
      Rphi = r_equa/sqrt(correc)                  ! rayon a la latitude calculee
      rhaut = alpha/cos(rlat) - Rphi              ! hauteur a la latitude calculee

      rx1 = (Rphi + rhaut)*cos(rlat)*cos(rlon)
      ry1 = (Rphi + rhaut)*cos(rlat)*sin(rlon)
      rz1 = (Rphi*crapl2 + rhaut)*sin(rlat)

      ! test de convergence sur les positions
      oknewt =((abs(rx1 - pos_car(1)) <= 1.0e2_pm_reel*epsreq) &
          .and.(abs(ry1 - pos_car(2)) <= 1.0e2_pm_reel*epsreq) &
          .and.(abs(rz1 - pos_car(3)) <= 1.0e2_pm_reel*epsreq))

!      ----------------------------------------------------------------
!      on verifie qu'il y a solution et si la solution obtenue par Newton est correcte
!      i.e. que -pi/2 <= rlat <= pi/2, que rhaut >=0 quand on se trouve
!      a l'exterieur de l'ellipsoide et que les residus en rx, ry et rz
!      sont faibles
!      ----------------------------------------------------------------
      if (testcv .OR. ((.not. intell).and.(rhaut < -1.0e+3_pm_reel*epsreq)) .OR. &
          (.not.oknewt) .OR.((Rphi*crapl2 + rhaut) < 0._pm_reel)) then
         code_retour%valeur = pm_err_conv_car_geod
         go to 6000
      end if

   end if ! fin Newton (hors plan de l'equateur)

end if ! fin cas : axe des poles

pos_geod%lat = rlat
pos_geod%long= rlon
pos_geod%haut= rhaut

if (present(jacob)) then

   if (code_retour%valeur == pm_warn_pos_Oz_ref) then
      code_retour%valeur = pm_err_jac_non_calc_poles
      go to 6000
   end if

   K = (1._pm_reel - exc2)/ correc               ! K dans [0,1]

  ! si apla = 0 : exc2 = 0 ; correc = 1 ; Rphi = r_equa; K = 1

   KRphiplusH = K*Rphi + rhaut
   RphiplusH = Rphi + rhaut

   if (KRphiplusH < epsreq) then
      code_retour%valeur = pm_err_jac_non_calc_alt_neg
      go to 6000
   end if

   jacob(1,1) = -sin(rlat)*cos(rlon)/KRphiplusH
   jacob(1,2) = -sin(rlat)*sin(rlon)/KRphiplusH
   jacob(1,3) = cos(rlat)/KRphiplusH

   jacob(2,1) = -sin(rlon)/(RphiplusH*cos(rlat))
   jacob(2,2) = cos(rlon)/(RphiplusH*cos(rlat))
   jacob(2,3) = 0._pm_reel

   jacob(3,1) = cos(rlat)*cos(rlon)
   jacob(3,2) = cos(rlat)*sin(rlon)
   jacob(3,3) = sin(rlat)

end if
   

6000 continue

code_retour%routine = pm_num_mt_car_geod
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mt_car_geod
