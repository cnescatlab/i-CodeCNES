subroutine mv_cir_car (mu, cir, pos_car, vit_car, code_retour, jacob)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Passage des parametres orbitaux adaptes a l'orbite CIRculaire, aux parametres CARtesiens
! ===
!
! Note d'utilisation:  1) Applicable aux orbites circulaires ou elliptiques, equatoriales ou non
! ==================   3) Non applicable aux orbites hyperboliques et paraboliques
!                      4) Pour les orbites hyperboliques ou paraboliques, voir mv_kep_car.
!                      5) L'element (i,j) de la jacobienne correspond a la derivee partielle du parametre cartesien numero i
!                         par rapport a la derivee partielle du parametre dit adapte a l'orbite circulaire non equatoriale numero j.
!                      6) Les unites en entree doivent etre coherentes entre elles. Ex.:  mu en m**3/s**2,cir en metres et
!                         radians,ainsi pos_car sera en metres et vit_car en m/s.
!                      7) La transformation inverse (sauf en cas d'orbite equatoriale) peut s'effectuer
!                         par mv_car_cir.
!$Historique
! ==========
!   + Version 1.0 (SP 235 ed01 rev00): creation a partir de la routine MVOSRC de la MSLIB f77
!                         (Date: 08/1998 - Realisation: Veronique Lepine)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Bruno Revelin, Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
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

use int_chgmnt_variables, only : mv_kepler_gene

use test_mslib

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use numero_routine_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       
real(pm_reel), intent(in)               :: mu  ! constante de la gravitation universelle
type(tm_orb_cir),intent(in)             :: cir      ! parametres orbitaux de l'orbite circulaire
real(pm_reel), dimension(3), intent(out):: pos_car  ! vecteur position du satellite
real(pm_reel), dimension(3), intent(out):: vit_car  ! vecteur vitesse du satellite
type(tm_code_retour), intent(out)       ::  code_retour
real(pm_reel), dimension(6,6), intent(out), optional :: jacob ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel)     :: a,ex,ey,ai,agom,um                       ! parametres orbitaux :
                                                              ! a
                                                              ! ex =e*cos(petit omega)
                                                              ! ey =e*sin(petit omega)
                                                              ! i
                                                              ! grand omega
                                                              ! um = petit omega + M
real(pm_reel)     :: ue,e2                                    ! ue = petit omega + E, e2 = e*e 
real(pm_reel)     :: cai,sai,cgo,sgo,cue,sue                  ! variables cos et sin des angles i,gomega,ue
real(pm_reel)     :: exey,ex2,ey2,exceys,eycexs,sq            ! autres variables intermediaires
real(pm_reel)     :: umey, betey, betex
real(pm_reel)     :: rld1,rmu1,rnu1,rki1,sqmusa               ! variables ld1,mu1,nu1,ki1,sqrt(mu/a)
real(pm_reel)     :: xdldex,xdmuex,xdldey,xdmuey,xdldue,xdmuue! et leurs derivees par rapport a ex, ey et ue
real(pm_reel)     :: xdnuex,xdkiex,xdnuey,xdkiey,xdnuue,xdkiue
real(pm_reel)     :: xdueum,xdueex,xdueey                     ! derivees de ue % um , de ue % ex et de ue % ey
real(pm_reel)     :: beta,xdbex,xdbey                         ! variable beta et ses derivees par rapport a ex et ey
real(pm_reel)     :: eps100                                   ! variable epsilon machine*100 
real(pm_reel), dimension(3) :: o,h                            ! vecteurs unitaires o et h
real(pm_reel), dimension(3) :: xdoai,xdhai,xdogo,xdhgo        ! derivees par rapport a i et gomega

intrinsic epsilon, sqrt, sin, cos, present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mv_cir_car.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mv_cir_car.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

      a  = cir%a
      ex = cir%ex
      ey = cir%ey
      ai = cir%i
      agom = cir%gom
      um = cir%pso_M

! initialisation constante de test
! ................................

eps100 = 100._pm_reel * epsilon(1._pm_reel)

! Controle des parametres d'entree
!=================================

e2 = ex*ex + ey*ey
if (sqrt(e2) > ( 1._pm_reel -  pm_eps_parab )) then!      orbite parabolique ou hyperbolique
   code_retour%valeur = pm_err_e_non_ellip
   go to 6000
end if

if (mu <= eps100) then
   if (mu < 0._pm_reel) then              ! constante de la gravitation negative
      code_retour%valeur = pm_err_mu_negatif
   else
      code_retour%valeur = pm_err_mu_nul  ! constante de la gravitation nulle
   end if
   go to 6000
end if

if (a <= eps100) then
   if (a < 0._pm_reel) then              ! demi grand axe negatif
      code_retour%valeur = pm_err_a_negatif
   else
      code_retour%valeur = pm_err_a_nul  ! demi grand axe nul
   end if
   go to 6000
end if

! calcul ue (= petit omega + E) par equation de kepler generalisee
! ==========================================================

call mv_kepler_gene (um,ex,ey,ue,code_retour)! en retour de mv_kepler_gene:  
                                             !code_retour%valeur = 0, pm_err_e_non_ellip, pm_err_conv_kepler_gene
if (code_retour%valeur /= pm_OK) go to 6000

!*************************************************************
! 1- calcul vecteur position r = a (ld1*o + mu1*h)
!        et vecteur vitesse rv = sqrt(mu/a) (nu1*o + ki1*h)
!*************************************************************

cue = cos(ue) ! cosinus de pom + E
sue = sin(ue) ! sinus de pom + E

cai = cos(ai)  !     cosinus de i 
sai = sin(ai)  !     sinus de i
cgo = cos(agom)!     cosinus de gomega
sgo = sin(agom)!     sinus degomega

sq = sqrt(1._pm_reel-e2)!     calcul de sqrt(1-ex**2-ey**2) remarque: 1-e2 > 0 car on a teste si e etait bien dans [0., 1.[
exey = ex * ey
ex2 = ex*ex
ey2 = ey*ey

exceys = (ex * cue) + (ey * sue)
eycexs = (ey * cue) - (ex * sue)

o(1) = cgo
o(2) = sgo          ! calcul des composantes du vecteur p en fonction des angles i,gomega
o(3) = 0._pm_reel

h(1) = - cai * sgo
h(2) = cai * cgo    ! calcul des composantes du vecteur q en fonction des angles i,gomega
h(3) = sai

beta = 1._pm_reel / (1._pm_reel + sq)!     calcul variable beta = 1 / 1+sqrt(1-ex2-ey2)

rld1 = -ex + ((1._pm_reel - (beta*ey2)) * cue) + (beta * exey * sue)! calcul de ld1 et de
rmu1 = -ey + ((1._pm_reel - (beta*ex2)) * sue) + (beta * exey * cue)! mu1
  
sqmusa = sqrt(mu/a)! avec mu et a > 0 a ce stade des calculs

rnu1 = (-sue + (beta*ey*exceys)) / (1._pm_reel - exceys)!  remarque: (1-exceys) different de 0 car e < 1 a ce stade des calculs
rki1 = (cue - (beta*ex*exceys)) / (1._pm_reel - exceys)

! calcul des composantes des vecteurs position et vitesse
!========================================================

pos_car(:) = a * ((rld1 * o(:)) + (rmu1 * h(:)))
vit_car(:) = sqmusa * ((rnu1 * o(:)) + (rki1 * h(:)))

!********************************************
! 2- calcul du jacobien de la transformation
!********************************************

if (present(jacob)) then

   xdoai(:) = 0._pm_reel!     derivees des composantes de o par rapport a i

   xdhai(1) = sai * sgo
   xdhai(2) = -sai * cgo!     derivees des composantes de h par rapport a i
   xdhai(3) = cai

   xdogo(1) = - sgo
   xdogo(2) = cgo        !     derivees des composantes de o par rapport a gomega
   xdogo(3) = 0._pm_reel

   xdhgo(1) = - cai * cgo
   xdhgo(2) = - cai * sgo!     derivees des composantes de h par rapport a gomega
   xdhgo(3) = 0._pm_reel

   xdbex = (ex / sq) * (1._pm_reel / (1._pm_reel+sq)**2)!     calcul derivee de beta par rapport a ex 
   xdbey = (ey / sq) * (1._pm_reel / (1._pm_reel+sq)**2)!     et ey

   xdldex = -1._pm_reel - (ey2*cue*xdbex) + (beta*ey*sue) + (xdbex*exey*sue)! calcul des derivees de ld1 et mu1 par rapport a ex
   xdmuex = (beta * (eycexs - (ex*sue))) + (xdbex*ex*eycexs)

   xdldey = - (beta * ((ey*cue) + eycexs)) - (xdbey*ey*eycexs)              ! par rapport a ey 
   xdmuey = -1._pm_reel - (ex2*sue*xdbey) + (beta*ex*cue) + (xdbey*exey*cue)

   xdldue = ((1._pm_reel - (beta*ey2)) * (-sue)) + (beta*exey*cue)          !et par rapport a ue
   xdmuue = ((1._pm_reel - (beta*ex2)) * cue) - (beta*exey*sue)

   umey = 1._pm_reel - exceys
   betey = beta*ey*exceys
   betex = beta*ex*exceys

   ! calcul des derivees de nu1 et ki1 par rapport a ex,ey et ue
   xdnuex = ((-cue * (sue - betey)) / umey**2)+ (((xdbex*ey*exceys) + (beta*ey*cue)) / umey)
   xdkiex = ((cue * (cue - betex)) / umey**2)- (((xdbex*ex*exceys) + (beta * (exceys+(ex*cue)))) /umey)
   xdnuey = ((-sue * (sue - betey)) / umey**2)+ (((xdbey*ey*exceys) + (beta * (exceys+(ey*sue)))) /umey)
   xdkiey = ((sue * (cue - betex)) / umey**2)- (((xdbey*ex*exceys) + (beta*ex*sue)) / umey)
   xdnuue = ((-cue + (beta*ey*eycexs)) / umey)+ (((-sue + betey) * eycexs) / umey**2)
   xdkiue = ((-sue - (beta*ex*eycexs)) / umey)+ (((cue - betex) * eycexs) / umey**2)

   xdueum = 1._pm_reel / umey!    calcul derivees de ue par rapport a um , ex et ey
   xdueex = sue * xdueum     !    due/dum = 1 / 1-exceys, due/dex = sinue * due/dum
   xdueey = -cue * xdueum    !    due/dey = -cosue * due/dum

   !===========================================================
   ! calcul des derivees partielles des vecteurs position et
   ! vitesse par rapport a a,ex,ey,i,gomega,um
   !===========================================================

   !        par rappport a a
   jacob(1:3,1) = (rld1 * o(:)) + (rmu1 * h(:))
   jacob(4:6,1) = (-0.5_pm_reel*sqmusa/a) * ((rnu1 * o(:)) + (rki1 * h(:)))

   !        par rappport a ex ( = derivee/ex + derivee/ue * derivee ue/ex)
   jacob(1:3,2) = a * (((xdldex*o(:)) + (xdmuex*h(:))) +((xdldue*o(:)) + (xdmuue*h(:))) * xdueex)
   jacob(4:6,2) = sqmusa * (((xdnuex*o(:)) + (xdkiex*h(:))) +((xdnuue*o(:)) + (xdkiue*h(:))) * xdueex)

   !        par rappport a ey ( = derivee/ey + derivee/ue * derivee ue/ey)
   jacob(1:3,3) = a * (((xdldey*o(:)) + (xdmuey*h(:))) +((xdldue*o(:)) + (xdmuue*h(:))) * xdueey)
   jacob(4:6,3) = sqmusa * (((xdnuey*o(:)) + (xdkiey*h(:))) +((xdnuue*o(:)) + (xdkiue*h(:))) * xdueey)

   !        par rappport a i
   jacob(1:3,4) = a * ((rld1 * xdoai(:)) + (rmu1 * xdhai(:)))
   jacob(4:6,4) = sqmusa * ((rnu1 * xdoai(:)) + (rki1 * xdhai(:)))

   !        par rappport a gomega
   jacob(1:3,5) = a * ((rld1 * xdogo(:)) + (rmu1 * xdhgo(:)))
   jacob(4:6,5) = sqmusa * ((rnu1 * xdogo(:)) + (rki1 * xdhgo(:)))

   !        par rappport a um ( = derivee/ue * derivee de ue/um)
   jacob(1:3,6) = a * ((xdldue * o(:)) + (xdmuue * h(:))) * xdueum
   jacob(4:6,6) = sqmusa * ((xdnuue * o(:)) + (xdkiue * h(:))) *xdueum

end if

6000 continue

code_retour%routine = pm_num_mv_cir_car
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mv_cir_car
