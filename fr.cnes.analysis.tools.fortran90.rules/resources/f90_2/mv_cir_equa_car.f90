subroutine mv_cir_equa_car (mu, cir_equa, pos_car, vit_car, code_retour, jacob)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But: Passage des parametres orbitaux dits adaptes a l'orbite CIRculaire EQUAtoriale, aux parametres CARtesiens 
! ===
!
! Note d'utilisation:  1) Applicable aux orbites circulaires ou elliptiques, equatoriales ou non.
! ==================   2) Non applicable aux orbites hyperboliques ou paraboliques.
!                      3) Pour les cas hyperboliques ou paraboliques, voir mv_kep_car.
!                      4) L'element (i,j) de la jacobienne correspond a la derivee partielle du parametre cartesien numero i
!                         par rapport a la derivee partielle du parametre dit adapte a l'orbite circulaire equatoriale numero j.
!                      5) Les unites en entree doivent etre coherentes entre elles. Ex.:  mu en m**3/s**2, cir_equa en metres et
!                         radians,ainsi pos_car sera en metres et vit_car en m/s.
!                      6) La routine mv_car_cir_equa effectue la transformation inverse.

!$Historique
! ==========
!   + Version 1.0 (SP 245 ed01 rev00): creation a partir de la routine MVOSRE de la MSLIB f77
!                         (Date: 08/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (FA 373 ed01 rev00): Introduction du parametre pm_err_i_equa_retro : h**2 > 4*(1 - 100*epsilon(1._pm_reel))
!                         (Date: 09/1999 - Realisation: Sylvain Vresk)
!   + Version 3.1 (DE globale 439 ed01 rev00) : ajout des champs %biblio et %message pour le code retour
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 4.1 (DE globale 482 ed01 rev00): Corrections qualite
!                         (Date: 05/2003 - Realisation: Veronique Lepine)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
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
                                                       
real(pm_reel), intent(in)               :: mu                 ! constante de la gravitation universelle
type(tm_orb_cir_equa),intent(in)        :: cir_equa           ! parametres orbitaux de l'orbite circulaire equatoriale
real(pm_reel), dimension(3), intent(out):: pos_car            ! vecteur position du satellite
real(pm_reel), dimension(3), intent(out):: vit_car            ! vecteur vitesse du satellite
type(tm_code_retour), intent(out)       :: code_retour
real(pm_reel), dimension(6,6), intent(out), optional :: jacob! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel)  ::   a,ex,ey,hx,hy,alm,ale    !     parametres orbitaux (a,ex(=e*cos(petit omega+grand omega)),
                                             !     ey(=e*sin(petit omega+grand omega)),hx(=2*sin(i/2)*cos(gomega)),
                                             !     hy(=2*sin(i/2)*sin(gomega)),lm(=pomega+gomega+M),
                                             !     le(=pomega+gomega+E))

real(pm_reel), dimension(3)  :: x1,y1,xdx1hx,xdy1hx,xdx1hy,xdy1hy ! vecteurs unitaires x1 et y1, derivees par rapport a hx et hy
real(pm_reel)  ::   cle,sle                                       ! variables cos et sin de l'angle le
real(pm_reel)  ::   exey,ex2,ey2,exceys,eycexs,sq,e2,h2           ! autres variables 
real(pm_reel)  ::   sqh,xdsqhx,xdsqhy,hx2,hy2,hxhy,unme           ! intermediaires
real(pm_reel)  ::   rld2,rmu2,rnu2,rki2,sqmusa                    ! variables ld2,mu2,nu2,ki2,sqrt(mu/a)
real(pm_reel)  ::   xdldex,xdmuex,xdldey,xdmuey,xdldle,xdmule     ! et leurs derivees par rapport a ex, ey et le
real(pm_reel)  ::   xdnuex,xdkiex,xdnuey,xdkiey,xdnule,xdkile
real(pm_reel)  ::   xdlelm,xdleex,xdleey                          ! derivees de le % lm , de le % ex et de le % ey
real(pm_reel)  ::   beta,xdbex,xdbey                              ! variable beta et ses derivees par rapport a ex et ey
real(pm_reel)  ::   eps100                                        ! epsilon machine*100

intrinsic epsilon, present, sqrt, cos, sin

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mv_cir_equa_car.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mv_cir_equa_car.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! initialisation constante de test
! ................................

eps100 = 100._pm_reel * epsilon(1._pm_reel)

a   = cir_equa%a
ex  = cir_equa%ex
ey  = cir_equa%ey
hx  = cir_equa%ix
hy  = cir_equa%iy
alm = cir_equa%pso_M

!==========================================================
! test des parametres d'entree
!==========================================================

ex2 = ex*ex
ey2 = ey*ey
e2 = ex2 + ey2

if (sqrt(e2) > ( 1._pm_reel -  pm_eps_parab )) then !    orbite parabolique ou hyperbolique
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
   if (a < 0._pm_reel) then            ! demi-grand axe negatif
      code_retour%valeur = pm_err_a_negatif
   else                                ! demi-grand axe nul
      code_retour%valeur = pm_err_a_nul
   end if
   go to 6000
end if

hx2 = hx*hx
hy2 = hy*hy
h2 = hx2 + hy2
if (h2 > 4._pm_reel) then                   !  norme du vecteur inclinaison trop grande
   code_retour%valeur = pm_err_ix_iy_sup2
   go to 6000
end if
if (h2 > (4._pm_reel*(1._pm_reel-eps100))) then      ! la definition du vecteur inclinaison (hx,hy) impose que l'inclinaison i soit
   code_retour%valeur = pm_err_i_equa_retro ! inferieure a (pi - epsilon_retro) -> test si h**2 <= 4*(cos(epsilon_retro/2)**2)
   go to 6000                               ! soit h**2 <= 4*(1-100*epsilon) si epsilon_retro = 20*sqrt(epsilon) 
end if

! ==============================================================
! calcul ale (=pomega+gomega+E) par equation kepler generalisee
! ==============================================================

call mv_kepler_gene(alm, ex, ey, ale, code_retour)   ! en retour de mv_kepler_gene:  
                                                     !code_retour%valeur = 0, pm_err_e_non_ellip, pm_err_conv_kepler_gene
if (code_retour%valeur < pm_OK) go to 6000

cle = cos(ale)
sle = sin(ale)

! *************************************************************
! 1- calcul vecteur position r = a (ld2*x1 + mu2*y1)
!        et vecteur vitesse rv = sqrt(mu/a) (nu2*x1 + ki2*y1)
! *************************************************************

!     -------------------------------------
!     calcul de variables intermediaires
!     -------------------------------------

sq = sqrt(1._pm_reel-e2)   !     calcul de sqrt(1-ex**2-ey**2)
                           !     remarque: 1-e2 > 0 car on a teste si e etait dans [0.,1.[
exey = ex * ey   
     
exceys = (ex * cle) + (ey * sle)   ! ->  ex*cos( petit omega + grand omega + E) + ey*sin ( petit omega + grand omega + E) 
eycexs = (ey * cle) - (ex * sle)   ! ->  ey*cos( petit omega + grand omega + E) - ex*sin ( petit omega + grand omega + E)

hxhy = hx * hy                     !     calcul variable sqh = sqrt(1._pm_reel-((hx2+hy2)/4._pm_reel))

sqh = sqrt(1._pm_reel - (h2/4._pm_reel))!     remarque: 1-h2/4 > 0 car on a teste si h2 etait > 4.

x1(1) = 1._pm_reel - (0.5_pm_reel*hy2)
x1(2) = 0.5_pm_reel * hxhy            ! calcul des composantes du vecteurs x1  en fonction de hx,hy
x1(3) = -hy * sqh

y1(1) = 0.5_pm_reel * hxhy
y1(2) = 1._pm_reel - (0.5_pm_reel*hx2)! calcul des composantes du vecteurs  y1 en fonction de hx,hy
y1(3) = hx * sqh

beta = 1._pm_reel / (1._pm_reel + sq) !     calcul variable beta = 1 / 1+sqrt(1-ex2-ey2)

rld2 = -ex + ((1._pm_reel - (beta*ey2)) * cle) + (beta * exey * sle)! calcul de ld2 
rmu2 = -ey + ((1._pm_reel - (beta*ex2)) * sle) + (beta * exey * cle)! calcul de mu2

sqmusa = sqrt(mu/a)!  mu et a > 0 a ce stade des calculs

rnu2 = (-sle + (beta*ey*exceys)) / (1._pm_reel - exceys)! remarque: (1-exceys) different de 0 car e < 1 a ce stade des calculs 
rki2 = (cle - (beta*ex*exceys)) / (1._pm_reel - exceys) 

!================================================================
! calcul des composantes des vecteurs position et vitesse
!================================================================

   pos_car(:) = a * ((rld2 * x1(:)) + (rmu2 * y1(:)))
   vit_car(:) = sqmusa * ((rnu2 * x1(:)) + (rki2 * y1(:)))

if (present(jacob)) then      ! calcul du jacobien de la transformation

   !     -----------------------------------
   !     calcul de variables intermediaires
   !     -----------------------------------

   !     calcul des derivees de sqh par rapport a hx et hy
   xdsqhx = -hx / (4._pm_reel*sqh)
   xdsqhy = -hy / (4._pm_reel*sqh)

   !     derivees des composantes de x1 et y1 par rapport a hx et hy
   xdx1hx(1) = 0._pm_reel
   xdx1hx(2) = 0.5_pm_reel * hy
   xdx1hx(3) = -hy * xdsqhx
   xdy1hx(1) = 0.5_pm_reel * hy
   xdy1hx(2) = -hx
   xdy1hx(3) = sqh + (hx*xdsqhx)
   xdx1hy(1) = -hy
   xdx1hy(2) = 0.5_pm_reel * hx
   xdx1hy(3) = -sqh - (hy*xdsqhy)
   xdy1hy(1) = 0.5_pm_reel * hx
   xdy1hy(2) = 0._pm_reel
   xdy1hy(3) = hx * xdsqhy

   !     calcul derivee de beta par rapport a ex et ey
   xdbex = (ex / sq) * (1._pm_reel / (1._pm_reel+sq)**2)
   xdbey = (ey / sq) * (1._pm_reel / (1._pm_reel+sq)**2)

   ! calcul des derivees de ld2 et mu2 par rapport a ex,ey et le
   xdldex = -1._pm_reel - (ey2*cle*xdbex) + (beta*ey*sle) + (xdbex*exey*sle)
   xdmuex = (beta * (eycexs - (ex*sle))) + (xdbex*ex*eycexs)
   xdldey = - (beta * ((ey*cle) + eycexs)) - (xdbey*ey*eycexs)
   xdmuey = -1._pm_reel - (ex2*sle*xdbey) + (beta*ex*cle) + (xdbey*exey*cle)
   xdldle = ((1._pm_reel - (beta*ey2)) * (-sle)) + (beta*exey*cle)
   xdmule = ((1._pm_reel - (beta*ex2)) * cle) - (beta*exey*sle)

   unme = 1._pm_reel-exceys

   ! calcul des derivees de nu2 et ki2 par rapport a ex,ey et le
   xdnuex =((-cle * (sle - (beta*ey*exceys))) / unme**2)+(((xdbex*ey*exceys) + (beta*ey*cle)) / unme)
   xdkiex =((cle * (cle - (beta*ex*exceys))) / unme**2)-(((xdbex*ex*exceys) + (beta * (exceys+(ex*cle)))) /unme)
   xdnuey =((-sle * (sle - (beta*ey*exceys))) / unme**2)+(((xdbey*ey*exceys) + (beta * (exceys+(ey*sle)))) /unme)
   xdkiey = ((sle * (cle - (beta*ex*exceys))) / unme**2)- (((xdbey*ex*exceys) + (beta*ex*sle)) / unme)
   xdnule = ((-cle + (beta*ey*eycexs)) / unme)+ (((-sle + (beta*ey*exceys)) * eycexs) / unme**2)
   xdkile = ((-sle - (beta*ex*eycexs)) / unme)+ (((cle - (beta*ex*exceys)) * eycexs) / unme**2)

   ! calcul derivees de le par rapport a lm , ex et ey
   !    dle/dlm = 1 / 1-exceys, dle/dex = sinue * dle/dlm
   !    dle/dey = -cosle * dle/dlm
   xdlelm = 1._pm_reel / unme
   xdleex = sle * xdlelm
   xdleey = -cle * xdlelm

   !===========================================================
   ! calcul des derivees partielles des vecteurs position et
   ! vitesse par rapport a a,ex,ey,hx,hy,lm
   !===========================================================

      !        par rapport a a
      jacob(1:3,1) = (rld2 * x1(:)) + (rmu2 * y1(:))
      jacob(4:6,1) = (-0.5_pm_reel*sqmusa/a) * ((rnu2 * x1(:)) + (rki2 * y1(:)))

      !        par rapport a ex ( = derivee/ex + derivee/le * derivee le/ex)
      jacob(1:3,2) = a * (((xdldex*x1(:)) + (xdmuex*y1(:))) +((xdldle*x1(:)) + (xdmule*y1(:))) * xdleex)
      jacob(4:6,2) = sqmusa * (((xdnuex*x1(:)) + (xdkiex*y1(:))) +((xdnule*x1(:)) + (xdkile*y1(:))) * xdleex)

      !        par rapport a ey ( = derivee/ey + derivee/le * derivee le/ey)
      jacob(1:3,3) = a * (((xdldey*x1(:)) + (xdmuey*y1(:))) +((xdldle*x1(:)) + (xdmule*y1(:))) * xdleey)
      jacob(4:6,3) = sqmusa * (((xdnuey*x1(:)) + (xdkiey*y1(:))) +((xdnule*x1(:)) + (xdkile*y1(:))) * xdleey)

      !        par rapport a hx
      jacob(1:3,4) = a * ((rld2 * xdx1hx(:)) + (rmu2 * xdy1hx(:)))
      jacob(4:6,4) = sqmusa * ((rnu2 * xdx1hx(:)) + (rki2 * xdy1hx(:)))

      !        par rapport a hy
      jacob(1:3,5) = a * ((rld2 * xdx1hy(:)) + (rmu2 * xdy1hy(:)))
      jacob(4:6,5) = sqmusa * ((rnu2 * xdx1hy(:)) + (rki2 * xdy1hy(:)))

      !        par rapport a um ( = derivee/ue * derivee de ue/um)
      jacob(1:3,6) = a * ((xdldle * x1(:)) + (xdmule * y1(:))) * xdlelm
      jacob(4:6,6) = sqmusa * ((xdnule * x1(:)) + (xdkile * y1(:))) *xdlelm

end if

6000  continue

      code_retour%routine = pm_num_mv_cir_equa_car
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

    end subroutine mv_cir_equa_car
