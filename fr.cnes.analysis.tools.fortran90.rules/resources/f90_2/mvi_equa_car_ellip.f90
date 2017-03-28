subroutine mvi_equa_car_ellip (equa,mu,pos_car,vit_car,retour,jacob)

! (C) Copyright CNES - MSLIB - 1998-2003

!************************************************************************
!
! But:  Passage des parametres orbitaux dits adaptes aux orbites EQUAtoriales non circulaires
! ===   aux parametres CARtesiens dans le cas ELLIPtique.
!
! Note d'utilisation:  - Routine interne
! ==================   - L'appelant doit tester que :
!                           1) l'excentricite est bien dans [0., 1.[
!                           2) le demi grand axe est > 0.
!                           3) la constante de la gravitation est > 0.
!                           4) la norme du vecteur inclinaison est < 2
!
!$Historique
! ==========
!   + Version 1.0 (SP 241 ed01 rev00): creation a partir de la routine MVELQC de la MSLIB f77
!                         (Date: 08/1998 - Realisation: Veronique Lepine)
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
use int_chgmnt_variables, only : mv_kepler_std

use precision_mslib
use type_mslib
use valeur_code_retour_mslib
use longueur_chaine_mslib

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                       

type(tm_orb_equa),intent(in)             :: equa     ! parametres orbitaux de l'orbite equatoriale
real(pm_reel), intent(in)                :: mu       ! constante de la gravitation universelle

real(pm_reel), dimension(3), intent(out) :: pos_car  ! vecteur position du satellite
real(pm_reel), dimension(3), intent(out) :: vit_car  ! vecteur vitesse du satellite
integer, intent(out)                     :: retour
real(pm_reel), dimension(6,6), intent(out), optional :: jacob ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel), dimension(3)   :: f ,g ,dfdx ,dfdy ,dgdx ,dgdy , rau, rp, drau

real(pm_reel)   :: a,anome,cof1,cof2,cose,coso
real(pm_reel)   :: danede,danedm,df1dan,df1de
real(pm_reel)   :: df1dom,df2dan,df2de,df2dom
real(pm_reel)   :: dg1dan,dg1de,dg1dom,dg2dan
real(pm_reel)   :: dg2de,dg2dom,dh1dan,dh1de
real(pm_reel)   :: dh2dan,dh2de,drace,drachx
real(pm_reel)   :: drachy,dudan,dude,e
real(pm_reel)   :: f1,f2,g1,g2,h1,h2,hx,hx2,hy
real(pm_reel)   :: hy2,ompom,raca,race,rach 
real(pm_reel)   :: sine,sino,unsec,xmoy

type(tm_code_retour)  :: code_retour

intrinsic cos, sin, sqrt, present

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB mvi_equa_car_ellip.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: mvi_equa_car_ellip.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
code_retour%valeur = pm_OK

! initialisations locales des parametres equatoriaux
! --------------------------------------------------

a=equa%a
e=equa%e
ompom=equa%pgom
hx=0.5_pm_reel*equa%ix
hy=0.5_pm_reel*equa%iy
xmoy=equa%M

coso=cos(ompom)
sino=sin(ompom)

call mv_kepler_std(xmoy, e, anome, code_retour)! anomalie excentrique ( resolution de l'equation de kepler )
retour = code_retour%valeur!     seuls code_retour%valeur = 0 ou pm_err_conv_kepler_ellip issu de mv_kepler_std possible ici
if (code_retour%valeur /= pm_OK) go to 6000

! calcul variables intermediaires
! -------------------------------

cose=cos(anome)
sine=sin(anome)

raca=sqrt(mu/a)
race=sqrt(1._pm_reel-e*e)
unsec=1._pm_reel/(1._pm_reel-e*cose)

h1=cose-e
h2=race*sine

hx2=hx**2
hy2=hy**2
rach=sqrt(1._pm_reel-hx2-hy2)

f(1)=1._pm_reel-2._pm_reel*hy2
f(2)=2._pm_reel*hx*hy
f(3)=-2._pm_reel*rach*hy

g(1)=f(2)
g(2)=1._pm_reel-2._pm_reel*hx2
g(3)=2._pm_reel*rach*hx

f1=h1*coso-h2*sino
f2=h1*sino+h2*coso

dh1dan=-sine
dh2dan=race*cose
df1dan=dh1dan*coso-dh2dan*sino
df2dan=dh1dan*sino+dh2dan*coso

g1=unsec*df1dan
g2=unsec*df2dan

! ********************
! calcul des positions
! ********************

rau(:)=f1*f(:)+f2*g(:)
pos_car(:)=a*rau(:)

! *******************
! calcul des vitesses
! *******************

rp(:)=raca*(g1*f(:)+g2*g(:))
vit_car(:)=rp(:)

if (present(jacob))  then

   !***************************************
   ! calcul du jacobien de la transformation
   !***************************************

   ! calcul derivees intermediaires
   ! ------------------------------

   cof1=-sine*unsec
   cof2=cose*unsec

   drace=-e/race

   dude=cof2*unsec
   dudan=e*cof1*unsec

   danede=-cof1
   danedm=unsec
   dh1de=-1._pm_reel
   dh2de=drace*sine
   drachx=-hx/rach
   drachy=-hy/rach

   dfdx(1)=0._pm_reel
   dfdy(1)=-4._pm_reel*hy
   dfdx(2)=2._pm_reel*hy
   dfdy(2)=2._pm_reel*hx
   dfdx(3)=-2._pm_reel*drachx*hy
   dfdy(3)=-2._pm_reel*(rach+drachy*hy)

   dgdx(1)=dfdx(2)
   dgdy(1)=dfdy(2)
   dgdx(2)=-4._pm_reel*hx
   dgdy(2)=0._pm_reel
   dgdx(3)=2._pm_reel*(drachx*hx+rach)
   dgdy(3)=2._pm_reel*drachy*hx

   df1de=dh1de*coso-dh2de*sino
   df1dom=-f2

   df2de=dh1de*sino+dh2de*coso
   df2dom=+f1

   ! derivees des positions
   ! ----------------------

   jacob(1:3,1)=rau(:)
   drau(:)= df1dan*f(:)+df2dan*g(:)
   jacob(1:3,2)=a*(df1de*f(:)+df2de*g(:)+drau(:)*danede)
   jacob(1:3,3)=a*(df1dom*f(:)+df2dom*g(:))
   jacob(1:3,4)=0.5_pm_reel*a*(f1*dfdx(:)+f2*dgdx(:))
   jacob(1:3,5)=0.5_pm_reel*a*(f1*dfdy(:)+f2*dgdy(:))
   jacob(1:3,6)=a*drau(:)*danedm

   dg1de=dude*df1dan+unsec*(-drace*cose*sino)
   dg2de=dude*df2dan+unsec*drace*cose*coso

   dg1dan=dudan*df1dan+unsec*(-cose*coso+race*sine*sino)
   dg2dan=dudan*df2dan+unsec*(-cose*sino-race*sine*coso)

   dg1dom=unsec*(sine*sino-race*cose*coso)
   dg2dom=unsec*(-sine*coso-race*cose*sino)

   ! derivees des vitesses
   ! ---------------------

   jacob(4:6,1)=-0.5_pm_reel*rp(:)/a
   drau(:)=dg1dan*f(:)+dg2dan*g(:)
   jacob(4:6,2)=raca*(dg1de*f(:)+dg2de*g(:)+drau(:)*danede)
   jacob(4:6,3)=raca*(dg1dom*f(:)+dg2dom*g(:))
   jacob(4:6,4)=0.5_pm_reel*raca*(g1*dfdx(:)+g2*dgdx(:))
   jacob(4:6,5)=0.5_pm_reel*raca*(g1*dfdy(:)+g2*dgdy(:))
   jacob(4:6,6)=raca*drau(:)*danedm

end if

6000  continue

end subroutine mvi_equa_car_ellip
