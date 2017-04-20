subroutine ms_pos_soleil_lune (date, dir_sol, dist_sol, dir_lune, dist_lune, code_retour)

! (C) Copyright CNES - MSLIB - 2004

!************************************************************************
!
! But:  Calcul des positions du Soleil et de la Lune dans le repere de Veis
! ===   a une date donnee.
!
! Note d'utilisation:  * l'unite de distance en sortie est le km
! ==================   * les calculs sont effectues dans le repere Gamma 50
!                        a partir d'une version simplifiee des theories de
!                        BROWN pour la Lune et NEWCOMB pour le Soleil
!
!$Historique
! ==========
!   + Version 6.1 : creation
!                         (Date: 09/2004 - Realisation: Bruno Revelin)
!   + Version 6.3 (DM-ID 394) : Qualite : augmentation du taux de commentaires
!                 (Date: 11/2005 - Realisation: Atos Origin)   
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 6.6 : DM-ID 616 remplacement du module math_mslib
!     par une sélection de int_constantes
!                   (Date: 05/2007 - Realisation: Atos origin)
!   + Version 6.9 : DM-ID 1058 : Suppression des warnings G95
!                   (Date: 09/2008 - Realisation: Atos origin)
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

type(tm_jour_sec), intent(in)                        ::  date      ! date (JJCNES)
real(pm_reel), dimension(3), intent(out)             ::  dir_sol   ! cosinus directeur du Soleil dans Veis
real(pm_reel), intent(out)                           ::  dist_sol  ! distance Terre-Soleil
real(pm_reel), dimension(3), intent(out)             ::  dir_lune  ! cosinus directeur de la Lune dans Veis
real(pm_reel), intent(out)                           ::  dist_lune ! distance Terre-Lune 
type(tm_code_retour), intent(out)                    ::  code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

real(pm_reel)               :: t,f,xl,d,xlp,g,e,ce,se,rot,cr,sr
real(pm_reel)               :: dl,b,u,cu,su,cb,sb,asrl,q,dasr
real(pm_reel)               :: cl,sl,asrs, rx, ry

intrinsic cos,sin,sqrt

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSLIB ms_pos_soleil_lune.f90: derniere modification V6.13 >'

! Ne pas toucher a la ligne suivante
character(len=pm_longueur_rcs_id), parameter :: rcs_id =' $Id: ms_pos_soleil_lune.f90 362 2013-02-15 18:01:28Z bbjc $ '

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK

! Corps du programme
! ==================

t    = real(date%jour,kind=pm_reel) + date%sec/86400._pm_reel - 10000._pm_reel
f    = 225.768_pm_reel + 13.2293505_pm_reel*t
f    = modulo(f*pm_deg_rad,pm_deux_pi)
xl   = 185.454_pm_reel + 13.064992_pm_reel*t
xl   = modulo(xl*pm_deg_rad,pm_deux_pi)
d    = 11.786_pm_reel + 12.190749_pm_reel*t
d    = modulo(d*pm_deg_rad,pm_deux_pi)
xlp  = 134.003_pm_reel + 0.9856_pm_reel*t
xlp  = modulo(xlp*pm_deg_rad,pm_deux_pi)
g    = 282.551_pm_reel + 0.000047_pm_reel*t
g    = modulo(g*pm_deg_rad,pm_deux_pi)
e    = 23.44223_pm_reel - 0.00000035626_pm_reel*t
e    = modulo(e*pm_deg_rad,pm_deux_pi)
ce   = COS(e)
se   = SIN(e)
rot  = 0.0000006119022_pm_reel*(real(date%jour,kind=pm_reel) + date%sec/86400._pm_reel)
cr   = COS(rot)
sr   = SIN(rot)

! position de la Lune

dl   = 10976._pm_reel*SIN(xl) - 2224._pm_reel*SIN(xl-2._pm_reel*d) + 1149._pm_reel*SIN(2._pm_reel*d)   &
     + 373._pm_reel*SIN(2._pm_reel*xl) - 324._pm_reel*SIN(xlp) - 200._pm_reel*SIN(2._pm_reel*f)   &
     - 103._pm_reel*SIN(2._pm_reel*xl-2._pm_reel*d) - 100._pm_reel*SIN(xl+xlp-2._pm_reel*d)   &
     + 93._pm_reel*SIN(xl+2._pm_reel*d)   &
     - 80._pm_reel*SIN(xlp-2._pm_reel*d) + 72._pm_reel*SIN(xl-xlp) - 61._pm_reel*SIN(d)   &
     - 53._pm_reel*SIN(xl+xlp)   &
     + 14._pm_reel*SIN(xl-xlp-2._pm_reel*d) + 19._pm_reel*SIN(xl-2._pm_reel*f)   &
     - 19._pm_reel*SIN(xl-4._pm_reel*d)   &
     + 17._pm_reel*SIN(3._pm_reel*xl) - 27._pm_reel*SIN(f+f-2._pm_reel*d)   &
     - 12._pm_reel*SIN(xlp+2._pm_reel*d)   &
     - 22._pm_reel*SIN(xl+2._pm_reel*f) - 15._pm_reel*SIN(2._pm_reel*xl-4._pm_reel*d)   &
     + 7._pm_reel*SIN(2._pm_reel*xl+2._pm_reel*d) + 9._pm_reel*SIN(xl-d)   &
     - 6._pm_reel*SIN(3._pm_reel*xl-2._pm_reel*d)   &
     + 7._pm_reel*SIN(4._pm_reel*d) + 9._pm_reel*SIN(xlp+d) + 7._pm_reel*SIN(xl-xlp+2._pm_reel*d)   &
     + 5._pm_reel*SIN(2._pm_reel*xl-xlp)
dl   = dl*0.00001_pm_reel

b    = 8950._pm_reel*SIN(f) + 490._pm_reel*SIN(xl+f) + 485._pm_reel*SIN(xl-f)   &
     - 302._pm_reel*SIN(f-2._pm_reel*d)   &
     - 97._pm_reel*SIN(xl-f-2._pm_reel*d) - 81._pm_reel*SIN(xl+f-2._pm_reel*d)   &
     + 57._pm_reel*SIN(f+2._pm_reel*d)   &
     - 14._pm_reel*SIN(xlp+f-2._pm_reel*d) + 16._pm_reel*SIN(xl-f+2._pm_reel*d)   &
     + 15._pm_reel*SIN(2._pm_reel*xl-f) + 30._pm_reel*SIN(2._pm_reel*xl+f)   &
     - 6._pm_reel*SIN(xlp-f+2._pm_reel*d) - 7._pm_reel*SIN(2._pm_reel*xl+f-2._pm_reel*d)   &
     + 7._pm_reel*SIN(xl+f+2._pm_reel*d)
b    = b*0.00001_pm_reel

u    = 68.341_pm_reel + 13.176397_pm_reel*t
u    = modulo(u*pm_deg_rad,pm_deux_pi) + dl
cu   = COS(u)
su   = SIN(u)
cb   = COS(b)
sb   = SIN(b)
rx   = cu*cb
ry   = su*cb*ce-sb*se

! cosinus directeur de la Lune dans Veis
dir_lune(3)   = sb*ce + su*cb*se
dir_lune(2)   = ry*cr - rx*sr
dir_lune(1)   = rx*cr + ry*sr

dasr = 5450._pm_reel*COS(xl) + 1002._pm_reel*COS(xl-2._pm_reel*d) + 825._pm_reel*COS(2._pm_reel*d)    &
     + 297._pm_reel*COS(2._pm_reel*xl) + 90._pm_reel*COS(xl+2._pm_reel*d)    &
     + 56._pm_reel*COS(xlp-2._pm_reel*d)    &
     + 42._pm_reel*COS(xl+xlp-2._pm_reel*d) + 34._pm_reel*COS(xl-xlp)    &
     - 12._pm_reel*COS(xlp) - 29._pm_reel*COS(d) - 21._pm_reel*COS(xl-2._pm_reel*f)    &
     + 18._pm_reel*COS(xl-4._pm_reel*d) - 28._pm_reel*COS(xl+xlp)    &
     + 11._pm_reel*COS(2._pm_reel*xl-4._pm_reel*d) + 18._pm_reel*COS(3._pm_reel*xl)    &
     - 9._pm_reel*COS(xlp+2._pm_reel*d) - 7._pm_reel*COS(xl-xlp-2._pm_reel*d)    &
     + 7._pm_reel*COS(xl-xlp+2._pm_reel*d)    &
     - 9._pm_reel*COS(2._pm_reel*xl-2._pm_reel*d) + 8._pm_reel*COS(2._pm_reel*xl+2._pm_reel*d)    &
     + 8._pm_reel*COS(4._pm_reel*d)
asrl = 1._pm_reel + 0.00001_pm_reel*dasr

! distance Terre-Lune
dist_lune   = 384389.3_pm_reel/asrl

! position du Soleil

cl   = 99972._pm_reel*COS(xlp+g) + 1671._pm_reel*COS(2._pm_reel*xlp+g) - 1678._pm_reel*COS(g)    &
     + 32._pm_reel*COS(3._pm_reel*xlp+g) + COS(4._pm_reel*xlp+g)    &
     - 4._pm_reel*COS(g-xlp) - 2._pm_reel*COS(xlp-d+g) + 4._pm_reel*COS(f-d)    &
     - 4._pm_reel*COS(2._pm_reel*xlp-f+d+2._pm_reel*g) + 2._pm_reel*COS(xlp+d+g)
cl   = cl*0.00001_pm_reel

sl   = 99972._pm_reel*SIN(xlp+g) + 1671._pm_reel*SIN(2._pm_reel*xlp+g) - 1678._pm_reel*SIN(g)    &
     + 32._pm_reel*SIN(3._pm_reel*xlp+g) + SIN(4._pm_reel*xlp+g)    &
     - 4._pm_reel*SIN(g-xlp) - 2._pm_reel*SIN(xlp-d+g) + 4._pm_reel*SIN(f-d)    &
     - 4._pm_reel*SIN(2._pm_reel*xlp-f+d+2._pm_reel*g) + 2._pm_reel*SIN(xlp+d+g)
sl   = sl*0.00001_pm_reel

q    = SQRT(cl*cl+sl*sl)

! cosinus directeur du soleil dans Veis
dir_sol(3)   = (sl*se)/q
dir_sol(2)   = (sl*ce*cr - cl*sr)/q
dir_sol(1)   = (cl*cr + sl*ce*sr)/q

dasr = 1672.2_pm_reel*COS(xlp)+28._pm_reel*COS(2._pm_reel*xlp)-0.35_pm_reel*COS(d)
asrs = 1._pm_reel+ 0.00001_pm_reel*dasr

! distance Terre-Soleil
dist_sol   = 149597870._pm_reel/asrs

code_retour%routine = pm_num_ms_pos_soleil_lune
code_retour%biblio = pm_mslib90
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine ms_pos_soleil_lune
