subroutine mu_integrer_jjfrac (fsub, integrateur, t0, y0, t, y, t_fin, retour_fsub, pb_fsub, code_retour, &
     num_commut, nb_commut, evt_commut)

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  Integrer l'equation differentielle y'=f(t,y) au point (t,y(t))
! ===
!
! Note d'utilisation:  
! ==================
!
! * L'architecture et le code ont ete recopies directement depuis l'architecture et le code
! de la bibliotheque Mantissa (http://www.spaceroots.org/software/mantissa/index.html), apres
! transcription du code Java en Fortran 90
! Mantissa est un produit libre developpe par Luc Maisonobe et diffuse
! sous une licence BSD modifiee autorisant cet emprunt et ces modifications.
!
!$Historique
! ==========
!  Revision 366  2013/02/18 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 5.2 : creation
!                         (Date: 01/2005 - Realisation: Bruno Revelin)
!   + Version 5.3 :  DM-ID 408 : mettre un Cowell dans les integrateurs MSPRO
!                         (Date: 10/2005 - Realisation: Equipe Patrimoine Mecanique Spatiale - Atos Origin)
!   + Version 5.4 : DM-ID 472 : Amelioration de la detection d evenement
!                         (Date: 02/2006 - Realisation: Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                         (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.6 : DM-ID 516 : détection d'évènements simultanés par les intégrateurs MSPRO
!                         (Date: 09/2006 - Réalisation : Atos Origin) 
!   + Version 5.8 : FA-ID 829 : Supression des variables inutilisées
!                         (Date: 10/2007 - réalisation : Atos Origin)
!   + Version 5.10: DM-ID 1058 : Correction des warnings levés par g95
!                   (Date: 8/2008 - Realisation: Atos origin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use int_util_internes_mspro, only : mui_integ_RK
use int_util_internes_mspro, only : mui_integ_RKF

use type_mspro
use parametre_mspro
use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

interface
   subroutine fsub(t,y,ydot,retour)     ! equation differentielle

   use mslib

   real(pm_reel),intent(in)                      ::  t     ! abscisse
   real(pm_reel),dimension(:),intent(in)         ::  y     ! vecteur d'etat
   real(pm_reel),dimension(:),intent(out)        ::  ydot  ! derivee en t
   integer,      intent(out)                     ::  retour
   
   end subroutine fsub
end interface

type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
real(pm_reel),                         intent(in)                     ::  t0           ! valeur initiale de t
real(pm_reel),dimension(integrateur%n),intent(in)                     ::  y0           ! valeur de la fonction a t0
real(pm_reel),                         intent(in)                     ::  t            ! abscisse du point demande
real(pm_reel),dimension(integrateur%n),intent(out)                    ::  y            ! valeur de la fonction en t
real(pm_reel),                         intent(out)                    ::  t_fin        ! abscisse effective du y calcule
integer,                               intent(out)                    ::  retour_fsub  ! code retour de fsub
real(pm_reel),                         intent(out)                    ::  pb_fsub      ! abscisse posant pb a fsub
type(tm_code_retour),                  intent(out)                    ::  code_retour
integer,                               intent(out), optional          ::  num_commut   ! identificateur de la derniere routine ayant commute
integer,                               intent(out), optional          ::  nb_commut    ! nombre total de commutations
type(tm_evt_commut),dimension(:),      pointer,     optional         ::  evt_commut    ! pointeurs sur les évènements de commutation 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

integer :: retour_local
integer :: num_com  ! numero de la derniere commutation (0 si aucune)
integer :: nb_com   ! nombre total de commutations

intrinsic abs,epsilon

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mu_integrer.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK
retour_local = 0
retour_fsub = 0
pb_fsub = 0.0_pm_reel

! autres initialisations
num_com = 0
nb_com = 0
integrateur%t_deb = t0
integrateur%t_fin = t0
t_fin = 0._pm_reel

! Verifications

if ((integrateur%n /= size(y0)).or. (integrateur%n /= size(y))) then
   code_retour%valeur = pm_err_dimension
   go to 6000
end if
if (abs(t0-t) < epsilon(1._pm_reel)) then
   code_retour%valeur = pm_err_val_para
   go to 6000
end if

! Orientation selon l'integrateur choisi

select case (integrateur%type)

case (pm_Gill)
   if (present(evt_commut)) then
      call mui_integ_RK (fsub, integrateur, t0, y0, t, y, num_com, nb_com,&
           retour_fsub, pb_fsub, retour_local, evt_commut)
   else
      call mui_integ_RK (fsub, integrateur, t0, y0, t, y, num_com, nb_com,&
           retour_fsub, pb_fsub, retour_local)
   endif
   t_fin = integrateur%t_fin

case (pm_DOP853)
   if (present(evt_commut)) then
      call mui_integ_RKF (fsub, integrateur, t0, y0, t, y, num_com, nb_com,&
           retour_fsub, pb_fsub, retour_local, evt_commut)
   else
      call mui_integ_RKF (fsub, integrateur, t0, y0, t, y, num_com, nb_com,&
           retour_fsub, pb_fsub, retour_local)
   endif
   t_fin = integrateur%t_fin

case (pm_Cowell)
! sortie en erreur
   retour_local = pm_err_val_para

case default
   
   retour_local = pm_err_val_para

end select

if (present(num_commut)) then
   num_commut = num_com
endif

if (present(nb_commut)) then
   nb_commut = nb_com
endif

if (retour_local /= pm_OK) then
   code_retour%valeur = retour_local
   if (retour_local < pm_OK) go to 6000
end if

   
6000 continue

code_retour%routine = pm_num_mu_integrer
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_integrer_jjfrac


subroutine mu_integrer_jjsec (fsub_cowell, integrateur, t0, y0, t, y, t_fin, retour_fsub, pb_fsub, code_retour, &
     num_commut, nb_commut)

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  Integrer l'equation differentielle y'=f(t,y) au point (t,y(t))
! ===
!
! Note d'utilisation:  
! ==================
!
! * L'architecture et le code ont ete recopies directement depuis l'architecture et le code
! de la bibliotheque Mantissa (http://www.spaceroots.org/software/mantissa/index.html), apres
! transcription du code Java en Fortran 90
! Mantissa est un produit libre developpe par Luc Maisonobe et diffuse
! sous une licence BSD modifiee autorisant cet emprunt et ces modifications.
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 5.2 : creation
!                         (Date: 01/2005 - Realisation: Bruno Revelin)
!   + Version 5.3 :  DM-ID 408 : mettre un Cowell dans les integrateurs MSPRO
!                     (Date: 10/2005 - Realisation: Equipe Patrimoine Mecanique Spatiale - Atos Origin)
!   + Version 5.4 : DM-ID 472 : Amelioration de la detection d evenement
!                         (Date: 02/2006 - Realisation: Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.9 : DM-ID 964 : Evolution du Cowell pour prendre en compte 
!                               le changement de sens d'intégration
!                         (Date: 03/2008 - Réalisation Y.TANGUY Atos Origin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use int_util_internes_mspro, only : mui_integ_RK
use int_util_internes_mspro, only : mui_integ_RKF

use type_mspro
use parametre_mspro
use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

interface
   subroutine fsub_cowell(t,y,ydot,retour)     ! equation differentielle

   use mslib

   type(tm_jour_sec),intent(in)                  ::  t     ! abscisse
   real(pm_reel),dimension(:),intent(in)         ::  y     ! vecteur d'etat
   real(pm_reel),dimension(:),intent(out)        ::  ydot  ! derivee en t
   integer,      intent(out)                     ::  retour
   
   end subroutine fsub_cowell
end interface

type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
type(tm_jour_sec),                     intent(in)                     ::  t0           ! valeur initiale de t
real(pm_reel),dimension(integrateur%n),intent(in)                     ::  y0           ! valeur de la fonction a t0
type(tm_jour_sec),                     intent(in)                     ::  t            ! abscisse du point demande
real(pm_reel),dimension(integrateur%n),intent(out)                    ::  y            ! valeur de la fonction en t
type(tm_jour_sec),                     intent(out)                    ::  t_fin        ! abscisse effective du y calcule
integer,                               intent(out)                    ::  retour_fsub  ! code retour de fsub
type(tm_jour_sec),                     intent(out)                    ::  pb_fsub      ! abscisse posant pb a fsub
type(tm_code_retour),                  intent(out)                    ::  code_retour
integer,                               intent(out), optional          ::  num_commut   ! identificateur de la derniere routine ayant commute
integer,                               intent(out), optional          ::  nb_commut    ! nombre total de commutations

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================

integer :: retour_local
integer :: num_com  ! numero de la derniere commutation (0 si aucune)
integer :: nb_com   ! nombre total de commutations
real(pm_reel) :: jourfrac0, jourfrac 
type(tm_code_retour) :: code_retour_local     ! code retour local

intrinsic abs,epsilon

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mu_integrer.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK
retour_local = 0
retour_fsub = 0
pb_fsub%jour = 0
pb_fsub%sec = 0.0_pm_reel

! autres initialisations
num_com = 0
nb_com = 0

call md_joursec_jourfrac (t0,jourfrac0,code_retour_local)  ! pas d'erreur possible
call md_joursec_jourfrac (t,jourfrac,code_retour_local)  ! pas d'erreur possible

integrateur%t_deb = jourfrac0
integrateur%t_fin = jourfrac0

! Verifications

if ((integrateur%n /= size(y0)).or. (integrateur%n /= size(y))) then
   code_retour%valeur = pm_err_dimension
   go to 6000
end if

! Orientation selon l'integrateur choisi

select case (integrateur%type)

case (pm_Gill)
! sortie en erreur
   retour_local = pm_err_val_para

case (pm_DOP853)
! sortie en erreur
   retour_local = pm_err_val_para

case (pm_Cowell)
   call mui_integ_cowell (fsub_cowell, integrateur, t0, y0, t, y, retour_fsub, pb_fsub, retour_local)
   t_fin = integrateur%joursec_fin

case default
   
   retour_local = pm_err_val_para

end select

if (present(num_commut)) then
   num_commut = num_com
endif

if (present(nb_commut)) then
   nb_commut = nb_com
endif

if (retour_local /= pm_OK) then
   code_retour%valeur = retour_local
   if (retour_local < pm_OK) go to 6000
end if

   
6000 continue

code_retour%routine = pm_num_mu_integrer
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_integrer_jjsec

subroutine mu_integrer_jjsec_simp (fsub_cowell, integrateur, t0, y0, t, y, t_fin, retour_fsub, pb_fsub, code_retour, &
     num_commut, nb_commut, fcowell_simp)

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  Integrer l'equation differentielle y'=f(t,y) au point (t,y(t))
! ===
!
! Note d'utilisation:  
! ==================
!
! * L'architecture et le code ont ete recopies directement depuis l'architecture et le code
! de la bibliotheque Mantissa (http://www.spaceroots.org/software/mantissa/index.html), apres
! transcription du code Java en Fortran 90
! Mantissa est un produit libre developpe par Luc Maisonobe et diffuse
! sous une licence BSD modifiee autorisant cet emprunt et ces modifications.
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 5.7 : creation
!                   DM-ID 738 : Evolution du Cowell
!                   (Date: 06/2007 - Realisation: Sandrine Avril - Atos origin)
!   + Version 5.9 : DM-ID 964 : Evolution du Cowell pour prendre en compte 
!                               le changement de sens d'intégration
!                         (Date: 03/2008 - Réalisation Y.TANGUY Atos Origin)
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use int_util_internes_mspro, only : mui_integ_RK
use int_util_internes_mspro, only : mui_integ_RKF

use type_mspro
use parametre_mspro
use valeur_code_retour_mspro
use numero_routine_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

interface
   subroutine fsub_cowell(t,y,ydot,retour)     ! equation differentielle

   use mslib

   type(tm_jour_sec),intent(in)                  ::  t     ! abscisse
   real(pm_reel),dimension(:),intent(in)         ::  y     ! vecteur d'etat
   real(pm_reel),dimension(:),intent(out)        ::  ydot  ! derivee en t
   integer,      intent(out)                     ::  retour
   
   end subroutine fsub_cowell
end interface


type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
type(tm_jour_sec),                     intent(in)                     ::  t0           ! valeur initiale de t
real(pm_reel),dimension(integrateur%n),intent(in)                     ::  y0           ! valeur de la fonction a t0
type(tm_jour_sec),                     intent(in)                     ::  t            ! abscisse du point demande
real(pm_reel),dimension(integrateur%n),intent(out)                    ::  y            ! valeur de la fonction en t
type(tm_jour_sec),                     intent(out)                    ::  t_fin        ! abscisse effective du y calcule
integer,                               intent(out)                    ::  retour_fsub  ! code retour de fsub
type(tm_jour_sec),                     intent(out)                    ::  pb_fsub      ! abscisse posant pb a fsub
type(tm_code_retour),                  intent(out)                    ::  code_retour
integer,                               intent(out), optional          ::  num_commut   ! identificateur de la derniere routine ayant commute
integer,                               intent(out), optional          ::  nb_commut    ! nombre total de commutations

interface
   subroutine fcowell_simp(t,y,ydot,retour)     ! equation differentielle
   use mslib

   type(tm_jour_sec),intent(in)                  ::  t     ! abscisse
   real(pm_reel),dimension(*),intent(in)         ::  y     ! vecteur d'etat
   real(pm_reel),dimension(*),intent(out)        ::  ydot  ! derivee en t
   integer,      intent(out)                     ::  retour
   
   end subroutine fcowell_simp

end interface

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! ===================
integer :: MsproConvertitPointeur
external MsproConvertitPointeur

integer :: retour_local
integer :: num_com  ! numero de la derniere commutation (0 si aucune)
integer :: nb_com   ! nombre total de commutations
real(pm_reel) :: jourfrac0, jourfrac 
type(tm_code_retour) :: code_retour_local     ! code retour local

intrinsic abs,epsilon

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mu_integrer.f90: derniere modification V5.15 >'

!************************************************************************

! initialisations
! ===============

! initialisation de la valeur du code retour

code_retour%valeur = pm_OK
retour_local = 0
retour_fsub = 0
pb_fsub%jour = 0
pb_fsub%sec = 0.0_pm_reel

! autres initialisations
num_com = 0
nb_com = 0

call md_joursec_jourfrac (t0,jourfrac0,code_retour_local)  ! pas d'erreur possible
call md_joursec_jourfrac (t,jourfrac,code_retour_local)  ! pas d'erreur possible

integrateur%t_deb = jourfrac0
integrateur%t_fin = jourfrac0

! Verifications

if ((integrateur%n /= size(y0)).or. (integrateur%n /= size(y))) then
   code_retour%valeur = pm_err_dimension
   go to 6000
end if

! Orientation selon l'integrateur choisi

select case (integrateur%type)

case (pm_Gill)
! sortie en erreur
   retour_local = pm_err_val_para

case (pm_DOP853)
! sortie en erreur
   retour_local = pm_err_val_para

case (pm_Cowell)
   !! récupération de l'adresse de la fonction simplifiée
   integrateur%adr_fcowell_simp=MsproConvertitPointeur(fcowell_simp)
   call mui_integ_cowell (fsub_cowell, integrateur, t0, y0, t, y, retour_fsub, pb_fsub, retour_local)
   t_fin = integrateur%joursec_fin

case default
   
   retour_local = pm_err_val_para

end select

if (present(num_commut)) then
   num_commut = num_com
endif

if (present(nb_commut)) then
   nb_commut = nb_com
endif

if (retour_local /= pm_OK) then
   code_retour%valeur = retour_local
   if (retour_local < pm_OK) go to 6000
end if

   
6000 continue

code_retour%routine = pm_num_mu_integrer
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '

end subroutine mu_integrer_jjsec_simp
