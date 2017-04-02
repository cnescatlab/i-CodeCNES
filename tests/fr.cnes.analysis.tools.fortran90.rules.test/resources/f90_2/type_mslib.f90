module type_mslib

! (C) Copyright CNES - MSLIB - 1998

!************************************************************************
!
! But:  Definition des types derives MSLIB. 
! ===
!
! Note d'utilisation: Ce module est accessible a l'utilisateur via le module mslib.
! ==================
!
!$Historique
! ==========
!   + Version 0.1 (SP 149 ed01 rev00): creation
!                         (Date: 01/1998 - Realisation: Guylaine Prat)
!   + Version 0.1.1 (DE globale 182 ed01 rev00): modification regle de marquage pour info_utilisateur
!                         et rajout de commentaires
!                         (Date: 02/1998 - Realisation: Guylaine Prat)
!   + Version 1.0 (DE 216 ed01 rev00) : ajout de nouveaux types 
!                         (Date: 07/1998 - Realisation: Veronique Lepine)
!   + Version 2.0 (DE 405 ed01 rev00) : ajout de commentaires sur le champ %a pour certains types
!                         et changement (expres) de l'ordre d'apparition des champs dans la structure
!                         (Date: 02/2000 - Realisation: Sylvain Vresk)
!   + Version 3.0 (DE 428 ed01 rev00) : ajout du type quaternion tm_quat 
!                         (Date: 09/2000 - Realisation: Veronique Lepine)
!   + Version 3.1 (DE 440 ed01 rev00) : ajout des champs biblio et message pour tm_code_retour
!                         et mise dans un ordre logique l'apparition des champs dans la structure (cf volume 3)
!                         (Date: 04/2001 - Realisation: Guylaine Prat)
!   + Version 3.2 (DE 449 ed01 rev00) : ajout de l'attribut sequence a tous les types derives
!                         (Date: 04/2002 - Realisation: Mickael Hazak)
!   + Version 6.3 : DM-ID 381 : Integrer des routines du theme trajectoires interplanetaires
!                   rajout du type tm_coniq
!                   (Date: 09/2005 - Realisation: Claire Fabre)
!   + Version 6.4 : DM-ID 426 : routine de calcul des angles de precession
!                 (Date: 04/2006 - Realisation: Claire Fabre - Atos Origin)
!   + Version 6.5 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!VERSION:V6.13:FA-ID:1410:30/09/2010:Ajout marqueur fin historique
!
!Revision 362 2013/03/15 bbjc
!DM-ID 1513: Suppression des warnings de compilation
!
!$FinHistorique
!
!************************************************************************

use precision_mslib           ! definition des precisions retenues
use longueur_chaine_mslib     ! definition des longueurs de chaines de caracteres

implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: type_mslib.f90 362 2013-02-15 18:01:28Z bbjc $'


type tm_code_retour     
   ! utile pour toutes les routines utilisateurs

   sequence
   integer                   :: valeur    ! valeur du code retour
   integer                   :: routine   ! numero de la routine
   integer                   :: biblio    ! bibliotheque
   character(len=pm_message) :: message   ! message

end type tm_code_retour

type tm_sgd             
   ! utile aussi bien pour des positions que pour des vitesses,
   ! que l'axe des X soit a l'Est ou au Nord.

   sequence
   real(pm_reel) :: s   ! site
   real(pm_reel) :: g   ! gisement
   real(pm_reel) :: d   ! distance

end type tm_sgd

type tm_jour_sec 
   ! utilise pour des dates juliennes ou des durees

   sequence  
   integer(pm_entier) :: jour      ! nombre de jours
   real(pm_reel)      :: sec       ! nombres de secondes dans le jour

end type tm_jour_sec

! Les types de structures suivants (tm_orb_...) sont utilises aussi bien pour les parametres osculateurs que pour les 
! parametres moyens, ou que pour les ecarts admissibles dans les routines d'extrapolation d'orbite.

type tm_orb_kep

   sequence
   real(pm_reel) :: a   ! demi-grand axe (ou parametre p pour la parabole)
   real(pm_reel) :: e   ! excentricite
   real(pm_reel) :: i   ! inclinaison
   real(pm_reel) :: pom ! argument du perigee
   real(pm_reel) :: gom ! longitude du noeud ascendant
   real(pm_reel) :: M   ! anomalie moyenne

end type tm_orb_kep

type tm_orb_cir

   sequence
   real(pm_reel) :: a     ! demi-grand axe
   !    vecteur excentricite
   real(pm_reel) :: ex    
   real(pm_reel) :: ey    
   real(pm_reel) :: i     ! inclinaison
   real(pm_reel) :: gom   ! longitude du noeud ascendant
   real(pm_reel) :: pso_M ! argument du perigee + anomalie moyenne

end type tm_orb_cir

type tm_orb_equa

   sequence
   real(pm_reel) :: a      ! demi-grand axe (ou parametre p pour la parabole)
   real(pm_reel) :: e      ! excentricite
   real(pm_reel) :: pgom   ! argument du perigee + longitude du noeud ascendant
   !    vecteur inclinaison
   real(pm_reel) :: ix     
   real(pm_reel) :: iy     
   real(pm_reel) :: M      ! anomalie moyenne

end type tm_orb_equa

type tm_orb_cir_equa

   sequence
   real(pm_reel) :: a      ! demi-grand axe
   !    vecteur excentricite
   real(pm_reel) :: ex   
   real(pm_reel) :: ey   
   !    vecteur inclinaison
   real(pm_reel) :: ix 
   real(pm_reel) :: iy 
   real(pm_reel) :: pso_M   ! argument du perigee + longitude du noeud ascendant + anomalie moyenne

end type tm_orb_cir_equa

type tm_geodesique

   sequence    
   real(pm_reel) :: lat     ! latitude geodesique
   real(pm_reel) :: long    ! longitude 
   real(pm_reel) :: haut    ! hauteur

end type tm_geodesique

type tm_geocentrique

   sequence
   real(pm_reel) :: lat     ! latitude geocentrique
   real(pm_reel) :: long    ! longitude 
   real(pm_reel) :: dist    ! distance centre Terre

end type tm_geocentrique

type tm_nuta
              ! Ce type de structure est utilise aussi bien pour la nutation, que pour la derivee premiere
              ! de la nutation, ou que la derivee seconde de la nutation

   sequence
   real(pm_reel) :: long    ! longitude
   real(pm_reel) :: obli    ! obliquite

end type tm_nuta

type tm_prec
              ! type pour la precession equatoriale
   sequence
   real(pm_reel) :: dzeta    ! angle dzeta 
   real(pm_reel) :: z        ! angle z
   real(pm_reel) :: theta    ! angle theta

end type tm_prec

type tm_quat                            
       ! type quaternion
  
   sequence
   real(pm_reel)                :: q0   ! partie reelle q0
   real(pm_reel),dimension(1:3) :: q123 ! partie imaginaire = vecteur (q1, q2, q3)

end type tm_quat

type tm_coniq

   sequence    
   real(pm_reel) :: p      ! parametre P de la conique
   real(pm_reel) :: e      ! excentricite de la conique 
   real(pm_reel) :: i      ! inclinaison du plan de l'orbite 
   real(pm_reel) :: pom    ! argument du perigee
   real(pm_reel) :: gom    ! longitude du noeud ascendant
   real(pm_reel) :: anom_v ! anomalie vraie du point de depart

end type tm_coniq

!................................................................................................................

character(len=pm_longueur_info_utilisateur),private, parameter :: info_utilisateur = &
     '@(#) Fichier MSLIB type_mslib.f90: derniere modification V6.13 >'

!................................................................................................................

character(len=pm_longueur_rcs_id), private, parameter :: rcs_id =' $Id: type_mslib.f90 362 2013-02-15 18:01:28Z bbjc $ '

end module type_mslib
