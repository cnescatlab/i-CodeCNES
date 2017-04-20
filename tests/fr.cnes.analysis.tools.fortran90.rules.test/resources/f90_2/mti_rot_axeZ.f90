subroutine mti_rot_axeZ ( angle, pos_ref ,pos_tourn, retour, vit_ref, vit_tourn, jacob )

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:   Passage d'un repere de reference a un 2e repere issu du 1er par rotation d'axe Z
! ===
!
! Note d'utilisation:  Attention: on ne calcule pas une rotation, mais un changement de repere !
! ==================
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 3.0 : creation a partir de rien
!                         (Date: 11/2002 - Realisation: Bruno Revelin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use valeur_code_retour_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel),              intent(in)            ::  angle      ! angle du changement de repere
real(pm_reel), dimension(3),intent(in)            ::  pos_ref    ! position en entree
real(pm_reel), dimension(3),intent(out)           ::  pos_tourn  ! position en sortie
integer,                    intent(out)           ::  retour     
real(pm_reel), dimension(3),intent(in),optional   ::  vit_ref    ! vitesse en entree    
real(pm_reel), dimension(3),intent(out),optional  ::  vit_tourn  ! vitesse en sortie
real(pm_reel),dimension(6,6),intent(out),optional ::  jacob      ! jacobienne de la transformation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

real(pm_reel) :: rcos   ! cosinus de l'angle de rotation
real(pm_reel) :: rsin   ! inus de l'angle de rotation

intrinsic cos, sin

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mti_rot_axeZ.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! -------------------
! Calcul des positions
! -------------------

rcos = cos(angle)
rsin = sin(angle)

pos_tourn(1) = rcos*pos_ref(1) + rsin*pos_ref(2)
pos_tourn(2) = -rsin*pos_ref(1) + rcos*pos_ref(2)
pos_tourn(3) = pos_ref(3)

! -------------------
! calcul optionnels
! -------------------

if ((present(vit_ref).and..not.present(vit_tourn)).or. &
    (present(vit_tourn).and..not.present(vit_ref)))    then  ! erreur de parametrage

   retour =pm_err_para_option
   go to 6000

else 
   if  (present(vit_ref).and.present(vit_tourn)) then      ! calcul des composantes vitesses

      vit_tourn(1) = rcos*vit_ref(1) + rsin*vit_ref(2)
      vit_tourn(2) = -rsin*vit_ref(1) + rcos*vit_ref(2)
      vit_tourn(3) = vit_ref(3)
   end if
end if

if (present(jacob)) then  ! calcul du jacobien : calcul des derivees partielles des vecteurs position et vitesse 
   jacob(:,:) = 0._pm_reel

   !     derivee partielle de x :
   !     ------------------------
   jacob(1,1) = rcos
   jacob(1,2) = rsin

   !     derivee partielle de y :
   !     ------------------------
   jacob(2,1) = -1._pm_reel*rsin
   jacob(2,2) = rcos

   !     derivee partielle de z :
   !     ------------------------
   jacob(3,3) = 1._pm_reel

   !     derivee partielle de vx :
   !     -------------------------
   jacob(4,4) = rcos
   jacob(4,5) = rsin

   !     derivee partielle de vy :
   !     -------------------------
   jacob(5,4) = -1._pm_reel*rsin
   jacob(5,5) = rcos

   !     derivee partielle de vz :
   !     -------------------------
   jacob(6,6) = 1._pm_reel

end if

6000 continue

end subroutine mti_rot_axeZ
