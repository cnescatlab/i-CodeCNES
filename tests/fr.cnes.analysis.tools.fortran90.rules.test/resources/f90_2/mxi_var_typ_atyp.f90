subroutine mxi_var_typ_atyp ( typ_var_in, typ_var_out, &
                              atypique, retour )

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  Determination du caractere typique ou atypique d'une 
! ===   transformation a partir des types de variable demandes
!       en entree et en sortie par l'utilisateur
!
! Note d'utilisation:
! ==================
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 3.0 : creation
!                   (Date: 11/2002 - Realisation: Michel Lagreca et Bruno Revelin)
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

use parametre_themeX_mspro
use parametre_interne_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)   :: typ_var_in   ! type de variable utilisateur initiale 
integer, intent(in)   :: typ_var_out  ! type de variable utilisateur finale    
logical, intent(out)  :: atypique     ! transformation typique ou atypique
integer, intent(out)  :: retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
     '@(#) Fichier MSPRO mxi_var_typ_atyp.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

! Controle de la presence de typ_var_in et typ_var_out dans le meme lot
! .....................................................................

if ((typ_var_in >= pm_mx_type_var_interv0) .AND. (typ_var_in < pm_mx_type_var_interv1) .AND. &
     (typ_var_out >= pm_mx_type_var_interv0) .AND. (typ_var_out < pm_mx_type_var_interv1)) then
   atypique = pm_i_non

else if ((typ_var_in >= pm_mx_type_var_interv1) .AND. (typ_var_in < pm_mx_type_var_interv2) .AND. &
     (typ_var_out >= pm_mx_type_var_interv1) .AND. (typ_var_out < pm_mx_type_var_interv2)) then
      atypique = pm_i_non

else if ((typ_var_in >= pm_mx_type_var_interv2) .AND. (typ_var_in < pm_mx_type_var_interv3) .AND. &
     (typ_var_out >= pm_mx_type_var_interv2) .AND. (typ_var_out < pm_mx_type_var_interv3)) then
      atypique = pm_i_non

else if ((typ_var_in >= pm_mx_type_var_interv3) .AND. (typ_var_in < pm_mx_type_var_interv4) .AND. &
     (typ_var_out >= pm_mx_type_var_interv3) .AND. (typ_var_out < pm_mx_type_var_interv4)) then
      atypique = pm_i_non

else if (typ_var_in == pm_car) then
   atypique = pm_i_non
else if (typ_var_out == pm_car) then
   atypique = pm_i_non

else ! cas atypique
   atypique = pm_i_oui
end if 

end subroutine mxi_var_typ_atyp
