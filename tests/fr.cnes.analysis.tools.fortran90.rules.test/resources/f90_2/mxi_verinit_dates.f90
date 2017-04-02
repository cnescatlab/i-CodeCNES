subroutine mxi_verinit_dates ( indic_date, complement_message, para, retour, message )

! (C) Copyright CNES - MSPRO - 2002

!************************************************************************
!
! But:  suivant l'indicateur de dates, initialiser les donnees predefinies ou effectuer une verification 
! ===   essentielle pour le cas pm_autre_date
!
! Note d'utilisation:  Le parametre "para" est en inout
! ==================
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 3.0 : creation
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

use type_themeX_interne_mspro    ! inclus le use  type_mspro
use parametre_interne_mspro 
use parametre_themeX_interne_mspro 
use parametre_themeX_mspro 
use valeur_code_retour_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer,                   intent(in)                       :: indic_date              ! indic_date
character(len=3),          intent(in)                       :: complement_message      ! complement_message
type(tm_i_rep_para_opt),       intent(inout)                    :: para                    ! para
integer,                   intent(out)                      :: retour                  ! retour
character(len=pm_message), intent(out)                      :: message                 ! message

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

intrinsic trim

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
                     '@(#) Fichier MSPRO mxi_verinit_dates.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation de la valeur du code retour
! ..........................................
retour = pm_OK

if (indic_date == pm_1janvier1950_00h00) then
! ecrasement des valeurs utilisateurs
   para%val_date%valeur%jour = pm_i_jour_1janvier1950_00h00
   para%val_date%valeur%sec = pm_i_sec_1janvier1950_00h00
   para%delta_tu1%valeur =  pm_i_dtu1_1janvier1950_00h00
   para%delta_tai%valeur =  pm_i_dtai_1janvier1950_00h00

else if (indic_date == pm_1janvier2000_12h00) then
! ecrasement des valeurs utilisateurs
   para%val_date%valeur%jour = pm_i_jour_1janvier2000_12h00
   para%val_date%valeur%sec = pm_i_sec_1janvier2000_12h00
   para%delta_tu1%valeur =  pm_i_dtu1_1janvier2000_12h00
   para%delta_tai%valeur =  pm_i_dtai_1janvier2000_12h00

else     ! indic_date = pm_autre_date   
! presence necessaire
   if (.NOT.para%val_date%presence) then
      retour = pm_err_para_opt_abs  
      message = 'val_date_'//trim(complement_message)
      go to 6000
   else
       para%val_date%superflu = pm_i_non
   end if
   if (.NOT.para%delta_tai%presence) then
      retour = pm_err_para_opt_abs  
      message = 'delta_tai_'//trim(complement_message)
      go to 6000
   else
       para%delta_tai%superflu = pm_i_non
   end if

end if
      

6000 continue

end subroutine mxi_verinit_dates
