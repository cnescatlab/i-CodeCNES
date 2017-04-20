program maj_bullA

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  maj_bullA
!
!$Resume
!  Enregistrement des données issues du fichier brut du bulletin A
!  dans la base COMPAS
!
!$Description
!  Enregistrement des données issues du fichier brut du bulletin A
!  dans la base COMPAS par l'appel à la fonction cps_majBullA
!
! Usage
!
!  maj_bullA date fichier fichier_2000
!
!>E date <jjmmaa> : date sur 6 digits 
!>E fichier : fichier bulletin A
!>E fichier_2000 : fichier bulletin A au format 2000
!
!!$Auteur
!  G. MERCADIER (ATOS ORIGIN), 20/02/2006
!
!$Version
!  $Id: maj_bullA.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: maj_bullA.F90,v $
!  Revision 1.4  2010/10/25 08:35:49  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.3  2008/05/06 08:14:02  vivaresf
!  FA-ID 889 : validation et test
!
!  Revision 1.2  2008/04/11 12:59:46  vivaresf
!  Version 2.4 AQ : mise à jour des cartouches
!
!  Revision 1.1  2008/02/08 17:50:49  vivaresf
!  FA-ID 889, DM-ID 820 :
!  - réorganisation du code pour que tout soit dans le répertoire src
!  Revision 1.2  2006/03/03 16:38:22  vpg
!  Suppression de use MSLIB
!  Revision 1.1  2006/02/22 09:57:45  mercadig
!  DM-ID 387 Programme d enregistrement des donnees du bulletin A dans COMPAS
!
!$FinHistorique
!
!$Structure
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

use ui_bullA

implicit none

! SVN Source File Id
  character(len=256) :: SVN_VER =  '$Id: maj_bullA.F90 69 2012-09-11 08:33:34Z ffsm $'


type (tm_jour_sec) :: jour_sec
type (tm_code_retour) :: code_retour

integer :: nbarg, iargc, lnblnk
character (LEN=6) :: ch_date
character (LEN=100) :: fichier_brut, fichier_brut_2000
integer :: jj, mm, aa, h, min
real(KIND=PM_REEL) :: date, sec
integer :: ierr

! Lecture des arguments
nbarg = iargc ()
if (nbarg == 3) then
   call GETARG (1,ch_date)
   call GETARG (2,fichier_brut)
   fichier_brut = fichier_brut(1:lnblnk(fichier_brut))
   call GETARG (3,fichier_brut_2000)
   fichier_brut_2000 = fichier_brut_2000(1:lnblnk(fichier_brut_2000))
endif

read(ch_date(1:2),'(i2)') jj
read(ch_date(3:4),'(i2)') mm
read(ch_date(5:6),'(i2)') aa

aa = aa + 2000
h = 0
min = 0
sec = 0._pm_reel

! Conversion en jours juliens
call md_calend_julien(aa, mm, jj, h, min, sec, jour_sec, code_retour)

! Conversion en jours fractionnaires
call md_joursec_jourfrac(jour_sec, date, code_retour)

date = date + 33282._pm_reel
call cps_init_utilisateur()
! Mise à jour des données dans COMPAS
ierr=cps_majBullA(date, fichier_brut, fichier_brut_2000)
  if (ierr.eq.CPS_OK) then
     write(*,"(a)") "Mise a jour OK"
  else
     write(*,"(a)") "Echec de la mise a jour"
  end if
call cps_close_utilisateur()

end program maj_bullA
