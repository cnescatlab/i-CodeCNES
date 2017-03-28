program maj_acsol2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  maj_acsol2
!
!$Resume
!  outil de mise à jour de COMPAS à partir du fichier ACSOL2
!$Description
!  outil de mise à jour de COMPAS à partir du fichier ACSOL2
!
! Usage
!
!  maj_acsol2 date fichier_acsol2
!
!>E date <jjmmaa> : date sous forme 6 chiffres
!>E fichier_acsol2 : fichier dont le contenu est à insérer
!
!$Auteur
!
!$Version
!  $Id: maj_acsol2.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: maj_acsol2.F90,v $
!  Revision 1.4  2010/10/25 08:35:49  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.3  2008/05/06 08:13:59  vivaresf
!  FA-ID 889 : validation et test
!
!  Revision 1.2  2008/04/11 12:59:30  vivaresf
!  FA-ID 778 : rajout du cartouche
!
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

  use ui_acsol2
  implicit none

! SVN Source File Id
  character(len=256) :: SVN_VER =  '$Id: maj_acsol2.F90 69 2012-09-11 08:33:34Z ffsm $'

  
  type (tm_jour_sec) :: jour_sec
  type (tm_code_retour) :: code_retour
  
  integer :: nbarg, iargc, lnblnk
  character (LEN=6) :: ch_date
  character (LEN=100) :: fichier_brut
  integer :: jj, mm, aa, h, min
  real(KIND=PM_REEL) :: date, sec
  integer :: ierr
  
  ! Lecture des arguments
  nbarg = iargc ()
  if (nbarg == 2) then
     call GETARG (1,ch_date)
     call GETARG (2,fichier_brut)
     fichier_brut = fichier_brut(1:lnblnk(fichier_brut))
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
  
  call cps_init_utilisateur()
  ! Mise à jour des données dans COMPAS
  ierr=cps_majAcsol2(date, fichier_brut)
  if (ierr.eq.CPS_OK) then
     write(*,"(a)") "Mise a jour OK"
  else
     write(*,"(a)") "Echec de la mise a jour"
  end if
  call flush(6)
  call cps_close_utilisateur()
  
end program maj_acsol2

