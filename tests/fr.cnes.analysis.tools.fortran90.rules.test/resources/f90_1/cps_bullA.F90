module cps_bullA

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  cps_bullA
!
!$Resume
!
!$Description
! Module contenant des utilitaires relatifs à la manipulation des
! données issues du bulletin A.
!
!$Auteur
!
!$Version
!  $Id: cps_bullA.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: cps_bullA.F90,v $
!  Revision 1.7  2010/10/21 13:46:22  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.6  2007/05/21 07:01:26  vivaresf
!  Version 2.2 : révision des cartouches
!
!  Revision 1.5  2006/05/12 12:06:09  bouillaj
!  Amelioration qualite : complements sur les cartouches
!  Revision 1.4  2006/05/02 09:38:26  vpg
!  Suppression des variables non utilisees
!  Revision 1.3  2006/03/20 15:54:11  vpg
!  Mise a jour des cartouches
!
!$FinHistorique
!
!$Usage
!  use cps_bullA
!
!$Structure
!
!$Global
!
!$Common
!
!$Routines
!- cpsi_getCleBullA
!
!$Fonctions
!- cpsi_getCleBullA_JJ_MM_AAAA
!- cpsi_getCleBullA_MJD
!
!$Include
!
!$Module
!#V
!- MSLIB
!#
!
!$Interface
!> cpsi_getclebulla :  cpsi_getCleBullA_MJD, cpsi_getCleBullA_JJ_MM_AAAA
!#V
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!.  cpsi_getCleBullA_JJ_MM_AAAA cpsi_getCleBullA_MJD cpsi_getCleBullA
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  use MSLIB


! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_bullA.F90 69 2012-09-11 08:33:34Z ffsm $'

  interface cpsi_getCleBullA

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getCleBullA
!
!$Resume
!
!$Description
!
!$Acces
!  PUBLIC
!
!$Usage
!  cle = cpsi_getCleBullA(date)
!.    real(KIND=PM_REEL) :: date
!.    character(LEN=256) :: cle
!
!  cle = cpsi_getCleBullA(jj, mm, aaaa)
!.    integer :: jj, mm, aaaa
!.    character(LEN=256) :: cle
!
!$Procedures
!- cpsi_getCleBullA_MJD
!- cpsi_getCleBullA_JJ_MM_AAAA
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure cpsi_getCleBullA_MJD, cpsi_getCleBullA_JJ_MM_AAAA
  end interface

contains


  function cpsi_getCleBullA_JJ_MM_AAAA(jj, mm, aaaa) result(cle)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getCleBullA_JJ_MM_AAAA
!
!$Resume
! V2.0
! Retourne le nom de la structure MADONA correspondant a la date au
! format jj/mm/aaaa.
!
!$Description
! V2.0
! Retourne le nom de la structure MADONA correspondant a la date au
! format jj/mm/aaaa.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  cle = cpsi_getCleBullA_JJ_MM_AAAA(jj, mm, aaaa)
!.    integer :: jj, mm, aaaa
!.    character(LEN=256) :: cle
!
!$Arguments
!>E     jj    :<integer>   jour
!>E     mm    :<integer>   mois
!>E     aaaa  :<integer>   année
!>S     cle   :<LEN=256>   nom de la structure MADONA
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    ! arguments
    integer, intent(in) :: jj, mm, aaaa
    
    ! variables locales
    character(LEN=10) :: buff_jj, buff_mm, buff_aaaa
    
    ! resultat
    character(LEN=256) :: cle
    
    write(buff_jj, 10) jj
    if (buff_jj(1:1).eq.' ') then
       buff_jj(1:1) = '0'
    end if
    write(buff_mm, 10) mm
    if (buff_mm(1:1).eq.' ') then
       buff_mm(1:1) = '0'
    end if
    write(buff_aaaa, 20) aaaa
    cle = "bullA_"//trim(buff_jj)//"_"//trim(buff_mm)//"_"//trim(buff_aaaa)
    
10  format (i2)
20  format (i4)

  end function cpsi_getCleBullA_JJ_MM_AAAA
  
  
  function cpsi_getCleBullA_MJD(date) result(cle)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getCleBullA_MJD
!
!$Resume
! V2.0
! Retourne le nom de la structure MADONA correspondant a la date au
! format MJD 1950.
!
!$Description
! V2.0
! Retourne le nom de la structure MADONA correspondant a la date au
! format MJD 1950.
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  cle = cpsi_getCleBullA_MJD(date)
!.    real(KIND=PM_REEL) :: date
!.    character(LEN=256) :: cle
!
!$Arguments
!>E     date  :<PM_REEL>   date (MJD)
!>S     cle   :<LEN=256>   nom de la structure MADONA
!
!$Common
!
!$Routines
!- md_jourfrac_joursec
!- md_julien_calend
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    implicit none
    ! arguments
    real(KIND=PM_REEL), intent(in) :: date
    
    ! resultat
    character(LEN=256) :: cle
    
    ! variables locales
    type(tm_jour_sec) :: jour_sec
    type(tm_code_retour) :: code_retour
    integer :: jj, mm, aaaa, h, min
    real(KIND=PM_REEL) :: date1950, sec
    
    ! conversion
    date1950 = date-33282
    call md_jourfrac_joursec(date1950, jour_sec, code_retour)
    call md_julien_calend(jour_sec, aaaa, mm, jj, h, min, sec, code_retour)
    
    cle = cpsi_getCleBullA_JJ_MM_AAAA(jj, mm, aaaa)
    
  end function cpsi_getCleBullA_MJD

  
end module cps_bullA
