module cps_constantes

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  cps_constantes
!
!$Resume
!
!$Description
!  Ce module contient des paramètres globaux de la bibliothèque
!  COMPAS ainsi que des fonctions et des routines utilitaires.
!
!$Auteur
!  vpg
!
!$Version
!  $Id: cps_constantes.F90 355 2013-02-14 12:16:41Z aadt $
!
!$Historique
!  $Log: cps_constantes.F90,v $
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.21  2010/10/21 13:46:20  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.20  2010/10/21 10:44:16  ogarat
!  VERSION::FA-ID:1451:21/10/2010:Mise a jour des routines de lecture pour l'atmosphere tabulee
!
!  Revision 1.19  2010/04/30 14:12:31  jlrobin
!  V2.9::AQ::30/04/2010:merge de la branche de developpement modeles atmospheriques
!
!  Revision 1.18.2.1  2010/02/23 09:59:58  cmartel
!  VERSION::DM-ID:1361:23/02/2010:Ajout de la categorie atmosphere tabulee
!
!  Revision 1.18  2009/11/02 09:16:29  cmartel
!  DM-ID 842 : Ajout de la categorie activite solaire sur fichier
!
!  Revision 1.17  2008/10/03 12:44:34  cml
!  FA-ID 1024 : Renommage des variables len et precision
!
!  Revision 1.16  2008/10/01 17:37:15  tanguyy
!  DM-ID 1058 / AQ : suppression des boucles infinies
!
!  Revision 1.15  2008/09/16 08:05:41  cml
!  DM-ID 1111 : Ajout d'une constante pour les modeles de pole sur fichier
!
!  Revision 1.14  2008/02/11 08:46:49  huec
!  DM-ID 11 : Ajout d un code pour la categorie saut_du_tuc
!
!  Revision 1.13  2007/05/21 06:56:19  vivaresf
!  Version 2.2 : révision des cartouches
!
!  Revision 1.12  2006/05/30 12:29:03  vivaresf
!  Separation COMPAS_UI,
!  suppression MSPRO
!  robustesse (iostat sur les deallocate)
!  Revision 1.11  2006/05/30 08:27:46  vivaresf
!  DM-ID 387 : entete MADONA
!  suppression des codes commentés
!  commentaires vrais sur le code
!  Revision 1.10  2006/05/15 15:06:38  vpg
!  Amelioration qualite : complements sur les cartouches
!  Revision 1.9  2006/03/22 13:25:52  vpg
!  Ajout de CPS_CATEG_EPHEM
!  Revision 1.8  2006/03/20 15:52:43  vpg
!  Mise a jour des cartouches
!
!$FinHistorique
!
!$Usage
!  use cps_constantes
!
!$Structure
!
!$Global
!
!>  CPS_MAX_BULLA_SURE               : <integer,parameter,public>  
!>  CPS_MAX_BULLA_PRED               : <integer,parameter,public>  
!>  CPS_MAX_ACSOL2_SURE              : <integer,parameter,public>  
!>  CPS_MAX_ACSOL2_PRED              : <integer,parameter,public>  
!>  CPS_ERR_DEF                      : <integer,parameter,public>  
!>  CPS_OK                           : <integer,parameter,public>  
!>  CPS_ENTIER                       : <integer,parameter,public>  
!>  CPS_REEL                         : <integer,parameter,public>  
!>  CPS_STRING                       : <integer,parameter,public>  
!>  CPS_MAXLG                        : <integer,parameter,public>  
!>  MAXCORPS                         : <integer,parameter,public>  
!>  CPS_CATEG_CORPS                  : <LEN=5,parameter,public>    
!>  CPS_CATEG_THEORIE                : <LEN=7,parameter,public>    
!>  CPS_CATEG_CSTES_CORPS_THEORIE    : <LEN=19,parameter,public>   
!>  CPS_CATEG_KEPLER_CORPS_THEORIE   : <LEN=26,parameter,public>   
!>  CPS_CATEG_POTENTIEL              : <LEN=17,parameter,public>   
!>  CPS_CATEG_ATMOSPHERE             : <LEN=18,parameter,public>   
!>  CPS_CATEG_EPHEM                  : <LEN=11,parameter,public>   
!>  CPS_CATEG_BULLA_SURE             : <LEN=14,parameter,public>   
!>  CPS_CATEG_BULLA_PRED             : <LEN=14,parameter,public>   
!>  CPS_CATEG_ACSOL2_SURE            : <LEN=11,parameter,public>   
!>  CPS_CATEG_ACSOL2_PRED            : <LEN=11,parameter,public>   
!>  CPS_CATEG_TAITUC                 : <LEN=11,parameter,public> 
!>  CPS_UNITE_UNDEF                  : <LEN=5,parameter,public>    
!>  CPS_DATE_REF                     : <LEN=??,parameter,public>   
!>  CPS_DGA                          : <LEN=??,parameter,public>   
!>  CPS_EXC                          : <LEN=??,parameter,public>   
!>  CPS_INC                          : <LEN=??,parameter,public>   
!>  CPS_POM                          : <LEN=??,parameter,public>   
!>  CPS_GOM                          : <LEN=??,parameter,public>   
!>  CPS_ANM                          : <LEN=??,parameter,public>   
!>  CPS_FIN_SEQ                      : <integer,parameter,public>  
!>  CPS_NO_RESTRICTION               : <LEN=??,parameter,public>   
!>  CPS_ALL_ID                       : <LEN=??,parameter,public>   
!$Common
!
!$Routines
!- cpsi_getRepAndNameFic
!
!$Fonctions
!- cpsi_intToChar
!- cpsi_getRep
!- cpsi_getMinuscules
!- cpsi_compareReels
!
!$Include
!
!$Module
!#V
!- MSLIB
!#
!
!$Interface
!#V
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!.  cpsi_intToChar cpsi_getRep cpsi_getMinuscules cpsi_compareReels cpsi_getRepAndNameFic
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use MSLIB

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_constantes.F90 355 2013-02-14 12:16:41Z aadt $'


  integer, parameter, public :: CPS_MAX_BULLA_SURE = 1000
  integer, parameter, public :: CPS_MAX_BULLA_PRED = 120
  
  integer, parameter, public :: CPS_MAX_ACSOL2_SURE = 1000
  integer, parameter, public :: CPS_MAX_ACSOL2_PRED = 50

  integer, parameter, public :: CPS_ERR_DEF = -1
  integer, parameter, public :: CPS_OK = 0

  integer, parameter, public :: CPS_ENTIER = 0
  integer, parameter, public :: CPS_REEL = 1
  integer, parameter, public :: CPS_STRING = 2

  integer, parameter, public :: CPS_MAXLG = 256
  integer, parameter, public :: MAXCORPS = 100

  character(LEN=5), parameter, public :: CPS_CATEG_CORPS = 'corps'
  character(LEN=7), parameter, public :: CPS_CATEG_THEORIE = 'theorie'
  character(LEN=19), parameter, public :: CPS_CATEG_CSTES_CORPS_THEORIE = 'cstes_corps_theorie'
  character(LEN=26), parameter, public :: CPS_CATEG_KEPLER_CORPS_THEORIE = 'param_kepler_corps_theorie'
  character(LEN=17), parameter, public :: CPS_CATEG_POTENTIEL = 'modeles_potentiel'
  character(LEN=18), parameter, public :: CPS_CATEG_ATMOSPHERE = 'modeles_atmosphere'
  character(LEN=11), parameter, public :: CPS_CATEG_EPHEM = 'ephemerides'
  character(LEN=14), parameter, public :: CPS_CATEG_BULLA_SURE = 'bulletinA_sure'
  character(LEN=14), parameter, public :: CPS_CATEG_BULLA_PRED = 'bulletinA_pred'
  character(LEN=11), parameter, public :: CPS_CATEG_ACSOL2_SURE = 'acsol2_sure'
  character(LEN=11), parameter, public :: CPS_CATEG_ACSOL2_PRED = 'acsol2_pred'
  character(LEN=14), parameter, public :: CPS_CATEG_ACSOL2_FICHIER = 'acsol2_fichier'
  character(LEN=11), parameter, public :: CPS_CATEG_TAITUC = 'saut_du_tuc'
  character(LEN=24), parameter, public :: CPS_CATEG_POLETSID = 'modeles_poletsid_fichier'
  
  character(LEN=5), parameter, public :: CPS_UNITE_UNDEF = "undef"
  
  character(8), parameter, public :: CPS_DATE_REF = "dateref"
  character(3), parameter, public :: CPS_DGA = "dga"
  character(3), parameter, public :: CPS_EXC = "exc"
  character(3), parameter, public :: CPS_INC = "inc"
  character(3), parameter, public :: CPS_POM = "pom"
  character(3), parameter, public :: CPS_GOM = "gom"
  character(3), parameter, public :: CPS_ANM = "anm"

  integer, parameter, public :: CPS_FIN_SEQ = 0

  character(4), parameter, public :: CPS_NO_RESTRICTION = "tout"
  character(1), parameter, public :: CPS_ALL_ID = "*"
  
contains

  function cpsi_intToChar(num) result (s)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_intToChar
!
!$Resume
!
!$Description
!  Cette fonction convertit une entier en sa representation en chaîne 
!  de caractères.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  s = cpsi_intToChar(num)
!.    integer :: num
!.    character(LEN=CPS_MAXLG) :: s
!
!$Arguments
!>E     num  :<integer>         entier à convertir  
!>S     s    :<LEN=CPS_MAXLG>   chaîne de caractères représentant l'entier  
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
    ! argument
    integer, intent(in) :: num

    ! resultat
    character(LEN=CPS_MAXLG) :: s

    ! variables locales
    integer :: q, r, n
    
    s = trim("")
    n = num
    
    ! initialisation de q pour la condition de boucle
    q = -1

    do while (q /= 0) 
       q = n/10
       r = n - 10*q
       select case (r)
       case (0)
          s = "0"//trim(s)
       case (1)
          s = "1"//trim(s)
       case (2)
          s = "2"//trim(s)
       case (3)
          s = "3"//trim(s)
       case (4)
          s = "4"//trim(s)
       case (5)
          s = "5"//trim(s)
       case (6)
          s = "6"//trim(s)
       case (7)
          s = "7"//trim(s)
       case (8)
          s = "8"//trim(s)
       case (9)
          s = "9"//trim(s)
       end select
       n = q
    end do
    
  end function cpsi_intToChar

  
  function cpsi_getRep(rep) result(res)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getRep
!
!$Resume
!
!$Description
!  Supprime le caractere '/' si besoin a la chaine
!  pour former un repertoire sans '/' a la fin.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  res = cpsi_getRep(rep)
!.    character(LEN=*) :: rep
!.    character(LEN=CPS_MAXLG) :: res
!
!$Arguments
!>E     rep  :<LEN=*>           
!>S     res  :<LEN=CPS_MAXLG>   
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
    ! argument
    character(LEN=*), intent(in) :: rep

    ! resultat
    character(LEN=CPS_MAXLG) :: res

    ! variable locale : Longueur de la chaine
    integer :: long_chaine
    
    long_chaine = len_trim(rep)
    ! Si le dernier caractère est "/" on le supprime
    if (rep(long_chaine:long_chaine).eq."/") then
       res = trim(rep)
       res(long_chaine:long_chaine) = " "
    else
       res = trim(rep)
    end if
    
  end function cpsi_getRep


  function cpsi_getMinuscules(s) result(res)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getMinuscules
!
!$Resume
!
!$Description
!  Fonction qui met toutes les lettres d'une chaîne de caractères
!  en minuscules.
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  res = cpsi_getMinuscules(s)
!.    character(LEN=*) :: s
!.    character(LEN=CPS_MAXLG) :: res
!
!$Arguments
!>E     s    :<LEN=*>           chaîne de caractères à convertir en minuscules
!>S     res  :<LEN=CPS_MAXLG>   chaîne de caractères en minuscules
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
    ! argument
    character(LEN=*), intent(in) :: s
    
    ! resultat
    character(LEN=CPS_MAXLG) :: res
    
    ! variable locale
    integer :: i

    res = ""
    do i=1, len_trim(s)
       select case (s(i:i))
       case ("A")
          res(i:i) = "a"
       case ("B")
          res(i:i) = "b"
       case ("C")
          res(i:i) = "c"
       case ("D")
          res(i:i) = "d"
       case ("E")
          res(i:i) = "e"
       case ("F")
          res(i:i) = "f"
       case ("G")
          res(i:i) = "g"
       case ("H")
          res(i:i) = "h"
       case ("I")
          res(i:i) = "i"
       case ("J")
          res(i:i) = "j"
       case ("K")
          res(i:i) = "k"
       case ("L")
          res(i:i) = "l"
       case ("M")
          res(i:i) = "m"
       case ("N")
          res(i:i) = "n"
       case ("O")
          res(i:i) = "o"
       case ("P")
          res(i:i) = "p"
       case ("Q")
          res(i:i) = "q"
       case ("R")
          res(i:i) = "r"
       case ("S")
          res(i:i) = "s"
       case ("T")
          res(i:i) = "t"
       case ("U")
          res(i:i) = "u"
       case ("V")
          res(i:i) = "v"
       case ("W")
          res(i:i) = "w"
       case ("X")
          res(i:i) = "x"
       case ("Y")
          res(i:i) = "y"
       case ("Z")
          res(i:i) = "z"
       case default
          res(i:i) = s(i:i)
       end select
    end do

  end function cpsi_getMinuscules




  subroutine cpsi_getRepAndNameFic(path, rep_fic, name_fic)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_getRepAndNameFic
!
!$Resume
!
!$Description
!  Routine qui extrait le repertoire et le nom d'un fichier à partir
!  du nom complet d'un fichier.
!  Par exemple :
!  si path="/usr/local_ms/MADONA/V3-10-1/lib/libmadona.a"
!  alors rep_fic="/usr/local_ms/MADONA/V3-10-1/lib"
!  et name_fic="libmadona.a"
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_getRepAndNameFic(path, rep_fic, name_fic)
!.    character(LEN=*) :: path
!.    character(LEN=*) :: rep_fic, name_fic
!
!$Arguments
!>E     path      :<LEN=*>   
!>S     rep_fic   :<LEN=*>   
!>S     name_fic  :<LEN=*>   
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
    character(LEN=*), intent(in) :: path
    character(LEN=*), intent(out) :: rep_fic, name_fic

    ! variables locales
    integer :: ind, ind_courant
    character(LEN=CPS_MAXLG) :: buff
    
    ind = 0
    rep_fic = ""
    name_fic = trim(path)
    buff = trim(path)
    
    ! initialisation de la condition de boucle
    ind_courant = 1

    do while (ind_courant > 0)
       ind_courant = index(buff, "/")
       if (ind_courant.gt.0) then
          buff(1:) = buff(ind_courant+1:)
          ind = ind+ind_courant
       end if
    end do

    if (ind.gt.1) then
       rep_fic = path(1:ind-1)
       name_fic = path(ind+1:)
    end if

  end subroutine cpsi_getRepAndNameFic


function cpsi_compareReels(a,b) result(res)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_compareReels
!
!$Resume
!
!$Description
!  Fonction qui compare deux réels à la précision machine.
!  res = cpsi_compareReels(a,b)
!  res = 0 si a=b à la precision machine pres
!  res = +1 si a>b
!  res = -1 si a<b
!
!$Auteur
!  vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  res = cpsi_compareReels(a,b)
!.    real(KIND=PM_REEL) :: a, b
!.    integer :: res
!
!$Arguments
!>E     a    :<PM_REEL>   premier réel à comparer
!>E     b    :<PM_REEL>   second réel à comparer
!>S     res  :<integer>   b et -1 si b<a
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
  real(KIND=PM_REEL), intent(in) :: a, b

  ! resultat
  integer :: res
  
  ! variables locales
  real(KIND=PM_REEL) :: eps_machine
  
  ! precision machine
  eps_machine = epsilon(a)
  
  if (abs(a-b).lt.eps_machine) then
     res = 0
  elseif (a.gt.b) then
     res = 1
  else
     res = -1
  end if
  
end function cpsi_compareReels

end module cps_constantes
