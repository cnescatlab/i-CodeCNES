program lecture_bullA

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  lecture_bullA
!
!$Resume
!  Lecture des données d'un fichier passé en argument et stockage des
!  données dans un tableau de structures
!
!$Description
!  Lecture des données au format du bulletin A de l'IERS, contrôle
!  d'erreur et stockage des données dans un tableau de structures,
!  chaque structure contient une ligne du fichier
!
!$Auteur
!  G. MERCADIER (ATOS ORIGIN), 07/02/2006 
!
!$Version
!  $Id: lecture_bullA.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: lecture_bullA.F90,v $
!  Revision 1.3  2010/10/25 08:35:49  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.2  2008/04/11 12:59:44  vivaresf
!  Version 2.4 AQ : mise à jour des cartouches
!
!  Revision 1.1  2008/02/08 17:50:48  vivaresf
!  FA-ID 889, DM-ID 820 :
!  - réorganisation du code pour que tout soit dans le répertoire src
!  Revision 1.2  2006/10/25 07:29:08  vpg
!  DM 425 : passage PSIMU sous Linux. Modification des tableaux declares allocatable en pointeur
!  Revision 1.1  2006/02/22 09:56:19  mercadig
!  DM-ID 387 Programme de controle du fichier bulletin A
!
!$FinHistorique
!
!$Structure
!
!$Global
!
!>  nbarg       : <integer>                      
!>  iargc       : <integer>                      
!>  lnblnk      : <integer>                      
!>  file        : <LEN=100>                      
!>  ierr        : <integer>                      
!>  ligne       : <integer>                      
!>  tab_enreg   : <ligne_bullA,DIM=(:),pointer>  
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
  character(len=256) :: SVN_VER =  '$Id: lecture_bullA.F90 69 2012-09-11 08:33:34Z ffsm $'


integer :: nbarg, iargc, lnblnk
character (LEN=100) :: file
integer :: ierr
integer :: ligne

type (ligne_bullA),dimension(:),pointer :: tab_enreg

! Lecture des arguments
nbarg = iargc ()
if (nbarg == 1) then
   call GETARG (1,file)
   file = file(1:lnblnk(file))
endif

! Lecture du fichier "file" pour contrôle d'erreur
ierr=cps_lirebulla(file,tab_enreg,ligne)

! Le code d'erreur ierr est retourné par le paramètre "iostat" de la fonction "read" 
if (ierr > 0) then
   write(6,*) 'La ligne ',ligne,' comporte peut etre des erreurs de format'
else           
   write(6,*) 'Controle OK pour le fichier'
endif

if (associated(tab_enreg)) then
   deallocate (tab_enreg)
end if

end program lecture_bullA
