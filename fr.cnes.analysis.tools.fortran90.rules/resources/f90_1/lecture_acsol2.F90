program lecture_acsol2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  lecture_acsol2
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
!  $Id: lecture_acsol2.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: lecture_acsol2.F90,v $
!  Revision 1.5  2010/10/25 08:35:49  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.4  2009/11/23 10:08:05  cmartel
!  AQ : Suppression de variables inutilisees
!
!  Revision 1.3  2009/11/09 13:29:06  cmartel
!  DM-ID 842 : Modification de la signature de cps_lireAscol2
!
!  Revision 1.2  2008/04/11 12:59:43  vivaresf
!  Version 2.4 AQ : mise à jour des cartouches
!
!  Revision 1.1  2008/02/08 17:50:48  vivaresf
!  FA-ID 889, DM-ID 820 :
!  - réorganisation du code pour que tout soit dans le répertoire src
!  Revision 1.2  2006/10/25 07:29:08  vpg
!  DM 425 : passage PSIMU sous Linux. Modification des tableaux declares allocatable en pointeur
!  Revision 1.1  2006/02/22 09:55:30  mercadig
!  DM-ID 387 Programme de controle du fichier acsol2
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
!>  tab_enreg   : <ligne_acsol2,DIM=(:),pointer>  
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
use cps_acsol2
use msp_gestion_erreur

implicit none

! SVN Source File Id
  character(len=256) :: SVN_VER =  '$Id: lecture_acsol2.F90 69 2012-09-11 08:33:34Z ffsm $'


integer :: nbarg, iargc, lnblnk
character (LEN=100) :: file
integer :: ligne

type (ligne_acsol2),dimension(:), pointer :: tab_enreg

! Lecture des arguments
nbarg = iargc ()
if (nbarg == 1) then
   call GETARG (1,file)
   file = file(1:lnblnk(file))
endif

! Lecture du fichier "file" pour contrôle d'erreur
call cps_lireAcsol2(file,tab_enreg,ligne)


! Le code d'erreur ierr est retourné par le paramètre "iostat" de la fonction "read" 
if ( MSP_ERREUR) then
   write(6,*) 'La ligne ',ligne+7,' comporte peut etre des erreurs de format'
else           
   write(6,*) 'Controle OK pour le fichier'
endif

call MSP_effacer_message(MSP_tous_messages)

if (associated(tab_enreg)) then
   deallocate (tab_enreg)
end if

end program lecture_acsol2

