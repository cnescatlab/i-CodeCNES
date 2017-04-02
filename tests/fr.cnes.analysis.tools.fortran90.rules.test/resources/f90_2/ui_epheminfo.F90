program ui_epheminfo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_epheminfo
!
!$Resume
!  Informations sur les méthodes et théories éphémérides disponibles
!
!$Description
!.    Si option "-m"       liste et descriptif rapide des codes méthodes
!                          théories disponibles
!.    Si option "-code"    descriptif du code demandé
!.    Si "-code" et "-fic" descriptif du code demandé et liste des fichiers 
!                          disponibles
!.    Si "-code ... -fic nomdefichier" 
!                          descriptif du fichier demandé (théorie, corps présent, 
!                          période couverte), en anglais pour les fichiers NAIF
!.    Si "-l"              codage des corps 
!.    Si "-ll"             option obsolète, utiliser ui_compasinfo 
!.    Si "-r"              numéro des principaux repères
!.    Si "-cc codecorps"   liste des fichiers NAIF contenant le corps codecorps
!     pour trouver le code d'une planète donnée, faire :
!.     ui_epheminfo -ll | grep -i planete
!
!$Auteur
!  F.VIVARES           (SchlumbergerSema)
!
!$Version
!  $Id: ui_epheminfo.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ui_epheminfo.F90,v $
!  Revision 1.9  2010/10/25 08:35:51  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.8  2010/05/28 11:29:42  jlrobin
!  VERSION::FA-ID:1358:28/05/2010:Ajout de messages d'erreurs
!
!  Revision 1.6  2009/09/09 15:08:04  cmartel
!  FA-ID 1196 : Correction mineures sur les aides des utilitaires
!
!  Revision 1.5  2009/03/23 14:04:14  cml
!  DM-ID 1159 : Correction de declaration des arguments
!
!  Revision 1.4  2008/10/31 13:08:42  cml
!  FA-ID 1076 : Corrections mineurs dans les resultats des utilitaires
!
!  Revision 1.3  2008/04/28 13:00:46  vivaresf
!  FA-ID 664 : présentation des sorties et cartouches
!
!  Revision 1.2  2008/04/10 11:02:08  vivaresf
!  Version 2.4 AQ : correction des cartouches, suppression du code en commentaire
!
!  Revision 1.1  2008/02/08 17:51:40  vivaresf
!  FA-ID 889, DM-ID 820 :
!  - réorganisation du code pour que tout soit dans le répertoire src
!  Revision 1.11  2007/09/18 12:51:31  huec
!  DM-ID 698 : Amelioration des moyens de tests COMPAS
!  Revision 1.10  2007/07/05 17:14:34  vivaresf
!  FA-ID 664 : correction de la présentation
!  Revision 1.9  2007/01/18 14:04:03  fabrec
!  FA-ID 669 : resultat de ui_epheminfo
!  Revision 1.8  2006/11/21 12:08:42  vivaresf
!  Version 2.1 : passage Understand, variables inutilisées
!  Revision 1.7  2006/11/02 12:42:13  mle
!  passage understand
!  Revision 1.6  2006/10/06 08:51:57  mle
!  DM-ID 462 : version finale
!  Revision 1.5  2006/09/26 15:26:04  mle
!  DM-ID 462 : modification de l'option -ll
!  Revision 1.4  2006/07/03 08:52:12  vpg
!  Prise en compte des fichiers Tchebychev dans une base locale
!  Revision 1.3  2006/06/16 17:32:05  vivaresf
!  Cartouches d'entete
!  Revision 1.2  2006/05/31 13:08:38  vivaresf
!  COMPAS 2.0 : validation
!  Revision 1.1.1.1  2006/02/06 15:48:20  vpg
!  ihm et utilitaires COMPAS V2-0
!  Revision 1.2  2005/12/08 18:24:27  vivaresf
!  Mise a jour des cartcouhes
!  Revision 1.1.1.1  2005/12/07 07:23:10  vivaresf
!  Refonte de COMPAS
!  Revision 1.11  2005/11/07 15:51:47  bouillaj
!  Amelioration qualite sur la LIBEPHEM
!  Revision 1.10  2005/10/13 10:37:41  bouillaj
!  FA-ID 371 : Cloture du FT (Corrections dans libephem pour passage linux)
!  Revision 1.9.2.1  2005/10/13 10:26:57  bouillaj
!  FA-ID 371 : Version initiale du FT (Corrections dans libephem pour passage linux)
!  Revision 1.9  2005/03/14 09:13:15  vivaresf
!  Documentation des cartouches
!  Revision 1.8  2005/03/09 09:12:06  vivaresf
!  Correction des cartouches
!  Revision 1.7  2004/12/17 14:42:46  vivaresf
!  FA-ID 327 : Initialisation
!  Revision 1.6  2004/12/03 17:48:11  vivaresf
!  DM-ID 158 : suppression de l'AMLIB
!  Revision 1.5  2004/12/03 12:40:25  vivaresf
!  maj cartouches
!  Revision 1.4  2004/12/03 10:07:29  vivaresf
!  Cartouche
!  Revision 1.3  2004/09/17 14:41:11  tanguy
!  Compilation avec les nouvelles
!  versions mslib 6.0.1, mspro 5.0.1 et mage 1.3.1
!  Revision 1.2  2004/05/25 10:06:05  vivaresf
!  Renommage des constantes ephempath en ephv_ephempath et
!  Renommage des codes reperes
!  Format de sorties pour la liste des corps
!  Revision 1.1.1.1  2004/04/02 09:07:25  vivaresf
!  Gestion de configuration locale
!  Revision 1.16  2004/01/12 09:45:58  bremard
!  Mise à jour cartouche
!  Revision 1.15  2004/01/08 11:08:03  bremard
!  Modification de la présentation du résultat de l'option -cc
!  Revision 1.14  2004/01/07 16:23:44  bremard
!  Ajout option [-cc codecorps]
!  Revision 1.13  2003/12/30 16:01:27  bremard
!  Passage de sous-programmes dans eph_info.F90
!  Revision 1.12  2003/09/03 11:37:59  bremard
!  PhB - Correction conflit sur cas affichage des noms longs des cometes
!  Revision 1.11  2003/06/24 10:29:43  vivaresf
!  Affichage
!  Revision 1.10  2003/06/06 15:00:15  bremard
!  Particularisation des parametres
!  Revision 1.7  2002/10/08 08:14:00  vivaresf
!  Commentaires
!  Revision 1.5  2001/12/17 17:41:51  vivaresf
!  Noms des reperes
!  Revision 1.4  2001/12/13 10:14:34  bremard
!  PhB - Ajout option -r pour liste des numéros de repère
!  Revision 1.3  2001/12/07 16:44:44  vivaresf
!  Presentation fm des cartouches
!  Revision 1.2  2001/12/05 16:50:17  bremard
!  PhB - Mise à jour du cartouche
!  Revision 1.1  2001/11/27 15:45:09  vivaresf
!  Version initiale
!
!$FinHistorique
!
!$Structure
!
!$Remarques
!
!$Mots-cles
!     coordonnées
!
!$Voir-Aussi
!     eph_poscor
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use msp_gestion_erreur
  use eph_info
  use eph_constantes
  use ui_ioplus

  implicit none

! SVN Source File Id
  character(len=256) :: SVN_VER =  '$Id: ui_epheminfo.F90 69 2012-09-11 08:33:34Z ffsm $'


#include "formats.h"

  ! entrées/sorties
  integer :: code
  logical :: listem=.false.
  logical :: listec=.false.
  logical :: listeconstantes=.false.
  logical :: helpc=.false.
  logical :: listefic=.false.
  logical :: listerep=.false.
  logical :: testfic=.false.
  logical :: corpsinfic=.false.
  logical :: presencefic=.false.

  ! 
  integer :: ncodes
  integer, dimension(ephv_maxcodes) :: codes
  character(len=50) :: rep, mm, th
  character(len=80) :: ephempath

  ! variables locales
  type(MSP_MESSAGE) :: messages
  integer :: noptions, ierfin, ind, nvar, iargc, nbargumentsok
  character(LEN=256), dimension(50) :: l_opt

  character(len=ephv_lgmax) :: fichier
  integer :: ii, ier
  integer :: long
  character(len=10) :: codecorps
  character(len=ephv_lgmax) :: fd

  logical :: calcul=.false.
  logical :: arg_lu, base_locale

  ! Lecture des paramètres (en ligne ou sur fichier)

  ! initialisation
  !---------------
  ind = 1
  call cps_init_utilisateur()
  if (MSP_gen_messages("ui_epheminfo")) goto 999

  ! analyse des arguments
  !----------------------

  ! lecture des arguments
  call ui_lire_options(noptions, l_opt)
  call ui_test_arguments ("-h", "", ierfin)
  nvar=iargc()

  ! -h ou pas de parametres
  if(ierfin.eq.1.or.nvar==0) then
     call ui_ecrire_help("ui_epheminfo")
     goto 999
  endif

  nbargumentsok=0
  ! decodage
  do while(ind<=nvar)
     select case (l_opt(ind))
     case ("-code")
        helpc=.true.
        call ui_lire_arguments(ier, code=code)
        if(ier.ge.0) calcul=.true.
        call ui_lire_fic1 (listefic, testfic, fichier)
        nbargumentsok=nbargumentsok+2        

     case ("-m")
        listem=.true.
        calcul=.true.
        nbargumentsok=nbargumentsok+1 

     case ("-l")
        listec=.true.
        calcul=.true.
        nbargumentsok=nbargumentsok+1 
        
     case ("-ll")
        listeconstantes=.true.
        calcul=.true.
        nbargumentsok=nbargumentsok+1         

     case ("-r")
        listerep=.true.
        calcul=.true.
        nbargumentsok=nbargumentsok+1 

     case ("-cc")
        call ui_lire_argcode ("-cc",codecorps,arg_lu)
        corpsinfic=.true.
        calcul=.true.
        nbargumentsok=nbargumentsok+2 

     case ("-fic")
        presencefic=.true.
        nbargumentsok=nbargumentsok+2 

     end select
     ind = ind+1
  end do

  ! Tester si tous les arguments sont corrects
  if ( nbargumentsok /= nvar ) then
     ! verifier s'il ne s'agit pas du listing "-fic"
     if ( .not.( (nbargumentsok==(nvar+1)).and.presencefic ) ) then
         write(*,1000) "les arguments sont erronés"
         call ui_ecrire_help("ui_epheminfo")
         goto 999
     endif
  endif

  ! corps du programme

  if(.not.calcul) goto 999

  ! liste des codes methode (-m)
  if(listem) then
     call eph_infocodes(codes, ncodes)
     if(ncodes.gt.0) write(*,1000) "Code   Méthode        Théorie       Fichier/défaut"

     do ii=1, ncodes
        call eph_infogetLocal(codes(ii), base_locale, repertoire=rep, &
             fichierd=fd, methode=mm, theorie=th)
        if (.not.base_locale) then
           call eph_infoget(codes(ii), repertoire=rep, fichierd=fd, &
                methode=mm, theorie=th)
        end if
        write(*,"((i4,a4,a15,a15,a30))") codes(ii), " ", mm, th, fd
     end do
  endif

  ! explication d'un code methode (-code [-fic])

  if(helpc) then
     call ui_listecodemethode(testfic, listefic, code, fichier)    
  endif

  ! codages planètes/satellites (-l)
  if(listec) then
     call ui_listeplanetesat()
  endif

  ! corps et constantes disponibles (-ll) :
  ! l'option est toujours disponible mais le resultat est donne par ui_compasinfo
  if(listeconstantes) then
     write(*,1000) "Pour lister les codes corps disponibles, il faut utiliser ui_compasinfo."
  endif

  ! codes repères (-r)
  if(listerep) then
     call ui_listereperes()
  endif


  ! Liste des fichiers contenant corps (-cc)
  !  Uniquement pour la métode NAIF
  if(corpsinfic) then

     if(.not.ephv_acces_open) call eph_infoinit()

     ! Suppression du / final
     long=len_trim(ephv_ephempath)
     if (ephv_ephempath(long:long)=="/") ephv_ephempath(long:long)=""

     ephempath=trim(ephv_ephempath)//"/NAIF"

     write(*,'(a)') "Fonction disponible uniquement pour la méthode NAIF"
     write(*,1000) ""
     write(*,'(a)') "Fichiers BSP sous  : "//trim(ephempath)
     write(*,'(a)') "Contenant le corps : "//trim(codecorps)
     write(*,1000) ""
     call flush(6)

     call ephui_corps_in_fic(ephempath,codecorps,affichage=.true.)

  endif


  call eph_infoclose()

999 continue

! affichage des erreurs
  if (MSP_PROBLEME) then
     call MSP_recuperer_message(message=messages, nb_mes=MSP_tous_messages)
     call MSP_afficher_message(message=messages, unit=0)
  endif
  
  ! fin
  call cps_close_utilisateur()

end program ui_epheminfo

subroutine ui_lire_fic1(listefic, testfic, fichier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ui_lire_fic1
!
!$Resume
!  lecture des arguments 
!
!$Description
!  Lecture des arguments fichiers 
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ui_lire_fic1(listefic, testfic, fichier)
!.    character(len=*) :: fichier
!.    logical :: listefic, testfic
!
!$Arguments
!>S     listefic  :<logical>         un ou plusieurs fichiers présents
!>S     testfic   :<logical>         option -fic présente
!>S     fichier   :<LEN=*)>          fichier
!
!$Common
!
!$Routines
!- getarg
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

  character(len=*), intent(out) ::fichier
  logical, intent(out) :: listefic, testfic

  integer::nb_arg,pos
  integer::iargc
  character(len=100)::argument
    
  listefic=.false.
  testfic=.false.
  fichier=""

  nb_arg = iargc()
  
  do pos=1,nb_arg
     call getarg(pos,argument)
     
     if(trim(argument).eq."-fic") then 
        if ((pos+1).le.nb_arg) then 
           call getarg(pos+1,argument)
           fichier=trim(argument)
           testfic=.true.           
        else
           listefic=.true.
        endif
     endif
  enddo

end subroutine ui_lire_fic1

subroutine ui_lire_argcode(arg, param, arg_lu)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ui_lire_argcode
!
!$Resume
!  lecture d'un argument
!
!$Description
!  lecture d'un argument
! lecture du paramètre suivant un argument donné
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ui_lire_argcode(arg, param, arg_lu)
!.    character(len=*) :: arg
!.    character(len=*) :: param
!.    logical :: arg_lu
!
!$Arguments
!>E     arg     :<LEN=*>     nom de l'option
!>S     param   :<LEN=*>     paramètres suivant cette option
!>S     arg_lu  :<logical>   paramètre lu, oui/non
!
!$Common
!
!$Routines
!- getarg
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

  character(len=*), intent(in) :: arg
  character(len=*), intent(out) ::param
  logical, intent(out) :: arg_lu

  integer::nb_arg,pos
  integer::iargc
  character(len=128)::argument
    
  arg_lu=.false.

  nb_arg = iargc()
  
  do pos=1,nb_arg
     call getarg(pos,argument)
     
     if(trim(argument)==trim(arg)) then 
        if ((pos+1).le.nb_arg) then 
           call getarg(pos+1,argument)
           param=trim(argument)
           arg_lu=.true.         
        endif
     endif
  enddo

end subroutine ui_lire_argcode


