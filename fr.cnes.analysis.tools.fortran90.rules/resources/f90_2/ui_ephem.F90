program ui_ephem

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_ephem
!
!$Resume
!    Ephémérides à une date donnée
!
!$Description
!   Simple appel de eph_poscor
!
! Usage
!
!.    ui_ephem [-nv] -code code [-fic fichier [fichier]] -cc cc -ci ci 
!.             -date date [-nrep nrep] [-trep trep] [-tau]
!
! Arguments
!
!>E   date    : <PM_REEL> date de calcul B1950 [~jul]
!>E   code    : <integer> code méthode/théorie
!>E   cc      : <integer> code corps central
!>E   ci      : <integer> code corps d'intérêt
!>[E] nrep    : <integer> code repère (voir ui_epheminfo -r)
!>[E] trep    : <PM_REEL> date de définition du repère (TE). Si trep n'est pas
!                         présent, date est prise comme date de référence
!                         pour le repère.
!>[E] nv      : <logical> si présent n'affiche pas la ligne de description
!                          des sorties
!>[E] fichier : <string>  fichier(s) à utiliser (sinon "fichier" de ephem.conf)
!>[E] tau     : <logical> si présent, prise en compte du temps lumière.
!                         Les p/v sont alors les p/v vraies et non les p/v 
!                         géométriques (théories NAIF uniquement).
!
!$Auteur
!    F.VIVARES           (SchlumbergerSema)
!
!$Version
!  $Id: ui_ephem.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ui_ephem.F90,v $
!  Revision 1.10  2010/10/25 08:35:51  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.9  2009/10/06 14:53:28  cmartel
!  FA-ID 1280 : Modification de l'apparence pour des dates tres grandes
!
!  Revision 1.8  2009/09/09 15:08:03  cmartel
!  FA-ID 1196 : Correction mineures sur les aides des utilitaires
!
!  Revision 1.7  2008/11/07 14:36:01  cml
!  AQ : Correction de pointeurs non initialises
!
!  Revision 1.6  2008/10/31 13:08:41  cml
!  FA-ID 1076 : Corrections mineurs dans les resultats des utilitaires
!
!  Revision 1.5  2008/04/28 13:00:47  vivaresf
!  FA-ID 664 : présentation des sorties et cartouches
!
!  Revision 1.4  2008/04/25 18:02:33  vivaresf
!  DM-ID 11 : intégration des sauts du TUC de COMPAS_BASE
!
!  Revision 1.3  2008/04/10 11:02:10  vivaresf
!  Version 2.4 AQ : correction des cartouches, suppression du code en commentaire
!
!  Revision 1.2  2008/04/10 10:57:33  vivaresf
!  Version 2.4 : correction des cartouches
!
!  Revision 1.1  2008/02/08 17:51:39  vivaresf
!  FA-ID 889, DM-ID 820 :
!  - réorganisation du code pour que tout soit dans le répertoire src
!  Revision 1.5  2007/09/18 12:51:42  huec
!  DM-ID 698 : Amelioration des moyens de tests COMPAS
!  Revision 1.4  2006/10/23 12:56:17  vpg
!  DM-ID 566 : Cloture du FT (Amelioration de l ergonomie des IHM de COMPAS_UI V2-0)
!  Revision 1.3.2.1  2006/10/23 10:12:03  vpg
!  DM-ID 566 : Version initiale du FT (Amelioration de l ergonomie des IHM de COMPAS_UI V2-0)
!  Revision 1.3  2006/06/16 17:32:05  vivaresf
!  Cartouches d'entete
!  Revision 1.2  2006/05/31 13:08:38  vivaresf
!  COMPAS 2.0 : validation
!  Revision 1.1.1.1  2006/02/06 15:48:20  vpg
!  ihm et utilitaires COMPAS V2-0
!  Revision 1.4  2005/12/19 08:48:38  bouillaj
!  Amelioration qualite sur l utilitaire
!  Revision 1.3  2005/12/09 14:46:24  vivaresf
!  Mise en forme des tests
!  Revision 1.2  2005/12/08 18:24:28  vivaresf
!  Mise a jour des cartcouhes
!  Revision 1.1.1.1  2005/12/07 07:23:10  vivaresf
!  Refonte de COMPAS
!  Revision 1.11  2005/11/07 15:51:46  bouillaj
!  Amelioration qualite sur la LIBEPHEM
!  Revision 1.10  2005/10/12 13:05:16  bouillaj
!  DM-ID 147. Ajout extrapolation keplerienne
!  Revision 1.9  2005/03/09 09:12:06  vivaresf
!  Correction des cartouches
!  Revision 1.8  2004/12/17 14:57:22  vivaresf
!  Documentation
!  Revision 1.7  2004/12/03 17:48:11  vivaresf
!  DM-ID 158 : suppression de l'AMLIB
!  Revision 1.6  2004/12/03 12:40:25  vivaresf
!  maj cartouches
!  Revision 1.5  2004/12/03 10:07:29  vivaresf
!  Cartouches
!  Revision 1.4  2004/11/15 15:07:15  vivaresf
!  essai de modif
!  Revision 1.3  2004/09/17 14:40:44  tanguy
!  Compilation avec les nouvelles
!  versions mslib 6.0.1, mspro 5.0.1 et mage 1.3.1
!  Revision 1.2  2004/05/25 10:06:37  vivaresf
!  Utilisation de MAGE et non plus de la MECASPA
!  Revision 1.1.1.1  2004/04/02 09:07:25  vivaresf
!  Gestion de configuration locale
!  Revision 1.20  2004/01/08 16:03:45  bremard
!  Lecture de la ressource fcf_mecaspa dans interplanetaire.rc
!  Revision 1.19  2003/12/30 16:02:25  bremard
!  Suppression du cas Extrapolation Keplerienne (61)
!  Revision 1.18  2003/06/13 10:04:53  vivaresf
!  Corps central Soleil pour les cometes
!  Revision 1.17  2003/06/12 15:41:47  vivaresf
!  Lecture de fichiers GS_BULLETIN_IP
!  Revision 1.16  2002/10/08 08:13:59  vivaresf
!  Commentaires
!  Revision 1.14  2002/01/17 11:00:25  bremard
!  PhB - Modif pour prise en compte lecture 2 fichiers (-fic)
!  Revision 1.13  2001/12/11 16:23:50  bremard
!  PhB - Ajout changement de repère
!  Revision 1.12  2001/12/07 16:44:44  vivaresf
!  Presentation fm des cartouches
!  Revision 1.11  2001/12/07 10:06:29  vivaresf
!  Changement de repere
!  Revision 1.10  2001/12/07 09:29:00  vivaresf
!  Changement de repere
!  Revision 1.9  2001/12/06 09:08:02  vivaresf
!  Usage a conserver
!  Revision 1.8  2001/12/05 16:37:50  bremard
!  PhB - Chemins par défaut + option calcul avec tau pour méthode NAIF
!  Revision 1.7  2001/12/05 16:32:31  vivaresf
!  PhB - Prise en compte du parametre tau
!  Revision 1.6  2001/12/04 11:15:07  vivaresf
!  Valeurs par defaut des ressources
!  Revision 1.5  2001/12/03 16:36:50  vivaresf
!  mise au point methode extrapolation
!  Revision 1.4  2001/12/03 13:19:15  vivaresf
!  Appel de eph_extraire_fictxt
!  Revision 1.3  2001/11/30 11:18:00  vivaresf
!  Essai avec extrapolation keplerienne
!  Revision 1.2  2001/11/29 16:19:43  vivaresf
!  methode d'extrapolation + correction fichier de ressources
!  Revision 1.1  2001/11/27 15:45:09  vivaresf
!  Version initiale
!
!$FinHistorique
!
!$Remarques
!
!$Mots-cles
!     coordonnées
!
!$Voir-Aussi
!     libephem
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use msp_gestion_erreur
  use ui_ioplus
  use ephem
  use cps_acces

  implicit none

! SVN Source File Id
  character(len=256) :: SVN_VER =  '$Id: ui_ephem.F90 69 2012-09-11 08:33:34Z ffsm $'


#include "formats.h"

! entrées/sorties
  real(KIND=PM_REEL) :: date
  integer :: code, nc, ni, nrep
  logical :: nv=.false.
  character(len=100), dimension(2)::fichier
  character(len=100) ::comcoord

! Constante : date à partir de laquelle un affichage spécial est requis
  real(kind=pm_reel), parameter :: DATE_LIMITE_AFFICHEE = 1.0e5_pm_reel
  character(len=6), parameter :: FORMAT_DATE_DEFAUT = "f15.9"
  integer, parameter :: NB_DIGIT_PARTIE_REELLE = 11

! 
  real(KIND=PM_REEL) :: tau, trep
  real(KIND=PM_REEL), dimension(6) :: coord!, coord2

! variables locales
  logical :: calcul=.false.
  logical :: nofichier=.true.
  logical :: calctau=.false.
  logical :: trepok=.false.
  character(len=80)::iers
  integer :: ier, lrep_fcf, nrep1, ierfin
  type(MSP_MESSAGE) :: messages
  character(len=100)::rep_fcf

  integer :: nn1
  integer :: nbfic !, nb_saut_tuc
  integer :: nbbull
  integer, dimension(:), pointer :: corpsci => NULL()
  type(EPH_BULLETIN), dimension(:), pointer :: tabull => NULL()
  character(len=6) :: format_date


  ! initialisation
  call cps_init_utilisateur()

  ! Initialisation du tableau des bulletins et du tableau des codes des corps d'intérêt
  allocate (corpsci(4))
  allocate (tabull(4))

  ! repertroire ou trouver le fichier tai-tuc.dat
  ier = AMv_rc_get("fcf_gslib", "compas", "", "" , rep_fcf,lrep_fcf)

  ! Lecture des paramètres (en ligne ou sur fichier)

  call ui_test_arguments ("-h", "-code", ierfin)

      ! lecture effective
      if(ierfin.eq.0) then
         ! Lecture des arguments : -code, -nv, -date
         call ui_lire_arguments(ier, code=code, nv=nv, date=date)
         if(ier.ge.0) calcul=.true.

         ! Lecture de l'argument : -nrep (numéro du repère de sortie)
         call ui_test_arguments ("-nrep", "", ier)
         if(ier.eq.1) call ui_lire_arguments(ier, nrep=nrep)
         
         ! Lecture de l'argument : -trep (date du définition du repère)
         call ui_test_arguments ("-trep", "", ier)
         if(ier.eq.1) then
            call ui_lire_arguments(ier, trep=trep)
            trepok=.true.
         endif
            
         ! Lecture de l'argument : -fic -cc et -ci
         call ui_test_arguments ("-fic", "", ier)
         if(ier.eq.1) nofichier=.false.

         call ui_lire_fic2(fichier, nbfic, nc, ni)
         if(MSP_erreur) calcul=.false.

         ! tau demande ou non
         call ui_test_arguments ("-tau", "", ier)
         if(ier.eq.1) calctau=.true.

      else
         call ui_ecrire_help("ui_ephem")
         goto 999
      endif

! corps du programme

    if(.not.calcul) goto 999

    ! Initialisations preliminaires
    if (code==11) then
       call eph_kep_lirekep(ni, nc, tabull, corpsci, nbbull)
       call eph_initposcor(code, tabull=tabull, codecorps=corpsci, nbbull=nbbull)
    else
       if(nofichier) then 
          call eph_initposcor(code)
       else 
          call eph_initposcor(code, nbfic, fichiers=fichier)
       endif
    endif
    
    ! le tau n'a pas de sens hors methodes NAIF
    if(code/10.ne.2) calctau = .false.
    
    ! erreur sur l'init
    if (MSP_erreur) goto 999
    
    ! appel effectif
    if(calctau) then
       call eph_poscor(code, date, nc, ni, coord, tau=tau)
    else
       call eph_poscor(code, date, nc, ni, coord)
    endif
    
    ! erreur dans l'appel
    if (MSP_erreur) goto 999
    
    ! repere
    comcoord="Coordonnées EME2000 "
    ! EME2000 = 223 (notation AMLIB)
    nrep1=223
    if((code/10 == 4).or.(code/10 == 5)) then
       comcoord="Coordonnées dans le repère du fichier Tchebychev"
    endif
    
    ! Changement de repere
    if(nrep>0) then
       call ui_changer_repere(nrep, code, date, coord, comcoord, trep, &
            trepok, rep_fcf)

       write(comcoord,1001) "Coordonnées dans le repère ", nrep
       if (trepok) write(comcoord,1030) "Coordonnées dans le repère ", nrep, " à ", trep
    else
       nrep=nrep1
    endif
    
    ! fermeture
    call eph_closeposcor()
    if (MSP_erreur) goto 999

    ! evaluation : est-ce que la date va rentrer dans les 15 caractères dispo 
    ! dont un pour le point et 9 décimales
    if ( date < DATE_LIMITE_AFFICHEE ) then
       format_date = FORMAT_DATE_DEFAUT
    else
       nn1=INT(LOG10(date)) + NB_DIGIT_PARTIE_REELLE
       write(format_date,'(a,i2,a)') 'f', nn1, '.9'
    endif
            

    ! sortie écran
    if(calctau) then
	  nn1=len_trim(comcoord)
	  write(iers,'(a5,i2,a5)') "(a9,a", nn1, ",a35)"
          if(.not.nv) write(*,iers) "# Date ", trim(comcoord), &
            " (x y z vx vy vz) ~km et ~km/s, tau"

          write(*,fmt='(('//trim(format_date) //'),3(f16.3),4(f16.6))') date, coord(1),coord(2),coord(3),&
               coord(4),coord(5), coord(6), tau
       else
	  nn1=len_trim(comcoord)
	  write(iers,'(a5,i2,a5)') "(a9,a", nn1, ",a30)"
          if(.not.nv) write(*,iers) "# Date ", trim(comcoord), &
            " (x y z vx vy vz) ~km et ~km/s"
          write(*,fmt='(('//trim(format_date) //'),3(f16.3),3(f16.6))') date, coord(1),coord(2),coord(3),&
               coord(4),coord(5), coord(6)
    endif
       

999 continue
    if (MSP_PROBLEME) then
       call MSP_recuperer_message (message=messages,nb_mes=MSP_tous_messages)
       call MSP_afficher_message (message=messages,unit=0)
    endif

end program ui_ephem

subroutine ui_lire_fic2(fic, nbfic, nc, ni)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ui_lire_fic2
!
!$Resume
!  Lecture des paramètres méthodes
!
!$Description
!  Lecture des arguements de ui_ephem
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ui_lire_fic2(fic, nbfic, nc, ni)
!.    character(len=*), dimension(2) :: fic
!.    integer :: nbfic, nc, ni
!
!$Arguments
!>S     fic    :<LEN=*,DIM=(2)>   Tableau des fichiers lus 
!>S     nbfic  :<integer>         Nombre de fichiers lus (1 ou 2)
!>S     nc     :<integer>         Numéro du corps central  
!>S     ni     :<integer>         Numéro du corps d'interet  
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use msp_gestion_erreur

  implicit none

  character(len=*), dimension(2), intent(out) ::fic
  integer, intent(out) :: nbfic, nc, ni

  integer::nb_arg,pos
  character(len=100)::argument, argument2
  logical :: ficok=.false.
  logical :: ccok=.false.
  logical :: ciok=.false.
    
  ! Fonctions
  integer::iargc

  ! Initialisations
  nbfic = 1
  nb_arg = iargc()
  
  do pos=1,nb_arg
     call getarg(pos,argument)
     
     if(trim(argument).eq."-fic") then 
        if ((pos+1).le.nb_arg) then 
           call getarg(pos+1,argument2)
           fic(1)=trim(argument2)
           nbfic = 1
           ficok=.true.

        endif
        if ((pos+2).le.nb_arg) then
           call getarg(pos+2,argument2)
           fic(2)=trim(argument2)
           if (fic(2)(1:1)=="-") then
              ! Cas d'une option (-cc, -ci, -date, ...)
              nbfic=1
           else
              nbfic = 2
           endif
        endif
     endif
     if(trim(argument).eq."-cc") then 
        if ((pos+1).le.nb_arg) then 
           call getarg(pos+1,argument2)
           read(trim(argument2),*) nc
           ccok=.true.
        endif
     endif
     if(trim(argument).eq."-ci") then 
        if ((pos+1).le.nb_arg) then 
           call getarg(pos+1,argument2)
           read(trim(argument2),*) ni
           ciok=.true.
        endif
     endif
  enddo

  if (.not.ccok) call MSP_signaler_message(cle_mes="EPH_ERR_ARG", &
                      partie_variable="-cc", routine="ui_ephem")
  if (.not.ciok) call MSP_signaler_message(cle_mes="EPH_ERR_ARG", &
                      partie_variable="-ci", routine="ui_ephem")


end subroutine ui_lire_fic2
