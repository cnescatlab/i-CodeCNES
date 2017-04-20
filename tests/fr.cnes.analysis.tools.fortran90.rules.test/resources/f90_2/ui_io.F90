module ui_ioplus

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ui_ioplus
!
!$Resume
!  Fonction d'entrées / sorties
!
!$Description
!  Ce module regroupe les fonctions utilisées par les utilitaires COMPAS
!  qui ne peuvent pas etre dans COMPAS base (usage de la MSPRO).
!  Ce module complete le module ui_io de COMPAS base.
!
!$Auteur
!   Florence VIVARES (ATOS Orign)
!
!$Version
!  $Id: ui_io.F90 414 2013-03-11 08:08:38Z ffsm $
!
!$Historique
!  $Log: ui_io.F90,v $
!  Revision 414  2013/03/11 ffsm
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 1.16  2010/10/25 08:35:51  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.15  2010/05/25 10:29:04  jlrobin
!  VERSION::FA-ID:1355:25/05/2010:Manque des accents
!
!  Revision 1.14  2009/11/23 10:08:06  cmartel
!  AQ : Suppression de variables inutilisees
!
!  Revision 1.13  2009/10/01 14:24:52  cmartel
!  FA-ID 1277 : Ajout de routines d'extractions des infos des format JPL et IMCEE
!
!  Revision 1.12  2009/09/09 15:07:27  cmartel
!  FA-ID 1196 : Correction mineures sur les sorties des utilitaires
!  Revision 1.11  2009/09/01 09:09:12  cmartel
!  FA-ID 1315 : Correction de l'utilisation de la base locale
!  Revision 1.10  2009/03/24 10:19:07  cml
!  DM-ID 1159 : Correction du numero de DM de la mise en conf. precedente
!  Revision 1.9  2009/03/24 08:59:04  cml
!  DM-ID 1159 : Ajout d'initialisations de pointeurs manquantes
!  Revision 1.8  2009/03/23 14:03:21  cml
!  DM-ID 1159 : Correction de declaration des arguments
!  Revision 1.7  2008/12/22 10:11:15  cml
!  FA-ID 1119 : Correction de la taille de la chaine pour le path complet
!  Revision 1.6  2008/11/04 10:20:36  cml
!  AQ : Corrections mineures
!  Revision 1.5  2008/10/31 13:19:00  cml
!  FA-ID 1076 : Corrections mineurs dans les affichages des resultats des utilitaires
!  Revision 1.4  2008/04/29 08:09:05  vivaresf
!  COMPAS_UI, V2.4, AQ : suppression des variables inutilisées
!  Revision 1.3  2008/04/25 18:02:34  vivaresf
!  DM-ID 11 : intégration des sauts du TUC de COMPAS_BASE
!  Revision 1.2  2008/04/11 12:43:20  vivaresf
!  Version 2.4 AQ : correction des cartouches
!  Revision 1.1  2008/02/08 17:51:26  vivaresf
!  FA-ID 889, DM-ID 820 :
!  - réorganisation du code pour que tout soit dans le répertoire src
!  Revision 1.11  2007/07/05 17:14:33  vivaresf
!  FA-ID 664 : correction de la présentation
!  Revision 1.10  2006/10/23 12:56:41  vpg
!  DM-ID 566 : Cloture du FT (Amelioration de l ergonomie des IHM de COMPAS_UI V2-0)
!  Revision 1.9.2.1  2006/10/23 10:12:27  vpg
!  DM-ID 566 : Version initiale du FT (Amelioration de l ergonomie des IHM de COMPAS_UI V2-0)
!  Revision 1.9  2006/07/03 08:52:12  vpg
!  Prise en compte des fichiers Tchebychev dans une base locale
!  Revision 1.8  2006/06/16 17:32:07  vivaresf
!  Cartouches d'entete
!  Revision 1.7  2006/06/16 09:52:02  vivaresf
!  Suppression des codes inutilisés
!  Revision 1.6  2006/06/14 12:37:06  vivaresf
!  DM-ID 387 : mise au point des utilitaires
!  Revision 1.5  2006/05/31 13:08:40  vivaresf
!  COMPAS 2.0 : validation
!  Revision 1.4  2006/02/23 15:35:32  vpg
!  Modification de ui_lire_option() : prise en compte des options sans argument
!  Revision 1.3  2006/02/10 10:37:18  bouillaj
!  mise a jour cartouche
!  Revision 1.2  2006/02/09 16:34:30  vpg
!  Ajout de la routine ui_lire_options()
!  Revision 1.3  2006/01/30 09:04:52  bouillaj
!  Ajout d une fonction de lecture de parametre
!  Revision 1.2  2005/12/08 18:39:31  vivaresf
!  Cartouches et vérification des déclarations
!  Revision 1.1.1.1  2005/12/07 07:23:14  vivaresf
!  Refonte de COMPAS
!
!$FinHistorique
!
!$Usage
!  use ui_ioplus
!
!$Routines
!- ui_lire_arguments
!- ui_changer_repere
!- ui_listereperes
!- ui_listeplanetesat
!- ui_listecodemethode
!- ephui_jpl_info
!- ephui_vsop87_info
!- ephui_tchebmadinfo
!- ephui_tchebinfo
!- ephui_corps_in_fic
!
!$Fonctions
!
!$Module
!#V
!- msp_gestion_erreur
!- ephem
!- mspro
!- cps_acces
!- ui_io
!- MSLIB
!- eph_jpl_ephemeris
!#
!
!$Voir-Aussi
!.  ui_lire_arguments ui_changer_repere ui_listereperes ui_listeplanetesat ui_listecodemethode
!.  ephui_jpl_info ephui_vsop87_info ephui_tchebmadinfo ephui_tchebinfo ephui_corps_in_fic
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
  use msp_gestion_erreur
  use ephem
  use mspro
  use cps_acces
  use ui_io
  
  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ui_io.F90 414 2013-03-11 08:08:38Z ffsm $'


contains
  
  subroutine ui_lire_arguments(ier, coord, mu,requa,apla,M,alt,fpa,hper,hap,long,BT,&
       BR,date,ecart,tvol,deltaB,vrot,refer, datesortie, nv, ncol,pas,npas,mini,&
       fin,fout, npla, nrep, trep, vinf, avk, sig, moy, ro, p, eps,nx,ny,nmax,&
       fic,code)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ui_lire_arguments
!
!$Resume
!   Lecture des arguments d'une commande
!
!$Description
!   Suivant les libellés demandés, lit les valeurs à l'écran
!
!$Auteur
!   Florence VIVARES (SchlumbergerSema)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ui_lire_arguments(ier, [coord], [mu],[requa],[apla],[m],[alt],[fpa],[hper],[hap],[long],[bt],&
!.           [br],[date],[ecart],[tvol],[deltab],[vrot],[refer], [datesortie], [nv], [ncol],[pas],[npas],[mini],&
!.           [fin],[fout], [npla], [nrep], [trep], [vinf], [avk], [sig], [moy], [ro], [p], [eps],[nx],[ny],[nmax],&
!.           [fic],[code])
!.    character(len=*) :: fin, fout, fic
!.    real(KIND=PM_REEL),dimension(6) :: coord
!.    real(KIND=PM_REEL),dimension(3) :: vinf, sig, moy
!.    real(KIND=PM_REEL) :: mu, requa, apla
!.    real(KIND=PM_REEL) :: M, alt, fpa, hper, hap, avk
!.    real(KIND=PM_REEL) :: long, BT, BR
!.    real(KIND=PM_REEL) :: date, datesortie, ecart, tvol, trep
!.    real(KIND=PM_REEL) :: deltaB, vrot, pas, mini
!.    real(KIND=PM_REEL) :: ro, p, eps
!.    integer :: refer, ncol, npas, npla, nrep, nx, ny,nmax, code
!.    logical :: nv
!.    integer :: ier
!
!$Arguments
!>S     ier         :<integer>           code d'erreur            
!>[S]   coord       :<PM_REEL,DIM=(6)>   
!>[S]   mu          :<PM_REEL>           
!>[S]   requa       :<PM_REEL>           
!>[S]   apla        :<PM_REEL>           
!>[S]   M           :<PM_REEL>           
!>[S]   alt         :<PM_REEL>           
!>[S]   fpa         :<PM_REEL>           
!>[S]   hper        :<PM_REEL>           
!>[S]   hap         :<PM_REEL>           
!>[S]   long        :<PM_REEL>           
!>[S]   BT          :<PM_REEL>           
!>[S]   BR          :<PM_REEL>           
!>[S]   date        :<PM_REEL>           
!>[S]   ecart       :<PM_REEL>           
!>[S]   tvol        :<PM_REEL>           
!>[S]   deltaB      :<PM_REEL>           
!>[S]   vrot        :<PM_REEL>           
!>[S]   refer       :<integer>           
!>[S]   datesortie  :<PM_REEL>           
!>[E/S] nv          :<logical>           
!>[S]   ncol        :<integer>           
!>[S]   pas         :<PM_REEL>           
!>[S]   npas        :<integer>           
!>[S]   mini        :<PM_REEL>           
!>[S]   fin         :<LEN=*>             
!>[S]   fout        :<LEN=*>             
!>[S]   npla        :<integer>           
!>[S]   nrep        :<integer>           
!>[S]   trep        :<PM_REEL>           
!>[S]   vinf        :<PM_REEL,DIM=(3)>   
!>[S]   avk         :<PM_REEL>           
!>[S]   sig         :<PM_REEL,DIM=(3)>   
!>[S]   moy         :<PM_REEL,DIM=(3)>   
!>[S]   ro          :<PM_REEL>           
!>[S]   p           :<PM_REEL>           
!>[S]   eps         :<PM_REEL>           
!>[S]   nx          :<integer>           
!>[S]   ny          :<integer>           
!>[S]   nmax        :<integer>           
!>[S]   fic         :<LEN=*>             
!>[S]   code        :<integer>           
!
!$Routines
!- getarg
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

!------------------------------
!	ARGUMENTS
!------------------------------

    character(len=*),intent(out),optional :: fin, fout, fic
    real(KIND=PM_REEL),intent(out),dimension(6),optional :: coord
    real(KIND=PM_REEL),intent(out),dimension(3),optional :: vinf, sig, moy
    real(KIND=PM_REEL),intent(out),optional :: mu, requa, apla
    real(KIND=PM_REEL),intent(out),optional :: M, alt, fpa, hper, hap, avk
    real(KIND=PM_REEL),intent(out),optional :: long, BT, BR
    real(KIND=PM_REEL),intent(out),optional :: date, datesortie, ecart, tvol, trep
    real(KIND=PM_REEL),intent(out),optional :: deltaB, vrot, pas, mini
    real(KIND=PM_REEL),intent(out),optional :: ro, p, eps
    integer, intent(out),optional::refer, ncol, npas, npla, nrep, nx, ny,nmax, code
    logical,optional :: nv

    ! code d'erreur
    integer, intent(out) :: ier

!-------------------------------
!	VARIABLES LOCALES
!-------------------------------

    character(len=100)::argument
    character(len=1)::arg1, arg2
    integer::nb_arg,i,iargc,pos
    integer::noptions = 0
    character(len=100),dimension(50)::l_opt,arguments
    integer,dimension(50)::p_opt

!-------------------------------
!	INITIALISATION
!-------------------------------

    ier=0
 
!//----------------------------------
!//       CORPS DU SOUS-PROGRAMME
!//----------------------------------

    ! Recuperation du nombre d'arguments
    nb_arg = iargc()

    ! tous les arguments sont dans arguments, les options et leur positions 
    ! dans loptions et p_opt

    do i=1,nb_arg
       call getarg(i,argument)
       arguments(i)=trim(argument)
       arg1=argument(1:1)
       arg2=argument(2:2)
      if (arg1.eq."-".and.nonum(arg2)) then
          l_opt(noptions+1)=trim(argument)
          p_opt(noptions+1)=i
          noptions=noptions+1
       endif
    enddo

!
! tester tous les arguments demandes sinon erreur
!
 
    ! Dates
    if (present(date)) then 
       pos=ui_exist("-date", l_opt, p_opt)
       if (pos.gt.0) then 
          read(arguments(pos+1),*) date
       else
          ier=-1
          goto 999
       endif
    endif

    ! date du repere (non obligatoire)
    if (present(trep)) then 
       pos=ui_exist("-trep", l_opt, p_opt)
       if(pos.gt.0) read(arguments(pos+1),*) trep
    endif

    ! option "no vrebose"
    if (present(nv)) then 
       pos=ui_exist("-nv", l_opt, p_opt)
       nv=(pos.gt.0)
    endif

    if (present(code)) then 
       pos=ui_exist("-code", l_opt, p_opt)
       if(pos.gt.0) read(arguments(pos+1),*) code
    endif
    if (present(nrep)) then 
       pos=ui_exist("-nrep", l_opt, p_opt)
       if(pos.gt.0) read(arguments(pos+1),*) nrep
       if (pos.le.0) then
          ier=-1
          goto 999
       endif
    endif

    ier=1
    
! fin de la routine

999 continue
    return

  end subroutine ui_lire_arguments

! ui_test_arguments : dans ui_io
! ui_ecrire_help : dans ui_io

! ui_lire_options : dans ui_io
! eph_kep_lirekep : dans ui_io

  subroutine ui_changer_repere(nrep, code, date, coord, comcoord, trep, trepok, rep_fcf )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ui_changer_repere
!
!$Resume
!  Routine de changement de repère pour l'utilitaire ui_ephem
!
!$Description
!  Routine de changement de repère pour l'utilitaire ui_ephem
!
!$Auteur
!  Julien Bouillant (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ui_changer_repere(nrep, code, date, coord, comcoord, [trep], [trepok], rep_fcf )
!.    integer :: nrep, code
!.    real(kind=PM_REEL) :: date
!.    real(kind=PM_REEL) :: trep
!.    real(kind=PM_REEL), dimension(6) :: coord
!.    logical :: trepok
!.    character(len=100) :: rep_fcf
!.    character(len=100) :: comcoord
!
!$Arguments
!>E     nrep      :<integer>           Code  repère
!>E     code      :<integer>           code méthode/théorie
!>E     date      :<PM_REEL>           date de calcul B1950 [~jul]
!>E/S   coord     :<PM_REEL,DIM=(6)>   Coordonnées avant et après le changement de repère
!>S     comcoord  :<LEN=100>           Commentaire pour les coordonnées
!>[E/S] trep      :<PM_REEL>           date de définition du repère (TE). Si trep n'est pas
!                                      présent, date est prise comme date de réference
!                                      pour le repère.
!>[E/S] trepok    :<logical>           
!>E     rep_fcf   :<LEN=100>           
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use MSLIB
    implicit none


    ! Arguments
    integer, intent(IN) :: nrep, code
    real(kind=PM_REEL), intent(IN) :: date
    real(kind=PM_REEL), optional, intent(INOUT) :: trep
    real(kind=PM_REEL), dimension(6), intent(INOUT) :: coord
    logical, optional, intent(INOUT) :: trepok
    character(len=100), intent(IN) :: rep_fcf
    character(len=100), intent(OUT) :: comcoord

    ! Variables locales
    integer :: plarep1, plarep2, cledate1, cledate2, modprec2
    integer :: typerep1, typerep2
    character(len=80) :: iers
    integer :: erreur
    type(tm_jour_sec) :: datereps, date_tmp
    type(tm_code_retour) :: code_retour
    real(kind=PM_REEL), dimension(6) :: coord2
    type(tm_pole_uv) :: pole
    integer ::  delta_tai_tuc
    real(KIND=PM_REEL)  ::  ecart_tu1,ecart_tai

    logical :: ok

    ! changement de repère
    if((code/10 == 4).or.(code/10 == 5)) then 
       write(iers,*) "Changement de repère non implémenté pour méthode ", code
       call MSP_signaler_message(message=iers, routine="ui_changer_repere", &
            code="ATT_OPTIONINC", type=MSP_enum_warning)
    else
       ! EME2000
       typerep1 = PM_equa_moy
       cledate1 = PM_1janvier2000_12h00
       plarep1  = EPH_Terre
       
          
       ! description du repere souhaite
       typerep2 = nrep/100
       cledate2 = (nrep - typerep2*100)/10
       plarep2  = (nrep - typerep2*100 - cledate2*10)
       modprec2 = pm_lieske_wahr_aoki
       
       erreur=0
       if(plarep2 < 1 .or. plarep2 > 9) erreur=1
       if(cledate2 < 0 .or. plarep2 > 4) erreur=1
       select case (typerep2)
       case(1)  ! Gamma/Equatorial Vrai 
          if(plarep2 .ne. 3) erreur=1
          typerep2=pm_equa_vrai
             if(cledate2 == 0)  erreur=1
             if(cledate2 == 1) cledate2=pm_1janvier1950_00h00
             if(cledate2 == 2) cledate2=pm_1janvier2000_12h00
             if(cledate2 == 3) cledate2=pm_autre_date
             plarep2=399
       case(2)  ! Gamma/Equatorial Moyen
          if (plarep2 .ne. 3) erreur=1
          typerep2=pm_equa_moy
             if(cledate2 == 0)  erreur=1
             if(cledate2 == 1) cledate2=pm_1janvier1950_00h00
             if(cledate2 == 2) cledate2=pm_1janvier2000_12h00
             if(cledate2 == 3) cledate2=pm_autre_date
             plarep2=399
       case(3)  ! Ecliptique 2000 uniquement
          if(cledate2 .ne. 2) erreur=1
          cledate2=pm_1janvier2000_12h00
          typerep2=pm_ecli_moy
             plarep2=399
       case(6)  ! Veis de la date (pas de mauvaise definition)
          cledate2=pm_autre_date
          if(cledate2 .eq. 1) then
             cledate2=pm_1janvier1950_00h00
             trep=0.
             trepok=.true.
          endif
          typerep2=pm_veis
             if(plarep2 == 0) erreur=0
             plarep2=399
       case(7)  ! Planetocentrique inertiel de la date
          if(cledate2 .ne. 3) erreur=1
          cledate2=pm_autre_date
          plarep2=plarep2*100+99
          typerep2=pm_planeto_ref_iner
       case(8)  ! PQ / Equatorial UAI de la date
          if(plarep2 == 3) erreur=1
          plarep2=plarep2*100+99
          typerep2=pm_equa_uai
          cledate2=pm_autre_date
       case default
          erreur=1
       end select

       if(erreur == 1) then
          write(iers,*) "Code repère inconnu ou non implemente", nrep
          call MSP_signaler_message(message=iers, routine="ui_changer_repere", &
               code="ATT_OPTIONINC", type=MSP_enum_erreur)
       else
          
          ! Dans le cas de la définition d'un repère de sortie avec donnée 
          ! d'une date de référence, la date de référence est celle donnée en 
          ! argument sinon c'est la date courante.
          if (trepok) then
             call md_jourfrac_joursec(trep,datereps,code_retour)
          else
             call md_jourfrac_joursec(date,datereps,code_retour)
          endif
          
          ! Delta entre les echelles de temps
          call cps_get_sautTAITUC(datereps,delta_tai_tuc)
          if (MSP_gen_messages("ui_changer_repere")) return

          ! écart TAI - TE
          call md_ech_temps(pm_TE,datereps,pm_TAI,ecart_tai,date_tmp,&
               code_retour=code_retour)
          call MSP_signaler_message (ier_mslib=code_retour)
          ok = MSP_gen_messages("ui_changer_repere - Appel md_ech_temps TAI ")

          ecart_tu1 = ecart_tai - real(delta_tai_tuc,kind=PM_REEL) ! écart TU1 - TE 
          
          ! Changement de repère 
          ! les repères station ne sont pas traités
          if(plarep2 /= 399) modprec2 = pm_uai2000
          call mx_rep(typerep1, plarep1,cledate1,pm_lieske_wahr_aoki,&
               coord(1:3),coord(4:6), &
               typerep2,plarep2,cledate2,modprec2, &
               coord2(1:3),coord2(4:6), code_retour, &
               val_date_out= datereps,&
               delta_tai_out=ecart_tai, delta_tu1_out=ecart_tu1, pole_out=pole, &
               long_ref_out=0._PM_REEL)
          
          if (code_retour%valeur<0) call MSP_signaler_message (ier_mslib=code_retour)
          if (MSP_gen_messages("ui_changer_repere - Appel mx_rep ")) then
             write(*,*) code_retour%valeur
             write(*,*) trim(code_retour%message)
          else
             coord(1:6)=coord2(1:6)
             write(comcoord, fmt=*) "Coordonnées dans le repère ", nrep
          endif
       endif
    endif
  
  end subroutine ui_changer_repere

  subroutine ui_listereperes()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ui_listereperes
!
!$Resume
!  Donne la liste des repères pour l'utilitaire ui_epheminfo
!
!$Description
!  Donne la liste des repères pour l'utilitaire ui_epheminfo
!
!$Auteur
!  Julien Bouillant (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ui_listereperes()
!
!$Arguments
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

    write(*,*) ""
     write(*,*) "Libellés des principaux repères disponibles      =  code "
     write(*,*) ""
     write(*,*) "Gamma vrai 1950 (équatorial vrai terrien 1950)   =  ",EPHREP_gv1950
     write(*,*) "Gamma vrai 2000 (équatorial vrai terrien 2000)   =  ",EPHREP_gv2000
     write(*,*) "Gamma vrai de la date (*)                        =  ",EPHREP_gvd
     write(*,*) "Gamma moyen terrien 1950             / EME1950   =  ",EPHREP_gm1950
     write(*,*) "Gamma moyen terrien 2000             / EME2000   =  ",EPHREP_gm2000
     write(*,*) "Gamma moyen terrien de la date (*)               =  ",EPHREP_gmd
     write(*,*) "Ecliptique 2000                                  =  ",EPHREP_ecl2000
     write(*,*) "Veis de la date  (*)                             =  ",EPHREP_veis
     write(*,*) "Planétocentrique inertiel Terre de la date (*)   =  ",EPHREP_pld
     write(*,*) "Planétocentrique inertiel Mars (*)   / MMEQEQ    =  ",EPHREP_mar_pld
     write(*,*) "Planétocentrique inertiel autres planètes (*)    =  ",EPHREP_mer_pld, " ... ", EPHREP_plu_pld
     write(*,*) "PQ martien (équatorial UAI)          / MMEIAU    =  ",EPHREP_mar_PQ
     write(*,*) "PQ autres planètes   (équatorial UAI)            =  ",EPHREP_mer_PQ,  " ... ", EPHREP_plu_PQ
     write(*,*)
     write(*,*) "(*) signifie qu'une date de définition du repère est nécessaire."
     write(*,*) "Cette date peut être donnée par l'option -trep, sinon c'est la "
     write(*,*) "date de calcul qui est prise comme date de définition du repère."

  end subroutine ui_listereperes

  subroutine ui_listeplanetesat()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ui_listeplanetesat
!
!$Resume
!  Donne la liste des planètes et satellites pour l'utilitaire ui_epheminfo
!
!$Description
!  Donne la liste des planètes et satellites pour l'utilitaire ui_epheminfo
!
!$Auteur
!  Julien Bouillant (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ui_listeplanetesat()
!
!$Arguments
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

     write(*,*) "Code barycentre système solaire : ", eph_bary_soleil
     write(*,*) "Codes barycentre système planétaire : 1 à 9"
     write(*,*) " Baryc. Terre = ", eph_bary_terre,  &
          "   Baryc. Mars = ", eph_bary_mars, &
          " ... Baryc. Pluton = ", eph_bary_pluton
     write(*,*) " "
     write(*,*) "Code Soleil :", eph_soleil
     write(*,*) " "
     write(*,*) 'Code planète : code barycentre*100+"99"'
     write(*,*) " Mercure= ", eph_mercure, "   Venus = ", eph_venus, &
          " Terre  = ", eph_terre, "   Mars = ", eph_mars, " ..."
     write(*,*) " "
     write(*,*) "Code satellite : code barycentre*100+numéro d'ordre"
     write(*,*) " Lune   = ", eph_lune
     write(*,*) " Phobos = ", eph_phobos, "  Deimos = ", eph_deimos
     write(*,*) " Io = ", eph_io, "  Europa = ", eph_europa, &
          " Ganymede = ", eph_ganymede, "  Callisto = ", eph_callisto, " ..."
     write(*,*) " "
     write(*,*) "Code astéroïde : 2000000+numéro"
     write(*,*) " Ceres  = ", eph_ceres, "     Pallas  = ", eph_pallas, &
          "     Junon =",  eph_junon
     write(*,*) " Vesta  = ", eph_vesta, "     Chaldaea= ", eph_chaldaea, &
          "   Orpheus =",  eph_orpheus
     write(*,*) " "
     write(*,*) "Code comète  : 1000000+numéro"
     write(*,*) " Wirtanen =",eph_wirtanen, "   Hale-Bopp= ",eph_halebopp, &
          "   Tempel-one = ",eph_tempel1
     write(*,*)

  end subroutine ui_listeplanetesat
      
  subroutine ui_listecodemethode(testfic,listefic, code, fichier )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ui_listecodemethode
!
!$Resume
!  Donne la liste des méthodes et théorie en fonction des besoins de 
!  l'utilitaire ui_epheminfo
!
!$Description
!  Donne la liste des méthodes et théorie en fonction des besoins de 
!  l'utilitaire ui_epheminfo
!
!$Auteur
!  Julien Bouillant (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ui_listecodemethode(testfic,listefic, code, fichier )
!.    logical :: testfic, listefic
!.    integer :: code
!.    character(len=ephv_lgmax) :: fichier
!
!$Arguments
!>E/S   testfic   :<logical>          Fichier a decrire
!>E/S   listefic  :<logical>          liste des fichiers a fournir
!>E     code      :<integer>          Code méthode/théorie
!>E     fichier   :<LEN=ephv_lgmax>   Fichier utilisé dans méthode ou théorie.
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
    ! Arguments
    
    logical, intent(INOUT) :: testfic, listefic
    integer, intent(IN) :: code
    character(len=ephv_lgmax), intent(IN) :: fichier
 
    ! Variables locales
    
    character(len=20) :: ext
    character(len=50) :: mm, th
    character(len=CPS_MAXLG) :: rep
    character(len=ephv_lgmax) :: fd, cmd,cmdbrief,cmdcomm
    logical :: present=.false.
    integer ::ncodes, ier, lcmd
    logical :: base_locale

    ! Initialisations
    ier = AMv_rc_get("DIRNAIF","compas","",&
         "/usr/local_ms/NAIF/toolkit/exe",cmdbrief,lcmd)
    
    cmdcomm = trim(cmdbrief)//"/commnt"
    cmdbrief = trim(cmdbrief)//"/brief"


    ! Affichage des infos generales
    mm=""
    call eph_infogetLocal(code, base_locale, repertoire=rep, fichierd=fd, &
         methode=mm, theorie=th)
    if (.not.base_locale) then
       call eph_infoget(code, repertoire=rep, fichierd=fd, &
            methode=mm, theorie=th)
    end if

    if(len_trim(mm).eq.0) then
       listefic=.false.
       write(*,*) "Code méthode ", code, " inconnu"
       goto 999
    endif
    write(*,*) "Code     : ", code
    write(*,*) "Méthode  : ", trim(mm)
    write(*,*) "Théorie  : ", trim(th)
    if(len_trim(fd).gt.0) write(*,*) "Fichier par défaut : ", trim(fd)
    if(len_trim(rep).gt.0) write(*,*) "Répertoire de sources : ", trim(rep)
    call flush(6)

    ! Descriptif d'un fichier
    if (testfic) then
       fd=trim(rep) // trim(fichier)
       inquire(file=fd, exist=present)
       if (.not.present) then
          write(*,*) "Fichier introuvable pour cette méthode / théorie : " // &
               trim(fichier)
       endif
       ncodes = code / 10
       ! methode NAIF
       if (ncodes.eq.2) then 
          cmd = trim(cmdcomm)//" -r "//trim(fd) // " | /bin/grep Planetary "
           call flush(6)
          call system(trim(cmd))
          write(*,*) " "
          cmd = trim(cmdbrief)// " "// trim(fd) // " | /bin/tail +6 "
           call flush(6)
          call system(trim(cmd))
       endif
       ! methode BDL-VSOP82
       if (code.eq.30) then 
          if (trim(fichier).eq."COEFTCHE.DAT") then
             write(*,'(100a)') "Méthode BDL-VSOP82, fichier " // trim(fd)
             write(*,*) "Corps disponibles : "
             write(*,*) " Mercure (199), Venus (299), Mars (499), Terre (399), Jupiter (599)"
             write(*,*) " Saturne (699), Uranus (799), Neptune (899), Pluton (999)"
             write(*,*) " Soleil (10), Lune (301), Baryc.T-L (3)" 
             write(*,*) " Ceres (2000001), Pallas (2000002), Junon (2000003), Vesta (2000004)"
             write(*,*) "Période couverte :"
             write(*,*) " 04/01/1900 au 01/01/2050"
          else
             write(*,*) "Description non disponible pour ce fichier"
          endif
       endif
       ! methode EPROC
       if (ncodes.eq.3.and.code.ne.30) then 
          write(*,*) "Méthode EPROC, description non disponible en ligne"
       endif
       ! methode Tchebytchev
       if (ncodes.eq.4) then 
          call ephui_tchebinfo(fd)
       endif
       ! methode Tchebytchev MADONA
       if (ncodes.eq.5) then 
          call ephui_tchebmadinfo(fd)
        endif
       ! methode JPL
       if (ncodes.eq.6) then 
          call ephui_jpl_info(fd)
        endif
       ! methode IMCEE/VSOP87
       if (ncodes.eq.7) then 
          call ephui_vsop87_info(fichier)
        endif
     endif
     
     if (listefic) then
        if(len_trim(rep).gt.0) then 
           write(*,*) "Fichiers disponibles :"
           ncodes = code / 10
           ext= ""
           if (ncodes.eq.2) ext=" | /bin/grep bsp"
           fd=trim(rep)
           cmd = " /bin/ls -1 " // trim(fd) // ext
           call flush(6)
           call system(trim(cmd))
        else
           write(*,*) "Méthode sans fichiers sources"
        endif
     endif

999 continue

   end subroutine ui_listecodemethode


subroutine ephui_jpl_info(fd) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ephui_jpl_info
!
!$Resume
!  Extraction des informations d'un fichier au format JPL
!
!$Description
!  Extraction des informations d'un fichier au format JPL
!
!$Auteur
!  Cédric MARTEL (Atos Origin)
!
!$Acces
!  PRIVE
!
!$Usage
!  call ephui_jpl_info(fd) 
!.    character(LEN=ephv_lgmax) :: fd
!
!$Arguments
!>E     fd  :<LEN=ephv_lgmax>   Nom du fichier
!
!$Common
!
!$Routines
!- eph_util_ficunit90
!- MSP_signaler_message
!- eph_initjpl
!- eph_jpl_infos
!- cps_codenom
!- eph_closejpl
!
!$Include
!
!$Module
!#V
!- eph_jpl_ephemeris
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use eph_jpl_ephemeris

   implicit none

   ! Arguments
   character(LEN=ephv_lgmax),intent(in) :: fd

   ! Constante
   integer, parameter :: NB_CORPS_MAX = 1000

   ! Variables locales
   integer :: nb_corps = 0
   integer :: ii, unite_logique, ier_appel
   integer, dimension(NB_CORPS_MAX) :: liste_corps
   real(kind=pm_reel) :: date_debut = 0.0_pm_reel
   real(kind=pm_reel) :: date_fin = 0.0_pm_reel
   character(LEN=CPS_MAXLG) :: buff

   ! Recherche d'une unité logique disponible
   call eph_util_ficunit90(unite_logique,ier_appel)
   if (ier_appel.lt.0) then
      call MSP_signaler_message (cle_mes="EPH_ERR_SYST", &
         partie_variable="reservation d'un lfn impossible", &
         routine="eph_initposcor" )
      return
   endif

   ! Init de la méthode avec le fichier
   call eph_initjpl(unite_logique, fd)

   ! Extraction des données intéressantes du fichier JPL
   call eph_jpl_infos(nb_corps, liste_corps, date_debut, date_fin)
   
   ! Affichage des 3 lignes d'informations
   write(*,*) "Date de début (en jj1950) : ", date_debut
   write(*,*) "Date de fin   (en jj1950) : ", date_fin

   if (nb_corps > 0 ) then
      ! Traduction des noms des corps et affichage successif
      write(*,*) "Ce fichier contient les éphémérides des", nb_corps, " corps suivants : "
      do ii = 1, nb_corps
         call cps_codenom(liste_corps(ii), buff, lang="fr")
         write(*,*) "  ", trim(buff), "(", liste_corps(ii), ")"
      enddo
   else
      write(*,*) 'Aucun corps'
   endif
 
   ! Fermeture
   call eph_closejpl()

end subroutine ephui_jpl_info

subroutine ephui_vsop87_info(fd)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ephui_vsop87_info
!
!$Resume
!  Extraction des informations d'un fichier au format IMCEE/VSOP87
!
!$Description
!  Extraction des informations d'un fichier au format IMCEE/VSOP87
!
!$Auteur
!  Cédric MARTEL (Atos Origin)
!
!$Acces
!  PRIVE
!
!$Usage
!  call ephui_vsop87_info(fd)
!.    character(LEN=ephv_lgmax) :: fd
!
!$Arguments
!>E     fd  :<LEN=ephv_lgmax>   Nom du fichier
!
!$Common
!
!$Routines
!- eph_vsop87_infos
!- cps_codenom
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

   ! Entrée
   character(LEN=ephv_lgmax),intent(in) :: fd

   ! Variables locales
   integer :: code_corps
   character(LEN=CPS_MAXLG) :: buff

   ! Affichage d'un message d'info pour les dates
   write(*,*) "Cette méthode n'a pas de limite de validité sur la date en entrée."
   
   ! Extraction des données intéressantes de la méthode
   call eph_vsop87_infos(fd, code_corps)
   
   if ( .not. MSP_ERREUR ) then
      ! Traduction des noms des corps et affichage successif
      call cps_codenom(code_corps, buff, lang="fr")
      write(*,*) "Ce fichier contient les éphémérides du corps : ", &
           trim(buff), "(", code_corps, ")"      
   else
      write(*,*) 'Aucun corps'
   endif
 

end subroutine ephui_vsop87_info

subroutine ephui_tchebmadinfo(fd)
!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  ephui_tchebmadinfo
!
!$Resume
!   Extraction des infos d'entête d'un fichier Tchebytchev MADONA
!
!$Description
!   Extraction des infos d'entête d'un fichier Tchebytchev MADONA
!
!$Auteur
!   Florence VIVARES (SchlumbergerSema)
!
!$Mots-cles
!
!$Usage
!  call ephui_tchebmadinfo(fd)
!.    character(LEN=ephv_lgmax) :: fd
!
!$Arguments
!>E     fd  :<LEN=ephv_lgmax>   nom du fichier Tchebychev MADONA
!
!$Acces
!  PUBLIC
!
!$Remarques
!
!$Voir-Aussi
!
!$<>
!***********************************************************************
      implicit none 

! Arguments

    character(LEN=ephv_lgmax),intent(in) :: fd

! Variables locales
    integer ::acces, ier
    integer :: nn, ipos, degre
    character(len=ephv_lgmax) :: tmp1, tmp2, tmp3
    real(kind=PM_REEL) :: dd

! Code
    acces = acc_load(trim(fd))
    write(*,*)
    write(*,'(100a)') " Fichier : "//trim(fd)
    ier=acc_gets(acces,"Corps_central", tmp1)
    write(*,*) "Corps central : "//trim(tmp1)
    ier=acc_gets(acces,"Repere", tmp1)
    write(*,*) "Repère : "//trim(tmp1)
    nn=acc_get_dim(acces,"Planetes")

    ier=acc_select(acces,"Planetes",ACC_TABL)
    tmp1=""
    tmp2=""
    tmp3=""
    do ipos=1,nn
       ier=acc_set_index(acces,ipos)
       ier=acc_gets(acces, ACC_INDEX, tmp1)
       if(ipos.eq.1) then
          tmp2=trim(tmp1)
       else
          tmp3=trim(tmp2)//", "//trim(tmp1)
          tmp2=trim(tmp3)
       endif
    enddo
    ier=acc_select_end(acces)
    write(*,*) nn, " planètes : ", trim(tmp2)
    
    ier=acc_geti(acces,"Degre_tchebycheff", degre)
    write(*,*) "Degré du polynôme de Tchebychev : ", degre
    
    ier=acc_getd(acces,"Pas_tchebycheff", dd, "j")
    write(*,'(a70,(f15.9))') "Durée de validité du polynôme de Tchebychev (jours fractionnaires) : ", dd
    
    ier=acc_gets(acces,"Echelle_temps", tmp1)
    write(*,*) "Echelle de temps : "//trim(tmp1)
    
    ier=acc_gets(acces,"Date_debut", tmp1)
    write(*,*) "Date de début du fichier : "//trim(tmp1)
    
    ier=acc_gets(acces,"Date_fin", tmp1)
    write(*,*) "Date de fin du fichier (à la durée de validité près) : "//trim(tmp1)
    ier=acc_close(acces)
    
  end subroutine ephui_tchebmadinfo

  subroutine ephui_tchebinfo(fd)
!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  ephui_tchebinfo
!
!$Resume
!   Extraction des infos d'entête d'un fichier Tchebytchev 
!
!$Description
!   Extraction des infos d'entête d'un fichier Tchebytchev 
!
!$Auteur
!   Florence VIVARES (SchlumbergerSema)
!
!$Mots-cles
!
!$Usage
!  call ephui_tchebinfo(fd)
!.    character(LEN=ephv_lgmax) :: fd
!
!$Arguments
!>E     fd  :<LEN=ephv_lgmax>   nom du fichier Tchebychev
!
!$Acces
!  PUBLIC
!
!$Remarques
!
!$Voir-Aussi
!
!$<>
!***********************************************************************
      implicit none 

! Arguments
    character(LEN=ephv_lgmax),intent(in) :: fd

! Variables locales
     character(len=80) :: tmp1
     integer :: fln=1, ipos, ier
 
! Code    
     call eph_util_ficunit90(fln,ier)
     if(ier.lt.0)then
        return
     endif
     open(fln, file=fd)

     ipos=1     
     read(fln,'(a80)') tmp1

     do while (ipos==1 .or. tmp1(1:1)/="*")
        write(*,*) trim(tmp1)
        ipos=ipos+1
        read(fln,'(a80)') tmp1
     enddo
     write(*,*) trim(tmp1)

    close(fln)
    
  end subroutine ephui_tchebinfo


  subroutine ephui_corps_in_fic(ephempath,codecorps,tabtheo,tabfic,affichage)
!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  ephui_corps_in_fic
!
!$Resume
!   Recherche d'un code corps dans tous les fichiers bsp sous les répertoires 
!   théories de la métode NAIF 
!
!$Description
!   Recherche d'un code corps dans tous les fichiers bsp sous les répertoires 
!   théories de la méthode NAIF. On utilise la commande NAIF 'brief' permettant
!   d'extraire les corps contenus dans le fichier et l'intervalle de validité 
!   du fichier. 
!
!$Auteur
!   Philippe Brémard (SchlumbergerSema)
!
!$Mots-cles
!
!$Usage
!  call ephui_corps_in_fic(ephempath,codecorps,[tabtheo],[tabfic],[affichage])
!.    character(len=*) :: ephempath
!.    character(len=*) :: codecorps
!.    character(len=ephv_lgmax),dimension(:), pointer :: tabtheo
!.    character(len=ephv_lgmax),dimension(:), pointer :: tabfic
!.    logical :: affichage
!
!$Arguments
!>E     ephempath  :<LEN=*>                            répertoire de la méthode sous laquelle la recherche est effectuée 
!>E     codecorps  :<LEN=*>                            code du corps recherché
!>[E/S] tabtheo    :<LEN=ephv_lgmax,DIM=(:),pointer>   tableau des théories rattachées au tableau des fichiers trouvés 
!>[E/S] tabfic     :<LEN=ephv_lgmax,DIM=(:),pointer>   tableau des fichiers contenant codecorps
!>[E]   affichage  :<logical>                          =.true. , affichage des résultats
!
!$Acces
!  PUBLIC
!
!$Remarques
!
!$Voir-Aussi
!
!$<>
!***********************************************************************
    implicit none

    ! Arguments
    character(len=*),intent(in) :: ephempath
    character(len=*),intent(in) :: codecorps
    character(len=ephv_lgmax),dimension(:), pointer, optional :: tabtheo
    character(len=ephv_lgmax),dimension(:), pointer, optional :: tabfic
    logical,intent(in),optional :: affichage
    
    ! Variables locales
    character(len=1024) :: cmd
    character(len=ephv_lgmax) :: ficrep,ficfic,ficres, ficok
    character(len=ephv_lgmax) :: rep, fic
    character(len=256) :: ligne, cmdbrief
    character(len=128) :: date
    
    integer :: lfntmp1, lfntmp2, lfntmp3, lcmd
    integer :: ll, ier, indic, iret
    
    ! Variables locales images des arguments optionnels
    logical :: laffichage
    integer, parameter :: eph_nbmaxfic = 200
    character(len=ephv_lgmax),dimension(:), pointer :: ltabtheo => NULL()
    character(len=ephv_lgmax),dimension(:), pointer :: ltabfic => NULL()
  
    ! Début code

    if(.not.ephv_acces_open) call eph_infoinit()
    ! Traitement des argument optionnels en entrée
    if (PRESENT(affichage)) then
       laffichage = affichage
    else
       laffichage = .false.
    endif
    
    ! commande brief de la toolkit de NAIF
    ier = AMv_rc_get("DIRNAIF","compas","",&
         "/usr/local_ms/NAIF/toolkit/exe",cmdbrief,lcmd)
    
    cmdbrief = trim(cmdbrief)//"/brief"
    
    ! Allocation des tableaux locaux
    if (associated(ltabtheo)) deallocate(ltabtheo)
    allocate(ltabtheo(eph_nbmaxfic))
    
    if (associated(ltabfic)) deallocate(ltabfic)
    allocate(ltabfic(eph_nbmaxfic))
    
    ! Initialisation du compteur de fichiers trouvés
    indic = 0
    
    ! Création de fichiers temporaires
    iret = AMv_fic_maketmp("uiepheminfo","tmp1",ficrep)
    iret = AMv_fic_maketmp("uiepheminfo","tmp2",ficfic)
    iret = AMv_fic_maketmp("uiepheminfo","tmpres",ficres)
    iret = AMv_fic_maketmp("uiepheminfo","tmpok",ficok)
    
    ! Contenu du répertoire ephempath dans ficrep avec le caractère /
    ! en fin des répertoires
    cmd="/bin/ls -1F  "//trim(ephempath)//" > "//trim(ficrep)
    call system(trim(cmd))
    
    call eph_util_ficunit90(lfntmp1,ier)
    open(lfntmp1,file=ficrep)
    
    do while (.true.)
       
       read(lfntmp1,'(a)',END=901,ERR=901) ligne
       ll=len_trim(ligne)

       
       if ( ligne(ll:ll)=="/" .and. trim(ligne).ne."CVS") then
          
          rep=ligne(:ll-1)
          
          ! Si on est dans un répertoire, lecture des fichiers de ce répertoire 
          ! dans ficfic
          cmd="//bin/ls -1 "//trim(ephempath)//"/"//trim(rep)//&
               " > "//trim(ficfic)
          call system(trim(cmd))
          
          call eph_util_ficunit90(lfntmp2,ier)
          open(lfntmp2,file=ficfic)
          
          call eph_util_ficunit90(lfntmp3,ier)
          
          cmd = "/bin/cp /dev/null "// trim(ficok)
          call system(trim(cmd)) 
          
          do while (.true.)
             
             read(lfntmp2,'(a)',END=800,ERR=800) fic
             ll=len_trim(fic)
             
             if (fic(ll-3:ll)==".bsp") then
                
                ! mettre dans ficok les fichiers voulus
                cmd = trim(cmdbrief)//" "//trim(ephempath)//&
                     "/"//trim(rep)//"/"//trim(fic)//&
                     " | /bin/grep '("//trim(codecorps)//")' 1> /dev/null && /bin/echo " // &
                     trim(fic) // " 2> /dev/null >> " //trim(ficok)
                call system(trim(cmd))
                cmd = trim(cmdbrief)//" "//trim(ephempath)//&
                     "/"//trim(rep)//"/"//trim(fic)//" | /bin/grep "//trim(codecorps)//&
                     " | /bin/grep Body | grep -v '(' 1> /dev/null && /bin/echo " // trim(fic) // &
                     " 2> /dev/null >> " //trim(ficok)
                call system(trim(cmd))
                cmd = trim(cmdbrief)//" "//trim(ephempath)//&
                     "/"//trim(rep)//"/"//trim(fic)//" | grep "//trim(codecorps)//&
                     " | /bin/grep Bodies | grep -v '(' 1> /dev/null && /bin/echo " // trim(fic) // &
                     " 2> /dev/null >> " //trim(ficok)
                call system(trim(cmd))
             endif
          end do
          
800       close (lfntmp2)
          
          open(lfntmp2,file=ficok)
          do while (.true.)
             
             read(lfntmp2,'(a)',END=900,ERR=900) fic
             ll=len_trim(fic)
             
             if (fic(ll-3:ll)==".bsp") then
                cmd = trim(cmdbrief)//" "//trim(ephempath)//&
                     "/"//trim(rep)//"/"//trim(fic)//&
                     " | /bin/tail -2 | /bin/head -1 2> /dev/null "//" >> " //trim(ficres)
                call system(trim(cmd))
                
                open(lfntmp3,file=ficres)
                read(lfntmp3,'(a)',END=850,ERR=850) date
                
                if (len_trim(date)>0 .and. len_trim(date)<ephv_lgmax ) then
                   
                   indic=indic+1
                   if (indic <= eph_nbmaxfic) then
                      ltabtheo(indic) = trim(rep)
                      ltabfic(indic)  = trim(fic)
                   endif
                   
                   if (laffichage) then
                      
                      if (indic == 1) then
                         ! Affichage libellés collonnes au 1er passage
                         write(*,'(T2,a,T25,a,T79,a)') 'Théorie','Fichier','Période couverte'
                      endif
                      
                      write(*,'(T3,a,T12,a,T50,a)') trim(rep), trim(fic), trim(date)
                      
                   endif
                   
                endif
                
             endif
850          close(lfntmp3)
             
          enddo
          
       endif
       
900    continue
       close(lfntmp2)
       
    enddo
    close(lfntmp1)
    
901 continue
    
    if (laffichage .and. indic == 0) then
       ! Pas de fichiers 
       write(*,'(T2,a)') 'Aucun fichier ne contient le corps '//trim(codecorps)
    endif
    
    ! Suppression des fichiers temporaires
    
    iret = AMv_fic_rmtmp(ficrep)
    iret = AMv_fic_rmtmp(ficfic)
    iret = AMv_fic_rmtmp(ficres)
    iret = AMv_fic_rmtmp(ficok)
    
    ! Affectation des arguments optionnels en sortie
    
    if (PRESENT(tabtheo)) then
       if (associated(tabtheo)) deallocate(tabtheo)
       allocate(tabtheo(indic))
       tabtheo(1:indic) = ltabtheo(1:indic)
    endif
    
    if (PRESENT(tabfic)) then
       if (associated(tabfic)) deallocate(tabfic)
       allocate(tabfic(indic))
       tabfic(1:indic) = ltabfic(1:indic)
    endif
    
    deallocate(ltabtheo)
    deallocate(ltabfic)
    
  end subroutine ephui_corps_in_fic
  
end module ui_ioplus
