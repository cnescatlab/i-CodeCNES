program ui_compas

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_compas
!
!$Resume
!    Affichage des informations de la base de données COMPAS
!
!$Description
!    Utilitaire de consultation de la base de données COMPAS
!
!    Usage
!.    ui_compas -ci codecorps [ -all ] [-kep] [ -cst constante ] [-att attribut ] [-nv]
!.    ui_compas -lf theorie [ -all ] [ -cst constante ] [-nv]
!.    ui_compas -h
!
!    Arguments
!>E  codecorps: <integer>       code du corps d'intérêt
!>E  theorie  : <LEN=cps_maxlg> théorie d'intérêt
!>E  -h       : aide en ligne
!>[S] -all    : tout ce qui concerne le corps ou la théorie
!>[S] -kep    : bulletin képlérien pour un corps (dga, exc, inc, pom, gom, anm, dateref)
!>[S] -cst    : valeur de la constante souhaitée (voir ui_compasinfo -lt)
!>[S] -att    : valeur de la constante souhaitée pour un corps (voir les "nom" donnés par ui_compasinfo -lt corps)
!>[S] -nv     : si présent n'affiche pas les unités ni les commentaires
!
!$Auteur
!    F.VIVARES           (SchlumbergerSema)
!
!$Version
!  $Id: ui_compas.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ui_compas.F90,v $
!  Revision 1.11  2010/10/25 08:35:51  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.10  2009/03/24 10:18:53  cml
!  DM-ID 1159 : Correction du numero de DM de la mise en conf. precedente
!
!  Revision 1.9  2009/03/24 09:00:02  cml
!  DM-ID 1179 : Ajout d'initialisations de pointeurs manquantes
!
!  Revision 1.8  2009/03/23 14:04:05  cml
!  DM-ID 1159 : Correction d'un format d'ecriture incorrect
!
!  Revision 1.7  2008/10/31 13:08:38  cml
!  FA-ID 1076 : Corrections mineurs dans les resultats des utilitaires
!
!  Revision 1.6  2008/04/28 13:01:25  vivaresf
!  FA-ID 664 : présentation des sorties et cartouches
!
!  Revision 1.5  2008/04/11 12:47:34  vivaresf
!  Version 2.4 AQ : correction des cartouches
!
!  Revision 1.4  2008/04/11 10:09:35  vivaresf
!  FA-ID 778 : renommage des variables mal nommées
!  suppression des doubles déclarations
!  rajout de implicit none
!  rajout de intent(in/out)
!  suppression des lignes de code commentées
!  Revision 1.3  2008/04/10 13:07:27  vivaresf
!  Version 2.4 AQ : ordre des paramètres dans cpsi_initIdent pour respecter le RNC
!  Revision 1.2  2008/04/10 10:25:06  vivaresf
!  FA-ID 664 : correction de la présentation, accents
!  AQ : mise à jour des cartouches, supression du code commenté
!  Revision 1.1  2008/02/08 17:51:37  vivaresf
!  FA-ID 889, DM-ID 820 :
!  - réorganisation du code pour que tout soit dans le répertoire src
!  Revision 1.17  2007/09/18 12:52:33  huec
!  DM-ID 698 : Amelioration des moyens de tests COMPAS
!  Revision 1.16  2007/07/11 14:07:43  couturis
!  FA-ID 708: ajout de [-h] dans le cartouche + description
!  Revision 1.15  2007/07/05 17:14:35  vivaresf
!  FA-ID 664 : correction de la présentation
!  Revision 1.14  2007/07/05 09:16:03  vivaresf
!  FA-ID 596 suppression du modèle géométrique (obsolète)
!  Revision 1.13  2006/11/21 12:08:41  vivaresf
!  Version 2.1 : passage Understand, variables inutilisées
!  Revision 1.12  2006/11/15 08:48:18  mle
!  DM-ID 462 : date de reference avec l'option -kep (complements)
!  Revision 1.11  2006/11/02 12:42:12  mle
!  passage understand
!  Revision 1.10  2006/10/25 13:45:59  vpg
!  DM-ID 462 : affichage de la date de reference avec l'option -kep
!  Revision 1.9  2006/10/23 12:55:39  vpg
!  DM-ID 566 : Cloture du FT (Amelioration de l ergonomie des IHM de COMPAS_UI V2-0)
!  Revision 1.8.2.1  2006/10/23 10:11:50  vpg
!  DM-ID 566 : Version initiale du FT (Amelioration de l ergonomie des IHM de COMPAS_UI V2-0)
!  Revision 1.8  2006/10/06 08:51:56  mle
!  DM-ID 462 : version finale
!  Revision 1.7  2006/09/28 16:06:53  mle
!  DM-ID 462 : modification des options-version initiale
!  Revision 1.6  2006/06/30 14:48:58  vpg
!  Mise a jour des cartouches
!  Revision 1.5  2006/06/16 17:32:00  vivaresf
!  Cartouches d'entete
!  Revision 1.4  2006/06/01 09:57:34  vivaresf
!  Routine dupliquee
!  Revision 1.3  2006/05/31 13:08:37  vivaresf
!  COMPAS 2.0 : validation
!  Revision 1.2  2006/04/26 11:28:48  bouillaj
!  FA-ID 463 : Homogeneisation des resultats entre les options -kep et -all
!  Revision 1.1.1.1  2006/02/06 15:48:20  vpg
!  ihm et utilitaires COMPAS V2-0
!  Revision 1.4  2005/12/09 14:46:23  vivaresf
!  Mise en forme des tests
!  Revision 1.3  2005/12/09 12:28:19  vivaresf
!  Dernieres validation
!  Revision 1.2  2005/12/08 18:24:28  vivaresf
!  Mise a jour des cartcouhes
!  Revision 1.1.1.1  2005/12/07 07:23:09  vivaresf
!  Refonte de COMPAS
!  Revision 1.17  2005/09/29 14:33:04  bouillaj
!  DM-ID 148 : Cloture du FT (ajout de bulletins  une date de reference pour chaque corps de COMPAS)
!  Revision 1.16.4.1  2005/09/29 14:26:32  bouillaj
!  DM-ID 148 : Version initiale du FT (ajout de bulletins  une date de reference pour chaque corps de COMPAS)
!  Revision 1.16  2005/03/07 15:32:16  vivaresf
!  DM-ID 318 : integration
!  Revision 1.15  2005/03/07 14:41:09  vivaresf
!  Version 1-5
!  Revision 1.14  2005/03/07 08:15:50  vivaresf
!  Version 1-5 : présentation de la documentation extraite des cartouches
!  Revision 1.13  2005/03/04 16:25:13  vivaresf
!  Livraison version 1-5 : correction des entêtes
!  Revision 1.12  2004/12/08 10:06:56  vivaresf
!  Presentation de l'aide en ligne
!  Revision 1.11  2004/12/07 14:07:33  tanguy
!  Modifs dans le cartouche : ordre et libelles des sections
!  Revision 1.10  2004/12/06 10:03:19  tanguy
!  Ajout parametre -coef_prs dans la section [usage]
!  Revision 1.9  2004/12/03 14:23:31  tanguy
!  Champs usage / arguments de l'entete
!  Revision 1.8  2004/12/03 10:51:46  tanguy
!  Prise en compte du coefficient de pression de radiation solaire (option -coef_prs)
!  Revision 1.7  2004/12/01 10:56:06  vivaresf
!  Gestion des messages d'erreur
!  Revision 1.6  2004/10/08 07:31:19  adminam
!  Version sans l'uilib
!  Revision 1.5  2004/03/29 15:06:36  vivaresf
!  DM_1
!  Revision 1.2.4.2  2004/03/29 14:53:36  vivaresf
!  Vresion V1_3 du CNES
!  Revision 1.2.4.1  2004/03/15 11:40:37  vivaresf
!  Evolutions suite a l'utilisation de COMPAS
!  Revision 1.1.1.1  2003/01/07 08:29:06  vivaresf
!  Bibliotheque de COnstantes et Modeles Associes au systeme Solaire
!
!$FinHistorique
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!     ui_compasinfo
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use cps_acces
  use ui_ioplus
  use cps_utilisateur

  implicit none

! SVN Source File Id
  character(len=256) :: SVN_VER =  '$Id: ui_compas.F90 69 2012-09-11 08:33:34Z ffsm $'


#include "formats.h"

  ! entrées
  character(len=CPS_MAXLG) ::  theorie
  integer :: ni

  ! options 
  logical :: nv = .false.
  logical :: alld = .false.
  logical :: corpsd=.false.
  logical :: optcorps=.false.
  logical :: optTheorie = .false.
  logical :: optAtt
  logical :: kepler = .false.
  logical :: optCst = .false.
  logical, dimension(25) :: options

  ! variables locales
  logical, dimension(25) :: existent
  character(len=CPS_MAXLG)  :: typec, corps, corpsc, fichier
  character(len=CPS_MAXLG), dimension(:), pointer :: atm => NULL()
  character(len=CPS_MAXLG), dimension(:), pointer :: geom => NULL()
  character(len=CPS_MAXLG), dimension(:), pointer :: pot => NULL()
  character(LEN=256), dimension(50) :: l_opt
  character(LEN=256) :: dirfcf
  character(LEN=50) :: constante, att
  character(len=CPS_MAXLG), dimension(25) :: nom, unites
  character(len=25)  :: format0
  character(LEN=50) :: unite, reponse
  integer :: lch_dirfcf
  integer :: noptions
  integer :: codecorps, codecc
  integer :: ii, nn, iargc, ier, i, ios, reponseInt
  REAL(KIND=PM_REEL), dimension(25) ::val
  real(KIND=pm_reel) :: valeur, dateref
  type(MSP_MESSAGE) :: messages
  type(tm_orb_kep):: param_kepl
  logical :: err

  ! Initialisations
  ier = AMv_rc_get ('compas_ui_fcf','compas','','dirfcf',dirfcf, &
       lch_dirfcf)
  call MSP_ajouter_fichier_message (trim(dirfcf)//"/CPSUI_erreur_fr")
  call cps_init_utilisateur()
  ! par défaut on prend la théorie UAI1994
  theorie = "UAI1994"

  ! Lecture des paramètres d'entrée

  ! aide
  ! lecture des arguments
  call ui_lire_options(noptions, l_opt)
  call ui_test_arguments ("-h", "", ier)

  ! -h ou pas de parametres
  nn=iargc()
  if(ier.eq.1.or.nn == 0) then
     call ui_ecrire_help("ui_compas")
     goto 999
  endif

  ! decodage
  call ui_lire_fic(fichier, ni)

  do i=1, noptions
     select case (l_opt(2*i-1))
     case ("-ci")
        corpsd=.true.
        read(trim(l_opt(2*i)), '(i10)') codecorps
        optcorps=.true. 
     case ("-lf")
        optTheorie = .true.
        theorie = trim(l_opt(2*i))
     case("-att")
        optAtt = .true.
        att = trim(l_opt(2*i))
     case("-all")
        alld=.true.
     case("-kep")
        kepler = .true.
        options(21)=.true.
     case("-cst")
        optCst = .true.
        constante = trim(l_opt(2*i))
     case ("-nv")
        nv=.true.
     end select
  end do

  ! on identifie att
  if (optAtt) then
     if ( (att/="nom_fr").and.(att/="nom_id").and. (att/="nom_en").and.(att/="type") &
          .and.(att/="corpsc").and.(att/="code").and.(att/="ep_constante")) then
        call MSP_signaler_message(cle_mes="CPS_CSTINEXIST", &
             partie_variable=trim(att), routine="ui_compas")
        goto 999
     end if
  end if

  call cpsi_initIdent(optCst,constante,options,existent,nom,unites,optCorps,nv, kepler, err)
  if (err) goto 999

  ! corps du programme
  !-------------------

  !Gestion des erreurs
  if ( (.not. corpsd) .and. (.not. optTheorie) .and.  (.not. optCst) ) then
     call MSP_signaler_message(cle_mes="CPS_ERR_ARG", &
          partie_variable="code corps/theorie/", routine="ui_compas")
     goto 999
  end if

  ! Gestion des cas particuliers
  !-ci ou -lf revient à -ci -all ou -lf -all
  if( (noptions==1) .and. (corpsd.or.optTheorie) ) alld = .true.
  ! -ci -lf revient à -ci -lf -all
  if( (noptions==2) .and. corpsd .and. optTheorie ) alld = .true.
  ! -ci  -cst ou -ci -cst -lf
  if ( (optCst .and. corpsd) .or. (optCst .and. corpsd .and. optTheorie) ) then
     ios = cps_getCsteTh(codecorps, theorie, constante, valeur, unite)
     if (ios==CPS_OK) then
        if (nv) then
           format0 = '(d15.9)'
           write(*,format0)valeur
        else
           format0 = '((a13),(d15.9),(a20))'
           call cps_codenom(ni, corps)
           write(*,1071) "Corps ", trim(corps), " (", ni, ")"
           write(*,1004) "Théorie ", trim(theorie)
           write(*,format0) trim(constante) //" =", valeur, " "//unite
        end if
     else
        call MSP_signaler_message(cle_mes="CPS_ERR_PARAMABS", &
             partie_variable=trim(constante), routine="ui_compas")
     end if
     goto 999
  end if
  ! -cst ou -cst -lf
  if ( (optCst) .or. (optCst .and. optTheorie) ) then
     ios = cps_getCsteGenTh(theorie, constante, valeur, unite)
     if (ios==CPS_OK) then
        if (nv) then
           format0 = '(d15.9)'
           write(*,format0)valeur
        else
           format0 = '((a13),(d15.9),(a20))'
           write(*,1004) "Theorie ", trim(theorie)
           write(*,format0) trim(constante) //" =", valeur, " "//unite
        end if
     else
        call MSP_signaler_message(cle_mes="CPS_ERR_PARAMABS", &
             partie_variable=trim(constante), routine="ui_compas")
        goto 999
     end if
  end if
  ! -att -ci
  if ( optAtt .and. corpsd ) then
     if (att=="corpsc") then
        ios = cps_getAtt(codecorps, att, reponseInt)
        call cps_codenom(reponseInt, reponse)
        if (ios==CPS_OK) then
           if (nv) then
              write(*,1000)reponse
           else
              call cps_codenom(ni, corps)
              write(*,1071) "Corps ", trim(corps), " (", ni, ")"
              write(*,1004) trim(att) //" =", reponse
           end if
        end if
     else
        ios = cps_getAtt(codecorps, att, reponse)
        if (ios==CPS_OK) then
           if (nv) then
              write(*,1000)reponse
           else
              call cps_codenom(ni, corps)
              write(*,1071) "Corps ", trim(corps), " (", ni, ")"
              write(*,1004) trim(att) //" =", reponse
           end if
        end if
     end if
     if (ios/=CPS_OK) then
        call MSP_signaler_message(cle_mes="CPS_ERR_PARAMABS", &
             partie_variable=trim(att), routine="ui_compas")
     end if
     goto 999
  end if
  if ( optAtt .and. (.not. corpsd) ) then
     call MSP_signaler_message(cle_mes="CPS_ERR_ARG", &
          partie_variable="code corps", routine="ui_compas")
     goto 999
  end if

  ! Cas généraux
  ! code corps connu
  if (corpsd)  then

     call cps_codenom(ni, corps)

     !option -all
     if (alld) then
        kepler = .true.
        options(21)=.true.
        do ii = 1, 5
           options(ii) = .true.
           ios = cps_getCsteTh(ni,theorie, trim(nom(ii)),val(ii),unites(ii))
           if (ios/=CPS_OK) then
              call MSP_signaler_message(cle_mes="CPS_ERR_PARAMABS", &
                   partie_variable=trim(nom(ii)), routine="ui_compas")
           end if
        end do
        options(6) = .true.
        ios = cps_getModelesPotentielCorps(ni, pot)
        if (ios/=CPS_OK) then
           options(6) = .false.
           call MSP_signaler_message(cle_mes="CPS_ERR_PARAMABS", &
                partie_variable=trim(nom(6)), routine="ui_compas")
           !goto 999
        end if
        options(7) = .true.
        ios = cps_getListEltsCorps(ni, "modele_atmosphere", atm)
        if (ios/=CPS_OK) then
           options(7) = .false.
           call MSP_signaler_message(cle_mes="CPS_ERR_PARAMABS", &
                partie_variable=trim(nom(7)), routine="ui_compas")
           !goto 999
        end if

        options(9) = .true.
        ios = cps_getAtt(ni, trim(nom(9)),typec)
        if (ios/=CPS_OK) then
           call MSP_signaler_message(cle_mes="CPS_ERR_PARAMABS", &
                partie_variable=trim(nom(9)), routine="ui_compas")
           !goto 999
        end if
        options(10) = .true.
        ios = cps_getAtt(ni, trim(nom(10)),codecc)
        if (ios/=CPS_OK) then
           call MSP_signaler_message(cle_mes="CPS_ERR_PARAMABS", &
                partie_variable=trim(nom(10)), routine="ui_compas")
           !goto 999
        end if
        call cps_codenom(codecc, corpsc)
     end if
     ! option -kep
     if (kepler) then
        do ii = 11, 16
           options(ii) = .true.
        end do
        ios = cps_getKeplerTh(ni, theorie,param_kepl,dateref)
        if (ios/=CPS_OK) then
           call MSP_signaler_message(cle_mes="CPS_ERR_PARAMABS", &
                partie_variable="bulletin képlérien", routine="ui_compas")
           !goto 999
        end if
        val(21) = dateref
        val(11) = param_kepl%a
        val(12) = param_kepl%e
        val(13) = param_kepl%i
        val(14) = param_kepl%pom
        val(15) = param_kepl%gom
        val(16) = param_kepl%M
     end if


     call cpsi_affichage(nv,optcorps,corps,ni,optTheorie,theorie,optAtt,codecorps, &
     nom,options,val,unites,typec,corpsc,atm,pot,geom)

     ! theorie connue (sans le corps)
  else if (optTheorie) then
     write(*,1004) "Theorie ", trim(theorie)
     if (kepler) then
        call MSP_signaler_message(cle_mes="CPS_ERR_KEP", routine="ui_compas")
        goto 999
     end if
     do ii = 17, 20
        ios = cps_getCsteGenTh(theorie, trim(nom(ii)), valeur, unite)
        if (ios==CPS_OK) then
           if (nv) then
              format0 = '(d15.9)'
              write(*,format0)valeur
           else
              format0 = '((a13),(d15.9),(a20))'
              write(*,format0) trim(nom(ii)) //" =", valeur," "//unite
           end if
        else
           if (nv) then
              write(*,1000)""
           else
              format0 = '((a13),(d15.9),(a20))'
              write(*,format0) trim(nom(ii)) //" =", 0._pm_reel," "//unite
           end if
           call MSP_signaler_message(cle_mes="CPS_ERR_ARG", &
                partie_variable=trim(nom(ii)), routine="ui_compas")
        end if
     end do
  end if


999 continue

  if (MSP_PROBLEME) then
     if (MSP_ERREUR) &
          call MSP_recuperer_message (message=messages,nb_mes=MSP_tous_messages)
     call MSP_afficher_message (message=messages,unit=0)
  endif

  ! fin
  call cps_close_utilisateur()

contains

subroutine cpsi_affichage(nv,optcorps,corps,ni,optTheorie,theorie,optAtt,codecorps, &
     nom,options,val,unites,typec,corpsc,atm,pot,geom)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_affichage
!
!$Resume
!  Affichage écran des sorties
!
!$Description
!  Affichage écran des sorties
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_affichage(nv,optcorps,corps,ni,optTheorie,theorie,optAtt,codecorps, &
!.         nom,options,val,unites,typec,corpsc,atm,pot,geom)
!.    logical :: nv,optcorps,optTheorie,optAtt
!.    logical, dimension(25) :: options
!.    character(len=CPS_MAXLG),dimension(25) :: nom,unites
!.    real(kind=pm_reel),dimension(25) :: val
!.    integer :: ni, codecorps
!.    character(len=CPS_MAXLG) :: typec, corpsc,corps
!.    character(len=CPS_MAXLG), dimension(:), pointer :: atm,pot,geom
!.    character(len=CPS_MAXLG) :: theorie
!
!$Arguments
!>E/S   nv          :<logical>                         pas de commentaires             
!>E/S   optcorps    :<logical>                         corps demandé           
!>E     corps       :<LEN=CPS_MAXLG>                   nom corps             
!>E     ni          :<integer>                         code corps
!>E/S   optTheorie  :<logical>                         théorie demandée
!>E/S   theorie     :<LEN=CPS_MAXLG>                   théorie              
!>E/S   optAtt      :<logical>                         attribut demandé
!>E     codecorps   :<integer>                         code corps
!>E/S   nom         :<LEN=CPS_MAXLG,DIM=(25)>          nom corps   
!>E     options     :<logical,DIM=(25)>                liste d'options possibles 
!>E     val         :<pm_reel,DIM=(25)>                valeurs des attributs à afficher
!>E/S   unites      :<LEN=CPS_MAXLG,DIM=(25)>          unités des attributs
!>E     typec       :<LEN=CPS_MAXLG>                   type de corps
!>E     corpsc      :<LEN=CPS_MAXLG>                   corps central
!>E/S   atm         :<LEN=CPS_MAXLG,DIM=(:),pointer>   modèles d'atmosphère
!>E/S   pot         :<LEN=CPS_MAXLG,DIM=(:),pointer>   modèles de potentiels
!>E/S   geom        :<LEN=CPS_MAXLG,DIM=(:),pointer>   modèles géométriques
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use cps_utilisateur
  use mslib
  implicit none

#include "formats.h"

  !arguments
  logical,intent(inout)::nv,optcorps,optTheorie,optAtt
  logical, dimension(25), intent(in)::options
  character(len=CPS_MAXLG),dimension(25), intent(inout)::nom,unites
  real(kind=pm_reel),dimension(25), intent(in)::val
  integer, intent(in)::ni, codecorps
  character(len=CPS_MAXLG),intent(in)  :: typec, corpsc,corps
  character(len=CPS_MAXLG), dimension(:), pointer :: atm,pot,geom
  character(len=CPS_MAXLG) ::  theorie

  !variables locales
  character(len=22)::format0,format1
  integer::ii
  character(len=1024)::lib
  character(len=CPS_MAXLG)  :: path

  ! Affichage de la ligne d'info d'entete
  if (.not.nv) then
     if (optcorps) then
        write(*,1071) "Corps ", trim(corps), " (", ni, ")"
     endif
     if (optTheorie) then
        write(*,1004) "Théorie ", trim(theorie)
     endif
     if (optAtt) write(*,1001) "code = ",codecorps 
     format0 = '((a13),(d15.9),(a20))'
     format1 = '((a13),(a10))'
     do ii=1,5
        nom(ii) = trim(nom(ii)) // " = "
     enddo
     do ii=9,25
        nom(ii) = trim(nom(ii)) // " = "
     enddo
  else
     format0 = '((a),(d15.9),(a20))'
     format1 = '((a),(a10))'
     do ii=1,5
        nom(ii) = ""
     enddo
     do ii=9,25
        nom(ii) = ""
     enddo
  endif

  ! affichage des valeurs demandées
  do ii=1,5
     if (options( ii)) then
        write(*,format0) trim(nom(ii)),val(ii)," "//trim(unites(ii)) 
     endif
  enddo
  do ii=11,21
     if (options(ii)) then
        write(*,format0) trim(nom(ii)),val(ii)," "//unites(ii) 
     endif
  enddo
  ! affichage de la date de référence pour les paramètres képlériens
  if (options(22)) then
     write(*,format0) trim(nom(21)),val(21)," "//unites(21)
  end if
     
  if (options(9))  write(*,format1) trim(nom(9)),  typec
  if (options(10)) write(*,format1) trim(nom(10)), corpsc
  
     ! affichage des listes
  
  if (options(6)) then
     lib = ""
     if(len_trim(pot(1)).gt.0) lib=trim(pot(1))
     do ii=2,size(pot)
           if (len_trim(pot(ii)).gt.0) &
                lib = trim(lib)//", "//pot(ii)(1:len_trim(pot(ii)))
        enddo

        call cps_constante(corps, path, modele=nom(6))
        write(*,1000) " modele_potentiel = [ "//trim(lib)//" ]"
        write(*,1004) "       repertoire = ", trim(path)
     endif

     if (options(7)) then
        lib = ""
        if(len_trim(atm(1)).gt.0) lib=trim(atm(1))
        do ii=2,size(atm)
           if(len_trim(atm(ii)).gt.0) lib = trim(lib)//", "//trim(atm(ii))
        enddo
        call cps_constante(corps, path, modele=nom(7))
        write(*,1000) "modele_atmosphere = [ "//trim(lib)//" ]"
        write(*,1004) "       repertoire = ", trim(path)
        !          write(*,'((a10),(a3),(a132))') trim(nom(7)), " = ", trim(lib)//"]"
     endif

   end subroutine cpsi_affichage

end program ui_compas

subroutine ui_lire_fic(fic, ni)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ui_lire_fic
!
!$Resume
!  Lecture d'un ou deux fichiers à partir de l'arguement "-fic"
!
!$Description
!  Lecture d'un ou deux fichiers à partir de l'arguement "-fic"
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ui_lire_fic(fic, ni)
!.    character(len=*), dimension(2) :: fic
!.    integer :: ni
!
!$Arguments
!>S     fic  :<LEN=*,DIM=(2)>   Tableau des fichiers lus 
!>S     ni   :<integer>         Numéro du corps d'interêt  
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
  integer, intent(out) :: ni

  integer::nb_arg,pos
  character(len=100)::argument, argument2
  logical :: ficok=.false.
  logical :: ciok=.false.

  ! Fonctions
  integer::iargc

    
  nb_arg = iargc()
  ni=-1

  do pos=1,nb_arg
     call getarg(pos,argument)
     
     if(trim(argument).eq."-f") then 
        if ((pos+1).le.nb_arg) then 
           call getarg(pos+1,argument2)
           fic(1)=trim(argument2)
           ficok=.true.

        endif
        if ((pos+2).le.nb_arg) then
           call getarg(pos+2,argument2)
           fic(2)=trim(argument2)
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

end subroutine ui_lire_fic

subroutine cpsi_initIdent(optCst,constante, options,existent,nom,unites,optCorps,nv, kepler,err)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_initIdent
!
!$Resume
!  Initialisation des variables et identification des options possibles
!
!$Description
!  Initialisation des variables et identification des options possibles
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_initIdent(optCst,constante, options,existent,nom,unites,optCorps,nv, kepler,err)
!.    logical, dimension(25) :: existent,options
!.    character(len=*),dimension(25) :: nom,unites
!.    logical :: optCst
!.    logical :: optCorps,nv,kepler
!.    character(len=50) :: constante
!.    logical :: err
!
!$Arguments
!>E     optCst     :<logical>            
!>E     constante  :<LEN=50>             
!>S     options    :<logical,DIM=(25)>   options possibles
!>S     existent   :<logical,DIM=(25)>   options existantes
!>S     nom        :<LEN=*,DIM=(25)>     noms
!>S     unites     :<LEN=*,DIM=(25)>     unitées
!>E/S   optCorps   :<logical>            
!>E/S   nv         :<logical>            
!>E/S   kepler     :<logical>            
!>S     err        :<logical>            
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

#include "formats.h"

  !arguments
  logical, dimension(25), intent(out)::existent,options
  character(len=*),dimension(25), intent(out)::nom,unites
  logical, intent(in)::optCst
  logical, intent(inout)::optCorps,nv,kepler
  character(len=50), intent(in)::constante
  logical, intent(out)::err

  !variables locales
  logical :: cstExiste = .false.
  integer :: ii

  err = .false.
  options(1:25)=.false.
  existent(1:25)=.false.

  nom(1) = "mu"
  nom(2) = "requa"
  nom(3) = "apla"
  nom(4) = "vrot"
  nom(5) = "J2"
  nom(6) = "pot"
  nom(7) = "atm"
  nom(8) = "geom"
  nom(9) = "type"
  nom(10) = "cc"
  nom(11) = "dga"
  nom(12) = "exc"
  nom(13) = "inc"
  nom(14) = "pom"
  nom(15) = "gom"
  nom(16) = "anm"
  nom(17) = "G"
  nom(18) = "vlum"
  nom(19) = "ua"
  nom(20) = "coef_prs"
  nom(21) = "dateref"
  nom(22) = "kep"
  nom(23) = ""
  nom(24) = ""
  nom(25) = ""

  unites(1:25)=""

  if(optCst) then  
     ! on identifie la constante
     do ii =1, 16
        if ( trim(constante) == trim(nom(ii)) ) then
           options(ii)=.true.
           optcorps=.true.
           cstExiste = .true.
        end if
     end do
     if ( trim(constante) == trim(nom(21)) ) then
        options(21)=.true.
        optcorps=.true.
        cstExiste = .true.
     end if
     do ii =17, 20
        if ( trim(constante) == trim(nom(ii)) ) then
           options(ii)=.true.
           cstExiste = .true.
        end if
     end do
     if (.not. cstExiste) then
        call MSP_signaler_message(cle_mes="CPS_CSTINEXIST", &
             partie_variable=trim(constante), routine="ui_compas")
        err=.true.
     end if
  end if

  ! -kep correspond à la 22ème option 
  if (kepler) then
     options(22)=.true.
     optcorps=.true.
  end if

  ! Initialisation supplémentaire si besoin
  nom(6) = "potentiel"
  nom(7) = "atmosphere"
!  nom(8) = "geometrie"
  nom(10) = "corpsc"

  if (.not.nv) then
     unites(1)  = "~m^3/s^2"
     unites(2)  = "~m"
     unites(4)  = "~rad/s"
     unites(11) = "~m"
     unites(13) = "~rad"
     unites(14) = "~rad"
     unites(15) = "~rad"
     unites(16) = "~rad"
     unites(17) = "~m^3/s^2/kg"
     unites(18) = "~m/s"
     unites(19) = "~m"   
     unites(20) = "~km^3*kg/s^2/m^2"
     unites(21) = "~MJD1950"
  endif

end subroutine cpsi_initIdent


