program ui_potentiel

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_potentiel
!
!$Resume
!  Affichage des coefficients des modèles de potentiel planétaire.
!
!$Description
!  Utilitaire permettant l'affichage des coefficients utilisés dans les différents
!  modèles de potentiel planétaire.
!  Les coefficients affichés sont normalisés et dénormalisés.
!  Il est possible d'afficher des données utiles au modèle
!
!
! Usage
!.  ui_potentiel (-J -degmin deg1 -degmax deg2) | (-C|-S -ordre ordre -degre degre)  -m modele -jour jdeb -opt -nv
!
!>E  modele       : <LEN=256>            modèle de potentiel utilisé 
!>E  jdeb         : <pm_reel>            jour de calcul (MJD)
!>E  degre        : <integer>            degré du coefficient demandé
!>E  ordre        : <integer>            ordre du coefficient demandé
!>E  deg1         : <integer>            affichage des Jdeg1 à Jdeg2
!>E  deg2         : <integer>            affichage des Jdeg1 à Jdeg2
!>E  -J           : <>                   coefficient zonal demandé
!>E  -C           : <>                   coefficient tesseral demandé en cosinus 
!>E  -S           : <>                   coefficient tesseral demandé en sinus
!>E  -opt         : <>                   informations générales du modèle
!>E  [-nv]        : <>                   mode non verbeux
!
!
!>S  C            : <PM_REEL>            coefficient tesseral en cosinus
!>S  S            : <PM_REEL>            coefficient tesseral en sinus
!>S  requa        : <PM_REEL>            rayon équatorial (m)
!>S  apla         : <PM_REEL>            aplatissement
!>S  mu           : <PM_REEL>            mu (m3/s²)
!>S  vrot         : <PM_REEL>            vitesse de rotation (rad/s)
!>S  degremax     : <integer>            degré maximum du modèle
!>S  ordremax     : <integer>            ordre maximum du modèle
!
!$Auteur
!  Julien BOUILLANT (ATOS ORIGIN)
!
!$Version
!  $Id: ui_potentiel.F90 355 2013-02-14 12:16:41Z aadt $
!
!$Historique
!  $Log: ui_potentiel.F90,v $
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.38  2010/11/02 16:16:17  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.37  2010/11/02 14:24:09  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.36  2010/10/21 13:46:22  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.35  2010/03/29 14:45:11  vivaresf
!  VERSION::FA-ID1377:29/03/2010: affichage des coefs sans commentaires
!
!  Revision 1.34  2008/10/28 14:11:46  cml
!  FA-ID 1061 : Ajout du test d erreur apres initialisation de la base
!
!  Revision 1.33  2008/10/14 09:21:26  cml
!  DM-ID 1058 : Correction d un test effectue sur une variable non initialisee
!
!  Revision 1.32  2008/10/14 07:45:49  cml
!  DM-ID 1058 : Correction d un test effectue sur une variable non initialisee
!
!  Revision 1.31  2008/10/03 07:22:06  cml
!  FA-ID 1010 : Ajout du mot clef fmt au differents read des utilitaires
!
!  Revision 1.30  2008/08/04 14:09:30  gss
!  DM-ID 1058 : (portage g95) initialisation à NULL des pointeurs lors de leur
!  déclaration, suppression des variables locales non utilisées et suppression des
!  labels (format) non utilisés.
!
!  Revision 1.29  2008/07/11 12:08:35  cml
!  FA-ID 1070 : Corrections d\'orthographe dans les cartouches et les messages
!
!  Revision 1.28  2008/04/10 07:34:43  vivaresf
!  FA-ID 664 : présentation des utilitaires
!
!  Revision 1.27  2008/04/04 18:00:31  vivaresf
!  Version 2.4 relecture des cartouches
!
!  Revision 1.26  2008/04/04 17:10:36  vivaresf
!  Version 2.4 : mise à jour des cartouches
!
!  Revision 1.25  2008/04/03 18:03:44  vivaresf
!  FA-ID 664 : correction des cartouches et de la présentation
!  Revision 1.24  2007/11/21 16:50:00  huec
!  DM-ID 698 : Amelioration des moyens de tests COMPAS
!  Revision 1.23  2007/11/20 08:57:01  vivaresf
!  DM-ID 539 : test pour éviter des messages divers
!  Revision 1.22  2006/11/21 12:11:08  vivaresf
!  Version 2.1 : passage Understand, variables inutilisées
!  Revision 1.21  2006/11/20 17:27:05  vivaresf
!  Version 2-1 : métriques understand, suppression variables inutile
!  Revision 1.20  2006/11/20 08:13:23  vpg
!  Mise a jour des cartouches
!  Revision 1.19  2006/11/14 17:57:31  vivaresf
!  DM-ID 425 : formattage des sorties
!  Revision 1.18  2006/11/14 09:22:28  mle
!  MAJ des cartouches de ui_potentiel
!  Revision 1.17  2006/11/02 12:43:45  mle
!  passage understand
!  Revision 1.16  2006/10/26 13:45:36  vpg
!  DM-ID 425 : passage de PSIMU sous Linux
!  Revision 1.15  2006/10/24 08:49:09  vpg
!  DM-ID 566 : amelioration de l'ergonomie des IHM de COMPAS_UI ; les sorties des utilitaires donnent leur resultats sur stdout, les messages d'erreurs sur stderr
!  Revision 1.14  2006/10/18 09:56:51  vivaresf
!  Intégration DM 425/ DM 462
!  Revision 1.13  2006/10/18 09:45:25  mle
!  DM-ID 462 : validation
!  Revision 1.12.2.1  2006/09/26 12:23:10  vpg
!  DM-ID 425 : Version initiale du FT (Passage PSIMU sous Linux)
!  Revision 1.12  2006/09/25 14:01:33  mle
!  DM-ID 425 : pointeurs plutot que allocatable pour simplifier
!  me portage LINUX
!  Revision 1.11  2006/08/30 09:52:40  vivaresf
!  FA-ID 576 : complément de message d'erreur
!  Revision 1.10  2006/08/30 09:39:09  vivaresf
!  FA-ID 576 : gestion d'erreur
!  Revision 1.9  2006/08/30 08:53:51  vivaresf
!  FA-ID 578 : warning si pas d'option demandée,
!  traitement d'erreur pour obtenir un message si le format est incorrect
!  Revision 1.8  2006/05/23 09:30:47  vivaresf
!  DM-ID 523 : ordre maximal a 60
!  - simplification des codes
!  - simplification des messages ecran
!  - complement de description + usage
!  Revision 1.7  2006/05/12 12:06:14  bouillaj
!  Amelioration qualite : complements sur les cartouches
!  Revision 1.6  2006/05/02 09:38:39  vpg
!  Suppression des variables non utilisees
!  Revision 1.5  2006/04/25 09:19:24  bouillaj
!  Affichage de coefficients normalises et denormalises
!  Revision 1.4  2006/04/18 09:31:20  bouillaj
!  Augmentation du nombre de caractere pour fichier
!  Revision 1.3  2006/04/12 15:49:37  bouillaj
!  *** empty log message ***
!  Revision 1.2  2006/03/30 08:27:38  bouillaj
!  Mise a jour du cartouche
!
!$FinHistorique
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use mslib
  use msp_gestion_erreur
  use ui_io
  use cps_potentiel
  use cps_utilisateur

  implicit none

! SVN Source File Id
  character(len=256), parameter :: SVN_VER = &
   '$Id: ui_potentiel.F90 355 2013-02-14 12:16:41Z aadt $'


  ! Entrees/sorties

  character(LEN=256) :: modele
  real(kind=pm_reel) :: jdeb
  integer :: degre, ordre, deg1, deg2

  ! Variable locale
  character(LEN=256), dimension(50) :: l_opt
  integer :: ierfin, noptions, i, trouve, ii
  logical :: rep, denorm
  real(KIND=PM_REEL),dimension(:,:), pointer :: C => NULL(), S => NULL()
  character(LEN=256) :: fichier
  real(KIND=PM_REEL), dimension(:,:), pointer :: C_norm => NULL(), S_norm => NULL()
  real(KIND=PM_REEL) :: requa, apla, mu, vrot
  integer :: degremax, ordremax
 
  logical :: opt_mode, opt_C, opt_S, opt_opt, opt_jdeb, opt_ordre 
  logical :: opt_deg, opt_coefJ, opt_nv, opt_min, opt_max, opt_fic
 
  type(MSP_MESSAGE) :: messages

  ! initialisation
  call cps_init_utilisateur()
  if ( MSP_gen_messages("ui_potentiel") ) then
     ! Si l'initialisation échoue, on arrête l'utilitaire
     call MSP_recuperer_message(nb_mes=MSP_tous_messages, message=messages)
     call MSP_afficher_message(message=messages, unit=0)
     call MSP_effacer_message()
     stop
  endif

  rep = .true.
  denorm = .false.

  opt_fic = .false.
  opt_mode = .false.
  opt_C = .false.
  opt_S = .false.
  opt_opt = .false.
  opt_jdeb = .false.
  opt_ordre = .true.
  opt_deg = .false.
  opt_min = .false.
  opt_max = .false.
  opt_coefJ = .false.
  opt_nv = .false.

  ! analyse des arguments
  !-------------------------

  ! aide
  ! lecture des arguments
  call ui_lire_options(noptions, l_opt)
  call ui_test_arguments ("-h", "", ierfin)

  ! -h ou pas de parametres
  if(ierfin.eq.1.or.noptions == 0) then
     call ui_ecrire_help("ui_potentiel")
     goto 999
  endif

  ! decodage
  do i=1, noptions
     call cpsi_lireOptions(l_opt(2*i-1),l_opt(2*i),opt_fic,fichier,opt_mode,modele,opt_C,opt_S, &
     opt_opt,opt_jdeb,jdeb,opt_ordre,ordre,opt_deg,degre,opt_coefJ,opt_min,deg1,opt_max,deg2,opt_nv)
  end do


  ! Vérification des données obligatoires.
  !-------------------------

  if (.not.opt_mode .and. .not. opt_fic) then
     call msp_signaler_message()
     ! il faut que le modele existe et que corresponde un fichier
     write(0,*) "Erreur : modèle de potentiel (-m modele ou -f fichier) manquant"
     goto 999
  endif

  if(.not.opt_jdeb) then
     call msp_signaler_message()
     write(0,*) " Erreur : jour (-jour) manquant"
     goto 999
  endif

  if ((opt_min .or. opt_max) .and. .not. opt_coefJ) then
     call MSP_signaler_message(cle_mes="CPS_OPT_J", & 
          routine="ui_potentiel")
     goto 999
  end if


  if (opt_mode) then
     trouve = cps_getFichierPotentiel(trim(modele), fichier, rep)
     if (trouve == CPS_ERR_DEF) then
        call msp_signaler_message()
        write (0,*) "Erreur : modèle " // trim(modele) //" absent"
        goto 999
     endif
  end if

  ! Lecture des coefficients
  !-------------------------

  denorm = .false.
  call cps_lirePotentielGRGS(fichier, jdeb, denorm, C_norm, S_norm, requa, apla, &
       mu, vrot, degremax, ordremax)
  if (MSP_gen_messages("ui_potentiel")) then
     write(0,*) "Erreur de lecture du fichier "// trim(fichier)     
     write(0,*) "Le fichier est-il au format GRGS ?"
     goto 999
  endif

  ! Erreur : degre superieur a 60
  ! Certains degrés ne sont exploités que si l'option est activée
  if (((opt_C .or. opt_S) .and. ( degre.gt.60.or. ordre.gt.60) ) &
       .or. (opt_coefJ .and. deg2.gt.60) ) then
     if (.not.opt_nv) write(*,'(a)') "Attention : dénormalisation non " // &
          "implémentée pour ordre ou degré supérieur à 60"
  else
     denorm = .true.
     call cps_lirePotentielGRGS(fichier, jdeb, denorm, C, S, requa, apla, &
          mu, vrot, degremax, ordremax)
  endif

  if (MSP_gen_messages("ui_potentiel")) then
     write(0,*) "Erreur de lecture du fichier "// trim(fichier)     
     write(0,*) "Le fichier est-il au format GRGS ?"
     goto 999
  endif


  ! Coefficients tesseraux  
  !-----------------------
  if(opt_C .or. opt_S) then

     ! Erreur : degre ou ordre manquant
     if(.not.opt_deg.or..not.opt_ordre) then
        call msp_signaler_message()
        write(0,*) "Erreur : ordre ou degré du terme demandé manquant"
        goto 999
     endif

     call cpsi_ecrireCoeffTesseraux(opt_C, opt_S, opt_nv, denorm, C, C_norm, S, S_norm, degre, ordre)

  endif
  
  ! Coefficents J
  !--------------
  if(opt_coefJ) then

     if (.not.opt_min .or. .not.opt_max) then
        call MSP_signaler_message(cle_mes="CPS_OPT_J", & 
             routine="ui_potentiel")
        goto 999
     end if

     ! Coef J
     do ii = deg1, deg2
        if (.not.opt_nv) then
           if (denorm) write(*,1001) "Coefficient J",ii," dénormalisé : ", -C(ii,0)
           write(*,1001) "Coefficient J",ii," normalisé   : ", -C_norm(ii,0)
        else
           if (denorm) write(*,1002) -C(ii,0)
           write(*,1002) -C_norm(ii,0)
        endif
     end do
  endif

  ! Options du modèle
  !-------------------
  if (opt_opt) then
     call cpsi_ecrireOptions(opt_nv, requa, apla, mu, vrot, ordremax, degremax)
  endif
  

  ! aucunes des options précédentes
  !--------------------------------
  if (.not.(opt_opt.or. opt_coefJ.or.opt_S.or.opt_C)) then
     call msp_signaler_message()
     write(0,*) "Erreur : aucune sortie demandée"     
     goto 999
  endif


999 continue

  ! affichage des erreurs
  if (MSP_PROBLEME) then
     call MSP_recuperer_message(message=messages, nb_mes=MSP_tous_messages)
     call MSP_afficher_message(message=messages, unit=0)
  endif
 
  ! fin
  call cps_close_utilisateur()

 ! liberation memoire
  if (associated(C_norm)) then
 	deallocate(C_norm)
  end if
  if (associated(S_norm)) then
 	deallocate(S_norm)
  end if
  if (associated(C)) then
     deallocate(C)
  end if
  if (associated(S)) then
 	deallocate(S)
  end if

! formats d'écriture pour compatibilité des sorties inter-plate-forme
1001 format (a15,i3,a15,e22.15)
1002 format (e22.15)

contains

  subroutine cpsi_lireOptions(chaine1,chaine2,opt_fic,fichier,opt_mode,modele,opt_C,opt_S, &
       opt_opt,opt_jdeb,jdeb,opt_ordre,ordre,opt_deg,degre,opt_coefJ,opt_min,deg1,opt_max,deg2,opt_nv)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_lireOptions
!
!$Resume
!  Lecture des options de ui_potentiel
!$Description
!  Lecture des options de ui_potentiel et remplissage des variables en conséquence
!  on lit les options 2 à 2
!
!$Auteur
!  ATos Origin
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_lireOptions(chaine1,chaine2,opt_fic,fichier,opt_mode,modele,opt_C,opt_S, &
!.           opt_opt,opt_jdeb,jdeb,opt_ordre,ordre,opt_deg,degre,opt_coefJ,opt_min,deg1,opt_max,deg2,opt_nv)
!.    character(len=*) :: chaine1,chaine2,fichier,modele
!.    logical :: opt_fic,opt_mode,opt_C,opt_S,opt_opt,opt_jdeb
!.    logical :: opt_ordre,opt_deg, opt_coefJ,opt_min,opt_max,opt_nv
!.    integer :: deg1, deg2, ordre, degre
!.    real(kind=pm_reel) :: jdeb
!
!$Arguments
!>E/S   chaine1    :<LEN=*>     option 1
!>E/S   chaine2    :<LEN=*>     option 2
!>E/S   opt_fic    :<logical>   présence de -fic
!>E/S   fichier    :<LEN=*>     fichier si -fic
!>E/S   opt_mode   :<logical>   présence de -m
!>E/S   modele     :<LEN=*>     modèle si -m
!>E/S   opt_C      :<logical>   présence de -C
!>E/S   opt_S      :<logical>   présence de -S
!>E/S   opt_opt    :<logical>   présence de -opt
!>E/S   opt_jdeb   :<logical>   présence de -jdeb
!>E/S   jdeb       :<pm_reel>   jdeb si -jdeb
!>E/S   opt_ordre  :<logical>   présence de -ordre
!>E/S   ordre      :<integer>   ordre
!>E/S   opt_deg    :<logical>   présence de -degre
!>E/S   degre      :<integer>   degré
!>E/S   opt_coefJ  :<logical>   présence de -J
!>E/S   opt_min    :<logical>   présence de -degmin
!>E/S   deg1       :<integer>   deg1
!>E/S   opt_max    :<logical>   présence de -degmax
!>E/S   deg2       :<integer>   deg2
!>E/S   opt_nv     :<logical>   présence de -nv
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use mslib
    implicit none
    
    !arguments
    character(len=*), intent(inout)::chaine1,chaine2,fichier,modele
    logical, intent(inout)::opt_fic,opt_mode,opt_C,opt_S,opt_opt,opt_jdeb
    logical, intent(inout):: opt_ordre,opt_deg, opt_coefJ,opt_min,opt_max,opt_nv
    integer,intent(inout)::deg1, deg2, ordre, degre
    real(kind=pm_reel), intent(inout)::jdeb

    ! Pas de variables locales

    select case (chaine1)
    case ("-f")
       opt_fic = .true.
       fichier = chaine2
    case ("-m")
       opt_mode = .true.
       read(chaine2,fmt=*) modele 
    case ("-C")
       opt_C = .true.
    case ("-S")
       opt_S = .true.
    case ("-opt")
       opt_opt = .true.
    case ("-jour")
       opt_jdeb = .true.
       read(chaine2,fmt=*) jdeb
    case ("-ordre")
       opt_ordre = .true.
       read(chaine2, fmt='(i2)') ordre
    case ("-degre")
       opt_deg = .true.
       read(chaine2, fmt='(i2)') degre
    case ("-J")
       opt_coefJ = .true.
    case("-degmin")
       opt_min = .true.
       read(chaine2, fmt='(i2)') deg1
    case("-degmax")
       opt_max = .true.
       read(chaine2, fmt='(i2)') deg2
    case ("-nv ")
       opt_nv = .true.
    end select
  end subroutine cpsi_lireOptions

  subroutine cpsi_ecrireOptions(nv, requa, apla, mu, vrot, ordremax, degremax)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_ecrireOptions
!
!$Resume
!  Ecriture écran des informations générales
!$Description
!Ecriture des informations générales :rayon equatorial, aplatissement, mu,
!vitesse de rotation, degre et ordre maximaux
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_ecrireOptions(nv, requa, apla, mu, vrot, ordremax, degremax)
!.    logical :: nv
!.    real(kind=pm_reel) :: requa, apla, mu, vrot
!.    integer :: ordremax, degremax
!
!$Arguments
!>E     nv        :<logical>   si nv, pas d'affichage des commentaires 
!>E     requa     :<pm_reel>   rayon équatorial
!>E     apla      :<pm_reel>   aplatissement
!>E     mu        :<pm_reel>   mu
!>E     vrot      :<pm_reel>   vitesse de rotation
!>E     ordremax  :<integer>   degré min du modèle
!>E     degremax  :<integer>   degré max du modèle
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use mslib
    implicit none
    
    !arguments
    logical, intent(in)::nv
    real(kind=pm_reel), intent(in)::requa, apla, mu, vrot
    integer, intent(in)::ordremax, degremax
    
    !variables locales

    
    ! avec l'option nv on n'ecrit pas les commentaires
    if (.not.nv) then
       write(*,1003) "Rayon équatorial              (m)     = ", requa
       write(*,1003) "Aplatissement                         = ", apla
       write(*,1003) "Potentiel gravitationnel      (m3/s2) = ", mu
       write(*,1003) "Vitesse de rotation           (rad/s) = ", vrot
       write(*,1005) "Ordre maximal disponible              = ", ordremax
       write(*,1005) "Degré maximal disponible              = ", degremax
       write(*,'(a)') "Attention: dénormalisation non implémentée pour un degré ou ordre supérieur à 60"
    else
       write(*,1004) requa
       write(*,1004) apla
       write(*,1004) mu
       write(*,1004) vrot
       write(*,'(i3)') ordremax
       write(*,'(i3)') degremax
    end if

! formats d'écriture pour compatibilité des sorties inter-plate-forme
1003 format (a,e22.15)
1004 format (e22.15)
1005 format (a,I3)

  end subroutine cpsi_ecrireOptions

  subroutine cpsi_ecrireCoeffTesseraux(opt_C, opt_S, opt_nv, denorm, C, C_norm, S, S_norm, degre, ordre)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cpsi_ecrireCoeffTesseraux
!
!$Resume
!  Ecriture des coef. tesseraux
!$Description
! Ecriture des coefficients tesseraux en cosinus C ou en sinus S (ou les deux) à
! l'ordre et au degré demandes
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cpsi_ecrireCoeffTesseraux(opt_C, opt_S, opt_nv, denorm, C, C_norm, S, S_norm, degre, ordre)
!.    logical :: opt_C, opt_S, opt_nv, denorm
!.    real(KIND=PM_REEL),dimension(:,:), pointer :: C, C_norm, S, S_norm
!.    integer :: degre, ordre
!
!$Arguments
!>E     opt_C   :<logical> écriture des C             
!>E     opt_S   :<logical> écriture des S
!>E     opt_nv  :<logical> nv : pas de commentaires                    
!>E     denorm  :<logical> coefficients dénormalisés
!>E/S   C       :<PM_REEL,DIM=(:,:),pointer>   coeffictients en C
!>E/S   C_norm  :<PM_REEL,DIM=(:,:),pointer>   coeffictients en C normalisés
!>E/S   S       :<PM_REEL,DIM=(:,:),pointer>   coeffictients en S
!>E/S   S_norm  :<PM_REEL,DIM=(:,:),pointer>   coeffictients en S normalisés
!>E     degre   :<integer>  degré 
!>E     ordre   :<integer>  ordre 
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use mslib
    implicit none
    
    ! arguments
    logical, intent(in)::opt_C, opt_S, opt_nv, denorm
    real(KIND=PM_REEL),dimension(:,:), pointer ::C, C_norm, S, S_norm
    integer, intent(in)::degre, ordre
    
    ! Coef tesseral en Cosinus
    if (opt_C) then
       if (.not.opt_nv) then
          if (denorm) &
          write(*,1005) "Coefficient tesseral en cosinus dénormalisé : ", C(degre,ordre)
          write(*,1005) "Coefficient tesseral en cosinus normalisé   : ", C_norm(degre,ordre)
       else
          if (denorm) write(*,1006) C(degre,ordre)
          write(*,1006) C_norm(degre, ordre)
       endif
    endif

    ! Coef tesseral en Sinus
    if (opt_S) then
       if (.not.opt_nv) then
          if (denorm) &
          write(*,1005) "Coefficient tesseral en sinus dénormalisé   : ", S(degre,ordre)
          write(*,1005) "Coefficient tesseral en sinus normalisé     : ", S_norm(degre,ordre)
       else
          if (denorm) write(*,1006) S(degre,ordre)
          write(*,1006) S_norm(degre,ordre)
       endif
    endif

! formats d'écriture pour compatibilité des sorties inter-plate-forme
1005 format (a,e22.15)
1006 format (e22.15)
  end subroutine cpsi_ecrireCoeffTesseraux
end program
