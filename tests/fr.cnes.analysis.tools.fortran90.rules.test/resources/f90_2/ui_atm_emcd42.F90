program ui_atm_emcd42

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_atm_emcd42
!
!$Resume
! Utilitaire pour le fonctionnement du modèle d'atmosphère EMCD42
!
!$Description
! Utilitaire pour le fonctionnement du modèle d'atmosphère EMCD42
!
! Usage
!.     ui_atm_emcd42 -choice_date choice_date -localtime localtime 
!.              -day day -month month -year year -hour hour -minute minute -second second 
!.              -zkey zkey -xz xz -hireskey hireskey -xlat xlat -xlon xlon 
!.              -dust dust -perturkey perturkey -seedin seedin -gwlength gwlength -extvarkey extvarkey
!
!
!>E  choice_date      : <LEN=1>    date terrestre (e) ou martienne (m)
!>E  month            : <integer>  mois (si choice_date=e)
!>E  day              : <integer>  jour (si choice_date=e)
!>E  year             : <integer>  année (si choice_date=e)  
!>E  hour             : <integer>  heure (si choice_date=e)  
!>E  minute           : <integer>  minute (si choice_date=e) 
!>E  second           : <integer>  seconde (si choice_date=e)
!>E  zkey             : <integer>  indicateur pour le type de coordonnées en Z
!>E  xz               : <real>     coordonnée Z
!>E  xlon             : <real>     longitude Est  (deg)
!>E  xlat             : <real>     latitude Nord  (deg)
!>E  hireskey         : <integer>  indicateur de résolution (0: off, 1: on) 
!>E  xdate            : <DBLE>     longitude solaire "Ls" (si choice_date=m)
!>E  localtime        : <real>     heure locale à longitude xlon (si choice_date=m)
!>E  dust             : <integer>  scénario de poussières/UV solaire
!>E  perturkey        : <integer>  type de perturbation
!>E  seedin           : <real>     graine du générateur aléatoire (pour perturkey=1,2,3 or 4)
!>E  gwlength         : <real>     longueur d'onde des ondes de gravité (pour perturkey=3 or 4)
!>E  extvarkey        : <integer>  option d'écriture des sorties supplémentaires (1: toutes les variables, 0: sorties simples)
!>S  pres             : <real>     pression
!>S  ro               : <real>     masse volumique
!>S  temp             : <real>     température atmosphérique
!>S  u                : <real>     vent zonal
!>S  v                : <real>     vent méridional
!>S  meanvar          : <real,DIM=(5)>   valeurs moyennes des 5 variables principales
!>S  extvar           : <real,DIM=(100)> valeurs complémentaires
!>S  seedout          : <real>     graine courante pour les variables aléatoires
!>S  ier              : <integer>  code d'erreur (erreur uniquement)
!
!$Auteur
!  JG PICCINALI (ATOS ORIGIN)
!
!$Version
!  $Id: ui_atm_emcd42.F90 355 2013-02-14 12:16:41Z aadt $
!
!$Historique
!  $Log: ui_atm_emcd42.F90,v $
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.20  2010/11/03 10:50:58  mercadig
!  VERSION::FA-ID:1443:03/11/2010:Correction des formats
!
!  Revision 1.19  2010/11/02 16:16:16  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.18  2010/10/21 13:46:22  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.17  2010/05/31 13:53:13  jlrobin
!  VERSION::FA-ID:1376:31/05/2010:densite en masse volumique
!
!  Revision 1.16  2008/10/28 14:11:39  cml
!  FA-ID 1061 : Ajout du test d erreur apres initialisation de la base
!
!  Revision 1.15  2008/10/03 13:37:03  cml
!  FA-ID 1024 : Correction syntaxe
!
!  Revision 1.14  2008/10/03 07:22:01  cml
!  FA-ID 1010 : Ajout du mot clef fmt au differents read des utilitaires
!
!  Revision 1.13  2008/08/04 13:52:48  gss
!  DM-ID 1058 : (portage g95) initialisation des sorties, suppression de varaibles
!  non utilisées et suppression de labels non utilisés.
!
!  Revision 1.12  2008/07/11 12:08:10  cml
!  FA-ID 1070 : Corrections d\'orthographe dans les cartouches et les messages
!
!  Revision 1.11  2008/07/04 12:24:17  huec
!  FA-ID 1010 : Correction de la syntaxe F90
!
!  Revision 1.10  2008/04/04 17:10:35  vivaresf
!  Version 2.4 : mise à jour des cartouches
!
!  Revision 1.9  2008/04/03 18:02:54  vivaresf
!  FA-ID 664 : correction des cartouches et de la présentation
!
!  Revision 1.8  2008/03/07 16:15:45  vivaresf
!  FA-ID 939 : code retour en erreur supprimé si date martienne (ce n'est aps une erreur
!
!  Revision 1.7  2008/02/29 16:52:22  vivaresf
!  FA-IF 939 : écriture des message d'erreur sur le stderr
!
!  Revision 1.6  2007/11/21 16:49:57  huec
!  DM-ID 698 : Amelioration des moyens de tests COMPAS
!
!  Revision 1.5  2007/11/15 08:36:07  vivaresf
!  DM-ID 808 : modèle EMCD42
!
!  Revision 1.4  2007/11/14 07:50:01  sbd
!  DM-ID 827 suppression variables inutilisees
!
!  Revision 1.3  2007/11/13 14:39:29  vivaresf
!  DM-ID 698 : réels formmatés pour les portages à venir
!
!  Revision 1.2  2007/11/13 09:29:54  sbd
!  DM-ID 751 DM-ID 808 conversion de gwlength de km en m
!
!  Revision 1.1  2007/11/12 18:43:09  sbd
!  DM-ID 751 DM-ID 808 creation utilitaire du modele emcd42
!
!  Revision 1.0  2007/10/31 00:00:00  jpi
!  DM-ID 551 creation utilitaires pour modele emcd42
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
  use cps_modele_emcd42
  use cps_acces

  implicit none

! SVN Source File Id
  character(len=256), parameter :: SVN_VER = &
   '$Id: ui_atm_emcd42.F90 355 2013-02-14 12:16:41Z aadt $'


  ! entrées
  character(len=1) :: choice_date !  character :: choice_date*1
  integer :: month,day,year,hour,minute,second ! for Earth date input
  integer :: zkey ! flag to choose the type of z coordinates
  real :: xz     ! value of the z coordinate
  real :: xlon   ! east longitude (degrees)
  real :: xlat   ! north latitude (degrees)
  integer :: hireskey ! high resolution flag (0: off, 1: on) 
  integer :: datekey  ! date flag (0: Earth date 1: Mars date)
  double precision :: xdate ! Julian date (if datekey=0) or
  ! solar longitude Ls (if datekey=1)
  real :: localtime ! local time at longitude xlon (only if datekey=1)
  character(LEN=80) :: dset ! path to MCD datasets; unset here
  ! (ie: defaults to MCD_DATA/)
  integer :: dust  ! dust and solar EUV scenario
  integer :: perturkey ! perturbation type
  real :: seedin  ! random generator seed and flag (if perturkey=1,2,3 or 4)
  ! coefficient to multiply std. dev. by (if perturkey=5)
  real :: gwlength ! Gravity wave wavelength (needed if perturkey=3 or 4)
  integer :: extvarkey ! extra output variables (1: yes, 0: no)

  !
  ! sorties
  real :: pres  ! atmospheric pressure
  real :: ro    ! atmospheric density
  real :: temp  ! atmospheric temperature
  real :: u     ! zonal wind
  real :: v     ! meridional wind
  real :: meanvar(5)  ! unperturbed values of main meteorological variables
  real :: extvar(100) ! extra output variables
  real :: seedout     ! current value of random generator seed index
  integer :: ier ! call_mcd status (=0 if all went well)

  !
  ! variables locales
  integer :: i 

  ! Jeu de données :
  !.        Un ou plusieurs blancs pour avoir les données par défaut (répertoire
  !         distribué par COMPAS)
  !.     ou Répertoire des données EMCD avec slash final (e.g. '/dir/path/'),

  type(MSP_MESSAGE) :: messages
  integer :: noptions, ii, jj, ierfin
  character (len=256)            :: sLine
  character(len=60),dimension(50):: l_opt
  logical :: logchoice_date,loglocaltime
  logical :: logday,logmonth,logyear,loghour,logminute,logsecond
  logical :: logzkey,logxz,loghireskey,logxlat,logxlon
  logical :: logdust,logperturkey,logseedin,loggwlength,logextvarkey

  integer :: numfich=11

  ! initialisation

  ! initialisation
  call cps_init_utilisateur()
  if ( MSP_gen_messages("ui_atm_emcd42") ) then
     ! Si l'initialisation échoue, on arrête l'utilitaire
     call MSP_recuperer_message(nb_mes=MSP_tous_messages, message=messages)
     call MSP_afficher_message(message=messages, unit=0)
     call MSP_effacer_message()
     stop
  endif

  !  init_atm = .true.
  dset = " "

  logchoice_date=.false.
  loglocaltime=.false.
  logday=.false.
  logmonth=.false.
  logyear=.false.
  loghour=.false.
  logminute=.false.
  logsecond=.false.
  logzkey=.false.
  logxz=.false.
  loghireskey=.false.
  logxlat=.false.
  logxlon=.false.
  logdust=.false.
  logperturkey=.false.
  logseedin=.false.
  loggwlength=.false.
  logextvarkey=.false.

  extvar(1:100) = 0.
  meanvar(1:5) = 0.
  pres = 0.
  seedout = 0.
  ro = 0.
  temp = 0.
  u = 0.
  v = 0.

  ! Lecture des paramètres (en ligne ou sur fichier)

  call ui_test_arguments ("-h", "-fic", ierfin)

  ! lecture effective
  if(ierfin.eq.1) then
     call ui_ecrire_help("ui_atm_emcd42")
     goto 999
  elseif(ierfin.eq.0) then
     call ui_lire_options(noptions, l_opt)
     open(unit=numfich, file=trim(l_opt(2)))
!    Reading arguments from *.don file (" " separator)
     read(unit=numfich,fmt='(A)') sLine
     sLine=trim(adjustl(sLine))
     do ii=1,36
       jj = 1
       do while (sLine(jj:jj) /= " ")
         jj = jj + 1
       end do
       l_opt(ii)=trim(adjustl(sLine(:jj-1)))
       sLine=trim(adjustl(sLine(jj+1:)))
     end do
     noptions=18
  else
     ! Lecture des arguments :
     call ui_lire_options(noptions, l_opt)
     if (noptions == 0) then
        call ui_ecrire_help("ui_atm_emcd42")
        goto 999
     endif
  endif

  do ii=1, noptions

     if (l_opt(2*ii-1)=="-choice_date") then
        read(l_opt(2*ii), fmt=*) choice_date
        logchoice_date = .true.

     elseif (l_opt(2*ii-1)=="-localtime") then
        read(l_opt(2*ii), fmt=*) localtime
        loglocaltime = .true.

     elseif (l_opt(2*ii-1)=="-day") then
        read(l_opt(2*ii), fmt=*) day
        logday = .true.

     elseif (l_opt(2*ii-1)=="-month") then
        read(l_opt(2*ii), fmt=*) month
        logmonth = .true.

     elseif (l_opt(2*ii-1)=="-year") then
        read(l_opt(2*ii), fmt=*) year
        logyear = .true.

     elseif (l_opt(2*ii-1)=="-hour") then
        read(l_opt(2*ii), fmt=*) hour
        loghour = .true.

     elseif (l_opt(2*ii-1)=="-minute") then
        read(l_opt(2*ii), fmt=*) minute
        logminute = .true.

     elseif (l_opt(2*ii-1)=="-second") then
        read(l_opt(2*ii), fmt=*) second
        logsecond = .true.

     elseif (l_opt(2*ii-1)=="-zkey") then
        read(l_opt(2*ii), fmt=*) zkey
        logzkey = .true.

     elseif (l_opt(2*ii-1)=="-xz") then
        read(l_opt(2*ii), fmt=*) xz
        logxz = .true.

     elseif (l_opt(2*ii-1)=="-hireskey") then
        read(l_opt(2*ii), fmt=*) hireskey
        loghireskey = .true.

     elseif (l_opt(2*ii-1)=="-xlat") then
        read(l_opt(2*ii), fmt=*) xlat
        logxlat = .true.

     elseif (l_opt(2*ii-1)=="-xlon") then
        read(l_opt(2*ii), fmt=*) xlon
        logxlon = .true.

     elseif (l_opt(2*ii-1)=="-dust") then
        read(l_opt(2*ii), fmt=*) dust
        logdust = .true.

     elseif (l_opt(2*ii-1)=="-perturkey") then
        read(l_opt(2*ii), fmt=*) perturkey
        logperturkey = .true.

     elseif (l_opt(2*ii-1)=="-seedin") then
        read(l_opt(2*ii), fmt=*) seedin
        logseedin = .true.

     elseif (l_opt(2*ii-1)=="-gwlength") then
        read(l_opt(2*ii), fmt=*) gwlength
        loggwlength = .true.
        ! conversion de km (ihm en km) à m (modele en m)
        gwlength=gwlength * 1000.

     elseif (l_opt(2*ii-1)=="-extvarkey") then
        read(l_opt(2*ii), fmt=*) extvarkey
        logextvarkey = .true.

     endif
  enddo

  ! corps du programme

  ! Initialisations preliminaires

  if( logchoice_date.and.loglocaltime &
       .and.logday.and.logmonth.and.logyear.and.loghour.and.logminute &
       .and.logsecond .and.logzkey.and.logxz.and.loghireskey.and.logxlat &
       .and.logxlon .and.logdust.and.logperturkey.and.logseedin &
       .and.loggwlength.and.logextvarkey) then

     ! erreur sur l'init
     if (MSP_erreur) goto 999

     call affichage_in(choice_date,datekey,localtime,&
          day,month,year,hour,minute,second,ier,xdate)

     ! appel effectif         
     call cps_atmemcd_42(zkey,xz,xlon,xlat,hireskey, &
            datekey,xdate,localtime,dset,dust, &
            perturkey,seedin,gwlength,extvarkey, & 
            pres,ro,temp,u,v,meanvar,extvar,seedout,ier)

     ! erreur dans l'appel
     if (MSP_erreur) goto 999       

     ! sortie écran
     ! affichage_resultats
     call affichage_resultats(ier,pres,ro,temp,u,v,meanvar,extvar)
     ! nettoyage memoire
     call cps_atmemcd_42_close()
  endif

999 continue
  if (MSP_PROBLEME) then
     call MSP_recuperer_message (message=messages,nb_mes=MSP_tous_messages)
     call MSP_afficher_message (message=messages,unit=0)
  endif

    call cps_atmemcd_42_close()
    call cps_close_utilisateur()

contains

  subroutine affichage_in(choice_date,datekey,localtime,&
       day,month,year,hour,minute,second,ier,xdate)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  affichage_in
!
!$Resume
!   Conversion des entrées du calcul
!
!$Description
!   Conversion des entrées du calcul
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call affichage_in(choice_date,datekey,localtime,&
!.            day,month,year,hour,minute,second,ier,xdate)
!.    character(LEN=1) :: choice_date
!.    integer :: datekey
!.    real :: localtime
!.    integer :: day,month,year,hour,minute,second
!.    integer :: ier
!.    double precision :: xdate
!
!$Arguments
!>E     choice_date  :<LEN=1>     type de date (e : terrestre, m : martienne)
!>S     datekey      :<integer>   clé pour le type de date (e=>0, m=>1)
!>E/S   localtime    :<real> 
!>E     day          :<integer>   | 
!>E     month        :<integer>   | 
!>E     year         :<integer>   | entrées pour les dates terrestre
!>E     hour         :<integer>   | 
!>E     minute       :<integer>   | 
!>E     second       :<integer>   | 
!>S     ier          :<integer>   code retour
!>S     xdate        :<DBLE>      date julienne
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

    character(LEN=1), INTENT(IN)::choice_date
    integer, INTENT(OUT)::datekey
    real, INTENT(INOUT)::localtime
    integer, INTENT(IN)::day,month,year,hour,minute,second
    !
    integer, INTENT(OUT)::ier
    double precision, INTENT(OUT)::xdate
    
    if (choice_date.eq.'e') then       ! Earth date
       datekey=0
       localtime=0. !compulsary with earth date
       call julian(month,day,year,hour,minute,second,ier,xdate)
    else if (choice_date.eq.'m')  then     ! Mars date
       ier=0
       xdate=0.d0
       datekey=1
    end if

  end subroutine affichage_in

  subroutine affichage_resultats(ier,pres,ro,temp,u,v,meanvar,extvar)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  affichage_resultats
!
!$Resume
!   Affichage des résultats du calcul
!$Description
!   Affichage des résultats du calcul
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call affichage_resultats(ier,pres,ro,temp,u,v,meanvar,extvar)
!.    real :: pres 
!.    real :: ro 
!.    real :: temp 
!.    real :: u 
!.    real :: v 
!.    real :: meanvar(5) 
!.    real :: extvar(100) 
!.    integer :: ier 
!
!$Arguments
!>E     ier      :<integer>          code retour
!>E     pres     :<real>             pression
!>E     ro       :<real>             masse volumique
!>E     temp     :<real>             température
!>E     u        :<real>             vent zonal
!>E     v        :<real>             vent méridional
!>E     meanvar  :<real,DIM=(5)>     variables moyennes
!>E     extvar   :<real,DIM=(100)>   variables complémentaires
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
    real, INTENT(IN) :: pres  ! atmospheric pressure
    real, INTENT(IN) :: ro    ! atmospheric density
    real, INTENT(IN) :: temp  ! atmospheric temperature
    real, INTENT(IN) :: u     ! zonal wind
    real, INTENT(IN) :: v     ! meridional wind
    real, INTENT(IN) :: meanvar(5)  ! unperturbed values of main meteorological variables
    real, INTENT(IN) :: extvar(100) ! extra output variables
    integer, INTENT(IN) :: ier ! call_mcd status (=0 if all went well)

    if (ier.eq.0) then
       write(*,'(''Pression        (Pa)    = '',1pe12.2)') pres
       write(*,'(''Masse volumique (kg/m3) = '',1pe12.2)') ro
       write(*,'(''Température     (K)     = '',1pe12.2)') temp
       write(*,'(''Vent zonal      (m/s)   = '',1pe12.2)') u
       write(*,'(''Vent méridional (m/s)   = '',1pe12.2)') v
       write(*,'(a1)') ' '
       do i=1,5
          write(*,'(''Meanvar('',i2,'')             = '',1pe12.2)') i,meanvar(i)
       end do
       write(*,'(a1)') ' '
       if(extvarkey.ne.0) then 
          do i = 1,50 ! write all 50 extra variables
             write(*,'(''Extvar('',i2,'')              = '',1pe12.2)') i,extvar(i)
          enddo
       else ! write the first 7 extvar()
          do i=1,7
             write(*,'(''Extvar('',i2,'')              = '',1pe12.2)') i,extvar(i)
          enddo
       end if
    else
       write(0,'(a)')'Erreur dans cps_atmemcd_42'
       write(0,'(a,i2)')'         code retour : ', ier
    end if

  end subroutine affichage_resultats

end program ui_atm_emcd42
