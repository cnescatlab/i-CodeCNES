program ui_atm_emcd43

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_atm_emcd43
!
!$Resume
!  Utilitaire pour le fonctionnement du modèle d'atmosphère EMCD43
!
!$Description
!  Utilitaire pour le fonctionnement du modèle d'atmosphère EMCD43
!
!$Usage
!.     ui_atm_emcd43 -choice_date choice_date -localtime localtime 
!.              -day day -month month -year year -hour hour -minute minute -second second 
!.              -zkey zkey -xz xz -hireskey hireskey -xlat xlat -xlon xlon 
!.              -dust dust -perturkey perturkey -seedin seedin -gwlength gwlength -extvarkey extvarkey
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
!>E  gwlength         : <real>     longueur d'onde des ondes de gravité (pour perturkey=3 or 4) (km)
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
!  Cédric MARTEL (ATOS Origin)
!
!$Version
!  $Id: ui_atm_emcd43.F90 355 2013-02-14 12:16:41Z aadt $
!
!$Historique
!  $Log: ui_atm_emcd43.F90,v $
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.12  2010/11/03 10:50:58  mercadig
!  VERSION::FA-ID:1443:03/11/2010:Correction des formats
!
!  Revision 1.11  2010/11/02 16:16:16  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.10  2010/10/21 13:46:22  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.9  2010/10/12 09:52:53  ogarat
!  VERSION::FA-ID:1447:12/10/2010:Densite en Masse volumique
!
!  Revision 1.8  2008/12/18 14:07:19  cml
!  FA-ID 1149 : Correction mineure du cartouche de l utilitaire
!
!  Revision 1.7  2008/10/29 13:02:28  cml
!  DM-ID 1024 : Correction d une mauvaise declaration d une chaine
!
!  Revision 1.6  2008/10/28 14:11:39  cml
!  FA-ID 1061 : Ajout du test d erreur apres initialisation de la base
!
!  Revision 1.5  2008/10/20 13:20:19  cml
!  DM-ID 1091 : Ajout d une liberation de memoire manquante
!
!  Revision 1.4  2008/10/03 07:22:01  cml
!  FA-ID 1010 : Ajout du mot clef fmt au differents read des utilitaires
!
!  Revision 1.3  2008/08/04 15:34:28  cml
!  DM-ID 1091 : Longueur d'onde en km et correction Warning a la compilation
!
!  Revision 1.2  2008/07/29 08:02:56  cml
!  DM-ID 1091 : Passage des variables reelles du modele en pm_reel
!
!  Revision 1.1  2008/07/24 08:54:50  cml
!  DM-ID 1091 : Mise en place de l'utilitaire de test du modele EMCD 4.3
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

  use msp_gestion_erreur
  use ui_io
  use cps_acces
  use cps_modele_emcd43

  implicit none

! SVN Source File Id
  character(len=256), parameter :: SVN_VER = &
   '$Id: ui_atm_emcd43.F90 355 2013-02-14 12:16:41Z aadt $'


  character(len=1) ::  choice_date
  integer :: month,day,year,hour,minute,second ! for Earth date input
  integer  :: i 

  ! -------------------------------------------------------------------
  ! Variables utilisées par CALL_MCD 
  ! Entrées :
  integer :: zkey ! flag to choose the type of z coordinates
  real(pm_reel) :: xz     ! value of the z coordinate
  real(pm_reel) :: xlon   ! east longitude (degrees)
  real(pm_reel) :: xlat   ! north latitude (degrees)
  integer :: hireskey ! high resolution flag (0: off, 1: on) 
  integer :: datekey  ! date flag (0: Earth date 1: Mars date)
  real(pm_reel) :: xdate ! Julian date (if datekey=0) or
  ! solar longitude Ls (if datekey=1)
  real(pm_reel) :: localtime ! local time at longitude xlon (only if datekey=1)
  character :: dset ! path to MCD datasets; unset here
  ! (ie: defaults to MCD_DATA/)
  integer :: dust  ! dust and solar EUV scenario
  integer :: perturkey ! perturbation type
  real(pm_reel) :: seedin  ! random generator seed and flag (if perturkey=1,2,3 or 4)
  ! coefficient to multiply std. dev. by (if perturkey=5)
  real(pm_reel) :: gwlength ! Gravity wave wavelength (needed if perturkey=3 or 4)
  integer :: extvarkey ! extra output variables (1: yes, 0: no)

  ! Sorties :
  real(pm_reel) :: pres  ! atmospheric pressure
  real(pm_reel) :: ro    ! atmospheric density
  real(pm_reel) :: temp  ! atmospheric temperature
  real(pm_reel) :: u     ! zonal wind
  real(pm_reel) :: v     ! meridional wind
  real(pm_reel) :: meanvar(5)  ! unperturbed values of main meteorological variables
  real(pm_reel) :: extvar(100) ! extra output variables
  real(pm_reel) :: seedout     ! current value of random generator seed index
  integer :: ier ! call_mcd status (=0 if all went well)


  ! -------------------------------------------------------------------
  ! Messages en sorties des fonctions
  type(MSP_MESSAGE) :: messages
  ! TODO
  integer :: noptions, ii, jj, ierfin
  character (len=256)            :: sLine
  character(len=60),dimension(50):: l_opt

  ! Flags indiquant si l'option est belle et bien spécifiée.
  ! On ne tolerer que deux cas : aucune ou toutes les valeurs sont spécifiées
  logical :: logchoice_date,loglocaltime
  logical :: logday,logmonth,logyear,loghour,logminute,logsecond
  logical :: logzkey,logxz,loghireskey,logxlat,logxlon
  logical :: logdust,logperturkey,logseedin,loggwlength,logextvarkey

  ! Unite logique pour le fichier
  integer :: numfich=11

  ! On ne choisit pas les données d'entrée, le chemin sera celui par défaut
  dset = " "

  ! Init des variables qui vont être remplies
  extvar(1:100) = 0.
  meanvar(1:5) = 0.
  pres = 0.
  seedout = 0.
  ro = 0.
  temp = 0.
  u = 0.
  v = 0.

  ! Initialisation des flags a false, seule la lecture les fait passer à true
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

  ! initialisation
  call cps_init_utilisateur()
  if ( MSP_gen_messages("ui_atm_emcd43") ) then
     ! Si l'initialisation échoue, on arrête l'utilitaire
     call MSP_recuperer_message(nb_mes=MSP_tous_messages, message=messages)
     call MSP_afficher_message(message=messages, unit=0)
     call MSP_effacer_message()
     stop
  endif

  ! Lecture des paramètres (en ligne ou sur fichier)
  call ui_test_arguments ("-h", "-fic", ierfin)

  ! lecture effective
  if(ierfin.eq.1) then
     call ui_ecrire_help("ui_atm_emcd43")
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
        call ui_ecrire_help("ui_atm_emcd43")
        goto 999
     endif
  endif


  ! ---------------------------------------------
  !  Analyse des options fournies par l'utilisateur
  ! ---------------------------------------------
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
        ! Besoin de conversion de km (ihm en km) à m (modele en m)
        gwlength = 1000.0_pm_reel * gwlength
     elseif (l_opt(2*ii-1)=="-extvarkey") then
        read(l_opt(2*ii), fmt=*) extvarkey
        logextvarkey = .true.

     endif
  enddo

     ! erreur sur l'init
     if (MSP_erreur) goto 999


  ! ---------------------------------------------
  ! Si toutes les valeurs sont bien specifiées,
  ! Alors on lance la routine de calcul
  ! ---------------------------------------------
  if( logchoice_date.and.loglocaltime &
       .and.logday.and.logmonth.and.logyear.and.loghour.and.logminute &
       .and.logsecond .and.logzkey.and.logxz.and.loghireskey.and.logxlat &
       .and.logxlon .and.logdust.and.logperturkey.and.logseedin &
       .and.loggwlength.and.logextvarkey) then

      ! Conversion de la date calendaire en jour julien fractionnaire
      call affichage_in(choice_date,datekey,localtime,&
          day,month,year,hour,minute,second,ier,xdate)

      ! appel effectif         
      call cps_atmemcd_43(zkey,xz,xlon,xlat,hireskey, &
                   datekey,xdate,localtime,dset,dust, &
                   perturkey,seedin,gwlength,extvarkey, &
                   pres,ro,temp,u,v,meanvar,extvar,seedout,ier)

     ! erreur dans l'appel
     if (MSP_erreur) goto 999       

     ! sortie écran
     ! affichage_resultats
     call affichage_resultats(ier,pres,ro,temp,u,v,meanvar,extvar)


  endif

  ! ---------------------------------------------
  ! Traitement des erreurs 
  ! ---------------------------------------------

999 continue
  if (MSP_PROBLEME) then
     call MSP_recuperer_message (message=messages,nb_mes=MSP_tous_messages)
     call MSP_afficher_message (message=messages,unit=0)
  endif
 
  ! Liberation de la mémoire interne au modèle
  call cps_atmemcd_43_close()

  ! Fermeture de la base
  call cps_close_utilisateur()

  ! ===================================================
  !  Routines internes
  ! ===================================================

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
!  Conversion des données d'entrée au format demandé par cps_atmemcd_43
!
!$Description
!  Conversion des données d'entrée au format demandé par cps_atmemcd_43
!
!$Auteur
!  Cédric MARTEL (ATOS Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call affichage_in(choice_date,datekey,localtime,&
!.           day,month,year,hour,minute,second,ier,xdate)
!.    character(LEN=1) :: choice_date
!.    integer :: datekey
!.    real(pm_reel) :: localtime
!.    integer :: day,month,year,hour,minute,second
!.    integer :: ier
!.    real(pm_reel) :: xdate
!
!$Arguments
!>E     choice_date  :<LEN=1>      date terrestre (e) ou martienne (m)
!>S     datekey      :<integer>    clé pour le type de date (e=>0, m=>1)
!>E/S   localtime    :<pm_reel>    heure locale à longitude xlon (si choice_date=m)
!>E     day          :<integer>    jour (si choice_date=e)
!>E     month        :<integer>    mois (si choice_date=e)
!>E     year         :<integer>    année (si choice_date=e)  
!>E     hour         :<integer>    heure (si choice_date=e) 
!>E     minute       :<integer>    minute (si choice_date=e) 
!>E     second       :<integer>    seconde (si choice_date=e)
!>S     ier          :<integer>    code d'erreur (erreur uniquement)
!>S     xdate        :<pm_reel>    date en jours juliens fractionnaires
!
!$Routines
!- julian
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

    ! Entrées
    character(LEN=1), INTENT(IN)::choice_date
    integer, INTENT(OUT)::datekey
    real(pm_reel), INTENT(INOUT)::localtime
    integer, INTENT(IN)::day,month,year,hour,minute,second
    
    ! Sorties
    integer, INTENT(OUT)::ier
    real(pm_reel), INTENT(OUT)::xdate
    
    ! Si on a spécifié une date Tarrestre
    if (choice_date.eq.'e') then 
       datekey=0
       localtime=0._pm_reel 
       ! Appel à la méthode de du module cps_modele_emcd43
       call julian(month,day,year,hour,minute,second,ier,xdate)

    ! Si on a spécifié une date martienne
    else if (choice_date.eq.'m')  then   
       ier=0
       xdate=0._pm_reel
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
!   Affichage des résultats du calcul du modèle d'atmosphère
!
!$Description
!   Affichage des résultats du calcul du modèle d'atmosphère
!
!$Auteur
!  Cédric MARTEL (ATOS Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call affichage_
!.    real(pm_reel) :: pres 
!.    real(pm_reel) :: ro 
!.    real(pm_reel) :: temp 
!.    real(pm_reel) :: u 
!.    real(pm_reel) :: v 
!.    real(pm_reel) :: meanvar(5) 
!.    real(pm_reel) :: extvar(100) 
!.    integer :: ier 
!
!$Arguments
!>E     ier      :<integer>             code retour 
!>E     pres     :<pm_reel>             pression
!>E     ro       :<pm_reel>             masse volumique
!>E     temp     :<pm_reel>             température
!>E     u        :<pm_reel>             vent zonal
!>E     v        :<pm_reel>             vent méridional
!>E     meanvar  :<pm_reel,DIM=(5)>     variables moyennes
!>E     extvar   :<pm_reel,DIM=(100)>   variables complémentaires
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
    real(pm_reel), INTENT(IN) :: pres  ! atmospheric pressure
    real(pm_reel), INTENT(IN) :: ro    ! atmospheric density
    real(pm_reel), INTENT(IN) :: temp  ! atmospheric temperature
    real(pm_reel), INTENT(IN) :: u     ! zonal wind
    real(pm_reel), INTENT(IN) :: v     ! meridional wind
    real(pm_reel), INTENT(IN) :: meanvar(5)  ! unperturbed values of main meteorological variables
    real(pm_reel), INTENT(IN) :: extvar(100) ! extra output variables
    integer, INTENT(IN) :: ier ! call_mcd status (=0 if all went well)

      if (ier.eq.0) then
         write(*,'(''Pression        (Pa)    = '',g21.12)') pres
         write(*,'(''Masse volumique (kg/m3) = '',g21.12)') ro
         write(*,'(''Température     (K)     = '',g21.12)') temp
         write(*,'(''Vent zonal      (m/s)   = '',g21.12)') u
         write(*,'(''Vent méridional (m/s)   = '',g21.12)') v
         write(*,'(a1)') ' '
         do i=1,5
            write(*,'(''Meanvar('',i2,'')             = '',g21.12)') i,meanvar(i)
         end do
         write(*,'(a1)') ' '
         if(extvarkey.ne.0) then 
          do i = 1,50 ! write all 50 extra variables
           write(*,'(''Extvar('',i2,'')              = '',g21.12)') i,extvar(i)
          enddo
         else ! write the first 7 extvar()
          do i=1,7
           write(*,'(''Extvar('',i2,'')              = '',g21.12)') i,extvar(i)
          enddo
         end if
      else
         write(0,'(a)')'Erreur dans cps_atmemcd_43'
         write(0,'(a,i2)')'         code retour : ', ier
      end if

  end subroutine affichage_resultats

end program ui_atm_emcd43
