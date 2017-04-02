program ui_atm_emcd41

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_atm_emcd41
!
!$Resume
! Utilitaire pour le fonctionnement du modèle d'atmosphère EMCD41
!$Description
! 
! Utilitaire pour le fonctionnement du modèle d'atmosphère martien EMCD41
!
!
! Usage
!.     ui_atm_emcd41 -xz xz -xlat xlat -xlon xlon -xdate xdate 
!.                   -scena scena -typper1 typper1 -typper2 typper2 
!.                   -invar invar -zkey zkey -ikey ikey
!. ou
!.    ui_atm_emcd41 -fic fichier_contenant_la_sequence_d_appel 
!
!>E xz    : <PM_REEL>  hauteur  / surface (m)
!>E xlat  : <PM_REEL>  latitude (rad)
!>E xlon  : <PM_REEL>  longitude Est (rad)
!>E xdate : <PM_REEL>  date
!.             si xdate > 0 : jours juliens terrestre
!.             si xdate < 0 : 
!. date martienne = -(Int(solar longitude [deg] ) + localtime/100)
!. (ex : xdate = -90.12 correspond à Ls=90 ; LT=12:00) 
!. (ex : xdate = -173.18 correspond à Ls=173 ; LT=18:00) 
!>E scena : <integer>  scénario de poussières : 
!.                           1 = modèle MGS,
!.                           2 = modèle Viking,
!.                           3 = modèle faible,
!.                           4 = tempête (tau=2),
!.                           5 = tempête (tau=5).
!>E typper: <PM_REEL, dim=2> type de perturbation
!.   aucune : typper(1) = 1. typper(2) = 0.
!.   grande échelle : typper(1) = 2. typper(2) = graine pour les variables aléatoires (voir seedout)
!.   petite échelle : typper(1) = 3. typper(2) = graine pour les variables aléatoires
!.   grande et petite échelle : typper(1) = 4. typper(2) = graine pour les variables aléatoires
!.   n fois l'écart type : typper(1) = 5. typper(2) = n
!>E invar : <double precision> valeurs contrôlant la variabilité des modèles
!.     invar = longueur d'onde des perturbations dues aux ondes de gravités (m)
!.     invar = 0 pour avoir la valeur par défaut (16000m)
!>E zkey  : <integer> type de hauteur / surface                    
!.                        1 = rayon depuis le centre de la planète (m) 
!.                        2 = hauteur depuis l'aréoïde (m) (MOLA)      
!.                        3 = hauteur depuis la surface (m)            
!.                        4 = niveau de pression (Pa)
!>E ikey  : <integer> type de sortie
!.                 0 = pression, masse volumique, température, vent
!.                 1 = calcul aussi les variables statistiques dans extvar
!>S seedout : <PM_REEL> graine des variables aléatoires pour l'appel suivant
!>S pres    : <PM_REEL>   pression (Pa)
!>S ro      : <PM_REEL>   masse volumique (kg/m3)
!>S temp    : <PM_REEL>   température (K)
!>S ventu   : <PM_REEL>   composante zonale du vent (Est-Ouest)
!>S ventv   : <PM_REEL>   composante méridionale du vent (Nord-Sud)
!>S meanvar : <PM_REEL, dim=5> tableau des valeurs moyennes
!.                 meanvar(1) = pression moyenne
!.                 meanvar(2) = masse volumique moyenne
!.                 meanvar(3) = température moyenne
!.                 meanvar(4) = composante zonale moyenne du vent
!.                 meanvar(5) = composante méridionale moyenne du vent
!>S extvar : <double pre.> tableau de 50 variables statistiques        *
!.           extvar(1) = dist. radiale depuis centre de la planète (m) *
!.           extvar(2) = altitude depuis l'aréoïde, géoïde martien (m) *
!.           extvar(3) = altitude depuis la surface (m)                *
!.           extvar(4) = hauteur orographique (m)                      *
!.           extvar(5) = longitude aréocentrique de mars (deg)         *
!.           extvar(6) = heure locale solaire (hrs)                    *
!.           extvar(7) = temps universel solaire (LST a lon=0) (hrs)   *
!.           extvar(8) = capacité calorifique de l'air Cp (J kg-1 K-1) *
!.           extvar(9) = rapport calorifique spécifique gamma = Cp/Cv  *
!.           extvar(10) = variation journalière masse volumique RMS (kg/m^3)    *
!.           extvar(11) = écart type total saisonnier masse volumique (kg/m^3)  *
!.           extvar(12) = perturbation moyenne de la masse volumique (kg/m^3)   *
!.           extvar(13) = échelle de hauteur H(p) (km)                  *
!.           extvar(14) = inutilisé ( = 0 )                             *
!.           extvar(15) = température de surface (K)                    *
!.           extvar(16) = température moyenne max de surface (K)        *
!.           extvar(17) = température moyenne min de surface (K)        *
!.           extvar(18) = variation journalière température surface (K) *
!.           extvar(19) = pression moyenne de surface (Pa)              *
!.           extvar(20) = pression moyenne max de surface (Pa)          *
!.           extvar(21) = pression moyenne min de surface (Pa)          *
!.           extvar(22) = variation journalière pression surface (Pa)   *
!.           extvar(23) = variation journalière température surface (K) *
!.           extvar(24) = variation journalière vent zonal (m/s)        *
!.           extvar(25) = variation journalière vent méridional (m/s)   *
!.           extvar(26) = composante verticale du vent (m/s)            *
!.           extvar(27) = variation journalière vent vertical (m/s)     *
!.           extvar(28) = pert. petite échelle onde gravité (kg/m^3)    *
!.           extvar(29) = q2 : énergie cinétique turbulence (m2/s2)    *
!.           extvar(30) = inutilisé ( = 0 )                             *
!.           extvar(31) = flux thermique IR de surface (W/m2)           *
!.           extvar(32) = flux solaire de surface (W/m2)                *
!.           extvar(33) = flux thermique IR d'espace (W/m2)             *
!.           extvar(34) = flux solaire réfléchi dans l'espace (W/m2)   *
!.           extvar(35) = CO2 : niveau gelé en surface (kg/m2)          *
!.           extvar(36) = DOD : profondeur visible optique colonne      *
!.           extvar(37) = rapport du mélange poussière / masse (kg/kg)  *
!.           extvar(38) = DOD : variation journalière RMS               *
!.           extvar(39) = DOD : écart type total saisonnier de masse volumique  *
!.           extvar(40) = colonne de vapeur (kg/m2)                     *
!.           extvar(41) = rapport mélange volume eau / vapeur (mol/mol) *
!.           extvar(42) = colonne eau glacée (kg/m2)                    *
!.           extvar(43) = rapport mélange eau / glace (mol/mol)         *
!.           extvar(44) = [O3]  rapport mélange volumique (mol/mol)     *
!.           extvar(45) = [CO2] rapport mélange volumique (mol/mol)     *
!.           extvar(46) = [O]   rapport mélange volumique (mol/mol)     *
!.           extvar(47) = [N2]  rapport mélange volumique (mol/mol)     *
!.           extvar(48) = [CO]  rapport mélange volumique (mol/mol)     *
!.           extvar(49) = R : constante moléculaire du gaz (J K-1 kg-1) *
!.           extvar(50) = estimation viscosité de l'air (N s m-2) 
!
!$Auteur
! Sandrine Baudrand(ATOS ORIGIN)
!
!$Version
!  $Id: ui_atm_emcd41.F90 355 2013-02-14 12:16:41Z aadt $
!
!$Historique
!  $Log: ui_atm_emcd41.F90,v $
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.17  2010/11/03 10:50:58  mercadig
!  VERSION::FA-ID:1443:03/11/2010:Correction des formats
!
!  Revision 1.16  2010/11/02 16:16:16  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.15  2010/10/21 13:46:22  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.14  2010/05/31 13:53:13  jlrobin
!  VERSION::FA-ID:1376:31/05/2010:densite en masse volumique
!
!  Revision 1.13  2009/09/09 07:55:49  cmartel
!  FA-ID 1150 : Correction mineures sur les sorties des utilitaires
!
!  Revision 1.12  2008/10/28 14:11:38  cml
!  FA-ID 1061 : Ajout du test d erreur apres initialisation de la base
!
!  Revision 1.11  2008/10/03 07:22:00  cml
!  FA-ID 1010 : Ajout du mot clef fmt au differents read des utilitaires
!
!  Revision 1.10  2008/08/04 13:50:57  gss
!  DM-ID 1058 : (portage g95) initialisation des sorties et suppression d'un
!  label non utilisé.
!
!  Revision 1.9  2008/07/11 12:08:04  cml
!  FA-ID 1070 : Corrections d\'orthographe dans les cartouches et les messages
!
!  Revision 1.8  2008/07/04 12:24:26  huec
!  FA-ID 1010 : Correction de la syntaxe F90
!
!  Revision 1.7  2008/04/04 17:10:35  vivaresf
!  Version 2.4 : mise à jour des cartouches
!
!  Revision 1.6  2008/04/03 18:02:53  vivaresf
!  FA-ID 664 : correction des cartouches et de la présentation
!
!  Revision 1.5  2007/11/21 11:49:20  sbd
!  DM-ID 797 amelioration affichage resultats
!
!  Revision 1.4  2007/11/12 08:27:26  sbd
!  DM-ID 797 ajout commentaires et amelioration affichage des resultats extvar
!
!  Revision 1.3  2007/11/09 07:47:20  sbd
!  DM-ID 797 unite utilisateur en km
!
!  Revision 1.2  2007/10/31 17:59:02  sbd
!  DM-ID 797 modification en tete et variables appels
!
!  Revision 1.1  2007/10/30 11:09:24  sbd
!  DM-ID 797 creation utilitaires pour terre mars et venus
!
!
!#V
!#
!
!#V
      
!#
!
!
!#V
!- mslib
!- msp_gestion_erreur
!- ui_io
!- cps_modele_emcd41
!- cps_acces
!#
!
!#V
!- MSP_ajouter_fichier_message
!- ui_test_arguments
!- ui_ecrire_help
!- ui_lire_options
!- cps_modele_emcd41
!- MSP_recuperer_message
!- MSP_afficher_message
!#
!
!$FinHistorique
!
!$Include
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
  use cps_modele_emcd41
  use cps_acces

  implicit none

! SVN Source File Id
  character(len=256), parameter :: SVN_VER = &
   '$Id: ui_atm_emcd41.F90 355 2013-02-14 12:16:41Z aadt $'


! entrées
  real(kind=PM_REEL) ::  xz,xlat,xlon
  real(kind=PM_REEL) :: xdate

  integer :: scena
  real(kind=PM_REEL) :: typper(2)
  real(kind=PM_REEL) :: invar
  integer :: ikey, zkey

! variables locales

! Jeu de données :
!.        Un ou plusieurs blancs pour avoir les données par défaut (répertoire
!         distribué par COMPAS)
!.     ou Répertoire des données EMCD avec slash final (e.g. '/dir/path/'),
!.     ou un lien sur le répertoire des données EMCD  (e.g. 'EMCD_DATA/').
  character(len=80) :: dset

  logical init_atm
  real(kind=PM_REEL) :: seedout
  real(kind=PM_REEL) :: pres
  real(kind=PM_REEL) :: ro
  real(kind=PM_REEL) :: temp
  real(kind=PM_REEL) :: ventu
  real(kind=PM_REEL) :: ventv
  real(kind=PM_REEL) :: meanvar(5)
  real(kind=PM_REEL) :: extvar(50)
  integer :: ier

  type(MSP_MESSAGE) :: messages
  integer :: noptions, ii, jj, ierfin
  character (len=256)            :: sLine
  character(len=60),dimension(50):: l_opt
  logical :: logxz,logxlat,logxlon,logxdate
  logical :: logscena
  logical :: logtypper(2)
  logical :: loginvar,logikey,logzkey

  integer :: numfich=11

! initialisation

  ! initialisation
  call cps_init_utilisateur()
  if ( MSP_gen_messages("ui_atm_emcd41") ) then
     ! Si l'initialisation échoue, on arrête l'utilitaire
     call MSP_recuperer_message(nb_mes=MSP_tous_messages, message=messages)
     call MSP_afficher_message(message=messages, unit=0)
     call MSP_effacer_message()
     stop
  endif

  init_atm = .true.
  dset = " "
  logxz =.false.
  logxlat =.false.
  logxlon =.false.
  logxdate =.false.
  logscena =.false.
  logtypper(1) =.false.
  logtypper(2) =.false.
  loginvar =.false.
  logikey =.false.
  logzkey =.false.

  extvar(1:50) = 0._pm_reel
  meanvar(1:5) = 0._pm_reel
  ro = 0._pm_reel
  pres = 0._pm_reel
  seedout = 0._pm_reel
  temp = 0._pm_reel
  ventu = 0._pm_reel
  ventv = 0._pm_reel

  ier = 0

! Lecture des paramètres (en ligne ou sur fichier)

  call ui_test_arguments ("-h", "-fic", ierfin)

      ! lecture effective
      if(ierfin.eq.1) then
         call ui_ecrire_help("ui_atm_emcd41")
         goto 999
      elseif(ierfin.eq.0) then
         call ui_lire_options(noptions, l_opt)
         open(unit=numfich, file=trim(l_opt(2)))
!        Reading arguments from *.don file (" " separator)
         read(unit=numfich,fmt='(A)') sLine
         sLine=trim(adjustl(sLine))
         do ii=1,20
           jj = 1
           do while (sLine(jj:jj) /= " ")
             jj = jj + 1
           end do
           l_opt(ii)=trim(adjustl(sLine(:jj-1)))
           sLine=trim(adjustl(sLine(jj+1:)))
         end do
         noptions=10
      else
         ! Lecture des arguments :
         call ui_lire_options(noptions, l_opt)
         if (noptions == 0) then
            call ui_ecrire_help("ui_atm_emcd41")
            goto 999
      endif
      endif
      do ii=1, noptions
               if (l_opt(2*ii-1)=="-xz") then
                  read(l_opt(2*ii), fmt=*) xz
                  logxz = .true.
               elseif(l_opt(2*ii-1)=="-xlat") then
                  read(l_opt(2*ii), fmt=*) xlat
                  logxlat=.true.
               elseif(l_opt(2*ii-1)=="-xlon") then
                 read(l_opt(2*ii), fmt=*) xlon
                  logxlon=.true.
               elseif(l_opt(2*ii-1)=="-xdate") then
                  read(l_opt(2*ii), fmt=*) xdate
                  logxdate=.true.
               elseif(l_opt(2*ii-1)=="-scena") then
                  read(l_opt(2*ii), fmt=*) scena
                  logscena=.true.
               elseif(l_opt(2*ii-1)=="-typper1") then
                  read(l_opt(2*ii), fmt=*) typper(1)
                  logtypper(1)=.true.
               elseif(l_opt(2*ii-1)=="-typper2") then
                  read(l_opt(2*ii), fmt=*) typper(2)
                  logtypper(2)=.true.
               elseif(l_opt(2*ii-1)=="-invar") then
                  read(l_opt(2*ii), fmt=*) invar
                  loginvar=.true.
               elseif(l_opt(2*ii-1)=="-ikey") then
                  read(l_opt(2*ii), fmt=*) ikey
                  logikey=.true.
               elseif(l_opt(2*ii-1)=="-zkey") then
                  read(l_opt(2*ii), fmt=*) zkey
                  logzkey=.true.
               endif
            enddo
 
! corps du programme

! Initialisations preliminaires

      if(logxz.and.logzkey.and.logxlat.and.logxlon.and.logxdate &
         .and.logscena.and.logtypper(1).and.logtypper(2)&
         .and.loginvar.and.logikey.and.logzkey) then
           
         ! erreur sur l'init
         if (MSP_erreur) goto 999
         
	 ! conversion de invar de km (unité à ihm)  à m (unite dans le modele)
	 invar = invar * 1000._pm_reel

         ! appel effectif
         
         call cps_atmemcd_41 (xz,zkey,xlat,xlon,xdate,dset,scena,typper,invar, &
              init_atm, seedout,ikey,pres,ro,temp,ventu,ventv,meanvar,extvar,ier)

        
         ! erreur dans l'appel
         if (MSP_erreur) goto 999
       
       
         ! sortie écran	 
	 write(*,'(''Seedout                 = '',g21.12)') seedout
  	 write(*,'(''Pression        (Pa)    = '',e20.10)') pres
  	 write(*,'(''Masse volumique (kg/m3) = '',e20.10)') ro
  	 write(*,'(''Température     (K)     = '',e20.10)') temp
  	 write(*,'(''Vent zonal      (m/s)   = '',e20.10)') ventu
  	 write(*,'(''Vent méridional (m/s)   = '',e20.10)') ventv
  	 write(*,1000) ""
  	 do ii=1,5
     		write(*,'(''Meanvar('',i2,'')             = '',e20.10)') ii,meanvar(ii)
  	 end do
	 if(ikey.eq.1) then
  	 	write(*,1000) ""
  	 	do ii = 1,50
     			write(*,'(''Extvar('',i2.2,'')              = '',e20.10)') ii,extvar(ii)
  	 	enddo
	 endif
      else
         write(0,*) "Erreur : les arguments ne sont pas correctement remplis"

      endif

999 continue
    if (MSP_PROBLEME) then
       call MSP_recuperer_message (message=messages,nb_mes=MSP_tous_messages)
       call MSP_afficher_message (message=messages,unit=0)
    endif

    call cps_atmemcd_41_close()
    call cps_close_utilisateur()

1000 FORMAT(a)

  end program ui_atm_emcd41
