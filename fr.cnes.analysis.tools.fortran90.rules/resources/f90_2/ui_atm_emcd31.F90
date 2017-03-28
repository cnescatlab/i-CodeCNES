program ui_atm_emcd31

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_atm_emcd31
!
!$Resume
! Utilitaire pour le fonctionnement du modèle d'atmosphère EMCD31
!$Description
! 
! Utilitaire pour le fonctionnement du modèle d'atmosphère martien EMCD31
!
!
! Usage
!.     ui_atm_emcd31 -xz xz -xlat xlat -xlon xlon -xdate xdate
!.                   -scena scena -typper1 typper1 -typper2 typper2 
!.                   -invar invar -ikey ikey
!. ou
!.    ui_atm_emcd31 -fic fichier_contenant_la_sequence_d_appel 
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
!.     invar = longueur d'onde des perturbations dues aux ondes de gravité (km)
!.     invar = 0 pour avoir la valeur par défaut (16km)
!>E ikey  : <integer> type de sortie
!.                 0 = pression, masse volumique, température, vent
!.                 1 = calcul aussi les variables statistiques dans extvar
!>S seedout : <PM_REEL> graine des variables aléatoires pour l'appel suivant
!>S pres  : <PM_REEL>   pression (Pa)
!>S ro    : <PM_REEL>   masse volumique (kg/m^3)
!>S temp  : <PM_REEL>   température (K)
!>S ventu : <PM_REEL>   composante zonale du vent (Est-Ouest)
!>S ventv : <PM_REEL>   composante méridionale du vent (Nord-Sud)
!>S meanvar : <PM_REEL, dim=5> tableau des valeurs moyennes
!.                 meanvar(1) = pression moyenne
!.                 meanvar(2) = masse volumique moyenne
!.                 meanvar(3) = température moyenne
!.                 meanvar(4) = composante zonale moyenne du vent
!.                 meanvar(5) = composante méridionale moyenne du vent
!>S extvar : <PM_REEL, dim=25>  tableau de 25 variables statistiques :
!.                 extvar(1) = valeur maximum de la masse volumique (kg/m^3)
!.                 extvar(2) = valeur minimum de la masse volumique (kg/m^3)
!.                 extvar(3) = écart type sur la masse volumique (kg/m^3)
!.                 extvar(4) = perturbations sur la masse volumique (kg/m^3)
!.                 extvar(5) = hauteur à pression "pressure" H(p)
!.                 extvar(6) = hauteur à masse volumique "rho" H(rho) (km)
!.                 extvar(7) = inutilisé
!.                 extvar(8) = inutilisé
!.                 extvar(9) = inutilisé
!.                 extvar(10) = température moyenne à la surface du sol (K)
!.                 extvar(11) = température maximum quotidienne à la surface du sol (K)
!.                 extvar(12) = température minimum quotidienne à la surface du sol (K)
!.                 extvar(13) = perturbations à petites échelles
!                              (ondes de gravité) (kg/m^3)
!.                 extvar(14) = hauteur orographique(m)
!.                 extvar(15) = pression de surface moyenne (Pa)
!.                 extvar(16) = pression surface quotidienne moyenne maximum  (Pa)
!.                 extvar(17) = pression surface quotidienne moyenne minimum (Pa)
!.                 extvar(18) = écart type saisonnier de la pression de surface (Pa)
!.                 extvar(19) = flux LW moyen vers la surface (W/m2)
!.                 extvar(20) = flux SW moyen vers la surface (W/m2)
!.                 extvar(21) = flux LW moyen vers l'espace (W/m2)
!.                 extvar(22) = flux SW moyen vers l'espace (W/m2)
!.                 extvar(23) = longitude aréocentrique de Mars, Ls (deg)
!.                 extvar(24) = heure solaire locale (hrs)
!.                 extvar(25) = temps universel (hrs) (=heure locale à lon=0)

!
!$Auteur
! Sandrine Baudrand(ATOS ORIGIN)
!
!$Version
!  $Id: ui_atm_emcd31.F90 355 2013-02-14 12:16:41Z aadt $
!
!$Historique
!  $Log: ui_atm_emcd31.F90,v $
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.15  2010/11/03 10:50:58  mercadig
!  VERSION::FA-ID:1443:03/11/2010:Correction des formats
!
!  Revision 1.14  2010/11/02 16:16:16  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.13  2010/10/21 13:46:22  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.12  2010/05/31 13:53:13  jlrobin
!  VERSION::FA-ID:1376:31/05/2010:densite en masse volumique
!
!  Revision 1.11  2008/10/28 14:11:38  cml
!  FA-ID 1061 : Ajout du test d erreur apres initialisation de la base
!
!  Revision 1.10  2008/10/03 07:22:00  cml
!  FA-ID 1010 : Ajout du mot clef fmt au differents read des utilitaires
!
!  Revision 1.9  2008/08/04 13:50:26  gss
!  DM-ID 1058 : (portage g95) initialisation des sorties et suppression d'un
!  label non utilisé.
!
!  Revision 1.8  2008/07/11 12:07:57  cml
!  FA-ID 1070 : Corrections d\'orthographe dans les cartouches et les messages
!
!  Revision 1.7  2008/07/04 12:24:35  huec
!  FA-ID 1010 : Correction de la syntaxe F90
!
!  Revision 1.6  2008/04/04 17:10:34  vivaresf
!  Version 2.4 : mise à jour des cartouches
!
!  Revision 1.5  2008/04/03 18:02:53  vivaresf
!  FA-ID 664 : correction des cartouches et de la présentation
!
!  Revision 1.4  2007/11/21 11:49:20  sbd
!  DM-ID 797 amelioration affichage resultats
!
!  Revision 1.3  2007/11/12 08:27:25  sbd
!  DM-ID 797 ajout commentaires et amelioration affichage des resultats extvar
!
!  Revision 1.2  2007/10/31 17:59:01  sbd
!  DM-ID 797 modification en tete et variables appels
!
!  Revision 1.1  2007/10/30 11:09:23  sbd
!  DM-ID 797 creation utilitaires pour terre mars et venus
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
  use cps_modele_emcd31
  use cps_acces

  implicit none

! SVN Source File Id
  character(len=256), parameter :: SVN_VER = &
   '$Id: ui_atm_emcd31.F90 355 2013-02-14 12:16:41Z aadt $'


! entrées
  real(kind=PM_REEL) ::  xz,xlat,xlon
  real(kind=PM_REEL) :: xdate
  integer :: scena
  real(kind=PM_REEL) :: typper(2)
  real(kind=PM_REEL) :: invar
  integer :: ikey

! sorties
  character(len=80) :: dset
  real(kind=PM_REEL) :: seedout
  real(kind=PM_REEL) :: pres
  real(kind=PM_REEL) :: ro
  real(kind=PM_REEL) :: temp
  real(kind=PM_REEL) :: ventu
  real(kind=PM_REEL) :: ventv
  real(kind=PM_REEL) :: meanvar(5)
  real(kind=PM_REEL) :: extvar(25)
  integer :: ier

! variables locales
  logical init_atm

  type(MSP_MESSAGE) :: messages
  integer :: noptions, ii, jj, ierfin
  character (len=256)            :: sLine
  character(len=60),dimension(50):: l_opt
  logical :: logxz,logxlat,logxlon,logxdate
  logical :: logscena
  logical :: logtypper(2)
  logical :: loginvar,logikey

  integer :: numfich=11

! initialisation

  ! initialisation
  call cps_init_utilisateur()
  if ( MSP_gen_messages("ui_atm_emcd31") ) then
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

  extvar(1:25) = 0._pm_reel
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
         call ui_ecrire_help("ui_atm_emcd31")
         goto 999
      elseif(ierfin.eq.0) then
         call ui_lire_options(noptions, l_opt)
         open(unit=numfich, file=trim(l_opt(2)))
!        Reading arguments from *.don file (" " separator)
         read(unit=numfich,fmt='(A)') sLine
         sLine=trim(adjustl(sLine))
         do ii=1,18
           jj = 1
           do while (sLine(jj:jj) /= " ")
             jj = jj + 1
           end do
           l_opt(ii)=trim(adjustl(sLine(:jj-1)))
           sLine=trim(adjustl(sLine(jj+1:)))
         end do
         noptions=9
      else
         ! Lecture des arguments :
         call ui_lire_options(noptions, l_opt)
         if (noptions == 0) then
            call ui_ecrire_help("ui_atm_emcd31")
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
               endif
            enddo


! corps du programme

! Initialisations preliminaires

      if(logxz.and.logxlat.and.logxlon.and.logxdate.and.logscena  &
         .and.logtypper(1).and.logtypper(2).and.loginvar.and.logikey) then
           
         ! erreur sur l'init
         if (MSP_erreur) goto 999
         
         ! appel effectif
         
         call cps_atmemcd_31(xz,xlat,xlon,xdate,dset,scena,typper,invar,init_atm, &
              seedout,ikey,pres,ro,temp,ventu,ventv,meanvar,extvar,ier)
        
         ! erreur dans l'appel
         if (MSP_erreur) goto 999
       
       
         ! sortie écran
	 write(*,'(''Seedout                 = '',e23.12)') seedout
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
  	 	do ii=1,25
     			write(*,'(''Extvar('',i2,'')              = '',e20.10)') ii,extvar(ii)
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

    call cps_atmemcd_31_close()
    call cps_close_utilisateur()

1000 FORMAT(a)

  end program ui_atm_emcd31
