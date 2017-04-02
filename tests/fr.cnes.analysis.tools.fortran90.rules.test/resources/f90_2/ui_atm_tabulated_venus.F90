program ui_atm_tabulated_venus

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_atm_tabulated_venus
!
!$Resume
! Utilitaire pour le fonctionnement du modèle d'atmosphère Venus tabule
!
!$Description
! Utilitaire pour le fonctionnement du modèle d'atmosphère Venus tabule.
! Il utilise la routine cps_atm_tv_calculer de cps_atmosphere_venus.F90 de COMPAS_BASE.
! Usage
!.     ui_atm_tabulated_venus -sigma sigma -heure heure -latitude latitude -altitude altitude 
!
!>E  sigma      : <pm_reel>          sigma
!>E  heure      : <pm_reel>          heure solaire (heure)
!>E  latitude   : <pm_reel>          latitude (degré)
!>E  altitude   : <pm_reel>          altitude (km) 
!    
!>S  masse volumique :<pm_reel>          masse volumique (kg/m3)     
!>S  temperature     :<pm_reel>          température (K)      
!>S  pression        :<pm_reel>          pression (bar)      
!>S  norme vent      :<pm_reel>          norme du vent (m/s)
!>S  azimut vent     :<pm_reel>          azimut du vent (rad)
!
!$Auteur
! Atos Origin
!
!$Version
!  $Id: ui_atm_tabulated_venus.F90 355 2013-02-14 12:16:41Z aadt $
!
!$Historique
!  $Log: ui_atm_tabulated_venus.F90,v $
!  Revision 355  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!  Revision 1.7  2010/11/02 16:16:17  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.6  2010/11/02 14:23:51  mercadig
!  VERSION::FA-ID:1443:02/11/2010:Unites manquantes et homogeneisation de la presentation
!
!  Revision 1.5  2010/10/29 12:48:09  mercadig
!  VERSION::AQ::29/10/2010:Suppression des variables inutilisees errmess, ok et unite
!
!  Revision 1.4  2010/10/28 15:39:22  mercadig
!  VERSION::AQ::28/10/2010:Mise a jour du cartouche
!
!  Revision 1.3  2010/10/21 13:46:22  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.2  2010/10/19 13:42:34  mercadig
!  VERSION::DM-ID:1422:19/10/2010:Ajout de la sortie azimut du vent dans le modele
!
!  Revision 1.1  2010/10/08 13:05:03  ogarat
!  VERSION::FA-ID:1450:08/10/2010:Renommage de ui_atm_tabulated
!
!
! Anciennement ui_atm_tabulated.F90
! Historique de ui_atm_tabulated.F90
!  Revision 1.5  2010/10/07 14:05:29  ogarat
!  VERSION::FA-ID:1450:07/10/2010:Complement de l'aide en ligne de ui_atm_tabulated
!
!  Revision 1.4  2010/10/04 11:08:33  ogarat
!  VERSION::FA-ID:1446:04/10/2010: Alignement des signes = dans l'affichage des résultats
!
!  Revision 1.3  2010/05/31 13:53:13  jlrobin
!  VERSION::FA-ID:1376:31/05/2010:densite en masse volumique
!
!  Revision 1.2  2010/04/30 14:14:02  jlrobin
!  V2.9::AQ::30/04/2010:merge de la branche de developpement modeles atmospheriques
!
!  Revision 1.1.2.2  2010/03/09 12:42:22  fabrec
!  V2.9::DM-ID:1361:09/03/2010:Ajout du modele d'atmosphere venus tabule
!
!
!#V
!#V
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
  use cps_atmosphere_venus
  use cps_acces

 implicit none

! SVN Source File Id
  character(len=256), parameter :: SVN_VER = &
   '$Id: ui_atm_tabulated_venus.F90 355 2013-02-14 12:16:41Z aadt $'


! entrées
  real(KIND=PM_REEL) :: sigma, heure, latitude, altitude

! sorties
  real(KIND=PM_REEL) :: densite, temperature, pression, norme_vent, azim_vent 

! variables locales
  integer :: i
  type(MSP_MESSAGE) :: messages
  integer :: noptions, ii, ierfin
  character(len=60),dimension(50):: l_opt
  ! Trace des données en entrées
  logical :: logsigma, loghsol, loglat, logalt
  ! Affichage de l'erreur
  type(MSP_MESSAGE) :: cps_mess
  character(LEN=CPS_MAXLG) :: nom_fichier    ! Nom absolu du fichier
  integer :: trouve                          ! Flag indiquant le succès de la recherche
  character(len=CPS_MAXLG), dimension(:), pointer :: listeFichiers => NULL()

! initialisation

  call cps_init_utilisateur()
  if ( MSP_gen_messages("ui_atm_tabulated_venus") ) then
     ! Si l'initialisation échoue, on arrête l'utilitaire
     call MSP_recuperer_message(nb_mes=MSP_tous_messages, message=messages)
     call MSP_afficher_message(message=messages, unit=0)
     call MSP_effacer_message()
     stop
  endif

  ! Test d'interrogation de la base
  ! 
!  write(*,*) "Extraction de la liste des fichiers disponibles : "
  call cps_getListeFichiersAtmTab(listeFichiers, 299)
  if ( cpsi_size_ptr(listeFichiers) > 0 ) then
     do i = 1, cpsi_size_ptr(listeFichiers)
!         write(*,*) trim(listeFichiers(i))
     enddo
  else
!     write(*,*) "Aucun fichier tabule."
  endif

  ! Test de lecture
  ! Si au moins un fichier dans la base
  if ( cpsi_size_ptr(listeFichiers) > 0) then

!     write(*,1000) "Lecture d'un fichier de données"
     ! Interrogation de la base
     trouve = cps_getFichierAtmTab(trim(listeFichiers(1)), nom_fichier, rep=.true.)
     if (trouve == CPS_OK) then
        ! Lecture des données
       call cps_atm_ouvrir_fictab(nom_fichier) 
    endif

     ! Filtrage en cas d'erreur
     if ( trouve /= CPS_OK .or. MSP_ERREUR ) then
 !       write (*,1000) "Comportement innatendu ! "
        call MSP_recuperer_message(nb_mes=MSP_tous_messages, message=cps_mess)
        call MSP_afficher_message(message=cps_mess, unit=0)
        call MSP_effacer_message()
     endif
  endif

  ! Mise a zero des flags
  logsigma =.false.
  loghsol =.false.
  loglat = .false.
  logalt = .false.

! Lecture des paramètres (en ligne)
  call ui_test_arguments ("-h", "", ierfin)

      ! lecture effective
      if(ierfin.eq.1) then
         call ui_ecrire_help("ui_atm_tabulated_venus")
         goto 999
      else
         ! Lecture des arguments :
         ! -sigma -heure -latitude -altitude 
         call ui_lire_options(noptions, l_opt)
         if (noptions == 0) then
            call ui_ecrire_help("ui_atm_tabulated_venus")
            goto 999
         else
            ! Lecture des options saisies en entrée
            do ii=1, noptions
               if (l_opt(2*ii-1)=="-sigma") then
                  read(l_opt(2*ii), fmt=*) sigma
                  logsigma = .true.
               elseif(l_opt(2*ii-1)=="-heure") then
                  read(l_opt(2*ii), fmt=*) heure
                  loghsol=.true.
               elseif(l_opt(2*ii-1)=="-latitude") then
                 read(l_opt(2*ii), fmt=*) latitude
                  loglat=.true.
               elseif(l_opt(2*ii-1)=="-altitude") then
                  read(l_opt(2*ii), fmt=*) altitude
                  logalt=.true.
               endif
            enddo
         endif
         
      endif

! corps du programme

! Test uniquement sur les arguments obligatoires

      if(logsigma.and.loghsol.and.loglat.and.logalt) then
          
         ! erreur sur l'init
         if (MSP_erreur) goto 999
         
         ! appel effectif
         call cps_atm_tv_calculer(sigma,heure,latitude,altitude,densite,&
              temperature,pression,norme_vent,azim_vent) 
         if ( MSP_ERREUR ) goto 999
         ! sortie écran
         write (*,'(a,e15.9)') "Masse volumique (kg/m3) = ", densite
         write (*,'(a,e15.9)') "Température     (K)     = ", temperature
         write (*,'(a,e15.9)') "Pression        (bar)   = ", pression
         write (*,'(a,e15.9)') "Norme du vent   (m/s)   = ", norme_vent
	 write (*,'(a,e15.9)') "Azimut du vent  (rad)   = ", azim_vent
         
      else
         write(0,*) "Erreur : les arguments ne sont pas correctement remplis"
      endif

999   continue
      if (MSP_PROBLEME) then
         call MSP_recuperer_message (message=messages,nb_mes=MSP_tous_messages)
         call MSP_afficher_message (message=messages,unit=0)
      endif

      ! Fermeture de la base
      call cps_close_utilisateur()
      
    end program ui_atm_tabulated_venus
