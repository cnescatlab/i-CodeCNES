program ui_tephemtche

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  program
!
!$Nom
!  ui_tephemtche
!
!$Resume
!  Programme principal pour la comparaison d'éphémérides
!
!$Description
!  Validation des éphémérides créées par CREATEPHEM par rapport aux 
!  éphémérides sources
!. Programme de comparaison d'un fichier éphémérides planétaires présenté
!  sous la forme de jeux de coefficients de Tchebychev avec les éphémérides
!  issues de la méthode source ou d'une autre méthode 
!
! Usage
!. ui_tephemtche 
! 
!  ui_tephemtche lit sur 8 lignes ou plus ses paramètres dans l'ordre suivant
!.  <...> : fichier de Tchebychev à contrôler (MADONA/ Colonne)
!.  1/2 : type de fichier de Tchebychev
!.  date1 : date de début du contrôle
!.  date2 : date de fin du contrôle
!.  pas_de_calcul : pas de calcul 
!.  code :  corps à contrôler par rapport au corps central du fichier de Tchebychev
!.  codeephem : code éphémérides
!.  nb : nombre de fichier d'éphémérides
!.  <...> : nb fichiers d'éphémérides, un par ligne
!
!$Auteur
!
!$Version
!  $Id: ui_tephemtche.F90 418 2013-03-11 13:49:17Z ffsm $
!
!$Historique
!  $Log: ui_tephemtche.F90,v $
!  Revision 418 2013/03/11 Fran Simarro/FFSM - GMV
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 1.8  2010/11/09 08:43:42  ogarat
!  VERSION::FA-ID:1454:09/11/2010:Correction du calcul de l'écart relatif
!
!  Revision 1.7  2010/10/25 08:35:51  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.6  2010/05/25 10:29:04  jlrobin
!  VERSION::FA-ID:1355:25/05/2010:Manque des accents
!
!  Revision 1.5  2009/09/09 15:08:04  cmartel
!  FA-ID 1196 : Correction mineures sur les aides des utilitaires
!
!  Revision 1.4  2008/10/31 13:09:02  cml
!  FA-ID 1076 : Corrections mineurs dans les resultats des utilitaires
!
!  Revision 1.3  2008/04/11 10:09:37  vivaresf
!  FA-ID 778 : renommage des variables mal nommées
!  suppression des doubles déclarations
!  rajout de implicit none
!  rajout de intent(in/out)
!  suppression des lignes de code commentées
!
!  Revision 1.2  2008/04/10 13:03:56  vivaresf
!  Version 2.4 AQ : mise à jour des cartouches,
!  suppression de la lecture du répertoire fcf de la GSLIB, inutilisé
!
!  Revision 1.1  2008/02/08 17:51:42  vivaresf
!  FA-ID 889, DM-ID 820 :
!  - réorganisation du code pour que tout soit dans le répertoire src
!  Revision 1.9  2006/11/17 17:26:59  mle
!  FA-ID 609 : Erreurs dans les libelles des fenetres  de messages des IHM
!  Revision 1.8  2006/10/25 12:17:02  vpg
!  Resolution d'un probleme de compilation
!  Revision 1.7  2006/06/16 17:32:09  vivaresf
!  Cartouches d'entete
!  Revision 1.6  2006/06/16 15:17:14  vivaresf
!  Validation
!
!  Revision 1.5  2006/06/16 10:43:11  vivaresf
!  Deplacement de la routine pour utilisation en IHM
!
!  Revision 1.4  2006/06/14 12:36:09  vivaresf
!  DM-ID 387 : mise au point de CREATEPHEM
!
!  Revision 1.3  2006/05/31 13:08:41  vivaresf
!  COMPAS 2.0 : validation
!
!  Revision 1.2  2006/04/11 13:40:38  vpg
!  Utilisation de GS_changer_repere() au lieu de l'AMLIB
!
!  Revision 1.1  2006/03/17 10:22:28  fortinh
!  Ajout de source de createphem
!
!  Revision 1.6  2004/06/03 08:31:25  bremard
!  PhB - Modification associée à la FA-ID 157 - mise à jour de l'interface compatible avec la libephem V1_9
!
!  Revision 1.5  2003/10/13 14:36:27  bremard
!  PhB - Correction LEN=80 de liste_fic + affichage messages
!
!  Revision 1.4  2003/07/23 10:16:08  bremard
!  Suppression taper 1 pour continuer - JR
!
!  Revision 1.3  2002/10/25 09:35:45  bremard
!  PhB - Formattage et précision d'unité sur les sorties
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

  use mspro
  use msp_gestion_erreur
  use cps_utilisateur
  use ephem

      implicit none

! SVN Source File Id
  character(len=256) :: SVN_VER =  '$Id: ui_tephemtche.F90 418 2013-03-11 13:49:17Z ffsm $'


!-------------------------------
!	VARIABLES LOCALES
!-------------------------------
      character (LEN=255) :: fich
      type(MSP_MESSAGE) :: messages
      character(len=256)::rep_fcf
      integer :: lrep_fcf

      integer :: ier
      integer :: fmt_fics                            ! Format fichier éphéméride

!-------------------------------
!       CORPS DU PROGRAMME
!-------------------------------

! DM 387 : initialisation COMPAS
      call cps_init_utilisateur()
      call eph_infoinit()
      if (MSP_gen_messages("ui_tephemtche")) return
!

      ! Fichier d'erreur CREATEPHEM/TEPHEMTCHE
      ier = AMv_rc_get ('compas_ui_fcf','compas','','dirfcf',rep_fcf, lrep_fcf)
      call MSP_ajouter_fichier_message (trim(rep_fcf)//"/CPSUI_erreur_fr")

      write(0,*)'# NOM DU FICHIER TCHEBYCHEV CNES A TRAITER = ? '
      read(*,'(a)')fich

      write(0,*)'# Format du fichier (1 : MADONA ;  2 : colonnes) = ?'      
      read(*,*)fmt_fics

!	lecture de l'EN-TETE

      if (fmt_fics .eq. 1) then
         call Comp_Ephem_MAD(fich,ier)
      else 
         call Comp_Ephem_COL(fich,ier)
      endif
      if (MSP_gen_messages("ui_tephemtche")) return

      if (MSP_PROBLEME) then
         call MSP_recuperer_message (message=messages,nb_mes=MSP_tous_messages)
         call MSP_afficher_message (message=messages,unit=0)
      endif

! DM 387 : fermeture COMPAS
      call cps_close_utilisateur()

  end program ui_tephemtche

      subroutine Comp_Ephem_COL(fich,ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  Comp_Ephem_COL
!
!$Resume
!   Comparaison d'éphéméride au format colonnes
!
!$Description
!   Lecture des éphéméride Tchebychev Colonnes dans l'entete
!   puis comparaison avec les éphémérides sources
!
!$Acces
!  PUBLIC
!
!$Usage
!  call Comp_Ephem_COL(fich,ier)
!.    character (LEN=*) :: fich
!.    integer :: ier
!
!$Arguments
!>E     fich     :<LEN=*>     nom du fichier de polynomes de Tchebychev au format Colonnes 
!>S     ier      :<integer>   code erreur retour 
!
!$Remarques
!
!$Mots-cles
!   éphémérides, Tchebychev
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use mspro
      use EPHEM
      use msp_gestion_erreur
      use ui_io

      implicit none

      ! Parametres
      character (LEN=*), intent(in) :: fich
      integer, intent(out) :: ier

      ! Variables locales
      character (LEN=80)  :: liste_fic(10)

      integer :: ibid1,ibid2,i
      integer :: numplac,numframe,nbplanet           
      integer :: iu
      integer :: numeropla                           ! Numero planete a calculer
      integer :: ipt,npt
      integer :: numpla(15)
      integer :: nbfic                               ! Nbr de fichier NAIF

      real (KIND=pm_reel) :: bid2,tdeb,tfin,tcalcul
      real (KIND=pm_reel) :: tcalcul1,tcalcul2,pascalcul
      real (KIND=pm_reel) :: tframe
      real (KIND=pm_reel) :: xreel_eme(6),xreel(6),xtcheby(6)
      real (KIND=PM_REEL) :: epmin, epmax            ! Ecart de position min et max
      real (KIND=PM_REEL) :: evmin, evmax            ! Ecart de vitesse  min et max
      real (KIND=PM_REEL) :: epminrel, epmaxrel      ! Ecart relatif de position min et max
      real (KIND=PM_REEL) :: evminrel, evmaxrel      ! Ecart relatif de vitesse  min et max
      integer :: ktype                               ! Type d'éphéméride 
      
      ! DM 387
      integer :: repe, plae, eche, nutae
      integer :: reps, plas, echs, nutas
      type(tm_jour_sec) :: datereps, datetmp
      type(tm_pole_uv) :: pole
      type(tm_code_retour) :: code_retour
      real(KIND=PM_REEL) :: delta_tai, delta_tu1

      real(KIND=PM_REEL) :: vrot
      real(KIND=PM_REEL) :: mm, diffe
      logical :: datecour

      character(LEN=32) :: echtout, nomframeout

      ! Pour l'initialisation des bulletins
      integer, dimension(:), allocatable :: corpsci
      type(EPH_BULLETIN), dimension(:), allocatable :: tabull
      integer :: ii,nbbulltot,nbbull

      ! Initialisations
      epmin=100.
      evmin=100.
      epmax=-100.
      evmax=-100.

      epminrel=100.
      evminrel=100.
      epmaxrel=-100.
      evmaxrel=-100.

      call eph_util_ficunit90(iu,ier)
      if (MSP_gen_messages("Comp_Ephem_COL")) return
      if(ier.lt.0)then
         return
      endif
      open(iu, file=fich, recl=16606,status="old", blank='NULL', &
           form='FORMATTED', access='SEQUENTIAL', iostat=ier)

      call eph_lentfic_pla77(iu, ibid1,numplac,numframe,tframe, &
           nbplanet,numpla,bid2,ibid2,tdeb,tfin, echtout, nomframeout)

      if(MSP_ERREUR)then
            ier = - 1
            call MSP_signaler_message(cle_mes='CREA_tephemtche',routine='comp_ephem_col' &
                 ,partie_variable='Erreur après appel eph_lentfic_pla ')
            go to 999
      endif

      write(0,1000)'# Numéro cnes corps central       ',numplac
      write(0,1000)'# Numéro cnes repère sortie       ',numframe
      write(0,1000)'# Nombre de planètes du fichier   ',nbplanet
      write(0,*)'# Date du repère de sortie   ',tframe   ! PTPT

      do i =1,nbplanet
      	write(0,1010)'# Numéro Planète ',i,numpla(i)
      end do
      write(0,1020)'# Date julienne cnes début de fichier  ',tdeb

      write(0,*)'# Date CNES début  pour la comparaison = ?'
      read(*,*)tcalcul1

      write(0,1020)'# Date julienne cnes fin de fichier  ',tfin
      write(0,*)'# Date CNES fin pour la comparaison = ?'
      read(*,*)tcalcul2

      write(0,*)'# Pas calcul pour la comparaison = ?'
      ! pascalcul en jours
      read(*,*)pascalcul

      write(0,*)'# Numéro planète à calculer = ?'
      read(*,*)numeropla

      write(0,*)'# Type éphémérides tchebychevisée = ?'
      write(0,*)'#    = 10 si EPHEMERIDES ANALYTIQUES BDL VSOP82'
      write(0,*)'#    = 20 si EPHEMERIDES NAIF DE200'
      write(0,*)'#    = 21 si EPHEMERIDES NAIF DE403'
      write(0,*)'#    = 22 si EPHEMERIDES NAIF DE405'
      write(0,*)'#    = 30 si EPHEMERIDES FICHIER BDL VSOP82'
      read(*,*)ktype

!     Lecture des fichiers NAIF
      write(0,*)'# Nombre de fichiers NAIF = ?'
      read(*,*)nbfic
      do i =1,nbfic
        write(0,*)'# Nom fichier ',i, ' = ?'
        read(*,*)liste_fic(i)
      	write(0,*)'# Nom du fichier de la liste ',trim(liste_fic(i))
      end do

!     Comparaison pour les fichier au format Colonne
!         
  ! Initialisation 
      if ((ktype.ge.40) .and. (ktype.le.59)) then
         write(0,*)'# Comparaison non disponible'
         goto 999
      endif
      if (ktype==11) then
         allocate (corpsci(4*nbplanet))
         allocate (tabull(4*nbplanet))
         
         nbbull=0
         nbbulltot=0
         do ii=1,nbplanet
            call eph_kep_lirekep(numpla(ii), numplac, tabull(nbbull+1:nbbull+4), &
                 corpsci(nbbull+1:nbbull+4), nbbull)
            nbbulltot=nbbulltot+nbbull
         enddo
         call eph_initposcor(ktype, tabull=tabull, codecorps=corpsci, &
              nbbull=nbbulltot)
      else
         call eph_initposcor(ktype, nbfic, fichiers=liste_fic)
      endif
      if (MSP_gen_messages("Comp_Ephem_COL")) return

!     Initialisation du pas de calcul
      npt = ( tcalcul2 - tcalcul1)/ pascalcul + 1
      if(tcalcul1+(npt*pascalcul).lt.tcalcul2)then
      	npt=npt+1
      endif

      rewind iu

      write(6,*) "# Nombre de pas de calcul=", npt

      write(6,2001)
      write(6,2000)


      ! REPERE D'ENTREE : EME 2000
      repe = pm_equa_moy
      plae = pm_pla_terre
      eche = pm_1janvier2000_12h00
      nutae = pm_lieske_wahr_aoki
      delta_tu1=0.0_PM_REEL
      
      ! REPERE DE SORTIE
      call eph_codageRepMSPRO (numframe, reps, plas, echs)
      nutas = pm_uai2000
      if (plas == 399) nutas = pm_lieske_wahr_aoki

      call cps_constante(plas, vrot=vrot)

      ! Dans le cas de la définition d'un repère de sortie avec donnée 
      ! d'une date de référence, la date de référence est celle contenue 
      ! dans tframe
         
      call md_jourfrac_joursec(tframe,datereps, code_retour)
      call md_ech_temps(pm_TE,datereps,pm_TAI,delta_tai,datetmp, &
           code_retour=code_retour)
      datecour=.false.

      if (echs == pm_autre_date+1) datecour=.true.
      if (echs == pm_autre_date.and. tframe==0.and.reps.ne.pm_ecli_moy) then
         datecour=.true.
      endif

!     Boucle de calcul
      do ipt=1,npt
         tcalcul = tcalcul1 + (ipt-1)*pascalcul
         xreel_eme=0
         call eph_poscor (ktype, tcalcul, numplac, numeropla, xreel_eme) 
         if (MSP_gen_messages("Comp_Ephem_COL")) return

         if (datecour) call md_jourfrac_joursec(tcalcul,datereps, code_retour)
!
!!! Changement de repere
!

         ! Appel a mx_rep
         call mx_rep(repe,plae,eche,nutae,xreel_eme(1:3),xreel_eme(4:6),         &
              reps,plas,echs,nutas,xreel(1:3),xreel(4:6),              &
              code_retour=code_retour, val_date_out=datereps,          &
              delta_tai_out=delta_tai,long_ref_out=0._PM_REEL,          &
              pole_out=pole, delta_tu1_out=delta_tu1, vit_rot_out=vrot)
         if (code_retour%valeur < 0) then
            call MSP_signaler_message(routine='comp_ephem_col', cle_mes='CREA_REPCHREP', &
                 partie_variable='après exécution mx_rep ')
            return
         endif


!!! Fin du changement de repere
         !
         
         ! on utilise eph_ephemtche
         call eph_ephemtche(iu,tcalcul,numplac,numeropla,xtcheby)
         if (MSP_ERREUR) then 
            call MSP_signaler_message(cle_mes='CREA_tephemtche ',routine='comp_ephem_col', &
                partie_variable= 'après exécution eph_ephemtche')
            goto 999
         endif
         
         write(6,2005)ipt,tcalcul,(xtcheby(i)-xreel(i),i=1,3) &
              ,(xtcheby(i+3)-xreel(i+3),i=1,3)
         
         call flush(6)
         
         !       Calcul des ecarts min et max en position et vitesse
         do i=1,3
           ! Difference
           diffe=abs(xtcheby(i)-xreel(i) )
           ! Max des 2 valeurs
           mm = abs(xreel(i))
           if (mm<1e-300) mm = 1

           if (diffe < epmin) epmin= diffe
           if (diffe/mm < epminrel ) epminrel= diffe/mm

           if (diffe > epmax) epmax= diffe
           if (diffe/mm > epmaxrel ) epmaxrel= diffe/mm

           diffe=abs(xtcheby(i+3)-xreel(i+3) )
           ! Max des 2 valeurs
           mm = abs(xreel(i+3))
           if (mm.le.1e-300) mm = 1
           if (diffe < evmin) evmin= diffe
           if (diffe/mm < evminrel ) evminrel= diffe/mm

           if (diffe > evmax) evmax= diffe
           if (diffe/mm > evmaxrel ) evmaxrel= diffe/mm
         enddo
         
         
      end do
!     Fin de la boucle de calcul

!     tracer les ecarts min et max
      write(6,*)"#"
      write(6,2009)
      write(6,2010) epmax,evmax
      write(6,*)"#"
      write(6,2011)
      write(6,2010) epmaxrel,evmaxrel
      write(6,*)"#"
      call flush(6)

1000	format(1x,a35,' = ',i5)
1010	format(1x,a20,i3,' = ',i7)
1020	format(1x,a35,' = ',f15.5)
2000	format(1x,'# N',T5,'DATE CNES',T22,'Xtch-Xr',T37,'Ytch-Yr', &
        T52,'Ztcheb-Zr',T67,'VXtch-VXr',T82,'VYtch-VYr',T97,'VZtch-VZr')
2001	format(1x,'# Ecarts entre les positions/vitesse Tchebychev et réelles (km, km/s)')
2005	format(i4,f12.5,T20,e12.3,T35,e12.3,T50,e12.3,T65,e12.3,T80,e12.3,T95,e12.3)
2009    format(1x,"# Max des écarts en position et vitesse en valeur absolue (km et km/s)")
2010	format(1x,"# |Ecarts|  :  MAX pos.",d12.3,4x, "MAX vit.",d12.3)
2011    format(1x,"# Max des écarts relatif en position et vitesse en valeur absolue")
999	continue

  end subroutine Comp_Ephem_COL

  subroutine Comp_Ephem_MAD(fich, ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  Comp_Ephem_MAD
!
!$Resume
!   Comparaison d'éphéméride au format MADONA
!
!$Description
!   Lecture des caracteristiques de l'éphéméride Tchebychev dans l'entete
!   MADONA du fichier
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call Comp_Ephem_MAD(fich, ier)
!.    character (LEN=*) :: fich
!.    integer :: ier
!
!$Arguments
!>E     fich     :<LEN=*>     nom du fichier de polynomes de Tchebychev au format MADONA.
!>S     ier      :<integer>   code erreur retour
!
!$Remarques
!
!$Mots-cles
!   éphémérides, Tchebychev, MADONA
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use mspro
      use EPHEM
      use msp_gestion_erreur
      use ui_io

      implicit none

      character (LEN=*), intent(in) :: fich
      integer, intent(out) :: ier

      ! Variables locales
      character (LEN=200) :: chemin
      character (LEN=100) :: nomfich
      character (LEN=80) :: liste_fic(10)

      integer :: ibid1,ibid2,i
      integer :: numplac,numframe,nbplanet           
      integer :: numeropla                           ! Numero planete a calculer
      integer :: ipt,npt
      integer :: numpla(15)
      integer :: nbfic                               ! Nbr de fichier NAIF

      real (KIND=pm_reel) :: bid2,tdeb,tfin,tcalcul
      real (KIND=pm_reel) :: tcalcul1,tcalcul2,pascalcul
      real (KIND=pm_reel) :: tframe
      real (KIND=pm_reel) :: xreel_eme(6),xreel(6),xtcheby(6)
      real (KIND=PM_REEL) :: epmin, epmax            ! Ecart de position min et max
      real (KIND=PM_REEL) :: evmin, evmax            ! Ecart de vitesse  min et max
      real (KIND=PM_REEL) :: epminrel, epmaxrel      ! Ecart relatif de position min et max
      real (KIND=PM_REEL) :: evminrel, evmaxrel      ! Ecart relatif de vitesse  min et max

      integer :: ktype                               ! Type d'éphémeride 
      integer :: rindex

      ! DM 387
      integer :: repe, plae, eche, nutae
      integer :: reps, plas, echs, nutas
      type(tm_jour_sec) :: datereps, datetmp
      type(tm_pole_uv) :: pole
      type(tm_code_retour) :: code_retour
      real(KIND=PM_REEL) :: delta_tai, delta_tu1

      real(KIND=PM_REEL) :: pole_u, pole_v, obliq, vrot, lref
      real(KIND=PM_REEL) :: mm, diffe
      logical :: datecour

      ! Pour l'initialisation des bulletins
      integer, dimension(:), allocatable :: corpsci
      type(EPH_BULLETIN), dimension(:), allocatable :: tabull
      integer :: ii,nbbulltot,nbbull

      ! Initialisations
      epmin=100.
      evmin=100.
      epmax=-100.
      evmax=-100.
      
      call lentfic_pla_mad(fich,ibid1,numplac,numframe,tframe          &
           ,nbplanet,numpla,bid2,ibid2,tdeb,tfin,vrot, lref, pole_u,   &
           pole_v, obliq, datecour, ier)
      if(ier.lt.0)then
            ier = - 1
            call MSP_signaler_message(cle_mes='CREA_tephemtche',routine='comp_ephem_mad', &
                                      partie_variable='Erreur après appel lentfic_pla_mad ')
            go to 999
      endif

      ! Valeurs lues dans le fichier
      write(0,1000)'# Numéro cnes corps central       ',numplac
      write(0,1000)'# Numéro cnes repère sortie       ',numframe
      write(0,1000)'# Nombre de planètes du fichier   ',nbplanet
      write(0,*)'# Date du repère de sortie   ',tframe

      do i =1,nbplanet
      	write(0,1010)'# Numéro Planète ',i,numpla(i)
      end do
      write(0,1020)'# Date julienne cnes début de fichier  ',tdeb

      ! Lecture des parametres passes par l'IHM
      write(0,*)'# Date CNES début pour la comparaison = ?'
      read(5,*)tcalcul1

      write(0,*)'# Date CNES fin pour la comparaison = ?'
      read(5,*)tcalcul2

      write(0,*)'# Pas calcul pour la comparaison = ?'
      ! pascalcul en jours
      read(5,*)pascalcul

      write(0,*)'# Numéro planète à calculer = ?'
      read(5,*)numeropla

      write(0,*)'# Type éphémérides tchebychevisée = ?'
      write(0,*)'#    = 10 si EPHEMERIDES ANALYTIQUES BDL VSOP82'
      write(0,*)'#    = 20 si EPHEMERIDES NAIF DE200'
      write(0,*)'#    = 21 si EPHEMERIDES NAIF DE403'
      write(0,*)'#    = 22 si EPHEMERIDES NAIF DE405'
      write(0,*)'#    = 30 si EPHEMERIDES FICHIER BDL VSOP82'
      read(5,*)ktype

!     Si methode NAIF, fichier *.bsp utilises
      write(0,*)'# Nombre de fichiers NAIF = ?'
      read(5,*)nbfic
      do i =1,nbfic
        write(0,*)'# Nom fichier ',i, ' = ?'
        read(5,*)liste_fic(i)
      	write(0,*)'# Nom du fichier de la liste ',liste_fic(i)      !PTPT a supprimer
      end do

      if ((ktype.ge.40) .and. (ktype.le.59)) then
         write(0,*)'# Comparaison non disponible'
         goto 999
      else
         if (ktype==11) then
            allocate (corpsci(4*nbplanet))
            allocate (tabull(4*nbplanet))
            
            nbbull=0
            nbbulltot=0
            do ii=1,nbplanet
               call eph_kep_lirekep(numpla(ii), numplac, tabull(nbbull+1:nbbull+4), &
                    corpsci(nbbull+1:nbbull+4), nbbull)
               nbbulltot=nbbulltot+nbbull
            enddo
            call eph_initposcor(ktype, tabull=tabull, codecorps=corpsci, nbbull=nbbulltot)
         else
            call eph_initposcor(ktype, nbfic, fichiers=liste_fic)
         endif
      endif
      if (MSP_gen_messages("Comp_Ephem_MAD")) return
      

!     Lecture du fichier au format MADONA
      chemin = fich(1:rindex(fich,"/"))
      nomfich = fich(rindex(fich,"/")+1:len_trim(fich))
      call eph_inittchemad(chemin,nomfich)            
      if (MSP_gen_messages("Comp_Ephem_MAD")) return

!     Initialisation du pas de calcul
      npt = ( tcalcul2 - tcalcul1)/ pascalcul + 1
      if(tcalcul1+(npt-1)*pascalcul.lt.tcalcul2)then
      	npt=npt+1
      endif

      write(6,1997)
      write(6,1998)
      write(6,1997)
      write(6,*) "# Nombre de pas de calcul=", npt
      write(6,2001)
      write(6,2000)
      call flush(6)


      ! REPERE D'ENTREE : EME 2000
      repe = pm_equa_moy
      plae = pm_pla_terre
      eche = pm_1janvier2000_12h00
      nutae = pm_lieske_wahr_aoki
      delta_tu1=0.0_PM_REEL
      
      ! REPERE DE SORTIE
      call eph_codageRepMSPRO (numframe, reps, plas, echs)
      nutas = pm_uai2000
      if (plas == 399) nutas = pm_lieske_wahr_aoki
      ! Dans le cas de la définition d'un repère de sortie avec donnée 
      ! d'une date de référence, la date de référence est celle contenue 
      ! dans tframe
         
      call md_jourfrac_joursec(tframe,datereps, code_retour)
      call md_ech_temps(pm_TE,datereps,pm_TAI,delta_tai,datetmp, &
           code_retour=code_retour)

      datecour=.false.

      if (echs == pm_autre_date+1) datecour=.true.
      if (echs == pm_autre_date.and. tframe==0.and.reps.ne.pm_ecli_moy) then
         datecour=.true.
      endif

!     Boucle de calcul
      do ipt=1,npt
         tcalcul = tcalcul1 + (ipt-1)*pascalcul
         xreel_eme=0

! Ephémérides source
         call eph_poscor (ktype, tcalcul, numplac, numeropla, xreel_eme) 
         if (MSP_gen_messages("Comp_Ephem_MAD")) return

!
!!! Changement de repere
!
         if (datecour) call md_jourfrac_joursec(tcalcul,datereps, code_retour)

         ! Appel a mx_rep
         call mx_rep(repe,plae,eche,nutae,xreel_eme(1:3),xreel_eme(4:6),         &
              reps,plas,echs,nutas,xreel(1:3),xreel(4:6),              &
              code_retour=code_retour, val_date_out=datereps,          &
              delta_tai_out=delta_tai,long_ref_out=0._PM_REEL,          &
              pole_out=pole, delta_tu1_out=delta_tu1, vit_rot_out=vrot)
         if (code_retour%valeur < 0) then
            call MSP_signaler_message(routine='comp_ephem_col', cle_mes='CREA_REPCHREP', &
                 partie_variable='après exécution mx_rep ')
            return
         endif
!
!!! Fin du changement de repere
!


! Ephémérides de Tchebytchev

      xtcheby(:)=0
      call eph_pvtchebmad(numplac,numeropla,tcalcul,xtcheby)
      if (MSP_gen_messages("Comp_Ephem_MAD")) then 
    	   call MSP_signaler_message(cle_mes='CREA_tephemtche ',&
                routine='comp_ephem_mad', &
        	partie_variable='après exécution eph_pvtchebmad')
      endif

      write(6,2005)ipt,tcalcul,(xtcheby(i)-xreel(i),i=1,3) &
           ,(xtcheby(i+3)-xreel(i+3),i=1,3)

      call flush(6)

!       Calcul des ecarts min et max en position et vitesse
        do i=1,3
           ! Difference sur les positions
           diffe=abs(xtcheby(i)-xreel(i) )
           ! Max des 2 valeurs
           mm = abs(xreel(i))
           if (mm.le.1e-30_pm_reel) mm = 1

           if (diffe < epmin) epmin = diffe
           if (diffe/mm < epminrel ) epminrel= diffe/mm

           if (diffe > epmax) epmax= diffe
           if (diffe/mm > epmaxrel ) epmaxrel= diffe/mm

           ! Difference sur les vitesses
           diffe=abs(xtcheby(i+3)-xreel(i+3) )
           ! Max des 2 valeurs
           mm = abs(xreel(i+3))
           if (mm.le.1e-30_pm_reel) mm = 1

           if (diffe < evmin) evmin= diffe
           if (diffe/mm < evminrel ) evminrel= diffe/mm

           if (diffe > evmax) evmax= diffe
           if (diffe/mm > evmaxrel ) evmaxrel= diffe/mm
        enddo

      end do
!     Fin de la boucle de calcul

!     tracer les ecarts min et max
      write(6,*)""
      write(6,2009)
      write(6,2010) epmax,   evmax
      write(6,*)""
      write(6,2011)
      write(6,2010) epmaxrel,evmaxrel
      write(6,*)""
      call flush(6)

999   continue

!     Fin d'utilisation de la méthode  Tchebytchev/MADONA
      call eph_closetchemad()


! Formats pour la procedure
1000	format(1x,a35,' = ',i5)
1010	format(1x,a20,i3,' = ',i5)
1020	format(1x,a35,' = ',f15.5)
1997    format(1x,'#')
1998    format(1x,'# Attention: la comparaison à partir d''un fichier MADONA n''est possible que sur un nombre limité de corps.')
2000	format(1x,'# N',T5,'DATE CNES',T22,'Xtch-Xr',T37,'Ytch-Yr', &
        T52,'Ztcheb-Zr',T67,'VXtch-VXr',T82,'VYtch-VYr',T97,'VZtch-VZr')
2001	format(1x,'# Ecarts entre les positions/vitesse Tchebychev et réelles (km, km/s)')
2005	format(i3,f12.5,T20,e12.3,T35,e12.3,T50,e12.3,T65,e12.3,T80,e12.3,T95,e12.3)
2009    format(1x,"# Max des écarts en position et vitesse en valeur absolue (km, km/s)")
2010	format(1x,"# |Ecarts| :  MAX pos.",d12.3,4x, "MAX vit.",d12.3)
2011    format(1x,"# Max des écarts relatif en position et vitesse en valeur absolue")

      endsubroutine Comp_Ephem_MAD

