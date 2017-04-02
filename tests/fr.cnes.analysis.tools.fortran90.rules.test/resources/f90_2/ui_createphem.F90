program ui_createphem
!************************************************************************
!$<AM-V2.0>
!
!$Type
!  program
!
!$Resume
!      Programme de génération d'un fichier éphémérides planétaires 
!      sous la forme de jeux de coefficients de Tchebychev.   
!
!$Description
!     Ce programme permet de représenter sous la forme de jeux de
!     coefficients de TCHEBYCHEV les éphémérides de planètes du système
!     solaire ainsi que de la Lune en ayant la possibilité de choisir :
!.     	- le corps (Planète) central
!.		- le système de référence spatial
!.                Ex : Coordonnées de Jupiter par rapport au centre
!                    de Mars dans le système EME50 (Earth Mean Equator 1950.0)
!.     Pour cela on a le choix entre 3 types de données de base :
!.			EPHEMERIDES FICHIER BDL VSOP82
!.                       EPHEMERIDES NAIF
!.			EPHEMERIDES ANALYTIQUES BDL VSOP82
!.     L'intérêt de cette représentation est d'éviter à l'utilisation toutes
!     les transformations faisant passer des données de base aux éphémérides
!     que l'on désire...D'où un gain en temps calcul trés important (de l'ordre
!     de 20 à 1) et une programmation plus légère.
!
!$Auteur
!  Ph. Bremard (à l'origine J.BERNARD)
!
!$Version
!  $Id: ui_createphem.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ui_createphem.F90,v $
!  Revision 1.14  2010/10/25 08:35:51  ogarat
!  VERSION::AQ::24/10/2010:Ajout des FinHistorique
!
!  Revision 1.13  2010/05/25 10:29:04  jlrobin
!  VERSION::FA-ID:1355:25/05/2010:Manque des accents
!
!  Revision 1.12  2009/09/09 15:08:03  cmartel
!  FA-ID 1196 : Correction mineures sur les aides des utilitaires
!
!  Revision 1.11  2009/09/09 12:56:10  cmartel
!  FA-ID 1157 : Correction de constantes non nommées
!
!  Revision 1.10  2008/11/17 09:22:27  cml
!  AQ : Correction orthographe
!
!  Revision 1.9  2008/11/07 14:36:00  cml
!  AQ : Correction de pointeurs non initialises
!
!  Revision 1.8  2008/11/07 09:55:46  cml
!  AQ : Correction de warning foresys
!
!  Revision 1.7  2008/10/31 13:08:40  cml
!  FA-ID 1076 : Corrections mineurs dans les resultats des utilitaires
!
!  Revision 1.6  2008/04/29 08:08:58  vivaresf
!  COMPAS_UI, V2.4, AQ : suppression des variables inutilisées
!
!  Revision 1.5  2008/04/28 13:01:52  vivaresf
!  COMPAS_UI V2.4, AQ : suppression du chargement (inutile) de MSP_MESSAGES
!
!  Revision 1.4  2008/04/11 10:57:55  vivaresf
!  FA-ID 778 : suppression des variables inutilisées
!
!  Revision 1.3  2008/04/11 10:09:36  vivaresf
!  FA-ID 778 : renommage des variables mal nommées
!  suppression des doubles déclarations
!  rajout de implicit none
!  rajout de intent(in/out)
!  suppression des lignes de code commentées
!
!  Revision 1.2  2008/04/10 10:57:32  vivaresf
!  Version 2.4 : correction des cartouches
!
!  Revision 1.1  2008/02/08 17:51:38  vivaresf
!  FA-ID 889, DM-ID 820 :
!  - réorganisation du code pour que tout soit dans le répertoire src
!  Revision 1.12  2007/07/12 13:55:40  vivaresf
!  DM-ID 538 : gestion des théories
!  Revision 1.11  2007/07/11 11:53:13  couturis
!  DM-ID 538: integration de GS_SYSTEME_REF qui implique une modification de syst_const (string) qui est le systeme de constantes en code_thpla (entier)
!  Revision 1.10  2006/10/25 12:17:01  vpg
!  Resolution d'un probleme de compilation
!  Revision 1.9  2006/10/23 12:56:03  vpg
!  DM-ID 566 : Cloture du FT (Amelioration de l ergonomie des IHM de COMPAS_UI V2-0)
!  Revision 1.8.4.1  2006/10/23 10:11:59  vpg
!  DM-ID 566 : Version initiale du FT (Amelioration de l ergonomie des IHM de COMPAS_UI V2-0)
!  Revision 1.8  2006/06/16 17:32:03  vivaresf
!  Cartouches d'entete
!  Revision 1.7  2006/06/16 15:17:14  vivaresf
!  Validation
!  Revision 1.6  2006/06/14 12:38:13  vivaresf
!  DM-ID 387 : mise au point de CREATEPHEM
!  Revision 1.5  2006/05/31 13:08:37  vivaresf
!  COMPAS 2.0 : validation
!  Revision 1.4  2006/04/20 13:17:10  vpg
!  Recuperation du repertoire fcf/ avec le parametre compas_ui_fcf
!  Revision 1.3  2006/04/11 13:39:49  vpg
!  Remplacement de l'appel a read_saisie_createphem() par la routine read_donnees()
!  Revision 1.2  2006/03/17 16:44:58  fortinh
!  Mise a jour
!  Revision 1.1  2006/03/17 10:22:29  fortinh
!  Ajout des sources de CREATEPHEM
!
!$FinHistorique
!
!$Remarques
!
!$Mots-cles
!    Tchebychev, éphémérides
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  use mspro
  use msp_gestion_erreur
  use EPHEM
  use cps_acces
  use ui_io
  
  implicit none

! SVN Source File Id
  character(len=256) :: SVN_VER =  '$Id: ui_createphem.F90 69 2012-09-11 08:33:34Z ffsm $'

  
  integer, parameter  :: nbplanetmx = 15
  !Longueur max d'unn enregistrement du fichier de sortie = 722*23
  integer, parameter  :: RecordLength=16606
  
  character (LEN=100) :: fich
  character (LEN=100) :: ficheph
  character (LEN=100) :: directo
  character (LEN=32)  :: date_repere, ctdeb, ctfin
  character (LEN=32)  :: nomplanete(nbplanetmx)
  integer  :: codeplanete(nbplanetmx)
  
  
  integer :: ndeg,lfnperso
  integer :: ii
  integer :: ier, iret, iostat
  integer :: iaccesw
  
  real (KIND=pm_reel) :: dureej
  real (KIND=pm_reel) :: tref
  
  character(LEN=256) :: ch_data,ch_resul,ch
  integer :: lch
  
  type (tm_code_retour) :: code_retour
  type (tm_jour_sec)    :: jjsec, jjsec2
  real (KIND=pm_reel)   :: sec
  integer :: an,mois,jour,heure,min
  
  character(LEN=256) :: dirfcf_gslib, dirfcf
  integer :: lch_dirfcf_gslib, lch_dirfcf
  
  ! DM 387 : variables pour GS_AXES
  !real(KIND=PM_REEL), dimension(10) :: rep
  !logical :: valid
  real(KIND=PM_REEL) :: vrot, angle_u, angle_v, daterepsor
  real(KIND=PM_REEL) :: obliq, lref, delta_tai
  integer :: nbbulltot,nbbull
  integer, dimension(:), pointer :: corpsci => NULL()
  type(EPH_BULLETIN), dimension(:), pointer :: tabull => NULL()
  logical :: datecour
      
  type(MSP_MESSAGE) :: messages

  ! Common
  !     tref         = Date de reference du repere de sortie en JJ50 TE
  !     delta_tai    = ecart TE/TAI (32.184)
  common/date_ref/tref, delta_tai, vrot, datecour


  ! Suppression du common cps_createphem partagé avec l'IHM
  ! imodref	   Source éphémérides
  ! code_thpla     Code de la théorie planétaire
  ! nbfic	  
  ! fichier_init  
  ! testfs	   1 = MADONNA ; 2 = colonne 
  ! nbplanet	   Nombre de planetes a etudier
  ! datedebfic     Date debut fichier
  ! datefinfic     Date fin fichier
  
  ! echdatedeb     Echelle date début fichier
  ! echdatefin     Echelle date fin fichier
  ! echdaterep     Echelle date définition repère de sortie
  integer :: idrepsor , code_rep,plarepsor,codedaterepsor
  integer :: echdatedeb,echdatefin,echdaterep
  integer :: corcen, nbplanet,imodref,testfs,nbfic,degretch
  common /createphem3/imodref,testfs

  character(len=80) :: fichier_init(10)
  integer :: code_thpla ! code de la théorie planétaire
  real(kind=PM_REEL) :: datedebfic, datefinfic, dureetch
  integer, dimension(15) :: codepla ! Code des corps etudiés
  character(len=32), dimension(15) :: nompla ! Nom des corps d interet
  character(len=50) :: textetyperep, textedaterep, texteplarep
  character(len=10) :: unite
  integer :: trouve
  character (LEN=256)  :: nom_theorie

  ! Tableaux des valeurs de saut du TUC lue dans un fichier
  integer     :: nb_saut_tuc
  type(tm_jour_sec), dimension(pm_nb_max_saut_tuc) ::  tdate_saut_tuc
  real(KIND=PM_REEL),dimension(pm_nb_max_saut_tuc) ::  tdelta_saut_tuc
  character(len=80) :: nomfic_tuc

  ! Initialisations
  !----------------
  
  ! Ressources

  ! Domaine de traduction CREATEPHEM
  ier = AMv_rc_get ('compas_ui_fcf','compas','','dirfcf',dirfcf, &
       lch_dirfcf)
  ier = DOMchargeDomaine(dirfcf(1:lch_dirfcf)// &
       "/CREAT_MESSAGE_fr.conf","createphem_fr")
  
  ! Erreur CREATEPHEM
  call MSP_ajouter_fichier_message (trim(dirfcf)//"/CPSUI_erreur_fr")

  ! Repertoires
  iret= AMv_rc_get('DATA_CREATEPHEM','compas','interplanetaire','',ch,lch)
  if (ch(lch:lch)=="/") lch=lch-1
  ch_data=ch(:lch)
  iret= AMv_rc_get('RESUL_CREATEPHEM','compas','interplanetaire','',ch,lch)
  if (ch(lch:lch)=="/") lch=lch-1
  ch_resul=ch(:lch)
  
  
  ! Initialisation Domaine de traduction GSLIB
  ier = AMv_rc_get('fcf_gslib','compas','','fcf_gslib', &
       dirfcf_gslib,lch_dirfcf_gslib)
  ier = DOMchargeDomaine(dirfcf_gslib(1:lch_dirfcf_gslib)// &
       "/GS_MESSAGES_fr.conf","gslib")
  
  ! Initialisations COMPAS
  call cps_init_utilisateur()
  
  !Lecture des donnees 
  !--------------------
  
  call read_donnees(trim(ch_data)//"/createphem.donref", code_rep,     &
       vrot, obliq, lref, angle_u, angle_v, code_thpla, imodref, nbfic,&
       fichier_init, testfs, corcen, dureetch, degretch, datedebfic,   &
       datefinfic, echdatedeb, echdatefin, echdaterep, idrepsor,       &
       plarepsor, codedaterepsor, daterepsor, nbplanet, nompla, codepla, datecour, &
       textetyperep, textedaterep, texteplarep)
  
  ! fichier de sortie
  directo  = trim(ch_resul)
  fich     = "createphem.resul"
  
  !~ Recupération de la liste des coprs a étudier
  do ii=1,nbplanet
     nomplanete(ii) = nompla(ii)(1:)
     codeplanete(ii) = codepla(ii)
  enddo
 
  !     DM-538: initialisation théorie courante via son code (code_thpla)
  call cps_CodeToTheorie(code_thpla,nom_theorie)

  ! Repere
  tref = daterepsor
  if (vrot.eq.0) trouve = cps_getCsteTh(plarepsor, nom_theorie, "vrot", vrot,unite)
  if (vrot.eq.0) trouve = cps_getCsteThCourante(plarepsor, "vrot", vrot,unite)

  dureej = dureetch
  ndeg   = degretch
  
  ficheph=trim(directo)//"/"//trim(fich)
  
  if(nbplanet.gt.nbplanetmx) then
     ier = -1
     call MSP_signaler_message(routine='PROGRAMME creat_ephem ',       &
          cle_mes='CREA_PLAMAX',                                       &
          partie_variable='Nombre de planetes demandées > nombre autorisé')
     goto 999
  endif
  
  !- Si la date est en TUC (code 333) elle est passee en TE (code 111)
  !- automatiquement sinon elle reste telle quelle.
  !------------------------------------------

  ! Fichier des sauts du TUC
  nomfic_tuc=trim(dirfcf_gslib)//"/tai-tuc.dat"

  call md_lire_saut_tuc(nomfic_tuc, nb_saut_tuc, tdate_saut_tuc, tdelta_saut_tuc, &
        code_retour=code_retour)

  call md_jourfrac_joursec(datedebfic,jjsec,code_retour)
  ! convertir en TE
  jjsec2 = jjsec
  if (echdatedeb == pm_TUC) call md_ech_temps(pm_TUC,jjsec,pm_TE,delta_tai,jjsec2, &
       nb_saut_tuc=nb_saut_tuc, date_saut_tuc=tdate_saut_tuc, &
       delta_saut_tuc=tdelta_saut_tuc,code_retour=code_retour)

  ! ecrire sous forme de date calendaire
  call md_julien_calend(jjsec2,an,mois,jour,heure,min,sec,code_retour)
  write(ctdeb,1000) jour,mois,an,heure,min,sec
  call md_joursec_jourfrac(jjsec2,datedebfic,code_retour)
  

  ! Date de fin
  call md_jourfrac_joursec(datefinfic,jjsec,code_retour)
  ! convertir en TE
  jjsec2 = jjsec
  if (echdatefin == pm_TUC) call md_ech_temps(pm_TUC,jjsec,pm_TE,delta_tai,jjsec2, &
       nb_saut_tuc=nb_saut_tuc, date_saut_tuc=tdate_saut_tuc, &
       delta_saut_tuc=tdelta_saut_tuc,code_retour=code_retour)
  ! ecrire sous forme de date calendaire
  call md_julien_calend(jjsec2,an,mois,jour,heure,min,sec,code_retour)
  write(ctfin,1000) jour,mois,an,heure,min,sec
  call md_joursec_jourfrac(jjsec2,datefinfic,code_retour)


  ! Date de référence du repère
  call md_jourfrac_joursec(tref,jjsec,code_retour)
  ! convertir en TE
  jjsec2 = jjsec
  if (echdaterep == pm_TUC) call md_ech_temps(pm_TUC,jjsec,pm_TE,delta_tai,jjsec2, &
       nb_saut_tuc=nb_saut_tuc, date_saut_tuc=tdate_saut_tuc, &
       delta_saut_tuc=tdelta_saut_tuc,code_retour=code_retour)

  ! ecrire sous forme de date calendaire
  call md_julien_calend(jjsec2,an,mois,jour,heure,min,sec,code_retour)
  write(date_repere,1000) jour,mois,an,heure,min,sec
  call md_joursec_jourfrac(jjsec2,tref,code_retour)

  ! Ecart TAI/TE
  call md_ech_temps(pm_TE,jjsec,pm_TAI,delta_tai,jjsec2, code_retour=code_retour)

  !-----------------------------------------------------------------------------
  ! 	NUMERO MODELE REFERENCE A UTILISER POUR CREER LE FICHIER
  !                    = 10 si EPHEMERIDES  ANALYTIQUES BDL VSOP82'
  !                    = 20 si EPHEMERIDES NAIF DE200'
  !                    = 21 si EPHEMERIDES NAIF DE403'
  !                    = 22 si EPHEMERIDES NAIF DE405'
  !                    = 30 si EPHEMERIDES  FICHIER BDL VSOP82'
  !-----------------------------------------------------------------------------
  if (((imodref/10).eq.4).or.((imodref/10).eq.5)) then 
     ier  = -4
     call MSP_signaler_message(routine='PROGRAMME creat_ephem ',       &
          cle_mes='CREA_MODREF',                                       &
          partie_variable='Erreur paramètre imodref (N0 modèle référence)')
     goto 999
  endif
  
  !-----------------------------------------------------------------------------
  ! Initialisation de la méthode
  !-----------------------------------------------------------------------------
  
  ! Initialisation 
  if (imodref==11) then
     allocate (corpsci(4*nbplanet))
     allocate (tabull(4*nbplanet))

     nbbull=0
     nbbulltot=0
     do ii=1,nbplanet
        call eph_kep_lirekep(codepla(ii), corcen, tabull(nbbull+1:nbbull+4), &
             corpsci(nbbull+1:nbbull+4), nbbull)
        nbbulltot=nbbulltot+nbbull
     enddo
     call eph_initposcor(imodref, tabull=tabull, codecorps=corpsci, nbbull=nbbulltot)
  else
     call eph_initposcor(imodref, nbfic, fichiers=fichier_init)
  endif
  
  !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  !
  !	REALISATION DU FICHIER EPHEMERIDES CNES TCHEBYCHEV
  !
  !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  
  !----------------------------------------------------------------------------
  !	Ouverture du fichier	
  !----------------------------------------------------------------------------
  
  !     On verifie le type de format du fichier a produire
  if (testfs == 2) then
     !        Fichier au format Colonnes
     call eph_util_ficunit90(lfnperso,ier)
     if(ier.lt.0)then
        goto 999
     endif
     
     open(lfnperso,FILE=ficheph,ACTION='WRITE',STATUS='REPLACE',       &
          RECL=RecordLength,IOSTAT=iostat)
     if (iostat.ne.0) then
        ier = -4
        call MSP_signaler_message(routine='PROGRAMME creat_ephem ',    &
             cle_mes='CREA_FICOUV', &
             partie_variable='problème à l ouverture du fichier résultat')
        goto 999
     endif
  else
     !Fichier au format MADONA
     iaccesw = acc_open()
     
     iret=acc_connect(iaccesw,ficheph,'w')
     if (iret.ne.0) then
        write(6,'(a45,a100)') 'ERREUR -- Impossible d''accéder au' &
             // 'fichier : ',ficheph
        goto 999
     endif
     lfnperso=iaccesw
     
  endif
  
  !----------------------------------------------------------------------------
  !	Calculs proprement dits	
  !----------------------------------------------------------------------------
  
  call creatfic_ephem(lfnperso,ctdeb,datedebfic,ctfin,datefinfic,dureej,&
       ndeg,corcen,nbplanet,nomplanete,codeplanete,idrepsor,            &
       code_rep, date_repere, codedaterepsor,daterepsor,                    &
       plarepsor,imodref,code_thpla,vrot, obliq, lref, angle_u,    &
       angle_v, datecour, ier,textetyperep, textedaterep, texteplarep)
  if(ier.lt.0)then
     ier = -4
     call MSP_signaler_message(routine='PROGRAMME creat_ephem ',cle_mes='CREA_FICEPHEM', &
          partie_variable='aprés execution creatfic_ephem ')
     goto 999
  endif
  
  !----------------------------------------------------------------------------
  !	On ferme le fichier qu'on a crée 	
  !----------------------------------------------------------------------------
  
  if (testfs ==2) then       ! Colonnes
     close(lfnperso)
  endif
  
999 continue
  call flush(6)
  ! Messages d'erreur
    if (MSP_PROBLEME) then
       call MSP_recuperer_message (message=messages,nb_mes=MSP_tous_messages)
       call MSP_afficher_message (message=messages,unit=0)
    endif

  ! Formats iternes
1000 format(i2.2,"/",i2.2,"/",i4.4," ",i2.2,":",i2.2,":",f6.3)
  
end program ui_createphem


! DM 538 : entrée dans creatfic_ephem d'un code de théorie (code_thpla)
! au lieu d'une chaîne de caractère (sys_const)
subroutine creatfic_ephem(lfn,ctdeb,tdeb,ctfin,tfin,dureej,ndeg,       &
     numplac, nbplanet,nompla,numpla,idrepsor,code_rep, date_repere,   &
     echdaterep,daterepsor,plarepsor, ktype,code_thpla,vrot,      &
     obliq, lref, angle_u, angle_v, datecour, ier, &
     textetyperep, textedaterep, texteplarep)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  creatfic_ephem
!
!$Resume
! 	CREATION FICHIER CNES D'EPHEMERIDES DES PLANETES DU SYSTEME SOLAIRE
!       PRESENTEES SOUS FORME DE COEFFICIENTS DE TCHEBYCHEV
!
!$Description
! 	CREATION FICHIER CNES D'EPHEMERIDES DES PLANETES DU SYSTEME SOLAIRE
!       PRESENTEES SOUS FORME DE COEFFICIENTS DE TCHEBYCHEV
!
!$Auteur
!       Equipe interplanétaire
!
!$Acces
!  PUBLIC
!
!$Usage
!  call creatfic_ephem(lfn,ctdeb,tdeb,ctfin,tfin,dureej,ndeg,       &
!.         numplac, nbplanet,nompla,numpla,idrepsor,code_rep, date_repere,   &
!.         echdaterep,daterepsor,plarepsor, ktype,code_thpla,vrot,      &
!.         obliq, lref, angle_u, angle_v, datecour, ier, &
!.         textetyperep, textedaterep, texteplarep)
!.    integer :: lfn
!.    integer :: ndeg
!.    integer :: nbplanet
!.    integer :: ktype
!.    integer :: code_thpla
!.    integer :: numplac
!.    character(LEN=32), dimension(15) :: nompla
!.    integer, dimension(15) :: numpla
!.    integer :: idrepsor
!.    integer :: code_rep 
!.    character(LEN=32) :: date_repere 
!.    integer :: echdaterep
!.    integer :: daterepsor
!.    integer :: plarepsor
!.    character(LEN=*) :: ctdeb
!.    character(LEN=*) :: ctfin
!.    real (KIND=pm_reel) :: dureej
!.    real (KIND=pm_reel) :: tdeb
!.    real (KIND=pm_reel) :: tfin
!.    real (KIND=pm_reel) :: vrot, obliq, lref, angle_u, angle_v
!.    logical :: datecour
!.    integer :: ier 
!.    character (LEN=*) :: textetyperep, textedaterep, texteplarep
!.    integer :: ier
!
!$Arguments
!>E     lfn           :<integer>            Numéro de tape du fichier         
!>E     ctdeb         :<LEN=*>              Date début du fichier format calendaire (TE)
!>E     tdeb          :<pm_reel>            Date début du fichier J50, TE
!>E     ctfin         :<LEN=*>              Date début du fichier forùat calendaire (TE)    
!>E     tfin          :<pm_reel>            Date fin du fichier J50, TE         
!>E     dureej        :<pm_reel>            Intervalle de validité (en jours) des polynomes 
!>E     ndeg          :<integer>            Degré des polynômes de Tchebychev         
!>E/S   numplac       :<integer>            Code du corps central
!>E     nbplanet      :<integer>            Nombre de corps dont on veut les éphemérides (max 15)
!>E/S   nompla        :<LEN=32,DIM=(15)>    Liste des corps dont on veut les éphémérides 
!>E/S   numpla        :<integer,DIM=(15)>   Codes des corps dont on veut les éphémérides 
!>E     idrepsor      :<integer>            Type de repère (MSPRO)        
!>E     code_rep      :<integer>            code du repère de sortie (Sur 10 caractères)         
!>E     date_repere   :<LEN=32>             Date de reference du repere (sur 32 caracteres) 
!>E     echdaterep    :<integer>            Type de date de reference du repere         
!>E     daterepsor    :<integer>            Date de reference du repère
!>E     plarepsor     :<integer>            Planete de reference du repere                 
!>E     ktype         :<integer>            Type d'éphémérides qu'ont veut traiter
!.                    = 10 ---->....  EPHEMERIDES FICHIER BDL VSOP82
!.                    = 20 ---->....  EPHEMERIDES NAIF DE200'
!.                    = 21 ---->....  EPHEMERIDES NAIF DE403'
!.                    = 22 ---->....  EPHEMERIDES NAIF DE405'
!.                    = 30 ---->....  EPHEMERIDES ANALYTIQUES BDL VSOP82'
!>E     code_thpla    :<integer>            Théorie planète          
!>E     vrot          :<pm_reel>            Vitesse de rotation de plarepsor
!>E     obliq         :<pm_reel>            Obliquité 
!>E     lref          :<pm_reel>            Longitude de reference du repere   
!>E     angle_u       :<pm_reel>            Angle pole%u du repere
!>E     angle_v       :<pm_reel>            Angle pole%v du repere          
!>E     datecour      :<logical>            OK si repere a la date courante
!>E/S   ier           :<integer>            code d'erreur (OK=0)         
!>E     textetyperep  :<LEN=*>              Champs GS_LIB du repere "rep"
!>E     textedaterep  :<LEN=*>              Champs GS_LIB du repere "ech_date"
!>E     texteplarep   :<LEN=*>              Champs GS_LIB du repere "pla"
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
  use EPHEM
  use cps_utilisateur
  use msp_gestion_erreur
  use cps_math_tchcal_mod
  use ui_createphem_io

  implicit none
  
  ! Arguments
  
  integer, intent(in) :: lfn
  integer, intent(in) :: ndeg
  integer, intent(in) :: nbplanet
  integer, intent(in) :: ktype
  integer, intent(in) :: code_thpla
  integer, intent(inout) :: numplac
  character(LEN=32), dimension(15), intent(inout) :: nompla
  integer, dimension(15), intent(inout) :: numpla
  integer, intent(in) :: idrepsor
  integer, intent(in) :: code_rep ! DM 387 : code a trois chiffres du repere de sortie
  character(LEN=32), intent(in) :: date_repere ! DM 387
  integer, intent(in) :: echdaterep
  integer, intent(in) :: daterepsor
  integer, intent(in) :: plarepsor
  character(LEN=*), intent(in)  :: ctdeb
  character(LEN=*), intent(in)  :: ctfin
  real (KIND=pm_reel), intent(in) :: dureej
  real (KIND=pm_reel), intent(in) :: tdeb
  real (KIND=pm_reel), intent(in) :: tfin
  real (KIND=pm_reel), intent(in) :: vrot, obliq, lref, angle_u, angle_v
  logical, intent(in) :: datecour
  integer, intent(out) :: ier 
  character (LEN=*), intent(in) :: textetyperep, textedaterep, texteplarep
  
  ! Paramètres
  integer, parameter ::nbplanetmx=15
  integer, parameter ::nparam = 3
  integer, parameter ::ndegp1f3max=48
  integer, parameter ::nbdatemax=4000

  integer, parameter :: lg_max_chaine = 80
  integer, parameter :: code_erreur_entete = -6
  integer, parameter :: code_erreur_lecture = -11
  integer, parameter :: valeur_non_fixee = -999

  ! Common

  integer :: imodref
  integer :: testfs
  common /createphem3/imodref,testfs
 
  
  ! Variables locales
  character(LEN=1) :: baratin(lg_max_chaine)
  integer :: icoeff
  integer :: iplanet
  integer :: nbcoeff
  integer :: nrtab
  integer :: ntotpar
  character(LEN=32) :: nomplac
  integer :: npt
  integer :: ind,indmax
  integer :: iaccesw
  integer :: i,j,k
  integer :: indtps
  integer :: iret,ndegp1,ndegp1f3
  integer :: idate,ipla
  integer :: trouve
  
  real (KIND=pm_reel) :: dureejj
  
  real (KIND=pm_reel) :: tab_date(nbdatemax)
  real (KIND=pm_reel) :: tab_tcheb(nbdatemax,nbplanetmx,ndegp1f3max)
  character(len=4)  :: cnbdatemax
  
  !-----------------------------------------------------------------------------
  !	nb coeff tchebychev maxi =
  !          nb composantes*(ndeg maxi+1)*(nombre planetes maxi)
  !         = nparam            *16           *15
  !----------------------------------------------------------------------------
  
  integer, parameter :: nbcoeffmx = 3*16*15
  integer, parameter :: nrtabmx = 21
  real (KIND=pm_reel) :: t1, t2
  real (KIND=pm_reel) :: rtab(nrtabmx)
  real (KIND=pm_reel) :: ctch(nbcoeffmx)
  real (KIND=pm_reel) :: mu(nbplanetmx)        ! constantes de gravitation
  character (LEN=20)  :: mu_unit(nbplanetmx)   ! unites des constantes de gravitation
  character (LEN=256)  :: nom_theorie

  ! Fonctions
  
  external tr_creat_ephem
  
  data baratin /'*','*','*','*','*','*','*','*','*','*' &
       ,'*','*','*','*','*','*','*','*','*','*'         &
       ,'*','*','*','*','*','*','*','*','*','*'         &
       ,'*','*','*','*','*','*','*','*','*','*'         &
       ,'*','*','*','*','*','*','*','*','*','*'         &
       ,'*','*','*','*','*','*','*','*','*','*'         &
       ,'*','*','*','*','*','*','*','*','*','*'         &
       ,'*','*','*','*','*','*','*','*','*','*'/
  
  !-------------------------------------------------------------------------------
  !	Test sur le nombre de planètes
  !-------------------------------------------------------------------------------
  
  if(nbplanet.gt.nbplanetmx)then
     ier = -1
     call MSP_signaler_message(routine='creatfic_ephem ',              &
          cle_mes='CREA_PLAMAX',                                       &
          partie_variable='Nombre de planètes > nombre autorisé ')
     return
  endif
  
  !-------------------------------------------------------------------------------
  !	Numéro Corps central
  !-------------------------------------------------------------------------------
  
  call cps_codenom(numplac,nomplac,"fr")      
  
  !-------------------------------------------------------------------------------
  !       Determination des constantes de gravitation des planetes etudiees
  !-------------------------------------------------------------------------------

  call cps_CodeToTheorie(code_thpla,nom_theorie)
  
  do i=1,nbplanet
     trouve=cps_getCsteTh(numpla(i), nom_theorie, "mu", mu(i), mu_unit(i))
  enddo
  
  !-------------------------------------------------------------------------------
  !	Calcul du nombre total de paramètres à tchebycheviser
  !       et du nombre de coefficients à calculer
  !	On calcule aussi le nombre de paramètres à stocker dans le tableau rtab
  !-------------------------------------------------------------------------------
  
  ntotpar = nparam*nbplanet
  nbcoeff = ntotpar*(ndeg+1)
  if (nbcoeff.gt.nbcoeffmx) then
     ier = -2
     call MSP_signaler_message(routine='creatfic_ephem ',cle_mes='CREA_TCHEMAX', &
          partie_variable='Nombre coefficients Tchebychev > nombre autorisé')
     return
  endif
  nrtab = nbplanet + 4
  if (nrtab.gt.nrtabmx) then
     ier = -3
     call MSP_signaler_message(routine='creatfic_ephem ',cle_mes='CREA_TABMAX', &
          partie_variable='Tableau rtab insuffisamment dimensionné  ')
     return
  endif
  
  !-------------------------------------------------------------------------------
  !	Réajustement du pas de calcul
  !------------------------------------------------------------------------------
  
  npt = int((tfin-tdeb)/dureej)
  if((tfin-tdeb).gt.(dureej+1.e-12_pm_reel))then
     dureejj = dureej 
  else
     dureejj = (tfin-tdeb)
  endif
  
  !-------------------------------------------------------------------------------
  !	Test sur le nombre de dates dans le cas MADONA
  !-------------------------------------------------------------------------------
  
  if (testfs == 1) then
     write(cnbdatemax,'(i4)') nbdatemax
     
     if (npt > nbdatemax) then
        ier = code_erreur_lecture ! => -11
        call MSP_signaler_message(routine='creatfic_ephem ',cle_mes='CREA_DATEMAX', &
             partie_variable='Nombre de dates calculées > maximum autorisé ('//trim(cnbdatemax)//')')
        return
     endif
  endif
  
  
  !-------------------------------------------------------------------------------
  !	Ecriture de l'En-Tete
  !-------------------------------------------------------------------------------
  
  if (testfs == 2) then
     !Format de sortie demande :  Colonne
     call ecrire_entete_col(lfn,ktype,nomplac,code_rep,date_repere,&
          nbplanet,nompla,dureejj,ndeg,ctdeb,ctfin,datecour, ier)
     if (ier.lt.0) then
        ier = code_erreur_entete  ! => -6
        call MSP_signaler_message(routine='creatfic_ephem ',           &
             cle_mes='CREA_CENTFICPLA',                                &
             partie_variable='après  exécution centfic_pla  ')
        return
     endif
  else
     !Format de sortie demande : MADONA
     iaccesw=lfn
     call ecrire_entete_mad(iaccesw,ktype,nomplac,code_rep, date_repere,&
          nbplanet,nompla,mu,dureejj,ndeg,ctdeb,ctfin,vrot,obliq,lref,  &
          angle_u, angle_v,datecour, ier,textetyperep, textedaterep, texteplarep)
     
     if(ier.lt.0)then
        ier = code_erreur_entete  ! => -6
        call MSP_signaler_message(routine='creatfic_ephem ',       &
             cle_mes='CREA_CENTFICPLA',                            &
             partie_variable='après  exécution centfic_pla')  
        return
     endif
     
  endif
      
  !-------------------------------------------------------------------------------
  !	Initialisations
  !	On stocke dans rtab un certain nombre de paramètres qu'on récupèrera
  !	lors de l'éxecution de la subroutine d'interface tr_creat_ephem
  !-------------------------------------------------------------------------------
      
  rtab(1) = real(numplac, KIND=pm_reel)    ! Corps central
  rtab(2) = real(nbplanet, KIND=pm_reel)   ! Nombre de planete a étudier
  rtab(3) = real(idrepsor, KIND=pm_reel)   ! le repere de sortie
  rtab(4) = real(echdaterep, KIND=pm_reel) ! l'échelle de date du repere de sortie
  rtab(5) = real(plarepsor, KIND=pm_reel)  ! la planete du repere de sortie
  rtab(6) = real(ktype, KIND=pm_reel)      ! la théorie d'éphémérides utilisée
  do  iplanet=1,nbplanet
     rtab(iplanet+6) = real(numpla(iplanet), KIND=pm_reel) ! les planetes a étudier
  end do
  
  t1 = tdeb
  
  !     Initialisations specifique pour le format MADONA
  ndegp1 = ndeg + 1
  ndegp1f3 = nparam * ndegp1
  indtps = 0
      
      
      !--------------------------------------------------------------------------------
      !	Bouclage sur les dates
      !--------------------------------------------------------------------------------

20    continue

      t2 = t1 + dureejj
      if (t1.lt.(tfin+dureejj)) then
         !
         !--------------------------------------------------------------------------------
         !	calcul des coefficients de tchebychev
         !--------------------------------------------------------------------------------
         
         indmax = int(((tfin-t1+1.e-12_pm_reel)/(tfin-tdeb))*80._pm_reel)
         if (indmax.ge.1) then
            write(6,'(1x,80(a1))')(baratin(ind),ind=1,indmax)
            call flush(6)
         endif
         
         call cps_math_tchcal (t1,t2,rtab,ndeg,ntotpar, &
              tr_creat_ephem,nbcoeffmx,ctch,ier)
         if (ier.lt.0) then
            ier = -10
            call MSP_signaler_message(routine='creatfic_ephem ',       &
                 cle_mes='CREA_CENTFICPLA',                            &
                 partie_variable='après  exécution cps_math_tchcal ')
            return
         endif
         
         
         !--------------------------------------------------------------------------------
         !	Ecriture sur le fichier des intervalles de temps et des coefficients de
         !	Tchebychev correspondant
         !--------------------------------------------------------------------------------
         !     Pour le format de sortie Colonnes  ecriture des dates
         if (testfs == 2) then
            write(lfn,150)t1,t2,(ctch(icoeff),icoeff=1,nbcoeff)
            t1 = t2
            goto 20
         else
            indtps = indtps + 1
            tab_date(indtps) = t1
            do j=1,nbplanet
               do k=1,ndegp1f3
                  tab_tcheb(indtps,j,k) = ctch((j-1)*ndegp1f3+k)
               enddo
            enddo
            t1 = t2
            goto 20
         endif
      endif

      ! Fin boucle
      
      !Pour le format de sortie MADONA  ecriture du tableau des dates
      if (testfs == 1) then
         
         iret = acc_create(iaccesw,"Dates_tchebycheff",ACC_TABL,"")
         iret = acc_select(iaccesw,"Dates_tchebycheff",ACC_TABL)
         do idate=1,indtps
            iret = acc_set_index(iaccesw,idate)
            iret = acc_putd(iaccesw,ACC_INDEX,tab_date(idate),"")
         enddo
         
         iret = acc_select_end(iaccesw)
         iret = acc_create(iaccesw,"Coefficients_tchebycheff",ACC_TABL,"")
         iret = acc_select(iaccesw,"Coefficients_tchebycheff",ACC_TABL)
         
         do idate=1,indtps
            iret = acc_set_index(iaccesw,idate)
            iret = acc_create(iaccesw,ACC_INDEX,ACC_TABL,"")
            iret = acc_select(iaccesw,ACC_INDEX,ACC_TABL)
            
            do ipla=1,nbplanet
               iret = acc_set_index(iaccesw,ipla)
               iret = acc_create(iaccesw,ACC_INDEX,ACC_TABL,"")
               iret = acc_select(iaccesw,ACC_INDEX,ACC_TABL)
               
               do icoeff=1,ndegp1f3
                  iret = acc_set_index(iaccesw,icoeff)
                  iret = acc_putd(iaccesw,ACC_INDEX                    &
                       ,tab_tcheb(idate,ipla,icoeff),"")
               enddo
               iret =  acc_select_end(iaccesw)
            enddo
            iret = acc_select_end(iaccesw)
         enddo
         iret = acc_select_end(iaccesw)
         iret = acc_putcom(iaccesw,"Coefficients_tchebycheff",1 &
              ,"Tableau des coefficients de Tchebychev de dimension 3 :")
         ! Note valeur_non_fixee = -999
         iret = acc_putcom(iaccesw,"Coefficients_tchebycheff",valeur_non_fixee &
              ,"dim 1 : indice sur les dates")
         iret = acc_putcom(iaccesw,"Coefficients_tchebycheff",valeur_non_fixee &
              ,"dim 2 : indice sur les planètes")
         iret = acc_putcom(iaccesw,"Coefficients_tchebycheff",valeur_non_fixee &
              ,"dim 3 : coefficients de Tchebychev des 3 coordonnées")
         
         !Ecriture de la zone d'acces
         
         iret = acc_write(iaccesw,ACC_ALL)
         iret = acc_close(iaccesw)
      endif

! 5000  format(1x,i5)
      !--------------------------------------------------------------------------------
      !	le format de sortie est égal à :
      !       3*(15+1)*15 + 2 = 722  (Cf Valeurs maxi autorisées)
      !--------------------------------------------------------------------------------
      
150   format(722d23.16)

    end subroutine creatfic_ephem
    

    subroutine tr_creat_ephem (t1950,rtab,nx,x,ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  tr_creat_ephem
!
!$Resume
!   Routine d'appel de eph_poscor + chgt de repere
!
!$Description
!
!$Auteur
!       Equipe interplanétaire
!
!$Acces
!  PUBLIC
!
!$Usage
!  call tr_creat_ephem (t1950,rtab,nx,x,ier)
!.    integer :: nx
!.    real(KIND=pm_reel) :: t1950
!.    real(KIND=pm_reel), dimension(*) :: rtab
!.    real(KIND=pm_reel), dimension(*) :: x
!.    integer :: ier
!
!$Arguments
!>E     t1950  :<pm_reel>           Date courante 1950.0 CNES de calcul (Echelle de temps TE) 
!>E     rtab   :<pm_reel,DIM=(*)>   Tableau de tramsfert de paramètres de fonctionnement  
!>E     nx     :<integer>           Nombre de composantes à calculer        
!               (Ici = 3*nombre de planètes)  	
!>S     x      :<pm_reel,DIM=(*)>   Positiions des planètes
!               x(1)             = composante cart.  en x première planète
!               x(1)             = composante cart.  en y première planète
!               x(1)             = composante cart.  en z première planète
!               ....................................................
!       	x(3*nbplanet-2)  = composante cart.  en x dernière planète
!      		x((3*nbplanet-1) = composante cart.  en y dernière planète
!      	        x((3*nbplanet)   = composante cart.  en z dernière planète
!                                                  etc.....      	
!>S     ier    :<integer>           code d'erreur
!				=  0 si ok
!                               = -1 ... Arrêt aprés  exécution poscor
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
      use ephem
      use msp_gestion_erreur
      
      implicit none
      
      ! Arguments
      integer, intent(in) :: nx
      real(KIND=pm_reel), intent(in) :: t1950
      real(KIND=pm_reel), dimension(*), intent(in) :: rtab
      real(KIND=pm_reel), dimension(*), intent(out) :: x
      integer, intent(out) :: ier
      
      ! Variables locales
      integer :: iplanet,index,nbplanet,numpla,numplac
      integer :: ktype
      real (KIND=pm_reel) :: xpla(6), xpla2(6)
      
      integer :: repe, eche, plae, nutae
      integer :: reps, echs, plas, nutas
      real(KIND=PM_REEL)::  delta_tu1
      type(tm_jour_sec) :: datereps
      type(tm_pole_uv) :: pole
      type(tm_code_retour) :: code_retour
      
      ! Common
      real (KIND=pm_reel) :: tref, delta_tai, vrot
      logical :: datecour
      common/date_ref/tref, delta_tai, vrot, datecour

      !! Initialisation
      
      ier = 0
      delta_tu1=0.0_PM_REEL

      !-------------------------------------------------------------------------------
      !       Récupération des coefficients spécifiques
      !-------------------------------------------------------------------------------
      
      numplac  = rtab(1)
      nbplanet = rtab(2)
      ktype = rtab(6)
      

      ! Changement de repère 
      ! les repères station et rampe ne sont pas traités
      
      ! REPERE D'ENTREE : EME 2000
      repe = pm_equa_moy
      plae = pm_pla_terre
      eche = pm_1janvier2000_12h00
      nutae = pm_lieske_wahr_aoki
      
      ! REPERE DE SORTIE (info de rtab)
      reps = rtab(3)
      plas = rtab(5)
      echs = rtab(4)
      nutas = pm_uai2000
      if (plas == 399) nutas = pm_lieske_wahr_aoki

      ! Dans le cas de la définition d'un repère de sortie avec donnée 
      ! d'une date de référence, la date de référence est celle contenue 
      ! dans tref
         
      if (datecour) then
         call md_jourfrac_joursec(t1950,datereps, code_retour)
      else
         call md_jourfrac_joursec(tref,datereps, code_retour)
      endif
         
      !-------------------------------------------------------------------------------
      !	Calcul des éphémérides de toutes les planètes
      !-------------------------------------------------------------------------------
      
      index = 1
      do iplanet = 1,nbplanet
         numpla = rtab(6+iplanet)
         
         call eph_poscor(ktype, t1950, numplac, numpla, xpla)
         if (MSP_ERREUR) then
            ier = -1
            call MSP_signaler_message(routine='tr_creat_ephem ',cle_mes='CREA_EPHPOSCOR', &
                 partie_variable='après exécution eph_poscor ')
            return
         endif
         
         !-------------------------------------------------------------------------------
         !	Changement de repere
         !-------------------------------------------------------------------------------

         call mx_rep(repe,plae,eche,nutae,xpla(1:3),xpla(4:6),         &
              reps,plas,echs,nutas,xpla2(1:3),xpla2(4:6),              &
              code_retour=code_retour, val_date_out=datereps,          &
              delta_tai_out=delta_tai,long_ref_out=0._PM_REEL,         &
              vit_rot_out=vrot, pole_out=pole, delta_tu1_out=delta_tu1)
         if (code_retour%valeur < 0) then
            call MSP_signaler_message(routine='tr_creat_ephem', cle_mes='CREA_REPCHREP', &
                 partie_variable='après exécution mx_rep ')
            return
         endif
         
         x(index)   = xpla2(1)
         x(index+1) = xpla2(2)
         x(index+2) = xpla2(3)
         index = index +3
      end do


    end subroutine tr_creat_ephem
    

! Lecture des donnees du repere de sortie et du systeme 
! d'éphémérides de reference
    subroutine read_donnees(nomfic, code_rep, vrot, obli, lref,        &
         angle_u, angle_v, code_thpla, code_ephem, nb_fic, fic,format_sortie,&
         corpsc, duree, degre, date_deb, date_fin, ech_date_deb,       &
         ech_date_fin, ech_date_rep, id_rep, pla_rep, codedate_rep, date_rep, nb_pla,&
         noms_pla, codes_pla, datecour, textetyperep, textedaterep, texteplarep)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  read_donnees
!
!$Resume
! Lecture des donnees saisie a l'IHM
!
!$Description
! Lecture des donnees saisie a l'IHM 
! Remplace le read_saisie_createphem avec lequel l'edition de lien ne
! fonctionne pas.
!
!$Auteur
!  Hervé Fortin (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call read_donnees(nomfic, code_rep, vrot, obli, lref,        &
!.             angle_u, angle_v, code_thpla, code_ephem, nb_fic, fic,format_sortie,&
!.             corpsc, duree, degre, date_deb, date_fin, ech_date_deb,       &
!.             ech_date_fin, ech_date_rep, id_rep, pla_rep, codedate_rep, date_rep, nb_pla,&
!.             noms_pla, codes_pla, datecour, textetyperep, textedaterep, texteplarep)
!.    character(LEN=*) :: nomfic
!.    integer :: code_rep, code_ephem, nb_fic
!.    real(KIND=PM_REEL) :: vrot, obli, lref, angle_u, angle_v
!.    integer :: code_thpla
!.    character(LEN=80), dimension(10) :: fic
!.    integer :: format_sortie, corpsc, degre, nb_pla
!.    real(KIND=PM_REEL) :: duree, date_deb, date_fin, date_rep
!.    integer :: ech_date_deb, ech_date_fin, ech_date_rep
!.    integer :: id_rep, pla_rep,codedate_rep
!.    integer, dimension(15) :: codes_pla
!.    character(LEN=32), dimension(15) :: noms_pla
!.    logical :: datecour
!.    character(LEN=*) :: textetyperep, textedaterep, texteplarep
!
!$Arguments
!>E     nomfic         :<LEN=*>              
!>S     code_rep       :<integer>            
!>S     vrot           :<PM_REEL>            
!>S     obli           :<PM_REEL>            
!>S     lref           :<PM_REEL>            
!>S     angle_u        :<PM_REEL>            
!>S     angle_v        :<PM_REEL>            
!>S     code_thpla     :<integer>            
!>S     code_ephem     :<integer>            
!>S     nb_fic         :<integer>            
!>S     fic            :<LEN=80,DIM=(10)>    
!>S     format_sortie  :<integer>            
!>S     corpsc         :<integer>            
!>S     duree          :<PM_REEL>            
!>S     degre          :<integer>            
!>S     date_deb       :<PM_REEL>            
!>S     date_fin       :<PM_REEL>            
!>S     ech_date_deb   :<integer>            
!>S     ech_date_fin   :<integer>            
!>S     ech_date_rep   :<integer>            
!>S     id_rep         :<integer>            
!>S     pla_rep        :<integer>            
!>S     codedate_rep   :<integer>            
!>S     date_rep       :<PM_REEL>            
!>S     nb_pla         :<integer>            
!>S     noms_pla       :<LEN=32,DIM=(15)>    
!>S     codes_pla      :<integer,DIM=(15)>   
!>S     datecour       :<logical>            
!>S     textetyperep   :<LEN=*>              
!>S     textedaterep   :<LEN=*>              
!>S     texteplarep    :<LEN=*>              
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
      use eph_info
      implicit none

      ! arguments
      character(LEN=*), intent(in) :: nomfic
      integer, intent(out) :: code_rep, code_ephem, nb_fic
      real(KIND=PM_REEL), intent(out) :: vrot, obli, lref, angle_u, angle_v
      integer, intent(out) :: code_thpla
      character(LEN=80), dimension(10), intent(out) :: fic
      integer, intent(out) :: format_sortie, corpsc, degre, nb_pla
      real(KIND=PM_REEL), intent(out) :: duree, date_deb, date_fin, date_rep
      integer, intent(out) :: ech_date_deb, ech_date_fin, ech_date_rep
      integer, intent(out) :: id_rep, pla_rep,codedate_rep
      integer, dimension(15), intent(out) :: codes_pla
      character(LEN=32), dimension(15), intent(out) :: noms_pla
      logical, intent(out) :: datecour
      character(LEN=*), intent(out)  :: textetyperep, textedaterep, texteplarep

      ! Constantes
      integer, parameter :: th_defaut = 1994
      integer, parameter :: selth_defaut = 12
      integer, parameter :: cpsi_10 = 10

      ! variables locales
      integer :: acces, i, ret
      character(LEN=50) :: buffer
      real(KIND=PM_REEL) :: jj, sec
      integer :: code_thuai
      integer :: seltheorie

      ! initialisation de code_thuai
      code_thuai = th_defaut
      seltheorie = selth_defaut

      ! ouverture du fichier
      acces = acc_load(trim(nomfic))
      
      ! lecture du code du repere de sortie
      ret = acc_geti(acces, "code_repere_sortie", code_rep)

      ! conversion du code a trois chiffres identifiant
      ! le repere de sortie en codes compatibles MSPRO
      call eph_codageRepMSPRO(code_rep, id_rep, pla_rep, codedate_rep)
 
      
      ! lecture du type de repère
      if (acc_exist(acces, "rep") == 1) ret = acc_gets(acces, "rep", textetyperep)
      ! lecture de la planète du repère
      if (acc_exist(acces, "pla") == 1) ret = acc_gets(acces, "pla", texteplarep)
      ! lecture du type de date
      if (acc_exist(acces, "ech_date") == 1) then
         ret = acc_gets(acces, "ech_date", textedaterep)
      else
         ! Repères sans date de bulletin, c'est a dire toujours à la date courante
         ! (planeto)
         textedaterep="date du bulletin"   
      endif

      ! Date de reference du repère si existe
      if (acc_exist(acces, "tref.jj") == 1) then
         ret = acc_getd(acces, "tref.jj", jj, "jj1950")
         ret = acc_getd(acces, "tref.sec", sec, "sec")
         date_rep = jj + (sec/86400._pm_reel)
         datecour=.false.
      else
         ! Repères a la date courante
         datecour = (trim(textedaterep).eq."date du bulletin")
         date_rep=0._pm_reel

         if (id_rep == pm_planeto_ref) datecour=.true.
         if (id_rep == pm_planeto_ref_iner) datecour=.true.
         if (id_rep == pm_planeto_vrai) datecour=.true.         
         if (id_rep == pm_equa_uai) datecour=.true.         
      endif

      ! Autres repères a dates fixes
      if (trim(textedaterep).eq."2000") then
         date_rep=18262.5_pm_reel 
         datecour=.false.
      endif
      if (trim(textedaterep).eq."1950") datecour=.false.
          
      ! lecture de la vitesse de rotation de la plenète du repère
      ret = acc_exist(acces, "vrot")
      if (ret.eq.1) then
         ret = acc_getd(acces, "vrot", vrot, "rad/s")
      end if

      ! lecture de l'obliqutephemtche.1col.errtephemtche.1col.errite
      ret = acc_exist(acces, "obli")
      if (ret.eq.1) then
         ret = acc_getd(acces, "obli", obli, "deg")
      end if

      ! lecture de la longitude de reference
      ret = acc_exist(acces, "lref")
      if (ret.eq.1) then
         ret = acc_getd(acces, "lref", lref, "deg")
      end if

      ! lecture 
      ret = acc_exist(acces, "angle_u")
      if (ret.eq.1) then
         ret = acc_getd(acces, "angle_u", angle_u, "deg")
      end if
      
      ! lecture 
      ret = acc_exist(acces, "angle_v")
      if (ret.eq.1) then
         ret = acc_getd(acces, "angle_v", angle_v, "deg")
      end if

! DM-538: lecture des parametres de la structure EPHEMERIDES
      ret = acc_exist(acces, "EPHEMERIDES")
      if (ret.eq.1) then
         ret = acc_select(acces, "EPHEMERIDES", ACC_STRUCT)

         ! lecture du systeme de constantes
         ret = acc_geti(acces, "code_thpla", code_thpla)
         
         ! lecture du code (methode/theorie) de reference
         ret = acc_geti(acces, "cle_theorie", code_ephem)

         ! lecture du nombre de fichier
         ret = acc_geti(acces, "nb_fichiers", nb_fic)

         ! lecture du nom des fichiers
         if (nb_fic .ge. 1) then
            do i=1, nb_fic
               write(buffer,'(I2)') i
               if (i.lt.cpsi_10) then
                  ret = acc_gets(acces, "Fichier_"//buffer(2:2)//".nom_fichier", fic(i))
               else
                  ret = acc_gets(acces, "Fichier_"//buffer(1:2)//".nom_fichier", fic(i))
               end if
            end do
         endif

         ! fin de la selection
         ret = acc_select_end(acces)
      end if

      
      ! lecture du format de sortie
      ret = acc_gets(acces, "Format_sortie", buffer)
      if (trim(buffer).eq."MADONA") then
         format_sortie = 1
      else
         format_sortie = 2
      end if
      
      ! lecture du corps central
      ret = acc_geti(acces, "corcen", corpsc)
      
      ! lecture de la duree de validite
      ret = acc_getd(acces, "Duree_tchebychev", duree, "j")
      
      ! lecture du degre maximal
      ret = acc_geti(acces, "Degre_tchebychev", degre)
      
      ! lecture de la date de debut
      ret = acc_getd(acces, "date_debut.jj", jj, "jj1950")
      ret = acc_getd(acces, "date_debut.sec", sec, "sec")
      date_deb = jj + (sec/86400._pm_reel)
      
      ! lecture de la date de fin
      ret = acc_getd(acces, "date_fin.jj", jj, "jj1950")
      ret = acc_getd(acces, "date_fin.sec", sec, "sec")
      date_fin = jj + (sec/86400._pm_reel)
      
      ! lecture de l'echelle de temps de la date de debut
      ret = acc_gets(acces, "echdate_debut", buffer)
      if (trim(buffer).eq."TUC") then
         ech_date_deb = pm_TUC
      else
         ech_date_deb = pm_TE
      end if
      
      ! lecture de l'echelle de temps de la date de fin
      ret = acc_gets(acces, "echdate_fin", buffer)
      if (trim(buffer).eq."TUC") then
         ech_date_fin = pm_TUC
      else
         ech_date_fin = pm_TE
      end if

      ! lecture de l'echelle de temps de la date du repere
      ret = acc_exist(acces, "echtref")
      if (ret.eq.1) then
         ret = acc_gets(acces, "echtref", buffer)
         if (trim(buffer).eq."TUC") then
            ech_date_rep = pm_TUC
         else
            ech_date_rep = pm_TE
         end if
      else
         ! par defaut : TE
         ech_date_rep = pm_TE
      end if
      
      ! nombre de planetes
      nb_pla = acc_get_dim(acces, "code_corps")
      
      ! lecture des codes des planetes
      ret = acc_select(acces, "code_corps", ACC_TABL)
      do i=1, nb_pla
         ret = acc_set_index(acces, i)
         ret = acc_geti(acces, ACC_INDEX, codes_pla(i))
      end do
      ret = acc_select_end(acces)
      
      ! lecture des noms des planetes
      ret = acc_select(acces, "nom_corps", ACC_TABL)
      do i=1, nb_pla
         ret = acc_set_index(acces, i)
         ret = acc_gets(acces, ACC_INDEX, noms_pla(i))
      end do
      ret = acc_select_end(acces)
      
      ! fermeture du fichier
      ret = acc_close(acces)
      
    end subroutine read_donnees
