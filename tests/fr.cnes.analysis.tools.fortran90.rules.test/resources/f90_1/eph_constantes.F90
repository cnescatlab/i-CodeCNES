module eph_constantes

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  eph_constantes
!
!$Resume
!  Définition des constantes utilisées dans la LIBEPHEM
!
!$Description
!  Ce module contient les constantes utilisées dans la LIBEPHEM :
!.     - numéro des corps,
!.     - facteurs de conversion,
!  ainsi que les sous-programmes de conversion de numéro d'un
!  système de numérotation dans un autre. 
!
!$Auteur
!  Florence Vivares / Philippe Brémard (SchlumbergerSema)
!
!$Version
!  $Id: eph_constantes.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: eph_constantes.F90,v $
!  Revision 1.21  2010/10/21 13:46:20  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.20  2010/02/24 09:08:59  cmartel
!  VERSION::AQ::24/02/2010:Corrections suite a la recette interne
!
!  Revision 1.19  2009/12/18 15:23:25  mercadig
!  VERSION::FA-ID:1340:18/12/2009:Correction du test d erreur apres acc_open (routine eph_fic2code)
!
!  Revision 1.18  2009/02/20 10:26:37  cml
!  AQ : Suppression de use inutiles
!
!  Revision 1.17  2009/02/20 09:17:38  cml
!  DM-ID 960 : Ajout de la valeur de l'unite astronomique de l'UAI1976 necessaire a VSOP87
!
!  Revision 1.16  2009/01/06 10:53:55  cml
!  FA-ID 1151 : Correction orthographe
!
!  Revision 1.15  2008/10/28 12:44:36  tanguyy
!  DM-ID 1058 : utilisation de l'interface cpsi_size_ptr pour evaluer la taille d'un pointeur
!
!  Revision 1.14  2008/10/08 15:46:15  cml
!  FA-ID 1062 : Ajout de compteurs afin d interrompre une boucle potentiellement infinie
!
!  Revision 1.13  2008/10/02 13:31:04  tanguyy
!  AQ : Constantes designant les differentes methodes
!
!  Revision 1.12  2008/08/04 13:23:37  gss
!  DM-ID 1058 : (portage g95) initialisation à NULL des pointeurs lors de leur
!  déclaration. Ajout des gestions d'erreur des fonctions d'accès aux fichiers
!  madona.
!
!  Revision 1.11  2006/10/18 09:52:07  vivaresf
!  DM-ID 425 : Cloture du FT (Passage PSIMU sous Linux)
!
!  Revision 1.10.2.1  2006/09/26 12:12:37  vpg
!  DM-ID 425 : Version initiale du FT (Passage PSIMU sous Linux)
!
!  Revision 1.10  2006/05/31 13:07:26  vivaresf
!  version 2.0 : mise au point COMPAS/COMPAS_UI
!
!  Revision 1.9  2006/05/30 15:22:43  vivaresf
!  Metriques : supression d'un niveau d'imbrication
!
!  Revision 1.8  2006/05/30 12:29:06  vivaresf
!  Separation COMPAS_UI,
!  suppression MSPRO
!  robustesse (iostat sur les deallocate)
!
!  Revision 1.7  2006/05/30 08:23:16  vivaresf
!  DM-ID 387 : variables inutiles
!
!  Revision 1.6  2006/04/26 11:54:16  bouillaj
!  FA-ID 371 : Modification de la fabrication de certains noms de fichiers pour eviter les blancs
!
!  Revision 1.5  2006/04/18 09:53:29  vivaresf
!  FA 500 : traces da la DM 391
!
!  Revision 1.4  2006/01/23 13:43:45  bouillaj
!  Suppression de la metheode EPROC
!
!  Revision 1.3  2005/12/09 08:38:09  vivaresf
!  suppression de codes commentés
!
!  Revision 1.2  2005/12/08 18:39:03  vivaresf
!  Cartouches et vérification des déclarations
!
!  Revision 1.1.1.1  2005/12/07 07:23:08  vivaresf
!  Refonte de COMPAS
!  Revision 1.7  2005/11/07 15:56:14  bouillaj
!  DM-ID 391 : Amelioration qualite sur la LIBEPHEM
!  Revision 1.6  2005/10/13 10:37:55  bouillaj
!  FA-ID 371 : Cloture du FT (Corrections dans libephem pour passage linux)
!  Revision 1.5.2.1  2005/10/13 10:29:20  bouillaj
!  FA-ID 371 : Version initiale du FT (Corrections dans libephem pour passage linux)
!  Revision 1.5  2005/05/09 14:17:31  vivaresf
!  V2-1, documentation : correction des cartouches
!  Revision 1.4  2005/03/09 09:12:03  vivaresf
!  Correction des cartouches
!  Revision 1.3  2004/12/17 14:58:10  vivaresf
!  Documentation
!  Revision 1.2  2004/05/25 13:54:14  vivaresf
!   Version V1_9 : sans la MECASPA
!  Revision 1.1.1.1  2004/04/02 09:07:24  vivaresf
!  Gestion de configuration locale
!  Revision 1.25  2004/01/13 09:16:26  bremard
!  Mise à jour cartouche
!  Revision 1.22  2004/01/07 16:20:24  bremard
!  eph_conv_minusc et eph_util_ficunit dans module eph_util + maj cartouche + gestion d'erreur
!  Revision 1.21  2003/12/30 16:09:36  bremard
!  Ajout et utilisation eph_conv_minusc + MSP_signaler_message remplace AM_err_setmsg pour non dépendance AMLIB
!  Revision 1.20  2003/06/16 14:11:03  vivaresf
!  Cometes supplementaires
!  Revision 1.19  2003/06/06 14:52:12  bremard
!  Particularisation des parametres
!  Revision 1.17  2002/10/16 13:32:17  vivaresf
!  PhB - Ajout du sous-programme eph_util_ficunit
!  Revision 1.16  2002/10/08 08:15:33  vivaresf
!  Rajout des noms standards anciens baryss et barytl
!  Revision 1.11  2001/12/17 17:40:47  vivaresf
!  Conversion amlib
!  Revision 1.9  2001/12/11 17:14:25  bremard
!  PhB - ajout eph_uakm pour theorie IERS92
!  Revision 1.5  2001/12/04 11:13:32  vivaresf
!  Conversion de constantes lues dans un fichier Madona
!  Revision 1.3  2001/11/29 16:15:49  vivaresf
!  eph_date_jj50_j2000
!  Revision 1.2  2001/11/28 13:31:52  bremard
!  PhB - Ajout de constantes physiques
!
!$FinHistorique
!
!$Usage
!  use eph_constantes
!
!$Structure
!
!$Global
!
!>  codedef               : <integer,parameter>  corps indéterminé
!>  eph_bary_soleil       : <integer,parameter>  
!>  eph_bary_mercure      : <integer,parameter>  
!>  eph_bary_venus        : <integer,parameter>  
!>  eph_bary_terre        : <integer,parameter>  
!>  eph_bary_mars         : <integer,parameter>  
!>  eph_bary_jupiter      : <integer,parameter>  
!>  eph_bary_saturne      : <integer,parameter>  
!>  eph_bary_uranus       : <integer,parameter>  
!>  eph_bary_neptune      : <integer,parameter>  
!>  eph_bary_pluton       : <integer,parameter>  
!>  eph_soleil            : <integer,parameter>  
!>  eph_mercure           : <integer,parameter>  
!>  eph_venus             : <integer,parameter>  
!>  eph_terre             : <integer,parameter>  
!>  eph_mars              : <integer,parameter>  
!>  eph_jupiter           : <integer,parameter>  
!>  eph_saturne           : <integer,parameter>  
!>  eph_uranus            : <integer,parameter>  
!>  eph_neptune           : <integer,parameter>  
!>  eph_pluton            : <integer,parameter>  
!>  eph_lune              : <integer,parameter>  
!>  eph_phobos            : <integer,parameter>  
!>  eph_deimos            : <integer,parameter>  
!>  eph_io                : <integer,parameter>  
!>  eph_europa            : <integer,parameter>  
!>  eph_ganymede          : <integer,parameter>  
!>  eph_callisto          : <integer,parameter>  
!>  eph_mimas             : <integer,parameter>  
!>  eph_enceladus         : <integer,parameter>  
!>  eph_thetys            : <integer,parameter>  
!>  eph_dione             : <integer,parameter>  
!>  eph_rhea              : <integer,parameter>  
!>  eph_titan             : <integer,parameter>  
!>  eph_hyperion          : <integer,parameter>  
!>  eph_iapetus           : <integer,parameter>  
!>  eph_wirtanen          : <integer,parameter>  
!>  eph_halebopp          : <integer,parameter>  
!>  eph_tempel1           : <integer,parameter>  
!>  eph_tempel2           : <integer,parameter>  
!>  eph_churyumov         : <integer,parameter>  
!>  eph_howell            : <integer,parameter>  
!>  eph_schwassmann3c     : <integer,parameter>  
!>  eph_schwassmann3b     : <integer,parameter>  
!>  eph_schwassmann3e     : <integer,parameter>  
!>  eph_schwassmann3      : <integer,parameter>  
!>  eph_ceres             : <integer,parameter>  
!>  eph_pallas            : <integer,parameter>  
!>  eph_junon             : <integer,parameter>  
!>  eph_vesta             : <integer,parameter>  
!>  eph_chaldaea          : <integer,parameter>  
!>  eph_orpheus           : <integer,parameter>  
!>  eph_uakm              : <pm_reel,parameter>  Conversion UA en km
!>  eph_uajkms            : <pm_reel,parameter>  Conversion UA/j en km/s
!>  eph_uakm_iers92       : <pm_reel,parameter>  Conversion UA en km pour le modèle IERS92
!>  eph_uajkms_iers92     : <pm_reel,parameter>  Conversion UA/j en km/s  pour le modèle IERS92
!>  eph_date_dj1950       : <pm_reel,parameter>  Conversion jours juliens en jours 1950
!>  eph_date_jj50_j2000   : <pm_reel,parameter>  Conversion jours 1950 en jours 2000
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!.  eph_cnes2bdl eph_cnes2eproc eph_cnes2anal eph_fic2code eph_cnes2old
!
!$Routines
!- eph_cnes2bdl
!- eph_cnes2eproc
!- eph_cnes2anal
!- eph_fic2code
!- eph_cnes2old
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use msp_gestion_erreur
  use cps_acces

      implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: eph_constantes.F90 69 2012-09-11 08:33:34Z ffsm $'


!***********************************************************************
! Constantes
!***********************************************************************

      ! valeur par defaut 
      integer,parameter::codedef  = -2**30

      ! barycentre du système solaire
      integer,parameter::eph_bary_soleil  = 0

      ! barycentres des systèmes palnétaires
      integer,parameter::eph_bary_mercure = 1
      integer,parameter::eph_bary_venus   = 2
      integer,parameter::eph_bary_terre   = 3
      integer,parameter::eph_bary_mars    = 4
      integer,parameter::eph_bary_jupiter = 5
      integer,parameter::eph_bary_saturne = 6
      integer,parameter::eph_bary_uranus  = 7
      integer,parameter::eph_bary_neptune = 8
      integer,parameter::eph_bary_pluton  = 9
      
      ! soleil
      integer,parameter::eph_soleil     =  10
      
      ! planètes   
      integer,parameter::eph_mercure    = 199
      integer,parameter::eph_venus      = 299
      integer,parameter::eph_terre      = 399
      integer,parameter::eph_mars       = 499
      integer,parameter::eph_jupiter    = 599
      integer,parameter::eph_saturne    = 699
      integer,parameter::eph_uranus     = 799
      integer,parameter::eph_neptune    = 899
      integer,parameter::eph_pluton     = 999

      ! satellite de la Terre
      integer,parameter::eph_lune       = 301

      ! satellite de Mars
      integer,parameter::eph_phobos     = 401
      integer,parameter::eph_deimos     = 402
      
      ! satellites de Jupiter
      integer,parameter::eph_io         = 501
      integer,parameter::eph_europa     = 502
      integer,parameter::eph_ganymede   = 503
      integer,parameter::eph_callisto   = 504

      ! satellites de Saturne
      integer,parameter::eph_mimas      = 601
      integer,parameter::eph_enceladus  = 603
      integer,parameter::eph_thetys     = 603
      integer,parameter::eph_dione      = 604
      integer,parameter::eph_rhea       = 605
      integer,parameter::eph_titan      = 606
      integer,parameter::eph_hyperion   = 607
      integer,parameter::eph_iapetus    = 608

      ! comètes
      integer,parameter::eph_wirtanen   = 1000109
      integer,parameter::eph_halebopp   = 1000132
      integer,parameter::eph_tempel1    = 1000093

      integer,parameter::eph_tempel2    = 1000094
      integer,parameter::eph_churyumov  = 1000012
      integer,parameter::eph_howell     = 1000046
      integer,parameter::eph_schwassmann3c = 1000081 
      integer,parameter::eph_schwassmann3b = 1000320
      integer,parameter::eph_schwassmann3e = 1000324
      integer,parameter::eph_schwassmann3  = 1000394

      ! astéroïdes
      integer,parameter::eph_ceres      = 2000001
      integer,parameter::eph_pallas     = 2000002
      integer,parameter::eph_junon      = 2000003
      integer,parameter::eph_vesta      = 2000004
      integer,parameter::eph_chaldaea   = 2000313
      integer,parameter::eph_orpheus    = 2003316
      
! Constantes physiques
      
      ! Conversion d'unités

      ! ua -> km     ; 66 a rajouter
      real(kind=pm_reel), parameter :: eph_uakm   = 1.4959787066e+08_PM_REEL
!      real(kind=pm_reel), parameter :: eph_uakm   = 1.4959787000e+08_PM_REEL
                                                    
      ! ua/j -> km/s
      real(kind=pm_reel), parameter :: eph_uajkms = eph_uakm / 86400._PM_REEL

      ! ua -> km       (théorie IERS92)
      real(kind=pm_reel), parameter :: eph_uakm_iers92   = 1.49597870691e+08_PM_REEL
      ! ua/j -> km/s   (théorie IERS92)
      real(kind=pm_reel), parameter :: eph_uajkms_iers92 = eph_uakm_iers92 / 86400._PM_REEL

      ! ua -> km       (théorie UAI1976)
      real(kind=pm_reel), parameter :: eph_uakm_uai1976   = 1.49597870000e+08_PM_REEL
      ! ua/j -> km/s   (théorie UAI1976)
      real(kind=pm_reel), parameter :: eph_uajkms_uai1976 = eph_uakm_uai1976 / 86400._PM_REEL

      ! date 1/1/1950 en jours juliens vrais 
      real(kind=pm_reel), parameter :: eph_date_dj1950 = 2433282.5_PM_REEL   

      ! date 1/1/2000 en B1950
      real(kind=pm_reel), parameter :: eph_date_jj50_j2000 = 18262.5_PM_REEL
      

      ! Méthodes d'éphémérides employées : utilisées pour 
      ! effectuer un contrôle sur la cohérence des initialisations
      ! et des appels
      integer, parameter :: eph_methode_analytique     = 1
      integer, parameter :: eph_methode_naif           = 2
      integer, parameter :: eph_methode_bdl            = 3
      integer, parameter :: eph_methode_tchebychev     = 4
      integer, parameter :: eph_methode_tchebychev_mad = 5

contains

   subroutine eph_cnes2bdl(n1,n2,isens,ier)

!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  eph_cnes2bdl
!
!$Resume
!       Changement du codage des planètes et vérification (méthode BDL)
!
!$Description
!       Ce sous-programme calcule la correspondance entre le numéro 
!       d'une planète dans le codage CNES (NAIF), et son numéro dans le 
!       codage BDL.
!
!$Auteur
!   Florence Vivares / Philippe Brémard (SchlumbergerSema) 
!
!$Mots-cles
!        codage planète
!
!$Usage
!  call eph_cnes2bdl(n1,n2,isens,ier)
!.    integer :: n1,n2,ier,isens
!
!$Arguments
!>E/S   n1     :<integer>   numéro CNES
!>E/S   n2     :<integer>   numéro BDL
!>E/S   isens  :<integer>   1: CNES -> BDL, -1 : BDL -> CNES
!>E/S   ier    :<integer>   code retour
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


! PARAMETRES
      integer :: n1,n2,ier,isens

! VARIABLES LOCALES
      character(len=2) :: csens

! INITIALISATION
      ier = 0
      n2 = codedef
      write(csens,'(I2)') isens

! correspondances

      if (isens.eq.1) then
         
         select case (n1)
        case(eph_mercure)
            n2 = 1
         case(eph_venus)
            n2 = 2
         case(eph_terre)
            n2 = 12
         case(eph_soleil)
            n2 = 11
         case(eph_lune)
            n2 = 10
         case(eph_mars)
            n2 = 4
         case(eph_jupiter)
            n2 = 5
         case(eph_bary_jupiter)
            n2 = 5
         case(eph_saturne)
            n2 = 6
         case(eph_bary_saturne)
            n2 = 6
         case(eph_uranus)
            n2 = 7
         case(eph_bary_uranus)
            n2 = 7
         case(eph_neptune)
            n2 = 8
         case(eph_bary_neptune)
            n2 = 8
         case(eph_pluton)
            n2 = 9
         case(eph_bary_pluton)
            n2 = 9
         case (eph_bary_terre)
            n2 = 3
         case (eph_bary_soleil)
            n2 = 0
         case (eph_ceres)
            n2 = 13
         case (eph_pallas)
            n2 = 14
         case (eph_junon)
            n2 = 15
         case (eph_vesta)
            n2 = 16
         end select

      elseif (isens.eq.-1) then

         select case (n1)
         case(1)
            n2 = eph_mercure
         case(2)
            n2 = eph_venus
         case(12)
            n2 = eph_terre
         case(11)
            n2 = eph_soleil
         case(10) 
            n2 = eph_lune
         case(4)
            n2 = eph_mars
         case(5)
            n2 = eph_jupiter
         case(6)
            n2 = eph_saturne
         case(7)
            n2 = eph_uranus
         case(8)
            n2 = eph_neptune
         case(9)
            n2 = eph_pluton
         case(3)
            n2 = eph_bary_terre
         case(0)
            n2 = eph_bary_soleil
         case(13)
            n2 = eph_ceres
         case(14)
            n2 =  eph_pallas
         case(15)
            n2 = eph_junon
         case(16)
            n2 = eph_vesta
         end select

      else
         ! erreur mecaspa isens : code impossible
         call MSP_signaler_message(cle_mes="EPH_ERR_CODESENS", &
                     routine="eph_cnes2bdl", &
                     partie_variable=csens)
         return
      endif

      if (n2 .eq.codedef) then
         ! erreur mecaspa n1 : code inconnu pour cette méthode
         call MSP_signaler_message(cle_mes="EPH_ERR_CODEPLA", &
                     routine="eph_cnes2bdl", &
                     partie_variable="non trouvé")     
         n2=n1 
         return
      endif

      end subroutine eph_cnes2bdl

   subroutine eph_cnes2anal(n1,n2,isens,ier)

!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  eph_cnes2anal
!
!$Resume
!       Changement du code des planètes et vérification (méthode VSOP82 analytique)
!
!$Description
!       Ce sous-programme calcule la correspondance entre le numéro 
!       d'une planète dans le codage CNES (NAIF), et son numéro dans le 
!       codage LIBENVINT (VSOP82 analytique).
!
!$Auteur
!   Florence Vivares / Philippe Brémard (SchlumbergerSema) 
!
!$Mots-cles
!        codage planète
!
!$Usage
!  call eph_cnes2anal(n1,n2,isens,ier)
!.    integer :: n1,n2,ier,isens
!
!$Arguments
!>E/S   n1     :<integer>   numéro CNES
!>E/S   n2     :<integer>   numéro LIBENVINT
!>E/S   isens  :<integer>   1 (CNES-> LIBENVINT) ou -1 (LIBENVINT->CNES) 
!>E/S   ier    :<integer>   code retour
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


! PARAMETRES
      integer :: n1,n2,ier,isens

! VARIABLES LOCALES
      character(len=2) :: csens

! INITIALISATION
      ier = 0
      n2 = codedef
      write(csens,'(I2)') isens

! correspondances

      if (isens.eq.1) then
         
         select case (n1)
         case(eph_terre)
            n2 = 3
         case(eph_soleil)
            n2 = 10
         case(eph_lune)
            n2 = 13
         case(eph_mars)
            n2 = 4
         case(eph_jupiter)
            n2 = 5
         case(eph_saturne)
            n2 = 6
         case(eph_uranus)
            n2 = 7
         case(eph_neptune)
            n2 = 8
!         case(eph_pluton)
!            n2 = 9
!         case (eph_deimos)
!            n2 = 15
!         case (eph_phobos)
!           n2 = 14
         case (eph_mercure)
            n2 = 1
         case (eph_venus)
            n2 = 2
         end select

      elseif (isens.eq.-1) then

         select case (n1)
         case(3)
            n2 = eph_terre
         case(10)
            n2 = eph_soleil
         case(13) 
            n2 = eph_lune
         case(4)
            n2 = eph_mars
         case(5)
            n2 = eph_jupiter
         case(6)
            n2 = eph_saturne
         case(7)
            n2 = eph_uranus
         case(8)
            n2 = eph_neptune
!         case(9)
!            n2 = eph_pluton
!         case(15)
!            n2 = eph_deimos
!         case(14)
!            n2 = eph_phobos
!         case(1)
!            n2 = eph_mercure
         case(2)
            n2 =  eph_venus
         end select

      else
         ! erreur mecaspa isens : code impossible
         call MSP_signaler_message(cle_mes="EPH_ERR_CODESENS", &
                     routine="eph_cnes2anal", &
                     partie_variable=csens)
         return
      endif

      if (n2 .eq.codedef) then
         ! erreur mecaspa n1 : code inconnu pour cette méthode
         call MSP_signaler_message(cle_mes="EPH_ERR_CODEPLA", &
                     routine="eph_cnes2anal", &
                     partie_variable="non trouvé")     
         n2=n1 
         return
      endif

      end subroutine eph_cnes2anal

   subroutine eph_fic2code(nomfic, nomcorps, libelle, code)

!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  eph_fic2code
!
!$Resume
!   Récupération du code d'un corps à partir de son nom 
!
!$Description
!   Récupération du code d'un corps à partir du nom du corps lu dans
!   un fichier MADONA.    
!
!$Auteur
!   Florence Vivares / Philippe Brémard (SchlumbergerSema)
!
!$Mots-cles
!   codage planète
!
!$Usage
!  call eph_fic2code(nomfic, nomcorps, [libelle], [code])
!.    character(len=*) :: nomfic
!.    character(len=*) :: libelle
!.    character(len=*) :: nomcorps
!.    integer :: code
!
!$Arguments
!>E     nomfic    :<LEN=*>     nom du fichier Madona
!>E/S   nomcorps  :<LEN=*>     nom de la planète ou du corps
!>[E]   libelle   :<LEN=*>     libellé
!>[E/S] code      :<integer>   code planète
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

     use eph_info
     use eph_varglob, only : EPHV_NB_MAX_STRUCT_FICHIER

     implicit none
     ! PARAMETRES
      character(len=*), intent(in) :: nomfic
      character(len=*), intent(in), optional :: libelle
      character(len=*), intent(inout) :: nomcorps
      integer, intent(inout), optional :: code

     ! VARIABLES LOCALES
      integer :: accescour, ier
      character(len=80) :: nom1, nom2, nom3
      logical :: trouve
      character(LEN=CPS_MAXLG), dimension(:), pointer :: fichiersCorps => NULL()
      integer :: nature, k, iostat
      character(LEN=CPS_MAXLG) :: lib
      character (len=CPS_MAXLG), dimension(3) :: msp_mess
      logical :: fin_fichier   ! Flag indiquant la fin du fichier
      integer :: nb_champs_lus ! Nombre de champs lus dans le fichier

      trouve = .false.
      
      if (.not.cps_var_init) call cps_init()

     ! nom du corps dans le fichier Madona
      if(present(libelle)) then 
         accescour=acc_open()
         if (accescour<0) then
            write(msp_mess(1),*) accescour
            write(msp_mess(2),*) "acc_open"
            call MSP_signaler_message(cle_mes="CPS_ERR_ACC", &
                 routine="eph_fic2code", &
                 partie_variable=msp_mess(1:2))
         endif
         if(acc_connect(accescour, nomfic, ACC_R).ne.0) goto 4000
         if(acc_read(accescour, ACC_ALL).ne.0) goto 4000
         ier=acc_gets(accescour, libelle, nomcorps)
         if (ier<0) then
            write(msp_mess(1),*) ier
            write(msp_mess(2),*) trim(libelle)
            write(msp_mess(3),*) trim(nomfic)
            call MSP_signaler_message(cle_mes="CPS_ERR_ACC_GET", &
                 routine="eph_fic2code", &
                 partie_variable=msp_mess)
         endif
         ier=acc_close(accescour)
         if (ier<0) then
            call MSP_signaler_message(cle_mes="CPS_ERR_CLOSE", &
                 routine="eph_fic2code", &
                 partie_variable=trim(nomfic))
         endif
      endif
   
      if (.not.present(code)) return

      nom3=nomcorps
      call eph_conv_minusc(nom3)

      if (len_trim(nom3)==0) then
         if(present(libelle)) nom3=libelle
         call MSP_signaler_message(cle_mes="EPH_ERR_CODEPLA", &
              routine="eph_fic2code", &
              partie_variable="pour "//trim(nom3)//" non trouvé")
      endif

      ! lecture du fichier ephem.conf

      call  cps_getListFichiersCateg(CPS_CATEG_CORPS, fichiersCorps)
      do k=1, cpsi_size_ptr(fichiersCorps)
         call cpsi_getAccesMadona(fichiersCorps(k), accescour)
         ier = acc_scan_reset(accescour)
         if (ier<0) then
            write(msp_mess(1),*) ier
            write(msp_mess(2),*) "acc_scan_reset"
            call MSP_signaler_message(cle_mes="CPS_ERR_ACC", &
                 routine="eph_fic2code", &
                 partie_variable=msp_mess(1:2))
         endif

         ! Parcours jusqu'à atteindre la condition d'arret
         ! ou jusqu'à la fin du fichier
         ! ou la limite algorithmique fixé à MAXCORPS
         nb_champs_lus = 0
         fin_fichier = .false.
         do while (.not. trouve &
              .and. .not. fin_fichier &
              .and. nb_champs_lus <= EPHV_NB_MAX_STRUCT_FICHIER)
 
            ier = acc_scan(accescour, lib, nature)
            if (ier<0) then
               write(msp_mess(1),*) ier
               write(msp_mess(2),*) "acc_scan"
               call MSP_signaler_message(cle_mes="CPS_ERR_ACC", &
                    routine="eph_fic2code", &
                    partie_variable=msp_mess(1:2))
            endif

            if (nature.eq.CPS_FIN_SEQ) then
               fin_fichier = .true.
            elseif (nature.eq.ACC_STRUCT) then
               ier = acc_select(accescour, lib, ACC_STRUCT)
               if (ier<0) then
                  write(msp_mess(1),*) ier
                  write(msp_mess(2),*) "acc_select"
                  call MSP_signaler_message(cle_mes="CPS_ERR_ACC", &
                       routine="eph_fic2code", &
                       partie_variable=msp_mess(1:2))
               endif
               ier = acc_gets(accescour, "nom_fr", nom1)
               if (ier<0) then
                  write(msp_mess(1),*) ier
                  write(msp_mess(2),*) "nom_lec"
                  write(msp_mess(3),*) trim(fichiersCorps(k))
                  call MSP_signaler_message(cle_mes="CPS_ERR_ACC_GET", &
                       routine="eph_fic2code", &
                       partie_variable=msp_mess)
               endif
               call eph_conv_minusc(nom1)
               ier = acc_gets(accescour, "nom_en", nom2)
               if (ier<0) then
                  write(msp_mess(1),*) ier
                  write(msp_mess(2),*) "nom_en"
                  write(msp_mess(3),*) trim(fichiersCorps(k))
                  call MSP_signaler_message(cle_mes="CPS_ERR_ACC_GET", &
                       routine="eph_fic2code", &
                       partie_variable=msp_mess)
               endif
               call eph_conv_minusc(nom1)
               if (trim(nom3).eq.trim(nom1).or.trim(nom3).eq.trim(nom2)) then
                  ier = acc_geti(accescour, "code", code)
                  if (ier<0) then
                     write(msp_mess(1),*) ier
                     write(msp_mess(2),*) "code"
                     write(msp_mess(3),*) trim(fichiersCorps(k))
                     call MSP_signaler_message(cle_mes="CPS_ERR_ACC_GET", &
                          routine="eph_fic2code", &
                          partie_variable=msp_mess)
                  endif
                  trouve = .true.
               endif
               ier = acc_select_end(accescour)

               ! Mise a jour du compteur de champs
               nb_champs_lus = nb_champs_lus +1
            end if ! Fin si c'est une structure

         end do

         ! Traitement du cas où l'on est sorti de la boucle trop tôt
         if (nb_champs_lus > EPHV_NB_MAX_STRUCT_FICHIER) then
            call MSP_signaler_message(cle_mes="EPH_ERR_LECT_CHAMPS_FICHIER", &
               partie_variable=trim(fichiersCorps(k)), &
               routine="eph_fic2code")
            return
         endif    

         ! Si la recherche est ok dans le fichier courant 
         ! on ne passe pas au fichier suivant
         if (trouve) exit
         
      end do

      if (associated(fichiersCorps)) then
         deallocate(fichiersCorps, stat=iostat)
      end if

      ! quelques codes CNES spécifiques pour compatibilité ascendante
      if (.not.trouve) then
         if(trim(nom3).eq."baryss") then 
            code = 0
            trouve=.true.
         endif
         if(trim(nom3).eq."barytl") then 
            code = 3
            trouve=.true.
         endif
      endif


      ! si non trouvé decodage NAIF
      
      if(.not.trouve) then
         ! sous-programme SPICELIB de conversion de chaine "nomcorps" en code 
         call zzbodn2c(nomcorps, code, trouve)
         if(.not.trouve) then
            call MSP_signaler_message(cle_mes="EPH_ERR_CODEPLA", &
                 routine="eph_fic2code", &
                 partie_variable="pour "//trim(nomcorps)//" non trouvé")
            return
         endif
      endif
      
      
4000  continue
      
      return
      
    end subroutine eph_fic2code

    subroutine eph_cnes2old(n1,n2,isens,ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_cnes2old
!
!$Resume
!       Changement du codage des planètes et vérification (codage AMLIB)
!
!$Description
!       Ce sous-programme calcule la correspondance entre le numéro 
!       d'une planète dans le codage CNES (NAIF), et son numéro dans le 
!       codage de l'AMLIB
!
!$Auteur
!   Florence Vivares / Philippe Brémard (SchlumbergerSema) 
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_cnes2old(n1,n2,isens,ier)
!.    integer :: n1,isens
!.    integer :: n2,ier
!
!$Arguments
!>E     n1     :<integer>   numéro CNES si isens=1 - numéro AMLIB si isens=-1
!>S     n2     :<integer>   numéro AMLIB si isens=1 - numéro CNES si isens=-1
!>E     isens  :<integer>   sens de traduction
!>S     ier    :<integer>   code retour
!
!$Common
!
!$Routines
!- MSP_signaler_message
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!        codage planète
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     implicit none

      !	Parametres
      integer, intent(in) :: n1,isens
      integer, intent(out) :: n2,ier
      
      ! VARIABLES LOCALES
      character(len=2) :: csens

      !	Initialisation
      ier = 0
      n2 = codedef
      write(csens,'(I2)') isens

      ! isens = 1 --> CNES -> amlib

      if (isens.eq.1) then
         select case (n1)
         case (eph_mercure) 
            n2 = 1
!         case (eph_bary_mercure) 
!            n2 = 1
         case (eph_venus)
            n2 = 2
!         case (eph_bary_venus)
!            n2 = 2
         case (eph_terre) 
            n2 = 3
         case (eph_mars) 
            n2 = 4
!         case (eph_bary_mars) 
!            n2 = 4
         case (eph_jupiter) 
            n2 = 5
         case (eph_bary_jupiter) 
            n2 = 5
         case (eph_saturne) 
            n2 = 6
         case (eph_bary_saturne) 
            n2 = 6
         case (eph_uranus) 
            n2 = 7
         case (eph_bary_uranus) 
            n2 = 7
         case (eph_neptune) 
            n2 = 8
         case (eph_bary_neptune) 
            n2 = 8
         case (eph_pluton) 
            n2 = 9
         case (eph_bary_pluton) 
            n2 = 9
         case (eph_soleil) 
            n2 = 10
         case (eph_bary_terre) 
            n2 = 11
         case (eph_bary_soleil) 
            n2 = 12
         case (eph_lune) 
            n2 = 13
         case (eph_phobos) 
            n2 = 14
         case (eph_deimos) 
            n2 = 15
         case(eph_ceres)
            n2 = 1001
         case(eph_pallas)
            n2 = 1002
         case(eph_junon)
            n2 = 1003
         case(eph_vesta)
            n2 = 1004
         case(eph_chaldaea)
            n2 = 1005
         case(eph_wirtanen)
            n2 = 10001
         case(eph_halebopp)
            n2 = 10002
         case(eph_tempel1)
            n2 = 10003
         end select

      ! isens = -1 --> amlib -> CNES (NAIF)

      elseif (isens.eq.-1) then
         select case (n1)
         case(1) 
            n2 = eph_mercure
         case(2) 
            n2 = eph_venus
         case (3) 
            n2 = eph_terre
         case (4) 
            n2 = eph_mars
         case (5) 
            n2 = eph_bary_jupiter
         case (6) 
            n2 = eph_bary_saturne
         case (7) 
            n2 = eph_bary_uranus
         case (8) 
            n2 = eph_bary_neptune
         case (9) 
            n2 = eph_bary_pluton
         case (10) 
            n2 = eph_soleil
         case (11) 
            n2 = eph_bary_terre
         case (12) 
            n2 = eph_bary_soleil
         case (13) 
            n2 = eph_lune
         case (14) 
            n2 = eph_phobos
         case (15) 
            n2 = eph_deimos
         case(1001)
            n2 = eph_ceres
         case(1002)
            n2 = eph_pallas
         case(1003)
            n2 = eph_junon
         case(1004)
            n2 = eph_vesta
         case(1005)
            n2 = eph_chaldaea
         case(10001)
            n2 = eph_wirtanen
         case(10002)
            n2 = eph_halebopp
         case(10003)
            n2 = eph_tempel1
         end select

      else
         ! erreur mecaspa isens : code impossible
         ier = -1
         call MSP_signaler_message(cle_mes="EPH_ERR_CODESENS", &
                     routine="eph_cnes2old", &
                     partie_variable=csens)
         return
      endif

      if (n2 .eq.codedef) then
         ! erreur mecaspa n1 : code inconnu pour cette méthode
         ier = -2
         call MSP_signaler_message(cle_mes="EPH_ERR_CODEPLA", &
                     routine="eph_cnes2old", &
                     partie_variable="non trouvé")     
         n2=n1 
         return
      endif

    end subroutine eph_cnes2old

end module eph_constantes
