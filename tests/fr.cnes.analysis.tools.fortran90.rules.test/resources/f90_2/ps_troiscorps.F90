module ps_troiscorps

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ps_troiscorps
!
!$Resume
!  Module gérant le calcul des forces dues au "troisième corps".
!
!$Description
!  Module gérant le calcul des forces dues au "troisième corps".
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: ps_troiscorps.F90 368 2013-02-19 14:43:59Z aadt $
!
!$Historique
!  $Log: ps_troiscorps.F90,v $
!  Revision 368  2013/02/19 aadt
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 1.31  2010/10/25 13:10:59  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.30  2009/12/03 13:21:35  mercadig
!  AQ: Suppression de variables inutilisees
!
!  Revision 1.29  2009/09/04 15:12:12  mercadig
!  DM-ID 1218: Remplacement de la MECASPA dans ps_propage par des appels a COMPAS (modes Tchebytchev et analytique), suppression d'un changement de repere devenu obsolete
!
!  Revision 1.28  2008/12/02 16:55:08  tanguyy
!  AQ : mise à jour des cartouches avant livraison de PSIMU V9.3
!
!  Revision 1.27  2008/12/02 10:45:56  huec
!  DM-ID 1058 : Suppression de variable inutilisee
!  Revision 1.26  2008/12/02 08:17:42  tanguyy
!  DM-ID 733 : correction mineures du code suite aux remarques de la relecture de code, mise en forme des cartouches
!  Revision 1.25  2008/11/26 08:52:03  tanguyy
!  DM-ID 733 : reorganisation du calcul des forces de 3è corps
!  Revision 1.24  2008/10/24 09:41:39  huec
!  AQ : Simplification de la gestion des modules
!  Revision 1.23  2008/10/17 12:18:39  mercadig
!  FA-ID 1030: Calcul analytique des ephemerides pour Mars et Venus
!  Revision 1.22  2008/09/04 07:53:19  tanguyy
!  DM-ID 1058 : phase 1 du portage / suppression des warnings - initialisations
!  Revision 1.21  2008/04/16 15:53:19  huec
!  DM-ID 859 : Utilisation de boucles explicites
!  Revision 1.20  2008/04/03 14:31:01  ttn
!  FA-ID 658 : suppression des variables inutilisees
!  Revision 1.19  2008/03/07 09:57:10  huec
!  DM-ID 859 : Utilisation de matmul3, mulvecttrans3, tests des code_erreur
!  Revision 1.18  2008/02/15 16:36:24  huec
!  DM-ID 11 : Suppression de l utilisation d un fichier de saut du TUC hors base COMPAS
!  Revision 1.17  2007/12/03 10:23:05  tanguyy
!  FA-ID 840 : correction de 'terre' en eph_terre
!  Revision 1.16  2007/09/24 15:06:22  tanguyy
!  FA-ID 787 ; suppression des variables inutilisees
!  Revision 1.15  2007/06/20 12:33:34  vivaresf
!  Intégration FA 725 patch V8.7a3
!  Revision 1.14  2007/04/16 14:11:32  couturis
!  FA-ID 725 : mu Soleil et mu Lune initialises et retournes dans le cas analytique
!  Revision 1.13.2.5  2007/06/15 17:17:23  vivaresf
!  PSIMU V8.7a4 : essais pour fuites mémoire
!  Revision 1.13.2.4  2007/04/17 09:06:39  vivaresf
!  FA-ID 725 : validation des optimisations
!  Revision 1.13.2.3  2007/04/17 08:25:18  vivaresf
!  FA-ID 725 : optimisation des changements de repères des éphémérides
!  Revision 1.13.2.2  2007/04/17 08:23:00  vivaresf
!  FA-ID 725 : optimisation des changements de repères des éphémérides
!  Revision 1.13.2.1  2007/04/16 09:50:28  couturis
!  FA-ID 725 : mu Soleil et Lune initialises et retourns dans le cas analytique
!  Revision 1.13  2006/10/19 15:08:32  tanguyy
!  DM-ID 478 : Cloture du FT (Integrateur de Cowell : modification de l interface)
!  Revision 1.12.2.1  2006/10/13 07:55:19  tanguyy
!  DM-ID 478 : utilisation jj/sec. 1ere version fonctionnelle
!  Revision 1.12  2006/04/10 12:39:59  tanguyy
!  Code redondant supprime
!  Revision 1.11  2006/03/15 13:25:22  tanguyy
!  Livraison PSIMU V8-4 ; relecture code / suppression code mort / maj cartouches
!  Revision 1.10  2005/12/14 11:33:32  tanguyy
!  DM-ID 397
!  Revision 1.9  2005/12/13 13:24:46  vivaresf
!  Integration DM 6
!  Revision 1.8.2.1  2005/12/08 08:18:21  tanguyy
!  Correction d'une anomalie : conversion systematique de la date
!  Revision 1.8  2005/11/16 11:20:44  vivaresf
!  DM-ID 5 : option repVEIS pour les épéhémrides de Tchebytchev
!  Revision 1.7  2005/11/15 08:15:50  vivaresf
!  DM-ID 5 : option repveis pour avoir des ephemerides dans le VEIS de la date
!  Revision 1.6  2005/11/10 18:37:10  vivaresf
!  Mise à jour des cartouches
!  Revision 1.5  2005/11/10 17:10:39  vivaresf
!  DM-ID 6 : ephemerides analytiques
!  Revision 1.4  2005/11/09 13:20:56  vivaresf
!  DM-ID 6 : routine commune d'éphémérides
!  Revision 1.3  2005/01/28 10:39:42  fabrec
!  maj cartouches
!  Revision 1.2  2002/11/26 17:05:57  boschett
!  Ajout de implicit none
!  Revision 1.1.1.1  2002/09/30 14:59:35  laurent
!  Industrialisation PSIMU
!  Revision 1.4  2000/04/17 10:58:18  util_am
!  Version multi_satellite en Fortran90
!  Revision 1.3  1999/10/26 10:56:24  util_am
!  Prise en compte du 3ème corps
!  Revision 1.2  1999/08/04 11:28:19  util_am
!  Prise en compte de la gestion des erreurs de MECASPA
!
!$FinHistorique
!
!$Usage
!  use ps_troiscorps
!
!$Structure
!
!$Global
!
!$Common
!
!$Routines
!- ps_propage
!- ps_rep_dateinit_datecour
!- psi_rep_veis_ri
!- psi_rep_eme2000_veis
!- psi_rep_eme2000_ri
!- ps_acc3corps
!#V
!- ps_propage_analytique
!- ps_propage_tchebytchev
!#
!
!$Fonctions
!
!$Include
!
!$Module
!#V
!- MECASPA
!- ps_generalites
!- ps_integration_don
!- ps_bulletin
!- ps_calcul_forces
!- ephem
!#
!
!$Interface
!#V
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use MECASPA
   use ps_generalites
   use ps_integration_don
   use ps_bulletin
   use ps_calcul_forces

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ps_troiscorps.F90 368 2013-02-19 14:43:59Z aadt $'

   

   private :: ps_propage_analytique, ps_propage_tchebytchev

contains

  subroutine ps_propage(typephem, date,corps,pos,mu, &
       repveis, repinteg)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_propage
!
!$Resume
!  Calcul d'éphémérides pour PSIMU (analytique ou Thcebytchev)
!
!$Description
!  Calcul d'éphémérides pour PSIMU (analytique ou Thcebytchev)
!
!$Auteur
!  Florence VIVARES (ATOS Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_propage(typephem, date,corps,pos,mu, &
!.           [repveis], [repinteg])
!.    integer :: typephem
!.    type(tm_jour_sec) :: date
!.    integer :: corps
!.    real (KIND=pm_reel), dimension(3) :: pos
!.    real (KIND=pm_reel) :: mu
!.    integer :: repveis, repinteg
!
!$Arguments
!>E     typephem  :<integer>           Type d'éphémérides 
!.                                         1 : Tchebytchev (avec eph_poscor)
!.                                         2 : Analytique  (avec eph_poscor)
!>E     date      :<tm_jour_sec>       Date jour,sec (jj 1950 TE)
!>E     corps     :<integer>           code NAIF du corps d interet
!>S     pos       :<pm_reel,DIM=(3)>   positions % au corps central (Terre par défaut) ~m
!>S     mu        :<pm_reel>           mu du corps d interet
!>[E]   repveis   :<integer>           Si présent et si typephem == 2
!>[E]   repinteg  :<integer>           Si présent et si typephem == 2
!
!$Common
!
!$Routines
!#V
!- ps_propage_analytique
!- ps_propage_tchebytchev
!#
!
!$Include
!
!$Module
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

    integer, intent(IN) :: typephem
    type(tm_jour_sec), intent(IN)  :: date
    integer, intent(IN) :: corps

    real (KIND=pm_reel), dimension(3), intent(out)  :: pos
    real (KIND=pm_reel), intent(out)  :: mu

    integer, intent(in), optional :: repveis, repinteg

    ! variable locale
    integer :: ii

    ! Initialisations mu et pos du corps
    mu = 0._PM_REEL
    do ii = 1,3	
       pos(ii) = 0._PM_REEL 
    enddo 

    if (typephem == 2) then
       ! Méthode analytique
       call ps_propage_analytique(date,corps,pos,mu, &
               repveis, repinteg)
    else
       ! Méthode Tchebytchev
       call ps_propage_tchebytchev(date,corps,pos,mu, &
               repveis, repinteg)
    endif

  end subroutine ps_propage

  subroutine ps_rep_dateinit_datecour(pos_init,datecour,pos_datecour,inverse)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_rep_dateinit_datecour
!
!$Resume
!
!$Description
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_rep_dateinit_datecour(pos_init,datecour,pos_datecour,[inverse])
!.    real(kind=pm_reel), dimension(3) :: pos_init
!.    type(tm_jour_sec) :: datecour
!.    real(kind=pm_reel), dimension(3) :: pos_datecour
!.    integer :: inverse
!
!$Arguments
!>E     pos_init      :<pm_reel,DIM=(3)>   
!>E     datecour      :<tm_jour_sec>       
!>S     pos_datecour  :<pm_reel,DIM=(3)>   
!>[E]   inverse       :<integer>           
!
!$Common
!
!$Routines
!- mu_angle2
!- MSP_signaler_message
!- mr_tsid_veis
!
!$Include
!
!$Module
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
    
    ! Arguments
    ! =========
    real(kind=pm_reel), dimension(3), intent(in) :: pos_init
    type(tm_jour_sec), intent(in) :: datecour
    real(kind=pm_reel), dimension(3), intent(out) :: pos_datecour
    integer, optional, intent(in) :: inverse


    ! Variables locales
    ! =================
    
    real(kind=pm_reel) :: rayvec, sinphi, phisat, xlosat, rlon
    real(kind=pm_reel) :: teta_RTS_g50, tsid
    
    type(tm_jour_sec) :: date_tuc
    type(tm_code_retour) :: code_erreur
    
    ! Début du code
    ! =============    
  
    ! INITIALISATIONS
    ! ==============
    date_tuc =  datecour + str_bul(iveh)%ecart_te_tuc
    
    ! Calcul du rayon vecteur
    rayvec = sqrt (pos_init(1)*pos_init(1)+pos_init(2)*pos_init(2)+pos_init(3)*pos_init(3))
    
    ! Latitude:
    sinphi = pos_init(3)/rayvec
    phisat = asin(sinphi)

    ! Longitude:
    call mu_angle2 (pos_init(1),pos_init(2),xlosat,code_erreur)
    if (code_erreur%valeur < 0) then
       call MSP_signaler_message (ier_mslib=code_erreur)
       if (MSP_gen_messages("ps_rep_dateinit_datecour" )) return
    end if

    ! Calcul de la longitude dans un repère planétographique
    if ( str_gen(iveh)%planet == eph_terre ) then

       call mr_tsid_veis (date_tuc, 0._pm_reel,teta_RTS_g50,code_erreur)
       call MSP_signaler_message (ier_mslib=code_erreur)
       if (MSP_gen_messages("ps_rep_dateinit_datecour")) return
         
       tsid = teta_RTS_g50 - str_int(iveh)%teta_RI_g50
    else
                
       tsid = (datecour-str_bul(iveh)%datbul0_js)* str_mod(iveh)%vitrotpla
    endif

    ! Longitude dans le repère planéto
    
    if (PRESENT(inverse)) then
       rlon = xlosat + tsid
    else
       rlon = xlosat - tsid
    endif

    ! Coordonnées dans le repère planéto
    pos_datecour(1) = rayvec*cos(rlon)*cos(phisat)
    pos_datecour(2) = rayvec*sin(rlon)*cos(phisat)
    pos_datecour(3) = rayvec*sin(phisat)


  end subroutine ps_rep_dateinit_datecour

  
  subroutine ps_propage_analytique(date,corps,pos,mu, &
                                   repveis, repinteg)
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_propage_analytique
!
!$Resume
!  Calcul d'éphémérides pour PSIMU (méthode analytique)
!
!$Description
!  Calcul d'éphémérides pour PSIMU (méthode analytique)
!
!$Auteur
!  Gérald Mercadier (ATOS Origin)
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_propage_analytique(date,corps,pos,mu, &
!.                                       [repveis], [repinteg])
!.    type(tm_jour_sec) :: date
!.    integer :: corps
!.    real (KIND=pm_reel), dimension(3) :: pos
!.    real (KIND=pm_reel) :: mu
!.    integer :: repveis, repinteg
!
!$Common
!
!$Routines
!- md_joursec_jourfrac
!- MSP_signaler_message
!- ms_pos_soleil_lune
!- psi_rep_veis_ri
!- eph_poscor
!- psi_rep_eme2000_veis
!- psi_rep_eme2000_ri
!
!$Include
!
!$Module
!#V
!- ephem
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    use ephem, only: eph_poscor

    implicit none 
  
    type(tm_jour_sec), intent(IN)  :: date
    integer, intent(IN) :: corps
    
    real (KIND=pm_reel), dimension(3), intent(out)  :: pos
    real (KIND=pm_reel), intent(out)  :: mu

    integer, intent(in), optional :: repveis, repinteg

    ! variables locales
    type(tm_code_retour) :: code_retour
    real(kind=pm_reel) :: date_frac
    real (KIND=pm_reel), dimension(3)  :: pos_corps
    real (KIND=pm_reel), dimension(6)  :: coord
    character(len=20) :: strcorps
    integer :: ii
    
    
    ! Conversion date (jour,sec) en date frac
    call md_joursec_jourfrac(date, date_frac, code_retour)
    if(code_retour%valeur < 0) then
       call MSP_signaler_message(ier_mslib=code_retour)
       return
    end if    
   
    if (str_gen(iveh)%planet == eph_terre) then
       ! Corps central : Terre

       ! Calcul analytique de la position du corps (coord)
       ! Repère = EME2000, unité de distance = km        
       call eph_poscor(PSI_METHODE_EPH_ANALYTIQUE_FIC, date_frac, &
            str_gen(iveh)%planet, corps, coord)    

    else if (str_gen(iveh)%planet == eph_mars .OR. str_gen(iveh)%planet == eph_venus) then
       ! Corps central : Mars ou Vénus

       ! Calcul analytique de la position du corps (coord)
       ! Repère = EME2000, unité de distance = km  
       call eph_poscor(PSI_METHODE_EPH_ANALYTIQUE, date_frac, &
            str_gen(iveh)%planet, corps, coord)   

    end if
	    
    ! Calcul de la position du corps (pos_corps)
    do ii = 1,3   
       pos_corps(ii) = coord(ii) * 1000_PM_REEL
    enddo 

    ! Sortie par défaut (dans EME2000)
    pos = pos_corps
    
    if (corps == MSP_ENUM_SOLEIL) then
       mu = str_3co(iveh)%mu_soleil
    else if (corps == MSP_ENUM_LUNE) then
       mu = str_3co(iveh)%mu_lune
    else if (corps == MSP_ENUM_PHOBOS) then      
       mu = str_3co(iveh)%mu_phobos
    else if (corps == MSP_ENUM_DEIMOS) then     
       mu = str_3co(iveh)%mu_deimos
    else
       ! Erreur
       write(strcorps,*) corps
       call MSP_signaler_message(cle_mes="PS_MODELES_001", partie_variable=strcorps)
    endif

    ! Changement de repères (optionnels)
    if (present(repveis)) then      
       ! Conversion EME2000 VEIS
       call psi_rep_eme2000_veis(date, pos_corps, pos)         
    else if (present(repinteg)) then
       ! Conversion EME2000 RI
       call psi_rep_eme2000_ri(pos_corps, pos)
    endif   
  
  end subroutine ps_propage_analytique
  
  
  subroutine ps_propage_tchebytchev(date,corps,pos,mu, &
                                    repveis, repinteg)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_propage_tchebytchev
!
!$Resume
!  Calcul d'éphémérides pour PSIMU (méthode Tchebytchev)
!
!$Description
!  Calcul d'éphémérides pour PSIMU (méthode Tchebytchev)
!
!$Auteur
!  Gérald Mercadier (ATOS Origin)
!
!$Acces
!  PRIVE
!
!$Usage
!  call ps_propage_tchebytchev(date,corps,pos,mu, &
!.                             [repveis], [repinteg])
!.    type(tm_jour_sec) :: date
!.    integer :: corps
!.    real (KIND=pm_reel), dimension(3) :: pos
!.    real (KIND=pm_reel) :: mu
!.    integer :: repveis, repinteg
!
!$Common
!
!$Routines
!- md_joursec_jourfrac
!- MSP_signaler_message
!- eph_poscor
!- psi_rep_eme2000_veis
!- psi_rep_eme2000_ri
!
!$Include
!
!$Module
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    use ephem, only: eph_poscor
    
    implicit none 
  
    type(tm_jour_sec), intent(IN)  :: date
    integer, intent(IN) :: corps

    real (KIND=pm_reel), dimension(3), intent(OUT)  :: pos
    real (KIND=pm_reel), intent(OUT)  :: mu

    integer, intent(IN), optional :: repveis, repinteg

    ! variables locales
    type(tm_code_retour) :: code_retour
    real(kind=pm_reel) :: date_frac
    real (KIND=pm_reel), dimension(3)  :: pos_corps
    real (KIND=pm_reel), dimension(6)  :: coord
    character(len=20) :: strcorps
    integer :: ii
    

    ! Conversion date (jour,sec) en date frac
    call md_joursec_jourfrac(date, date_frac, code_retour)
    if(code_retour%valeur < 0) then
       call MSP_signaler_message(ier_mslib=code_retour)
       return
    end if
  
    ! Calcul de la position du corps (coord), méthode COMPAS TCHEB-MAD
    ! Repère = EME2000, unité de distance = km 
    call eph_poscor(PSI_METHODE_EPH_TCHEMAD, date_frac, &
            str_gen(iveh)%planet, corps, coord) 
        
    do ii = 1,3   
       pos(ii) = coord(ii) * 1000_PM_REEL
    enddo 
    
    
    if (corps == MSP_ENUM_SOLEIL) then     
       mu = str_3co(iveh)%mu_soleil 
    else if (corps == MSP_ENUM_LUNE) then
       mu = str_3co(iveh)%mu_lune  	 	     
    else if (corps == MSP_ENUM_PHOBOS) then      
       mu = str_3co(iveh)%mu_phobos
    else if (corps == MSP_ENUM_DEIMOS) then     
       mu = str_3co(iveh)%mu_deimos
    else
       ! Erreur
       write(strcorps,*) corps
       call MSP_signaler_message(cle_mes="PS_MODELES_001", partie_variable=strcorps)
    endif
    
    pos_corps = pos

    ! Changement de repères (optionnels)
    if (present(repveis)) then
       ! Conversion EME2000 VEIS 
       call psi_rep_eme2000_veis(date, pos_corps, pos)
    else if (present(repinteg)) then
       ! Conversion EME2000 RI
       call psi_rep_eme2000_ri(pos_corps, pos)
    endif

  end subroutine ps_propage_tchebytchev
  
  
  subroutine psi_rep_veis_ri(date, pos0, pos1)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psi_rep_veis_ri
!
!$Resume
!  Changement de repère VEIS->RI
!
!$Description
!  Changement de repère VEIS->RI
!
!$Auteur
!  Gérald Mercadier (ATOS Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psi_rep_veis_ri(date, pos0, pos1)
!.    type(tm_jour_sec) :: date 
!.    real (KIND=pm_reel), dimension(3) :: pos0
!.    real (KIND=pm_reel), dimension(3) :: pos1
!
!$Common
!
!$Routines
!- MSP_calcul_ecart_echt
!- mr_veis_TerVrai
!- MSP_signaler_message
!- mr_TerVrai_TerRef
!- ps_rep_dateInit_dateCour
!
!$Include
!
!$Module
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
  
    type(tm_jour_sec), intent(IN)  :: date    
    real (KIND=pm_reel), dimension(3), intent(IN)  :: pos0
    real (KIND=pm_reel), dimension(3), intent(OUT)  :: pos1
    
    ! variables locales
    real (KIND=pm_reel), dimension(3) :: pos_tmp
    real (KIND=pm_reel) :: delta_tu1, delta_tai
    type(tm_code_retour) :: code_retour
    

    call MSP_calcul_ecart_echt(date, pm_TE, ecart_tu1=delta_tu1, &
         ecart_tai=delta_tai)
    if ( MSP_gen_messages ("psi_rep_veis_ri")) return
 
    ! VEIS date courante vers Planéto date initiale

    ! -> Planéto vrai
    call mr_veis_TerVrai(date, delta_tu1, pos0, pos1, code_retour)
    if(code_retour%valeur < 0) then
       call MSP_signaler_message(ier_mslib=code_retour)
       return
    end if
    
    ! -> Planéto ref
    call mr_TerVrai_TerRef(0._PM_REEL,0._PM_REEL, pos1, pos_tmp, &
		  code_retour)
    if(code_retour%valeur < 0) then
       call MSP_signaler_message(ier_mslib=code_retour)
       return
    end if
		  
    ! date courante -> date initiale
    call ps_rep_dateInit_dateCour(pos_tmp, date, pos1, inverse=1)

  end subroutine psi_rep_veis_ri
  
  
  subroutine psi_rep_eme2000_veis(date, pos0, pos1)
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psi_rep_eme2000_veis
!
!$Resume
!  Changement de repère EME2000->VEIS
!
!$Description
!  Changement de repère EME2000->VEIS
!
!$Auteur
!  Gérald Mercadier (ATOS Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psi_rep_eme2000_veis(date, pos0, pos1)
!.    type(tm_jour_sec) :: date 
!.    real (KIND=pm_reel), dimension(3) :: pos0
!.    real (KIND=pm_reel), dimension(3) :: pos1
!
!$Common
!
!$Routines
!- MSP_calcul_ecart_echt
!- mu_mulvect3
!- ps_rep_dateInit_dateCour
!- mr_TerRef_TerVrai
!- MSP_signaler_message
!- mr_TerVrai_Veis
!
!$Include
!
!$Module
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
  
    type(tm_jour_sec), intent(IN)  :: date    
    real (KIND=pm_reel), dimension(3), intent(IN)  :: pos0
    real (KIND=pm_reel), dimension(3), intent(OUT)  :: pos1
    
    ! variables locales
    real (KIND=pm_reel), dimension(3) :: pos_tmp
    real (KIND=pm_reel) :: delta_tu1, delta_tai
    type(tm_code_retour) :: code_retour
    
    call MSP_calcul_ecart_echt(date, pm_TE, ecart_tu1=delta_tu1, &
         ecart_tai=delta_tai)
    if ( MSP_gen_messages ("psi_rep_eme2000_veis")) return
  
    ! Conversion EME2000 VEIS 

    ! -> Conversion EME2000 -> Planéto de la date initiale
    call mu_mulvect3(str_int(iveh)%Mat_ME2000_RI(1:3,1:3),pos0,pos_tmp,code_retour)

    ! date initiale  -> date courante
    call ps_rep_dateInit_dateCour(pos_tmp, date, pos1)

    ! -> Planéto Vrai
    call mr_TerRef_TerVrai(0._PM_REEL,0._PM_REEL, pos1, pos_tmp, code_retour)
    if(code_retour%valeur < 0) then
       call MSP_signaler_message(ier_mslib=code_retour)
       return
    end if

    ! -> Veis
    call mr_TerVrai_Veis(date, delta_tu1, pos_tmp, pos1, code_retour)
    if(code_retour%valeur < 0) then
       call MSP_signaler_message(ier_mslib=code_retour)
       return
    end if
  
  end subroutine psi_rep_eme2000_veis

  
  subroutine psi_rep_eme2000_ri(pos0, pos1)
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psi_rep_eme2000_ri
!
!$Resume
!  Changement de repère EME2000->RI
!
!$Description
!  Changement de repère EME2000->RI
!
!$Auteur
!  Gérald Mercadier (ATOS Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psi_rep_eme2000_ri(pos0, pos1)
!.    real (KIND=pm_reel), dimension(3) :: pos0
!.    real (KIND=pm_reel), dimension(3) :: pos1
!
!$Common
!
!$Routines
!- mu_mulvect3
!
!$Include
!
!$Module
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
  
    real (KIND=pm_reel), dimension(3), intent(IN)  :: pos0
    real (KIND=pm_reel), dimension(3), intent(OUT)  :: pos1
    
    ! variable locale
    type(tm_code_retour) :: code_retour
      
  
    ! EME2000 vers Planéto ou PQ de la date initiale
       
    call mu_mulvect3(str_int(iveh)%Mat_ME2000_RI(1:3,1:3),pos0,pos1,code_retour)
 
  end subroutine psi_rep_eme2000_ri


  subroutine ps_acc3corps(date,pos_RI,pos_Soleil_RI,mu_soleil,acc_3corps_RI)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_acc3corps
!
!$Resume
!  Routine chapeau de calcul des accélérations des 3è corps (Soleil, Lune ou Phobos/Deimos)
!
!$Description
!  Routine chapeau de calcul des accélérations des 3è corps (Soleil, Lune ou Phobos/Deimos).
!  Cette routine calcule les éphémérides manquantes, puis calcule une accélération
!  à l'aide de la MECASPA.
!  Note : les éphémérides du soleil sont données en entrée, car la position du soleil
!  sert dans d'autres calculs de forces. Par contre, la position de la lune, de phobos ou deimos (selon les modes)
!  ne sert que pour calculer l'accélération liées à ces "lunes". Le calcul des éphémérides
!  est donc uniquement réalisé dans cette routine.
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_acc3corps(date,pos_RI,pos_Soleil_RI,mu_soleil,acc_3corps_RI)
!.    type(tm_jour_sec) :: date
!.    real(kind=pm_reel), dimension(3) :: pos_RI
!.    real(kind=pm_reel), dimension(3) :: pos_Soleil_RI
!.    real(kind=pm_reel) :: mu_soleil
!.    real(kind=pm_reel), dimension(3) :: acc_3corps_RI
!
!$Arguments
!>E     date           :<tm_jour_sec>       date en jj 1950 TE (jours/secondes)
!>E     pos_RI         :<pm_reel,DIM=(3)>   position du véhicule dans le repère d'intégration (m)
!>E     pos_Soleil_RI  :<pm_reel,DIM=(3)>   position du Soleil dans le repère d'intégration (m)
!>E     mu_soleil      :<pm_reel>           mu du Soleil
!>S     acc_3corps_RI  :<pm_reel,DIM=(3)>   accélération liée aux 3è corps
!
!$Common
!
!$Routines
!- mu_mulvect3
!- ps_propage
!- MSP_signaler_message
!- MSP_acc_3corps
!
!$Include
!
!$Module
!#V
!- ps_integration_don
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use ps_integration_don



    implicit none


    ! Arguments
    !==========
    type(tm_jour_sec) ,               intent(in)  :: date
    real(kind=pm_reel), dimension(3), intent(in)  :: pos_RI
    real(kind=pm_reel), dimension(3), intent(in)  :: pos_Soleil_RI
    real(kind=pm_reel),               intent(in)  :: mu_soleil
    real(kind=pm_reel), dimension(3), intent(out) :: acc_3corps_RI


    ! Variables locales
    !==================
    real(kind=pm_reel), dimension(3) :: acc_lune_eme2000, acc_lune_RI
    real(kind=pm_reel), dimension(3) :: acc_soleil_RI
    real(kind=pm_reel), dimension(3) :: acc_phobos, acc_deimos
    real(kind=pm_reel), dimension(3) :: dir_soleil,dir_lune,dir_phobos,dir_deimos
    real(kind=pm_reel) :: dis_soleil,dis_lune,dis_phobos,dis_deimos
    real(kind=pm_reel), dimension(3) :: pos_eme2000,pos_lune,pos_phobos,pos_deimos
    real(kind=pm_reel) ::  mu_lune,mu_phobos,mu_deimos
    character(len=128) :: chaine

    type(tm_code_retour) :: code_retour_local

    ! Accélération due au potentiel lunaire (ou Phobos/Deimos pour Mars)
    ! Calcul d'abord dans l'EME2000, puis dans le repère d'intégration
    ! grâce à la matrice RI_ME2000
    !
    ! 3 étapes : 
    ! - exprimer la position satellite dans l'EME 2000
    ! - calculer les éphémérides du (des) lune(s) dans EME 2000 (ps_propage)
    ! - calculer l'accélération du(des) lune(s) dans EME 2000
    !=================================================================
    if ( str_mod(iveh)%ikle(1) /= 0 ) then

       ! 1) Calcul de la position du satellite dans l'EME 2000
       !======================================================
       call mu_mulvect3(str_int(iveh)%Mat_RI_ME2000(1:3,1:3),&
            pos_RI,pos_EME2000,code_retour_local)


       ! 2) Calcul des éphémérides
       !==========================
       if ( str_gen(iveh)%planet == eph_terre) then 
          call ps_propage(str_3co(iveh)%typephem, date,MSP_ENUM_LUNE,&
               pos_lune,mu_lune)
          if ( MSP_gen_messages("ps_acc3corps") ) return                            
       else if ( str_gen(iveh)%planet == eph_mars) then 
          call ps_propage(str_3co(iveh)%typephem, date,MSP_ENUM_PHOBOS,&
               pos_phobos,mu_phobos)
          if ( MSP_gen_messages("ps_acc3corps") ) return                            
          call ps_propage(str_3co(iveh)%typephem, date,MSP_ENUM_DEIMOS,&
               pos_deimos,mu_deimos)
          if ( MSP_gen_messages("ps_acc3corps") ) return                            
       else
          ! Pas de prise en compte du troisième corps pour des planètes autres que la Terre ou Mars
          ! ---------------------------------------------------------------------------------------
          write (chaine, *) str_gen(iveh)%planet
          call MSP_signaler_message (cle_mes="PS_MODELES_001",partie_variable=chaine)
          return
       endif

       ! 3) Calcul des accélérations
       !============================
       if ( str_gen(iveh)%planet == eph_terre) then 
          call MSP_acc_3corps (pos_EME2000,mu_lune,pos_lune,dis_lune,dir_lune,acc_lune_EME2000)
          if ( MSP_gen_messages("ps_acc3corps") ) return                            
          ! calcul des accélérations dans le RI
          call mu_mulvect3(str_int(iveh)%Mat_ME2000_RI(1:3,1:3),acc_lune_EME2000,acc_lune_RI,code_retour_local)

       else if ( str_gen(iveh)%planet == eph_mars) then
          call MSP_acc_3corps (pos_EME2000,mu_phobos,pos_phobos,dis_phobos,&
               dir_phobos,acc_phobos)
          if ( MSP_gen_messages("ps_acc3corps") ) return                            

          call MSP_acc_3corps (pos_EME2000,mu_deimos,pos_deimos,dis_deimos,&
               dir_deimos,acc_deimos)
          if ( MSP_gen_messages("ps_acc3corps") ) return                                


          acc_lune_EME2000(1) = acc_phobos(1) + acc_deimos(1)
          acc_lune_EME2000(2) = acc_phobos(2) + acc_deimos(2)
          acc_lune_EME2000(3) = acc_phobos(3) + acc_deimos(3)

          ! calcul des accélérations dans le RI
          call mu_mulvect3(str_int(iveh)%Mat_ME2000_RI(1:3,1:3),acc_lune_EME2000,&
               acc_lune_RI,code_retour_local)
       else
          ! Pas de prise en compte du troisième corps pour des planètes autres que la Terre ou Mars
          ! ---------------------------------------------------------------------------------------
          write (chaine, *) str_gen(iveh)%planet            
          call MSP_signaler_message (cle_mes="PS_MODELES_001",&
               partie_variable=chaine)
          return
       endif
    else
       ! Pas de potentiel 'lunaire' 
       acc_lune_RI(1) = 0._pm_reel
       acc_lune_RI(2) = 0._pm_reel
       acc_lune_RI(3) = 0._pm_reel
    endif

    ! Accélération due au potentiel solaire:
    ! (calcul direct dans RI, car la position
    ! du soleil est déjà donnée dans RI
    !=======================================
    if ( str_mod(iveh)%ikle(2) /= 0 ) then
       call MSP_acc_3corps (pos_RI,mu_soleil,pos_soleil_RI,&
            dis_soleil,dir_soleil,acc_soleil_RI)
       if ( MSP_gen_messages("ps_acc3corps") ) return                                
    else
       acc_soleil_RI(1) = 0._pm_reel
       acc_soleil_RI(2) = 0._pm_reel
       acc_soleil_RI(3) = 0._pm_reel
    endif

    ! Somme des accélérations 
    acc_3corps_RI(1) = acc_lune_RI(1) + acc_soleil_RI(1)
    acc_3corps_RI(2) = acc_lune_RI(2) + acc_soleil_RI(2)
    acc_3corps_RI(3) = acc_lune_RI(3) + acc_soleil_RI(3)


  end subroutine ps_acc3corps



end module ps_troiscorps
