module eph_analytique

!***********************************************************************
!$<AM-V2.0>
!
!$Type
!  module
!
!$Resume
!   EPHEMERIDES : méthode VSOP82 analytique
!
!$Description
!
!$Auteur
!  Equipe interplanetaire
!  Maj Florence VIVARES (SchlumbergerSema)
!
!$Version
!   $Id: eph_analytique.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!   $Log: eph_analytique.F90,v $
!   Revision 1.14  2010/10/21 13:46:20  ogarat
!   VERSION::AQ::21/10/2010:Ajout du fin historique
!
!   Revision 1.13  2009/08/26 15:01:41  cml
!   FA-1238 : Decomposition du changement de repère
!
!   Revision 1.12  2009/07/17 14:02:02  cml
!   AQ : Suppression d'une variable inutilisee
!
!   Revision 1.11  2009/07/17 13:40:32  cml
!   FA-ID 1238 : Correction du changement de repre ecliptique de la date vers EME2000
!
!   Revision 1.10  2009/03/03 14:05:16  cml
!   DM-ID 1241 : Ajout de la routine chapeau eph_calcule_vsop82
!   Revision 1.9  2008/10/01 16:11:57  tanguyy
!   FA-ID 1007 : suppression de code mort et amelioration du parenthesage dans un test
!   Revision 1.8  2008/08/04 13:17:30  gss
!   DM-ID 1058 : (portage g95) suppression de la variable ier non utilisée.
!   Revision 1.7  2006/05/30 12:29:05  vivaresf
!   Separation COMPAS_UI,
!   suppression MSPRO
!   robustesse (iostat sur les deallocate)
!   Revision 1.6  2006/05/30 08:23:17  vivaresf
!   DM-ID 387 : variables inutiles
!   Revision 1.5  2006/04/26 11:54:16  bouillaj
!   FA-ID 371 : Modification de la fabrication de certains noms de fichiers pour eviter les blancs
!   Revision 1.4  2006/04/06 16:02:17  vpg
!   Mise a jour de la lecture du code et de la date de reference du repere
!   Revision 1.3  2006/03/09 12:05:22  vivaresf
!   FA-ID 500 : trace de la DM 391
!   Revision 1.2  2005/12/08 18:39:02  vivaresf
!   Cartouches et vérification des déclarations
!   Revision 1.1.1.1  2005/12/07 07:23:08  vivaresf
!   Refonte de COMPAS
!   Revision 1.7  2005/11/07 15:56:12  bouillaj
!   DM-ID 391 : Amelioration qualite sur la LIBEPHEM
!   Revision 1.6  2005/10/13 10:38:20  bouillaj
!   FA-ID 371 : Cloture du FT (Corrections dans libephem pour passage linux)
!   Revision 1.5.2.1  2005/10/13 10:30:01  bouillaj
!   FA-ID 371 : Version initiale du FT (Corrections dans libephem pour passage linux)
!   Revision 1.5  2005/10/13 09:04:13  bouillaj
!   FA-ID 366 : erreur dans le modulo de anmod
!   Revision 1.4  2005/03/09 09:12:02  vivaresf
!   Correction des cartouches
!   Revision 1.3  2004/12/17 14:58:09  vivaresf
!   Documentation
!   Revision 1.2  2004/05/25 13:15:59  vivaresf
!   DM_158
!   Revision 1.1.1.1.2.2  2004/05/25 09:59:01  vivaresf
!   On revient sur les valeurs anciennes valeurs pour compatibilite ascendante
!   Revision 1.1.1.1.2.1  2004/05/24 16:00:40  vivaresf
!   Version V1_9 de la libEPHEM :
!   - Utilisation directe de la MSLIB a la place de la MECASPA
!   ==> des differences de l'ordre du km sur Y et Z, et du mm/s sur vY et vZ dues
!       a l'utilisation de la valeur de l'obliquite par defaut et non plus
!       a la valeur MECASPA/AMLIB MSP_obliquite = 0.40909261307192995_pm_reel
!   Revision 1.1.1.1  2004/04/02 09:07:24  vivaresf
!   Gestion de configuration locale
!   Revision 1.15  2004/01/13 09:16:26  bremard
!   Mise à jour cartouche
!   Revision 1.14  2004/01/09 16:20:01  bremard
!   Mise à jour des cartouches
!   Revision 1.13  2004/01/09 16:03:18  bremard
!   Remplacement AM_rep_chg6 par MSP_conv_typrep
!   Revision 1.12  2004/01/07 16:18:12  bremard
!   Mise à jour cartouche
!   Revision 1.11  2003/12/31 16:00:25  bremard
!   Ajout ss-prog eph_vsop82 simplifié à partir de la version AMLIB + init fichier saut TUC
!   Revision 1.10  2003/12/30 16:16:13  bremard
!   Ajout commentaire
!   Revision 1.9  2002/04/09 09:37:27  vivaresf
!   Initialisation des variables xpla1 et xpla2
!   Revision 1.8  2002/03/13 08:04:13  bremard
!   PhB - Changement de repere sans prise en compte de la vitesse d'entrainement (keyvit=0)
!   Revision 1.7  2001/12/20 10:42:12  vivaresf
!   Correction numero de repere
!   Revision 1.6  2001/12/18 16:15:01  vivaresf
!   Maj documentation
!   Revision 1.5  2001/12/05 10:48:05  bremard
!   PhB - Mise à jour des cartouches
!   Revision 1.4  2001/12/05 10:31:11  bremard
!   PhB - Mise à jour des cartouches
!   01/92   Création Equipe interplanétaire (pvcnes)
!   01/96   Modifications AMLIB (environnement poscor)
!   04/99 - Ph BREMARD - Modification de l'interface:
!                - ajout de tref 
!
!$FinHistorique
!
!$Routines
!- eph_pvcnes
!- eph_calcule_vsop82
!- eph_vsop82
!
!$Module
!#V
!- mslib
!- msp_gestion_erreur
!- eph_varglob
!- eph_constantes
!#
!
!$Voir-Aussi
!.  eph_pvcnes eph_calcule_vsop82 eph_vsop82
!
!$<>
!***********************************************************************

    use mslib
    use msp_gestion_erreur
    use eph_varglob
    use eph_constantes, only : eph_date_jj50_j2000

    implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: eph_analytique.F90 69 2012-09-11 08:33:34Z ffsm $'


contains

  subroutine eph_pvcnes(numpla,ncorpsc,t1950,xpla)
!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  eph_pvcnes
!
!$Resume
!  Calcul analytique des éphémérides des planètes (théorie VSOP82)
!
!$Description
!  Cette subroutine calcule les positions de différentes planètes
!  du Système solaire, grâce à la théorie VSOP82 analytique développée par
!  le BDL.
!
!$Usage
!  call eph_pvcnes(numpla,ncorpsc,t1950,xpla)
!.    real (KIND=pm_reel) :: t1950
!.    integer :: numpla,ncorpsc
!.    real (KIND=PM_REEL), dimension(6) :: xpla
!
!$Arguments
!>E     numpla   :<integer>           numéro du corps d'intérêt    
!>E     ncorpsc  :<integer>           numéro du corps central
!>E     t1950    :<pm_reel>           date (B1950)
!>S     xpla     :<PM_REEL,DIM=(6)>   coordonnées cartésiennes 
!
!$Mots-cles
!       éphémérides
!
!$Routines
!- eph_calcule_vsop82
!- md_jourfrac_joursec
!- mr_rep_fon
!- MSP_signaler_message
!- mu_mulvect3
!
!$Remarques
!.       Le repère de sortie est l'EME200.
!.       Corps possibles : Soleil + 8 planètes (toutes sauf Pluton).
!.       Peut être appelée sans initialisations.
!
!$Voir-Aussi
!
!$<>
!***********************************************************************
 
    implicit none

! parametres
    real (KIND=pm_reel), intent(in) :: t1950
    integer, intent(in) :: numpla,ncorpsc
    real (KIND=PM_REEL), dimension(6), intent(out) :: xpla

! variables locales
    real (KIND=PM_REEL), dimension(6) :: xpla_ecli, xpla_ecli_J2000
    type(tm_code_retour) :: code_retour
    real(pm_reel), dimension(3,3) :: mat   ! matrice de passage 
    type(tm_jour_sec) :: jj1950_date 
    type(tm_jour_sec) :: jj1950_J2000 

! initialisation
    xpla(1:6) = 0.0_pm_reel
    
    ! Appel à la routine dans le repère de l'écliptique à la date t1950
    call eph_calcule_vsop82(numpla,ncorpsc,t1950,xpla_ecli)
    if (MSP_gen_messages("eph_pvcnes") ) return

    ! Si le calcul a bien réussi
    if ( xpla_ecli(1) /= 0.0_pm_reel ) then

       ! Conversion des dates en jour / secondes
       call md_jourfrac_joursec(t1950, jj1950_date, code_retour)
       call md_jourfrac_joursec(eph_date_jj50_j2000, jj1950_J2000, code_retour)

       ! Changement de repère "Ecliptique de la date"
       ! vers "EME2000" imposé par la méthode VSOP82
       call mr_rep_fon(pm_ecli_moy_ecli_moy, pm_lieske_wahr, &
            jj1950_date, jj1950_J2000, mat, code_retour, delta_tai=0._pm_reel)
       if (code_retour%valeur < 0) then
          call MSP_signaler_message(ier_mslib=code_retour, &
               routine="eph_pvcnes - appel a mr_rep_fon")
          return
       endif

       ! Produit matriciel pour transformation de la position puis de la vitesse
       call mu_mulvect3(mat, xpla_ecli(1:3), xpla_ecli_J2000(1:3),code_retour) 
       call mu_mulvect3(mat, xpla_ecli(4:6), xpla_ecli_J2000(4:6),code_retour) 

       call mr_ecliJ2000_J2000(xpla_ecli_J2000(1:3), xpla(1:3), code_retour, &
            vit_ecliJ2000=xpla_ecli_J2000(4:6), vit_J2000=xpla(4:6),&
            obliquite = 0.40909261307192995_PM_REEL)
       if (code_retour%valeur < 0) then
          call MSP_signaler_message(ier_mslib=code_retour, &
               routine="eph_pvcnes - appel a mr_ecliJ2000_J2000")
          return
       endif

    endif

! fin de routine

  end subroutine eph_pvcnes

  subroutine eph_calcule_vsop82(numpla,ncorpsc,t1950,xpla_ecli)
!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  eph_calcule_vsop82
!
!$Resume
!  Calcul analytique des éphémérides des planètes (théorie VSOP82)
!
!$Description
!  Cette subroutine calcule les positions de différentes planètes
!  du Système solaire, grâce à la théorie VSOP82 analytique développée par
!  le BDL. Le repère de sorite est l'ecliptique de la date.
!
!$Usage
!  call eph_calcule_vsop82(numpla,ncorpsc,t1950,xpla_ecli)
!.    real (KIND=pm_reel) :: t1950
!.    integer :: numpla,ncorpsc
!.    real (KIND=PM_REEL), dimension(6) :: xpla_ecli
!
!$Arguments
!>E     numpla     :<integer>           numéro du corps d'intérêt    
!>E     ncorpsc    :<integer>           numéro du corps central
!>E     t1950      :<pm_reel>           date (B1950)
!>S     xpla_ecli  :<PM_REEL,DIM=(6)>   coordonnées cartésiennes dans le repère
!                                     écliptique de la date
!
!$Mots-cles
!       éphémérides
!
!$Routines
!- MSP_signaler_message
!- eph_vsop82
!
!$Remarques
!.       Le repère de sortie est l'ecliptique de la date.
!.       Corps possibles : Soleil + 8 planètes (toutes sauf Pluton).
!.       Peut être appelée sans initialisation.
!
!$Voir-Aussi
!
!$<>
!***********************************************************************
    implicit none

! parametres
    real (KIND=pm_reel), intent(in) :: t1950
    integer, intent(in) :: numpla,ncorpsc
    real (KIND=PM_REEL), dimension(6), intent(out) :: xpla_ecli

! variables locales
 
    integer :: ii
    ! integer :: lfictuc,ier
    integer :: n1=0
    integer :: n2=0
    character (LEN=10) :: ns
    real (KIND=PM_REEL), dimension(6) :: xpla1, xpla2

! initialisation
    xpla_ecli(1:6) = 0.d0
    xpla1(1:6) = 0.d0
    xpla2(1:6) = 0.d0
    n1 = numpla
    n2 = ncorpsc

! contrôles

    ! conversion numpla, ncorpsc 
    if(numpla .gt.100.and.(mod(numpla,100) .eq.99)) n1 = numpla /100
    if(ncorpsc.gt.100.and.(mod(ncorpsc,100).eq.99)) n2 = ncorpsc/100

    ! Vérification des numéros de planètes
    if((n1.lt.1).or.(n1.gt.10).or.(n1.eq.9)) then
       write(ns,*) n1
       call MSP_signaler_message(cle_mes="EPH_ERR_CODEPLA", &
            partie_variable=trim(ns) // &
            " impossible pour cette méthode", routine="eph_calcule_vsop82")
       return
    endif
    if((n2.lt.1).or.(n2.gt.10).or.(n2.eq.9)) then
       write(ns,*) n2
       call MSP_signaler_message(cle_mes="EPH_ERR_CODEPLA", &
            partie_variable=trim(ns) // &
            " impossible pour cette méthode", routine="eph_calcule_vsop82")
       return
    endif

! appel a la fonction de calcul
    if (n1.ne.n2) then
       ! appel à l'amlib
       if(n2.ne.10) call eph_vsop82(t1950,n2,xpla2)
       if (MSP_gen_messages("eph_calcule_vsop82")) return 
 

       if(n1.ne.10) call eph_vsop82(t1950,n1,xpla1)
       if (MSP_gen_messages("eph_calcule_vsop82")) return 

       ! composition des deux couples  positions-vitesses
       do ii = 1,6 
          xpla_ecli(ii) = xpla1(ii) - xpla2(ii)
       enddo

    endif

! fin de routine

  end subroutine eph_calcule_vsop82

  subroutine eph_vsop82(t1950,numpla,ephem)
!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  eph_vsop82
!
!$Resume
!  Calcul des éphémérides des planètes grâce à la méthode VSOP82
!
!$Description
!  Cette subroutine calcule les positions de différentes planètes
!  du Système solaire, grâce à la théorie VSOP82 analytique développée par
!  le BDL.
!  Les calculs sont effectués dans l'écliptique dynamique de
!  la date, selon la méthode analytique issue du BDL
!  VSOP82 (cf "CONNAISSANCE DES TEMPS 1988, page XXXIII a XXXVIII).
!
!$Usage
!  call eph_vsop82(t1950,numpla,ephem)
!.    real(KIND=PM_REEL) :: t1950
!.    integer :: numpla
!.    real(KIND=PM_REEL), dimension(6) :: ephem
!
!$Arguments
!>E     t1950   :<PM_REEL>           date (JJ CNES 1950)
!>E     numpla  :<integer>           numéro du corps d'intérêt    
!.                                    (numérotation AMLIB)
!.                                    1 pour Mercure
!.                                    2 pour Vénus
!.                                    3 pour Terre
!.                                    4 pour Mars
!.                                    5 pour Jupiter
!.                                    6 pour Saturne
!.                                    7 pour Uranus
!.                                    8 pour Neptune
!>S     ephem   :<PM_REEL,DIM=(6)>   coordonnées
!
!$Mots-cles
!       éphémérides
!
!$Routines
!- MSP_signaler_message
!- mv_cir_equa_car
!
!$Remarques
!.       Le repère de sortie est l'EME200.
!.       Corps possibles : Soleil + 8 planètes (toutes sauf Pluton).
!.       Peut être appelée sans initialisations.
!
!$Voir-Aussi
!
!$<>
!***********************************************************************

        implicit none

! Arguments
        real(KIND=PM_REEL), intent(in) :: t1950
        integer, intent(in)            :: numpla
        real(KIND=PM_REEL), dimension(6),intent(out) :: ephem
         
! Cariables locales
        character (LEN=10) :: ns
        integer :: indi,indj

        real(KIND=PM_REEL),dimension(8,6,7) :: coeffbdl
        real(KIND=PM_REEL),dimension(6,7)   :: coef
        real(KIND=PM_REEL),dimension(6)     :: adapt
        real(KIND=PM_REEL) :: xmus,t2000,ua,tt,anmod
        
        type(tm_code_retour) :: code_ret
        type(tm_orb_cir_equa) :: orbcirequ


!-------------------------------------------------------------------------------
!	Initialisation sous forme de data de :
!             - la constante d'attraction solaire (m3.s-2) 
!	      - l'unité astronomique (m)
!       On utilise ici les valeurs fournies par la Connaissance des Temps 1988
!       du BDL pour être cohérent avec les éphémérides fournies
!------------------------------------------------------------------------------

        data xmus/1.32712438e20_PM_REEL/
        data ua/1.4959787e11_PM_REEL/

!-------------------------------------------------------------------------------
!	Définition des tables des coefficients de la théorie
!	indi = 1,6  : 6 paramètres adaptés
!	indj = 1,7  : coefficients multiplicatifs de la date élevée 
!                    aux puissances respectivement 0,6
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!	Coefficients pour MERCURE
!-------------------------------------------------------------------------------

        data ( coeffbdl(1,indi,1),indi = 1 , 6)/ &
                        0.38709830982_PM_REEL ,  &
                        252.250905528_PM_REEL ,  &
                        0.044660598_PM_REEL,     &
                        0.200723314_PM_REEL,     &
                        0.040615634_PM_REEL ,    &
                        0.045635505_PM_REEL /

        data ( coeffbdl(1,indi,2),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        5381066600.9664_PM_REEL, &
                      - 0.054483487_PM_REEL,     &
                        0.012331538_PM_REEL ,    &
                      - 0.009342389_PM_REEL ,    &
                        0.008527127_PM_REEL /

        data ( coeffbdl(1,indi,3),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        109.4280_PM_REEL ,       &
                      - 0.001806305_PM_REEL ,    &
                      - 0.007374087_PM_REEL,     &
                      - 0.000919439_PM_REEL ,    &
                      - 0.000955462_PM_REEL /

        data ( coeffbdl(1,indi,4),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0639_PM_REEL ,         &
                        0.000663185_PM_REEL ,    &
                      - 0.000185002_PM_REEL ,    &
                        0.000065185_PM_REEL ,    &
                      - 0.000067142_PM_REEL /

        data ( coeffbdl(1,indi,5),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL ,            &
                        0.000014667_PM_REEL ,    &
                        0.000044532_PM_REEL ,    &
                        0.000003685_PM_REEL ,    &
                        0.000003324_PM_REEL /

        data ( coeffbdl(1,indi,6),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL ,            &
                      - 0.000002376_PM_REEL ,    &
                        0.000000945_PM_REEL ,    &
                      - 0.000000134_PM_REEL ,    &
                        0.000000159_PM_REEL /

        data ( coeffbdl(1,indi,7),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL ,            &
                      - 0.000000051_PM_REEL ,    &
                      - 0.000000105_PM_REEL ,    &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL /

!-------------------------------------------------------------------------------
! 	Coefficients pour VENUS
!-------------------------------------------------------------------------------

        data ( coeffbdl(2,indi,1),indi = 1 , 6)/ &
                        0.72332981996_PM_REEL ,  &
                        181.97980083333_PM_REEL, &
                       -0.004492821_PM_REEL,     &
                        0.005066847_PM_REEL,     &
                        0.006824101_PM_REEL,     &
                        0.028822858_PM_REEL /

        data ( coeffbdl(2,indi,2),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        2106691669.0859_PM_REEL, &
                      - 0.000923135_PM_REEL,     &
                      - 0.001456941_PM_REEL,     &
                      - 0.004512949_PM_REEL,     &
                        0.001158456_PM_REEL /

        data ( coeffbdl(2,indi,3),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        111.8173_PM_REEL ,       &
                        0.000225036_PM_REEL ,    &
                      - 0.000058477_PM_REEL ,    &
                      - 0.000118430_PM_REEL ,    &
                      - 0.000349201_PM_REEL /

        data ( coeffbdl(2,indi,4),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0523_PM_REEL ,         &
                        0.000001441_PM_REEL ,    &
                        0.000022573_PM_REEL ,    &
                        0.000017766_PM_REEL ,    &
                      - 0.000008779_PM_REEL /

        data ( coeffbdl(2,indi,5),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL ,            &
                      - 0.000001682_PM_REEL ,    &
                      - 0.000000603_PM_REEL ,    &
                        0.000000472_PM_REEL ,    &
                        0.000000594_PM_REEL /

        data ( coeffbdl(2,indi,6),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL ,            &
                        0.000000062_PM_REEL ,    &
                      - 0.000000101_PM_REEL ,    &
                      - 0.000000004_PM_REEL ,    &
                        0.000000017_PM_REEL /

        data ( coeffbdl(2,indi,7),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL /

!-------------------------------------------------------------------------------
! 	Coefficients pour la TERRE
!-------------------------------------------------------------------------------

        data ( coeffbdl(3,indi,1),indi = 1 , 6)/ & 
                        1.00000101778_PM_REEL ,  &
                        100.46644850_PM_REEL ,   &
                       -0.003740816_PM_REEL,     &
                        0.016284477_PM_REEL,     &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL /

        data ( coeffbdl(3,indi,2),indi = 1 , 6)/ & 
                        0.0_PM_REEL ,            &
                        1296027713.6329_PM_REEL, &
                      - 0.004793106_PM_REEL,     &
                      - 0.001532379_PM_REEL ,    &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL /

        data ( coeffbdl(3,indi,3),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        109.3241_PM_REEL ,       &
                        0.000281128_PM_REEL,     &
                      - 0.000720171_PM_REEL,     &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL /

        data ( coeffbdl(3,indi,4),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0762_PM_REEL ,         &
                        0.000073831_PM_REEL ,    &
                        0.000032299_PM_REEL ,    &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL /

        data ( coeffbdl(3,indi,5),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL ,            &
                       -0.000002651_PM_REEL  ,   &
                        0.000005789_PM_REEL ,    &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL /

        data ( coeffbdl(3,indi,6),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL ,            &
                       -0.000000368_PM_REEL ,    &
                       -0.000000166_PM_REEL ,    &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL /

        data ( coeffbdl(3,indi,7),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL ,            &
                        0.000000008_PM_REEL ,    &
                      - 0.000000020_PM_REEL ,    &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL /

!-------------------------------------------------------------------------------
!	Coefficients pour MARS
!-------------------------------------------------------------------------------

        data ( coeffbdl(4,indi,1),indi = 1 , 6)/     &
                        1.52367934191_PM_REEL ,      &
                        355.433274638888860_PM_REEL, &
                        0.085365603_PM_REEL ,        &
                       -0.037899732_PM_REEL,         &
                        0.010470426_PM_REEL,         &
                        0.012284493_PM_REEL /

        data ( coeffbdl(4,indi,2),indi = 1 , 6)/ &
                        0.00000000031_PM_REEL ,  &
                        689101073.0852_PM_REEL,  &
                        0.013005053_PM_REEL,     &
                        0.027062760_PM_REEL,     &
                      - 0.001689431_PM_REEL ,    &
                        0.001371039_PM_REEL /

        data ( coeffbdl(4,indi,3),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        111.9499_PM_REEL ,       &
                      - 0.004287376_PM_REEL ,    &
                        0.002245677_PM_REEL ,    &
                      - 0.000082811_PM_REEL,     &
                      - 0.000107356_PM_REEL /

        data ( coeffbdl(4,indi,4),indi = 1 , 6)/  &
                        0.0_PM_REEL ,             &
                        0.0549_PM_REEL ,          &
                      - 0.000259837_PM_REEL ,     &
                      - 0.000451825_PM_REEL ,     &
                        0.000003613_PM_REEL ,     &
                      - 0.000002604_PM_REEL /

        data ( coeffbdl(4,indi,5),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL ,            &
                        0.000035436_PM_REEL  ,   &
                      - 0.000022566_PM_REEL ,    &
                      - 0.000000022_PM_REEL ,    &
                      - 0.000000068_PM_REEL /

        data ( coeffbdl(4,indi,6),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL ,            &
                        0.000001562_PM_REEL ,    &
                        0.000002191_PM_REEL ,    &
                        0.000000024_PM_REEL ,    &
                      - 0.000000011_PM_REEL /

        data ( coeffbdl(4,indi,7),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL ,            &
                      - 0.000000111_PM_REEL ,    &
                        0.000000090_PM_REEL ,    &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL /

!-------------------------------------------------------------------------------
! 	Coefficients pour JUPITER
!-------------------------------------------------------------------------------

        data ( coeffbdl(5,indi,1),indi = 1 , 6)/    &
                        5.20260319132_PM_REEL ,     &
                        34.351483916666666_PM_REEL, &
                        0.046985721_PM_REEL ,       &
                        0.012003857_PM_REEL,        &
                       -0.002065611_PM_REEL,        &
                        0.011183772_PM_REEL /

        data ( coeffbdl(5,indi,2),indi = 1 , 6)/ &
                        0.00000191323_PM_REEL ,  &
                        109306900.4007_PM_REEL,  &
                      - 0.001796949_PM_REEL,     &
                        0.013628604_PM_REEL,     &
                      - 0.001905724_PM_REEL ,    &
                      - 0.000839731_PM_REEL /

        data ( coeffbdl(5,indi,3),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        80.5462_PM_REEL ,        &
                      - 0.002042137_PM_REEL ,    &
                        0.000042602_PM_REEL ,    &
                        0.000108273_PM_REEL,     &
                      - 0.000159489_PM_REEL /

        data ( coeffbdl(5,indi,4),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0902_PM_REEL ,         &
                      - 0.000040262_PM_REEL ,    &
                      - 0.000210827_PM_REEL ,    &
                        0.000008934_PM_REEL ,    &
                        0.000007916_PM_REEL /

        data ( coeffbdl(5,indi,5),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL ,            &
                        0.000016854_PM_REEL  ,   &
                      - 0.000006128_PM_REEL ,    &
                      - 0.000000422_PM_REEL ,    &
                        0.000000378_PM_REEL /

        data ( coeffbdl(5,indi,6),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL ,            &
                        0.000000580_PM_REEL ,    &
                        0.000001103_PM_REEL ,    &
                      - 0.000000012_PM_REEL ,    &
                      - 0.000000020_PM_REEL /

        data ( coeffbdl(5,indi,7),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL ,            &
                      - 0.000000061_PM_REEL ,    &
                        0.000000041_PM_REEL ,    &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL /

!-------------------------------------------------------------------------------
! 	Coefficients pour SATURNE
!-------------------------------------------------------------------------------

        data ( coeffbdl(6,indi,1),indi = 1 , 6)/ &
                        9.55490959574_PM_REEL ,  &
                        50.07747136_PM_REEL ,    &
                       -0.002960036_PM_REEL,     &
                        0.055429643_PM_REEL,     &
                       -0.008717474_PM_REEL ,    &
                        0.019891473_PM_REEL /

        data ( coeffbdl(6,indi,2),indi = 1 , 6)/ &
                       -0.00002138917_PM_REEL ,  &
                        44046396.5063_PM_REEL ,  &
                      - 0.018813318_PM_REEL,     &
                      - 0.004477771_PM_REEL ,    &
                      - 0.002914183_PM_REEL ,    &
                      - 0.001633044_PM_REEL /

        data ( coeffbdl(6,indi,3),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        187.0274_PM_REEL ,       &
                        0.001283285_PM_REEL ,    &
                      - 0.003261143_PM_REEL,     &
                        0.000157351_PM_REEL ,    &
                      - 0.000223323_PM_REEL /

        data ( coeffbdl(6,indi,4),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                      - 0.0115_PM_REEL ,         &
                        0.000384811_PM_REEL ,    &
                        0.000200072_PM_REEL ,    &
                        0.000012382_PM_REEL ,    &
                        0.000011193_PM_REEL /

        data ( coeffbdl(6,indi,5),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL ,            &
                       -0.000021446_PM_REEL  ,   &
                        0.000034661_PM_REEL ,    &
                      - 0.000000753_PM_REEL ,    &
                        0.000000588_PM_REEL /

        data ( coeffbdl(6,indi,6),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL ,            &
                       -0.000002522_PM_REEL ,    &
                      - 0.000001734_PM_REEL ,    &
                      - 0.000000028_PM_REEL ,    &
                      - 0.000000055_PM_REEL /

        data ( coeffbdl(6,indi,7),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL ,            &
                        0.000000111_PM_REEL ,    &
                      - 0.000000154_PM_REEL ,    &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL /

!-------------------------------------------------------------------------------
! 	Coefficients pour URANUS
!-------------------------------------------------------------------------------

        data ( coeffbdl(7,indi,1),indi = 1 , 6)/ &
                        19.21844606178_PM_REEL , &
                        314.055005111_PM_REEL ,  &
                       -0.045951324_PM_REEL,     &
                        0.005637913_PM_REEL,     &
                        0.001859151_PM_REEL ,    &
                        0.006486170_PM_REEL /

        data ( coeffbdl(7,indi,2),indi = 1 , 6)/ &
                       -0.00000037163_PM_REEL ,  &
                        15475106.0196_PM_REEL ,  &
                      - 0.001191266_PM_REEL,     &
                      - 0.011954073_PM_REEL ,    &
                      - 0.000571320_PM_REEL ,    &
                        0.000234059_PM_REEL /

        data ( coeffbdl(7,indi,3),indi = 1 , 6)/ &
                        0.00000009791_PM_REEL ,  &
                        109.5619_PM_REEL ,       &
                        0.001544939_PM_REEL ,    &
                      - 0.000135566_PM_REEL,     &
                      - 0.000019753_PM_REEL ,    &
                        0.000010660_PM_REEL /

        data ( coeffbdl(7,indi,4),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0933_PM_REEL ,         &
                        0.000011213_PM_REEL ,    &
                        0.000132033_PM_REEL ,    &
                      - 0.000004990_PM_REEL ,    &
                      - 0.000001193_PM_REEL /

        data ( coeffbdl(7,indi,5),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL ,            &
                       -0.000008360_PM_REEL  ,   &
                        0.000000734_PM_REEL ,    &
                        0.000000019_PM_REEL ,    &
                      - 0.000000483_PM_REEL /

        data ( coeffbdl(7,indi,6),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL ,            &
                       -0.000000038_PM_REEL ,    &
                       -0.000000416_PM_REEL ,    &
                        0.000000032_PM_REEL ,    &
                      - 0.000000005_PM_REEL /

        data ( coeffbdl(7,indi,7),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL ,            &
                        0.000000017_PM_REEL ,    &
                      - 0.000000001_PM_REEL ,    &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL /

!-------------------------------------------------------------------------------
! 	Coefficients pour NEPTUNE
!-------------------------------------------------------------------------------

        data ( coeffbdl(8,indi,1),indi = 1 , 6)/ &
                        30.11038686942_PM_REEL , &
                        304.348665471_PM_REEL ,  &
                        0.005999776_PM_REEL,     &
                        0.006692424_PM_REEL,     &
                      - 0.010291478_PM_REEL ,    &
                        0.011516840_PM_REEL /

        data ( coeffbdl(8,indi,2),indi = 1 , 6)/ &
                      - 0.00000166346_PM_REEL ,  &
                        7915799.1328_PM_REEL ,   &
                      - 0.001623178_PM_REEL,     &
                        0.001541238_PM_REEL ,    &
                      - 0.001674318_PM_REEL ,    &
                      - 0.002585402_PM_REEL /

        data ( coeffbdl(8,indi,3),indi = 1 , 6)/ &
                        0.00000006857_PM_REEL ,  &
                        111.3346_PM_REEL ,       &
                      - 0.000202253_PM_REEL ,    &
                      - 0.000192797_PM_REEL,     &
                        0.000305826_PM_REEL ,    &
                      - 0.000118272_PM_REEL /

        data ( coeffbdl(8,indi,4),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0632_PM_REEL ,         &
                        0.000014843_PM_REEL ,    &
                      - 0.000018028_PM_REEL ,    &
                        0.000005675_PM_REEL ,    &
                        0.000023739_PM_REEL /

        data ( coeffbdl(8,indi,5),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL ,            &
                        0.000001222_PM_REEL ,    &
                        0.000000822_PM_REEL ,    &
                      - 0.000001401_PM_REEL ,    &
                        0.000000209_PM_REEL/
 
        data (  coeffbdl(8,indi,6),indi = 1 , 6)/ &
                         0.0_PM_REEL ,            &
                         0.0_PM_REEL ,            &
                       - 0.000000034_PM_REEL ,    &
                         0.000000067_PM_REEL ,    &
                       - 0.000000005_PM_REEL ,    &
                       - 0.000000069_PM_REEL /

        data ( coeffbdl(8,indi,7),indi = 1 , 6)/ &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL ,            &
                        0.000000000_PM_REEL ,    &
                      - 0.000000000_PM_REEL ,    &
                        0.0_PM_REEL ,            &
                        0.0_PM_REEL /

!-------------------------------------------------------------------------------
! 	Code de calcul
!-------------------------------------------------------------------------------

!	Vérification du numéro de planète en entrée

        if( numpla.lt.1.or.numpla.gt.8 ) then
           write(ns,*) numpla
           call MSP_signaler_message(cle_mes="EPH_ERR_CODEPLA", &
                partie_variable=trim(ns) // &
                " impossible pour cette méthode", routine="eph_vsop82")
           return
        endif


!	Conversion dans l'echelle de temps t2000:
!	Les expressions numeriques des quantites relatives à la 
!	précession sont données dans ce repère pour le modèle de Lieske.

	t2000 = t1950 - eph_date_jj50_j2000

!    	Conversion dans l'echelle de temps :
!    	on calcule  l' intervalle de temps par rapport au 01/01/2000 
!    	a 12h en milliers d annees de 365250 jours 

        tt =  t2000/365250.0_PM_REEL

!    	Récupération de la table des coefficients

        do indi = 1,6
           do indj = 1,7
              coef(indi,indj) = coeffbdl(numpla,indi,indj)
           enddo
        enddo

        do indj = 1,7
	   coef(2,indj) = coef(2,indj) * pm_pi / 180.0_PM_REEL
	   if( (indj >= 2) .and. (indj <= 4) ) then
	      coef(2,indj) = coef(2,indj) / 3600.0_PM_REEL
	   endif
	enddo 


!   	 Calcul des éléments orbitaux moyens adaptés CNES-BDL
!      		adapt(1) = demi grand axe (en unités astronomiques !!!!!!!)
!      		adapt(2) = longitude moyenne de la planète
!               	= (pom+gom+anom moyenne)
!      		adapt(3) = ex = e cos(pom+gom)
!      		adapt(4) = ey = e sin(pom+gom)
!      		adapt(5) = sin(incl/2) * cos(gom)
!      		adapt(6) = sin(incl/2) * cos(gom)

        do indi = 1 ,6
	   adapt(indi) = coef(indi,1) + &
                      tt*( coef(indi,2) + tt*( coef(indi,3) + &
                      tt*( coef(indi,4) + tt*( coef(indi,5) + &
                      tt*( coef(indi,6) + tt*coef(indi,7) )))))

	   if(indi.eq.2) then
              anmod = dmod(adapt(2),pm_deux_pi)
              if (anmod < 0.0_PM_REEL) then
                 anmod = anmod + pm_deux_pi
              endif
	      adapt(2) = anmod

	   endif

	end do

! Transformation du 1/2 grand axe en m

	adapt(1) = adapt(1) * ua

! Calcul des coordonnées cartésiennes

        ! Parametres adaptés pour mv_cir_equa_car

        orbcirequ%a     = adapt(1)
        orbcirequ%ex    = adapt(3)
        orbcirequ%ey    = adapt(4)
        orbcirequ%ix    = adapt(5) * 2._PM_REEL
        orbcirequ%iy    = adapt(6) * 2._PM_REEL
        orbcirequ%pso_M = adapt(2)

	call mv_cir_equa_car(xmus,orbcirequ,ephem(1:3),ephem(4:6),code_ret)

	if (code_ret%valeur /= 0) then
           call MSP_signaler_message (ier_mslib=code_ret)
           return
        endif

        ! Unités en km et km/s
        ephem(:) = ephem(:) / 1000._PM_REEL

        end subroutine eph_vsop82

end module eph_analytique
