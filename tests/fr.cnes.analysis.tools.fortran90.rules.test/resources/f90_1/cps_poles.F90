module cps_poles

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  cps_poles
!
!$Resume
!  Module permettant le calcul de l'ascension droite, de la déclinaison, du
!  temps sidéral et de sa dérivée pour les axes polaires des différentes
!  planètes et satellites du système solaire.
!
!$Description
!  Module permettant le calcul de l'ascension droite, de la déclinaison, du
!  temps sidéral et de sa dérivée pour les axes polaires des différentes
!  planètes et satellites du système solaire.
!
!$Auteur
!  Julien Bouillant (Atos Origin)
!
!$Version
!  $Id: cps_poles.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: cps_poles.F90,v $
!  Revision 1.22  2010/10/21 13:46:22  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.21  2008/08/04 13:46:21  gss
!  DM-ID 1058 : (portage g95) initialisation à NULL des pointeurs lors de leur
!  déclaration.
!
!  Revision 1.20  2007/05/21 06:59:46  vivaresf
!  DM-ID 538 : rajout d'un accès par code entier aux théories
!  avec interface pour conserver la compatibilité ascendante
!
!  Revision 1.19  2007/02/01 16:36:28  vivaresf
!  FA-ID 685 : correction de la gestion d'erreur,
!  correction des commentaires
!  utilisation de ncoefs au lieu de chiffres en dur pour les sommations de coefficients
!  Revision 1.18  2007/01/25 09:26:41  vivaresf
!  FA-ID 685 : initialisation des tableaux pour les cas maquants
!  Revision 1.17  2007/01/15 15:57:47  vivaresf
!  FA-ID 685 : pas d'erreur pour les corps de
!  mouvements linéarire
!  initialisation à zéro
!  sorties à zéro en cas d'erreur
!  Revision 1.16  2007/01/15 14:54:37  vivaresf
!  FA-ID 685 : rajout des appels à MSP_signaler_messages
!  et suppression du message pas de données pour hypérion
!  FA-ID 686 : 64.49 au lieu de 64.50 sur IO et 0.183 au lieu de 0.18 sur Janus
!  Revision 1.15  2006/08/30 08:52:01  vivaresf
!  FA-ID 576 : unités dans les commentaires
!  desallocation des variables
!  Revision 1.14  2006/05/30 15:16:52  vivaresf
!  Suppression des use mspro
!  Revision 1.13  2006/05/30 12:31:40  vivaresf
!  Suppression des use mspro internes
!  Revision 1.12  2006/05/30 12:29:11  vivaresf
!  Separation COMPAS_UI,
!  suppression MSPRO
!  robustesse (iostat sur les deallocate)
!  Revision 1.11  2006/05/30 08:16:37  vivaresf
!  DM-ID 387 : validation
!  Revision 1.10  2006/05/12 12:06:09  bouillaj
!  Amelioration qualite : complements sur les cartouches
!  Revision 1.9  2006/05/02 09:38:27  vpg
!  Suppression des variables non utilisees
!  Revision 1.8  2006/02/06 16:34:14  bouillaj
!  *** empty log message ***
!  Revision 1.7  2006/02/06 15:55:39  bouillaj
!  *** empty log message ***
!  Revision 1.6  2006/01/23 16:13:51  bouillaj
!  *** empty log message ***
!  Revision 1.5  2006/01/13 09:07:29  bouillaj
!  Relecture des coefficients et corrections
!  Revision 1.4  2006/01/10 16:42:31  bouillaj
!  Ajout des coefficients de la theorie UAI 91
!
!$FinHistorique
!
!$Usage
!  use cps_poles
!
!$Structure
!
!$Global
!
!>  cps_uai2000   : <integer,parameter>  code pour le modèles UAI2000
!>  cps_uai1991   : <integer,parameter>  code pour le modèle UAI1991
!
!$Common
!
!$Routines
!- cps_getMouvementsPolesCorps
!- cps_getMouvementsPolesCorps_str
!- cps_getMouvementsPolesCorps_int
!- cps_poletsid
!- cps_getcoefpoly
!- cps_calccoef
!- cps_poletsid91
!- cps_getcoefpoly91
!- cps_calccoef91
!
!$Fonctions
!
!$Include
!
!$Module
!#V
!- cps_utilisateur
!- eph_constantes
!#
!
!$Interface
!> cps_getmouvementspolescorps :  cps_getMouvementsPolesCorps_str, &, 
!                                 cps_getMouvementsPolesCorps_int
!#V
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!.  cps_getMouvementsPolesCorps cps_getMouvementsPolesCorps_str cps_getMouvementsPolesCorps_int
!.  cps_poletsid cps_getcoefpoly cps_calccoef cps_poletsid91 cps_getcoefpoly91 cps_calccoef91
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  use cps_utilisateur
  use eph_constantes, only : eph_date_jj50_j2000

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_poles.F90 69 2012-09-11 08:33:34Z ffsm $'


  integer, parameter :: cps_uai2000 = 2000
  integer, parameter :: cps_uai1991 = 1991

  interface cps_getMouvementsPolesCorps

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getMouvementsPolesCorps
!
!$Resume
!  Routine chapeau pour le calcul des modèles UAI
!
!$Description
!
!  Routine chapeau pour le calcul du temps sidéral, de sa dérivée et de 
!  l'orientation des axes des pôles des corps du système solaire 
!  suivants les modèles UAI. 2 modèles disponibles :
!  "UAI91" (code cps_uai1991) et "UAI2000" (code cps_uai2000).
! 
!>E  Les routines demandent en entrée :
!.  le code du corps,
!.  le code (entier : 1991 ou 2000) ou le nom de la théorie (UAI91 ou UAI2000),
!.  la date courante
!>S  et fournissent en sortie les éléments de la structure MSPRO (tm_pole_tsid) :
!.  alpha0/delta0 : (ra/dec) orientation du pôle, unité rad, repère EME2000
!.  W/dW  : temps sidéral et sa dérivée (rad et rad/s)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getMouvementsPolesCorps(code_corps, theorie, dateref, &
!.           alpha0, delta0, W, dW)
!.    integer :: code_corps
!.    character(LEN=*) :: theorie 
!.    type(TM_JOUR_SEC) :: dateref
!.    real(KIND=PM_REEL) :: alpha0, delta0, W, dW
!
!
!  call cps_getMouvementsPolesCorps(code_corps, theorie, dateref, &
!.           alpha0, delta0, W, dW)
!.    integer :: code_corps
!.    integer :: theorie 
!.    type(TM_JOUR_SEC) :: dateref
!.    real(KIND=PM_REEL) :: alpha0, delta0, W, dW
!
!$Procedures
!- cps_getMouvements_str
!- cps_getMouvements_int
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure cps_getMouvements_str, cps_getMouvements_int
  end interface

  private :: cps_getMouvements_str, cps_getMouvements_int

contains

  subroutine cps_getMouvements_str(code_corps, theorie, dateref, &
       alpha0, delta0, W, dW)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getMouvementsPolesCorps_str
!
!$Resume
!  Routine chapeau pour le calcul des coefficients pour un certain corps,
!  à une certaine date.
!
!$Description
!  Routine chapeau pour le calcul des coefficients pour un certain corps,
!  à une certaine date.
!
!$Auteur
!  Julien Bouillant (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getMouvementsPolesCorps_str(code_corps, theorie, dateref, &
!.           alpha0, delta0, W, dW)
!.    integer :: code_corps
!.    character(LEN=*) :: theorie 
!.    type(TM_JOUR_SEC) :: dateref
!.    real(KIND=PM_REEL) :: alpha0, delta0, W, dW
!
!$Arguments
!>E     code_corps  :<integer>       code du corps étudié
!>E     theorie     :<LEN=*>         théorie de calcul
!>E     dateref     :<TM_JOUR_SEC>   date de calcul (jj 1950)
!>S     alpha0      :<PM_REEL>       ascension droite (rad)
!>S     delta0      :<PM_REEL>       déclinaison (rad)
!>S     W           :<PM_REEL>       temps sidéral (rad)
!>S     dW          :<PM_REEL>       dérivée du temps sidéral (rad/s)
!
!$Common
!
!$Routines
!- cps_poletsid
!- cps_poletsid91
!- MSP_signaler_message
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

    integer, intent(in) :: code_corps
    character(LEN=*), intent(in) :: theorie   !! UAI2000 ou UAI91
    type(TM_JOUR_SEC), intent(in) :: dateref
    real(KIND=PM_REEL), intent(out) :: alpha0, delta0, W, dW

    ! Initialisations
    alpha0 = 0.0_PM_REEL
    delta0 = 0.0_PM_REEL
    W = 0.0_PM_REEL
    dW = 0.0_PM_REEL

    ! Calcul
    if (theorie.eq."UAI2000") then
       call cps_poletsid(code_corps, dateref, alpha0, delta0, W, dW)
    elseif (theorie.eq."UAI91") then
       call cps_poletsid91(code_corps, dateref, alpha0, delta0, W, dW)
    else
       ! erreur : theorie indisponible
       call MSP_signaler_message(cle_mes="CPS_ERR_THEORIE", &
            routine="cps_getMouvementsPolesCorps", &
            partie_variable=trim(theorie))
       return
    end if
    if (MSP_gen_messages("cps_getMouvementsPolesCorps")) return
        
    ! Conversion en radian
    alpha0 = alpha0*PM_DEG_RAD
    delta0 = delta0*PM_DEG_RAD
    W = W*PM_DEG_RAD
    dW = dW*PM_DEG_RAD
    
    ! dW est en rad/jour : passer en rad/s
    dW = dW/86400_pm_reel
    
  end subroutine cps_getMouvements_str

  subroutine cps_getMouvements_int(code_corps, theorie, dateref, &
       alpha0, delta0, W, dW)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getMouvementsPolesCorps_int
!
!$Resume
!  Routine chapeau pour le calcul des coefficients pour un certain corps,
!  à une certaine date.
!
!$Description
!  Routine chapeau pour le calcul des coefficients pour un certain corps,
!  à une certaine date.
!
!$Auteur
!  Julien Bouillant (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getMouvementsPolesCorps_int(code_corps, theorie, dateref, &
!.           alpha0, delta0, W, dW)
!.    integer :: code_corps
!.    integer :: theorie 
!.    type(TM_JOUR_SEC) :: dateref
!.    real(KIND=PM_REEL) :: alpha0, delta0, W, dW
!
!$Arguments
!>E     code_corps  :<integer>       code du corps étudié
!>E     theorie     :<integer>       code théorie de calcul
!>E     dateref     :<TM_JOUR_SEC>   date de calcul (jj 1950)
!>S     alpha0      :<PM_REEL>       ascension droite (rad)
!>S     delta0      :<PM_REEL>       déclinaison (rad)
!>S     W           :<PM_REEL>       temps sidéral (rad)
!>S     dW          :<PM_REEL>       dérivée du temps sidéral (rad/s)
!
!$Common
!
!$Routines
!- cps_poletsid
!- cps_poletsid91
!- MSP_signaler_message
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

    integer, intent(in) :: code_corps
    integer, intent(in) :: theorie   !! UAI2000 ou UAI91
    type(TM_JOUR_SEC), intent(in) :: dateref
    real(KIND=PM_REEL), intent(out) :: alpha0, delta0, W, dW

    ! Variables locales
    character(LEN=80) :: mess

    ! Initialisations
    alpha0 = 0.0_PM_REEL
    delta0 = 0.0_PM_REEL
    W = 0.0_PM_REEL
    dW = 0.0_PM_REEL

    ! Calcul
    if (theorie == cps_uai2000) then
       call cps_poletsid(code_corps, dateref, alpha0, delta0, W, dW)
    elseif (theorie == cps_uai1991) then
       call cps_poletsid91(code_corps, dateref, alpha0, delta0, W, dW)
    else
       ! erreur : theorie indisponible
       write(mess, *) "code :", theorie
       call MSP_signaler_message(cle_mes="CPS_ERR_THEORIE", &
            routine="cps_getMouvementsPolesCorps", &
            partie_variable=trim(mess))
       return
    end if
    if (MSP_gen_messages("cps_getMouvementsPolesCorps")) return
        
    ! Conversion en radian
    alpha0 = alpha0*PM_DEG_RAD
    delta0 = delta0*PM_DEG_RAD
    W = W*PM_DEG_RAD
    dW = dW*PM_DEG_RAD
    
    ! dW est en rad/jour : passer en rad/s
    dW = dW/86400_pm_reel
    
  end subroutine cps_getMouvements_int

  subroutine cps_poletsid(numpla, dateref, alpha0, delta0, W, dW)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_poletsid
!
!$Resume
!  Calcul effectif du "pole_tsid" dans la théorie UAI 2000
!
!$Description
!  Calcul effectif de l'ascension droite, de la déclinaison, du temps 
!  sidéral et de sa dérivée pour les axes du pôle d'un corps du
!  système solaire dans la théorie UAI 2000
!
!$Auteur
!  Julien Bouillant (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_poletsid(numpla, dateref, alpha0, delta0, W, dW)
!.    integer :: numpla
!.    type(tm_jour_sec) :: dateref 
!.    real(kind=PM_REEL) :: alpha0
!.    real(kind=PM_REEL) :: delta0
!.    real(kind=PM_REEL) :: W
!.    real(kind=PM_REEL) :: dW
!
!$Arguments
!>E     numpla   :<integer>       code de la planète central du corps étudié
!>E     dateref  :<tm_jour_sec>   date de calcul (jj 1950)
!>S     alpha0   :<PM_REEL>       ascension droite (deg)
!>S     delta0   :<PM_REEL>       déclinaison (deg)
!>S     W        :<PM_REEL>       temps sidéral (deg)
!>S     dW       :<PM_REEL>       dérivée du temps sidéral (deg/j)
!
!$Common
!
!$Routines
!- md_joursec_jourfrac
!- cps_getcoefpoly
!- MSP_signaler_message
!- cps_calccoef
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

    !Arguments
    integer           , intent(IN) :: numpla
    type(tm_jour_sec) , intent(IN) :: dateref  !! DATEREF DOIT ETRE DONNEE EN JJ1950
    real(kind=PM_REEL), intent(OUT):: alpha0
    real(kind=PM_REEL), intent(OUT):: delta0
    real(kind=PM_REEL), intent(OUT):: W
    real(kind=PM_REEL), intent(OUT):: dW

    ! Variables locales
    real(kind=PM_REEL) :: d, T
    real(kind=PM_REEL) :: ca0, ca1, ca2
    real(kind=PM_REEL) :: cd0, cd1, cd2
    real(kind=PM_REEL) :: cW0, cW1, cW2

    real(kind=PM_REEL), dimension(:), pointer :: coefs
    real(kind=PM_REEL), dimension(:), pointer :: fcoefa => NULL(), fcoefd => NULL()
    real(kind=PM_REEL), dimension(:), pointer :: fcoefW => NULL(), fcoefdW => NULL()
    real(kind=PM_REEL), dimension(:), pointer :: coefsd1
    integer :: ncoefs, codebary, ii, iostat

    real(kind=PM_REEL) :: date
    type(tm_code_retour) :: code
    real(kind=PM_REEL) :: siecle = 36525_PM_REEL

    ! traitement d'erreur
    character(len=30), dimension(2) :: errtab

    ! Calcul des parametres de changement de repere
    !----------------------------------------------
    ! (mouvement du pole, temps sideral) 

    ! dates J50 en jours / siecles J2000
    call md_joursec_jourfrac(dateref, date, code)
    d=date-eph_date_jj50_j2000
    T = d / siecle


    !! cas linéaire (planetes sauf Neptune)
    call cps_getcoefpoly(numpla,ca0, ca1, cd0, cd1,cW0, cW1,ca2,cd2, cW2)
    if (MSP_gen_messages("cps_getMouvementsPolesCorps")) return

    alpha0 = ca0+ ca1*T 
    delta0 = cd0+ cd1*T 
    W = cW0+ cW1*d 
    dW = cW1

    codebary = numpla/100   ! barycentre : les coefs E/M/J/S/U/N sont par planète

    ! pas de coefs E/M/J/S/U/N nécessaire
    if (mod(numpla,100) == 99.and. numpla /= 899) return
    
    !! si coefs E/M/J/S/U/N 
    if (numpla == 899) codebary=numpla ! traite Neptune avec un coefficent N différent du
                                       ! N pour ses satellites

    ! nombre de coefficients en fonctions du la planete de reference
    ! pour les tableaux dynamiques en sortie de cps_calccoef
    ncoefs=0
    select case (codebary)
    case(1,2)
       ncoefs=0
    case(3) 
       ncoefs=13 
    case(4) 
       ncoefs=3
    case(5) 
       ncoefs=8
    case(6) 
       ncoefs=7
    case(7) 
       ncoefs=16
    case(8) 
       ncoefs=9 ! Il faut rajouter un coefficient supplémentaire par rapport aux 8 existant pour 
                ! pouvoir écrire les formules des coefficients de Triton.
    case(9) 
       ncoefs=0
    case(899) 
       ncoefs=1
    case default
       alpha0 = 0.0_PM_REEL
       delta0 = 0.0_PM_REEL
       W = 0.0_PM_REEL
       dW = 0.0_PM_REEL

       ! Cas d'erreur
       write(errtab(1),*) "de barycentre ",  codebary
       errtab(2) =  "UAI2000"
       call MSP_signaler_message(cle_mes="CPS_ERR_CORPSNONDEC", &
            routine="cps_poletsid", &
            partie_variable=errtab)
       return
    end select

    ! pas de coefs E/M/J/S/U/N nécessaire
    if (ncoefs == 0) return

    if (associated(coefs)) deallocate(coefs, stat=iostat)
    allocate(coefs(ncoefs))
    if (associated(coefsd1)) deallocate(coefsd1, stat=iostat)
    allocate(coefsd1(ncoefs))
    if (ncoefs>0) then
       coefs(1:ncoefs) = 0._PM_REEL
       coefsd1(1:ncoefs) = 0._PM_REEL       
    endif

    ! donne les coefficients permettant de calculer E/M/J/S/U/N 
    call cps_calccoef(codebary, d, T, ncoefs, coefs, coefsd1)
    if (MSP_gen_messages("cps_getMouvementsPolesCorps")) goto 9999

    ! Facteurs applicables au coefficients coef. E/M/J/S/U/N
    if (ncoefs>0) then
       if (associated(fcoefa))  deallocate(fcoefa, stat=iostat)
       if (associated(fcoefd))  deallocate(fcoefd, stat=iostat)
       if (associated(fcoefW))  deallocate(fcoefW, stat=iostat)
       if (associated(fcoefdW)) deallocate(fcoefdW, stat=iostat)
       allocate(fcoefa(ncoefs))
       allocate(fcoefd(ncoefs))
       allocate(fcoefW(ncoefs))
       allocate(fcoefdW(ncoefs))
       fcoefa(1:ncoefs) = 0._PM_REEL
       fcoefd(1:ncoefs) = 0._PM_REEL
       fcoefW(1:ncoefs) = 0._PM_REEL
       fcoefdW(1:ncoefs) = 0._PM_REEL
    endif

    select case (numpla)
    ! Lune
    case(301)
       fcoefa(1) = -3.8787_PM_REEL
       fcoefa(2) = -0.1204_PM_REEL
       fcoefa(3) = 0.0700_PM_REEL
       fcoefa(4) = -0.0172_PM_REEL 
       fcoefa(6) = 0.0072_PM_REEL
       fcoefa(10) = -0.0052_PM_REEL
       fcoefa(13) = 0.0043_PM_REEL
       
       fcoefd(1) = 1.5419_PM_REEL
       fcoefd(2) = 0.0239_PM_REEL
       fcoefd(3) = -0.0278_PM_REEL 
       fcoefd(4) = 0.0068_PM_REEL 
       fcoefd(6) = -0.0029_PM_REEL 
       fcoefd(7) = 0.0009_PM_REEL 
       fcoefd(10) = 0.0008_PM_REEL
       fcoefd(13) = -0.0009_PM_REEL

       fcoefW(1) = 3.5610_PM_REEL 
       fcoefW(2) = 0.1208_PM_REEL 
       fcoefW(3) = -0.0642_PM_REEL
       fcoefW(4) = 0.0158_PM_REEL 
       fcoefW(5) = 0.0252_PM_REEL 
       fcoefW(6) = -0.0066_PM_REEL 
       fcoefW(7) = -0.0047_PM_REEL
       fcoefW(8) = -0.0046_PM_REEL 
       fcoefW(9) = 0.0028_PM_REEL 
       fcoefW(10) = 0.0052_PM_REEL 
       fcoefW(11) = 0.0040_PM_REEL
       fcoefW(12) = 0.0019_PM_REEL 
       fcoefW(13) = -0.0044_PM_REEL 

       do ii=1, ncoefs
          fcoefdW(ii) = fcoefW(ii) * coefsd1(ii) 
       enddo
    ! Phobos
    case(401)
       fcoefa(1) = 1.79_PM_REEL
       fcoefd(1) = -1.08_PM_REEL
       fcoefW(1) = -1.42_PM_REEL
       fcoefW(2) = -0.78_PM_REEL

       ! attention a la derivée : temps en siecle
       fcoefdW(1) = 1.42_PM_REEL*0.4357640_PM_REEL
       fcoefdW(2) = (8.864_PM_REEL/siecle**2_PM_REEL)*2_PM_REEL
       fcoefdW(2) = -0.78_PM_REEL*(1128.4096700_PM_REEL+fcoefdW(2)*d)
    ! Deimos
    case(402)
       fcoefa(1) = 2.98_PM_REEL
       fcoefd(1) = -1.78_PM_REEL
       fcoefW(1) = -2.58_PM_REEL
       fcoefW(2) = 0.19_PM_REEL
       fcoefdW(1) = 2.58_PM_REEL*0.0181510_PM_REEL
       fcoefdW(2) = - 0.19_PM_REEL*0.0181510_PM_REEL
    ! Z! Io (Jupiter)
    case(501)
       fcoefa(3) = 0.094_PM_REEL
       fcoefa(4) = 0.024_PM_REEL
       fcoefd(3) = 0.040_PM_REEL
       fcoefd(4) = 0.011_PM_REEL
       fcoefW(3) = -0.085_PM_REEL
       fcoefW(4) = -0.022_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(3) = -0.085_PM_REEL*4850.7_PM_REEL/ siecle
       fcoefdW(4) = -0.022_PM_REEL*1191.3_PM_REEL/ siecle
    ! Europa (Jupiter)
    case(502)
       fcoefa(4) = 1.086_PM_REEL
       fcoefa(5) = 0.060_PM_REEL
       fcoefa(6) = 0.015_PM_REEL
       fcoefa(7) = 0.009_PM_REEL
       fcoefd(4) = 0.468_PM_REEL
       fcoefd(5) = 0.026_PM_REEL
       fcoefd(6) = 0.007_PM_REEL
       fcoefd(7) = 0.002_PM_REEL
       fcoefW(4) = -0.980_PM_REEL
       fcoefW(5) = -0.054_PM_REEL
       fcoefW(6) = -0.014_PM_REEL
       fcoefW(7) = -0.008_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(4) = -0.980_PM_REEL*1191.3_PM_REEL / siecle
       fcoefdW(5) = -0.054_PM_REEL*262.1_PM_REEL / siecle
       fcoefdW(6) = -0.014_PM_REEL*64.3_PM_REEL / siecle
       fcoefdW(7) = -0.008_PM_REEL*2382.6_PM_REEL / siecle
    ! Ganymede (Jupiter)
    case(503)
       fcoefa(4) = -0.037_PM_REEL
       fcoefa(5) = 0.431_PM_REEL
       fcoefa(6) = 0.091_PM_REEL
       fcoefd(4) = -0.016_PM_REEL
       fcoefd(5) = 0.186_PM_REEL
       fcoefd(6) = 0.039_PM_REEL
       fcoefW(4) = 0.033_PM_REEL
       fcoefW(5) = -0.389_PM_REEL
       fcoefW(6) = -0.082_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(4) = 0.033_PM_REEL*1191.3_PM_REEL/ siecle
       fcoefdW(5) = -0.389_PM_REEL*262.1_PM_REEL/ siecle
       fcoefdW(6) = -0.082_PM_REEL*64.3_PM_REEL / siecle
    ! Callisto (Jupiter)
    case(504)
       fcoefa(5) = -0.068_PM_REEL
       fcoefa(6) = 0.590_PM_REEL
       fcoefa(8) = 0.010_PM_REEL
       fcoefd(5) = -0.029_PM_REEL
       fcoefd(6) = 0.254_PM_REEL
       fcoefd(8) = -0.004_PM_REEL
       fcoefW(5) = 0.061_PM_REEL
       fcoefW(6) = -0.533_PM_REEL
       fcoefW(8) = -0.009_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(5) = 0.061_PM_REEL*262.1_PM_REEL / siecle
       fcoefdW(6) = -0.533_PM_REEL*64.3_PM_REEL/ siecle
       fcoefdW(8) = -0.009_PM_REEL*6070.0_PM_REEL / siecle
    ! Amalthea (Jupiter)
    case(505)
       fcoefa(1) = -0.84_PM_REEL
       fcoefa(2) = 0.01_PM_REEL
       fcoefd(1) = -0.36_PM_REEL
       fcoefW(1) = 0.76_PM_REEL
       fcoefW(2) = -0.01_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(1) = 0.76_PM_REEL*91472.9_PM_REEL / siecle
       fcoefdW(2) = -0.01_PM_REEL*91472.9_PM_REEL*2_PM_REEL / siecle
    ! Thebe (Jupiter)
    case(514)
       fcoefa(2) = -2.11_PM_REEL
       fcoefa(3) = 0.04_PM_REEL
       fcoefd(2) = -0.91_PM_REEL
       fcoefd(3) = 0.01_PM_REEL
       fcoefW(2) = 1.91_PM_REEL
       fcoefW(3) = -0.04_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(2) = 1.91_PM_REEL*45137.2_PM_REEL / siecle
       fcoefdW(3) = -0.04_PM_REEL*45137.2_PM_REEL*2_PM_REEL / siecle
    ! Mimas (Saturne)
    case(601)
       fcoefa(3) = 13.56_PM_REEL
       fcoefd(3) = -1.53_PM_REEL
       fcoefW(3) = -13.48_PM_REEL
       fcoefW(5) = -44.85_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(3) = -13.48_PM_REEL*(-36505.5_PM_REEL) / siecle
       fcoefdW(5) = -44.85_PM_REEL*506.2_PM_REEL / siecle
    ! Tethys (Saturne)
    case(603)
       fcoefa(4) = 9.66_PM_REEL
       fcoefd(4) = -1.09_PM_REEL
       fcoefW(4) = -9.60_PM_REEL
       fcoefW(5) = 2.23_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(4) = -9.60_PM_REEL*(-7225.9_PM_REEL) / siecle
       fcoefdW(5) = 2.23_PM_REEL*506.2_PM_REEL / siecle
    ! Reha (Saturne)
    case(605)
       fcoefa(6) = 3.10_PM_REEL
       fcoefd(6) = -0.35_PM_REEL
       fcoefW(6) = -3.08_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(6) = -3.08_PM_REEL*(-1016.3_PM_REEL) / siecle
    ! Titan(Saturne)
    case(606)
       fcoefa(7) = 2.66_PM_REEL
       fcoefd(7) = -0.30_PM_REEL
       fcoefW(7) = -2.64_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(7) = -2.64_PM_REEL*(-52.1_PM_REEL) / siecle
    ! Z! Janus (Saturne)
    case(610)
       fcoefa(2) = -1.623_PM_REEL
       fcoefa(3) = 0.023_PM_REEL
       fcoefd(2) = -0.183_PM_REEL
       fcoefd(3) = 0.001_PM_REEL
       fcoefW(2) = 1.613_PM_REEL
       fcoefW(3) = -0.023_PM_REEL
       ! attention à la derivée : temps en siecle
       fcoefdW(2) = 1.613_PM_REEL*75706.7_PM_REEL/ siecle
       fcoefdW(3) = -0.023_PM_REEL*75706.7_PM_REEL*2_PM_REEL/ siecle
    ! Epimethe (Saturne)
    case(611)
       fcoefa(1) = -3.153_PM_REEL
       fcoefa(2) = 0.086_PM_REEL
       fcoefd(1) = -0.356_PM_REEL
       fcoefd(2) = 0.005_PM_REEL
       fcoefW(1) = 3.133_PM_REEL
       fcoefW(2) = -0.086_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(1) = 3.133_PM_REEL*75706.7_PM_REEL / siecle
       fcoefdW(2) = -0.0086_PM_REEL*75706.7_PM_REEL*2_PM_REEL / siecle
    ! Ariel (Uranus)
    case(701)
       fcoefa(13) = 0.29_PM_REEL
       fcoefd(13) = 0.28_PM_REEL
       fcoefW(12) = 0.05_PM_REEL
       fcoefW(13) = 0.08_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(12) = 0.05_PM_REEL * 2863.96_PM_REEL / siecle
       fcoefdW(13) = 0.08_PM_REEL * (-51.94_PM_REEL) / siecle
    ! Umbriel (Uranus)
    case(702)
       fcoefa(14) = 0.21_PM_REEL
       fcoefd(14) = 0.20_PM_REEL
       fcoefW(12) = -0.09_PM_REEL
       fcoefW(14) = 0.06_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(12) = -0.09_PM_REEL * 2863.96_PM_REEL / siecle
       fcoefdW(14) = 0.06_PM_REEL * (-75.32_PM_REEL) / siecle
    ! Titania (Uranus)
    case(703)
       fcoefa(15) = 0.29_PM_REEL
       fcoefd(15) = 0.28_PM_REEL
       fcoefW(15) = 0.08_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(15) = 0.08_PM_REEL*(-75.32_PM_REEL) / siecle
    ! Oberon  (Uranus)
    case(704)
       fcoefa(16) = 0.16_PM_REEL
       fcoefd(16) = 0.16_PM_REEL
       fcoefW(16) = 0.04_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(16) = 0.04_PM_REEL*(-504.81_PM_REEL) / siecle
    ! Miranda  (Uranus)
    case(705)
       fcoefa(11) = 4.41_PM_REEL
       fcoefa(12) = -0.04_PM_REEL
       fcoefd(11) = 4.25_PM_REEL
       fcoefd(12) = -0.02_PM_REEL
       fcoefW(10) = -0.09_PM_REEL
       fcoefW(11) = 1.15_PM_REEL
       fcoefW(12) = -1.27_PM_REEL
       fcoefW(13) = 0.15_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(10) = -0.09_PM_REEL * (-2024.22_PM_REEL) / siecle
       fcoefdW(11) = 1.15_PM_REEL * 2_PM_REEL *(-2024.22_PM_REEL) / siecle
       fcoefdW(12) = -1.27_PM_REEL * 2863.96_PM_REEL / siecle
       fcoefdW(13) = 0.15_PM_REEL * 2_PM_REEL * 2863.96_PM_REEL / siecle
    ! Cordelia (Uramus)
    case(706)
       fcoefa(1) = -0.15_PM_REEL
       fcoefd(1) = 0.14_PM_REEL
       fcoefW(1) = -0.04_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(1) = -0.04_PM_REEL * 54991.87_PM_REEL / siecle
    ! Ophelia (Uramus)
    case(707)
       fcoefa(2) = -0.09_PM_REEL
       fcoefd(2) = 0.09_PM_REEL
       fcoefW(2) = -0.03_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(2) = -0.03_PM_REEL * 41877.66_PM_REEL / siecle
    ! Bianca (Uramus)
    case(708)
       fcoefa(3) = -0.16_PM_REEL
       fcoefd(3) = 0.16_PM_REEL
       fcoefW(3) = -0.04_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(3) = -0.04_PM_REEL * 29927.35_PM_REEL / siecle
    ! Cressida (Uramus)
    case(709)
       fcoefa(4) = -0.04_PM_REEL
       fcoefd(4) = 0.04_PM_REEL
       fcoefW(4) = -0.01_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(4) = -0.01_PM_REEL * 25733.59_PM_REEL / siecle
    ! Desdemona (Uranus)
    case(710)
       fcoefa(5) = -0.17_PM_REEL
       fcoefd(5) = 0.16_PM_REEL
       fcoefW(5) = -0.04_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(5) = -0.04_PM_REEL * 24471.46_PM_REEL / siecle
    ! Juliet (Uranus)
    case(711)
       fcoefa(6) = -0.06_PM_REEL
       fcoefd(6) = 0.06_PM_REEL
       fcoefW(6) = -0.02_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(6) = -0.02_PM_REEL * 22278.41_PM_REEL / siecle
    ! Portia (Uranus)
    case(712)
       fcoefa(7) = -0.09_PM_REEL
       fcoefd(7) = 0.09_PM_REEL
       fcoefW(7) = -0.02_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(7) = -0.02_PM_REEL * 20289.42_PM_REEL / siecle
    ! Rosalind (Uranus)
    case(713)
       fcoefa(8) = -0.29_PM_REEL
       fcoefd(8) = 0.28_PM_REEL
       fcoefW(8) = -0.08_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(8) = -0.08_PM_REEL * 16652.76_PM_REEL / siecle
    ! Belinda (Uranus)
    case(714)
       fcoefa(9) = -0.03_PM_REEL
       fcoefd(9) = 0.03_PM_REEL
       fcoefW(9) = -0.01_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(9) = -0.01_PM_REEL * 12872.63_PM_REEL / siecle
    ! Puck (Uranus)
    case(715)
       fcoefa(10) = -0.33_PM_REEL
       fcoefd(10) = 0.031_PM_REEL
       fcoefW(10) = -0.09_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(10) = -0.09_PM_REEL * 8061.81_PM_REEL / siecle
    ! Neptune
    case(899)
       fcoefa(1) = 0.70_PM_REEL
       fcoefd(1) = -0.51_PM_REEL
       fcoefW(1) = -0.48_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(1) = -0.48_PM_REEL * 52.316_PM_REEL / siecle
    ! Triton (Neptune)
    case(801) 
       fcoefa(1) = -32.35_PM_REEL
       fcoefa(2) = -6.28_PM_REEL
       fcoefa(3) = -2.08_PM_REEL
       fcoefa(4) = -0.74_PM_REEL
       fcoefa(5) = -0.28_PM_REEL
       fcoefa(6) = -0.11_PM_REEL
       fcoefa(7) = -0.07_PM_REEL
       fcoefa(8) = -0.02_PM_REEL
       fcoefa(9) = -0.01_PM_REEL

       fcoefd(1) = 22.55_PM_REEL
       fcoefd(2) = 2.10_PM_REEL
       fcoefd(3) = 0.55_PM_REEL
       fcoefd(4) = 0.16_PM_REEL
       fcoefd(5) = 0.05_PM_REEL
       fcoefd(6) = 0.02_PM_REEL
       fcoefd(7) = 0.01_PM_REEL

       fcoefW(1) = 22.25_PM_REEL
       fcoefW(2) = 6.73_PM_REEL
       fcoefW(3) = 2.05_PM_REEL
       fcoefW(4) = 0.74_PM_REEL
       fcoefW(5) = 0.28_PM_REEL
       fcoefW(6) = 0.11_PM_REEL
       fcoefW(7) = 0.05_PM_REEL
       fcoefW(8) = 0.02_PM_REEL
       fcoefW(9) = 0.01_PM_REEL

       ! attention a la derivée : temps en siecle
       fcoefdW(1) = 22.25_PM_REEL * 52.316_PM_REEL / siecle
       fcoefdW(2) = 6.73_PM_REEL * 52.316_PM_REEL * 2_PM_REEL / siecle 
       fcoefdW(3) = 2.05_PM_REEL * 52.316_PM_REEL * 3_PM_REEL / siecle
       fcoefdW(4) = 0.74_PM_REEL * 52.316_PM_REEL * 4_PM_REEL / siecle
       fcoefdW(5) = 0.28_PM_REEL * 52.316_PM_REEL * 5_PM_REEL / siecle
       fcoefdW(6) = 0.11_PM_REEL * 52.316_PM_REEL * 6_PM_REEL / siecle
       fcoefdW(7) = 0.05_PM_REEL * 52.316_PM_REEL * 7_PM_REEL / siecle
       fcoefdW(8) = 0.02_PM_REEL * 52.316_PM_REEL * 8_PM_REEL / siecle
       fcoefdW(9) = 0.01_PM_REEL * 52.316_PM_REEL * 9_PM_REEL / siecle 
    ! Naiad (Neptune)
    case(803)
       fcoefa(8) = 0.70_PM_REEL
       fcoefa(1) = -6.49_PM_REEL
       fcoefa(2) = 0.25_PM_REEL
       fcoefd(8) = -0.51_PM_REEL
       fcoefd(1) = -4.75_PM_REEL
       fcoefd(2) = 0.09_PM_REEL
       fcoefW(8) = -0.48_PM_REEL
       fcoefW(1) = 4.40_PM_REEL
       fcoefW(2) = -0.27_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(8) = -0.48_PM_REEL * 52.316_PM_REEL / siecle
       fcoefdW(1) = 4.40_PM_REEL * 62606.6_PM_REEL / siecle
       fcoefdW(2) = -0.27_PM_REEL * 55064.2_PM_REEL * 2_PM_REEL / siecle
    ! Thalassa (Neptune)
    case(804)
       fcoefa(8) = 0.70_PM_REEL
       fcoefa(2) = -0.28_PM_REEL
       fcoefd(8) = -0.51_PM_REEL
       fcoefd(2) = -0.21_PM_REEL
       fcoefW(8) = -0.48_PM_REEL
       fcoefW(2) = 0.19_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(8) = -0.48_PM_REEL * 52.316_PM_REEL / siecle
       fcoefdW(2) = 0.19_PM_REEL * 55064.2_PM_REEL / siecle
    ! Despina (Neptune)
    case(805)
       fcoefa(8) = 0.7_PM_REEL
       fcoefa(3) = -0.09_PM_REEL
       fcoefd(8) = -0.51_PM_REEL
       fcoefd(3) = -0.07_PM_REEL
       fcoefW(8) = -0.49_PM_REEL
       fcoefW(3) =0.06_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(8) = -0.49_PM_REEL * 52.316_PM_REEL / siecle
       fcoefdW(3) = 0.06_PM_REEL * 46564.5_PM_REEL / siecle
    ! Galathea (Neptune)
    case(806)
       fcoefa(8) = 0.7_PM_REEL
       fcoefa(4) = -0.07_PM_REEL
       fcoefd(8) = -0.51_PM_REEL
       fcoefd(4) = -0.05_PM_REEL
       fcoefW(8) = -0.48_PM_REEL
       fcoefW(4) = 0.05_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(8) = -0.48_PM_REEL * 52.316_PM_REEL / siecle
       fcoefdW(4) = 0.05_PM_REEL * 26109.4_PM_REEL / siecle
    ! Larissa (Neptune)
    case(807)
       fcoefa(8) = 0.70_PM_REEL
       fcoefa(5) = -0.27_PM_REEL
       fcoefd(8) = -0.51_PM_REEL
       fcoefd(5) = -0.20_PM_REEL
       fcoefW(8) = -0.48_PM_REEL
       fcoefW(5) =0.19_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(8) = -0.48_PM_REEL * 52.316_PM_REEL / siecle
       fcoefdW(5) = 0.19_PM_REEL * 14325.4_PM_REEL / siecle
   ! Proteus (Neptune)
    case(808)
       fcoefa(8) = 0.70_PM_REEL
       fcoefa(6) = -0.05_PM_REEL
       fcoefd(8) = -0.51_PM_REEL
       fcoefd(6) = -0.05_PM_REEL
       fcoefW(8) = -0.48_PM_REEL
       fcoefW(6) = 0.04_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(8) = -0.48_PM_REEL * 52.316_PM_REEL / siecle
       fcoefdW(6) = 0.04_PM_REEL * 2824.6_PM_REEL / siecle

    ! rien pour les planètes sauf Neptune
    case(199,299,399,499,599,699,799,999)

    ! sans oublier les satellites dont le mouvement est linéaire
    case (515,516,602,604,608,609,612,613,614,615,616,617,618,901)

    case default
       write(errtab(1),*) numpla
       errtab(2) =  "UAI2000"
       call MSP_signaler_message(cle_mes="CPS_ERR_CORPSNONDEC", &
            routine="cps_poletsid", &
            partie_variable=errtab)
       alpha0 = 0.0_PM_REEL
       delta0 = 0.0_PM_REEL
       W = 0.0_PM_REEL
       dW = 0.0_PM_REEL
       goto 9999
    end select

    ! Calcul des parametres de changement de repère pour les lunes des planètes
    ! (mouvement du pole + Temps sidéral et sa dérivée)
    ! - coefa contient les facteurs multiplicatifs pour alpha
    ! - coefd contient les facteurs multiplicatifs pour delta
    ! - coefW contient les facteurs multiplicatifs pour W
    ! - coefdW contient les facteurs multiplicatifs pour dW
    ! - coefs contient les E/M/J/S/U/N

    ! les formules sont en fonction des corps, mais quelques
    ! factorisation possibles

    select case (numpla)
    case(301)
    ! Lune
       do ii=1,ncoefs
          alpha0=alpha0 + fcoefa(ii)*sin(coefs(ii))
          delta0=delta0 + fcoefd(ii)*cos(coefs(ii))
       enddo
       
       W = W + cW2*d**2_PM_REEL
       dW = dW + 22_PM_REEL*cW2*d
       do ii=1,13
          W = W + fcoefW(ii)*sin(coefs(ii)) 
          dW = dW + fcoefdW(ii)*cos(coefs(ii))
       enddo

    ! Phobos
    case(401)
       alpha0=alpha0 + fcoefa(1)*sin(coefs(1))

       delta0=delta0 + fcoefd(1)*cos(coefs(1))

       W = W + cW2*T**2_PM_REEL
       W = W + fcoefW(1)*sin(coefs(1)) 
       W = W + fcoefW(2)*sin(coefs(2)) 

       dW = dW + 2_PM_REEL*cW2*T
       dW = dW + fcoefdW(1)*cos(coefs(1))
       dW = dW + fcoefdW(2)*cos(coefs(2))

    ! Deimos
    case(402)
       alpha0=alpha0 + fcoefa(1)*sin(coefs(3))
       delta0=delta0 + fcoefd(1)*cos(coefs(3))

       W = W + cW2*T**2_PM_REEL
       W = W + fcoefW(1)*sin(coefs(3))
       W = W + fcoefW(2)*cos(coefs(3))

       dW = dW + 2_PM_REEL*cW2*T
       dW = dW + fcoefdW(1)*cos(coefs(3))
       dW = dW + fcoefdW(2)*sin(coefs(3))

    ! Amalthea (Jupiter)
    case(505)
       alpha0 = alpha0 + fcoefa(1)*sin(coefs(1))
       alpha0 = alpha0 + fcoefa(2)*sin(2*coefs(1))
       delta0 = delta0 + fcoefd(1)*cos(coefs(1))
       W = W + fcoefW(1)*sin(coefs(1))
       W = W + fcoefW(2)*sin(2*coefs(1))
       dW = dW + fcoefdW(1)*cos(coefs(1))
       dW = dW + fcoefdW(2)*cos(2*coefs(1))
    ! Thebe (Jupiter)
    case(514)
       alpha0 = alpha0 + fcoefa(2)*sin(coefs(2))
       alpha0 = alpha0 + fcoefa(3)*sin(2*coefs(2))
       delta0 = delta0 + fcoefd(2)*cos(coefs(2))
       delta0 = delta0 + fcoefd(3)*cos(2*coefs(2))
       W = W + fcoefW(2)*sin(coefs(2))
       W = W + fcoefW(3)*sin(2*coefs(2))
       dW = dW + fcoefdW(2)*cos(coefs(2))
       dW = dW + fcoefdW(3)*cos(2*coefs(2))
    ! Janus (Saturne)
    case(610)
       alpha0 = alpha0 + fcoefa(2)*sin(coefs(2))
       alpha0 = alpha0 + fcoefa(3)*sin(2*coefs(2))
       delta0 = delta0 + fcoefd(2)*cos(coefs(2))
       delta0 = delta0 + fcoefd(3)*cos(2*coefs(2))
       W = W + fcoefW(2)*sin(coefs(2))
       W = W + fcoefW(3)*sin(2*coefs(2))
       dW = dW + fcoefdW(2)*cos(coefs(2))
       dW = dW + fcoefdW(3)*cos(2*coefs(2))

    ! Epimethe (Saturne)
    case(611)
       alpha0 = alpha0 + fcoefa(1)*sin(coefs(1))
       alpha0 = alpha0 + fcoefa(2)*sin(2*coefs(1))
       delta0 = delta0 + fcoefd(1)*cos(coefs(1))
       delta0 = delta0 + fcoefd(2)*cos(2*coefs(1))
       W = W + fcoefW(1)*sin(coefs(1))
       W = W + fcoefW(2)*sin(2*coefs(1))
       dW = dW + fcoefdW(1)*cos(coefs(1))
       dW = dW + fcoefdW(2)*cos(2*coefs(1))

    ! Miranda  (Uranus)
    case(705)
       alpha0 = alpha0 + fcoefa(11)*sin(coefs(11))
       alpha0 = alpha0 + fcoefa(12)*sin(2*coefs(11))
       delta0 = delta0 + fcoefd(11)*cos(coefs(11))
       delta0 = delta0 + fcoefd(12)*cos(2*coefs(11))

       W = W + fcoefW(10)*sin(coefs(11))
       W = W + fcoefW(11)*sin(2*coefs(11))
       W = W + fcoefW(12)*sin(coefs(12))
       W = W + fcoefW(13)*sin(2*coefs(12))

       dW = dW + fcoefW(10)*cos(coefs(11))
       dW = dW + fcoefW(11)*cos(2*coefs(11))
       dW = dW + fcoefW(12)*cos(coefs(12))
       dW = dW + fcoefW(13)*cos(2*coefs(12))
    ! Neptune
    case(899)
       alpha0=alpha0 + fcoefa(1)*sin(coefs(1))
       delta0=delta0 + fcoefd(1)*cos(coefs(1))
       W=W + fcoefW(1)*sin(coefs(1))
       dW=dW + fcoefdW(1)*cos(coefs(1))

    ! Triton (Neptune)
    case(801)
       do ii=1,ncoefs
          alpha0=alpha0 + fcoefa(ii)*sin(ii*coefs(7))
          delta0=delta0 + fcoefd(ii)*cos(ii*coefs(7))
          W=W + fcoefW(ii)*sin(ii*coefs(7))
          dW=dW + fcoefdW(ii)*cos(ii*coefs(7))
       enddo
    ! Naiad (Neptune)
    case(803)
       alpha0=alpha0 + fcoefa(1)*sin(coefs(1))
       alpha0=alpha0 + fcoefa(2)*sin(coefs(1))
       alpha0=alpha0 + fcoefa(8)*sin(coefs(8))

       delta0=delta0 + fcoefd(1)*cos(coefs(1))
       delta0=delta0 + fcoefd(2)*cos(coefs(1))
       delta0=delta0 + fcoefd(8)*cos(coefs(8))

       W=W + fcoefW(1)*sin(coefs(1))
       W=W + fcoefW(2)*sin(coefs(1))
       W=W + fcoefW(8)*sin(coefs(8))

       dW=dW + fcoefdW(1)*cos(coefs(1))
       dW=dW + fcoefdW(2)*cos(coefs(1))
       dW=dW + fcoefdW(8)*cos(coefs(8))

    ! Satellites de Jupiter gallileens
    ! Satellites de Saturne
    ! Satellites d'Uranus
    ! Satellites de Neptune
    case(501,502,503,504,&
         601,603,605,606, &
         701,702,703,704,706,707,708,709,710,711,712,713,714,715,&
         804,805,806,807,808)
       do ii=1,ncoefs
          alpha0 = alpha0 + fcoefa(ii)*sin(coefs(ii))
          delta0 = delta0 + fcoefd(ii)*cos(coefs(ii))
          W = W + fcoefW(ii)*sin(coefs(ii))
          dW = dW + fcoefdW(ii)*cos(coefs(ii))
       enddo

    ! sans oublier les corps dont le mouvement est linéaire
    case (515,516,602,604,608,609,612,613,614,615,616,617,618,901)

    ! cas d'erreur
    case default
       alpha0 = 0.0_PM_REEL
       delta0 = 0.0_PM_REEL
       W = 0.0_PM_REEL
       dW = 0.0_PM_REEL

      ! Cas d'erreur
       write(errtab(1),*) numpla
       errtab(2) =  "UAI2000"
       call MSP_signaler_message(cle_mes="CPS_ERR_CORPSNONDEC", &
            routine="cps_poletsid", &
            partie_variable=errtab)
    end select

! Fin commune
9999 continue

    ! Désallocation des tableaux externes
    if(associated(coefs))   deallocate(coefs, stat=iostat)
    if(associated(coefsd1)) deallocate(coefsd1, stat=iostat)

    ! Désallocation des tableaux internes
    if(associated(fcoefa))  deallocate(fcoefa, stat=iostat)
    if(associated(fcoefd))  deallocate(fcoefd, stat=iostat)
    if(associated(fcoefW))  deallocate(fcoefW, stat=iostat)
    if(associated(fcoefdW)) deallocate(fcoefdW, stat=iostat)

  end subroutine cps_poletsid



  subroutine cps_getcoefpoly(numpla,ca0, ca1, cd0, cd1,cW0, cW1,ca2,cd2, cW2)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getcoefpoly
!
!$Resume
!  coefs pour calculer les aplha/delta et temps sidéral UAI2000
!
!$Description
!  Routine donnant les coefficients polynomiaux nécessaire au calcul des
!  coordonnées standards et du temps sidéral (et sa dérivée) pour la théorie
!  UAI 2000
!
!$Auteur
!  Julien Bouillant (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getcoefpoly(numpla,ca0, ca1, cd0, cd1,cW0, cW1,ca2,cd2, cW2)
!.    real(kind=PM_REEL) :: ca0, ca1, cd0, cd1,cW0, cW1,ca2,cd2, cW2
!.    integer :: numpla
!
!$Arguments
!>E     numpla  :<integer>   code du corps étudié
!>S     ca0     :<PM_REEL>   coefficient polynomial (0) de l'ascension droite
!>S     ca1     :<PM_REEL>   coefficient polynomial (1) de l'ascension droite
!>S     cd0     :<PM_REEL>   coefficient polynomial (0) de la déclinaison
!>S     cd1     :<PM_REEL>   coefficient polynomial (1) de la déclinaison
!>S     cW0     :<PM_REEL>   coefficient polynomial (0) du temps sidéral
!>S     cW1     :<PM_REEL>   coefficient polynomial (1) du temps sidéral
!>S     ca2     :<PM_REEL>   coefficient polynomial (2) de l'ascension droite
!>S     cd2     :<PM_REEL>   coefficient polynomial (2) de la déclinaison
!>S     cW2     :<PM_REEL>   coefficient polynomial (2) du temps sidéral
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
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
   implicit none
   
   real(kind=PM_REEL), intent(out) :: ca0, ca1, cd0, cd1,cW0, cW1,ca2,cd2, cW2
   integer, intent(in) :: numpla

    ! traitement d'erreur
    character(len=30), dimension(2) :: errtab

   ! Saisie des 9 coefs en fonction des corps
      
   select case (numpla)
   ! Mercure
   case(199)
      ca0=281.01_PM_REEL
      ca1=-0.033_PM_REEL
      cd0=61.45_PM_REEL
      cd1=-0.005_PM_REEL
      cW0=329.548_PM_REEL
      cW1=6.1385025_PM_REEL
      ! Pas de coefs en T**2
      ca2=0._PM_REEL
      cd2=0._PM_REEL
      cW2=0._PM_REEL
   ! Venus
   case(299)
      ca0=272.76_PM_REEL
      ca1=0._PM_REEL
      cd0=67.16_PM_REEL
      cd1=0._PM_REEL
      cW0=160.20_PM_REEL
      cW1=-1.4813688_PM_REEL
      ! Pas de coefs en T**2
      ca2=0._PM_REEL
      cd2=0._PM_REEL
      cW2=0._PM_REEL
   ! Terre
   case(399)
      ca0=0._PM_REEL
      ca1=-0.641_PM_REEL
      cd0=90._PM_REEL
      cd1=-0.557_PM_REEL
      cW0=190.16_PM_REEL
      cW1=360.9856235_PM_REEL
      ! Pas de coefs en T**2
      ca2=0._PM_REEL
      cd2=0._PM_REEL
      cW2=0._PM_REEL
   ! Lune
   case(301) 
      ca0=269.9949_PM_REEL
      ca1=0.0031_PM_REEL
      cd0=66.5392_PM_REEL
      cd1=0.0130_PM_REEL
      cW0=38.3213_PM_REEL
      cW1=13.17635815_PM_REEL
      ! Pas de coefs en T**2 pour alpha/delta
      ca2=0._PM_REEL
      cd2=0._PM_REEL
      ! temps sideral en T**2
      cW2=-1.4_PM_REEL*10_PM_REEL**(-12_PM_REEL)
   ! Mars 
   case(499)
      ca0=317.68143_PM_REEL
      ca1=-0.1061_PM_REEL
      cd0=52.88650_PM_REEL
      cd1=-0.0609_PM_REEL
! Z! diff par rapport a MSLIB 176.630 (erreur MSLIB?) % UAI2000
      cW0=176.753_PM_REEL
      cW1=350.89198226_PM_REEL
      ! Pas de coefs en T**2
      ca2=0._PM_REEL
      cd2=0._PM_REEL
      cW2=0._PM_REEL
    ! Phobos
   case(401)
      ca0=317.67_PM_REEL
      ca1=-0.108_PM_REEL
      cd0=52.90_PM_REEL
      cd1=-0.061_PM_REEL
      cW0=35.06_PM_REEL
      cW1=1128.8445850_PM_REEL
      ! temps sideral en T**2
      cW2=8.864_PM_REEL
      ! Pas de coefs en T**2 pour alpha/delta
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Deimos
   case(402)
      ca0=316.65_PM_REEL
      ca1=-0.108_PM_REEL
      cd0=53.52_PM_REEL
      cd1=-0.061_PM_REEL
      cW0=79.41_PM_REEL
      cW1=285.1618970_PM_REEL
      ! temps sideral en T**2
      cW2=-0.520_PM_REEL
      ! Pas de coefs en T**2 pour alpha/delta
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Jupiter
   case(599)
      ca0=268.05_PM_REEL
      ca1=-0.009_PM_REEL
      cd0=64.49_PM_REEL
      cd1=0.003_PM_REEL
      cW0=284.95_PM_REEL
      cW1=870.5366420_PM_REEL
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Z! Io (Jupiter)
   case(501)
      ca0=268.05_PM_REEL
      ca1=-0.009_PM_REEL
      cd0=64.50_PM_REEL
      cd1=0.003_PM_REEL
      cW0=200.39_PM_REEL
      cW1=203.4889538_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Europa (Jupiter)
    case(502)
      ca0=268.08_PM_REEL
      ca1=-0.009_PM_REEL
      cd0=64.51_PM_REEL
      cd1=0.003_PM_REEL
      cW0=35.67_PM_REEL
      cW1=101.3747235_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Ganymede (Jupiter)
     case(503)
      ca0=268.20_PM_REEL
      ca1=-0.009_PM_REEL
      cd0=64.57_PM_REEL
      cd1=0.003_PM_REEL
      cW0=44.04_PM_REEL
      cW1=50.3176081_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Callisto (Jupiter)
     case(504)
      ca0=268.72_PM_REEL
      ca1=-0.009_PM_REEL
      cd0=64.83_PM_REEL
      cd1=0.003_PM_REEL
      cW0=259.73_PM_REEL
      cW1=21.5710715_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Amalthea (Jupiter)
   case(505)
      ca0=268.05_PM_REEL
      ca1=-0.009_PM_REEL
      cd0=64.49_PM_REEL
      cd1=0.003_PM_REEL
      cW0=231.67_PM_REEL
      cW1=722.6314560_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Thebe (Jupiter)
   case(514)
      ca0=268.05_PM_REEL
      ca1=-0.009_PM_REEL
      cd0=64.49_PM_REEL
      cd1=0.003_PM_REEL
      cW0=8.56_PM_REEL
      cW1=533.7004100_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
   ! Adrastea (Jupiter)
   case(515)
      ca0=268.05_PM_REEL
      ca1=-0.009_PM_REEL
      cd0=64.49_PM_REEL
      cd1=0.003_PM_REEL
      cW0=33.29_PM_REEL
      cW1=1206.9986602_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
   ! Metis (Jupiter)
   case(516)
      ca0=268.05_PM_REEL
      ca1=-0.009_PM_REEL
      cd0=64.49_PM_REEL
      cd1=0.003_PM_REEL
      cW0=346.09_PM_REEL
      cW1=1221.2547301_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Saturne
   case(699)
      ca0=40.589_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.537_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=38.90_PM_REEL
      cW1=810.7939024_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Mimas (Saturne)
   case(601)
      ca0=40.66_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.52_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=337.46_PM_REEL
      cW1=381.9945550_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
   ! Encelade (Saturne)
   case(602)
      ca0=40.66_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.52_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=2.82_PM_REEL
      cW1=262.7318996_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Tethys (Saturne)
   case(603)
      ca0=40.66_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.52_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=10.45_PM_REEL
      cW1=190.6979085_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
   ! Dionée  (Saturne)
   case(604)
      ca0=40.66_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.52_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=357.00_PM_REEL
      cW1=131.5349316_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Reha (Saturne)
   case(605)
      ca0=40.38_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.55_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=235.16_PM_REEL
      cW1=79.6900478_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Titan(Saturne)
   case(606)
      ca0=36.41_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.94_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=189.64_PM_REEL
      cW1=22.5769768_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
   ! Japet(Saturne)
   case(608)
      ca0=318.16_PM_REEL
      ca1=-3.949_PM_REEL
      cd0=75.03_PM_REEL
      cd1=-1.143_PM_REEL
      cW0=350.20_PM_REEL
      cW1=4.5379572_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
   ! Phoebe  (Saturne)
   case(609)
      ca0=355.00_PM_REEL
      ca1=0._PM_REEL
      cd0=68.70_PM_REEL
      cd1=0._PM_REEL
      cW0=304.70_PM_REEL
      cW1=930.8338720_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Janus (Saturne)
   case(610)
      ca0=40.58_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.52_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=58.83_PM_REEL
      cW1=518.2359876_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Epimethée (Saturne)
   case(611)
      ca0=40.58_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.52_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=293.87_PM_REEL
      cW1=518.4907239_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
   ! Hélène (Saturne)
   case(612)
      ca0=40.85_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.34_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=245.12_PM_REEL
      cW1=131.6174056_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
   ! Télesto (Saturne)
   case(613)
      ca0=50.51_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=84.06_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=56.88_PM_REEL
      cW1=190.6979332_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
   ! Calypso (Saturne)
   case(614)
      ca0=36.41_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=85.04_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=153.51_PM_REEL
      cW1=190.6742373_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
   ! Atlas (Saturne)
   case(615)
      ca0=40.58_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.53_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=137.88_PM_REEL
      cW1=598.3060000_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
   ! Prométhée (Saturne)
   case(616)
      ca0=40.58_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.53_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=296.14_PM_REEL
      cW1=587.289000_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
   ! Pandora (Saturne)
   case(617)
      ca0=40.58_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.53_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=162.92_PM_REEL
      cW1=572.7891000_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
   ! Pan (Saturne)
   case(618)
      ca0=40.6_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.5_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=48.8_PM_REEL
      cW1=626.0440000_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Uranus
   case(799)
      ca0=257.311_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.175_PM_REEL
      cd1=0._PM_REEL
      cW0=203.81_PM_REEL
      cW1=-501.1600928_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Ariel (Uranus)
   case(701)
      ca0=257.43_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.08_PM_REEL
      cd1=0._PM_REEL
      cW0=30.70_PM_REEL
      cW1=-254.6906892_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Umbriel (Uranus)
    case(702)
      ca0=257.43_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.10_PM_REEL
      cd1=0._PM_REEL
      cW0=108.05_PM_REEL
      cW1=-86.8688923_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Titania (Uranus)
    case(703)
      ca0=257.43_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.10_PM_REEL
      cd1=0._PM_REEL
      cW0=77.74_PM_REEL
      cW1=-41.3514316_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Oberon  (Uranus)
    case(704)
      ca0=257.43_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.10_PM_REEL
      cd1=0._PM_REEL
      cW0=6.77_PM_REEL
      cW1=-26.7394932_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Miranda  (Uranus)
    case(705)
      ca0=257.43_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.08_PM_REEL
      cd1=0._PM_REEL
      cW0=30.70_PM_REEL
      cW1=-254.6906892_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Cordelia (Uramus)
    case(706)
      ca0=257.31_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.18_PM_REEL
      cd1=0._PM_REEL
      cW0=127.69_PM_REEL
      cW1=-1074.5205730_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Ophelia (Uramus)
     case(707)
      ca0=257.31_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.18_PM_REEL
      cd1=0._PM_REEL
      cW0=130.35_PM_REEL
      cW1=-956.4068150_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Bianca (Uramus)
    case(708)
      ca0=257.31_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.18_PM_REEL
      cd1=0._PM_REEL
      cW0=105.46_PM_REEL
      cW1=-828.3914760_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Cressida (Uramus)
     case(709)
      ca0=257.31_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.18_PM_REEL
      cd1=0._PM_REEL
      cW0=59.16_PM_REEL
      cW1=-776.5816320_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Desdemona (Uranus)
    case(710)
      ca0=257.31_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.18_PM_REEL
      cd1=0._PM_REEL
      cW0=95.08_PM_REEL
      cW1=-760.0531690_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Juliet (Uranus)
    case(711)
      ca0=257.31_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.18_PM_REEL
      cd1=0._PM_REEL
      cW0=302.56_PM_REEL
      cW1=-730.1253660_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Portia (Uranus)
    case(712)
      ca0=257.31_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.18_PM_REEL
      cd1=0._PM_REEL
      cW0=25.03_PM_REEL
      cW1=-701.4865870_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Rosalind (Uranus)
    case(713)
      ca0=257.31_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.18_PM_REEL
      cd1=0._PM_REEL
      cW0=314.90_PM_REEL
      cW1=-644.6311260_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Belinda (Uranus)
    case(714)
      ca0=257.31_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.18_PM_REEL
      cd1=0._PM_REEL
      cW0=297.46_PM_REEL
      cW1=-577.3628170_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Puck (Uranus)
    case(715)
      ca0=257.31_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.18_PM_REEL
      cd1=0._PM_REEL
      cW0=91.24_PM_REEL
      cW1=-472.5450690_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Neptune
   case(899)
      ca0=299.36_PM_REEL
      ca1=0._PM_REEL
      cd0=43.46_PM_REEL
      cd1=0._PM_REEL
      cW0=253.18_PM_REEL
      cW1=536.3128492_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Triton (Neptune)
   case(801)
      ca0=299.36_PM_REEL
      ca1=0._PM_REEL
      cd0=41.17_PM_REEL
      cd1=0._PM_REEL
      cW0=296.53_PM_REEL
      cW1=-61.2572637_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Naiad (Neptune)
   case(803)
      ca0=299.36_PM_REEL
      ca1=0._PM_REEL
      cd0=43.36_PM_REEL
      cd1=0._PM_REEL
      cW0=254.06_PM_REEL
      cW1=1222.8441209_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Thalassa (Neptune)
   case(804)
      ca0=299.36_PM_REEL
      ca1=0._PM_REEL
      cd0=43.45_PM_REEL
      cd1=0._PM_REEL
      cW0=102.06_PM_REEL
      cW1=1155.7555612_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Despina (Neptune)
   case(805)
      ca0=299.36_PM_REEL
      ca1=0._PM_REEL
      cd0=43.45_PM_REEL
      cd1=0._PM_REEL
      cW0=306.51_PM_REEL
      cW1=1075.7341562_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Galathea (Neptune)
   case(806)
      ca0=299.36_PM_REEL
      ca1=0._PM_REEL
      cd0=43.43_PM_REEL
      cd1=0._PM_REEL
      cW0=258.09_PM_REEL
      cW1=839.6597686_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Larissa (Neptune)
   case(807)
      ca0=299.36_PM_REEL
      ca1=0._PM_REEL
      cd0=43.41_PM_REEL
      cd1=0._PM_REEL
      cW0=179.41_PM_REEL
      cW1=649.0534470_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
   ! Proteus (Neptune)
   case(808)
      ca0=299.27_PM_REEL
      ca1=0._PM_REEL
      cd0=42.91_PM_REEL
      cd1=0._PM_REEL
      cW0=93.38_PM_REEL
      cW1=320.7654228_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
   ! Pluton
   case(999)
      ca0=313.02_PM_REEL 
      ca1=0._PM_REEL
      cd0=9.09_PM_REEL
      cd1=0._PM_REEL
      cW0=236.77_PM_REEL
      cW1=-56.3623195_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
   ! Charon
   case(901)
      ca0=313.02_PM_REEL     
      ca1=0._PM_REEL
      cd0=9.09_PM_REEL
      cd1=0._PM_REEL
      cW0=56.77_PM_REEL
      cW1=-56.3623195_PM_REEL
      ! Pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
   ! Hypérion  (Saturne), entre autre
   case default
      ! Cas d'erreur à rajouter
       write(errtab(1),*) numpla
       errtab(2) =  "UAI200"
       call MSP_signaler_message(cle_mes="CPS_ERR_CORPSNONDEC", &
            routine="cps_getcoefpoly", &
            partie_variable=errtab)
   end select

  end subroutine cps_getcoefpoly

  subroutine cps_calccoef(codebary, d, T, ncoefs, coefs,coefsd1)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_calccoef
!
!$Resume
!  Routine donnant les coefficients E/M/J/S/U/N UAI 2000
!
!$Description
!  Routine donnant les coefficients E/M/J/S/U/N pour le calcul des 
!  coordonnées et du temps sidéral (et sa dérivée) dans la théorie 
!  UAI 2000
!
!$Auteur
!  Julien Bouillant (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_calccoef(codebary, d, T, ncoefs, coefs,coefsd1)
!.    integer :: codebary
!.    real(kind=PM_REEL) :: d, T
!.    integer :: ncoefs
!.    real(kind=PM_REEL), dimension(ncoefs) :: coefs
!.    real(kind=PM_REEL), dimension(ncoefs) :: coefsd1
!
!$Arguments
!>E     codebary  :<integer>                code du corps central du corps étudié
!>E     d         :<PM_REEL>                interval de temps (jour jj2000)
!>E     T         :<PM_REEL,DIM=(IN)>       interval de temps (siècle jj2000)
!>E     ncoefs    :<integer>                nombre de coefficient
!>S     coefs     :<PM_REEL,DIM=(ncoefs)>   tableau de coefficient E/M/J/S/U/N (rad)
!>S     coefsd1   :<PM_REEL,DIM=(ncoefs)>   coefficients de W (pour calcul de dW)
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
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
   implicit none 
   
   !Arguments
   integer           , intent(IN) :: codebary
   real(kind=PM_REEL), intent(IN):: d, T
   integer, intent(in):: ncoefs
   real(kind=PM_REEL), dimension(ncoefs), intent(out) :: coefs
   real(kind=PM_REEL), dimension(ncoefs), intent(out) :: coefsd1
   
   ! Variables locales
   real(kind=PM_REEL), dimension(:), pointer :: coefs0 => NULL(), coefsd2 => NULL() 
   real(kind=PM_REEL), dimension(:), pointer :: coefsT1 => NULL(), coefsT2 => NULL()
   integer :: ii, iostat

   ! traitement d'erreur
   character(len=30), dimension(2) :: errtab
   

   ! Initialisation des variables locales
   if (ncoefs>0) then
      allocate(coefs0(ncoefs))
      allocate(coefsT1(ncoefs))
      allocate(coefsT2(ncoefs))
      allocate(coefsd2(ncoefs))      

      coefs0(1:ncoefs)  = 0._PM_REEL
      coefsd2(1:ncoefs) = 0._PM_REEL
      coefsT1(1:ncoefs) = 0._PM_REEL
      coefsT2(1:ncoefs) = 0._PM_REEL
   endif

   ! initialisation des sorties
   coefs(:) = 0._PM_REEL
   coefsd1(:) = 0._PM_REEL

   !! si coefs E/M/J/S/U/N supplémentaires 
   select case (codebary)
   ! Pas de coefs spécifiques pour Pluton (formules simplifiées)
   ! ni Mercure, ni Vénus qui n'ont pas de satellite
   case(1,2,9)
   ! Satellites de la Terre (Lune)
   ! Coefs pour les E1 a E13
   case(3)
      coefs0(1) = 125.045_PM_REEL
      coefs0(2) = 250.089_PM_REEL
      coefs0(3) = 260.008_PM_REEL
      coefs0(4) = 176.625_PM_REEL
      coefs0(5) = 357.529_PM_REEL
      coefs0(6) = 311.589_PM_REEL
      coefs0(7) = 134.963_PM_REEL
      coefs0(8) = 276.617_PM_REEL
      coefs0(9) = 34.226_PM_REEL
      coefs0(10) = 15.134_PM_REEL
      coefs0(11) = 119.743_PM_REEL
      coefs0(12) = 239.961_PM_REEL
      coefs0(13) =  25.053_PM_REEL

      coefsd1(1) = -0.0529921_PM_REEL
      coefsd1(2) = -0.1059842_PM_REEL
      coefsd1(3) = 13.0120009_PM_REEL
      coefsd1(4) = 13.3407154_PM_REEL
      coefsd1(5) = 0.9856003_PM_REEL
      coefsd1(6) = 26.4057084_PM_REEL
      coefsd1(7) = 13.0649930_PM_REEL
      coefsd1(8) = 0.3287146_PM_REEL
      coefsd1(9) = 1.7484877_PM_REEL
      coefsd1(10) = -0.1589763_PM_REEL
      coefsd1(11) = 0.0036096_PM_REEL
      coefsd1(12) = 0.1643573_PM_REEL
      coefsd1(13) = 12.9590088_PM_REEL
   ! Satellites de Mars
   ! Coefs pour les M1/M2
   case(4)
      coefs0(1) = 169.51_PM_REEL
      coefs0(2) = 192.93_PM_REEL
      coefs0(3) = 53.47_PM_REEL

      coefsd1(1) = -0.4357640_PM_REEL
      coefsd1(2) = 1128.4096700_PM_REEL
      coefsd1(3) = -0.0181510_PM_REEL

      coefsT2(2) = 8.864_PM_REEL
  case(5)
  ! Satellites de Jupiter
  ! Coefs pour les J1 a J8
      coefs0(1) = 73.32_PM_REEL
      coefs0(2) = 24.62_PM_REEL
      coefs0(3) = 283.90_PM_REEL
      coefs0(4) = 355.80_PM_REEL
      coefs0(5) = 119.90_PM_REEL
      coefs0(6) = 229.80_PM_REEL
      coefs0(7) = 352.25_PM_REEL
      coefs0(8) = 113.35_PM_REEL

      coefsT1(1) = 91472.9_PM_REEL
      coefsT1(2) = 45137.2_PM_REEL
      coefsT1(3) = 4850.7_PM_REEL
      coefsT1(4) = 1191.3_PM_REEL
      coefsT1(5) = 262.1_PM_REEL
      coefsT1(6) = 64.3_PM_REEL
      coefsT1(7) = 2382.6_PM_REEL
      coefsT1(8) = 6070.0_PM_REEL
   case(6)
   ! Satellites de Saturne
   ! Coefs pour les S1 a S7
      coefs0(1) = 353.32_PM_REEL
      coefs0(2) = 28.72_PM_REEL
      coefs0(3) = 177.40_PM_REEL
      coefs0(4) = 300.00_PM_REEL
      coefs0(5) = 316.45_PM_REEL
      coefs0(6) = 345.20_PM_REEL
      coefs0(7) = 29.80_PM_REEL

      coefsT1(1) = 75706.7_PM_REEL
      coefsT1(2) = 75706.7_PM_REEL
      coefsT1(3) = -36505.5_PM_REEL
      coefsT1(4) = -7225.9_PM_REEL
      coefsT1(5) = 506.2_PM_REEL
      coefsT1(6) = -1016.3_PM_REEL
      coefsT1(7) = -52.1_PM_REEL
   case(7)
   ! Satellites d'Uranus
   ! Coefs pour les U1 a U16
      coefs0(1) = 115.75_PM_REEL
      coefs0(2) = 141.69_PM_REEL
      coefs0(3) = 135.03_PM_REEL
      coefs0(4) =  61.77_PM_REEL
      coefs0(5) = 249.32_PM_REEL
      coefs0(6) = 43.86_PM_REEL
      coefs0(7) =  77.66_PM_REEL
      coefs0(8) = 157.36_PM_REEL
      coefs0(9) = 101.81_PM_REEL
      coefs0(10) = 138.64_PM_REEL
      coefs0(11) = 102.23_PM_REEL
      coefs0(12) = 316.41_PM_REEL
      coefs0(13) = 304.01_PM_REEL
      coefs0(14) = 308.71_PM_REEL
      coefs0(15) = 340.82_PM_REEL
      coefs0(16) = 259.14_PM_REEL

      coefsT1(1) = 54991.87_PM_REEL
      coefsT1(2) = 41887.66_PM_REEL
      coefsT1(3) = 29927.35_PM_REEL
      coefsT1(4) = 25733.59_PM_REEL
      coefsT1(5) = 24471.46_PM_REEL
      coefsT1(6) = 22278.41_PM_REEL
      coefsT1(7) = 20289.42_PM_REEL
      coefsT1(8) = 16652.76_PM_REEL
      coefsT1(9) = 12872.63_PM_REEL
      coefsT1(10) = 8061.81_PM_REEL
      coefsT1(11) = -2024.22_PM_REEL
      coefsT1(12) = 2863.96_PM_REEL
      coefsT1(13) = -51.94_PM_REEL
      coefsT1(14) = -93.17_PM_REEL
      coefsT1(15) = -75.32_PM_REEL
      coefsT1(16) = -504.81_PM_REEL
   case(8)
   ! Satellites de Neptune
   ! Coefs pour les N1 a N8
      coefs0(1) = 323.92_PM_REEL
      coefs0(2) = 220.51_PM_REEL
      coefs0(3) = 354.27_PM_REEL
      coefs0(4) = 75.31_PM_REEL
      coefs0(5) = 35.36_PM_REEL
      coefs0(6) = 142.61_PM_REEL
      coefs0(7) = 177.85_PM_REEL
      coefs0(8) = 357.85_PM_REEL

      coefsT1(1) = 62606.6_PM_REEL
      coefsT1(2) = 55064.2_PM_REEL
      coefsT1(3) = 46564.5_PM_REEL
      coefsT1(4) = 26109.4_PM_REEL
      coefsT1(5) = 14325.4_PM_REEL
      coefsT1(6) = 2824.6_PM_REEL
      coefsT1(7) = 52.316_PM_REEL
      coefsT1(8) = 52.316_PM_REEL

   ! Coefs pour N (Neptune seule)
   case(899)
      coefs0(1) = 357.85_PM_REEL
      coefsT1(1) = 52.316_PM_REEL
   case default
       write(errtab(1),*) "de barycentre ",  codebary
       errtab(2) =  "UAI2000"
       call MSP_signaler_message(cle_mes="CPS_ERR_CORPSNONDEC", &
            routine="cps_calccoef", &
            partie_variable=errtab)
   end select

   ! Calcul des E/M/J/S/U/N
   if (ncoefs==0) then 
      return
   else
      do ii=1 , ncoefs
         coefs(ii)=coefs0(ii)+ coefsd1(ii)*d + coefsd2(ii)*d**2 + &
              coefsT1(ii)*T + coefsT2(ii)*T**2
         coefs(ii)=coefs(ii)*pm_deg_rad
      enddo
   endif

   ! On retourne le tableau coefs (+ncoefs)

   ! desallocation des tableaux dynamiques
   deallocate(coefs0, stat=iostat)
   deallocate(coefsT1, stat=iostat)
   deallocate(coefsT2, stat=iostat)
   deallocate(coefsd2, stat=iostat)

  end subroutine cps_calccoef


  subroutine cps_poletsid91(numpla, dateref, alpha0, delta0, W, dW)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_poletsid91
!
!$Resume
!  Calcul effectif du "pole_tsid" dans la théorie UAI 91
!
!$Description
!  Calcul effectif de l'ascension droite, de la déclinaison, du temps 
!  sidéral et de sa dérivée pour les axes du pôle d'un corps du
!  système solaire dans la théorie UAI 91
!
!$Auteur
!  Julien Bouillant (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_poletsid91(numpla, dateref, alpha0, delta0, W, dW)
!.    integer :: numpla
!.    type(tm_jour_sec) :: dateref 
!.    real(kind=PM_REEL) :: alpha0
!.    real(kind=PM_REEL) :: delta0
!.    real(kind=PM_REEL) :: W
!.    real(kind=PM_REEL) :: dW
!
!$Arguments
!>E     numpla   :<integer>       code de la planète central du corps étudié
!>E     dateref  :<tm_jour_sec>   date de calcul (jj 1950)
!>S     alpha0   :<PM_REEL>       ascension droite (deg)
!>S     delta0   :<PM_REEL>       déclinaison (deg)
!>S     W        :<PM_REEL>       temps sidéral (deg)
!>S     dW       :<PM_REEL>       dérivée du temps sidéral (deg/j)
!
!$Common
!
!$Routines
!- md_joursec_jourfrac
!- cps_getcoefpoly91
!- MSP_signaler_message
!- cps_calccoef91
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

    !Arguments
    integer           , intent(IN) :: numpla
    type(tm_jour_sec) , intent(IN) :: dateref  !! DATEREF DOIT ETRE DONNEE EN JJ1950
    real(kind=PM_REEL), intent(OUT):: alpha0
    real(kind=PM_REEL), intent(OUT):: delta0
    real(kind=PM_REEL), intent(OUT):: W
    real(kind=PM_REEL), intent(OUT):: dW

    ! Variables locales
    real(kind=PM_REEL) :: d, T
    real(kind=PM_REEL) :: ca0, ca1, ca2
    real(kind=PM_REEL) :: cd0, cd1, cd2
    real(kind=PM_REEL) :: cW0, cW1, cW2

    real(kind=PM_REEL), dimension(:), pointer :: coefs
    real(kind=PM_REEL), dimension(:), pointer :: fcoefa => NULL(), fcoefd => NULL()
    real(kind=PM_REEL), dimension(:), pointer :: fcoefW => NULL(), fcoefdW => NULL()
    real(kind=PM_REEL), dimension(:), pointer :: coefsd1
    integer :: ncoefs, codebary, ii, iostat

    real(kind=PM_REEL) :: date
    type(tm_code_retour) :: code
    real(kind=PM_REEL) :: siecle = 36525_PM_REEL

    ! traitement d'erreur
    character(len=30), dimension(2) :: errtab

    ! Calcul des parametres de changement de repere (mouvement du pole,
    ! temps sideral et dérivée du temsp sidéral) 
    !------------------------------------------------------------------

    ! dates J50 en jours / siecles depuis J2000
    call md_joursec_jourfrac(dateref, date, code)
    d=date-eph_date_jj50_j2000
    T = d / siecle

    ! cas linéaire (planetes sauf Neptune)
    call cps_getcoefpoly91(numpla,ca0, ca1, cd0, cd1,cW0, cW1,ca2,cd2, cW2)
    if (MSP_gen_messages("cps_poletsid91")) return

    ! Calcul dans le cas linéaire
    alpha0 = ca0+ ca1*T 
    delta0 = cd0+ cd1*T 
    W = cW0+ cW1*d 
    dW = cW1

    ! barycentre : les coefs E/M/J/S/U/N sont par système planétaire
    codebary = numpla/100

    ! pas de coefs E/M/J/S/U/N supplementaires nécessaires
    if (mod(numpla,100) == 99.and. numpla /= 899) return

    ! si coefs E/M/J/S/U/N
    if (numpla == 899) codebary=numpla ! on traite Neptune avec un coefficent N
                                       ! différent de ses satellites

    ! nombre de coefficients en fonctions du la planete de reference
    select case (codebary)
    ! Mercure et Vénus : pas de satellites connus
    case(1) 
       ncoefs=0
    case(2) 
       ncoefs=0    
    ! satellites de la Terre
    case(3) 
       ncoefs=5 
    ! satellites de Mars 
    case(4) 
       ncoefs=3
    ! satellites de Jupiter
    case(5) 
       ncoefs=8
    ! satellites de Saturne
    case(6) 
       ncoefs=7
    ! satellites d'Uranus
    case(7) 
       ncoefs=16

   ! satellites de Neptune
   ! Il faut rajouter un coefficient supplémentaire par rapport au 8 existant pour 
   ! pouvoir écrire les formules des coefficients de Triton.
    case(8) 
       ncoefs=9
   ! Modèle simplifié pour Charron
    case(9) 
       ncoefs=0
   ! Modèle pour Neptune seul
    case(899) 
       ncoefs=1
    case default
       alpha0 = 0.0_PM_REEL
       delta0 = 0.0_PM_REEL
       W = 0.0_PM_REEL
       dW = 0.0_PM_REEL

       ! Cas d'erreur
       write(errtab(1),*) "de barycentre ",  codebary
       errtab(2) =  "UAI91"
       call MSP_signaler_message(cle_mes="CPS_ERR_CORPSNONDEC", &
            routine="cps_poletsid91", &
            partie_variable=errtab)
       return
    end select

    ! pas de coefs E/M/J/S/U/N nécessaire
    if (ncoefs == 0) return

    ! tableaux dynamiques pour les coefficients 
    if (associated(coefs)) deallocate(coefs, stat=iostat)
    allocate(coefs(ncoefs))
    if (associated(coefsd1)) deallocate(coefsd1, stat=iostat)
    allocate(coefsd1(ncoefs))
    if (ncoefs>0) then
       coefs(1:ncoefs) = 0._PM_REEL
       coefsd1(1:ncoefs) = 0._PM_REEL       
    endif

    ! donne les coefficients E/M/J/S/U/N 
    call cps_calccoef91(codebary, d, T, ncoefs, coefs, coefsd1) 
    if (MSP_gen_messages("cps_poletsid91")) goto 9999

    ! Facteurs des coef. E/M/J/S/U/N , par corps
    if (ncoefs>0) then
       if (associated(fcoefa))  deallocate(fcoefa, stat=iostat)
       if (associated(fcoefd))  deallocate(fcoefd, stat=iostat)
       if (associated(fcoefW))  deallocate(fcoefW, stat=iostat)
       if (associated(fcoefdW)) deallocate(fcoefdW, stat=iostat)
       allocate(fcoefa(ncoefs))
       allocate(fcoefd(ncoefs))
       allocate(fcoefW(ncoefs))
       allocate(fcoefdW(ncoefs))
       fcoefa(1:ncoefs) = 0._PM_REEL
       fcoefd(1:ncoefs) = 0._PM_REEL
       fcoefW(1:ncoefs) = 0._PM_REEL
       fcoefdW(1:ncoefs) = 0._PM_REEL
    endif

    select case (numpla)
    ! Lune
    case(301)
       fcoefa(1) = -3.878_PM_REEL
       fcoefa(2) = -0.120_PM_REEL
       fcoefa(3) = 0.070_PM_REEL
       fcoefa(4) = -0.017_PM_REEL
  
       fcoefd(1) = 1.543_PM_REEL
       fcoefd(2) = 0.024_PM_REEL
       fcoefd(3) = -0.028_PM_REEL 
       fcoefd(4) = 0.007_PM_REEL 

       fcoefW(1) = 3.558_PM_REEL 
       fcoefW(2) = 0.121_PM_REEL 
       fcoefW(3) = -0.064_PM_REEL
       fcoefW(4) = 0.016_PM_REEL
       fcoefW(5) = 0.025_PM_REEL 

       do ii=1, ncoefs
          fcoefdW(ii) = fcoefW(ii) * coefsd1(ii) 
       enddo
    ! Phobos
    case(401)
       fcoefa(1) = 1.79_PM_REEL
       fcoefd(1) = -1.08_PM_REEL
       fcoefW(1) = -1.42_PM_REEL
       fcoefW(2) = -0.78_PM_REEL

       fcoefdW(1) = 1.42_PM_REEL*0.4357640_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(2) = -0.78_PM_REEL*(1128.4096700_PM_REEL+(8.864_PM_REEL/siecle**2)*2*d)

    ! Deimos
    case(402)
       fcoefa(1) = 2.98_PM_REEL
       fcoefd(1) = -1.78_PM_REEL
       fcoefW(1) = -2.58_PM_REEL
       fcoefW(2) = 0.19_PM_REEL
       fcoefdW(1) = 2.58_PM_REEL*0.0181510_PM_REEL
       fcoefdW(2) = - 0.19_PM_REEL*0.0181510_PM_REEL
    ! Io (Jupiter)
    case(501)
       fcoefa(3) = 0.094_PM_REEL
       fcoefa(4) = 0.024_PM_REEL
       fcoefd(3) = 0.040_PM_REEL
       fcoefd(4) = 0.011_PM_REEL
       fcoefW(3) = -0.085_PM_REEL
       fcoefW(4) = -0.022_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(3) = -0.085_PM_REEL*4850.7_PM_REEL/ siecle
       fcoefdW(4) = -0.022_PM_REEL*1191.3_PM_REEL/ siecle
    ! Europa (Jupiter)
    case(502)
       fcoefa(4) = 1.086_PM_REEL
       fcoefa(5) = 0.060_PM_REEL
       fcoefa(6) = 0.015_PM_REEL
       fcoefa(7) = 0.009_PM_REEL
       fcoefd(4) = 0.468_PM_REEL
       fcoefd(5) = 0.026_PM_REEL
       fcoefd(6) = 0.007_PM_REEL
       fcoefd(7) = 0.002_PM_REEL
       fcoefW(4) = -0.980_PM_REEL
       fcoefW(5) = -0.054_PM_REEL
       fcoefW(6) = -0.014_PM_REEL
       fcoefW(7) = -0.008_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(4) = -0.980_PM_REEL*1191.3_PM_REEL / siecle
       fcoefdW(5) = -0.054_PM_REEL*262.1_PM_REEL / siecle
       fcoefdW(6) = -0.014_PM_REEL*64.3_PM_REEL / siecle
       fcoefdW(7) = -0.008_PM_REEL*2382.6_PM_REEL / siecle
    ! Ganymede (Jupiter)
    case(503)
       fcoefa(4) = -0.037_PM_REEL
       fcoefa(5) = 0.431_PM_REEL
       fcoefa(6) = 0.091_PM_REEL
       fcoefd(4) = -0.016_PM_REEL
       fcoefd(5) = 0.186_PM_REEL
       fcoefd(6) = 0.039_PM_REEL
       fcoefW(4) = 0.033_PM_REEL
       fcoefW(5) = -0.389_PM_REEL
       fcoefW(6) = -0.082_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(4) = 0.033_PM_REEL*1191.3_PM_REEL/ siecle
       fcoefdW(5) = -0.389_PM_REEL*262.1_PM_REEL/ siecle
       fcoefdW(6) = -0.082_PM_REEL*64.3_PM_REEL / siecle
    ! Callisto (Jupiter)
    case(504)
       fcoefa(5) = -0.068_PM_REEL
       fcoefa(6) = 0.590_PM_REEL
       fcoefa(8) = 0.010_PM_REEL
       fcoefd(5) = -0.029_PM_REEL
       fcoefd(6) = 0.254_PM_REEL
       fcoefd(8) = -0.004_PM_REEL
       fcoefW(5) = 0.061_PM_REEL
       fcoefW(6) = -0.533_PM_REEL
       fcoefW(8) = -0.009_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(5) = 0.061_PM_REEL*262.1_PM_REEL / siecle
       fcoefdW(6) = -0.533_PM_REEL*64.3_PM_REEL/ siecle
       fcoefdW(8) = -0.009_PM_REEL*6070.0_PM_REEL / siecle
    ! Amalthea (Jupiter)
    case(505)
       fcoefa(1) = -0.84_PM_REEL
       fcoefa(2) = 0.01_PM_REEL
       fcoefd(1) = -0.36_PM_REEL
       fcoefW(1) = 0.76_PM_REEL
       fcoefW(2) = -0.01_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(1) = 0.76_PM_REEL*91472.9_PM_REEL / siecle
       fcoefdW(2) = -0.01_PM_REEL*91472.9_PM_REEL*2_PM_REEL / siecle
    ! Thebe (Jupiter)
    case(514)
       fcoefa(2) = -2.12_PM_REEL
       fcoefa(3) = 0.04_PM_REEL
       fcoefd(2) = -0.91_PM_REEL
       fcoefd(3) = 0.01_PM_REEL
       fcoefW(2) = 1.91_PM_REEL
       fcoefW(3) = -0.04_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(2) = 1.91_PM_REEL*45137.2_PM_REEL / siecle
       fcoefdW(3) = -0.04_PM_REEL*45137.2_PM_REEL*2_PM_REEL / siecle
    ! Mimas (Saturne)
    case(601)
       fcoefa(3) = 13.56_PM_REEL
       fcoefd(3) = -1.53_PM_REEL
       fcoefW(3) = -13.48_PM_REEL
       fcoefW(5) = -44.85_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(3) = -13.48_PM_REEL*(-36505.5_PM_REEL) / siecle
       fcoefdW(5) = -44.85_PM_REEL*506.2_PM_REEL / siecle
    ! Tethys (Saturne)
    case(603)
       fcoefa(4) = 9.66_PM_REEL
       fcoefd(4) = -1.09_PM_REEL
       fcoefW(4) = -9.60_PM_REEL
       fcoefW(5) = 2.23_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(4) = -9.60_PM_REEL*(-7225.9_PM_REEL) / siecle
       fcoefdW(5) = 2.23_PM_REEL*506.2_PM_REEL/ siecle
    ! Reha (Saturne)
    case(605)
       fcoefa(6) = 3.10_PM_REEL
       fcoefd(6) = -0.35_PM_REEL
       fcoefW(6) = -3.08_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(6) = -3.08_PM_REEL*(-1016.3_PM_REEL) / siecle
    ! Titan(Saturne)
    case(606)
       fcoefa(7) = 2.66_PM_REEL
       fcoefd(7) = -0.30_PM_REEL
       fcoefW(7) = -2.64_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(7) = -2.64_PM_REEL*(-52.1_PM_REEL) / siecle
    ! Janus (Saturne)
    case(610)
       fcoefa(2) = -1.623_PM_REEL
       fcoefa(3) = 0.023_PM_REEL
       fcoefd(2) = -0.183_PM_REEL
       fcoefd(3) = 0.001_PM_REEL
       fcoefW(2) = 1.613_PM_REEL
       fcoefW(3) = -0.023_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(2) = 1.613_PM_REEL*75706.7_PM_REEL/ siecle
       fcoefdW(3) = -0.023_PM_REEL*75706.7_PM_REEL*2_PM_REEL/ siecle
    ! Epimethe (Saturne)
    case(611)
       fcoefa(1) = -3.153_PM_REEL
       fcoefa(2) = 0.086_PM_REEL
       fcoefd(1) = -0.356_PM_REEL
       fcoefd(2) = 0.005_PM_REEL
       fcoefW(1) = 3.133_PM_REEL
       fcoefW(2) = -0.086_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(1) = 3.133_PM_REEL*75706.7_PM_REEL / siecle
       fcoefdW(2) = -0.0086_PM_REEL*75706.7_PM_REEL *2_PM_REEL / siecle
    ! Ariel (Uranus)
    case(701)
       fcoefa(13) = 0.29_PM_REEL
       fcoefd(13) = 0.28_PM_REEL
       fcoefW(12) = 0.05_PM_REEL
       fcoefW(13) = 0.08_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(12) = 0.05_PM_REEL * 2863.96_PM_REEL / siecle
       fcoefdW(13) = 0.08_PM_REEL * (-51.94_PM_REEL) / siecle
    ! Umbriel (Uranus)
    case(702)
       fcoefa(14) = 0.21_PM_REEL
       fcoefd(14) = 0.20_PM_REEL
       fcoefW(12) = -0.09_PM_REEL
       fcoefW(14) = 0.06_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(12) = -0.09_PM_REEL   * 2863.96_PM_REEL / siecle
       fcoefdW(14) = 0.06_PM_REEL * (-75.32_PM_REEL) / siecle
    ! Titania (Uranus)
    case(703)
       fcoefa(15) = 0.29_PM_REEL
       fcoefd(15) = 0.28_PM_REEL
       fcoefW(15) = 0.08_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(15) = 0.08_PM_REEL*(-75.32_PM_REEL) / siecle
     ! Oberon  (Uranus)
   case(704)
       fcoefa(16) = 0.16_PM_REEL
       fcoefd(16) = 0.16_PM_REEL
       fcoefW(16) = 0.04_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(16) = 0.04_PM_REEL*(-504.81_PM_REEL) / siecle
    ! Miranda  (Uranus)
    case(705)
       fcoefa(11) = 4.41_PM_REEL
       fcoefa(12) = -0.04_PM_REEL
       fcoefd(11) = 4.25_PM_REEL
       fcoefd(12) = -0.02_PM_REEL
       fcoefW(10) = -0.09_PM_REEL
       fcoefW(11) = 1.15_PM_REEL
       fcoefW(12) = -1.27_PM_REEL
       fcoefW(13) = 0.15_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(10) = -0.09_PM_REEL* (-2024.22_PM_REEL ) / siecle
       fcoefdW(11) = 1.15_PM_REEL  * 2 *(-2024.22_PM_REEL ) / siecle
       fcoefdW(12) = -1.27_PM_REEL * 2863.96_PM_REEL  / siecle
       fcoefdW(13) = 0.15_PM_REEL  * 2 * 2863.96_PM_REEL  / siecle
    ! Cordelia (Uramus)
    case(706)
       fcoefa(1) = -0.15_PM_REEL 
       fcoefd(1) = 0.14_PM_REEL 
       fcoefW(1) = -0.04_PM_REEL 
       ! attention a la derivée : temps en siecle
       fcoefdW(1) = -0.04_PM_REEL  * 54991.87_PM_REEL  / siecle
    ! Ophelia (Uramus)
    case(707)
       fcoefa(2) = -0.09_PM_REEL 
       fcoefd(2) = 0.09_PM_REEL 
       fcoefW(2) = -0.03_PM_REEL 
       ! attention a la derivée : temps en siecle
       fcoefdW(2) = -0.03_PM_REEL  * 41877.66_PM_REEL  / siecle
    ! Bianca (Uramus)
    case(708)
       fcoefa(3) = -0.16_PM_REEL 
       fcoefd(3) = 0.16_PM_REEL 
       fcoefW(3) = -0.04_PM_REEL 
       ! attention a la derivée : temps en siecle
       fcoefdW(3) = -0.04_PM_REEL  * 29927.35_PM_REEL  / siecle
    ! Cressida (Uramus)
    case(709)
       fcoefa(4) = -0.04_PM_REEL 
       fcoefd(4) = 0.04_PM_REEL 
       fcoefW(4) = -0.01_PM_REEL 
       ! attention a la derivée : temps en siecle
       fcoefdW(4) = -0.01_PM_REEL  * 25733.59 / siecle
    ! Desdemona (Uranus)
    case(710)
       fcoefa(5) = -0.17_PM_REEL 
       fcoefd(5) = 0.16_PM_REEL 
       fcoefW(5) = -0.04_PM_REEL 
       ! attention a la derivée : temps en siecle
       fcoefdW(5) = -0.04_PM_REEL  * 24471.46_PM_REEL  / siecle
    ! Juliet (Uranus)
    case(711)
       fcoefa(6) = -0.06_PM_REEL 
       fcoefd(6) = 0.06_PM_REEL 
       fcoefW(6) = -0.02_PM_REEL 
       ! attention a la derivée : temps en siecle
       fcoefdW(6) = -0.02_PM_REEL  * 22278.41_PM_REEL  / siecle
    ! Portia (Uranus)
    case(712)
       fcoefa(7) = -0.09_PM_REEL 
       fcoefd(7) = 0.09_PM_REEL 
       fcoefW(7) = -0.02_PM_REEL 
       ! attention a la derivée : temps en siecle
      fcoefdW(7) = -0.02_PM_REEL  * 20289.42_PM_REEL  / siecle
     ! Rosalind (Uranus)
    case(713)
       fcoefa(8) = -0.29_PM_REEL 
       fcoefd(8) = 0.28_PM_REEL 
       fcoefW(8) = -0.08_PM_REEL 
       ! attention a la derivée : temps en siecle
       fcoefdW(8) = -0.08_PM_REEL  * 16652.76_PM_REEL  / siecle
    ! Belinda (Uranus)
    case(714)
       fcoefa(9) = -0.03_PM_REEL 
       fcoefd(9) = 0.03_PM_REEL 
       fcoefW(9) = -0.01_PM_REEL 
       ! attention a la derivée : temps en siecle
       fcoefdW(9) = -0.01_PM_REEL  * 12872.63_PM_REEL  / siecle
     ! Puck (Uranus)
   case(715)
       fcoefa(10) = -0.33_PM_REEL 
       fcoefd(10) = 0.031_PM_REEL 
       fcoefW(10) = -0.09_PM_REEL 
       ! attention a la derivée : temps en siecle
       fcoefdW(10) = -0.09_PM_REEL  * 8061.81_PM_REEL  / siecle
    ! Neptune
    case(899)
       fcoefa(1) = 0.70_PM_REEL 
       fcoefd(1) = -0.51_PM_REEL 
       fcoefW(1) = -0.48_PM_REEL 
       ! attention a la derivée : temps en siecle
       fcoefdW(1) = -0.48_PM_REEL  * 52.316_PM_REEL  / siecle
    ! Triton (Neptune)
    case(801) 
       fcoefa(1) = -32.35_PM_REEL 
       fcoefa(2) = -6.28_PM_REEL 
       fcoefa(3) = -2.08_PM_REEL 
       fcoefa(4) = -0.74_PM_REEL 
       fcoefa(5) = -0.28_PM_REEL 
       fcoefa(6) = -0.11_PM_REEL 
       fcoefa(7) = -0.07_PM_REEL 
       fcoefa(8) = -0.02_PM_REEL 
       fcoefa(9) = -0.01_PM_REEL 

       fcoefd(1) = 22.55_PM_REEL 
       fcoefd(2) = 2.10_PM_REEL 
       fcoefd(3) = 0.55_PM_REEL
       fcoefd(4) = 0.16_PM_REEL
       fcoefd(5) = 0.05_PM_REEL
       fcoefd(6) = 0.02_PM_REEL
       fcoefd(7) = 0.01_PM_REEL

       fcoefW(1) = 22.25_PM_REEL
       fcoefW(2) = 6.73_PM_REEL
       fcoefW(3) = 2.05_PM_REEL
       fcoefW(4) = 0.74_PM_REEL
       fcoefW(5) = 0.28_PM_REEL
       fcoefW(6) = 0.11_PM_REEL
       fcoefW(7) = 0.05_PM_REEL
       fcoefW(8) = 0.02_PM_REEL
       fcoefW(9) = 0.01_PM_REEL

       ! attention a la derivée : temps en siecle
       fcoefdW(1) = 22.25_PM_REEL * 52.316_PM_REEL / siecle
       fcoefdW(2) = 6.73_PM_REEL * 52.316_PM_REEL * 2_PM_REEL / siecle
       fcoefdW(3) = 2.05_PM_REEL * 52.316_PM_REEL * 3_PM_REEL / siecle
       fcoefdW(4) = 0.74_PM_REEL * 52.316_PM_REEL * 4_PM_REEL / siecle
       fcoefdW(5) = 0.28_PM_REEL * 52.316_PM_REEL * 5_PM_REEL / siecle
       fcoefdW(6) = 0.11_PM_REEL * 52.316_PM_REEL * 6_PM_REEL / siecle
       fcoefdW(7) = 0.05_PM_REEL * 52.316_PM_REEL * 7_PM_REEL / siecle
       fcoefdW(8) = 0.02_PM_REEL * 52.316_PM_REEL * 8_PM_REEL / siecle
       fcoefdW(9) = 0.01_PM_REEL * 52.316_PM_REEL * 9_PM_REEL / siecle
    ! Naiad (Neptune)
    case(803)
       fcoefa(8) = 0.70_PM_REEL
       fcoefa(1) = -6.49_PM_REEL
       fcoefa(2) = 0.25_PM_REEL

       fcoefd(8) = -0.51_PM_REEL
       fcoefd(1) = -4.75_PM_REEL
       fcoefd(2) = 0.09_PM_REEL

       fcoefW(8) = -0.48_PM_REEL
       fcoefW(1) = 4.40_PM_REEL
       fcoefW(2) = -0.27_PM_REEL

       ! attention a la derivée : temps en siecle
       fcoefdW(8) = -0.48_PM_REEL * 52.316_PM_REEL / siecle
       fcoefdW(1) = 4.40_PM_REEL * 62606.6_PM_REEL / siecle
       fcoefdW(2) = -0.27_PM_REEL * 55064.2_PM_REEL * 2_PM_REEL / siecle
    ! Thalassa (Neptune)
    case(804)
       fcoefa(8) = 0.70_PM_REEL
       fcoefa(2) = -0.28_PM_REEL
       fcoefd(8) = -0.51_PM_REEL
       fcoefd(2) = -0.21_PM_REEL
       fcoefW(8) = -0.48_PM_REEL
       fcoefW(2) = 0.19_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(8) = -0.48_PM_REEL * 52.316_PM_REEL / siecle
       fcoefdW(2) = 0.19_PM_REEL * 55064.2_PM_REEL / siecle
    ! Despina (Neptune)
    case(805)
       fcoefa(8) = 0.7_PM_REEL
       fcoefa(3) = -0.09_PM_REEL
       fcoefd(8) = -0.51_PM_REEL
       fcoefd(3) = -0.07_PM_REEL
       fcoefW(8) = -0.49_PM_REEL
       fcoefW(3) =0.06_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(8) = -0.49_PM_REEL * 52.316_PM_REEL / siecle
       fcoefdW(3) = 0.06_PM_REEL * 46564.5_PM_REEL / siecle
    ! Galathea (Neptune)
    case(806)
       fcoefa(8) = 0.7_PM_REEL
       fcoefa(4) = -0.07_PM_REEL
       fcoefd(8) = -0.51_PM_REEL
       fcoefd(4) = -0.05_PM_REEL
       fcoefW(8) = -0.48_PM_REEL
       fcoefW(4) =0.05_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(8) = -0.48_PM_REEL * 52.316_PM_REEL / siecle
       fcoefdW(4) = 0.05_PM_REEL * 26109.4_PM_REEL / siecle
    ! Larissa (Neptune)
    case(807)
       fcoefa(8) = 0.70_PM_REEL
       fcoefa(5) = -0.27_PM_REEL
       fcoefd(8) = -0.51_PM_REEL
       fcoefd(5) = -0.20_PM_REEL
       fcoefW(8) = -0.48_PM_REEL
       fcoefW(5) =0.19_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(8) = -0.48_PM_REEL * 52.316_PM_REEL / siecle
       fcoefdW(5) = 0.19_PM_REEL * 14325.4_PM_REEL / siecle
   ! Proteus (Neptune)
    case(808)
       fcoefa(8) = 0.70_PM_REEL
       fcoefa(6) = -0.05_PM_REEL
       fcoefd(8) = -0.51_PM_REEL
       fcoefd(6) = -0.04_PM_REEL
       fcoefW(8) = -0.48_PM_REEL
       fcoefW(6) = 0.04_PM_REEL
       ! attention a la derivée : temps en siecle
       fcoefdW(8) = -0.48_PM_REEL * 52.316_PM_REEL / siecle
       fcoefdW(6) = 0.04_PM_REEL * 2824.6_PM_REEL / siecle

    ! sans oublier les corps dont le mouvement est linéaire
    case (515,516,602,604,608,609,612,613,614,615,616,617,618,901)

    ! Cas d'erreur
    case default
       write(errtab(1),*) numpla
       errtab(2) =  "UAI91"
       call MSP_signaler_message(cle_mes="CPS_ERR_CORPSNONDEC", &
            routine="cps_poletsid91", &
            partie_variable=errtab)
       alpha0 = 0.0_PM_REEL
       delta0 = 0.0_PM_REEL
       W = 0.0_PM_REEL
       dW = 0.0_PM_REEL
       goto 9999
    end select

    ! Calcul des parametres de changement de repère pour les lunes des planètes
    ! (mouvement du pole + Temps sidéral et sa dérivée)
    select case (numpla)
    ! Lune
    case(301)
       do ii=1,ncoefs
          alpha0=alpha0 + fcoefa(ii)*sin(coefs(ii))
          delta0=delta0 + fcoefd(ii)*cos(coefs(ii))
       enddo
       
       W = W + cW2*d**2_PM_REEL
       dW = dW + 2_PM_REEL*cW2*d
       do ii=1,ncoefs
          W = W + fcoefW(ii)*sin(coefs(ii)) 
          dW = dW + fcoefdW(ii)*cos(coefs(ii))
       enddo

    ! Phobos
    case(401)
       alpha0=alpha0 + fcoefa(1)*sin(coefs(1))

       delta0=delta0 + fcoefd(1)*cos(coefs(1))

       W = W + cW2*T**2_PM_REEL
       W = W + fcoefW(1)*sin(coefs(1)) 
       W = W + fcoefW(2)*sin(coefs(2)) 

       dW = dW + 2_PM_REEL*cW2*T
       dW = dW + fcoefdW(1)*cos(coefs(1))
       dW = dW + fcoefdW(2)*cos(coefs(2))

    ! Deimos
    case(402)
       alpha0=alpha0 + fcoefa(1)*sin(coefs(3))

       delta0=delta0 + fcoefd(1)*cos(coefs(3))

       W = W + cW2*T**2
       W = W + fcoefW(1)*sin(coefs(3))
       W = W + fcoefW(2)*cos(coefs(3))

       dW = dW + 2_PM_REEL*cW2*T
       dW = dW + fcoefdW(1)*cos(coefs(3))
       dW = dW + fcoefdW(2)*sin(coefs(3))
    ! Amalthea (Jupiter)
    case(505)
       ! ne depand que de J1
       alpha0 = alpha0 + fcoefa(1)*sin(coefs(1))
       alpha0 = alpha0 + fcoefa(2)*sin(2*coefs(1))
       delta0 = delta0 + fcoefd(1)*cos(coefs(1))
       W = W + fcoefW(1)*sin(coefs(1))
       W = W + fcoefW(2)*sin(2*coefs(1))
       dW = dW + fcoefdW(1)*cos(coefs(1))
       dW = dW + fcoefdW(2)*cos(2*coefs(1))
    ! Thebe (Jupiter)
    case(514)
       ! ne depand que de S2
       alpha0 = alpha0 + fcoefa(2)*sin(coefs(2))
       alpha0 = alpha0 + fcoefa(3)*sin(2*coefs(2))
       delta0 = delta0 + fcoefd(2)*cos(coefs(2))
       delta0 = delta0 + fcoefd(3)*cos(2*coefs(2))
       W = W + fcoefW(2)*sin(coefs(2))
       W = W + fcoefW(3)*sin(2*coefs(2))
       dW = dW + fcoefdW(2)*cos(coefs(2))
       dW = dW + fcoefdW(3)*cos(2*coefs(2))
    ! Janus (Saturne)
    case(610)
       ! ne depand que de S2
       alpha0 = alpha0 + fcoefa(2)*sin(coefs(2))
       alpha0 = alpha0 + fcoefa(3)*sin(2*coefs(2))
       delta0 = delta0 + fcoefd(2)*cos(coefs(2))
       delta0 = delta0 + fcoefd(3)*cos(2*coefs(2))
       W = W + fcoefW(2)*sin(coefs(2))
       W = W + fcoefW(3)*sin(2*coefs(2))
       dW = dW + fcoefdW(2)*cos(coefs(2))
       dW = dW + fcoefdW(3)*cos(2*coefs(2))
    ! Epimethe (Saturne)
    case(611)
       ! ne depand que de S1
       alpha0 = alpha0 + fcoefa(1)*sin(coefs(1))
       alpha0 = alpha0 + fcoefa(2)*sin(2*coefs(1))
       delta0 = delta0 + fcoefd(1)*cos(coefs(1))
       delta0 = delta0 + fcoefd(2)*cos(2*coefs(1))
       W = W + fcoefW(1)*sin(coefs(1))
       W = W + fcoefW(2)*sin(2*coefs(1))
       dW = dW + fcoefdW(1)*cos(coefs(1))
       dW = dW + fcoefdW(2)*cos(2*coefs(1))
    ! Miranda  (Uranus)
    case(705)
       alpha0 = alpha0 + fcoefa(11)*sin(coefs(11))
       alpha0 = alpha0 + fcoefa(12)*sin(2*coefs(11))
       delta0 = delta0 + fcoefd(11)*cos(coefs(11))
       delta0 = delta0 + fcoefd(12)*cos(2*coefs(11))
       W = W + fcoefW(10)*sin(coefs(11))
       W = W + fcoefW(11)*sin(2*coefs(11))
       W = W + fcoefW(12)*sin(coefs(12))
       W = W + fcoefW(13)*sin(2*coefs(12))
       dW = dW + fcoefW(10)*cos(coefs(11))
       dW = dW + fcoefW(11)*cos(2*coefs(11))
       dW = dW + fcoefW(12)*cos(coefs(12))
       dW = dW + fcoefW(13)*cos(2*coefs(12))
    ! Neptune
    case(899)
          ! un seul N
       alpha0=alpha0 + fcoefa(1)*sin(coefs(1))
       delta0=delta0 + fcoefd(1)*cos(coefs(1))
       W=W + fcoefW(1)*sin(coefs(1))
       dW=dW + fcoefdW(1)*cos(coefs(1))
    ! Triton (Neptune)
    case(801)
       do ii =1, 9
          ! ne depend que de N7
          alpha0=alpha0 + fcoefa(ii)*sin(ii*coefs(7))
          delta0=delta0 + fcoefd(ii)*cos(ii*coefs(7))
          W=W + fcoefW(ii)*sin(ii*coefs(7))
          dW=dW + fcoefdW(ii)*cos(ii*coefs(7))
       enddo
    ! Naiad (Neptune)
    case(803)
       alpha0=alpha0 + fcoefa(1)*sin(coefs(1))
       alpha0=alpha0 + fcoefa(2)*sin(coefs(1))
       alpha0=alpha0 + fcoefa(8)*sin(coefs(8))
       delta0=delta0 + fcoefd(1)*cos(coefs(1))
       delta0=delta0 + fcoefd(2)*cos(coefs(1))
       delta0=delta0 + fcoefd(8)*cos(coefs(8))
       W=W + fcoefW(1)*sin(coefs(1))
       W=W + fcoefW(2)*sin(coefs(1))
       W=W + fcoefW(8)*sin(coefs(8))
       dW=dW + fcoefdW(1)*cos(coefs(1))
       dW=dW + fcoefdW(2)*cos(coefs(1))
       dW=dW + fcoefdW(8)*cos(coefs(8))

    ! Satellites de Jupiter gallileens
    ! Satellites de Saturne
    ! Satellites d'Uranus
    ! Satellites de Neptune
    case(501,502,503,504,&
         601,603,605,606, &
         701,702,703,704,706,707,708,709,710,711,712,713,714,715, &
         804,805,806,807,808)
       do ii=1,ncoefs
          alpha0 = alpha0 + fcoefa(ii)*sin(coefs(ii))
          delta0 = delta0 + fcoefd(ii)*cos(coefs(ii))
          W = W + fcoefW(ii)*sin(coefs(ii))
          dW = dW + fcoefdW(ii)*cos(coefs(ii))
       enddo
    ! Planètes : rien
    case (199,299,399,499,599,699,799,999)

    ! sans oublier les corps dont le mouvement est linéaire
    case (515,516,602,604,608,609,612,613,614,615,616,617,618,901)

    ! Cas d'erreur
    case default
       alpha0 = 0.0_PM_REEL
       delta0 = 0.0_PM_REEL
       W = 0.0_PM_REEL
       dW = 0.0_PM_REEL
       write(errtab(1),*) numpla
       errtab(2) =  "UAI91"
       call MSP_signaler_message(cle_mes="CPS_ERR_CORPSNONDEC", &
            routine="cps_poletsid91", &
            partie_variable=errtab)
    end select

! Fin générale à toutes les sorties possibles
9999 continue

    ! desallocation des tableaux externes
    if (associated(coefs))   deallocate(coefs,   stat=iostat)
    if (associated(coefsd1)) deallocate(coefsd1, stat=iostat)

    ! desallocation des tableaux internes
    if (associated(fcoefa))  deallocate(fcoefa,  stat=iostat)
    if (associated(fcoefd))  deallocate(fcoefd,  stat=iostat)
    if (associated(fcoefW))  deallocate(fcoefW,  stat=iostat)
    if (associated(fcoefdW)) deallocate(fcoefdW, stat=iostat)

  end subroutine cps_poletsid91


  subroutine cps_getcoefpoly91(numpla,ca0, ca1, cd0, cd1,cW0, cW1,ca2,cd2, cW2)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getcoefpoly91
!
!$Resume
!  Routine donnant les coefficients polynomiaux nécessaire au calcul des
!  coordonnées standards et du temps sidéral (et sa dérivée) pour la théorie
!  UAI 91
!
!$Description
!  Routine donnant les coefficients polynomiaux nécessaire au calcul des
!  coordonnées standards et du temps sidéral (et sa dérivée) pour la théorie
!  UAI 91
!
!$Auteur
!  Julien Bouillant (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getcoefpoly91(numpla,ca0, ca1, cd0, cd1,cW0, cW1,ca2,cd2, cW2)
!.    real(kind=PM_REEL) :: ca0, ca1, cd0, cd1,cW0, cW1,ca2,cd2, cW2
!.    integer :: numpla
!
!$Arguments
!>E     numpla  :<integer>   code du corps étudié
!>S     ca0     :<PM_REEL>   coefficient polynomial cst de l'ascension droite
!>S     ca1     :<PM_REEL>   coefficient polynomial cst de l'ascension droite
!>S     cd0     :<PM_REEL>   coefficient polynomial cst de la déclinaison
!>S     cd1     :<PM_REEL>   coefficient polynomial en T de la déclinaison
!>S     cW0     :<PM_REEL>   coefficient polynomial en T du temps sidéral
!>S     cW1     :<PM_REEL>   coefficient polynomial en T du temps sidéral
!>S     ca2     :<PM_REEL>   coefficient polynomial en T**2 de l'ascension droite
!>S     cd2     :<PM_REEL>   coefficient polynomial en T**2 de la déclinaison
!>S     cW2     :<PM_REEL>   coefficient polynomial en T**2 du temps sidéral
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
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
   implicit none
   
   real(kind=PM_REEL), intent(out) :: ca0, ca1, cd0, cd1,cW0, cW1,ca2,cd2, cW2
   integer, intent(in) :: numpla

   ! traitement d'erreur
   character(len=30), dimension(2) :: errtab
   
   
   select case (numpla)
   ! Mercure
   case(199)
      ca0=281.01_PM_REEL
! Z! different MSLIB -0.033 (UAI94)
      ca1=-0.003_PM_REEL
      cd0=61.45_PM_REEL
      cd1=-0.005_PM_REEL
! Z! different MSLIB 329.68 (UAI94)
      cW0=329.71_PM_REEL
      cW1=6.1385025_PM_REEL

      ! pas de coefs en T**2
      ca2=0._PM_REEL
      cd2=0._PM_REEL
      cW2=0._PM_REEL
   ! Venus
   case(299)
      ca0=272.76_PM_REEL
      ca1=0._PM_REEL
      cd0=67.16_PM_REEL
      cd1=0._PM_REEL
      cW0=160.20_PM_REEL
      cW1=-1.4813688_PM_REEL
      ! pas de coefs en T**2
      ca2=0._PM_REEL
      cd2=0._PM_REEL
      cW2=0._PM_REEL
   ! Terre
   case(399)
      ca0=0._PM_REEL
      ca1=-0.641_PM_REEL
      cd0=90._PM_REEL
      cd1=-0.557_PM_REEL
      cW0=190.16_PM_REEL
      cW1=360.9856235_PM_REEL
      ! pas de coefs en T**2
      ca2=0._PM_REEL
      cd2=0._PM_REEL
      cW2=0._PM_REEL
   ! Lune
   case(301) 
      ca0=270.000_PM_REEL
      ca1=0.003_PM_REEL
      cd0=66.541_PM_REEL
      cd1=0.013_PM_REEL
      cW0=38.317_PM_REEL
      cW1=13.1763582_PM_REEL
      ! pas de coefs en T**2
      ca2=0._PM_REEL
      cd2=0._PM_REEL
      cW2=0._PM_REEL
   ! Mars 
   case(499)
      ca0=317.681_PM_REEL
      ca1=-0.108_PM_REEL
      cd0=52.886_PM_REEL
      cd1=-0.061_PM_REEL
      cW0=176.868_PM_REEL
      cW1=350.8919830_PM_REEL
      ! pas de coefs en T**2
      ca2=0._PM_REEL
      cd2=0._PM_REEL
      cW2=0._PM_REEL
    ! Phobos
   case(401)
      ca0=317.68_PM_REEL
      ca1=-0.108_PM_REEL
      cd0=52.90_PM_REEL
      cd1=-0.061_PM_REEL
      cW0=35.06_PM_REEL
      cW1=1128.8445850_PM_REEL
      ! tsid T**2
      cW2=8.864_PM_REEL
      ! pas de coefs en T**2
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Deimos
   case(402)
      ca0=316.65_PM_REEL
      ca1=-0.108_PM_REEL
      cd0=53.52_PM_REEL
      cd1=-0.061_PM_REEL
      cW0=79.41_PM_REEL
      cW1=285.1618970_PM_REEL
      ! tsid T**2
      cW2=-0.520_PM_REEL
      ! pas de coefs en T**2
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Jupiter
   case(599)
      ca0=268.05_PM_REEL
      ca1=-0.009_PM_REEL
      cd0=64.49_PM_REEL
      cd1=0.003_PM_REEL
      cW0=284.95_PM_REEL
      cW1=870.5360000_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Io (Jupiter)
   case(501)
      ca0=268.05_PM_REEL
      ca1=-0.009_PM_REEL
      cd0=64.49_PM_REEL
      cd1=0.003_PM_REEL
      cW0=200.39_PM_REEL
      cW1=203.4889538_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Europa (Jupiter)
    case(502)
      ca0=268.08_PM_REEL
      ca1=-0.009_PM_REEL
      cd0=64.51_PM_REEL
      cd1=0.003_PM_REEL
      cW0=35.72_PM_REEL
      cW1=101.3747235_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Ganymede (Jupiter)
     case(503)
      ca0=268.20_PM_REEL
      ca1=-0.009_PM_REEL
      cd0=64.57_PM_REEL
      cd1=0.003_PM_REEL
      cW0=43.14_PM_REEL
      cW1=50.3176081_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Callisto (Jupiter)
     case(504)
      ca0=268.72_PM_REEL
      ca1=-0.009_PM_REEL
      cd0=64.83_PM_REEL
      cd1=0.003_PM_REEL
      cW0=259.67_PM_REEL
      cW1=21.5710715_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Amalthea (Jupiter)
   case(505)
      ca0=268.05_PM_REEL
      ca1=-0.009_PM_REEL
      cd0=64.49_PM_REEL
      cd1=0.003_PM_REEL
      cW0=231.67_PM_REEL
      cW1=722.6314560_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Thebe (Jupiter)
   case(514)
      ca0=268.05_PM_REEL
      ca1=-0.009_PM_REEL
      cd0=64.49_PM_REEL
      cd1=0.003_PM_REEL
      cW0=9.91_PM_REEL
      cW1=533.7005330_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
! Z!
   case(515)
      ca0=268.05_PM_REEL
      ca1=-0.009_PM_REEL
      cd0=64.49_PM_REEL
      cd1=0.003_PM_REEL
      cW0=5.75_PM_REEL
      cW1=1206.9950400_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
! Z!
   case(516)
      ca0=268.05_PM_REEL
      ca1=-0.009_PM_REEL
      cd0=64.49_PM_REEL
      cd1=0.003_PM_REEL
      cW0=302.24_PM_REEL
      cW1=1221.2489660_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Saturne
   case(699)
! Z! MSLIB 40.589 (UAI94)
      ca0=40.58_PM_REEL
      ca1=-0.036_PM_REEL
! Z! MSLIB 83.537 (UAI94)
      cd0=83.54_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=38.90_PM_REEL
      cW1=810.7939024_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
   ! Mimas (Saturne)
   case(601)
      ca0=40.66_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.52_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=337.46_PM_REEL
      cW1=381.9945550_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
   ! Encelade (Saturne)
   case(602)
      ca0=40.66_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.52_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=2.82_PM_REEL
      cW1=262.7318996_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Tethys (Saturne)
   case(603)
      ca0=40.66_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.52_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=10.45_PM_REEL
      cW1=190.6979085_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
! Z!
   case(604)
      ca0=40.66_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.52_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=357.00_PM_REEL
      cW1=131.5349316_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Reha (Saturne)
   case(605)
      ca0=40.38_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.55_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=235.16_PM_REEL
      cW1=79.6900478_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Titan(Saturne)
   case(606)
      ca0=36.41_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.94_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=189.64_PM_REEL
      cW1=22.5769768_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
! Z!
   case(608)
      ca0=318.16_PM_REEL
      ca1=-3.949_PM_REEL
      cd0=75.03_PM_REEL
      cd1=-1.143_PM_REEL
      cW0=350.20_PM_REEL
      cW1=4.5379572_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
! Z!
   case(609)
      ca0=355.00_PM_REEL
      ca1=0._PM_REEL
      cd0=68.70_PM_REEL
      cd1=0._PM_REEL
      cW0=304.70_PM_REEL
      cW1=930.8338720_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Janus (Saturne)
   case(610)
      ca0=40.58_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.52_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=58.83_PM_REEL
      cW1=518.2359876_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Epimethe (Saturne)
   case(611)
      ca0=40.58_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.52_PM_REEL
      cd1=0.004_PM_REEL
      cW0=293.87_PM_REEL
      cW1=518.4907239_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
! Z!
   case(612)
      ca0=40.85_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.34_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=245.12_PM_REEL
      cW1=131.6174056_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
! Z!
   case(613)
      ca0=50.51_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=84.06_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=56.88_PM_REEL
      cW1=190.6979332_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
! Z!
   case(614)
      ca0=36.41_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=85.04_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=153.51_PM_REEL
      cW1=190.6742373_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
! Z!
   case(615)
      ca0=40.58_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.53_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=137.88_PM_REEL
      cW1=598.3060000_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
! Z!
   case(616)
      ca0=40.58_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.53_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=296.14_PM_REEL
      cW1=587.289000_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
! Z!
   case(617)
      ca0=40.58_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.53_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=162.92_PM_REEL
      cW1=572.7891000_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
! Z!
   case(618)
      ca0=40.6_PM_REEL
      ca1=-0.036_PM_REEL
      cd0=83.5_PM_REEL
      cd1=-0.004_PM_REEL
      cW0=48.8_PM_REEL
      cW1=626.0440000_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Uranus
   case(799)
! Z! MSLIB 257.311 (UAI94)
      ca0=257.43_PM_REEL
      ca1=0._PM_REEL
! Z! MSLIB  -15.175 (UAI94)
      cd0=-15.10_PM_REEL
      cd1=0._PM_REEL
      cW0=203.81_PM_REEL
      cW1=-501.1600928_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Ariel (Uranus)
   case(701)
      ca0=257.43_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.10_PM_REEL
      cd1=0._PM_REEL
      cW0=156.22_PM_REEL
      cW1=-142.8356681_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Umbriel (Uranus)
    case(702)
      ca0=257.43_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.10_PM_REEL
      cd1=0._PM_REEL
      cW0=108.05_PM_REEL
      cW1=-86.8688923_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Titania (Uranus)
    case(703)
      ca0=257.43_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.10_PM_REEL
      cd1=0._PM_REEL
      cW0=77.74_PM_REEL
      cW1=-41.3514316_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Oberon  (Uranus)
    case(704)
      ca0=257.43_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.10_PM_REEL
      cd1=0._PM_REEL
      cW0=6.77_PM_REEL
      cW1=-26.7394932_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Miranda  (Uranus)
    case(705)
      ca0=257.43_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.08_PM_REEL
      cd1=0._PM_REEL
      cW0=30.70_PM_REEL
      cW1=-254.6906892_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Cordelia (Uramus)
    case(706)
      ca0=257.31_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.18_PM_REEL
      cd1=0._PM_REEL
      cW0=127.69_PM_REEL
      cW1=-1074.5205730_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Ophelia (Uramus)
     case(707)
      ca0=257.31_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.18_PM_REEL
      cd1=0._PM_REEL
      cW0=130.35_PM_REEL
      cW1=-956.4068150_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Bianca (Uramus)
    case(708)
      ca0=257.31_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.18_PM_REEL
      cd1=0._PM_REEL
      cW0=105.46_PM_REEL
      cW1=-828.3914760_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Cressida (Uramus)
     case(709)
      ca0=257.31_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.18_PM_REEL
      cd1=0._PM_REEL
      cW0=59.16_PM_REEL
      cW1=-776.5816320_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Desdemona (Uranus)
    case(710)
      ca0=257.31_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.18_PM_REEL
      cd1=0._PM_REEL
      cW0=95.08_PM_REEL
      cW1=-760.0531690_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Juliet (Uranus)
    case(711)
      ca0=257.31_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.18_PM_REEL
      cd1=0._PM_REEL
      cW0=302.56_PM_REEL
      cW1=-730.1253660_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Portia (Uranus)
    case(712)
      ca0=257.31_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.18_PM_REEL
      cd1=0._PM_REEL
      cW0=25.03_PM_REEL
      cW1=-701.4865870_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Rosalind (Uranus)
    case(713)
      ca0=257.31_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.18_PM_REEL
      cd1=0._PM_REEL
      cW0=314.90_PM_REEL
      cW1=-644.6311260_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Belinda (Uranus)
    case(714)
      ca0=257.31_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.18_PM_REEL
      cd1=0._PM_REEL
      cW0=297.46_PM_REEL
      cW1=-577.3628170_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Puck (Uranus)
    case(715)
      ca0=257.31_PM_REEL
      ca1=0._PM_REEL
      cd0=-15.18_PM_REEL
      cd1=0._PM_REEL
      cW0=91.24_PM_REEL
      cW1=-472.5450690_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
     ! Neptune
  case(899)
      ca0=299.36_PM_REEL
      ca1=0._PM_REEL
      cd0=43.46_PM_REEL
      cd1=0._PM_REEL
      cW0=253.18_PM_REEL
      cW1=536.3128492_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Triton (Neptune)
   case(801)
      ca0=299.36_PM_REEL
      ca1=0._PM_REEL
      cd0=41.17_PM_REEL
      cd1=0._PM_REEL
      cW0=296.53_PM_REEL
      cW1=-61.2572637_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Naiad (Neptune)
   case(803)
      ca0=299.36_PM_REEL
      ca1=0._PM_REEL
      cd0=43.36_PM_REEL
      cd1=0._PM_REEL
      cW0=254.06_PM_REEL
      cW1=1222.8441209_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Thalassa (Neptune)
   case(804)
      ca0=299.36_PM_REEL
      ca1=0._PM_REEL
      cd0=43.45_PM_REEL
      cd1=0._PM_REEL
      cW0=102.06_PM_REEL
      cW1=1155.7555612_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Despina (Neptune)
   case(805)
      ca0=299.36_PM_REEL
      ca1=0._PM_REEL
      cd0=43.45_PM_REEL
      cd1=0._PM_REEL
      cW0=306.51_PM_REEL
      cW1=1075.7341562_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Galathea (Neptune)
   case(806)
      ca0=299.36_PM_REEL
      ca1=0._PM_REEL
      cd0=43.43_PM_REEL
      cd1=0._PM_REEL
      cW0=258.09_PM_REEL
      cW1=839.6597686_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
    ! Larissa (Neptune)
   case(807)
      ca0=299.36_PM_REEL
      ca1=0._PM_REEL
      cd0=43.41_PM_REEL
      cd1=0._PM_REEL
      cW0=179.41_PM_REEL
      cW1=649.0534470_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
   ! Proteus (Neptune)
   case(808)
      ca0=299.27_PM_REEL
      ca1=0._PM_REEL
      cd0=42.91_PM_REEL
      cd1=0._PM_REEL
      cW0=93.38_PM_REEL
      cW1=320.7654228_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
   ! Pluton
   case(999)
      ca0=313.02_PM_REEL 
      ca1=0._PM_REEL
      cd0=9.09_PM_REEL
      cd1=0._PM_REEL
      cW0=236.77_PM_REEL
      cW1=-56.3623195_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL
   ! Charon
   case(901)
      ca0=313.02_PM_REEL
      ca1=0._PM_REEL
      cd0=9.09_PM_REEL
      cd1=0._PM_REEL
      cW0=56.77_PM_REEL
      cW1=-56.3623195_PM_REEL
      ! pas de coefs en T**2
      cW2=0._PM_REEL
      ca2=0._PM_REEL
      cd2=0._PM_REEL

    ! Cas d'erreur
    case default
       write(errtab(1),*) numpla
       errtab(2) =  "UAI91"
       call MSP_signaler_message(cle_mes="CPS_ERR_CORPSNONDEC", &
            routine="cps_getcoefpoly91", &
            partie_variable=errtab)
   end select

 end subroutine cps_getcoefpoly91


  subroutine cps_calccoef91(codebary, d, T, ncoefs, coefs,coefsd1)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_calccoef91
!
!$Resume
!  Routine donnant les coefficients E/M/J/S/U/N pour la théorie UAI 91
!
!$Description
!  Routine donnant les coefficients E/M/J/S/U/N pour le calcul des 
!  coordonnées et du temps sidéral (et sa dérivée) dans la théorie 
!  UAI 91
!
!$Auteur
!  Julien Bouillant (Atos Origin)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_calccoef91(codebary, d, T, ncoefs, coefs,coefsd1)
!.    integer :: codebary
!.    real(kind=PM_REEL) :: d, T
!.    integer :: ncoefs
!.    real(kind=PM_REEL), dimension(ncoefs) :: coefs
!.    real(kind=PM_REEL), dimension(ncoefs) :: coefsd1
!
!$Arguments
!>E     codebary  :<integer>                code du corps central du corps étudié
!>E     d         :<PM_REEL>                interval de temps (siècle julien)
!>E     T         :<PM_REEL,DIM=(IN)>       interval de temps (jour)
!>E     ncoefs    :<integer>                nombre de coefficient
!>S     coefs     :<PM_REEL,DIM=(ncoefs)>   tableau de coefficient E/M/J/S/U/N
!>S     coefsd1   :<PM_REEL,DIM=(ncoefs)>   facteur de d dans le calcul de coefs
!                                           (pour la derivée)
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
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
   implicit none 
   
   !Arguments
   integer           , intent(IN) :: codebary
   real(kind=PM_REEL), intent(IN):: d, T
   integer, intent(in):: ncoefs
   real(kind=PM_REEL), dimension(ncoefs), intent(out) :: coefs
   real(kind=PM_REEL), dimension(ncoefs), intent(out) :: coefsd1
   
   ! Variables locales
   real(kind=PM_REEL), dimension(:), pointer :: coefs0 => NULL(), coefsd2 => NULL()
   real(kind=PM_REEL), dimension(:), pointer :: coefsT1 => NULL(), coefsT2 => NULL()
   integer :: ii, iostat

   ! traitement d'erreur
   character(len=30), dimension(2) :: errtab
      
   
   if (ncoefs>0) then
      allocate(coefs0(ncoefs))
      allocate(coefsT1(ncoefs))
      allocate(coefsT2(ncoefs))
      allocate(coefsd2(ncoefs))      

      coefs0(1:ncoefs)  = 0.
      coefsd1(1:ncoefs) = 0.
      coefsd2(1:ncoefs) = 0.
      coefsT1(1:ncoefs) = 0.
      coefsT2(1:ncoefs) = 0.
   endif

   !! si coefs E/M/J/S/U/N supplémentaires 
   select case (codebary)

   ! Rien pour Mercure et Venus : pas de satellites naturels connus
   case(1,2)

   ! Coefs pour les E1 a E5 pour les satellites de la Terre (en fait
   ! uniquement pour la Lune
   case(3)
      coefs0(1) = 125.045
      coefs0(2) = 250.090
      coefs0(3) = 260.008 
      coefs0(4) = 176.625 
      coefs0(5) = 357.529

      coefsd1(1) = -0.052992
      coefsd1(2) = -0.105984
      coefsd1(3) = -13.012001
      coefsd1(4) = 13.340716
      coefsd1(5) = 0.985600
   
   ! Coefs pour les M1/M2 pour les satellites de Mars
   case(4)
      coefs0(1) = 169.51
      coefs0(2) = 192.93
      coefs0(3) = 53.47 

      coefsd1(1) = -0.4357640
      coefsd1(2) = 1128.4096700
      coefsd1(3) = -0.0181510

      coefsT2(2) = 8.864

    ! Coefs pour les J1 a J8 pour les satellites de Jupiter
   case(5) 
      coefs0(1) = 73.32
      coefs0(2) = 198.54
      coefs0(3) = 283.90
      coefs0(4) = 355.80
      coefs0(5) = 119.90
      coefs0(6) = 229.80
      coefs0(7) = 352.25  
      coefs0(8) = 113.35

      coefsT1(1) = 91472.9
      coefsT1(2) = 44243.8
      coefsT1(3) = 4850.7
      coefsT1(4) = 1191.3
      coefsT1(5) = 262.1
      coefsT1(6) = 64.3
      coefsT1(7) = 2382.6
      coefsT1(8) = 6070.0

   ! Coefs pour les S1 a S7 pour les satellites de Saturne
   case(6)
      coefs0(1) = 353.32
      coefs0(2) = 28.72
      coefs0(3) = 177.40
      coefs0(4) = 300.00
      coefs0(5) = 316.45
      coefs0(6) = 345.20
      coefs0(7) = 29.80  

      coefsT1(1) = 75706.7
      coefsT1(2) = 75706.7
      coefsT1(3) = -36505.5
      coefsT1(4) = -7225.9
      coefsT1(5) = 506.2
      coefsT1(6) = -1016.3
      coefsT1(7) = -52.1

    ! Coefs pour les U1 a U16
   case(7)
      coefs0(1) = 115.75
      coefs0(2) = 141.69
      coefs0(3) = 135.03
      coefs0(4) =  61.77
      coefs0(5) = 249.32
      coefs0(6) = 43.86
      coefs0(7) =  77.66
      coefs0(8) = 157.36
      coefs0(9) = 101.81
      coefs0(10) = 138.64
      coefs0(11) = 102.23
      coefs0(12) = 316.41
      coefs0(13) = 304.01
      coefs0(14) = 308.71
      coefs0(15) = 340.82
      coefs0(16) = 259.14

      coefsT1(1) = 54991.87
      coefsT1(2) = 41887.66
      coefsT1(3) = 29927.35
      coefsT1(4) = 25733.59
      coefsT1(5) = 24471.46
      coefsT1(6) = 22278.41
      coefsT1(7) = 20289.42
      coefsT1(8) = 16652.76
      coefsT1(9) = 12872.63
      coefsT1(10) = 8061.81
      coefsT1(11) = -2024.22
      coefsT1(12) = 2863.96
      coefsT1(13) = -51.94
      coefsT1(14) = -93.17
      coefsT1(15) = -75.32
      coefsT1(16) = -504.81

    ! Coefs pour les N1 a N8
   case(8)
      coefs0(1) = 323.92
      coefs0(2) = 220.51
      coefs0(3) = 354.27
      coefs0(4) = 75.31
      coefs0(5) = 35.36
      coefs0(6) = 142.61
      coefs0(7) = 177.85
      coefs0(8) = 357.85

      coefsT1(1) = 62606.6
      coefsT1(2) = 55064.2
      coefsT1(3) = 46564.5
      coefsT1(4) = 26109.4
      coefsT1(5) = 14325.4
      coefsT1(6) = 2824.6
      coefsT1(7) = 52.316
      coefsT1(8) = 52.316

   ! Coefs pour N (Netpune seule)
   case(899)
      coefs0(1) = 357.85
      coefsT1(1) = 52.316

   ! Rien pour Pluton : modèle simplifié car couple Pluton/Charron mal connu
   case(9)

   ! Cas d'erreur
   case default
      write(errtab(1),*) codebary
      errtab(2) =  "UAI91"
      call MSP_signaler_message(cle_mes="CPS_ERR_CORPSNONDEC", &
           routine="cps_calccoef91", &
           partie_variable=errtab)
   end select
   
   ! Calcul des E/M/J/S/U/N
   if (ncoefs==0) then 
      return
   else
      do ii=1 , ncoefs
         coefs(ii)=coefs0(ii)+ coefsd1(ii)*d + coefsd2(ii)*d**2 + &
              coefsT1(ii)*T + coefsT2(ii)*T**2
      enddo
   endif

   ! desallocation des tableaux dynamiques
   deallocate(coefs0,  stat=iostat)
   deallocate(coefsT1, stat=iostat)
   deallocate(coefsT2, stat=iostat)
   deallocate(coefsd2, stat=iostat)
   
 end subroutine cps_calccoef91

end module cps_poles
