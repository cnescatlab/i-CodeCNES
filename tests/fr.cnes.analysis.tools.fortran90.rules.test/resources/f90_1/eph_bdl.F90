module eph_bdl

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  eph_bdl
!
!$Resume
!  Module des sous-programmes relatifs à la méthode BDL.
!
!$Description
!  Module contenant les sous-programmes de calcul pour 
!  la méthode BDL.
!
!$Auteur
! Philippe Brémard / Florence Vivares (SchlumbergerSema)
!
!$Version
!  $Id: eph_bdl.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: eph_bdl.F90,v $
!  Revision 1.7  2010/10/21 13:46:20  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.6  2009/01/06 10:53:20  cml
!  FA-ID 1151 : Amelioration du message d erreur pour les codes de corps
!
!  Revision 1.5  2008/10/02 11:43:35  cml
!  FA-ID 995 : Ajout d un test sur les messages d erreur de eph_syst_sol
!
!  Revision 1.4  2008/08/04 13:21:38  gss
!  DM-ID 1058 : (portage g95) initialisation à 0 des variables utilisées en
!  sortie de routines. Suppression des variables non utilisées. Test de la
!  validité du nombre de blocs lu.
!
!  Revision 1.3  2005/12/09 08:38:09  vivaresf
!  suppression de codes commentés
!
!  Revision 1.2  2005/12/08 18:39:02  vivaresf
!  Cartouches et vérification des déclarations
!
!  Revision 1.1.1.1  2005/12/07 07:23:08  vivaresf
!  Refonte de COMPAS
!  Revision 1.5  2005/05/09 14:17:31  vivaresf
!  V2-1, documentation : correction des cartouches
!  Revision 1.4  2005/03/09 09:12:02  vivaresf
!  Correction des cartouches
!  Revision 1.3  2004/12/17 14:58:10  vivaresf
!  Documentation
!  Revision 1.2  2004/05/25 13:54:14  vivaresf
!   Version V1_9 : sans la MECASPA
!  Revision 1.1.1.1  2004/04/02 09:07:24  vivaresf
!  Gestion de configuration locale
!  Revision 1.13  2004/01/13 09:16:26  bremard
!  Mise à jour cartouche
!  Revision 1.10  2004/01/07 16:18:41  bremard
!  Mise à jour cartouche
!  Revision 1.9  2003/12/30 16:10:23  bremard
!  Valeurs réelles en PM_REEL
!  Revision 1.8  2003/06/06 14:52:12  bremard
!  Particularisation des parametres
!  Revision 1.7  2002/09/26 15:42:03  vivaresf
!  Suppression de l'appel a la libenvint pour la VSOP82 fichier
!  Revision 1.6  2001/12/18 16:15:01  vivaresf
!  Maj documentation
!  Revision 1.5  2001/12/05 10:49:14  bremard
!  PhB - Mise à jour des cartouches
!  Revision 1.4  2001/12/05 10:31:11  bremard
!  PhB - Mise à jour des cartouches
!  Revision 1.3  2001/12/04 10:48:22  bremard
!  PhB - Mise à jour cartouche + mise en forme du code
!  Revision 1.2  2001/11/29 16:16:31  vivaresf
!  Cartouche
!
!$FinHistorique
!
!$Usage
!  use eph_bdl
!
!$Routines
!- eph_pvbdl
!- eph_syst_sol
!
!$Module
!#V
!- msp_gestion_erreur
!- eph_constantes
!- eph_varglob
!#
!
!$Interface
!#V
!#
!
!$Remarques
!
!$Mots-cles
!   Ephémérides , VSOP82
!
!$Voir-Aussi
!.  eph_pvbdl eph_syst_sol
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use msp_gestion_erreur
      use eph_constantes
      use eph_varglob, only : ephv_lfnbdl
      implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: eph_bdl.F90 69 2012-09-11 08:33:34Z ffsm $'


contains

  subroutine eph_pvbdl(numpla,ncorpsc,t1950,xpla)

!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  eph_pvbdl
!
!$Resume
!       Calcul des éphémérides des planètes par la méthode BDL
!
!$Description
!       Cette subroutine calcule les positions et vitesses de \
!       différentes planètes du Système solaire, grâce aux routines \
!       d'éphémérides du BDL. La théorie est la VSOP82. Les positions
!       et vitesses sont données dans le repère EME2000.
!
!$Auteur
!      Ph. Bremard/F. Vivares (SchlumbergerSema)
!
!$Mots-cles
!        éphémérides BDL
!
!$Usage
!  call eph_pvbdl(numpla,ncorpsc,t1950,xpla)
!.    real (KIND=pm_reel) :: t1950
!.    real (KIND=pm_reel), dimension(6) :: xpla
!.    integer :: numpla,ncorpsc
!
!$Arguments
!>E     numpla   :<integer>           numéro du corps d'intérêt
!>E     ncorpsc  :<integer>           numéro du corps central
!>E     t1950    :<pm_reel>           date de calcul (B1950)
!>S     xpla     :<pm_reel,DIM=(6)>   coordonnées du vecteur ncorpsc-numpla
!                           exprimées dans le repère EME2000, en km et km/s
!
!$Acces
!  PUBLIC
!
!$Remarques
!.     Le repère de sortie est le repère de référence EME2000.
!>     Corps possibles :
!.     - Soleil, Lune
!.     - 9 planètes + barycentre Terre/Lune
!.     - barycentre Système solaire (en corps central uniquement)
!.     - 4 astéroïdes (Cérès, Pallas, Junon, Vesta) 
!
!$Voir-Aussi
!     libenvint (EI_syst_sol)
!
!$<>
!***********************************************************************
    implicit none

! PARAMETRES
      real (KIND=pm_reel), intent(in) :: t1950
      real (KIND=pm_reel), dimension(6), intent(out) :: xpla
      integer, intent(in) :: numpla,ncorpsc

! VARIABLES LOCALES
      real (KIND=pm_reel) :: t1950v
      real (KIND=pm_reel),dimension(6) :: r 

      integer :: ier, i
      integer :: nplabdl, ncorbdl


! INITIALISATIONS
      nplabdl = 0
      ncorbdl = 0
      ier = 0

!/ début du code

! conversion des numéros des corps CNES en numéros BDL
      call eph_cnes2bdl(ncorpsc, ncorbdl, 1, ier)
!      if (MSP_erreur) return

      call eph_cnes2bdl(numpla, nplabdl, 1, ier)
!      if (MSP_erreur) return

! date en J2000
      t1950v = t1950 + eph_date_dj1950

! calcul des éphémérides avec la routine du bdl

      call eph_syst_sol(t1950v,nplabdl,ncorbdl,1,ephv_lfnbdl,r)

      if (MSP_gen_messages("eph_pvbdl")) return
      

! transformation en km et km/s

      do i = 1,3
         xpla(i) = r(i)*eph_uakm
      enddo
      do i = 4,6
         xpla(i) = r(i)*eph_uajkms
      enddo

      return
    end subroutine eph_pvbdl

      subroutine eph_syst_sol(tdj,icorps,icent,ider,nul,r2)

!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  eph_syst_sol
!
!$Resume
!       Calcul des coordonnées des principaux corps du Système solaire
!
!$Description
!       Calcul de coordonnées rectangulaires écliptiques moyennes des \
!	principaux corps du Système solaire à partir des éphémérides \
!	Tchebychev sources BDL82 dans le repère inertiel VSOP82 \
!       (équinoxe écliptique inertiels J2000).
!	Les éphémérides sont sorties dans le DE200 !!
!	ref : BDL-GF9405.
!
!$Auteur
!      Equipe interplanétaire        
!      Ph. Bremard/F. Vivares (SchlumbergerSema) (à partir de EI_syst_sol)
!
!$Mots-cles
!        coordonnées corps 
!
!$Usage
!  call eph_syst_sol(tdj,icorps,icent,ider,nul,r2)
!.    real(KIND=PM_REEL) :: tdj
!.    integer :: icorps
!.    integer :: icent
!.    integer :: ider
!.    integer :: nul
!.    real(KIND=PM_REEL), dimension(6) :: r2
!
!$Arguments
!>E     tdj     :<PM_REEL>           Jours juliens
!>E     icorps  :<integer>           Indice du corps
!.            icorps=1 Mercure
!.            icorps=2 Vénus
!.            icorps=3 Baryc.T-L
!.            icorps=4 Mars
!.            icorps=5 Jupiter  
!.            icorps=6 Saturne 
!.            icorps=07 Uranus  
!.            icorps=08 Neptune 
!.            icorps=09 Pluton  
!.            icorps=10 Lune    
!.            icorps=11 Soleil
!.            icorps=12 Terre
!.            icorps=13 Cérès
!.            icorps=14 Pallas
!.            icorps=15 Junon
!.            icorps=16 Vesta
!>E     icent   :<integer>           indice du centre du système de coordonnées,
!		icent a le même code que le paramètre icorps, avec en
!		plus le code 0 pour le barycentre du Système solaire.
!>E     ider    :<integer>           indice dérivées
!.		      ider = 0 : positions sans dérivées
!.		      ider = 1 : positions et dérivées
!>E     nul     :<integer>           numéro d'unité logique du fichier des éphémérides
!.		Tchebychev sources
!>S     r2      :<PM_REEL,DIM=(6)>   Table des coordonnées rectangulaires
!.		x = r(1): position (ua),  r(4): dérivée (ua/j).
!.		y = r(2): position (ua),  r(5): dérivée (ua/j).
!.		z = r(3): position (ua),  r(6): dérivée (ua/j).
!*		dans le DE 200 
!
!$Acces
!  PUBLIC
!
!$Remarques
!.      Les éphémérides Tchebychev sources BDL82 sont lues dans un 
!      fichier à accès direct (unité logique nul) qui doit être déjà ouvert.
!.       Deux réels A et B sont numériquement égaux lorsque :
!.       |A - B| <= 100 * EPSDP * (|A| + |B|)
!.       avec EPSDP (précision machine : 1e-15).
!
!$Voir-Aussi
!
!$<>
!***********************************************************************

      implicit none

!	CONSTANTES

!	ARGUMENTS

      real(KIND=PM_REEL), intent(in) :: tdj
      integer,            intent(in) :: icorps
      integer,            intent(in) :: icent
      integer,            intent(in) :: ider
      integer,            intent(in) :: nul
      real(KIND=PM_REEL), dimension(6), intent(out) :: r2

!	SOUS-PROGRAMMES UTILISES

!	COMMONS

!	VARIABLES LOCALES

      real(KIND=PM_REEL),DIMENSION(6)        :: r
      real(KIND=PM_REEL),DIMENSION(6,3),save :: resul
      real(KIND=PM_REEL),DIMENSION(636)      :: coef
      real(KIND=PM_REEL),DIMENSION(638),save :: bloc
      real(KIND=PM_REEL),DIMENSION(3,3)      :: rde200
      real(KIND=PM_REEL) :: f
      real(KIND=PM_REEL) :: tp,tf,cp,dt,cd
      real(KIND=PM_REEL) :: epsdp

      real(KIND=PM_REEL),save :: f1,f2,dj1,tdeb,tfin,delta

      integer, DIMENSION(3)    :: ic
      integer, DIMENSION(3,16),save :: loc
      integer :: i,j,k
      integer :: nder,irec,ncas,nsub,n,ncf,jp,jt
      integer :: l,is
      
      integer, save :: idf,nbloc,ndmot

      integer,save :: ideb = 0
      integer,save :: nul0 = 0
      integer,save :: irec0 = 0
      integer,save :: npc0 = 0
      integer,save :: ndc0 = 0
      integer,dimension(3),save :: ic0 = (/0,0,0/)
      integer,dimension(3),save :: ider0 = (/0,0,0/)
      real(KIND=PM_REEL),save :: pc2=2._PM_REEL
      real(KIND=PM_REEL),save :: dc3=100._PM_REEL
      real(KIND=PM_REEL),dimension(3),save :: tdj0=(/0._PM_REEL,0._PM_REEL,0._PM_REEL/)
      real(KIND=PM_REEL),dimension(15),save :: pc=(/1._PM_REEL,2._PM_REEL,(0._PM_REEL,i=1,13)/)
      real(KIND=PM_REEL),dimension(15),save :: dc=(/0._PM_REEL,1._PM_REEL,(0._PM_REEL,i=1,13)/)

      real(KIND=PM_REEL), parameter :: rmlt = 0.01230002_PM_REEL      ! Rapport de la masse Lune à la masse de la Terre.
      character (len=CPS_MAXLG), dimension(3) :: msp_mess ! Structure pour message d'erreur
    
!-------------------------------
!	INITIALISATION
!-------------------------------
      equivalence (coef(1),bloc(3))

!-----------------------------------------------------------------------
!     rde200 : On sort les résultats dans le repère de200
!-----------------------------------------------------------------------
      data rde200/ &
       1.000000000000_PM_REEL, -0.000000450877_PM_REEL,  0.000000000000_PM_REEL, &        
       0.000000413671_PM_REEL,  0.917482137087_PM_REEL,  0.397776982902_PM_REEL, &        
      -0.000000179348_PM_REEL, -0.397776982902_PM_REEL,  0.917482137087_PM_REEL/

!---------------------------------------
! CORPS DU SOUS-PROGRAMME <eph_syst_sol>
!---------------------------------------

      epsdp = 1.e-15_PM_REEL
!- DM1058 -----------
      f = 0._pm_reel
      dt = 0._pm_reel
!--------------------

!-----------------------------------------------------------------------
!     Lecture du descriptif et test des paramètres.
!-----------------------------------------------------------------------
      if ( (ideb==0) .or. (nul/=nul0) ) then
         ideb = 1
         read (nul,rec=1) idf,tdeb,tfin,delta,nbloc,ndmot,loc
         if (idf /= 2) then 
            call MSP_signaler_message(cle_mes="EPH_ERR_PARAM", partie_variable="idf", &
                                      routine="eph_syst_sol")
            return
         endif
         f1 = -rmlt/(1._PM_REEL+rmlt)
         f2 = 1._PM_REEL/(1._PM_REEL+rmlt)
      endif

      if ( nbloc < 0 ) then
         call MSP_signaler_message(cle_mes="EPH_ERR_PARAM", partie_variable="nbloc", &
                                   routine="eph_syst_sol")
         return
      endif

      if ( (tdj < tdeb) .or. (tdj > tfin) ) then
         ! Recopie pour affichage d'un message
         write(msp_mess(1),*) tdj
         write(msp_mess(2),*) tdeb
         write(msp_mess(3),*) tfin
         call MSP_signaler_message(cle_mes="EPH_ERR_BORNES_DATES", & 
            partie_variable=msp_mess, &
            routine="eph_syst_sol")
         return
      endif

      if ( (icorps < 1) .or. (icorps > 16) ) then
         ! Recopie pour affichage d'un message
         write(msp_mess(1),*) icorps
         write(msp_mess(2),*)"icorps"
         call MSP_signaler_message(cle_mes="EPH_ERR_PARAM_CORPS_DEMANDE", & 
                                   partie_variable=msp_mess, &
                                   routine="eph_syst_sol")
         return
     endif

      if ( (icent < 0) .or. (icent > 16) .or. (icent==icorps) ) then
         ! Recopie pour affichage d'un message
         write(msp_mess(1),*)icent
         write(msp_mess(2),*)"icent"
         call MSP_signaler_message(cle_mes="EPH_ERR_PARAM_CORPS_CENTRAL", & 
                                   partie_variable=msp_mess, &
                                   routine="eph_syst_sol")
         return
      endif

      if ( (ider < 0) .or. (ider > 1) ) then
         call MSP_signaler_message(cle_mes="EPH_ERR_PARAM", partie_variable="ider", &
                                   routine="eph_syst_sol")
         return
      endif

      if ( (icorps==12) .or. (icent==12) ) f = f1
      if ( (icorps==10) .or. (icent==10) ) f = f2
      nder = 3*(ider+1)
 
!-----------------------------------------------------------------------
!     Lecture des éphémérides Tchebychev sources BDL82.
!-----------------------------------------------------------------------
      irec = int( (tdj-tdeb)/delta+2._PM_REEL )
      if ( (abs(tdj-tfin)) < (100._PM_REEL*epsdp*(abs(tdj)+abs(tfin))) ) irec = irec-1
      if ( (nul/=nul0) .or. (irec/=irec0) ) then
         read (nul,rec=irec) (bloc(i),i=1,ndmot)
         dj1   = bloc(1)
         nul0  = nul
         irec0 = irec
      endif

      tf   = (tdj-dj1)/delta
      ncas = 0

!-----------------------------------------------------------------------
!     Distinction des différents cas.
!-----------------------------------------------------------------------
      if ( ((icent==12) .and. (icorps==10)) .or. &
     &     ((icent==10) .and. (icorps==12)) ) then
         ncas = 1
      else
         if (icent==0) then
            ncas = 2
         else
            if ( (icent==11) .or. (icorps==11) ) then
               ncas = 3
            else
               if ( (icorps==12) .or. (icent==12) .or. &
     &              (icorps==10) .or. (icent==10) ) then
                  ncas = 4
               else
                  ncas = 5
               endif
            endif
         endif
      endif

!-----------------------------------------------------------------------
!     Initialisation des différents cas.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!     case (1)
!-----------------------------------------------------------------------
      if (ncas==1) then
         nsub  = 1
         ic(1) = 10
      endif

!-----------------------------------------------------------------------
!      case (2)
!-----------------------------------------------------------------------
      if (ncas==2)then
         if (icorps==11) then
            nsub  = 1
            ic(1) = 11
         else
            if ( (icorps==12) .or. (icorps==10) ) then
               nsub  = 3
               ic(1) = 11
               ic(2) = 3
               ic(3) = 10
            else
               nsub  = 2
               ic(1) = 11
               ic(2) = icorps
            endif
         endif
      endif

!-----------------------------------------------------------------------
!      case (3)
!-----------------------------------------------------------------------
      if (ncas==3)then
         if ( (icorps==12) .or. (icent==12) .or. &
              (icorps==10) .or. (icent==10) ) then
            nsub  = 2
            ic(1) = 3
            ic(2) = 10
         else
            nsub = 1
            if (icent==11) ic(1) = icorps
            if (icorps==11) ic(1) = icent
         endif
      endif

!-----------------------------------------------------------------------
!      case (4)
!-----------------------------------------------------------------------
      if (ncas==4)then
         if ( (icorps==3) .or. (icent==3) ) then
            nsub  = 1
            ic(1) = 10
         else
            nsub = 3
            if ( (icent==12) .or. (icent==10) ) ic(1) = icorps
            if ((icorps==12) .or. (icorps==10)) ic(1) = icent
            ic(2) = 3
            ic(3) = 10
         endif
      endif

!-----------------------------------------------------------------------
!      case (5)
!-----------------------------------------------------------------------
      if (ncas==5) then
         nsub  = 2
         ic(1) = icorps
         ic(2) = icent
      endif

!-----------------------------------------------------------------------
!      end select
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!     Début boucle calcul.
!-----------------------------------------------------------------------
      do n=1,nsub
         if ( (ic(n)==ic0(n)) .and. &
              ( abs(tdj-tdj0(n)) < (100.d0*epsdp*(abs(tdj)+abs(tdj0(n))))) .and. &
              (ider==ider0(n)) ) CYCLE
         ic0(n)   = ic(n)
         tdj0(n)  = tdj
         ider0(n) = ider
         k        = ic(n)
         tp       = tf*loc(3,k)
         ncf      = loc(2,k)
         if (ncf==0) then
            call MSP_signaler_message(cle_mes="EPH_ERR_PARAM", partie_variable="ncf", &
                                      routine="eph_syst_sol")
            return
         endif
         l = loc(1,k) + 3*ncf*int(tp-int(tf))

!-----------------------------------------------------------------------
!        Calcul des positions.
!-----------------------------------------------------------------------
         cp = 2.d0*(tp-int(tp)+int(tf)) - 1._PM_REEL
         pc(2) = cp
         if ( (abs(pc(2)-pc2) > (100._PM_REEL*epsdp*(abs(pc(2))+abs(pc2)))) &
             .or. (ncf > npc0) ) then
            npc0 = ncf
            pc2  = pc(2)
            dt   = cp + cp
            do i=3,ncf
               pc(i) = dt*pc(i-1) - pc(i-2)
            enddo
         endif
         do i=1,3
            resul(i,n) = 0._PM_REEL
            do j=1,ncf
               jp = ncf - j + 1
               jt = l + ncf*(i-1) + jp-1
               resul(i,n) = resul(i,n) + pc(jp)*coef(jt)
            enddo
         enddo
         if (ider==0) CYCLE

!-----------------------------------------------------------------------
!        Calcul des dérivées.
!-----------------------------------------------------------------------
         cd = 2._PM_REEL*loc(3,k)/delta
         dc(3) = dt + dt
         if ( (abs(dc(3)-dc3) > (100._PM_REEL*epsdp*(abs(dc(3))+abs(dc3)))) &
             .or. (ncf > ndc0) ) then
            ndc0 = ncf
            dc3  = dc(3)
            do i=4,ncf
               dc(i) = dt*dc(i-1) + 2._PM_REEL*pc(i-1) - dc(i-2)
            enddo
         endif
         do i=1,3
            resul(i+3,n) = 0._PM_REEL
            do j=2,ncf
               jp = ncf - j + 2
               jt = l + ncf*(i-1) + jp - 1
               resul(i+3,n) = resul(i+3,n) + dc(jp)*coef(jt)
            enddo
            resul(i+3,n) = resul(i+3,n)*cd
         enddo

!-----------------------------------------------------------------------
!     Fin boucle calcul.
!-----------------------------------------------------------------------
      enddo

!-----------------------------------------------------------------------
!     Rangement des résultats des différents cas.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!     case (1)
!-----------------------------------------------------------------------
      if (ncas==1) then
         is = +1
         if (icent==10) is = -1
         do i=1,nder
            r(i) = resul(i,1)*is
         enddo
      endif

!-----------------------------------------------------------------------
!     case (2)
!-----------------------------------------------------------------------
      if (ncas==2) then
         do i=1,nder
            r(i) = resul(i,1)
            if (icorps.ne.11) then
               r(i) = r(i) + resul(i,2)
               if ( (icorps==12) .or. (icorps==10) ) then
                  r(i) = r(i) + f*resul(i,3)
               endif
            endif
         enddo
      endif

!-----------------------------------------------------------------------
!      case (3)
!-----------------------------------------------------------------------
      if (ncas==3) then
         is = +1
         if (icorps==11) is = -1
         do i=1,nder
            r(i) = resul(i,1)*is
            if ( (icorps==12) .or. (icent==12) .or. &
                 (icorps==10) .or. (icent==10) ) then
               r(i) = r(i) + f*resul(i,2)*is
            endif
         enddo
      endif

!-----------------------------------------------------------------------
!     case (4)
!-----------------------------------------------------------------------
      if (ncas==4) then
         is = +1
         if ((icorps==12).or.(icorps==10)) is = -1
         do i=1,nder
            if ( (icorps==3) .or. (icent==3) ) then
               r(i) = -f*resul(i,1)*is
            else
               r(i) = (resul(i,1) - resul(i,2) - f*resul(i,3))*is
            endif
         enddo
      endif

!-----------------------------------------------------------------------
!      case (5)
!-----------------------------------------------------------------------
      if (ncas==5) then
         do i=1,nder
            r(i) = resul(i,1) - resul(i,2)
         enddo
      endif

!-----------------------------------------------------------------------
!     Passage dans le repère DE200.
!-----------------------------------------------------------------------
      r2(1) = 0._PM_REEL
      r2(2) = 0._PM_REEL
      r2(3) = 0._PM_REEL
      if (ider /= 0) then
         r2(4) = 0._PM_REEL
         r2(5) = 0._PM_REEL
         r2(6) = 0._PM_REEL
      endif

      do i=1,3
         do j=1,3
            r2(i) = r2(i) + rde200(i,j)*r(j)
            if (ider==1) r2(i+3) = r2(i+3) + rde200(i,j)*r(j+3)
         enddo
      enddo

      return
      end subroutine eph_syst_sol

end module eph_bdl
