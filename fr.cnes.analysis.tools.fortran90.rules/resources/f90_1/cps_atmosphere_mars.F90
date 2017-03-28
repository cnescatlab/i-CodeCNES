module CPS_ATMOSPHERE_MARS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  CPS_ATMOSPHERE_MARS
!
!$Resume
!  Modèles d'Atmosphère martiens
!
!$Description
! Ce module contient 2 modèles :
! - le modele martien Russe (MARS90) porté depuis IO_e_atmars90
! - le modele martien exponentiel "expo"
!
!$Auteur
!  Florence VIVARES (ATOS Origin)
!
!$Version
!  $Id: cps_atmosphere_mars.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: cps_atmosphere_mars.F90,v $
!  Revision 1.11  2010/10/21 13:46:21  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.10  2010/04/30 14:12:37  jlrobin
!  V2.9::AQ::30/04/2010:merge de la branche de developpement modeles atmospheriques
!
!  Revision 1.9.2.2  2010/03/05 15:19:24  jlrobin
!  V2.9::DM-ID:1360:02/03/2010:Ajout du modele EMCD 4.3.1
!
!  Revision 1.9.2.1  2010/03/01 17:26:39  jlrobin
!  V2.9::DM-ID:1359:01/03/2010:Integration du modele d'atmosphere MARS GRAM 2005
!
!  Revision 1.9  2009/08/21 09:45:29  cml
!  DM-ID 1120 : Deplacement du modele exponentiel dans cps_atm_exp
!
!  Revision 1.8  2008/10/14 07:52:43  cml
!  DM-ID 1058 : Ajout d initialisations
!  Revision 1.7  2008/09/15 16:16:06  tanguyy
!  DM-ID 1091 : rajout du use cps_modele_emcd43
!  Revision 1.6  2008/08/11 09:33:17  cml
!  DM-ID 1091 : Ajout d une valeur d enumere pour l'EMCD 4.3
!  Revision 1.5  2008/04/08 06:50:47  vivaresf
!  FA-ID 1009 : suppression de cps_acces, récupération des
!  répertoires
!  Revision 1.4  2008/04/07 09:13:54  vivaresf
!  FA-ID 1009 :
!  - rajout de constantes pour chaque modèle
!  - suppression de cps_acces (obsolète)
!  - correction des cartouches
!  - rajout des modèles associés à chaque corps
!  Revision 1.3  2008/02/26 14:28:54  vivaresf
!  FA-ID 939 : écriture des messages d'erreur sur le stderr au lieu du stdout
!
!$FinHistorique
!
!$Usage
!  use CPS_ATMOSPHERE_MARS
!
!$Remarques
!
!$Mots-cles
!  ATMOSPHERE MARS90
!
!$Voir-Aussi
!#V
!.  apl ape getatm paratm find mars_russe_init
!#
!.  cps_atmars90
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use cps_utilisateur

  use cps_modele_emcd23
  use cps_modele_emcd31
  use cps_modele_emcd41
  use cps_modele_emcd42
  use cps_modele_emcd43
  use cps_modele_emcd431

  use cps_atm_gram
  use cps_atm_gram05

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_atmosphere_mars.F90 69 2012-09-11 08:33:34Z ffsm $'


  character(len=20), parameter :: cps_modatm_emcd23="EMCD2_3"
  character(len=20), parameter :: cps_modatm_emcd31="EMCD3_1"
  character(len=20), parameter :: cps_modatm_emcd41="EMCD4_1"
  character(len=20), parameter :: cps_modatm_emcd42="EMCD4_2"
  character(len=20), parameter :: cps_modatm_emcd43="EMCD4_3"
  character(len=20), parameter :: cps_modatm_emcd431="EMCD4_3_1"
  character(len=20), parameter :: cps_modatm_gram2001="GRAM2001"
  character(len=20), parameter :: cps_modatm_gram2005="GRAM2005"
  character(len=20), parameter :: cps_modatm_mars90="MARS90"
  character(len=20), parameter :: cps_modatm_exp="EXP"


  private getatm, paratm, find,apl,ape,mars_russe_init

   contains

      subroutine cps_atmars90 (indatm,coeff,z,t,p,r,a,xmu,ier)
       
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_atmars90
!
!$Resume
!  modèle d'atmosphère Martienne (0 --> 119 km) ; MA90 
!
!$Description
!  Modèle d'atmosphère russe, MA90. Les sources ont été récuperées de la
!  LIB_ATMARS, routine atmars90. Ce modèle est valable entre 0 et 119km.
!  Les données du modèle sont cherchées dans les données distribuées par
!  COMPAS (Modèles d'atmosphere martienne), sous le répertoire "data_mars_russe".
!  En cas d'absence des ressources COMPAS, les données sont
!  cherchées dans le sous-repertoire "data_mars_russe" du répertoire courant.
!
!$Auteur
!  13/03/95  TE/IS/MS/AS     
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_atmars90 (indatm,coeff,z,t,p,r,a,xmu,ier)
!.    integer :: indatm
!.    integer :: ier
!.    real(kind=PM_REEL) :: coeff
!.    real(kind=PM_REEL) :: z
!.    real(kind=PM_REEL) :: t,p,r,a,xmu
!
!$Arguments
!>E     indatm  :<integer>             indice pour le modele : 
!.                                1 : modèle nominal      
!.                                2 : modèle maximal       
!.                                3 : modèle minimal
!>E     coeff   :<PM_REEL>             coefficient multiplicatif de r        
!>E/S   z       :<PM_REEL>             altitude planétocentrique/Mars sphèrique (m)
!>S     t       :<PM_REEL,DIM=(out)>   température (k)      
!>S     p       :<PM_REEL>             pression (pa)           
!>S     r       :<PM_REEL>             densité (kg/m3)               
!>S     a       :<PM_REEL>             vitesse du son (m/s)            
!>S     xmu     :<PM_REEL>             viscosité dynamique (kg/m/s)        
!>S     ier     :<integer>             test d' erreur (OK si 0)        
!
!$Common
!
!$Routines
!- MSP_signaler_message
!#V
!- getatm
!- paratm
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
!       implicit double precision (a-h,o-z)
       integer, intent(in) :: indatm
       integer, intent(out) :: ier
       real(kind=PM_REEL), intent(in) :: coeff
       real(kind=PM_REEL), intent(inout) :: z
       real(kind=PM_REEL), intent(out) :: t,p,r,a,xmu

!
       real(kind=PM_REEL) :: ah(177),aroa(177),ata(177),apa(177),aa(177),&
                   anua(177)
       integer :: mua,nh
       common /atm/ ah,aroa,ata,apa,aa, anua,mua,nh
       real(kind=PM_REEL) :: nua, ar

       integer :: ifois
       logical :: zgrand
       data ifois/0/
!!
       ier=0
       ! Init ajouté pour suppression des warning
       nua = 0._pm_reel
       ar = 0._pm_reel
!
!
!      1er appel : chargement du common atm:
!      ====================================
!
       if (ifois.eq.0) then
         call getatm (indatm)
         ifois=1
       endif
!
!      Calcul des parametres:
!      =====================
!
!! modif Hubert le 16/11/98 ro=0 au dessus de 124 km
       zgrand = .FALSE.
       if (z.gt.119000.d0) then
         zgrand = .TRUE.
         z = 119000.d0
       endif
!
!! modif S ROSTAN le 04/01/05 sortie en erreur si altitude negative
       if (z.lt.0.d0) then
       call MSP_signaler_message (cle_mes="MSP_cal_atmosphere_001", Routine="atmars90")
       ier = -1
       return
       endif
!


       mua = 1
       call paratm(z,t,p,nua,r,ar)
!
!!!    a= dsqrt (1.359d0 *p/r)
!!!    a= dsqrt (1.330d0 *p/r)
       a= ar
!
!	Valeur moyenne de la viscosite dynamique :
!
       xmu = 1.25d-5
!
!! modif Hubert le 16/11/98 ro=0 au dessus de 124 km
       if (zgrand) then
         t = 0.
         p = 0.
         r = 0.
         a = 0.
         xmu = 0.
       endif
! rajout d'un coefficient multiplicatif sur ro
        r = r*coeff
!
     end subroutine cps_atmars90
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
     subroutine getatm (indatm)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  getatm
!
!$Resume
!  sous programme de lecture (ba/am)
!
!$Acces
!  PRIVE
!
!$Usage
!  call getatm (indatm)
!.    integer :: indatm
!
!$Arguments
!>E     indatm  :<integer>   
!
!$Common
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!       implicit double precision (a-h,o-z)

       integer, intent(in) :: indatm

       real(kind=PM_REEL) :: ah(177),aroa(177),ata(177),apa(177),aa(177),&
                   anua(177)
       integer :: mua,nh
        common /atm/ ah,aroa,ata,apa,aa,anua,mua,nh

       character *20 filei(3)
       logical ok
       integer ioerr, i

       logical inirep_mars_russe
       character*256 rep_mars_russe
       common /inirep_mars_russe/ inirep_mars_russe, rep_mars_russe

       data filei/'moyen','chaud','froid'/
!
!      lecture du fichier contenant le modele
!
!      Initialisation du nom du répertoire où aller lire les fichiers:
       ! si COMPAS connait son emplacement
       if ( .not. inirep_mars_russe ) call mars_russe_init()
       ! sinon
       if ( .not. inirep_mars_russe ) then
          inirep_mars_russe = .true.
          rep_mars_russe = "data_mars_russe"
       endif

       ! on verifie l'existence du fichier 
       inquire(file=trim(rep_mars_russe)//'/'// &
            filei(indatm), EXIST=ok, iostat=ioerr)

       if (.not.ok.or.ioerr.ne.0) then
          call MSP_signaler_message(cle_mes="CPS_ERR_FICABS", & 
               routine="getatm (cps_mars90)", &
               partie_variable=trim(filei(indatm)))
          return
       endif

       open(1,file=trim(rep_mars_russe)//'/'//filei(indatm),status='old')
!
       read(1,*)nh
       read(1,*)
!
       do 10,i=1,nh,1
        read(1,*)ah(i),aroa(i),ata(i),apa(i),anua(i),aa(i)

10     continue
!
       close(1)
     end  subroutine getatm
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!---------------------------------------------------------------------
!     sous - programmes envoyees par les russes
!---------------------------------------------------------------------
!
     subroutine paratm(h,ta,pa,nua,roa,a)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  paratm
!
!$Resume
!
!$Description
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call paratm(h,ta,pa,nua,roa,a)
!.    real(kind=PM_REEL) :: h,ta,pa,roa,a,nua
!
!$Arguments
!>E/S   h    :<PM_REEL>   
!>E/S   ta   :<PM_REEL>   
!>E/S   pa   :<PM_REEL>   
!>E/S   nua  :<PM_REEL>   
!>E/S   roa  :<PM_REEL>   
!>E/S   a    :<PM_REEL>   
!
!$Common
!
!$Routines
!#V
!- find
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

!       implicit double precision (a-h,o-z)
!
       real(kind=PM_REEL) :: h,ta,pa,roa,a,nua


       real(kind=PM_REEL) :: ah(177),aroa(177),ata(177),apa(177),aa(177),&
                   anua(177)
       integer :: mua,nh
        common /atm/ ah,aroa,ata,apa,aa, anua,mua,nh
       real(kind=PM_REEL)  :: pok
!
       ! Init pour suppression des warning
       pok=0._pm_reel
       call find(h/1000.d0, ah, nh, mua, pok)
       a = apl(mua, pok, aa  )
       ta= apl(mua, pok, ata )
       roa=ape(mua, pok, aroa)
       pa= ape(mua, pok, apa )
       nua=ape(mua, pok, anua)
       
     end subroutine paratm
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
     subroutine find(x, xx, km, kk, mno)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  find
!
!$Resume
!
!$Description
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!  call find(x, xx, km, kk, mno)
!.    real(kind=PM_REEL) :: x, xx(km), mno
!.    integer :: km, kk
!
!$Arguments
!>E/S   x    :<PM_REEL,DIM=(km)>   
!>E/S   xx   :<PM_REEL,DIM=(km)>   
!>E/S   km   :<integer>            
!>E/S   kk   :<integer>            
!>E/S   mno  :<PM_REEL>            
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!       implicit double precision (a-h,o-z)
       real(kind=PM_REEL) ::  x, xx(km), mno
       integer :: i, km, kk

       if (x .lt. xx(1) .or. x .gt. xx(km))  go to 3
       if (x .le. xx(2)) kk=2

       do 1 i=kk-1, km-1
          
          if (x .le. xx(i+1) .and. x .gt. xx(i)) go to 2
1      continue
!      enddo

2      kk = i
       mno = (x - xx(i)) / (xx(i+1) - xx(i))
          
       return
3      write (0,*) 'exit var from massiv !!'
       stop
     end subroutine find
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      real(kind=PM_REEL) function apl (n, mno, yy)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  apl
!
!$Resume
!
!$Description
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!.          real(kind=PM_REEL) function apl (n, mno, yy)
!.    real(kind=PM_REEL) :: yy(n+1), mno
!.    integer :: n
!
!$Arguments
!>E/S   n    :<integer>             
!>E/S   mno  :<PM_REEL>             
!>E/S   yy   :<PM_REEL,DIM=(n+1)>   
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        implicit double precision (a-h,o-z)
        real(kind=PM_REEL) :: yy(n+1), mno
        integer :: n

         apl=yy(n) + mno*(yy(n+1)-yy(n))
    end function apl

    real(kind=PM_REEL) function ape (n, mno, yy)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ape
!
!$Resume
!
!$Description
!
!$Auteur
!
!$Acces
!  PRIVE
!
!$Usage
!.        real(kind=PM_REEL) function ape (n, mno, yy)
!.    real(kind=PM_REEL) :: yy(n+1), mno
!.    integer :: n
!
!$Arguments
!>E/S   n    :<integer>             
!>E/S   mno  :<PM_REEL>             
!>E/S   yy   :<PM_REEL,DIM=(n+1)>   
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      implicit double precision (a-h,o-z)
      real(kind=PM_REEL) :: yy(n+1), mno, b
      integer :: n

      b = log( yy(n)/yy(n+1) )
      ape = yy(n) * exp( b*(-mno) )

    end function ape


    SUBROUTINE mars_russe_init ()


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  mars_russe_init
!
!$Resume
!     Initialisation du répertoire où aller lire les fichiers de données du modèle. 
!
!$Description
!     Initialisation du répertoire où aller lire les fichiers de données du modèle. 
!     Le common inirep_mars_russe reçoit le repertoire des modèles
!     d'atmosphere martienne distribué par COMPAS.
!
!$Auteur
!     J.F. GOESTER
!
!$Acces
!  PRIVE
!
!$Version
!     06/05/2000
!
!$Usage
!  call mars_russe_init ()
!
!$Remarques
!
!$Arguments
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use cps_utilisateur

      implicit none
      
      ! Common d'initialisation
      logical :: inirep_mars_russe
      character*256 :: rep_mars_russe
      common /inirep_mars_russe/ inirep_mars_russe, rep_mars_russe
      
      ! Variables locales
      integer :: trouve
      

      ! acces  par COMPAS au repertoire contenant les fichiers du modele
      trouve = cps_getFichierModele("atmosphere", "MARS90", rep_mars_russe, &
           rep=.true.)
      if (MSP_gen_messages("mars_russe_init")) return

      if (trouve >= 0 ) inirep_mars_russe = .true.
      
    end  subroutine mars_russe_init

end module CPS_ATMOSPHERE_MARS
