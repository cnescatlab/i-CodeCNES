module eph_kepler

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  eph_kepler
!
!$Resume
!  Ensemble de routines pour le calcul d'éphémérides par la méthode képlerienne
!
!$Description
!  Ensemble de routines pour le calcul d'éphémérides par la méthode képlerienne
!
!$Auteur
!  ATOS
!
!$Version
!  $Id: eph_kepler.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: eph_kepler.F90,v $
!  Revision 1.10  2010/10/21 13:46:20  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.9  2008/10/01 17:40:53  tanguyy
!  DM-ID 1058 : amélioration de la gestion mémoire
!
!  Revision 1.8  2008/08/04 13:30:38  gss
!  DM-ID 1058 : (portage g95) suppression des labels et des variables non utilisés.
!
!  Revision 1.7  2008/07/03 13:23:57  tanguyy
!  FA-ID 988 : remplacement de la variable fin par ok pour assurer l'initialisation de la variable ok en sortie
!
!  Revision 1.6  2006/05/30 15:21:29  vivaresf
!  Metriques : supression d'un niveau d'imbrication
!  Revision 1.5  2006/05/30 12:29:08  vivaresf
!  Separation COMPAS_UI,
!  suppression MSPRO
!  robustesse (iostat sur les deallocate)
!  Revision 1.4  2006/05/30 08:20:59  vivaresf
!  DM-ID 387 : remplacement de mx_var par mv_mv_kep_car
!  Revision 1.3  2006/03/09 12:04:42  vivaresf
!  FA-ID 500 : trace de la DM 391
!  Revision 1.2  2005/12/08 18:39:04  vivaresf
!  Cartouches et vérification des déclarations
!  Revision 1.1.1.1  2005/12/07 07:23:09  vivaresf
!  Refonte de COMPAS
!  Revision 1.5  2005/11/07 15:56:23  bouillaj
!  DM-ID 391 : Amelioration qualite sur la LIBEPHEM
!  Revision 1.4  2005/10/12 12:37:07  bouillaj
!  DM-ID 147 Extrapolation képlerienne
!  Revision 1.2  2004/12/17 14:58:10  vivaresf
!  Documentation
!  Revision 1.1.1.1  2004/04/02 09:07:24  vivaresf
!  Gestion de configuration locale
!  Revision 1.11  2003/06/12 15:13:14  vivaresf
!  Erreur dans la constante de conversion degrad
!  Revision 1.10  2002/09/26 15:45:50  vivaresf
!  Suppression de l'appel a la libenvint pour la propagation képlerienne
!  Revision 1.9  2002/03/15 11:03:21  vivaresf
!  Commentaire erronne
!  Revision 1.8  2001/12/18 16:13:45  vivaresf
!  Maj documentation
!  Revision 1.7  2001/12/07 16:45:26  vivaresf
!  Presentation fm des cartouches
!  Revision 1.6  2001/12/05 14:45:43  bremard
!  PhB - Mise à jour des cartouches+ conversion bulletin dans EME2000 (eph_initkep)
!  Revision 1.5  2001/12/03 16:33:33  vivaresf
!  Mise au point
!  Revision 1.4  2001/11/30 11:19:30  vivaresf
!  ua en variable globale externe
!  Revision 1.3  2001/11/29 16:14:13  vivaresf
!  Cartouche + gestion d'erreur
!  Revision 1.2  2001/11/28 08:18:44  vivaresf
!  Conversions mecaspa
!  Revision 1.1  2001/11/27 15:45:09  vivaresf
!  Version initiale
!
!$FinHistorique
!
!$Usage
!  use eph_kepler
!
!$Global
!
!>  eph_cste_kep     : <integer,parameter>                     
!#V
!>  ephv_initbul     : <logical,private>                       méthode initialisée ou non
!>  ephv_nbcorps     : <integer,private>                       Nombre de corps chargés 
!>  ephv_codeci      : <integer,DIM=(:),pointer,private>       corps d'intérêt    
!>  EPHV_BULLETINS   : <EPH_BULLETIN,DIM=(:),pointer,private>  bulletin
!#
!$Remarques
!
!$Structure
!
!: EPH_DATE : 
!>     date_ref       : <tm_jour_sec>      
!>     cle_date       : <integer>          
!>     origine_date   : <integer>          
!
!: EPH_TYPBUL : 
!>     iorb           : <integer>          
!>     param          : <PM_REEL,DIM=(6)>  
!>     mu             : <PM_REEL>          
!>     requa          : <PM_REEL>          
!>     apla           : <PM_REEL>          
!
!: EPH_BULLETIN : 
!>     typbul         : <EPH_TYPBUL>       
!>     date           : <EPH_DATE>         
!>     corpscen       : <integer>          
!
!$Module
!#V
!- MSLIB
!- eph_constantes
!- msp_gestion_erreur
!- mslib
!#
!
!$Routines
!- eph_initkep
!- eph_initkep_scal
!- eph_initkep_tab
!- eph_closekep
!- eph_pvkep
!- eph_eph_kep
!- eph_kep_verifkep
!
!$Fonctions
!
!$Interface
!> eph_initkep :  eph_initkep_scal, eph_initkep_tab
!#V
!#
!
!$Mots-cles
!
!$Voir-Aussi
!.  eph_initkep eph_initkep_scal eph_initkep_tab eph_closekep eph_pvkep eph_eph_kep eph_kep_verifkep
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use MSLIB
      use eph_constantes
      use msp_gestion_erreur

      implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: eph_kepler.F90 69 2012-09-11 08:33:34Z ffsm $'


      type EPH_TYPBUL
         integer :: iorb
         real(kind=PM_REEL), dimension(6) :: param
         real(kind=PM_REEL) :: mu
         real(kind=PM_REEL) :: requa       ! restent vide pour la propa képlerienne
         real(kind=PM_REEL) :: apla        !
      end type EPH_TYPBUL


      type EPH_DATE
         type(tm_jour_sec) :: date_ref
         integer :: cle_date
         integer :: origine_date
      end type EPH_DATE

      type EPH_BULLETIN
         type(EPH_TYPBUL) :: typbul
         type(EPH_DATE)  :: date
         integer :: corpscen
     end type EPH_BULLETIN

      ! bulletin Keplériens
      logical, save, private :: ephv_initbul=.false.
      integer, save, private :: ephv_nbcorps=0

      ! Valeur MSPRO (a confirmer)
      integer, parameter :: eph_cste_kep=1152

      integer, dimension(:), pointer, private :: ephv_codeci => NULL()                ! corps concernés      
      type(EPH_BULLETIN), dimension(:), pointer, private :: EPHV_BULLETINS => NULL()

! interfaces
  interface eph_initkep

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_initkep
!
!$Resume
! Chargement d'un bulletin képlerien
!
!$Description
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_initkep(bulletin, ci)
!.    type(EPH_bulletin) :: bulletin
!.    integer :: ci
!
!  call eph_initkep(bulletin, nb, ci)
!.    integer :: nb
!.    type(EPH_bulletin), dimension(nb) :: bulletin
!.    integer, dimension(nb) :: ci
!
!$Procedures
!- eph_initkep_scal
!- eph_initkep_tab
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     module procedure eph_initkep_scal, eph_initkep_tab
  end interface

contains

      subroutine eph_initkep_scal(bulletin, ci)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_initkep_scal
!
!$Resume
!   Chargement d'un bulletin képlerien
!
!$Description
!   Chargement d'un bulletin képlerien
!
!$Auteur
!   Florence VIVARES (SchlumbergerSema)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_initkep_scal(bulletin, ci)
!.    type(EPH_bulletin) :: bulletin
!.    integer :: ci
!
!$Arguments
!>E     bulletin  :<EPH_bulletin>   bulletin képlérien à charger 
!>E     ci        :<integer>        corps d'intérêt
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


        type(EPH_bulletin), intent(in) :: bulletin
        integer, intent(in) :: ci

        ! Variables locales
        integer :: iostat


! extension de ephv_bulletins
        type(EPH_BULLETIN), dimension(:), pointer :: BULLETIN_tmp => NULL()
        integer, dimension(:), pointer :: codeci_tmp => NULL()

!! Extension de ephv_bulletins et ephv_codeci de ephv_nbcorps à ephv_nbcorps + 1

        ephv_nbcorps=ephv_nbcorps+1
        if (associated(bulletin_tmp)) deallocate(bulletin_tmp, stat=iostat) 
        if (associated(codeci_tmp)) deallocate(codeci_tmp, stat=iostat) 

        if (associated(ephv_bulletins)) then
           allocate(bulletin_tmp(ephv_nbcorps-1))
           bulletin_tmp(:)=ephv_bulletins(:)
           deallocate(ephv_bulletins, stat=iostat)
           allocate(ephv_bulletins(ephv_nbcorps))
           ephv_bulletins(1:ephv_nbcorps-1) = bulletin_tmp
           ephv_bulletins(ephv_nbcorps) = bulletin
           deallocate(bulletin_tmp, stat=iostat)
        else 
           allocate(ephv_bulletins(1))
           Ephv_bulletins(1)=bulletin
        endif

        if (associated(ephv_codeci)) then
           allocate(codeci_tmp(ephv_nbcorps-1))
           codeci_tmp(:)=ephv_codeci(:)
           deallocate(ephv_codeci, stat=iostat)
           allocate(ephv_codeci(ephv_nbcorps))
           ephv_codeci(1:ephv_nbcorps-1) = codeci_tmp
           ephv_codeci(ephv_nbcorps) = ci
           deallocate(codeci_tmp, stat=iostat)
        else 
           allocate(ephv_codeci(1))
           ephv_codeci(1)=ci
        endif

!! Enregistrement

       ephv_bulletins(ephv_nbcorps)%typbul%iorb=eph_cste_kep
       ephv_bulletins(ephv_nbcorps)%typbul%param=bulletin%typbul%param
       ephv_bulletins(ephv_nbcorps)%typbul%mu=bulletin%typbul%mu



!! initialisation OK
        
        ephv_initbul=.true.


 end subroutine eph_initkep_scal

 subroutine eph_initkep_tab(bulletin, nb, ci)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_initkep_tab
!
!$Resume
!   Chargement d'un bulletin képlerien pour un ensemble de corps
!
!$Description
!
!$Auteur
!   Florence VIVARES (SchlumbergerSema)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_initkep_tab(bulletin, nb, ci)
!.    integer :: nb
!.    type(EPH_bulletin), dimension(nb) :: bulletin
!.    integer, dimension(nb) :: ci
!
!$Arguments
!>E     bulletin  :<EPH_bulletin,DIM=(nb)>   bulletins chargés
!>E     nb        :<integer>                 nombre de corps
!>E     ci        :<integer,DIM=(nb)>        corps d'intérêt
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

        implicit none 

! Arguments        
        integer, intent(in) :: nb
        type(EPH_bulletin), dimension(nb), intent(in) :: bulletin
        integer, dimension(nb), intent(in) :: ci

! Variables locales
        integer :: ii, iostat

!! entree de mx_var
!        integer :: pm_kep
!! sortie de mx_var
!        type(tm_code_retour) :: code_retour


! extension de ephv_bulletins
        type(EPH_BULLETIN), dimension(:), pointer :: BULLETIN_tmp => NULL()
        integer, dimension(:), pointer :: codeci_tmp => NULL()

!! Extension de ephv_bulletins et codeci de ephv_nbcorps à ephv_nbcorps + 1

        ephv_nbcorps=ephv_nbcorps+nb
        if (associated(bulletin_tmp)) deallocate(bulletin_tmp, stat=iostat) 
        if (associated(codeci_tmp)) deallocate(codeci_tmp, stat=iostat) 

        if (associated(ephv_bulletins)) then
           allocate(bulletin_tmp(ephv_nbcorps-nb))
           bulletin_tmp(:)=ephv_bulletins(:)
           deallocate(ephv_bulletins, stat=iostat)
           allocate(ephv_bulletins(ephv_nbcorps))
           ephv_bulletins(1:ephv_nbcorps-nb) = bulletin_tmp
           ephv_bulletins(ephv_nbcorps-nb+1:ephv_nbcorps) = bulletin
           deallocate(bulletin_tmp, stat=iostat)
        else 
           allocate(ephv_bulletins(1:nb))
           Ephv_bulletins(1:nb)=bulletin
        endif

        if (associated(ephv_codeci)) then
           allocate(codeci_tmp(ephv_nbcorps-nb))
           codeci_tmp(:)=ephv_codeci(:)
           deallocate(ephv_codeci, stat=iostat)
           allocate(ephv_codeci(ephv_nbcorps))
           ephv_codeci(1:ephv_nbcorps-nb) = codeci_tmp
           ephv_codeci(ephv_nbcorps-nb+1:ephv_nbcorps) = ci
           deallocate(codeci_tmp, stat=iostat)
        else 
           allocate(ephv_codeci(1:nb))
           Ephv_codeci(1:nb)=ci
        endif



!! Enregistrement


        do ii = 1, nb
           ephv_bulletins(ephv_nbcorps-nb+ii)%typbul%iorb=eph_cste_kep
           ephv_bulletins(ephv_nbcorps-nb+ii)%typbul%param=&
                bulletin(ii)%typbul%param
           ephv_bulletins(ephv_nbcorps-nb+ii)%typbul%mu=bulletin(ii)%typbul%mu
        enddo
        

!! initialisation OK
        
        ephv_initbul=.true.


      end subroutine eph_initkep_tab


      subroutine eph_closekep()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_closekep
!
!$Resume
!  Fermeture de la méthode képlerienne
!
!$Description
!  La méthode képlerienne n'est plus initialisée.  
!
!$Auteur
!   Florence Vivares / Philippe Brémard (SchlumbergerSema) 
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_closekep()
!
!$Arguments
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
! Variables locales
        integer :: iostat

! Fermeture
        ephv_nbcorps=0
        if(associated(ephv_bulletins)) then
           deallocate(ephv_bulletins, stat=iostat)
           if (iostat < 0) then
              call MSP_signaler_message(cle_mes="CPS_ERR_DESALLOC",&
                   routine="eph_closekep")
              return
           end if
        end if
        if(associated(ephv_codeci)) then
           deallocate(ephv_codeci, stat=iostat)
           if (iostat < 0) then
              call MSP_signaler_message(cle_mes="CPS_ERR_DESALLOC",&
                   routine="eph_closekep")
              return
           end if
        end if
        ephv_initbul=.false.
      end subroutine eph_closekep

      subroutine eph_pvkep(numc, numi,t1950,xpla)
!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  eph_pvkep
!
!$Resume
!       Calcul des éphémerides de certains corps a partir \
!       des paramètres d'orbite képleriens.
!
!$Description
!       Cette subroutine calcule les positions de différents corps \
!       du système Solaire, a partir des paramètres képleriens des \
!       orbites de ces corps. 
!       Ces paramètres doivent avoir été chargés avec eph_initkep.
!       Un seul bulletin est disponible à la fois.
!
!$Auteur
!   Florence VIVARES (SchlumbergerSema)
!   Philippe BREMARD (SchlumbergerSema)
!
!$Historique
!   5/12/2001 - Création à partir de EI_eph_kep (libenvint)
!   11/00   Creation pour l'asteroide Chaldea et la comete Temple1.
!
!$FinHistorique
!
!$Mots-cles
!        éphémérides képlerien
!
!$Usage
!  call eph_pvkep(numc, numi,t1950,xpla)
!.    real (KIND=pm_reel) :: t1950
!.    real (KIND=PM_REEL) , dimension(6) :: xpla
!.    integer :: numc,numi
!
!$Arguments
!>E     numc   :<integer>           corps central
!>E     numi   :<integer>           corps d'intérêt
!>E     t1950  :<pm_reel>           Date de calcul (B1950)
!>S     xpla   :<PM_REEL,DIM=(6)>   Coordonnées EME2000 du vecteur numc-numi
!                                    en km et km/s
!
!$Remarques
!
!$Voir-Aussi
!
!$<>
!***********************************************************************

        implicit none 

! PARAMETRES
      real (KIND=pm_reel), intent(in) :: t1950
      real (KIND=PM_REEL) , dimension(6), intent(out) :: xpla
      integer, intent(in) :: numc,numi

! VARIABLES LOCALES
      real (KIND=pm_reel), dimension(6) :: parkep, pvkep
      real(KIND=PM_REEL) :: tkep
      integer :: jj, ii
      integer, dimension(4) :: signe, sortieci
      type(EPH_BULLETIN), dimension(4) :: bull
      type(tm_code_retour) :: code_retour
      logical :: ok

! Initialisation
      if(.not.ephv_initbul) then
         ! méthode non initialisée
         call MSP_signaler_message(cle_mes="EPH_ERR_INITABS", &
              partie_variable=" Extrapolation ", & 
              routine="eph_pvkep")
         goto 9999
      endif

      xpla(:)=0._PM_REEL

! Vérifier que l'on peut aller de numc a numi
      
      call eph_kep_verifkep(numi, numc, sortieci=sortieci, sortiebull=bull, &
           signe=signe, ok=ok)
      if (MSP_gen_messages("eph_pvkep")) return
     
! Calcul des éphémérides du corps

      ! extraire les coefficients
      ! EPHV_BULLETINS est exprimé dans le repère EME2000

      do ii = 1, 4
         if (signe(ii) /= 0) then
            call md_joursec_jourfrac(bull(ii)%date%date_ref, jourfrac=tkep, &
                 code_retour=code_retour)
            if (code_retour%valeur < 0) then
               call MSP_signaler_message(cle_mes="EPH_ERR_APPEL", &
                    partie_variable="md_joursec_jourfrac", &
                    routine="eph_pvkep")
            else
               parkep = bull(ii)%typbul%param
               call eph_eph_kep(parkep, tkep, t1950, pvkep=pvkep, &
                    mu=bull(ii)%typbul%mu)

               if (MSP_gen_messages("eph_pvkep")) return
               do jj = 1, 6
                  xpla(jj) = xpla(jj) + signe(ii)*pvkep(jj)
               enddo
            endif
         endif
      enddo
      
9999  continue
      
    end subroutine eph_pvkep

    subroutine eph_eph_kep(parkep,tref,t1950,pvkep, mu)

!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  eph_eph_kep
!
!$Resume
!  Calcul de la position/vitesse à une date t1950 à partir
!  des paramètres képleriens d'une orbite héliocentrique 
!  prise à une date tref.
!
!$Description
!  Cette subroutine calcule la position/vitesse à une date t1950 
!  à partir des paramètres képleriens d'une orbite pris à une date tref.
!  La position/vitesse est donnée dans le repère dans lequel sont 
!  exprimés les paramètres képlerien. 
!
!$Auteur
!   Florence VIVARES (SchlumbergerSema)
!   Philippe BREMARD (SchlumbergerSema)
!   à partir de EI_eph_kep
!
!$Historique
!   5/12/2001 - Création à partir de EI_eph_kep (libenvint)
!   11/00   Création pour l'asteroide Chaldea et la comete Temple1.
!
!$FinHistorique
!
!$Mots-cles
!        éphémérides képlerien
!
!$Usage
!  call eph_eph_kep(parkep,tref,t1950,pvkep, mu)
!.    real(KIND=PM_REEL), dimension(6) :: parkep
!.    real(KIND=PM_REEL) :: tref
!.    real(KIND=PM_REEL) :: t1950
!.    real(KIND=PM_REEL), dimension(6) :: pvkep(6)
!.    real(kind=PM_REEL) :: mu
!
!$Arguments
!>E/S   parkep  :<PM_REEL,DIM=(6)>           Paramètres képleriens:  a(km),e,i,pom,gom,M (deg)    
!>E     tref    :<PM_REEL>                   Date à laquelle sont donnés les paramètres képleriens en JJ50
!>E     t1950   :<PM_REEL>                   Date de calcul (B1950)
!>S     pvkep   :<PM_REEL,DIM=(6),DIM=(6)>   Paramètres cartésiens à la date t1950.
!>E     mu      :<PM_REEL>                   Constante d'attraction du corps central
!
!$Remarques
!
!$Voir-Aussi
!
!$<>
!***********************************************************************
      use mslib

      implicit none

! Arguments

      real(KIND=PM_REEL), dimension(6), intent(inout) :: parkep
      real(KIND=PM_REEL),               intent(in) :: tref
      real(KIND=PM_REEL),               intent(in) :: t1950
      real(KIND=PM_REEL), dimension(6), intent(out) :: pvkep(6)
      real(kind=PM_REEL), intent(in) :: mu

! VARIABLES LOCALES
!      integer :: pm_kep, pm_car
      type(tm_code_retour) :: code_retour
      type(tm_orb_kep) ::  orb_kep
      integer :: i
      real(KIND=PM_REEL) :: n,Mt, inter

!-------------------------------
!	INITIALISATION
!-------------------------------

!-------------------------------
! CORPS DU SOUS-PROGRAMME <eph_eph_kep>
!-------------------------------

!-------------------------------
! Calcul du moyen mouvement (n) 
! et de l'anomalie moyenne a t1950
!-------------------------------
      inter=mu/(parkep(1)*parkep(1)*parkep(1))
      if (inter>0) then
         n=dsqrt(mu/(parkep(1)*parkep(1)*parkep(1))) 
      else
         call MSP_signaler_message(cle_mes="EPH_ERR_APPEL", &
                                   partie_variable="racine négative", & 
                                   routine="eph_eph_kep")
         return      
      endif
      Mt = n*(t1950-tref)*86400.e0_PM_REEL
      parkep(6) = parkep(6) + Mt 

!-------------------------------
! Passage en position/vitesse (m, m/s)
!-------------------------------

      pvkep(:)=0_PM_REEL
      orb_kep%a = parkep(1)
      orb_kep%e = parkep(2)
      orb_kep%i = parkep(3)
      orb_kep%pom = parkep(4)
      orb_kep%gom = parkep(5)
      orb_kep%M = parkep(6)

     call mv_kep_car(mu, orb_kep, pvkep(1:3), pvkep(4:6), &
           code_retour=code_retour)

      if (code_retour%valeur < 0) then 
         call MSP_signaler_message(cle_mes="EPH_ERR_APPEL", &
              partie_variable="mv_kep_car", routine="eph_eph_kep")
         return
      endif

!-------------------------------
! Passage en km et km/s
!-------------------------------
 
     do i=1,6 
         pvkep(i) = pvkep(i)/1000.e0_PM_REEL
      enddo 

    end subroutine eph_eph_kep



 subroutine eph_kep_verifkep(ni, nc, sortieci, sortiebull, signe, ok)

!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  eph_kep_verifkep
!
!$Resume
!
!$Description
!
!$Auteur
!   Florence VIVARES (SchlumbergerSema)
! !
!
!$Historique
!
!$FinHistorique
!
!$Mots-cles
!        éphémérides képlerien
!
!$Usage
!  call eph_kep_verifkep(ni, nc, sortieci, sortiebull, signe, ok)
!.    integer :: ni, nc
!.    integer, dimension(4) :: sortieci
!.    integer, dimension(4) :: signe
!.    type(EPH_BULLETIN), dimension(4) :: sortiebull
!.    logical :: ok
!
!$Arguments
!>E     ni          :<integer>                Code du corps d'entrée    
!>E     nc          :<integer>                Code du corps de sortie    
!>S     sortieci    :<integer,DIM=(4)>        Codes des corps pris en compte dans les bulletins
!>S     sortiebull  :<EPH_BULLETIN,DIM=(4)>   Bulletins qui entre en compte dans la sommation
!>S     signe       :<integer,DIM=(4)>        Signe à appliquer au bulletin dans la sommation des bulletins
!>S     ok          :<logical>                Si tous les corps ont été trouvés.
!
!$Remarques
!
!$Voir-Aussi
!
!$<>
!***********************************************************************

   implicit none

   !Arguments
   integer, intent(in) :: ni, nc
   integer, dimension(4), intent(out) :: sortieci
   integer, dimension(4), intent(out) ::  signe
   type(EPH_BULLETIN), dimension(4), intent(out) :: sortiebull
   logical, intent(out) :: ok
   
   ! Variables locales
   integer :: ii, jj, kk, ll
   
   if (ni == 10) then
      signe(1)=0
      signe(2)=0
   endif
   if (nc == 10) then
      signe(3)=0
      signe(4)=0
   endif
   sortieci(:)=0
   
   ok = .false.

   do jj = 1, ephv_nbcorps
      if (ni==ephv_codeci(jj)) then
         sortieci(1)=ni
         sortiebull(1)=EPHV_BULLETINS(jj)
         signe(1)=-1
         if ((EPHV_BULLETINS(jj)%corpscen /= 10).and. &
              (EPHV_BULLETINS(jj)%corpscen /= nc)) then
            do ii = 1, ephv_nbcorps
               if (EPHV_BULLETINS(jj)%corpscen == ephv_codeci(ii)) then
                  sortieci(2)=EPHV_BULLETINS(jj)%corpscen
                  sortiebull(2)=EPHV_BULLETINS(ii)
                  signe(2)=-1
               endif
            enddo
         else
            signe(2)=0
            if (EPHV_BULLETINS(jj)%corpscen == nc)   ok = .true.
         endif
      endif
   enddo
   
   if (ok) then
      signe(3)=0
      signe(4)=0
      return
   endif

   do kk = 1, ephv_nbcorps
      if (nc==ephv_codeci(kk)) then
         sortieci(1)=nc
         sortiebull(3)=EPHV_BULLETINS(kk)
         signe(3)=1
         if ((EPHV_BULLETINS(kk)%corpscen /= 10)) then
            do ll = 1, ephv_nbcorps
               if (EPHV_BULLETINS(kk)%corpscen == ephv_codeci(ll)) then
                  sortieci(4)=EPHV_BULLETINS(kk)%corpscen
                  sortiebull(4)=EPHV_BULLETINS(ll)
                  signe(4)=1
               endif
            enddo
         else
            signe(4)=0
         endif
      endif
   enddo
   

 end subroutine eph_kep_verifkep

end module eph_kepler
