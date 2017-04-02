subroutine psimu (ifinal,tfinal,vetat,jflag,calsim,planete)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  psimu
!
!$Resume
!  Sous-programme appelant PSIMU en Fortran 77.
!
!$Description
!  Sous-programme appelant PSIMU en Fortran 77.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: spsimu.F90 368 2013-02-19 14:43:59Z aadt $
!
!$Historique
!  $Log: spsimu.F90,v $
!  Revision 368  2013/02/19 aadt
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 1.11  2010/10/25 13:10:59  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.10  2008/12/02 16:56:14  tanguyy
!  AQ : mise à jour des cartouches avant livraison de PSIMU V9.3
!
!  Revision 1.9  2007/07/09 12:36:12  tanguyy
!  PSIMU V9.0 - DM-ID 688 : code NAIF (ex : 399) pour indiquer le mode d'utilisation PSIMU
!  Revision 1.8  2007/02/05 17:27:06  tanguyy
!  DM-ID 643 : modifs des interfaces pour le mode sous-programme
!  Revision 1.7  2006/10/17 09:54:31  tanguyy
!  Finalisation DM-ID 478 (AQ : suppression var inutilisees, commentaires)
!  Revision 1.6  2006/03/15 13:25:24  tanguyy
!  Livraison PSIMU V8-4 ; relecture code / suppression code mort / maj cartouches
!  Revision 1.5  2005/11/10 18:37:11  vivaresf
!  Mise à jour des cartouches
!  Revision 1.4  2005/01/28 10:39:42  fabrec
!  maj cartouches
!  Revision 1.3  2004/10/11 16:21:19  vivaresf
!  DM 81 : la date du bulletin dans ps_don_bul est en jour/secondes
!  Revision 1.2  2003/01/29 14:53:59  boschett
!  modifications
!  Revision 1.1.1.1  2002/09/30 14:59:35  laurent
!  Industrialisation PSIMU
!  Revision 1.7  2000/06/21 15:23:25  util_am
!  ajout d'un test d'erreur apres l'appel a psimu90
!  Revision 1.6  2000/04/17 10:58:18  util_am
!  Version multi_satellite en Fortran90
!  Revision 1.5  2000/02/08 09:54:08  util_am
!  Modification pour tenir compte d'un delta de date sur les lois lors d'une réinitialisation de bulletin
!  (=> Ajout du paramètre datbul pour les utilitaires de conversions)
!  Revision 1.4  1999/12/09 10:21:34  util_am
!  Ajout d'un mode de calcul simplifié avec sorties dans Greenwich
!  Revision 1.3  1999/10/26 11:00:13  util_am
!  Mise à jour des cartouches
!  Revision 1.2  1999/08/04 11:28:20  util_am
!  Prise en compte de la gestion des erreurs de MECASPA
!
!$FinHistorique
!
!$Acces
!  PUBLIC
!
!$Usage
!  call psimu (ifinal,tfinal,vetat,jflag,calsim,planete)
!.    integer :: planete
!.    integer :: ifinal,calsim
!.    real (KIND=pm_reel) :: tfinal
!.    real (KIND=pm_reel) :: vetat(nvmax)
!.    integer :: jflag
!
!$Arguments
!>E     ifinal   :<integer>               indice pour savoir si tfinal correspond à une durée (sec) ou une date (JJ)
!.                                        ifinal = 1 -> date de fin
!.                                        ifinal = 2 -> durée
!>E     tfinal   :<pm_reel>               date de fin ou durée de l'extrapolation
!>S     vetat    :<pm_reel,DIM=(nvmax)>   tableau des variables de sortie (t=tfinal)
!>S     jflag    :<integer>               indice permettant de connaitre le mode de sortie de PSIMU:
!.                                        jflag = 1 -> date finale atteinte
!.                                        jflag = 2 -> altitude finale atteinte
!.                                        jflag = -1 -> sortie en erreur
!>E     calsim   :<integer>               indice pour savoir si on sort toutes les variables ou non:
!.                                        icalsim = 0 -> on sort toutes les variables
!.                                        icalsim = 1 => calcul simplifié avec sorties dans le repère d'intégration
!.                                        icalsim = 2 => calcul simplifié avec sorties dans le repère d'intégration et dans Greenwich
!>E     planete  :<integer>               nom de la planète (argument optionnel, par défaut, "Terre")
!
!$Common
!
!$Routines
!- psimu90
!- psconv_psimu_vers_msp
!
!$Include
!
!$Module
!#V
!- ps_interface_psimu_don
!- ps_spsimu
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   use ps_interface_psimu_don
   use ps_spsimu

   implicit none

   integer, intent(IN)     :: planete
   integer, intent(IN)              :: ifinal,calsim
   real (KIND=pm_reel), intent(IN)  :: tfinal
   real (KIND=pm_reel), intent(OUT) :: vetat(nvmax)
   integer, intent(OUT)             :: jflag

   real (KIND=pm_reel)            :: datbul
   type (MSP_BULLETIN)            :: don_bul
   type (MSP_VEHICULE)            :: don_car
   type (MSP_MODELE)              :: don_mod
   type (MSP_MODVENT)             :: don_vent
   type (PS_STR_INTEGRATION)      :: don_int
   type (MSP_SCENARIO_LOI)        :: don_sep
   type (MSP_SCENARIO_LOI)        :: don_ati
   type (MSP_SCENARIO_LOI)        :: don_pro
   integer :: init

   init = kbul + 10*kcar + 100*kmod + 1000*kint + 10000*kati + 100000*ksep + 1000000*kpro

   ! initalisation  de datbul
   datbul = ps_don_bul%jjbul + (ps_don_bul%secbul/86400._pm_reel)


   select case (init)
      case (0)
         call psimu90 (ifinal,tfinal,vetat,jflag,calsim,planete)
      case (1)
         call psconv_psimu_vers_msp (ps_don_bul,don_bul)
         if ( MSP_ERREUR ) goto 100
         call psimu90 (ifinal,tfinal,vetat,jflag,calsim,planete,PBUL=don_bul)
      case (10)
         call psconv_psimu_vers_msp (ps_don_car,don_car)
         if ( MSP_ERREUR ) goto 100
         call psimu90 (ifinal,tfinal,vetat,jflag,calsim,planete,PCAR=don_car)
      case (100)
         call psconv_psimu_vers_msp (ps_don_mod,don_mod)
         if ( MSP_ERREUR ) goto 100
         
         call psconv_psimu_vers_msp (ps_don_mod,don_vent)
         if ( MSP_ERREUR ) goto 100

         call psimu90 (ifinal,tfinal,vetat,jflag,calsim,planete,PMOD=don_mod,PVENT=don_vent)
      case (1000)
         don_int = ps_don_int
         if ( MSP_ERREUR ) goto 100
         call psimu90 (ifinal,tfinal,vetat,jflag,calsim,planete,PINT=don_int)
      case (10000)
         call psconv_psimu_vers_msp (ps_don_ati,datbul,don_ati)
         if ( MSP_ERREUR ) goto 100
         call psimu90 (ifinal,tfinal,vetat,jflag,calsim,planete,PATI=don_ati)
      case (100000)
         call psconv_psimu_vers_msp (ps_don_sep,datbul,don_sep)
         if ( MSP_ERREUR ) goto 100
         call psimu90 (ifinal,tfinal,vetat,jflag,calsim,planete,PSEP=don_sep)
      case (1000000)
         call psconv_psimu_vers_msp (ps_don_pro,datbul,don_pro)
         if ( MSP_ERREUR ) goto 100
         call psimu90 (ifinal,tfinal,vetat,jflag,calsim,planete,PPRO=don_pro)
      case default
         call psconv_psimu_vers_msp (ps_don_bul,don_bul)
         if ( MSP_ERREUR ) goto 100
         call psconv_psimu_vers_msp (ps_don_pro,datbul,don_pro)
         if ( MSP_ERREUR ) goto 100
         call psconv_psimu_vers_msp (ps_don_sep,datbul,don_sep)
         if ( MSP_ERREUR ) goto 100
         call psconv_psimu_vers_msp (ps_don_ati,datbul,don_ati)
         if ( MSP_ERREUR ) goto 100
         call psconv_psimu_vers_msp (ps_don_car,don_car)
         if ( MSP_ERREUR ) goto 100
         call psconv_psimu_vers_msp (ps_don_mod,don_mod)
         if ( MSP_ERREUR ) goto 100
         call psconv_psimu_vers_msp (ps_don_mod,don_vent)
         if ( MSP_ERREUR ) goto 100
         don_int = ps_don_int
         if ( MSP_ERREUR ) goto 100
         call psimu90 (ifinal,tfinal,vetat,jflag,calsim,planete,&
                       don_bul,don_car,don_mod,don_vent,don_int,don_ati,don_sep,don_pro)
         if ( MSP_ERREUR ) goto 100
   end select

   100 continue
   if ( MSP_ERREUR ) then
      jflag = -1
      return
   endif

   kbul = 0
   kcar = 0
   kmod = 0
   kint = 0
   kati = 0
   ksep = 0
   kpro = 0

end subroutine psimu

