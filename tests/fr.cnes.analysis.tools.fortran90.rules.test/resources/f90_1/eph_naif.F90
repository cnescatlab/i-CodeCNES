module eph_naif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  eph_naif
!
!$Resume
!  Module relatif à la méthode de calcul d'éphémérides NAIF
!
!$Description
!  Ce module contient le sous-programme de calcul d'éphémérides 
!  par la méthode NAIF. 
!
!$Auteur
!   Florence Vivares / Philippe Brémard (SchlumbergerSema)
!   (à partir de EI_pvnaif) 
!
!$Version
!   $Id: eph_naif.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!   $Log: eph_naif.F90,v $
!   Revision 1.4  2010/10/21 13:46:20  ogarat
!   VERSION::AQ::21/10/2010:Ajout du fin historique
!
!   Revision 1.3  2006/03/09 12:04:41  vivaresf
!   FA-ID 500 : trace de la DM 391
!
!   Revision 1.2  2005/12/08 18:39:04  vivaresf
!   Cartouches et vérification des déclarations
!
!   Revision 1.1.1.1  2005/12/07 07:23:09  vivaresf
!   Refonte de COMPAS
!   Revision 1.6  2005/11/07 15:56:24  bouillaj
!   DM-ID 391 : Amelioration qualite sur la LIBEPHEM
!   Revision 1.5  2005/05/09 14:17:32  vivaresf
!   V2-1, documentation : correction des cartouches
!   Revision 1.4  2005/03/09 09:12:04  vivaresf
!   Correction des cartouches
!   Revision 1.3  2004/12/17 14:58:11  vivaresf
!   Documentation
!   Revision 1.2  2004/05/25 13:54:15  vivaresf
!    Version V1_9 : sans la MECASPA
!   Revision 1.1.1.1  2004/04/02 09:07:24  vivaresf
!   Gestion de configuration locale
!   Revision 1.10  2004/01/13 09:16:26  bremard
!   Mise à jour cartouche
!   Revision 1.7  2004/01/07 16:16:48  bremard
!   Mise à jour cartouche
!   Revision 1.6  2001/12/18 16:12:51  vivaresf
!   aj documentation
!   Revision 1.5  2001/12/07 16:46:25  vivaresf
!   Presentation fm des cartouches
!   Revision 1.4  2001/12/05 15:16:38  bremard
!   PhB - Mise à jour des cartouches
!
!$FinHistorique
!
!$Usage
!  use eph_naif
!
!$Structure
!
!$Routines
!- eph_pvnaif
!
!$Fonctions
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
!
!$Voir-Aussi
!.  eph_pvnaif
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use msp_gestion_erreur
      use eph_constantes
      use eph_varglob
      implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: eph_naif.F90 69 2012-09-11 08:33:34Z ffsm $'


contains

      subroutine eph_pvnaif(numpla,ncorpsc,t1950,xpla,tau)

!***********************************************************************
!$<AM-V2.0>
!
!$Nom
!  eph_pvnaif
!
!$Resume
!       Calcul des éphémérides des planètes par une méthode NAIF
!
!$Description
!       Cette subroutine calcule les positions et vitesses de 
!       différentes planètes du Système solaire, grâce aux routines
!       NAIF et aux éphémérides du JPL.
!
!$Mots-cles
!        éphémérides NAIF
!
!$Usage
!  call eph_pvnaif(numpla,ncorpsc,t1950,xpla,[tau])
!.    real (KIND=pm_reel) :: t1950
!.    real (KIND=pm_reel), dimension(6) :: xpla
!.    integer :: numpla,ncorpsc
!.    real (KIND=pm_reel) :: tau
!
!$Arguments
!>E     numpla   :<integer>           numéro de la planète dont on veut la
!                                     position relativement à ncorpsc
!.		 	              Les planètes possibles sont variables 
!                                     suivant la théorie et le fichier chargé
!>E     ncorpsc  :<integer>           numéro de la planète centre du repère
!                                     dans lequel sont fournis les résultats
!>E     t1950    :<pm_reel>           date de calcul (B1950)
!>S     xpla     :<pm_reel,DIM=(6)>   coordonnées du vecteur numpla-ncorpsc
!                                     exprimées dans le repère en km et km/s
!>[S]   tau      :<pm_reel>           temps de parcours d'une onde entre les
!                                     deux planètes
!.      Si tau est présent, xpla donne les coordonnées vraies (corrigées de la vitesse de
!       la lumière), sinon, il s'agit des coordonnées apparentes.
!
!$Acces
!  PUBLIC
!
!$Remarques
!.     Le repère de sortie est le repère NAIF de référence (EME2000).
!.     La théorie utilisée est la dernière théorie NAIF chargée.
!
!$Voir-Aussi
!
!$<>
!***********************************************************************
     implicit none

! PARAMETRES
      real (KIND=pm_reel), intent(in) :: t1950
      real (KIND=pm_reel), dimension(6), intent(out) :: xpla
      integer, intent(in) :: numpla,ncorpsc
      real (KIND=pm_reel),optional, intent(out) :: tau

! VARIABLES LOCALES
      character (LEN=4) :: abcorr
      character (LEN=80) :: iers
      real (KIND=pm_reel) :: et, taul

! FONCTIONS (de la spicelib pour la gestion d'erreur)
      logical :: failed

! contrôle que la méthode est initialisée
      if(.not.initnaif) then
         call MSP_signaler_message(cle_mes="EPH_ERR_INITABS", &
              partie_variable=" NAIF ", & 
              routine="eph_pvnaif")
         goto 9999
      endif

! Initialisation de l'indicateur de calcul de correction d'aberration
      abcorr = 'none'
      if(present(tau)) abcorr='lt'

! temps en secondes
      et = (t1950 - eph_date_jj50_j2000) * 86400._PM_REEL

! appel NAIF
      call spkez(numpla,et,'J2000    ',abcorr,ncorpsc,xpla,taul)
      if( failed() ) then
         write(iers,*) "spkez avec cc=",ncorpsc," ci=", numpla
         call MSP_signaler_message(cle_mes="EPH_ERR_APPEL", &
              partie_variable=iers , &
              routine="eph_pvnaif")
         goto 9999
      endif

      if(present(tau)) tau=taul

! Gestion des erreurs
 9999 continue

    end subroutine eph_pvnaif

end module eph_naif


