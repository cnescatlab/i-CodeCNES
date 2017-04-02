subroutine cps_rsphere(liste, nb, rsph, ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_rsphere
!
!$Resume
!  Calcul des sphères d'influence d'un tableau de corps
!
!$Description
!  Calcul des sphères d'influence d'un tableau de corps
!
!$Auteur
!  Florence VIVARES (ATOS Origin)
!
!$Acces
!  PUBLIC
!
!$Version
!  $Id: cps_rsphere.F90 360 2013-02-15 11:38:21Z aadt $
!
!$Historique
!  $Log: cps_rsphere.F90,v $
!  Revision 360  2013/02/14 aadt
!  DM-ID 1513: Montee de niveau Gfortran
!
!  Revision 1.6  2010/10/21 13:46:20  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.5  2008/10/01 14:27:42  cml
!  AQ : Mise a jour du cartouche pour les codes de retour
!
!  Revision 1.4  2008/08/04 13:10:01  gss
!  DM-ID 1058 : (portage g95) ajout de la gestion d'erreurs.
!
!  Revision 1.3  2006/04/11 09:41:56  bouillaj
!  Modification de la signature de la routine
!
!  Revision 1.2  2005/12/08 18:39:30  vivaresf
!  Cartouches et vérification des déclarations
!
!  Revision 1.1.1.1  2005/12/07 07:23:14  vivaresf
!  Refonte de COMPAS
!  Revision 1.7  2005/03/08 14:02:25  vivaresf
!  Marqueur Log et Id
!
!$FinHistorique
!
!$Usage
!  call cps_rsphere(liste, nb, rsph, ier)
!.    character*(*), dimension(nb) :: liste
!.    integer :: nb
!.    integer :: ier
!.    real(kind=PM_REEL), dimension(nb) :: rsph
!
!$Arguments
!>E     liste  :<LEN=??,DIM=(nb)>    Liste de 'nb' corps dont on desire connaître la
!                                    sphère d'influence
!>E     nb     :<integer>            Nombre de corps dans la liste
!>S     rsph   :<PM_REEL,DIM=(nb)>   Rayons des 'nb' spheres d'influences
!>S     ier    :<integer>            Code retour :
!.                    ier = 0 : tous les calculs ont été effectués 
!.                    ier < 0 : erreur
!.                    ier > 0 : nombre de valeurs manquantes dans COMPAS.
!                               Une valeur différente de 0 ne permet pas d'assurer la fiabilité des résultats
!
!$Common
!
!$Routines
!- cps_constante
!
!$Include
!
!$Module
!#V
!- cps_acces
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
  use mslib
  use cps_acces
  
  implicit none
  
  ! Parametres
  integer, dimension(nb), intent(in) :: liste
  integer, intent(in) :: nb
  integer, intent(out) ::ier
  real(kind=PM_REEL),  dimension(nb), intent(out) :: rsph
  
  
  ! Variables locales
  real(kind=PM_REEL),  dimension(275) :: mu, dga, muc, rap
  integer,  dimension(275) :: corpsc
  character(LEN=CPS_MAXLG) :: unite
  type(tm_orb_kep) :: kep
  real(kind=PM_REEL) :: date
  integer :: ii
  integer :: trouve
  

   ! Initialisation
    corpsc(1:275)=0
    muc(1:275)=0.0
    rap(1:275)=0.0
    dga(1:275)=0.0
    mu(1:275)=0.0

   ! Calcul
   ier=0
   do ii=1, nb
      rsph(ii)=0.0
      if(liste(ii).ne.1000000) then
         trouve=cps_getCsteThCourante(liste(ii), "mu", mu(ii), unite)
         if (trouve == CPS_ERR_DEF) then
            ier = ier + 1
            continue
         endif

         trouve=cps_getKeplerThCourante(liste(ii), kep, date)
         if (trouve == CPS_ERR_DEF) then
            ier = ier + 1
            continue
         endif

         dga(ii)=kep%a

         trouve=cps_getAtt(liste(ii), "corpsc", corpsc(ii))
         if (trouve == CPS_ERR_DEF) then
            ier = ier + 1
            continue
         endif

         trouve=cps_getCsteThCourante(corpsc(ii), "mu", muc(ii), unite)
         if (trouve == CPS_ERR_DEF) then
            ier = ier + 1
            continue
         endif

         ! rapport des mu (i.e. rapport des masses)
         if(muc(ii).ne. 0) rap(ii)  = mu(ii) / muc(ii)
         ! racine 0.4
         rsph(ii) = rap(ii) ** (0.4)
         ! facteur de proportion global corps central / corps d'interet
         rap(ii)  = rsph(ii) * 0.87055
         ! distance au corps central (approximative)
         rsph(ii) = dga(ii) * rap(ii)
         if (rsph(ii).eq.0) then
            ier=ier+1
         endif
      endif
   enddo
      
   
   if (MSP_ERREUR) then
      ier = ier*(-1)
   endif

end subroutine cps_rsphere
