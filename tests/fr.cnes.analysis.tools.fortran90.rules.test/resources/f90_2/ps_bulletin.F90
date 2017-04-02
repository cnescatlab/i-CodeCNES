module ps_bulletin

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ps_bulletin
!
!$Resume
!  Module décrivant les données du bulletin orbital initial.
!
!$Description
!  Module décrivant les données du bulletin orbital initial.
!
!$Auteur
!  J. F. GOESTER
!
!$Version
!  $Id: ps_bulletin.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ps_bulletin.F90,v $
!  Revision 1.16  2010/10/25 13:10:58  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.15  2008/12/02 16:39:16  tanguyy
!  AQ : mise à jour des cartouches avant livraison de PSIMU V9.3
!
!  Revision 1.14  2008/11/26 08:02:30  tanguyy
!  AQ : suppression de l'écart TE-TUC en jj et utilisation exclusive de l'écart en secondes
!  Revision 1.13  2008/05/02 14:37:19  tanguyy
!  FA-ID 865 : suppression de modprec (obsolete dans GSLIB et inutilise dans PSIMU)
!  Revision 1.12  2006/10/17 09:54:21  tanguyy
!  Finalisation DM-ID 478 (AQ : suppression var inutilisees, commentaires)
!  Revision 1.11  2006/03/15 13:25:17  tanguyy
!  Livraison PSIMU V8-4 ; relecture code / suppression code mort / maj cartouches
!  Revision 1.10  2005/11/10 18:37:04  vivaresf
!  Mise à jour des cartouches
!  Revision 1.9  2005/01/28 10:37:36  fabrec
!  maj cartouches
!  Revision 1.8  2005/01/17 15:24:12  fabrec
!  DM-ID 175 : maj des cartouches
!  Revision 1.7  2004/11/22 08:57:33  vivaresf
!  DM_ID 200 : lecture des requa et apla des utilises a l'IHM pour les conversions de type
!  Revision 1.6  2004/06/22 10:42:08  vivaresf
!  Mise a jour des entetes
!  Revision 1.5  2004/06/22 10:39:26  vivaresf
!  DM-ID 108 : Utilisation du rmu de psbul
!  Revision 1.4.2.1  2004/06/03 14:05:49  adm_ipsi
!  DM_108, version initiale
!  Revision 1.4  2004/01/15 16:33:24  adm_ipsi
!  DM-ID 10, les calculs internes de PSIMU se font en date TE
!  Revision 1.3  2003/02/14 15:34:44  rodier
!  PhB - Adaptation de la structure à la structure du common
!  Revision 1.2  2002/11/26 15:51:17  boschett
!  Ajout de implicit none
!  Revision 1.1.1.1  2002/09/30 14:59:34  laurent
!  Industrialisation PSIMU
!  Revision 1.5  2000/04/17 10:58:16  util_am
!  Version multi_satellite en Fortran90
!  Conversion des variables en type structure indice
!  Revision 1.4  1999/10/26 11:00:12  util_am
!  Mise à jour des cartouches
!  Revision 1.3  1999/08/31 11:56:10  util_am
!  Prise en compte des nouvelles échelles de date et de temps
!  Revision 1.2  1999/08/04 11:28:10  util_am
!  Prise en compte de la gestion des erreurs de MECASPA
!
!$FinHistorique
!
!$Usage
!  use ps_bulletin
!
!$Structure
!
!: PS_STR_INT_BULLETIN : 
!>     datbul          : <pm_reel>           date (Jours Juliens CNES)
!>     datbul_js       : <tm_jour_sec>       
!>     param           : <pm_reel,DIM=(6)>   paramètres de position/vitesse (dépend de iorb)
!>     rep             : <pm_reel,DIM=(10)>  paramètres du repère
!>     vitrot          : <pm_reel>           vitesse de rotation de la planète (rad/s)
!>     obli            : <pm_reel>           Obliquité
!>     polu            : <pm_reel>           Angle U du pôle vrai
!>     polv            : <pm_reel>           Angle V du pôle vrai
!>     mu              : <pm_reel>           Constante d'attraction du corps
!>     requa_r         : <pm_reel>           Rayon équatorial utilisé pour le changement de repère
!>     apla_r          : <pm_reel>           Aplatissement utilisé pour le changement de repère
!>     requa           : <pm_reel>           
!>     apla            : <pm_reel>           
!>     iorb            : <integer>           type de bulletin
!>     ech_temps_bul   : <integer>           Echelle de temps de la date du bulletin
!>     cle_date        : <integer>           clé fixant la date de définition du repère à une date prédéfinie
!>     num_pla         : <integer>           numéro de la planète
!>     num_cen         : <integer>           numéro du corps central
!>     datbul0         : <pm_reel>           date initale de l'extrapolation (Jours Juliens CNES) jj1950 TE
!>     datbul0_js      : <tm_jour_sec>       date initale de l'extrapolation (Jours/Secondes) jj1950 TE
!>     ecart_te_tuc    : <pm_reel>           Ecart TE/TUC à la date du bulletin, en secondes
!
!$Global
!
!>  str_bul   : <PS_STR_INT_BULLETIN,DIM=(PS_NVMAX)>  type structure bulletin
!$Common
!
!$Routines
!
!$Fonctions
!
!$Include
!
!$Module
!#V
!- MECASPA
!- ps_generalites
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

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ps_bulletin.F90 69 2012-09-11 08:33:34Z ffsm $'


   type PS_STR_INT_BULLETIN
      real (KIND=pm_reel) :: datbul               ! date (Jours Juliens CNES) en TE (dm478 : devra passer en jjsec)                           
      type (tm_jour_sec)  :: datbul_js            ! date (JJ/S en Jours Juliens 01/01/1950) (échelle TE)
      real (KIND=pm_reel), dimension(6) :: param  ! paramètres de position/vitesse (dépend de iorb)      
      real (KIND=pm_reel), dimension(10) :: rep   ! paramètres du repère                                 
      real (KIND=pm_reel) :: vitrot               ! vitesse de rotation de la planète (rad/s)            
      real (KIND=pm_reel) :: obli                 ! Obliquité                                            
      real (KIND=pm_reel) :: polu                 ! Angle U du pôle vrai                                 
      real (KIND=pm_reel) :: polv                 ! Angle V du pôle vrai                                 
      real (KIND=pm_reel) :: mu                   ! Constante d'attraction du corps                      
      real (KIND=pm_reel) :: requa_r              ! Rayon équatorial utilisé pour le changement de repère
      real (KIND=pm_reel) :: apla_r               ! Aplatissement utilisé pour le changement de repère   
      real (KIND=pm_reel) :: requa                !                                                      
      real (KIND=pm_reel) :: apla                 !                                                      
      integer :: iorb                             ! type de bulletin                                     
      integer :: ech_temps_bul                    ! Echelle de temps de la date du bulletin              
      integer :: cle_date                         ! clé fixant la date de définition du repère à une date prédéfinie          
      integer :: num_pla                          ! numéro de la planète                                
      integer :: num_cen                          ! numéro du corps central                             
      real (KIND=pm_reel) :: datbul0              ! date initale de l'extrapolation (Jours Juliens CNES) (TE)
      type (tm_jour_sec)  :: datbul0_js           ! date initale de l'extrapolation en jours/secondes (jj1950) (TE)
      real (kind=pm_reel) :: ecart_te_tuc         ! écart TUC - TE à la date du bulletin, exprimé en secondes
   end type PS_STR_INT_BULLETIN

   type (PS_STR_INT_BULLETIN) :: str_bul(PS_NVMAX)

end module ps_bulletin
