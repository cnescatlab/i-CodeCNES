module gs_gestion_bulletin

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  gs_gestion_bulletin
!
!$Resume
!  Module contenant la structure nécessaire à la gestion des popup 
!  de changement de repère dans le bulletin
!
!$Description
!  Module contenant la structure nécessaire à la gestion des popup 
!  de changement de repère dans le bulletin
!
!$Auteur
!  Cédric Martel (Atos Origin)
!
!$Version
!  $Id: GS_gestion_bulletin.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: GS_gestion_bulletin.F90,v $
!  Revision 1.3  2010/10/15 11:20:59  ogarat
!  VERSION::FA-ID:1331:15/10/2010:Ajout du FinHistorique
!
!  Revision 1.2  2009/11/17 14:15:49  kvernelo
!  AQ:Correction declarations apres controles g95
!
!  Revision 1.1  2009/10/12 13:13:06  cmartel
!  FA-ID 970 : Ajout d'un module de gestion du bulletin
!
!
!$FinHistorique
!
!$Usage
!  use gs_gestion_bulletin
!
!$Structure
!
!: gs_gestion_chgt_repere : 
!>     sequence   : <>         
!>     blocr      : <logical>  Booléen contrôlant l'affichage des données
!                              de changement de repère
!>     bloct      : <logical>  Booléen contrôlant l'affichage des données 
!                              de changement de type de bulletin
!>     validr     : <logical>  Booléen indiquant une validation par l'utilisateur 
!                              du changement de repère
!>     validt     : <logical>  Booléen indiquant une validation par l'utilisateur 
!                              de changement de type de bulletin
!
!$Global
!
!>  GS_GESTION_DEFAUT   : <gs_gestion_chgt_repere>  Structure par défaut
!
!$Common
!
!$Routines
!
!$Fonctions
!
!$Include
!
!$Module
!
!$Interface
!#V
!#
!
!$Remarques
!  La structure GS_GESTION_DEFAUT permet de simplifier l'utilisation du bulletin
!  seulement si il n'y en a qu'une seule instance dans le programme appelant.
!  Se référer au manuel utilisateur pour l'utilisation de GS_BULLETIN_IP.
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: GS_gestion_bulletin.F90 69 2012-09-11 08:33:34Z ffsm $'


  type gs_gestion_chgt_repere 
     sequence

   logical :: blocr = .false.
   logical :: bloct = .false.
   logical :: validr = .false.
   logical :: validt = .false.
 
  end type gs_gestion_chgt_repere

  type(gs_gestion_chgt_repere),save :: GS_GESTION_DEFAUT

end module gs_gestion_bulletin
