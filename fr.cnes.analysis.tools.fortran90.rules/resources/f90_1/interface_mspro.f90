module interface_mspro

! (C) Copyright CNES - MSPRO - 2000-2005

!************************************************************************
!
! But:  Definition des interfaces explicites des routines utilisateur MSPRO. 
! ===
!
! Note d'utilisation:  Ce module est accessible a l'utilisateur via le module mspro.
! ==================
!
!$Historique
! ==========
!   + Version 0.1 : creation
!                         (Date: 10/2000 - Realisation: Veronique Lepine)
!   + Version 1.0 (sans DE) : ajout de nouveaux modules interfaces
!                         (Date: 01/2001 - Realisation: Veronique Lepine)
!   + Version 2.0 (sans DE) : ajout de nouveaux modules interfaces
!                         (Date: 09/2001 - Realisation: Guylaine Prat et Mickael Hazak)
!   + Version 3.0 (DE globale 2) : suppression de modules interfaces pour les routines 
!                         passant dans la MSLIGHT
!                         (Date: 10/2002 - Realisation: Guylaine Prat)
!   + Version 3.0 (sans DE) : ajout de nouveaux modules interfaces
!                         (Date: 11/2002 - Realisation: Bruno Revelin)
!   + Version 4.0 (DE globale 8): suppression de modules interfaces pour les routines 
!                         passant dans la MSLIB90
!                         (Date: 11/2003 - Realisation: Veronique Lepine)
!   + Version 5.0 (DE globale 11): suppression de modules interfaces pour les routines 
!                         passant dans la MSLIB90
!                         (Date: 04/2004 - Realisation: Veronique Lepine)   
!   + Version 5.1 (sans DE) : ajout de nouveaux modules interfaces
!                         (Date: 09/2004 - Realisation: Bruno Revelin)
!   + Version 5.2 (sans DE) : ajout de nouveaux modules interfaces
!                         (Date: 01/2005 - Realisation: Bruno Revelin)
!   + Version 5.3 : DM-ID 381 : Integrer des routines du theme trajectoires interplanetaires
!                    suppression de modules interfaces
!                   (Date: 09/2005 - Realisation: Claire Fabre- Atos origin)
!   + Version 5.5 : DM-ID 413 : Calcul du systeme de parametres Vinfini d arrivee
!                   (Date: 05/2006 - Realisation: Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

   use mslib

  ! liste de tous les modules interfaces existants (uniquement des routines utilisateur)
  ! ==============================================
 
use int_attitude
use int_dates_mspro
use int_geophysique_mspro
use int_chgmnt_reperes_mspro
use int_utilitaires_mspro
use int_chgmnt_variables_mspro
use int_chapeau
use int_codes_retour

  implicit none 

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: interface_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'


  character(len=pm_longueur_info_utilisateur), private :: info_utilisateur = &
                     '@(#) Fichier MSPRO interface_mspro.f90: derniere modification V5.15 >'

end module interface_mspro

