module mspro

! (C) Copyright CNES - MSPRO - 2000 - 2003

!************************************************************************
!
! But:  acces simplifie pour l'utilisateur a tous les modules necessaires. 
! ===   Definition de la chaine de caracteres "version_MSPRO_id" donnant
!       via le binaire (commande what) le numero de version de la librairie.
!
! Note d'utilisation: l'utilisateur n'a besoin que de faire: use mspro
! ==================  En interne il n'est pas possible d'utiliser ce module
!                     a cause du module interface (compilation impossible).
!
!$Historique
! ==========
!   + Version 0.1 : creation
!                         (Date: 10/2000 - Realisation: Veronique Lepine)
!   + Version 3.0 (DE globale 3) : Remplacement de l'anti-slash par le caractere >
!                         (Date: 03/2003 - Realisation: Bruno Revelin)
!   + Version 3.1 (DE globale 4) : Modifications suite aux remarques qualite ATV
!                         (Date: 07/2003 - Realisation: Bruno Revelin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!     Revision 389  2013/02/25 ffsm
!     DM-ID 1513: Montee de niveau Gfortran
!
!$FinHistorique
!
!************************************************************************

 use interface_mspro          ! Definition de toutes les interfaces des routines utilisateurs.
 use parametre_mspro          ! Definition de parametres MSPRO (hors numeros de routines et codes retour)
 use type_mspro               ! Definition de tous les types MSPRO.
 use numero_routine_mspro     ! numeros des routines de la MSPRO
 use valeur_code_retour_mspro ! valeurs des codes retour de la MSPRO

 implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: mspro.f90 389 2013-02-25 14:03:50Z ffsm $'


 ! numero de version MSPRO pour le binaire (avec commande what)
 character(len=300),private  :: version_MSPRO_id = '@(#) '//pm_version_MSPRO//'>'

!................................................................................................................

 character(len=pm_longueur_info_utilisateur),private :: info_utilisateur = &
                    '@(#) Fichier MSPRO mspro.f90: derniere modification V6.0 >'

end module mspro
