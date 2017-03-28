module netcdf_f90
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  NETCDF_F90
!
!$Resume
!   Interface avec les fonctions de lecture C pour les modules EMCD 3.1 et 4.1
!
!$Description
!   Interface avec les fonctions de lecture C pour les modules EMCD 3.1 et 4.1
!
!$Auteur
!   H.M.Pau (Atos Origin)
!
!$Version
!  $Id: cps_netcdf_interface.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: cps_netcdf_interface.F90,v $
!  Revision 1.8  2010/10/21 13:46:21  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.7  2007/10/30 13:31:11  jpi
!  DM-ID 551 : premiere version du modele EMCD42
!
!  Revision 1.6  2007/10/25 09:31:13  jpi
!  DM-ID 551 : nf_get_var_int2(:,:) pour EMCD 4.2
!
!  Revision 1.5  2007/10/24 07:55:27  jpi
!  DM-ID 551 : nf_get_var_int2 pour EMCD 4.2
!
!  Revision 1.4  2007/10/23 16:30:44  vivaresf
!  DM-ID 551 : nf_get_var_int2 pour EMCD 4.2
!
!  Revision 1.3  2006/05/30 08:17:44  vivaresf
!  DM-ID 387 : commentaires explicatifs
!  regles de coddage
!
!  Revision 1.2  2005/12/08 18:25:02  vivaresf
!  implicit none
!
!  Revision 1.1.1.1  2005/12/07 07:23:08  vivaresf
!  Refonte de COMPAS
!
!  Revision 1.5  2005/10/12 11:05:10  vivaresf
!  DM-ID 386 : Modèle d'atmosphère martien EMCD 4.1
!
!  Revision 1.4  2005/10/05 09:30:26  bouchacp
!  DM-ID 386 : interface avec la NETCDF pour EMCD 4.1
!
!  Revision 1.3  2005/03/07 08:15:50  vivaresf
!  Version 1-5 : présentation de la documentation extraite des cartouches
!  Revision 1.2  2005/03/04 16:39:50  vivaresf
!  DM-ID 318 : traduction des entêtes
!
!$FinHistorique
!
!$Usage
!  use NETCDF_F90
!
!$Structure
!
!$Global
!
!>  nf_nowrite    : <integer>       code pour écriture
!>  nf_noerr      : <integer>       code pour écriture en erreur
!$Routines
!- interface
!
!$Fonctions
!- nf_get_var_real
!- nf_open
!- nf_close
!- nf_strerror
!- nf_inq_varid
!
!$Include
!
!$Module
!
!$Interface
!> interface :  nf_inq_varid
!               nf_get_var_real
!               nf_close
!               nf_open
!               nf_strerror
!               
!#V
!#
!
!$Remarques
!
!$Mots-cles
!
!$Voir-Aussi
!.  nf_get_var_real nf_open nf_close nf_inq_varid nf_strerror
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: cps_netcdf_interface.F90 69 2012-09-11 08:33:34Z ffsm $'


    interface

    function nf_inq_varid(ncid,name,varid) result(resultat)
!   Interface FORTRAN pour la fonction C nc_inq_varid de la NETCDF
! -> retourne varid permettant l'acces aux valeurs
      integer          :: ncid
      character(LEN=*) :: name
      integer          :: varid
      integer          :: resultat
    end function nf_inq_varid

    function nf_get_var_real(ncid,varid,rvals) result(resultat)
!   Interface FORTRAN pour la fonction C nc_get_var_real de la NETCDF
! -> retourne rvals a partir de varid
      integer           :: ncid
      integer           :: varid
      real,dimension(1) :: rvals
      integer           :: resultat
    end function nf_get_var_real

    function nf_get_var_int2(ncid,varid,ivals) result(resultat)
!   Interface FORTRAN pour la fonction C nc_get_var_int2 de la NETCDF
! -> retourne ivals a partir de varid
      integer           :: ncid
      integer           :: varid
      integer*2, dimension(1)  :: ivals
      integer           :: resultat
    end function nf_get_var_int2

    function nf_inq_dimid(ncid, name, dimid) result(resultat) 
! -> retourne l'ID dimid en fonction du nom name de la dimension netCDF
      integer           :: ncid
      character*(*)     :: name
      integer           :: dimid
      integer           :: resultat
    end function nf_inq_dimid

    function  nf_inq_dimlen(ncid, dimid, len) result(resultat) 
! -> renvoie une dimension dimid, len contient la taille de la dimension
      integer  :: ncid 
      integer  :: dimid
      integer  :: len
      integer  :: resultat
    end function nf_inq_dimlen

    function nf_close(ncid) result(resultat)
!   Interface FORTRAN pour la fonction C nc_close de la NETCDF
! cloture d'un fichier
      integer          :: resultat
    end function nf_close

    function nf_open(path,mode,ncid) result(resultat)
!   Interface FORTRAN pour la fonction C nc_open de la NETCDF
! ouverture d'un fichier
      character*(*)     :: path
      integer           :: mode
      integer           :: ncid
      integer           :: resultat
    end function nf_open

    function nf_strerror(ier) result(resultat) 
!   Interface FORTRAN pour la fonction C nc_strerror de la NETCDF
!   fonction d'erreur
      integer           :: ier
      character(len=80)    :: resultat
    end function nf_strerror

 end interface 

!! Codes / constantes pour les fonctions d'accès NETCDF
    integer :: nf_nowrite
    parameter (nf_nowrite = 0)
    integer :: nf_noerr
    parameter (nf_noerr = 0)

end module netcdf_F90
