!---------------------------------------------------------------------
!
!	Interface FORTRAN 77
!$Version
!	$Id: MSP_GESTION_ERREUR_F77.F90 69 2012-09-11 08:33:34Z ffsm $
!$Historique
!	$Log: MSP_GESTION_ERREUR_F77.F90,v $
!	Revision 1.3  2005/03/14 08:46:00  vivaresf
!	FA 226 : marquage des fichiers
!	
!
!---------------------------------------------------------------------


logical function MSP_test_erreur()


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_test_erreur
!
!$Resume
!	Fonction appelable du f77 permettant de savoir si une erreur s'est produite
!
!$Description
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PUBLIC
!
!$Usage
!.    logical function MSP_test_erreur()
!
!$Arguments
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- MSP_gestion_erreur
!#
!
!$Remarques
!
!$Mots-cles
!  ERREUR TEST FORTRAN77
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use MSP_gestion_erreur 

  implicit none

  MSP_test_erreur = MSP_erreur
end function MSP_test_erreur

logical function MSP_test_warning()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_test_warning
!
!$Resume
!	Fonction appelable du f77 permettant de savoir si un warning s'est produit
!
!$Description
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PUBLIC
!

!$Usage
!.    logical function MSP_test_warning()
!
!$Arguments
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- MSP_gestion_erreur
!#
!
!$Remarques
!
!$Mots-cles
!  ERREUR TEST
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use MSP_gestion_erreur

  implicit none

  MSP_test_warning = MSP_warning
end function MSP_test_warning

logical function MSP_test_probleme()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_test_probleme
!
!$Resume
!	Fonction permettant de savoir si une erreur ou un warning s'est produit
!
!$Description
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PUBLIC
!
!$Usage
!.    logical function MSP_test_probleme()
!
!$Arguments
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- MSP_gestion_erreur
!#
!
!$Remarques
!
!$Mots-cles
!  ERREUR TEST FORTRAN77
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use MSP_gestion_erreur

  implicit none

  MSP_test_probleme = MSP_probleme
end function MSP_test_probleme


subroutine MSP_afficher_message(unilog,nbmes)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  MSP_afficher_message
!
!$Resume
!	Routine utilisable en f77 permettant d'afficher un message dans un numéro d'unité
!	logique
!
!$Description
!
!$Auteur
!	S. ROUSSEAU
!
!$Acces
!  PUBLIC
!
!$Usage
!  call MSP_afficher_message(unilog,nbmes)
!.    integer :: unilog,nbmes
!
!$Arguments
!>E/S   unilog  :<integer>   
!>E/S   nbmes   :<integer>   nombre de messages à afficher
!>	0     : tous les messages
!>	n     : les n derniers messages
!
!$Common
!
!$Routines
!- MSP_recuperer_message
!- afficher_mes
!
!$Include
!
!$Module
!#V
!- MSP_gestion_erreur
!#
!
!$Remarques
!
!$Mots-cles
!  ERREUR AFFICHER MESSAGE FORTRAN77
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use MSP_gestion_erreur,  afficher_mes => MSP_afficher_message

  implicit none

  integer :: unilog,nbmes
  type (MSP_message) ::message
  integer ::nbmes_loc
  
  !--------------------------------------------------
  !    Traitement du nombre de messages à récupérer
  !--------------------------------------------------

  if (nbmes == 0) then
     nbmes_loc = MSP_tous_messages
  else
     nbmes_loc = nbmes
  end if

  ! Récupération de la struture contenant l'historique des messages
  call MSP_recuperer_message(message,nb_mes=nbmes_loc)
  call afficher_mes(message,unit=unilog)
end subroutine MSP_afficher_message
