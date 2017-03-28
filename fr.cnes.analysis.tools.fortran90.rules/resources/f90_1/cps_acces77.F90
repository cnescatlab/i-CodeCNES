!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Resume
!	Interface FORTRAN 77 aaux fonctions d'acces COMPAS
!
!$Version
!  $Id: cps_acces77.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: cps_acces77.F90,v $
!  Revision 1.31  2010/10/21 13:46:20  ogarat
!  VERSION::AQ::21/10/2010:Ajout du fin historique
!
!  Revision 1.30  2010/04/30 14:12:31  jlrobin
!  V2.9::AQ::30/04/2010:merge de la branche de developpement modeles atmospheriques
!
!  Revision 1.29.2.2  2010/03/01 17:31:56  jlrobin
!  V2.9::DM-ID:1359:01/03/2010:Integration du modele d'atmosphere MARS GRAM 2005
!
!
!  Revision 1.29  2009/08/26 15:00:24  cml
!  FA-ID 1315 : Suppression de la construction manuelle du path complet de ephem_infoget
!
!  Revision 1.28  2009/01/05 13:49:58  cml
!  FA-ID 1133 : Suppression des constantes numeriques non nommees
!
!  Revision 1.27  2008/10/28 13:06:03  tanguyy
!  DM-ID 1058 : utilisation de l'interface cpsi_size_ptr pour evaluer la taille d'un pointeur
!
!  Revision 1.26  2008/10/28 12:43:48  tanguyy
!  DM-ID 1058 : utilisation de l'interface cpsi_size_ptr pour evaluer la taille d'un pointeur
!
!  Revision 1.25  2008/10/20 13:18:22  cml
!  AQ : Correction d un mauvais traitement d erreur
!
!  Revision 1.24  2008/10/06 07:50:49  cml
!  FA-ID 1059 : Suppression de la routine obsolete cps_phys77
!
!  Revision 1.23  2008/10/03 12:42:57  cml
!  FA-ID 1024 : Renommage des arguments des fonctions ayant pour nom min,max ou mod
!
!  Revision 1.22  2008/09/25 15:04:27  cml
!  DM-ID 1058 : Correction d une mauvaise utilisation de tableaux
!
!  Revision 1.21  2008/08/04 12:53:11  gss
!  DM-ID 1058 : (portage g95) initialisation à NULL des pointeurs lors de leur
!  déclaration et initialisation à 0 du nombre de modèles si la liste est vide,
!  dans la fonction d'obtention de cette liste.
!
!  Revision 1.20  2008/07/04 11:46:16  huec
!  DM-ID 1058 : Initialisations de variables
!
!  Revision 1.19  2007/05/21 06:56:18  vivaresf
!  Version 2.2 : révision des cartouches
!
!  Revision 1.18  2006/11/20 17:27:35  vivaresf
!  Version 2-1 : métriques understand, déclarations obsolètes
!
!  Revision 1.17  2006/10/24 14:47:57  vpg
!  Correction d'un bug lie a l'interface F77 : declaration de la taille d'un tableau passe en parametre de cps_constante77
!
!  Revision 1.16  2006/10/18 09:52:26  vivaresf
!  DM-ID 425 : Cloture du FT (Passage PSIMU sous Linux)
!
!  Revision 1.15.2.1  2006/09/26 12:13:22  vpg
!  DM-ID 425 : Version initiale du FT (Passage PSIMU sous Linux)
!
!  Revision 1.15  2006/07/03 08:53:53  vpg
!  Prise en compte des fichiers Tchebychev dans une base locale
!
!  Revision 1.14  2006/05/30 15:20:01  vivaresf
!  regle de codage : suppression de *(*) obsolete
!
!  Revision 1.13  2006/05/30 12:29:01  vivaresf
!  Separation COMPAS_UI,
!  suppression MSPRO
!  robustesse (iostat sur les deallocate)
!
!  Revision 1.12  2006/05/30 09:11:21  vivaresf
!  Commentaires
!
!  Revision 1.11  2006/05/30 08:27:45  vivaresf
!  DM-ID 387 : entete MADONA
!  suppression des codes commentés
!  commentaires vrais sur le code
!
!$FinHistorique
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 subroutine cps_liste77(liste, nbliste, typec, corpsc, codes, fermer, ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_liste77
!
!$Resume
!   Accès fortran 77 à cps_liste (obsolète)
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
!  call cps_liste77(liste, nbliste, typec, corpsc, codes, fermer, ier)
!.    character(len=*), dimension(MAXCORPS) :: liste
!.    character(len=*) :: typec, corpsc
!.    integer, dimension(MAXCORPS) :: codes
!.    integer :: nbliste, ier
!.    logical :: fermer
!
!$Arguments
!>S     liste    :<LEN=*,DIM=(MAXCORPS)>     Liste de corps
!>S     nbliste  :<integer>                  Nombre d'éléments de liste
!>E     typec    :<LEN=*>                    filtre : type des corps
!>E     corpsc   :<LEN=*>                    filtre : corps central
!>S     codes    :<integer,DIM=(MAXCORPS)>   Codes des corps de liste
!>E     fermer   :<logical>                  si oui, la base est refermée après appel
!>S     ier      :<integer>                  code d'erreur
!
!$Common
!
!$Routines
!- cps_liste
!- cps_close
!
!$Include
!
!$Module
!#V
!- cps_acces
!#
!
!$Remarques
!  ier = 0 : ok
!  ier = 1 : (warning) base vide
!  ier = 2 : (warning) pas de corps de type typec
!  ier = 3 : (warning) pas de corps de corps central corpsc
!  ier = 4 : (warning) pas de corps de corps central corpsc et de type typec
!  ier < 0 : (erreur)  Erreur au chargement de la base
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use cps_acces
   implicit none

    character(len=*), dimension(MAXCORPS), intent(out) :: liste
    character(len=*), intent(in) :: typec, corpsc
    integer, dimension(MAXCORPS), intent(out) :: codes
    integer, intent(out) :: nbliste, ier
    logical, intent(in) :: fermer

    ier = 0

    call cps_liste(liste, nbliste, codes=codes)
    if (nbliste.le.0) then 
       ier = 1
       if (MSP_ERREUR) ier = -1
       goto 999
    endif

    if (corpsc.ne."".and.typec.ne."") then
       call cps_liste(liste, nbliste, typec=typec, corpsc=corpsc, codes=codes)
       if (nbliste.le.0) then 
          ier = 4
          if (MSP_ERREUR) ier = -4
       endif
       goto 999
    endif

    if (typec.ne."") then
       call cps_liste(liste, nbliste, typec=typec, codes=codes)
       if (nbliste.le.0)  then 
          ier = 2
          if (MSP_ERREUR) ier = -2
       endif
       goto 999
    endif


    if (corpsc.ne."") then
       call cps_liste(liste, nbliste, corpsc=corpsc, codes=codes)
       if (nbliste.le.0) then 
          ier = 3
          if (MSP_ERREUR) ier = -3
          return
       endif
    endif

999 continue
    if (fermer) call cps_close()

  end subroutine cps_liste77

  logical function cps_existe77(corps, mu, requa, apla, J2, vrot, &
          potentiel, atmosphere, geometrie, dga, exc, inc, &
          corpsc, typec, G, vlum, ua, fermer, ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_existe77
!
!$Resume
!  Accès fortran 77 à cps_existe (obsolète)
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
!.      logical function cps_existe77(corps, mu, requa, apla, J2, vrot, &
!.              potentiel, atmosphere, geometrie, dga, exc, inc, &
!.              corpsc, typec, G, vlum, ua, fermer, ier)
!.    character(len=*) :: corps
!.    logical :: mu, requa, apla, J2, vrot
!.    logical :: potentiel, atmosphere, geometrie
!.    logical :: dga, exc, inc, corpsc, typec
!.    logical :: G, vlum, ua
!.    integer :: ier
!.    logical :: fermer
!
!$Arguments
!>E     corps       :<LEN=*>     Corps (ou assimile) de la base
!>S     mu          :<logical>   champs mu    existe
!>S     requa       :<logical>   champs requa existe
!>S     apla        :<logical>   champs apla  existe
!>S     J2          :<logical>   champs J2    existe
!>S     vrot        :<logical>   champs vrot  existe
!>S     potentiel   :<logical>   champs potentiel existe
!>S     atmosphere  :<logical>   champs atmosphere existe
!>S     geometrie   :<logical>   champs geometrie existe
!>S     dga         :<logical>   champs dga   existe
!>S     exc         :<logical>   champs exc   existe
!>S     inc         :<logical>   champs inc   existe
!>S     corpsc      :<logical>   champs corpsc existe
!>S     typec       :<logical>   champs typec existe
!>S     G           :<logical>   champs G     existe
!>S     vlum        :<logical>   champs vlum  existe
!>S     ua          :<logical>   champs ua    existe
!>E     fermer      :<logical>   si oui, la base est refermée après appel
!>S     ier         :<integer>   code d'erreur
!
!$Common
!
!$Routines
!- cps_close
!
!$Include
!
!$Module
!#V
!- cps_acces
!#
!
!$Remarques
!  ier = 0 : ok
!  ier = 1 : (warning) le corps n'existe pas
!  ier < 0 : (erreur)  Erreur au chargement de la base
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
   use cps_acces
   implicit none

    character(len=*), intent(in) :: corps
    logical, intent(out) :: mu, requa, apla, J2, vrot
    logical, intent(out) :: potentiel, atmosphere, geometrie
    logical, intent(out) :: dga, exc, inc, corpsc, typec
    logical, intent(out) :: G, vlum, ua
    logical :: ok
    integer, intent(out) :: ier
    logical, intent(in)  :: fermer

    ok = cps_existe(corps, mu=mu, requa=requa, apla=apla, J2=J2, vrot=vrot, &
          potentiel=potentiel, atmosphere=atmosphere, geometrie=geometrie, &
          dga=dga, exc=exc, inc=inc, corpsc=corpsc, typec=typec, &
          G=G, vlum=vlum, ua=ua)

    if (MSP_ERREUR) then
       ier = -1
       ok = .false.
       cps_existe77 = ok
       return 
    endif
       
    ok = cps_existe(corps)
    if(.not.ok) ier = 1
    
    if (fermer) call cps_close()
    cps_existe77 = ok

  end function cps_existe77

  subroutine cps_constante77(corps, nb, mu, requa, apla, J2, vrot, &
       potentiel, atmosphere, geometrie, dga, exc, inc, &
       corpsc, typec, code, fermer, ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_constante77
!
!$Resume
!  Accès fortran 77 à cps_constante (obsolète)
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
!  call cps_constante77(corps, nb, mu, requa, apla, J2, vrot, &
!.           potentiel, atmosphere, geometrie, dga, exc, inc, &
!.           corpsc, typec, code, fermer, ier)
!.    character(len=*), dimension(nb) :: corps
!.    integer :: nb
!.    real(kind=PM_REEL), dimension(nb) :: mu, requa, apla, J2, vrot
!.    real(kind=PM_REEL), dimension(nb) :: dga, exc, inc
!.    character(len=*), dimension(100) :: potentiel
!.    character(len=*), dimension(100) :: atmosphere
!.    character(len=*), dimension(100) :: geometrie
!.    integer, dimension(nb) :: code
!.    character(len=*), dimension(nb) :: corpsc, typec
!.    logical :: fermer
!.    integer :: ier
!
!$Arguments
!>E     corps       :<LEN=*,DIM=(nb)>     Corps (ou assimile) de la base 
!>E     nb          :<integer>            Taille de corps
!>S     mu          :<PM_REEL,DIM=(nb)>   valeur de mu    (constante d'attraction)
!>S     requa       :<PM_REEL,DIM=(nb)>   valeur de requa (rayon equatorial)
!>S     apla        :<PM_REEL,DIM=(nb)>   valeur de apla  (coefficient d'applatissement)
!>S     J2          :<PM_REEL,DIM=(nb)>   valeur de J2    (coefficient en J2)
!>S     vrot        :<PM_REEL,DIM=(nb)>   valeur de vrot  (vitesse de rotation)
!>S     potentiel   :<LEN=*,DIM=(100)>    valeur de modele_potentiel 
!>S     atmosphere  :<LEN=*,DIM=(100)>    valeur de modele_atmosphere
!>S     geometrie   :<LEN=*,DIM=(100)>    valeur de modele_geometrie
!>S     dga         :<PM_REEL,DIM=(nb)>   valeur de dga (demi-grand axe a J2000)
!>S     exc         :<PM_REEL,DIM=(nb)>   valeur de exc (excentricite a J2000)
!>S     inc         :<PM_REEL,DIM=(nb)>   valeur de inc (inclinaison a J2000)
!>S     corpsc      :<LEN=*,DIM=(nb)>     corps central 
!>S     typec       :<LEN=*,DIM=(nb)>     type de corps
!>S     code        :<integer,DIM=(nb)>   code du corps
!>E     fermer      :<logical>            si oui, la base est refermée après appel
!>S     ier         :<integer>            code d'erreur     
!
!$Common
!
!$Routines
!- cps_constante
!- cps_close
!
!$Include
!
!$Module
!#V
!- cps_acces
!#
!
!$Remarques
!  ier =  0 : ok
!  ier >  0 : (warning) nombre de corps avec champs manquant
!  ier = -1 : (erreur)  Erreur au chargement de la base
!  ier = -2 : (erreur)  L'un des corps n'existe pas dans la base
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use cps_acces
   implicit none

    character(len=*), dimension(nb), intent(in) :: corps
    integer, intent(in) :: nb
    real(kind=PM_REEL), dimension(nb), intent(out) :: mu, requa, apla, J2, vrot
    real(kind=PM_REEL), dimension(nb), intent(out) :: dga, exc, inc
    character(len=*), dimension(100),intent(out) :: potentiel
    character(len=*), dimension(100),intent(out) :: atmosphere
    character(len=*), dimension(100),intent(out) :: geometrie
    integer, intent(out),  dimension(nb) :: code
    character(len=*), intent(out),  dimension(nb) :: corpsc, typec

    logical, intent(in) :: fermer
    integer, intent(out) :: ier

    integer :: ii

    ier = 0
    do ii=1, nb
    if (cps_existe(corps(ii))) then
       
       call cps_constante(corps(ii), mu=mu(ii), requa=requa(ii), apla=apla(ii), &
            J2=J2(ii), vrot=vrot(ii), potentiel=potentiel, atmosphere=atmosphere, &
            geometrie=geometrie, dga=dga(ii), exc=exc(ii), inc=inc(ii), &
            corpsc=corpsc(ii), typec=typec(ii), code=code(ii))

       if (MSP_WARNING) ier = ier + 1 
       if (MSP_ERREUR) goto 999
    else
       ier = -2
       goto 999
    endif
    enddo
    
999 continue

    if (MSP_ERREUR) ier = -1

    if (fermer) call cps_close()

  end subroutine cps_constante77

  subroutine cps_constante77a(corps, nb, mu, requa, apla, vrot, &
       code, fermer, ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_constante77a
!
!$Resume
!  Accès fortran 77 à cps_constante (obsolète)
!
!$Description
!  Accès fortran 77 à cps_constante, pour les constantes utilisées 
!  dans les changements de repère (par tableau)
!
!$Auteur
!   Florence VIVARES (SchlumbergerSema)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_constante77a(corps, nb, mu, requa, apla, vrot, &
!.           code, fermer, ier)
!.    integer :: nb
!.    character(len=*), dimension(nb) :: corps
!.    real(kind=PM_REEL), dimension(nb) :: mu, requa, apla,vrot
!.    integer, dimension(nb) :: code
!.    logical :: fermer
!.    integer :: ier
!
!$Arguments
!>E     corps   :<LEN=*,DIM=(nb)>     Corps (ou assimile) de la base 
!>E     nb      :<integer>            Taille de corps
!>S     mu      :<PM_REEL,DIM=(nb)>   valeur de mu    (constante d'attraction)
!>S     requa   :<PM_REEL,DIM=(nb)>   valeur de requa (rayon equatorial)
!>S     apla    :<PM_REEL,DIM=(nb)>   valeur de apla  (coefficient d'applatissement)
!>S     vrot    :<PM_REEL,DIM=(nb)>   valeur de vrot  (vitesse de rotation)
!>S     code    :<integer,DIM=(nb)>   code du corps
!>E     fermer  :<logical>            si oui, la base est refermée après appel
!>S     ier     :<integer>            code d'erreur     
!
!$Common
!
!$Routines
!- cps_constante
!- cps_close
!
!$Include
!
!$Module
!#V
!- cps_acces
!#
!
!$Remarques
!  ier =  0 : ok
!  ier >  0 : (warning) nombre de corps avec champs manquant
!  ier = -1 : (erreur)  Erreur au chargement de la base
!  ier = -2 : (erreur)  L'un des corps n'existe pas dans la base
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use cps_acces
   implicit none

    integer, intent(in) :: nb
    character(len=*),  dimension(nb), intent(in) :: corps
    real(kind=PM_REEL),  dimension(nb), intent(out) :: mu, requa, apla,vrot
    integer, intent(out),  dimension(nb) :: code

    logical, intent(in) :: fermer
    integer, intent(out) :: ier

    integer :: ii

    ier = 0
    do ii=1, nb
       if (cps_existe(corps(ii))) then
          call cps_constante(corps(ii), mu=mu(ii), requa=requa(ii), &
               apla=apla(ii), vrot=vrot(ii), code=code(ii))
          if (MSP_WARNING) ier = ier + 1 
          if (MSP_ERREUR) goto 999
       else
          ier = -2
          goto 999
       endif
    enddo

999 continue

    if (MSP_ERREUR) ier = -1

    if (fermer) call cps_close()

  end subroutine cps_constante77a

  subroutine cps_constante77b(corps, nb, dga, exc, inc, &
       code, fermer, ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_constante77b
!
!$Resume
!  Accès fortran 77 à cps_constante  (obsolète)
!
!$Description
!  Accès fortran 77 à cps_constante, pour les constantes décrivant
!  l'orbite à J2000 (dga, exc, inc)
!
!$Auteur
!   Florence VIVARES (SchlumbergerSema)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_constante77b(corps, nb, dga, exc, inc, &
!.           code, fermer, ier)
!.    integer :: nb
!.    character(len=*), dimension(nb) :: corps
!.    real(kind=PM_REEL), dimension(nb) :: dga, exc, inc
!.    integer, dimension(nb) :: code
!.    logical :: fermer
!.    integer :: ier
!
!$Arguments
!>E     corps   :<LEN=*,DIM=(nb)>     Corps (ou assimile) de la base 
!>E     nb      :<integer>            Taille du tableau corps
!>S     dga     :<PM_REEL,DIM=(nb)>   valeur de dga (demi-grand axe)
!>S     exc     :<PM_REEL,DIM=(nb)>   valeur de exc (excentricite)
!>S     inc     :<PM_REEL,DIM=(nb)>   valeur de inc (inclinaison)
!>S     code    :<integer,DIM=(nb)>   code du corps
!>E     fermer  :<logical>            si oui, la base est refermée après appel
!>S     ier     :<integer>            code d'erreur     
!
!$Common
!
!$Routines
!- cps_constante
!- cps_close
!
!$Include
!
!$Module
!#V
!- cps_acces
!#
!
!$Remarques
!  ier =  0 : ok
!  ier >  0 : (warning) nombre de corps avec champs manquant
!  ier = -1 : (erreur)  Erreur au chargement de la base
!  ier = -2 : (erreur)  L'un des corps n'existe pas dans la base
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use cps_acces
   implicit none

    integer, intent(in) :: nb
    character(len=*), dimension(nb), intent(in) :: corps
    real(kind=PM_REEL), dimension(nb), intent(out) :: dga, exc, inc
    integer, intent(out), dimension(nb) :: code

    logical, intent(in) :: fermer
    integer, intent(out) :: ier

    integer :: ii

    ier = 0
    do ii=1, nb
       if (cps_existe(corps(ii))) then
          call cps_constante(corps(ii), dga=dga(ii), exc=exc(ii), inc=inc(ii), &
               code=code(ii))
          if (MSP_WARNING) ier = ier + 1 
          if (MSP_ERREUR) goto 999
       else
          ier = -2
          goto 999
       endif
    enddo

999 continue

    if (MSP_ERREUR) ier = -1

    if (fermer) call cps_close()

  end subroutine cps_constante77b

  subroutine cps_constante77c(corps, nb, mu, requa, apla, vrot, &
       fermer, ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_constante77c
!
!$Resume
!  Accès fortran 77 à cps_constante (obsolète) 
!
!$Description
!  Accès fortran 77 à cps_constante, pour les constantes utilisées 
!  dans les changements de repère
!
!$Auteur
!   Florence VIVARES (SchlumbergerSema)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_constante77c(corps, nb, mu, requa, apla, vrot, &
!.           fermer, ier)
!.    integer :: nb
!.    integer, dimension(nb) :: corps
!.    real(kind=PM_REEL), dimension(nb) :: mu, requa, apla,vrot
!.    logical :: fermer
!.    integer :: ier
!
!$Arguments
!>E     corps   :<integer,DIM=(nb)>   Corps (ou assimile) de la base 
!>E     nb      :<integer>            Taille de corps
!>S     mu      :<PM_REEL,DIM=(nb)>   valeur de mu    (constante d'attraction)
!>S     requa   :<PM_REEL,DIM=(nb)>   valeur de requa (rayon equatorial)
!>S     apla    :<PM_REEL,DIM=(nb)>   valeur de apla  (coefficient d'applatissement)
!>S     vrot    :<PM_REEL,DIM=(nb)>   valeur de vrot  (vitesse de rotation)
!>E     fermer  :<logical>            si oui, la base est refermée après appel
!>S     ier     :<integer>            code d'erreur     
!
!$Common
!
!$Routines
!- cps_constante
!- cps_close
!
!$Include
!
!$Module
!#V
!- cps_acces
!#
!
!$Remarques
!  ier =  0 : ok
!  ier >  0 : (warning) nombre de corps avec champs manquant
!  ier = -1 : (erreur)  Erreur au chargement de la base
!  ier = -2 : (erreur)  L'un des corps n'existe pas dans la base
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use cps_acces
   implicit none

    integer, intent(in) :: nb
    integer, dimension(nb), intent(in) :: corps
    real(kind=PM_REEL),  dimension(nb), intent(out) :: mu, requa, apla,vrot

    logical, intent(in) :: fermer
    integer, intent(out) :: ier

    integer :: ii

    ier = 0
    do ii=1, nb
       call cps_constante(corps(ii), mu=mu(ii), requa=requa(ii), &
            apla=apla(ii), vrot=vrot(ii))
       if (MSP_WARNING) ier = ier + 1 
       if (MSP_ERREUR) goto 999
    enddo

999 continue

    if (MSP_ERREUR) ier = -1

    if (fermer) call cps_close()

  end subroutine cps_constante77c


  subroutine cps_constante77d(code, potentiel, atmosphere, geometrie, &
       nbpot, nbatm, nbgeom, fermer, ier)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_constante77d
!
!$Resume
!  Accès fortran 77 à cps_constante, pour les modèles  (obsolète) 
!
!$Description
!  Accès fortran 77 à cps_constante, pour les modèles (potentiel,
!  atmosphère, géométrie.
!
!$Auteur
!   Florence VIVARES (SchlumbergerSema)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_constante77d(code, potentiel, atmosphere, geometrie, &
!.           nbpot, nbatm, nbgeom, fermer, ier)
!.    integer :: code
!.    character(len=*), dimension(100) :: potentiel
!.    character(len=*), dimension(100) :: atmosphere
!.    character(len=*), dimension(100) :: geometrie
!.    integer :: nbpot, nbatm, nbgeom
!.    logical :: fermer
!.    integer :: ier
!
!$Arguments
!>E     code        :<integer>           Corps (ou assimile) de la base 
!>S     potentiel   :<LEN=*,DIM=(100)>   Modèles de potentiel    
!>S     atmosphere  :<LEN=*,DIM=(100)>   Modèles d'atmosphère
!>S     geometrie   :<LEN=*,DIM=(100)>   Modèles géométriques
!>S     nbpot       :<integer>           Nombre de modèles dans potentiel
!>S     nbatm       :<integer>           Nombre de modèles dans atmosphere
!>S     nbgeom      :<integer>           Nombre de modèles dans geometrie
!>E     fermer      :<logical>           si oui, la base est refermée après appel
!>S     ier         :<integer>           code d'erreur     
!
!$Common
!
!$Routines
!- cps_constante
!- cps_close
!
!$Include
!
!$Module
!#V
!- cps_acces
!#
!
!$Remarques
!  ier =  0 : ok
!  ier >  0 : (warning) nombre de corps avec champs manquant
!  ier = -1 : (erreur)  Erreur au chargement de la base
!  ier = -2 : (erreur)  L'un des corps n'existe pas dans la base
!
!$Mots-cles
!
!$Voir-Aussi
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use cps_acces
   implicit none

    integer, intent(in) :: code
    character(len=*), dimension(100),intent(out) :: potentiel
    character(len=*), dimension(100),intent(out) :: atmosphere
    character(len=*), dimension(100),intent(out) :: geometrie
    integer, intent(out) :: nbpot, nbatm, nbgeom
    logical, intent(in) :: fermer
    integer, intent(out) :: ier

    integer :: ii

    ier = 0
    nbpot=0
    nbatm=0
    nbgeom=0

    potentiel(:size(potentiel))  = " "
    atmosphere(:size(atmosphere)) = " "
    geometrie(:size(geometrie))  = " "

    call cps_constante(code, potentiel=potentiel, atmosphere=atmosphere, &
         geometrie=geometrie)
    if (MSP_WARNING) ier = 1 
    if (MSP_ERREUR) ier = -1

    do ii=1,size(potentiel)
       if(len_trim(potentiel(ii)).ne.0)  nbpot  = ii
    enddo
    do ii=1,size(atmosphere)
       if(len_trim(atmosphere(ii)).ne.0) nbatm  = ii
    enddo
    do ii=1,size(geometrie)
       if(len_trim(geometrie(ii)).ne.0)  nbgeom = ii
    enddo

    if (fermer) call cps_close()

  end subroutine cps_constante77d


  subroutine cps_codenom77(code, nom, lang, ier)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_codenom77
!
!$Resume
!   Nom du corps désigné par code
!.  Par défaut il s'agit du nom de la structure, 
!.  si lang est non vide, il s'agit alors du champ nom_+'lang'
!
!$Description
!
!$Auteur
!  Florence VIVARES (SchlumbergerSema)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_codenom77(code, nom, lang, ier)
!.    integer :: code
!.    character(len=*) :: nom
!.    character(len=*) :: lang
!.    integer :: ier
!
!$Arguments
!>E     code  :<integer>   Code libephem
!>S     nom   :<LEN=*>     Nom du corps
!>E     lang  :<LEN=*>     'fr' ou 'en'
!>S     ier   :<integer>   Code d'erreur
!
!$Common
!
!$Routines
!- cps_codenom
!- MSP_effacer_message
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

   use cps_acces
   implicit none

    integer, intent(in) :: code
    character(len=*), intent(out) :: nom
    character(len=*), intent(in) :: lang
    integer, intent(out) :: ier

    if (trim(lang).ne." ") then
       call cps_codenom(code, nom, lang=lang)
    else
       call cps_codenom(code, nom)
    endif

    ier = 0

    if (MSP_WARNING) ier=1

    if (MSP_ERREUR) ier=-1

    call MSP_effacer_message ()

  end subroutine cps_codenom77


  subroutine cps_get_reel77(corps, libelle, vald, unit, ier) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_get_reel77
!
!$Resume
!  Récupération d'une valeur réelle dans un fichier COMPAS
!
!$Description
!  Récupération d'une valeur réelle dans un fichier COMPAS
!  Interface a cps_get du module cps_acces
!
!$Auteur
!  Y TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_get_reel77(corps, libelle, vald, unit, ier) 
!.    character(len=*) :: corps, libelle
!.    real(kind=8) :: vald
!.    character(len=*) :: unit
!.    integer :: ier
!
!$Arguments
!>E     corps    :<LEN=*>     Nom du corps ou de la structure englobant la variable recherchée
!>E     libelle  :<LEN=*>     Nom de la variable recherchée
!>S     vald     :<KIND=8>    Valeur de la variable
!>E     unit     :<LEN=*>     Unité demandée pour cette variable
!>S     ier      :<integer>   Code retour (< 0 si erreur)
!
!$Common
!
!$Routines
!- cps_get
!- MSP_effacer_message
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
    
    use cps_acces
    implicit none
    
    character(len=*), intent(in) :: corps, libelle
    real(kind=8), intent(out) :: vald
    character(len=*), intent(in) :: unit
    integer, intent(out) :: ier


    call cps_get(corps,libelle=libelle,vald=vald,unit=unit)
    
    if(MSP_ERREUR) then
       ier = -1
    else 
       ier = 0
    end if

    call MSP_effacer_message ()

  end subroutine cps_get_reel77


  subroutine cps_get_int77(corps, libelle, vali, ier) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_get_int77
!
!$Resume
!  Récuperation d'une valeur entière dans un fichier COMPAS
!
!$Description
!  Récupération d'une valeur entière dans un fichier COMPAS
!  Interface a cps_get du module cps_acces
!
!$Auteur
!  Y TANGUY (ATOS)
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_get_int77(corps, libelle, vali, ier) 
!.    character(len=*) :: corps, libelle
!.    integer :: vali
!.    integer :: ier
!
!$Arguments
!>E     corps    :<LEN=*>     Nom du corps ou de la structure englobant la variable recherchée
!>E     libelle  :<LEN=*>     Nom de la variable recherchée
!>S     vali     :<integer>   Valeur de la variable
!>S     ier      :<integer>   Code retour (< 0 si erreur)
!
!$Common
!
!$Routines
!- cps_get
!- MSP_effacer_message
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
    
    use cps_acces
    implicit none
    
    character(len=*), intent(in) :: corps, libelle
    integer, intent(out) :: vali
    integer, intent(out) :: ier

    vali = 0
    ier = 0
    call cps_get(corps,libelle=libelle,vali=vali)
    
    if(MSP_ERREUR) then
       ier = -1
    else 
       ier = 0
    end if

    call MSP_effacer_message ()
    
  end subroutine cps_get_int77


  subroutine cps_get_string77(corps, libelle, vals, ier) 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_get_string77
!
!$Resume
!  Récuperation d'une valeur de type string dans un fichier COMPAS
!
!$Description
!  Récupération d'une valeur de type string dans un fichier COMPAS
!  Interface a cps_get du module cps_acces
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_get_string77(corps, libelle, vals, ier) 
!.    character(len=*) :: corps, libelle
!.    character(len=*) :: vals
!.    integer :: ier
!
!$Arguments
!>E     corps    :<LEN=*>     Nom du corps ou de la structure englobant la variable recherchée
!>E     libelle  :<LEN=*>     Nom de la variable recherchée
!>S     vals     :<LEN=*>     Valeur de la variable
!>S     ier      :<integer>   Code retour (< 0 si erreur)
!
!$Common
!
!$Routines
!- cps_get
!- MSP_effacer_message
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
    
    use cps_acces
    implicit none

    character(len=*), intent(in) :: corps, libelle
    character(len=*), intent(out) :: vals
    integer, intent(out) :: ier

    vals=""
    call cps_get(corps,libelle=libelle,vals=vals)
    
    if(MSP_ERREUR) then
       ier = -1
    elseif(MSP_WARNING) then
       ier = 1
    else
       ier = 0
    end if

    call MSP_effacer_message ()

  end subroutine cps_get_string77


  subroutine cps_orbit77_23(date,sunlat,sunlon,ls,marsau,outmodelday)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_orbit77_23
!
!$Resume
!  Retourne le jour martien au sens EMCD 2.3
!
!$Description
!  Retourne le jour martien au sens EMCD 2.3
!  Interface de cps_orbit_23 pour GS_ATMOSPHERES_EMCD
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_orbit77_23(date,sunlat,sunlon,ls,marsau,outmodelday)
!.    real(kind=8) :: date 
!.    real(kind=PM_REEL) :: sunlat 
!.    real(kind=PM_REEL) :: sunlon 
!.    real(kind=PM_REEL) :: ls 
!.    real(kind=PM_REEL) :: marsau 
!.    real(kind=PM_REEL) :: outmodelday
!
!$Arguments
!>E/S   date         :<KIND=8>    
!>E/S   sunlat       :<PM_REEL>   
!>E/S   sunlon       :<PM_REEL>   
!>E/S   ls           :<PM_REEL>   
!>E/S   marsau       :<PM_REEL>   
!>E/S   outmodelday  :<PM_REEL>   
!
!$Common
!
!$Routines
!- cps_orbit_23
!
!$Include
!
!$Module
!#V
!- cps_modele_emcd23
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
    use cps_modele_emcd23
    implicit none
      real(kind=8) :: date   !Julian date
      real(kind=PM_REEL)   :: sunlat !subsolar latitude
      real(kind=PM_REEL)   ::  sunlon !subsolar longitude
      real(kind=PM_REEL)   :: ls     !Ls
      real(kind=PM_REEL)   :: marsau !Sun-Mars distance in AU
      real(kind=PM_REEL)   :: outmodelday

    call cps_orbit_23(date,sunlat,sunlon,ls,marsau,outmodelday)
  end subroutine cps_orbit77_23

  subroutine cps_season77_23(ls,numsaison)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_season77_23
!
!$Resume
!  Retourne la saison martienne en sens EMCD 2.3
!
!$Description
!  Retourne la saison martienne en sens EMCD 2.3
!  en fonction du jour martien
!  Interface de cps_season_23 pour GS_ATMOSPHERES_EMCD
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_season77_23(ls,numsaison)
!.    real(kind=8) :: ls
!.    integer :: numsaison
!
!$Arguments
!>E/S   ls         :<KIND=8>    
!>E/S   numsaison  :<integer>   
!
!$Common
!
!$Routines
!- cps_season_23
!
!$Include
!
!$Module
!#V
!- cps_modele_emcd23
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
    use cps_modele_emcd23
    implicit none
    real(kind=8) :: ls
    integer :: numsaison

    call cps_season_23(ls,numsaison)

  end subroutine cps_season77_23

  subroutine cps_orbit77_31(date,sunlat,sunlon,ls,marsau,outmodelday)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_orbit77_31
!
!$Resume
!  Retourne le jour martien au sens EMCD 3.1
!
!$Description
!  Retourne le jour martien au sens EMCD 3.1
!  Interface de cps_orbit_31 pour GS_ATMOSPHERES_EMCD
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_orbit77_31(date,sunlat,sunlon,ls,marsau,outmodelday)
!.    real(kind=8) :: date 
!.    real :: sunlat 
!.    real :: sunlon 
!.    real :: ls 
!.    real :: marsau 
!.    real :: outmodelday
!
!$Arguments
!>E/S   date         :<KIND=8>   
!>E/S   sunlat       :<real>     
!>E/S   sunlon       :<real>     
!>E/S   ls           :<real>     
!>E/S   marsau       :<real>     
!>E/S   outmodelday  :<real>     
!
!$Common
!
!$Routines
!- cps_orbit_31
!
!$Include
!
!$Module
!#V
!- cps_modele_emcd31
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
    use cps_modele_emcd31
    implicit none
    real(kind=8) :: date   !Julian date
    real   :: sunlat !subsolar latitude
    real   :: sunlon !subsolar longitude
    real   :: ls     !Ls
    real   :: marsau !Sun-Mars distance in AU
    real   :: outmodelday
    
    call cps_orbit_31(date,sunlat,sunlon,ls,marsau,outmodelday)
  end subroutine cps_orbit77_31

  subroutine cps_season77_31(ls,numsaison)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_season77_31
!
!$Resume
!  Retourne la saison martienne en sens EMCD 3.1
!
!$Description
!  Retourne la saison martienne en sens EMCD 3.1
!  en fonction du jour martien
!  Interface de cps_season_31 pour GS_ATMOSPHERES_EMCD
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_season77_31(ls,numsaison)
!.    real :: ls
!.    integer :: numsaison
!
!$Arguments
!>E/S   ls         :<real>      jour martien
!>E/S   numsaison  :<integer>   numero de saison
!
!$Common
!
!$Routines
!- cps_season_31
!
!$Include
!
!$Module
!#V
!- cps_modele_emcd31
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
    use cps_modele_emcd31
    implicit none
    real :: ls
    integer :: numsaison

    call cps_season_31(ls,numsaison)

  end subroutine cps_season77_31

  subroutine cps_orbit77_41(date,sunlat,sunlon,ls,marsau,outmodelday)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_orbit77_41
!
!$Resume
!  Retourne le jour martien au sens EMCD 4.1
!
!$Description
!  Retourne le jour martien au sens EMCD 4.1
!  Interface de cps_orbit_41 pour GS_ATMOSPHERES_EMCD
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_orbit77_41(date,sunlat,sunlon,ls,marsau,outmodelday)
!.    real(kind=8) :: date 
!.    real :: sunlat 
!.    real :: sunlon 
!.    real :: ls 
!.    real :: marsau 
!.    real :: outmodelday
!
!$Arguments
!>E/S   date         :<KIND=8>   
!>E/S   sunlat       :<real>     
!>E/S   sunlon       :<real>     
!>E/S   ls           :<real>     
!>E/S   marsau       :<real>     
!>E/S   outmodelday  :<real>     
!
!$Common
!
!$Routines
!- cps_orbit_41
!
!$Include
!
!$Module
!#V
!- cps_modele_emcd41
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
    use cps_modele_emcd41
    implicit none

      real(kind=8) :: date   !Julian date
      real   :: sunlat !subsolar latitude
      real   :: sunlon !subsolar longitude
      real   :: ls     !Ls
      real   :: marsau !Sun-Mars distance in AU
      real   :: outmodelday

    call cps_orbit_41(date,sunlat,sunlon,ls,marsau,outmodelday)
  end subroutine cps_orbit77_41

  subroutine cps_season77_41(ls,numsaison)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_season77_41
!
!$Resume
!  Retourne la saison martienne en sens EMCD 4.1
!
!$Description
!  Retourne la saison martienne en sens EMCD 4.1
!  en fonction du jour martien
!  Interface de cps_season_41 pour GS_ATMOSPHERES_EMCD
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_season77_41(ls,numsaison)
!.    real :: ls
!.    integer :: numsaison
!
!$Arguments
!>E/S   ls         :<real>      
!>E/S   numsaison  :<integer>   
!
!$Common
!
!$Routines
!- cps_season_41
!
!$Include
!
!$Module
!#V
!- cps_modele_emcd41
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
    use cps_modele_emcd41
    implicit none

    real    :: ls
    integer :: numsaison

    call cps_season_41(ls,numsaison)

  end subroutine cps_season77_41


  subroutine cps_orbit77_2001(xdate,latsun,lonsun,Lsubs,radius)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_orbit77_2001
!
!$Resume
!  Retourne le jour martien au sens GRAM 2001
!
!$Description
!  Retourne le jour martien au sens GRAM 2001
!  Interface de cps_orbit_2001 pour GS_ATMOSPHERES_EMCD
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_orbit77_2001(xdate,latsun,lonsun,Lsubs,radius)
!.    real(kind=8) :: xdate 
!.    real(kind=8) :: latsun 
!.    real(kind=8) :: lonsun 
!.    real(kind=8) :: Lsubs 
!.    real(kind=8) :: radius 
!
!$Arguments
!>E     xdate   :<KIND=8>   
!>S     latsun  :<KIND=8>   
!>S     lonsun  :<KIND=8>   
!>S     Lsubs   :<KIND=8>   
!>S     radius  :<KIND=8>   
!
!$Common
!
!$Routines
!- cps_orbit_2001
!
!$Include
!
!$Module
!#V
!- cps_atm_gram
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
    use cps_atm_gram
    implicit none

      real(kind=8), intent(in)  :: xdate   !Julian date
      real(kind=8), intent(out) :: latsun  !latitude and longitude of sub-solar
      real(kind=8), intent(out) :: lonsun  !point on the surface
      real(kind=8), intent(out) :: Lsubs   !areocentric longitude of the Sun (Ls)
      real(kind=8), intent(out) :: radius  ! current orbital radius from Sun to Mars

    call cps_orbit_2001(xdate,latsun,lonsun,Lsubs,radius)
  end subroutine cps_orbit77_2001

  function cps_dustvsls77_2001(als) result(res)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_dustvsls77_2001
!
!$Resume
!  Retourne la saison martienne en sens GRAM 2001
!
!$Description
!  Retourne la saison martienne en sens GRAM 2001
!  en fonction du jour martien (type de tempete de poussiere)
!  Interface de cps_season_41 pour GS_ATMOSPHERES_EMCD
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  res = cps_dustvsls77_2001(als)
!.    real(kind=8) :: als 
!.    real(kind=8) :: res 
!
!$Arguments
!>E     als  :<KIND=8>   
!>S     res  :<KIND=8>   
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- cps_atm_gram
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
    use cps_atm_gram
    implicit none

      real(kind=8), intent(in)  :: als  ! solar angle (Ls)
      real(kind=8) :: res  ! Viking-like annual variation for non-
!!     dust-storm dust optical depth

    res= cps_dustvsls_2001(als)

  end function cps_dustvsls77_2001

 
  subroutine cps_marsephm77_M05(xday,sunlat,sunlon,sunLsubs,radius,owlt,EOT)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_marsephm77_M05
!
!$Resume
!  Retourne les latitudes et longitudes subsolaire
!
!$Description
!  Retourne le jour martien au sens GRAM 2001
!  Interface de cps_marsephm_M05 pour GS_ATMOSPHERES_MARS
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_marsephm77_M05(xday,sunlat,sunlon,sunLsubs,radius,owlt,EOT)
!.    real(kind=8) :: xday 
!.    real(kind=8) :: sunlat 
!.    real(kind=8) :: sunlon 
!.    real(kind=8) :: sunLsubs
!.    real(kind=8) :: radius 
!.    real(kind=8) :: owlt
!.    real(kind=8) :: EOT
!
!$Arguments
!>E     xday    :<KIND=8>   
!>S     sunlat  :<KIND=8>   
!>S     sunlon  :<KIND=8>   
!>S     sunLsubs:<KIND=8>   
!>S     radius  :<KIND=8>   
!>S     owlt    :<KIND=8>   
!>S     EOT     :<KIND=8>
!
!$Common
!
!$Routines
!- cps_marsephm_M05
!
!$Include
!
!$Module
!#V
!- cps_atm_gram05
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
    use cps_atm_gram05
    implicit none

      real(kind=8), intent(in)  :: xday     !date
      real(kind=8), intent(out) :: sunlat   !latitude of sub-solar point (deg)  
      real(kind=8), intent(out) :: sunlon   !longitude of sub-solar point (deg)
      real(kind=8), intent(out) :: sunLsubs !areocentric longitude of the Sun (Ls)
      real(kind=8), intent(out) :: radius   !current orbital radius from Sun to Mars
      real(kind=8), intent(out) :: owlt     !Mars-Earth one-way light time (minutes) 
      real(kind=8), intent(out) :: EOT      !Equation of time (deg)

  !     Computes sunlat, sunlon= latitude and longitude of sub-solar     
  !     point on the surface, sunLsubs= areocentric longitude of the Sun  
  !     (Ls), radius= current orbital radius from Sun to Mars, heliolon=  
  !     Mars heliocentric longitude, owlt= Mars-Earth one-way light       
  !     time (minutes), and EOT= equation of time (deg), calculated from  
  !     Julian day and time, xday.  Notes: input xday is NOT UTC, but   
  !     Terrestrial (Dynamical) Mars-Event Time (NOT Earth-Receive Time).
  !     Mars Local Mean Solar Time (hrs ) = Local True Solar Time (hrs)  
  !     minus EOT (in hrs). Output is for Terrestrial (Dynamical)         
  !     Mars Event Time (corresponding to input xday).                    

    call cps_marsephm_M05(xday,sunlat,sunlon,sunLsubs,radius,owlt,EOT)

  end subroutine cps_marsephm77_M05


  function cps_dustvsls77_M05(als,Dustmin,Dustmax) result(res)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_dustvsls77_M05
!
!$Resume
!  Retourne la saison martienne en sens GRAM 2005 en fonction de
!  l'angle solaire
!
!$Description
!  Retourne la saison martienne en sens GRAM 2005
!  en fonction du jour martien (type de tempete de poussiere)
!  Interface de cps_season_41 pour GS_ATMOSPHERES_EMCD
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  res = cps_dustvsls77_M05(als)
!.    real(kind=8) :: als 
!.    real(kind=8) :: Dustmin
!.    real(kind=8) :: Dustmax
!.    real(kind=8) :: res 
!
!$Arguments
!>E     als      :<KIND=8>   
!>E     Dustmin  :<KIND=8>
!>E     Dustmax  :<KIND=8>
!>S     res      :<KIND=8>   
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- cps_atm_gram05
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
    use cps_atm_gram05
    implicit none

      real(kind=8), intent(in)  :: als  ! solar angle (Ls)
      real(kind=8), intent(in)  :: Dustmin  ! Minimum seasonal dust tau 
      real(kind=8), intent(in)  :: Dustmax  ! Maximum seasonal dust tau 
      real(kind=8) :: res
  !Assumed seasonal variation (versus solar angle Ls) for non-       
  !dust-storm optical depth (Dustmin at Ls=90; Dustmax at Ls=270)    

    res= cps_dustvsls_M05(als,Dustmin,Dustmax)

  end function cps_dustvsls77_M05


  subroutine cps_init77()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_init77
!
!$Resume
! Initialisation de COMPAS.  (obsolète) 
!
!$Description
! V2.0
! Initialisation de COMPAS.
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_init77()
!
!$Arguments
!
!$Common
!
!$Routines
!- cps_init
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
    use cps_acces
    implicit none
    
    if (.not.cps_var_init) then
       call cps_init()
    end if
  end subroutine cps_init77

  
  subroutine cps_close77()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_close77
!
!$Resume
! Fermeture de COMPAS. (obsolète) 
!
!$Description
! V2.0
! Fermeture de COMPAS.
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_close77()
!
!$Arguments
!
!$Common
!
!$Routines
!- cps_close
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
    use cps_acces
    implicit none
    
    if (cps_var_init) then
       call cps_close()
    end if
  end subroutine cps_close77

  
  subroutine cps_restreindre77(id)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_restreindre77
!
!$Resume
! Restriction de l'utilisation des fichiers de COMPAS.
!
!$Description
! V2.0
! Restriction de l'utilisation des fichiers de COMPAS.
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_restreindre77(id)
!.    character(len=*) :: id
!
!$Arguments
!>E     id  :<LEN=*>   
!
!$Common
!
!$Routines
!- cps_restreindre
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
    use cps_acces
    implicit none
    ! argument
    character(len=*), intent(in) :: id
    
    call cps_restreindre(id)
  end subroutine cps_restreindre77

  
  subroutine cps_getlistId77(listId, nbId)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getlistId77
!
!$Resume
! Liste des ids disponibles pour les fichiers.
!
!$Description
! V2.0
! Routine qui renvoie la liste des ids
! disponibles pour les fichiers.
! Interface F77/GENESIS/C
! Le nombre d'ID renvoyés est limité à 100
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getlistId77(listId, nbId)
!.    character(len=CPS_MAXLG), dimension(100) :: listId
!.    integer :: nbId
!
!$Arguments
!>S     listId  :<LEN=CPS_MAXLG,DIM=(100)>   
!>S     nbId    :<integer>                   
!
!$Common
!
!$Routines
!- cpsi_getListId
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
    use cps_acces
    implicit none
    ! arguments
    character(len=CPS_MAXLG), dimension(100), intent(out) :: listId
    integer, intent(out) :: nbId
    
    ! variables locales
    character(len=CPS_MAXLG), dimension(:), pointer :: listId_tmp => NULL()
    integer :: iostat
    integer, parameter :: NB_MAX_ID = 100


    call cpsi_getListId(listId_tmp)
    nbId = cpsi_size_ptr(listId_tmp)
    if (nbId.gt.NB_MAX_ID) then
       nbId = NB_MAX_ID
    end if

    listId(1:nbId) = listId_tmp(1:nbId)

    if (associated(listId_tmp)) then
       deallocate(listId_tmp, stat=iostat)
    end if

  end subroutine cps_getlistId77

  
  subroutine cps_getlistCateg77(listCateg, nbCateg)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getlistCateg77
!
!$Resume
! Liste des categories disponibles
!
!$Description
! V2.0
! Routine qui renvoie la liste des categories (ie. tables)
! disponibles pour les fichiers.
! Interface F77/GENESIS/C
! La liste est de 100 catégories au maximum.
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getlistCateg77(listCateg, nbCateg)
!.    character(len=CPS_MAXLG), dimension(100) :: listCateg
!.    integer :: nbCateg
!
!$Arguments
!>S     listCateg  :<LEN=CPS_MAXLG,DIM=(100)>   
!>S     nbCateg    :<integer>                   
!
!$Common
!
!$Routines
!- cpsi_getListCateg
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
    use cps_acces
    implicit none
    ! arguments
    character(len=CPS_MAXLG), dimension(100), intent(out) :: listCateg
    integer, intent(out) :: nbCateg

    ! variables locales
    character(len=CPS_MAXLG), dimension(:), pointer :: listCateg_tmp => NULL()
    integer :: iostat
    integer, parameter :: NB_MAX_CATEG = 100

    call cpsi_getListCateg(listCateg_tmp)
    nbCateg = cpsi_size_ptr(listCateg_tmp)
    if (nbCateg.gt.NB_MAX_CATEG) then
       nbCateg = NB_MAX_CATEG
    end if

    listCateg(1:nbCateg) = listCateg_tmp(1:nbCateg)

    if (associated(listCateg_tmp)) then
       deallocate(listCateg_tmp, stat=iostat)
    end if

  end subroutine cps_getlistCateg77

  
  subroutine cps_getListCorps77(listCorps, nbCorps)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getListCorps77
!
!$Resume
! Liste des corps disponibles
!
!$Description
! V2.0
! Routine qui renvoie la liste des corps disponibles
! en tenant compte des restrictions d'utilisation.
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getListCorps77(listCorps, nbCorps)
!.    integer, dimension(10000) :: listCorps
!.    integer :: nbCorps
!
!$Arguments
!>S     listCorps  :<integer,DIM=(10000)>   
!>S     nbCorps    :<integer>               
!
!$Common
!
!$Routines
!- cps_getListCorps
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
    use cps_acces
    implicit none
    ! arguments
    integer, dimension(10000), intent(out) :: listCorps
    integer, intent(out) :: nbCorps
    
    ! variables locales
    integer, dimension(:), pointer :: list_corps => NULL()
    character(LEN=CPS_MAXLG), dimension(:), pointer :: list_noms => NULL()
    integer :: iostat
    integer, parameter :: NB_CORPS_MAX = 10000

    call cps_getListCorps(list_corps)

    nbCorps = cpsi_size_ptr(list_corps)
    ! Reduction du nombre de corps a 10000 au maximum
    if (nbCorps.gt.NB_CORPS_MAX) then
       nbCorps = NB_CORPS_MAX
    end if

    listCorps(1:nbCorps) = list_corps(1:nbCorps)
    
    if (associated(list_corps)) then
       deallocate(list_corps, stat=iostat)
    end if
    if (associated(list_noms)) then
       deallocate(list_noms, stat=iostat)
    end if

  end subroutine cps_getListCorps77


  subroutine cps_getListCorps77Int(nom_att, val_att, listCorps, nbCorps)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getListCorps77Int
!
!$Resume
! Liste des corps d'attribut 'nom_att' de valeur 'val_att' (entier)
!
!$Description
! V2.0
! Routine qui renvoie la liste des corps dont l'attribut 'nom_att' est
! a la valeur 'val_att', en tenant compte des restrictions
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getListCorps77Int(nom_att, val_att, listCorps, nbCorps)
!.    character(LEN=*) :: nom_att
!.    integer :: val_att
!.    integer, dimension(10000) :: listCorps
!.    integer :: nbCorps
!
!$Arguments
!>E     nom_att    :<LEN=*>                 
!>E     val_att    :<integer>               
!>S     listCorps  :<integer,DIM=(10000)>   
!>S     nbCorps    :<integer>               
!
!$Common
!
!$Routines
!- cps_getCorps
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
    use cps_acces
    implicit none
    ! arguments
    character(LEN=*), intent(in) :: nom_att
    integer, intent(in) :: val_att
    integer, dimension(10000), intent(out) :: listCorps
    integer, intent(out) :: nbCorps

    ! variables locales
    integer, dimension(:), pointer :: list_corps => NULL()
    integer :: iostat
    integer, parameter :: NB_CORPS_MAX = 10000

    call cps_getCorps(trim(nom_att), val_att, list_corps)
    ! Reduction du nombre de corps a 10000 au maximum
    nbCorps = cpsi_size_ptr(list_corps)
    if (nbCorps.gt.NB_CORPS_MAX) then
       nbCorps = NB_CORPS_MAX
    end if

    listCorps(1:nbCorps) = list_corps(1:nbCorps)
    
    if (associated(list_corps)) then
       deallocate(list_corps, stat=iostat)
    end if

  end subroutine cps_getListCorps77Int
  

  subroutine cps_getCorps77Min(nom_att, listCorps, nbCorps, valeurMin)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getCorps77Min
!
!$Resume
! Liste des corps d'attribut 'nom_att' > 'valeurMin'
!
!$Description
! V2.0
! Routine qui renvoie la liste des code des corps dont l'atribut
! 'nom_att' est superieur à la valeur 'valeurMin'.
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getCorps77Min(nom_att, listCorps, nbCorps, valeurMin)
!.    character(LEN=*) :: nom_att
!.    integer, dimension(10000) :: listCorps
!.    integer :: nbCorps
!.    real(KIND=PM_REEL) :: valeurMin
!
!$Arguments
!>E     nom_att    :<LEN=*>                Nom de l'attribut                 
!>S     listCorps  :<integer,DIM=(10000)>  Liste des code des corps   
!>S     nbCorps    :<integer>              Nombre de corps               
!>E     valeurMin  :<PM_REEL>              Valeur minimum recherchée  
!
!$Common
!
!$Routines
!- cps_getCorps
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
    use cps_acces
    implicit none
    ! arguments
    character(LEN=*), intent(in) :: nom_att
    integer, dimension(10000), intent(out) :: listCorps
    integer, intent(out) :: nbCorps
    real(KIND=PM_REEL), intent(in) :: valeurMin

    ! variables locales
    integer, dimension(:), pointer :: list_corps => NULL()
    integer :: iostat
    integer, parameter :: NB_CORPS_MAX = 10000

    nbCorps = 0

    call cps_getCorps(trim(nom_att), list_corps, valeurMin=valeurMin)
    
    nbCorps = cpsi_size_ptr(list_corps)
    ! Reduction du nombre de corps a 10000 au maximum
    if (nbCorps.gt.NB_CORPS_MAX) then
       nbCorps = NB_CORPS_MAX
    end if
    
    listCorps(1:nbCorps) = list_corps(1:nbCorps)

    if (associated(list_corps)) then
       deallocate(list_corps, stat=iostat)
    end if 

  end subroutine cps_getCorps77Min

  
  subroutine cps_getCorps77Max(nom_att, listCorps, nbCorps, valeurMax)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getCorps77Max
!
!$Resume
! Liste des corps d'attribut 'nom_att' < 'valeurMax'
!
!$Description
! V2.0
! Routine qui renvoie la liste des code des corps dont l'atribut
! 'nom_att' est inferieur à la valeur 'max'.
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getCorps77Max(nom_att, listCorps, nbCorps, valeurMax)
!.    character(LEN=*) :: nom_att
!.    integer, dimension(10000) :: listCorps
!.    integer :: nbCorps
!.    real(KIND=PM_REEL) :: valeurMax
!
!$Arguments
!>E     nom_att    :<LEN=*>               Nom de l'attribut     
!>S     listCorps  :<integer,DIM=(10000)> Liste des code des corps  
!>S     nbCorps    :<integer>             Nombre de corps 
!>E     valeurMax  :<PM_REEL>             Valeur maximum recherchée
!
!$Common
!
!$Routines
!- cps_getCorps
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
    use cps_acces
    implicit none
    ! arguments
    character(LEN=*), intent(in) :: nom_att
    integer, dimension(10000), intent(out) :: listCorps
    integer, intent(out) :: nbCorps
    real(KIND=PM_REEL), intent(in) :: valeurMax

    ! variables locales
    integer, dimension(:), pointer :: list_corps => NULL()
    integer :: iostat
    integer, parameter :: NB_CORPS_MAX = 10000

    nbCorps = 0

    call cps_getCorps(trim(nom_att), list_corps, valeurMax=valeurMax)
    
    nbCorps = cpsi_size_ptr(list_corps)

    if (nbCorps.gt.NB_CORPS_MAX) then
       nbCorps = NB_CORPS_MAX
    end if
    
    listCorps(1:nbCorps) = list_corps(1:nbCorps)

    if (associated(list_corps)) then
       deallocate(list_corps, stat=iostat)
    end if

  end subroutine cps_getCorps77Max

  
  subroutine cps_getCorps77MinMax(nom_att, listCorps, nbCorps, valeurMin, valeurMax)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getCorps77MinMax
!
!$Resume
! Liste des corps d'attribut 'nom_att' < 'max' et > 'min'
!
!$Description
! V2.0
! Routine qui renvoie la liste des code des corps dont l'atribut
! 'nom_att' est compris dans l'intervalle [valeurMin, valeurMax].
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getCorps77MinMax(nom_att, listCorps, nbCorps, valeurMin, valeurMax)
!.    character(LEN=*) :: nom_att
!.    integer, dimension(10000) :: listCorps
!.    integer :: nbCorps
!.    real(KIND=PM_REEL) :: valeurMin, valeurMax
!
!$Arguments
!>E     nom_att    :<LEN=*>               Nom de l'attribut          
!>S     listCorps  :<integer,DIM=(10000)> Liste des code des corps  
!>S     nbCorps    :<integer>             Nombre de corps 
!>E     valeurMin  :<PM_REEL>             Valeur minimum recherchée  
!>E     valeurMax  :<PM_REEL>             Valeur maximum recherchée  
!
!$Common
!
!$Routines
!- cps_getCorps
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
    use cps_acces
    implicit none
    ! arguments
    character(LEN=*), intent(in) :: nom_att
    integer, dimension(10000), intent(out) :: listCorps
    integer, intent(out) :: nbCorps
    real(KIND=PM_REEL), intent(in) :: valeurMin, valeurMax

    ! variables locales
    integer, dimension(:), pointer :: list_corps => NULL()
    integer :: iostat
    integer, parameter :: NB_CORPS_MAX = 10000

    nbCorps = 0

    call cps_getCorps(trim(nom_att), list_corps, valeurMin=valeurMin, valeurMax=valeurMax)
    
    nbCorps = cpsi_size_ptr(list_corps)
    ! Reduction du nombre de corps a 10000 au maximum
    if (nbCorps.gt.NB_CORPS_MAX) then
       nbCorps = NB_CORPS_MAX
    end if
    
    listCorps(1:nbCorps) = list_corps(1:nbCorps)

    if (associated(list_corps)) then
       deallocate(list_corps, stat=iostat)
    end if

  end subroutine cps_getCorps77MinMax

  
  subroutine cps_getListCorps77String(nom_att, val_att, listCorps, nbCorps)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getListCorps77String
!
!$Resume
! Liste des corps d'attribut 'nom_att' de valeur 'val_att' (string)
!
!$Description
! V2.0
! Routine qui renvoie la liste des corps dont l'attribut 'nom_att' est
! a la valeur 'val_att', en tenant compte des restrictions.
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getListCorps77String(nom_att, val_att, listCorps, nbCorps)
!.    character(LEN=*) :: nom_att
!.    character(LEN=*) :: val_att
!.    integer, dimension(10000) :: listCorps
!.    integer :: nbCorps
!
!$Arguments
!>E     nom_att    :<LEN=*>                 
!>E     val_att    :<LEN=*>                 
!>S     listCorps  :<integer,DIM=(10000)>   
!>S     nbCorps    :<integer>               
!
!$Common
!
!$Routines
!- cps_getCorps
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
    use cps_acces
    implicit none
    ! arguments
    character(LEN=*), intent(in) :: nom_att
    character(LEN=*), intent(in) :: val_att
    integer, dimension(10000), intent(out) :: listCorps
    integer, intent(out) :: nbCorps

    ! variables locales
    integer, dimension(:), pointer :: list_corps => NULL()
    integer :: iostat
    integer, parameter :: NB_CORPS_MAX = 10000

    call cps_getCorps(trim(nom_att), val_att, list_corps)
    nbCorps = cpsi_size_ptr(list_corps)
    if (nbCorps.gt.NB_CORPS_MAX) then
       nbCorps = NB_CORPS_MAX
    end if

    listCorps(1:nbCorps) = list_corps(1:nbCorps)
    
    if (associated(list_corps)) then
       deallocate(list_corps, stat=iostat)
    end if

  end subroutine cps_getListCorps77String

  
  function cps_getParamKepThCourante77(corps, paramKep) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getParamKepThCourante77
!
!$Resume
!  Retour des parametres kepleriens d'un corps (theorie courante)
!
!$Description
! V2.0
! Obtenir les parametres kepleriens d'un corps selon la theorie courante.
! Date de reference a recuperer par ailleurs
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getParamKepThCourante77(corps, paramKep)
!.    integer :: corps
!.    real(KIND=PM_REEL), dimension(7) :: paramKep
!.    integer :: trouve
!
!$Arguments
!>E     corps     :<integer>           
!>S     paramKep  :<PM_REEL,DIM=(7)>   
!>S     trouve    :<integer>           
!
!$Common
!
!$Routines
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
    use cps_acces
    implicit none
    ! arguments
    integer, intent(in) :: corps
    ! paramKep(1) : dga
    ! paramKep(2) : exc
    ! paramKep(3) : inc
    ! paramKep(4) : pom
    ! paramKep(5) : gom
    ! paramKep(6) : anm
    ! paramKep(7) : dateref
    real(KIND=PM_REEL), dimension(7), intent(out) :: paramKep
    
    ! resultat
    integer :: trouve

    ! variables locales
    type(tm_orb_kep) :: param_kepl
    
    ! initialisation
    trouve = CPS_ERR_DEF
    
    ! appel au module cps_utilisateur
    trouve = cps_getKeplerThCourante(corps, param_kepl, paramKep(7))
    if (trouve.eq.CPS_OK) then
       paramKep(1) = param_kepl%a
       paramKep(2) = param_kepl%e
       paramKep(3) = param_kepl%i
       paramKep(4) = param_kepl%pom
       paramKep(5) = param_kepl%gom
       paramKep(6) = param_kepl%M
    end if

  end function cps_getParamKepThCourante77

  
  function cps_getParamKepTh77(corps, theorie, paramKep) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getParamKepTh77
!
!$Resume
!  Retour des parametres kepleriens d'un corps (theorie donnée)
!
!$Description
! V2.0
! Obtenir les parametres kepleriens d'un corps selon une theorie.
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getParamKepTh77(corps, theorie, paramKep)
!.    integer :: corps
!.    character(LEN=*) :: theorie
!.    real(KIND=PM_REEL), dimension(7) :: paramKep
!.    integer :: trouve
!
!$Arguments
!>E     corps     :<integer>           
!>E     theorie   :<LEN=*>             
!>S     paramKep  :<PM_REEL,DIM=(7)>   
!>S     trouve    :<integer>           
!
!$Common
!
!$Routines
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
    use cps_acces
    implicit none
    ! arguments
    integer, intent(in) :: corps
    ! paramKep(1) : dga
    ! paramKep(2) : exc
    ! paramKep(3) : inc
    ! paramKep(4) : pom
    ! paramKep(5) : gom
    ! paramKep(6) : anm
    ! paramKep(7) : dateref
    character(LEN=*), intent(in) :: theorie
    real(KIND=PM_REEL), dimension(7), intent(out) :: paramKep
    
    ! resultat
    integer :: trouve
    
    ! variables locales
    type(tm_orb_kep) :: param_kepl
    
    ! initialisation
    trouve = CPS_ERR_DEF

    ! appel au module cps_utilisateur
    trouve = cps_getKeplerTh(corps, trim(theorie), param_kepl, paramKep(7))
    if (trouve.eq.CPS_OK) then
       paramKep(1) = param_kepl%a
       paramKep(2) = param_kepl%e
       paramKep(3) = param_kepl%i
       paramKep(4) = param_kepl%pom
       paramKep(5) = param_kepl%gom
       paramKep(6) = param_kepl%M
    end if
    
  end function cps_getParamKepTh77

  
  subroutine cps_getInfosCorps77(corps, nb_att, &
       noms_att, types_att, vals_i, vals_d, vals_s, unites, att_def)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getInfosCorps77
!
!$Resume
!  Retour des infos generales sur un corps 
!
!$Description
! V2.0
! Routine qui renvoie les attributs disponibles pour un corps
! dans les fichiers associes aux categories CPS_CATEG_CORPS,
! CPS_CATEG_CSTES_CORPS_THEORIE et CPS_CATEG_KEPLER_CORPS_THEORIE
! pour la theorie courante.
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getInfosCorps77(corps, nb_att, &
!.           noms_att, types_att, vals_i, vals_d, vals_s, unites, att_def)
!.    integer :: corps
!.    integer :: nb_att
!.    character(LEN=CPS_MAXLG), dimension(100) :: noms_att
!.    integer, dimension(100) :: types_att
!.    integer, dimension(100) :: vals_i
!.    real(KIND=PM_REEL), dimension(100) :: vals_d
!.    character(LEN=CPS_MAXLG), dimension(100) :: vals_s
!.    character(LEN=CPS_MAXLG), dimension(100) :: unites
!.    logical, dimension(100) :: att_def
!
!$Arguments
!>E     corps      :<integer>                   
!>S     nb_att     :<integer>                   
!>S     noms_att   :<LEN=CPS_MAXLG,DIM=(100)>   
!>S     types_att  :<integer,DIM=(100)>         
!>S     vals_i     :<integer,DIM=(100)>         
!>S     vals_d     :<PM_REEL,DIM=(100)>         
!>S     vals_s     :<LEN=CPS_MAXLG,DIM=(100)>   
!>S     unites     :<LEN=CPS_MAXLG,DIM=(100)>   
!>S     att_def    :<logical,DIM=(100)>         
!
!$Common
!
!$Routines
!- cps_getInfosCorps
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
    use cps_acces
    implicit none
    ! arguments
    integer, intent(in) :: corps
    integer, intent(out) :: nb_att
    character(LEN=CPS_MAXLG), dimension(100), intent(out) :: noms_att
    integer, dimension(100), intent(out) :: types_att
    integer, dimension(100), intent(out) :: vals_i
    real(KIND=PM_REEL), dimension(100), intent(out) :: vals_d
    character(LEN=CPS_MAXLG), dimension(100), intent(out) :: vals_s
    character(LEN=CPS_MAXLG), dimension(100), intent(out) :: unites
    logical, dimension(100), intent(out) :: att_def

    ! variables locales
    character(LEN=CPS_MAXLG), dimension(:), pointer :: noms_att_tmp => NULL()
    integer, dimension(:), pointer :: types_att_tmp => NULL()
    integer, dimension(:), pointer :: vals_i_tmp => NULL()
    real(KIND=PM_REEL), dimension(:), pointer :: vals_d_tmp => NULL()
    character(LEN=CPS_MAXLG), dimension(:), pointer :: vals_s_tmp => NULL()
    character(LEN=CPS_MAXLG), dimension(:), pointer :: unites_tmp => NULL()
    logical, dimension(:), pointer :: att_def_tmp => NULL()
    integer, parameter :: NB_ATT_MAX = 100

    integer :: ind_att
    integer :: iostat

    ! appel au module cps_utilisateur
    call cps_getInfosCorps(corps, &
         noms_att_tmp,            &
         types_att_tmp,           &
         vals_i_tmp,              &
         vals_d_tmp,              &
         vals_s_tmp,              &
         unites_tmp,              &
         att_def_tmp)
    
    nb_att = cpsi_size_ptr(noms_att_tmp)
    ! Limitation du nombre d'attributs a 100
    if (nb_att.gt.NB_ATT_MAX) then
       nb_att = NB_ATT_MAX
    end if
    
    ! initialisation
    noms_att(:) = ""
    types_att(:) = -1
    vals_i(:) = 0
    vals_d(:) = 0.
    vals_s(:) = ""
    unites(:) = ""

    do ind_att=1, nb_att
       noms_att(ind_att) = noms_att_tmp(ind_att)
       types_att(ind_att) = types_att_tmp(ind_att)
       select case (types_att(ind_att))
       case (CPS_ENTIER)
          vals_i(ind_att) = vals_i_tmp(ind_att)
       case (CPS_REEL)
          vals_d(ind_att) = vals_d_tmp(ind_att)
          unites(ind_att) = trim(unites_tmp(ind_att))
       case (CPS_STRING)
          vals_s(ind_att) = vals_s_tmp(ind_att)
       end select
       att_def(ind_att) = att_def_tmp(ind_att)
    end do
    
    
    ! liberation memoire
    if (associated(noms_att_tmp)) then
       deallocate(noms_att_tmp, stat=iostat)
    end if
    if (associated(types_att_tmp)) then
       deallocate(types_att_tmp, stat=iostat)
    end if
    if (associated(vals_i_tmp)) then
       deallocate(vals_i_tmp, stat=iostat)
    end if
    if (associated(vals_d_tmp)) then
       deallocate(vals_d_tmp, stat=iostat)
    end if
    if (associated(vals_s_tmp)) then
       deallocate(vals_s_tmp, stat=iostat)
    end if
    if (associated(unites_tmp)) then
       deallocate(unites_tmp, stat=iostat)
    end if
    if (associated(att_def_tmp)) then
       deallocate(att_def_tmp, stat=iostat)
    end if

  end subroutine cps_getInfosCorps77

  
  subroutine cps_getInfosCorps77SansDoublon(corps, nb_att, &
       noms_att, types_att, vals_i, vals_d, vals_s, unites, att_def)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getInfosCorps77SansDoublon
!
!$Resume
!  Retour des infos generales sur un corps  (sans redondance)
!
!$Description
! V2.0
! Routine qui renvoie les attributs disponibles sans redondance pour un corps
! dans les fichiers associes aux categories CPS_CATEG_CORPS,
! CPS_CATEG_CSTES_CORPS_THEORIE et CPS_CATEG_KEPLER_CORPS_THEORIE
! pour la theorie courante.
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getInfosCorps77SansDoublon(corps, nb_att, &
!.           noms_att, types_att, vals_i, vals_d, vals_s, unites, att_def)
!.    integer :: corps
!.    integer :: nb_att
!.    character(LEN=CPS_MAXLG), dimension(100) :: noms_att
!.    integer, dimension(100) :: types_att
!.    integer, dimension(100) :: vals_i
!.    real(KIND=PM_REEL), dimension(100) :: vals_d
!.    character(LEN=CPS_MAXLG), dimension(100) :: vals_s
!.    character(LEN=CPS_MAXLG), dimension(100) :: unites
!.    logical, dimension(100) :: att_def
!
!$Arguments
!>E     corps      :<integer>                   
!>S     nb_att     :<integer>                   
!>S     noms_att   :<LEN=CPS_MAXLG,DIM=(100)>   
!>S     types_att  :<integer,DIM=(100)>         
!>S     vals_i     :<integer,DIM=(100)>         
!>S     vals_d     :<PM_REEL,DIM=(100)>         
!>S     vals_s     :<LEN=CPS_MAXLG,DIM=(100)>   
!>S     unites     :<LEN=CPS_MAXLG,DIM=(100)>   
!>S     att_def    :<logical,DIM=(100)>         
!
!$Common
!
!$Routines
!- cps_getInfosCorpsSansDoublon
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
    use cps_acces
    implicit none
    ! arguments
    integer, intent(in) :: corps
    integer, intent(out) :: nb_att
    character(LEN=CPS_MAXLG), dimension(100), intent(out) :: noms_att
    integer, dimension(100), intent(out) :: types_att
    integer, dimension(100), intent(out) :: vals_i
    real(KIND=PM_REEL), dimension(100), intent(out) :: vals_d
    character(LEN=CPS_MAXLG), dimension(100), intent(out) :: vals_s
    character(LEN=CPS_MAXLG), dimension(100), intent(out) :: unites
    logical, dimension(100), intent(out) :: att_def
    
    ! variables locales
    character(LEN=CPS_MAXLG), dimension(:), pointer :: noms_att_tmp => NULL()
    integer, dimension(:), pointer :: types_att_tmp => NULL()
    integer, dimension(:), pointer :: vals_i_tmp => NULL()
    real(KIND=PM_REEL), dimension(:), pointer :: vals_d_tmp => NULL()
    character(LEN=CPS_MAXLG), dimension(:), pointer :: vals_s_tmp => NULL()
    character(LEN=CPS_MAXLG), dimension(:), pointer :: unites_tmp => NULL()
    logical, dimension(:), pointer :: att_def_tmp => NULL()
    integer, parameter :: NB_ATT_MAX = 100
    
    integer :: ind_att
    integer :: iostat

    ! appel au module cps_utilisateur
    call cps_getInfosCorpsSansDoublon(corps, &
         noms_att_tmp,                       &
         types_att_tmp,                      &
         vals_i_tmp,                         &
         vals_d_tmp,                         &
         vals_s_tmp,                         &
         unites_tmp,                         &
         att_def_tmp)
    
    nb_att = cpsi_size_ptr(noms_att_tmp)
    ! Limitation du nombre d'attributs a 100
    if (nb_att.gt.NB_ATT_MAX) then
       nb_att = NB_ATT_MAX
    end if

    ! initialisation
    noms_att(:) = ""
    types_att(:) = -1
    vals_i(:) = 0
    vals_d(:) = 0.
    vals_s(:) = ""
    unites(:) = ""

    do ind_att=1, nb_att
       noms_att(ind_att) = trim(noms_att_tmp(ind_att))
       types_att(ind_att) = types_att_tmp(ind_att)
       vals_i(ind_att) = vals_i_tmp(ind_att)
       vals_d(ind_att) = vals_d_tmp(ind_att)
       unites(ind_att) = trim(unites_tmp(ind_att))
       vals_s(ind_att) = trim(vals_s_tmp(ind_att))
       att_def(ind_att) = att_def_tmp(ind_att)
    end do
    
    
    ! liberation memoire
    if (associated(noms_att_tmp)) then
       deallocate(noms_att_tmp, stat=iostat)
    end if
    if (associated(types_att_tmp)) then
       deallocate(types_att_tmp, stat=iostat)
    end if
    if (associated(vals_i_tmp)) then
       deallocate(vals_i_tmp, stat=iostat)
    end if
    if (associated(vals_d_tmp)) then
       deallocate(vals_d_tmp, stat=iostat)
    end if
    if (associated(vals_s_tmp)) then
       deallocate(vals_s_tmp, stat=iostat)
    end if
    if (associated(unites_tmp)) then
       deallocate(unites_tmp, stat=iostat)
    end if
    if (associated(att_def_tmp)) then
       deallocate(att_def_tmp, stat=iostat)
    end if

  end subroutine cps_getInfosCorps77SansDoublon


  
  subroutine cps_getNoms77(listCorps, nb_corps, lang, listNoms)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getNoms77
!
!$Resume
! Retour des noms de corps
!
!$Description
! V2.0
! Routine qui renvoie les noms d'une liste de corps dans la
! langue 'lang' ('fr' ou 'en').
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getNoms77(listCorps, nb_corps, lang, listNoms)
!.    integer, dimension(*) :: listCorps
!.    integer :: nb_corps
!.    character(LEN=*) :: lang
!.    character(LEN=CPS_MAXLG), dimension(*) :: listNoms
!
!$Arguments
!>E     listCorps  :<integer,DIM=(*)>         
!>E     nb_corps   :<integer>                     
!>E     lang       :<LEN=*>                       
!>S     listNoms   :<LEN=CPS_MAXLG,DIM=(*)>   
!
!$Common
!
!$Routines
!- cps_getNoms
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
    use cps_acces
    implicit none 
    ! arguments
    integer, dimension(*), intent(in) :: listCorps
    integer, intent(in) :: nb_corps
    character(LEN=*), intent(in) :: lang
    character(LEN=CPS_MAXLG), dimension(*), intent(out) :: listNoms

    ! variables locales
    character(LEN=CPS_MAXLG), dimension(:), pointer :: list_noms_tmp => NULL()
    integer :: iostat, ii

    ! Initialisation
    do ii=1, nb_corps
        listNoms(ii) = ""
    enddo

    ! Appel
    call cps_getNoms(listCorps(1:nb_corps), lang, list_noms_tmp)
    ! Recopie des valeurs de la structure temporaire
    do ii=1, nb_corps
       listNoms(ii) = list_noms_tmp(ii)
    enddo

    ! liberation memoire
    if (associated(list_noms_tmp)) then
       deallocate(list_noms_tmp, stat=iostat)
    end if

  end subroutine cps_getNoms77



  subroutine cps_intersecterCodes77(list_codes_criteres, nb_codes_criteres, &
       nb_criteres, list_codes, nb_codes)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_intersecterCodes77
!
!$Resume
! Retourne l'intersection de plusieurs listes
!
!$Description
! V2.0
! Routine d'inserction de plusieurs listes de codes.
! Interface F77/GENESIS/C 
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_intersecterCodes77(list_codes_criteres, nb_codes_criteres, &
!.           nb_criteres, list_codes, nb_codes)
!.    integer, dimension(10,10000) :: list_codes_criteres
!.    integer, dimension(10) :: nb_codes_criteres
!.    integer :: nb_criteres
!.    integer, dimension(10000) :: list_codes
!.    integer :: nb_codes
!
!$Arguments
!>E     list_codes_criteres  :<integer,DIM=(10,10000)>   
!>E     nb_codes_criteres    :<integer,DIM=(10)>         
!>E     nb_criteres          :<integer>                  
!>S     list_codes           :<integer,DIM=(10000)>      
!>S     nb_codes             :<integer>                  
!
!$Common
!
!$Routines
!- cps_intersecterCodes
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
    use cps_acces
    implicit none
    ! arguments
    integer, dimension(10,10000), intent(in) :: list_codes_criteres
    integer, dimension(10), intent(in) :: nb_codes_criteres
    integer, intent(in) :: nb_criteres
    integer, dimension(10000), intent(out) :: list_codes
    integer, intent(out) :: nb_codes
    
    ! variables locales
    integer :: i, nb_max_corps
    integer, dimension(:,:), pointer :: codes_entree => NULL()
    integer, dimension(:), pointer :: codes_tmp => NULL()
    integer :: iostat
    
    ! initialisation
    nb_max_corps = 0
    do i=1, nb_criteres
       if (nb_codes_criteres(i).gt.nb_max_corps) then
          nb_max_corps = nb_codes_criteres(i)
       end if
    end do

    ! allocation memoire
    allocate(codes_entree(nb_criteres,nb_max_corps))
    
    codes_entree(1:nb_criteres, 1:nb_max_corps) = list_codes_criteres(1:nb_criteres, 1:nb_max_corps)

    ! intersection
    call cps_intersecterCodes(codes_entree, nb_criteres, codes_tmp)

    ! recopie du resultats
    nb_codes = cpsi_size_ptr(codes_tmp)
    list_codes(1:nb_codes) = codes_tmp(1:nb_codes)
    
    ! liberation memoire
    if (associated(codes_entree)) then
       deallocate(codes_entree, stat=iostat)
    end if
    if (associated(codes_tmp)) then
       deallocate(codes_tmp, stat=iostat)
    end if
    
  end subroutine cps_intersecterCodes77


  subroutine cps_getTheorie77(theorie)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getTheorie77
!
!$Resume
! theorie courante
!
!$Description
! V2.0
! Routine qui renvoie la theorie courante.
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getTheorie77(theorie)
!.    character(LEN=*) :: theorie
!
!$Arguments
!>S     theorie  :<LEN=*>   
!
!$Common
!
!$Routines
!- cps_getTheorie
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
    use cps_acces
    implicit none
    ! argument
    character(LEN=*), intent(out) :: theorie

    call cps_getTheorie(theorie)
  end subroutine cps_getTheorie77


  subroutine cps_setTheorie77(theorie)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_setTheorie77
!
!$Resume
!  Fixe la théorie courante
!
!$Description
! V2.0
! Routine qui positionne la théorie courante.
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_setTheorie77(theorie)
!.    character(LEN=*) :: theorie
!
!$Arguments
!>E     theorie  :<LEN=*>   
!
!$Common
!
!$Routines
!- cps_setTheorie
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
    use cps_acces
    implicit none
    ! argument
    character(LEN=*), intent(in) :: theorie
    
    call cps_setTheorie(theorie)
  end subroutine cps_setTheorie77

  
  subroutine cps_getListTheories77(listeTheories, nbTheories)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getListTheories77
!
!$Resume
!  Liste des theories disponibles
!
!$Description
! V2.0
! Renvoie la liste des theories disponibles.
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getListTheories77(listeTheories, nbTheories)
!.    character(LEN=CPS_MAXLG), dimension(100) :: listeTheories
!.    integer :: nbTheories
!
!$Arguments
!>S     listeTheories  :<LEN=CPS_MAXLG,DIM=(100)>   
!>S     nbTheories     :<integer>                   
!
!$Common
!
!$Routines
!- cps_getListTheories
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
    use cps_acces
    implicit none
    !argmument
    character(LEN=CPS_MAXLG), dimension(100), intent(out) :: listeTheories
    integer, intent(out) :: nbTheories

    ! variables locales
    character(LEN=CPS_MAXLG), dimension(:), pointer :: listeTheories_tmp => NULL()
    integer :: i
    integer :: iostat
    integer, parameter :: NB_THEORIES_MAX = 100

    ! initialisation
    nbTheories = 0

    ! 
    call cps_getListTheories(listeTheories_tmp)
    nbTheories = cpsi_size_ptr(listeTheories_tmp)
    ! Limitation du nombre de théories a 100
    if (nbTheories.gt.NB_THEORIES_MAX) then
        nbTheories = NB_THEORIES_MAX
    end if
    do i=1, nbTheories
       listeTheories(i) = trim(listeTheories_tmp(i))
    end do
    
    ! liberation memoire
    if (associated(listeTheories_tmp)) then
       deallocate(listeTheories_tmp, stat=iostat)
    end if
    
  end subroutine cps_getListtheories77

  
  subroutine cps_getListFichiersLocal77(listFichiers, nb)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getListFichiersLocal77
!
!$Resume
! Liste des fichiers de la base locale.
!
!$Description
! V2.0
! Renvoie la liste des fichiers de la base locale.
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getListFichiersLocal77(listFichiers, nb)
!.    character(LEN=CPS_MAXLG), dimension(200) :: listFichiers
!.    integer :: nb
!
!$Arguments
!>S     listFichiers  :<LEN=CPS_MAXLG,DIM=(200)>   
!>S     nb            :<integer>                   
!
!$Common
!
!$Routines
!- cps_getListFichiersLocal
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
    use cps_acces
    implicit none
    ! arguments
    character(LEN=CPS_MAXLG), dimension(200), intent(out) :: listFichiers
    integer, intent(out) :: nb

    ! variables locales
    character(LEN=CPS_MAXLG), dimension(:), pointer :: list_fichiers => NULL()
    integer :: iostat
    integer, parameter :: NB_FICHIERS_MAX = 200
    
    call cps_getListFichiersLocal(list_fichiers)
    nb = cpsi_size_ptr(list_fichiers)
    ! Réduction du nombre de fichiers à 200
    if (nb.gt.NB_FICHIERS_MAX) then
       nb = NB_FICHIERS_MAX
    end if
    listFichiers(1:nb) = list_fichiers(1:nb)
    
    ! liberation memoire
    if (associated(list_fichiers)) then
       deallocate(list_fichiers, stat=iostat)
    end if
    
  end subroutine cps_getListFichiersLocal77

  
  subroutine cps_getListFichiersRef77(listFichiers, nb)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getListFichiersRef77
!
!$Resume
!  Liste des fichiers de la base de reference
!
!$Description
! Renvoie la liste des fichiers de la base de reference.
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getListFichiersRef77(listFichiers, nb)
!.    character(LEN=CPS_MAXLG), dimension(200) :: listFichiers
!.    integer :: nb
!
!$Arguments
!>S     listFichiers  :<LEN=CPS_MAXLG,DIM=(200)>   
!>S     nb            :<integer>                   
!
!$Common
!
!$Routines
!- cps_getListFichiersRef
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
    use cps_acces
    implicit none
    ! arguments
    character(LEN=CPS_MAXLG), dimension(200), intent(out) :: listFichiers
    integer, intent(out) :: nb

    ! variables locales
    character(LEN=CPS_MAXLG), dimension(:), pointer :: list_fichiers => NULL()
    integer :: iostat
    integer, parameter :: NB_FICHIERS_MAX = 200
    
    call cps_getListFichiersRef(list_fichiers)
    nb = cpsi_size_ptr(list_fichiers)
    ! Réduction du nombre de fichiers à 200
    if (nb.gt.NB_FICHIERS_MAX) then
       nb = NB_FICHIERS_MAX
    end if
    listFichiers(1:nb) = list_fichiers(1:nb)
    
    ! liberation memoire
    if (associated(list_fichiers)) then
       deallocate(list_fichiers, stat=iostat)
    end if
    
  end subroutine cps_getListFichiersRef77

  
  subroutine cps_getRepBaseRef77(rep)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getRepBaseRef77
!
!$Resume
!  Repertoire de la base de reference
!
!$Description
! V2.0
! Renvoie le repertoire de la base de reference.
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getRepBaseRef77(rep)
!.    character(LEN=*) :: rep
!
!$Arguments
!>S     rep  :<LEN=*>   
!
!$Common
!
!$Routines
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
    use cps_acces
    implicit none
    ! arguments
    character(LEN=*), intent(out) :: rep

    rep = trim(rep_base_ref)
  end subroutine cps_getRepBaseRef77

  
  subroutine cps_getRepAndNameFic77(path, rep, name)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getRepAndNameFic77
!
!$Resume
!  retourne le repertoire et le fichier d'un path
!
!$Description
! V2.0
! Separe le chemin absolu d'un fichier en son chemin 
! et son nom
! exemple :
! path = /home/user/dev/COMPAS/db_ref/config
! =>
! rep = /home/user/dev/COMPAS/db_ref
! name = config
! Cf. basename/dirname
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getRepAndNameFic77(path, rep, name)
!.    character(LEN=*) :: path
!.    character(LEN=*) :: rep, name
!
!$Arguments
!>E     path  :<LEN=*>   
!>S     rep   :<LEN=*>   
!>S     name  :<LEN=*>   
!
!$Common
!
!$Routines
!- cpsi_getRepAndNameFic
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
    use cps_acces
    implicit none
    ! arguments
    character(LEN=*), intent(in) :: path
    character(LEN=*), intent(out) :: rep, name
    
    call cpsi_getRepAndNameFic(path, rep, name)
    
  end subroutine cps_getRepAndNameFic77


  subroutine cps_setBaseLocale77(repBaseLocale)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_setBaseLocale77
!
!$Resume
!  Charger une base locale.
!
!$Description
! V2.0
! Charger une base locale.
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_setBaseLocale77(repBaseLocale)
!.    character(LEN=*) :: repBaseLocale
!
!$Arguments
!>E     repBaseLocale  :<LEN=*>   
!
!$Common
!
!$Routines
!- cps_setBaseLocale
!
!$Include
!
!$Module
!#V
!- cps_utilisateur
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
    use cps_utilisateur
    implicit none
    ! arguments
    character(LEN=*), intent(in) :: repBaseLocale
    
    call cps_setBaseLocale(repBaseLocale)

  end subroutine cps_setBaseLocale77

  
  function cps_getAtt77_i(code_corps, nom_att, val_i) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getAtt77_i
!
!$Resume
!  Données sur un corps hors theorie (entiers)
!
!$Description
! V2.0
! Obtenir une donnee pour un corps de type ENTIER.
! Les donnees sont recherchees dans les fichiers de la categorie 'corps'.
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getAtt77_i(code_corps, nom_att, val_i)
!.    integer :: code_corps
!.    character(LEN=*) :: nom_att
!.    integer :: val_i
!.    integer :: trouve
!
!$Arguments
!>E     code_corps  :<integer>   
!>E     nom_att     :<LEN=*>     
!>S     val_i       :<integer>   
!>S     trouve      :<integer>   
!
!$Common
!
!$Routines
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
    use cps_acces
    implicit none
    ! arguments
    integer, intent(in) :: code_corps
    character(LEN=*), intent(in) :: nom_att
    integer, intent(out) :: val_i
    
    ! resultat
    integer :: trouve
    
    ! initialisation
    trouve = CPS_ERR_DEF
    
    ! appel au module cps_utilisateur
    trouve = cps_getAtt(code_corps, trim(nom_att), val_i)

  end function cps_getAtt77_i

  
  function cps_getAtt77_s(code_corps, nom_att, val_s) result(trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getAtt77_s
!
!$Resume
!  Données sur un corps hors theorie (string)
!
!$Description
! V2.0
! Obtenir une donnee pour un corps de type STRING.
! Les donnees sont recherchees dans les fichiers de la categorie 'corps'.
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getAtt77_s(code_corps, nom_att, val_s)
!.    integer :: code_corps
!.    character(LEN=*) :: nom_att
!.    character(LEN=*) :: val_s
!.    integer :: trouve
!
!$Arguments
!>E     code_corps  :<integer>   
!>E     nom_att     :<LEN=*>     
!>S     val_s       :<LEN=*>     
!>S     trouve      :<integer>   
!
!$Common
!
!$Routines
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
    use cps_acces
    implicit none
    ! arguments
    integer, intent(in) :: code_corps
    character(LEN=*), intent(in) :: nom_att
    character(LEN=*), intent(out) :: val_s
    
    ! resultat
    integer :: trouve
    
    ! initialisation
    trouve = CPS_ERR_DEF
    
    ! appel au module cps_utilisateur
    trouve = cps_getAtt(code_corps, trim(nom_att), val_s)

  end function cps_getAtt77_s

  
  function cps_getCsteGenTh77(nom_theorie, nom_cste, val, unite) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getCsteGenTh77
!
!$Resume
!  constante generale selon une theorie
!
!$Description
! V2.0
! Obtenir une constante generale de l'astronomie selon une theorie.
! Les donnees sont recherchees dans les fichiers de la categorie 'theorie'.
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getCsteGenTh77(nom_theorie, nom_cste, val, unite)
!.    character(LEN=*) :: nom_theorie, nom_cste
!.    real(KIND=PM_REEL) :: val
!.    character(LEN=*) :: unite
!.    integer :: trouve
!
!$Arguments
!>E     nom_theorie  :<LEN=*>     
!>E     nom_cste     :<LEN=*>     
!>S     val          :<PM_REEL>   
!>S     unite        :<LEN=*>     
!>S     trouve       :<integer>   
!
!$Common
!
!$Routines
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
    use cps_acces
    implicit none
    ! arguments
    character(LEN=*), intent(in) :: nom_theorie, nom_cste
    real(KIND=PM_REEL), intent(out) :: val
    character(LEN=*), intent(out) :: unite
    
    ! resultat
    integer :: trouve
    
    ! initialisation
    trouve = CPS_ERR_DEF

    ! appel au module cps_utilisateur
     trouve = cps_getCsteGenTh(trim(nom_theorie), trim(nom_cste), val, unite)
    
  end function cps_getCsteGenTh77


  function cps_getCsteGenThCourante77(nom_cste, val, unite) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getCsteGenThCourante77
!
!$Resume
!  constante generale selon la theorie courante
!
!$Description
! V2.0
! Obtenir une constante generale de l'astronomie selon la theorie courante.
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getCsteGenThCourante77(nom_cste, val, unite)
!.    character(LEN=*) :: nom_cste
!.    real(KIND=PM_REEL) :: val
!.    character(LEN=*) :: unite
!.    integer :: trouve
!
!$Arguments
!>E     nom_cste  :<LEN=*>     
!>S     val       :<PM_REEL>   
!>S     unite     :<LEN=*>     
!>S     trouve    :<integer>   
!
!$Common
!
!$Routines
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
    use cps_acces
    implicit none
    ! arguments
    character(LEN=*), intent(in) :: nom_cste
    real(KIND=PM_REEL), intent(out) :: val
    character(LEN=*), intent(out) :: unite
    
    ! resultat
    integer :: trouve
    
    ! initialisation
    trouve = CPS_ERR_DEF

    ! appel au module cps_utilisateur
    trouve = cps_getCsteGenThCourante(trim(nom_cste), val, unite)
    
  end function cps_getCsteGenThCourante77

  
  function cps_getCsteTh77(code_corps, nom_theorie, cste, val, unite) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getCsteTh77
!
!$Resume
!  constante pour un  corps selon une theorie
!
!$Description
! V2.0
! Obtenir une constante pour un  corps selon une theorie.
! Les donnees sont recherchees dans les fichiers de la categorie 
! 'cstes_corps_theorie'.
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getCsteTh77(code_corps, nom_theorie, cste, val, unite)
!.    integer :: code_corps
!.    character(LEN=*) :: nom_theorie, cste
!.    real(KIND=PM_REEL) :: val
!.    character(LEN=*) :: unite
!.    integer :: trouve
!
!$Arguments
!>E     code_corps   :<integer>   
!>E     nom_theorie  :<LEN=*>     
!>E     cste         :<LEN=*>     
!>S     val          :<PM_REEL>   
!>S     unite        :<LEN=*>     
!>S     trouve       :<integer>   
!
!$Common
!
!$Routines
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
    use cps_acces
    implicit none
    ! arguments
    integer, intent(in) :: code_corps
    character(LEN=*), intent(in) :: nom_theorie, cste
    real(KIND=PM_REEL), intent(out) :: val
    character(LEN=*), intent(out) :: unite
    
    ! resultat
    integer :: trouve
    
    ! initialisation
    trouve = CPS_ERR_DEF
    
    ! appel au module cps_utilisateur
    trouve = cps_getCsteTh(code_corps, trim(nom_theorie), trim(cste), val, unite)
    
  end function cps_getCsteTh77
  

  function cps_getCsteThCourante77(code_corps, cste, val, unite) result (trouve)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getCsteThCourante77
!
!$Resume
!  constante pour un  corps selon la theorie courante
!
!$Description
! V2.0
! Obtenir une constante pour un  corps selon la theorie courante.
! Interface F77/GENESIS/C
!
!$Auteur
! vpg
!
!$Acces
!  PUBLIC
!
!$Usage
!  trouve = cps_getCsteThCourante77(code_corps, cste, val, unite)
!.    integer :: code_corps
!.    character(LEN=*) :: cste
!.    real(KIND=PM_REEL) :: val
!.    character(LEN=*) :: unite
!.    integer :: trouve
!
!$Arguments
!>E     code_corps  :<integer>   
!>E     cste        :<LEN=*>     
!>S     val         :<PM_REEL>   
!>S     unite       :<LEN=*>     
!>S     trouve      :<integer>   
!
!$Common
!
!$Routines
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
    use cps_acces
    implicit none
    ! arguments
    integer, intent(in) :: code_corps
    character(LEN=*), intent(in) :: cste
    real(KIND=PM_REEL), intent(out) :: val
    character(LEN=*), intent(out) :: unite
    
    ! resultat
    integer :: trouve
    
    ! initialisation
    trouve = CPS_ERR_DEF
    
    ! appel au module cps_utilisateur
    trouve = cps_getCsteThCourante(code_corps, trim(cste), val, unite)
    
  end function cps_getCsteThCourante77



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Fournit la liste des modeles (atmosphere, potentiel) disponibles   !!
!! pour un corps                                                      !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine cps_getListModelesCorps77(corps, modele, nb_mod, liste_mod)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getListModelesCorps77
!
!$Resume
!  liste des modeles pour un corps
!
!$Description
! V2.0
! Fournit la liste des modeles (atmosphere, potentiel) disponibles
! pour un corps.
! Interface F77/GENESIS/C
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getListModelesCorps77(corps, modele, nb_mod, liste_mod)
!.    integer :: corps
!.    character(LEN=*) :: modele
!.    integer :: nb_mod
!.    character(LEN=256), dimension(100) :: liste_mod
!
!$Arguments
!>E     corps      :<integer>            Code du corps 
!>E     modele     :<LEN=*>              Modèle recherché
!>S     nb_mod     :<integer>            Nombre de modèles trouvés
!>S     liste_mod  :<LEN=256,DIM=(100)>  Liste des modèles 
!
!$Common
!
!$Routines
!
!$Include
!
!$Module
!#V
!- cps_utilisateur
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
    use cps_utilisateur
    implicit none
    ! arguments
    integer, intent(in) :: corps
    character(LEN=*), intent(in) :: modele
    integer, intent(out) :: nb_mod
    character(LEN=256), dimension(100), intent(out) :: liste_mod
    
    ! variables locales
    character(LEN=256), dimension(:), pointer :: liste_mod_tmp => NULL()
    integer :: trouve
    integer :: iostat
    integer, parameter :: NB_MODELES_MAX = 100
    
    ! Initialisation
    nb_mod = 0
    ! Appel a la routine
    trouve = cps_getListModelesCorps(corps, trim(modele), liste_mod_tmp)
    if (trouve == CPS_OK) then
       nb_mod = cpsi_size_ptr(liste_mod_tmp)
       ! Réduction du nombre de modèles à 100
       if (nb_mod.gt.NB_MODELES_MAX) then
          nb_mod = NB_MODELES_MAX
       end if
       ! Recopie partielle
       liste_mod(1:nb_mod) = liste_mod_tmp(1:nb_mod)
    endif
    
    ! liberation_memoire même en erreur
    if (associated(liste_mod_tmp)) then
       deallocate(liste_mod_tmp, stat=iostat)
    end if
    
  end subroutine cps_getListModelesCorps77
  

  subroutine cps_getEphemPath(ephem_path)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  cps_getEphemPath
!
!$Resume
!  repertoire éphémérides dans la base de référence 
!
!$Description
! V2.0
! Fournit le repertoire dans la base de reference qui contient les ressources
! necessaires au calcul des ephemerides.
! Interface F77/GENESIS/C
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call cps_getEphemPath(ephem_path)
!.    character(LEN=*) :: ephem_path
!
!$Arguments
!>S     ephem_path  :<LEN=*>   
!
!$Common
!
!$Routines
!- eph_infoinit
!
!$Include
!
!$Module
!#V
!- eph_info
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
    use eph_info
    implicit none
    ! arguments
    character(LEN=*), intent(out) :: ephem_path
  ! appel au module eph_info
    if (.not.ephv_acces_open) then
       call eph_infoinit()
    end if
    ephem_path = trim(ephv_ephempath)
  end subroutine cps_getEphemPath



  subroutine eph_infoget77(code, repertoire, fichierd, methode, theorie)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_infoget77
!
!$Resume
!  informations liees a un couple (methode/theorie)
!
!$Description
! V2.0
! Lecture des informations liees a un couple (methode/theorie) pour le
! calcul des ephemerides a partir de la categorie CPS_CATEG_EPHEM.
! Interface F77/GENESIS/C
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_infoget77(code, repertoire, fichierd, methode, theorie)
!.    integer :: code
!.    character(LEN=*) :: repertoire, fichierd, methode, theorie
!
!$Arguments
!>E     code        :<integer>   
!>S     repertoire  :<LEN=*>     
!>S     fichierd    :<LEN=*>     
!>S     methode     :<LEN=*>     
!>S     theorie     :<LEN=*>     
!
!$Common
!
!$Routines
!- eph_infogetLocal
!
!$Include
!
!$Module
!#V
!- eph_info
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
    use eph_info
    implicit none
    ! arguments
    integer, intent(in) :: code
    character(LEN=*), intent(out) :: repertoire, fichierd, methode, theorie
    logical :: base_locale
    
    ! initialisation
    base_locale = .false.
    
    ! recherche dans la base locale
    call eph_infogetLocal(code, base_locale, repertoire, fichierd, methode, theorie)
    if (.not.base_locale) then
       call eph_infoget(code, repertoire, fichierd, methode, theorie)
    end if
    
  end subroutine eph_infoget77


  subroutine eph_infocodes77(codes_ephem, nb_codes)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  eph_infocodes77
!
!$Resume
!  Liste des codes methode/theorie disponibles
!
!$Description
! V2.0
! Fournit tous les codes (methode/theorie) identifiant les calculs 
! des ephemerides.
! Interface F77/GENESIS/C
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call eph_infocodes77(codes_ephem, nb_codes)
!.    integer, dimension(50) :: codes_ephem
!.    integer :: nb_codes
!
!$Arguments
!>S     codes_ephem  :<integer,DIM=(50)>   
!>S     nb_codes     :<integer>            
!
!$Common
!
!$Routines
!- eph_infocodes
!
!$Include
!
!$Module
!#V
!- eph_info
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
    use eph_info
    implicit none
    integer, dimension(50), intent(out) :: codes_ephem
    integer, intent(out) :: nb_codes
    call eph_infocodes(codes_ephem, nb_codes)
  end subroutine eph_infocodes77
