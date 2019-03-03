subroutine ostc_lecdon (cficdon,ier)

!***********************************************************************
!$<DOC>
!$Nom
!        subroutine ostc_lecdon
!$Auteur
!        D. CLAUDE (IXI)
!$Resume
!		if x=0
!$Description
!        Lecture du fichier de donnees (.don) contenant toutes les donnees
!        necessaires et suffisantes a la fonction "Generation TCs SCAO"
!$Mots-cles
!        TCs SCAO , Initialisation commons
!$Acces
!       PRIVE
!*************************************************************************


!------------------------------
!        PARAMETRES
!------------------------------
      character*(*) cficdon
      integer ier
      
      
!-------------------------------
!        VARIABLES LOCALES
!-------------------------------
!     Variables utilisees pour les acces MADONA
      integer iacces, mode
      integer i, j, k, m
      integer i_tc, j_tc, k_tc
      integer imode_gen
      logical tc_inhibee
      type(st_type_groupe)  type_grp
      type(st_groupe)  grp
      type(st_tc_groupe)  tc
      integer ilg

!     Code retour des routines appelees
      integer ier_app
!     Variables pour la gestion des messages
      integer igravite
      character*(PLGVARMESS) cvarerreur
!     Numero du jeu de biais utilise et nombre de jeux de biais disponible
      integer num_jeu, nb_jeux

!     Nombre de tuyeres
      integer nb_propu
      
      
!-------------------------------
!        INITIALISATIONS
!-------------------------------

!     Parametres de sortie
      ier = 0

!     Variables locales
      iacces = 0
      mode = 0
      i = 0
      j = 0
      k = 0
      i_tc = 0
      j_tc = 0
      k_tc = 0
      ilg = 0
      igravite = 0
      cvarerreur = ' '
      tc_inhibee = .FALSE.
      num_jeu =  0
      nb_jeux = 0
      nb_propu = 0
      

!     Ligne a ne pas supprimer (necessaire pour l'affectation
!     de la strategie GS faite plus bas) : on met initialement
!     la strategie GS a "GS BLOQUE"
      WEoctc_param_tc_strategie_gs = PNSTRATEGIE_GS_BLOQUES

      call ostc_interfaceGene(imode_gen,ier_app)
      if (ier_app /= 0) then !   Cas d'erreur grave ou fatale : il faut propager
      
         call osme_propager_erreur('ostc_lecdon','ostc_interfaceGene', ier_app)
         if (ier_app < 0) then
!           Erreur fatale : on arrete le traitement
            ier = -1
            goto 999
         else
            continue
         endif
      endif
   end
