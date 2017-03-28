subroutine mu_supprimer_evenement (integrateur, ident_g_commut, code_retour)

! (C) Copyright CNES - MSPRO - 2005

!************************************************************************
!
! But:  Ajouter une subroutine signalant l'occurrence d'un evenement.
! ===
!
! Note d'utilisation:  
! ==================
!
!
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 5.6 : DM-ID 473 : création
!                         (Date: 09/2006 - Réalisation Atos Origin)
!   + Version 5.10: DM-ID 1058 : Correction des warnings levés par g95
!                   (Date: 8/2008 - Realisation: Atos origin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
  use mslib

  use type_mspro
  use parametre_interne_mspro
  use valeur_code_retour_mspro
  use numero_routine_mspro
  
  implicit none

! Arguments
! ============
  type(tm_integrateur), intent(inout)                  ::  integrateur         ! intégrateur concerné
  integer,              intent(in)                     ::  ident_g_commut      ! identificateur de la subroutine à supprimer
  type(tm_code_retour), intent(out)                    ::  code_retour

! Variables locales
! =================
  integer                                              ::  i,j                 ! compteur
  logical                                              ::  booleen=.true.      ! flag
  integer                                              ::  alloc               ! retour des (des)allocations dynamiques
  type(tm_g_commut), dimension(:), allocatable         ::  tableau_tampon      ! tableau intermediaire

  character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = &
       '@(#) Fichier MSPRO mu_spher_car.f90: derniere modification V5.15 >'

! Initialisation 
! ==============

! verification 1: la lite des routines de l'integrateur n'est pas vide
  if (integrateur%nb_commut==0) then
     code_retour%valeur = pm_err_dans_sub_commut
     return
  endif

! verification 2: ident_g_commut appartient bien à la liste des identificateurs
  do i=1,integrateur%nb_commut
     booleen = booleen .or. (ident_g_commut == integrateur%g_commut(i)%ident)
  enddo

! si la routine n'est pas présente, sortie en erreur
  if (.not. booleen) then
     code_retour%valeur = pm_err_dans_sub_commut
     return
  endif

! allocation de la taille nb_commut-1 au tableau intermédiaire de copie 
  if (integrateur%nb_commut > 1) then! (cas nb_commut == 1 traité à part)

!si besoin est, on désalloue tableau_tampon
     if (allocated(tableau_tampon)) then
        deallocate(tableau_tampon,stat=alloc)
        if (alloc /= pm_OK) then ! la desallocation dynamique a echoue
           code_retour%valeur = pm_err_deallocate
           go to 6000
        end if
     endif

! puis on lui alloue la taille integrateur%nb_commut-1 ( avec 
! integrateur%nb_commut-1>0
     allocate(tableau_tampon(integrateur%nb_commut-1),STAT=alloc)
     if(alloc /= pm_OK) then
        code_retour%valeur = pm_err_allocate
        go to 6000
     endif

   ! parcours de integrateur%g_commut, pour copier toutes les routines - celle à supprimer
   ! on initialise un indice pour tmp_tab
     j = 1
   ! un autre indice sert a parcourir g_commut
     do i=1, integrateur%nb_commut
      ! on regarde à quelle routine on a à faire
        if (integrateur%g_commut(i)%ident /= ident_g_commut) then
         ! si ce n'est pas la routine à supprimer, on la copie dans tableau_tampon 
           tableau_tampon(j) = integrateur%g_commut(i)
         ! on incrémente j
           j = j + 1 
        endif
     enddo

   ! décrémentation de nb_commut
     integrateur%nb_commut = integrateur%nb_commut-1

   !désallocation de integrateur%g_commut
     deallocate(integrateur%g_commut, STAT = alloc)
     if(alloc /= pm_OK) then
        code_retour%valeur = pm_err_deallocate
        go to 6000
     endif

   ! pour re-allouer une taille au tableau integrateur%g_commut, il ne faut pas se fier
   ! a nb_commut (represente le nombre de routines de commutation de l'integrateur), 
   ! mais a la valeur size_commut (represente la taille du tableau contenant les routines, 
   ! size_commut >= nb_commut)

   ! NB la valeur de size_commut n'a pas de raison d'être inférieure à nb_commut, 
   ! on avait size_commut>=nb_commut avant de décrémenter nb_commut
   ! (cf mu_ajouter_evenement, qui fait eventuellement varier la valeur de size_commut)
     allocate(integrateur%g_commut(integrateur%size_commut), STAT = alloc)
     if(alloc /= pm_OK) then
        code_retour%valeur = pm_err_allocate
        go to 6000
     endif
   
   ! copie de tableau_tampon dans integrateur%g_commut
     do i=1,integrateur%nb_commut
        integrateur%g_commut(i)=tableau_tampon(i)
     enddo
   
  else ! cas nb_commut = 1

     integrateur%nb_commut = 0
     deallocate(integrateur%g_commut,stat=alloc)
     if(alloc /= pm_OK) then
        code_retour%valeur = pm_err_deallocate
        go to 6000
     endif

  endif

! avant de quitter on libère la mémoire allouée à tableau_tampon

  if (allocated(tableau_tampon)) then
     deallocate(tableau_tampon,STAT=alloc)
     if(alloc /= pm_OK) then
        code_retour%valeur = pm_err_deallocate
        go to 6000
     endif
  endif

6000 continue

code_retour%routine = pm_num_mu_supprimer_evenement
code_retour%biblio = pm_mspro
if (code_retour%valeur /= pm_OK) code_retour%message = ' '


end subroutine mu_supprimer_evenement
