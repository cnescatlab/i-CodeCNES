subroutine mxi_var_ench_transfo ( liste_variables, nombre_transfo, para_opt, coord_in, &
                                  coord_out, message, retour, jacob)

! (C) Copyright CNES - MSPRO - 2002-2004

!************************************************************************
!
! But:  Realiser les calculs de changements de variables d'apres la liste 
! ===   de transformations successives fournie  
!
! Note d'utilisation:  
! ==================   
!                                           
!$Historique
! ==========
!  Revision 357  2013/02/14 aadt
!  DM-ID 1513: Suppression des warnings de compilation
!
!   + Version 3.0 : creation
!                   (Date: 11/2002 - Realisation: Michel Lagreca, Bruno Revelin et Guylaine Prat)
!   + Version 3.1 (DE globale 4) : Modifications suite aux remarques qualite ATV
!                         (Date: 08/2003 - Realisation: Bruno Revelin)
!   + Version 3.1 (DE globale 7) : ajout des parametres perigee/apogee
!                         (Date: 08/2003 - Realisation: Bruno Revelin)
!   + Version 3.1 (DE 1) : choix de routines pour cartesien -> keplerien,
!             cartesien -> circulaire, cartesien -> circulaire-equatorial
!                         (Date: 10/2003 - Realisation: Bruno Revelin)
!   + Version 5.0 (DE globale 11): suppression du use aux modules interfaces de routines
!                                  passees dans la MSLIB90
!                         (Date: 03/2004 - Realisation: Veronique Lepine)
!   + Version 5.0 (FA  1): ajout de go to 6000 manquant lors de la detection de parametres 
!                          optionnels necessaires mais absents
!                         (Date: 03/2004 - Realisation: Veronique Lepine)
!   + Version 5.2 (DE globale 14) : ajout des parametres orbitaux hyperboliques 
!                         (Date: 11/2004 - Realisation: Bruno Revelin)
!   + Version 5.6 : DM-ID 72 : Coordonnees hyperboliques dans l'objet bulletin de la GSLIB
!                              (Date: 09/2006 - Realisation: Atos Origin)
!   + Version 5.6 : DM-ID 548 : diminution du nombre de fichiers sources
!                   (Date: 10/2006 - Realisation: Atos origin)
!   + Version 5.9 : DM-ID 859 : utilisation de mu_matmul6
!                   (Date: 03/2008 - Realisation: Atos origin)
!
!VERSION:V5.15:FA-ID:1398:30/09/2010:Ajout marqueur fin historique
!
!$FinHistorique
!
!************************************************************************

! Modules
! =======
use mslib

use type_themeX_interne_mspro           ! inclus use type_mspro

use parametre_themeX_interne_mspro
use parametre_interne_mspro

use surcharge_egal_mspro                ! interface surcharges operateur '='

use int_chgmnt_reperes_mspro, only : mt_meca_vol_car
use int_chgmnt_reperes_mspro, only : mt_car_meca_vol
use int_chgmnt_reperes_mspro, only : mt_car_gps_ard
use int_chgmnt_reperes_mspro, only : mt_gps_ard_car

use int_var_internes_mspro, only : mvi_cir_equa_equa
use int_var_internes_mspro, only : mvi_cir_equa_cir
use int_var_internes_mspro, only : mvi_kep_hpha
use int_var_internes_mspro, only : mvi_hpha_kep

use int_chgmnt_variables_mspro, only : mv_Vinf_kep
use int_chgmnt_variables_mspro, only : mv_kep_Vinf
use int_chgmnt_variables_mspro, only : mv_Vinfarr_kep
use int_chgmnt_variables_mspro, only : mv_kep_Vinfarr

use valeur_code_retour_mspro

! Declarations
! ============
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, dimension(pm_i_nombre_variable),intent(in)   :: liste_variables   ! liste des transformations de variables 
integer,                                  intent(in)  :: nombre_transfo    ! nombre de transformations
type(tm_i_var_para_opt),                  intent(in)  :: para_opt          ! structure des parametres en entree
real(pm_reel), dimension(6),              intent(in)  :: coord_in          ! coordonnees en entree
real(pm_reel), dimension(6),              intent(out) :: coord_out         ! coordonnees en sortie
character(len=pm_message),                intent(out) :: message           ! Message associe au code de retour
integer,                                  intent(out) :: retour                                 

real(pm_reel), dimension(6,6), intent(out), optional :: jacob             ! jacobienne

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Autres declarations
! -------------------

! donnees locales utilisees lors du parcours de l'arbre
! .....................................................

integer :: indice_courant               ! indice courant de parcours de l'arbre
integer :: indice_precedent             ! indice precedent de parcours de l'arbre
integer :: nombre_variables             ! nombre de types de variables a parcourir
integer :: fils                         ! numero du fils courant
integer :: sens                         ! sens de parcours dans un couple pere/fils

integer, parameter :: up=+1             ! valeur possible pour le sens fils -> pere
integer, parameter :: down=-1           !                              pere -> fils

! donnees locales utilisees par les transformations
! .................................................

integer :: i                                    ! indice boucles
real(pm_reel), dimension(6) :: coord_entree     ! coordonnees locales en entree
real(pm_reel), dimension(6) :: coord_sortie     ! coordonnees locales en sortie

real(pm_reel)               :: mu             ! mu local
type(tm_i_car_pos_vit)      :: car_pos_vit    ! coordonnees cartesiennes locales pos vit
type(tm_orb_Vinf)           :: Vinf           ! elements hyperboliques locaux Vinf depart
type(tm_orb_Vinf)           :: Vinfarr        ! elements hyperboliques locaux Vinf arrivee
type(tm_orb_hpha)           :: hpha           ! elements perigee/apogee locaux
type(tm_orb_kep)            :: kep            ! elements kepleriens locaux
type(tm_orb_cir)            :: cir            ! elements circulaires locaux
type(tm_orb_equa)           :: equa           ! elements equatoriaux locaux
type(tm_orb_cir_equa)       :: cir_equa       ! elements circulaires equatoriaux locaux
type(tm_i_geoc_pos_vit)     :: geoc_pos_vit   ! elements geocentriques locaux
real(pm_reel)               :: r_equa         ! rayon equatorial local
real(pm_reel)               :: apla           ! aplatissement local
type(tm_i_meca_vol_pos_vit) :: meca_vol_pos_vit ! elements mecanique du vol locaux
type(tm_i_gps_ard_pos_vit)  :: gps_ard_pos_vit  ! elements gps ard locaux
type(tm_i_sgd_pos_vit)      :: sgd_pos_vit      ! elements sgd locaux

real(pm_reel),dimension(6,6):: jacob_local      ! jacobienne locale des transformations
real(pm_reel),dimension(6,6):: jacob_temp       ! jacobienne intermediaire

character(len=pm_message):: lst_para_utilises   ! liste des parametres optionnels utilises
integer                  :: nb_para_opt_rest    ! nb de parametres optionnels fournis mais non utilises

logical              :: mu_utilise        ! flags d'utilisation du param optionnel mu
logical              :: ellips_utilise    ! flags d'utilisation du param optionnel ellips

type(tm_code_retour) :: code_retour_local      ! code retour local
integer              :: retour_local           ! code retour local pour routine interne

real(pm_reel),parameter :: eps_e_faible = 1.e-2_pm_reel  ! valeur sup de e pour utilisation de car->cir_equa->kep/equa

character(len=pm_longueur_info_utilisateur), parameter :: info_utilisateur = & 
    '@(#) Fichier MSPRO mxi_var_ench_transfo.f90: derniere modification V5.15 >'

!************************************************************************

! initialisation du code retour
! .............................
retour = pm_OK
message = ''

! Initialisations du nb de parametres optionnels restants
! .......................................................
nb_para_opt_rest = 0

! chaine contenant les parametres optionnels utilises
! ...................................................
lst_para_utilises = ''

! flags d'utilisation des parametres optionnels
! .............................................
mu_utilise = pm_i_non
ellips_utilise = pm_i_non

! indice courant de parcours de l'arbre
! .....................................
indice_courant = 2

! nombre de types de variables en jeu en fonction
! du nombre de transformations a effectuer
! ...............................................
nombre_variables = nombre_transfo + 1

! controle du nombre de transfos qui doit etre >= 1
! .................................................

if (nombre_transfo < 1) then
   retour = pm_err_valid
   message = ''
   go to 6000
end if

! Recuperation avant la boucle sur les transformations des
! coordonnees en entree
! ........................................................

coord_entree(1:6) = coord_in(1:6)

! Initialisation par la matrice identite de la jacobienne 
! avant toute transformation si elle a ete demandee
! .......................................................

if (present(jacob)) then
   jacob(:,:) = 0._pm_reel
   do i = 1,6
      jacob(i,i) = 1._pm_reel
   end do
end if

! boucle de parcours de tous les types de la liste
! ............................................................

do while (indice_courant <= nombre_variables) ! parcours de la liste a partir du 2ieme element

   indice_precedent = indice_courant - 1  ! indice du repere precedent dans la liste

   ! determination du pere et du fils dans le couple
   ! et du sens de la transformation
   ! ...............................................

   if (liste_variables(indice_precedent) < liste_variables(indice_courant)) then
      sens = up
      fils = liste_variables(indice_precedent)
   else
      sens = down
      fils = liste_variables(indice_courant)
   end if

   ! selection de la transformation unitaire de changement de variables
   ! ..................................................................

   SELECT CASE (fils) ! chaque fils possede un unique pere.
                      ! La resolution de la transformation a appliquer est simple et
                      ! ne depend que du sens de transformation (up ou down)

   ! **************************************************************************************
   case (pm_i_Vinf) ! Pere: pm_i_kep

      ! lecture des parametres optionnels

      if (.NOT. para_opt%mu%presence) then
         retour  = pm_err_para_opt_abs
         message = 'Le parametre optionnel manquant est mu.'
         go to 6000
      else
         mu =  para_opt%mu%valeur
         mu_utilise = pm_i_oui
      end if

      ! appel a la routine de base en fonction du sens de la transformation

      if (sens == up) then

         Vinf = coord_entree(1:6) ! surcharge "=" pour type tm_orb_Vinf

         if (present(jacob)) then
            call mv_Vinf_kep(mu, Vinf, kep, code_retour_local, jacob = jacob_local)
         else
            call mv_Vinf_kep(mu, Vinf, kep, code_retour_local)
         end if

         coord_sortie(1:6) = kep ! surcharge "=" pour type tm_orb_kep

      else
         
         kep = coord_entree(1:6)    ! surcharge "=" pour type tm_orb_kep

         if (present(jacob)) then
            call mv_kep_Vinf(mu, kep, Vinf, code_retour_local, jacob = jacob_local)
         else
            call mv_kep_Vinf(mu, kep, Vinf, code_retour_local)
         end if

         coord_sortie(1:6) = Vinf ! surcharge "=" pour type tm_orb_Vinf

      end if

      if( code_retour_local%valeur /= pm_OK ) then ! traitement du code retour
         retour  = code_retour_local%valeur
         message = 'L''erreur est survenue dans le passage entre parametres kepleriens et hyperboliques.'
         if (code_retour_local%valeur < pm_OK) go to 6000
      end if

   ! **************************************************************************************
   case (pm_i_Vinfarr) ! Pere: pm_i_kep

      ! lecture des parametres optionnels

      if (.NOT. para_opt%mu%presence) then
         retour  = pm_err_para_opt_abs
         message = 'Le parametre optionnel manquant est mu.'
         go to 6000
      else
         mu =  para_opt%mu%valeur
         mu_utilise = pm_i_oui
      end if

      ! appel a la routine de base en fonction du sens de la transformation

      if (sens == up) then

         Vinfarr = coord_entree(1:6) ! surcharge "=" pour type tm_orb_Vinf

         if (present(jacob)) then
            call mv_Vinfarr_kep(mu, Vinfarr, kep, code_retour_local, jacob = jacob_local)
         else
            call mv_Vinfarr_kep(mu, Vinfarr, kep, code_retour_local)
         end if

         coord_sortie(1:6) = kep ! surcharge "=" pour type tm_orb_kep

      else
         
         kep = coord_entree(1:6)    ! surcharge "=" pour type tm_orb_kep

         if (present(jacob)) then
            call mv_kep_Vinfarr(mu, kep, Vinfarr, code_retour_local, jacob = jacob_local)
         else
            call mv_kep_Vinfarr(mu, kep, Vinfarr, code_retour_local)
         end if

         coord_sortie(1:6) = Vinfarr ! surcharge "=" pour type tm_orb_Vinf

      end if

      if( code_retour_local%valeur /= pm_OK ) then ! traitement du code retour
         retour  = code_retour_local%valeur
         message = 'L''erreur est survenue dans le passage entre parametres kepleriens et hyperboliques.'
         if (code_retour_local%valeur < pm_OK) go to 6000
      end if

   ! **************************************************************************************
   case (pm_i_hpha) ! Pere: pm_i_kep

      ! lecture des parametres optionnels

      if (.NOT. para_opt%ellips%presence) then
         retour  = pm_err_para_opt_abs
         message = 'Les parametres optionnels manquants sont r_equa et apla .'
         go to 6000
      else
         r_equa =  para_opt%ellips%valeur%r_equa
         ellips_utilise = pm_i_oui
      end if

      ! appel a la routine de base en fonction du sens de la transformation

      if (sens == up) then

         hpha = coord_entree(1:6) ! surcharge "=" pour type tm_orb_hpha

         if (present(jacob)) then
            call mvi_hpha_kep(r_equa, hpha, kep, retour_local, jacob = jacob_local)
         else
            call mvi_hpha_kep(r_equa, hpha, kep, retour_local)
         end if

         coord_sortie(1:6) = kep ! surcharge "=" pour type tm_orb_kep

      else
         
         kep = coord_entree(1:6)    ! surcharge "=" pour type tm_orb_kep

         if (present(jacob)) then
            call mvi_kep_hpha(r_equa, kep, hpha, retour_local, jacob = jacob_local)
         else
            call mvi_kep_hpha(r_equa, kep, hpha, retour_local)
         end if

         coord_sortie(1:6) = hpha ! surcharge "=" pour type tm_orb_hpha

      end if

      if( retour_local /= pm_OK ) then ! traitement du code retour
         retour  = retour_local
         message = 'L''erreur est survenue dans le passage entre parametres kepleriens et hpha.'
         if (retour < pm_OK) go to 6000
      end if

   ! **************************************************************************************
   case (pm_i_kep) ! Pere: pm_i_car

      ! lecture des parametres optionnels

      if (.NOT. para_opt%mu%presence) then
         retour  = pm_err_para_opt_abs
         message = 'Le parametre optionnel manquant est mu.'
         go to 6000
      else
         mu =  para_opt%mu%valeur
         mu_utilise = pm_i_oui
      end if

      ! appel a la routine de base en fonction du sens de la transformation

      if (sens == up) then

         kep = coord_entree(1:6)    ! surcharge "=" pour type tm_orb_kep

         if (present(jacob)) then
            call mv_kep_car(mu, kep, &
                 car_pos_vit%pos, car_pos_vit%vit, code_retour_local, jacob = jacob_local)
         else
            call mv_kep_car(mu, kep, &
                 car_pos_vit%pos, car_pos_vit%vit, code_retour_local)
         end if

         coord_sortie(1:6) = car_pos_vit ! surcharge "=" pour type tm_i_car_pos_vit

      else

        car_pos_vit = coord_entree(1:6) ! surcharge "=" pour type tm_i_car_pos_vit

        ! calcul avec mv_car_kep a priori
        if (present(jacob)) then
           call mv_car_kep(mu, car_pos_vit%pos, car_pos_vit%vit, &
                kep, code_retour_local, jacob = jacob_local)
        else
           call mv_car_kep(mu, car_pos_vit%pos, car_pos_vit%vit, &
                kep, code_retour_local)
        end if
        if ((code_retour_local%valeur == pm_err_e_faible).OR. &
             (code_retour_local%valeur == pm_err_i_equa ) .OR. &
             (kep%e < eps_e_faible)) then     
        ! cas (quasi)circulaire ou equatorial: on refait le calcul avec car -> cir_equa -> kep

           if (present(jacob)) then
              call mv_car_cir_equa(mu, car_pos_vit%pos, car_pos_vit%vit, &
                   cir_equa, code_retour_local, jacob = jacob_local)
              if( code_retour_local%valeur /= pm_OK ) then ! traitement du code retour
                 retour  = code_retour_local%valeur
                 message = code_retour_local%message
                 if (retour < pm_OK) go to 6000
              end if
              call mu_matmul6(jacob_local,jacob,jacob_temp,code_retour_local)
              ! Pas d'erreur possible donc le code retour n est pas teste
              jacob(:,:) = jacob_temp(:,:)             
              call mv_cir_equa_kep(cir_equa, kep, code_retour_local, jacob = jacob_local)
              if( code_retour_local%valeur /= pm_OK ) then ! traitement du code retour
                 retour  = code_retour_local%valeur
                 message = code_retour_local%message
                 if (retour < pm_OK) go to 6000
              end if
           else
              call mv_car_cir_equa(mu, car_pos_vit%pos, car_pos_vit%vit, &
                   cir_equa, code_retour_local)
              if( code_retour_local%valeur /= pm_OK ) then ! traitement du code retour
                 retour  = code_retour_local%valeur
                 message = code_retour_local%message
                 if (retour < pm_OK) go to 6000
              end if
              call mv_cir_equa_kep(cir_equa, kep, code_retour_local)
              if( code_retour_local%valeur /= pm_OK ) then ! traitement du code retour
                 retour  = code_retour_local%valeur
                 message = code_retour_local%message
                 if (retour < pm_OK) go to 6000
              end if
           end if
           
        else
           if ( code_retour_local%valeur /= pm_OK ) then  ! autre erreur ou warning
              retour  = code_retour_local%valeur
              message = code_retour_local%message
              if (retour < pm_OK) go to 6000
           end if
        end if
        
        coord_sortie(1:6) = kep ! surcharge "=" pour type tm_orb_kep

      end if

   ! **************************************************************************************
   case (pm_i_cir) ! Pere: pm_i_car

      ! lecture des parametres optionnels

      if (.NOT. para_opt%mu%presence) then
         retour  = pm_err_para_opt_abs
         message = 'Le parametre optionnel manquant est mu.'
         go to 6000
      else
         mu =  para_opt%mu%valeur
         mu_utilise = pm_i_oui
      end if

      ! appel a la routine de base en fonction du sens de la transformation

      if (sens == up) then

         cir = coord_entree(1:6) ! surcharge "=" pour type tm_orb_cir

         if (present(jacob)) then
            call mv_cir_car(mu, cir, &
                 car_pos_vit%pos, car_pos_vit%vit, code_retour_local, jacob = jacob_local)
         else
            call mv_cir_car(mu, cir, &
                 car_pos_vit%pos, car_pos_vit%vit, code_retour_local)
         end if

         coord_sortie(1:6) = car_pos_vit ! surcharge "=" pour type tm_i_car_pos_vit

      else

        car_pos_vit = coord_entree(1:6) ! surcharge "=" pour type tm_i_car_pos_vit

        ! calcul avec car_cir a priori
        if (present(jacob)) then
           call mv_car_cir(mu, car_pos_vit%pos, car_pos_vit%vit, &
                cir, code_retour_local, jacob = jacob_local)
        else
           call mv_car_cir(mu, car_pos_vit%pos, car_pos_vit%vit, &
                cir, code_retour_local)
        end if
        if( code_retour_local%valeur /= pm_OK ) then ! traitement du code retour

           if (code_retour_local%valeur == pm_err_i_equa) then     
           ! cas equatorial: on refait le calcul avec car -> cir_equa -> cir
           
              if (present(jacob)) then
              ! On en peut pas faire le calcul de la jacobienne. Donc sortie en erreur
                 retour  = code_retour_local%valeur
                 message = code_retour_local%message
                 go to 6000                 
              else
                 call mv_car_cir_equa(mu, car_pos_vit%pos, car_pos_vit%vit, &
                      cir_equa, code_retour_local)
                 if( code_retour_local%valeur /= pm_OK ) then ! traitement du code retour
                    retour  = code_retour_local%valeur
                    message = code_retour_local%message
                    if (retour < pm_OK) go to 6000
                 end if
                 call mvi_cir_equa_cir(cir_equa, cir, retour_local)
                 if( retour_local /= pm_OK ) then ! traitement du code retour
                    retour  = retour_local
                    if (retour < pm_OK) go to 6000
                 end if
              end if
             
           else  ! autre erreur ou warning
              retour  = code_retour_local%valeur
              message = code_retour_local%message
              if (retour < pm_OK) go to 6000
           end if

        end if

        coord_sortie(1:6) = cir ! surcharge "=" pour type tm_orb_cir

      end if

   ! **************************************************************************************
   case (pm_i_equa) ! Pere: pm_i_car

      ! lecture des parametres optionnels

      if (.NOT. para_opt%mu%presence) then
         retour  = pm_err_para_opt_abs
         message = 'Le parametre optionnel manquant est mu.'
         go to 6000
      else
         mu =  para_opt%mu%valeur
         mu_utilise = pm_i_oui
      end if

      ! appel a la routine de base en fonction du sens de la transformation

      if (sens == up) then

         equa = coord_entree(1:6) ! surcharge "=" pour type tm_orb_equa

         if (present(jacob)) then
            call mv_equa_car(mu, equa, &
                 car_pos_vit%pos, car_pos_vit%vit, code_retour_local, jacob = jacob_local)
         else
            call mv_equa_car(mu, equa, &
                 car_pos_vit%pos, car_pos_vit%vit, code_retour_local)
         end if

         coord_sortie(1:6) = car_pos_vit ! surcharge "=" pour type tm_i_car_pos_vit

      else

        car_pos_vit = coord_entree(1:6) ! surcharge "=" pour type tm_i_car_pos_vit

        ! calcul avec car_equa a priori
        if (present(jacob)) then
           call mv_car_equa(mu, car_pos_vit%pos, car_pos_vit%vit, &
                equa, code_retour_local, jacob = jacob_local)
        else
           call mv_car_equa(mu, car_pos_vit%pos, car_pos_vit%vit, &
                equa, code_retour_local)
        end if
        if ((code_retour_local%valeur == pm_err_e_faible).OR. &
             (equa%e < eps_e_faible)) then     
        ! cas (quasi)circulaire: on refait le calcul avec car -> cir_equa -> kep
           
           if (present(jacob)) then
              call mv_car_cir_equa(mu, car_pos_vit%pos, car_pos_vit%vit, &
                   cir_equa, code_retour_local, jacob = jacob_local)
              if( code_retour_local%valeur /= pm_OK ) then ! traitement du code retour
                 retour  = code_retour_local%valeur
                 message = code_retour_local%message
                 if (retour < pm_OK) go to 6000
              end if
              call mu_matmul6(jacob_local,jacob,jacob_temp,code_retour_local)
              ! Pas d'erreur possible donc le code retour n est pas teste
              jacob(:,:) = jacob_temp(:,:)             
              call mvi_cir_equa_equa(cir_equa, equa, retour_local, jacob = jacob_local)
              if( retour_local /= pm_OK ) then ! traitement du code retour
                 retour  = retour_local
                 if (retour < pm_OK) go to 6000
              end if
              
           else
              call mv_car_cir_equa(mu, car_pos_vit%pos, car_pos_vit%vit, &
                   cir_equa, code_retour_local)
              if( code_retour_local%valeur /= pm_OK ) then ! traitement du code retour
                 retour  = code_retour_local%valeur
                 message = code_retour_local%message
                 if (retour < pm_OK) go to 6000
              end if
              call mvi_cir_equa_equa(cir_equa, equa, retour_local)
              if( retour_local /= pm_OK ) then ! traitement du code retour
                 retour  = retour_local
                 if (retour < pm_OK) go to 6000
              end if
           end if
           
        else 
           if( code_retour_local%valeur /= pm_OK ) then  ! autre erreur ou warning
              retour  = code_retour_local%valeur
              message = code_retour_local%message
              if (retour < pm_OK) go to 6000
           end if

        end if

         coord_sortie(1:6) = equa ! surcharge "=" pour type tm_orb_equa
         
      end if

   ! **************************************************************************************
   case (pm_i_cir_equa) ! Pere: pm_i_car

      ! lecture des parametres optionnels

      if (.NOT. para_opt%mu%presence) then
         retour  = pm_err_para_opt_abs
         message = 'Le parametre optionnel manquant est mu.'
         go to 6000
      else
         mu =  para_opt%mu%valeur
         mu_utilise = pm_i_oui
      end if

      ! appel a la routine de base en fonction du sens de la transformation

      if (sens == up) then

         cir_equa = coord_entree(1:6) ! surcharge "=" pour type tm_orb_cir_equa 

         if (present(jacob)) then
            call mv_cir_equa_car(mu, cir_equa, &
                 car_pos_vit%pos, car_pos_vit%vit, code_retour_local, jacob = jacob_local)
         else
            call mv_cir_equa_car(mu, cir_equa, &
                 car_pos_vit%pos, car_pos_vit%vit, code_retour_local)
         end if

         coord_sortie(1:6) = car_pos_vit ! surcharge "=" pour type tm_i_car_pos_vit

      else

        car_pos_vit = coord_entree(1:6) ! surcharge "=" pour type tm_i_car_pos_vit

         if (present(jacob)) then
            call mv_car_cir_equa(mu, car_pos_vit%pos, car_pos_vit%vit, &
                 cir_equa, code_retour_local, jacob = jacob_local)
         else
            call mv_car_cir_equa(mu, car_pos_vit%pos, car_pos_vit%vit, &
                 cir_equa, code_retour_local)
         end if

         coord_sortie(1:6) = cir_equa ! surcharge "=" pour type tm_orb_cir_equa

      end if

      if( code_retour_local%valeur /= pm_OK ) then ! traitement du code retour
         retour  = code_retour_local%valeur
         message = code_retour_local%message
         if (retour < pm_OK) go to 6000
      end if

   ! **************************************************************************************
   case (pm_i_geoc) ! Pere: pm_i_car

      ! appel a la routine de base en fonction du sens de la transformation

      if (sens == up) then

         geoc_pos_vit = coord_entree(1:6) ! surcharge "=" pour type tm_i_geoc_pos_vit

         if (present(jacob)) then
            call mt_geoc_car(geoc_pos_vit%pos, car_pos_vit%pos, code_retour_local, &
                 vit_geoc = geoc_pos_vit%vit, vit_car = car_pos_vit%vit, jacob = jacob_local)
         else
            call mt_geoc_car(geoc_pos_vit%pos, car_pos_vit%pos, code_retour_local, &
                 vit_geoc = geoc_pos_vit%vit, vit_car = car_pos_vit%vit)
         end if

         coord_sortie(1:6) = car_pos_vit ! surcharge "=" pour type tm_i_car_pos_vit

      else

         car_pos_vit = coord_entree(1:6) ! surcharge "=" pour type tm_i_car_pos_vit

         if (present(jacob)) then
            call mt_car_geoc(car_pos_vit%pos, geoc_pos_vit%pos, code_retour_local, &
                 vit_car = car_pos_vit%vit, vit_geoc = geoc_pos_vit%vit, jacob = jacob_local)
         else
            call mt_car_geoc(car_pos_vit%pos, geoc_pos_vit%pos, code_retour_local, &
                 vit_car = car_pos_vit%vit, vit_geoc = geoc_pos_vit%vit)
         end if

         coord_sortie(1:6) = geoc_pos_vit ! surcharge "=" pour type tm_i_geoc_pos_vit

      end if

      if( code_retour_local%valeur /= pm_OK ) then ! traitement du code retour
         retour  = code_retour_local%valeur
         message = code_retour_local%message
         if (retour < pm_OK) go to 6000
      end if

   ! **************************************************************************************
   case (pm_i_geod_meca_vol) ! Pere: pm_i_car

      ! lecture des parametres optionnels

      if (.NOT. para_opt%ellips%presence) then
         retour  = pm_err_para_opt_abs
         message = 'Les parametres optionnels manquants sont r_equa et apla .'
         go to 6000
      else
         r_equa =  para_opt%ellips%valeur%r_equa
         apla   =  para_opt%ellips%valeur%apla
         ellips_utilise = pm_i_oui
      end if

      ! appel a la routine de base en fonction du sens de la transformation

      if (sens == up) then

         meca_vol_pos_vit = coord_entree(1:6) ! surcharge "=" pour type tm_i_meca_vol_pos_vit

         if (present(jacob)) then
            call mt_meca_vol_car (r_equa, apla, meca_vol_pos_vit%pos, meca_vol_pos_vit%vit, &
                 car_pos_vit%pos, car_pos_vit%vit, code_retour_local, jacob = jacob_local)
         else
            call mt_meca_vol_car (r_equa, apla, meca_vol_pos_vit%pos, meca_vol_pos_vit%vit, &
                 car_pos_vit%pos, car_pos_vit%vit, code_retour_local)
         end if

         coord_sortie(1:6) = car_pos_vit ! surcharge "=" pour type tm_i_car_pos_vit

      else

        car_pos_vit = coord_entree(1:6) ! surcharge "=" pour type tm_i_car_pos_vit

         if (present(jacob)) then
            call mt_car_meca_vol (r_equa, apla, car_pos_vit%pos, car_pos_vit%vit, &
                 meca_vol_pos_vit%pos, meca_vol_pos_vit%vit, code_retour_local, jacob = jacob_local)
         else
            call mt_car_meca_vol (r_equa, apla, car_pos_vit%pos, car_pos_vit%vit, &
                 meca_vol_pos_vit%pos, meca_vol_pos_vit%vit, code_retour_local)
         end if

         coord_sortie(1:6) = meca_vol_pos_vit      ! surcharge "=" pour type tm_i_meca_vol_pos_vit

      end if

      if( code_retour_local%valeur /= pm_OK ) then ! traitement du code retour
         retour  = code_retour_local%valeur
         message = code_retour_local%message
         if (retour < pm_OK) go to 6000
      end if

   ! **************************************************************************************
   case (pm_i_geod_gps_ard) ! Pere: pm_i_car

      ! lecture des parametres optionnels

      if (.NOT. para_opt%ellips%presence) then
         retour  = pm_err_para_opt_abs
         message = 'Les parametres optionnels manquants sont r_equa et apla .'
         go to 6000
      else
         r_equa =  para_opt%ellips%valeur%r_equa
         apla   =  para_opt%ellips%valeur%apla
         ellips_utilise = pm_i_oui
      end if

      ! appel a la routine de base en fonction du sens de la transformation

      if (sens == up) then

         gps_ard_pos_vit = coord_entree(1:6) ! surcharge "=" pour type tm_i_gps_ard_pos_vit

         if (present(jacob)) then
            call mt_gps_ard_car (r_equa, apla, gps_ard_pos_vit%pos, gps_ard_pos_vit%vit, &
                 car_pos_vit%pos, car_pos_vit%vit, code_retour_local, jacob = jacob_local)
         else
            call mt_gps_ard_car (r_equa, apla, gps_ard_pos_vit%pos, gps_ard_pos_vit%vit, &
                 car_pos_vit%pos, car_pos_vit%vit, code_retour_local)
         end if

         coord_sortie(1:6) = car_pos_vit ! surcharge "=" pour type tm_i_car_pos_vit
      else

        car_pos_vit = coord_entree(1:6) ! surcharge "=" pour type tm_i_car_pos_vit

         if (present(jacob)) then
            call mt_car_gps_ard (r_equa, apla, car_pos_vit%pos, car_pos_vit%vit, &
                 gps_ard_pos_vit%pos, gps_ard_pos_vit%vit, code_retour_local, jacob = jacob_local)
         else
            call mt_car_gps_ard (r_equa, apla, car_pos_vit%pos, car_pos_vit%vit, &
                 gps_ard_pos_vit%pos, gps_ard_pos_vit%vit, code_retour_local)
         end if

         coord_sortie(1:6) = gps_ard_pos_vit ! surcharge "=" pour type tm_i_gps_ard_pos_vit

      end if

      if( code_retour_local%valeur /= pm_OK ) then ! traitement du code retour
         retour  = code_retour_local%valeur
         message = code_retour_local%message
         if (retour < pm_OK) go to 6000
      end if

   ! **************************************************************************************
   case (pm_i_sgd_nord) ! Pere: pm_i_car

      ! appel a la routine de base en fonction du sens de la transformation

      if (sens == up) then

         sgd_pos_vit = coord_entree(1:6) ! surcharge "=" pour type tm_i_sgd_pos_vit

         if (present(jacob)) then
            call mt_topo_N_sgd_car (sgd_pos_vit%pos, &
                 car_pos_vit%pos, code_retour_local,      &
                 vit_sgd = sgd_pos_vit%vit, vit_car = car_pos_vit%vit, jacob = jacob_local)
         else
            call mt_topo_N_sgd_car (sgd_pos_vit%pos, &
                 car_pos_vit%pos, code_retour_local,      &
                 vit_sgd = sgd_pos_vit%vit, vit_car = car_pos_vit%vit)
         end if

         coord_sortie(1:6) = car_pos_vit ! surcharge "=" pour type tm_i_car_pos_vit

      else

         car_pos_vit = coord_entree(1:6) ! surcharge "=" pour type tm_i_car_pos_vit

         if (present(jacob)) then
            call mt_topo_N_car_sgd (car_pos_vit%pos, &
                 sgd_pos_vit%pos, code_retour_local,      &
                 vit_car = car_pos_vit%vit, vit_sgd = sgd_pos_vit%vit, jacob = jacob_local)
         else
            call mt_topo_N_car_sgd (car_pos_vit%pos, &
                 sgd_pos_vit%pos, code_retour_local,      &
                 vit_car = car_pos_vit%vit, vit_sgd = sgd_pos_vit%vit)
         end if

         coord_sortie(1:6) = sgd_pos_vit  ! surcharge "=" pour type tm_i_sgd_pos_vit

      end if

      if( code_retour_local%valeur /= pm_OK ) then ! traitement du code retour
         retour  = code_retour_local%valeur
         message = code_retour_local%message
         if (retour < pm_OK) go to 6000
      end if

 
   ! **************************************************************************************
   case (pm_i_sgd_est) ! Pere: pm_i_car

      ! appel a la routine de base en fonction du sens de la transformation

      if (sens == up) then

         sgd_pos_vit = coord_entree(1:6) ! surcharge "=" pour type tm_i_sgd_pos_vit

         if (present(jacob)) then
            call mt_topo_E_sgd_car (sgd_pos_vit%pos, &
                 car_pos_vit%pos, code_retour_local,      &
                 vit_sgd = sgd_pos_vit%vit, vit_car = car_pos_vit%vit, jacob = jacob_local)
         else
            call mt_topo_E_sgd_car (sgd_pos_vit%pos, &
                 car_pos_vit%pos, code_retour_local,      &
                 vit_sgd = sgd_pos_vit%vit, vit_car = car_pos_vit%vit)
         end if

         coord_sortie(1:6) = car_pos_vit ! surcharge "=" pour type tm_i_car_pos_vit
  
      else

         car_pos_vit = coord_entree(1:6) ! surcharge "=" pour type tm_i_car_pos_vit

         if (present(jacob)) then
            call mt_topo_E_car_sgd (car_pos_vit%pos, &
                 sgd_pos_vit%pos, code_retour_local,      &
                 vit_car = car_pos_vit%vit, vit_sgd = sgd_pos_vit%vit, jacob = jacob_local)
         else
            call mt_topo_E_car_sgd (car_pos_vit%pos, &
                 sgd_pos_vit%pos, code_retour_local,      &
                 vit_car = car_pos_vit%vit, vit_sgd = sgd_pos_vit%vit)
         end if

         coord_sortie(1:6) = sgd_pos_vit  ! surcharge "=" pour type tm_i_sgd_pos_vit

      end if

      if( code_retour_local%valeur /= pm_OK ) then ! traitement du code retour
         retour  = code_retour_local%valeur
         message = code_retour_local%message
         if (retour < pm_OK) go to 6000
      end if

  ! **************************************************************************************
  ! Pas de case (pm_i_car ) 
  ! car il s'agit du noeud racine donc ne possede pas de pere ...
  ! **************************************************************************************

   END SELECT ! fin du select case sur le fils

   indice_courant = indice_courant + 1

   ! transfert des sorties locales en entrees locales pour transformation suivante si besoin
   coord_entree(:) = coord_sortie(:)

   if (present(jacob)) then
      call mu_matmul6(jacob_local,jacob,jacob_temp,code_retour_local)
      ! Pas d'erreur possible donc le code retour n est pas teste
      jacob(:,:) = jacob_temp(:,:)
   end if

end do

! affectation des sorties
! .......................

coord_out(1:6) = coord_sortie(1:6)

! pour le jacobien: a ete calcule au fur et a mesure des transformations

! bilan des parametres optionnels eventuellement fournis en trop par l'utilisateur
! ................................................................................

if (para_opt%mu%presence) then
   if (mu_utilise) then
      lst_para_utilises = trim(lst_para_utilises) // ',mu'
   else
      nb_para_opt_rest = nb_para_opt_rest + 1
   end if
end if
if (para_opt%ellips%presence) then
   if (ellips_utilise) then
      lst_para_utilises = trim(lst_para_utilises) // ',ellips'
   else
      nb_para_opt_rest = nb_para_opt_rest + 1
   end if
end if

if (trim(lst_para_utilises) == '') lst_para_utilises = ',aucun'
if (nb_para_opt_rest > 0) then
   ! on signale la presence de parametres en trop dans un warning
   retour = pm_warn_para_opt_trop

   ! Rq: lors de la concatenation, la taille de la chaine a inserer dans message 
   ! est suffisamment petite pour ne pas necessiter de test specifique
   write(message,1000) trim(lst_para_utilises(2:)),nb_para_opt_rest
end if
    

6000 continue

1000 format('Les parametres optionnels utilises sont: ',A,' .Il en reste ',I2.2,' non utilise(s).')

end subroutine mxi_var_ench_transfo
