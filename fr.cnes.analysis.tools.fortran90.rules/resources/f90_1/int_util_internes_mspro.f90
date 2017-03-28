module int_util_internes_mspro
implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: int_util_internes_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'

public
interface
     subroutine mui_cowell_calcab(iord,ideb,ifin,koeff,a,b,d,dp,retour)

       use mslib
       use type_mspro
       use parametre_interne_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   integer , intent(in) :: iord    ! ordre de cowell
   integer , intent(in) :: ideb    ! adresse extreme inferieure des alpha et beta
   integer , intent(in) :: ifin    ! adresse extreme superieure des alpha et beta
   integer, dimension(pm_i_dim17,pm_i_dim15), intent(in) :: koeff  ! tableau calcule par cigcag

   real(pm_reel), dimension(pm_i_dim16), intent(out) :: a   ! jeme colonne du tableau alpha
   real(pm_reel), dimension(pm_i_dim16), intent(out) :: b   ! jeme colonne du tableau beta
   real(pm_reel), intent(out) :: d   ! coefficient denormalisant
   real(pm_reel), intent(out) :: dp 
   integer , intent(out) :: retour    ! code retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_cowell_calcab
     subroutine mui_cowell_calcoe(integrateur,ideb,ifin,ic,lcf,kini0,kini,koeff,retour)

       use mslib
       use type_mspro
       use parametre_interne_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
   integer , intent(in) :: ideb  ! adresse extreme inferieure des alpha et beta
   integer , intent(in) :: ifin  ! adresse extreme superieure des alpha et beta
   integer , intent(in) :: ic    ! point central ( = 8 )
   integer , intent(in) :: lcf   ! dimension de kini ( = iord-1 )
   integer , intent(in) :: kini0 ! 
   integer, dimension(pm_i_dim15) , intent(inout) :: kini ! vecteur auxiliaire
   integer, dimension(pm_i_dim17,pm_i_dim15) , intent(out) :: koeff ! tableau resultat
   integer , intent(out) :: retour ! code retour


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_cowell_calcoe
     subroutine mui_cowell_chavar(integrateur,y,ys,yp,yps,s,indic,n1,n2,n4,retour)

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   type(tm_integrateur), intent(inout)  ::  integrateur              ! integrateur utilise
   real(pm_reel), dimension(integrateur%n) , intent(inout):: y       ! fonctions integrees
   real(pm_reel), dimension(integrateur%n) , intent(inout) :: ys     ! fonctions integrees
   real(pm_reel), dimension(integrateur%n) , intent(inout):: yp      ! tableau des derivees des fonctions / ss si indic # 0
   real(pm_reel), dimension(integrateur%n) , intent(inout) :: yps    ! tableau des derivees des fonctions / t  si indic = 0
   real(pm_reel), intent(in) :: s ! variable independante
   integer , intent(in) :: indic  ! sens de la transformation
   integer , intent(in) :: n1   ! nombre d'equations a integrer par l'utilisateur
   integer , intent(in) :: n2   ! nombre d'equations du 2eme ordre
   integer , intent(in) :: n4   ! nombre total d'equations integrees par le ss-programme
   integer , intent(out):: retour  ! code retour


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_cowell_chavar
     subroutine mui_cowell_ckeptp(integrateur,s,ireg, tk,retour)

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


   type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
   real(pm_reel), intent(in) :: s   ! variable independante ( anomalie vraie )
   integer, intent(in) :: ireg      ! cle de regularisation
   real(pm_reel), intent(out) :: tk ! partie keplerienne du temps
   integer, intent(out) :: retour


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_cowell_ckeptp
     subroutine mui_cowell_cnicow(integrateur,retour)

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
   integer , intent(out) :: retour    ! code retour


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_cowell_cnicow
     subroutine mui_cowell_cowcir(integrateur,icircl,per,x,xp,ier)

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
   integer, intent(in) :: icircl    ! cle de circularisation
   real(pm_reel) :: per ! periode reelle du mouvement pseudo-circulaire (s)
   real(pm_reel), dimension(integrateur%n), intent(in) :: x  ! fonctions integrees
   real(pm_reel), dimension(integrateur%n), intent(in) :: xp ! derivees des fonctions
   integer, intent(out) :: ier

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_cowell_cowcir
     subroutine mui_cowell_cowint(integrateur,cys,cyps,s,tt, &
      hpas,n,n1,n2,n4,ireg,ihyp, yssig,ypssig,sigma,retour)

       use mslib
       use type_mspro
       use parametre_interne_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
   real(pm_reel), dimension(integrateur%n, pm_i_dim17), intent(in) :: cys  ! tableau a interpoler centre sur s
   real(pm_reel), dimension(integrateur%n, pm_i_dim17), intent(in) :: cyps ! tableau a interpoler centre sur s
   real(pm_reel), intent(in) :: s   ! abscisse (variable indépendante)
   real(pm_reel), intent(in) :: tt  ! date d'interpolation
   real(pm_reel), intent(in) :: hpas   ! pas des tableaux (en abscisse)
   integer, intent(in) :: n  ! ordre de l'interpolation
   integer, intent(in) :: n1 ! nombre total d'equations differentielles
   integer, intent(in) :: n2 ! nombre d'equations du second ordre
   integer, intent(in) :: n4 ! nombre d'equations differentielles
   integer, intent(in) :: ireg ! cle de regularisation
   integer, intent(in) :: ihyp ! cle indiquant le cas hyperbolique

   real(pm_reel), dimension(integrateur%n), intent(out) :: yssig  ! resultat de l'interpolation
   real(pm_reel), dimension(integrateur%n), intent(out) :: ypssig  ! resultat de l'interpolation
   real(pm_reel), intent(out) :: sigma  ! solution de l'equation:tt=theta0+yyssig(n1+1)+sigma
   integer, intent(out) :: retour ! code retour


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_cowell_cowint
     subroutine mui_cowell_cowitg(fsub_cowell,integrateur,cxs,cxps,s,dels,iter,& 
          eps,eps_var_integ,n1,n2,n4,iboucl,retour_fsub,pb_fsub,retour,mode_iteratif)

       use mslib
       use type_mspro
       use parametre_interne_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   interface
      subroutine fsub_cowell(t,y,ydot,retour)     ! equation differentielle
        
        use mslib
        
        type(tm_jour_sec),intent(in)                    ::  t     ! abscisse
        real(pm_reel),dimension(:),intent(in)           ::  y     ! vecteur d'etat
        real(pm_reel),dimension(:),intent(out)          ::  ydot  ! derivee en t
        integer,                   intent(out)          ::  retour
   
   
      end subroutine fsub_cowell
   end interface

   type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
   real(pm_reel), dimension(integrateur%n, pm_i_dim17), intent(inout) :: cxs  ! valeurs a integrer
   real(pm_reel), dimension(integrateur%n, pm_i_dim17), intent(inout) :: cxps ! derivees par rapport a la variable independante s
   real(pm_reel), intent(inout) :: s
   real(pm_reel), dimension(integrateur%n, pm_i_dim17), intent(inout) :: dels ! table des differences finies de la fonction
   integer, intent(inout) :: iter   ! numero de l'iteration
   real(pm_reel), intent(in) :: eps ! critere d'arret de stabilisation des derivees n-iemes
   real(pm_reel), intent(in) :: eps_var_integ ! critere d'arret de stabilisation de la variable d'intégration
   integer, intent(in) :: n1        ! nombre d'equations differentielles utilisateur
   integer, intent(in) :: n2        ! nombre d'equations du second ordre
   integer, intent(in) :: n4        ! nombre total d'equations differentielles

   integer, intent(out) :: iboucl  ! 
   integer, intent(out) :: retour_fsub
   type(tm_jour_sec),  intent(out) ::  pb_fsub      ! abscisse posant pb a fsub
   integer, intent(out) :: retour
   logical, intent(in), optional  :: mode_iteratif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_cowell_cowitg
     subroutine mui_cowell_cowreg(integrateur,y,yp,per,icircl,ier)

       use mslib
       use type_mspro
       use parametre_interne_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   type(tm_integrateur), intent(inout)  ::  integrateur  ! integrateur utilise
   real(pm_reel), dimension(integrateur%n), intent(in) :: y  ! fonctions integrees
   real(pm_reel), dimension(integrateur%n), intent(in) :: yp ! derivees
   real(pm_reel), intent(in) :: per  ! periode reelle du mouvement pseudo-circulaire
   integer, intent(in) :: icircl     ! cle de circularisation
   integer, intent(out) :: ier

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_cowell_cowreg
     subroutine mui_cowell_cowsec(fsub_cowell,integrateur,ys1,yps1,s,n1,n2,n4,nup, &
          isimp,y2s,retour_fsub, pb_fsub, retour,korrec)

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

interface
   subroutine fsub_cowell(t,y,ydot,retour)     ! equation differentielle

   use mslib
   
   type(tm_jour_sec),intent(in)                    ::  t     ! abscisse
   real(pm_reel),dimension(:),intent(in)           ::  y     ! vecteur d'etat
   real(pm_reel),dimension(:),intent(out)          ::  ydot  ! derivee en t
   integer,                   intent(out)          ::  retour
   
   
   end subroutine fsub_cowell
end interface

   type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
   real(pm_reel), dimension(integrateur%n), intent(in) :: ys1  ! tableau des fonctions integrees
   real(pm_reel), dimension(integrateur%n), intent(in) :: yps1 ! tableau normalise des derivees 1eres par rapport a la variable ss
   real(pm_reel), intent(in) :: s  ! variable independante
   integer, intent(in) ::n1        ! nombre d'equations differentielles utilisateur
   integer, intent(in) ::n2        ! nombre d'equations du second ordre
   integer, intent(in) ::n4        ! nombre total d'equations differentielles
   integer, intent(in) ::nup       ! indice dans le tableau spys
   integer, intent(in) ::isimp     ! cle d'utilisation (= 0,aucun calcul,spys progresse, # 0 ,calcul de second membre)
   real(pm_reel), dimension(integrateur%n), intent(out) :: y2s  ! tableau normalise des derivees secondes par rapport a s
   integer, intent(out)  ::  retour_fsub  ! code retour de fsub
   type(tm_jour_sec),  intent(out) ::  pb_fsub      ! abscisse posant pb a fsub
   integer, intent(out) ::retour
   integer, intent(in), optional :: korrec ! indice d'utilisation du modèle simplifié
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_cowell_cowsec
     subroutine mui_cowell_decala(integrateur,cys,cyps,iordre,n4,cysi,cypsi,retour)

       use mslib
       use type_mspro
       use parametre_interne_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
   real(pm_reel), dimension(integrateur%n, pm_i_dim17), intent(in) :: cys   ! tableau de cowell de la fonction a integrer                   
   real(pm_reel), dimension(integrateur%n, pm_i_dim17), intent(in) :: cyps  ! tableau de cowell des derivees de cette fonction    
   integer , intent(in):: iordre    ! ordre de l integrateur de cowell
   integer , intent(in):: n4      ! nombre total d equations differentielles
   real(pm_reel), dimension(integrateur%n, pm_i_dim17) , intent(inout) :: cysi  ! tableau de cowell de la fonction a integrer decale
   real(pm_reel), dimension(integrateur%n, pm_i_dim17) , intent(inout):: cypsi ! tableau de cowell des derivees de cette fonction
   integer , intent(out):: retour    ! code retour


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_cowell_decala
     subroutine mui_cowell_everep(hpas,y,tm,t,n,f,retour)

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   real(pm_reel), intent(in) :: hpas
   real(pm_reel), dimension(*), intent(in) :: y
   real(pm_reel), intent(in) :: tm
   real(pm_reel), intent(in) :: t
   integer, intent(in) :: n
   real(pm_reel), intent(out) :: f
   integer, intent(out) :: retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_cowell_everep
     subroutine mui_cowell_inicow(fsub_cowell,integrateur,y,yp,icircl,n1,n2,n4, de, retour_fsub, pb_fsub, ier)

       use mslib
       use type_mspro
       use parametre_interne_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   interface
      subroutine fsub_cowell(t,y,ydot,retour)     ! equation differentielle
        
        use mslib
        
        type(tm_jour_sec),intent(in)                    ::  t     ! abscisse
        real(pm_reel),dimension(:),intent(in)           ::  y     ! vecteur d'etat
        real(pm_reel),dimension(:),intent(out)          ::  ydot  ! derivee en t
        integer,                   intent(out)          ::  retour
   
   
      end subroutine fsub_cowell
   end interface
         
   type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
   real(pm_reel), dimension(integrateur%n) , intent(in):: y  ! fonctions integrees
   real(pm_reel), dimension(integrateur%n) , intent(in):: yp ! derivees des fonctions integrees / variable indep.
   integer , intent(in) :: icircl ! cle de circularisation
   integer , intent(in) :: n1 ! nombre total d'equations differentielles
   integer , intent(in) :: n2 ! nombre d'equations du second ordre
   integer , intent(in) :: n4 ! nombre total d'equations differentielles

   real(pm_reel), dimension(integrateur%n,pm_i_dim17) , intent(out) :: de ! tableau des differences secondes de cowell
   integer , intent(out) ::  retour_fsub  ! code retour de fsub
   type(tm_jour_sec),  intent(out) ::  pb_fsub      ! abscisse posant pb a fsub
   integer , intent(out) :: ier ! code retour


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_cowell_inicow
     subroutine mui_cowell_normal(integrateur,yn,ypn,y,yp,u,indic,n1,n2,n4,retour)

       use mslib
       use type_mspro
       use parametre_interne_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
   real(pm_reel), dimension(integrateur%n), intent(inout) :: yn
   real(pm_reel), dimension(integrateur%n), intent(inout) :: ypn
   real(pm_reel), dimension(integrateur%n), intent(inout) :: y
   real(pm_reel), dimension(integrateur%n), intent(inout) :: yp
   real(pm_reel), intent(inout) :: u

   integer, intent(in) :: indic
   integer, intent(in) :: n1
   integer, intent(in) :: n2
   integer, intent(in) :: n4

   integer, intent(out) :: retour


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_cowell_normal
     subroutine mui_cowell_tempo(jdeb,secdeb,t,jfin,secfin,retour)

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   integer, intent(in) :: jdeb                 ! jours juliens date initiale
   real (kind=pm_reel), intent(in) :: secdeb   ! secondes date initiale
   real (kind=pm_reel), intent(in) :: t        ! intervalle de temps (sec) 
   integer, intent(out) :: jfin                ! jours juliens date finale  
   real (kind=pm_reel), intent(out) :: secfin  ! secondes date finale
   integer, intent(out) :: retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_cowell_tempo
     subroutine mui_dot_product3 ( vect1 , vect2 , prod_scal , retour )
       
       use type_mspro

!***********************************************************************
!***********************************************************************
  real (pm_reel), dimension(3), intent(in)   :: vect1       ! vecteur
  real (pm_reel), dimension(3), intent(in)   :: vect2       ! vecteur
  real (pm_reel)              , intent(out)  :: prod_scal   ! produit scalaire
  integer                     , intent(out)  :: retour
!***********************************************************************



     end subroutine mui_dot_product3
     subroutine mui_integ_RK ( fsub, integrateur, t0, y0, t, y, num_commut, nb_commut, &
          retour_fsub, pb_fsub, retour, evt_commut_tab)

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

interface
   subroutine fsub(t,y,ydot,retour)     ! equation differentielle

   use mslib
   
   real(pm_reel),intent(in)                        ::  t     ! abscisse
   real(pm_reel),dimension(:),intent(in)           ::  y     ! vecteur d'etat
   real(pm_reel),dimension(:),intent(out)          ::  ydot  ! derivee en t
   integer,                   intent(out)          ::  retour
   
   
   end subroutine fsub
end interface
   
type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
real(pm_reel),                         intent(in)                     ::  t0           ! valeur initiale de t
real(pm_reel),dimension(integrateur%n),intent(in)                     ::  y0           ! valeur de la fonction a t0
real(pm_reel),                         intent(in)                     ::  t            ! abscisse du point demande
real(pm_reel),dimension(integrateur%n),intent(out)                    ::  y            ! valeur de la fonction en t
integer,                               intent(out)                    ::  num_commut   ! numero de la derniere routine ayant commute
integer,                               intent(out)                    ::  nb_commut    ! nombre total de commutations
integer,                               intent(out)                    ::  retour_fsub  ! code retour de fsub
real(pm_reel),                         intent(out)                    ::  pb_fsub      ! abscisse posant pb a fsub
integer,                               intent(out)                    ::  retour
!!$ DM_ID 516
type(tm_evt_commut),dimension(:),      pointer,      optional         ::  evt_commut_tab   ! pointeurs sur les évènements de commutation 




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_integ_RK
     subroutine mui_integ_RKF ( fsub, integrateur, t0, y0, t, y, num_commut, nb_commut, &
          retour_fsub, pb_fsub, retour, evt_commut_tab)

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


interface
   subroutine fsub(t,y,ydot,retour)     ! equation differentielle

   use mslib
   
   real(pm_reel),intent(in)                        ::  t     ! abscisse
   real(pm_reel),dimension(:),intent(in)           ::  y     ! vecteur d'etat
   real(pm_reel),dimension(:),intent(out)          ::  ydot  ! derivee en t
   integer,                   intent(out)          ::  retour
   
   
   end subroutine fsub
end interface
   
type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
real(pm_reel),                         intent(in)                     ::  t0           ! valeur initiale de t
real(pm_reel),dimension(integrateur%n),intent(in)                     ::  y0           ! valeur de la fonction a t0
real(pm_reel),                         intent(in)                     ::  t            ! abscisse du point demande
real(pm_reel),dimension(integrateur%n),intent(out)                    ::  y            ! valeur de la fonction en t
integer,                               intent(out)                    ::  num_commut   ! numero de la derniere routine ayant commute
integer,                               intent(out)                    ::  nb_commut    ! nombre total de commutations
integer,                               intent(out)                    ::  retour_fsub  ! code retour de fsub
real(pm_reel),                         intent(out)                    ::  pb_fsub      ! abscisse posant pb a fsub
integer,                               intent(out)                    ::  retour
!!$ DM_ID 516
type(tm_evt_commut),dimension(:),      pointer,      optional         ::  evt_commut_tab   ! pointeurs sur les évènements de commutation 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_integ_RKF
     subroutine mui_integ_butcher ( integrateur, retour )

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_integrateur), intent(inout)    ::  integrateur  ! integrateur a completer
integer, intent(out)                   ::  retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_integ_butcher
     subroutine mui_integ_commut ( integrateur, prem_fois, prem_commut, der_commut, t_commut, action, retour )

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


type(tm_integrateur),      intent(inout)                  ::  integrateur   ! integrateur
logical,                   intent(in)                     ::  prem_fois     ! indique si on doit faire une initialisation
integer,                   intent(out)                    ::  prem_commut   ! numero de la 1ere commutation (0 si aucune)
integer,                   intent(out)                    ::  der_commut    ! numero de la derniere commutation (0 si aucune)
real(pm_reel),             intent(out)                    ::  t_commut      ! temps de la 1ere commutation
integer,                   intent(out)                    ::  action        ! action a effectuer
integer,                   intent(out)                    ::  retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_integ_commut
     subroutine mui_integ_cowell ( fsub_cowell, integrateur, t0, y0, t, y, retour_fsub, pb_fsub, retour )

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

interface
   subroutine fsub_cowell(t,y,ydot,retour)     ! equation differentielle

   use mslib
   
   type(tm_jour_sec),intent(in)                    ::  t     ! abscisse
   real(pm_reel),dimension(:),intent(in)           ::  y     ! vecteur d'etat
   real(pm_reel),dimension(:),intent(out)          ::  ydot  ! derivee en t
   integer,                   intent(out)          ::  retour
   
   
   end subroutine fsub_cowell
end interface
   
type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
type(tm_jour_sec),                     intent(in)                     ::  t0           ! valeur initiale de t
real(pm_reel),dimension(integrateur%n),intent(in)                     ::  y0           ! valeur de la fonction a t0
type(tm_jour_sec),                     intent(in)                     ::  t            ! abscisse du point demande
real(pm_reel),dimension(integrateur%n),intent(out)                    ::  y            ! valeur de la fonction en t
integer,                               intent(out)                    ::  retour_fsub  ! code retour de fsub
type(tm_jour_sec),  intent(out) ::  pb_fsub      ! abscisse posant pb a fsub
integer,                               intent(out)                    ::  retour




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_integ_cowell
     subroutine mui_integ_erreur ( integrateur,y_deb,y_fin,h,erreur,retour )

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

type(tm_integrateur),                   intent(in)   ::  integrateur  ! integrateur utilise
real(pm_reel),dimension(integrateur%n), intent(in)   ::  y_deb        ! etat en debut de pas
real(pm_reel),dimension(integrateur%n), intent(in)   ::  y_fin        ! etat en fin de pas
real(pm_reel),                          intent(in)   ::  h            ! pas
real(pm_reel),                          intent(out)  ::  erreur       ! erreur estimee
integer,                                intent(out)  ::  retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_integ_erreur
     subroutine mui_integ_init_pas ( fsub, integrateur, t0, y0, en_avant, h, retour_fsub, pb_fsub, retour )

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


interface
   subroutine fsub(t,y,ydot,retour)     ! equation differentielle

   use mslib
   
   real(pm_reel),intent(in)                        ::  t     ! abscisse
   real(pm_reel),dimension(:),intent(in)           ::  y     ! vecteur d'etat
   real(pm_reel),dimension(:),intent(out)          ::  ydot  ! derivee en t
   integer,                   intent(out)          ::  retour
   
   
   end subroutine fsub
end interface
   
type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
real(pm_reel),                         intent(in)                     ::  t0           ! valeur initiale de t
real(pm_reel),dimension(integrateur%n),intent(in)                     ::  y0           ! valeur de la fonction a t0
logical,                               intent(in)                     ::  en_avant     ! sens d'integration
real(pm_reel),                         intent(out)                    ::  h            ! pas initialise
integer,                               intent(out)                    ::  retour_fsub  ! code retour de fsub
real(pm_reel),                         intent(out)                    ::  pb_fsub      ! abscisse posant pb a fsub
integer,                               intent(out)                    ::  retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_integ_init_pas
     subroutine mui_integ_interp ( integrateur, t, y, retour )

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


type(tm_integrateur),                  intent(in)     ::  integrateur   ! integrateur utilise
real(pm_reel),                         intent(in)     ::  t             ! abscisse
real(pm_reel),dimension(integrateur%n),intent(out)    ::  y             ! vecteur d'etat interpole
integer,                               intent(out)    ::  retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_integ_interp
     subroutine mui_integ_racine ( integrateur, n_commut, A, gA, B, gB, err_abs, iter_max, &
          nb_racine, criter_arret, sol, retour )

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


type(tm_integrateur), intent(in)                     ::  integrateur  ! integrateur
integer,       intent(in)                            ::  n_commut     ! indice designant la fonction g de commutation a etudier
real(pm_reel), intent(in)                            ::  A   ! borne inferieure de l'intervalle de recherche de solution
real(pm_reel), intent(in)                            ::  gA  ! valeur de g en A
real(pm_reel), intent(in)                            ::  B   ! borne superieure de l'intervalle de recherche de solution
real(pm_reel), intent(in)                            ::  gB  ! valeur de g en B
real(pm_reel), intent(in)                            ::  err_abs    ! erreur absolue autorisee par l'utilisateur
integer, intent(in)                                  ::  iter_max   ! nombre maximum d'iterations
integer, intent(out)                                 ::  nb_racine  ! indication sur le nombre de racines
integer, intent(out)                                 ::  criter_arret  ! critere d'arret utilise
real(pm_reel), intent(out)                           ::  sol    ! valeur de la racine si la routine en a trouve une
integer,       intent(out)                           ::  retour


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_integ_racine
     subroutine mui_interp_DOP853 ( integrateur, t, y, retour )

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


type(tm_integrateur),                  intent(in)    ::  integrateur   ! integrateur utilise
real(pm_reel),                         intent(in)    ::  t             ! abscisse
real(pm_reel),dimension(integrateur%n),intent(out)   ::  y             ! vecteur d'etat interpole
integer,                               intent(out)   ::  retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_interp_DOP853
     subroutine mui_interp_Gill ( integrateur, t, y, retour )

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


type(tm_integrateur),                  intent(in)    ::  integrateur   ! integrateur utilise
real(pm_reel),                         intent(in)    ::  t             ! abscisse
real(pm_reel),dimension(integrateur%n),intent(out)   ::  y             ! vecteur d'etat interpole
integer,                               intent(out)   ::  retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_interp_Gill
     subroutine mui_interp_init ( fsub, integrateur, retour_fsub, pb_fsub, retour )

       use mslib
       use type_mspro



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


interface
   subroutine fsub(t,y,ydot,retour)     ! equation differentielle

   use mslib
   
   real(pm_reel),intent(in)                        ::  t     ! abscisse
   real(pm_reel),dimension(:),intent(in)           ::  y     ! vecteur d'etat
   real(pm_reel),dimension(:),intent(out)          ::  ydot  ! derivee en t
   integer,                   intent(out)          ::  retour
   
   
   end subroutine fsub
end interface
   
type(tm_integrateur),                  intent(inout)                  ::  integrateur  ! integrateur utilise
integer,                               intent(out)                    ::  retour_fsub  ! code retour de fsub
real(pm_reel),                         intent(out)                    ::  pb_fsub      ! abscisse posant pb a fsub
integer,                               intent(out)                    ::  retour



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mui_interp_init
     function mui_interp_newton ( a1, a2, b1, b2, b )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in) :: a1 ! element 1 de type a
real(pm_reel), intent(in) :: a2 ! element 2 de type a
real(pm_reel), intent(in) :: b1 ! element 1 de type b
real(pm_reel), intent(in) :: b2 ! element 2 de type b
real(pm_reel), intent(in) :: b  ! grandeur physique
real(pm_reel)             :: mui_interp_newton ! valeur calculee

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end function mui_interp_newton
     function mui_recale_angle ( alpha, angle_ref )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real(pm_reel), intent(in) :: alpha               ! angle initial 
real(pm_reel), intent(in) :: angle_ref           ! angle servant de centre au recalage
real(pm_reel)             :: mui_recale_angle    ! angle recale

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


       end function mui_recale_angle
end interface

end module int_util_internes_mspro
