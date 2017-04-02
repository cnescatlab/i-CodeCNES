module int_geophysique_mspro
implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: int_geophysique_mspro.f90 69 2012-09-11 08:33:34Z ffsm $'

public
interface
     subroutine mp_atm_cira ( mois, pos_geod, tempe, pres, dens, code_retour )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)                            :: mois     ! mois de l'annee
type(tm_geodesique), intent(in)                :: pos_geod ! coordonnees geodesiques
real(pm_reel), intent(out)                     :: tempe    ! temperature
real(pm_reel), intent(out)                     :: pres     ! pression
real(pm_reel), intent(out)                     :: dens     ! densite
type(tm_code_retour), intent(out)              ::  code_retour


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mp_atm_cira
     subroutine mp_atm_cira_msis86 (date,flux_veille,flux_3rot,tab_ap,lat,long,alt,heure_sol, &
                                    dens,code_retour,temp,pres)

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


type(tm_jour_sec), intent(in)           :: date         ! date julienne 1950
real(pm_reel), intent(in)               :: flux_veille  ! flux solaire du jour precedent
real(pm_reel), intent(in)               :: flux_3rot    ! flux solaire moyen sur les 3 
real(pm_reel), dimension(7), intent(in) :: tab_ap       ! evolution de l'activite 
real(pm_reel), intent(in)               :: lat          ! latitude geodesique
real(pm_reel), intent(in)               :: long         ! longitude geodesique
real(pm_reel), intent(in)               :: alt          ! altitude geodesique
real(pm_reel), intent(in)               :: heure_sol    ! heure solaire locale
real(pm_reel), intent(out)              :: dens         ! densite atmospherique
type(tm_code_retour), intent(out)       :: code_retour
real(pm_reel), intent(out), optional    :: temp         ! temperature
real(pm_reel), intent(out), optional    :: pres         ! pression



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mp_atm_cira_msis86
     subroutine mp_atm_dtm78 ( date,flux_veille,flux_3rot,ap_3h,lat,alt,heure_sol, &
                               dens,code_retour,inv_haut_ech )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


type(tm_jour_sec), intent(in)           :: date         ! date julienne 1950
real(pm_reel), intent(in)               :: flux_veille  ! flux solaire du jour precedent
real(pm_reel), intent(in)               :: flux_3rot    ! flux solaire moyen sur les 3 dernieres rotations solaires
real(pm_reel), intent(in)               :: ap_3h        ! indice geomagnetique ap des 3 heures precedentes                                         
real(pm_reel), intent(in)               :: lat          ! latitude geodesique
real(pm_reel), intent(in)               :: alt          ! altitude geodesique
real(pm_reel), intent(in)               :: heure_sol    ! heure solaire locale
real(pm_reel), intent(out)              :: dens         ! densite atmospherique
type(tm_code_retour), intent(out)       :: code_retour
real(pm_reel), intent(out), optional    :: inv_haut_ech ! inverse de la hauteur d'echelle


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mp_atm_dtm78
     subroutine mp_atm_msis86 ( date,flux_veille,flux_3rot,tab_ap,lat,long,alt,heure_sol, &
                                dens,code_retour,temp,pres,inv_haut_ech )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


type(tm_jour_sec), intent(in)           :: date         ! date julienne 1950
real(pm_reel), intent(in)               :: flux_veille  ! flux solaire du jour precedent
real(pm_reel), intent(in)               :: flux_3rot    ! flux solaire moyen sur les 3 
real(pm_reel), dimension(7), intent(in) :: tab_ap       ! evolution de l'activite 
real(pm_reel), intent(in)               :: lat          ! latitude geodesique
real(pm_reel), intent(in)               :: long         ! longitude geodesique
real(pm_reel), intent(in)               :: alt          ! altitude geodesique
real(pm_reel), intent(in)               :: heure_sol    ! heure solaire locale
real(pm_reel), intent(out)              :: dens         ! densite atmospherique
type(tm_code_retour), intent(out)       :: code_retour
real(pm_reel), intent(out), optional    :: temp         ! temperature
real(pm_reel), intent(out), optional    :: pres         ! pression
real(pm_reel), intent(out), optional    :: inv_haut_ech ! inverse de la hauteur d'echelle


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mp_atm_msis86
     subroutine mp_atm_us76d (delta_t,alt,dens,code_retour,delta_dens,vit_son,temp,pres,visco)

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


real(pm_reel),dimension(0:7),intent(in)   ::delta_t ! valeur des deltaTM a ajouter aux 8 valeur de TM

real(pm_reel),intent(in)                   :: alt          ! altitude
real(pm_reel),intent(out)                  :: dens         ! densite atmospherique
type(tm_code_retour), intent(out)          :: code_retour 
real(pm_reel), intent(in),  optional       :: delta_dens   ! delta pour la variation de densite atmospherique
real(pm_reel), intent(out), optional       :: vit_son      ! vitesse du son
real(pm_reel), intent(out), optional       :: temp         ! temperature
real(pm_reel), intent(out), optional       :: pres         ! pression
real(pm_reel), intent(out), optional       :: visco        ! viscosite dynamique
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mp_atm_us76d
     subroutine mp_mag_ap_kp (ap,kp,code_retour)

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




real(pm_reel), intent(in)  :: ap       !  entree indice d'activite geomagnetique ap
real(pm_reel),intent(out)  :: kp       !  sortie indice d'activite geomagnetique kp
type(tm_code_retour), intent(out)      :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mp_mag_ap_kp
     subroutine mp_mag_kp_ap (kp,ap,code_retour)

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




real(pm_reel), intent(in)  :: kp       !  entree indice d'activite geomagnetique kp
real(pm_reel),intent(out)  :: ap       !  sortie indice d'activite geomagnetique ap
type(tm_code_retour), intent(out)      :: code_retour

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mp_mag_kp_ap
end interface

end module int_geophysique_mspro
