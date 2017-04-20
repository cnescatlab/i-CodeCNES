module int_mwi_internes
implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: int_mwi_internes.f90 69 2012-09-11 08:33:34Z ffsm $'

public
interface
     subroutine mwi_alc_unite_logique ( unific, retour )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(out)  :: unific  ! Unite logique associee au fichier saut de TUC
integer, intent(out)  :: retour  ! Code de retour du sous programme interne

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mwi_alc_unite_logique
     subroutine mwi_chercher_fichier ( nomfic, retour )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

character(len=*), intent(in) :: nomfic  ! Nom du fichier saut de TUC (chemin+nom)
integer, intent(out) :: retour          ! Code de retour du sous programme interne

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mwi_chercher_fichier
     subroutine mwi_fermer_fichier ( unific, retour )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer, intent(in)   :: unific  ! Unite logique associee au fichier saut de TUC
integer, intent(out)  :: retour  ! Code de retour du sous programme interne

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mwi_fermer_fichier
     subroutine mwi_ouvrir_fichier ( nomfic, unific, retour )

       use mslib



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

character(len=*), intent(in) :: nomfic  ! Nom du fichier saut de TUC (chemin+nom)
integer, intent(in) :: unific           ! unite logique allouee au fichier
integer, intent(out) :: retour          ! Code de retour du sous programme interne

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


     end subroutine mwi_ouvrir_fichier
end interface

end module int_mwi_internes
