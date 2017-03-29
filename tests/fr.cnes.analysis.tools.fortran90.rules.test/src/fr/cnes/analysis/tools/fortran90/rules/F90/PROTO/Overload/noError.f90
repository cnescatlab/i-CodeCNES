MODULE t_vecteur

   type vecteur
        real  :: v(5)
   end type vecteur

END MODULE t_vecteur

! *****************************************************************************
! L'unite de programme ci-dessous (appel sans parametre) effectue une operation 
! d'ajout sur deux vecteurs : dans un premier temps au moyen d'une fonction 
! specifique puis par surcharge de l'operateur '+' associe au type utilisateur 
! 'vecteur' qui a ete defini specifiquement 
! *****************************************************************************
PROGRAM ESSAI 

      USE t_vecteur
      implicit NONE

      interface operator (+)
         type(vecteur) function ajouter_vecteur(v1,v2) result (v3)
            USE t_vecteur
            implicit NONE
            type (vecteur), intent(in) :: v1,v2
         end function ajouter_vecteur
      end interface

      type (vecteur) a,b,c
   
! --- Initialisation du contenu des deux vecteurs 
      a = vecteur((/1., 2., 3., 4., 5./))
      b = vecteur((/5., 4., 3., 2., 1./))

      print *, ' --- Addition de deux vecteurs (c=a+b) sans surcharge d''operateur --'
      c = ajouter_vecteur(a,b)
      print *, 'a=', a
      print *, 'b=', b
      print *, 'c=', c

! --- On peut utiliser aussi la surcharge d'operateur et on obtient le meme resultat 
      c = a + b
      print *, ' --- Addition de deux vecteurs (c=a+b) avec surcharge d''operateur --'
      print *, 'a=', a
      print *, 'b=', b
      print *, 'c=', c

END PROGRAM ESSAI

! *******************************************************************************
! Cette fonction effectue l'ajout de deux vecteurs de type 'vecteur' et retourne 
!    le resultat obtenu sous forme d'un vecteur 
! *******************************************************************************
type(vecteur) function ajouter_vecteur(v1,v2) result (v3)

   USE t_vecteur
   implicit NONE

   type (vecteur), intent(in) :: v1,v2
   v3%v = v1%v + v2%v

end function ajouter_vecteur 



