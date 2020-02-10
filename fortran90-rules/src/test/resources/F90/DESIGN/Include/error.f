      INCLUDE 'F90_Org.FichiersInclude_conversion_temperature_ko.f90'
! !!! Regle 'Org.FichiersInclude' pas respectee, 
! !!! les fonctions C_To_F et F_To_C sont importees via cette clause INCLUDE 
! !!! Il serait preferable d'utiliser un module 

!
! Cette unite de programme (appel sans parametre) effectue : 
! - la saisie d'une valeur de temperature en degres Celsius, la converti en degres Fahrenheit puis l'affiche 
! - la saisie d'une valeur de temperature en degres Fahrenheit, la converti en degres Celsius puis l'affiche 
!
PROGRAM ESSAI

      IMPLICIT NONE

      REAL                :: Tc
      REAL                :: Tf
      REAL                :: C_To_F
      REAL                :: F_To_C

      WRITE(*, '(3X,"Temperature en Celsius:")')
      READ(*,*) Tc
      WRITE(*, '(3X,"->Conversion en Fahrenheit:",F8.3)') C_To_F(Tc)

      WRITE(*, '(" ")')
      WRITE(*, '(3X,"Temperature en Fahrenheit:")')
      READ(*,*) Tf
      WRITE(*, '(3X,"->Conversion en Celsius:",F8.3)') F_To_C(Tf)

END PROGRAM ESSAI
