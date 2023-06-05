MODULE ESSAI

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

END MODULE ESSAI

MODULE ESSAI2

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

END MODULE ESSAI2
