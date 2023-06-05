MODULE ESSAI
      
      !-------------------------------------------------------------------------------
      ! Component Name:  component name
      ! File: file name (it may be automatically inserted by the code management tool)
      ! Author: author name
      ! Copyright: EUMETSAT 2015
      ! Description: brief description of the purpose of the file content (e.g. class
      !              description)
      !-------------------------------------------------------------------------------
      

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
