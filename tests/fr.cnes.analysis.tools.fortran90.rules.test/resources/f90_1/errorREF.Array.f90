program test

	  INTEGER(ENTIER), parameter :: N = 10
      REAL(DOUBLE), parameter :: X = 1.34_DOUBLE
      REAL(DOUBLE), parameter :: B = 2.05_DOUBLE

      REAL(DOUBLE), DIMENSION(N) :: Y
      REAL(DOUBLE), DIMENSION(N)  :: A
      REAL(DOUBLE), DIMENSION(N)  :: C

      DATA A / 2*3.25, 3*1.18, 2*0.75, 3*2.15 /
      DATA C / 2*0.25, 2*0.28, 2*0.36, 2*0.44, 2*0.66 / 

      Y = A*X + B - C

end program test