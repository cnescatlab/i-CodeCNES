PROGRAM ESSAI
	  IF (SCABS1(CA).EQ.0.0E+0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
         DO I = 1,N
            CY(I) = CY(I) + CA*CX(I)
         END DO
      ELSE
         IX = 1
         IY = 1
         IF (INCX.LT.0) IX = (N+1)*INCX + 1
         IF (INCY.LT.0) IY = (N+1)*INCY + 1
         DO I = 1,N
            CY(IY) = CY(IY) + CA*CX(IX)
            IX = IX + INCX
            IY = IY + INCY
         END DO
      END IF
END PROGRAM ESSAI