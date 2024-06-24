      SUBROUTINE C3BODY(GD, RCD, CDUV, RXD, XDUV, ACC, ACCUV)

      IF (BARYC .NE. 10) THEN

C     Check that BARYC as index does not exceed PEQUR array
            IF (BARYC .NE. 12) THEN
                  RADIUS = PEQUR(BARYC)   
            ELSE
                  RADIUS = CSM_RADIUS
            END IF
      END IF

      END SUBROUTINE