FUNCTION TEST

DO I = 1,10
	DO WHILE (J < K + L)
		IF TEST THEN
			IF TEST2 THEN
				VAR = I + 1
			ELSE
				DO WHILE (COND)
					VAR = COND + 5
				END DO
			END IF
			K = 2
			L = 8
		END IF
	END DO
	J = J + 1
END DO

DO WHILE (M < N)
	M = N + 1
	M = 5
END DO

DO WHILE (O + P < Q)
	P = Q + 1
	O = 12
END DO

DO WHILE (COND)
	DO I = 1,5
		DO J = 1,2,VAR
			IF (CONDITION) THEN
				DO K = 1,2,VAR2
					VALUE = VAR + VAR2
				END DO
			ELSE IF (OTHER) THEN
				VALUE = 6*VAR
			END IF
		END DO
		VAR = VAR + 1	
	END DO
END DO
CALL test317()


END FUNCTION

subroutine test317()
INTEGER rdelta
	do while (abs(rdelta) > 1._pm_reel) ! iterations tant que abs(rdlta) > 1
    	imodif = imodif + 1
    	rdelta = rdelta * 0.25_pm_reel
	end do 
end subroutine FUNCTION TEST

DO I = 1,10
	DO WHILE (J < K + L)
		IF TEST THEN
			IF TEST2 THEN
				VAR = I + 1
			ELSE
				DO WHILE (COND)
					VAR = COND + 5
				END DO
			END IF
			K = 2
			L = 8
		END IF
	END DO
	J = J + 1
END DO

DO WHILE (M < N)
	M = N + 1
	M = 5
END DO

DO WHILE (O + P < Q)
	P = Q + 1
	O = 12
END DO

DO WHILE (COND)
	DO I = 1,5
		DO J = 1,2,VAR
			IF (CONDITION) THEN
				DO K = 1,2,VAR2
					VALUE = VAR + VAR2
				END DO
			ELSE IF (OTHER) THEN
				VALUE = 6*VAR
			END IF
		END DO
		VAR = VAR + 1	
	END DO
END DO
CALL test318()


END FUNCTION

subroutine test318()
INTEGER rdelta
	do while (abs(rdelta) > 1._pm_reel) ! iterations tant que abs(rdlta) > 1
    	imodif = imodif + 1
    	rdelta = rdelta * 0.25_pm_reel
	end do 
end subroutine test318







