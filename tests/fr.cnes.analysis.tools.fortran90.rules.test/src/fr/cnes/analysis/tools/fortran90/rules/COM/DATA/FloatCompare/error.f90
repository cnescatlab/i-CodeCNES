       REAL A,B
       integer i
   	   double precision tval1(5),tval2(5)

       IF (A .EQ. B) THEN
            .TRUE.
       END IF
	   where (tval1 == tval2) .TRUE.
       forall(i=1:5,tval1(i).NE.tval2(i)).TRUE.