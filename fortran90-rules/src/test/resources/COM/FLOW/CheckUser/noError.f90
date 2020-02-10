   PROGRAM MAIN
   C --- Utilisation de la fonction GETUID
   IF ( GETUID() .ne. 0 ) WRITE (*,*) 'Conformité de la regle 
		     					     COM.FLOW.CheckUser : OK'
		     					     
   INTEGER Y
   INTEGER RES
   
   Y = 0
   RES = TEST(Y)
   
   END PROGRAM MAIN


   FUNCTION TEST(X)

   INTEGER TEST
   INTEGER X
   TEST = 2*X

   END FUNCTION TEST