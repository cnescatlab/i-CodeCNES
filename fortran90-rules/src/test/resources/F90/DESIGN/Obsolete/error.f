      function RETOURNE(CHAINE)

      IMPLICIT NONE

      INTEGER                 :: i
      INTEGER                 :: longueur

      character*(*) CHAINE, RETOURNE
! !!! Regle 'QA.ObsoleteProchain' pas respectee dans la sequence d'instructions ci-dessus .
! !!! Il ne faut pas utiliser 'CHARACTER*(*), il faut preferer une declaration avec specification de longueur

      LONGUEUR = len(CHAINE)

      do I=1,LONGUEUR
            RETOURNE(I:I) =                                      &
      CHAINE(LONGUEUR-I+1:LONGUEUR-I+1)
      enddo
      RETURN

      end function RETOURNE

	  
	  
!
! Elevation d'une valeur entiere au carre, et au cube .
! Si cette valeur est negative, sortie en erreur par l'etiquette  
!
      SUBROUTINE carre_cube (A, B, C, *)

      IMPLICIT NONE 

      INTEGER          :: a
      INTEGER          :: b
      INTEGER          :: c
!
      IF ( A < 0 ) THEN
         RETURN(1)
      ENDIF
      IF ( A == 0 ) THEN
         RETURN(2)
      ENDIF
!
      b = a**2
      c = a**3
!
      END SUBROUTINE carre_cube

PROGRAM ESSAI

      IMPLICIT NONE

      INTEGER, parameter   :: i_stdin = 5
      INTEGER, parameter   :: i_stdout = 6
      INTEGER, parameter   :: i_stderr = 0
      INTEGER, parameter   :: n = 10

      INTEGER              :: i
      INTEGER              :: j
      INTEGER              :: nret
      INTEGER              :: fmt
      INTEGER              :: carre
      INTEGER              :: cube 

      REAL                 :: a = 1.34
      REAL                 :: b = 2.05 
      REAL                 :: w

      REAL, DIMENSION(N,N) :: t
      REAL, DIMENSION(N)   :: x
      REAL, DIMENSION(N)   :: y
      REAL, DIMENSION(N)   :: z

      REAL F_TO_C, F, TEMP
      F_TO_C(F) = 5.0*(F - 32.0)/9.0

	  

      CHARACTER*32                         :: chaine_format
      CHARACTER*32                         :: ma_chaine = 'Ceci est une chaine'
! !!! Regle 'QA.ObsoleteProchain' pas respectee dans l'instruction ci-dessus 
! !!! Il faut eviter l'emploi de la notation CHARACTER*N et remplacer par CHARACTER(LEN=N) .

      CHARACTER(LEN=LEN(ma_chaine))        :: ma_eniahc
      CHARACTER(LEN=LEN(ma_chaine))        :: retourne

      DATA x / 2*3.25, 3*1.18, 2*0.75, 3*2.15 /

! --- Saisie clavier d'une valeur et branchement sur l'etiquette associee
10    CONTINUE
      write(i_stdout, '("Entrez un entier entre 1 et 3:")')
      read(i_stdin, *) i 
      goto (110, 120, 130) i
! !!! Regle 'QA.ObsoleteProchain' pas respectee dans l'instruction ci-dessus 
! !!! Il faut eviter l'emploi de goto calcule et remplacer par SELECT CASE
      write(i_stderr, '("Ce nombre ne correspond pas")')
      GO TO 10 

! --- si i vaut 1 
110   CONTINUE
      PRINT *, 'Etiquette 110 : i=', i
      GOTO 190

! --- si i vaut 2
120   CONTINUE
      PRINT *, 'Etiquette 120 : i=', i
      GOTO 190

! --- si i vaut 3
130   CONTINUE
      PRINT *, 'Etiquette 130 : i=', i

! --- Boucle sur la valeur de x
190   CONTINUE
      print *, '   --- La valeur de x est incremente a chaque cycle de la valeur 0.2 ---'
      do w = 0.0, 1.0, 0.2
         print *, 'valeur de w: ', w
      end do
! !!! Regle 'QA.Obsolete' pas respectee dans l'instruction ci-dessus 
! !!! Les termes d'une boucle doivent etre des entiers : utiliser des reels est interdit 

! --- Remplissage de la matrice 
      do 200 i=1,10
            do 200 j=1,10
200   t(i,j) = i*j
! !!! Regle 'QA.Obsolete' pas respectee dans l'instruction ci-dessus 
! !!! Chaque boucle doit posseder sa propre instruction de terminaison de boucle . Il est interdit de partager la meme instruction de fin //
! !!! Solution : terminer chaque boucle avec une instruction end do qui lui est propre .

! --- On poursuit 
      ma_eniahc = retourne(ma_chaine) 
      PRINT *, 'ma_chaine=', ma_chaine
      PRINT *, 'retournee=', ma_eniahc

      DATA y / 2*0.25, 2*0.28, 2*0.36, 2*0.44, 2*0.66 /

	  
	  
	  

      z(:) = (x(:)*a + b) - y(:)

! --- Affichage sur sortie standard des matrices obtenues avec format variable
      write (chaine_format, '( "(", I4, "(f8.3))" )' ) n
      write(i_stdout, '(" --- tableau x ---")')
      write (*, chaine_format )  (x(i), i=1,n)
      write(i_stdout, '(" --- tableau y ---")')
      write (*, chaine_format )  (y(i), i=1,n)
      write(i_stdout, '(" --- tableau z ---")')
      write (*, chaine_format )  (z(i), i=1,n)

! --- On poursuit les traitements 
      write(i_stdout, '("Entrez un entier entre 1 et 32767:")')
      read(i_stdin, *) i 
      call carre_cube(i,carre,cube,*380,*390)

	  
      WRITE(*,'("I=",I5," CARRE=",I8," CUBE=",I8)') i, carre, cube
      GOTO 400

! --- La valeur de i saisie est negative -> pas de calcul effectue
380   CONTINUE 
      WRITE(*,'("I=",I5," -> La valeur de I est negative !!!")') i
      GOTO 400

! --- La valeur de i saisie est nulle -> pas de calcul effectue
390   CONTINUE 
      WRITE(*,'("I=",I5," -> La valeur de I est nulle !!!")') i

! --- En utilisant le concept de fonction - instruction 
400   CONTINUE
      assign 490 to nret

	  
      goto 500
490   continue 
      pause
! !!! Regle 'QA.Obsolete' pas respectee dans l'instruction ci-dessus 
! !!! Il faut eviter d'utiliser l'instruction pause 
500   continue
      WRITE(i_stdout,*) 'Entrer une valeur de temperature en Fahrenheit'
      READ(i_stdin,*) TEMP
510   format('Fahrenheit = ',F8.3,' et en Celsius = ',F8.3)
      assign 510 to fmt
! !!! Regle 'QA.Obsolete' pas respectee dans l'instruction ci-dessus 
! !!! Il faut eviter d'utiliser l'instruction ASSIGN
      write(i_stdout,fmt) TEMP, F_TO_C(TEMP)

	  
      goto nret

660    format(3Hxxx)
! !!! Regle 'QA.ObsoleteProchain' pas respectee dans l'instruction ci-dessus 
! !!! L'emploi du descripteur HOLLERITH dans les formats est interdit 

END PROGRAM ESSAI
