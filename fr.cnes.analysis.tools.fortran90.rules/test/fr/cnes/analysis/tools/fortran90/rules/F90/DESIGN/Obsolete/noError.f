!
! Cette fonction prend en parametre d'entree une chaine de caracteres puis 
! inverse tous les caracteres presents (le premier caractere de la chaine 
! devient le dernier de la chaine resultat, le deuxieme l'avant dernier, etc ...
! et retourne le resultat sous forme d'une chaine a l'unite de programme appelante .
!  
function RETOURNE(CHAINE)

      INTEGER                 :: i
      INTEGER                 :: longueur


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
SUBROUTINE carre_cube (a, b, c, s)

      IMPLICIT NONE

      INTEGER, intent(IN)      :: a
      INTEGER, intent(OUT)     :: b
      INTEGER, intent(OUT)     :: c
      INTEGER, intent(OUT)     :: s
!
      IF ( a < 0 ) THEN
         s = 1
         RETURN
      ENDIF
      IF ( a == 0 ) THEN
         s = 2
         RETURN
      ENDIF
!
      b = a**2
      c = a**3
      s = 0
      RETURN

END SUBROUTINE carre_cube


! 
! L'unite de programme ci-dessous (appel sans parametre) effectue diverses operations : 
! - declaration de donnees avec clause DATA
! - branchement conditionnel de type SELECT CASE selon valeur d'une variable  
! - appel d'une fonction interne au programme  

PROGRAM ESSAI

      IMPLICIT NONE

      INTEGER, parameter   :: i_stdin = 5
      INTEGER, parameter   :: i_stdout = 6
      INTEGER, parameter   :: i_stderr = 0
      INTEGER, parameter   :: n = 10

      INTEGER              :: i
      INTEGER              :: carre
      INTEGER              :: cube
      INTEGER              :: i_stat

      REAL                 :: a = 1.34
      REAL                 :: b = 2.05 
      REAL                 :: temp
      REAL, DIMENSION(N)   :: x
      REAL, DIMENSION(N)   :: y
      REAL, DIMENSION(N)   :: z

      CHARACTER(LEN=16)                    :: c_saisie
      CHARACTER(LEN=32)                    :: ma_chaine = 'Ceci est une chaine'
      CHARACTER(LEN=LEN(ma_chaine))        :: ma_eniahc
      CHARACTER(LEN=LEN(ma_chaine))        :: retourne
      CHARACTER(LEN=32)                    :: chaine_format

      DATA x / 2*3.25, 3*1.18, 2*0.75, 3*2.15 /
      DATA y / 2*0.25, 2*0.28, 2*0.36, 2*0.44, 2*0.66 /

! --- Saisie clavier d'une valeur et branchement sur l'etiquette associee
      write(i_stdout, '("Entrez un entier entre 1 et 3:")')
      read(i_stdin, *) i 

! --- Branchement conditionnel selon la valeur de i (saisie) 
      SELECT CASE (I)

         CASE(1)
            PRINT *, 'Case(1) : i=', i
         CASE(2)
            PRINT *, 'Case(2) : i=', i
         CASE(3)
            PRINT *, 'Case(3) : i=', i
         CASE DEFAULT
            Print*, "Case(DEFAULT) -> I ne correspond a aucune valeur attendue"

      END SELECT

! --- On poursuit les traitements avec l'inversion d'une chaine de caracteres 
      ma_eniahc = retourne(ma_chaine) 
      PRINT *, 'ma_chaine=', ma_chaine
      PRINT *, 'retournee=', ma_eniahc

! --- De meme, en effectuant une operation de calcul qui met en jeu le contenu complet de deux matrices plus des parametres additionnels 
      z(:) = (x(:)*a + b) - y(:)

! --- Affichage sur la sortie standard des matrices obtenues avec format variable
      write (chaine_format, '( "(", I4, "(f8.3))" )' ) n
      write(i_stdout, '(" --- tableau x ---")')
      write (*, chaine_format )  (x(i), i=1,n)
      write(i_stdout, '(" --- tableau y ---")')
      write (*, chaine_format )  (y(i), i=1,n)
      write(i_stdout, '(" --- tableau z ---")')
      write (*, chaine_format )  (z(i), i=1,n)

!!!!!!!!


! --- On poursuit les traitements
      write(i_stdout, '("Entrez un entier entre 1 et 32767:")')
      read(i_stdin, *) i
      call carre_cube(i,carre,cube,i_stat)

      select case (i_stat)
        case (0) 
           WRITE(*,'("I=",I5," CARRE=",I8," CUBE=",I8)') i, carre, cube
        case (1)
           WRITE(*,'("I=",I5," -> La valeur de I est negative !!!")') i
        case (2)
           WRITE(*,'("I=",I5," -> La valeur de I est nulle !!!")') i
        case DEFAULT
           WRITE(*,'("I=",I5," -> La valeur de I ne correspond a aucune valeur attendue !!!")') i
      end select

! --- En utilisant le concept de fonction - instruction
400   CONTINUE
      write(i_stdout, '("taper <go> pour continuer, tout autre caractere pour arret ")')
      read (i_stdin,'(A)') c_saisie
      if (c_saisie /= 'go') then
         stop
      endif 
! --- On continue le traitement 
      WRITE(i_stdout,*) 'Entrer une valeur de temperature en Fahrenheit'
      READ(i_stdin,*) temp
      write(i_stdout,510) temp, F_to_C(temp)
510   format('Fahrenheit = ',F8.3,' et en Celsius = ',F8.3)
      goto 400

660   format('xxx')

CONTAINS

! --- Conversion de degres CELSIUS en FAHRENHEIT
      REAL FUNCTION C_to_F(Tc)                ! Function interne a l'unite de programme 
         
            IMPLICIT NONE

            real, intent(in)  :: Tc           ! temperature en celsius
            real              :: Tf           ! temperature en fahrenheit

            Tf = (9.0/5.0)*Tc+32.0
            C_to_F = Tf

            RETURN

      END FUNCTION C_to_F

! --- Conversion de degres FAHRENHEIT en CELSIUS
      REAL FUNCTION F_to_C (Tf)

            IMPLICIT NONE

            real, intent(in)  :: Tf           ! temperature en fahrenheit
            real              :: Tc           ! temperature en celsius

            Tc = (5.0/9.0)*(Tf-32.0)
            F_to_C = Tc

            RETURN

      END FUNCTION F_to_C

END PROGRAM ESSAI
