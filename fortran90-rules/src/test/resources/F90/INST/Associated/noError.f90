! 
! Regle : 
!    Avant d'appeler la fonction ASSOCIATED, s'assurer que le pointeur passe en parametre a ete mis a NULL 
!    (au moyen de la fonction NULLIFY) ou est assigne a une cible.
! Justification :
!    Dans le cas d'un pointeur non null et non associe a une cible, la fonction ASSOCIATED 
!    renvoie une valeur indeterminee.
!

!
! --- Le module 'ma_precision' defini ici permet de standardiser les types de variables
! --- utilises dans l'unite de programme avec leur presision associee
!
MODULE ma_precision

     integer, parameter :: DOUBLE = selected_real_kind(15)
     integer, parameter :: SIMPLE = selected_real_kind(6)
     integer, parameter :: LONG   = selected_int_kind(18)
     integer, parameter :: ENTIER = selected_int_kind(9)
     integer, parameter :: COURT  = selected_int_kind(4)

     real(DOUBLE), parameter :: CONSTANTE_PI = 3.141592653589793239_DOUBLE

END MODULE ma_precision

! 
! Cette unite de programme (appel sans parametre), effectue des 
! operations d'association entre pointeurs et variables .
!
PROGRAM ESSAI
 
      USE ma_precision
      IMPLICIT NONE

      integer(ENTIER)       :: iom

      real(SIMPLE), target  :: A 
      real(SIMPLE), pointer :: ptr_1
      real(SIMPLE), pointer :: ptr_2

      CHARACTER(64)         :: message_syslog 

      DATA A / 3.1415926 /
	  nullify(ptr_1)
      print *, associated(ptr_1) ! valeur indéfinie (affiche .true. ou .false.)
!

      print *, associated (ptr_1) ! affiche .false.
!
      allocate(ptr_1, STAT=iom)
      if (iom /= 0) then
         write(message_syslog,'("Allocation du pointeur ",A5," a echouee")') 'ptr_1'
         call f_Syslog(message_syslog)
         stop
      else
         print *, associated (ptr_1) ! affiche .true.
      end if

! --- ci-dessous, association du pointeur avec sa cible 
      ptr_1 => A

! --- On verifie que l'association pointeur / cible est effective 
      if (associated(ptr_1, target=A)) then
         print *, 'ptr_1 est associe avec "a"'
         print *, 'ptr_1=', ptr_1
      endif

! --- memes operations avec ptr_2
	  nullify(ptr_2)
      allocate(ptr_2, STAT=iom)
      if (iom /= 0) then
         write(message_syslog,'("Allocation du pointeur ",A5," a echouee")') 'ptr_2'
         call f_Syslog(message_syslog)
         stop
      else
         print *, associated (ptr_2) ! affiche .true.
      end if

! --- ci-dessous, ptr_2 est associe avec ptr_1 => ptr_1 et ptr_2 pointent vers le meme objet 
      ptr_2 => ptr_1
      print *, 'ptr_2=', ptr_2

! !!! Ci-dessous, Il est necessaire de supprimer l'association du pointeur avec sa cible 
! !!! avant de desallouer la memoire associee au pointeur => sinon faute memoire  
       nullify(ptr_2)
      deallocate(ptr_2, STAT=iom)
      if (iom /= 0) then
         write(message_syslog,'("Desallocation du pointeur ",A5," a echouee")') 'ptr_2'
         call f_Syslog(message_syslog)
      else
         print *, associated (ptr_2) ! valeur indéfinie (affiche .true. ou .false.) 
      end if

! --- Desallouer la memoire associee au pointeur .
      nullify(ptr_1)
      deallocate(ptr_1, STAT=iom)
      if (iom /= 0) then
         write(message_syslog,'("Desallocation du pointeur ",A5," a echouee")') 'ptr_1'
         call f_Syslog(message_syslog)
      else
         print *, associated (ptr_1) ! valeur indéfinie (affiche .true. ou .false.) 
      end if

END PROGRAM ESSAI 
