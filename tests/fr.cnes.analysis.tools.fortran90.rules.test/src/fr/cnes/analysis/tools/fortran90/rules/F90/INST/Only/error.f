module mes_fonctions_intrinseques

contains
!
! --- Fonction my_getuid developpee specifiquement : rend le meme service que la fnction intrinseque getuid
!
      integer function my_getuid()

            IMPLICIT NONE

            INTEGER                      :: status
            INTEGER                      :: f_unit = 15
            INTEGER                      :: ios
            INTEGER                      :: i_uid

            CHARACTER(32), parameter     :: id_command_1 = 'rm -f /tmp/myfifo'
            CHARACTER(32), parameter     :: id_command_2 = 'mkfifo /tmp/myfifo'
            CHARACTER(32), parameter     :: id_command_3 = 'id -u >/tmp/myfifo&'
            CHARACTER(80)                :: line

! --- On commence par supprimer la FIFO, au cas ou existe deja
            status = system(id_command_1)
            if (status /= 0) then
                  print *, 'status rm fifo=', status
                  my_getuid = -1
                  return
            endif


! --- On cree la FIFO
            status = system(id_command_2)
            if (status /= 0) then
                  print *, 'status mkfifo=', status
                  my_getuid = -2
                  return
            endif

! --- On appelle la commande systeme, son resultat est stocke dans la FIFO
            status = system(id_command_3)
            if (status /= 0) then
                  print *, 'status id -u =', status
                  my_getuid = -3
                  return
            endif

! --- Maintenant on vient lire dans la FIFO
            open(unit=f_unit,file='/tmp/myfifo', status='old')
            read(unit=f_unit, fmt='(a80)', iostat=ios) line
            close(unit=f_unit)

! --- Affichage dans stdout de ce qu'on a lu
            print *, 'line=', line

! --- Conversion en entier
            read(line,*) i_uid
            my_getuid = i_uid

            RETURN

      end function my_getuid

end module mes_fonctions_intrinseques 

!
! L'unite de programme ci-dessous (appel sans parametre), effectue l'appel d'une fonction 
! nommee getuid qui a ete renommee ce qui genere un conflit de nom avec la fonction intrinseque getuid .
! Cette fonction developpee specifiquement offre les memes services que la fonction 
! intrinseque est localisee dans un module : dans le module, elle s'appelle my_getuid.
! 

program ESSAI

      use mes_fonctions_intrinseques, ONLY : getuid => my_getuid
! !!! La regle 'Id.Renommage' n'est pas respectee : l'option de renommage d'une fonction 
! !!! a l'importation depuis un module doit se limiter a lever les conflits ; or ici, 
! !!! elle introduit un conflit puisque la fonction my_getuid renommee en getuid va porter 
! !!! le meme nom que la fonction intrinseque 'getuid' .
! !!! Le compilateur gfortran ne signale pas de warning .
! !!! Comme la clause EXTERNAL getuid n'a pas ete precisee, on s'attend a ce que la fonction intrinseque 
! !!! getuid soit appelee mais c'est en fait la fonction specifique qui est appelee.
! !!! En fait, il faudrait preciser par une clause INTRINSIC getuid que l'on veut utiliser la 
! !!! fonction intrinseque .
! !!! Il se trouve que fonction intrinseque et fonction specifique rendent le meme service mais 
! !!! ce ne sera pas toujours le cas ...
      IMPLICIT NONE

      INTEGER                   :: i_stdout
      INTEGER                   :: i_uid

      i_stdout = 6
      WRITE(i_stdout, 9001)
      i_uid = getuid()
! !!! c'est la fonction developpee specifiquement qui va etre appelee 
      WRITE(i_stdout, *) 'UID =', i_uid

!
! --------------------------------------------------------------------------
!      F O R M A T S
! --------------------------------------------------------------------------
!
9001  FORMAT(1X, '--- Recuperer le User Id ---')

      STOP

end program ESSAI
