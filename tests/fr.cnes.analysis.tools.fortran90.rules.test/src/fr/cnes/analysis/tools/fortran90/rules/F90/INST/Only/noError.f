module mes_fonctions_intrinseques

contains
!
! --- Fonction getuid developpee specifiquement : porte le meme nom que la fonction intrinseque getuid() et surtout rend le meme service
!
      integer function getuid()

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
                  getuid = -1
                  return
            endif


! --- On cree la FIFO
            status = system(id_command_2)
            if (status /= 0) then
                  print *, 'status mkfifo=', status
                  getuid = -2
                  return
            endif

! --- On appelle la commande systeme, son resultat est stocke dans la FIFO
            status = system(id_command_3)
            if (status /= 0) then
                  print *, 'status id -u =', status
                  getuid = -3
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
            getuid = i_uid

            RETURN

      end function getuid

end module mes_fonctions_intrinseques 

!
! L'unite de programme ci-dessous (appel sans parametre), effectue l'appel d'une fonction 
! nommee my_getuid qui a ete renommee afin d'eviter le conflit de nom avec la fonction intrinseque getuid .
! Cette fonction developpee specifiquement offre les memes services que la fonction 
! intrinseque est localisee dans un module : dans le module, elle s'appelle getuid.
! 

program ESSAI
! EN ce cas la, l instruction ONLY est correcte, parce que...explication
      use mes_fonctions_intrinseques, ONLY : my_getuid => getuid
      IMPLICIT NONE

      INTEGER                   :: i_stdout
      INTEGER                   :: i_uid

      i_stdout = 6
      WRITE(i_stdout, 9001)
      i_uid = my_getuid()
      WRITE(i_stdout, *) 'UID =', i_uid

!
! --------------------------------------------------------------------------
!      F O R M A T S
! --------------------------------------------------------------------------
!
9001  FORMAT(1X, '--- Recuperer le User Id ---')

      STOP

end program ESSAI
