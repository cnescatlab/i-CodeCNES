MODULE t_arbre 

   type noeud
      integer :: indice
      integer :: valeur
      character(32) :: commande
      type (noeud), pointer :: gauche, droite
   end type noeud

END MODULE t_arbre 

! ***********************************************************************
! Cette unite de programme effectue la mise en oeuvre de la notion de 
! pointeur selon deux aspects : 
! - Arbre binaire 
! - Partie de tableau 
! - Tableau alloue dans un sous-programme utilise dans le programme 
! ***********************************************************************
program ESSAI 

   implicit none

   integer                      :: i, nb
   real, dimension (:), pointer :: p_vecteur_a

   ! --- Exemple mise en oeuvre POINTER avec arbre binaire 
   call arbre_binaire 

   ! --- Exemple mise en oeuvre POINTER sur partie de tableau 
   call partie_tableau

   ! --- Exemple d'allocation et remplissage d'un vecteur dans un sous-programme, 
   ! --- impression du contenu dans le programme 
   call allouer_vecteur(p_vecteur_a) 
   nb = size(p_vecteur_a)
   write(*,'(" --- Nombre elements vecteur=",I3)') nb
   write(*,'(" --- Affichage du contenu du vecteur ---")')
   write(*, '(5(1X,F8.3))') (p_vecteur_a(i), i=1,nb)
      
contains

   ! *********************************************************************
   ! Cette routine implemente le tri d'un arbre binaire, dans 
   ! lequel est stocke une liste de mot cles (resultat de apropos linux) 
   ! dont le choix (pour insertion dans l'arbre) a ete effectue 
   ! aleatoirement . Affiche l'arbre trie sur indice de mot cle 
   ! *********************************************************************
   subroutine arbre_binaire 

      USE t_arbre
      implicit NONE

      type (noeud), pointer :: t                  ! Arbre binaire
      integer :: indice, valeur, taille
      real r

      character(32)                   :: commande
      character(32), dimension(64)    :: apropos_linux

      DATA apropos_linux / 'PAM', 'agetty', 'arp', 'bootparam', 'capabilities', 'cfdisk', 'chattr', &
                           'console_codes', 'ctstat', 'ddp', 'e2fsck', 'fdisk', 'filesystems', 'fs',&
                           'fsck', 'fsck.ext2', 'fsck.ext3', 'fsck.ext4', 'fsck.ext4dev',           &
                           'fsck.minix', 'getty', 'glibc', 'icmp', 'insmod', 'ioctl_list', 'ip',    &
                           'IPPROTO_ICMP', 'ipv6', 'ld-linux', 'ld-linux.so', 'ld.so', 'LDP',       &
                           'libc', 'linux32', 'linux64', 'lnstat', 'lsattr', 'lsmod', 'man-pages',  &
                           'mk_modmap', 'mkfs', 'mkfs.minix', 'mklost+found', 'mkswap', 'modinfo',  &
                           'modprobe', 'netdevice', 'NETLINK_ROUTE', 'pam', 'pam_selinux',          &
                           'pam_sepermit', 'PF_INET6', 'raw', 'rmmod', 'route', 'rtnetlink',        &
                           'rtstat', 'setupcon', 'sfdisk', 'SOCK_RAW', 'socket', 'syscalls',        &
                           'top', 'udev' /

      write(*,'(1x,"Nombre de noeud de l''arbre binaire:")')
      read (*, *) taille

      nullify (t)                                  ! On demarre avec un arbre vide
      call init_random_seed()
      do indice = 1, taille, 1
         call random_number ( harvest = r )
         valeur = mod(int ( 500.0 * r ),64)
         commande = apropos_linux (valeur)

         call insere (t, indice, valeur, commande) ! Inserer le nouveau noeud dans l'arbre 
      end do

      ! Affiche le contenu de l'arbre trie selon l'indice 
      write(*,'(" --- Affichage du contenu de l''arbre binaire ---")')
      call affiche (t)

   end subroutine arbre_binaire

   ! ***************************************************************************
   ! Insere dans l'arbre une branche contenant 3 feuilles le tout au bon endroit 
   ! ***************************************************************************
   recursive subroutine insere (t, indice, valeur, commande)

      USE t_arbre
      implicit NONE

      type (noeud), pointer       :: t              ! Arbre binaire
      integer, intent (in)        :: indice
      integer, intent (in)        :: valeur
      character(32), intent (in) :: commande

      ! Si l'arbre est vide, on range tout en haut 
      if (.not. associated (t)) then
         allocate (t)
         t % indice = indice
         t % valeur = valeur
         t % commande = commande
         nullify (t % gauche)
         nullify (t % droite)
      ! Sinon inserer au bon endroit 
      else if (valeur < t % valeur) then
         call insere (t % gauche, indice, valeur, commande)
      else
         call insere (t % droite, indice, valeur, commande)
      end if

   end subroutine insere

   ! ***********************************************************************
   ! Afficher le contenu de l'arbre selon critere de tri (indice de mot cle) 
   ! ***********************************************************************
   recursive subroutine affiche (t)

      USE t_arbre
      implicit NONE

      type (noeud), pointer :: t                     ! Arbre binaire

      if (associated (t)) then
         call affiche (t % gauche)
         print *, t % indice, t % valeur, t % commande
         call affiche (t % droite)
      end if

   end subroutine affiche

   ! ************************************************************************
   ! Implemente l'utilisation de POINTER sur des parties de tableau (matrice) 
   ! ************************************************************************
   subroutine partie_tableau 

      implicit NONE

      integer, parameter            :: m = 5
      integer, parameter            :: n = 5

      real, target, dimension(n,m)  :: a
      real, pointer                 :: b(:,:)
      real, pointer                 :: c(:,:)
      real, pointer                 :: d(:)

      a = reshape((/ 0.58, 3.22, 1.02, 5.54, 1.66, 2.72, 3.31, 0.91, 2.56, 9.29, 4.47, 3.36, 2.41, &
                     1.53, 4.18, 0.27, 6.80, 1.59, 3.35, 2.83, 1.06, 5.20, 2.66, 1.09, 3.65  /), shape(a))

      b => a                        ! --- b pointe sur l'ensemble de la matrice

      c => a(3:5,3:5)               ! --- c pointe sur une partie de la matrice

      d => a(5,:)                   ! --- d pointe sur la 5eme ligne de la matrice

      ! --- Affichage des differents contenus ---
      print *, ' --- Affichage des differents contenus ---'
      print *, 'a=', a
      print *, 'b=', b
      print *, 'c=', c
      print *, 'd=', d

      if ( associated (b) ) then
         write(*,*) ' b est associe '
      else
         write(*,*) ' b n''est pas associe '
      endif
      nullify  (b)
      if ( associated (b) ) then
         write(*,*) ' b est associe '
      else
         write(*,*) ' b n''est pas associe '
      endif

   end subroutine partie_tableau

   ! ****************************************************************************************
   ! La routine ci-dessous recoit en parametre un pointeur de vecteur ; cette routine alloue 
   ! un vecteur d'une certaine taille (qu'elle est seule a connaitre), puis rempli le 
   ! vecteur avec des valeurs .
   ! Le programme principal a acces au vecteur via le pointeur ; il connait la taille du 
   ! vecteur au moyen de la primitive 'size' .
   ! ****************************************************************************************
   subroutine allouer_vecteur(p_vecteur)

      implicit NONE

      integer, parameter           :: m=15
      integer                      :: i 
      real, dimension (:), pointer :: p_vecteur

      ! --- Allocation du vecteur dans la routine 
      allocate (p_vecteur(m))

      ! --- On remplit le vecteur avec sin(1.0/i) avec I=1->15
      do i=1,m,1
         p_vecteur(i) = sin(1.0/i)
      end do 

   end subroutine allouer_vecteur

end program ESSAI
