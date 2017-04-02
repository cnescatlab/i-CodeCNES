module ps_fichiers

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Type
!  module
!
!$Nom
!  ps_fichiers
!
!$Resume
!  Module contenant une moulinette de changement de format de fichiers
!
!$Description
!
!$Auteur
!
!$Version
!  $Id: ps_fichiers.F90 69 2012-09-11 08:33:34Z ffsm $
!
!$Historique
!  $Log: ps_fichiers.F90,v $
!  Revision 1.3  2010/10/25 13:04:28  mercadig
!  VERSION::AQ::25/10/2010:Ajout du marqueur de fin historique
!
!  Revision 1.2  2006/12/05 15:10:05  fabrec
!  suppression de variables redondantes
!
!  Revision 1.1  2005/01/25 16:52:32  fabrec
!  DM-ID 175 : ajout de ps_fichiers.F90
!
!
!$FinHistorique
!
!$Usage
!  use ps_fichiers
!
!$Structure
!
!$Global
!
!$Common
!
!$Routines
!- ps_change_format
!
!$Fonctions
!
!$Include
!
!$Module
!#V
!- MECASPA
!- ps_generalites
!- ps_bulletin
!#
!
!$Interface
!#V
!#
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   use MECASPA
   use ps_generalites
   use ps_bulletin

   implicit none

! SVN Source File Id
  character(len=256), private :: SVN_VER =  '$Id: ps_fichiers.F90 69 2012-09-11 08:33:34Z ffsm $'



   contains

      subroutine ps_change_format (fic1, struct, fic2)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!$<AM-V2.0>
!
!$Nom
!  ps_change_format
!
!$Resume
!  Module contenant une moulinette de changement de format de fichiers de type
!  POUSSEES_GES_PSIMU_TEST vers le type FICPRO_test
!
!$Description
!
!$Auteur
!
!$Acces
!  PUBLIC
!
!$Usage
!  call ps_change_format (fic1, struct, fic2)
!.    character(LEN=80) :: fic1, fic2
!.    character(LEN=80) :: struct
!
!$Arguments
!>E     fic1    :<LEN=80>   nom du fichier à lire et à transformer
!>E     struct  :<LEN=80>   nom de la structure à prendre en compte
!>E     fic2    :<LEN=80>   nom du fichier à ecrire
!
!$Common
!
!$Routines
!- MSP_signaler_message
!- getenv
!- gssetenv
!
!$Include
!
!$Module
!
!$Remarques
!
!$<>
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      character(LEN=80), intent(IN)   :: fic1, fic2
      character(LEN=80), intent(IN)   :: struct

      character(LEN=256) :: rctmp
      integer :: ier,longueur,acces1, acces2, lnblnk
      real (KIND=pm_reel) :: isp
      integer :: dirref
      real (KIND=pm_reel), dimension(:), pointer :: dates
      real (KIND=pm_reel), dimension(:), pointer :: fptab
      real (KIND=pm_reel), dimension(:), pointer :: omtab
      real (KIND=pm_reel), dimension(:), pointer :: omptab
      integer :: dimtab, i
      integer :: itypdat, jjtimpori
      real (KIND=pm_reel)  :: sectimpori, xmerg
      character(LEN=1024) :: madona_data_path
      character(LEN=256)  :: dirunit
      character(LEN=12)   :: psnum_version

      ! Le répertoire data_psimu est lu dans le fichier de ressources modifiable
      ier = AMv_rc_get ('data_psimu','psimu','','data_psimu',rctmp,longueur)
      if ( ier < 0 ) then
         call MSP_signaler_message (cle_mes="PS_LECT_FIC_CONF",&
              partie_variable="data_psimu",routine="ps_change_format")
         return
      else
         dirdat = rctmp(1:longueur)
      endif

      ! Le nom du fichier unités est lu dans le fichier de ressources associé à la version et non modifiable
      ier = AMv_rc_get ('unites','psimu_'//trim(psnum_version()),'','.',rctmp,longueur)
      if ( ier < 0 ) then
         call MSP_signaler_message (cle_mes="PS_LECT_FIC_CONF",partie_variable="unites",routine="ps_change_format")
         return
      else
         dirunit = rctmp(1:longueur)
      endif

      ! Positionnement de MADONA_DATA_PATH pour le fichier unités */
      call getenv ("MADONA_DATA_PATH",madona_data_path)
      call gssetenv ('MADONA_DATA_PATH='//dirunit(1:lnblnk(dirunit))//':'//madona_data_path(1:lnblnk(madona_data_path)))
      ier = acc_unit_load(dirunit(1:lnblnk(dirunit))//'/unites')

      ! Ouverture du fichier d'entree
      acces1 = acc_open()
      ier = acc_connect (acces1, dirdat(1:LEN_TRIM(dirdat))//"/"//fic1(1:LEN_TRIM(fic1)), ACC_R)
      ier = acc_read(acces1, ACC_ALL)

      ! Ouverture du fichier de sortie de type colonne
      acces2 = acc_open()
      ier = acc_connect (acces2, dirdat(1:LEN_TRIM(dirdat))//"/"//fic2(1:LEN_TRIM(fic2)), ACC_W)
      ier = acc_set_ftype(acces2, ACC_FIL_COL)

      !Lecture du fichier d'entree
      ier = acc_select (acces1,struct(1:LEN_TRIM(struct)),ACC_STRUCT)

      ! Lecture de Isp:
      ier = acc_getd(acces1,"Isp",isp,"s")

      ! Lecture de dirref:
      ier = acc_geti(acces1,"dirref", dirref)

      ! Lecture de dates:
      dimtab = acc_get_dim(acces1,"dates")
      allocate(dates(dimtab))
      allocate(fptab(dimtab))
      allocate(omtab(dimtab))
      allocate(omptab(dimtab))

      ier = acc_select(acces1,"dates",ACC_TABL)

      do i = 1 , dimtab
         ier = acc_set_index(acces1,i)
         ier = acc_getd(acces1,ACC_INDEX,dates(i),"s")
      enddo
      ier = acc_select_end(acces1)

      ! Lecture de F:
      dimtab = acc_get_dim(acces1,"F")

      ier = acc_select(acces1,"F",ACC_TABL)

      do i = 1 , dimtab
         ier = acc_set_index(acces1,i)
         ier = acc_getd(acces1,ACC_INDEX,fptab(i),"N")
      enddo
      ier = acc_select_end(acces1)

      ! Lecture de w:
      dimtab = acc_get_dim(acces1,"w")

      ier = acc_select(acces1,"w",ACC_TABL)

      do i = 1 , dimtab
         ier = acc_set_index(acces1,i)
         ier = acc_getd(acces1,ACC_INDEX,omtab(i),"deg")
      enddo

      ier = acc_select_end(acces1)

      ! Lecture de wp:
      dimtab = acc_get_dim(acces1,"wp")

      ier = acc_select(acces1,"wp",ACC_TABL)

      do i = 1 , dimtab
         ier = acc_set_index(acces1,i)
         ier = acc_getd(acces1,ACC_INDEX,omptab(i),"deg")
      enddo
      ier = acc_select_end(acces1)
      ier = acc_select_end(acces1)

      ! Ecriture du fichier de sortie :
      ier = acc_begin_header(acces2)
      ! Ecriture de typdat
      itypdat = 2
      ier = acc_puti (acces2,"typdat",itypdat)
      ier = acc_putcom(acces2, "typdat",1, "Type de date : Date absolue=1, Date relative=2, Date absolue relative=3")
      ! Ecritude de JJ date dorigine
      jjtimpori = 0
      ier = acc_puti (acces2,"jjtimpori",jjtimpori)
      ier = acc_putcom(acces2, "jjtimpori",1,"Pour type de date=3, JJ de date origine")
      ! Ecriture de sec date origine
      sectimpori=0.0_pm_reel
      ier = acc_putd (acces2,"sectimpori",sectimpori,"s")
      ier = acc_putcom(acces2, "sectimpori",1,"Pour type de date=3, sec de date origine")
      ! Ecriture de la référence pour la direction de poussée
      ier = acc_puti (acces2,"dirref",dirref)
      ier = acc_putcom(acces2, "dirref",1,"Référence pour la direction de poussée")
      ! Ecriture de l'impulsion specifique
      ier = acc_putd (acces2,"xisp",isp,"s")
      ier = acc_putcom(acces2, "xisp",1,"Impulsion Specifique")
      ! Ecriture de la masse d ergol
      xmerg = 0.0
      ier = acc_putd (acces2,"xmerg",xmerg,"kg")
      ier = acc_putcom(acces2, "xmerg",1,"Masse d'ergols disponible pendant la poussée")

      !Description des colonnes
      ier = acc_begin_desc(acces2)
      ier = acc_create(acces2, "timp", ACC_PARAM, "s")
      ier = acc_putcom(acces2, "timp",1,"Dates")
      ier = acc_create(acces2, "fptab", ACC_PARAM, "N")
      ier = acc_putcom(acces2, "fptab",1,"Module de la poussée")
      ier = acc_create(acces2, "omtab", ACC_PARAM, "deg")
      ier = acc_putcom(acces2, "omtab",1,"Direction de la poussée dans le plan")
      ier = acc_create(acces2, "omptab", ACC_PARAM, "deg")
      ier = acc_putcom(acces2, "omptab",1,"Direction de la poussée hors du plan")
      ier = acc_write(acces2, ACC_HEADER)
      ier = acc_end_desc(acces2)

     ! Ecriture des colonnes jjtimp/sectimp/fptab/omtab/omptab pour typdat=1
     ! Ecriture des colonnes timp/fptab/omtab/omptab pour typdat=2 ou 3
      ier=acc_set_format(acces2, "timp", "4")
      ier=acc_set_format(acces2, "fptab", "15")
      ier=acc_set_format(acces2, "omtab", "15")
      ier=acc_set_format(acces2, "omptab", "15")

      do i=1, dimtab
         ier = acc_putd(acces2,"timp",dates(i), "s" )
         ier = acc_putd(acces2,"fptab",fptab(i) , "N")
         ier = acc_putd(acces2,"omtab",omtab(i) , "deg")
         ier = acc_putd(acces2,"omptab",omptab(i) , "deg")
         ier = acc_write(acces2, ACC_ALL)
      enddo

      deallocate(dates)
      deallocate(fptab)
      deallocate(omtab)
      deallocate(omptab)

      ier = acc_deconnect (acces1,  ACC_R)
      ier = acc_deconnect (acces2,  ACC_W)

      ! Fermeture des fichiers
      ier = acc_close(acces1)
      ier = acc_close(acces2)

      end subroutine ps_change_format

end module ps_fichiers
