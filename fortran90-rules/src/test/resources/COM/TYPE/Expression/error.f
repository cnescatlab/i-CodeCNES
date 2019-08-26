 PROGRAM ESSAI

C..   Formal Arguments .. 
      double precision, parameter :: APX
      double precision :: AKPX
      integer :: IND
      logical :: ERREUR
C 
C..   Local Scalars .. 
      integer :: I = 2, IM = 1
      logical :: FIN_TRAITEMENT
      double precision :: epsilon
      save epsilon
C 
C..   Local Arrays .. 
      double precision, dimension(3) :: AKP , AP
	  STOP
	  
	  APX = APX + AKPX
	  APX = IND + AKPX
	  AKPX = epsilon * I
	  I = AP ** IM 
	  I = I ** ERREUR 
	   
  END PROGRAM
  
SUBROUTINE GS_calcul_ecart_echt(date_pm,ech_temps,ecart_tu1,ecart_tai,ier)

! Arguments

   type(tm_jour_sec),  intent(IN)   :: date_pm
   integer,            intent(IN)   :: ech_temps
   real(kind=pm_reel), intent(OUT)  :: ecart_tu1,ecart_tai
   integer ,           intent(OUT)  :: ier

! Variables locales

   type(tm_jour_sec)   :: date_tmp
   real(KIND=PM_REEL)  :: delta_te_tai
   integer             :: delta_tai_tuc

   type(tm_code_retour) :: code_retour

! Début code

   if (ech_temps == pm_TE)  then
   ! -- Ecarts avec le TE
      ecart_tu1 = - (delta_te_tai + delta_tai_tuc)    ! écart TU1 - TE
      ecart_tai = -delta_te_tai                       ! écart TAI - TE
   else 
   ! -- Ecarts avec le TUC
      ecart_tu1 = 0._PM_REEL                          ! écart TU1 - TUC
      ecart_tai = delta_tai_tuc                       ! écart TAI - TUC
   endif
         
 end subroutine GS_calcul_ecart_echt
 
subroutine MSP_calculer_actsol (str_act,date_js,fljop,flmoy,ap)

      real (KIND=pm_reel), intent(OUT) :: fljop,flmoy,ap(7)

      real (KIND=pm_reel) :: fljopold,flmoyold,apold(7),heure,date
      integer :: i,ibid,ij,ih,j6,j7,jj,flag_w
      integer :: ith(2,20),idatold,indjold,indh,indhold,indj,idat
      
      heure = ( date-real(idat,kind=pm_reel) )*24._pm_reel
   	  indh = 1 + int(heure/3._pm_reel)
   
end subroutine
