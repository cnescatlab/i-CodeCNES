!*-------------------------------------------------------------*
!*
!* nom du module : plancklib_module.f
!*
!* version : $Revision: 1.3 $ 
!* auteur : Noveltis 
!* 
!* date de revision : $Date: 2011-04-12 08:32:31 $ 
!*-------------------------------------------------------------* 
!* objet : Routines de manipulation de la fonction de Planck en
!*         m-1. et cm-1
!*-------------------------------------------------------------* 

module plancklib_module
  use precision_type
  use constantes_type
!
  implicit none
!
  public ::           &
           plkdirect  &
          ,plkinverse &
          ,plkderive  &
          ,plkdir     &
          ,plkder     &
          ,plkderdble &
          ,tbrillans
!
contains

!     calcul de la fonction de plank directe t en K ond en m-1
!     resultat w en watt/m**2/steradian/m-1

      subroutine plkdirect(t,ond,w)

      implicit none

! --- Variables d'entree ---
      real(kind=DOUBLE) ,intent(in)  :: t
      real(kind=DOUBLE) ,intent(in)  :: ond
      real(kind=DOUBLE) ,intent(out) :: w

! --- Variables locales ---
      real(kind=DOUBLE) :: sca,denom

      sca=Cst_sca2*ond/t

      if(sca < 100.) then
         denom=dexp(sca)-1.
         w=Cst_sca1*ond**3/denom
      else
         w=0.
      end if

      return

      end subroutine plkdirect


!     fonction de plank inverse w en watt/m**2/steradian/m-1 ond en m-1
!     resultat t en K

      subroutine plkinverse(t,ond,w)
      implicit none

! --- Variables d'entree ---
      real(kind=DOUBLE) ,intent(out) :: t
      real(kind=DOUBLE) ,intent(in)  :: ond
      real(kind=DOUBLE) ,intent(in)  :: w

! --- Variables locales ---
      real(kind=DOUBLE) :: sca,denom

      if(w.gt.0.) then
        denom=(1./w)*Cst_sca1*ond**3
        sca=log(denom+1.)
        t=Cst_sca2*ond/sca
      else
        t=0
      end if

      return

      end subroutine plkinverse


!     calcul de la fonction de plank directe t en K ond en m-1
!     et de sa derivee par rapport a  t
!     resultat w en watt/m**2/stradian/m-1
!              dwsdt en watt/m**2/steradian/m-1/K

      subroutine plkderive(t,ond,w,dwsdt)
      implicit none

! --- Variables d'entree ---
      real(kind=DOUBLE) ,intent(in)  :: t
      real(kind=DOUBLE) ,intent(in)  :: ond
      real(kind=DOUBLE) ,intent(out) :: w
      real(kind=DOUBLE) ,intent(out) :: dwsdt

! --- Variables locales ---
      real(kind=DOUBLE) :: sca,denom

      sca=Cst_sca2*ond/t

      if(sca < 100.) then
        denom=exp(sca)-1.
        w=Cst_sca1*ond**3/denom
        dwsdt=w/denom*(denom+1.)*sca/t
      else
        w=0.
        dwsdt=0.
      end if

      return

      end subroutine plkderive

! plkdir -- Public
!
! * Purpose
!
!   This sub-routine is dedicated to the computation of the direct
!   Planck function.
!
! * Inputs
!
!     * t:     Temperature in K
!     * ond:   Wave number in cm-1
!
! * Outputs
!
!     * w:     Planck function in watt/m**2/stradian/cm-1
!

subroutine plkdir(t,ond,w)

  implicit none

  real(kind=SIMPLE), intent(in)  :: t, ond
  real(kind=SIMPLE), intent(out) :: w

  real(kind=DOUBLE) :: t_d, ond_d, w_d
  real(kind=DOUBLE) :: sca, denom, ondm

  t_d  = dble(t)
  ond_d= dble(ond)

  ondm = ond_d*100.0d+00
  sca=Cst_sca2*ondm/t_d

  if (sca < 100) then
     denom = exp(sca)-1.
     w_d   = 100.0*Cst_sca1*ondm**3/denom
  else
     w_d   = 0.
  endif

  w  =  real(w_d)

  return

end subroutine plkdir

! plkder -- Public
!
! * Purpose
!
!   This sub-routine is dedicated to the computation of the direct
!   Planck function and of its derivative with respect to the
!   temperature.
!
! * Inputs
!
!     * t:     Temperature in K
!     * ond:   Wave number in cm-1
!
! * Outputs
!
!     * w:     Planck function in watt/m**2/stradian/cm-1
!     * dwsdt: Derivative of w in watt/m**2/steradian/cm-1/K
!

subroutine plkder(t,ond,w,dwsdt)

  implicit none

  real(kind=SIMPLE), intent(in)  :: t, ond
  real(kind=SIMPLE), intent(out) :: w, dwsdt

  real(kind=DOUBLE) :: t_d, ond_d, w_d, dwsdt_d

  real(kind=DOUBLE) :: sca, denom, ondm

  t_d     = dble(t)
  ond_d   = dble(ond)

  w_d     = 0.e0
  dwsdt_d = 0.e0

  ondm = ond_d*100.0d+00
  sca=Cst_sca2*ondm/t_d

  if (sca < 100.) then
     denom   = exp(sca)-1.
     w_d     = 100.0*Cst_sca1*ondm**3/denom
     dwsdt_d = w_d/denom*(denom+1.)*sca/t_d
  else
     w_d     = 0.
     dwsdt_d = 1e-40
  endif

  w     = real(w_d)
  dwsdt = real(dwsdt_d)

  return

end subroutine plkder


! plkderdble -- Public
!
! * Purpose
!
!   This sub-routine is dedicated to the computation of the direct
!   Planck function and of its derivative with respect to the
!   temperature. This sub-routine works on double precision
!   floating point values.
!
! * Inputs
!
!     * t:     Temperature in K
!     * ond:   Wave number in cm-1
!
! * Outputs
!
!     * w:     Planck function in watt/m**2/stradian/cm-1
!     * dwsdt: Derivative of w in watt/m**2/steradian/cm-1/K
!

subroutine plkderdble(t,ond,w,dwsdt)

  implicit none

  real(kind=DOUBLE), intent(in)  :: t, ond
  real(kind=DOUBLE), intent(out) :: w, dwsdt

  real(kind=DOUBLE) :: sca, denom, ondm

  w     = 0.d0
  dwsdt = 0.d0
  ondm = ond*100.0
  sca=Cst_sca2*ondm/t

  if (sca < 100.) then
     denom = exp(sca)-1.
     w     = 100.0*Cst_sca1*ondm**3/denom
     dwsdt = w/denom*(denom+1.)*sca/t
  else
     w     = 0.
     dwsdt = 1e-40
  endif
  return
end subroutine plkderdble



! tbrillans -- Public
!
! * Purpose
!
!   This function is dedicated to the computation of the inverse
!   Planck function.
!
! * Inputs
!
!     * nu:        Wave number in cm-1
!     * b:         Planck function in watt/m**2/steradian/cm-1
!
! * Outputs
!
!     * tbrillans: Brightness Temperature in K
!

function tbrillans(nu,b) result (return_tbrillans)

  implicit none

  real(kind=SIMPLE)             :: return_tbrillans
  real(kind=SIMPLE), intent(in) :: nu,b

  real(kind=DOUBLE) :: tbrillans_d
  real(kind=DOUBLE) :: nu_d,b_d

  real(kind=DOUBLE) :: nu0
  real(kind=DOUBLE) :: scale

  nu_d        = dble(nu)
  b_d         = dble(b)

  scale       = 100.0d+00
  nu0         = nu_d*scale
  tbrillans_d = Cst_sca2*nu0/log((scale*Cst_sca1*nu0**3/b_d)+1)
  return_tbrillans   = real(tbrillans_d)

  return
end function tbrillans

end module plancklib_module
