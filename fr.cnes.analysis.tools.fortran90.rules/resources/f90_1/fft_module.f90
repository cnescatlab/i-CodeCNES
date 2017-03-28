! fft_module.f90 --
!
!           Project: SPS_GENERIC
!           Authors: NOVELTIS/B.TOURNIER
!              Date: october 2009
!           Version: $Revision: 1.9 $
! Last modification: $Date: 2011-11-02 14:57:49 $
!
! Fourier Transform -- Module
!
! * Purpose
!   Module for Fourier Transform
!
module fft_module
   use precision_type
   use error_type
   use spectrum_type
   use interferogram_type
!
   implicit none
!
!
   private :: factorise     &
             ,factor_to_str &
             ,left_justify
!
   public :: tabule_NsFft &
            ,iftosp       &
            ,iftosp_open  &
            ,sptoif       &
            ,fft_r2r      &
            ,fft_c2c      &
            ,fft_r2c      &
            ,fft_c2r      &
            ,fft_r2r_open &
            ,fft_c2c_open &
            ,fft_r2c_open &
            ,fft_c2r_open
!
!
   contains
!
!
   subroutine tabule_NsFft( NSample, NsFft, ios )
     integer(kind=LONG), intent(in)                         :: NSample
     integer(kind=LONG), intent(out)                        :: NsFft
     integer(kind=LONG), intent(out)                        :: ios
     integer(kind=LONG)                                     :: n
     integer(kind=LONG), parameter                          :: pmax=11
     integer(kind=LONG)                                     :: np
     integer(kind=LONG)              , dimension(100)       :: p
     integer(kind=LONG)              , dimension(100)       :: e
     character(len=4)                                       :: etat
     character(len=60)                                      :: str_factors
!
!    Tabulation des NsFFT possibles
     n = NSample-1
     etat = 'NOK '
     do while( n .le. 2*NSample .and. etat .eq. 'NOK ' )
        n = n + 1
        call factorise ( n, np, p, e)
        call factor_to_str( np, p, e, str_factors )
        if (p(np) .le. pmax) then
           etat='OK  '
        else
           etat='NOK '
        end if
     end do
     if( etat .eq. 'OK  ' ) then
        NsFft = n
        if( mod(NsFft,2) == 1 ) then
          NsFft = NsFft/p(np)*(p(np)+1)
          ios = 0
          write(*,'(2i10,a,a3,a)') &
           n,NsFft,' modified ',etat,str_factors
        else
          ios = 0
          write(*,'(2i10,a,a3,a)') &
           n,NsFft,' ',etat,str_factors
        end if
     else
        ios = 1
        write(*,*) 'NsFft ECHEC',n,' ',etat,str_factors
     end if
!
     return
   end subroutine tabule_NsFft
!
!      
   subroutine factorise( n, np, p, e )
!
!    decompose l'entier n en facteurs premiers
!    n = p(1)**e(1) * p(2)**e(2) * .... * p(np)*e(np)
     integer(kind=LONG), intent(in)                  :: n
     integer(kind=LONG), intent(out)                 :: np
     integer(kind=LONG), intent(out), dimension(100) :: p
     integer(kind=LONG), intent(out), dimension(100) :: e
!    variables locales
     integer(kind=LONG)  :: d,m,q
!
     np = 0
     m  = n
!
     do d = 2, n
        if (mod(m,d).eq.0) then         ! diviseur de n trouve
          np = np+1
          p(np) = d
          e(np) = 0

10          m = m/d                       ! recherche de l'exposant
            e(np)=e(np)+1
          if (mod(m,d) .eq. 0) goto 10  
        endif
        q = n/d
        if (q .lt. d) goto 20             ! q<d ==> m premier
     end do

20   continue
!     d = 2
!     do while( (d.le.n) .and. (mod(m,d).eq.0) .and. (q.ge.d) )
!         np = np+1
!         p(np) = d
!         e(np) = 0
!         m = m/d                       ! recherche de l'exposant
!         e(np) = e(np)+1
!         if (mod(m,d).eq.0) then
!           m = m/d                       ! recherche de l'exposant
!           e(np) = e(np)+1
!         end if
!         q = n/d
!     end do

     if (m .gt. 1) then
       np = np+1
       p(np) = m
       e(np) = 1
     end if
     return
   end subroutine factorise
      
   subroutine factor_to_str( np, p, e, str_factors )
!    Met en forme les facteurs premiers de n pour que ca tienne sur 1 ligne

     integer(kind=LONG), intent(in)                          :: np
     integer(kind=LONG), intent(in), dimension(100)          :: p
     integer(kind=LONG), intent(in), dimension(100)          :: e
     character(len=60) , intent(out)                         :: str_factors
!
     integer(kind=LONG)                                      :: k
     character(len=1)                                        :: mult
     character(len=12)                                       :: str_p
     character(len=12)                                       :: str_e
!
      str_factors = ''
      mult = '*'
      do k = 1, np
        write(str_p,'(i12)') p(k)
        write(str_e,'(i12)') e(k)
        call left_justify(str_p,12,str_p) 
        call left_justify(str_e,12,str_e) 
        if (k.eq.np) mult=''
        if (e(k).eq.1) then
          str_factors = str_factors(1:len_trim(str_factors))  &
                        // str_p(1:len_trim(str_p)) // mult
        else
          str_factors = str_factors(1:len_trim(str_factors))  &
                        // str_p(1:len_trim(str_p)) // '^'    &
                        // str_e(1:len_trim(str_e)) // mult
        end if
      end do
      return
   end subroutine factor_to_str
!
!
   subroutine left_justify (str,l,str_lj)

!    justifie a gauche une chaine de caracteres FORTRAN
!
      character(len=12) , intent(inout)  :: str
      integer(kind=LONG), intent(in)     :: l
      character(len=12) , intent(inout)  :: str_lj
!
      integer(kind=LONG)                 :: i
      integer(kind=LONG)                 :: nb

      nb = 0
      i = 1
      do while( (i .le. l) .and. (str(i:i).eq.' ') )
          nb = nb+1
          i = i+ 1
      end do
      str_lj = str(nb+1:l)
      return
   end subroutine left_justify
!
!
   subroutine iftosp( Interferogram, Spectrum )
     !$PRAGMA C(mnpfftw)
     type(type_Interferogram), intent(in)             :: Interferogram
     type(type_Spectrum)     , intent(inout)          :: Spectrum
     integer(kind=LONG)                               :: NsFft
     integer(kind=LONG)                               :: NsFftp
     integer(kind=LONG)                               :: Sens
     real(kind=DOUBLE)    , dimension(:), allocatable :: crp
     real(kind=DOUBLE)    , dimension(:), allocatable :: cip
     integer(kind=LONG)                               :: ErrCode
     integer(kind=LONG)                               :: ioalloc
!
     NsFft  = Interferogram%N_Sample-1
     NsFftp = int(NsFft/2)
     Sens   = -1
!
!    allocation
     ioalloc = 0
     allocate( crp(NsFft), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( cip(NsFft), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if( ioalloc /= 0 ) then
        write(*,*) 'iftosp allocation Error',ioalloc
        call exit(1)
     end if
     crp(1:NsFft) = 0.d+00
     cip(1:NsFft) = 0.d+00
!    interferogram permutation
     if( Interferogram%Type(1:len_trim(Interferogram%Type))      == 'C'  ) then
       crp(1:NsFftp)       = dreal( Interferogram%Complex(NsFftp+1:NsFft) )
       cip(1:NsFftp)       = dimag( Interferogram%Complex(NsFftp+1:NsFft) )
       crp(NsFftp+1:NsFft) = dreal( Interferogram%Complex(1:NsFftp) )
       cip(NsFftp+1:NsFft) = dimag( Interferogram%Complex(1:NsFftp) )
       crp(NsFftp+1)       = 0.5d+00                                   &
                           * ( dreal( Interferogram%Complex(1) )       &
                              +dreal( Interferogram%Complex(NsFft+1) ) )
       cip(NsFftp+1)       = 0.5d+00 &
                           * ( dimag( Interferogram%Complex(1) )       &
                              +dimag( Interferogram%Complex(NsFft+1) ) )
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'RI' ) then
       crp(1:NsFftp)       = Interferogram%Real_Part(NsFftp+1:NsFft)
       cip(1:NsFftp)       = Interferogram%Imag_Part(NsFftp+1:NsFft)
       crp(NsFftp+1:NsFft) = Interferogram%Real_Part(1:NsFftp)
       cip(NsFftp+1:NsFft) = Interferogram%Imag_Part(1:NsFftp)
       crp(NsFftp+1)       = 0.5d+00                                   &
                           * ( Interferogram%Real_Part(1)              &
                              +Interferogram%Real_Part(NsFft+1) )
       cip(NsFftp+1)       = 0.5d+00                                   &
                           * ( Interferogram%Imag_Part(1)              &
                              +Interferogram%Imag_Part(NsFft+1) )
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'MA' ) then
       crp(1:NsFftp)       = Interferogram%Modulus(NsFftp+1:NsFft)     &
                           * dcos( Interferogram%Argument(NsFftp+1:NsFft) )
       crp(NsFftp+1:NsFft) = Interferogram%Modulus(1:NsFftp)           &
                           * dcos( Interferogram%Argument(1:NsFftp) )
       crp(NsFftp+1)       = 0.5d+00                                   &
                           * ( (Interferogram%Modulus(1)               &
                                *dcos( Interferogram%Argument(1) ))    &
                              +(Interferogram%Modulus(NsFft+1)         &
                                *dcos( Interferogram%Argument(NsFft+1) )) )
       cip(1:NsFftp)       = Interferogram%Modulus(NsFftp+1:NsFft)     &
                           * dsin( Interferogram%Argument(NsFftp+1:NsFft) )
       cip(NsFftp+1:NsFft) = Interferogram%Modulus(1:NsFftp)           &
                           * dsin( Interferogram%Argument(1:NsFftp) )
       cip(NsFftp+1)       = 0.5d+00                                   &
                           * ( (Interferogram%Modulus(1)               &
                                *dsin( Interferogram%Argument(1) ))    &
                              +(Interferogram%Modulus(NsFft+1)         &
                                *dsin( Interferogram%Argument(NsFft+1) )) )
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'R'  ) then
       crp(1:NsFftp)       = Interferogram%Real_Part(NsFftp+1:NsFft)
       crp(NsFftp+1:NsFft) = Interferogram%Real_Part(1:NsFftp)
       crp(NsFftp+1)       = 0.5d+00                                   &
                           * ( Interferogram%Real_Part(1)              &
                              +Interferogram%Real_Part(NsFft+1) )
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'I'  ) then
       cip(1:NsFftp)       = Interferogram%Imag_Part(NsFftp+1:NsFft) 
       cip(NsFftp+1:NsFft) = Interferogram%Imag_Part(1:NsFftp)
       cip(NsFftp+1)       = 0.5d+00                                   &
                           * ( Interferogram%Imag_Part(1)              &
                              +Interferogram%Imag_Part(NsFft+1) )
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'M'  ) then
       crp(1:NsFftp)       = Interferogram%Modulus(NsFftp+1:NsFft) 
       crp(NsFftp+1:NsFft) = Interferogram%Modulus(1:NsFftp)
       crp(NsFftp+1)       = 0.5d+00                                   &
                           * ( Interferogram%Modulus(1)                &
                              +Interferogram%Modulus(NsFft+1) )
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'A'  ) then
       crp(1:NsFftp)       = dcos( Interferogram%Argument(NsFftp+1:NsFft) )
       crp(NsFftp+1:NsFft) = dcos( Interferogram%Argument(1:NsFftp) )
       crp(NsFftp+1)       = 0.5d+00                                   &
                           * ( dcos( Interferogram%Argument(1) )       &
                              +dcos( Interferogram%Argument(NsFft+1) ) )
       cip(1:NsFftp)       = dsin( Interferogram%Argument(NsFftp+1:NsFft) )
       cip(NsFftp+1:NsFft) = dsin( Interferogram%Argument(1:NsFftp) )
       cip(NsFftp+1)       = 0.5d+00                                   &
                           * ( dsin( Interferogram%Argument(1) )       &
                              +dsin( Interferogram%Argument(NsFft+1) ) )
     end if
     crp(1:NsFft) = crp(1:NsFft) * Interferogram%dOpd
     cip(1:NsFft) = cip(1:NsFft) * Interferogram%dOpd
!
!    Fourier Transform
     call mnpfftw(%VAL(NsFft),crp,cip,%VAL(Sens))
!
     crp(1:NsFft) = crp(1:NsFft) / Spectrum%dWn
     cip(1:NsFft) = cip(1:NsFft) / Spectrum%dWn
!
!    spectrum extraction
     if( Spectrum%Type(1:len_trim(Spectrum%Type))           == 'C'  ) then
       Spectrum%Complex(1:Spectrum%N_Sample) = dcmplx                      &
              (crp(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1)&
              ,cip(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1))
     else if( Spectrum%Type(1:len_trim(Spectrum%Type))      == 'RI' ) then
       Spectrum%Real_Part(1:Spectrum%N_Sample) =                           &
              crp(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1)
       Spectrum%Imag_Part(1:Spectrum%N_Sample) =                           &
              cip(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1)
     else if( Spectrum%Type(1:len_trim(Spectrum%Type))      == 'MA' ) then
       Spectrum%Modulus(1:Spectrum%N_Sample) = dsqrt(                      &
           crp(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1)**2 &
          + cip(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1)**2)
       Spectrum%Argument(1:Spectrum%N_Sample) = datan2                     &
              (cip(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1)&
              ,crp(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1))
     else if( Spectrum%Type(1:len_trim(Spectrum%Type))      == 'R'  ) then
       Spectrum%Real_Part(1:Spectrum%N_Sample) =                           &
              crp(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1)
     else if( Spectrum%Type(1:len_trim(Spectrum%Type))      == 'I'  ) then
       Spectrum%Imag_Part(1:Spectrum%N_Sample) =                           &
              cip(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1)
     else if( Spectrum%Type(1:len_trim(Spectrum%Type))      == 'M'  ) then
       Spectrum%Modulus(1:Spectrum%N_Sample) = dsqrt(                      &
           crp(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1)**2 &
          + cip(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1)**2)
     else if( Spectrum%Type(1:len_trim(Spectrum%Type))      == 'A'  ) then
       Spectrum%Argument(1:Spectrum%N_Sample) = datan2                     &
              (cip(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1)&
              ,crp(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1))
     end if
     deallocate( crp )
     deallocate( cip )
     return
   end subroutine iftosp
!
!
   subroutine iftosp_open( Interferogram, Spectrum )
     !$PRAGMA C(mnpfftw)
     type(type_Interferogram), intent(in)             :: Interferogram
     type(type_Spectrum)     , intent(inout)          :: Spectrum
     integer(kind=LONG)                               :: NsFft
     integer(kind=LONG)                               :: NsFftp
     integer(kind=LONG)                               :: Sens
     real(kind=DOUBLE)    , dimension(:), allocatable :: crp
     real(kind=DOUBLE)    , dimension(:), allocatable :: cip
     integer(kind=LONG)                               :: ErrCode
     integer(kind=LONG)                               :: ioalloc
!
     NsFft  = Interferogram%N_Sample-1
     NsFftp = int(NsFft/2)
     Sens   = -1
!
!    allocation
     ioalloc = 0
     allocate( crp(NsFft), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( cip(NsFft), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if( ioalloc /= 0 ) then
        write(*,*) 'iftosp allocation Error',ioalloc
        call exit(1)
     end if
     crp(1:NsFft) = 0.d+00
     cip(1:NsFft) = 0.d+00
!    interferogram permutation
     if( Interferogram%Type(1:len_trim(Interferogram%Type))      == 'C'  ) then
       crp(1:NsFftp)       = dreal( Interferogram%Complex(NsFftp+1:NsFft) )
       cip(1:NsFftp)       = dimag( Interferogram%Complex(NsFftp+1:NsFft) )
       crp(NsFftp+1:NsFft) = dreal( Interferogram%Complex(1:NsFftp) )
       cip(NsFftp+1:NsFft) = dimag( Interferogram%Complex(1:NsFftp) )
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'RI' ) then
       crp(1:NsFftp)       = Interferogram%Real_Part(NsFftp+1:NsFft)
       cip(1:NsFftp)       = Interferogram%Imag_Part(NsFftp+1:NsFft)
       crp(NsFftp+1:NsFft) = Interferogram%Real_Part(1:NsFftp)
       cip(NsFftp+1:NsFft) = Interferogram%Imag_Part(1:NsFftp)
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'MA' ) then
       crp(1:NsFftp)       = Interferogram%Modulus(NsFftp+1:NsFft)     &
                           * dcos( Interferogram%Argument(NsFftp+1:NsFft) )
       crp(NsFftp+1:NsFft) = Interferogram%Modulus(1:NsFftp)           &
                           * dcos( Interferogram%Argument(1:NsFftp) )
       cip(1:NsFftp)       = Interferogram%Modulus(NsFftp+1:NsFft)     &
                           * dsin( Interferogram%Argument(NsFftp+1:NsFft) )
       cip(NsFftp+1:NsFft) = Interferogram%Modulus(1:NsFftp)           &
                           * dsin( Interferogram%Argument(1:NsFftp) )
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'R'  ) then
       crp(1:NsFftp)       = Interferogram%Real_Part(NsFftp+1:NsFft)
       crp(NsFftp+1:NsFft) = Interferogram%Real_Part(1:NsFftp)
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'I'  ) then
       cip(1:NsFftp)       = Interferogram%Imag_Part(NsFftp+1:NsFft) 
       cip(NsFftp+1:NsFft) = Interferogram%Imag_Part(1:NsFftp)
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'M'  ) then
       crp(1:NsFftp)       = Interferogram%Modulus(NsFftp+1:NsFft) 
       crp(NsFftp+1:NsFft) = Interferogram%Modulus(1:NsFftp)
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'A'  ) then
       crp(1:NsFftp)       = dcos( Interferogram%Argument(NsFftp+1:NsFft) )
       crp(NsFftp+1:NsFft) = dcos( Interferogram%Argument(1:NsFftp) )
       cip(1:NsFftp)       = dsin( Interferogram%Argument(NsFftp+1:NsFft) )
       cip(NsFftp+1:NsFft) = dsin( Interferogram%Argument(1:NsFftp) )
     end if
     crp(1:NsFft) = crp(1:NsFft) * Interferogram%dOpd
     cip(1:NsFft) = cip(1:NsFft) * Interferogram%dOpd
!
!    Fourier Transform
     call mnpfftw(%VAL(NsFft),crp,cip,%VAL(Sens))
!
     crp(1:NsFft) = crp(1:NsFft) / Spectrum%dWn
     cip(1:NsFft) = cip(1:NsFft) / Spectrum%dWn
!
!    spectrum extraction
     if( Spectrum%Type(1:len_trim(Spectrum%Type))           == 'C'  ) then
       Spectrum%Complex(1:Spectrum%N_Sample) = dcmplx                      &
              (crp(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1)&
              ,cip(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1))
     else if( Spectrum%Type(1:len_trim(Spectrum%Type))      == 'RI' ) then
       Spectrum%Real_Part(1:Spectrum%N_Sample) =                           &
              crp(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1)
       Spectrum%Imag_Part(1:Spectrum%N_Sample) =                           &
              cip(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1)
     else if( Spectrum%Type(1:len_trim(Spectrum%Type))      == 'MA' ) then
       Spectrum%Modulus(1:Spectrum%N_Sample) = dsqrt(                      &
           crp(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1)**2 &
          + cip(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1)**2)
       Spectrum%Argument(1:Spectrum%N_Sample) = datan2                     &
              (cip(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1)&
              ,crp(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1))
     else if( Spectrum%Type(1:len_trim(Spectrum%Type))      == 'R'  ) then
       Spectrum%Real_Part(1:Spectrum%N_Sample) =                           &
              crp(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1)
     else if( Spectrum%Type(1:len_trim(Spectrum%Type))      == 'I'  ) then
       Spectrum%Imag_Part(1:Spectrum%N_Sample) =                           &
              cip(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1)
     else if( Spectrum%Type(1:len_trim(Spectrum%Type))      == 'M'  ) then
       Spectrum%Modulus(1:Spectrum%N_Sample) = dsqrt(                      &
           crp(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1)**2 &
          + cip(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1)**2)
     else if( Spectrum%Type(1:len_trim(Spectrum%Type))      == 'A'  ) then
       Spectrum%Argument(1:Spectrum%N_Sample) = datan2                     &
              (cip(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1)&
              ,crp(Spectrum%Ns_First:Spectrum%Ns_First+Spectrum%N_Sample-1))
     end if
     deallocate( crp )
     deallocate( cip )
     return
   end subroutine iftosp_open
!
!
   subroutine sptoif( Spectrum, Interferogram )
     !$PRAGMA C(mnpfftw)
     type(type_Spectrum)     , intent(in)             :: Spectrum
     type(type_Interferogram), intent(inout)          :: Interferogram
     integer(kind=LONG)                               :: NsFft
     integer(kind=LONG)                               :: NsFftp
     integer(kind=LONG)                               :: NsFirst
     integer(kind=LONG)                               :: NsLast
     integer(kind=LONG)                               :: Sens
     real(kind=DOUBLE)    , dimension(:), allocatable :: crp
     real(kind=DOUBLE)    , dimension(:), allocatable :: cip
     integer(kind=LONG)                               :: ErrCode
     integer(kind=LONG)                               :: ioalloc
!
     NsFft  = Interferogram%N_Sample-1
     NsFftp = int(NsFft/2)
     Sens   = +1
!
!    allocation
     ioalloc = 0
     allocate( crp(NsFft+1), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( cip(NsFft+1), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if( ioalloc /= 0 ) then
        write(*,*) 'sptoif allocation Error',ioalloc
        call exit(1)
     end if
     crp(1:NsFft+1) = 0.d+00
     cip(1:NsFft+1) = 0.d+00
!    spectrum permutation
     NsFirst = Spectrum%Ns_First
     NsLast  = Spectrum%Ns_Last
     if( Spectrum%Type(1:len_trim(Spectrum%Type))      == 'C'  ) then
       crp(1+(NsFirst-1):1+(NsLast-1)) =                    &
               dreal( Spectrum%Complex(1:Spectrum%N_Sample) )
       cip(1+(NsFirst-1):1+(NsLast-1)) =                    &
               dimag( Spectrum%Complex(1:Spectrum%N_Sample) )
       crp(NsFft+1-(NsLast-1):NsFft+1-(NsFirst-1)) =           &
               dreal( Spectrum%Complex(Spectrum%N_Sample:1:-1) )
       cip(NsFft+1-(NsLast-1):NsFft+1-(NsFirst-1)) =           &
              -dimag( Spectrum%Complex(Spectrum%N_Sample:1:-1) )
     else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'RI' ) then
       crp(1+(NsFirst-1):1+(NsLast-1)) =             &
               Spectrum%Real_Part(1:Spectrum%N_Sample)
       cip(1+(NsFirst-1):1+(NsLast-1)) =             &
               Spectrum%Imag_Part(1:Spectrum%N_Sample)
       crp(NsFft+1-(NsLast-1):NsFft+1-(NsFirst-1)) =    &
               Spectrum%Real_Part(Spectrum%N_Sample:1:-1)
       cip(NsFft+1-(NsLast-1):NsFft+1-(NsFirst-1)) =    &
              -Spectrum%Imag_Part(Spectrum%N_Sample:1:-1)
     else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'MA' ) then
       crp(1+(NsFirst-1):1+(NsLast-1)) =                      &
               Spectrum%Modulus(1:Spectrum%N_Sample)          &
               * dcos( Spectrum%Argument(1:Spectrum%N_Sample) )
       crp(NsFft+1-(NsLast-1):NsFft+1-(NsFirst-1)) =             &
               Spectrum%Modulus(Spectrum%N_Sample:1:-1)          &
               * dcos( Spectrum%Argument(Spectrum%N_Sample:1:-1) )
       cip(1+(NsFirst-1):1+(NsLast-1)) =                      &
               Spectrum%Modulus(1:Spectrum%N_Sample)          &
               * dsin( Spectrum%Argument(1:Spectrum%N_Sample) )
       cip(NsFft+1-(NsLast-1):NsFft+1-(NsFirst-1)) =             &
              -Spectrum%Modulus(Spectrum%N_Sample:1:-1)          &
               * dsin( Spectrum%Argument(Spectrum%N_Sample:1:-1) )
     else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'R'  ) then
       crp(1+(NsFirst-1):1+(NsLast-1)) =             &
               Spectrum%Real_Part(1:Spectrum%N_Sample)
       crp(NsFft+1-(NsLast-1):NsFft+1-(NsFirst-1)) =    &
               Spectrum%Real_Part(Spectrum%N_Sample:1:-1)
     else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'I'  ) then
       cip(1+(NsFirst-1):1+(NsLast-1)) =             &
               Spectrum%Imag_Part(1:Spectrum%N_Sample) 
       cip(NsFft+1-(NsLast-1):NsFft+1-(NsFirst-1)) =    &
              -Spectrum%Imag_Part(Spectrum%N_Sample:1:-1)
     else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'M'  ) then
       crp(1+(NsFirst-1):1+(NsLast-1)) =           &
               Spectrum%Modulus(1:Spectrum%N_Sample) 
       crp(NsFft+1-(NsLast-1):NsFft+1-(NsFirst-1)) =  &
               Spectrum%Modulus(Spectrum%N_Sample:1:-1)
     else if( Spectrum%Type(1:len_trim(Spectrum%Type)) == 'A'  ) then
       crp(1+(NsFirst-1):1+(NsLast-1)) =                    &
               dcos( Spectrum%Argument(1:Spectrum%N_Sample) )
       crp(NsFft+1-(NsLast-1):NsFft+1-(NsFirst-1)) =           &
               dcos( Spectrum%Argument(Spectrum%N_Sample:1:-1) )
       cip(1+(NsFirst-1):1+(NsLast-1)) =                    &
               dsin( Spectrum%Argument(1:Spectrum%N_Sample) )
       cip(NsFft+1-(NsLast-1):NsFft+1-(NsFirst-1)) =           &
              -dsin( Spectrum%Argument(Spectrum%N_Sample:1:-1) )
     end if
     crp(1:NsFft) = crp(1:NsFft) * Spectrum%dWn
     cip(1:NsFft) = cip(1:NsFft) * Spectrum%dWn
!
!    Fourier Transform
     call mnpfftw(%VAL(NsFft),crp,cip,%VAL(Sens))
!
     crp(1:NsFft) = crp(1:NsFft) / Interferogram%dOpd
     cip(1:NsFft) = cip(1:NsFft) / Interferogram%dOpd
!
!    interferogram extraction
     if( Interferogram%Type(1:len_trim(Interferogram%Type))      == 'C'  ) then
       Interferogram%Complex(1:NsFftp) =                                 &
              dcmplx(crp(NsFftp+1:NsFft),cip(NsFftp+1:NsFft))
       Interferogram%Complex(NsFftp+1:NsFft+1) =                         &
              dcmplx(crp(1:NsFftp+1),cip(1:NsFftp+1))
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'RI' ) then
       Interferogram%Real_Part(1:NsFftp) = crp(NsFftp+1:NsFft)
       Interferogram%Imag_Part(1:NsFftp) = cip(NsFftp+1:NsFft)
       Interferogram%Real_Part(NsFftp+1:NsFft+1) = crp(1:NsFftp+1)
       Interferogram%Imag_Part(NsFftp+1:NsFft+1) = cip(1:NsFftp+1)
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'MA' ) then
       Interferogram%Modulus(1:NsFftp) = dsqrt(                          &
              crp(NsFftp+1:NsFft)**2+cip(NsFftp+1:NsFft)**2)
       Interferogram%Argument(1:NsFftp) = datan2                         &
              (cip(NsFftp+1:NsFft),crp(NsFftp+1:NsFft))
       Interferogram%Modulus(NsFftp+1:NsFft+1) = dsqrt(                  &
              crp(1:NsFftp)**2+cip(1:NsFftp)**2)
       Interferogram%Argument(NsFftp+1:NsFft+1) = datan2                 &
              (cip(1:NsFftp),crp(1:NsFftp))
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'R'  ) then
       Interferogram%Real_Part(1:NsFftp) = crp(NsFftp+1:NsFft)
       Interferogram%Real_Part(NsFftp+1:NsFft+1) = crp(1:NsFftp+1)
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'I'  ) then
       Interferogram%Imag_Part(1:NsFftp) = cip(NsFftp+1:NsFft)
       Interferogram%Imag_Part(NsFftp+1:NsFft+1) = cip(1:NsFftp+1)
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'M'  ) then
       Interferogram%Modulus(1:NsFftp) = dsqrt(                          &
              crp(NsFftp+1:NsFft)**2+cip(NsFftp+1:NsFft)**2)
       Interferogram%Modulus(NsFftp+1:NsFft+1) = dsqrt(                  &
              crp(1:NsFftp)**2+cip(1:NsFftp)**2)
     else if( Interferogram%Type(1:len_trim(Interferogram%Type)) == 'A'  ) then
       Interferogram%Argument(1:NsFftp) = datan2                         &
              (cip(NsFftp+1:NsFft),crp(NsFftp+1:NsFft))
       Interferogram%Argument(NsFftp+1:NsFft+1) = datan2                 &
              (cip(1:NsFftp),crp(1:NsFftp))
     end if
     deallocate( crp )
     deallocate( cip )
     return
   end subroutine sptoif
!
!
   subroutine fft_r2r( NsFft, Sens, R_In, dR_In,  dR_Out, R_Out )
   implicit none
     !$PRAGMA C(mnpfftw)
     integer(kind=LONG) ,intent(in)                        :: NsFft
     integer(kind=LONG) ,intent(in)                        :: Sens
     real(kind=DOUBLE)  ,intent(in)   ,dimension(1:NsFft+1):: R_In
     real(kind=DOUBLE)  ,intent(in)                        :: dR_In
     real(kind=DOUBLE)  ,intent(in)                        :: dR_Out
     real(kind=DOUBLE)  ,intent(inout),dimension(1:NsFft+1):: R_Out
     integer(kind=LONG)                                    :: NsFftp
     real(kind=DOUBLE)  ,dimension(:)  ,allocatable        :: crp
     real(kind=DOUBLE)  ,dimension(:)  ,allocatable        :: cip
     integer(kind=LONG)                                    :: ErrCode
     integer(kind=LONG)                                    :: ioalloc
!
!    allocation
     ioalloc = 0
     allocate( crp(NsFft), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( cip(NsFft), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if( ioalloc /= 0 ) then
        write(*,*) 'fft_r2r allocation Error',ioalloc
        call exit(1)
     end if
     crp(1:NsFft) = 0.d+00
     cip(1:NsFft) = 0.d+00
!
     NsFftp = int( NsFft/2 )
!
!    rangement des echantillons en entree
     crp(1:NsFftp)           = (R_In(1+NsFftp:2*NsFftp))*dR_In
     crp(1+NsFftp:2*NsFftp)  = (R_In(1:NsFftp))*dR_In
!
     crp(NsFftp+1) = 5.d-01*(R_In(1)+R_In(NsFft+1))*dR_In
!
!    Fourier Transform
     call mnpfftw(%VAL(NsFft),crp,cip,%VAL(Sens))
!
!    Rangement des echantillons en sortie
     R_Out(1+NsFftp:2*NsFftp) = crp(1:NsFftp)/dR_Out 
     R_Out(1:NsFftp) = crp(1+NsFftp:2*NsFftp)/dR_Out 
     R_Out(NsFft+1) = R_Out(1)
!
!    deallocation
     deallocate( crp )
     deallocate( cip )
     return
   end subroutine fft_r2r
!
!
   subroutine fft_c2c( NsFft, Sens, C_In, dC_In, dC_Out, C_Out )
   implicit none
     !$PRAGMA C(mnpfftw)
     integer(kind=LONG)   ,intent(in)                          :: NsFft
     integer(kind=LONG)   ,intent(in)                          :: Sens
     complex(kind=DOUBLE) ,intent(in)   ,dimension(1:NsFft+1)  :: C_In
     real(kind=DOUBLE)    ,intent(in)                          :: dC_In
     real(kind=DOUBLE)    ,intent(in)                          :: dC_Out
     complex(kind=DOUBLE) ,intent(inout),dimension(1:NsFft+1)  :: C_Out
     integer(kind=LONG)                                        :: NsFftp
     real(kind=DOUBLE)    ,dimension(:) ,allocatable           :: crp
     real(kind=DOUBLE)    ,dimension(:) ,allocatable           :: cip
     integer(kind=LONG)                                        :: ErrCode
     integer(kind=LONG)                                        :: ioalloc
!
!    allocation
     ioalloc = 0
     allocate( crp(NsFft), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( cip(NsFft), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if( ioalloc /= 0 ) then
        write(*,*) 'fft_c2c allocation Error',ioalloc
        call exit(1)
     end if
     crp(1:NsFft) = 0.d+00
     cip(1:NsFft) = 0.d+00
!
     NsFftp = int( NsFft/2 )
!
!    rangement des echantillons en entree
     crp(1:NsFftp)          = dreal(C_In(1+NsFftp:2*NsFftp))*dC_In
     crp(1+NsFftp:2*NsFftp) = dreal(C_In(1:NsFftp))*dC_In
     cip(1:NsFftp)          = dimag(C_In(1+NsFftp:2*NsFftp))*dC_In
     cip(1+NsFftp:2*NsFftp) = dimag(C_In(1:NsFftp))*dC_In
!
     crp(NsFftp+1) = 5.d-01*dreal(C_In(1)+C_In(NsFft+1))*dC_In
     cip(NsFftp+1) = 5.d-01*dimag(C_In(1)+C_In(NsFft+1))*dC_In
!
!    Fourier Transform
     call mnpfftw(%VAL(NsFft),crp,cip,%VAL(Sens))
!
!    Rangement des echantillons en sortie
     C_Out(1+NsFftp:2*NsFftp) = dcmplx( crp(1:NsFftp)/dC_Out &
                                       ,cip(1:NsFftp)/dC_Out )
     C_Out(1:NsFftp) = dcmplx( crp(1+NsFftp:2*NsFftp)/dC_Out &
                              ,cip(1+NsFftp:2*NsFftp)/dC_Out )
     C_Out(NsFft+1) = C_Out(1)
!
!    deallocation
     deallocate( crp )
     deallocate( cip )
     return
   end subroutine fft_c2c
!
!
   subroutine fft_r2c( NsFft, Sens, R_In, dR_In, dC_Out, C_Out )
   implicit none
     !$PRAGMA C(mnpfftw)
     integer(kind=LONG)   ,intent(in)                          :: NsFft
     integer(kind=LONG)   ,intent(in)                          :: Sens
     real(kind=DOUBLE)    ,intent(in)   ,dimension(1:NsFft+1)  :: R_In
     real(kind=DOUBLE)    ,intent(in)                          :: dR_In
     real(kind=DOUBLE)    ,intent(in)                          :: dC_Out
     complex(kind=DOUBLE) ,intent(inout),dimension(1:NsFft+1)  :: C_Out
     integer(kind=LONG)                                        :: NsFftp
     real(kind=DOUBLE)    ,dimension(:) ,allocatable           :: crp
     real(kind=DOUBLE)    ,dimension(:) ,allocatable           :: cip
     integer(kind=LONG)                                        :: ErrCode
     integer(kind=LONG)                                        :: ioalloc
!
!    allocation
     ioalloc = 0
     allocate( crp(NsFft), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( cip(NsFft), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if( ioalloc /= 0 ) then
        write(*,*) 'fft_r2c allocation Error',ioalloc
        call exit(1)
     end if
     crp(1:NsFft) = 0.d+00
     cip(1:NsFft) = 0.d+00
!
     NsFftp = int( NsFft/2 )
!
!    rangement des echantillons en entree
     crp(1:NsFftp)          = R_In(1+NsFftp:2*NsFftp)*dR_In
     crp(1+NsFftp:2*NsFftp) = R_In(1:NsFftp)*dR_In
!
     crp(NsFftp+1) = 5.d-01*(R_In(1)+R_In(NsFft+1))*dR_In
!
!    Fourier Transform
     call mnpfftw(%VAL(NsFft),crp,cip,%VAL(Sens))
!
!    Rangement des echantillons en sortie
     C_Out(1+NsFftp:2*NsFftp) = dcmplx( crp(1:NsFftp)/dC_Out &
                                       ,cip(1:NsFftp)/dC_Out )
     C_Out(1:NsFftp) = dcmplx( crp(1+NsFftp:2*NsFftp)/dC_Out &
                              ,cip(1+NsFftp:2*NsFftp)/dC_Out )
     C_Out(NsFft+1) = C_Out(1)
!
!    deallocation
     deallocate( crp )
     deallocate( cip )
     return
   end subroutine fft_r2c
!
!
   subroutine fft_c2r( NsFft, Sens, C_In, dC_In, dR_Out, R_Out )
   implicit none
     !$PRAGMA C(mnpfftw)
     integer(kind=LONG)  ,intent(in)                           :: NsFft
     integer(kind=LONG)  ,intent(in)                           :: Sens
     complex(kind=DOUBLE),intent(in)   ,dimension(1:NsFft+1)   :: C_In
     real(kind=DOUBLE)   ,intent(in)                           :: dC_In
     real(kind=DOUBLE)   ,intent(in)                           :: dR_Out
     real(kind=DOUBLE)   ,intent(inout),dimension(1:NsFft+1)   :: R_Out
     integer(kind=LONG)                                        :: NsFftp
     real(kind=DOUBLE)   ,dimension(:) ,allocatable            :: crp
     real(kind=DOUBLE)   ,dimension(:) ,allocatable            :: cip
     integer(kind=LONG)                                        :: ErrCode
     integer(kind=LONG)                                        :: ioalloc
!
!    allocation
     ioalloc = 0
     allocate( crp(NsFft), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( cip(NsFft), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if( ioalloc /= 0 ) then
        write(*,*) 'fft_r2c allocation Error',ioalloc
        call exit(1)
     end if
     crp(1:NsFft) = 0.d+00
     cip(1:NsFft) = 0.d+00
!
     NsFftp = int( NsFft/2 )
!
!    rangement des echantillons en entree
     crp(1:NsFftp)          = dreal(C_In(1+NsFftp:2*NsFftp))*dC_In
     crp(1+NsFftp:2*NsFftp) = dreal(C_In(1:NsFftp))*dC_In
     cip(1:NsFftp)          = dimag(C_In(1+NsFftp:2*NsFftp))*dC_In
     cip(1+NsFftp:2*NsFftp) = dimag(C_In(1:NsFftp))*dC_In
!
     crp(NsFftp+1) = 5.d-01*dreal(C_In(1)+C_In(NsFft+1))*dC_In
     cip(NsFftp+1) = 5.d-01*dimag(C_In(1)+C_In(NsFft+1))*dC_In
!
!    Fourier Transform
     call mnpfftw(%VAL(NsFft),crp,cip,%VAL(Sens))
!
!    Rangement des echantillons en sortie
     R_Out(1+NsFftp:2*NsFftp) = crp(1:NsFftp)/dR_Out 
     R_Out(1:NsFftp) = crp(1+NsFftp:2*NsFftp)/dR_Out
     R_Out(NsFft+1) = R_Out(1)
!
!    deallocation
     deallocate( crp )
     deallocate( cip )
     return
   end subroutine fft_c2r
!
!
   subroutine fft_r2r_open( NsFft, Sens, R_In, dR_In, dR_Out, R_Out )
   implicit none
     !$PRAGMA C(mnpfftw)
     integer(kind=LONG) ,intent(in)                        :: NsFft
     integer(kind=LONG) ,intent(in)                        :: Sens
     real(kind=DOUBLE)  ,intent(in)   ,dimension(1:NsFft+1):: R_In
     real(kind=DOUBLE)  ,intent(in)                        :: dR_In
     real(kind=DOUBLE)  ,intent(in)                        :: dR_Out
     real(kind=DOUBLE)  ,intent(inout),dimension(1:NsFft+1):: R_Out
     integer(kind=LONG)                                    :: NsFftp
     real(kind=DOUBLE)  ,dimension(:)  ,allocatable        :: crp
     real(kind=DOUBLE)  ,dimension(:)  ,allocatable        :: cip
     integer(kind=LONG)                                    :: ErrCode
     integer(kind=LONG)                                    :: ioalloc
!
!    allocation
     ioalloc = 0
     allocate( crp(NsFft), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( cip(NsFft), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if( ioalloc /= 0 ) then
        write(*,*) 'fft_r2r allocation Error',ioalloc
        call exit(1)
     end if
     crp(1:NsFft) = 0.d+00
     cip(1:NsFft) = 0.d+00
!
     NsFftp = int( NsFft/2 )
!
!    rangement des echantillons en entree
     crp(1:NsFftp)           = (R_In(1+NsFftp:2*NsFftp))*dR_In
     crp(1+NsFftp:2*NsFftp)  = (R_In(1:NsFftp))*dR_In
!
!    Fourier Transform
     call mnpfftw(%VAL(NsFft),crp,cip,%VAL(Sens))
!
!    Rangement des echantillons en sortie
     R_Out(1+NsFftp:2*NsFftp) = crp(1:NsFftp)/dR_Out 
     R_Out(1:NsFftp) = crp(1+NsFftp:2*NsFftp)/dR_Out 
     R_Out(NsFft+1) = R_Out(1)
!
!    deallocation
     deallocate( crp )
     deallocate( cip )
     return
   end subroutine fft_r2r_open
!
!
   subroutine fft_c2c_open( NsFft, Sens, C_In, dC_In, dC_Out, C_Out )
   implicit none
     !$PRAGMA C(mnpfftw)
     integer(kind=LONG)   ,intent(in)                          :: NsFft
     integer(kind=LONG)   ,intent(in)                          :: Sens
     complex(kind=DOUBLE) ,intent(in)   ,dimension(1:NsFft+1)  :: C_In
     real(kind=DOUBLE)    ,intent(in)                          :: dC_In
     real(kind=DOUBLE)    ,intent(in)                          :: dC_Out
     complex(kind=DOUBLE) ,intent(inout),dimension(1:NsFft+1)  :: C_Out
     integer(kind=LONG)                                        :: NsFftp
     real(kind=DOUBLE)    ,dimension(:) ,allocatable           :: crp
     real(kind=DOUBLE)    ,dimension(:) ,allocatable           :: cip
     integer(kind=LONG)                                        :: ErrCode
     integer(kind=LONG)                                        :: ioalloc
!
!    allocation
     ioalloc = 0
     allocate( crp(NsFft), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( cip(NsFft), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if( ioalloc /= 0 ) then
        write(*,*) 'fft_c2c allocation Error',ioalloc
        call exit(1)
     end if
     crp(1:NsFft) = 0.d+00
     cip(1:NsFft) = 0.d+00
!
     NsFftp = int( NsFft/2 )
!
!    rangement des echantillons en entree
     crp(1:NsFftp)          = dreal(C_In(1+NsFftp:2*NsFftp))*dC_In
     crp(1+NsFftp:2*NsFftp) = dreal(C_In(1:NsFftp))*dC_In
     cip(1:NsFftp)          = dimag(C_In(1+NsFftp:2*NsFftp))*dC_In
     cip(1+NsFftp:2*NsFftp) = dimag(C_In(1:NsFftp))*dC_In
!
!    Fourier Transform
     call mnpfftw(%VAL(NsFft),crp,cip,%VAL(Sens))
!
!    Rangement des echantillons en sortie
     C_Out(1+NsFftp:2*NsFftp) = dcmplx( crp(1:NsFftp)/dC_Out &
                                       ,cip(1:NsFftp)/dC_Out )
     C_Out(1:NsFftp) = dcmplx( crp(1+NsFftp:2*NsFftp)/dC_Out &
                              ,cip(1+NsFftp:2*NsFftp)/dC_Out )
     C_Out(NsFft+1) = C_Out(1)
!
!    deallocation
     deallocate( crp )
     deallocate( cip )
     return
   end subroutine fft_c2c_open
!
!
   subroutine fft_r2c_open( NsFft, Sens, R_In, dR_In, dC_Out, C_Out )
   implicit none
     !$PRAGMA C(mnpfftw)
     integer(kind=LONG)   ,intent(in)                          :: NsFft
     integer(kind=LONG)   ,intent(in)                          :: Sens
     real(kind=DOUBLE)    ,intent(in)   ,dimension(1:NsFft+1)  :: R_In
     real(kind=DOUBLE)    ,intent(in)                          :: dR_In
     real(kind=DOUBLE)    ,intent(in)                          :: dC_Out
     complex(kind=DOUBLE) ,intent(inout),dimension(1:NsFft+1)  :: C_Out
     integer(kind=LONG)                                        :: NsFftp
     real(kind=DOUBLE)    ,dimension(:) ,allocatable           :: crp
     real(kind=DOUBLE)    ,dimension(:) ,allocatable           :: cip
     integer(kind=LONG)                                        :: ErrCode
     integer(kind=LONG)                                        :: ioalloc
!
!    allocation
     ioalloc = 0
     allocate( crp(NsFft), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( cip(NsFft), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if( ioalloc /= 0 ) then
        write(*,*) 'fft_r2c allocation Error',ioalloc
        call exit(1)
     end if
     crp(1:NsFft) = 0.d+00
     cip(1:NsFft) = 0.d+00
!
     NsFftp = int( NsFft/2 )
!
!    rangement des echantillons en entree
     crp(1:NsFftp)          = R_In(1+NsFftp:2*NsFftp)*dR_In
     crp(1+NsFftp:2*NsFftp) = R_In(1:NsFftp)*dR_In
!
!    Fourier Transform
     call mnpfftw(%VAL(NsFft),crp,cip,%VAL(Sens))
!
!    Rangement des echantillons en sortie
     C_Out(1+NsFftp:2*NsFftp) = dcmplx( crp(1:NsFftp)/dC_Out &
                                       ,cip(1:NsFftp)/dC_Out )
     C_Out(1:NsFftp) = dcmplx( crp(1+NsFftp:2*NsFftp)/dC_Out &
                              ,cip(1+NsFftp:2*NsFftp)/dC_Out )
     C_Out(NsFft+1) = C_Out(1)
!
!    deallocation
     deallocate( crp )
     deallocate( cip )
     return
   end subroutine fft_r2c_open
!
!
   subroutine fft_c2r_open( NsFft, Sens, C_In, dC_In, dR_Out, R_Out )
   implicit none
     !$PRAGMA C(mnpfftw)
     integer(kind=LONG)  ,intent(in)                           :: NsFft
     integer(kind=LONG)  ,intent(in)                           :: Sens
     complex(kind=DOUBLE),intent(in)   ,dimension(1:NsFft+1)   :: C_In
     real(kind=DOUBLE)   ,intent(in)                           :: dC_In
     real(kind=DOUBLE)   ,intent(in)                           :: dR_Out
     real(kind=DOUBLE)   ,intent(inout),dimension(1:NsFft+1)   :: R_Out
     integer(kind=LONG)                                        :: NsFftp
     real(kind=DOUBLE)   ,dimension(:) ,allocatable            :: crp
     real(kind=DOUBLE)   ,dimension(:) ,allocatable            :: cip
     integer(kind=LONG)                                        :: ErrCode
     integer(kind=LONG)                                        :: ioalloc
!
!    allocation
     ioalloc = 0
     allocate( crp(NsFft), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     allocate( cip(NsFft), stat=ErrCode )
     if( ErrCode /= 0 ) ioalloc = ioalloc + 1
     if( ioalloc /= 0 ) then
        write(*,*) 'fft_r2c allocation Error',ioalloc
        call exit(1)
     end if
     crp(1:NsFft) = 0.d+00
     cip(1:NsFft) = 0.d+00
!
     NsFftp = int( NsFft/2 )
!
!    rangement des echantillons en entree
     crp(1:NsFftp)          = dreal(C_In(1+NsFftp:2*NsFftp))*dC_In
     crp(1+NsFftp:2*NsFftp) = dreal(C_In(1:NsFftp))*dC_In
     cip(1:NsFftp)          = dimag(C_In(1+NsFftp:2*NsFftp))*dC_In
     cip(1+NsFftp:2*NsFftp) = dimag(C_In(1:NsFftp))*dC_In
!
!    Fourier Transform
     call mnpfftw(%VAL(NsFft),crp,cip,%VAL(Sens))
!
!    Rangement des echantillons en sortie
     R_Out(1+NsFftp:2*NsFftp) = crp(1:NsFftp)/dR_Out 
     R_Out(1:NsFftp) = crp(1+NsFftp:2*NsFftp)/dR_Out
     R_Out(NsFft+1) = R_Out(1)
!
!    deallocation
     deallocate( crp )
     deallocate( cip )
     return
   end subroutine fft_c2r_open
!
!
end module fft_module
