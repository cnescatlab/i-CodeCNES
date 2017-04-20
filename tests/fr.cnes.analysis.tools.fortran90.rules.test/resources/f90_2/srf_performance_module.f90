!!# srf_performance_module.f90 --
!!#
!!#           Project: SPS_GENERIC
!!#           Authors: NOVELTIS/B.TOURNIER
!!#              Date: october 2013
!!#
!!# Language:  F90
!!# Standards: Noveltis
!!#
!!# --
!!#
!! 
!> srf_performance_module -- Module
!!
!! * Purpose
!!
!!     Module for Srf performance computations
!!
!! * Description

module srf_performance_module
   use precision_type
   use error_type
   use constantes_type
   use srf_type
   use srf_perfo_type
!
   implicit none
!
!
   public :: collection_srf,          &
             collection_srf_nc,       &
             anal_srf_spectral,       &
             shapeindex
!
!
   contains
!
!
   subroutine collection_srf( Nb_Srf,  &
                              File_Srf,&
                              Srf      )
   integer(kind=LONG)      ,intent(in)                          :: Nb_Srf
   character(len=*)        ,intent(in),dimension(Nb_Srf)        :: File_Srf
   type(type_Srf)          ,intent(out),dimension(:),allocatable:: Srf
!
   integer(kind=LONG)                                           :: ErrCode
   integer(kind=LONG)                                           :: Nb
   integer(kind=LONG)                                           :: iPos
!
   allocate( Srf(Nb_Srf) )
!
!  lopp on the srf collection
   do Nb = 1, Nb_Srf
      iPos = Nb
      Srf(Nb)%filename = File_Srf(Nb)
      call readsrf( Srf(Nb), ErrCode )
      if( ErrCode /= 0 ) then
         write(*,*) 'Srferogram Reading Error',ErrCode
         write(*,'(a)') File_Srf(Nb)(1:len_trim(File_Srf(Nb)))
         go to 999
      end if
   end do
!
   return
999 write(*,*) 'collection_srf Fatal error',iPos
   call exit(1)
!
   end subroutine collection_srf
!
!
   subroutine collection_srf_nc( Nb_Srf,  &
                                 File_Srf,&
                                 Srf      )
   integer(kind=LONG)      ,intent(in)                          :: Nb_Srf
   character(len=*)        ,intent(in),dimension(Nb_Srf)        :: File_Srf
   type(type_Srf)          ,intent(out),dimension(:),allocatable:: Srf
!
   integer(kind=LONG)                                           :: ErrCode
   integer(kind=LONG)                                           :: Nb
   integer(kind=LONG)                                           :: iPos
!
   allocate( Srf(Nb_Srf) )
!
!  lopp on the srf collection
   do Nb = 1, Nb_Srf
      iPos = Nb
      Srf(Nb)%filename = File_Srf(Nb)
      call readsrf_netcdf( Srf(Nb), ErrCode )
      if( ErrCode /= 0 ) then
         write(*,*) 'Srferogram Reading Error',ErrCode
         write(*,'(a)') File_Srf(Nb)(1:len_trim(File_Srf(Nb)))
         go to 999
      end if
   end do
!
   return
999 write(*,*) 'collection_srf Fatal error',iPos
   call exit(1)
!
   end subroutine collection_srf_nc
!
!
  subroutine perfo_srf( Nb_per,   &
                        Srf_per,  &
                        Nb_nom,   &
                        Srf_nom,  &
                        Srf_Perfo )
   integer(kind=LONG)      ,intent(in)                          :: Nb_per
   type(type_Srf)          ,intent(inout),dimension(1:Nb_per)   :: Srf_per
   integer(kind=LONG)      ,intent(in)                          :: Nb_nom
   type(type_Srf)          ,intent(inout),dimension(1:Nb_nom)   :: Srf_nom
   type(type_Srf_Perfo)    ,intent(out),dimension(:),allocatable:: Srf_Perfo
   integer(kind=LONG)                                           :: ErrCode
   integer(kind=LONG)                                           :: PN
   integer(kind=LONG)                                           :: NF
   integer(kind=LONG)                                           :: Nb
   integer(kind=LONG)                                           :: Mb
!
!  consistency check
   if( (Nb_nom /= 1) .and. (Nb_nom /= Nb_per) ) then
      write(0,*) 'Nb_nom Nb_per inconsitency ', Nb_nom, Nb_per
      go to 999
   end if
!
   allocate( Srf_Perfo(Nb_per), stat=ErrCode )
   if( ErrCode /= 0 ) then
      write(0,*) 'allocation Error'
      go to 999
   end if
!
!  
   do Nb = 1, Nb_per
      if( Nb_nom == 1 ) then
         Mb = 1
      else
         Mb= Nb
      end if
      if( Srf_per(Nb)%NsPixel  /= Srf_nom(Mb)%NsPixel  .or. &
          Srf_per(Nb)%NsWn0    /= Srf_nom(Mb)%NsWn0    .or. &
          Srf_per(Nb)%NsSDWn1a /= Srf_nom(Mb)%NsSDWn1a .or. &
          Srf_per(Nb)%NsSDWn1b /= Srf_nom(Mb)%NsSDWn1b .or. &
          Srf_per(Nb)%NsSDWn1c /= Srf_nom(Mb)%NsSDWn1c ) then
         write(*,*) 'Srf inconsistency dimension'
         go to 999
      end if
      Srf_Perfo(Nb)%NsPixel       = Srf_nom(Mb)%NsPixel
      Srf_Perfo(Nb)%NsWn0         = Srf_nom(Mb)%NsWn0
      Srf_Perfo(Nb)%NsSDWn1a      = Srf_nom(Mb)%NsSDWn1a
      Srf_Perfo(Nb)%NsSDWn1b      = Srf_nom(Mb)%NsSDWn1b
      Srf_Perfo(Nb)%NsSDWn1c      = Srf_nom(Mb)%NsSDWn1c
      Srf_Perfo(Nb)%SDWnMax1a     = Srf_nom(Mb)%SDWnMax1a
      Srf_Perfo(Nb)%SDWnMax1b     = Srf_nom(Mb)%SDWnMax1b
      Srf_Perfo(Nb)%SDWnMax1c     = Srf_nom(Mb)%SDWnMax1c
      Srf_Perfo(Nb)%dWn1a         = Srf_nom(Mb)%dWn1a
      Srf_Perfo(Nb)%dWn1b         = Srf_nom(Mb)%dWn1b
      Srf_Perfo(Nb)%dWn1c         = Srf_nom(Mb)%dWn1c
      Srf_Perfo(Nb)%Opd_effective = Srf_nom(Mb)%Opd_effective
      Srf_Perfo(Nb)%SigI          = Srf_nom(Mb)%SigI
      call alloc_Srf_Perfo( Srf_Perfo(Nb) )
      Srf_Perfo(Nb)%Wn0           = Srf_nom(Mb)%Wn0
      Srf_Perfo(Nb)%SDWn1a        = Srf_nom(Mb)%SDWn1a
      Srf_Perfo(Nb)%SDWn1b        = Srf_nom(Mb)%SDWn1b
      Srf_Perfo(Nb)%SDWn1c        = Srf_nom(Mb)%SDWn1c
      do PN = 1, Srf_nom(Mb)%NsPixel+1
         do NF = 1, Srf_nom(Mb)%NsWn0
!
!           Level L1a
            Srf_Perfo(Nb)%ErShift1a(NF,PN) = ( Srf_per(Nb)%WnShift1a(NF,PN)  &
                                              -Srf_nom(Mb)%WnShift1a(NF,PN) )&
                                             / Srf_per(Nb)%Wn0(NF)
            Srf_Perfo(Nb)%ErFWhm1a(NF,PN)  = ( Srf_per(Nb)%FWhm1a(NF,PN)  &
                                              -Srf_nom(Mb)%FWhm1a(NF,PN) )&
                                             / Srf_nom(Mb)%FWhm1a(NF,PN)
            call shapeindex( Srf_Perfo(Nb)%NsSDWn1a,        &
                             Srf_nom(Mb)%L1a(1,NF,PN),      &
                             Srf_per(Nb)%L1a(1,NF,PN),      &
                             Srf_nom(Mb)%dWn1a,             &
                             Srf_Perfo(Nb)%ShapeId1a(NF,PN) )
            Srf_Perfo(Nb)%ErShape1a(1:Srf_Perfo(Nb)%NsSDWn1a,NF,PN) =    &
                        ( Srf_per(Nb)%L1a(1:Srf_per(Nb)%NsSDWn1a,NF,PN)  &
                         -Srf_nom(Mb)%L1a(1:Srf_nom(Nb)%NsSDWn1a,NF,PN) )&
                        * Srf_Perfo(Nb)%dWn1a
!
!           Level L1b
            Srf_Perfo(Nb)%ErShift1b(NF,PN) = ( Srf_per(Nb)%WnShift1b(NF,PN)  &
                                              -Srf_nom(Mb)%WnShift1b(NF,PN) )&
                                             / Srf_per(Nb)%Wn0(NF)
            Srf_Perfo(Nb)%ErFWhm1b(NF,PN)  = ( Srf_per(Nb)%FWhm1b(NF,PN)  &
                                              -Srf_nom(Mb)%FWhm1b(NF,PN) )&
                                             / Srf_nom(Mb)%FWhm1b(NF,PN)
            call shapeindex( Srf_Perfo(Nb)%NsSDWn1b,        &
                             Srf_nom(Mb)%L1b(1,NF,PN),      &
                             Srf_per(Nb)%L1b(1,NF,PN),      &
                             Srf_nom(Mb)%dWn1b,             &
                             Srf_Perfo(Nb)%ShapeId1b(NF,PN) )
            Srf_Perfo(Nb)%ErShape1b(1:Srf_Perfo(Nb)%NsSDWn1b,NF,PN) =    &
                        ( Srf_per(Nb)%L1b(1:Srf_per(Nb)%NsSDWn1b,NF,PN)  &
                         -Srf_nom(Mb)%L1b(1:Srf_nom(Nb)%NsSDWn1b,NF,PN) )&
                        * Srf_Perfo(Nb)%dWn1b
!
!           Level L1c
            Srf_Perfo(Nb)%ErShift1c(NF,PN) = ( Srf_per(Nb)%WnShift1c(NF,PN)  &
                                              -Srf_nom(Mb)%WnShift1c(NF,PN) )&
                                             / Srf_per(Nb)%Wn0(NF)
            Srf_Perfo(Nb)%ErFWhm1c(NF,PN)  = ( Srf_per(Nb)%FWhm1c(NF,PN)  &
                                              -Srf_nom(Mb)%FWhm1c(NF,PN) )&
                                             / Srf_nom(Mb)%FWhm1c(NF,PN)
            call shapeindex( Srf_Perfo(Nb)%NsSDWn1c,        &
                             Srf_nom(Mb)%L1c(1,NF,PN),      &
                             Srf_per(Nb)%L1c(1,NF,PN),      &
                             Srf_nom(Mb)%dWn1c,             &
                             Srf_Perfo(Nb)%ShapeId1c(NF,PN) )
            Srf_Perfo(Nb)%ErShape1c(1:Srf_Perfo(Nb)%NsSDWn1c,NF,PN) =    &
                        ( Srf_per(Nb)%L1c(1:Srf_per(Nb)%NsSDWn1c,NF,PN)  &
                         -Srf_nom(Mb)%L1c(1:Srf_nom(Nb)%NsSDWn1c,NF,PN) )&
                        * Srf_Perfo(Nb)%dWn1c
         end do
      end do
      call dalloc_Srf( Srf_per(Nb) )
      if( Nb_nom == 1 ) then
         call dalloc_Srf( Srf_nom(Mb) )
      else
         call dalloc_Srf( Srf_nom(Mb) )
      end if
   end do
!
   return
999 write(0,*) 'perfo_srf Error'
   call exit(1)
  end subroutine perfo_srf
!
!

!!
!!
!> anal_srf_spectral -- Public
!!
!! * Purpose
!!
!!     SRF spectral performance analyse
!!     
!!
!! * Description
!!
!!     This performance estimation subroutine derives srf spectral performances thanks  
!!     the comparison between a nominal case and a perturbed one.
!!     First, for each spectral band, the srf sampling coherence is ckecked between the 
!!     nominal and the perturbed cases. Then, the shape index is computed.
!!
!! * Inputs 
!!
!!     - file_plot    : character / name of writing file with statistical results
!!     - NBand        : integer / spectral band number
!!     - Level        : character / flag for definition of analysed level
!!     - Srf_nom      : type_Srf / type for declaration and allocation of srf
!!     - Srf_per      : type_Srf / type for declaration and allocation of srf
!!
!! * Inputs/outputs
!!
!! * Outputs
!!
!! * References
!!

   subroutine anal_srf_spectral( file_plot, &
                                 Nband,     &
                                 Level,     &
                                 Srf_nom,   &
                                 Srf_per    )
   implicit none
     character(len=*)    ,intent(in)                         :: file_plot
     integer(kind=LONG)  ,intent(in)                         :: Nband
     character(len=*)    ,intent(in)                         :: Level
     type(type_Srf)      ,intent(in),dimension(Nband)        :: Srf_nom
     type(type_Srf)      ,intent(in),dimension(Nband)        :: Srf_per
     integer(kind=LONG)                                      :: iFile
     integer(kind=LONG)                                      :: iPos
     integer(kind=LONG)                                      :: PN
     integer(kind=LONG)                                      :: SB
     integer(kind=LONG)                                      :: NF
     real(kind=DOUBLE)                                       :: ShapeId
     integer(kind=LONG)                                      :: NsSDWn1a
     integer(kind=LONG)                                      :: Ns1a_n_0
     integer(kind=LONG)                                      :: Ns1a_p_0
     integer(kind=LONG)                                      :: NsSDWn1b
     integer(kind=LONG)                                      :: Ns1b_n_0
     integer(kind=LONG)                                      :: Ns1b_p_0
     integer(kind=LONG)                                      :: NsSDWn1c
     integer(kind=LONG)                                      :: Ns1c_n_0
     integer(kind=LONG)                                      :: Ns1c_p_0
!
     iFile = 100
     iPos = 1
     open(unit=iFile, file=file_plot, err=999)
     iPos = 2
     write(iFile,'(a,a)',err=999) '# Wn; Shift_nom; Shift_per; Resol_nom; ',&
                                  'Resol_per; ErShift; ErResol, ErShape'
     do SB = 1, Nband
!
!      Srf sampling coherence
       if( Srf_nom(SB)%NsSDWn1a /= Srf_per(SB)%NsSDWn1a ) then
         NsSDWn1a = min( Srf_nom(SB)%NsSDWn1a, Srf_per(SB)%NsSDWn1a )
         if( NsSDWn1a == Srf_nom(SB)%NsSDWn1a ) then
           Ns1a_n_0 = 1
           Ns1a_p_0 = int(Srf_per(SB)%NsSDWn1a/2)   &
                    - int(Srf_nom(SB)%NsSDWn1a/2) + 1
         else
           Ns1a_p_0 = 1
           Ns1a_n_0 = int(Srf_nom(SB)%NsSDWn1a/2)   &
                    - int(Srf_per(SB)%NsSDWn1a/2) + 1
         end if
       else
         NsSDWn1a = Srf_nom(SB)%NsSDWn1a
         Ns1a_n_0 = 1
         Ns1a_p_0 = 1
       end if
       if( Srf_nom(SB)%NsSDWn1b /= Srf_per(SB)%NsSDWn1b ) then
         NsSDWn1b = min( Srf_nom(SB)%NsSDWn1b, Srf_per(SB)%NsSDWn1b )
         if( NsSDWn1b == Srf_nom(SB)%NsSDWn1b ) then
           Ns1b_n_0 = 1
           Ns1b_p_0 = int(Srf_per(SB)%NsSDWn1b/2)   &
                    - int(Srf_nom(SB)%NsSDWn1b/2) + 1
         else
           Ns1b_p_0 = 1
           Ns1b_n_0 = int(Srf_nom(SB)%NsSDWn1b/2)   &
                    - int(Srf_per(SB)%NsSDWn1b/2) + 1
         end if
       else
         NsSDWn1b = Srf_nom(SB)%NsSDWn1b
         Ns1b_n_0 = 1
         Ns1b_p_0 = 1
       end if
       if( Srf_nom(SB)%NsSDWn1c /= Srf_per(SB)%NsSDWn1c ) then
         NsSDWn1c = min( Srf_nom(SB)%NsSDWn1c, Srf_per(SB)%NsSDWn1c )
         if( NsSDWn1c == Srf_nom(SB)%NsSDWn1c ) then
           Ns1c_n_0 = 1
           Ns1c_p_0 = int(Srf_per(SB)%NsSDWn1c/2)   &
                    - int(Srf_nom(SB)%NsSDWn1c/2) + 1
         else
           Ns1c_p_0 = 1
           Ns1c_n_0 = int(Srf_nom(SB)%NsSDWn1c/2)   &
                    - int(Srf_per(SB)%NsSDWn1c/2) + 1
         end if
       else
         NsSDWn1c = Srf_nom(SB)%NsSDWn1c
         Ns1c_n_0 = 1
         Ns1c_p_0 = 1
       end if
!       write(*,*) 'NsSDWn1a',NsSDWn1a
!       write(*,*) 'Ns1a_n_0',Ns1a_n_0
!       write(*,*) 'Ns1a_p_0',Ns1a_p_0
!       write(*,*) 'NsSDWn1b',NsSDWn1b
!       write(*,*) 'Ns1b_n_0',Ns1b_n_0
!       write(*,*) 'Ns1b_p_0',Ns1b_p_0
!       write(*,*) 'NsSDWn1c',NsSDWn1c
!       write(*,*) 'Ns1c_n_0',Ns1c_n_0
!       write(*,*) 'Ns1c_p_0',Ns1c_p_0
!
       PN = Srf_nom(SB)%NsPixel+1
       do NF = 1, Srf_nom(SB)%NsWn0
          if( Level == 'L1a' ) then
             call shapeindex( NsSDWn1a,                       &
                              Srf_nom(SB)%L1a(Ns1a_n_0,NF,PN),&
                              Srf_per(SB)%L1a(Ns1a_p_0,NF,PN),&
                              Srf_nom(SB)%dWn1a, ShapeId      )
             write(iFile,'(f10.2,4f10.6,3e12.4)',err=999)  &
                         Srf_nom(SB)%Wn0(NF)/100.,         &
                         Srf_nom(SB)%WnShift1a(NF,PN)/100.,&
                         Srf_per(SB)%WnShift1a(NF,PN)/100.,&
                         Srf_nom(SB)%FWhm1a(NF,PN)/100.,   &
                         Srf_per(SB)%FWhm1a(NF,PN)/100.,   &
                         ( Srf_per(SB)%WnShift1a(NF,PN)    &
                          -Srf_nom(SB)%WnShift1a(NF,PN))   &
                         /Srf_nom(SB)%Wn0(NF),             &
                         ( Srf_per(SB)%FWhm1a(NF,PN)       &
                          -Srf_nom(SB)%FWhm1a(NF,PN))      &
                         /Srf_nom(SB)%FWhm1a(NF,PN),       &
                         ShapeId
          else if( Level == 'L1b' ) then
             call shapeindex( NsSDWn1b,                       &
                              Srf_nom(SB)%L1b(Ns1b_n_0,NF,PN),&
                              Srf_per(SB)%L1b(Ns1b_p_0,NF,PN),&
                              Srf_nom(SB)%dWn1b, ShapeId      )
             write(iFile,'(f10.2,4f10.6,3e12.4)',err=999)  &
                         Srf_nom(SB)%Wn0(NF)/100.,         &
                         Srf_nom(SB)%WnShift1b(NF,PN)/100.,&
                         Srf_per(SB)%WnShift1b(NF,PN)/100.,&
                         Srf_nom(SB)%FWhm1b(NF,PN)/100.,   &
                         Srf_per(SB)%FWhm1b(NF,PN)/100.,   &
                         ( Srf_per(SB)%WnShift1b(NF,PN)    &
                          -Srf_nom(SB)%WnShift1b(NF,PN))   &
                         /Srf_nom(SB)%Wn0(NF),             &
                         ( Srf_per(SB)%FWhm1b(NF,PN)       &
                          -Srf_nom(SB)%FWhm1b(NF,PN))      &
                         /Srf_nom(SB)%FWhm1b(NF,PN),       &
                         ShapeId
          else if( Level == 'L1c' ) then
             call shapeindex( NsSDWn1c,                      &
                              Srf_nom(SB)%L1c(Ns1c_n_0,1,1), &
                              Srf_per(SB)%L1c(Ns1c_p_0,NF,1),&
                              Srf_nom(SB)%dWn1c, ShapeId     )
             write(iFile,'(f10.2,4f10.6,3e12.4)',err=999)  &
                         Srf_nom(SB)%Wn0(NF)/100.,         &
                         Srf_nom(SB)%WnShift1c(1,1)/100.,  &
                         Srf_per(SB)%WnShift1c(NF,1)/100., &
                         Srf_nom(SB)%FWhm1c(1,1)/100.,     &
                         Srf_per(SB)%FWhm1c(NF,1)/100.,    &
                         ( Srf_per(SB)%WnShift1c(NF,1)     &
                          -Srf_nom(SB)%WnShift1c(1,1))     &
                         /Srf_nom(SB)%Wn0(NF),             &
                         ( Srf_per(SB)%FWhm1c(NF,1)        &
                          -Srf_nom(SB)%FWhm1c(1,1))        &
                         /Srf_nom(SB)%FWhm1c(1,1),         &
                         ShapeId
          else if( Level == 'L1d' ) then
             call shapeindex( NsSDWn1c,                      &
                              Srf_nom(SB)%L1c(Ns1c_n_0,1,1), &
                              Srf_per(SB)%L1c(Ns1c_p_0,NF,1),&
                              Srf_nom(SB)%dWn1c, ShapeId     )
             write(iFile,'(f10.2,4f10.6,3e12.4)',err=999)  &
                         Srf_nom(SB)%Wn0(NF)/100.,         &
                         Srf_nom(SB)%WnShift1c(1,1)/100.,  &
                         Srf_per(SB)%WnShift1c(NF,1)/100., &
                         Srf_nom(SB)%FWhm1c(1,1)/100.,     &
                         Srf_per(SB)%FWhm1c(NF,1)/100.,    &
                         ( Srf_per(SB)%WnShift1c(NF,1)     &
                          -Srf_nom(SB)%WnShift1c(1,1))     &
                         /Srf_nom(SB)%Wn0(NF),             &
                         ( Srf_per(SB)%FWhm1c(NF,1)        &
                          -Srf_nom(SB)%FWhm1c(1,1))        &
                         /Srf_nom(SB)%FWhm1c(1,1),         &
                         ShapeId
          else
             write(*,*) ' Level Error',Level
             write(*,*) 'anal_srf_spectral Fatal Error'
             go to 999
          end if
       end do
     end do
     close(unit=iFile)
!
     return
 999 write(*,*) 'writing results error',iPos
     write(*,*) 'anal_srf_spectral Fatal Error'
     call exit(1)
   end subroutine anal_srf_spectral

!!
!!
!> shapeindex -- Public
!!
!! * Purpose
!!
!!     Shape index computation
!!
!! * Description
!!
!!     This subroutine computes the shape index between a reference spectral response 
!!     function and an analysed one.
!!
!! * Inputs 
!!
!!     - ndim  : dimension 
!!     - y_ref : reference spectral response function
!!     - y     : analysed spectral response function
!!     - dx    : wavenumber sampling
!!
!! * Inputs/outputs
!!
!!     - ShapeId : shape index
!!
!! * Outputs
!!
!! * References
!!

   subroutine shapeindex( ndim, y_ref, y, dx, ShapeId )
   implicit none
     integer(kind=LONG),intent(in)                  :: ndim
     real(kind=DOUBLE) ,intent(in) ,dimension(ndim) :: y_ref
     real(kind=DOUBLE) ,intent(in) ,dimension(ndim) :: y
     real(kind=DOUBLE) ,intent(in)                  :: dx
     real(kind=DOUBLE) ,intent(inout)               :: ShapeId
!
!    shape index 
     ShapeId = sum( dabs( y(1:ndim) - y_ref(1:ndim) ) ) * dx
!
     return
   end subroutine shapeindex
!
!
end module srf_performance_module
