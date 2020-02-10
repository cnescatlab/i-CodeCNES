      subroutine mpi_IO_e_us76 (init,zgeo,deltat,
     &                      dro,temp,pres,ro,vson,xmu)
c
c
c Version MSPRO 5.2 (FA 1 mp_atm_us76d) : ajout SAVE pour tmtab
c
c 
c
c     Test si zgeo > 1000 km:
c     ======================
c
      if (zgeo.gt.1000.d+03) then
c
c        write(6,1000) 
c 1000   format('Altitude superieure a 1000 km ...',/,
c    &'  Calcul des conditions atmospheriques impossible !',/)
c        stop

          if (init.ne.0) then
            do 50 i=0,7
               tmtab(i)=tmtab0(i)+deltat(i)
 50         continue
            do 60 i=0,6
               dttab(i)=(tmtab(i+1)-tmtab(i))/(htab(i+1)-htab(i))
               tmol=tmtab(i+1)
               hpot=htab(i+1)
               if (dabs(dttab(i)).lt.1.d-06) then
                  ptab(i+1)=ptab(i)*
     &                   dexp(-g0*xmol0*(hpot-htab(i))/(rstar*tmtab(i)))
               else
                  ptab(i+1)=ptab(i)*(tmtab(i)/tmol)**
     &                      (g0*xmol0/(rstar*dttab(i)))
               end if
 60         continue
            dttab(9)=12.d-03
c           Calcul des "ni" pour chaque element N2,O,O2,Ar,He et H:
            xmol=0.d0
            rn=0.d0
            do 70 l=2,7
               rnn=rntab(1,l)
               xmol=xmol+rnn*rmoltab(l)
               rn=rn+rnn
 70         continue
            xmol=xmol/rn
            temp=tmtab(7)*xmol/xmol0
c           Calcul de la pression:
            pres=rn*boltz*temp
            cfp=ptab(7)/pres
         end if
c
      end if

      end
