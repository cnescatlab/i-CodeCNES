c
c     Numerical Analysis:
c     The Mathematics of Scientific Computing
c     D.R. Kincaid & E.W. Cheney
c     Brooks/Cole Publ., 1990
c
c     Section 4.2
c
c     Example of general LU-factorization
c
c
c     file: genlu.f
c
      parameter (n=3)
      dimension a(n,n),l(n,n),u(n,n)
      real l
      logical t(n)
      data (a(1,j),j=1,n) / 60.0,30.0,20.0/
      data (a(2,j),j=1,n) / 30.0,20.0,15.0/
      data (a(3,j),j=1,n) / 20.0,15.0,12.0/
      data l(1,1),u(2,2),l(3,3) /6.0,2.0,4.0/
      data (t(i),i=1,n) /.true.,.false.,.true./
c
      print *
      print *,' General LU-factorization example'
      print *,' Section 4.2, Kincaid-Cheney'
      print *
c
      do 3 k=1,n
         sum = a(k,k)
         do 2 m=1,k-1
            sum = sum - l(k,m)*u(m,k)
 2       continue
         if (t(k)) then
            u(k,k) = sum/l(k,k)
         else
            l(k,k) = sum/u(k,k)
         endif
 3    continue
c
      do 9 k=1,n
         sum1 = a(k,k)
         do 4 m=1,k-1
            sum1 = sum1 - l(k,m)*u(m,k)
 4       continue
            u(k,k) = sum1/l(k,k)
         do 6 j=k+1,n
            sum2 = a(k,j)
            do 5 m=1,k-1
               sum2 = sum2 - l(k,m)*u(m,j)
 5          continue
            u(k,j) = sum2/l(k,k)
 6       continue
         do 8 i=k+1,n
            sum3 = a(i,k)
            do 7 m=1,k-1
               sum3 = sum3 - l(i,m)*u(m,k)
 7          continue
            l(i,k) = sum3/u(k,k)
 8        continue
 9    continue
c
      do 11 i=1,n
         do 10 j=1,n     
            print 12,i,j,l(i,j),i,j,u(i,j)
 10      continue
 11   continue
c
 12   format (1x,'l(',i2,',',i2,') =',e13.6,7x,
     +           'u(',i2,',',i2,') =',e13.6)
      stop
      end
