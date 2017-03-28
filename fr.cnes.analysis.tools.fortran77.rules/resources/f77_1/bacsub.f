c
c     Numerical Analysis:
c     The Mathematics of Scientific Computing
c     D.R. Kincaid & E.W. Cheney
c     Brooks/Cole Publ., 1990
c
c     Section 4.2
c
c     Example of Backward Substitution
c
c
c     file: bacsub.f
c
      parameter (n=3)
      dimension a(n,n),x(n),b(n)
      data (a(1,j),j=1,n) /4.0,2.0,1.0/
      data (a(2,j),j=1,n) /0.0,3.0,2.0/
      data (a(3,j),j=1,n) /0.0,0.0,6.0/
      data (b(i),i=1,n) /7.0,5.0,6.0/
c
      print *
      print *,' Backward Substitution example'
      print *,' Section 4.2, Kincaid-Cheney'
      print *
c
      do 3 i=n,1,-1
         sum = b(i)
         do 2 j=i+1,n
            sum = sum - a(i,j)*x(j)
 2       continue
         x(i) = sum/a(i,i)
 3    continue
c
      do 4 i=1,n
         print 5,i,x(i)
 4    continue
c
 5    format (1x,'x(',i2,') =',2x,e13.6)
      stop
      end
