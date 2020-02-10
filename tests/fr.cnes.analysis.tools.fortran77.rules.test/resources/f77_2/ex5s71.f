c
c     Numerical Analysis:
c     The Mathematics of Scientific Computing
c     D.R. Kincaid & E.W. Cheney
c     Brooks/Cole Publ., 1990
c
c     Section 7.1 
c
c     Computing the derivative using Richardson extrapolation
c
c
c     file: ex5s71.f
c
      parameter (M=30)
      dimension d(0:M),r(0:M)
      f(x) = atan(x)
c
      print *
      print *,' Derivative approximation: Richardson extrapolation'
      print *,' Section 7.1, Kincaid-Cheney'
      print *
      print 5,'k','d(k)','r(k)'
c
      a = sqrt(2.0)
      h = 1.0
c
      do 2 k=1,M
         h = 0.5*h
         d(k) = (f(a+h) - f(a-h))/(2.0*h)
 2    continue
c
      do 3 k=1,M
         r(k) = d(k) + (d(k) - d(k-1))/3.0
 3    continue
c
      do 4 k=1,M
         print 6,k,d(k),r(k)
 4    continue
c
 5    format(a6,a12,a14)
 6    format(1x,i5,2x,2(e13.6,2x))
      stop
      end
