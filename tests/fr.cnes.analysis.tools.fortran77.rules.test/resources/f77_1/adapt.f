c
c     Numerical Analysis:
c     The Mathematics of Scientific Computing
c     D.R. Kincaid & E.W. Cheney
c     Brooks/Cole Publ., 1990
c
c     Section 7.5
c
c     adaptive quadrature by simpson rule
c
c
c     file: adapt.f 
c
      parameter (n = 5)
      dimension v(6,n)  
      logical iprt
      data a,b,epsi/0.0,1.0,5.0e-5/
      data iprt/.true./
      f(x) = sqrt(x)
c
      print *
      print *,' Adaptive Quadrature'
      print *,' Section 7.5, Kincaid-Cheney'
      print *
      print 4,'k','v(1,n)','v(2,n)','v(3,n)','v(4,n)','v(5,n)','v(6,n)'

c
c     initialize everything, 
c     particularly the first column vector in the stack.
c
      delta = b-a
      sigma = 0.0
      h = delta/2.0
      c = 0.5*(a + b)
      k = 1
      abar = f(a)
      bbar = f(b)
      cbar = f(c)
      S = (abar + 4.0*cbar + bbar)*h/3.0
      v(1,1) = a
      v(2,1) = h
      v(3,1) = abar
      v(4,1) = cbar
      v(5,1) = bbar
      v(6,1) = S
      if (iprt) print 5,k,(v(i,1),i=1,6)
c
 2    continue
      if ( (k .lt. 1) .or. (k .gt. n) ) go to 3
c     
c     take the last column off the stack and process it.
c
      h = 0.5*v(2,k)
      y = v(1,k) + h
      ybar = f(y)
      Star = ( v(3,k) + 4.0*ybar + v(4,k) )*h/3.0
      z = v(1,k) + 3.0*h
      zbar = f(z)
      SStar = ( v(4,k) + 4.0*zbar + v(5,k) )*h/3.0
c
      tmp = Star + SStar - v(6,k)
      if ( abs(tmp) .le. 30.*epsi*h/delta ) then

c     if the tolerance is being met, add partial integral
c     and take a new vector from the bottom of the stack.
c
      sigma = sigma + Star + SStar + tmp/15.0
      if (iprt) print *,' k=',k,' sigma=',sigma
      k = k-1
c
      if (k .le. 0) then 
         print *,' value of integral is  ',sigma
         stop
      endif
      else
         if (k .ge. n) then 

c     if the stack has reached it maximum number of columns (n),
c     then stop and report a failure.
c
            print *,' method fails'
            stop 
         endif
c
c     if the tolerance is not being met, subdivide the interval
c     and create two new vectors in the stack, 
c     one of which  overwrites the vector just processed.

         if (iprt) print *,' split'
         vbar = v(5,k)
         v(2,k) = h
         v(5,k) = v(4,k)
         v(4,k) = ybar
         v(6,k) = Star
         if (iprt) print 5,k,(v(i,k),i=1,6)
c
         k = k+1
         v(1,k) = v(1,k-1) + 2.0*h
         v(2,k) = h
         v(3,k) = v(5,k-1)
         v(4,k) = zbar
         v(5,k) = vbar
         v(6,k) = SStar
         if (iprt) print 5,k,(v(i,k),i=1,6)
      endif
c
c     having created two new vectors in the stack, 
c     pick off the last one and start the proces anew.
c
      go to 2
c
 3    continue
c
 4    format (a2,a12,a14,a14,a14,a14,a14)
 5    format (i2,1x,6(e13.6,1x))
      stop
      end
