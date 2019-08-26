c
c     Numerical Analysis:
c     The Mathematics of Scientific Computing
c     D.R. Kincaid & E.W. Cheney
c     Brooks/Cole Publ., 1990
c
c     Section 3.2
c
c     Example of Newton's Method
c
c
c     file: ex1s32.f
c
      double precision  x,x0,x1,f,fx,fp,epsi,delta
      data  x0/-7.0d0/, M/8/, epsi/1.0e-12/, delta/1.0e-12/
      f(x) = exp(x) - 1.5d0 - atan(x)
      fp(x) = exp(x) - 1.0d0/(1.0d0 + x*x)
c
      print *
      print *,' Newton method example'
      print *,' Section 3.2, Kincaid-Cheney'
      print *
c
      fx = f(x0)
      print 3
      print 4,0,x0,fx	
      if (abs(fx) .lt. epsi) stop
      do 2 k = 1,M
         x1 = x0 - fx/fp(x0)
         fx = f(x1)
         print 4,k,x1,fx 
         if((abs(x1 - x0) .lt. delta) .or. (abs(fx) .lt. epsi)) stop
         x0 = x1
 2    continue   
c
 3    format(3x,'k',12x,'x',21x,'f(x)')
 4    format(1x,i3,2x,d22.15,2x,d22.15)   
      stop
      end 
