c
c     Numerical Analysis:
c     The Mathematics of Scientific Computing
c     D.R. Kincaid & E.W. Cheney
c     Brooks/Cole Publ., 1990
c
c     Section 7.3
c
c     Example of 5-point Gaussian integration formula
c     to integrate sqrt(x) over [0,1]
c
c
c     file: gauss5.f
c
      dimension w(0:3),x(0:3) 
      real a,b,c,d
      double precision x,w,S,f,sqrt,tmp,tmn
      data x(0) /0.0d0/
      data x(1) /0.53846 93101 05683d0/
      data x(2) /0.90617 98459 38664d0/
      data w(0) /0.56888 88888 88889d0/
      data w(1) /0.47862 86704 99366d0/
      data w(2) /0.23692 68850 56189d0/
      data a,b,c,d/0.,1.,-1.,1./
c
      f(x) = sqrt(x)
c      
      print *
      print *,' 5-point Gaussian integration example'
      print *,' Section 7.3, Kincaid-Cheney'
      print *
c
      S = 0.0d0
      do 2 i=0,3
         tmp = ( (b - a)*x(i) + a*d - b*c)/(d - c)
         tmn = (-(b - a)*x(i) + a*d - b*c)/(d - c)
         S = S + w(i)*(sqrt(tmp) + sqrt(tmn))
 2    continue
      S = (b - a)*S/(d - c)
      print *,' S = ',S
c
      stop
      end
