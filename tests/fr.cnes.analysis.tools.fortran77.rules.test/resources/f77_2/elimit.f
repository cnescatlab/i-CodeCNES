c
c     Numerical Analysis:
c     The Mathematics of Scientific Computing
c     D.R. Kincaid & E.W. Cheney
c     Brooks/Cole Publ., 1990
c
c     Section 1.2
c
c     Example of slowly converging sequence for irrational number e
c
c
c     file: elimit.f
c
      double precision x
      dimension x(1000)
c
      print *
      print *,' Slowly converging sequence for irrational number e'
      print *,' Section 1.2, Kincaid-Cheney'
      print *
c
      do 2 n=1,1000
         x(n) = (1.0d0 + 1.0d0/real(n))**n
 2    continue   
c
      print *,'    1,    x(1) =',x(1)
      print *,'   10,   x(10) =',x(10)
      print *,'   30,   x(30) =',x(30)
      print *,'   50,   x(50) =',x(50)
      print *,' 1000, x(1000) =',x(1000)
      print *,'      exp(1.0) =',exp(1.0d0)
      print *
      print *,'         error =',abs(x(1000) - exp(1.0d0))
c
      stop
      end
