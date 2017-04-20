c
c     Numerical Analysis:
c     The Mathematics of Scientific Computing
c     D.R. Kincaid & E.W. Cheney
c     Brooks/Cole Publ., 1990
c
c     Section 1.2
c
c     Example of Nested Multiplication
c
c
c     file: nest.f
c
      dimension a(0:4)
      data (a(i),i=0,4) /-2.0,-5.0,7.0,-4.0,1.0/
      data n,x /4,3.0/
c
      print *
      print *,' Nested Multiplication example' 
      print *,' Section 1.2, Kincaid-Cheney'
      print *
c
      p = a(n)
      do 2 k=n-1,0,-1
         p = x*p + a(k)
 2    continue
c
      print *,' The value of the polynomial at x=3 is ',p
c
      stop
      end
