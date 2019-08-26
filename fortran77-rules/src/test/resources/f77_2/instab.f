c
c     Numerical Analysis:
c     The Mathematics of Scientific Computing
c     D.R. Kincaid & E.W. Cheney
c     Brooks/Cole Publ., 1990
c
c     Section 2.3
c
c     Example of numerical instability
c
c
c     file: instab.f
c
      print *
      print *,' Numerical instability'
      print *,' Section 2.3, Kincaid-Cheney'
      print *
      print *,'  n        y(n)'
c
      e = exp(1.0)
      y = 1.0
      print 3,1,y
c
      do 2 n=2,20
         y = e - y*real(n)
         print 3,n,y
 2    continue
c
 3    format (i3,2x,e13.6)
      stop
      end
