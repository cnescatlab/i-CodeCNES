c
c     Numerical Analysis:
c     The Mathematics of Scientific Computing
c     D.R. Kincaid & E.W. Cheney
c     Brooks/Cole Publ., 1990
c
c     Section 1.2
c
c     Example of rapidly convergent sequence
c
c
c     file: sqrt2.f
c
      double precision x
c
      print *
      print *,' Rapidly converging sequence'
      print *,' Section 1.2, Kincaid-Cheney'
      print *
c
      x = 2.0d0
      print *,'k           x'
      print *,1,x
c
      do 2 k=2,4
         x = x/2.0d0 + 1.0d0/x
         print *,k,x
 2    continue
c
      print *,' ',sqrt(2.0d0),'= sqrt(2)'
c
      stop
      end
