c
c     Numerical Analysis:
c     The Mathematics of Scientific Computing
c     D.R. Kincaid & E.W. Cheney
c     Brooks/Cole Publ., 1990
c
c     Section 7.1
c
c     Computing the derivative of a function
c     using forward difference formula
c
c
c     file: ex1s71.f
c
      parameter (M=26)
      f(x) = atan(x)
c
      print *
      print *,' Derivative approximations: forward difference formula'
      print *,' Section 7.1, Kincaid-Cheney'
      print *
      print 3,'k','h','F2','F1','d','r'
c
      h = 1.0
      a = sqrt(2.0)
      f1 = f(a)
c
      do 2 k=0,M
         F2 = f(a+h)
         d = F2 - F1
         r = d/h
         print 4,k,h,F2,F1,d,r
         h = 0.5*h
 2    continue
c
 3    format(a4,a9,a16,a15,a14,a15)
 4    format(1x,i3,5(2x,e13.6))
      stop
      end
