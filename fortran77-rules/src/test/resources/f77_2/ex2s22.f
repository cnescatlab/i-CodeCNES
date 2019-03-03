c
c     Numerical Analysis:
c     The Mathematics of Scientific Computing
c     D.R. Kincaid & E.W. Cheney
c     Brooks/Cole Publ., 1990
c
c     Section 2.2
c
c     Example 2 
c     loss of significance in y = x - sin(x)
c
c
c     file: ex2s22.f
c
      double precision t,s,yd
c
      print *
      print *,' Using double precision to avoid loss of significance'
      print *,' Section 2.2, Kincaid-Cheney'
      print *
c
      M = 7
      x = 1.9
      y = x - sin(x)
      yd = dble(x) - sin(dble(x))
      p = (x**3/6.)*(1. - (x*x/20.)*(1. - (x*x/42.)*(1. - (x*x/72.))))
      print *,' x =',x,'  p =',p
      print *,' y =',y,'  yd =',yd
      print *
c
      t = x**3/6.0
      s = t
      error = abs((s - yd)/yd)
      print *,'  n            s(n)                    rel. error'
      print 3,1,s,error
c
      do 2 n=1,M
         t = -t*x*x/real((2.*real(n)+2.)*(2.*real(n)+3.))
         s = s + t
         error = abs((s - yd)/yd)
         print 3,n+1,s,error
 2    continue
c
 3    format(i3,4x,2(d22.15,4x))
      stop
      end
