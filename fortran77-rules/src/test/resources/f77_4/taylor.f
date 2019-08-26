c
c     Numerical Analysis:
c     The Mathematics of Scientific Computing
c     D.R. Kincaid & E.W. Cheney
c     Brooks/Cole Publ., 1990
c
c     Section 8.2
c
c     Solving the initial value problem using Taylor Series
c
c
c     file: taylor.f
c
      data M,h,t,x/200,0.01,-1.0,3.0/
c
      print *
      print *,' Taylor series method (order 4) '
      print *,' Section 8.1, Kincaid-Cheney'
      print *
      print 3,'k','t','x'
      print 4,0,t,x
c
      do 2 k=1,M
         x1 = cos(t) - sin(x) + t**2.0 
         x2 = -sin(t) - x1*cos(x) + 2.0*t       
         x3 = -cos(t) - x2*cos(x) + (x1**2.0)*sin(x) + 2.0       
         x4 = sin(t) + ((x3)**3.0 -x3)*cos(x) + 3.0*x1*x2*sin(x) 
         x = x + h*(x1 + (h/2.)*(x2 + (h/6.)*(x3 + (h/24.)*x4)))
         t = t + h       
         print 4,k,t,x 
 2    continue
c
 3    format(a6,a9,a15)
 4    format(1x,i5,2x,e13.6,2x,e13.6)      
      stop
      end 
