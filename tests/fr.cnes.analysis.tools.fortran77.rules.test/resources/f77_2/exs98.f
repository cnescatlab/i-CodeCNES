c
c     Numerical Analysis:
c     The Mathematics of Scientific Computing
c     D.R. Kincaid & E.W. Cheney
c     Brooks/Cole Publ., 1990
c
c     Section 9.8
c
c     Example of Damping of Errors
c
c
c     file: exs98.f
c
      parameter (n=63)
      dimension v(0:n+1),rho(100)
      integer p(4)
      data (p(i),i=1,4) /1,4,7,16/
c
      print *
      print *,' Damping of Errors'
      print *,' Section 9.8, Kincaid-Cheney' 
      print *
      print 6
c
      k = 100 
      do 5 l=1,4
         do 2 j=0,n+1
            v(j) = sin((j*p(l)*22.0/7.0)/(n+1)) 
 2       continue
         do 5 i=1,k
	    do 3 j=1,n
	       v(j) = (v(j-1) + v(j+1))/2.0
 3          continue
            rho(i) = abs(v(1))
            do 4 j=2,n
               if (rho(i) .ge. abs(v(j))) goto 4
               rho(i) = abs(v(j))
 4          continue
            print 7,p(l),i,rho(i)
 5    continue
c
 6    format (2x,'p',5x,'i',8x,'norm of v')
 7    format (1x,i2,3x,i3,6x,e13.6)
      stop
      end
