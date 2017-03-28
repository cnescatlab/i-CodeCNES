c
c     Numerical Analysis:
c     The Mathematics of Scientific Computing
c     D.R. Kincaid & E.W. Cheney
c     Brooks/Cole Publ., 1990
c
c     Section 2.1
c
c     Computing an approximate value of 
c     double precision machine precision
c
c
c     file: depsi.f
c
      double precision s,t
c
      print *
      print *,' Approximate value of double precision machine epsilon'
      print *,' Section 2.1, Kincaid-Cheney'
      print *
      print *,' n          computed               2**(-n)'
c      
      s = 1.0
c
      do 2 k=1,100
         s = 0.5d0*s
	 t = s + 1.0d0
	 if (t .le. 1.0d0) then
	    s = 2.0d0*s
	    t = 1.0d0/2.0d0**(k-1)
	    print 3,k-1,s,t
            stop
         endif
 2    continue
c
 3    format (i3,2(2x,d22.15)) 
      stop
      end
