c
c     Numerical Analysis:
c     The Mathematics of Scientific Computing
c     D.R. Kincaid & E.W. Cheney
c     Brooks/Cole Publ., 1990
c
c     Section 8.3
c
c     Runge-Kutta-Fehlberg method 
c     for solving an initial value problem   
c
c
c     file: rkfelbergg.f
c
      external f
      dimension a(6),e(6),c(6),d(6,6),p(6)
      data M/500/, t/1.0/, x/2.0/, h/0.1/, delta/5.0e-5/, b/3.0/
c
      data (a(i),i=1,6)/
     + .118518518518518518518518519, 0.,  .518986354775828460038986355,
     + .50613149034201665780613149, -.18, .0363636363636363636363636364/
      data (c(i),i=1,6)/
     + 0., .25, .375, .923076923076923076923076923, 1., .5/
      data (e(i),i=1,6)/
     + .00277777777777777777777777778,0.,-.0299415204678362573099415205,
     + -.0291998936735778841041998937,.02,.0363636363636363636363636364/
      data ((d(i,j),j=1,6),i=1,6)/
     + 0., 0., 0., 0., 0., 0.,
     + .25, 0., 0., 0., 0., 0., 
     + .09375, 0., .28125, 0., 0., 0.,
     + .879380974055530268548020027,0.,-3.27719617660446062812926718,
     + 3.32089212562585343650432408, 0., 0.,
     + 2.03240740740740740740740741,0.,-8.,7.17348927875243664717348928,
     + -.205896686159844054580896686, .45297270955165692007797271,
     + -.296296296296296296296296296,0.,2.,
     + -1.38167641325536062378167641, 0., -.275/
c
      print *
      print *,' Runge-Kutta-Fehlberg method'
      print *,' Section 8.3, Kincaid-Cheney'
      print *
      print 7,'k','t','x','e','h' 
      iflag = 1 
c
      do 6 k=1,M
         xd = b-t
         if (abs(xd) .le. h) then
            iflag = 0
            h = xd
         endif
         y = x
         s = t
c
         do 3 i=1,6
            sum = 0.0
            do 2 j=1,i-1
               sum = sum + d(i,j)*p(j)
 2          continue
            p(i) = h*f(t+(c(i)*h),x+sum)
 3       continue
c
         sum = 0.0
         do 4 i=1,6
            sum = sum + a(i)*p(i)
 4       continue
         x = x + sum
c 
         est = 0.0
         do 5 i=1,6
            est = est + e(i)*p(i)
 5       continue
c
         t = t + h
         print 8,k,t,y,est,h 
c
         if (iflag .eq. 0) then
            stop
         endif
c
         if (abs(est) .ge. delta) then
            print *,' half h'
            h = h/2.0
            t = s
            x = y
            k = k-1
         else 
            if (abs(est) .lt. delta/128.0) then 
               print *,' double h'
               h = 2*h
            endif   
         endif
 6    continue
c
 7    format(a6,a9,2a15,a15)
 8    format(1x,i5,2x,4(e13.6,2x))
      stop
      end 
c  
      function f(t,x) 
      f = (t*x - x**2.0)/t**2.0
      return
      end 
