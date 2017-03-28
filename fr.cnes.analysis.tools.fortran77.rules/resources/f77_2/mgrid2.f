c
c     Numerical Analysis:
c     The Mathematics of Scientific Computing
c     D.R. Kincaid & E.W. Cheney
c     Brooks/Cole Publ., 1990
c
c     Section 9.8
c
c     complete V-cycle in the multigrid algorithm to solve
c     uxx = g(x), with u(0)=u(l)=0.
c     parameter  m  gives the number of grids to be employed. 
c     parameter  k  gives the number of iterations to be performed 
c                   in each gauss-seidel iteration. 
c     parameter  np1 is simply n+1
c
c
c     file: mgrid2.f
c
      parameter (m=7, np1=128)
      dimension v(m,0:np1), f(m,0:np1), w(0:np1), error(15)
      integer p
      real g,true
      g(x) = cos(x)
      true(x) = -g(x) + x*(g(1.0) - 1.0) + 1.0
c
      print *   
      print *,' V-cycle in the multigrid method'
      print *,' Section 9.8, Kincaid-Cheney'
      print *
c
      error(1) = 1.0
      do 12 k=2,15
c
c     initialize arrays and
c     put best available guess into v**m
c
         n = 2**m - 1
         h = 1.0/real(n+1)
         print *,' n =',n,' h =',h
c
         do 1 i=1,m
            do 1 j=0,np1
               f(i,j) = 0.0
               v(i,j) = 0.0
 1       continue
c
c        put data from the bvp into f**m
c
         do 2 j=1,n
            f(m,j) = g(real(j)*h) 
 2       continue
c
         do 6 i=m,2,-1 
c
c        gauss-seidel iteration (k steps)
c
            do 3 p=1,k
               do 3 j=1,n
                  v(i,j) = 0.5*(v(i,j-1) + v(i,j+1) - (h**2)*f(i,j)) 
 3          continue
c
c           residual vector
c
            do 4 j=1,n
               w(j) = f(i,j) - (v(i,j-1) - 2.0*v(i,j) + v(i,j+1))/h**2 
 4          continue
c
c           apply restriction operator
c
            do 5 j=1,(n-1)/2
               f(i-1,j) = w(2*j)
 5          continue
c
            h = 2.0*h
            n = (n-1)/2
            print *,' n =',n,' h =',h
 6       continue
c
c        solve coarsest (smallest) system exactly
c
         v(1,1) = -g(0.5)/8.0
         print *,' Bottom of V-cycle'
c
c        end downward phase of V-cycle
c        start upward phase of V-cycle
c
         do 10 i=2,m
            h = 0.5*h
            n = 2*n + 1
            print *,' n =',n,' h =',h
c
c           apply extension operation, i.e.
c           interpolation up from coarse grid to fine grid
c
            do 7 j=0,(n+1)/2
               w(2*j) = v(i-1,j)
 7          continue
c
            do 8 j=1,(n+1)/2
               w(2*j-1) = 0.5*(v(i-1,j-1) + v(i-1,j))
 8          continue
c
c           add correction to extension operator
c
            do 9 j=0,n+1
               v(i,j) = v(i,j) + w(j)
 9          continue
c
c           gauss-seidel iteration (k steps)
c
            do 10 p=1,k
               do 10 j=1,n
                  v(i,j) = 0.5*(v(i,j-1) + v(i,j+1) - (h**2)*f(i,j)) 
 10      continue
c
c        output phase
c
         print *,' k =',k,' m =',m
         do 11 j=0,np1
            w(j) = true(real(j)*h) - v(m,j)
 11      continue
         error(k) = vnorm(np1,w)
         print *,' maximun error =', error(k),
     +           ' ratio =', error(k)/error(k-1)
c
 12   continue
c
      stop
      end 
c
      function vnorm(n,v)
c
c     infinity norm of vectorv v
c
      dimension v(0:n)
      real max
c
      temp = 0.0
      do 2 i=0,n
      temp = max(temp,abs(v(i)))
 2    continue
      vnorm = temp
c
      return
      end
