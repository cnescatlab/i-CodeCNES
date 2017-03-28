PROGRAM ESSAI

	character(len=256), private :: SVN_VER =  '$Id: cps_acces.F90 69 2012-09-11 08:33:34Z ffsm $'

	b = (b + c) - ((d * e) * f) ** (g + h)
	c = .NOT. a .EQ. b .LT. c .GE.d
	a = x(b) + y(b) * x(b)
	
	xv(i+1)= (xv(i+1) + xv(i)) - real(int(xv(i+1)+xv(i)),kind=pm_reel)
	
	data (x(i),i=1,78)/-4.5_PM_REEL,-4._PM_REEL,-3.8_PM_REEL,-3.6_PM_REEL,-3.5_PM_REEL,-3.4_PM_REEL,-3.3_PM_REEL, &
         -3.2_PM_REEL,-3.1_PM_REEL,-3._PM_REEL, -2.9_PM_REEL,-2.8_PM_REEL,-2.7_PM_REEL,-2.6_PM_REEL,-2.5_PM_REEL, &
         -2.4_PM_REEL,-2.3_PM_REEL,-2.2_PM_REEL,-2.1_PM_REEL,-2.0_PM_REEL, -1.9_PM_REEL,-1.8_PM_REEL,-1.7_PM_REEL,&
         -1.6_PM_REEL,-1.5_PM_REEL,-1.4_PM_REEL,-1.3_PM_REEL,-1.2_PM_REEL,-1.1_PM_REEL,-1.0_PM_REEL, &
         -0.9_PM_REEL,-0.8_PM_REEL,-0.7_PM_REEL,-0.6_PM_REEL,-0.5_PM_REEL,-0.4_PM_REEL,-0.3_PM_REEL,-0.2_PM_REEL,&
         -0.1_PM_REEL,0.0_PM_REEL, 0.1_PM_REEL,0.2_PM_REEL,0.3_PM_REEL,0.4_PM_REEL,0.5_PM_REEL,0.6_PM_REEL,&
         0.7_PM_REEL,0.8_PM_REEL,0.9_PM_REEL,1._PM_REEL,1.1_PM_REEL,1.2_PM_REEL,1.3_PM_REEL,1.4_PM_REEL,1.5_PM_REEL,&
         1.6_PM_REEL,1.7_PM_REEL,1.8_PM_REEL,1.9_PM_REEL,2._PM_REEL, &
         2.1_PM_REEL,2.2_PM_REEL,2.3_PM_REEL,2.4_PM_REEL,2.5_PM_REEL,2.6_PM_REEL,2.7_PM_REEL,2.8_PM_REEL,2.9_PM_REEL,3._PM_REEL, &
         3.1_PM_REEL,3.2_PM_REEL,3.3_PM_REEL,3.4_PM_REEL,3.6_PM_REEL,3.8_PM_REEL,4._PM_REEL,4.5_PM_REEL/
	
	 COMMON /globe/ m1,m2,a1,a2,a1w,a2w,a1vw,a2vw,ig1,ig2,lg1,lg2,cg1
	 SAVE /globe/
	 IF( WANTU1 ) THEN
            IF( COLMAJOR ) THEN
               CALL CLASR( 'R', 'V', 'F', P, IMAX-IMIN+1, &
      RWORK(IU1CS+IMIN-1), RWORK(IU1SN+IMIN-1), &
      U1(1,IMIN), LDU1 )
	
END PROGRAM ESSAI