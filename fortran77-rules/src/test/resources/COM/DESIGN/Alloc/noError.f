 PROGRAM ESSAI

	subroutine s1
		open(unit=a)
		close(unit=a)
	end subroutine s1
	
	procedure p1
		allocate(b)
		deallocate(b)
	end procedure p1
	   
  END PROGRAM
