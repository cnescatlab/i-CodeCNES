 PROGRAM ESSAI

	subroutine s1
		open(unit=a)
		open(b)
		close(unit=a)
		allocate(y)
	end subroutine s1
	
	procedure p1
		allocate(x)
		deallocate(y)
		deallocate(x)
	end procedure p1
	
	subroutine s2
		close(unit=b)
	end subroutine s2
	   
  END PROGRAM
