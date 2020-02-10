 PROGRAM ESSAI

	subroutine s1
		open(unit=a)
		open(b)
		close(unit=a)
		allocate(y)
	end subroutine s1
	
	function f1
		allocate(x)
		deallocate(y)
		deallocate(x)
	end function f1
	
	subroutine s2
		close(unit=b)
	end subroutine s2
	   
  END PROGRAM
