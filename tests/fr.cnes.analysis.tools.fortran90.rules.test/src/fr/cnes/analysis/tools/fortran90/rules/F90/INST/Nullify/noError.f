program ESSAI

	allocate(C(n1), STAT = iom)

	deallocate(C, stat=iom)
	nullify  (C)
	! pointeur NULL

end program