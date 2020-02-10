Subroutine alias 

      integer                         :: i = 1
      integer                         :: i_stdout = 6
      call incr(i)
      write (i_stdout,*) i

contains

     subroutine incr(j)
           integer, intent(inout)     :: j
           write (i_stdout,*) j       ! imprime 1
           j = j+1                    ! j est modifie mais i egalement 
           write (i_stdout,*) j       ! imprime 2 
     end subroutine incr

end subroutine alias

PROGRAM ESSAI

      call alias

END PROGRAM ESSAI

