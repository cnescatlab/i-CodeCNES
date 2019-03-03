! ********************************************************************************
! Dans cet exemple on traite le cas de figure ou dans un sous-programme  
! est effectue la modification du contenu d'une variable du programme hote . 
! Mais, cette operation a pour effet, de modifier egalement la valeur pointee 
! par le paramètre formel du sous-programme 
! ********************************************************************************
Subroutine alias 

      integer                         :: i = 1
      integer                         :: i_stdout = 6
      call incr(i)
      write (i_stdout,*) i

contains

     subroutine incr(j)
           integer, intent(inout)      :: j
           write (i_stdout,*) j        ! imprime 1
           i = i + 1                   ! i a ete defini avec une portee qui le rend accessible depuis la subroutine 
           write (i_stdout,*) j        ! la valeur de j, s'en trouve modifiee egalement -> imprime 2 
     end subroutine incr

end subroutine alias

PROGRAM ESSAI

      call alias

END PROGRAM ESSAI
