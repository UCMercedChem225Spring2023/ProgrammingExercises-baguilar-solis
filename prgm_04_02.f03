      Program prgm_04_02

!    This program will compute the dot product of two vectors with the
!    dimension being given by the user at the command line. The vectors
!    will be filled with random numbers. The cpu time needed to compute
!    the dot product will be recorded. 
      
      
      implicit None
      Integer::NDim
      Real::Time_Start,Time_End,scalar
      Real,Dimension(:),Allocatable::Vector1,Vector2
      Character(Len=64)::Character_Temp
!
!     Format Statement

1000 Format(1x,'Dimension=',I6,' Time: ',F10.8,' s.')

!     Begin by reading the dimension from the command line
!
      Call Get_Command_Argument(1,Character_Temp)
      Read(Character_Temp,*) NDim

!     Allocate sizes of matrices, using dimension given by the user. 
!     Fill Vector1 and Vector2 with random numbers 

      Allocate(Vector1(NDim),Vector2(NDim))
      Call random_number(Vector1)
      Call random_number(Vector2)

!
!     Compute dot product
!     
      Call cpu_time(Time_Start)
      scalar = Dot_Product(Vector1,Vector2)
      Call cpu_time(Time_End)

      Write(*,1000) NDim,Time_End-Time_Start

      End Program
