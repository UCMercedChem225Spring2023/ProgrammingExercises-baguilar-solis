      Program prgm_04_03

!    This program computes matrix-vector multiplication, to yield a new
!    vector. The leading dimension is given by the user and the cpu time
!    for the product is recorded.
!


!     Variable Declarations
     

      Integer::NDim
      Real::Time_Start,Time_End
      Real,Dimension(:,:),Allocatable::Matrix,Vector1,Vector2
      Character(Len=64)::Character_Temp
!
!     Format Statement

1000 Format(1x,'Dimension=',I6,' Time: ',F10.4,' s.')

!     Begin by reading the dimension from the command line
!
      Call Get_Command_Argument(1,Character_Temp)
      Read(Character_Temp,*) NDim

!     Allocate sizes of matrices, using dimension given by the user. 
!     Fill Matrix and Vector1 with random numbers 

      Allocate(Matrix(NDim,NDim),Vector1(NDim,1))
      Call random_number(Matrix)
      Call random_number(Vector1)

!
!     Compute matrix-vector product
!     
      Call cpu_time(Time_Start)
      Vector2 = MatMul(Matrix, Vector1)
      Call cpu_time(Time_End)
      
      Write(*,1000) NDim,Time_End-Time_Start

      End Program
