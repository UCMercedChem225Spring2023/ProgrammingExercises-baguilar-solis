      program prgm_02_01
!
!     This program carries out calculation of 1D particle-in-a-box values.
!     Specifically, the user provides the variable 'b' for the potential 
!     energy operator, particle mass, box length, and quantum
!     numbers for two canonical PIB eigenstates.
!     The potential is given by, V(x) = bx.
!
!     This program is written in atomic units.
!
!
!     Variable Declarations
      implicit none
      integer::i,NCmdLineArgs
      real::b,m,l,vMatrixElement
      real,external::PIB_1D_Modified_V_Element
      integer::n1,n2
      logical::fail
      character(len=1024)::cmd_buffer
!
!     Format Statements
!
 2000 format(1x,'Potential energy matrix element ',I5,',',I5,' is ',F12.5,'.')
 9000 format(1x,'Expected 5 command line arguments, but found ',i2,'.')
!
!
!     Read in b,m, l, n1, and n2 from the command line.
!
!      fail = .false.
      NCmdLineArgs = command_argument_count()
      if(NCmdLineArgs.ne.5) then
        write(*,9000) NCmdLineArgs
        fail = .true.
      endIf
      if(fail) goto 999
      call Get_Command_Argument(1,cmd_buffer)
      read(cmd_buffer,*) b
      call Get_Command_Argument(2,cmd_buffer)
      read(cmd_buffer,*) m
      call Get_Command_Argument(3,cmd_buffer)
      read(cmd_buffer,*) l
      call Get_Command_Argument(4,cmd_buffer)
      read(cmd_buffer,*) n1
      call Get_Command_Argument(5,cmd_buffer)
      read(cmd_buffer,*) n2
!
!     Given the input parameters, evaluate the potential energy integral between
!     particle-in-a-box eigenfunctions n1 and n2.
!
      vMatrixElement = PIB_1D_Modified_V_Element(b,l,n1,n2)
      write(*,2000) n1,n2,vMatrixElement
!
!     The end of the job...
!
  999 continue
      end program prgm_02_01


      real function PIB_1D_Modified_V_Element(b,l,n1,n2)
!
!     This function evaluates the potential energy matrix element < n1 | V(x) | n2 >,
!     where n1 and n2 are particle-in-a-box eigenstate labels and V(x) is the
!     potential energy, where V(x) = bx.
!
!
!     Variable Declarations
      implicit none
      real,intent(in)::l,b
      integer,intent(in)::n1,n2
      real::prefactor
      real,parameter::pi=float(4)*atan(1.0)
!
!     The case where n1=n2 is different than n1\=n2. For this reason, we use an
!     if block to separate the evaluation of the potential energy integral for
!     these two different cases.
!
      if(n1.eq.n2) then

        PIB_1D_Modified_V_Element = (b*l/2)

      else

        PIB_1D_Modified_V_Element = 0

      endIf
!
      end function PIB_1D_Modified_V_Element
