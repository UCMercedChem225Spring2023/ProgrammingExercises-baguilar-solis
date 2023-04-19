      Program prgm_05_01
!
!     This program determines the eigenvalues and eigenfunctions for 
!     the slanted PIB problem, with V(x) = bx. The basis set is given 
!     by the 1D PIB eigenfucntions, psi = sin(n*pi*x/L). The number of
!     basis functions will be given by the user, where this will
!     represent the N lowest eigenfunctions. 

!     In this program m = L = hbar = 1
!
!
!     Variable Declarations
      implicit none
      integer::i,j,k,IError,NCmdLineArgs,NBasis
      real::b,m,l
      Real,Dimension(:,:),Allocatable::hMatrix,EVecs
      real,dimension(:),Allocatable::packed_hMatrix,EVals,Temp_Vector
      real,external::PIB_1D_Modified_Hamiltonian_Element
      integer::n1,n2
      logical::fail
      character(len=1024)::cmd_buffer
!
!     Format Statements
!
! 2000 format(1x,'Hamiltonian matrix element ',I5,',',I5,' is ',F12.5,'.')
 9000 format(1x,'Expected 1 command line argument, but found ',i2,'.')
!
!
!     Initialize mass (m), length (l), and b (V = bx)

      m = 1
      l = 1
!      b = 1
      b = 10

!     Read in number of basis functions from the command line.
!
      NCmdLineArgs = command_argument_count()
      if(NCmdLineArgs.ne.1) then
        write(*,9000) NCmdLineArgs
        fail = .true.
      endIf
      if(fail) goto 999
      call Get_Command_Argument(1,cmd_buffer)
      read(cmd_buffer,*) NBasis

!     Allocate dimension of hMatrix

      Allocate(hMatrix(NBasis,NBasis))

!     Given the input parameters, evaluate the potential energy integral between
!     particle-in-a-box eigenfunctions n1 and n2.
!
      do n1 = 1,NBasis
        do n2 = 1,NBasis
         hMatrix(n1,n2)=PIB_1D_Modified_Hamiltonian_Element(m,b,l,n1,n2)
!          write(*,'(f5.1)') hMatrix(n1,n2)
        enddo
      enddo

!      write(*,*) 'Full H Matrix: '
!      write(*,'(f5.1)') hMatrix

!     Form packed form of hMatrix 

      Allocate(packed_hMatrix((NBasis*(NBasis+1))/2))

      k = 1

      do i = 1,NBasis
        do j = i,NBasis
          packed_hMatrix(k) = hMatrix(j,i)
          k = k+1
        endDo
      endDo

!     Allocate dimesions to evecs, evals, and temp_vec

      Allocate(EVals(NBasis),EVecs(NBasis,NBasis),&
        Temp_Vector(3*NBasis))

!     Diagonalize hmatrix

      Call SSPEV('V','L',NBasis,packed_hMatrix,EVals,EVecs,&
        NBasis,Temp_Vector,IError)

      If(IError.ne.0) then
        Write(*,*)' Failure in DSPEV.'
        STOP
      endIf

!     Print evals and evecs

      write(*,*) 'Evals: '
      Call Print_Matrix_Full_Real(RESHAPE(Evals,(/1,NBasis/)),1,&
        NBasis)

      write(*,*) 'Evecs: '
      Call Print_Matrix_Full_Real(Evecs,NBasis,NBasis)

  999 continue
      End Program prgm_05_01


      real function PIB_1D_Modified_Hamiltonian_Element(m,b,l,n1,n2)
!
!     This function evaluates the Hamiltonian matrix element < n1 | H | n2 >,
!     where n1 and n2 are particle-in-a-box eigenstate labels and H is
!     the total energy operator. H = T + V
!
!
!     Variable Declarations
      implicit none
      real,intent(in)::l,b,m
      integer,intent(in)::n1,n2
      real,external::PIB_1D_Modified_V_Element, PIB_1D_T_Element

!     The kinetic and potential energies were computed in different
!     functions. These are called here. 

      PIB_1D_Modified_Hamiltonian_Element = &
      PIB_1D_T_Element(m,l,n1,n2) + PIB_1D_Modified_V_Element(b,l,n1,n2)
        
!
      end function PIB_1D_Modified_Hamiltonian_Element
  
!     ----- Kinetic Energy Function -----    
!    
      real function PIB_1D_T_Element(m,l,n1,n2)
!
!     This function evaluates the kinetic energy matrix element < n1 | T
!     | n2 >,
!     where n1 and n2 are particle-in-a-box eigenstate labels and T is
!     the
!     kinetic energy operator.
!
!
!     Variable Declarations
      implicit none
      real,intent(in)::l,m
      integer,intent(in)::n1,n2

!     Local Variables 
      real::prefactor
      real,parameter::pi=float(4)*atan(1.0)
!
!     The case where n1=n2 is different than n1\=n2. For this reason, we
!     use an
!     if block to separate the evaluation of the kinetic energy integral
!     for
!     these two different cases.
!

!      pi  =float(4)*atan(1.0)
      prefactor = pi**2/m

      if(n1.eq.n2) then

        PIB_1D_T_Element = prefactor*((n1**2)/(2*l**2))

      else

        PIB_1D_T_Element = 0 

      endIf
!
      end function PIB_1D_T_Element
!
!     ----- Potential Energy Function ------

      real function PIB_1D_Modified_V_Element(b,l,n1,n2)
!
!     This function evaluates the potential energy matrix element < n1 |
!     V(x) | n2 >,
!     where n1 and n2 are particle-in-a-box eigenstate labels and V(x)
!     is the
!     potential energy, where V(x) = bx.
!
!
!     Variable Declarations
      implicit none
      real,intent(in)::l,b
      integer,intent(in)::n1,n2

!     Local Variables
      real::prefactor
      real,parameter::pi=float(4)*atan(1.0)
!
!     The case where n1=n2 is different than n1\=n2. For this reason, we
!     use an
!     if block to separate the evaluation of the potential energy
!     integral for
!     these two different cases.
!
      if(n1.eq.n2) then

        PIB_1D_Modified_V_Element = (b*l/2) 

      else

        PIB_1D_Modified_V_Element = (b*l/pi**2) &
              * (((cos((n1-n2)*pi)-1)/(n1-n2)**2) &
              + ((1-cos((n1+n2)*pi))/(n1+n2)**2))

!        write(*,*) (cos((n1-n2)*pi)-1)/(n1-n2)**2
!        write(*,*) (1-cos((n1+n2)*pi))/(n1+n2)**2

      endIf
!
      end function PIB_1D_Modified_V_Element

!     Print matrices 

      Subroutine Print_Matrix_Full_Real(AMat,M,N)
!
!     This subroutine prints a real matrix that is fully dimension -
!     i.e.,
!     not stored in packed form. AMat is the matrix, which is
!     dimensioned
!     (M,N).
!
!     The output of this routine is sent to unit number 6 (set by the
!     local
!     parameter integer IOut).
!
!
!     Variable Declarations
!
      implicit none
      integer,intent(in)::M,N
      real,dimension(M,N),intent(in)::AMat
!
!     Local variables
      integer,parameter::IOut=6,NColumns=5
      integer::i,j,IFirst,ILast
!
 1000 Format(1x,A)
 2000 Format(5x,5(7x,I7))
 2010 Format(1x,I7,5F14.6)
!
      Do IFirst = 1,N,NColumns
        ILast = Min(IFirst+NColumns-1,N)
        write(IOut,2000) (i,i=IFirst,ILast)
        Do i = 1,M
          write(IOut,2010) i,(AMat(i,j),j=IFirst,ILast)
        endDo
      endDo
!
      Return
      End Subroutine Print_Matrix_Full_Real

