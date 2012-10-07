module globals
	implicit none 
	integer, parameter :: wp = selected_real_kind(p=14)
	integer, parameter :: Nx = 50
	integer, parameter :: Ny = 50
	integer, dimension(2) :: LN(Nx,Ny)
end module globals

program Life
	use globals
	use gif
	IMPLICIT NONE
	integer  :: i,j,iocheck,steps
	logical, dimension(2) :: grid(Nx,Ny)
	integer, dimension(:), allocatable :: PopRec

	steps = 150
	allocate(PopRec(steps))

	call setup(grid)
	call itterate(steps,.true.,PopRec)

	write(*,*) BoolSum(140,PopRec)

	contains

	subroutine itterate(steps,write,PopRec)
		use globals
		IMPLICIT NONE
		integer :: stpr,steps
		logical :: write
		integer, dimension(1) :: PopRec

		if (write.eqv..true.) then
			call WriteFrame(grid,0)
		end if
		write(*,*) 0,GridStats(grid)
		PopRec(1) = GridStats(grid)
		do stpr = 2, steps
			call SingleStep(grid)
			write(*,*) stpr,GridStats(grid)
			PopRec(stpr) = GridStats(grid)
			if (write.eqv..true.) then
				call WriteFrame(grid,stpr)
			end if
			if(stpr.ge.11)then
				if(BoolSum(stpr,PopRec)) exit
			end if
			
			
		end do
		
	end subroutine itterate
	
	subroutine SingleStep(grid)
		use globals
		IMPLICIT NONE
		integer :: i,j
		logical, dimension(2) :: gridNew(Nx,Ny),grid(Nx,Ny)
		
		forall(i=1:Nx,j=1:Ny)
			LN(i,j) = NNpop(i,j)
		end forall

		gridNew = grid

		forall(i=1:Nx,j=1:Ny,(grid(i,j).eqv..true.).and.(LN(i,j).le.1))
				gridNew(i,j) = .false.
		end forall
		forall(i=1:Nx,j=1:Ny,(grid(i,j).eqv..true.).and.(LN(i,j).gt.3))
				gridNew(i,j) = .false.
		end forall
		forall(i=1:Nx,j=1:Ny,(grid(i,j).eqv..false.).and.(LN(i,j).eq.3))
			gridNew(i,j) = .true.
		end forall

		grid = gridNew

	end subroutine SingleStep
	
	pure function NNpop(i,j) result (LiveNeighbors)
		use globals
		IMPLICIT NONE
		integer, intent(in) :: i,j
		integer :: ii,jj,LiveNeighbors
		LiveNeighbors = 0
		
		if(((i.ne.1).and.(i.ne.Nx)).and.((j.ne.1).and.(j.ne.Ny))) then
		!!!Center boxes
			do ii = i-1,i+1
				do jj = j-1,j+1
					if((ii.ne.i).or.(jj.ne.j)) then
						if(grid(ii,jj).eqv..true.) then
							LiveNeighbors = LiveNeighbors + 1
					 	end if
					end if
				end do
			end do

		!!!Edges
			!!Left
			else if((i.eq.1).and.(j.ne.1).and.(j.ne.Ny)) then
				LiveNeighbors = 0
				do ii = i,i+1
				do jj = j-1,j+1
					if((ii.ne.i).or.(jj.ne.j)) then
						if(grid(ii,jj).eqv..true.) then
							LiveNeighbors = LiveNeighbors + 1
					 	end if
					end if
				end do
			end do
		
			!!Right
			else if((i.eq.Nx).and.(j.ne.1).and.(j.ne.Ny)) then
				LiveNeighbors = 0
				do ii = i-1,i
					do jj = j-1,j+1
						if((ii.ne.i).or.(jj.ne.j)) then
							if(grid(ii,jj).eqv..true.) then
								LiveNeighbors = LiveNeighbors + 1
						 	end if
						end if
					end do
				end do
			!!Top
			else if((j.eq.1).and.(i.ne.1).and.(i.ne.Nx)) then
				LiveNeighbors = 0
				do ii = i-1,i+1
					do jj = j,j+1
						if((ii.ne.i).or.(jj.ne.j)) then
							if(grid(ii,jj).eqv..true.) then
								LiveNeighbors = LiveNeighbors + 1
						 	end if
						end if
					end do
				end do
		
			!!Bottom
			else if((j.eq.Ny).and.(i.ne.1).and.(i.ne.Nx)) then
				LiveNeighbors = 0
				do ii = i-1,i+1
					do jj = j-1,j
						if((ii.ne.i).or.(jj.ne.j)) then
							if(grid(ii,jj).eqv..true.) then
								LiveNeighbors = LiveNeighbors + 1
						 	end if
						end if
					end do
				end do
		
		!!!Corners
			!!TopRight
			else if((i.eq.1).and.(j.eq.Ny)) then
				LiveNeighbors = 0
				do ii = i,i+1
				do jj = j-1,j
					if((ii.ne.i).or.(jj.ne.j)) then
						if(grid(ii,jj).eqv..true.) then
							LiveNeighbors = LiveNeighbors + 1
					 	end if
					end if
				end do
			end do

			
			!!TopLeft
			else if((i.eq.1).and.(j.eq.1)) then
				LiveNeighbors = 0
				do ii = i,i+1
				do jj = j,j+1
					if((ii.ne.i).or.(jj.ne.j)) then
						if(grid(ii,jj).eqv..true.) then
							LiveNeighbors = LiveNeighbors + 1
					 	end if
					end if
				end do
			end do
			
			!!BottomLeft
			else if((i.eq.Nx).and.(j.eq.1)) then
				LiveNeighbors = 0
				do ii = i-1,i
				do jj = j,j+1
					if((ii.ne.i).or.(jj.ne.j)) then
						if(grid(ii,jj).eqv..true.) then
							LiveNeighbors = LiveNeighbors + 1
					 	end if
					end if
				end do
			end do
		
			!!BottomRight
			else if((i.eq.Nx).and.(j.eq.Ny)) then
				LiveNeighbors = 0
				do ii = i-1,i
				do jj = j-1,j
					if((ii.ne.i).or.(jj.ne.j)) then
						if(grid(ii,jj).eqv..true.) then
							LiveNeighbors = LiveNeighbors + 1
					 	end if
					end if
				end do
				end do

		end if	
	end function NNpop

	subroutine WriteFrame(grid,framenum)
		use globals
		use gif_util
		IMPLICIT NONE
		logical, dimension(2) :: grid(Nx,Ny)
		integer, dimension(2) :: CMint(3,3)
		integer, dimension(:,:), allocatable  :: Pic
		integer :: Scale,BorderW
		integer :: framenum
		character(64) :: framenumc
		
		CMint = 0
		CMint(:,1) = 0		! Black	Pic = 0
		CMint(:,2) = 125	! Gray	Pic = 1
		CMint(:,3) = 255	! White	Pic = 2

		!Set scale factors
		Scale   = 15		! Square cell size
		BorderW = 2		! Border width, cannot be equal to 1 

		!Allocate the picture size
		allocate(Pic(Scale*Nx+BorderW*(Nx+1),Scale*Ny+BorderW*(Ny+1)))

		!White washes the picture
		Pic = 2

		!! Draw the borders
		!Left
		forall(i=1:BorderW)
			Pic(i,:) = 1
		end forall
		!Right
		forall(i=Scale*Nx+BorderW*(Nx+1)-BorderW+1:Scale*Nx+BorderW*(Nx+1))
			Pic(i,:) = 1
		end forall		
		!Top
		forall(j=1:BorderW)
			Pic(:,j) = 1
		end forall
		!Bottom
		forall(j=Scale*Ny+BorderW*(Ny+1)-BorderW+1:Scale*Ny+BorderW*(Ny+1))
			Pic(:,j) = 1
		end forall		

		!! Draw verticle bars
		do i = 0, Nx-1
			Pic(1+Scale*i+i*borderW:Scale*i+(borderW)+(i)*borderW,:) = 1
		end do

		!! Draw horizontal bars
		do i = 0, Ny-1
			Pic(:,1+Scale*i+i*borderW:Scale*i+(borderW)+(i)*borderW) = 1
		end do

		! Shade the live pixels black
		forall(i=1:Nx,j=1:Ny,(grid(i,j).eqv..true.))
			Pic(borderW*i+(i-1)*scale+1 : borderW*i+(i-1)*scale+scale&
				,borderW*j+(j-1)*scale+1 : borderW*j+(j-1)*scale+scale) = 0
		end forall


		write(framenumc,'(I5.5)') framenum
		call writegif(trim(framenumc)//".gif",Pic,CMint)

	end subroutine WriteFrame

	pure function tester() result (resl)
		use globals
		IMPLICIT NONE
		integer :: resl
		
	end function tester

	pure function GridStats(grid) result(sum)
		use globals
		IMPLICIT NONE
		logical, dimension(2),intent(in) :: grid(Nx,Ny)
		integer :: i,j,sum
		
		sum = 0 
		do i=1,Nx
			do j=1,Ny
				if(grid(i,j).eqv..true.) then
					sum = sum + 1
				end if
			end do
		end do
	

	end function 

	function BoolSum(i,PopRec) result (sameq)
		use globals
		IMPLICIT NONE
		integer, intent(in) :: i
		logical, dimension(1) ::  MiniRec(10)
		integer, dimension(:) :: PopRec
	 	integer :: j,summ
		logical :: sameq
	
		MiniRec = PopRec(i:i-10).eq.PopRec(i-1:i-11)
		
		summ = 0
		do j = 1 , 10
			if(MiniRec(j).eqv..true.)then
				summ = summ + 1
			end if
		end do
		
		if(summ.eq.10)then
			sameq = .true.
		else
			sameq = .false.
		end if
	end function BoolSum

	subroutine setup(grid)
		use globals
		IMPLICIT NONE
		logical, dimension(2) :: grid(Nx,Ny)
		real, dimension(2) :: rgrid(Nx,Ny)
		integer :: i,j
		

		grid = .false.
		grid(5+5:6+5,5+5) = .true.
		grid(6+5,6+5) = .true.
		grid(10+5:12+5,6+5) = .true.
		grid(11+5,4+5) = .true.
		
		
		!! Random intial distribution
!		call random_seed()
!		call random_number(rgrid)	
!		do i=1,Nx
!			do j=1,Ny
!				if (rgrid(i,j).lt.(0.2)) then
!					grid(i,j) = .true.
!				else
!					grid(i,j) = .false.
!				end if
!			end do
!		end do


	end subroutine setup

end program Life