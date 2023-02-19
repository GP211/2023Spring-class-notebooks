# < well.T Grid  n1=100 n2=30 interp=1 > mesh.H
#%
integer esize
from history:	integer n1:k1, n2:k2
from par:	integer n1=100, n2=30, esize=4
allocate:	real triple(k1,k2)
allocate:	real mesh  (n1,n2)

subroutine dumb( triple,k1,k2,  mesh,n1,n2)
integer i,j, i1,i2,     k1,k2,       n1,n2, interp
real		 triple(k1,k2), mesh(n1,n2)
real		 o1,d1, o2,d2,   xmin,xmax, ymin,ymax
from par:	integer interp=1
from either:	real xmin,xmax, ymin,ymax
	o1 = xmin;	d1 = (xmax-xmin)/(n1-1)
	o2 = ymin;	d2 = (ymax-ymin)/(n2-1)
to history: real  o1,d1, o2,d2
call null( mesh, n1*n2)
call sreed('in', triple, 4*k1*k2)
do j=1,k2 {
	i1 = (triple(1,j)-o1)/d1
	i2 = (triple(2,j)-o2)/d2
	if( 0<i1 & i1<=n1 &
	    0<i2 & i2<=n2  ) 
		mesh(i1,i2) = mesh(i1,i2) + triple(3,j)
	}
call hclose()
call srite('out', mesh, 4*n1*n2)
return; end

