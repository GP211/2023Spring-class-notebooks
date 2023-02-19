include ${SEPINC}/SEP.top
UF77LIBS = ${GEELIB}

default: Grid.x
#	< s2000.5 field 3 4 5 | Table > s2000.TT   plus hand edit end of file
#	< s2300.5 field 3 4 5 | Table > s2300.TT
#	< w2000.5 field 3 4 5 | Table > w2000.TT
#	< w2300.5 field 3 4 5 | Table > w2300.TT
#
#	<s2000.TT Dd esize=4| Window out=stdout > s2000.T
#	<s2300.TT Dd esize=4| Window out=stdout > s2300.T
#	<w2000.TT Dd esize=4| Window out=stdout > w2000.T
#	<w2300.TT Dd esize=4| Window out=stdout > w2300.T
	<s2000.T Grid.x > s2000.H
	<w2000.T Grid.x > w2000.H
	<s2000.H Wiggle >/dev/null out=s2000.v
	<w2000.H Wiggle >/dev/null out=w2000.v
	vp_SideBySideAniso s2000.v w2000.v > junk.v
	tube junk.v

clean: jclean
	${RM} *.T

include ${SEPINC}/SEP.bottom
