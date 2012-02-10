
SRCS	=	src/ofile.mli		\
		src/ofile.ml		\
		src/documents.mli	\
		src/documents.ml	\
		src/ocsidocs.ml

INCLUDE	=	-I +extlib extLib.cma unix.cma

all	:
		 eliomc -I +extlib extLib.cma -I src -I _server/src/ -c $(SRCS)

clean	:
		rm -f *.cm[iox] *.o
		rm -f _server/*.cm[iox] _server/*.o
		rm -f _server/src/*.cm[iox] _server/src/*.o
		rm -f src/*.cm[iox] src/*.o
