
SRCS	=	src/ocsidocs.ml

all	:
		eliomc -c $(SRCS)

clean	:
		rm -f *.cm[iox] *.o
