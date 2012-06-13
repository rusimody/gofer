# Crude makefile for producing Ascii version of Gofer docs

all		: pfmt
		  ./pfmt -c < usrguide.doc 2> contents
		  ./pfmt    < usrguide.doc
		  ./pfmt    < notes221.doc
		  ./pfmt    < notes228.doc
		  ./pfmt    < notes230.doc
		  -mkdir ../docs
		  mv ch* appx_* release.* ../docs
		  rm contents

pfmt		: pfmt.c
		  cc -o pfmt -O pfmt.c

clean		:
		  -rm contents pfmt

