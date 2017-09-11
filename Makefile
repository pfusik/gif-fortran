run: gif2tga kurChuck.gif kurChuck-interlaced.gif
	./gif2tga kurChuck.gif
	cmp kurChuck-expected.tga kurChuck.tga
	./gif2tga kurChuck-interlaced.gif
	cmp kurChuck-expected.tga kurChuck-interlaced.tga

gif2tga: gif2tga.f90
	i686-w64-mingw32-gfortran -o $@ -s -static -O2 -Wall $<
