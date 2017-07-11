writepng: writepng.scm encode.so
	csc writepng.scm

encode.so: encode.scm crc.so
	csc -O3 -inline -specialize -k -s -j encode encode.scm

crc.so: crc.scm
	csc -O3 -k -s -j crc crc.scm `pkg-config --cflags --libs zlib`

clean:
	rm *.so writepng
