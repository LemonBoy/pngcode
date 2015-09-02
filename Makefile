encode: encode.scm crc.so
	csc encode.scm

crc.so: crc.scm
	csc -s -j crc crc.scm `pkg-config --cflags --libs zlib`

clean:
	rm *.so encode
