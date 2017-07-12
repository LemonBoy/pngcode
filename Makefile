writepng: writepng.scm encode.so
	csc writepng.scm

encode.so: encode.scm crc.so zopfli.so
	csc -C -O3 -O3 -inline -specialize -k -s -j encode encode.scm

zopfli.so: zopfli.scm
	csc -O3 -k -s -j zopfli -ot zopfli.types zopfli.scm -lzopfli

crc.so: crc.scm
	csc -O3 -k -s -j crc -ot crc.types crc.scm `pkg-config --cflags --libs zlib`

clean:
	rm *.so writepng
