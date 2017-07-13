CSC ?= csc

writepng: writepng.scm encode.so
	$(CSC) writepng.scm

encode.so: encode.scm zlib1.so zopfli.so
	$(CSC) -C -O3 -O3 -inline -specialize -k -s -j encode encode.scm

zopfli.so: zopfli.scm
	$(CSC) -O3 -k -s -j zopfli -ot zopfli.types zopfli.scm -lzopfli

zlib1.so: zlib1.scm
	$(CSC) -O3 -k -s -j zlib1 -ot zlib1.types zlib1.scm `pkg-config --cflags --libs zlib`

clean:
	rm *.so writepng
