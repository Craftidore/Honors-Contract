test:ppm-generator.cl
	./ppm-generator.cl
	mv image.ppm clisp.ppm
	../oop/ppm-generator-linux-0.1
	diff image.ppm clisp.ppm

run:ppm-generator.cl
	./ppm-generator.cl

ppm-generator.cl:ppm-generator.src.cl
	# dynamically add interpreter info at top
	{ printf "#!"; whereis clisp | awk -F ' ' '{print $$2}'; } | cat - ppm-generator.src.cl > ppm-generator.cl
	chmod +x ppm-generator.cl

debug:ppm-generator.cl
	cp ppm-generator.cl ppm-generator-debug.cl
	sed -i "s/;DEBUG//g" ppm-generator-debug.cl
	./ppm-generator-debug.cl

clean:
	rm -rf ppm-generator.cl ppm-generator-debug.cl *.ppm
