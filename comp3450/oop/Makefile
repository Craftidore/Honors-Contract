debug:ppm-generator-debug-bin
	gdb ./ppm-generator-debug-bin

run:ppm-generator-bin
	./ppm-generator-bin

ppm-generator-bin:*.cpp 
	g++ *.cpp -o ppm-generator-bin

ppm-generator-debug-bin:*.cpp
	g++ -g *.cpp -o ppm-generator-debug-bin

clean:
	rm *.o *-bin *.out *.ppm

