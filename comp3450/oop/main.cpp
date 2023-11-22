#include "Color.hpp"
#include "PortablePixelMap.hpp"
#include <cstdlib>
#include <fstream>
#include <sstream>
#include <string>
#include <iostream>

#define PPM_GEN_VERSION ("0.0.1")

void HandleMetaFlags(int argc, char** argv) {
    if (argc > 1 && std::string("--version") == argv[1]) {
        std::cout << "ppm-generator by Craftidore" << '\n';
        std::cout << "Version: " << PPM_GEN_VERSION << '\n';
        exit(EXIT_SUCCESS);
    }
    else if (argc > 1 && std::string("--help") == argv[1]) {
        std::cout << "ppm-generator by Craftidore" << '\n';
        std::cout << "Version: " << PPM_GEN_VERSION << '\n';
        std::cout << '\n';
        std::cout << "ppm-generator [y_size [x_size [filename]]]" << '\n';
        exit(EXIT_SUCCESS);
    }
}

void HandleArguments(int argc, char** argv, int& size_y, int& size_x, std::string& filename) {
#define VALIDATE_INPUT(stream, msg) \
    if (!(stream)) { \
        std::cerr << (msg) << std::endl; \
        exit(EXIT_FAILURE); \
    }

    filename = "image.ppm";
    size_y = 400;
    size_x = 800;
    if (argc > 1) {
        std::istringstream y_in(argv[1]);
        VALIDATE_INPUT(y_in >> size_y, std::string("Invalid y size: ") + argv[1])
    }
    if (argc > 2) {
        std::istringstream x_in(argv[2]);
        VALIDATE_INPUT(x_in >> size_x, std::string("Invalid x size: ") + argv[2]);
    }
    if (argc > 3) {
        filename = argv[3];
        // add `.ppm` onto end, if no `.ppm` is there already
        auto extensionBegin = filename.find_last_of('.');
        if (extensionBegin == std::string::npos || filename.substr(extensionBegin) != ".ppm") {
            filename += ".ppm";
        }
    }
#undef VALIDATE_INPUT
}

int main(int argc, char** argv) {
    // Check for --version or --help
    HandleMetaFlags(argc, argv); // may exit

    // Otherwise...
    int size_y, size_x;
    std::string filename;
    HandleArguments(argc, argv, size_y, size_x, filename);// sets size_y, size_x & filename

    // create & print gradient
    PortablePixelMap ppm = PortablePixelMap::make_gradient(size_y,size_x,Color::Green(),Color::Red());
    std::ofstream out_file(filename);
    ppm.printImageFile(out_file);
}

