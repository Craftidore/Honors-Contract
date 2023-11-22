#include "PortablePixelMap.hpp"
#include <cstddef>
#include <exception>
#include <sstream>
#include <stdexcept>
#include <string>

std::string PortablePixelMap::magicNumberPrefix = "P3";

PortablePixelMap::PortablePixelMap(size_t y, size_t x, Color color) {
    if (y == 0 || x == 0) {
        throw std::domain_error("y and x must be whole numbers greater than 0.");
    }
    resize(y, x, color);
}

void PortablePixelMap::resize(size_t y, size_t x, Color color) {
    pixelMap.resize(y);
    for (auto& row : pixelMap) {
        row.resize(x, color);
    }
}

PortablePixelMap PortablePixelMap::make_gradient(size_t y, size_t x, Color startColor, Color endColor) {
    auto adjust = [&startColor, &endColor](double adjustment) {
        int red = startColor.red() + adjustment*(endColor.red() - startColor.red());
        int green = startColor.green() + adjustment*(endColor.green() - startColor.green());
        int blue = startColor.blue() + adjustment*(endColor.blue() - startColor.blue());
        return Color(red,green,blue);
    };

    PortablePixelMap gradient(y,x,startColor);
    for (int col = 0; col < x; ++col) {
        double percentTowardEnd = col/(double)x; // yes, I know it's not really a percent
        Color colColor = adjust(percentTowardEnd);
        for (int row = 0; row < y; ++row) {
            gradient.pixelMap[row][col] = colColor;
        }
    }
    return gradient;
}

Color& PortablePixelMap::at(size_t y, size_t x) {
    return pixelMap.at(y).at(x);
}

const Color& PortablePixelMap::at(size_t y, size_t x) const {
    return pixelMap.at(y).at(x);
}

std::string PortablePixelMap::printPixel(Color color) {
    std::stringstream pixelStream;
    pixelStream << color.red() << ' ' << color.green() << ' ' << color.blue();
    return pixelStream.str();
}

void PortablePixelMap::printImageFile(std::ostream& out) const {
    // magic number to tell the image reader how to interpret the image 
    out << magicNumberPrefix << '\n';
    out << "# Generated with ppm-generator" << '\n';
    // width and height
    out << pixelMap[0].size() << ' ' << pixelMap.size() << '\n';
    // color max value (255 for 8-bit colors)
    out << Color::GetMaxColor() << '\n';
    for (auto const& row : pixelMap) {
        bool startOfRow = true;
        for (Color const& pixel : row) {
            std::string pixelString = printPixel(pixel);
            out << (startOfRow ? "" : " ");
            startOfRow = false;
            out << pixelString;
        }
        out << '\n';
    }
}
