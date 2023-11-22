#ifndef __PortablePixelMap_HPP_INCLUDED__
#define __PortablePixelMap_HPP_INCLUDED__

#include <cstddef>
#include <ostream>
#include <string>
#include <vector>
#include "Color.hpp"

class PortablePixelMap {
private:
    std::vector<std::vector<Color>> pixelMap;
    static std::string magicNumberPrefix;
    static std::string printPixel(Color pixel);
public:
    PortablePixelMap(size_t y, size_t x, Color color = Color::Black());
    void resize(size_t y, size_t x, Color color = Color::Black());
    static PortablePixelMap make_gradient(size_t y, size_t x, Color startColor = Color::White(), Color endColor = Color::Black());
    void printImageFile(std::ostream& out) const;
    Color& at(size_t y, size_t x);
    const Color& at(size_t y, size_t x) const;
};

#endif // !__PortablePixelMap_HPP_INCLUDED__
