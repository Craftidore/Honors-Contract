#include "Color.hpp"

RGBColor::RGBColor() 
    : _red(Color::Black().red()),_green(Color::Black().green()),_blue(Color::Black().blue())
{
    validate();
}

RGBColor::RGBColor(int red, int green, int blue) 
    : _red(red),_green(green),_blue(blue)
{
    validate();
}

int RGBColor::COLORMAX = 255;

RGBColor RGBColor::Black() {
    return RGBColor(0,0,0);
}

RGBColor RGBColor::White() {
    return RGBColor(255,255,255);
}

RGBColor RGBColor::Red() {
    return RGBColor(255,0,0);
}

RGBColor RGBColor::Green() {
    return RGBColor(0,255,0);
}

RGBColor RGBColor::Blue() {
    return RGBColor(0,0,255);
}

int RGBColor::red() const {
    return _red;
}

int RGBColor::green() const {
    return _green;
}

int RGBColor::blue() const {
    return _blue;
}

void RGBColor::red(int red) {
    _red = red;
    validate();
}

void RGBColor::green(int green) {
    _green = green;
    validate();
}

void RGBColor::blue(int blue) {
    _blue = blue;
    validate();
}

bool RGBColor::operator==(RGBColor& rhs) {
    bool sameRed = _red == rhs._red;
    bool sameBlue = _blue == rhs._blue;
    bool sameGreen = _green == rhs._green;
    return sameRed && sameBlue && sameGreen;
}

RGBColor& RGBColor::operator=(RGBColor const& rhs) {
    _red = rhs._red;
    _blue = rhs._blue;
    _green = rhs._green;
    return *this;
}

bool RGBColor::operator<(RGBColor const& rhs) const {
    bool sameRed = _red < rhs._red;
    bool sameBlue = _blue < rhs._blue;
    bool sameGreen = _green < rhs._green;
    return sameRed || sameBlue || sameGreen;
}

bool RGBColor::validate() {
    bool wasInvalid = true;
    auto keepInRange = [&wasInvalid](int& color) {
        bool isNegative = color < 0;
        bool aboveMax = color > COLORMAX;
        if (isNegative) {
            color = 0;
            wasInvalid = true;
        }
        else if (aboveMax) {
            color = COLORMAX;
            wasInvalid = true;
        }
    };
    keepInRange(_red);
    keepInRange(_blue);
    keepInRange(_green);
    return wasInvalid;
}

int RGBColor::GetMaxColor() {
    return COLORMAX;
}
