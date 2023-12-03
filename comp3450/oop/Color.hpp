#ifndef __Color_HPP_INCLUDED__
#define __Color_HPP_INCLUDED__

class RGBColor {
private:
    int _red, _green, _blue;
    bool validate();
    static int COLORMAX; //! inclusive
public:
    static int GetMaxColor();
    RGBColor();
    RGBColor(int red, int green, int blue);
    static RGBColor Black();
    static RGBColor White();
    static RGBColor Red();
    static RGBColor Green();
    static RGBColor Blue();
    int red() const;
    int green() const;
    int blue() const;
    void red(int red);
    void green(int green);
    void blue(int blue);
    bool operator==(RGBColor& rhs);
    RGBColor& operator=(RGBColor const& rhs);
    bool operator<(RGBColor const& rhs) const;
};

using Color = RGBColor;

#endif // !__Color_HPP_INCLUDED__
