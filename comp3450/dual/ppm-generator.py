#!/bin/python

import os
import io
import math

class Color:
    COLOR_MAX = 255
    def __init__(self, red, green, blue):
        self._red = int(red);
        self._green = int(green);
        self._blue = int(blue);
    def _correct_invariant(self):
        move_in_bounds = lambda x: 0 if (x < 0) else (Color.COLOR_MAX if (x > Color.COLOR_MAX) else x)
        self._red = move_in_bounds(self._red)
        self._green = move_in_bounds(self._green)
        self._blue = move_in_bounds(self._blue)
    def __repr__(self):
        return "Color({0}, {1}, {2})".format(self._red,self._green,self._blue)
    def __str__(self):
        return "{0} {1} {2}".format(self._red,self._green,self._blue)
    @property
    def red(self):
        return self._red
    @property
    def green(self):
        return self._green
    @property
    def blue(self):
        return self._blue
GREEN = Color(0,255,0)
RED = Color(255,0,0)

class PortablePixelMap:
    # _x_size: int
    # _y_size: int
    # _image_grid: list(list(Color))
    def create_portable_pixel_map(y_size = 0, x_size = 0, default_color=Color(0,0,0)):
        ppm = PortablePixelMap([])
        ppm._x_size = x_size
        ppm._y_size = y_size
        for y in range(y_size):
            ppm._image_grid.append([])
            for x in range(x_size):
                ppm._image_grid[y].append(default_color)
        if not ppm._check_invariant():
            raise Exception("Invalid types in constructor")
        return ppm
    def __init__(self, image_grid):
        self._image_grid = image_grid
        self._y_size = len(image_grid)
        self._x_size = len(image_grid[0]) if self._y_size > 0 else 0
        if not self._check_invariant():
            raise Exception("Invalid types in constructor")
    def _check_invariant(self):
        invariant = True
        invariant &= isinstance(self._x_size, int) and self._x_size >= 0
        invariant &= isinstance(self._y_size, int) and self._y_size >= 0
        invariant &= self._check_image_grid_invariant();
        return invariant
    def _check_image_grid_invariant(self):
        grid_is_list = isinstance(self._image_grid, list)
        rows_are_valid = True
        if grid_is_list:
            for row in self._image_grid:
                if not rows_are_valid:
                    break
                row_is_list = isinstance(row, list)
                row_items_are_valid = True
                if (row_is_list):
                    for item in row:
                        if (not row_items_are_valid):
                            break
                        row_items_are_valid = isinstance(item, Color)
                rows_are_valid &= row_is_list and row_items_are_valid
        return grid_is_list and rows_are_valid
    def __str__(self):
        return "PortablePixelMap({0})".format(self._image_grid.__str__())
    def map_to_image(self, visit):
        new_image = PortablePixelMap(self._image_grid)
        for row_num in range(self._y_size):
            for col_num in range(self._x_size):
                new_image._image_grid[row_num][col_num] = visit(row_num, col_num, new_image._image_grid[row_num][col_num])
        return new_image
    def image_to_file(self, filepath):
        with io.open(filepath, mode="w", encoding="utf-8") as file:
            file.write("P3\n")
            file.write("# Generated with ppm-generator\n")
            file.write("{0} {1}\n".format(self._x_size, self._y_size))
            file.write("{0}\n".format(Color.COLOR_MAX))
            for row in self._image_grid:
                first_col = True
                for color in row:
                    if not first_col:
                        file.write(" ")
                    file.write("{0} {1} {2}".format(color.red, color.green, color.blue))
                    first_col = False
                file.write("\n")
    @property
    def x_size(self):
        return self._x_size
    @property
    def y_size(self):
        return self._y_size

def get_percent_of_total(col_num, total_cols):
    return col_num / total_cols

def get_color_adjustment(percent_of_total, start_color_value, end_color_value):
    return math.floor((end_color_value - start_color_value) * percent_of_total)

def adjust_color_value(col_num, total_cols, start_color_value, end_color_value):
    return start_color_value + get_color_adjustment(get_percent_of_total(col_num, total_cols), start_color_value, end_color_value)

def adjust_color(col_num, total_cols, original_color, end_color):
    return Color(
        adjust_color_value(col_num, total_cols, original_color.red, end_color.red),
        adjust_color_value(col_num, total_cols, original_color.green, end_color.green),
        adjust_color_value(col_num, total_cols, original_color.blue, end_color.blue)
    )

def main():
    ppm = PortablePixelMap.create_portable_pixel_map(400,800,GREEN)
    ppm.map_to_image(lambda row_num, col_num, color: adjust_color(col_num, ppm.x_size, color, RED))
    ppm.image_to_file("image.ppm")

if __name__ == "__main__":
    main()
