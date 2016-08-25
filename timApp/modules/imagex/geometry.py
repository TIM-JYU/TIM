# -*- coding: utf-8 -*-

import math

__author__ = 'iltapeur'
"""
This includes methods for calculating stuff related to rectangles and ellipses,
such as whether or not a point is inside a rectangle or and ellipse.
Take note that angles are calculated using radians.
Made for the image dragging excercice, although you can do whatever the hell you want with it i suppose.
See: https://tim.jyu.fi/view/tim/TIMin%20kehitys/Kuvaraahausteht%C3%A4v%C3%A4n%20speksi
"""


# Checks if point is inside a given object
def is_inside(type, size, angle, center, point):
    # If length table contains only one number append it with the same number so that height = width.
    if len(size) == 1:
        size.append(size[0])
    # Do rectangle stuff
    if type == "rectangle" or type == "":
        r = Rectangle(size, angle, center)
        if r.is_inside(point):
            return True
        else:
            return False
    # Do ellipse stuff
    if type == "ellipse":
        e = Ellipse(size, angle, center)
        if e.is_inside(point):
            return True
        else:
            return False
    # Add other shapes

    return False


# Class for rectangles
class Rectangle:

    # Create rectangle
    def __init__(self,size,angle,center):
        self.size = size
        # Corners
        self.cornertopright = [center[0]+size[0]/2,center[1] - size[1]/2]
        self.cornertopleft = [center[0] - size[0] /2, center[1] - size[1] /2]
        self.cornerbottomright = [center[0] + size[0] /2, center[1] + size[1] /2]
        self.cornerbottomleft = [center[0] - size[0] /2, center[1] + size[1] /2]
        # Center of the rectangle, cosines and sines of the angles.
        self.center = center
        self.sina = math.sin(-math.radians(angle))
        self.cosa = math.cos(-math.radians(angle))

    # Check if point is inside this rectangle
    def is_inside(self,point):
        # Rotate point
        rotatedx= self.cosa *(point[0]-self.center[0])-self.sina*(point[1]-self.center[1])
        rotatedy=self.cosa*(point[1]-self.center[1]) + self.sina*(point[0]-self.center[0])
        # Do the check on whether the point is inside the rectangle
        if (rotatedx + self.center[0] >= self.cornertopleft[0] and rotatedx + self.center[0] <= self.cornertopright[0]
        and rotatedy + self.center[1] >= self.cornertopleft[1] and rotatedy + self.center[1] <= self.cornerbottomright[1]):
            return True
        return False


# Class for ellipses
class Ellipse:

    # Initialize object
    def __init__(self,size,angle,center):
        # height, width, center and angle of the ellipse
        self.size = size
        self.angle = angle
        self.center = center
        # Sines and cosines.
        self.sina = math.sin(-math.radians(angle))
        self.cosa = math.cos(-math.radians(angle))

    # Check if point is inside this ellipse.
    def is_inside(self,point):
        rotatedx = self.cosa * (point[0] - self.center[0]) - self.sina * (point[1] - self.center[1])
        rotatedy = self.cosa * (point[1]-self.center[1]) + self.sina * (point[0] - self.center[0])

        if ((math.pow(rotatedx, 2) / math.pow(self.size[0]/2, 2)) +(math.pow(rotatedy, 2) / math.pow(self.size[1]/2, 2))) <= 1.0:
            return True
        return False
