# -*- coding: utf-8 -*-

import math

__author__ = 'iltapeur'
"""
This includes methods for calculating stuff related to rectangles and ellipses,
such as whether or not a point is inside a rectangle or and ellipse.
Made for the image dragging excercice, although you can do whatever the hell you want with it i suppose.
See: https://tim.jyu.fi/view/tim/TIMin%20kehitys/Kuvaraahausteht%C3%A4v%C3%A4n%20speksi
"""

# Checks if point is inside a given object
def isInside(type,size,angle,center,point):
    if(len(size) == 1):
        size.append(size[0])
    # Do rectangle stuff
    if type == "rectangle" or type == "":
        r = Rectangle(size,angle,center)
        if r.isInside(point):
            return True
        else:
            return False
    # Do ellipse stuff
    if type == "ellipse":
        e = Ellipse(size,angle,center)
        if e.isInside(point):
            return True
        else:
            return False
    #Add other shapes


    return False

# Class for rectangles
class Rectangle:
    #corners, [0] is x and [1] is y coordinate.
    #cornertopright = [0,0]
    #cornertopleft = [0,0]
    #cornerbottomright = [0,0]
    #cornerbottomleft = [0,0]
    #center of the rectangle
    #center = [0,0]
    #angle of the rectangle in degrees.
    #angle = 0

    #Create rectangle
    def __init__(self,size,angle,center):
        self.size = size
        #Corners
        self.cornertopright = [center[0]+size[0]*(0.5),center[1] - size[1]*(0.5)]
        self.cornertopleft = [center[0] - size[0] * (0.5), center[1] - size[1] * (0.5)]
        self.cornerbottomright = [center[0] + size[0] * (0.5), center[1] + size[1] * (0.5)]
        self.cornerbottomleft = [center[0] - size[0] * (0.5), center[1] + size[1] * (0.5)]
        # center of the rectangle
        self.center = center
        self.sina = math.sin(-math.radians(angle))
        self.cosa = math.cos(-math.radians(angle))



    #Check if point is inside rectangle
    def isInside(self,point):

        rotatedx= self.cosa *(point[0]-self.center[0])-self.sina*(point[1]-self.center[1])
        rotatedy=self.cosa*(point[1]-self.center[1]) + self.sina*(point[0]-self.center[0])
        if((rotatedx + self.center[0] >= self.cornertopleft[0] and rotatedx + self.center[0] <= self.cornertopright[0]
        and rotatedy + self.center[1] >= self.cornertopleft[1] and rotatedy + self.center[1] <= self.cornerbottomright[1])):
            #print("Inside rect")
            return True
        #print("outside rect")
        return False





#Class for ellipses
class Ellipse:
    #height, width and angle of the ellipse
    #size = [0,0]
    #angle = 0
    # Center of the ellipse
    #center = [0,0]

    #Initialize object
    def __init__(self,size,angle,center):
        # height, width, center and angle of the ellipse
        self.size = size
        self.angle = angle
        self.center = center
        self.sina = math.sin(-math.radians(angle))
        self.cosa = math.cos(-math.radians(angle))
        print("KOKO:"+ str(size))

    #Initializes object and checks if the point is inside the ellipse
    def isInside(self,point):
        rotatedx = self.cosa * (point[0] - self.center[0]) - self.sina * (point[1] - self.center[1])
        rotatedy = self.cosa * (point[1]-self.center[1]) + self.sina * (point[0] - self.center[0])

        if(((math.pow(rotatedx, 2) / math.pow(self.size[0], 2)) +(math.pow(rotatedy, 2) / math.pow(self.size[1], 2)))) <= 1.0:
            #print("inside ellipse")
            return True
        #print("outside ellipse")
        return False


