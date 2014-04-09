Interactive 3D Graphics in R with rgl
========================================================
author: Paul Manser
date: 4/10/2014

Introduction
========================================================
- Why interactive 3D graphics?
    - Exploratory data analysis of high-dimensional data
    - Easy response surface plotting and adjustment
    - Create topographic maps
    - Make your poster cooler
    - Impress your friends

OpenGL
========================================================

- First released in 1992
- Cross-language multi-platform graphics API
- Industry standard for portable interactive 3D graphics
    - Counterstrike
    - Minecraft
    - Google Earth
- Still actively developed

rgl
========================================================

- R visualization device with OpenGL renderer
- Real-time 3D engine written in C++
- Command line graphics creation with point and click interaction
- Maintained by Duncan Murdoch from the University of Western Ontario

A few things rgl can do
========================================================

- Interactive 3D scatterplots, surfaces, and shapes
- Up to 8 light sources
- Alpha-blending (transparency)
- Environmental effects (fog)
- An undo function
- Create short movies
