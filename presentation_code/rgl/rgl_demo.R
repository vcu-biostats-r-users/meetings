# install.packages('rgl')
library(rgl)

# check out a couple of examples  -----------------------------------------
example(plot3d)
example(surface3d)

# let's create our own 3d scatterplot ---------------------------------------

# create data
a <- 5
back.df <- data.frame(x = seq(0, 5, length.out = 200) + rnorm(200)/a, 
                      y = rnorm(200)/a, z = rnorm(200)/a,
                      section = rep(1, 200))

leg.df <- data.frame(x = seq(3, 5, length.out = 100) + rnorm(100)/a, 
                     y = seq(3, 5, length.out = 100)*.75 + rnorm(100)/a -1,
                     z = rnorm(100)/a,
                     section = rep(2, 100))

loop.df <- data.frame(x = seq(0, 3.5, length.out = 100) + rnorm(100)/a,
                      y = sqrt(1.75^2 - (seq(0, 3.5, length.out = 100) - 1.75)^2 ) + rnorm(100)/a,
                      z = rnorm(100)/a,
                      section = rep(3, 100))

r.df <- rbind(back.df, leg.df, loop.df)

# plot 3d specifying x, y, and z coords
plot3d(x = r.df$x, xlab = '',
       y = r.df$y, ylab = '',
       z = r.df$z, zlab = '',
       col = r.df$section)

# take a snapshot and save (snapshot has some problems in Windows 7)
# beware large number of points for rgl.postscript as this can get unwieldy!
rgl.bringtotop()
# rgl.snapshot('3D_R.png')
rgl.postscript('3d_R.pdf', fmt = 'pdf') 


# we can also use scatterplot for exploratory analysis using PCA ------------

data(iris)
pc.scores <- prcomp(iris[, (-5)])$x[, 1:3] # get first 3 PC scores

# plot first 3 PCs colored by species
plot3d(x = pc.scores[, 1], xlab = 'PC1',
       y = pc.scores[, 2], ylab = 'PC2',
       z = pc.scores[, 3], zlab = 'PC3',
       col = as.numeric(iris$Species))
axes3d()

# fit example 3d surface and plot -------------------------------------------

# create data
points <- seq(-2, 0, length=20)
XY <- expand.grid(X=points,Y=-points)
Zf <- function(X,Y) 2./exp((X-.5)^2+Y^2)-2./exp((X+.5)^2+Y^2)
Z <- Zf(XY$X, XY$Y)
zlim <- range(Z)
zlen <- zlim[2] - zlim[1] + 1

# set colors
jet.colors <- 
  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
colorzjet <- jet.colors(100)

# plot surface with graded colors
open3d()
rgl.surface(x=points, 
            y=matrix(Z,20), 
            z=-points, 
            coords=c(1,3,2),
            color=colorzjet[ findInterval(Z, seq(min(Z), max(Z), length=100))] )
axes3d()

# we can change the lighting on the surface
clear3d(type = "lights")
light3d(theta=0, phi=80) # change the angle
light3d(theta=0, phi=0)  # add a second light coming from the top

# add background grids to either front or back of each axis indicated by "+"
grid3d("x")
grid3d("y+")
grid3d("z")

# fit non-parametric smoothing surface ------------------------------------

# create wiggly fuzzy ribbon
x <- runif(300)*2*pi
y <- sin(x) + rnorm(300)/10
z <- runif(300)*2*pi

plot3d(x, y, z)

# fit non-parametric local regression surface
surface.df <- data.frame(x, y, z)
loess.fit <- loess(y ~ x*z, span=.4, data=surface.df)

# predict values on a grid that spans range of x & z
x.seq <- seq(min(x), max(x), length.out = 15)
z.seq <- seq(min(z), max(z), length.out = 15)
predict.df <- expand.grid(x = x.seq, z = z.seq)

pred.loess <- predict(loess.fit, predict.df)

# open3d()
# plot3d(x, y, z)
rgl.surface(x=x.seq, 
            y=pred.loess, 
            z=z.seq)
axes3d()

rgl.pop()  # we can remove the surface if we don't like it

# spin it around
movie3d(spin3d(), duration = 10)

# get many different angles
M <- par3d("userMatrix")
play3d( par3dinterp( 
  userMatrix=list(M, rotate3d(M, pi/2, 1, 0, 0), rotate3d(M, pi/2, 0, 1, 0) ) ), 
  duration=4 )


# create surface plot using lattice ---------------------------------------
# install.package('lattice')
library(lattice)

x11()
wireframe(c(pred.loess) ~ predict.df$x * predict.df$z,
          xlab = "X",
          ylab = "Z",
          zlab = 'Y',
          main = "Surface Fit",
          drape = TRUE,
          colorkey = TRUE,
          screen = list(z = -60, x = -60),
          scale = list(arrows = FALSE))

