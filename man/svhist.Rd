\name{svhist}
\alias{svhist}
\alias{huehist}
\alias{rgbhist}
\title{ Histogram showing HSV and RGB for image/object }
\description{ 
  \code{svhist} plots the density of the color saturation and value (brightness) for the imageMatrix object.
  \code{huehist} plots the density of the hue for the imageMatrix object.
  \code{rgbhist} plots the density of red, green, and blue channels for the imageMatrix object.
}

\usage{
svhist(imageMatrix, ...)
huehist(imageMatrix, ...)
rgbhist(imageMatrix, ...)
}

\arguments{
  \item{imageMatrix}{ an imageMatrix object }
  \item{...}{ extra parameters, e.g., to be passed to plot }
}
\value{
  Plots the density of the image features corresponding to the function specified.    
}

\author{Solomon Messing <[last-name] at stanford dot edu>}

\examples{
\dontrun{

## Compare brightness of video still capture from MSNBC footage of Obama, with still capture from 
## an ad featuring the same footage (only Obama appears darker) that appeared in an ad on the Clinton 
## campaign website, causing a stir in the blogosphere.

data(Campaign2008)
 
clinton = new("imageMatrix", X = readPNG(system.file("data", "Clinton.png", package="ImageMetrics")), type = "rgba")
clintonface = new("imageMatrix", X = ObjSelect( image = clinton@X, poly= clintonpoly ), type = "rgb")

msnbc = new("imageMatrix", X = readPNG(system.file("data", "MSNBC.png", package="ImageMetrics")), type = "rgba")
msnbcface = new("imageMatrix", X = ObjSelect( image = msnbc@X, poly= msnbcpoly ), type = "rgb")

svhist(clintonface, main="Clinton")
svhist(msnbcface, main="MSNBC")
}
}
\keyword{ hsv rgb histogram density imageMatrix ImageMetrics }

