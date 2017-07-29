\name{meanhsv}
\alias{meanrgb}
\alias{meanhsv}
\title{ Compute mean HSV and RGB of image/object }
\description{ 
  \code{meanrgb} returns the mean RGB values for the imageMatrix object.
  \code{meanHSV} returns the mean HSV values for the imageMatrix object.
}

\usage{
meanrgb(imageMatrix)
meanhsv(imageMatrix)
}

\arguments{
  \item{imageMatrix}{ an imageMatrix object }
}
\value{
  A list with three elements corresponding to the HSV or RGB measures in the imageMatrix.   
}

\author{Solomon Messing <[last-name] at stanford dot edu>}

\examples{
## Compare brightness of video still capture from MSNBC footage of Obama, with still capture from 
## an ad featuring the same footage (only Obama appears darker) that appeared in an ad on the Clinton 
## campaign website, causing a stir in the blogosphere.

data(Campaign2008)
 
clinton = new("imageMatrix", X = readPNG(system.file("extdata", "Clinton.png", package="ImageMetrics")), type = "rgba")
clintonface = new("imageMatrix", X = ObjSelect( image = clinton@X, poly= clintonpoly ), type = "rgb")

msnbc = new("imageMatrix", X = readPNG(system.file("extdata", "MSNBC.png", package="ImageMetrics")), type = "rgba")
msnbcface = new("imageMatrix", X = ObjSelect( image = msnbc@X, poly= msnbcpoly ), type = "rgb")

\dontrun{
plot(clinton)
plot(clintonface)
plot(msnbc)
plot(msnbcface)
}

meanhsv(clintonface)
meanhsv(msnbcface)


}
\keyword{ setObjPoly getObjPoly polygon }

