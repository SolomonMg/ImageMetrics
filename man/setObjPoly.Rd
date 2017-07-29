\name{setObjPoly}
\alias{getObjPoly}
\alias{setObjPoly}
\alias{getHeight} 
\alias{getPolyX}
\alias{getPolyY}
\alias{getWidth}
\alias{pkgVars}
\alias{setHeight} 
\alias{setPolyX}
\alias{setPolyY}
\alias{setWidth}
\title{ Interface to select objects within an image }
\description{ 
  \code{setObjPoly} pulls up a tcltk interface for the user to select an object from the image.
  \code{getObjPoly} returns the coordinates the user selected.
}

\usage{
setObjPoly(image)
getObjPoly(image)
}

\arguments{
  \item{image}{ an imageMatrix object }
}
\value{
  A vector of x coordinates and y coordinates corresponding to the object selected in the image.
}

\author{Solomon Messing <[last-name] at stanford dot edu>}

\note{ These two functions are used to hand-select an object in an image, so that metrics can be extracted. Thanks to 
\url{http://www.sciviews.org/_rgui/tcltk/InteractiveTkrPlot.html}, which outlines many key concepts used for these functions.}
\examples{
\dontrun{
	mcattackad = new("imageMatrix", X = readPNG(system.file("extdata", "jm-sep25-promise1.png", package="ImageMetrics")), type = "rgba")
	setObjPoly(mcattackad) ## opens tcltk interface
	mcattackadpoly = getObjPoly(mcattackad) ## returns polygon corresponding to the object the user selected.
	
	## create new image with only the object selected.
	mcattackadface = new("imageMatrix", X = ObjSelect( image = mcattackad@X, poly= mcattackadpoly ), type = "rgb")
	plot(mcattackadface)
	
	## save the image to the current directory
	getwd() 
	writePNG(mcattackadface@X, "obamaface.png")}
}
\keyword{ setObjPoly getObjPoly polygon }

