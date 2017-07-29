\name{ObjSelect}
\alias{ObjSelect}
\alias{height}
\alias{width}
\alias{polyx}
\alias{polyy}
\title{ Selects objects from within an image }
\description{ 
  \code{ObjSelect} selects an object with the polygon provided from an image.
}

\usage{
ObjSelect(image,poly)
}

\arguments{
  \item{image}{ the "@X" array of an imageMatrix object }
  \item{poly}{ an a polygon represented as a vector of concatenated x coordinates and y coordinates }
}
\value{
  A matrix or three dimensional array representing only the object in the image.  All pixels not in the polygon are set to NA.   
}

\author{Solomon Messing <[last-name] at stanford dot edu>}

\note{ This function relies on the maps package's point-in-polygon c-code for rapid selection}
\examples{

data(Campaign2008)

## User clicks around an object via tcktk device to define a polygon:
clinton = new("imageMatrix", X = readPNG(system.file("extdata", "Clinton.png", package="ImageMetrics")), type = "rgba")

# Select object using polygon:
clintonfacearray = ObjSelect( image = clinton@X, poly= clintonpoly )

# create new image with only the object selected.
clintonface = new("imageMatrix", X = clintonfacearray, type = "rgb")

\dontrun{
plot(clintonface)
}

}
\keyword{ setObjPoly getObjPoly polygon }

