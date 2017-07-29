\name{imageMatrix-class}
\Rdversion{1.1}
\docType{class}
\alias{imageMatrix-class}
\alias{plot,imageMatrix-method}
\alias{show,imageMatrix-method}

\title{Class "imageMatrix"}
\description{
  Makes an imageMatrix object from a matrix. 
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("imageMatrix", X, type, ncol, nrow, ...)}.

  X should be given as a 2 dimensional matrix for grey-scale, 3
  dimensional array for a color image (row, column, rgb channel),   
  X[,,1], X[,,2], X[,,3] are the red, green, and blue planes, respectively.

  The code for this object was inspired by the imagematrix package.
  The major difference between imageMatrix and imagematrix is 
  the S4 representation and additional functionality in terms of plotting.    
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"vector"} ~~ }
    \item{\code{X}:}{Object of class \code{"array"} corresponding to image pixels}
    \item{\code{type}:}{Object of class \code{"character"} corresponding to "rgb", "rgba", or "grey" }
    \item{\code{imgdim}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{ncol}:}{Object of class \code{"numeric"}, the width of the image }
    \item{\code{nrow}:}{Object of class \code{"numeric"}, the height of the image }
  }

}

\section{Extends}{
Class \code{"\linkS4class{structure}"}, directly.
Class \code{"\linkS4class{vector}"}, by class "structure", distance 2, with explicit coerce.
}

\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "imageMatrix")}: ... }
    \item{show}{\code{signature(object = "imageMatrix")}: ... }
	 }
  The plot(imageMatrix) method converts the values to rgb values that R understands and
  then uses rasterImage() to plot, which is a much faster way to view these kinds of
  objects than by using the image() function.
  
  The show() method outputs the image size and type.  
}

\author{Solomon Messing <[last-name] at stanford dot edu>}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{	
	clinton = new("imageMatrix", X = readPNG(system.file("extdata", "Clinton.png", package="ImageMetrics")), type = "rgba")
	plot(clinton)
}
\keyword{ rgb plot imagematrix imagemetrics hsv}
