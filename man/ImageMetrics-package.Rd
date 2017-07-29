\name{ImageMetrics-package}
\alias{ImageMetrics-package}
\alias{ImageMetrics}
\docType{package}
\title{ 
ImageMetrics - Image metrics for social scientists
}
\description{
Provides researchers a number of useful features for the study of images in social science.  Allows researchers to import image data from .jpg or .png files using the png and ReadImages packages, allows researchers to generate RBG and HSV readings for hand-selected portions of an image, which is useful when analyzing particular objects in an image like a face.        
}
\details{
\tabular{ll}{
Package: \tab ImageMetrics\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2011-03-14\cr
License: \tab GNU\cr
LazyLoad: \tab yes\cr
}

This package can be used in analyzing a corpus of images.  Perhaps most useful for social scientists are the capabilities to generate HSV (Hue, Saturation, and Value (brightness)) metrics to describe each image or a particular portion of an image.  For example, in Messing et al. (2009), a similar approach was used to document that the 2008 McCain campaign used more images that portrayed Obama's skin complexion as darker than normal in attack ads, and that both campaigns used images with markedly low color saturation in attack advertisements.  

To select a portion of an image prior to generating these metrics, follow the approach outlined in the package vignette.     

}
\author{
Solomon Messing
Maintainer: Solomon Messing [last-name] at stanford dot edu
}
\references{
Messing, S., Plaut, E., & Jabon, M. (2009). Bias in the Flesh: Attack Ads in the 2008 Presidential Campaign. In Proceedings of the 2009 American Political Science Association Annual Meeting.
}
\keyword{ ImageMetrics images hsv rgb  }
\seealso{
	\code{\link[ReadImages:ReadImages-package]{ReadImages}, \link[png:readPNG]{png}}
}
\examples{

data(Campaign2008)

## Compare brightness of video still capture from MSNBC footage of Obama, with still capture from 
## an ad featuring the same footage (only Obama appears darker) that appeared in an ad on the Clinton 
## campaign website, causing a stir in the blogosphere.
  
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
