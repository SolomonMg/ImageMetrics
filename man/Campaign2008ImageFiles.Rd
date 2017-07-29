\name{Campagin2008}
\alias{clintonpoly}
\alias{msnbcpoly}
\alias{Clinton.png}
\alias{MSNBC.png}
\alias{readings}
\alias{jm-sep19-nothingnew1.png}
\alias{jm-sep19-nothingnew2.png}
\alias{jm-sep19-nothingnew3.png}
\alias{jm-sep19-nothingnew4.png}
\alias{jm-sep21-chicagomachine5.png}
\alias{jm-sep23-mum4.png}
\alias{jm-sep23-mum6.png}
\alias{jm-sep25-promise1.png}
\alias{jm-sep25-promise2.png}
\alias{bo-sep15-fundamentals1.png}
\alias{bo-sep16-soldusout1.png}
\alias{bo-sep18-neededucation2.png}
\alias{bo-sep18-whoadvises1.png}
\alias{bo-sep20-promise1.png}
\alias{bo-sep21-article1.png}

\docType{data}
\title{
Selected image files from the 2008 Presidential campaign: Images (*.png), and coordinates corresponding to polygons that outline a candidate's face.    
}
\description{
Images from 2008 presidential campaign advertisements.  Images from Barack Obama ads begin with "bo" while images from McCain ads begin with "jm."  
Images captured constitute a small selection of images from the Stanford Political Communication Lab (PCL) \url{http://pcl.stanford.edu/}
archive, "Campaign 2008: Presidential Election: McCain vs. Obama," available at \url{http://pcl.stanford.edu/campaigns/2008/}.  Facial coordinates for 
each image are provided in the "readings" dataframe.  V1 corresponds to the file name and each row constitutes a vector corresponding to the 
x- then y- coordinates of each face in each image.  McCain ads featuring Obama are significantly less saturated with color than Obama's own ads 
(as are Obama's ads depicting McCain). 

Two additional images from the 2008 primary, "Clinton.png" and "MSNBC.png" are image captures from MSNBC footage of Obama, which the Clinton campaign
subsequently used in a campaign advertisement.  Obama appeared significantly darker and stretched horizontally in the Clinton ad, which 
served to accentuate his Afrocentric facial features.  This caused quite a stir among Democrat bloggers, most notably "Troutnut," who accused the
Clinton campaign of using racial bias as tool in attack advertising.  See 
\url{http://www.dailykos.com/story/2008/03/04/468408/-UPDATE-w-campaign-response-Hillarys-ad:-debate-footage-doctored-to-make-Obama-blacker}
for details.  The vectors "clintonpoly" and "msnbcpoly" correspond to the x- then y- coordinates of each face in each image.
}
\usage{data(Campaign2008)}
\format{
 .png format
}
\source{
\url{http://pcl.stanford.edu/}
\url{http://www.hillaryclinton.com/}
\url{http://www.msnbc.msn.com/}
}
\references{
Messing, S., Plaut, E., & Jabon, M. (2009). "Bias in the Flesh: Attack Ads in the 2008 Presidential Campaign." 
In Proceedings of the 2009 American Political Science Association Annual Meeting.
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
\keyword{datasets}
