# TODO: Add comment
# 
# Author: user01
###############################################################################

library(ReadImages)
library(png)
library(maps)
require(tcltk)
require(tkrplot)

# TODO: try putting these vars in the setObjPoly function.
# Set up global variables for the polygon
pkgVars <- new.env(parent=emptyenv())
getPolyX <- function() get("polyx", pkgVars, inherits=FALSE)
getPolyY <- function() get("polyy", pkgVars, inherits=FALSE)
getWidth <- function() get("width", pkgVars, inherits=FALSE)
getHeight <- function() get("height", pkgVars, inherits=FALSE)

setPolyX <- function(value) assign("polyx", value, envir=pkgVars)
setPolyY <- function(value) assign("polyy", value, envir=pkgVars)
setWidth <- function(value) assign("width", value, envir=pkgVars)
setHeight <- function(value) assign("height", value, envir=pkgVars)


setObjPoly = function(image){
	
	setPolyX(c())
	setPolyY(c())
	setWidth(numeric())
	setHeight(numeric())
	# preliminaries
	tt <- tktoplevel()
	tkwm.title(tt,"Click on a point to start the polygon, close the window to complete it")
	parPlotSize <- c()
	usrCoords <- c()
	
	# plot function needs to be defined - set so there is no margin
	plotFunction <- function(){
		params <- par(bg="white", oma=c(0,0,0,0), mar=c(0,0,0,0))
		plot(image)	
	}
	
	# More preliminaries
	img <- tkrplot(tt,fun=plotFunction,hscale=1,vscale=1)
	tkgrid(img)
	
	# Make sure we have tcltk loaded (leftover from other code)
	require(tcltk)
	setWidth( as.numeric(tclvalue(tkwinfo("reqwidth",img))) )  
	setHeight( as.numeric(tclvalue(tkwinfo("reqheight",img))) )
	
	# Do this when we left-click
	OnLeftClick <- function(x,y){
		setPolyX( c( getPolyX(), x ) ) 
		setPolyY( c( getPolyY(), y ) ) 
	}
	
	# bind the mouse button to that function
	tkbind(img, "<Button-1>",OnLeftClick)
	
	# set the mouse cursor to a hand
	tkconfigure(img,cursor="hand2")
	
	# TODO: after each click, draw a dot on screen.  
	# TODO: set it up so that users need not run getObjPoly separately. Something like:
	#	tkbind(img, "<Return>", getObjPoly )
}

getObjPoly = function(image){
	# Convert screen coords to image coords
	polyx = as.numeric(getPolyX())
	polyy = as.numeric(getPolyY())
	
	polyxp = polyx/getWidth() 
	polyyp = polyy/getHeight()
	
	polyxf = round(polyxp*image@nrow, 0)
	polyyf = round(polyyp*image@ncol, 0)
	
	return(c(polyxf, polyyf))
}


ObjSelect= function(image, poly){

	poly = as.numeric(poly)
	poly = na.omit(poly)
	
	# get polygon object
	poly1 = makepoly( data.frame(x=poly[1:(length(poly)/2)], y=poly[(length(poly)/2 + 1):length(poly)]), 0, TRUE )
	
	# return points in polygon... need a vector corresponding to each point...
	# first allocate two matrices corresponding to X and Y coordinates of the image 
	INX = matrix(1:dim(image)[1], nrow = dim(image)[1], ncol = dim(image)[2])
	INY = matrix(1:dim(image)[2], nrow = dim(image)[1], ncol = dim(image)[2], byrow = T)
	
	# Strip out structure so they are 1 x (nxn) vectors
	INX = as.vector(INX)
	INY = as.vector(INY)
	
	# Put the points back together so they correspond to a paired list:
	INXY = data.frame(INY,INX) # Must give Y as first vector, then X as second)
	
	# Return whether each point is in the polygon, put the matrix structure back
	IN = matrix(in.polygon(poly1, INXY), nrow = dim(image)[2], ncol = dim(image)[1])
	
	#TODO: Add flow control for b&w v color 
	
	# Create an array that will enable us to index the image 
	inmat = array(FALSE, c(dim(image)[1], dim(image)[2], dim(image)[3]))
	
	# Fill each level of the array (corresponding to each layer of the image (r,g,b, and possibly a).
	for(i in 1:length(image[1,1,])){
		inmat[,,i] = IN
	}
	
	# Now get set all other pixels to NA
	objsel = image
	objsel[!inmat] = NA
	
	# and return the object
	return(objsel)
}

setClass("imageMatrix", contains = "structure", 
		representation( X = "array", type = "character", imgdim = "numeric" , ncol= "numeric", nrow = "numeric") )

setMethod("initialize", 
		signature(.Object = "imageMatrix"),
		function (.Object, X, type=NULL, ncol=dim(X)[1], nrow=dim(X)[2], ...) 
		{
			# Check things:
			if(is.null(X) | is.null(type)){
				stop("X (image data) and type must be defined")
			}
			if (is.null(dim(X)) & is.null(type)){
				stop("Must specify type or provide an image object with structure.")
			} 
			if (length(dim(X)) == 2 & is.null(type)){
				type <- "grey"
			} 
			if (length(dim(X)) == 3 & is.null(type)){
				type <- "rgb"
			} 
			if (length(dim(X)) == 3 & dim(X)[3]==4 & is.null(type) ){
				type <- "rgba"
			} 
			if (type != "rgb" & type != "rgba" & type != "grey"){
				stop("Error, check matrix type.")
			} 
			
			# Set dimensions
			imgdim <- c(ncol, nrow, if (type == "rgb" | type == "rgba" ) 3 else NULL)
			
			# put it back into an array
			X <- array(X, dim=imgdim)
			
			.Object@X = X
			.Object@type = type
			.Object@imgdim = imgdim
			.Object@ncol = ncol
			.Object@nrow = nrow
			.Object
		}
)

setMethod("show", signature = "imageMatrix",
		function (object) 
		{
			x.dim <- dim(object@X)
			cat("size: ", x.dim[1], "x", x.dim[2], "\n")
			cat("type: ", object@type, "\n")
		}
)
#myimage

setMethod("plot", signature = "imageMatrix",
		function (x) 
		{
			x@X[is.na(x@X)] = 0
			image <- switch(x@type,
					grey=grey(x@X),
					rgb=rgb(x@X[,,1], x@X[,,2], x@X[,,3]),
					rgba=rgb(x@X[,,1], x@X[,,2], x@X[,,3]))
			if (is.null(image)) stop("Problem creating image.")
			colors <- unique(image)
			colmat <- array(match(image, colors), dim=dim(x@X)[1:2])
			if (exists("rasterImage")) { 
				plot(0:1, 0:1, type='n', bty = 'n', xaxt = 'n', yaxt = 'n', xlab = NA, ylab = NA)
				rasterImage(x@X, 0, 0, 1, 1)
			} else{
				image(x = 0:(dim(colmat)[2]), y=0:(dim(colmat)[1]),
						z = t(colmat[nrow(colmat):1, ]), col=colors,
						xlab="", ylab="", axes=FALSE, asp=1)
			}
		}
)

meanrgb = function (imageMatrix) {
	if(imageMatrix@type!="rgb"& imageMatrix@type!="rgba"){
		stop("Image must be in rgb color.")
	}
	meanr = mean(imageMatrix@X[,,1], na.rm=T)
	meang = mean(imageMatrix@X[,,2], na.rm=T)
	meanb = mean(imageMatrix@X[,,3], na.rm=T)
	list(R = meanr, G = meang, B = meanb)
}
#setMethod("meanrgb", signature = "imageMatrix", meanrgb)

meanhsv = function (imageMatrix) {
	if(imageMatrix@type=="grey"){
		return(list(V = mean(imageMatrix$X, na.rm=T)))
	}else{		
		hsvvec = as.numeric(rgb2hsv(t(data.frame(meanrgb(imageMatrix))), maxColorValue = 1))
		list(H = hsvvec[1], S = hsvvec[2], V = hsvvec[3] )
	}
}
#meanhsv(myimage)
#setMethod("meanhsv", signature = "imageMatrix", meanrgb)

rgbhist = function(imageMatrix, ...) {
	if(imageMatrix@type!="rgb"& imageMatrix@type!="rgba"){
		stop("Image must be in rgb color.")
	}
#	cols = c(rgb(1,0,0,.7), rgb(0,1,0,.7), rgb(0,0,1,.7))
	cols = c("red", "green", "blue")
	rd = density(na.omit(as.numeric(imageMatrix@X[,,1])))
	gd = density(na.omit(as.numeric(imageMatrix@X[,,2])))
	bd = density(na.omit(as.numeric(imageMatrix@X[,,3])))
	ymax = max(c(rd$y,bd$y, gd$y), na.rm=T)
	plot(c(-.1,1.1), c(0,ymax), type='n', xlab = "RGB proportions", ylab = "density", ...)
	lines(rd, col = cols[1])
	lines(bd, col = cols[2])
	lines(gd, col = cols[3])
#	if(!exists("main"))  title("RGB density plot")
	
}

svhist = function(imageMatrix, ...) {
	if(imageMatrix@type!="rgb"& imageMatrix@type!="rgba"){
		stop("Image must be in rgb color.")
	}
#	cols = c(rgb(.8,.6,.4,.7), rgb(.9,0,.9,.7), rgb(.5,.5,.5,.7))
	cols = c("brown", "red", "grey")
	hsvm = rgb2hsv(r = na.omit(as.numeric(imageMatrix@X[,,1])), 
			g = na.omit(as.numeric(imageMatrix@X[,,2])), 
			b = na.omit(as.numeric(imageMatrix@X[,,3])), 
			maxColorValue=1)
	sd = density(hsvm[2,])
	vd = density(hsvm[3,])
	ymax = max(c(sd$y, vd$y), na.rm=T)
	plot(c(-.1,1.1), c(0,ymax), type='n', xlab = "Saturation and value proportion", ylab = "density", ...)
	lines(sd, col = cols[2])
	lines(vd, col = cols[3])
#	if(!exists("main")){title("Saturation and value density plot")}  
	legend("topleft", c("Saturation", "Value (brightness)"), col = cols[2:3], lty=1, bty='n')
}

huehist = function(imageMatrix, ...) {
	if(imageMatrix@type!="rgb"& imageMatrix@type!="rgba"){
		stop("Image must be in rgb color.")
	}
#	cols = c(rgb(.7,.6,.4,.8), rgb(.9,0,.9,.7), rgb(.5,.5,.5,.7))
	cols = c("brown", "red", "grey")
	hsvm = rgb2hsv(r = na.omit(as.numeric(imageMatrix@X[,,1])), 
			g = na.omit(as.numeric(imageMatrix@X[,,2])), 
			b = na.omit(as.numeric(imageMatrix@X[,,3])), 
			maxColorValue=1)
	hd = density(hsvm[1,])
	ymax = max(hd$y, na.rm=T)
	plot(c(-.1,1.1), c(0,ymax), type='n', xlab = "Hue proportion", ylab = "density", xaxt= 'n', ...)
	lines(hd, col = cols[1])
	axis(1, at = c(0, .2, .4, .6, .8, 1), labels = c("red", "yellow", "green", "blue", "purple", "red"))
#	if(!exists("main")) title("Hue density plot")
}
