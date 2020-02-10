make_path <- function(x, y, scale, transx=NA, transy=NA){
   x <- x*scale
   y <- (-y)*scale
   if(is.na(transx)){
      transx <- min(x)
   }
   if(is.na(transy)){
      transy <- min(y)
   }
   x <- x-transx+lwd
   y <- y-transy+lwd
   i <- 1
   pts <- paste0('M', x[i], ',', y[i])
   for(i in 2:length(x)){
      pts <- c(pts, paste0('L', x[i], ',', y[i]))
   }
   attr(pts, "trans") <- c(transx, transy)
   return(pts)
}

scale <- 8

fill <- "darkred" #"##800000"
lcol <- "black" #"#000000"
bg <- "white" #"#FFFFFF"
lwd <- 2*scale

## SVG ----
tw <- sprintf(
   '<svg id="logo" width="%s" height="%s">',
   (30*2*scale)+(lwd*2), (30*2*scale)+(lwd*2)
)

## Hexagon ----
alpha <- c(seq(1, 11, by=2), 1)*pi/6
x <- 0+cos(alpha)*30
y <- 3+sin(alpha)*30
pts <- make_path(x, y, scale)
transx <- attr(pts, "trans")[1]
transy <- attr(pts, "trans")[2]
pts <- c(pts, "Z")
pts <- paste(pts, collapse=" ")
path <- sprintf(
   '<path d="%s" stroke="%s" stroke-width="%s" fill="%s" stroke-linecap="round"/>',
   pts, fill, lwd, bg
)
tw <- c(tw, path)

# ## Title ----
x <- 0
y <- 15
x <- x*scale
y <- (-y)*scale
x <- x-transx+lwd
y <- y-transy+lwd
path <- sprintf(
   paste0(
      '<text x="%s" y="%s" ',
      'text-anchor="middle" fill="%s" ',
      'stroke="%s"',
      'font-size="%s" ',
      'font-family="Geneva"',
      '>',
      'ReDaMoR',
      '</text>'
   ),
   x, y, fill,
   fill,
   lwd*4
)
tw <- c(tw, path)

# ## Heart ----
alpha <- seq(0, 2*pi, length.out=100)
x <- 16*sin(alpha)^3
y <- 13*cos(alpha)-5*cos(2*alpha)-2*cos(3*alpha)-cos(4*alpha)
pts <- make_path(x, y, scale, transx, transy)
pts <- c(pts, "Z")
pts <- paste(pts, collapse=" ")
path <- sprintf(
   '<path d="%s" stroke="%s" stroke-width="%s" fill="%s" stroke-linejoin="round" />',
   pts, lcol, lwd, fill
)
tw <- c(tw, path)

## Point ----
x <- 8
y <- 0.5*x
x <- x*scale
y <- (-y)*scale
x <- x-transx+lwd
y <- y-transy+lwd
path <- sprintf(
   '<circle cx="%s" cy="%s" r="%s" stroke="%s" fill="%s" />',
   x, y, lwd, lcol, lcol
)
tw <- c(tw, path)

## Arrow out ----
x <- c(8, 20)
y <- 0.5*x
pts <- make_path(x, y, scale, transx, transy)
pts <- paste(pts, collapse=" ")
path <- sprintf(
   '<path d="%s" stroke="%s" stroke-width="%s" stroke-linecap="round" />',
   pts, lcol, lwd
)
tw <- c(tw, path)
##
x <- c(17, 22, 19, 17)
y <- c(11, 11, 7, 11)
pts <- make_path(x, y, scale, transx, transy)
pts <- c(pts, "Z")
pts <- paste(pts, collapse=" ")
path <- sprintf(
   '<path d="%s" stroke="%s" stroke-width="%s" fill="%s" />',
   pts, lcol, lwd/2, lcol
)
tw <- c(tw, path)

## Arrow in ----
x <- c(-8, -15)
y <- 0.5*x
pts <- make_path(x, y, scale, transx, transy)
pts <- paste(pts, collapse=" ")
path <- sprintf(
   '<path d="%s" stroke="%s" stroke-width="%s" stroke-linecap="round" />',
   pts, lcol, lwd
)
tw <- c(tw, path)
##
x <- c(-17, -12, -15, -17)
y <- c(-6, -6, -10, -6)
pts <- make_path(x, y, scale, transx, transy)
pts <- c(pts, "Z")
pts <- paste(pts, collapse=" ")
path <- sprintf(
   '<path d="%s" stroke="%s" stroke-width="%s" fill="%s" />',
   pts, lcol, lwd/2, lcol
)
tw <- c(tw, path)

## SVG ----
tw <- c(tw, '</svg>')
doc <- htmltools::htmlTemplate(
   "supp/logo/ReDaMoR-template.html", svg=htmltools::HTML(tw)
)
write(as.character(doc), ncolumns=1, file="supp/logo/ReDaMoR.html")
