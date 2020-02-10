library(extrafont)
loadfonts()

pdf("supp/logo/ReDaMoR.pdf", width=7, height=7)
# png("supp/logo/ReDaMoR.png", width=500, height=500)

par(mar=rep(0,4))
fill <- "#800000"
lcol <- "black"
bg <- "white"
lwd <- 15
plot(
   0, 0, type="n",
   xlim=c(-17, 17), ylim=c(-26, 32),
   xlab="", ylab="", bty="n",
   xaxt="n", yaxt="n",
   asp=1
)
## Hexagon ----
alpha <- c(seq(1, 11, by=2), 1)*pi/6
polygon(
   x=0+cos(alpha)*30,
   y=3+sin(alpha)*30,
   lwd=lwd, col=bg, border=fill
)
## Title ----
text(
   0, 17,
   "ReDaMoR",
   family="DejaVu Serif",
   # vfont=c("serif", "bold italic"),
   # vfont=c("serif", "bold"),
   col=fill,
   adj=rep(0.5, 2),
   cex=5
)
## Heart ----
alpha <- seq(0, 2*pi, length.out=100)
polygon(
   x=16*sin(alpha)^3,
   y=13*cos(alpha)-5*cos(2*alpha)-2*cos(3*alpha)-cos(4*alpha),
   lwd=lwd, col=fill, border=lcol
)
## Arrow ----
x <- 8
points(
   x, 0.5*x, pch=19, col=lcol, cex=3
)
x <- c(8, 20)
points(
   x, 0.5*x,
   type="l", col=lcol, lwd=lwd
)
polygon(
   x=c(17, 20, 19, 17),
   y=c(11, 10, 7, 11),
   col=lcol, lwd=lwd
)
x <- c(-8, -16)
points(
   x, 0.5*x,
   type="l", col=lcol, lwd=lwd
)
polygon(
   x=c(-17, -12, -15, -17),
   y=c(-6, -6, -10, -6),
   col=lcol, lwd=lwd
)

dev.off()
