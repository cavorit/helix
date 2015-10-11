#' @author HF nach Felix Schönbrodt  
#' @description Zeichnet stanine-Plot
#' @details Zeichnet eine Glockenkurve mit Stanine-Klassen in Cavorit-Farben
#' @title staninePlot
staninePlot <- function(){
  
  # First: Calculate stanine breaks (on a z scale)
  stan.z <- c(-3, seq(-1.75, +1.75, length.out=8), 3)
  
  # Second: get cumulative probabilities for these z values
  stan.PR <- pnorm(stan.z)
  
  # define a color ramp from blue to red (... or anything else ...)
  orange <- rgb(200/255, 100/255, 0/255, 1)
  pantolgreen <- rgb(13/255, 83/255, 97/255)
  c_ramp <- colorRamp(c(orange, pantolgreen), space="Lab")
  
  # draw the normal curve, without axes; reduce margins on left, top, and right
  par(mar=c(2,0,0,0))
  curve(dnorm(x,0,1), xlim=c(-3,3), ylim=c(-0.03, .45), xlab="", ylab="", axes=FALSE)
  
  # Calculate polygons for each stanine region
  # S.x = x values of polygon boundary points, S.y = y values
  for (i in 1:(length(stan.z)-1)) {
    S.x  <- c(stan.z[i], seq(stan.z[i], stan.z[i+1], 0.01), stan.z[i+1])
    S.y  <- c(0, dnorm(seq(stan.z[i], stan.z[i+1], 0.01)), 0)
    polygon(S.x,S.y, col=rgb(c_ramp(i/9), max=255))
  }
  
  # print stanine values in white
  # font = 2 prints numbers in boldface
  text(seq(-2,2, by=.5), 0.015, label=1:9, col="white", font=2)
  
  # print cumulative probabilities in black below the curve
  text(seq(-1.75,1.75, by=.5), -0.015, label=paste(round(stan.PR[-c(1, 10)], 2)*100, "%", sep=""), col="black", adj=.5, cex=.8)
  text(0, -0.035, label="Percentage of sample <= this value", adj=0.5, cex=.8)
  
}





