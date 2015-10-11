#' @author HF nach Felix Schönbrodt  
#' @description Zeichnet Gaussglocke mit linker und rechter Farbe
#' @details Zeichnet eine Glockenkurve. Links des Prozentrangs wird alles in einer Farbe eingefäbrt. Rechts davon in einer anderen Farbe.
#' @title staninePlotDichotom
#' @param Prozentrang Der Prozentrang, der abgezeichnet werden soll.
#' @param AlphaKanal Eine Zahl zwischen 0 und 1 für den Alphakanal
staninePlotDichotom <- function(Prozentrang, AlphaKanal=1){
  orange <- rgb(200/255, 100/255, 0/255, alpha = AlphaKanal)
  pantolgreen <- rgb(13/255, 87/255, 93/255, alpha = AlphaKanal)
  
  FarbeLinks <- orange # darkblue
  FarbeRechts <- pantolgreen # red
  
  # draw the normal curve
  curve(dnorm(x,0,1), xlim=c(-3,3), main="Normal density")
  
  # define shaded region
  from.z <- -3
  to.z <- qnorm(.025)
  
  # Links
  S.x  <- c(from.z, seq(from.z, to.z, 0.01), to.z)
  S.y  <- c(0, dnorm(seq(from.z, to.z, 0.01)), 0)
  polygon(S.x,S.y, col=FarbeLinks)
  
  # Rechts
  S.x  <- c(to.z, seq(to.z, 3, 0.01), 3)
  S.y  <- c(0, dnorm(seq(to.z, 3, 0.01)), 0)
  polygon(S.x,S.y, col=FarbeRechts)
}


