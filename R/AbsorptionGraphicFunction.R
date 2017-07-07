#' Plotting absorption by plant and value stream
#' 
#' Creates a stacked bar chart of absorption numbers by value stream.  The plot is facetted by plant and provides 2 points for comparison.  The chart can be used to compare actuals against a forecast or two forecasts.
#' 
#' @param x dataframe with 4 columns. The columns should be as follows: 
#' \itemize{
#' \item{Value Stream- these shoule be abbreviations.  The values will be used as labels on the chart}
#' \item{Comparable- this is likely the actuals or new forecast}
#' \item{Comparable- this is likely the forecast or old forecast}
#' \item{Plant- Abbreviation of the plant name.  One of Rar, Roc, or Penc} }
#' @param labels vector of lenghth two.  Labels for the stacked bars
#' @param barwidth width of bars
#' @param col color scheme
#' @export
#' @return ggpplot object
#' @examples 
#' dat<- data.frame(VS= paste0("VS", 1:3),
#'                  comp1= c(4:6)*1000,
#'                  comp2= c(5:7)*1000,
#'                  Plant= rep("Rar",3))
#' AbsorptionGraphic(dat, c("FCST","ACT"))


AbsorptionGraphic<- function(x,labels, barwidth=.7, col= c(rgb(192,0,0, maxColorValue= 255), rgb(166,166,166, maxColorValue= 255))){

  
     # validate the inputs ----
  if (ncol(x)!=4){
    stop("x should have 4 columns: \n 1. Value Stream \n 
         2. Comparable \n 3. Base \n 4. Plant")
  }
  
  if (length(labels)!=2){
    stop("Provide 2 labels for the chart")
  }
  
  # -----
  # standardize the input data frame
  names(x)<- c("VS","comparable","base","Plant")
  x[,2:3]<- x[,2:3]/1000
  x$Dif <- round(x$comparable- x$base, 1)
  x$PCT <- x$Dif/ x$base * 100
  
  # ----
  y<- melt(x, id.vars=c("VS","Plant","PCT","Dif"),
                     variable.name="cat",
                     value.name="valu")
  y<- y[order(xtfrm(y$Plant), xtfrm(y$cat), -xtfrm(y$VS)),  ]
  y$cum<- do.call(c, by(y$valu,y[,c("cat","Plant")],cumsum))
  
  # ----
  # convert factors
  y$VS <- factor(y$VS)
  y$cat<- factor(y$cat, labels=labels)
  y$Plant<- factor(y$Plant, 
                   levels=c("Rar","Roc","Pen"), 
                   labels=c("Raritan","Rochester","Pencoed"))
  
  # ----
  # Prep for plotting
  cols<- colorRampPalette(col,space="rgb")
  
  
  # ----
  # Plotting
  n<- nrow(unique(y[, c("Plant","VS")]))
  
  
  a<- ggplot(y)
  a<- a + aes(x=cat,y=valu, group=VS, fill=VS) 
  a<- a + geom_col(width = barwidth, colour="black") 
  a<- a + scale_fill_manual(values=cols(n), levels(y$VS))
  a<- a + geom_text(aes(x= 1,
                        y= (cum- valu/2),
                        label=paste(VS,
                                    paste0("$",format(round(valu,1),big.mark=",")), 
                                    sep=", ")),
                    size=3,
                    fontface="bold",
                    colour="white",
                    data= y[which(y$cat==labels[1]), ])
  a<- a + geom_text(aes(x= 2,
                        y= cum - valu/2,
                        label=paste(paste0("$",format(Dif,big.mark=",")),
                                    paste0(round(PCT,0),"%"), 
                                    sep=", ")),
                    size=3,
                    fontface="bold",
                    colour="white",
                    data= y[which(y$cat==labels[2]), ])  
  a<- a + guides(fill=F)  
  a<- a + geom_line(aes(x = (1 - barwidth) * as.numeric(factor(cat)) + 1.5 * barwidth, 
                        y = cum, 
                        group = VS),
                    data= y,
                    inherit.aes = FALSE)
  a<- a + facet_grid(.~Plant)
  a<- a + theme(panel.background = element_rect(fill=NA),
                axis.ticks = element_blank(),
                axis.text.y = element_blank(),
                axis.title = element_blank(),
                axis.text.x = element_text(angle=0,size=15, face= "bold"),
                panel.spacing= unit(.3,"lines"),
                plot.title = element_text(hjust = 0.5),
                strip.background=element_rect(fill=rgb(192,0,0, maxColorValue=255)),
                strip.text = element_text(colour = "White",
                                          size=15))
  a
  
  }

