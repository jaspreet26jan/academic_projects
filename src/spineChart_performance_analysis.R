#Spine Chart of Mobile Data
library("support.CEs", lib.loc="~/R/win-library/3.4")
### ----------------------------------------

spine.chart <- function(conjoint.results,  
                        color.for.part.worth.point = "blue",
                        color.for.part.worth.line = "blue",
                        left.side.symbol.to.print.around.part.worths = "(",
                        right.side.symbol.to.print.around.part.worths = ")",
                        left.side.symbol.to.print.around.importance = "",
                        right.side.symbol.to.print.around.importance = "",
                        color.for.printing.importance.text = "dark red",  
                        color.for.printing.part.worth.text = "black",  
                        draw.gray.background = TRUE,  
                        draw.optional.grid.lines = TRUE,  
                        print.internal.consistency = TRUE,  
                        fix.max.to.4 = FALSE,  
                        put.title.on.spine.chart = FALSE,
                        title.on.spine.chart = paste("TITLE GOES HERE IF WE ASK FOR ONE",sep=""),
                        plot.framing.box = TRUE,  
                        do.standardization = TRUE,  
                        do.ordered.attributes = TRUE) {
  
  # fix.max.to.4  option to override the range for part-worth plotting 
  
  if(!do.ordered.attributes) effect.names <- conjoint.results$attributes   
  if(do.ordered.attributes) effect.names <- 
      conjoint.results$ordered.attributes      
  
  number.of.levels.of.attribute <- NULL
  for(index.for.factor in seq(along=effect.names))
    number.of.levels.of.attribute <- c(number.of.levels.of.attribute,
                                       length(conjoint.results$xlevels[[effect.names[index.for.factor]]]))
  
  # total number of levels needed for vertical length of spine the spine plot
  total.number.of.levels <- sum(number.of.levels.of.attribute)
  
  # define size of spaces based upon the number of part-worth levels to plot
  if(total.number.of.levels <= 20) {
    smaller.space <- 0.01
    small.space <- 0.02
    medium.space <- 0.03
    large.space <- 0.04
  }
  
  if(total.number.of.levels > 20) {
    smaller.space <- 0.01 * 0.9
    small.space <- 0.02 * 0.9
    medium.space <- 0.03 * 0.9
    large.space <- 0.04 * 0.9
  }
  
  if(total.number.of.levels > 22) {
    smaller.space <- 0.01 * 0.85
    small.space <- 0.02 * 0.85
    medium.space <- 0.03 * 0.825
    large.space <- 0.04 * 0.8
  }
  
  if(total.number.of.levels > 25) {
    smaller.space <- 0.01 * 0.8
    small.space <- 0.02 * 0.8
    medium.space <- 0.03 * 0.75
    large.space <- 0.04 * 0.75
  }
  
  if(total.number.of.levels > 35) {
    smaller.space <- 0.01 * 0.65
    small.space <- 0.02 * 0.65
    medium.space <- 0.03 * 0.6
    large.space <- 0.04 * 0.6
  }
  
  # of course there is a limit to how much we can plot on one page  
  if (total.number.of.levels > 45) 
    stop("\n\nTERMINATED: More than 45 part-worths on spine chart\n")
  
  if(!do.standardization) 
    part.worth.plotting.list <- conjoint.results$part.worths
  
  if(do.standardization) 
    part.worth.plotting.list <- conjoint.results$standardized.part.worths 
  
  # check the range of part-worths to see which path to go down for plotting
  # initialize these toggles to start
  max.is.less.than.40 <- FALSE 
  max.is.less.than.20 <- FALSE
  max.is.less.than.10 <- FALSE
  max.is.less.than.4 <- FALSE
  max.is.less.than.2 <- FALSE
  max.is.less.than.1 <- FALSE 
  
  if (max(abs(min(unlist(part.worth.plotting.list),na.rm=TRUE)),
          max(unlist(part.worth.plotting.list),na.rm=TRUE)) <= 40) {
    max.is.less.than.40 <- TRUE 
    max.is.less.than.20 <- FALSE
    max.is.less.than.10 <- FALSE
    max.is.less.than.4 <- FALSE
    max.is.less.than.2 <- FALSE
    max.is.less.than.1 <- FALSE
  }  
  
  if (max(abs(min(unlist(part.worth.plotting.list),na.rm=TRUE)),
          max(unlist(part.worth.plotting.list),na.rm=TRUE)) <= 20) {
    max.is.less.than.40 <- FALSE 
    max.is.less.than.20 <- TRUE
    max.is.less.than.10 <- FALSE
    max.is.less.than.4 <- FALSE
    max.is.less.than.2 <- FALSE
    max.is.less.than.1 <- FALSE
  }
  
  if(max(abs(min(unlist(part.worth.plotting.list),na.rm=TRUE)),
         max(unlist(part.worth.plotting.list),na.rm=TRUE)) <= 10) {
    max.is.less.than.40 <- FALSE 
    max.is.less.than.20 <- FALSE
    max.is.less.than.10 <- TRUE
    max.is.less.than.4 <- FALSE
    max.is.less.than.2 <- FALSE
    max.is.less.than.1 <- FALSE
  }
  
  if (max(abs(min(unlist(part.worth.plotting.list),na.rm=TRUE)),
          max(unlist(part.worth.plotting.list),na.rm=TRUE)) <= 4) {
    max.is.less.than.40 <- FALSE 
    max.is.less.than.20 <- FALSE
    max.is.less.than.4 <- TRUE
    max.is.less.than.10 <- FALSE
    max.is.less.than.2 <- FALSE
    max.is.less.than.1 <- FALSE
  }
  
  if(max(abs(min(unlist(part.worth.plotting.list),na.rm=TRUE)),
         max(unlist(part.worth.plotting.list),na.rm=TRUE)) <= 2) {
    max.is.less.than.40 <- FALSE 
    max.is.less.than.20 <- FALSE
    max.is.less.than.4 <- FALSE
    max.is.less.than.10 <- FALSE
    max.is.less.than.2 <- TRUE
    max.is.less.than.1 <- FALSE
  }
  
  if(max(abs(min(unlist(part.worth.plotting.list),na.rm=TRUE)),
         max(unlist(part.worth.plotting.list),na.rm=TRUE)) <= 1) {
    max.is.less.than.40 <- FALSE 
    max.is.less.than.20 <- FALSE
    max.is.less.than.4 <- FALSE
    max.is.less.than.10 <- FALSE
    max.is.less.than.2 <- FALSE
    max.is.less.than.1 <- TRUE
  }
  
  # sometimes we override the range for part-worth plotting
  # this is not usually done... but it is an option
  if (fix.max.to.4) {
    max.is.less.than.40 <- FALSE 
    max.is.less.than.20 <- FALSE
    max.is.less.than.10 <- FALSE
    max.is.less.than.4 <- TRUE
    max.is.less.than.2 <- FALSE
    max.is.less.than.1 <- FALSE 
  }
  
  if (!max.is.less.than.1 & !max.is.less.than.2 & !max.is.less.than.4 & 
      !max.is.less.than.10 & !max.is.less.than.20 & !max.is.less.than.40) 
    stop("\n\nTERMINATED: Spine chart cannot plot |part-worth| > 40")
  
  # determine point positions for plotting part-worths on spine chart  
  if (max.is.less.than.1 | max.is.less.than.2 | max.is.less.than.4 | 
      max.is.less.than.10 | max.is.less.than.20 | max.is.less.than.40) {
    # begin if-block plotting when all part-worths in absolute value 
    # are less than one of the tested range values
    # part-worth positions for plotting 
    # end if-block plotting when all part-worths in absolute value 
    # are less than one of the tested range values
    # offsets for plotting vary with the max.is.less.than setting
    if(max.is.less.than.1) {
      list.scaling <- function(x) {0.75 + x/5}
      part.worth.point.position <- 
        lapply(part.worth.plotting.list,list.scaling)
    }
    
    if(max.is.less.than.2) {
      list.scaling <- function(x) {0.75 + x/10}
      part.worth.point.position <- 
        lapply(part.worth.plotting.list,list.scaling)
    }  
    
    if(max.is.less.than.4) {
      list.scaling <- function(x) {0.75 + x/20}
      part.worth.point.position <- 
        lapply(part.worth.plotting.list,list.scaling)
    }
    
    if(max.is.less.than.10) {
      list.scaling <- function(x) {0.75 + x/50}
      part.worth.point.position <- 
        lapply(part.worth.plotting.list,list.scaling)
    }
    
    if(max.is.less.than.20) {
      list.scaling <- function(x) {0.75 + x/100}
      part.worth.point.position <- 
        lapply(part.worth.plotting.list,list.scaling)
    }
    
    if(max.is.less.than.40) {
      list.scaling <- function(x) {0.75 + x/200}
      part.worth.point.position <- 
        lapply(part.worth.plotting.list,list.scaling)
    }
    
    part.worth.point.position <- lapply(part.worth.plotting.list,list.scaling)
  } 
  
  if (plot.framing.box) plot(c(0,0,1,1),c(0,1,0,1),xlab="",ylab="",
                             type="n",xaxt="n",yaxt="n")
  
  if (!plot.framing.box) plot(c(0,0,1,1),c(0,1,0,1),xlab="",ylab="",
                              type="n",xaxt="n",yaxt="n", bty="n")
  
  if (put.title.on.spine.chart) {
    text(c(0.50),c(0.975),pos=3,labels=title.on.spine.chart,cex=01.5)
    y.location <- 0.925  # starting position with title
  }
  
  if (!put.title.on.spine.chart) y.location <- 0.975  # no-title start 
  
  # store top of vertical line for later plotting needs
  y.top.of.vertical.line <- y.location 
  
  x.center.position <- 0.75  # horizontal position of spine
  
  # begin primary plotting loop 
  # think of a plot as a collection of text and symbols on screen or paper
  # we are going to construct a plot one text string and symbol at a time
  # (note that we may have to repeat this process at the end of the program)
  for(k in seq(along=effect.names)) { 
    y.location <- y.location - large.space
    text(c(0.4),c(y.location),pos=2,
         labels=paste(effect.name.map(effect.names[k])," ",sep=""),cex=01.0)
    text(c(0.525),c(y.location),pos=2,col=color.for.printing.importance.text,
         labels=paste(" ",left.side.symbol.to.print.around.importance,
                      pretty.print(
                        unlist(conjoint.results$attribute.importance[effect.names[k]])),"%",
                      right.side.symbol.to.print.around.importance,sep=""),cex=01.0)
    
    # begin loop for printing part-worths
    for(m in seq(1:number.of.levels.of.attribute[k])) { 
      y.location <- y.location - medium.space
      text(c(0.4),c(y.location),pos=2,
           conjoint.results$xlevel[[effect.names[k]]][m],cex=01.0)
      #   part.worth.label.data.frame[k,m],cex=01.0)
      
      text(c(0.525),c(y.location),pos=2,
           col=color.for.printing.part.worth.text,
           labels=paste(" ",left.side.symbol.to.print.around.part.worths,
                        pretty.print(part.worth.plotting.list[[effect.names[k]]][m]),
                        right.side.symbol.to.print.around.part.worths,sep=""),cex=01.0)
      
      points(part.worth.point.position[[effect.names[k]]][m],y.location, 
             type = "p", pch = 20, col = color.for.part.worth.point, cex = 2)
      segments(x.center.position, y.location, 
               part.worth.point.position[[effect.names[k]]][m], y.location,
               col = color.for.part.worth.line, lty = 1, lwd = 2)      
    } 
  } 
  
  y.location <- y.location - medium.space
  
  # begin center axis and bottom plotting
  y.bottom.of.vertical.line <- y.location  # store top of vertical line
  
  below.y.bottom.of.vertical.line <- y.bottom.of.vertical.line - small.space/2
  
  if (!draw.gray.background) {
    # four optional grid lines may be drawn on the plot parallel to the spine
    if (draw.optional.grid.lines) {
      segments(0.55, y.top.of.vertical.line, 0.55, 
               y.bottom.of.vertical.line, col = "black", lty = "solid", lwd = 1)  
      
      segments(0.65, y.top.of.vertical.line, 0.65, 
               y.bottom.of.vertical.line, col = "gray", lty = "solid", lwd = 1)  
      
      segments(0.85, y.top.of.vertical.line, 0.85, 
               y.bottom.of.vertical.line, col = "gray", lty = "solid", lwd = 1)    
      
      segments(0.95, y.top.of.vertical.line, 0.95, 
               y.bottom.of.vertical.line, col = "black", lty = "solid", lwd = 1)  
    }
  }
  
  # gray background for plotting area of the points
  if (draw.gray.background) {
    rect(xleft = 0.55, ybottom = y.bottom.of.vertical.line, 
         xright = 0.95, ytop = y.top.of.vertical.line, density = -1, angle = 45,
         col = "light gray", border = NULL, lty = "solid", lwd = 1)
    
    # four optional grid lines may be drawn on the plot parallel to the spine
    if (draw.optional.grid.lines) {
      segments(0.55, y.top.of.vertical.line, 0.55, 
               y.bottom.of.vertical.line, col = "black", lty = "solid", lwd = 1)  
      
      segments(0.65, y.top.of.vertical.line, 0.65, 
               y.bottom.of.vertical.line, col = "white", lty = "solid", lwd = 1)  
      
      segments(0.85, y.top.of.vertical.line, 0.85, 
               y.bottom.of.vertical.line, col = "white", lty = "solid", lwd = 1)    
      
      segments(0.95, y.top.of.vertical.line, 0.95, 
               y.bottom.of.vertical.line, col = "black", lty = "solid", lwd = 1)  
    }     
  }   
  
  # draw the all-important spine on the plot
  segments(x.center.position, y.top.of.vertical.line, x.center.position, 
           y.bottom.of.vertical.line, col = "black", lty = "dashed", lwd = 1)  
  
  # horizontal line at top           
  segments(0.55, y.top.of.vertical.line, 0.95, y.top.of.vertical.line,
           col = "black", lty = 1, lwd = 1)      
  
  # horizontal line at bottom       
  segments(0.55, y.bottom.of.vertical.line, 0.95, y.bottom.of.vertical.line,
           col = "black", lty = 1, lwd = 1)          
  
  # plot for ticks and labels 
  segments(0.55, y.bottom.of.vertical.line, 
           0.55, below.y.bottom.of.vertical.line,
           col = "black", lty = 1, lwd = 1)   # tick line at bottom
  
  segments(0.65, y.bottom.of.vertical.line, 
           0.65, below.y.bottom.of.vertical.line,
           col = "black", lty = 1, lwd = 1)   # tick line at bottom
  
  segments(0.75, y.bottom.of.vertical.line, 
           0.75, below.y.bottom.of.vertical.line,
           col = "black", lty = 1, lwd = 1)   # tick line at bottom      
  
  segments(0.85, y.bottom.of.vertical.line, 
           0.85, below.y.bottom.of.vertical.line,
           col = "black", lty = 1, lwd = 1)   # tick line at bottom      
  
  segments(0.95, y.bottom.of.vertical.line, 
           0.95, below.y.bottom.of.vertical.line,
           col = "black", lty = 1, lwd = 1)   # tick line at bottom      
  
  # axis labels vary with the max.is.less.than range being used
  if (max.is.less.than.1) text(c(0.55,0.65,0.75,0.85,0.95),
                               rep(below.y.bottom.of.vertical.line,times=5),
                               pos=1,labels=c("-1","-0.5","0","+0.5","+1"),cex=0.75)
  
  if (max.is.less.than.2) text(c(0.55,0.65,0.75,0.85,0.95),
                               rep(below.y.bottom.of.vertical.line,times=5),
                               pos=1,labels=c("-2","-1","0","+1","+2"),cex=0.75)
  
  if (max.is.less.than.4) text(c(0.55,0.65,0.75,0.85,0.95),
                               rep(below.y.bottom.of.vertical.line,times=5),
                               pos=1,labels=c("-4","-2","0","+2","+4"),cex=0.75)
  
  if (max.is.less.than.10) text(c(0.55,0.65,0.75,0.85,0.95),
                                rep(below.y.bottom.of.vertical.line,times=5),
                                pos=1,labels=c("-10","-5","0","+5","+10"),cex=0.75)
  
  if (max.is.less.than.20) text(c(0.55,0.65,0.75,0.85,0.95),
                                rep(below.y.bottom.of.vertical.line,times=5),
                                pos=1,labels=c("-20","-10","0","+10","+20"),cex=0.75)
  
  if (max.is.less.than.40) text(c(0.55,0.65,0.75,0.85,0.95),
                                rep(below.y.bottom.of.vertical.line,times=5),
                                pos=1,labels=c("-40","-20","0","+20","+40"),cex=0.75)
  
  y.location <- below.y.bottom.of.vertical.line - small.space
  
  if(do.standardization) 
    text(.75,y.location,pos=1,labels=c("Standardized Part-Worth"),cex=0.95)
  
  if(!do.standardization) text(.75,y.location,pos=1,labels=c("Part-Worth"),
                               cex=0.95)
  
  y.location <- below.y.bottom.of.vertical.line - small.space
  
  if(do.standardization) 
    text(0.75,y.location,pos=1,labels=c("Standardized Part-Worth"),cex=0.95)
  
  if(!do.standardization) text(0.75,y.location,pos=1,labels=c("Part-Worth"),
                               cex=0.95) 
  
  if(print.internal.consistency) {
    y.location <- y.location - medium.space
    text(c(0.525),c(y.location),pos=2,labels=paste("Internal consistency: ",
                                                   pretty.print(conjoint.results$internal.consistency),
                                                   sep=""))
  }
  
  # if we have grid lines we may have plotted over part-worth points
  # if we have a gray background then we have plotted over part-worth points
  # so let us plot those all-important part-worth points and lines once again
  if(draw.gray.background || draw.optional.grid.lines) {
    y.location <- y.top.of.vertical.line  # retreive the starting value 
    
    # repeat the primary plotting loop 
    for(k in seq(along=effect.names)) { 
      y.location <- y.location - large.space
      text(c(0.4),c(y.location),pos=2,
           labels=paste(effect.name.map(effect.names[k])," ",sep=""),cex=01.0)
      text(c(0.525),c(y.location),pos=2,col=color.for.printing.importance.text,
           labels=paste(" ",left.side.symbol.to.print.around.importance,
                        pretty.print(
                          unlist(conjoint.results$attribute.importance[effect.names[k]])),"%",
                        right.side.symbol.to.print.around.importance,sep=""),cex=01.0)
      
      # begin loop for printing part-worths
      for(m in seq(1:number.of.levels.of.attribute[k])) { 
        y.location <- y.location - medium.space
        text(c(0.4),c(y.location),pos=2,
             conjoint.results$xlevel[[effect.names[k]]][m],cex=01.0)
        
        text(c(0.525),c(y.location),
             pos=2,col=color.for.printing.part.worth.text,
             labels=paste(" ",left.side.symbol.to.print.around.part.worths,
                          pretty.print(part.worth.plotting.list[[effect.names[k]]][m]),
                          right.side.symbol.to.print.around.part.worths,sep=""),cex=01.0)
        
        points(part.worth.point.position[[effect.names[k]]][m],y.location, 
               type = "p", pch = 20, col = color.for.part.worth.point, cex = 2)
        segments(x.center.position, y.location, 
                 part.worth.point.position[[effect.names[k]]][m], y.location,
                 col = color.for.part.worth.line, lty = 1, lwd = 2)      
      } 
    } 
  } 
}


#generate a balanced set of product profiles for survey
provider.suvey = Lma.design(attribute.names =
                              list(brand = c("AT&T", "T-Mobile", "US Cellular", "Verizon"),
                                   startup = c('$50', '$100', '$150', '$200'),
                                   monthly = c('$50', '$100', '$150', '$200'),
                                   service = c("4G NO", "4G YES"),
                                   retail = c("retail NO", "retail YES"),
                                   apple = c("Apple No", "Apple YES"),
                                   samsung = c("Samsung NO", 'Samsumg YES'),
                                   google = c("Nexus NO", "Nexus YES")),
                            nalternatives = 1, nblocks = 1, seed=9999)
print(questionnaire(provider.suvey))

sink("question_for_suvey.txt")
sink()

# user-defined function for plotting descriptive attributes names
effect.name.map = function(effect.name){
  if(effect.name=="brand") return("mobile service provider")
  if(effect.name=="startup") return("start-up cost")
  if(effect.name=="monthly") return("monthly cost")
  if(effect.name=="service") return("offer 4g service")
  if(effect.name=="retail") return("has nearby retail store")
  if(effect.name=="apple") return("sell apple product")
  if(effect.name=="samsung") return("sell samsung product")
  if(effect.name=="google") return("sell google/nexus product")
}
# read in conjoint survey profiles with respondent ranks
conjoint.df <- read.csv("C:/Users/jaspr/iCloudDrive/QUARTER 6/DA 420/3/mobile.csv", header = TRUE)
print.digits <- 2 # set number of digits on print and spine chart

## set up sum of contrasts for effects coding as needed for conjoint analysis
options(contrasts=c("contr.sum","contr.poly"))
main.effect.model = {ranking ~ brand+startup+monthly+service+retail+apple+samsung+google}

# main effects model specification 
main.fit = lm(main.effect.model, data=conjoint.df)
print(summary(main.fit))

# fit linear regression model using main effects only (no interaction terms)
main.fit["contrasts"]
main.fit["xlevels"]
main.fit["coefficients"]

#save key list elements of fitted model as needed for conjoint measure
conjoint.results = main.fit[c("contrasts","xlevels","coefficients")]
conjoint.results$attributes <- names(conjoint.results$contrasts)

#compute and store part-worths in the conjoint.results list struture
end.index.for.coefficient = 1 #????????? ?????????????????? ?????????
part.worth.vector = NULL
for(index.for.attribute in seq(along=conjoint.results$contrasts)) {
  nlevels = length(unlist(conjoint.results$xlevels[index.for.attribute]))
  begin.index.for.coefficient = end.index.for.coefficient + 1
  end.index.for.coefficient = begin.index.for.coefficient + nlevels -2
  last.part.worth = -sum(conjoint.results$coefficients[
    begin.index.for.coefficient:end.index.for.coefficient])
  part.worths[index.for.attribute] = list(as.numeric(c(conjoint.results$coefficients[
    begin.index.for.coefficient:end.index.for.coefficient],
    last.part.worth)))
  part.worth.vector = c(part.worth.vector,unlist(part.worths[index.for.attribute]))
}

conjoint.results$part.worths = part.worths
conjoint.results$part.worths

#compute the standardized part-worths

standardize <- function(x) {(x - mean(x)) / sd(x)}
conjoint.results$standardized.part.worths = lapply(conjoint.results$part.worths,standardize)
conjoint.results$standardized.part.worths

#compute and store the part-worth ranges for each attributes
part.worth.ranges = conjoint.results$contrasts
for(index.for.attribute in seq(along=conjoint.results$contrasts))
  part.worth.ranges[index.for.attribute]=dist(range(conjoint.results$part.worths[index.for.attribute]))
conjoint.results$part.worth.ranges = part.worth.ranges
sum.part.worth.ranges = sum(as.numeric(conjoint.results$part.worth.ranges))

#compute and store importance values for each attribute
attribute.importance = conjoint.results$contrasts
for(index.for.attribute in seq(along=conjoint.results$contrasts))
  attribute.importance[index.for.attribute]= (dist(range(conjoint.results$part.worths[index.for.attribute]))/
                                                sum.part.worth.ranges) * 100
conjoint.results$attribute.importance = attribute.importance

#data frame for ordering attribute names
attribute.name = names(conjoint.results$contrasts)
attribute.importance = as.numeric(attribute.importance)
temp.frame = data.frame(attribute.name,attribute.importance)
conjoint.results$ordered.attributes = as.character(temp.frame[sort.list(temp.frame$attribute.importance,decreasing=TRUE),
                                                              "attribute.name"])
#respondant internal consistency added to list statistics
conjoint.results$internal.consistency = summary(main.fit)$r.squared

#user defined function for printing conjoint measures
if (print.digits == 2)
  pretty.print = function(x) {sprintf("%1.2f",round(x,digits = 2))}
if (print.digits == 3)
  pretty.print = function(x) {sprintf("%1.3f",round(x,digits = 3))}

#report conjoint measure to console 
#use pretty.print to provide nicely fromated output
for(k in seq(along=conjoint.results$ordered.attributes)) {
  cat("\n","\n")
  cat(conjoint.results$ordered.attributes[k],"Levels: ",
      unlist(conjoint.results$xlevels[conjoint.results$ordered.attributes[k]]))
  cat("\n"," Part-Worths: ")
  cat(pretty.print(unlist(conjoint.results$part.worths
                          [conjoint.results$ordered.attributes[k]])))
  cat("\n"," Standardized Part-Worths: ")
  cat(pretty.print(unlist(conjoint.results$standardized.part.worths
                          [conjoint.results$ordered.attributes[k]])))
  cat("\n"," Attribute Importance: ")
  cat(pretty.print(unlist(conjoint.results$attribute.importance
                          [conjoint.results$ordered.attributes[k]])))
}

#printing the spine chart
#all graphical output is routed to external pdf file

pdf(file= "preference_mobile_services.pdf", width=8.5, height=11)
spine.chart(conjoint.results)
dev.off #close the graphic output

