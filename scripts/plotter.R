library(beanplot)
library(ggplot2)

library(Hmisc)
library(hexbin)
library(grid)

plot_control_chart = function(df, index_col, metric_col, limits_for_y = NULL, title){
  x <- df[[index_col]]
  y <- df[[metric_col]]
  
  metric_mean = mean(y)
  three_std_below = metric_mean - (3 * sd(y))
  three_std_above = metric_mean + (3 * sd(y))
  
  vert_lines <-  numeric()
  outliers <- numeric()
  cutpoints = unique(df$cutpoint)
  
  for(i in 1:length(cutpoints)){
    chunk = subset(df, cutpoint == i)
    vert_lines[i] = head(chunk,1)$commit_seq
    outliers_below = which(chunk[,metric_col] < three_std_below)
    outliers_above = which(chunk[,metric_col] > three_std_above)
    outliers[i] = length(outliers_below) + length(outliers_above)
  }
  
  print(paste("Three std. below:",three_std_below))
  print(paste("Three std. above:",three_std_above))
  cat(outliers, sep = "\n")
  
  p <- ggplot(df, aes(x,y),environment = environment())
  p <- p + scale_y_continuous()

  p <- p + coord_cartesian(ylim = limits_for_y)

  p <- p + geom_point()

  p <- p + geom_hline(yintercept=metric_mean, linetype = "longdash", colour = "blue")
  p <- p + geom_hline(yintercept=three_std_below, linetype = "longdash", colour = "blue")
  p <- p + geom_hline(yintercept=three_std_above, linetype = "longdash", colour = "blue")
  
  p <- p + geom_vline(xintercept = vert_lines, linetype = "longdash", colour = "red")
  
  p <- p + ggtitle(title)
  p <- p + theme_bw()
  
  print_plot(p,title)
  
  #plot(df[,xcol], df[,ycol], type='b', main = title)
}

plot_points_2vars = function(df, xcol, ycol, breaks_for_x = waiver(), limits_for_x = NULL, breaks_for_y = waiver(), limits_for_y = NULL, title){
  x <- df[[xcol]]
  y <- df[[ycol]]
  p <- ggplot(df, aes(x,y),environment = environment())
  p <- p + scale_x_continuous(breaks=breaks_for_x)
  p <- p + scale_y_continuous(breaks=breaks_for_y)
  p <- p + coord_cartesian(xlim = limits_for_x, ylim = limits_for_y)
  p <- p + geom_point()
  p <- p + geom_smooth()
  p <- p + ggtitle(title)
  p <- p + theme_bw()
  
  print_plot(p,title)
  
  #plot(df[,xcol], df[,ycol], type='b', main = title)
}

plot_line_2vars = function(df, xcol, ycol, breaks_for_x = waiver(), 
                           limits_for_x = NULL, breaks_for_y = waiver(), 
                           limits_for_y = NULL, smooth = FALSE, title){
  x <- df[[xcol]]
  y <- df[[ycol]]
  
  p <- ggplot(df, aes(x,y),environment = environment())
  p <- p + scale_x_continuous(breaks=breaks_for_x)
  p <- p + scale_y_continuous(breaks=breaks_for_y)
  p <- p + coord_cartesian(xlim = limits_for_x, ylim = limits_for_y)
  p <- p + geom_line()
  p <- p + geom_point()
  
  if(smooth == TRUE){
    p <- p + geom_smooth()
  }
  
  p <- p + ggtitle(title)
  p <- p + theme_bw()
  
  print_plot(p,title)
  
  #plot(df[,xcol], df[,ycol], type='b', main = title)
}

plot_line_3vars = function(df, xcol, ycol, zcol, limits_for_y = NULL, title){
  
  x <- df[[xcol]]
  y <- df[[ycol]]
  z <- df[[zcol]]
  
  p <- ggplot(df, aes(factor(x),y,size=z),environment = environment())
  p <- p + coord_cartesian(ylim = limits_for_y)
  p <- p + geom_line()
  p <- p + ggtitle(title)
  p <- p + theme_bw()
  
  print_plot(p,title)
  
  #plot(df[,xcol], df[,ycol], type='b', main = title)
}

plot_histogram = function(df, col, binwidth = NULL, breaks_for_x = waiver(), limits_for_x = NULL, breaks_for_y = waiver(), limits_for_y = NULL, title){
  
  data <- df[[col]]
  
  p <- ggplot(df, aes(x=data), environment = environment()) 
  p <- p + geom_histogram(binwidth = binwidth, colour="black", fill="white")
  p <- p + scale_x_continuous(breaks=breaks_for_x, limits=limits_for_x) 
  p <- p + scale_y_continuous(breaks=breaks_for_y, limits=limits_for_y)
  p <- p + ggtitle(title)
  p <- p + theme_bw() 
  
  print_plot(p,title)
  
}

plot_histogram_categorical = function(df, col, binwidth = NULL, breaks_for_y = waiver(), limits_for_y = NULL, title){
  
  data <- df[[col]]
  
  p <- ggplot(df, aes(x=data), environment = environment()) 
  p <- p + geom_histogram(binwidth=binwidth, colour="black", fill="white")
  p <- p + scale_y_continuous(breaks=breaks_for_y, limits=limits_for_y)
  p <- p + ggtitle(title)
  p <- p + theme_bw() 
  
  print_plot(p,title)
  
}

plot_boxplot_1var = function(df, col, breaks_for_y = waiver(), limits_for_y = NULL, title){
  
  data <- df[[col]]
  
  p <- ggplot(df, aes(x=factor(col),y=data), environment = environment()) 
  p <- p + geom_boxplot()
  p <- p + scale_y_continuous(breaks=breaks_for_y)
  p <- p + coord_cartesian(ylim = limits_for_y)
  p <- p + ggtitle(title)
  p <- p + xlab(NULL)
  p <- p + theme_bw() 
  
  print_plot(p,title)

}

plot_boxplot_2vars = function(df, xcol, ycol, limits_for_x = NULL, 
                              xlab = xcol, ylab = ycol,
                              breaks_for_y = waiver(), 
                              limits_for_y = NULL, title = NULL, outfile = NULL){
  
  groups <- df[[xcol]]
  data <- df[[ycol]]
  
  p <- ggplot(df, aes(x=factor(groups),y=data), environment = environment()) 
  p <- p + geom_boxplot()
  p <- p + scale_y_continuous(breaks=breaks_for_y)
  p <- p + coord_cartesian(xlim = limits_for_x, ylim = limits_for_y)
  p <- p + ggtitle(title)
  p <- p + xlab(xlab) + ylab(ylab)
  p <- p + theme_bw() 
  
  print_plot(p,outfile)
}

plot_violin = function(df, xcol, ycol, limits_for_y = NULL, breaks_for_y = waiver(), title){
  
  groups <- df[[xcol]]
  data <- df[[ycol]]
  
  p <- ggplot(df, aes(x=factor(groups),y=data), environment = environment()) 
  p <- p + geom_violin(trim = FALSE, colour = "grey50")
  p <- p + coord_cartesian(ylim = limits_for_y)
  p <- p + scale_y_continuous(breaks=breaks_for_y)
  p <- p + ggtitle(title)
  p <- p + xlab(xcol)
  #p <- p + stat_summary(fun.y="median", geom="point")
  p <- p + geom_boxplot(width=.1, outlier.size = 5, outlier.shape = 95, fill = "grey90")
  p <- p + theme_bw() 
  
  print_plot(p,title)  
}

plot_beanplot = function(df, xcol, ycol, limits_for_y = NULL, title = NULL, outfile = NULL){
  
  yvector <- df[[ycol]]
  xvector <- df[[xcol]]
  
  if(!is.null(outfile)){
    png(filename=paste("./results/imgs/",outfile,".png",sep = ""),width = 480, height = 480)
  }
  
  beanplot(yvector ~ xvector, data = df, method = "overplot",
           what = c(TRUE, TRUE, TRUE, FALSE), beanlinewd = 2,
           overallline = "median", beanlines = "median",
           col = c("lightgray","black","black","black"), border = "gray", 
           log = "",  bw="nrd0", ylim=limits_for_y, main = title)
  
  minor.tick(ny = 4)
  
  if(!is.null(outfile)){
    dev.off()
  }
  #png(filename=paste(title,".png"),width = 1920, height = 1080)
  #dev.off()
}

plot_beanplot_singlevar = function(df, col, limits_for_y = NULL, title){
  
  numvector <- df[[col]]
  
  beanplot(numvector, method = "overplot",
           what = c(TRUE, TRUE, TRUE, TRUE), beanlinewd = 2,
           overallline = "median", beanlines = "median",
           col = c("lightgray","black","black","blue"), border = "gray", 
           log = "",  bw="nrd0", ylim=limits_for_y, main = title)
  
  minor.tick(ny = 4)
  
  #png(filename=paste(title,".png"),width = 1920, height = 1080)
  #dev.off()
}

plot_beanplot_2vars = function(df, metric_col, side_col, group_col, limits_for_y = NULL, title){
  
  metric_vector <- df[[metric_col]]
  side_vector <- df[[side_col]]
  group_vector <- df[[group_col]]
  
  colorsleft = c("lightgray","black","black","blue")
  colorsright = c("darkgray","black","black","blue")
  colors = list(colorsleft,colorsright)
  
  leftbordercolor = "lightgray"
  rightbordercolor = "darkgray"
  bordercolors = c(leftbordercolor,rightbordercolor)
  
  beanplot(metric_vector ~ side_vector * group_vector, data = df, method = "overplot",
           what = c(TRUE, TRUE, TRUE, TRUE), beanlinewd = 2,
           overallline = "median", beanlines = "mean",
           col = colors, log = "",  
           bw="nrd0", ylim=limits_for_y, main = title, 
           side = "both", border = bordercolors)
  
  minor.tick(ny = 4)
  
  #png(filename=paste(title,".png"),width = 1920, height = 1080)
  #dev.off()
}


plot_beanplot_2vars_withoutgroup = function(df, metric_col, side_col, 
                                            xlab = "", ylab = "",
                                            leftside = NULL, rightside = NULL,
                                            limits_for_y = NULL,
                                            legendposition = "topright", title,
                                            outfile = NULL){
  
  metric_vector <- df[[metric_col]]
  side_vector <- df[[side_col]]
  
  colorsleft = c("black","black","black","black")
  colorsright = c("darkgray","black","black","black")
  colors = list(colorsleft,colorsright)
  
  leftbordercolor = "black"
  rightbordercolor = "darkgray"
  bordercolors = c(leftbordercolor,rightbordercolor)
  
  if(!is.null(outfile)){
    png(filename=paste("./results/imgs/",outfile,".png",sep = ""),width = 480, height = 480)
  }
  
  beanplot(metric_vector ~ side_vector, data = df, method = "overplot",
           what = c(TRUE, TRUE, TRUE, FALSE), beanlinewd = 2,
           overallline = "median", beanlines = "median",
           col = colors, log = "",  
           bw="nrd0", ylim=limits_for_y, main = title, 
           side = "both", border = bordercolors,
           xlab = xlab, ylab = ylab, show.names = FALSE, 
           cex.axis=2.0, cex.main=2.0, cex.lab=1.5)
  
  if(!is.null(leftside) || !is.null(rightside)){
    
    legend(legendposition, bty="n", c(leftside, rightside),
           fill = c("lightgray", "darkgray"), cex = 2)  
  }
  
  if(!is.null(outfile)){
    dev.off()
  }
  
}

plot_lowess = function(df, xcol, ycol, limits_for_y = NULL, breaks_for_y = waiver(), spanvalue, title){
   
  xvector <- df[[xcol]]
  yvector <- df[[ycol]]
  
  #### Calculates LOWESS curve ######
  
  #Function as.numeric is applied to x and y components to make sure lowess
  #can be calculated.
  curve <- lowess(as.numeric(xvector), y = as.numeric(yvector), f = spanvalue, iter = 10)
  
  #first point and last point of curve
  
  x0 = round(head(curve$x, n=1), digits = 2)
  y0 = round(head(curve$y, n=1), digits = 2)
  xn = round(tail(curve$x, n=1), digits = 2)
  yn = round(tail(curve$y, n=1), digits = 2)
  
  #Converts list of 2 elements to dataframe
  curve <- data.frame(x = curve$x, y = curve$y)
  
  #GGPlot with hexbin and the lowess curve
  p <- ggplot(df, aes(x = df[,xcol], y = df[,ycol]), environment = environment()) 
  p <- p + geom_hex(colour = 'black',bins = 20) 
  p <- p + scale_fill_gradientn(colours=c('grey96','black')) 

  p <- p + geom_line(data=curve, colour = 'red', aes(x = curve$x, y = curve$y)) 
  #p <- p + geom_smooth(se = FALSE)
  
  #p <- p + ylim(1,50) --> This removes data, so I use the other option below
  #check coord_cartesian from ggplot
  p <- p + coord_cartesian(ylim = limits_for_y)
  p <- p + scale_y_continuous(breaks=breaks_for_y)
  
  p <- p + ggtitle(title)
  p <- p + xlab(xcol)
  p <- p + ylab(ycol)
  p <- p + theme_bw()
  
  text = paste("LOWESS Curve: (",x0,",",y0,"),(",xn,",",yn,")", sep = "")
  my_grob = grobTree(textGrob(text, x=0.1,  y=0.95, hjust=0, gp=gpar(col="red", fontsize=12, fontface="italic")))
  p <- p + annotation_custom(my_grob)
  
  print_plot(p,title)
}

plot_loess = function(df, xcol, ycol, limits_for_y = NULL, breaks_for_y = waiver(), spanvalue, title){
  
  xvector <- df[[xcol]]
  yvector <- df[[ycol]]
  
  #### Calculates LOESS curve ######
  #lo <- loess(y ~ x, data = data.frame(x = xvector, y = yvector), control = loess.control(iterations = 10))
  #y.predict <- predict(lo, data.frame(x = xvector))
  #curve <- data.frame(x = xvector, y = y.predict)
  
  #first point and last point of curve
  
  #x0 = round(head(curve$x, n=1), digits = 2)
  #y0 = round(head(curve$y, n=1), digits = 2)
  #xn = round(tail(curve$x, n=1), digits = 2)
  #yn = round(tail(curve$y, n=1), digits = 2)
  
  #GGPlot with hexbin and the loess curve
  p <- ggplot(df, aes(x = df[,xcol], y = df[,ycol]), environment = environment()) 
  p <- p + geom_hex(colour = 'black',bins = 20) 
  p <- p + scale_fill_gradientn(colours=c('grey96','black')) 
  
  p <- p + geom_smooth(se = TRUE, method = "loess")
  #p <- p + geom_line(data=curve, colour = 'red', aes(x = curve$x, y = curve$y)) 
  
  #p <- p + ylim(1,50) --> This removes data, so I use the other option below
  #check coord_cartesian from ggplot
  p <- p + coord_cartesian(ylim = limits_for_y)
  p <- p + scale_y_continuous(breaks=breaks_for_y)
  
  p <- p + ggtitle(title)
  p <- p + xlab(xcol)
  p <- p + ylab(ycol)
  p <- p + theme_bw()
  
  #text = paste("LOESS Curve: (",x0,",",y0,"),(",xn,",",yn,")", sep = "")
  #my_grob = grobTree(textGrob(text, x=0.1,  y=0.95, hjust=0, gp=gpar(col="red", fontsize=12, fontface="italic")))
  #p <- p + annotation_custom(my_grob)
  
  print_plot(p,title)
}

plot_lowess_loess = function(df, xcol, ycol, limits_for_y = NULL, breaks_for_y = waiver(), spanvalue, title){
  
  xvector <- df[[xcol]]
  yvector <- df[[ycol]]
  
  #### Calculates LOWESS curve ######
  
  #Function as.numeric is applied to x and y components to make sure lowess
  #can be calculated.
  curve <- lowess(as.numeric(xvector), y = as.numeric(yvector), f = spanvalue, iter = 10)
  
  #first point and last point of curve
  
  x0 = round(head(curve$x, n=1), digits = 2)
  y0 = round(head(curve$y, n=1), digits = 2)
  xn = round(tail(curve$x, n=1), digits = 2)
  yn = round(tail(curve$y, n=1), digits = 2)
  
  #Converts list of 2 elements to dataframe
  curve <- data.frame(x = curve$x, y = curve$y)
  
  #GGPlot with hexbin and the lowess curve
  p <- ggplot(df, aes(x = df[,xcol], y = df[,ycol]), environment = environment()) 
  p <- p + geom_hex(colour = 'black',bins = 20) 
  p <- p + scale_fill_gradientn(colours=c('grey96','black')) 
  
  p <- p + geom_line(data=curve, colour = 'red', aes(x = curve$x, y = curve$y)) 
  #LOESS curve
  p <- p + geom_smooth(se = FALSE)
  
  #p <- p + ylim(1,50) --> This removes data, so I use the other option below
  #check coord_cartesian from ggplot
  p <- p + coord_cartesian(ylim = limits_for_y)
  p <- p + scale_y_continuous(breaks=breaks_for_y)
  
  p <- p + ggtitle(title)
  p <- p + xlab(xcol)
  p <- p + ylab(ycol)
  p <- p + theme_bw()
  
  text = paste("LOWESS Curve: (",x0,",",y0,"),(",xn,",",yn,")", sep = "")
  my_grob = grobTree(textGrob(text, x=0.1,  y=0.95, hjust=0, gp=gpar(col="red", fontsize=12, fontface="italic")))
  p <- p + annotation_custom(my_grob)
  
  print_plot(p,title)
}


old_plot_lowess = function(df, xcol, ycol, spanvalue, title){
  
  title <- paste(title,"(",spanvalue,")")
  
  xvector <- df[[xcol]]
  yvector <- df[[ycol]]
  
  hbin <- hexbin(xvector,yvector, xbins = 10)
  hplt <- plot(hbin, main = title)
  
  pushHexport(hplt$plot.vp)
  
  #Calculates LOWESS curve
  #Function as.numeric is applied to x and y components to make sure lowess
  #can be calculated.
  curve <- lowess(as.numeric(xvector), y = as.numeric(yvector), f = spanvalue, iter = 5)
  
  #Adds the LOWESS curve to the graph
  #The super intuitive parameter 'gp = gpar(col = 2)' is just the line color
  grid.lines(curve$x, curve$y, gp = gpar(col = 2), default.units = "native")
  
  #Saves
  #png(filename=paste(title,".png"),width = 1920, height = 1080)
  #dev.off()
}

plot_scatterplot = function(df, xcol, ycol, limits_for_x = NULL, limits_for_y = NULL, title){
  
  xvector <- df[[xcol]]
  yvector <- df[[ycol]]
  data<-data.frame(xvector,yvector)
  
  medianForX = median(xvector)
  medianForY = median(yvector)
  
  print(paste("Median for X:",medianForX))
  print(paste("Median for Y:",medianForY))
  
  # Building the Scatterplot
  p = ggplot(data,aes(x=xvector,y=yvector)) 
  p = p + geom_point()
  #plot = plot + scale_x_continuous(breaks=seq(0,12,0.5))
  p = p + geom_hline(linetype = "dashed", yintercept=medianForY, colour = "red")
  p = p + geom_vline(linetype = "dashed", xintercept=medianForX, colour = "red")
  p = p + coord_cartesian(xlim = limits_for_x, ylim = limits_for_y)
  p = p + xlab(xcol)
  p = p + ylab(ycol)
  p = p + theme_bw()  
  p = p + ggtitle(title)
  
  print_plot(p,title)
  
  #scale_x_continuous(breaks=1:30, limits=c(1,30)
  
  #inquadrant = fn$sqldf("select count(*) as count from data where 
  #                      support_count > '$medianSupportCount' and 
  #                      median_hops > '$medianHops'")
  
  #cat("Number of elements in 1st Quadrant:", inquadrant$count, "\n")
  #cat("Percentage:", percent(inquadrant$count / nrow(project)))
  
  #return(plot)
}

print_plot = function(p, outfile = NULL){
  if (!is.null(outfile)){
    ggsave(paste("./results/imgs/",outfile,".png",sep = ""), plot = p, width=16, height=9)
  }
  p
}

#Trim images inside R using imager (relies on GraphicsMagick)
#
#library(imager)
#trim_images = function(){
#  
#  original_path = "./results/imgs/"
#  trimmed_path = "./results/imgs/trimmed/"
#  
#  image_file_list = list.files(path = original_path, pattern = "*.png")
#  for(image_file in image_file_list){
#    i = load.image(paste(original_path,image_file,sep=""))
#    i = autocrop(i,color = c(255,255,255))
#    save.image(i,paste(trimmed_path,image_file,sep=""))
#  }
#}
