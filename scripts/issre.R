##ISSRE 2015##

library(reshape2)
library(data.table)

wd <- "C:/Users/Gustavo/workspace/issre2015"
setwd(wd)

source("./scripts/plotter.r")

######################### CHANGE PROPAGATION ANALYSIS #########################


########### RQ1  ###########

allCompUnitLucene <- fread("./results/csv/allcompunit/allcompunit - lucene.csv", sep="|", header = TRUE)
allCompUnitMegamek <- fread("./results/csv/allcompunit/allcompunit - megamek.csv", sep="|", header = TRUE)
allCompUnitCommons <- fread("./results/csv/allcompunit/allcompunit - commonscsv.csv", sep="|", header = TRUE)
allCompUnitTomcat <- fread("./results/csv/allcompunit/allcompunit - tomcat7.csv", sep="|", header = TRUE)

rq1a = rqa
rq1a(rq = "rq1a", allCompUnitLucene, box_limits_for_y = c(0,50), bean_limits_for_y = c(-2,30), subjectsystem = "Lucene")
rq1a(rq = "rq1a", allCompUnitMegamek, box_limits_for_y = c(0,50), bean_limits_for_y = c(-2,50), subjectsystem = "Megamek")
rq1a(rq = "rq1a", allCompUnitCommons, subjectsystem = "Commons")
rq1a(rq = "rq1a", allCompUnitTomcat, box_limits_for_y = c(0,50), bean_limits_for_y = c(-2,30), subjectsystem = "Tomcat")

rq1b = rqb
rq1b(rq = "rq1b", allCompUnitLucene, subjectsystem = "Lucene")
rq1b(rq = "rq1b", allCompUnitMegamek, subjectsystem = "Megamek")
rq1b(rq = "rq1b", allCompUnitCommons, subjectsystem = "Commons")
rq1b(rq = "rq1b", allCompUnitTomcat, subjectsystem = "Tomcat")

########### RQ2 ###########

compUnitLucene <- fread("./results/csv/compunit/compunit - lucene.csv", sep="|", header = TRUE)
compUnitMegamek <- fread("./results/csv/compunit/compunit - megamek.csv", sep="|", header = TRUE)
compUnitCommons <- fread("./results/csv/compunit/compunit - commonscsv.csv", sep="|", header = TRUE)
compUnitTomcat <- fread("./results/csv/compunit/compunit - tomcat7.csv", sep="|", header = TRUE)

rq2a = rqa
rq2a(rq = "rq2a", compUnitLucene, box_limits_for_y = c(0,50), bean_limits_for_y = c(-1,10), subjectsystem = "Lucene")
rq2a(rq = "rq2a", compUnitMegamek, box_limits_for_y = c(0,50), bean_limits_for_y = c(-1,20), subjectsystem = "Megamek")
rq2a(rq = "rq2a", compUnitCommons, subjectsystem = "Commons")
rq2a(rq = "rq2a", compUnitTomcat, box_limits_for_y = c(0,50), bean_limits_for_y = c(-1,8), subjectsystem = "Tomcat")

rq2b = rqb
rq2b(rq = "rq2b", compUnitLucene, subjectsystem = "Lucene")
rq2b(rq = "rq2b", compUnitMegamek, subjectsystem = "Megamek")
rq2b(rq = "rq2b", compUnitCommons, subjectsystem = "Commons")
rq2b(rq = "rq2b", compUnitTomcat, subjectsystem = "Tomcat")


########### RQ3 ###########

commitpropLucene <- fread("./results/csv/commitprop/commitprop - lucene.csv", sep="|", header = TRUE)
commitpropLucene$system <- "Lucene"

summary(commitpropLucene$percent_propagated)
sd(commitpropLucene$percent_propagated)

commitpropMegamek <- fread("./results/csv/commitprop/commitprop - megamek.csv", sep="|", header = TRUE)
commitpropMegamek$system <- "Megamek"

summary(commitpropMegamek$percent_propagated)
sd(commitpropMegamek$percent_propagated)

commitpropCommons <- fread("./results/csv/commitprop/commitprop - commonscsv.csv", sep="|", header = TRUE)
commitpropCommons$system <- "Commons CSV"

summary(commitpropCommons$percent_propagated)
sd(commitpropCommons$percent_propagated)

commitpropTomcat <- fread("./results/csv/commitprop/commitprop - tomcat7.csv", sep="|", header = TRUE)
commitpropTomcat$system <- "Tomcat"

summary(commitpropTomcat$percent_propagated)
sd(commitpropTomcat$percent_propagated)

cols <- c("system","percent_propagated")
commitprop <- rbind(
  subset(commitpropLucene, select = cols),
  subset(commitpropMegamek, select = cols),
  subset(commitpropCommons, select = cols),
  subset(commitpropTomcat, select = cols)
)

plot_beanplot(df = commitprop, xcol = "system", ycol = "percent_propagated", limits_for_y = c(-0.1,1))

#(a) "Inside treatment" evaluation
rqa = function(rq = NULL, changePropDF, box_limits_for_y = NULL, bean_limits_for_y = NULL, subjectsystem = NULL){
  
  #Limitando a analise (dataset) somente para os commits em que A depende de B
  sub = subset(changePropDF, client_cochange + client_nocochange > 0, c("pair","client_cochange","client_nocochange"))
  
  print(length(sub$client_cochange))
  print(length(sub$client_nocochange))
  
  print(summary(sub$client_cochange))
  print(summary(sub$client_nocochange))
  
  submeltDF = melt(sub, id.vars = "pair", 
                   measure.vars = c("client_cochange","client_nocochange"))
  
  plot_boxplot_2vars(df = submeltDF, xcol = "variable", ycol = "value",
                     outfile = paste(rq,"boxplot",subjectsystem,sep="-"))
  
  plot_boxplot_2vars(df = submeltDF, xcol = "variable", ycol = "value",
                     limits_for_y = box_limits_for_y,
                     outfile = paste(rq,"boxplot-zoomed",subjectsystem,sep="-"))
  
  plot_beanplot_2vars_withoutgroup(df = submeltDF, metric_col = "value", 
                                   side_col = "variable", title = subjectsystem, 
                                   limits_for_y = bean_limits_for_y,
                                   legendposition = "bottomright",
                                   outfile = paste(rq,"beanplot",subjectsystem,sep="-"))
 
  print(
    wilcox.test(sub$client_cochange,
                sub$client_nocochange,
                alternative="two.sided", paired = TRUE, conf.level = 0.95, exact = FALSE) 
  )
  
  print(
    wilcox.test(sub$client_cochange,
              sub$client_nocochange,
              alternative="less", paired = TRUE, conf.level = 0.95, exact = FALSE)
  )
 
  print(
    wilcox.test(sub$client_cochange,
                sub$client_nocochange,
                alternative="greater", paired = TRUE, conf.level = 0.95, exact = FALSE) 
  )
  
}

## (b) Co-change ratio when there is dependency and when there is no dependency
## (evaluating the effect of the treatment)

rqb = function(rq = NULL, changePropDF, box_limits_for_y = NULL, bean_limits_for_y = NULL, subjectsystem = NULL){

  ratio_for_cochange_as_client <- changePropDF$ratio_for_cochange_as_client
  ratio_for_cochange_as_client <- subset(x = ratio_for_cochange_as_client, ratio_for_cochange_as_client != "NaN")
  
  ratio_for_cochange_as_non_client <- changePropDF$ratio_for_cochange_as_non_client
  ratio_for_cochange_as_non_client <- subset(x = ratio_for_cochange_as_non_client, ratio_for_cochange_as_non_client != "NaN")
  
  print(length(ratio_for_cochange_as_client))
  print(length(ratio_for_cochange_as_non_client))
  
  print(summary(ratio_for_cochange_as_client))
  print(sd(ratio_for_cochange_as_client))
  
  print(summary(ratio_for_cochange_as_non_client))
  print(sd(ratio_for_cochange_as_non_client))
  
  #print(table(ratio_for_cochange_as_client))
  #print(table(ratio_for_cochange_as_non_client))
  
  x = data.frame(variable = "ratio_for_cochange_as_client", ratio = ratio_for_cochange_as_client)
  y = data.frame(variable = "ratio_for_cochange_as_non_client", ratio = ratio_for_cochange_as_non_client)
  ratio_df = rbind(x,y)
  
  plot_boxplot_2vars(df = ratio_df, xcol = "variable", ycol = "ratio",
                     title = "Deps estruturais levam a co-changes",
                     outfile = paste(rq,"boxplot",subjectsystem,sep="-"))
  
  plot_beanplot_2vars_withoutgroup(df = ratio_df, metric_col = "ratio", 
                                   ylab = "Co-Change Ratio",
                                   side_col = "variable", title = subjectsystem,
                                   outfile = paste(rq,"beanplot",subjectsystem,sep="-"))
  
  wilcox.test(ratio_for_cochange_as_client,
              ratio_for_cochange_as_non_client,
              alternative="greater", paired = FALSE, conf.level = 0.95)
}

