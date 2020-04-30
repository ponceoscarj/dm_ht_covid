library("metafor")
library("dplyr")
library("tidyverse")
library("gt")
library("XML")
library("grid")
library("grImport")
library("readxl")
library("lme4")
library("forestplot")
library("tidyr")
library("ggplot2")
library("lattice")
library("rmarkdown")
library(grid)
library(DescTools)

options(dplyr.print_max = 500)
options(tibble.width = NULL)


#####Claning Data#####
getwd()
setwd("C:/Users/Oscar Ponce/Documents/Research/Frank/COVID/5 Data analysis")
list.files()

#C:/Users/Oscar Ponce/Documents/Research/Frank/COVID/5 Data analysis

prev1 <- read_excel("C:/Users/Oscar Ponce/Documents/Research/Frank/COVID/5 Data analysis/finalV2.xlsx", 
                    sheet = "Non-exclusive prevalence")

#C:/Users/Oscar Ponce/Documents/Research/Frank/COVID/5 Data analysis/finalV2.xlsx

names(prev1)[names(prev1) == "analyzed_n"] <- "n"
names(prev1)[names(prev1) == "n1"] <- "events"
names(prev1)[names(prev1) == "selection_criteria"] <- "group"
names(prev1)[names(prev1) == "country_1"] <- "country"

table(prev1$group)
prev1$group[prev1$group == 'asymptomatic'] <- 'general'
prev1$group[prev1$group == 'general_prexisting_cardiac_conditions'] <- 'cardiac'
prev1$group[prev1$group == 'general_cardiac_injury\r\n'] <- 'cardiac'
table(prev1$group)

table(prev1$setting)
prev1$setting[prev1$setting == 'both_'] <- 'both'
prev1$setting[prev1$setting == 'both_and_community'] <- 'overall'
prev1$setting[prev1$setting == 'in-patient'] <- 'inpatient'
prev1$setting[prev1$setting == 'out-patient'] <- 'outpatient'

table(prev1$country)
prev1$country[prev1$country == 'republic_of_korea'] <- 'korea'
prev1$country[is.na(prev1$country)] <- 'hong_kong'



colnames(prev1)

str(prev1$final_exposure)
table(prev1$final_exposure)
prev1$final_exposure[prev1$final_exposure == 
                       "diabetes_hypertension"] <- 'DM & HTN'
prev1$final_exposure[prev1$final_exposure == 
                       "diabetes"] <- 'DM'
prev1$final_exposure[prev1$final_exposure == 
                       "hypertension"] <- 'HTN'

prev1$pop1 <- ifelse((prev1$group == "death" & prev1$setting =="overall"),"death",
                    ifelse((prev1$group == "general" & prev1$setting =="outpatient"), "outpatient",
                    ifelse((prev1$group == "general" & prev1$setting =="inpatient")| 
                            (prev1$group == "severe" & prev1$setting =="inpatient") |
                            (prev1$group == "cardiac" & prev1$setting =="nr") |
                            (prev1$group == "cardiac" & prev1$setting =="inpatient"), "inpatient", ""))) 


prev1$pop2 <- ifelse (( prev1$group =="severe"), "severe", "")
prev1 <- prev1[,-c(17:27)]


#Cleaning for effect estimates
univ <- read_excel("C:/Users/Oscar Ponce/Documents/Research/Frank/COVID/5 Data analysis/finalV2.xlsx", 
                   sheet = "Non-exclusive data") 
#C:/Users/Oscar Ponce/Documents/Research/Frank/COVID/5 Data analysis/finalV2.xlsx

table(univ$selection_criteria)
names(univ)[names(univ) == "selection_criteria"] <- 'group'
univ$group[univ$group == 'general_prexisting_cardiac_conditions'] <- 'cardiac'
univ$group[univ$group == 'general_cardiac_injury\r\n'] <- 'cardiac'
univ$group[univ$group == 'asymptomatic'] <- 'general'
univ$group[univ$group == 'general_influenza'] <- 'general'
table(univ$group)




unimulti <- read_excel("C:/Users/Oscar Ponce/Documents/Research/Frank/COVID/5 Data analysis/final_unimulti.xlsx",
                       sheet = "unimulti") 
#C:/Users/Oscar Ponce/Documents/Research/Frank/COVID/5 Data analysis/final_unimulti.xlsx

unimulti <- unimulti[!(unimulti$`Time of measure of exposure`=="Symptom onset"),-8]
unimulti <- unimulti[!(unimulti$id %in% c(9123, 9171)), ]

names(unimulti)[4] <- 'unimulti'
names(unimulti)[5] <- 'adjusted'
names(unimulti)[7] <- 'effect'
names(unimulti)[8] <- 'eff'
names(unimulti)[9] <- 'llci'
names(unimulti)[10] <- 'ulci'
names(unimulti)[11] <- 'p'

unimulti[2][unimulti[2]=="Diabetes"] <- 'diabetes'
unimulti[2][unimulti[2]=="Hypertension"] <- 'hypertension'

unimulti[3][unimulti[3]=="Death"] <- 'death'
unimulti[3][unimulti[3]=="Severe disease at admission"] <- 'severe'
unimulti[3][unimulti[3]=="Severe/Critical Pneumonia"] <- 'severe'

unimulti[4][unimulti[4]=="Multivariate (From admission)"] <- 'multivariate'
unimulti[4][unimulti[4]=="Multivariate"] <- 'multivariate'

unimulti$p <- as.numeric(unimulti$p)
unimulti$p2 <- ifelse(unimulti$p == 0.001, 
                      paste("<",formatC(unimulti$p, format='f', digits =3)), 
                      formatC(unimulti$p, format='f', digits =3)) 

dm1 <- subset(unimulti, exposure == 'diabetes' & outcome == 'death' & unimulti == 'univariate')
dm2 <- subset(unimulti, exposure == 'diabetes' & outcome == 'death' & unimulti == 'multivariate')

ht1 <- subset(unimulti, exposure == 'hypertension' & outcome == 'severe' & unimulti == 'multivariate')
ht2 <- subset(unimulti, exposure == 'hypertension' & outcome == 'death' & unimulti == 'univariate')


#####Modifications or cleaning for individual plots (trial)#####
a <- BinomCI(prev1$events, prev1$n, 
             conf.level = 0.95,
             method = "logit")
prev1 <- cbind(prev1, a)
prev1[63,19] <- 0.02
prev1[63,20] <- 0.00
prev1[63,21] <- 0.22

prev1$prop <- paste(formatC(prev1$est, format='f', digits =2)," ",
                    "(",formatC(prev1$lwr.ci, format='f', digits =2),
                    "-",formatC(prev1$upr.ci, format='f', digits=2),")")

prev1$rate <- paste(prev1$events, "/", 
                    prev1$n)


prev1 <- prev1[order(prev1$est),]


#####Meta analysis of proportions by population type#####



ma1 <- rma.glmm(measure="PLO", xi=events, ni=n, subset=(final_exposure=="DM"), 
                data=prev1)
expma1 <- predict(ma1, transf=transf.ilogit, digits=3)




#Individual graph for ma1 (trial)

fma1 <- subset(prev1, final_exposure=="DM")
fma1 <- fma1 %>% add_row( .before = 32) 


tfma1 <- cbind( 
  c( "Author", fma1$author, 
     paste("Overall proportion for", ma1$k.eff, "studies","\n", 
  "(Tau^2 = ", (formatC(ma1$tau2, digits=2, format="f")), ", df = ", (ma1$k - ma1$p),
     ", p ", (ifelse(ma1$QMp < 0.001, 
                    paste("< 0.001"),
                    paste("= ", formatC(ma1$QMp, digits=2, format="f")))),
  "; ", "I^2", " = ", (formatC(ma1$I2, digits=1, format="f")), "%)")),
  c( "Frequency (n/N)",fma1$rate, paste(sum(ma1$xi), " / ",sum(ma1$ni))),
  c( "Prevalence (95% CI)", fma1$prop, 
     paste(formatC(expma1$pred, format='f', digits =2), 
           " (",formatC(expma1$ci.lb, format='f', digits=2),
           "-", formatC(expma1$ci.ub, format='f', digits=2), ")")))

rrsma1 <- structure(list(
  mean = c(NA,  fma1$est, expma1$pred, NA),
  lower = c(NA,  fma1$lwr.ci, expma1$ci.lb, NA),
  upper = c(NA,  fma1$upr.ci, expma1$ci.ub, NA)),
  .Names = c("mean", "lower", "upper"),
  row.names = c(NA, -35L),
  class = "data.frame")

tfma1 <- rbind(tfma1, NA)


trellis.device(device="windows", height = 25, width = 40, color=TRUE)
forestplot(tfma1,
           graph.pos = 3,
           zero = NA,
           rrsma1,
           new_page = TRUE,
           colgap = unit(5, "mm"),
           hrzl_lines = list("2" = gpar (lwd=1, columns=c(1:4), col="black") 
                             ),
           lineheight=unit(0.5,'cm'),
           line.margin = 2,
           is.summary = c(T, rep(F, 32), T),
           align = c("l","c"),
           ci.vertices = FALSE,
           txt_gp = fpTxtGp(label =gpar (cex=0.8), 
                            ticks = gpar(cex = 0.8, fontface="bold"),
                            summary = gpar(cex = 0.8),
                            xlab = gpar(cex=0.8)),
           xticks = c(0, 0.1, 0.2, 0.3, 0.4,0.5),
           xlog=FALSE,
           clip = c(0,  0.4),
           grid = gpar(lty=3, col="gray"),
           lwd.xaxis = 1,
           lwd.ci = 2.2,
           graphwidth = unit(10,"cm"),
           col=fpColors(box="black",line="grey", axes="grey20", summary="black"))
par(ask=F)


###


ma2 <- rma(measure="PLO", xi=events, ni=n, 
           subset=(final_exposure=="DM" & group=="general" & pop1=="outpatient" ), data=prev1)
expma2 <- predict(ma2, transf=transf.ilogit, digits=3)


ma3 <- rma.glmm(measure="PLO", xi=events, ni=n, 
                        subset=(final_exposure=="DM" & pop1 =="inpatient" ), data=prev1)
expma3 <- predict(ma3, transf=transf.ilogit, digits=3)


ma4 <- rma.glmm(measure="PLO", xi=events, ni=n, subset=(final_exposure=="DM" & pop2=="severe"), data=prev1)
expma4 <- predict(ma4, transf=transf.ilogit, digits=3)


ma5 <- rma(measure="PLO", xi=events, ni=n, subset=(final_exposure=="DM" & pop1=="death"), data=prev1)
expma5 <- predict(ma5, transf=transf.ilogit, digits=3)


#HTN

ma6 <- rma.glmm(measure="PLO", xi=events, ni=n, 
                subset=(final_exposure=="HTN"  & !(id== 1)), data=prev1)
expma6 <- predict(ma6, transf=transf.ilogit, digits=3)


ma7 <- rma.glmm(measure="PLO", xi=events, ni=n, 
                subset=(final_exposure=="HTN" & pop1=="outpatient"), data=prev1)
expma7 <- predict(ma7, transf=transf.ilogit, digits=3)

ma8 <- rma.glmm(measure="PLO", xi=events, ni=n, 
                subset=(final_exposure=="HTN" & pop1 =="inpatient"), data=prev1)
expma8 <- predict(ma8, transf=transf.ilogit, digits=3)

ht2 <- subset(prev1, final_exposure=="HTN" & pop1 == "inpatient")


trellis.device(device="windows", height = 25, width = 40, color=TRUE)
forest(ma8, transf=transf.ilogit, slab=paste(ht2$id))



ma9 <- rma.glmm(measure="PLO", xi=events, ni=n, 
                subset=(final_exposure=="HTN" & pop2=="severe"), data=prev1)
expma9 <- predict(ma9, transf=transf.ilogit, digits=3)

#HTN & DM
print(prev1[prev1$final_exposure=="DM & HTN" ,c(4,5,11,12,14,15,16,28,29)], order(prev1$id))

ma10 <- rma.glmm(measure="PLO", xi=events, ni=n, 
                 subset=(final_exposure=="DM & HTN" ), data=prev1)
expma10 <- predict(ma10, transf=transf.ilogit, digits=3)


ma12 <- rma(measure="PLO", xi=events, ni=n, 
                subset=(final_exposure=="DM & HTN" & pop2=="severe"), data=prev1)
expma12 <- predict(ma12, transf=transf.ilogit, digits=3)









x <- data.frame(rbind(
  c( "Overall",  ma1[[15]], sum(ma1[[55]]), sum(ma1[[61]]), ma1[[28]], expma1[[1]], expma1[[3]], expma1[[4]]), 
  c( "Outpatient",  ma2[[15]], 27, sum(ma2[[50]]), "na", expma2[[1]], expma2[[3]], expma2[[4]]), 
  c( "Inpatient",  ma3[[15]], sum(ma3[[55]]), sum(ma3[[61]]), ma3[[28]], expma3[[1]], expma3[[3]], expma3[[4]]), 
  c( "Severe COVID-19",  ma4[[15]], sum(ma4[[55]]), sum(ma4[[61]]), ma4[[28]], expma4[[1]], expma4[[3]], expma4[[4]]),
  c( "Death",  ma5[[15]], 16, sum(ma5[[50]]), "na", expma5[[1]], expma5[[3]], expma5[[4]]),
  
  c( "Overall",  ma6[[15]], sum(ma6[[55]]), sum(ma6[[61]]), ma6[[28]], expma6[[1]], expma6[[3]], expma6[[4]]), 
  c( "Outpatient",  ma7[[15]], sum(ma7[[55]]), sum(ma7[[61]]), ma7[[28]], expma7[[1]], expma7[[3]], expma7[[4]]), 
  c( "Inpatient",  ma8[[15]], sum(ma8[[55]]), sum(ma8[[61]]), ma8[[28]], expma8[[1]], expma8[[3]], expma8[[4]]), 
  c( "Severe COVID-19",  ma9[[15]], sum(ma9[[55]]), sum(ma9[[61]]), ma9[[28]], expma9[[1]], expma9[[3]], expma9[[4]]),  
  
  c( "Overall",  ma10[[15]], sum(ma10[[55]]), sum(ma10[[61]]), ma10[[28]], expma10[[1]], expma10[[3]], expma10[[4]]), 
  c( "Severe COVID-19",  ma12[[15]], 2, sum(ma12[[50]]), "na", expma12[[1]], expma12[[3]], expma12[[4]]) 
  
  ), 
  stringsAsFactors = FALSE) 



x[,2:8]<- sapply((x[,2:8]), as.numeric)

x[,9] <- paste(x[,3],"/",x[,4])
x[,10] <- paste("(",formatC(x[,7], format='f', digits =2),"-",formatC(x[,8], format='f', digits=2),")")
round2 = function(x, n=0) {scale<-10^n; sign(x)*trunc(abs(x)*scale+0.5)/scale}
x[,11] <- formatC( round2(x[,6], n=2), format='f', digits=2)
x[,5] <- round2(x[,5], n=0)
x$X5[c(1,3,4,6,7,8,9,10)] <- paste(x$X5[c(1,3,4,6,7,8,9,10)],"%")
x[,5] <- replace_na(x[,5],"na")
x[,9] <- as.character(x[,9])

x[,12] <- paste(x[,11], " ", x[,10])

view(x)


head <- c("Disease and subgroup", 'N of Studies', 'n', 'N', 'I^2', NA, NA, NA,
          'Frequency (n/N)', 'ci', 'pr2', 'Prevalence (95%CI)')
x1 <- rbind(head, x)
x1[,6:8]<- sapply((x1[,6:8]), as.numeric)


x1 <- x1 %>% add_row(X1='Diabetes', .before = 2) 
x1 <- x1 %>% add_row(X1="Hypertension", .before = 8) 
x1 <- x1 %>% add_row(X1="Diabetes and Hypertension", .before = 13) 


sub1 <- c(3,9,14) #overall
x1$X1[sub1] <- paste("  ",x1$X1[sub1]) 

sub2 <- c(4,5,7,10,11) #outpatient, inpatient, death
x1$X1[sub2] <- paste("     ",x1$X1[sub2]) 

sub3 <- c(6,12,15) #severe
x1$X1[sub3] <- paste("          ",x1$X1[sub3]) 

x2 <- x1[,c(1,2,9,5,12)]


rrs <- structure(list(
  mean = c(x1$X6),
  lower = c(x1$X7),
  upper = c(x1$X8)),
  .Names = c("mean", "lower", "upper"),
  row.names = c(NA, -15L),
  class = "data.frame")

trellis.device(device="windows", height = 25, width = 40, color=TRUE)
forestplot(x2,
           rrs,
           fn.ci_norm = fpDrawDiamondCI,
           graph.pos = 5,
           zero = NA,
           new_page = TRUE,
           colgap = unit(5, "mm"),
           hrzl_lines = list("2" = gpar (lwd=1, columns=c(1:6), col="black"), "3" = gpar (lwd=0.1, columns=c(1:5), col="black"), 
                             "9" = gpar (lwd=0.1, columns=c(1:5), col="black"), "14" = gpar (lwd=0.1, columns=c(1:5), col="black"), 
                             "16" = gpar (lwd=1, columns=c(1:6), col="black")),
           lineheight=unit(0.5,'cm'),
           line.margin = 2,
           is.summary = c(T, T, F, F, F, F, F, T, F, F, F, F, T, F, F),
           align = c("l","l"),
           ci.vertices = TRUE,
           txt_gp = fpTxtGp(ticks = gpar(cex = 0.8, fontface="bold"),
                            xlab  = gpar(cex = 0.8),
                            label = gpar(cex = 0.8),
                            summary = gpar(cex = 0.8)),
           col=fpColors(box="black", 
                        line="grey", 
                        summary="black", 
                        zero='grey20', 
                        axes='grey20'),
           xticks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
           xticks.digits = c(0, 0.1, 0.2, 0.3),
           xlog=FALSE,
           grid=gpar(lty=3, col="gray"),
           boxsize = unit(0.22, "cm"),
           lwd.xaxis = 1,
           lwd.ci = 2.2,
           graphwidth = unit(8,"cm"))
par(ask=F)


help("forestplot")

#####Meta analysis of proportions by country####
table(prev1$country)

print(prev1[prev1$final_exposure=="DM" ,c(1,2,4,5,11,12,14,15,16,28,29)])

ma1 #overall DM
expma1 #overall DM

ma13 <- rma(measure="PLO", xi=events, ni=n, subset=(final_exposure=="DM" & country=="austalia"), data=prev1)
expma13 <- predict(ma13, transf=transf.ilogit, digits=3)

ma14 <- rma.glmm(measure="PLO", xi=events, ni=n, subset=(final_exposure=="DM" & country=="china"), data=prev1)
expma14 <- predict(ma14, transf=transf.ilogit, digits=3)

ma15 <- rma(measure="PLO", xi=events, ni=n, subset=(final_exposure=="DM" & country=="singapore"), data=prev1)
expma15 <- predict(ma15, transf=transf.ilogit, digits=3)

ma16 <- rma(measure="PLO", xi=events, ni=n, subset=(final_exposure=="DM" & country=="hong_kong"), data=prev1)
expma16 <- predict(ma16, transf=transf.ilogit, digits=3)

ma17 <- rma.glmm(measure="PLO", xi=events, ni=n, subset=(final_exposure=="DM" & country=="korea"), data=prev1)
expma17 <- predict(ma17, transf=transf.ilogit, digits=3) 

ma18 <- rma(measure="PLO", xi=events, ni=n, subset=(final_exposure=="DM" & country=="italy"), data=prev1)
expma18 <- predict(ma18, transf=transf.ilogit, digits=3)

ma19 <- rma.glmm(measure="PLO", xi=events, ni=n, subset=(final_exposure=="DM" & country=="usa"), data=prev1)
expma19 <- predict(ma19, transf=transf.ilogit, digits=3)

 
ma6 # overall HTN
expma6 # overall HTN

ma20 <- rma(measure="PLO", xi=events, ni=n, subset=(final_exposure=="HTN" & country=="korea"), data=prev1)
expma20 <- predict(ma20, transf=transf.ilogit, digits=3)


ma21 <- rma(measure="PLO", xi=events, ni=n, subset=(final_exposure=="HTN" & country=="singapore"), data=prev1)
expma21 <- predict(ma21, transf=transf.ilogit, digits=3)

ma22 <- rma(measure="PLO", xi=events, ni=n, subset=(final_exposure=="HTN" & country=="bolivia"), data=prev1)
expma22 <- predict(ma22, transf=transf.ilogit, digits=3)

ma23 <- rma.glmm(measure="PLO", xi=events, ni=n, subset=(final_exposure=="HTN" & country=="usa"), data=prev1)
expma23 <- predict(ma23, transf=transf.ilogit, digits=3)

ma24 <- rma(measure="PLO", xi=events, ni=n, subset=(final_exposure=="HTN" & country=="france"), data=prev1)
expma24 <- predict(ma24, transf=transf.ilogit, digits=3)

ma25 <- rma(measure="PLO", xi=events, ni=n, subset=(final_exposure=="HTN" & country=="hong_kong"), data=prev1)
expma25 <- predict(ma24, transf=transf.ilogit, digits=3)

ma26 <- rma.glmm(measure="PLO", xi=events, ni=n, subset=(final_exposure=="HTN" & country=="china"), data=prev1)
expma26 <- predict(ma25, transf=transf.ilogit, digits=3)

ma27 <- rma.glmm(measure="PLO", xi=events, ni=n, subset=(final_exposure=="HTN" & country=="italy"), data=prev1)
expma27 <- predict(ma26, transf=transf.ilogit, digits=3)





print(prev1[prev1$final_exposure=="HTN" & prev1$country=="france",c(1,2,4,5,11,12,14,15,16,28,29)])


x <- data.frame(rbind(
  c( "group", "nstudies", "n", "N", "i2", "pr", "llci", "ulci"),
  c( "Overall",  ma1[[15]], sum(ma1[[55]]), sum(ma1[[61]]), ma1[[28]], expma1[[1]], expma1[[3]], expma1[[4]]), 
  c( "Australia",  ma13[[15]], 1, sum(ma13[[50]]), 'na', expma13[[1]], expma13[[3]], expma13[[4]]), 
  c( "Singapore",  ma15[[15]], 4, sum(ma15[[50]]), 'na', expma15[[1]], expma15[[3]], expma15[[4]]), 
  c( "China",  ma14[[15]], sum(ma14[[55]]), sum(ma14[[61]]), ma14[[28]], expma14[[1]], expma14[[3]], expma14[[4]]), 
  c( "Hong Kong",  ma16[[15]], 8, sum(ma16[[50]]), 'na', expma16[[1]], expma16[[3]], expma16[[4]]), 
  c( "South Korea",  ma17[[15]], sum(ma17[[55]]), sum(ma17[[61]]), ma17[[28]], expma17[[1]], expma17[[3]], expma17[[4]]),
  c( "Italy",  ma18[[15]], 180, sum(ma18[[50]]), 'na', expma18[[1]], expma18[[3]], expma18[[4]]), 
  c( "USA",  ma19[[15]], sum(ma19[[55]]), sum(ma19[[61]]), ma19[[28]], expma19[[1]], expma19[[3]], expma19[[4]]),
  
  c( "Overall",  ma6[[15]], sum(ma6[[55]]), sum(ma6[[61]]), ma6[[28]], expma6[[1]], expma6[[3]], expma6[[4]]), 
  c( "South Korea",  ma20[[15]], 0, sum(ma20[[50]]), 'na', expma20[[1]], expma20[[3]], expma20[[4]]), 
  c( "Singapore",  ma21[[15]], 1, sum(ma21[[50]]), 'na', expma21[[1]], expma21[[3]], expma21[[4]]), 
  c( "Bolivia",  ma22[[15]], 1, sum(ma22[[50]]), 'na', expma22[[1]], expma22[[3]], expma22[[4]]), 
  c( "USA",  ma23[[15]], sum(ma23[[55]]), sum(ma23[[61]]), ma23[[28]], expma23[[1]], expma23[[3]], expma23[[4]]), 
  c( "Italy",  ma27[[15]], sum(ma27[[55]]), sum(ma27[[61]]), ma27[[28]], expma27[[1]], expma27[[3]], expma27[[4]]), 
  c( "France",  ma24[[15]], 1, sum(ma24[[50]]), 'na', expma24[[1]], expma24[[3]], expma24[[4]]), 
  c( "Hong-Kong",  ma25[[15]], 13, sum(ma25[[50]]), 'na', expma25[[1]], expma25[[3]], expma25[[4]]), 
  c( "China",  ma26[[15]], sum(ma26[[55]]), sum(ma26[[61]]), ma26[[28]], expma26[[1]], expma26[[3]], expma26[[4]]) 
  ),stringsAsFactors = FALSE) 

colnames(x) <- x[1,]
x <- x[-1, ] 
x[,2:8]<- sapply((x[,2:8]), as.numeric)
x <- transform(x, prop=paste(n, N, sep="/"))
x$ci <- paste("(",formatC(x$llci, format='f', digits =2),"-",formatC(x$ulci, format='f', digits=2),")")
round2 = function(x, n=0) {scale<-10^n; sign(x)*trunc(abs(x)*scale+0.5)/scale}
x$pr2 <- formatC( round2(x$pr, n=2), format='f', digits=2)

x$i2 <- round2(x$i2, n=0)
x$prop <- as.character(x$prop)
x$i2[c(1, 4, 6, 8, 9, 13, 14, 17)] <- paste(x$i2[c(1, 4, 6, 8, 9, 13, 14, 17)],"%")

x$i2[c(2,3,5,7,10,11,12,15,16)] <- replace_na(x$i2[c(2,3,5,7,10,11,12,15,16)],"na")
View(x)

x$prci <- ifelse(is.na(x$pr2), NA,paste(x$pr2, " ", x$ci))


x <- x %>% add_row(group="Diabetes", .before = 1) 
x <- x %>% add_row(group="Hypertension", .before = 10) 


sub1 <- c(2,11) #overall
x$group[sub1] <- paste("   ",x$group[sub1]) 

sub2 <- c(3:9,12:19) #countries
x$group[sub2] <- paste("      ",x$group[sub2]) 



tabletext <- cbind( 
  c( "Disease and subgroup", x$group),
  c( "N of Studies",x$nstudies),
  c( "Frequency (n/N)",x$prop),
  c( "I^2", x$i2),
  c( "Prevalence (95% CI)", x$prci))



rrs <- structure(list(
  mean = c(NA,  x$pr),
  lower = c(NA,  x$llci),
  upper = c(NA,  x$ulci)),
  .Names = c("mean", "lower", "upper"),
  row.names = c(NA, -20L),
  class = "data.frame")



trellis.device(device="windows", height = 25, width = 40, color=TRUE)
forestplot(tabletext,
           fn.ci_norm = fpDrawDiamondCI,
           graph.pos = 5,
           zero = NA,
           rrs,new_page = TRUE,
           colgap = unit(5, "mm"),
           hrzl_lines = list("2" = gpar (lwd=1, columns=c(1:6), col="black"), "3" = gpar (lwd=0.1, columns=c(1:5), col="black")
                             , "12" = gpar (lwd=0.1, columns=c(1:5), col="black"), "21" = gpar (lwd=1, columns=c(1:6), col="black")),
           lineheight=unit(0.5,'cm'),
           line.margin = 2,
           is.summary = c(T,T,F,F,F,F,F,F,F,F,
                          T,F,F,F,F,F,F,F,F,F),
           align = c("l","l"),
           ci.vertices = TRUE,
           txt_gp = fpTxtGp(label =gpar (cex=0.8), 
                            ticks = gpar(cex = 0.8, fontface="bold"),
                            summary = gpar(cex = 0.8),
                            xlab = gpar(cex=0.8)),
           xticks = c(0, 0.1, 0.2, 0.3, 0.4,0.5, 0.6, 0.7),
           xlog=FALSE,
           grid = gpar(lty=3, col="gray"),
           boxsize = unit(0.22, "cm"),
           lwd.xaxis = 1,
           lwd.ci = 2.2,
           graphwidth = unit(8,"cm"),
           col=fpColors(box="black",line="grey", axes="grey20"))
par(ask=F)




#####Meta analysis of estimate of effects univariate and multivariate together#####


#Univariate RR for DM, HT and DM&HT



ma28 <- rma(measure="RR", ai=severe1, ci=severe2, n1i=n1, n2i=n2,
           subset=(final_exposure=="diabetes" & group=="general" & !is.na(severe1)), data=univ, method="REML")
expma28 <- predict(ma28, transf = transf.exp.int)

ma29 <- rma(measure="RR", ai=icu1, ci=icu2, n1i=n1, n2i=n2,
             subset=(final_exposure=="diabetes" & group=="general" & !is.na(icu1)), data=univ, method="REML")
expma29 <- predict(ma29, transf=transf.exp.int)

ma30 <- rma(measure="RR", ai=death1, ci=death2, n1i=n1, n2i=n2, 
           subset=(final_exposure=="diabetes" & group=="general" & !is.na(death1)), data=univ, method="REML")
expma30 <- predict(ma30, transf=transf.exp.int)


ma31 <- rma(measure="RR", ai=severe1, ci=severe2, n1i=n1, n2i=n2,
            subset=(final_exposure=="hypertension" & group=="general" & !is.na(severe1)), data=univ, method="REML")
expma31 <- predict(ma31, transf = transf.exp.int)

ma32 <- rma(measure="RR", ai=icu1, ci=icu2, n1i=n1, n2i=n2,
            subset=(final_exposure=="hypertension" & group=="general" & !is.na(icu1)), data=univ, method="REML")
expma32 <- predict(ma32, transf=transf.exp.int)

ma33 <- rma(measure="RR", ai=death1, ci=death2, n1i=n1, n2i=n2, 
            subset=(final_exposure=="hypertension" & group=="general" & !is.na(death1)), data=univ, method="REML")
expma33 <- predict(ma33, transf=transf.exp.int)


ma34 <- rma(measure="RR", ai=severe1, ci=severe2, n1i=n1, n2i=n2,
            subset=(final_exposure=="diabetes_hypertension" & group=="general" & !is.na(severe1)), data=univ, method="REML")
expma34 <- predict(ma34, transf = transf.exp.int)




x <- data.frame(rbind(
  c( "exposure", "outcome", "analysis (eff)", "nstudies", "n", "N", "i2", "eff", "llci", "ulci", "eff2", "ci", "p"),
  
  c(NA, "Severe COVID-19", "Unadjusted (RR)", ma28[[14]], sum(ma28[[42]], ma28[[44]]), sum(ma28[[50]]), ma28[[25]], expma28[[1]], expma28[[3]], expma28[[4]], NA, NA, ma28[[5]]),

  c(NA, "ICU admission", "Unadjusted (RR)",  ma29[[14]], sum(ma29[[42]], ma29[[44]]), sum(ma29[[50]]), ma29[[25]], expma29[[1]], expma29[[3]], expma29[[4]], NA, NA, ma29[[5]]),
  
  c(NA, "Mortality", "Unadjusted (RR)", ma30[[14]], sum(ma30[[42]], ma30[[44]]), sum(ma30[[50]]), ma30[[25]], expma30[[1]], expma30[[3]], expma30[[4]], NA, NA, ma30[[5]]),
  c(NA, NA, "Unadjusted (HR)", 1, "NR", dm1$n, "NA", dm1$eff, dm1$llci, dm1$ulci, NA, NA, dm1$p2),
  c(NA, NA,  "Adjusted (HR)*", 1, "NR", dm2$n, "NA", dm2$eff, dm2$llci, dm2$ulci, NA, NA, dm2$p2),
  
  
  c(NA, "Severe COVID-19", "Unadjusted (RR)", ma31[[14]], sum(ma31[[42]], ma31[[44]]), sum(ma31[[50]]), ma31[[25]], expma31[[1]], expma31[[3]], expma31[[4]], NA, NA, ma31[[5]]),
  c(NA, NA, "Adjusted (OR)**", 1, "NR", ht1$n, "NA", ht1$eff, ht1$llci, ht1$ulci, NA, NA, ht1$p2),
  
  c(NA, "ICU admission", "Unadjusted (RR)", ma32[[14]], sum(ma32[[42]], ma32[[44]]), sum(ma32[[50]]), ma32[[25]], expma32[[1]], expma32[[3]], expma32[[4]], NA, NA, ma32[[5]]),

  c(NA, "Mortality", "Unadjusted (RR)", ma33[[14]], sum(ma33[[42]], ma33[[44]]), sum(ma33[[50]]), ma33[[25]], expma33[[1]], expma33[[3]], expma33[[4]], NA, NA, ma33[[5]]),

  c(NA, "Severe COVID-19", "Unadjusted (RR)", ma34[[14]], sum(ma34[[42]], ma34[[44]]), sum(ma34[[50]]), "NA", expma34[[1]], expma34[[3]], expma34[[4]], NA, NA, ma34[[5]]),
  c(NA, NA, "Unadjusted (HR)", 1, "NR", ht2$n, "NA", ht2$eff, ht2$llci, ht2$ulci, NA, NA, ht2$p2)), 
  stringsAsFactors = FALSE)


colnames(x) <- x[1,]
x <- x[-1, ] 

View(x)

x[,7:10]<- sapply((x[,7:10]), as.numeric)
x$ci <- ifelse(!is.na(x$N) & is.na(x$ci),
               paste("(",formatC(x$llci, format='f', digits =2),"-", formatC(x$ulci, format='f', digits=2),")"),
               x$ci)
round2 = function(x, n=0) {scale<-10^n; sign(x)*trunc(abs(x)*scale+0.5)/scale}
x$eff2 <- ifelse(!is.na(x$N) & is.na(x$eff2), 
                 formatC( round2(x$eff, n=2), format='f', digits=2), x$eff2)
x$i2 <- round2(x$i2, n=0)
x$i2 <- ifelse(is.na(x$i2), NA, paste(x$i2, "%"))
x$i2 <- ifelse(!is.na(x$`analysis (eff)`) & is.na(x$i2), paste("na"), x$i2)
x$effci <- ifelse(is.na(x$eff2), NA, paste(x$eff2," ", x$ci))

x$p <- as.numeric(x$p)
x$p1 <- ifelse(is.na(x$p), NA, paste(formatC(x$p, format='f', digits =3)))
x$p1 <- ifelse(x$p1 == "0.000", paste("<0.001"), x$p1)

x <- x %>% add_row(.before = 1) 
x <- x %>% add_row(outcome="Diabetes", .before = 2) 
x <- x %>% add_row(.before = 8) 
x <- x %>% add_row(outcome="Hypertension", .before = 9) 
x <- x %>% add_row(.before = 14) 
x <- x %>% add_row(outcome="Diabetes and Hypertension", .before = 15) 
x <- x %>% add_row(.before = 18) 

sub1 <- c(3:5, 10, 12, 13, 16)
x$outcome[sub1] <- paste("    ",x$outcome[sub1]) 




tabletext <- cbind( 
  c( "Disease and outcome", x$outcome),
  c( "Effect estimate", x$`analysis (eff)`),
  c( "N of Studies",x$nstudies),
  c( "Total N",x$N),
  c( "I^2", x$i2),
  c( "Effect size (95%CI)", x$effci),
  c( "p value", x$p1))

rrs <- structure(list(
  mean = c(NA,  x$eff),
  lower = c(NA,  x$llci),
  upper = c(NA,  x$ulci)),
  .Names = c("mean", "lower", "upper"),
  row.names = c(NA, -19),
  class = "data.frame")



trellis.device(device="windows", height = 30, width = 40, color=TRUE)


forestplot(tabletext,
           fn.ci_norm = fpDrawDiamondCI,
           graph.pos = 6,
          
           rrs,new_page = TRUE,
           colgap = unit(2, "mm"),
           hrzl_lines = list("2" = gpar (lwd=2, columns=c(1:8), col="black"), "4" = gpar (lwd=0.1, columns=c(1:6), col="black")
                             , "11" = gpar (lwd=0.1, columns=c(1:6), col="black"), 
                             "17" = gpar (lwd=0.1, columns=c(1:6), col="black"), 
                             "20" = gpar (lwd=2, columns=c(1:8), col="black")),
           lineheight=unit(0.8,'cm'),
           line.margin = 2,
           is.summary = c(T,F,T, rep(F, 6), T, rep(F, 5), T, F, F),
           align = c("l","l"),
           
           
           ci.vertices = TRUE,
           txt_gp = fpTxtGp(ticks = gpar(cex = 1.2, fontface="bold"),
                            xlab  = gpar(cex = 1.2),
                            label = gpar(cex = 1.2),
                            summary = gpar(cex = 1.2)),
           col=fpColors(box="black", 
                        line="darkgrey", 
                        summary="black", 
                        zero='black', 
                        axes='grey20'),
           xticks = c(0.5,1, 2, 4, 8, 16),
           xlog=TRUE,
           clip=TRUE,
           boxsize = unit(0.3, "cm"), 
           lwd.xaxis = 1, 
           lwd.ci = 3.3,
           lwd.zero = 2)

par(ask=F)




