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
list.files()


prev1 <- read.csv("C:/Users/Oscar Ponce/Documents/Research/GitHub/dm_ht_covid/databases/proportions.csv")

#General info of included articles
info <- read.csv("C:/Users/Oscar Ponce/Documents/Research/GitHub/dm_ht_covid/databases/general_info.csv")


names(info)[1] <- 'id'
names(info)[3] <- 'author'


#General info of included articles


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
univ <- read.csv("C:/Users/Oscar Ponce/Documents/Research/GitHub/dm_ht_covid/databases/effectsize.csv")



table(univ$selection_criteria)
names(univ)[names(univ) == "selection_criteria"] <- 'group'
univ$group[univ$group == 'general_prexisting_cardiac_conditions'] <- 'cardiac'
univ$group[univ$group == 'general_cardiac_injury\r\n'] <- 'cardiac'
univ$group[univ$group == 'asymptomatic'] <- 'general'
univ$group[univ$group == 'general_influenza'] <- 'general'
table(univ$group)
names(univ)[names(univ) == 'id...20']<- 'id'



unimulti <- read.csv("C:/Users/Oscar Ponce/Documents/Research/GitHub/dm_ht_covid/databases/unimulti.csv")



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



univ$author <- info$author[match(univ$id, info$id)]



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
           ", p ", (ifelse(ma1$QEp.Wld < 0.001, 
                           paste("< 0.001"),
                           paste("= ", formatC(ma1$QEp.Wld, digits=2, format="f")))),
           "; ", "I^2", " = ", (formatC(ma1$I2, digits=1, format="f")), "%)")),
  c( "Frequency (n/N)",fma1$rate, paste(sum(ma1$xi), " / ",sum(ma1$ni))),
  c( "Prevalence (95% CI)", fma1$prop, 
     paste(formatC(expma1$pred, format='f', digits =2), 
           " (",formatC(expma1$ci.lb, format='f', digits=2),
           "-", formatC(expma1$ci.ub, format='f', digits=2), ")")))

rrsma1 <- structure(list(
  mean = c(NA,  fma1$est, expma1$pred, NA),
  lower = c(NA,   fma1$lwr.ci, expma1$ci.lb, NA),
  upper = c(NA,   fma1$upr.ci, expma1$ci.ub, NA)),
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


##



ma3 <- rma.glmm(measure="PLO", xi=events, ni=n, 
                subset=(final_exposure=="DM" & pop1 =="inpatient" ), data=prev1)
expma3 <- predict(ma3, transf=transf.ilogit, digits=3)


##


fma3 <- subset(prev1, final_exposure=="DM" & pop1 =="inpatient")
fma3 <- rbind(fma3, NA)

tfma3 <- cbind( 
  c( "Author", fma3$author, 
     paste("Overall proportion for", ma3$k.eff, "studies","\n", 
           "(Tau^2 = ", (formatC(ma3$tau2, digits=2, format="f")), ", df = ", (ma3$k - ma3$p),
           ", p ", (ifelse(ma3$QEp.Wld < 0.001, 
                           paste("< 0.001"),
                           paste("= ", formatC(ma3$QEp.Wld, digits=2, format="f")))),
           "; ", "I^2", " = ", (formatC(ma3$I2, digits=1, format="f")), "%)")),
  c( "Frequency (n/N)",fma3$rate, paste(sum(ma3$xi), " / ",sum(ma3$ni))),
  c( "Prevalence (95% CI)", fma3$prop, 
     paste(formatC(expma3$pred, format='f', digits =2), 
           " (",formatC(expma3$ci.lb, format='f', digits=2),
           "-", formatC(expma3$ci.ub, format='f', digits=2), ")")))


rrsma3 <- structure(list(
  mean = c(NA,  fma3$est, expma3$pred, NA),
  lower = c(NA,  fma3$lwr.ci, expma3$ci.lb, NA),
  upper = c(NA,  fma3$upr.ci, expma3$ci.ub, NA)),
  .Names = c("mean", "lower", "upper"),
  row.names = c(NA, -30L),
  class = "data.frame")

tfma3 <- rbind(tfma3, NA)

trellis.device(device="windows", height = 25, width = 40, color=TRUE)
forestplot(tfma3,
           graph.pos = 3,
           zero = NA,
           rrsma3,
           new_page = TRUE,
           colgap = unit(5, "mm"),
           hrzl_lines = list("2" = gpar (lwd=1, columns=c(1:4), col="black") 
           ),
           lineheight=unit(0.5,'cm'),
           line.margin = 2,
           is.summary = c(T, rep(F, 27), T, F),
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




##

ma4 <- rma.glmm(measure="PLO", xi=events, ni=n, subset=(final_exposure=="DM" & pop2=="severe"), data=prev1)
expma4 <- predict(ma4, transf=transf.ilogit, digits=3)


fma4 <- subset(prev1, final_exposure=="DM" & pop2=="severe")
fma4 <- rbind(fma4, NA)

tfma4 <- cbind( 
  c( "Author", fma4$author, 
     paste("Overall proportion for", ma4$k.eff, "studies","\n", 
           "(Tau^2 = ", (formatC(ma4$tau2, digits=2, format="f")), ", df = ", (ma4$k - ma4$p),
           ", p ", (ifelse(ma4$QEp.Wld < 0.001, 
                           paste("< 0.001"),
                           paste("= ", formatC(ma4$QEp.Wld, digits=2, format="f")))),
           "; ", "I^2", " = ", (formatC(ma4$I2, digits=1, format="f")), "%)")),
  c( "Frequency (n/N)",fma4$rate, paste(sum(ma4$xi), " / ",sum(ma4$ni))),
  c( "Prevalence (95% CI)", fma4$prop, 
     paste(formatC(expma4$pred, format='f', digits =2), 
           " (",formatC(expma4$ci.lb, format='f', digits=2),
           "-", formatC(expma4$ci.ub, format='f', digits=2), ")")))

rrsma4 <- structure(list(
  mean = c(NA,  fma4$est, expma4$pred, NA),
  lower = c(NA,  fma4$lwr.ci, expma4$ci.lb, NA),
  upper = c(NA,  fma4$upr.ci, expma4$ci.ub, NA)),
  .Names = c("mean", "lower", "upper"),
  row.names = c(NA, -9L),
  class = "data.frame")

tfma4 <- rbind(tfma4, NA)

trellis.device(device="windows", height = 25, width = 40, color=TRUE)
forestplot(tfma4,
           graph.pos = 3,
           zero = NA,
           rrsma4,
           new_page = TRUE,
           colgap = unit(5, "mm"),
           hrzl_lines = list("2" = gpar (lwd=1, columns=c(1:4), col="black") 
           ),
           lineheight=unit(0.5,'cm'),
           line.margin = 2,
           is.summary = c(T, rep(F, 6), T, F),
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


##

ma5 <- rma(measure="PLO", xi=events, ni=n, subset=(final_exposure=="DM" & pop1=="death"), data=prev1)
expma5 <- predict(ma5, transf=transf.ilogit, digits=3)



#HTN

ma6 <- rma.glmm(measure="PLO", xi=events, ni=n, 
                subset=(final_exposure=="HTN"  & !(id== 1)), data=prev1)
expma6 <- predict(ma6, transf=transf.ilogit, digits=3)





fma6 <- subset(prev1, final_exposure=="HTN"  & !(id== 1))
fma6 <- rbind(fma6, NA)

tfma6 <- cbind( 
  c( "Author", fma6$author, 
     paste("Overall proportion for", ma6$k.eff, "studies","\n", 
           "(Tau^2 = ", (formatC(ma6$tau2, digits=2, format="f")), ", df = ", (ma6$k - ma6$p),
           ", p ", (ifelse(ma6$QEp.Wld < 0.001, 
                           paste("< 0.001"),
                           paste("= ", formatC(ma6$QEp.Wld, digits=2, format="f")))),
           "; ", "I^2", " = ", (formatC(ma6$I2, digits=1, format="f")), "%)")),
  c( "Frequency (n/N)",fma6$rate, paste(sum(ma6$xi), " / ",sum(ma6$ni))),
  c( "Prevalence (95% CI)", fma6$prop, 
     paste(formatC(expma6$pred, format='f', digits =2), 
           " (",formatC(expma6$ci.lb, format='f', digits=2),
           "-", formatC(expma6$ci.ub, format='f', digits=2), ")")))

rrsma6 <- structure(list(
  mean = c(NA,  fma6$est, expma6$pred, NA),
  lower = c(NA,  fma6$lwr.ci, expma6$ci.lb, NA),
  upper = c(NA,  fma6$upr.ci, expma6$ci.ub, NA)),
  .Names = c("mean", "lower", "upper"),
  row.names = c(NA, -41L),
  class = "data.frame")

tfma6 <- rbind(tfma6, NA)


trellis.device(device="windows", height = 25, width = 40, color=TRUE)
forestplot(tfma6,
           graph.pos = 3,
           zero = NA,
           rrsma6,
           new_page = TRUE,
           colgap = unit(5, "mm"),
           hrzl_lines = list("2" = gpar (lwd=1, columns=c(1:4), col="black") 
           ),
           lineheight=unit(0.5,'cm'),
           line.margin = 2,
           is.summary = c(T, rep(F, 38), T, F),
           align = c("l","c"),
           ci.vertices = FALSE,
           txt_gp = fpTxtGp(label =gpar (cex=0.8), 
                            ticks = gpar(cex = 0.8, fontface="bold"),
                            summary = gpar(cex = 0.8),
                            xlab = gpar(cex=0.8)),
           xticks = c(0, 0.1, 0.2, 0.3, 0.4,0.5, 0.6, 0.7),
           xlog=FALSE,
           clip = c(0,  0.4),
           grid = gpar(lty=3, col="gray"),
           lwd.xaxis = 1,
           lwd.ci = 2.2,
           graphwidth = unit(10,"cm"),
           col=fpColors(box="black",line="grey", axes="grey20", summary="black"))
par(ask=F)



##

ma7 <- rma.glmm(measure="PLO", xi=events, ni=n, 
                subset=(final_exposure=="HTN" & pop1=="outpatient"), data=prev1)
expma7 <- predict(ma7, transf=transf.ilogit, digits=3)


fma7 <- subset(prev1, final_exposure=="HTN"  & pop1=="outpatient")
fma7 <- rbind(fma7, NA)

tfma7 <- cbind( 
  c( "Author", fma7$author, 
     paste("Overall proportion for", ma7$k.eff, "studies","\n", 
           "(Tau^2 = ", (formatC(ma7$tau2, digits=2, format="f")), ", df = ", (ma7$k - ma7$p),
           ", p ", (ifelse(ma7$QEp.Wld < 0.001, 
                           paste("< 0.001"),
                           paste("= ", formatC(ma7$QEp.Wld, digits=2, format="f")))),
           "; ", "I^2", " = ", (formatC(ma7$I2, digits=1, format="f")), "%)")),
  c( "Frequency (n/N)",fma7$rate, paste(sum(ma7$xi), " / ",sum(ma7$ni))),
  c( "Prevalence (95% CI)", fma7$prop, 
     paste(formatC(expma7$pred, format='f', digits =2), 
           " (",formatC(expma7$ci.lb, format='f', digits=2),
           "-", formatC(expma7$ci.ub, format='f', digits=2), ")")))


rrsma7 <- structure(list(
  mean = c(NA,  fma7$est, expma7$pred, NA),
  lower = c(NA,  fma7$lwr.ci, expma7$ci.lb, NA),
  upper = c(NA,  fma7$upr.ci, expma7$ci.ub, NA)),
  .Names = c("mean", "lower", "upper"),
  row.names = c(NA, -6L),
  class = "data.frame")

tfma7 <- rbind(tfma7, NA)


trellis.device(device="windows", height = 25, width = 40, color=TRUE)
forestplot(tfma7,
           graph.pos = 3,
           zero = NA,
           rrsma7,
           new_page = TRUE,
           colgap = unit(5, "mm"),
           hrzl_lines = list("2" = gpar (lwd=1, columns=c(1:4), col="black") 
           ),
           lineheight=unit(0.7,'cm'),
           line.margin = 2,
           is.summary = c(T, rep(F, 3), T, F),
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







ma8 <- rma.glmm(measure="PLO", xi=events, ni=n, 
                subset=(final_exposure=="HTN" & pop1 =="inpatient"), data=prev1)
expma8 <- predict(ma8, transf=transf.ilogit, digits=3)



fma8 <- subset(prev1, final_exposure=="HTN" & pop1 =="inpatient")
fma8 <- rbind(fma8, NA)

tfma8 <- cbind( 
  c( "Author", fma8$author, 
     paste("Overall proportion for", ma8$k.eff, "studies","\n", 
           "(Tau^2 = ", (formatC(ma8$tau2, digits=2, format="f")), ", df = ", (ma8$k - ma8$p),
           ", p ", (ifelse(ma8$QEp.Wld < 0.001, 
                           paste("< 0.001"),
                           paste("= ", formatC(ma8$QEp.Wld, digits=2, format="f")))),
           "; ", "I^2", " = ", (formatC(ma8$I2, digits=1, format="f")), "%)")),
  c( "Frequency (n/N)",fma8$rate, paste(sum(ma8$xi), " / ",sum(ma8$ni))),
  c( "Prevalence (95% CI)", fma8$prop, 
     paste(formatC(expma8$pred, format='f', digits =2), 
           " (",formatC(expma8$ci.lb, format='f', digits=2),
           "-", formatC(expma8$ci.ub, format='f', digits=2), ")")))

rrsma8 <- structure(list(
  mean = c(NA,  fma8$est, expma8$pred, NA),
  lower = c(NA,  fma8$lwr.ci, expma8$ci.lb, NA),
  upper = c(NA,  fma8$upr.ci, expma8$ci.ub, NA)),
  .Names = c("mean", "lower", "upper"),
  row.names = c(NA, -37L),
  class = "data.frame")

tfma8 <- rbind(tfma8, NA)

View(tfma8)

trellis.device(device="windows", height = 25, width = 40, color=TRUE)
forestplot(tfma8,
           graph.pos = 3,
           zero = NA,
           rrsma8,
           new_page = TRUE,
           colgap = unit(5, "mm"),
           hrzl_lines = list("2" = gpar (lwd=1, columns=c(1:4), col="black") 
           ),
           lineheight=unit(0.5,'cm'),
           line.margin = 2,
           is.summary = c(T, rep(F, 34), T, F),
           align = c("l","c"),
           ci.vertices = FALSE,
           txt_gp = fpTxtGp(label =gpar (cex=0.8), 
                            ticks = gpar(cex = 0.8, fontface="bold"),
                            summary = gpar(cex = 0.8),
                            xlab = gpar(cex=0.8)),
           xticks = c(0, 0.1, 0.2, 0.3, 0.4,0.5, 0.6, 0.7),
           xlog=FALSE,
           clip = c(0,  0.4),
           grid = gpar(lty=3, col="gray"),
           lwd.xaxis = 1,
           lwd.ci = 2.2,
           graphwidth = unit(10,"cm"),
           col=fpColors(box="black",line="grey", axes="grey20", summary="black"))
par(ask=F)







ma9 <- rma.glmm(measure="PLO", xi=events, ni=n, 
                subset=(final_exposure=="HTN" & pop2=="severe"), data=prev1)
expma9 <- predict(ma9, transf=transf.ilogit, digits=3)



fma9 <- subset(prev1, final_exposure=="HTN" & pop2=="severe")
fma9 <- rbind(fma9, NA)

tfma9 <- cbind( 
  c( "Author", fma9$author, 
     paste("Overall proportion for", ma9$k.eff, "studies","\n", 
           "(Tau^2 = ", (formatC(ma9$tau2, digits=2, format="f")), ", df = ", (ma9$k - ma9$p),
           ", p ", (ifelse(ma9$QEp.Wld < 0.001, 
                           paste("< 0.001"),
                           paste("= ", formatC(ma9$QEp.Wld, digits=2, format="f")))),
           "; ", "I^2", " = ", (formatC(ma9$I2, digits=1, format="f")), "%)")),
  c( "Frequency (n/N)",fma9$rate, paste(sum(ma9$xi), " / ",sum(ma9$ni))),
  c( "Prevalence (95% CI)", fma9$prop, 
     paste(formatC(expma9$pred, format='f', digits =2), 
           " (",formatC(expma9$ci.lb, format='f', digits=2),
           "-", formatC(expma9$ci.ub, format='f', digits=2), ")")))

rrsma9 <- structure(list(
  mean = c(NA,  fma9$est, expma9$pred, NA),
  lower = c(NA,  fma9$lwr.ci, expma9$ci.lb, NA),
  upper = c(NA,  fma9$upr.ci, expma9$ci.ub, NA)),
  .Names = c("mean", "lower", "upper"),
  row.names = c(NA, -8L),
  class = "data.frame")

tfma9 <- rbind(tfma9, NA)


trellis.device(device="windows", height = 25, width = 40, color=TRUE)
forestplot(tfma9,
           graph.pos = 3,
           zero = NA,
           rrsma9,
           new_page = TRUE,
           colgap = unit(5, "mm"),
           hrzl_lines = list("2" = gpar (lwd=1, columns=c(1:4), col="black") 
           ),
           lineheight=unit(0.6,'cm'),
           line.margin = 2,
           is.summary = c(T, rep(F, 5), T, F),
           align = c("l","c"),
           ci.vertices = FALSE,
           txt_gp = fpTxtGp(label =gpar (cex=0.8), 
                            ticks = gpar(cex = 0.8, fontface="bold"),
                            summary = gpar(cex = 0.8),
                            xlab = gpar(cex=0.8)),
           xticks = c(0, 0.1, 0.2, 0.3, 0.4,0.5, 0.6, 0.7),
           xlog=FALSE,
           clip = c(0,  0.4),
           grid = gpar(lty=3, col="gray"),
           lwd.xaxis = 1,
           lwd.ci = 2.2,
           graphwidth = unit(10,"cm"),
           col=fpColors(box="black",line="grey", axes="grey20", summary="black"))
par(ask=F)










#HTN & DM
print(prev1[prev1$final_exposure=="DM & HTN" ,c(4,5,11,12,14,15,16,28,29)], order(prev1$id))

ma10 <- rma.glmm(measure="PLO", xi=events, ni=n, 
                 subset=(final_exposure=="DM & HTN" ), data=prev1)
expma10 <- predict(ma10, transf=transf.ilogit, digits=3)



fma10 <- subset(prev1, final_exposure=="DM & HTN")
fma10 <- rbind(fma10, NA)

tfma10 <- cbind( 
  c( "Author", fma10$author, 
     paste("Overall proportion for", ma10$k.eff, "studies","\n", 
           "(Tau^2 = ", (formatC(ma10$tau2, digits=2, format="f")), ", df = ", (ma10$k - ma10$p),
           ", p ", (ifelse(ma10$QEp.Wld < 0.001, 
                           paste("< 0.001"),
                           paste("= ", formatC(ma10$QEp.Wld, digits=2, format="f")))),
           "; ", "I^2", " = ", (formatC(ma10$I2, digits=1, format="f")), "%)")),
  c( "Frequency (n/N)",fma10$rate, paste(sum(ma10$xi), " / ",sum(ma10$ni))),
  c( "Prevalence (95% CI)", fma10$prop, 
     paste(formatC(expma10$pred, format='f', digits =2), 
           " (",formatC(expma10$ci.lb, format='f', digits=2),
           "-", formatC(expma10$ci.ub, format='f', digits=2), ")")))

rrsma10 <- structure(list(
  mean = c(NA,  fma10$est, expma10$pred, NA),
  lower = c(NA,  fma10$lwr.ci, expma10$ci.lb, NA),
  upper = c(NA,  fma10$upr.ci, expma10$ci.ub, NA)),
  .Names = c("mean", "lower", "upper"),
  row.names = c(NA, -9L),
  class = "data.frame")

tfma10 <- rbind(tfma10, NA)


trellis.device(device="windows", height = 25, width = 40, color=TRUE)
forestplot(tfma10,
           graph.pos = 3,
           zero = NA,
           rrsma10,
           new_page = TRUE,
           colgap = unit(5, "mm"),
           hrzl_lines = list("2" = gpar (lwd=1, columns=c(1:4), col="black") 
           ),
           lineheight=unit(0.6,'cm'),
           line.margin = 2,
           is.summary = c(T, rep(F, 6), T, F),
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



#####Meta analysis of estimate of effects univariate and multivariate together#####


#Plot ma28 - severe in diabetes

prema28 <- escalc(measure="RR",ai=severe1, ci=severe2, n1i=n1, n2i=n2,
                  subset=(final_exposure=="diabetes" & group=="general" 
                          & !is.na(severe1)), data=univ)



prema28 <- summary(prema28)

prema28$rr <- paste(formatC((exp(prema28$yi)), format='f', digits=2),
                    " ","(", formatC((exp(prema28$ci.lb)), format='f', digits=2), "-",
                    formatC((exp(prema28$ci.ub)), format='f', digits=2),")")



ma28 <- rma(measure="RR", yi,vi, data=prema28, method="REML")
expma28 <- predict(ma28, transf = transf.exp.int)

weights(ma28)
boxsize <- (0.025*(weights(ma28)))

tfma28 <- cbind( 
  c( "Author", NA, prema28$author, 
     paste("Overall relative risk for", ma28$k.eff, "studies","\n", 
           "(Tau^2 = ", (formatC(ma28$tau2, digits=2, format="f")), ", df = ", (ma28$k - ma28$p),
           ", p ", (ifelse(ma28$QEp < 0.001, 
                           paste("< 0.001"),
                           paste("= ", formatC(ma28$QEp, digits=3, format="f")))),
           "; ", "I^2", "= ", (formatC(ma28$I2, digits=1, format="f")), "%)")),
  c( "With Diabetes", "(n/N)", paste(prema28$severe1,"/",prema28$n1), 
     paste(sum(prema28$severe1, na.rm=TRUE),"/", sum(prema28$n1, na.rm=TRUE))),
  
  c( "Without Diabetes", "(n/N)", paste(prema28$severe2,"/",prema28$n2), 
     paste(sum(prema28$severe2, na.rm=TRUE),"/",sum(prema28$n2, na.rm=TRUE))),
  
  c( "Relative Risk (95% CI)", NA,  prema28$rr, 
     paste(formatC(expma28$pred, format='f', digits =2), 
           " (",formatC(expma28$ci.lb, format='f', digits=2),
           "-", formatC(expma28$ci.ub, format='f', digits=2), ")")))

tfma28 <- as_tibble(tfma28)

tfma28 <- add_row(tfma28, .after = 8)
tfma28 <- add_row(tfma28, .after = 10)

rrsma28 <- structure(list(
  mean = c(NA, NA,  exp(prema28$yi), NA, expma28$pred, NA),
  lower = c(NA, NA,  exp(prema28$ci.lb), NA,  expma28$ci.lb, NA),
  upper = c(NA, NA, exp(prema28$ci.ub), NA, expma28$ci.ub, NA)),
  .Names = c("mean", "lower", "upper"),
  row.names = c(NA, -11L),
  class = "data.frame")



trellis.device(device="windows", height = 25, width = 40, color=TRUE)
forestplot(tfma28,
           graph.pos = 4,
           zero=1,
           rrsma28,
           new_page = TRUE,
           colgap = unit(5, "mm"),
           hrzl_lines = list("3" = gpar (lwd=1, columns=c(1:5), col="black") 
           ),
           lineheight=unit(0.6,'cm'),
           line.margin = 2,
           boxsize = c(NA, NA, boxsize, NA, 0.8, NA),
           is.summary = c(T, T, rep(F, 7), T, F),
           align = c("l","c", "c"),
           ci.vertices = TRUE,
           txt_gp = fpTxtGp(label =gpar (cex=0.8), 
                            ticks = gpar(cex = 0.8, fontface="bold"),
                            summary = gpar(cex = 0.8),
                            xlab = gpar(cex=0.8)),
           xticks = c(0.25, 0.5, 1, 2, 4, 8),
           xlog=TRUE,
           clip = c(0.5, 4),
           lwd.xaxis = 1,
           lwd.ci = 2.2,
           graphwidth = unit(7,"cm"),
           col=fpColors(box="black",line="grey", 
                        axes="grey20", summary="black", zero="black"))
par(ask=F)


#Plot ma20 - ICU admission in diabetes


prema29 <- escalc(measure="RR",ai=icu1, ci=icu2, n1i=n1, n2i=n2,
                  subset=(final_exposure=="diabetes" & group=="general" & 
                            !is.na(icu1)), data=univ)
prema29 <- summary(prema29)
prema29$rr <- paste(formatC((exp(prema29$yi)), format='f', digits=2),
                    " ","(", formatC((exp(prema29$ci.lb)), format='f', digits=2), "-",
                    formatC((exp(prema29$ci.ub)), format='f', digits=2),")")

ma29 <- rma(measure="RR", yi,vi, data=prema29, method="REML")
expma29 <- predict(ma29, transf = transf.exp.int)

weights(ma29)
boxsize <- (0.01666667*(weights(ma29)))


tfma29 <- cbind( 
  c( "Author", NA, prema29$author, 
     paste("Overall relative risk for", ma29$k.eff, "studies","\n", 
           "(Tau^2 = ", (formatC(ma29$tau2, digits=2, format="f")), 
           ", df = ", (ma29$k - ma29$p),
           ", p ", (ifelse(ma29$QEp < 0.001, 
                           paste("< 0.001"),
                           paste("= ", formatC(ma29$QEp, digits=3, format="f")))),
           "; ", "I^2", "= ", (formatC(ma29$I2, digits=1, format="f")), "%)")),
  c( "With Diabetes", "(n/N)", paste(prema29$icu1,"/",prema29$n1), 
     paste(sum(prema29$icu1, na.rm=TRUE),"/", sum(prema29$n1, na.rm=TRUE))),
  
  c( "Without Diabetes", "(n/N)", paste(prema29$icu2,"/",prema29$n2), 
     paste(sum(prema29$icu2, na.rm=TRUE),"/",sum(prema29$n2, na.rm=TRUE))),
  
  c( "Relative Risk (95% CI)", NA,  prema29$rr, 
     paste(formatC(expma29$pred, format='f', digits =2), 
           " (",formatC(expma29$ci.lb, format='f', digits=2),
           "-", formatC(expma29$ci.ub, format='f', digits=2), ")")))

tfma29 <- as_tibble(tfma29)

tfma29 <- add_row(tfma29, .after = 5)
tfma29 <- add_row(tfma29, .after = 7)

rrsma29 <- structure(list(
  mean = c(NA, NA,  exp(prema29$yi), NA, expma29$pred, NA),
  lower = c(NA, NA,  exp(prema29$ci.lb), NA,  expma29$ci.lb, NA),
  upper = c(NA, NA, exp(prema29$ci.ub), NA, expma29$ci.ub, NA)),
  .Names = c("mean", "lower", "upper"),
  row.names = c(NA, -8L),
  class = "data.frame")



trellis.device(device="windows", height = 25, width = 40, color=TRUE)
forestplot(tfma29,
           graph.pos = 4,
           zero=1,
           rrsma29,
           new_page = TRUE,
           colgap = unit(5, "mm"),
           hrzl_lines = list("3" = gpar (lwd=1, columns=c(1:5), col="black") 
           ),
           lineheight=unit(0.6,'cm'),
           line.margin = 2,
           boxsize = c(NA, NA, boxsize, NA, 1, NA),
           is.summary = c(T, T, rep(F, 4), T, F),
           align = c("l","c", "c"),
           ci.vertices = TRUE,
           txt_gp = fpTxtGp(label =gpar (cex=0.8), 
                            ticks = gpar(cex = 0.8, fontface="bold"),
                            summary = gpar(cex = 0.8),
                            xlab = gpar(cex=0.8)),
           xticks = c(0.25, 0.5, 1, 2, 4, 8),
           xlog=TRUE,
           clip = c(0.5, 4),
           lwd.xaxis = 1,
           lwd.ci = 2.2,
           graphwidth = unit(7,"cm"),
           col=fpColors(box="black",line="grey", 
                        axes="grey20", summary="black", zero="black"))
par(ask=F)


###Plot ma30 - risk of death in diabetes###


prema30 <- escalc(measure="RR",ai=death1, ci=death2, n1i=n1, n2i=n2,
                  subset=(final_exposure=="diabetes" & 
                            !is.na(death1)), data=univ)
prema30 <- summary(prema30)
prema30$rr <- paste(formatC((exp(prema30$yi)), format='f', digits=2),
                    " ","(", formatC((exp(prema30$ci.lb)), format='f', digits=2), "-",
                    formatC((exp(prema30$ci.ub)), format='f', digits=2),")")

ma30 <- rma(measure="RR", yi,vi, data=prema30, method="REML")
expma30 <- predict(ma30, transf = transf.exp.int)

weights(ma30)
boxsize <- (0.01666667*(weights(ma30)))



tfma30 <- cbind( 
  c( "Author", NA, prema30$author, 
     paste("Overall relative risk for", ma30$k.eff, "studies","\n", 
           "(Tau^2 = ", (formatC(ma30$tau2, digits=2, format="f")), 
           ", df = ", (ma30$k - ma30$p),
           ", p ", (ifelse(ma30$QEp < 0.001, 
                           paste("< 0.001"),
                           paste("= ", formatC(ma30$QEp, digits=3, format="f")))),
           "; ", "I^2", "= ", (formatC(ma30$I2, digits=1, format="f")), "%)")),
  c( "With Diabetes", "(n/N)", paste(prema30$death1,"/",prema30$n1), 
     paste(sum(prema30$death1, na.rm=TRUE),"/", sum(prema30$n1, na.rm=TRUE))),
  
  c( "Without Diabetes", "(n/N)", paste(prema30$death2,"/",prema30$n2), 
     paste(sum(prema30$death2, na.rm=TRUE),"/",sum(prema30$n2, na.rm=TRUE))),
  
  c( "Relative Risk (95% CI)", NA,  prema30$rr, 
     paste(formatC(expma30$pred, format='f', digits =2), 
           " (",formatC(expma30$ci.lb, format='f', digits=2),
           "-", formatC(expma30$ci.ub, format='f', digits=2), ")")))

tfma30 <- as_tibble(tfma30)

tfma30 <- add_row(tfma30, .after = 6)
tfma30 <- add_row(tfma30, .after = 8)


rrsma30 <- structure(list(
  mean = c(NA, NA,  exp(prema30$yi), NA, expma30$pred, NA),
  lower = c(NA, NA,  exp(prema30$ci.lb), NA,  expma30$ci.lb, NA),
  upper = c(NA, NA, exp(prema30$ci.ub), NA, expma30$ci.ub, NA)),
  .Names = c("mean", "lower", "upper"),
  row.names = c(NA, -9L),
  class = "data.frame")

trellis.device(device="windows", height = 25, width = 40, color=TRUE)
forestplot(tfma30,
           graph.pos = 4,
           zero=1,
           rrsma30,
           new_page = TRUE,
           colgap = unit(5, "mm"),
           hrzl_lines = list("3" = gpar (lwd=1, columns=c(1:5), col="black") 
           ),
           lineheight=unit(0.6,'cm'),
           line.margin = 2,
           boxsize = c(NA, NA, boxsize, NA, 1, NA),
           is.summary = c(T, T, rep(F, 5), T, F),
           align = c("l","c", "c"),
           ci.vertices = TRUE,
           txt_gp = fpTxtGp(label =gpar (cex=0.8), 
                            ticks = gpar(cex = 0.8, fontface="bold"),
                            summary = gpar(cex = 0.8),
                            xlab = gpar(cex=0.8)),
           xticks = c(0.25, 0.5, 1, 2, 4, 8),
           xlog=TRUE,
           clip = c(0.5, 4),
           lwd.xaxis = 1,
           lwd.ci = 2.2,
           graphwidth = unit(7,"cm"),
           col=fpColors(box="black",line="grey", 
                        axes="grey20", summary="black", zero="black"))
par(ask=F)


##Plot ma31 - risk of severe COVID-19 in hypertension##


prema31 <- escalc(measure="RR",ai=severe1, ci=severe2, n1i=n1, n2i=n2,
                  subset=(final_exposure=="hypertension" & group=="general" &
                            !is.na(severe1)), data=univ)
prema31 <- summary(prema31)
prema31$rr <- paste(formatC((exp(prema31$yi)), format='f', digits=2),
                    " ","(", formatC((exp(prema31$ci.lb)), format='f', digits=2), "-",
                    formatC((exp(prema31$ci.ub)), format='f', digits=2),")")

ma31 <- rma(measure="RR", yi,vi, data=prema31, method="REML")
expma31 <- predict(ma31, transf = transf.exp.int)

weights(ma31)
boxsize <- (0.025*(weights(ma31)))



tfma31 <- cbind( 
  c( "Author", NA, prema31$author, 
     paste("Overall relative risk for", ma31$k.eff, "studies","\n", 
           "(Tau^2 = ", (formatC(ma31$tau2, digits=2, format="f")), 
           ", df = ", (ma31$k - ma31$p),
           ", p ", (ifelse(ma31$QEp < 0.001, 
                           paste("< 0.001"),
                           paste("= ", formatC(ma31$QEp, digits=3, format="f")))),
           "; ", "I^2", "= ", (formatC(ma31$I2, digits=1, format="f")), "%)")),
  c( "With Diabetes", "(n/N)", paste(prema31$severe1,"/",prema31$n1), 
     paste(sum(prema31$severe1, na.rm=TRUE),"/", sum(prema31$n1, na.rm=TRUE))),
  
  c( "Without Diabetes", "(n/N)", paste(prema31$severe2,"/",prema31$n2), 
     paste(sum(prema31$severe2, na.rm=TRUE),"/",sum(prema31$n2, na.rm=TRUE))),
  
  c( "Relative Risk (95% CI)", NA,  prema31$rr, 
     paste(formatC(expma31$pred, format='f', digits =2), 
           " (",formatC(expma31$ci.lb, format='f', digits=2),
           "-", formatC(expma31$ci.ub, format='f', digits=2), ")")))

tfma31 <- as_tibble(tfma31)

tfma31 <- add_row(tfma31, .after = 10)
tfma31 <- add_row(tfma31, .after = 12)


rrsma31 <- structure(list(
  mean = c(NA, NA,  exp(prema31$yi), NA, expma31$pred, NA),
  lower = c(NA, NA,  exp(prema31$ci.lb), NA,  expma31$ci.lb, NA),
  upper = c(NA, NA, exp(prema31$ci.ub), NA, expma31$ci.ub, NA)),
  .Names = c("mean", "lower", "upper"),
  row.names = c(NA, -13L),
  class = "data.frame")

trellis.device(device="windows", height = 25, width = 40, color=TRUE)
forestplot(tfma31,
           graph.pos = 4,
           zero=1,
           rrsma31,
           new_page = TRUE,
           colgap = unit(5, "mm"),
           hrzl_lines = list("3" = gpar (lwd=1, columns=c(1:5), col="black") 
           ),
           lineheight=unit(0.6,'cm'),
           line.margin = 2,
           boxsize = c(NA, NA, boxsize, NA, 1, NA),
           is.summary = c(T, T, rep(F, 9), T, F),
           align = c("l","c", "c"),
           ci.vertices = TRUE,
           txt_gp = fpTxtGp(label =gpar (cex=0.8), 
                            ticks = gpar(cex = 0.8, fontface="bold"),
                            summary = gpar(cex = 0.8),
                            xlab = gpar(cex=0.8)),
           xticks = c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16),
           xlog=TRUE,
           clip = c(0.5, 4),
           lwd.xaxis = 1,
           lwd.ci = 2.2,
           graphwidth = unit(7,"cm"),
           col=fpColors(box="black",line="grey", 
                        axes="grey20", summary="black", zero="black"))
par(ask=F)



##Plot for ma32 - risk of ICU admission in hypertension



prema32 <- escalc(measure="RR",ai=icu1, ci=icu2, n1i=n1, n2i=n2,
                  subset=(final_exposure=="hypertension" & group=="general" &
                            !is.na(icu1)), data=univ)
prema32 <- summary(prema32)
prema32$rr <- paste(formatC((exp(prema32$yi)), format='f', digits=2),
                    " ","(", formatC((exp(prema32$ci.lb)), format='f', digits=2), "-",
                    formatC((exp(prema32$ci.ub)), format='f', digits=2),")")

ma32 <- rma(measure="RR", yi,vi, data=prema32, method="REML")
expma32 <- predict(ma32, transf = transf.exp.int)

weights(ma32)
boxsize <- (0.01666667*(weights(ma32)))


tfma32 <- cbind( 
  c( "Author", NA, prema32$author, 
     paste("Overall relative risk for", ma32$k.eff, "studies","\n", 
           "(Tau^2 = ", (formatC(ma32$tau2, digits=2, format="f")), 
           ", df = ", (ma32$k - ma32$p),
           ", p ", (ifelse(ma32$QEp < 0.001, 
                           paste("< 0.001"),
                           paste("= ", formatC(ma32$QEp, digits=3, format="f")))),
           "; ", "I^2", "= ", (formatC(ma32$I2, digits=1, format="f")), "%)")),
  c( "With Diabetes", "(n/N)", paste(prema32$icu1,"/",prema32$n1), 
     paste(sum(prema32$icu1, na.rm=TRUE),"/", sum(prema32$n1, na.rm=TRUE))),
  
  c( "Without Diabetes", "(n/N)", paste(prema32$icu2,"/",prema32$n2), 
     paste(sum(prema32$icu2, na.rm=TRUE),"/",sum(prema32$n2, na.rm=TRUE))),
  
  c( "Relative Risk (95% CI)", NA,  prema32$rr, 
     paste(formatC(expma32$pred, format='f', digits =2), 
           " (",formatC(expma32$ci.lb, format='f', digits=2),
           "-", formatC(expma32$ci.ub, format='f', digits=2), ")")))

tfma32 <- as_tibble(tfma32)

tfma32 <- add_row(tfma32, .after = 5)
tfma32 <- add_row(tfma32, .after = 7)


rrsma32 <- structure(list(
  mean = c(NA, NA,  exp(prema32$yi), NA, expma32$pred, NA),
  lower = c(NA, NA,  exp(prema32$ci.lb), NA,  expma32$ci.lb, NA),
  upper = c(NA, NA, exp(prema32$ci.ub), NA, expma32$ci.ub, NA)),
  .Names = c("mean", "lower", "upper"),
  row.names = c(NA, -8L),
  class = "data.frame")

trellis.device(device="windows", height = 25, width = 40, color=TRUE)
forestplot(tfma32,
           graph.pos = 4,
           zero=1,
           rrsma32,
           new_page = TRUE,
           colgap = unit(5, "mm"),
           hrzl_lines = list("3" = gpar (lwd=1, columns=c(1:5), col="black") 
           ),
           lineheight=unit(0.6,'cm'),
           line.margin = 2,
           boxsize = c(NA, NA, boxsize, NA, 1, NA),
           is.summary = c(T, T, rep(F, 4), T, F),
           align = c("l","c", "c"),
           ci.vertices = TRUE,
           txt_gp = fpTxtGp(label =gpar (cex=0.8), 
                            ticks = gpar(cex = 0.8, fontface="bold"),
                            summary = gpar(cex = 0.8),
                            xlab = gpar(cex=0.8)),
           xticks = c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16),
           xlog=TRUE,
           clip = c(0.5, 4),
           lwd.xaxis = 1,
           lwd.ci = 2.2,
           graphwidth = unit(7,"cm"),
           col=fpColors(box="black",line="grey", 
                        axes="grey20", summary="black", zero="black"))
par(ask=F)



#Plot for ma 33 - risk of death in people with hypertension


prema33 <- escalc(measure="RR",ai=death1, ci=death2, n1i=n1, n2i=n2,
                  subset=(final_exposure=="hypertension" &
                            !is.na(death1)), data=univ)
prema33 <- summary(prema33)
prema33$rr <- paste(formatC((exp(prema33$yi)), format='f', digits=2),
                    " ","(", formatC((exp(prema33$ci.lb)), format='f', digits=2), "-",
                    formatC((exp(prema33$ci.ub)), format='f', digits=2),")")

ma33 <- rma(measure="RR", yi,vi, data=prema33, method="REML")
expma33 <- predict(ma33, transf = transf.exp.int)

weights(ma33)
boxsize <- (0.025*(weights(ma33)))




tfma33 <- cbind( 
  c( "Author", NA, prema33$author, 
     paste("Overall relative risk for", ma33$k.eff, "studies","\n", 
           "(Tau^2 = ", (formatC(ma33$tau2, digits=2, format="f")), 
           ", df = ", (ma33$k - ma33$p),
           ", p ", (ifelse(ma33$QEp < 0.001, 
                           paste("< 0.001"),
                           paste("= ", formatC(ma33$QEp, digits=3, format="f")))),
           "; ", "I^2", "= ", (formatC(ma33$I2, digits=1, format="f")), "%)")),
  c( "With Diabetes", "(n/N)", paste(prema33$death1,"/",prema33$n1), 
     paste(sum(prema33$death1, na.rm=TRUE),"/", sum(prema33$n1, na.rm=TRUE))),
  
  c( "Without Diabetes", "(n/N)", paste(prema33$death2,"/",prema33$n2), 
     paste(sum(prema33$death2, na.rm=TRUE),"/",sum(prema33$n2, na.rm=TRUE))),
  
  c( "Relative Risk (95% CI)", NA,  prema33$rr, 
     paste(formatC(expma33$pred, format='f', digits =2), 
           " (",formatC(expma33$ci.lb, format='f', digits=2),
           "-", formatC(expma33$ci.ub, format='f', digits=2), ")")))

tfma33 <- as_tibble(tfma33)

tfma33 <- add_row(tfma33, .after = 10)
tfma33 <- add_row(tfma33, .after = 12)


rrsma33 <- structure(list(
  mean = c(NA, NA,  exp(prema33$yi), NA, expma33$pred, NA),
  lower = c(NA, NA,  exp(prema33$ci.lb), NA,  expma33$ci.lb, NA),
  upper = c(NA, NA, exp(prema33$ci.ub), NA, expma33$ci.ub, NA)),
  .Names = c("mean", "lower", "upper"),
  row.names = c(NA, -13L),
  class = "data.frame")

trellis.device(device="windows", height = 25, width = 40, color=TRUE)
forestplot(tfma33,
           graph.pos = 4,
           zero=1,
           rrsma33,
           new_page = TRUE,
           colgap = unit(5, "mm"),
           hrzl_lines = list("3" = gpar (lwd=1, columns=c(1:5), col="black") 
           ),
           lineheight=unit(0.6,'cm'),
           line.margin = 2,
           boxsize = c(NA, NA, boxsize, NA, 1, NA),
           is.summary = c(T, T, rep(F, 9), T, F),
           align = c("l","c", "c"),
           ci.vertices = TRUE,
           txt_gp = fpTxtGp(label =gpar (cex=0.8), 
                            ticks = gpar(cex = 0.8, fontface="bold"),
                            summary = gpar(cex = 0.8),
                            xlab = gpar(cex=0.8)),
           xticks = c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16),
           xlog=TRUE,
           clip = c(0.5, 4),
           lwd.xaxis = 1,
           lwd.ci = 2.2,
           graphwidth = unit(7,"cm"),
           col=fpColors(box="black",line="grey", 
                        axes="grey20", summary="black", zero="black"))
par(ask=F)




#Plot for ma34 - risk of severe covid 19 in people with DM and HT
ma34 <- rma(measure="RR", ai=severe1, ci=severe2, n1i=n1, n2i=n2,
            subset=(final_exposure=="diabetes_hypertension" & group=="general" & 
                      !is.na(severe1)), data=univ, method="REML")
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




