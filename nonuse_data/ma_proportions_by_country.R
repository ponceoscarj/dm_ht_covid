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

