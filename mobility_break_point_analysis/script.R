###########################################################################################################
############################################################################################################
dir.create("plots")
dir.create("output")
#Code fits models to the data assuming log-normal errors for 0,1,2,3,4,5,6 breakpoints
#For each model
#   -generates all permutations of breakpoints
#   -fits base model for each possible permutation returning -2*loglikelihood under 'AICc' column
#   -From this finds the maximum likelihood model
#   -Can then use relative likelihood to find the univaritae likelihhod profile 95% CI for each breakpoint
#
#Code returns best fit model for each number of breakpoint as lognlm in rds format
#Code returns list of permutations and likelihoods from which 95% CI can be easily calculated
#Code return AICc values calculated near end and saved as csv file
#Code also plots the best fit for the 5 breakpoint model that is the best fit for the date range selected currently
#(plot code is callibrated for this model fit right now)

#Import data and format 
fb <- import_data()
#get consistent date range for all data 
# all_data <- filter_by_date(all_data)
fb_national <- aggregate(fb$n_trips, 
                         by = list("date" = fb$date), FUN = sum)
fb_national$location <- rep("UK", nrow(fb_national))
fb_national$country <- rep("UK", nrow(fb_national))
fb_national <- get_percent_change(fb_national, "location", 
                                  "x", "country")

#Set minimum number of data points in a segment 
min_section <- 3

#Set minimum date for when wanting to cut start data
min_date_to_use <- as.Date("2000-01-01")
max_date_to_use <- as.Date("2020-05-23")

#Remove bank holidays
bank_holidays <- as.Date(c("2020-04-10", "2020-04-13", "2020-05-08",
                           "2020-05-25"))
#Reformat the data
fb_national$day <- weekdays(as.Date(fb_national$date))
fb_national$weekdays <- rep(1, nrow(fb_national))
fb_national[fb_national$day %in% 
              c("Sunday"),]$weekdays <- 0

fb_national[fb_national$day %in% 
              c("Saturday"),]$weekdays <- 2

fb_national[fb_national$date %in% 
              bank_holidays,]$weekdays <- 3

#Apply weighting for fittings (inverse-variance weighting)
fb_national<-fb_national[fb_national$date<max_date_to_use,]
fb_national$weights <- fb_national$n_trips
fb_national$weights <- (1/fb_national$weights)/mean(1/fb_national$weights)


#Changing name to something shorter to type
mob_df <- fb_national

#Column for splitting model into different sections
mob_df$cat <- 0

#Create a date numeric for the fitting
mob_df$date_numeric <- as.numeric(mob_df$date)
min_date_numeric <- min(mob_df$date_numeric)
mob_df$date_numeric <- mob_df$date_numeric + 1- min_date_numeric

#Convert to factors
mob_df$weekdays <- factor(mob_df$weekdays)

#Remove unwanted data 

mob_df<-mob_df[mob_df$date>min_date_to_use,]
mob_df<-mob_df[mob_df$date<max_date_to_use,]
mob_df$percent_diff<-mob_df$percent_diff+100
mob_df2 <- mob_df
mob_df<-mob_df[mob_df$day %in% c("Saturday", "Sunday")==FALSE,]
mob_df<-mob_df[mob_df$date %in% bank_holidays==FALSE,]
#Create vector of indices to test breaks 
max_date <- max(mob_df$date_numeric)
indices_to_break<-mob_df$date_numeric[min_section:(length(mob_df$date_numeric)-min_section)]
point_number<-seq(length(indices_to_break))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Fit linear model for 0 breaks
lm0 <- lognlm(percent_diff~date_numeric,
           data = mob_df,
           weights = weights)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Fit linear model for 1 breaks
co1 <- expand.grid(indices_to_break)
co1$AICc <- -99
for (i in seq_len(nrow(co1))){
  mob_df$cat <- 0 
  date_break <- co1$Var1[i]
  mob_df[mob_df$date_numeric<=date_break,]$cat <- 1
  
  mob_df$cat <- factor(mob_df$cat)
  lm1 <- lognlm(percent_diff~date_numeric*cat,
             data = mob_df,
             weights = weights)
  co1$AICc[i] <- -2*lm1$loglik
  
}
AICc1 <- min(co1$AICc)
mob_df$cat <- 0 
date_break <- co1[co1$AICc==min(co1$AICc),]$Var1
mob_df[mob_df$date_numeric<=date_break,]$cat <- 1
mob_df$cat <- factor(mob_df$cat)
lm1 <- lognlm(percent_diff~date_numeric*cat,
           data = mob_df,
           weights = weights)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Fit linear model for 2 breaks 
co2 <- expand.grid(indices_to_break, indices_to_break)
point_grid<- expand.grid(point_number, point_number)

co2$index<-seq(nrow(co2))
point_grid$index<-seq(nrow(point_grid))

point_grid <- subset(point_grid, (Var2>=Var1+min_section))
co2<-co2[co2$index %in% point_grid$index,]

co2$AICc <- -99
for (i in seq_len(nrow(co2))){
  mob_df$cat <- 0 
  date_break1 <- co2$Var1[i]
  date_break2 <- co2$Var2[i]
  mob_df[mob_df$date_numeric<=date_break2,]$cat <- 1
  mob_df[mob_df$date_numeric<=date_break1,]$cat <- 2
  mob_df$cat <- factor(mob_df$cat)
  lm2 <- lognlm(percent_diff~date_numeric*cat,
             data = mob_df,
             weights = weights)
  co2$AICc[i] <- -2*lm2$loglik
  
}
AICc2 <- min(co2$AICc)
mob_df$cat <- 0 
date_break1 <- co2[co2$AICc==min(co2$AICc),]$Var1
date_break2 <- co2[co2$AICc==min(co2$AICc),]$Var2
mob_df[mob_df$date_numeric<=date_break2,]$cat <- 1
mob_df[mob_df$date_numeric<=date_break1,]$cat <- 2
mob_df$cat <- factor(mob_df$cat)
lm2 <- lognlm(percent_diff~date_numeric*cat,
           data = mob_df,
           weights = weights)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Fit linear model for 3 breaks 
co3 <- expand.grid(indices_to_break, indices_to_break, indices_to_break)
point_grid<- expand.grid(point_number, point_number, point_number)

co3$index<-seq(nrow(co3))
point_grid$index<-seq(nrow(point_grid))

point_grid <- subset(point_grid, (Var2>=Var1+min_section & Var3>=Var2+min_section))
co3<-co3[co3$index %in% point_grid$index,]

co3$AICc <- -99

for (i in seq_len(nrow(co3))){
  mob_df$cat <- 0 
  date_break1 <- co3$Var1[i]
  date_break2 <- co3$Var2[i]
  date_break3 <- co3$Var3[i]
  mob_df[mob_df$date_numeric<=date_break3,]$cat <- 1
  mob_df[mob_df$date_numeric<=date_break2,]$cat <- 2
  mob_df[mob_df$date_numeric<=date_break1,]$cat <- 3
  mob_df$cat <- factor(mob_df$cat)
  lm3 <- lognlm(percent_diff~date_numeric*cat,
             data = mob_df,
             weights = weights)
  co3$AICc[i] <- -2*lm3$loglik
  
}

AICc3 <- min(co3$AICc)
mob_df$cat <- 0 
date_break1 <- co3[co3$AICc==min(co3$AICc),]$Var1
date_break2 <- co3[co3$AICc==min(co3$AICc),]$Var2
date_break3 <- co3[co3$AICc==min(co3$AICc),]$Var3
mob_df[mob_df$date_numeric<=date_break3,]$cat <- 1
mob_df[mob_df$date_numeric<=date_break2,]$cat <- 2
mob_df[mob_df$date_numeric<=date_break1,]$cat <- 3
mob_df$cat <- factor(mob_df$cat)
lm3 <- lognlm(percent_diff~date_numeric*cat,
           data = mob_df,
           weights = weights)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Fit linear model for 4 breaks 
co4 <- expand.grid(indices_to_break,indices_to_break, indices_to_break, indices_to_break)
point_grid<- expand.grid(point_number,point_number, point_number, point_number)

co4$index<-seq(nrow(co4))
point_grid$index<-seq(nrow(point_grid))

point_grid <- subset(point_grid, (Var2>=Var1+min_section & Var3>=Var2+min_section & Var4>=Var3+min_section))
co4<-co4[co4$index %in% point_grid$index,]
co4$AICc <- -99
for (i in seq_len(nrow(co4))){
  mob_df$cat <- 0 
  date_break1 <- co4$Var1[i]
  date_break2 <- co4$Var2[i]
  date_break3 <- co4$Var3[i]
  date_break4 <- co4$Var4[i]
  mob_df[mob_df$date_numeric<=date_break4,]$cat <- 1
  mob_df[mob_df$date_numeric<=date_break3,]$cat <- 2
  mob_df[mob_df$date_numeric<=date_break2,]$cat <- 3
  mob_df[mob_df$date_numeric<=date_break1,]$cat <- 4
  mob_df$cat <- factor(mob_df$cat)
  lm4 <- lognlm(percent_diff~date_numeric*cat,
             data = mob_df,
             weights = weights)
  co4$AICc[i] <- -2*lm4$loglik
  
}

AICc4 <- min(co4$AIC)
mob_df$cat <- 0 
date_break1 <- co4[co4$AICc==min(co4$AICc),]$Var1
date_break2 <- co4[co4$AICc==min(co4$AICc),]$Var2
date_break3 <- co4[co4$AICc==min(co4$AICc),]$Var3
date_break4 <- co4[co4$AICc==min(co4$AICc),]$Var4
mob_df[mob_df$date_numeric<=date_break4,]$cat <- 1
mob_df[mob_df$date_numeric<=date_break3,]$cat <- 2
mob_df[mob_df$date_numeric<=date_break2,]$cat <- 3
mob_df[mob_df$date_numeric<=date_break1,]$cat <- 4
mob_df$cat <- factor(mob_df$cat)
lm4 <- lognlm(percent_diff~date_numeric*cat,
           data = mob_df,
           weights = weights)

pdf("plots/BreakPoint4.pdf", height = 8.5, width = 12)
g<-graph_function_4()
grid.draw(g)
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Fit linear model for 5 breaks 
co5 <- expand.grid(indices_to_break,indices_to_break,indices_to_break, indices_to_break, indices_to_break)
point_grid<- expand.grid(point_number,point_number,point_number, point_number, point_number)

co5$index<-seq(nrow(co5))
point_grid$index<-seq(nrow(point_grid))

point_grid <- subset(point_grid, (Var2>=Var1+min_section & Var3>=Var2+min_section & Var4>=Var3+min_section & Var5>=Var4+min_section))
co5<-co5[co5$index %in% point_grid$index,]

co5$AICc <- -99

for (i in seq_len(nrow(co5))){
  mob_df$cat <- 0 
  date_break1 <- co5$Var1[i]
  date_break2 <- co5$Var2[i]
  date_break3 <- co5$Var3[i]
  date_break4 <- co5$Var4[i]
  date_break5 <- co5$Var5[i]
  mob_df[mob_df$date_numeric<=date_break5,]$cat <- 1
  mob_df[mob_df$date_numeric<=date_break4,]$cat <- 2
  mob_df[mob_df$date_numeric<=date_break3,]$cat <- 3
  mob_df[mob_df$date_numeric<=date_break2,]$cat <- 4
  mob_df[mob_df$date_numeric<=date_break1,]$cat <- 5
  mob_df$cat <- factor(mob_df$cat)
  mob_df
  lm5 <- lognlm(percent_diff~date_numeric*cat,
             data = mob_df,
             weights = weights)
  co5$AICc[i] <- -2*lm5$loglik
  
}



AICc5 <- min(co5$AICc)
mob_df$cat <- 0 
date_break1 <- co5[co5$AICc==min(co5$AICc),]$Var1
date_break2 <- co5[co5$AICc==min(co5$AICc),]$Var2
date_break3 <- co5[co5$AICc==min(co5$AICc),]$Var3
date_break4 <- co5[co5$AICc==min(co5$AICc),]$Var4
date_break5 <- co5[co5$AICc==min(co5$AICc),]$Var5


mob_df[mob_df$date_numeric<=date_break5,]$cat <- 1
mob_df[mob_df$date_numeric<=date_break4,]$cat <- 2
mob_df[mob_df$date_numeric<=date_break3,]$cat <- 3
mob_df[mob_df$date_numeric<=date_break2,]$cat <- 4
mob_df[mob_df$date_numeric<=date_break1,]$cat <- 5
mob_df$cat <- factor(mob_df$cat)
lm5 <- lognlm(percent_diff~date_numeric*cat,
           data = mob_df,
           weights = weights)

pdf("plots/BreakPoint5.pdf", height = 8.5, width = 12)
g<-graph_function_5()
grid.draw(g)
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Fit linear model for 6 breaks 
co52 <- expand.grid(indices_to_break,indices_to_break,indices_to_break, indices_to_break, indices_to_break)
point_grid<- expand.grid(point_number,point_number,point_number, point_number, point_number)

co52$index<-seq(nrow(co52))
point_grid$index<-seq(nrow(point_grid))

point_grid <- subset(point_grid, (Var2>=Var1+min_section & Var3>=Var2+min_section & Var4>=Var3+min_section & Var5>=Var4+min_section & Var5<35))
co52<-co52[co52$index %in% point_grid$index,]

listnum<- point_number[point_number>15]
listnum2<-indices_to_break[16:length(indices_to_break)]

point_grid3 <- expand.grid(listnum)
point_grid4 <- expand.grid(listnum2)

colnames(point_grid3) <- c("Var6")
colnames(point_grid4) <- c("Var6")

min_section
point_grid2 <- merge(point_grid, point_grid3)
co6 <- merge(co52, point_grid4)

co6$index<-seq(nrow(co6))
point_grid2$index<-seq(nrow(point_grid2))

point_grid2 <- subset(point_grid2, (Var6>=Var5+min_section))
co6<-co6[co6$index %in% point_grid2$index,]

co6$AICc <- -99


for (i in seq_len(nrow(co6))){
  mob_df$cat <- 0 
  date_break1 <- co6$Var1[i]
  date_break2 <- co6$Var2[i]
  date_break3 <- co6$Var3[i]
  date_break4 <- co6$Var4[i]
  date_break5 <- co6$Var5[i]
  date_break6 <- co6$Var6[i]
  mob_df[mob_df$date_numeric<=date_break6,]$cat <- 1
  mob_df[mob_df$date_numeric<=date_break5,]$cat <- 2
  mob_df[mob_df$date_numeric<=date_break4,]$cat <- 3
  mob_df[mob_df$date_numeric<=date_break3,]$cat <- 4
  mob_df[mob_df$date_numeric<=date_break2,]$cat <- 5
  mob_df[mob_df$date_numeric<=date_break1,]$cat <- 6
  mob_df$cat <- factor(mob_df$cat)
  mob_df
  lm6 <- lognlm(percent_diff~date_numeric*cat,
             data = mob_df,
             weights = weights)
  co6$AICc[i] <- -2*lm6$loglik
  
}


AICc6 <- min(co6$AICc)
mob_df$cat <- 0 
date_break1 <- co6[co6$AICc==min(co6$AICc),]$Var1
date_break2 <- co6[co6$AICc==min(co6$AICc),]$Var2
date_break3 <- co6[co6$AICc==min(co6$AICc),]$Var3
date_break4 <- co6[co6$AICc==min(co6$AICc),]$Var4
date_break5 <- co6[co6$AICc==min(co6$AICc),]$Var5
date_break6 <- co6[co6$AICc==min(co6$AICc),]$Var6

mob_df[mob_df$date_numeric<=date_break6,]$cat <- 1
mob_df[mob_df$date_numeric<=date_break5,]$cat <- 2
mob_df[mob_df$date_numeric<=date_break4,]$cat <- 3
mob_df[mob_df$date_numeric<=date_break3,]$cat <- 4
mob_df[mob_df$date_numeric<=date_break2,]$cat <- 5
mob_df[mob_df$date_numeric<=date_break1,]$cat <- 6
mob_df$cat <- factor(mob_df$cat)
lm6 <- lognlm(percent_diff~date_numeric*cat,
           data = mob_df,
           weights = weights)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Adjustments to AICc due to not including number of breaks in AICc calcs befoe
n=nrow(mob_df)

k=2
m=0
AICc0 =  2*(k+m) +((2*(k+m)*(k+m)+2*(k+m))/(n-k-1-m))-2*lm0$loglik

k=4
m=1
AICc1 =  2*(k+m) +((2*(k+m)*(k+m)+2*(k+m))/(n-k-1-m))-2*lm1$loglik

k=6
m=2
AICc2 =  2*(k+m) +((2*(k+m)*(k+m)+2*(k+m))/(n-k-1-m))-2*lm2$loglik

k=8
m=3
AICc3 =  2*(k+m) +((2*(k+m)*(k+m)+2*(k+m))/(n-k-1-m))-2*lm3$loglik

k=10
m=4
AICc4 =  2*(k+m) +((2*(k+m)*(k+m)+2*(k+m))/(n-k-1-m))-2*lm4$loglik

k=12
m=5
AICc5 =  2*(k+m) +((2*(k+m)*(k+m)+2*(k+m))/(n-k-1-m))-2*lm5$loglik

k=14
m=6
AICc6 =  2*(k+m) +((2*(k+m)*(k+m)+2*(k+m))/(n-k-1-m))-2*lm6$loglik
#Output AICc of the models as a csv
table_to_output <- data.frame( Number_of_breaks = c(0,1,2,3,4,5,6),
                               AICc = c(AICc0, AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
write.csv(table_to_output,'output/AICcValues.csv')


co1$likelihhod <- (min(co1$AICc)-co1$AICc)/2
co2$likelihhod <- (min(co2$AICc)-co2$AICc)/2
co3$likelihhod <- (min(co3$AICc)-co3$AICc)/2
co4$likelihhod <- (min(co4$AICc)-co4$AICc)/2
co5$likelihhod <- (min(co5$AICc)-co5$AICc)/2
co6$likelihhod <- (min(co6$AICc)-co6$AICc)/2

saveRDS(co1, "output/ModelFit1.rds")
saveRDS(co2, "output/ModelFit2.rds")
saveRDS(co3, "output/ModelFit3.rds")
saveRDS(co4, "output/ModelFit4.rds")
saveRDS(co5, "output/ModelFit5.rds")
saveRDS(co6, "output/ModelFit6.rds")

saveRDS(lm0, "output/Model0.rds")
saveRDS(lm1, "output/Model1.rds")
saveRDS(lm2, "output/Model2.rds")
saveRDS(lm3, "output/Model3.rds")
saveRDS(lm4, "output/Model4.rds")
saveRDS(lm5, "output/Model5.rds")
saveRDS(lm6, "output/Model6.rds")


