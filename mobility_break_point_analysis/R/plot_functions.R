
predictor4 <-function(lm4, grid){
  sum<-summary(lm4)
  for(i in seq_len(nrow(grid))){
    if(grid$cat[i]==0){
      grad = lm4$coefficients["date_numeric"][[1]]
      grad1=0
      inter = lm4$coefficients["(Intercept)"][[1]]
      inter1=0
      Sigma<-vcov(lm4)[1:2,1:2]
      mu=c(inter,grad)
      
    } 
    else if(grid$cat[i]==1){
      grad = lm4$coefficients["date_numeric"][[1]]
      grad1=lm4$coefficients["date_numeric:cat1"][[1]]
      inter = lm4$coefficients["(Intercept)"][[1]]
      inter1 = lm4$coefficients["cat1"][[1]]
      Sigma<-vcov(lm4)[1:7,1:7][-4:-6,-4:-6]
      mu=c(inter,grad, inter1, grad1)
      
    }
    else if(grid$cat[i]==2){
      grad = lm4$coefficients["date_numeric"][[1]]
      grad1=lm4$coefficients["date_numeric:cat2"][[1]]
      inter = lm4$coefficients["(Intercept)"][[1]]
      inter1=lm4$coefficients["cat2"][[1]]
      Sigma<-vcov(lm4)[1:8,1:8][-3,-3][-4:-6,-4:-6]
      mu=c(inter,grad, inter1, grad1)
    }
    else if(grid$cat[i]==3){
      grad = lm4$coefficients["date_numeric"][[1]]
      grad1=lm4$coefficients["date_numeric:cat3"][[1]]
      inter = lm4$coefficients["(Intercept)"][[1]]
      inter1=lm4$coefficients["cat3"][[1]]
      Sigma<-vcov(lm4)[1:9,1:9][-3:-4,-3:-4][-4:-6,-4:-6]
      mu=c(inter,grad, inter1, grad1)
      
    }
    else if(grid$cat[i]==4){
      grad = lm4$coefficients["date_numeric"][[1]]
      grad1=lm4$coefficients["date_numeric:cat4"][[1]]
      inter = lm4$coefficients["(Intercept)"][[1]]
      inter1=lm4$coefficients["cat4"][[1]]
      Sigma<-vcov(lm4)[1:10,1:10][-3:-5,-3:-5][-4:-6,-4:-6]
      mu=c(inter,grad, inter1, grad1)
    }
    grid$y[i] <- (grad+grad1)*grid$date_numeric[i] + inter+inter1
    ci<-predict_CI(mu,Sigma, grid$date_numeric[i])
    grid$se.hi[i]<-ci[[2]]
    grid$se.lo[i]<-ci[[1]]
    
  }
  return(grid)
}



predictor5 <-function(lm5, grid){
  sum<-summary(lm5)
  for(i in seq_len(nrow(grid))){
    if(grid$cat[i]==0){
      grad = lm5$coefficients["date_numeric"][[1]]
      grad1=0
      inter = lm5$coefficients["(Intercept)"][[1]]
      inter1=0
      Sigma<-vcov(lm5)[1:2,1:2]
      mu=c(inter,grad)
      
      
    } 
    else if(grid$cat[i]==1){
      grad = lm5$coefficients["date_numeric"][[1]]
      grad1=lm5$coefficients["date_numeric:cat1"][[1]]
      inter = lm5$coefficients["(Intercept)"][[1]]
      inter1 = lm5$coefficients["cat1"][[1]]
      Sigma<-vcov(lm5)[1:8,1:8][-4:-7,-4:-7]
      mu=c(inter,grad, inter1, grad1)
      
      
    }
    else if(grid$cat[i]==2){
      grad = lm5$coefficients["date_numeric"][[1]]
      grad1=lm5$coefficients["date_numeric:cat2"][[1]]
      inter = lm5$coefficients["(Intercept)"][[1]]
      inter1=lm5$coefficients["cat2"][[1]]
      Sigma<-vcov(lm5)[1:9,1:9][-3,-3][-4:-7,-4:-7]
      mu=c(inter,grad, inter1, grad1)
      
    }
    else if(grid$cat[i]==3){
      grad = lm5$coefficients["date_numeric"][[1]]
      grad1=lm5$coefficients["date_numeric:cat3"][[1]]
      inter = lm5$coefficients["(Intercept)"][[1]]
      inter1=lm5$coefficients["cat3"][[1]]
      Sigma<-vcov(lm5)[1:10,1:10][-3:-4,-3:-4][-4:-7,-4:-7]
      mu=c(inter,grad, inter1, grad1)
      
      
    }
    else if(grid$cat[i]==4){
      grad = lm5$coefficients["date_numeric"][[1]]
      grad1=lm5$coefficients["date_numeric:cat4"][[1]]
      inter = lm5$coefficients["(Intercept)"][[1]]
      inter1=lm5$coefficients["cat4"][[1]]
      Sigma<-vcov(lm5)[1:11,1:11][-3:-5,-3:-5][-4:-7,-4:-7]
      mu=c(inter,grad, inter1, grad1)
      
    }
    else if(grid$cat[i]==5){
      grad = lm5$coefficients["date_numeric"][[1]]
      grad1=lm5$coefficients["date_numeric:cat5"][[1]]
      inter = lm5$coefficients["(Intercept)"][[1]]
      inter1=lm5$coefficients["cat5"][[1]]
      Sigma<-vcov(lm5)[1:12,1:12][-3:-6,-3:-6][-4:-7,-4:-7]
      mu=c(inter,grad, inter1, grad1)
      
    }
    grid$y[i] <- (grad+grad1)*grid$date_numeric[i] + inter+inter1
    ci<-predict_CI(mu,Sigma, grid$date_numeric[i])
    grid$se.hi[i]<-ci[[2]]
    grid$se.lo[i]<-ci[[1]]
    
  }
  return(grid)
}




predict_CI <- function(mu, Sigma, date_numeric){
  list <- c()
  param<-mvrnorm(n = 1000, mu, Sigma, tol = 1e-6, empirical = FALSE)
  
  for (i in seq_len(1000)){
    if(ncol(param)==2){
      gradtemp <- param[i,2]
      intertemp <- param[i,1]
      list <- c(list, gradtemp*date_numeric + intertemp)
      
    }
    else if(ncol(param)==4){
      gradtemp <- param[i,2]+param[i,4]
      intertemp <- param[i,1]+param[i,3]
      list <- c(list, gradtemp*date_numeric + intertemp)
    }
  }
  ci<-quantile(list, probs = c(0.025,0.975))
  return (ci)
}


graph_function_4 <- function(){
  model = lm4
  date_break1 <- co4[co4$AICc==min(co4$AICc),]$Var1
  date_break2 <- co4[co4$AICc==min(co4$AICc),]$Var2
  date_break3 <- co4[co4$AICc==min(co4$AICc),]$Var3
  date_break4 <- co4[co4$AICc==min(co4$AICc),]$Var4
  grid <- expand.grid(date_numeric=seq(min(mob_df$date_numeric), max_date, length.out=500),weekdays=c(0,1), cat=c(0,1,2,3,4))
  grid$weekdays <- factor(grid$weekdays)
  grid$cat <- factor(grid$cat)
  grid <- predictor4(lm4, grid)
  grid<-grid[grid$weekdays==1,]
  
  grid1<-grid[grid$cat==0,]
  grid2<-grid[grid$cat==1,]
  grid3<-grid[grid$cat==2,]
  grid4<-grid[grid$cat==3,]
  grid5<-grid[grid$cat==4,]
  
  
  grid5<-grid5[grid5$date_numeric<=date_break1,]
  grid4<-grid4[grid4$date_numeric>date_break1+3 & grid4$date_numeric<=date_break2,]
  grid3<-grid3[grid3$date_numeric>date_break2+1 & grid3$date_numeric<=date_break3,]
  grid2<-grid2[grid2$date_numeric>date_break3+3 & grid2$date_numeric<=date_break4,]
  grid1<-grid1[grid1$date_numeric>date_break4+1,]
  
  min_grid <- min(grid1$date_numeric, grid2$date_numeric, grid3$date_numeric,grid4$date_numeric, grid5$date_numeric)
  
  grid1$date <- min(mob_df$date)+grid1$date_numeric - min_grid
  grid2$date <- min(mob_df$date)+grid2$date_numeric - min_grid
  grid3$date <- min(mob_df$date)+grid3$date_numeric - min_grid
  grid4$date <- min(mob_df$date)+grid4$date_numeric - min_grid
  grid5$date <- min(mob_df$date)+grid5$date_numeric - min_grid
  
  a<-ggplot(data=mob_df2, aes(x=date, y=percent_diff, group=weekdays, color=weekdays)) +
    labs(x = NULL, y = "Percentage of baseline", color=NULL)+
    scale_x_date(labels = NULL) +
    geom_point(data=mob_df2, aes(x=date, y=percent_diff, group=weekdays, color=weekdays)) +
    scale_color_manual(values = c(muted("#ff9700", l = 70, c = 100),muted("#080fff", l = 40, c = 100),muted("#ff0000", l = 40, c = 100),muted("#d100ff", l = 60, c = 100)))+
    geom_line(data=grid1, aes(y=y))+
    geom_line(data=grid2, aes(y=y))+
    geom_line(data=grid3, aes(y=y))+
    geom_line(data=grid4, aes(y=y))+
    geom_line(data=grid5, aes(y=y))+
    geom_ribbon(data = grid1, aes(y=y,ymin=se.lo, ymax=se.hi), alpha=0.1)+
    geom_ribbon(data = grid2, aes(y=y,ymin=se.lo, ymax=se.hi), alpha=0.1)+
    geom_ribbon(data = grid3, aes(y=y,ymin=se.lo, ymax=se.hi), alpha=0.1)+
    geom_ribbon(data = grid4, aes(y=y,ymin=se.lo, ymax=se.hi), alpha=0.1)+
    geom_ribbon(data = grid5, aes(y=y,ymin=se.lo, ymax=se.hi), alpha=0.1)+
    guides(color=guide_legend(nrow=2,byrow=TRUE), fill=FALSE) +
    geom_point(aes(colour = weekdays), size = 2)+
    plot_theme+
    theme(legend.position="none")
  
  co4$likelihhod <- (min(co4$AICc)-co4$AICc)/2
  Var1<-co4[co4$AICc==min(co4$AICc),]$Var1
  Var2<-co4[co4$AICc==min(co4$AICc),]$Var2
  Var3<-co4[co4$AICc==min(co4$AICc),]$Var3
  Var4<-co4[co4$AICc==min(co4$AICc),]$Var4
  temp1<-co4[co4$likelihhod>-1.96 & co4$Var4==Var4 & co4$Var2==Var2 & co4$Var3==Var3,]
  temp2<-co4[co4$likelihhod>-1.96 & co4$Var1==Var1 & co4$Var4==Var4 & co4$Var3==Var3,]
  temp3<-co4[co4$likelihhod>-1.96 & co4$Var1==Var1 & co4$Var2==Var2 & co4$Var4==Var4,]
  temp4<-co4[co4$likelihhod>-1.96 & co4$Var1==Var1 & co4$Var2==Var2 & co4$Var3==Var3,]
  
  mob_df$prob1 <- 0
  mob_df$prob2 <- 0
  mob_df$prob3 <- 0
  mob_df$prob4 <- 0
  for (i in seq_len(length(indices_to_break))){
    index=indices_to_break[i]
    if(nrow(temp1[temp1$Var1==index,])==1){
      mob_df[mob_df$date_numeric==index,]$prob1 <- 1
    }
    if(nrow(temp2[temp2$Var2==index,])==1){
      mob_df[mob_df$date_numeric==index,]$prob2 <- 1
    }
    if(nrow(temp3[temp3$Var3==index,])==1){
      mob_df[mob_df$date_numeric==index,]$prob3 <- 1
    }
    if(nrow(temp4[temp4$Var4==index,])==1){
      mob_df[mob_df$date_numeric==index,]$prob4 <- 1
    }
  }
  
  mob_df2$prob1 <- 0
  mob_df2$prob2 <- 0
  mob_df2$prob3 <- 0
  mob_df2$prob4 <- 0
  
  for(i in seq_len(nrow(mob_df2))){
    if(nrow(mob_df[mob_df$date_numeric==mob_df2$date_numeric[i],])==1){
      mob_df2$prob1[i] <- mob_df[mob_df$date_numeric==mob_df2$date_numeric[i],]$prob1
      mob_df2$prob2[i] <- mob_df[mob_df$date_numeric==mob_df2$date_numeric[i],]$prob2
      mob_df2$prob3[i] <- mob_df[mob_df$date_numeric==mob_df2$date_numeric[i],]$prob3
      mob_df2$prob4[i] <- mob_df[mob_df$date_numeric==mob_df2$date_numeric[i],]$prob4
    } else if(i>1){
      mob_df2$prob1[i] <- mob_df2$prob1[i-1]
      mob_df2$prob2[i] <- mob_df2$prob2[i-1]
      mob_df2$prob3[i] <- mob_df2$prob3[i-1]
      mob_df2$prob4[i] <- mob_df2$prob4[i-1]
    } else{
      mob_df2$prob1[i]=0
      mob_df2$prob2[i]=0
      mob_df2$prob3[i]=0
      mob_df2$prob4[i]=0
    } 
  }
  
  
  lower1 <- mob_df2[mob_df2$prob1==1,]$date[1]
  lower2 <- mob_df2[mob_df2$prob2==1,]$date[1]
  lower3 <- mob_df2[mob_df2$prob3==1,]$date[1]
  lower4 <- mob_df2[mob_df2$prob4==1,]$date[1]
  
  upper1 <- tail(mob_df2[mob_df2$prob1==1,]$date, n=1)+1
  upper2 <- tail(mob_df2[mob_df2$prob2==1,]$date, n=1)+1
  upper3 <- tail(mob_df2[mob_df2$prob3==1,]$date, n=1)+1
  upper4 <- tail(mob_df2[mob_df2$prob4==1,]$date, n=1)+1
  
  lower5 <-mob_df2[43,]$date-0.5
  upper5 <-mob_df2[45,]$date+0.5
  
  
  b<-ggplot(aes(x=date, y=prob1), data = mob_df2) +
    geom_errorbarh(data=mob_df2, aes(y= 0.5,xmin=lower1,xmax=upper1))+
    geom_errorbarh(data=mob_df2, aes(y= 0.5,xmin=lower2,xmax=upper2))+
    geom_errorbarh(data=mob_df2, aes(y= 0.5,xmin=lower3,xmax=upper3))+
    geom_errorbarh(data=mob_df2, aes(y= 0.5,xmin=lower4,xmax=upper4))+
    geom_errorbarh(data=mob_df2, aes(y= 0.5,xmin=lower5,xmax=upper5), linetype='dashed', color='white')+
    xlab("Date") + ylab("") +
    plot_theme+
    scale_y_continuous("", breaks=c(0), labels=c("0.0"), limits=c(0,1))+
    theme(axis.text.y=element_text(colour="white"))
  
  
  
  gb1 <- ggplot_build(a)
  gb2 <- ggplot_build(b)
  
  n1 <- length(gb1$panel$ranges[[1]]$y.labels)
  n2 <- length(gb2$panel$ranges[[1]]$y.labels)
  gA <- ggplot_gtable(gb1)
  gB <- ggplot_gtable(gb2)
  g <- gtable:::rbind_gtable(gA, gB, "last")
  panels <- g$layout$t[grep("panel", g$layout$name)]
  g$heights[panels][1]<-unit(10,"null")
  g$heights[panels]
  grid.newpage()
  
  return(g)
  
  
}




graph_function_5 <- function(){
  model = lm5
  date_break1 <- co5[co5$AICc==min(co5$AICc),]$Var1
  date_break2 <- co5[co5$AICc==min(co5$AICc),]$Var2
  date_break3 <- co5[co5$AICc==min(co5$AICc),]$Var3
  date_break4 <- co5[co5$AICc==min(co5$AICc),]$Var4
  date_break5 <- co5[co5$AICc==min(co5$AICc),]$Var5
  grid <- expand.grid(date_numeric=seq(min(mob_df$date_numeric), max_date, length.out=500),weekdays=c(0,1), cat=c(0,1,2,3,4,5))
  grid$weekdays <- factor(grid$weekdays)
  grid$cat <- factor(grid$cat)
  grid <- predictor5(lm5, grid)
  grid<-grid[grid$weekdays==1,]
  
  grid1<-grid[grid$cat==0,]
  grid2<-grid[grid$cat==1,]
  grid3<-grid[grid$cat==2,]
  grid4<-grid[grid$cat==3,]
  grid5<-grid[grid$cat==4,]
  grid6<-grid[grid$cat==5,]
  
  
  grid6<-grid6[grid6$date_numeric<=date_break1,]
  grid5<-grid5[grid5$date_numeric>date_break1+3 & grid5$date_numeric<=date_break2,]
  grid4<-grid4[grid4$date_numeric>date_break2+1 & grid4$date_numeric<=date_break3,]
  grid3<-grid3[grid3$date_numeric>date_break3+3 & grid3$date_numeric<=date_break4,]
  grid2<-grid2[grid2$date_numeric>date_break4+1 & grid2$date_numeric<=date_break5,]
  grid1<-grid1[grid1$date_numeric>date_break5+1,]
  
  min_grid <- min(grid1$date_numeric, grid2$date_numeric, grid3$date_numeric,grid4$date_numeric, grid5$date_numeric, grid6$date_numeric)
  
  grid1$date <- min(mob_df$date)+grid1$date_numeric - min_grid
  grid2$date <- min(mob_df$date)+grid2$date_numeric - min_grid
  grid3$date <- min(mob_df$date)+grid3$date_numeric - min_grid
  grid4$date <- min(mob_df$date)+grid4$date_numeric - min_grid
  grid5$date <- min(mob_df$date)+grid5$date_numeric - min_grid
  grid6$date <- min(mob_df$date)+grid6$date_numeric - min_grid
  
  a<-ggplot(data=mob_df2, aes(x=date, y=percent_diff, group=weekdays, color=weekdays)) +
    labs(x = NULL, y = "Percentage of baseline", color=NULL)+
    scale_x_date(labels = NULL) +
    geom_point(data=mob_df2, aes(x=date, y=percent_diff, group=weekdays, color=weekdays)) +
    scale_color_manual(values = c(muted("#ff9700", l = 70, c = 100),muted("#080fff", l = 40, c = 100),muted("#ff0000", l = 40, c = 100),muted("#d100ff", l = 60, c = 100)))+
    geom_line(data=grid1, aes(y=y))+
    geom_line(data=grid2, aes(y=y))+
    geom_line(data=grid3, aes(y=y))+
    geom_line(data=grid4, aes(y=y))+
    geom_line(data=grid5, aes(y=y))+
    geom_line(data=grid6, aes(y=y))+
    geom_ribbon(data = grid1, aes(y=y,ymin=se.lo, ymax=se.hi), alpha=0.1)+
    geom_ribbon(data = grid2, aes(y=y,ymin=se.lo, ymax=se.hi), alpha=0.1)+
    geom_ribbon(data = grid3, aes(y=y,ymin=se.lo, ymax=se.hi), alpha=0.1)+
    geom_ribbon(data = grid4, aes(y=y,ymin=se.lo, ymax=se.hi), alpha=0.1)+
    geom_ribbon(data = grid5, aes(y=y,ymin=se.lo, ymax=se.hi), alpha=0.1)+
    geom_ribbon(data = grid6, aes(y=y,ymin=se.lo, ymax=se.hi), alpha=0.1)+
    guides(color=guide_legend(nrow=2,byrow=TRUE), fill=FALSE) +
    geom_point(aes(colour = weekdays), size = 2)+
    plot_theme+
    theme(legend.position="none")
  
  co5$likelihhod <- (min(co5$AICc)-co5$AICc)/2
  Var1<-co5[co5$AICc==min(co5$AICc),]$Var1
  Var2<-co5[co5$AICc==min(co5$AICc),]$Var2
  Var3<-co5[co5$AICc==min(co5$AICc),]$Var3
  Var4<-co5[co5$AICc==min(co5$AICc),]$Var4
  Var5<-co5[co5$AICc==min(co5$AICc),]$Var5
  temp1<-co5[co5$likelihhod>-1.96 & co5$Var4==Var4 & co5$Var2==Var2 & co5$Var3==Var3& co5$Var5==Var5,]
  temp2<-co5[co5$likelihhod>-1.96 & co5$Var1==Var1 & co5$Var4==Var4 & co5$Var3==Var3& co5$Var5==Var5,]
  temp3<-co5[co5$likelihhod>-1.96 & co5$Var1==Var1 & co5$Var2==Var2 & co5$Var4==Var4& co5$Var5==Var5,]
  temp4<-co5[co5$likelihhod>-1.96 & co5$Var1==Var1 & co5$Var2==Var2 & co5$Var3==Var3& co5$Var5==Var5,]
  temp5<-co5[co5$likelihhod>-1.96 & co5$Var1==Var1 & co5$Var2==Var2 & co5$Var3==Var3& co5$Var4==Var4,]
  
  mob_df$prob1 <- 0
  mob_df$prob2 <- 0
  mob_df$prob3 <- 0
  mob_df$prob4 <- 0
  mob_df$prob5 <- 0
  for (i in seq_len(length(indices_to_break))){
    index=indices_to_break[i]
    if(nrow(temp1[temp1$Var1==index,])==1){
      mob_df[mob_df$date_numeric==index,]$prob1 <- 1
    }
    if(nrow(temp2[temp2$Var2==index,])==1){
      mob_df[mob_df$date_numeric==index,]$prob2 <- 1
    }
    if(nrow(temp3[temp3$Var3==index,])==1){
      mob_df[mob_df$date_numeric==index,]$prob3 <- 1
    }
    if(nrow(temp4[temp4$Var4==index,])==1){
      mob_df[mob_df$date_numeric==index,]$prob4 <- 1
    }
    if(nrow(temp5[temp5$Var5==index,])==1){
      mob_df[mob_df$date_numeric==index,]$prob5 <- 1
    }
  }
  
  mob_df2$prob1 <- 0
  mob_df2$prob2 <- 0
  mob_df2$prob3 <- 0
  mob_df2$prob4 <- 0
  mob_df2$prob5 <- 0
  
  for(i in seq_len(nrow(mob_df2))){
    if(nrow(mob_df[mob_df$date_numeric==mob_df2$date_numeric[i],])==1){
      mob_df2$prob1[i] <- mob_df[mob_df$date_numeric==mob_df2$date_numeric[i],]$prob1
      mob_df2$prob2[i] <- mob_df[mob_df$date_numeric==mob_df2$date_numeric[i],]$prob2
      mob_df2$prob3[i] <- mob_df[mob_df$date_numeric==mob_df2$date_numeric[i],]$prob3
      mob_df2$prob4[i] <- mob_df[mob_df$date_numeric==mob_df2$date_numeric[i],]$prob4
      mob_df2$prob5[i] <- mob_df[mob_df$date_numeric==mob_df2$date_numeric[i],]$prob5
    } else if(i>1){
      mob_df2$prob1[i] <- mob_df2$prob1[i-1]
      mob_df2$prob2[i] <- mob_df2$prob2[i-1]
      mob_df2$prob3[i] <- mob_df2$prob3[i-1]
      mob_df2$prob4[i] <- mob_df2$prob4[i-1]
      mob_df2$prob5[i] <- mob_df2$prob5[i-1]
    } else{
      mob_df2$prob1[i]=0
      mob_df2$prob2[i]=0
      mob_df2$prob3[i]=0
      mob_df2$prob4[i]=0
      mob_df2$prob5[i]=0
    } 
  }
  
  
  lower1 <- mob_df2[mob_df2$prob1==1,]$date[1]
  lower2 <- mob_df2[mob_df2$prob2==1,]$date[1]
  lower3 <- mob_df2[mob_df2$prob3==1,]$date[1]
  lower4 <- mob_df2[mob_df2$prob4==1,]$date[1]
  lower5 <- mob_df2[mob_df2$prob5==1,]$date[1]
  
  upper1 <- tail(mob_df2[mob_df2$prob1==1,]$date, n=1)+1
  upper2 <- tail(mob_df2[mob_df2$prob2==1,]$date, n=1)+1
  upper3 <- tail(mob_df2[mob_df2$prob3==1,]$date, n=1)+1
  upper4 <- tail(mob_df2[mob_df2$prob4==1,]$date, n=1)+1
  upper5 <- tail(mob_df2[mob_df2$prob5==1,]$date, n=1)+1

  
  
  b<-ggplot(aes(x=date, y=prob1), data = mob_df2) +
    geom_errorbarh(data=mob_df2, aes(y= 0.5,xmin=lower1,xmax=upper1))+
    geom_errorbarh(data=mob_df2, aes(y= 0.5,xmin=lower2,xmax=upper2))+
    geom_errorbarh(data=mob_df2, aes(y= 0.5,xmin=lower3,xmax=upper3))+
    geom_errorbarh(data=mob_df2, aes(y= 0.5,xmin=lower4,xmax=upper4))+
    geom_errorbarh(data=mob_df2, aes(y= 0.5,xmin=lower5,xmax=upper5))+
    xlab("Date") + ylab("") +
    plot_theme+
    scale_y_continuous("", breaks=c(0), labels=c("0.0"), limits=c(0,1))+
    theme(axis.text.y=element_text(colour="white"))
  
  
  
  gb1 <- ggplot_build(a)
  gb2 <- ggplot_build(b)
  
  n1 <- length(gb1$panel$ranges[[1]]$y.labels)
  n2 <- length(gb2$panel$ranges[[1]]$y.labels)
  gA <- ggplot_gtable(gb1)
  gB <- ggplot_gtable(gb2)
  g <- gtable:::rbind_gtable(gA, gB, "last")
  panels <- g$layout$t[grep("panel", g$layout$name)]
  g$heights[panels][1]<-unit(10,"null")
  g$heights[panels]
  grid.newpage()
  grid.draw(g)
  return(g)
  
  
}

