#Large diamond plot practice: single plot
dat2=read_excel("C:\\Users\\lhr19\\Desktop\\test.xlsx")
dat_Rice_thesis_PCBs_BDEs_OCPs_Q90 <-read_excel("C:\\Users\\lhr19\\Desktop\\REAL2\\Rice_thesis_PCBs_BDES_OCPs_Q50_reduced_4.xlsx")
View(dat_Rice_thesis_PCBs_BDEs_OCPs_Q90)
diamond_plot<-function(dataset,exposure=dataset$Exposure,outcome=dataset$Outcome,title="",labelcolor1="black",
                       labelcolor2="blue"){
require(readxl)
require(colorspace)
exposure=exposure
outcome=outcome
dat_exposure=subset(dataset,Exposure%in%exposure)
dat_outcome=subset(dat_exposure,Outcome%in%outcome)

chem_label<-unique(dat_outcome$Outcome)
chem_num<-length(chem_label)
fish_label<-unique(dat_outcome$Exposure)
fish_num<-length(fish_label)


n_matrix<-fish_num*chem_num;

{	# remaining code in brackets, to ensure that plot does not procede if error check fails
  nrows <- dim(dat_outcome)[1]
  if(n_matrix != nrows) stop(paste("Dimensions do not match--data matrix has",nrows,"rows, but",n_matrix,"rows are expected.")) 
  

  par(xpd=TRUE)
  
  plot(0,0, xlim=c(0-chem_num-5, fish_num+5), ylim=c(0-3,fish_num+chem_num),
       axes=F, type="n", xlab="", ylab="",main=title); 
  #-------------------------------------------------------------------------------------
  # 3. Obtain the coordinate of the center of each square cell (cx,cy) with diagonal= 2;
  #    i.e., the distance from the center to a vertex of a square cell equals to 1.
  #-------------------------------------------------------------------------------------
  
  ## fish_num and chem_num were reversed here; now swapped
  for (i in 1:chem_num)
  {
    if (i == 1){
      cx <- seq(0,(fish_num-1) )
      cy <- seq(1,fish_num)
    }
    else{
      cx <- c(cx, seq( (1-i),(fish_num-i) ) )
      cy <- c(cy, seq(i,(fish_num+i-1)) )
    }
  }
  

  beta_coef<- dat_outcome$estimate
  

  standard_error<-dat_outcome$SE
  maxstandard_error<-range(standard_error,na.rm=T)[2] 
  minstandard_error<-range(standard_error,na.rm=T)[1] 
  
  
  relative_n <- (standard_error / minstandard_error)^-2  # something like this (relative precision) might work better, 
  # but won't work if any SEs are exactly 0 as with your data 
  
  scale_graph <- 1.005 - 0.005*(fish_num+chem_num);
  
  relative_n <- relative_n * scale_graph 
  maxrn <- max(relative_n,na.rm=T)
  rnq <- quantile(relative_n,seq(0,1,0.2),na.rm=T)
  rnq[1] <- 0
  rncat <- cut(relative_n,breaks=rnq)
  relative_n <- (maxrn*seq(0.2,1,0.2))[rncat]
 
  #Hexagons inside
  x1<- cx - (1-relative_n)/2
  x2<- cx + (1-relative_n)/2
  x3<- cx + (1+relative_n)/2
  x4<- cx + (1-relative_n)/2
  x5<- cx - (1-relative_n)/2
  x6<- cx - (1+relative_n)/2
  y1<- cy + relative_n
  y2<- cy + relative_n
  y3<- cy
  y4<- cy - relative_n
  y5<- cy - relative_n
  y6<- cy
  
  
  maxav <- min(abs(range(beta_coef,na.rm=T)))  # use short range, centered at 0
  # or, consider choosing breaks manually, centered at 0
  breaks_col <- seq(-maxav,maxav,length=60)
  breaks_col[1] <- -Inf
  breaks_col[60] <- Inf
  rbPal <- diverge_hsv(60)
  color_names<-rbPal[as.numeric(.bincode(beta_coef,breaks_col,include.lowest=T))]
  
  
  color_names
  sort(beta_coef)
  
  #for hexagons/horizontal fill:
  
  for (i in 1:n_matrix) {        
    if (!is.na(color_names[i])) { 
      polygon(c(x1[i],x2[i],x3[i],x4[i],x5[i],x6[i]), 
              c(y1[i],y2[i],y3[i],y4[i],y5[i],y6[i]), density=-.1, border='black', col=c(color_names[i]))
    }
  }
  
  
  # Add labels to axes
  
  # axes were reversed; now swapped
  # Loop across chemicals to label:
  #Font 3 is ok
  for(i in 1:chem_num){
    text(paste(chem_label[i]),x=-0.05-i,y=-1.4+i+(i*0.01),srt=45,cex=0.65, adj = c(1,0),font=2,col = labelcolor1)
  }
  
  
  #Loop across chemicals to label
  for(i in 1:fish_num){
    text(paste(fish_label[i]),x=0.05+i,y=-1.4+i+(i*0.01),srt=-45,cex=0.65,col=labelcolor2, adj = c(0,0),font=2)
  }
  
  
}	
}
