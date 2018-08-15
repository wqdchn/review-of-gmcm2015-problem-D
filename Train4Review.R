#########################
# @program: helloworld
# @author: wqdong
# @description: review of gmcm2015 problem D
# @create: 2018-08-15 08:05
#################################################

detat<-0.05

##################################################################
##just tip, not real function ################
run2go<-function(){
  for (t in seq(0,timepoint1,detat)) {
    if(v<51.5){
      F<-203
    }else{
      F<-(-0.002032*v^3+0.4928*v^2-42.13*v+1343)
    }
    
    w0<-(a+b*v+c*v^2)
    aa<-((F*1000-w0*g*m/1000)/m)
    if(1<aa){
      aa<-1
    }else if(-1>aa){
      aa<-(-1)
    }
    rs[i,1]<-l
    rs[i,2]<-v
    l<-(l+v*detat/3.6+0.5*aa*detat^2)
    
    v<-(v+aa*3.6*detat)
    w<-(w+F*(v*detat/3.6+0.5*aa*detat^2))
    i<-i+1
  }
}
##just tip, not real function ################
run2lazy<-function(){
  for (t in seq(timepoint1,timepoint2,detat)){
    w0<-a+b*v+c*v^2
    bb<-(-(w0*g*m/1000)/m)
    rs[i,1]<-l
    rs[i,2]<-v
    l=l+v*detat/3.6+0.5*bb*detat*detat
    v=v+bb*detat*3.6
    i=i+1
  }
}
##just tip, not real function ################
run2stop<-function(){
  for (t in seq(timepoint2,time,detat)){
    if(v<=77){
      B=166
    }else if(v<=80){
      B=0.1343*v^2-25.07*v+1300
    }
    w0=a+b*v+c*v^2
    cc=-(B*1000+w0*g*m/1000)/m
    if(cc>1){
      cc=1
    }
    else if(-1>cc){
      cc=-1;
    }
    rs[i,1]<-l
    rs[i,2]<-v
    l=l+v*detat/3.6+0.5*cc*detat*detat;
    v=v+cc*detat*3.6;
    i=i+1;
  }
}

###################################################################

getLWVT<-function(distan,time,timepoint1,timepoint2){
  
  a<-2.031
  b<-0.0622
  c<-0.001807
  g<-9.8
  m<-194295
  
  l<-0
  w<-0
  v<-0
  
  i<-1
  rs<-matrix(data=NA,nrow=1.1*time/detat,ncol = 2)
  
  for (t in seq(0,timepoint1,detat)) {
    if(v<51.5){
      F<-203
    }else{
      F<-(-0.002032*v^3+0.4928*v^2-42.13*v+1343)
    }
    
    w0<-(a+b*v+c*v^2)
    aa<-((F*1000-w0*g*m/1000)/m)
    if(1<aa){
      aa<-1
    }else if(-1>aa){
      aa<-(-1)
    }
    rs[i,1]<-l
    rs[i,2]<-v
    l<-(l+v*detat/3.6+0.5*aa*detat^2)
    
    v<-(v+aa*3.6*detat)
    w<-(w+F*(v*detat/3.6+0.5*aa*detat^2))
    i<-i+1
  }
  for (t in seq(timepoint1,timepoint2,detat)){
    w0<-a+b*v+c*v^2
    bb<-(-(w0*g*m/1000)/m)
    rs[i,1]<-l
    rs[i,2]<-v
    l=l+v*detat/3.6+0.5*bb*detat*detat
    v=v+bb*detat*3.6
    i=i+1
  }
  for (t in seq(timepoint2,time,detat)){
    if(v<=77){
      B=166
    }else if(v<=80){
      B=0.1343*v^2-25.07*v+1300
    }
    w0=a+b*v+c*v^2
    cc=-(B*1000+w0*g*m/1000)/m
    if(cc>1){
      cc=1
    }
    else if(-1>cc){
      cc=-1;
    }
    rs[i,1]<-l
    rs[i,2]<-v
    l=l+v*detat/3.6+0.5*cc*detat*detat;
    v=v+cc*detat*3.6;
    i=i+1;
  }
  
  lwv<-data.frame(l,w,v)
  return(lwv)
}

#############################################################################

getPlot<-function(distan,time,timepoint1,timepoint2){
  
  plotdata<-data.frame(l=0,v=0)
  a<-2.031
  b<-0.0622
  c<-0.001807
  g<-9.8
  m<-194295
  
  l<-0
  w<-0
  v<-0
  
  i<-1
  rs<-matrix(data=NA,nrow=1.1*time/detat,ncol = 2)
  
  for (t in seq(0,timepoint1,detat)) {
    if(v<51.5){
      F<-203
    }else{
      F<-(-0.002032*v^3+0.4928*v^2-42.13*v+1343)
    }
    
    w0<-(a+b*v+c*v^2)
    aa<-((F*1000-w0*g*m/1000)/m)
    if(1<aa){
      aa<-1
    }else if(-1>aa){
      aa<-(-1)
    }
    rs[i,1]<-l
    rs[i,2]<-v
    l<-(l+v*detat/3.6+0.5*aa*detat^2)
    
    v<-(v+aa*3.6*detat)
    w<-(w+F*(v*detat/3.6+0.5*aa*detat^2))
    i<-i+1
  }
  for (t in seq(timepoint1,timepoint2,detat)){
    w0<-a+b*v+c*v^2
    bb<-(-(w0*g*m/1000)/m)
    rs[i,1]<-l
    rs[i,2]<-v
    l=l+v*detat/3.6+0.5*bb*detat*detat
    v=v+bb*detat*3.6
    i=i+1
  }
  for (t in seq(timepoint2,time,detat)){
    if(v<=77){
      B=166
    }else if(v<=80){
      B=0.1343*v^2-25.07*v+1300
    }
    w0=a+b*v+c*v^2
    cc=-(B*1000+w0*g*m/1000)/m
    if(cc>1){
      cc=1
    }
    else if(-1>cc){
      cc=-1;
    }
    rs[i,1]<-l
    rs[i,2]<-v
    l=l+v*detat/3.6+0.5*cc*detat*detat;
    v=v+cc*detat*3.6;
    i=i+1;
  }
  for(k in 1:nrow(rs)){
    plotdata<-rbind(plotdata,data.frame(l=rs[k,1],v=rs[k,2]))
  }
  plotdata<-na.omit(plotdata)
  
  for(i in 1:length(plotdata$l)){
    if(!is.na(plotdata$l[i])){
      if(plotdata$l[i]<120 & 
                    plotdata$l[i]>0){
        plotdata$vlim[i]<-55
      }else if(plotdata$l[i]<plotdata$l[time/detat]){
        plotdata$vlim[i]<-80
      }
    }
    plotdata$vlim[1]<-plotdata$vlim[time/detat] <-plotdata$vlim[length(plotdata$l)]<-0
  }
  
  return(plotdata)
  
}

#######################################

#build distan and time of station
A<-data.frame(Away='A1toA2',distan=1334,time=101.3)
A<-rbind(A,data.frame(Away='A2toA3',distan=1286,time=97.7))
A<-rbind(A,data.frame(Away='A3toA4',distan=2086,time=158.4))
A<-rbind(A,data.frame(Away='A4toA5',distan=2265,time=172))
A<-rbind(A,data.frame(Away='A5toA6',distan=2338,time=177.6))
A<-rbind(A,data.frame(Away='A6toA7',distan=1354,time=102.8))
A<-rbind(A,data.frame(Away='A7toA8',distan=1280,time=97.2))
A<-rbind(A,data.frame(Away='A8toA9',distan=1538,time=116.8))
A<-rbind(A,data.frame(Away='A9toA10',distan=993,time=75.4))
A<-rbind(A,data.frame(Away='A10toA11',distan=1982,time=150.5))
A<-rbind(A,data.frame(Away='A11toA12',distan=2366,time=179.7))
A<-rbind(A,data.frame(Away='A12toA13',distan=1275,time=96.8))
A<-rbind(A,data.frame(Away='A13toA14',distan=2631,time=199.8))

timepoint1<-seq(ceiling(A[1,]$time/9),ceiling(A[1,]$time/3),1)
timepoint2<-seq(ceiling(A[1,]$time-(A[1,]$time/3)),A[1,]$time,1)

##################################################################
###get timepoint that we need
for(k in 1:nrow(A)){
  rw<-10000000
  rv<-90
  timepoint1<-seq(ceiling(A[k,]$time/9),ceiling(A[k,]$time/3),1)
  timepoint2<-seq(ceiling(A[k,]$time-(A[k,]$time/3)),A[k,]$time-5,1)
  for (i in 1:length(timepoint1)) {
    for (j in 1:length(timepoint2)) {
      p<-getLWVT(A[k,]$distan,A[k,]$time,timepoint1[i],timepoint2[j])
      if(abs(p[3])< 1){
        if(p[1]<(A[k,]$distan+50)&p[1]>(A[k,]$distan-50)){
          if(p[2]<rw){
            rw<-p[2]
            A$timepoint1[k]<-timepoint1[i]
            A$timepoint2[k]<-timepoint2[j]
          }
        }
        
      }
    }
  }
}

##################################
###get plot of A1 to A14
q<-data.frame(l=0,v=0,vlim=0)
p<-getPlot(A[1,]$distan,A[1,]$time,A[1,]$timepoint1,A[1,]$timepoint2)
p<-na.omit(p)

for(i in 2:nrow(A)){
  q<-getPlot(A[i,]$distan,A[i,]$time,A[i,]$timepoint1,A[i,]$timepoint2)
  temp<-max(p$l)
  for(j in 1:nrow(q)){
    p<-rbind(p,data.frame(l=q[j,1]+temp,v=q[j,2],vlim=q[j,3]))
  }
  p<-na.omit(p)
  
}
plot(p$l,p$v,type='l',xlim=c(0,max(p$l)),ylim=c(0,90),xlab='A1-to-A14',ylab='')
                       +lines(p$l,p$vlim,type='l',lty=2)

plot(p$l,p$v,type='l',xlim=c(0,max(p$l)),ylim=c(0,90),xlab='A1-to-A14',ylab='')
                       +lines(p$l,p$vlim,type='l',lty=2)

####################################
######### multpack
b<-NA
for(k in 1:13){
  a<-rep(0,round(A$time[k]))
  for(i in 1:A$timepoint1[k]){
    a[i]<-1
  }
  for(i in A$timepoint2[k]:A$time[k]){
    a[i]<-(-1)
  }
  b<-append(b,a)
  if(k<=12){
    b<-append(b,rep(0,30))
  }
}

b<-na.omit(b)

length(b)
#b<-data.frame(b)

c<-rep(0,541)

for (k in 1:2085){
  d<-0
  for (j in 1:(2086-k)){
    if(-1==b[j]*b[j+k]){
      d<-d+1
    }
  }
  c[k]<-d
}

c1<-c[120:660]

c[1:119]<-c[661:1725]<-0
which.max(c)
c[c==0] <- NA
c<-na.omit(c)

####################################

w13<-0

for(k in 1:nrow(A)){
  #print(seq(ceiling(A[k,]$time-(A[k,]$time/3)),A[k,]$time-5,1))
  w13<-w13+getLWVT(A[k,]$distan,A[k,]$time,A[k,]$timepoint1,A[k,]$timepoint2)[2]
  print(getLWVT(A[k,]$distan,A[k,]$time,A[k,]$timepoint1,A[k,]$timepoint2))
}

##################################
#### just run it
x<-50
y<-49

b<-seq(120,660,1)

for(i in 1:length(b)){
  for(j in 1:length(b)){
      if(x*b[i]+y*b[j]==63900){
        print(b[i])
        print(b[j])
    }
  }
}

##################################











