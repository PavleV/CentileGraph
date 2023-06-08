
FUN.quantiles <- function(values, quantiles, model="beta"){

  if(model == "normal"){

    fitted_dis <- fitdistr(values, densfun="normal")
    qb <- qnorm(quantiles, mean=fitted_dis$estimate[1], sd=fitted_dis$estimate[2], lower.tail = TRUE, log.p = FALSE)

  }
  if(model == "beta"){

    fitted_dis <- MASS::fitdistr(values, densfun="beta",start=list(shape1=1/2,shape2=1/2))
    qb <- qbeta(quantiles, shape1=fitted_dis$estimate[1], shape2=fitted_dis$estimate[2], lower.tail = TRUE, log.p = FALSE)

  }

  qb

}



#

funbet.ext<-function(mytab,mycol=c("Qupath"),LH.start=6,LH.end=11,resolution.width=5){

  reslist<-list()
  for(k in 1:length(mycol)){

    resmat<-matrix(0,resolution.width,LH.end-LH.start+1)

    j<-1

    for (i in LH.start:LH.end){
      tab.d<-subset(mytab, day_of_cycle ==i & eval(as.name(mycol[k]))>0)[,mycol[k]]

      fb <- MASS::fitdistr(tab.d, densfun="beta",start=list(shape1=1/2,shape2=1/2))
      qb <- qbeta(c(0,0.25,0.5,0.75,1), shape1=fb$estimate[1], shape2=fb$estimate[2], lower.tail = TRUE, log.p = FALSE)


      resmat[,j]<-qb
      j<-j+1

    }

    maty<-resmat

    ny<-20

    endy<-ncol(maty)
    leny<-nrow(maty)
    nuend<-ny*(endy-1)
    resmat2<-matrix(0,leny,nuend)

    for (i in 1:leny){
      spliny<-spline(maty[i,1:endy],y=NULL,n=nuend)
      resmat2[i,]<-spliny$y

    }

    resmat2<-t(resmat2)
    colnames(resmat2)<-c("per0","per25","median","per75","per100")

    resmat2<-as.data.frame(resmat2)
    resmat2$xnum <- 1:nrow(resmat2)

    p1 <-  ggplot2::ggplot()+
      ggplot2::geom_line(data=resmat2,ggplot2::aes(xnum,median)) +
      ggplot2::geom_line(data=resmat2,ggplot2::aes(xnum,per25),linetype="dashed") +
      ggplot2::geom_line(data=resmat2,ggplot2::aes(xnum,per75),linetype="dashed") +

      #ggplot2::geom_ribbon(data=resmat2, ggplot2::aes(x=xnum,ymin=per75,ymax=ceiling(max(resmat2$per75)), fill="1) >75% CI"),alpha=0.3)+
      ggplot2::geom_ribbon(data=resmat2, ggplot2::aes(x=xnum,ymin=per75,ymax=0.5, fill="1) >75% CI"),alpha=0.3)+
      ggplot2::geom_ribbon(data=resmat2, ggplot2::aes(x=xnum,ymin=floor(min(resmat2$per25)),ymax=per25, fill="3) <25% CI"),alpha=0.3)+
      ggplot2::geom_ribbon(data=resmat2, ggplot2::aes(x=xnum,ymin=per25,ymax=per75, fill="2) 25-75% CI"),alpha=0.3)+

      ggplot2::ggtitle(mycol[k])+
      ggplot2::xlab("LH")+
      ggplot2::ylab("uNK proportion (Qupath)")+
      #ggplot2::scale_x_continuous(breaks=c(0,20,40,60,80,100),labels=c("6","7","8","9","10","11"))+
      ggplot2::scale_x_continuous(breaks=c(0,20,40,60,80,100,120,140),labels=c("5","6","7","8","9","10","11","12"))+
      #ggplot2::ylim(floor(min(resmat2$per25)),ceiling(max(resmat2$per75)))+
      ggplot2::ylim(0,0.5)+
      ggplot2::theme_bw()+
      ggplot2::theme(legend.title = ggplot2::element_blank())

    pdf(file=paste("New_FigS_",mycol[k],"_heatmap_220529.pdf",sep=""), width=6, height=4, paper="special", compress=F)


    print(p1)

    dev.off()

    reslist[[mycol[k]]] <- p1

  }

  reslist

}






plotCentile.FUN <- function(obj = qupath, LH.start=6,LH.end=11, resolution.width=5, quantiles=c(0,0.25,0.50,0.75,1), ylims = c(0,0.5)){

  data.to.plot <- data.frame(quantile=numeric(), day = numeric(), value=numeric())


  for(i in quantiles){

    spline.resolution <- length(LH.start:LH.end) * resolution.width

    spline.days <- spline(obj@quantiles$value[obj@quantiles$quantile == i], n = spline.resolution)$x
    spline.values <- spline(obj@quantiles$value[obj@quantiles$quantile == i], n = spline.resolution)$y

    data.to.plot <- rbind(data.to.plot, data.frame(quantile=rep(i,length(spline.days)), day = spline.days, value=spline.values))

    }


    p1 <-  ggplot2::ggplot()+
      ggplot2::geom_line(data=subset(data.to.plot, quantile == 0.5), ggplot2::aes(day,value)) +
      ggplot2::geom_line(data=subset(data.to.plot, quantile == 0.25), ggplot2::aes(day,value),linetype="dashed") +
      ggplot2::geom_line(data=subset(data.to.plot, quantile == 0.75), ggplot2::aes(day,value),linetype="dashed") +

      #ggplot2::geom_ribbon(data=resmat2, ggplot2::aes(x=xnum,ymin=per75,ymax=0.5, fill="1) >75% CI"),alpha=0.3)+
      #ggplot2::geom_ribbon(data=resmat2, ggplot2::aes(x=xnum,ymin=floor(min(resmat2$per25)),ymax=per25, fill="3) <25% CI"),alpha=0.3)+
      #ggplot2::geom_ribbon(data=resmat2, ggplot2::aes(x=xnum,ymin=per25,ymax=per75, fill="2) 25-75% CI"),alpha=0.3)+

      ggplot2::xlab("Day of cycle")+
      ggplot2::scale_x_continuous(breaks=c(0,20,40,60,80,100,120,140),labels=c("5","6","7","8","9","10","11","12"))+
      ggplot2::ylim(ylims[1],ylims[2])+
      ggplot2::theme_bw()+
      ggplot2::theme(legend.title = ggplot2::element_blank())



  return(p1) # returns ggplot2 object

}


