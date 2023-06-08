
# load data

#uNK_Qupath_230525 <- read.delim("~/Documents/Rprojects/Paul_paired_biopsy_230119/uNK_Qupath_230525.txt")

#qupath_edit <- uNK_Qupath_230525
#qupath_edit$Qupath <- plyr::mapvalues(qupath_edit$Qupath, from=c(0,100), to=c(1,99.9))

#qupath <- createCentileGraph(cycle_day = qupath_edit$day_of_cycle, value=qupath_edit$Qupath/100)
#qupath <- calculateQuantiles(qupath)


#qupath_edit$Qupath <- qupath_edit$Qupath/100
#funbet.ext(as.data.frame(qupath_edit),mycol="Qupath",,LH.start=5,LH.end=12)



#qupath <- createCentileGraph(cycle_day = uNK_Qupath_230525$day_of_cycle, value=uNK_Qupath_230525$Qupath/100)
#qupath <- calculateQuantiles(qupath)



#qpath8 <- subset(uNK_Qupath_230525, day_of_cycle == 8)
#qupath8 <- createCentileGraph(cycle_day = qpath8$day_of_cycle, value=qpath8$Qupath/100)
#qupath8 <- calculateQuantiles(qupath8)

# beta distribution cannot deal with 0 or 1
#MASS::fitdistr(qupath8@value[qupath8@value > 0],densfun="beta",start=list(shape1=0.5,shape2=0.5))


#library(fitdistrplus)
#fitdistrplus::descdist(uNK_Qupath_230525$Qupath[uNK_Qupath_230525$day_of_cycle == 10]/100, discrete = FALSE)
