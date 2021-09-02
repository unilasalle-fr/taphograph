######################################################################################
#                                PACKAGES                                            #
######################################################################################

if(!require("tidyverse")) install.packages("tidyverse")

######################################################################################
#                                EDIT DATA                                           #
######################################################################################

type_of_grain = "Mollusks"

#input values standardized by the maximum possible value so they range between 0 and 1 
fragmentation = c(0,1.5,1.5,1,1,0,1,0.5,1,1.5,0,1,0,0.5,0,0.5,0,1.5,1,1.5,1,0.5,0.5)
abrasion = c(3,2,2,2.5,2.5,1.5,2,2,2,2,1.5,2,2,2,2.5,2,2,2,2.5,2,2,2,3)
bioerosion = c(2,1.5,1,1,1.5,1,0.5,1,1.5,0.5,1,1,0.5,0,1.5,1,1,2,0.5,0.5,1,1.5,1.5)
encrustation = rep(0,23) #rep : repeat 0, 23 times


######################################################################################
#                                RESULTS (DO NOT MODIFY)                             #
######################################################################################

#standardize the variables by the total number of variables 
data=cbind(fragmentation=fragmentation/2, abrasion=abrasion/3, bioerosion=bioerosion/3,encrustation)/4 #cumulative sum of mean values
means=cumsum(apply(data,2,mean))
sds = apply(data,2,sd)
sample.cum.means=array(NA, dim=c(nrow(data), 4)) #repeat resampling for 1,000 times 
for (j in 1:nrow(data)) {
  resample.data=array(NA, dim=c(nrow(data), ncol(data)))
  #resample each variable
  for (i in 1:ncol(data)) {
    resample.data[,i]=sample(data[,i], length(data[,i]), replace=T)
  }
  #store resampled values of cumulative means
  sample.cum.means[j,]=cumsum(apply(resample.data,2,mean))
}

lower.95.ci=apply(sample.cum.means, 2, quantile, 0.025) 
upper.95.ci=apply(sample.cum.means, 2, quantile, 0.975)

######################################################################################
#                                PLOT                                                #
######################################################################################
par(mar=c(8,4,2,2))



plot(1:4, means, xaxt="n", type="b", xlab="", cex=1.4, frame=F, ylim=c(0,1.2), pch=16, ylab="Cumulative taphonomic score",
     xlim=c(0.5,4.5)) 

lines(1:4, means, lwd=1)

lines(1:4, c(0.25,0.5,0.75,1), lwd = 3)

segments(x0=1:4, x1=1:4, y0=lower.95.ci,
         y1=upper.95.ci) 

axis(1, at=1:4, labels=c("Fragmentation","Abrasion","Bioerosion","Encrustation"), las=2) #optional, if multiple samples are plotted, then the position of lines can correspond to means 

abline(h=means[1:3], lty=2, lwd=c(1,2,1,1)) 

text(x=3, y=means[1:3]-0.02, labels=c("Fragmentation", "Abrasion","Bioerosion"), pos=4)
text(x =4, y = 1.1, labels = "Max. Value")

title(paste("Taphograph for",type_of_grain))

#### Graphique amélioré
transpo = t(t(means))
transpo2 = data.frame(type = rownames(transpo), means = transpo, sd = sds)
transpo2$type = factor(transpo2$type, levels = c("fragmentation","abrasion","bioerosion","encrustation"))

transpo2 %>% ggplot(aes(x = type, y = means, group = 1)) + 
  geom_line(color = "blue") + 
  geom_point() +
  geom_errorbar(aes(ymin =means -sd, ymax =means + sd), width = 0.1) +
  geom_hline(yintercept = means, linetype = "dashed")+
  geom_abline(intercept = 0, slope = 0.25, color = "red", linetype = "dashed") +
  annotate("text", x = 4, y = 1, label = "Max. Values", color = "red") + 
  scale_y_continuous(limits = c(0,1.2)) +
  ggtitle(paste("Taphograph for", type_of_grain)) +
  ylab("Cumulative taphonomic score") + 
  xlab("") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


transpo2 %>% ggplot(aes(x = type, y = means, group = 1)) + 
  geom_line(color = "blue") + 
  geom_point() +
  geom_errorbar(aes(ymin =means -sd, ymax =means + sd), width = 0.1) +
  geom_hline(yintercept = means, linetype = "dashed")+
  ggtitle(paste("Taphograph for", type_of_grain)) +
  ylab("Cumulative taphonomic score") + 
  xlab("") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
