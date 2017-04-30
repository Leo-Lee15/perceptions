####---------------------------
#### ggplot Version -----------
####---------------------------


## Import files, load plot and data packages, fire up the number machine.
# setwd("~/Dropbox/R/Perceptions of Probability")
setwd(choose.dir())
probly_url <- 'https://github.com/Leo-Lee15/perceptions/blob/master/probly.csv'
library(rvest)
html_session(probly_url)
probly <- probly_url %>%
  read_html() %>%
  html_table(head = TRUE) %>%
  .[[1]] %>%
  .[, -1]
dplyr::tbl_df(probly)

numberly_url <- 'https://github.com/Leo-Lee15/perceptions/blob/master/numberly.csv'
html_session(numberly_url)
numberly <- numberly_url %>%
  read_html() %>%
  html_table(head = TRUE) %>%
  .[[1]] %>%
  .[, -1]
dplyr::tbl_df(numberly)

library(ggplot2)
library(scales)
library(RColorBrewer)

#Melt data into column format.
numberly <- reshape2::melt(numberly)
# numberly <- tidyr::gather(numberly, key = 'variable', value = 'value')
numberly$variable <- gsub("[.]", " ", numberly$variable)
probly <- reshape2::melt(probly)
# probly <- tidyr::gather(probly, key = 'variable', value = 'value')
probly$variable <- gsub("[.]"," ",probly$variable)

#Order in the court!
probly$variable <- factor(probly$variable,
                          c("Chances Are Slight",
                            "Highly Unlikely",
                            "Almost No Chance",
                            "Little Chance",
                            "Probably Not",
                            "Unlikely",
                            "Improbable",
                            "We Doubt",
                            "About Even",
                            "Better Than Even",
                            "Probably",
                            "We Believe",
                            "Likely",
                            "Probable",
                            "Very Good Chance",
                            "Highly Likely",
                            "Almost Certainly"))
numberly$variable <- factor(numberly$variable, 
                            c("Hundreds of",
                              "Scores of",
                              "Dozens",
                              "Many",
                              "A lot",
                              "Several",
                              "Some",
                              "A few",
                              "A couple",
                              "Fractions of"))

#Modify Theme:
z_theme <- function() {
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[5]
  color.axis.text = palette[7]
  color.axis.title = palette[7]
  color.title = palette[8]
  # Begin construction of chart
  theme_bw(base_size=9) +
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=20, vjust=1.25)) +
    theme(axis.text.x=element_text(size=14,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=14,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=16,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=16,color=color.axis.title, vjust=1.25))
}

#Plot probability data
png(file='plot1.png', width = 800, height = 800)
ggplot(probly,aes(variable,value))+
  geom_boxplot(aes(fill=variable),alpha=.5)+
  geom_jitter(aes(color=variable),size=4,alpha=.2)+
  coord_flip()+
  guides(fill=FALSE,color=FALSE)+
  xlab("Phrase")+
  ylab("Assigned Probability (%)")+
  z_theme()+
  scale_y_continuous(breaks=seq(0,100,10))+
  ggtitle("Perceptions of Probability")
dev.off()

#Plot numberly data
png(file='plot2.png', width = 800, height = 500)
ggplot(numberly,aes(variable,value))+
  geom_boxplot(aes(fill=variable),alpha=0.5)+
  geom_jitter(aes(color=variable),size=4,alpha=.2)+
  scale_y_log10(labels=trans_format("log10",math_format(10^.x)),
                breaks=c(.01,.1,1,10,100,1000,10000,100000))+
  guides(fill=FALSE,color=FALSE)+
  xlab("Phrase")+
  z_theme()+
  ylab("Assigned Number")+
  coord_flip()+
  ggtitle("Perceptions of Numbers")
dev.off()


####---------------------------
#### Base R Version -----------
####---------------------------

## https://github.com/mw55309/perceptions/blob/master/analysis.R

library(RColorBrewer)

# load the data and convert to a matrix
probly_url <- 'https://github.com/Leo-Lee15/perceptions/blob/master/probly.csv'
library(rvest)
html_session(probly_url)
probly <- probly_url %>%
  read_html() %>%
  html_table(head = TRUE) %>%
  .[[1]] %>%
  .[, -1]
p <- probly
dplyr::tbl_df(probly)
m <- as.matrix(p[,ncol(p):1])

# create some random data for jitter
r <-  (matrix(runif(nrow(m)*ncol(m)), nrow=nrow(m), ncol=ncol(m)) / 2) - 0.25

# create colours and colour matrix (for points)
cols  <- colorRampPalette(brewer.pal(12, "Set3"), alpha=TRUE)(ncol(m))
colsm <-matrix(rep(cols, each=nrow(m)), ncol=ncol(m))

# get the greys (stolen from https://github.com/zonination/perceptions/blob/master/percept.R)
palette <- brewer.pal("Greys", n=9)
color.background = palette[2]
color.grid.major = palette[5]

# set graphical area
par(bty="n", bg=palette[2], mar=c(5,8,3,1))

# plot initial boxplot
boxplot(m~col(m), horizontal=TRUE, outline=FALSE, lty=1, staplewex=0, boxwex=0.8, boxlwd=1, medlwd=1, col=cols, xaxt="n", yaxt="n")

# plot gridlines
for (i in seq(0,100,by=10)) {
	lines(c(i,i), c(0,20), col=palette[4])
}

for (i in seq(1,17,by=1)) {
	lines(c(-5,105), c(i,i), col=palette[4])
}

# plot points
points(m, col(m)+r, col=colsm, pch=16)

# overlay boxplot
boxplot(m~col(m), horizontal=TRUE, outline=FALSE, lty=1, staplewex=0, boxwex=0.8, boxlwd=1, medlwd=1, col=cols, add=TRUE, xaxt="n", yaxt="n")

# add axes and title
axis(side=1, at=seq(0,100,by=10), col.axis=palette[7], cex.axis=0.8, lty=0, tick=NA, line=-1)
axis(side=1, at=50, labels="Assigned Probability %", lty=0, tick=NA, col.axis=palette[7])
axis(side=2, at=1:17, col.axis=palette[7], cex.axis=0.8, lty=0, tick=NA, labels=colnames(m), las=2)
axis(side=2, at=17/2, labels="Phrase", col.axis=palette[7], lty=0, tick=NA, las=3, line=6)
title("Perceptions of Probability")












