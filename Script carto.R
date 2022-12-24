install.packages('devtools')
library(devtools)
install_github(repo = 'ALUES', username = 'alstat')
install.packages("remotes")
remotes::install_github("alstat/ALUES")
library(ALUES)
x <- MarinduqueTemp
View(MarinduqueTemp)
y <- BANANATemp
suitability_of_LUs <- suit("Banana",temp=x,y,sow_month = 1)
suitability_of_LUs

rice_suit <- suit("ricebr", water=MarinduqueWater, temp=MarinduqueTemp, sow_month = 1)
lapply(rice_suit[["water"]], function(x) head(x))


A4 <- FuzzyNumber(1, 6, 14, 15)
A4
## Fuzzy number with:
## support=[1,5],
## core=[2,4].
plot(A4)
A6 <- FuzzyNumber(1, 6, 14, 15,left=function(8),right=function(12))
view(iris)

p1<-ggplot()+geom_polygon(data=yde, aes(long+0.008,lat-0.005, group=group), fill="#9ecae1")+
  geom_polygon(data=yde, aes(long,lat, group=group), colour="grey10",fill="#fff7bc")+
  geom_text(data=munnames, aes(x=X1, y=X2,label=label), size=3, colour="grey20")+
  coord_equal()+theme_bw()+xlab("")+ylab("")+
  scale_x_continuous(breaks=seq(121.8,122.2, 0.1), labels=c(paste(seq(121.8,122.2, 0.1),"°E", sep="")))+
  scale_y_continuous(breaks=seq(13.2,13.6, 0.1), labels=c(paste(seq(13.2,13.6, 0.1),"°N", sep="")))+
  theme(axis.text.y =element_text(angle = 90, hjust=0.5))
png(file="mrdq.png",w=1800,h=1800, res=300)
grid.newpage()
A1 <- FuzzyNumber(1, 2, 4, 7,
                  left=function(x) x,
                  right=function(x) 1-x
)
plot(A1)
