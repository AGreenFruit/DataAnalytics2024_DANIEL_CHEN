EPI_data <- read.csv("C:/Users/chend17/Downloads/DataAnalytics/DataAnalytics2024_DANIEL_CHEN/Labs/2010EPI_data.csv")

#Replacing the header with the first column
names(EPI_data) <- EPI_data[1,]
EPI_data <- EPI_data[-1,]

View(EPI_data)

attach(EPI_data) 	# sets the ‘default’ object
fix(EPI_data) 	# launches a simple data editor
EPI_data
EPI <- EPI_data$EPI
EPI <- as.numeric(EPI)
EPI 			# prints out values EPI_data$EPI
tf <- is.na(EPI) # records True values if the value is NA
EPI <- EPI[!tf] # filters out NA values, new array
EPI

summary(EPI)
fivenum(EPI, na.rm=True)
stem(EPI)

png("EPI_Histogram.png")
hist(EPI)
hist(EPI,seq(30.,95.,1.0),prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw=1.))
rug(EPI)
dev.off()

png("EPI_CDF.png")
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)
dev.off()

png("EPI_QQNORM.png")
par(pty="s")
qqnorm(EPI)
qqline(EPI)
dev.off()

x<-seq(30,95,1)
png("X_QQPlot.png")
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for tdsn")
qqline(x)
dev.off()

# DALY data
DALY <- EPI_data$DALY
DALY <- as.numeric(DALY)
tf <- is.na(DALY) # records True values if the value is NA
DALY <- DALY[!tf]

summary(DALY)
fivenum(DALY, na.rm=True)
stem(DALY)

png("DALY_Histogram.png")
hist(DALY)
hist(DALY,seq(30.,95.,1.0),prob=TRUE)
lines(density(DALY,na.rm=TRUE,bw=1.))
rug(DALY)
dev.off()

png("DALY_CDF.png")
plot(ecdf(DALY), do.points=FALSE, verticals=TRUE)
dev.off()

png("DALY_QQNORM.png")
par(pty="s")
qqnorm(DALY)
qqline(DALY)
dev.off()

# WATER_H data
WATER <- EPI_data$WATER_H
WATER <- as.numeric(WATER)
tf <- is.na(WATER) # records True values if the value is NA
WATER <- WATER[!tf]

summary(WATER)
fivenum(WATER, na.rm=True)
stem(WATER)

png("WATER_Histogram.png")
hist(WATER)
lines(density(WATER,na.rm=TRUE,bw=1.))
rug(WATER)
dev.off()

png("WATER_CDF.png")
plot(ecdf(WATER), do.points=FALSE, verticals=TRUE)
dev.off()

png("WATER_QQNORM.png")
par(pty="s")
qqnorm(WATER)
qqline(WATER)
dev.off()

png("EPI_DALY_Boxplot.png")
boxplot(EPI, DALY)
dev.off()

png("EPI_DALY_QQPlot.png")
qqplot(EPI, DALY)
dev.off()


# EXERCISE 2
Landlock <- as.numeric(EPI_data$Landlock)
EPILand<-EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob=TRUE)







#other data
GRUMP_data <- read.csv("C:/Users/chend17/Downloads/DataAnalytics/DataAnalytics2024_DANIEL_CHEN/Labs/GPW3_GRUMP_SummaryInformation_2010.csv")
#View(GRUMP_data)
meanArea <- GRUMP_data$Mean.Extent..sq.km.
meanArea <- as.numeric(meanArea)
mask <- is.na(meanArea)
meanArea <- meanArea[!mask]
meanArea

summary(meanArea)
fivenum(meanArea, na.rm=True)
stem(meanArea)

hist(meanArea)

plot(ecdf(meanArea), do.points=FALSE, verticals=TRUE)

par(pty="s")
qqnorm(meanArea)
qqline(meanArea)


WATER_data <- read.csv("C:/Users/chend17/Downloads/DataAnalytics/DataAnalytics2024_DANIEL_CHEN/Labs/water-treatment.csv")
#View(WATER_data)
QE <- WATER_data$Q.E
QE <- as.numeric(QE)
mask <- is.na(QE)
QE <- QE[!mask]
QE

summary(QE)
fivenum(QE, na.rm=True)
stem(QE)

hist(QE)

plot(ecdf(QE), do.points=FALSE, verticals=TRUE)

par(pty="s")
qqnorm(QE)
qqline(QE)
