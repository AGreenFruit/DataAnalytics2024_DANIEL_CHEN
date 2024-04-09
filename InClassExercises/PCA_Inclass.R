data("iris")
head(iris)
irisdata1 <- iris[,1:4]
irisdata1
head(irisdata1)

principal_components <- princomp(irisdata1, cor = TRUE, score = TRUE)
summary(principal_components)

plot(principal_components)
plot(principal_components, type = "l")

biplot(principal_components)


install.packages('MASS')
data(Boston, package="MASS")
pca_out <- prcomp(Boston,scale. = T)

pca_out
plot(pca_out)

biplot(pca_out, scale = 0)
boston_pc <- pca_out$x
boston_pc

head(boston_pc)
summary(boston_pc)


data("USArrests")
states=row.names(USArrests)
states

names(USArrests )
apply(USArrests , 2, mean)
apply(USArrests , 2, var)

pr.out=prcomp(USArrests, scale=TRUE)
names(pr.out)

pr.out$center
pr.out$scale
pr.out$rotation

dim(pr.out$x)
biplot(pr.out, scale=0)
pr.out$sdev

pr.var = pr.out$sdev^2
pr.var

pve = pr.var/sum(pr.var)
pve

# wine_data <- read.table("http://archive.ics.uci.edu/dataset/109/wine/wine.data", sep = ",")
wine_data <- read.csv("C:/Users/danie/Documents/DataAnalytics2024_DANIEL_CHEN/InClassExercises/wine.data")
head(wine_data)
nrow(wine_data)
colnames(wine_data) <- c("Cvs", "Alcohol",
                         "Malic_Acid", "Ash", "Alkalinity_of_Ash",
                         "Magnesium", "Total_Phenols", "Flavanoids", "NonFlavanoid_Phenols",
                         "Proanthocyanins", "Color_Intensity", "Hue", "OD280/OD315_of_Diluted_Wine",
                         "Proline")
head(wine_data)
heatmap(cor(wine_data),Rowv = NA, Colv = NA)
heatmap(cor(wine_data),Colv=NA, Rowv=NA, scale="none")

principal_components <- princomp(wine_data, cor = TRUE, score = TRUE)
summary(principal_components)

cultivar_classes <- factor(wine_data$Cvs)
cultivar_classes
wine_data_PCA <- prcomp(scale(wine_data[,-1]))
summary(wine_data_PCA)
