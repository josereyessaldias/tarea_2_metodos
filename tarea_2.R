library(ggplot2)
library(readr)
library(dplyr)
library(ggmosaic)
install.packages("factoextra")
install.packages("ggfortify")
install.packages("NbClust")
install.packages("igraph")
install.packages("cluster")
library(factoextra)
library(ggfortify)
library(NbClust)
library(igraph)
library(cluster)

df <- read_csv('orders.csv')
ggplot(df) + 
  geom_mosaic(aes(x=product(State,Segment),fill=State)) +
  labs(title = "Gráfico de Mosaico", subtitle = "Segment por State") +
  theme(text=element_text(size=8)) +
  guides(fill = guide_legend(reverse = TRUE))


df2 <- df %>% filter(Sales >= 400 & Sales <= 4000)
graph <- ggplot(df2, aes(x=Sales,y=Profit)) + geom_point(alpha=0.5) +
  labs(title="Scatterplot entre Sales y Profit",subtitle = "Sales entre US$400 y US$4000")
graph 
graph + geom_smooth(method="lm", formula='y ~ x',se=F, aes(colour="Lineal")) +
  geom_smooth(method="lm", formula='y ~ poly(x,2)',se=F,aes(colour="Polinomial grado 2")) + 
  geom_smooth(method="lm", formula='y ~ poly(x,3)', se=F, aes(colour="Polinomial grado 3")) +
  scale_colour_manual(name="Leyenda", values=c("green", "purple","red"))

# span <- 0.1
# n.pts <- ceiling(nrow(df2)*span)
# prediction <- rep(NA,nrow(df2))
# 
# for (i in 1:nrow(df2)) {
#   aux <- data.frame(pos=1:nrow(df2),dist=abs(df2$Sales[i]-df2$Sales))
#   pos <- aux[order(aux$dist),]$pos[1:n.pts]
#   dist <- aux[order(aux$dist),]$dist[1:n.pts]
#   scl.dist <- dist/max(dist)
#   w <- (1-abs(scl.dist)^3)^3
#   x <- df2$Sales[pos]
#   y <- df2$Profit[pos]
#   model <- lm(y ~ x, weights=w)
#   prediction[i] <- predict(model,newdata=data.frame(x=df2$Sales[i]))
# }

#graph + geom_line(aes(y=prediction),col="black",cex=1)

graph + geom_smooth(method="loess",formula=y~x,span=0.1,method.args=list(degree=0),se=F, aes(colour="LOESS")) +
  scale_colour_manual(name="Leyenda", values=c("orange"))
  
graph + geom_smooth(method="lm", color="green", formula='y ~ x',se=F) +
  geom_smooth(method="lm", color="blue", formula='y ~ poly(x,2)',se=F) + 
  geom_smooth(method="lm", color="purple", formula='y ~ poly(x,3)',se=F) +
  geom_smooth(method="loess",formula=y~x,span=0.1,method.args=list(degree=0),se=F) +
  scale_colour_manual(name="Leyenda", values=c("oprange"))


df3 <- df %>% filter(State == "Florida")

res <- df3 %>% group_by(City) %>%
  summarise(Mean_Sales = mean(Sales),Mean_Profits=mean(Profit),Mean_Discount=mean(Discount), n = sum(Sales))

View(res)

st.res <- as.data.frame(scale(res[,-1]))
summary(st.res)

pca <- prcomp(st.res)
summary(pca)
pca
#autoplot(pca)


#k.nbClust <- NbClust(data=st.res,distance="euclidean",max.nc=4,method="kmeans",index = "alllong")
#fviz_nbclust(k.nbClust) + theme_minimal()

set.seed(2121);km2 <- kmeans(st.res,centers=2,nstart=100)

autoplot(km2,data=st.res) + labs(title="PCA agrupados con k-means con k = 2")

var <- get_pca(pca)
var$coord[1,2]

st.res2 <- st.res %>% mutate(var1 = Mean_Sales*var$coord[1,1]+
                               Mean_Profits*var$coord[2,1]+
                               Mean_Discount*var$coord[3,1]+
                               n*var$coord[4,1],
                             var2 = Mean_Sales*var$coord[1,2]+
                               Mean_Profits*var$coord[2,2]+
                               Mean_Discount*var$coord[3,2]+
                               n*var$coord[4,2])

ggplot(st.res2, aes(x=var1,y=var2,colour = var1 >1)) + geom_point(alpha=0.5) +
  labs(x="PC1",Y="PC2")


