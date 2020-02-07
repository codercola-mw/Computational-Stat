# question 1
RNGversion("3.5.1")

data=readxl::read_xls("~/Desktop/732A90_VT2020_Materials/population.xls")

data = data[-(1:8),]
data$`Statistics Sweden`= as.integer(data$`Statistics Sweden`)
data=data[-which(data$`Statistics Sweden`<100),]
data$Population=as.integer(data$...3)
data$city=data$...2

select_city <- function(data){
  rand_num <- runif(1, min=0, max=sum(data$Population))
  pop=0
  city_pop = cbind(City=data$city, Population=data$Population)
  
  for(i in 1:nrow(data)){
    pop = data$Population[i]+pop
    if (pop >= rand_num){return (city_pop[i,])}
  }
}
### 2
city_pop
cities=data.frame(City=vector(length=20), Population=vector(length=20))

for(i in 1:20){
  cities[i,] <- select_city(data)
}

new_city_pop = city_pop[-which(city_pop[,1] %in% cities[,1]),]
cities
new_city_pop

### 5

hist(data$Population,main="Population of all Cities",xlab="Cities",
     border="black", col="pink",breaks=30, ylim=c(0,200))
hist(cities[,2],main="Population of all 20 selected cities",xlab="Cities",
     border="black", col="pink",breaks=30)
