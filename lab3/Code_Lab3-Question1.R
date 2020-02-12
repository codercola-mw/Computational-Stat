# 1.

## 1-1. Importing necessary data
pop_data <- readxl::read_xls("~/Computational Stat/732A90_VT2020_Materials/population.xls",
          range="A6:C320") #Choosing only population
pop_data <- pop_data[-(1:3),] #deleting empty space
pop_data <- pop_data[(pop_data$Code>26),] #deleting counties
n <- dim(pop_data)[1]

## 1-2. Uniform number generator to select a city
city_selection <- function(data){
  U <- runif(1)
  cummulative <- 0
  prob <- data$Population/sum(data$Population)
  # Generating discrete RVs(lecture note)
  for (i in (1:n)){
    cummulative <- cummulative+prob[i]
    if ( cummulative >= U ) {
      return(i)
      } #returning the row index
  }
}

## 1-3. Selecting 20 cities
selections <- data.frame(City = as.character(), Population= as.numeric(), stringsAsFactors = FALSE)
data_use <- pop_data
for (j in 1:20){
  i <- city_selection(data=data_use)
  selections <- rbind(selections, data_use[i,c(2,3)]) #code not needed
  data_use = data_use[-i,]
}
names(selections) <- c("City","Population")

## 1-4. Selected 20 cities
cat("List of 20 selected cities are: ", "\n")
selections[,1]
   ## Majority of the populations in each city exceeds 20,000
   ## Several of them has a population that exceeds 100,000
   ## Selected cities are large cities based on their population

## 1-5. Histograms
par(mfrow=c(1,1))
hist(x=selections$Population, breaks = 40, 
     main="Population of the Selected Cities", xlab="Population")
hist(x=pop_data$Population, breaks=40,
     main="Population of All Cities", xlab="Population")

  ## The first histogram which shows the population of the
  ## selected cities shows that majority of the population
  ## are less than 200,000 and there are only few cities that
  ## have population more than 200,000.

  ## The second histogram which shows the populationof all
  ## cities in sweden more clearly shows that most of the
  ## cities in Sweden have population less than 200,000 
  ## (Furthermore, among these cities, majority of them have
  ## population less than 20,000), and very few cities have 
  ## population more than that.

  ## To conclude, selection of cities using random number
  ## generation worked quite well in producting an opinion
  ## pool. The selection is based on the city population;
  ## the cities with large population that exceeds 20,000 are
  ## mostly chosen.