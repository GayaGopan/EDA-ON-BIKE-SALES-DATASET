#Installing the necessary packages.

install.packages("dplyr")
install.packages("ggplot2")
install.packages("Data Explorer")
install.packages("vtree")
install.packages("corrplot")

#Importing the data set into R environment.


bike_data<-read.csv('C:/Users/Gaya Gopan/Documents/R program/bike_buyers.csv')
View(bike_data)

class(bike_data)
str(bike_data)

View(head(bike_data,5))
View(tail(bike_data,5))
print(summary(bike_data))

#Renaming the variables as per the Naming conversion.

names(bike_data)<-gsub("\\.","_",names(bike_data))
print(bike_data)
View(bike_data)

colSums(is.na(bike_data))

hist(bike_data$Income)

hist(bike_data$Children, breaks = 20)

hist(bike_data$Cars, breaks = 15)

hist(bike_data$Age)

#Missing value treatment.

median(na.omit((bike_data$Income)))
median(na.omit((bike_data$Age)))

bike_data_clean <- bike_data
colSums(is.na(bike_data_clean))

#Income and Age replaced with Median.

bike_data_clean$Income[is.na(bike_data_clean$Income)] <- median(na.omit((bike_data$Income)))

bike_data_clean$Age[is.na(bike_data_clean$Age)] <- median(na.omit((bike_data$Age)))

colSums(is.na(bike_data_clean))

#Replacing the values with Mode.

get_mode <- function(x) {                 
  unique_x <- unique(x)
  tabulate_x <- tabulate(match(x, unique_x))
  unique_x[tabulate_x == max(tabulate_x)]
}

# Marital Status replaced with Mode

bike_data_clean$Marital.Status[is.na(bike_data_clean$Marital.Status)] <- 
  get_mode(bike_data$Marital.Status)


# Gender replaced with Mode
bike_data_clean$Gender[is.na(bike_data_clean$Gender)] <- 
  get_mode(bike_data$Gender)

# Children replaced with Mode
bike_data_clean$Children[is.na(bike_data_clean$Children)] <- 
  get_mode(bike_data$Children)

# Home Owner replaced with Mode
bike_data_clean$Home.Owner[is.na(bike_data_clean$Home.Owner)] <- 
  get_mode(bike_data$Home.Owner)

colSums(is.na(bike_data_clean))

# Cars replaced with Mean
bike_data_clean$Cars[is.na(bike_data_clean$Cars)] <- 
  mean(bike_data$Cars, na.rm = TRUE)

colSums(is.na(bike_data_clean))


# Outlier Treatment.

boxplot(bike_data$Income, main = 'Income Boxplot')
boxplot(bike_data[,c(1,4)], main='Multiple Box plots')

OutValues = boxplot(bike_data$Income)$out
print(OutValues)

which(bike_data$Income %in% OutValues)

x = bike_data$Income [!(bike_data$Income %in% OutValues) ]
boxplot(x)

#Scatter Plot

plot(bike_data$Income, type= "p")










