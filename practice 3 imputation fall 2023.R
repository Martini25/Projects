
data <- read.csv("raw_data2.csv")


### Question 1:	Perform a quick check on variable y to determine whether it contains any missing values. If missing values are found, perform the missing imputation by using the unconditional mean of y. Report the mean/median/standard deviation of imputed x.

data_imputed_y <- with(data,
                       ifelse(is.na(y),
                              mean(y, na.rm=TRUE), y))

# Create a function to perform missing imputation by unconditional mean
imp_uncond_mean <- function(input_var) {
  ifelse(is.na(input_var), 
         mean(input_var, na.rm = TRUE),
         input_var)
  n_miss <- sum(ifelse(is.na(input_var), 1,0))
  cat("Number of missing values in the original vector",
      n_miss)
  return(input_var)  
}

data$imputed_y <- imp_uncond_mean(data$y)
with(data, imp_uncond_mean(y))


#### Question 2: Repeat the previous missing imputation by using the mean of y conditional on z. Report the mean/median/standard deviation of imputed x.
summary(data)
library(dplyr)
# Conditional mean
data %>%
  group_by(z)  %>%
  summarise(mean(y, na.rm = TRUE))

# add conditional means as an additional variable in data
data <- data %>%
  group_by(z) %>%
  mutate(cond_means_y_by_z = mean(y, na.rm = TRUE))
data$imputed_y_by_condmean <- with(data,
                                   ifelse(is.na(y),
                                          cond_means_y_by_z,
                                          y))
sd(data$imputed_y_by_condmean)

# create a function to perform imputation by conditional means
imp_cond_mean <- function(input_var, cond_var) {
  require(dplyr)
  # combine input_var and cond_var into a single data
  single_data <- data.frame(
    x=input_var,
    y=cond_var
  )
  # perform imputation
  single_data <- single_data %>%
    group_by(y) %>%
    mutate(x_mean=mean(x, na.rm = TRUE), 
           imp_x = ifelse(is.na(x), x_mean,x))
  # save the desired output 
  as.numeric(single_data$imp_x)
  return(as.numeric(single_data$imp_x))
}

data$imputed_y_by_condmean_v2 <- with(data, imp_cond_mean(input_var = y, cond_var = z))
