# Reproduction of page 7,8 in data paper
data <- read.csv("Data.csv")
library(ggplot2)
library(timeDate)
library(DescTools)
describe <- function(variable) {
  z <- data.frame(
    Mean = mean(variable),
    Median = median(variable),
    Std = sd(variable),
    Skewness = skewness(variable),
    Kurtosis = kurtosis(variable),
    Smallest = min(variable),
    Largest = max(variable),
    Obs = length(variable),
    Normality = as.numeric(ShapiroFranciaTest(variable)[2])
  )
  return(z)
}

result <- rbind(
  describe(data$price_1),
  describe(data$price_2),
  describe(data$ln_price_1),
  describe(data$ln_price_2),
  describe(data$perc_change_p2_to_p1),
  describe(data$days_between_sale)
)

print(result)

border <- "#092f4e"
fill <- "#204c71"

windows()
par(mfrow = c(2, 2))
ggplot(data = data, aes(ln_price_1)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
    binwidth = 0.18, col = border, fill = fill
  ) +
  ylab("Fraction") +
  ggtitle("Natural logarithm of first transaction price")

ggplot(data = data, aes(ln_price_2)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
    binwidth = 0.18, col = border, fill = fill
  ) +
  ylab("Fraction") +
  ggtitle("Natural logarithm of second transaction price")

ggplot(data = data, aes(perc_change_p2_to_p1)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
    binwidth = 0.3, col = border, fill = fill
  ) +
  ylab("Fraction") +
  ggtitle("Percentual price change between transactions")

ggplot(data = data, aes(days_between_sale)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
    bins = 35, col = border, fill = fill
  ) +
  ylab("Fraction") +
  ggtitle("Period of time between both transactions")


epc <- function(x) {
  f <- as.numeric(table(x)[2])
  if (is.na(f)) {
    f <- 0
  }
  z <- data.frame(
    Frequency = f,
    Fraction = f / 4201
  )
  return(z)
}

epc_band <- rbind(
  epc(data$epc_rating_a),
  epc(data$epc_rating_b),
  epc(data$epc_rating_c),
  epc(data$epc_rating_d),
  epc(data$epc_rating_e),
  epc(data$epc_rating_f),
  epc(data$epc_rating_g)
)

print(epc_band)

windows()
par(mfrow = c(1, 2))
ggplot(data = data, aes(epc_100)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
    bins = 36, col = border, fill = fill
  ) +
  ylab("Fraction") +
  ggtitle("EPC standard assessment proceducre(SAP) points")


ggplot(data = data, aes("", epc_100)) +
  geom_boxplot(col = border, fill = fill) +
  xlab(NULL) +
  ggtitle("EPC standard assessment proceducre(SAP) points")

windows()
par(mfrow = c(3, 2))

ggplot(data = data, aes(imd_score)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
    bins = 30, col = border, fill = fill
  ) +
  ylab("Fraction") +
  ggtitle("Index of Multiple deprivation(IMD) rank")

ggplot(data = data, aes(imd_level)) +
  geom_bar(aes(y = ..count.. / sum(..count..)), col = border, fill = fill) +
  ylab("Fraction") +
  ggtitle("Index of Multiple deprivation(IMD) decile")

ggplot(data = data, aes(barrier_score)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
    bins = 30, col = border, fill = fill
  ) +
  ylab("Fraction") +
  ggtitle("Barriers to housing and service rank")

ggplot(data = data, aes(barrier_level)) +
  geom_bar(aes(y = ..count.. / sum(..count..)), col = border, fill = fill) +
  ylab("Fraction") +
  ggtitle("Barriers to housing and service decile")

ggplot(data = data, aes(crime_score)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
    bins = 35, col = border, fill = fill
  ) +
  ylab("Fraction") +
  ggtitle("Crime rank")

ggplot(data = data, aes(crime_level)) +
  geom_bar(aes(y = ..count.. / sum(..count..)), col = border, fill = fill) +
  ylab("Fraction") +
  ggtitle("Crime decile")

windows()
par(mfrow = c(3, 2))

ggplot(data = data, aes(educ_score)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
    bins = 35, col = border, fill = fill
  ) +
  ylab("Fraction") +
  ggtitle("Education skills and training deprivation rank")

ggplot(data = data, aes(educ_level)) +
  geom_bar(aes(y = ..count.. / sum(..count..)), col = border, fill = fill) +
  ylab("Fraction") +
  ggtitle("Education skills and training deprivation decile")

ggplot(data = data, aes(emp_score)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
    bins = 35, col = border, fill = fill
  ) +
  ylab("Fraction") +
  ggtitle("Employment deprivation rank")

ggplot(data = data, aes(emp_level)) +
  geom_bar(aes(y = ..count.. / sum(..count..)), col = border, fill = fill) +
  ylab("Fraction") +
  ggtitle("Employment deprivation decile")


ggplot(data = data, aes(health_score)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
    bins = 35, col = border, fill = fill
  ) +
  ylab("Fraction") +
  ggtitle("Health deprivation and disability rank")

ggplot(data = data, aes(health_level)) +
  geom_bar(aes(y = ..count.. / sum(..count..)), col = border, fill = fill) +
  ylab("Fraction") +
  ggtitle("Health deprivation and disability level")


windows()
par(mfrow = c(2, 2))

ggplot(data = data, aes(income_score)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
    bins = 35, col = border, fill = fill
  ) +
  ylab("Fraction") +
  ggtitle("Income deprivation rank")

ggplot(data = data, aes(income_level)) +
  geom_bar(aes(y = ..count.. / sum(..count..)), col = border, fill = fill) +
  ylab("Fraction") +
  ggtitle("Income deprivation decile")


ggplot(data = data, aes(living_score)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
    bins = 35, col = border, fill = fill
  ) +
  ylab("Fraction") +
  ggtitle("Living environment deprivation rank")

ggplot(data = data, aes(living_level)) +
  geom_bar(aes(y = ..count.. / sum(..count..)), col = border, fill = fill) +
  ylab("Fraction") +
  ggtitle("Living environment deprivation decile")


geo <- function(x) {
  f <- as.numeric(table(x)[2])
  if (is.na(f)) {
    f <- 0
  }
  z <- data.frame(
    Transaction_Frequency = f,
    Transaction_Fraction = f / 4201
  )
  return(z)
}
geo_band <- rbind(
  geo(data$reg_north_west),
  geo(data$reg_yorkshire_and_the_humber),
  geo(data$reg_east_midlands),
  geo(data$reg_west_midlands),
  geo(data$reg_east_of_england),
  geo(data$reg_london),
  geo(data$reg_south_east),
  geo(data$reg_south_west),
  geo(data$reg_north_east)
)

print(geo_band)
# what is population fraction column in table 4 pg 10??