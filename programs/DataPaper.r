# Reproduction of page 7,8 in data paper
data <- read.csv("data/Data.csv")
library(ggplot2)
library(timeDate)
library(DescTools)
library(gridExtra)
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
nltp <- ggplot(data = data, aes(ln_price_1)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
    binwidth = 0.18, col = border, fill = fill
  ) +
  ylab("Fraction") +
  ggtitle("Natural logarithm of first transaction price")

nlsp <- ggplot(data = data, aes(ln_price_2)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
    binwidth = 0.18, col = border, fill = fill
  ) +
  ylab("Fraction") +
  ggtitle("Natural logarithm of second transaction price")

grid.arrange(nltp, nlsp, nrow = 1, ncol = 2)

windows()
ppt <- ggplot(data = data, aes(perc_change_p2_to_p1)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
    binwidth = 0.3, col = border, fill = fill
  ) +
  ylab("Fraction") +
  ggtitle("Percentual price change between transactions")

ptt <- ggplot(data = data, aes(days_between_sale)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
    bins = 35, col = border, fill = fill
  ) +
  ylab("Fraction") +
  ggtitle("Period of time between both transactions")
grid.arrange(ppt, ptt, nrow = 1, ncol = 2)

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
epchist <- ggplot(data = data, aes(epc_100)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
    bins = 36, col = border, fill = fill
  ) +
  ylab("Fraction") +
  ggtitle("EPC standard assessment proceducre(SAP) points")

epcbox <- ggplot(data = data, aes("", epc_100)) +
  geom_boxplot(col = border, fill = fill) +
  xlab(NULL) +
  ggtitle("EPC standard assessment proceducre(SAP) points")

grid.arrange(epchist, epcbox, nrow = 1, ncol = 2)

windows()

imdr <- ggplot(data = data, aes(imd_score)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
    bins = 30, col = border, fill = fill
  ) +
  ylab("Fraction") +
  ggtitle("Index of Multiple deprivation(IMD) rank")

imdd <- ggplot(data = data, aes(imd_level)) +
  geom_bar(aes(y = ..count.. / sum(..count..)), col = border, fill = fill) +
  ylab("Fraction") +
  ggtitle("Index of Multiple deprivation(IMD) decile")

bhsr <- ggplot(data = data, aes(barrier_score)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
    bins = 30, col = border, fill = fill
  ) +
  ylab("Fraction") +
  ggtitle("Barriers to housing and service rank")

bhsd <- ggplot(data = data, aes(barrier_level)) +
  geom_bar(aes(y = ..count.. / sum(..count..)), col = border, fill = fill) +
  ylab("Fraction") +
  ggtitle("Barriers to housing and service decile")

cr <- ggplot(data = data, aes(crime_score)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
    bins = 35, col = border, fill = fill
  ) +
  ylab("Fraction") +
  ggtitle("Crime rank")

cd <- ggplot(data = data, aes(crime_level)) +
  geom_bar(aes(y = ..count.. / sum(..count..)), col = border, fill = fill) +
  ylab("Fraction") +
  ggtitle("Crime decile")

grid.arrange(imdr, imdd, bhsr, bhsd, cr, cd, nrow = 3, ncol = 2)

windows()

estdr <- ggplot(data = data, aes(educ_score)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
    bins = 35, col = border, fill = fill
  ) +
  ylab("Fraction") +
  ggtitle("Education skills and training deprivation rank")

estdd <- ggplot(data = data, aes(educ_level)) +
  geom_bar(aes(y = ..count.. / sum(..count..)), col = border, fill = fill) +
  ylab("Fraction") +
  ggtitle("Education skills and training deprivation decile")

edr <- ggplot(data = data, aes(emp_score)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
    bins = 35, col = border, fill = fill
  ) +
  ylab("Fraction") +
  ggtitle("Employment deprivation rank")

edd <- ggplot(data = data, aes(emp_level)) +
  geom_bar(aes(y = ..count.. / sum(..count..)), col = border, fill = fill) +
  ylab("Fraction") +
  ggtitle("Employment deprivation decile")

hddr <- ggplot(data = data, aes(health_score)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
    bins = 35, col = border, fill = fill
  ) +
  ylab("Fraction") +
  ggtitle("Health deprivation and disability rank")

hddd <- ggplot(data = data, aes(health_level)) +
  geom_bar(aes(y = ..count.. / sum(..count..)), col = border, fill = fill) +
  ylab("Fraction") +
  ggtitle("Health deprivation and disability level")

grid.arrange(estdr, estdd, edr, edd, hddr, hddd, nrow = 3, ncol = 2)

windows()

idr <- ggplot(data = data, aes(income_score)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
    bins = 35, col = border, fill = fill
  ) +
  ylab("Fraction") +
  ggtitle("Income deprivation rank")

idd <- ggplot(data = data, aes(income_level)) +
  geom_bar(aes(y = ..count.. / sum(..count..)), col = border, fill = fill) +
  ylab("Fraction") +
  ggtitle("Income deprivation decile")


ledr <- ggplot(data = data, aes(living_score)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)),
    bins = 35, col = border, fill = fill
  ) +
  ylab("Fraction") +
  ggtitle("Living environment deprivation rank")

ledd <- ggplot(data = data, aes(living_level)) +
  geom_bar(aes(y = ..count.. / sum(..count..)), col = border, fill = fill) +
  ylab("Fraction") +
  ggtitle("Living environment deprivation decile")

grid.arrange(idr, idd, ledr, ledd, nrow = 2, ncol = 2)

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