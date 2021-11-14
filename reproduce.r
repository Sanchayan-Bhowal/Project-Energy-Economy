#Reproduction of page 7,8 in data paper
data <- read.csv("data.csv")
library(timeDate)
describe <- function(variable) {
    z <- data.frame(
        Mean = mean(variable),
        Median = median(variable),
        Std = sd(variable),
        Skewness = skewness(variable),
        Kurtosis = kurtosis(variable),
        Smallest = min(variable),
        Largest = max(variable),
        Obs = length(variable)
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

#We can add Normality check for the data set. qq plots.

windows()
par(mfrow = c(2, 2))
hist(data$ln_price_1, freq = F, breaks = 30, xlim = range(c(8, 16)),main="Natural logarithm of first transaction price")
hist(data$ln_price_2, freq = F, breaks = 30, xlim = range(c(10, 16)),main="Natural logarithm of second transaction price")
hist(data$perc_change_p2_to_p1, freq = F, breaks = 30,main="Percentual price change between transactions")
hist(data$days_between_sale, freq = F, breaks = 35,main="Period of time between both transactions")

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
hist(data$epc_100, freq = F,main="EPC standard assessment proceducre(SAP) points")
boxplot(data$epc_100)