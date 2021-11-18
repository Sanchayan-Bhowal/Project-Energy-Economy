data <- read.csv("data.csv")

epc_rating <- data$epc_rating_a +
    2 * data$epc_rating_b +
    3 * data$epc_rating_c +
    4 * data$epc_rating_d +
    5 * data$epc_rating_e + 6 * data$epc_rating_f + 7 * data$epc_rating_g

windows()
barplot(
    rbind(
        tapply(data$ln_price_1, epc_rating, mean),
        tapply(data$ln_price_2, epc_rating, mean)
    ),
    beside = TRUE, ylim = c(0, 12),
    names.arg = c("B", "C", "D", "E", "F", "G")
)

windows()
par(mfrow = c(1, 2))
boxplot(data$ln_price_1 ~ epc_rating)
boxplot(data$ln_price_2 ~ epc_rating)