data <- read.csv("data.csv")

epc_rating <- data$epc_rating_a +
    2 * data$epc_rating_b +
    3 * data$epc_rating_c +
    4 * data$epc_rating_d +
    5 * data$epc_rating_e + 6 * data$epc_rating_f + 7 * data$epc_rating_g

barplot(
    rbind(
        tapply(data$price_1, epc_rating, mean),
        tapply(data$price_2, epc_rating, mean)
    ),
    beside = TRUE, names.arg = c("B", "C", "D", "E", "F", "G")
)