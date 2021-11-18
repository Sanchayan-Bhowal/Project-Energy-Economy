data <- read.csv("data.csv")

epc_price_1 <- c(0)
epc_price_1[1] <- mean(data$price_1[data$epc_rating_b == 1])
epc_price_1[2] <- mean(data$price_1[data$epc_rating_c == 1])
epc_price_1[3] <- mean(data$price_1[data$epc_rating_d == 1])
epc_price_1[4] <- mean(data$price_1[data$epc_rating_e == 1])
epc_price_1[5] <- mean(data$price_1[data$epc_rating_f == 1])
epc_price_1[6] <- mean(data$price_1[data$epc_rating_g == 1])
epc_price_2 <- c(0)
epc_price_2[1] <- mean(data$price_2[data$epc_rating_b == 1])
epc_price_2[2] <- mean(data$price_2[data$epc_rating_c == 1])
epc_price_2[3] <- mean(data$price_2[data$epc_rating_d == 1])
epc_price_2[4] <- mean(data$price_2[data$epc_rating_e == 1])
epc_price_2[5] <- mean(data$price_2[data$epc_rating_f == 1])
epc_price_2[6] <- mean(data$price_2[data$epc_rating_g == 1])

barplot(
    rbind(epc_price_1, epc_price_2),
    beside = TRUE, names.arg = c("B", "C", "D", "E", "F", "G")
)