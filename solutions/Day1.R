library(tidyverse)
library(zoo)

getInput = function(day) {
  read_csv(paste("inputs/day", day, ".txt", sep = ""), col_names = c("input"))
}

day1Data = getInput(1)

day1Data = day1Data %>%
  mutate(deltaPlus = input > lag(input, n = 1, default = NA))

day1Ans1 = sum(day1Data$deltaPlus, na.rm = TRUE)

day1Data = day1Data %>%
  mutate(slidingSum = rollsum(input, k=3, na.pad = TRUE, align = "right")) %>%
  mutate(slidingDeltaPlus = slidingSum > lag(slidingSum, n = 1, default = NA))

day1Ans2 = sum(day1Data$slidingDeltaPlus, na.rm = TRUE)
