library(tidyverse)
library(zoo)

getInput = function(day) {
  read_csv(paste("inputs/day", day, ".txt", sep = ""), col_names = c("input"))
}

day2Data_raw = getInput(2)

day2Data = day2Data_raw %>% separate(input, into = c("command", "value"), sep = " ") %>% 
  mutate(value = as.numeric(value))

summedCommands = day2Data %>%
  group_by(command) %>%
  summarise(
    total = sum(value)
  )

day2Ans1 = (summedCommands$total[summedCommands$command == "down"] - summedCommands$total[summedCommands$command == "up"]) * summedCommands$total[summedCommands$command == "forward"]

day2Q2 = day2Data %>%
  mutate(deltaAim = if_else(command == "down", value, if_else(command == "up", -value, 0))) %>%
  mutate(aim = cumsum(deltaAim)) %>%
  filter(command == "forward") %>%
  mutate(deltaY = value * aim)

day2Q2Ans = sum(day2Q2$value) * sum(day2Q2$deltaY)
