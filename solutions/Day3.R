library(tidyverse)

##FUNCTIONS--------
getInput = function(day) {
  read_csv(paste("inputs/day", day, ".txt", sep = ""), col_names = c("input"))
}

calculateGammaBits = function(bitsum, rowcount) {
  gammaBit = if_else(bitsum > 0.5 * rowcount, 1, 0)
}

calculateEpsilonBits = function(bitsum, rowcount) {
  gammaBit = if_else(bitsum > 0.5 * rowcount, 0, 1)
}

##READ DATA-------------
day3Data = getInput(3)


##Q1 WORK----------------
d3q1 = day3Data %>%
  separate(input, into = c("badCol", LETTERS[1:12]), sep = "") %>%
  select(-badCol) %>%
  mutate(across(everything(), .fns = as.numeric)) %>%
  summarise(across(everything(), .fns = sum))

d3q1_gamma = d3q1 %>%
  mutate(across(everything(), ~calculateGammaBits(., nrow(day3Data)))) %>%
  unite("binaryGamma", 1:12, sep = "")

d3q1_gamma_int = strtoi(d3q1_gamma$binaryGamma[1], 2)

d3q1_eps = d3q1 %>%
  mutate(across(everything(), ~calculateEpsilonBits(., nrow(day3Data)))) %>%
  unite("binaryEps", 1:12, sep = "")

d3q1_eps_int = strtoi(d3q1_eps$binaryEps[1], 2)

d3a1 = d3q1_eps_int * d3q1_gamma_int


##Q2 WORK--------------------
d3q2 = day3Data %>%
  separate(input, into = c("badCol", LETTERS[1:12]), sep = "") %>%
  select(-badCol) %>%
  mutate(across(everything(), .fns = as.numeric)) 

o2Codes = d3q2
for (col in LETTERS[1:12]) {
  O2BIT = if_else(sum(o2Codes[[col]]) == 0.5 * nrow(o2Codes), 1, calculateGammaBits(sum(o2Codes[[col]]), nrow(o2Codes)))
  o2Codes = o2Codes %>% filter(o2Codes[[col]] == O2BIT)
  
  if (nrow(o2Codes) == 1) {
    break
  }
}

d3q2_o2 = o2Codes %>%
  unite("binaryo2", 1:12, sep = "")

d3q2_o2_int = strtoi(d3q2_o2$binaryo2[1], 2)

co2Codes = d3q2
for (col in LETTERS[1:12]) {
  CO2BIT = if_else(sum(co2Codes[[col]]) == 0.5 * nrow(co2Codes), 0, calculateEpsilonBits(sum(co2Codes[[col]]), nrow(co2Codes)))
  co2Codes = co2Codes %>% filter(co2Codes[[col]] == CO2BIT)
  
  if (nrow(co2Codes) == 1) {
    break
  }
}

d3q2_co2 = co2Codes %>%
  unite("binaryco2", 1:12, sep = "")

d3q2_co2_int = strtoi(d3q2_co2$binaryco2[1], 2)

d3a2 = d3q2_o2_int * d3q2_co2_int
