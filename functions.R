# If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
# Finish the solution so that it returns the sum of all the multiples of 3 or 5 below the number passed in. Additionally, if the number is negative, return 0 (for languages that do have them).
# Note: If the number is a multiple of both 3 and 5, only count it once.

solution <- function(number){
     if (number <= 0) return(0)
     x = seq(1,number-1)
     return(sum(x[x%%3==0 | x%%5==0]))
}

# Simple, given a string of words, return the length of the shortest word(s).
# String will never be empty and you do not need to account for different data types.

find_short <- function(s){
     min(nchar(unlist(strsplit(s, " "))))
}


# Given a month as an integer from 1 to 12, return to which quarter of the year it belongs as an integer number.
# For example: month 2 (February), is part of the first quarter; month 6 (June), is part of the second quarter; 
# and month 11 (November), is part of the fourth quarter.

quarter_of <- function(month) {
     
     if (month %% 3 == 0) {
          
          floor(month / 3)
          
     } else {
          
          floor((month / 3) + 1)
          
     } 
}

# You live in the city of Cartesia where all roads are laid out in a perfect grid. 
# You arrived ten minutes too early to an appointment, so you decided to take the opportunity to go for a short walk. 
# The city provides its citizens with a Walk Generating App on their phones -- everytime you press the button it sends 
# you an array of one-letter strings representing directions to walk (eg. ['n', 's', 'w', 'e']). You always walk only 
# a single block for each letter (direction) and you know it takes you one minute to traverse one city block, so create 
# a function that will return true if the walk the app gives you will take you exactly ten minutes 
# (you don't want to be early or late!) and will, of course, return you to your starting point. Return false otherwise.

isValidWalk <- function(walk){
     
     if (length(walk)!=10) {
          return(FALSE)
     }
     x <- 0
     y <- 0
     for (i in 1:10) {
          
          if (walk[i] == "n") {
               x = x + 1
          } 
          if (walk[i] == "s") {
               x = x - 1 
          }
          if (walk[i] == "e") {
               y = y + 1
          } 
          if (walk[i] == "w") {
               y = y - 1
          }
          
     }
     if (x==0 & y==0) {
          return(TRUE)
     }
     
     return(FALSE)
     
}


# Write a function which calculates the average of the numbers in a given list.
# Note: Empty arrays should return 0.

find_average <- function(vec){
     ifelse(length(vec) == 0, 0, mean(vec))
}