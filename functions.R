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

