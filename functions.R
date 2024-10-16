# If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
# Finish the solution so that it returns the sum of all the multiples of 3 or 5 below the number passed in. Additionally, if the number is negative, return 0 (for languages that do have them).
# Note: If the number is a multiple of both 3 and 5, only count it once.

is_multiples_3_or_5 <- function(number){
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

# Simple, remove the spaces from the string, then return the resultant string.

no_space <- function(x){
     x <- gsub(" ", "", x)
}

# Given two arrays a and b write a function comp(a, b) (orcompSame(a, b)) 
# that checks whether the two arrays have the "same" elements, with the same 
# multiplicities (the multiplicity of a member is the number of times it appears). 
# "Same" means, here, that the elements in b are the elements in a squared, regardless of the order.

comp <- function(a1, a2) {
     
     res=TRUE
     
     if (sum(a1 < 0)) {
          a1 = sort(abs(a1))
          
     } else {
          a1 = sort(a1)
     }
     a2 = sort(a2)
     
     if (length(a1) == length(a2)) {
          
          for (i in 1:length(a1)) {
               
               if (a1[i]*a1[i] != a2[i] & is.numeric(a1[i]) & is.numeric(a2[i]) & !is.null(a1[i]) & !is.null(a2[i])) {
                    res=FALSE
                    break
               }
               
          }
     } else {
          res=FALSE
     }
    
     res
}


# solution <- function(number){
#     number<-number-1
#     if (number < 0) {
#         
#         return(0)
#         
#     } else {
#         s <- seq(1:number)
#         a <- as.logical(!s%%3)
#         b <- as.logical(!s%%5)
#         result <- sum(s[a|b])
#         return(result)
#     }
# }

array_diff = function(a, b) {
    return(a[!a %in% b])
}
s<- "asdfadsf"
split(s,1:(nchar(s)/2))

substring()

s<-"The quick, brown fox jumps over the lazy dog!"
s<-"The quick, brown fox jumps"

is_pangram <- function(s){
    res <- length(table(strsplit(tolower(gsub("[^a-z]", "", s)), NULL)[[1]]))%%26
    return(!res)
}
is_pangram(s)


data0 <-"Rome:Jan 81.2,Feb 63.2,Mar 70.3,Apr 55.7,May 53.0,Jun 36.4,Jul 17.5,Aug 27.5,Sep 60.9,Oct 117.7,Nov 111.0,Dec 97.9
    London:Jan 48.0,Feb 38.9,Mar 39.9,Apr 42.2,May 47.3,Jun 52.1,Jul 59.5,Aug 57.2,Sep 55.4,Oct 62.0,Nov 59.0,Dec 52.9
    Paris:Jan 182.3,Feb 120.6,Mar 158.1,Apr 204.9,May 323.1,Jun 300.5,Jul 236.8,Aug 192.9,Sep 66.3,Oct 63.3,Nov 83.2,Dec 154.7
    NY:Jan 108.7,Feb 101.8,Mar 131.9,Apr 93.5,May 98.8,Jun 93.6,Jul 102.2,Aug 131.8,Sep 92.0,Oct 82.3,Nov 107.8,Dec 94.2
    Vancouver:Jan 145.7,Feb 121.4,Mar 102.3,Apr 69.2,May 55.8,Jun 47.1,Jul 31.3,Aug 37.0,Sep 59.6,Oct 116.3,Nov 154.6,Dec 171.5
    Sydney:Jan 103.4,Feb 111.0,Mar 131.3,Apr 129.7,May 123.0,Jun 129.2,Jul 102.8,Aug 80.3,Sep 69.3,Oct 82.6,Nov 81.4,Dec 78.2
    Bangkok:Jan 10.6,Feb 28.2,Mar 30.7,Apr 71.8,May 189.4,Jun 151.7,Jul 158.2,Aug 187.0,Sep 319.9,Oct 230.8,Nov 57.3,Dec 9.4
    Tokyo:Jan 49.9,Feb 71.5,Mar 106.4,Apr 129.2,May 144.0,Jun 176.0,Jul 135.6,Aug 148.5,Sep 216.4,Oct 194.1,Nov 95.6,Dec 54.4
    Beijing:Jan 3.9,Feb 4.7,Mar 8.2,Apr 18.4,May 33.0,Jun 78.1,Jul 224.3,Aug 170.0,Sep 58.4,Oct 18.0,Nov 9.3,Dec 2.7
    Lima:Jan 1.2,Feb 0.9,Mar 0.7,Apr 0.4,May 0.6,Jun 1.8,Jul 4.4,Aug 3.1,Sep 3.3,Oct 1.7,Nov 0.5,Dec 0.7"

strng <- data0

avg <- function(town, strng) {
    data_strings <- strsplit(tolower(strng),"\n")
    data_strings <- data.frame(
        All_data_column = data_strings[[1]]
    )
    
    df <- data.frame()
    for (current_string in data_strings$All_data_column) {
        city_and_data <- strsplit(current_string,":")
        city <- trimws(city_and_data[[1]][1])
        city_data <- city_and_data[[1]][2]
        city_data_df <- data.frame(city = city,data = city_data)
        df <- rbind(df,city_data_df)
    }
    
    result_df <- data.frame()
    temp_df <- data.frame()
    for (current_data in df$data) {
        months_values <- strsplit(current_data, ",")[1]
        months <- lapply(months_values[[1]], function(x) unlist(strsplit(x, " "))[1])
        values <- lapply(months_values[[1]], function(x) unlist(strsplit(x, " "))[2])
        
        temp_df <- data.frame(
            City <- df$city[df$data %in% current_data],
            Value <- values)
        names(temp_df) <- c("City",months)
        result_df <- rbind(result_df,temp_df)

    }
    mean(as.numeric(result_df[result_df$City==tolower(town),2:ncol(result_df)]))
}

avg("London",data0)

variance <- function(town, strng) {
    data_strings <- strsplit(tolower(strng),"\n")
    data_strings <- data.frame(
        All_data_column = data_strings[[1]]
    )
    
    df <- data.frame()
    for (current_string in data_strings$All_data_column) {
        city_and_data <- strsplit(current_string,":")
        city <- trimws(city_and_data[[1]][1])
        city_data <- city_and_data[[1]][2]
        city_data_df <- data.frame(city = city,data = city_data)
        df <- rbind(df,city_data_df)
    }
    
    result_df <- data.frame()
    temp_df <- data.frame()
    for (current_data in df$data) {
        months_values <- strsplit(current_data, ",")[1]
        months <- lapply(months_values[[1]], function(x) unlist(strsplit(x, " "))[1])
        values <- lapply(months_values[[1]], function(x) unlist(strsplit(x, " "))[2])
        
        temp_df <- data.frame(
            City <- df$city[df$data %in% current_data],
            Value <- values)
        names(temp_df) <- c("City",months)
        result_df <- rbind(result_df,temp_df)
        
    }
    var(as.numeric(result_df[result_df$City==tolower(town),2:ncol(result_df)])) * (ncol(result_df) - 2) / (ncol(result_df) - 1)
}

avg("London",data0)
variance("London",data0)
