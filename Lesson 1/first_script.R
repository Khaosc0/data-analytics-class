#clear console and R environment
shell('cls')
#remove from environment
rm(list = ls())

#create a variable x, 1 through 5
x <- c(1:5)

#create a variable y, 6 through 10
y <- c(6:10)

#Add up all elements in x and y
sum(x,y)
