#clear console and R environment
shell('cls')
#remove from environment
rm(list = ls())

a <- c(1:5)
b <- c(6:10)
sum(a,b)

#5 is greater than 8 OR 6 is not equal to 8 AND
#4 is greater than 3.9
#So below statement is true
5 > 8 || 6 != 8 && 4 > 3.9

#Create list of logicals from 1 to 10
ints <- sample(10)
#for every element in the list, is it larger than 5
ints > 5

#which element in ints is equal to 5
#returns the element's order number to the right
which(ints == 5)

#any elements in ints smaller than 0
any(ints < 0)
#are all the integers above 0
all(ints > 0)

#what class is ints
class(ints)
#how long is ints
length(ints)
#Meant for matrices
dim(ints)

#p used for points
plot(x = 1:10, y = 11:20, type = 'p')
#l used for line
plot(x = 1:10, y = 11:20, type = 'l')

#define some x and y values as functions
#c function tells R to generate a vector
x_val <- c(1:10)
y_val <- c(11:20)

plot(x_val, y_val)

#Libraries
library(magrittr)

#define function x
x <- c(1,4,54,2,5,32,7,21,87,91)

#subtract 1 from all elements
x-1

#define a function for sq rts
my_sqrt <- sqrt(x)

#takes first element of x and divides by first
#element og my_sqrt and so on
#R does this element by element
x/my_sqrt

#If vectors are not the same size can specify
#take a certain number of elements - so take the
#first 5 elements of each
x[1:5]/my_sqrt

#logical vector - make a numerical vector
num_vec <- c(0.5, 55, -10, 6)

#true or false - true if less than 1
tf <- num_vec < 1
tf

#character vectors
char_vec <- c('brain', 'victor')
new_char_vec <- c(char_vec, 'sofie')
#combine the strings with below function
paste(new_char_vec, collapse = ', ')

#Printing elements
#From vector x, take the first 5 elements
#R is 1-indexed so elements start at 1
#Python is 0-indexed so elements start at 0
x[1:5]

#Show the 1st, 2nd, and 4th element
#Access elements - use square brackets
x[c(1,2,4)]
#Want everything except the 1st, 2nd and 4th elements
x[c(-1,-2,-4)]
#Does the same as above if you put - out front
x[-c(1,2,4)]
#Return all elements in list larger than 5
x[x > 5]
#15th element is NA - not assigned
x[15]

#Dictionaries
vect <- c(apple = 12, banana = 2, lime = 6)
vect
vect[1]
vect['apple']

#Lists
z <- list(1, 'a', TRUE, 1+4i)
z

#Matrices
#Bind them to columns
cbind(x,y_val)
#Bind them to rows
rbind(x,y_val)

#Put x elements into 2 rows and 5 columns
#From up to down and then next column
dim(x) <- c(2,5)
#Do 5 rows and 2 columns
dim(x) <- c(5,2)
x

#Make a matrix with 2 rows and 3 columns
m <- matrix(1:6, nrow = 2, ncol = 3)
m
#Grab the element in the 1st row and 2nd column
m[1,2]
#Grab the elements in the 3rd column
m[,3]
#Grab the elements in the 2nd row
m[2,]
#Go 3 places forward and grab that element
m[3]

#Dataframes
#Make a dataframe
d <- data.frame(col1 = 1:4, col2 = c('T', 'T', 'T', 'F'))
d

y <- c(1:10)
x <- c(2:11)

data.frame(x,y)

cnames <- c('A', 'B')

colnames(d) <- cnames
#Grab B column - call it by name
d[,'B']

#Factors
#Not strings, does not have quotations
#Will treat no as a 1 and yes as a 2
#Could do a regression on a categorical value
f <- factor(c('yes','yes','no','yes','no'))
f
#tells you how many no's and yes's there are
table(f)

n <- c(1, 2, 1, 1, 2, 2, 1)
#Give it data and tell it 1 is Yes and 2 is No
#As many labels as unique values
#All will be unique values
fac <- factor(n, labels = c('Yes', 'No'))
fac

#Missing values
y <- c(1, NA, 3, 2, NA, -1)
is.na(y)
#Negate missing values in dataset
y[!is.na(y)]

#NA and NaN are different in R
is.nan(y)

