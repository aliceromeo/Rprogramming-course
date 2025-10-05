###############################################
####       BOOLEAN OPERATORS               ####
###############################################

#############################
## Boolean logic with numbers

# Assign boolean variables using comparison operators
x <- 3 > 2  # x is TRUE because 3 is greater than 2
y <- 2 > 3  # y is FALSE because 2 is not greater than 3
z <- 1 > 3  # z is FALSE because 1 is not greater than 3

# Logical AND operation:
# to obtained TRUE, both x and y must be TRUE
x & y

# Logical OR operation: 
# to obtain TRUE, either x or y is TRUE
x | y

# Exclusive OR (XOR):
# TRUE if exactly one is TRUE, FALSE otherwise
xor(x, y)

# Difference between OR and XOR:
# both y and z are FALSE
z | y
xor(x, y)

# Other examples...
a <- 4 > 3  # TRUE
b <- 5 > 4  # TRUE

# Logical OR and XOR for a and b
a | b
xor(a, b)

# Logical NOT operator: negates the value
!a # a is TRUE, the NOT operator generates FALSE
!(2 > 3)  # TRUE because 2 > 3 is FALSE, so negation is TRUE

#############################
## Boolean logic with strings

# Compare strings for equality
string1 <- "ciao"
string2 <- "cHao"

false_result <- (string1 == string2)  # FALSE
false_result

string3 <- "ciao"
true_result <- (string1 == string3)  # TRUE because strings are identical

#######################################
## Recap of boolean operators:

## AND (&) operator: TRUE if both are TRUE
true_result & true_result
true_result & false_result
false_result & true_result

## OR (|) operator: TRUE if at least one operand is TRUE
true_result | true_result
true_result | false_result
false_result | false_result

## XOR operator: TRUE if exactly one operand is TRUE
xor(true_result, true_result)
xor(true_result, false_result)

## NOT (!) operator: negates the boolean value
!true_result
!false_result
