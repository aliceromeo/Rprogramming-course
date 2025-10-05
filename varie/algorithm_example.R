######################################################
###             ALGORITHM EXAMPLE                  ###
### Count the number of cytosines in a sequence    ###
######################################################

## 1. "Manual" counting ##

# Define the sequence as a string
sequence <- "ACTGACCGGGACCCCCATCACA"

# Display the sequence
sequence

# Split the sequence into individual nucleotides
# Use strsplit to create a list containing a character vector
# Extract the vector with [[1]]
nucleotides <- strsplit(sequence, "")[[1]]

# Display the nucleotides vector
nucleotides

# Define a variable to count the number of cytosines
# Set it to 0 initially
nc <- 0

# Save the first nucleotide of the sequence in variable x
x <- nucleotides[1]

# Check if x is a "C" or not
comparison_result <- x == "C"
print(paste0("Is the first nucleotide a 'C'? ", comparison_result))

# Read the second nucleotide in the sequence
x <- nucleotides[2]

# Check if it is a "C" or not
comparison_result <- x == "C"
print(paste0("Is the second nucleotide a 'C'? ", comparison_result))

# If it is a "C", increase the counter variable by 1
if (comparison_result) {
  nc <- nc + 1
}
nc

# The above is equivalent to...
# if (x == "C") {
#  nc <- nc + 1
# }

## In theory, you should continue like this for the entire sequence... ##

#####
## 2. Write a "for" loop to count the number of "C" ###

# The first steps are the same as the previous method
# Define the sequence as a string
sequence <- "ACTGACCGGGACCCCCATCACA"

# Display the sequence
sequence

# Split the sequence into individual nucleotides
nucleotides <- strsplit(sequence, "")[[1]]

# Display the nucleotides vector
nucleotides

# Initialize the counter for cytosines to 0
nc <- 0

# Write a "for" loop that reads each nucleotide ("nt") in the vector "nucleotides"
for (nt in nucleotides){
  print(nt)        # Show which nt is being read
  if (nt == "C"){  # If the nt being read is a "C"...
    nc <- nc + 1   # Increase the "C" counter
  }
}

# Display the final number of cytosines
nc

####
# We can do the same with a random nucleotide sequence of any length
# For example, let's generate a random DNA sequence of length 1,000,000

# Define the nucleotides that can occur in a DNA sequence:
dna_nt <- c("A","C","G","T")

# Define the length of the sequence we want to create
length <- 1000000

# sample() takes as arguments the characters that can be in the final vector ("dna_nt")
# and the length of the vector ("length")
# The parameter "replace=TRUE" indicates that a single character ("A", "C", "G", "T")
# can be repeated multiple times within the sequence
# Save the resulting vector in the variable "nucleotides"
sequence_list <- sample(dna_nt, length, replace = TRUE)

# Reset the "C" counter to 0
nc <- 0
# Run the "for" loop again (this time not printing each nt because they are 1,000,000...)
for (nt in sequence_list){
  if (nt == "C"){  
    nc <- nc + 1   
  }
}

# Print the final number of cytosines
nc

######
## 3. Count the number of "C" more efficiently using a built-in function in R ##

# Load the "stringr" library, which contains the function we need
library(stringr)

# Join the sequence back into a string
sequence <- paste0(sequence_list, collapse = "")
# Use the str_count function, passing the sequence to analyze ("sequence")
# and the character to count within it ("C")
# N.B., in this case, we don't need to split the sequence into individual nucleotides
# because the function manages this automatically
nc <- str_count(sequence, "C")

# Display the final number of cytosines
nc



