source('brazilBingo.R')
#This driver calls the generate matrix with defaults

bb_matrix <- generate_matrix_9_by_9()
View( bb_matrix )
print( paste( "Is the matrix valid?", validate_matrix(bb_matrix), sep=" " ))
