source('brazilBingo.R')

bb_matrix <- generate_matrix_9_by_9()
View( bb_matrix )
print( validate_matrix(bb_matrix) )