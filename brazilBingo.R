generate_num_list <- function(){
  all_nums <- sample( 1:90, 90 )
  all_nums_fact <- c( rep('top', 45), rep('bottom', 45))
  split_nums <- split( all_nums, all_nums_fact )
  split_nums
}

create_matrix <- function(){
  matrix( nrow = 9, ncol = 9)
}

generate_indices_by_third <- function(third){
  start <- (third - 1) * 3 + 1
  end <- start + 2
  start:end
}

number_count <- function(array){
  sum( !is.na(array))
}
number_list_count <- function(list){
  sum( sapply( list, function(arr){number_count(arr)}) )
}
randomly_fill_third <- function(third, min_size  = 1){
  if( number_count( third ) < min_size){
    match_inds <- which( is.na(third))
    if( length( match_inds ) > 0 ){
      #set.seed(10001)
      sample_index <- sample( match_inds, 1 )
      third[ sample_index ] <- 0
    }
  }
  third
}
randomly_fill_list <- function(list){
  lapply( list, randomly_fill_third)
}
randomly_add_fifth <- function(list){
  indices_less_than_2 <- which( sapply( list, function(elem){ number_count(elem) < 2 }) )
  rand_index <- sample( indices_less_than_2, 1 )
  rand_array <- list[[rand_index]]
  randomly_filled_third <- randomly_fill_third(rand_array, 2)
  list[[rand_index]] <- randomly_filled_third
  list
}

generate_list_of_nine <- function(init=rep(NA,9)){
  ret_list <- init
  if(  number_count(ret_list) < 5 ){
    #first third
    first_third <- ret_list[1:3]
    second_third <- ret_list[4:6]
    third_third <- ret_list[7:9]
    
    list_break_down <- list( first_third, second_third, third_third )
        
    list_break_down <- randomly_fill_list( list_break_down )
    
    while( number_list_count(list_break_down) < 5 ){
      list_break_down <- randomly_add_fifth( list_break_down )  
    }
    ret_list <- unlist( list_break_down )
  }
  ret_list
}


generate_matrix_9_by_9 <- function(init=create_matrix()){
    for( d in 1:9 ){
      row <- generate_list_of_nine_acc( init[d,] )
      init[ d, ] <- row
      col <- generate_list_of_nine_acc( init[,d] )
      init[ ,d ] <- col
    }
    init
}
validate_dim <- function( dim ){
  ret_value <- TRUE
  for(third in 1:3){
    index_start <- (third -1) * 3 + 1
    third_of_row <- dim[index_start: (index_start + 2)]
    third_of_row_bool <- !is.na( third_of_row )
    third_of_row_bool_sum <- sum( third_of_row_bool)
    if( third_of_row_bool_sum > 2 | third_of_row_bool_sum == 0) {
      ret_value <- FALSE
      break
    }
  }
  ret_value
}
validate_matrix_rows <- function( mat ){
  ret_value <- TRUE
  for( rIndex in 1:nrow(mat)){
    row <- mat[ rIndex, ]
    if( !validate_dim(row) ){
      ret_value <- FALSE
      break
    }
  }
  ret_value
}

validate_matrix_cols <- function( mat ){
  ret_value <- TRUE
  for( cIndex in 1:ncol(mat)){
    col <- mat[ , cIndex ]
    if( !validate_dim(col) ){
      ret_value <- FALSE
      break
    }
  }
  ret_value
}

validate_matrix <- function( mat ){
  validate_matrix_cols(mat) & validate_matrix_rows(mat)
}

shuffle_matrix <- function( mat ){
  ret_matrix = matrix( nrow = nrow( mat ), ncol = ncol( mat ) )
  for( ind in  1 : ncol(mat)){
    ret_matrix[, ind] <- sample( mat[, ind] )
  }
  for( ind in 1 : nrow(mat)){
    ret_matrix[ ind, ] <- sample( mat[ ind, ] )
  }
  ret_matrix
}

find_matrix <- function(mat, max_iters=100){
  index <- 0
  start_mat <- mat
  found_matrix <- FALSE
  while( index <= max_iters ){
    index <- index + 1
    print( paste("testing iteration number", index, sep = " ") )
    if( validate_matrix( start_mat) ){
      print( "Found a matrix")
      print( start_mat )
      found_matrix <- TRUE
      break
    }
    else{
      start_mat <- shuffle_matrix(start_mat)
      print (start_mat)
    }
    
  }
  if( !found_matrix ){
    print( "No matrix found...")
  }
}