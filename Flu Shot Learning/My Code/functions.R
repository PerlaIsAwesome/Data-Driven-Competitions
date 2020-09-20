#ESSENTIAL LIBRARIES
#none

#USEFULL FUNCTIONS

#Info: unloads all non base functions
#Source: https://stackoverflow.com/questions/55655162/unload-all-loaded-packages
detachAllPackages <- function() {
  basic.packages.blank <- c(    
    "stats",    
    "graphics",    
    "grDevices",    
    "utils",   
    "datasets",  
    "methods",    
    "base"    
  )    
  basic.packages <- paste("package:", basic.packages.blank, sep = "")   
  package.list <- search()[ifelse(unlist(gregexpr("package:", search())) == 1, TRUE, FALSE)]   
  package.list <- setdiff(package.list, basic.packages)   
  if (length(package.list) > 0) {   
    for (package in package.list) {   
      detach(package, character.only = TRUE)   
    }   
  }    
}

#Info: finds all na values and returns a data frame with the column names, type of vairble, 
# and total of NA's in that column
na_per_col = function(data_set){
  total_na = c()
  type = c()
  for (i in 1:ncol(data_set)){
    total_na[i] = sum(is.na(data_set[i]))
    type[i] = typeof(data_set[1,i])
  }
  return(data.frame(colnames(data_set),type,total_na)[order(-total_na),])
}

#na_per_col(subset(trainFeat, !is.na(doctor_recc_h1n1)))

#algorith that removes the top n varibles with the most NA values
#remove_na_by_top_vars = function(data_set, num = 1, exclude = ){
#  [which.max(na_per_col(data_set)[3])]
#}

factor_all_col = function(data_set) {
  for (i in 1:ncol(data_set)){
    data_set[i] = as.factor(data_set[i])
  }
  return(data_set)
}


