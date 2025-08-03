library(dplyr)
library(ggplot2)
library(tree)

#toy dataset
dat = data.frame(
  Student = 1:6,
  StudyHours = c(60, 65, 23, 91, 84, 57),
  ProcrastinationHours = c(51, 23, 151, 7, 19, 21),
  PointsReached = c(71, 85, 20, 97, 89, 79)
)

dat

variables <- c("StudyHours", "ProcrastinationHours")

#plot data to get a feeling of what is going on
dat %>% ggplot(aes(x=StudyHours, y=ProcrastinationHours, color=PointsReached)) + geom_point() 

#build a tree from the package "tree" just to check if everything works
tr <- tree(PointsReached~ProcrastinationHours+StudyHours, data = dat, 
           control=tree.control(6,minsize = 2, mindev = 0.))
plot(tr)
text(tr, pretty = 0)

partition.tree(tr, add=FALSE)

# function that calculates the mse
mse <- function(y, y_hat){
  return(mean((y - y_hat) ** 2))
}

#iterates over all possible variables and their values to find the next split
get_best_split <- function(df, variables){
  
  #initialize variables
  best_split_mse <- 7531
  best_split_value <- 0
  best_split_col <- ""
  
  #iterate over all variables
  for(col in variables){ 
    
    #find all the unique values that variable col holds and sort (for convenience)
    uniques <- df[[col]] %>% unique %>% sort
    
    #iterate over all unique values in col
    for (i in 1:length(uniques)) { 
      
      #split data into two subsets r1 and r2
      r1 = df %>% subset(df[[col]] <= uniques[i])
      r2 = df %>% subset(df[[col]] > uniques[i])
      
      #calculate the mse of each subset
      r1_mse <- mse(mean(r1$PointsReached), r1$PointsReached)
      r2_mse <- mse(mean(r2$PointsReached), r2$PointsReached)
      
      #doublecheck if the mse is nan and if so, set to 0
      r1_mse = if (is.nan(r1_mse)) 0 else r1_mse
      r2_mse = if (is.nan(r2_mse)) 0 else r2_mse
      
      #calculate the overall mse of the current split
      split_mse <- r1_mse + r2_mse
      
      #check if current split is better than the previously best split
      #if so, set the "best-variables" to new split
      if (split_mse <= best_split_mse){
        best_split_col = col
        best_split_value = uniques[i]
        best_split_mse = split_mse
      }
      
    }
  }
  
  #split given data into two subsets with lowest mse
  df_split_1 = df %>% subset(df[[best_split_col]] <= best_split_value)
  df_split_2 = df %>%  subset(df[[best_split_col]] > best_split_value)
  
  
  return_list <- list("split_1" = df_split_1,
                      "split_2" = df_split_2, 
                      "best_mse" = best_split_mse,
                      "best_col" = best_split_col,
                      "best_value" = best_split_value)
  
  return (return_list)
}

#define tree object as a list
tree = list(
  "id" = "root",
  "node" = dat,
  "best_mse" = 0,
  "best_col" = "",
  "best_value" = 0
)

#build some sort of queue
nodes_todo = list(tree)


result_list = list()

counter <- 0

#iterate over the list "nodes_todo" which contains all the unvisited nodes
while(length(nodes_todo) > 0){
  #get the first node in the todo-list
  current_node = nodes_todo[[1]] 
  #delete the current_node out of the todo-list
  nodes_todo = nodes_todo[-1]
  
  print("-------------------------")
  print(current_node)
  #check if the node does contain more than one remaining sample.
  # if yes, continue or else add the node to the list without splitting further 
  # and continue with next iteration 
  if(nrow(current_node$node) <= 1){
    result_list[[length(result_list)+1]] <- current_node
    next
  }
  
  #get best split 
  res = get_best_split(current_node$node, variables=variables)
  
  #set variables of current node
  current_node$best_mse <- res$best_mse
  current_node$best_col <- res$best_col
  current_node$best_value <- res$best_value
  
  #define both new child objects
  new_child_1 = list(
    "id" = paste(current_node$id, "1", sep=""),
    "node" = res$split_1
  )
  new_child_2 = list(
    "id" = paste(current_node$id, "2", sep=""),
    "node" = res$split_2
  )
  #add children to the current_node object
  current_node$children <- list(new_child_1, new_child_2)
  
  #add children to the todo-list
  nodes_todo[[length(nodes_todo)+1]] <- new_child_1
  nodes_todo[[length(nodes_todo)+1]] <- new_child_2
  
  #add the current node to the result
  result_list[[length(result_list)+1]] <- current_node
  
  
}

