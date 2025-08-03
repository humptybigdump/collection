# Author: Konstantin Görgen
# Date: Tue May 12 16:33:48 2020
# --------------
# Modification:
# Author:
# Date:
# --------------

#Optional arguments in function

#built a sum function (inefficiently)
#... are elements to sum, either as seperate variables or vectors

#... is usually best read in as list or given to a function inside the function

sum_man<-function(...) {
  
  opt_arg<-list(...) 
  sum_elements<-as.numeric(unlist(opt_arg))
  return_element<-0
  for (i in 1:length(sum_elements))
  {
    return_element<-return_element+sum_elements[i]
  }
  return(return_element)
}


test_list<-list(a=1:10,b=12)
test_list #has two elements with different lengths
unlist(test_list) #gives back all elements pasted together
sum_man(1:10,12)
#function inside function
#multiply a by sum of all the other elements in ...
sum_prod<-function(a,...)
{
  a*sum(...)
}

sum_prod(2,1:10)
