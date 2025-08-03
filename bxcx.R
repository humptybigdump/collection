# Find lambda and transform variable (can be based on model or variable)
bxcx <- function(x) {

  require(car)

  if (!inherits(x, c("lm", "formula"))) {
    # Ensure the variable is positive
    if (min(x) <= 0) {
      shft <- abs(min(x)) + 1
      x <- x + shft
    } else {
      shft <- 0
    }
    # Retrieve lambda for the vector
    lambda <- car::powerTransform(x)$lambda
    names(lambda) <- NULL
  } else {
    y <- model.frame(x)[, 1]
    if (min(y) <= 0) {
      # Replace the response variable in the model object
      # with the shifted variable
      shft <- abs(min(y)) + 1
      y <- y + shft
      model_frame <- model.frame(x)
      model_frame[, 1] <- y
      x <- update(x, data = model_frame)
    } else {
      shft <- 0
    }
    # Retrieve lambda for the model
    lambda <- car::powerTransform(x)$lambda
    names(lambda) <- NULL

    # Overwrite x with the shifted response variable
    x <- y
  }

  # Apply the Box-Cox transformation
  if (lambda == 0) {
    transf <- log(x)
  } else {
    transf <- (x ^ lambda - 1) / lambda
  }

  # Set attributes for lambda and shift
  attr(transf, "lambda") <- lambda
  attr(transf, "shift") <- shft

  return(transf)
}

# Define the forward Box-Cox transformation function
bxcxforward <- function(x, lambda, shift = 0) {
  if (lambda == 0) {
    transf <- log(x + shift)
  } else {
    transf <- ((x + shift) ^ lambda - 1) / lambda
  }
  # Set attributes for lambda and shift
  attr(transf, "lambda") <- lambda
  attr(transf, "shift") <- shift
  return(transf)
}

bxcxback <- function(x, lambda, shift = 0) {
  if (lambda == 0) {
    original <- exp(x) - shift
  } else {
    original <- ((x * lambda) + 1)^(1 / lambda) - shift
  }
  return(original)
}