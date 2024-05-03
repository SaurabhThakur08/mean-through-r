# mean-through-r
# Create a function to cache computations
make_cacheable_function <- function(f) {
  # Initialize an empty environment to store cached values
  cache <- new.env()
  
  # Define a function that takes an argument and computes the result
  cached_function <- function(...) {
    args <- list(...)
    args_str <- paste(args, collapse = "_")  # Convert arguments to a string for cache key
    
    # Check if the result is already cached
    if (exists(args_str, envir = cache)) {
      message("Using cached result for ", args_str)
      return(cache[[args_str]])
    } else {
      # Compute the result using the provided function
      result <- f(...)
      
      # Cache the result
      cache[[args_str]] <- result
      
      return(result)
    }
  }
  
  # Return the cached function
  return(cached_function)
}

# Example usage: Create a cached version of mean function
cached_mean <- make_cacheable_function(mean)

# Test the cached mean function
vec <- 1:1000000
print(cached_mean(vec))  # This will compute and cache the mean
print(cached_mean(vec))  # This will use the cached result
