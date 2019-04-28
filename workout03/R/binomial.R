# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

library(dplyr)
library(ggplot2)
library(devtools)

# private function to check probability
check_prob <- function(prob) {
  if (length(prob) != 1) {
    stop("invalid prob value")
  } else if (!is.numeric(prob)) {
    stop("invalid prob value")
  } else if (prob > 1 | prob < 0) {
    stop("must be a number between 0 and 1")
  } else {
    return(TRUE)
  }
}

# private function to check trials
check_trials <- function(trials) {
  if (!is.numeric(trials)) {
    stop("invalid trials value")
  } else if (trials >= 0 & trials %% 1 == 0) {
    return(TRUE)
  } else {
    stop("invalid trials value")
  }
}

# private function to check success and trials
check_success <- function(success, trials) {
  if (length(trials) != 1) {
    stop("invalid trials value")
  } else if (!is.numeric(success))
    stop("invalid success value")
  else if (any(success < 0)) {
    stop("invalid success value")
  } else if (any(success %% 1 != 0)) {
    stop("invalid success value")
  } else if (any(success > trials)) {
    stop("success cannot be greater than trials")
  }
  return(TRUE)
}

# private auxiliary functions to calculate mean
aux_mean <- function(trials, prob) {
  return(trials*prob)
}

# private auxiliary functions to calculate variance
aux_variance <- function(trials, prob) {
  return(trials*prob*(1-prob))
}

# private auxiliary functions to calculate mode
aux_mode <- function(trials, prob) {
  return(as.integer(prob + prob*trials))
}

# private auxiliary functions to calculate skewness
aux_skewness <- function(trials, prob) {
  up <- 1 - 2*prob
  bot <-sqrt(aux_variance(trials, prob))
  return(up/bot)
}

# private auxiliary functions to calculate kurtosis
aux_kurtosis <- function(trials, prob) {
  up <- 1 - 6*prob*(1-prob)
  bot <- aux_variance(trials, prob)
  return(up/bot)
}


#' @title K choose K
#' @description compute the value of (n choose k)
#' @param n the value of n
#' @param k the value of k
#' @return computed value of n choose k
#' @export
#' @examples
#' bin_choose(n=5, k=2)
#'
#' bin_choose(5, 0)
#'
#' bin_choose(5, 1:3)
#'
bin_choose <- function(n, k) {
  if (length(n) != 1) {
    stop("incorrect n value")
  }
  if (k > n)
    stop("k cannot be greater than n")
  up <- factorial(n)
  bot <- factorial(k)*factorial(n-k)
  return(up/bot)
}

#' @title binomial probability
#' @description compute the probability of a binomail distribution giving certain result
#' @param success the number of successful trials
#' @param trials number of trials
#' @param prob probability that a trial will succeed
#' @return computed probability
#' @export
#' @examples
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#'
#' bin_probability(success = 0:2, trials = 5, prob = 0.5)
#'
#' bin_probability(success = 55, trials = 100, prob = 0.45)
#'
bin_probability <- function(success, trials, prob) {
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)
  fst <- bin_choose(trials, success)
  snd <- prob ** success
  trd <- (1-prob)**(trials-success)
  return(fst*snd*trd)
}

#' @title binomial distribution
#' @description compute the probability distribution of a binomial distribution
#' @param trials number of trials
#' @param prob probability that a trial will succeed
#' @return a data frame with the probability distribution
#' @export
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)
#'
bin_distribution <- function(trials, prob) {
  success_vector <- 0:trials
  prob_vector <- bin_probability(success_vector, trials, prob)
  df <- data.frame(success = success_vector, probability = prob_vector)
  class(df) <- append("bindis", class(df))
  return(df)

}

#' @export
plot.bindis <- function(df) {
  ggplot(df, aes(x=success, y=probability)) +
    geom_bar(stat="identity", color="grey") +
    theme_minimal()
}

#' @title binomial Cumulative
#' @description compute the cumulative probability distribution of a binomial distribution
#' @param trials number of trials
#' @param prob probability that a trial will succeed
#' @return computed cumulative probability distribution
#' @export
#' @examples
#' bin_cumulative(trials = 5, prob = 0.5)
#'
bin_cumulative <- function(trials, prob) {
  success_vector <- 0:trials
  prob_vector <- bin_probability(success_vector, trials, prob)
  df <- data.frame(
    success = success_vector,
    probability = prob_vector,
    cumulative = cumsum(prob_vector)
  )
  class(df) <- append("bincum", class(df))
  return(df)
}

#' @export
plot.bincum <- function(df) {
  ggplot(df, aes(x=success, y=cumulative)) +
    geom_path(stat="identity", color="grey") +
    geom_point() + ylab("probability") + theme_minimal()
}

#' @title binomial Variable
#' @description construct a binomial object with given parameters
#' @param trials number of trials
#' @param prob probability that a trial will succeed
#' @return binomial object
#' @export
#' @examples
#' bin_variable(trials = 5, prob = 0.45)
bin_variable <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  res <- list(
    trials = trials,
    prob = prob)
  class(res) <- "binvar"
  return(res)
}

#' @export
print.binvar <- function(x, ...) {
  cat('"Binomial variable"\n\n')
  cat('Parameters\n')
  cat(sprintf('- number of trials: %s\n', x$trials))
  cat(sprintf('- prob of success: %s', x$prob))
  invisible(x)
}

#' @export
summary.binvar <- function(x, ...) {
  freqs <- list(
    trials = x$trials,
    prob = x$prob,
    mean = aux_mean(x$trials, x$prob),
    variance = aux_variance(x$trials, x$prob),
    mode = aux_mode(x$trials, x$prob),
    skewness = aux_skewness(x$trials, x$prob),
    kurtosis = aux_kurtosis(x$trials, x$prob)
  )
  class(freqs) <- "summary.binvar"
  return(freqs)
}

#' @export
print.summary.binvar <- function(x,...) {
  cat('"Summary Binomial"\n\n')
  cat('Parameters\n')
  cat(sprintf('- number of trials: %s\n', x$trials))
  cat(sprintf('- prob of success: %s\n\n', x$prob))
  cat('Measures\n')
  cat(sprintf('- mean: %s\n', x$mean))
  cat(sprintf('- variance: %s\n', x$variance))
  cat(sprintf('- mode: %s\n', x$mode))
  cat(sprintf('- skewness: %s\n', x$skewness))
  cat(sprintf('- kurtosis: %s\n', x$kurtosis))
  invisible(x)
}

#' @title binomial Mean
#' @description compute the expected mean of a binomial distribution
#' @param trials number of trials
#' @param prob probability that a trial will succeed
#' @return computed mean
#' @export
#' @examples
#' bin_mean(trials = 5, prob = 0.3)
bin_mean <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials, prob))
}

#' @title binomial variance
#' @description compute the variance of a binomial distribution
#' @param trials number of trials
#' @param prob probability that a trial will succeed
#' @return computed variance
#' @export
#' @examples
#' bin_variance(trials = 5, prob = 0.3)
bin_variance <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials, prob))
}

#' @title binomial Mode
#' @description compute the mode of a binomial distribution
#' @param trials number of trials
#' @param prob probability that a trial will succeed
#' @return computed mode
#' @export
#' @examples
#' bin_mode(trials = 5, prob = 0.3)
bin_mode <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials, prob))
}

#' @title binomial Skewness
#' @description compute the skewness of a binomial distribution
#' @param trials number of trials
#' @param prob probability that a trial will succeed
#' @return computed skewness
#' @export
#' @examples
#' bin_skewness(trials = 5, prob = 0.3)
bin_skewness <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials, prob))

}

#' @title binomial Kurtosis
#' @description compute the kurtosis of a binomial distribution
#' @param trials number of trials
#' @param prob probability that a trial will succeed
#' @return computed kurtosis
#' @export
#' @examples
#' bin_kurtosis(trials = 5, prob = 0.3)
bin_kurtosis <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials, prob))
}
