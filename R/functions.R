#' An nbapackage function
#'
#' This function will give out the player who scored the most amount of points in the NBA for the selected year.
#' @param year The year from which you want the top scorer.
#' @keywords top scorer
#' @export
#' @examples
#' highest_points()

highest_points <- function(year) {
  temp <- filter(nba_data, Year == year)
  return(select(filter(temp, PTS == max(temp$PTS)),Year:Player))
}


#' An nbapackage function
#'
#' This function will give out the oldest player by age in any given year.
#' @param year The year from which you want the oldest player.
#' @keywords oldest player
#' @export
#' @examples
#' oldest_player()

oldest_player <- function(year) {
  temp <- filter(nba_data, Year == year)
  temp_oldest <- select(filter(temp, Age == max(temp$Age)),Year:Age)
  return(slice_min(temp_oldest,1))
}

#' An nbapackage function
#'
#' This function will output the correlation matrix of all continuous variables from a given year
#' @param year The year from which you want the correlation matrix
#' @keywords correlation matrix
#' @export
#' @examples
#' corr_mat()

corr_mat <- function(year) {
  temp <- filter(nba_data, Year == year)
  temp_cont <- temp[,sapply(temp, is.numeric)]
  return(cor(temp_cont))
}
