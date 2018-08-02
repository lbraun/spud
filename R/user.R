#' User
#'
#' This class represents a user of a spatial app.
#'
#' @section Usage:
#' \preformatted{
#' user = User$new("User 1", x)
#'
#' user$first_action()
#' user$actions_map(flavor = "leaflet")
#' user$path_map()
#'
#' print(user)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{id}{Character scalar, the id of the user.}
#'   \item{usage_data}{An sf object, representing the usage data of the user.}
#' }
#'
#' @section Details:
#' \code{$new()} sets up the class with two fields: id and usage_data
#'
#' \code{$print()} prints out some basic identifying information about the user.
#'
#' @importFrom R6 R6Class
#' @name User
#' @examples
#' x = read.spu("dummy_data.csv")
#' user <- User$new("User 1", x)
#'
NULL

#' @export
User = R6::R6Class("User",
  public = list(
    id = "character",
    usage_data = "sf",

    initialize = function(id = NA, usage_data = NA) {
      self$id = id
      self$usage_data = usage_data
    },

    print = function(...) {
      date = format(self$first_action()$datetime, format = "%B %d, %Y")
      cat("<User> ", self$id, " (active since ", date, ")\n", sep = "")
      invisible(self)
    },

    first_action = function() {
      self$usage_data %>% arrange(datetime) %>% top_n(1)
    },

    actions_map = function(flavor = "leaflet") {
      if (flavor == "leaflet") {
        plot_usage_actions_leaflet(self$usage_data)
      } else if (flavor == "mapview") {
        plot_usage_actions_mapview(self$usage_data)
      } else {
        stop("Invalid flavor!")
      }
    },

    path_map = function() {
      plot_user_path(self$usage_data, self$id)
    }
  )
)
