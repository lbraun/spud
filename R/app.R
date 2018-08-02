#' App
#'
#' This class represents a spatial app and its associated usage data.
#'
#' @section Usage:
#' \preformatted{
#' app = App$new("My app", x)
#'
#' app$users()
#' app$user_count()
#' app$get_user(user_id)
#' app$actions_map(flavor = "leaflet")
#'
#' print(app)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{name}{Character scalar, the name of the app.}
#'   \item{usage_data}{An sf object, representing the usage data for the app.}
#' }
#'
#' @section Details:
#' \code{$new()} sets up the class with two fields: name and usage_data
#'
#' \code{$print()} prints out some basic identifying information about the app.
#'
#' @importFrom R6 R6Class
#' @name App
#' @examples
#' x = read.spud("dummy_data.csv")
#' app = App$new("My app", x)
#'
NULL

#' @export
App = R6::R6Class("App",
  public = list(
    name = "character",
    usage_data = "sf",

    initialize = function(name = NA, usage_data = NA) {
      self$name = name
      self$usage_data = usage_data
    },

    print = function(...) {
      cat("<App> ", self$name, ", an app with ", self$user_count(), " users\n", sep = "")
      invisible(self)
    },

    users = function() {
      unique(self$usage_data$user)
    },

    user_count = function() {
      length(self$users())
    },

    get_user = function(user_id) {
      user_data = self$usage_data %>% filter(user == user_id)
      User$new(id = user_id, usage_data = user_data)
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

    first_actions_map = function() {
      first_actions = self$usage_data %>% group_by(user) %>% top_n(-1, datetime)
      mapview(first_actions, zcol = "action", legend = TRUE)
    }
  )
)
