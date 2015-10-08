package rta.parser.actions

import fastparse.noApi._
import rta.model.actions.LaunchApplication
import rta.parser.ActionParser

object LaunchApplicationRules extends ActionParser[LaunchApplication] {
  import white._

  def Rule: P[LaunchApplication] = "launch".withWS ~ ("application".withWS | "app".withWS) ~ (
    ("from".withWS ~ "package".withWS ~ SingleLineString map LaunchApplication.FromPackage) |
      (SingleLineString map LaunchApplication.UsingAppName))
}
