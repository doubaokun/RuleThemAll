package sta.parser.actions

import fastparse.noApi._
import sta.model.actions.LaunchApplication
import sta.parser.ActionParser

object LaunchApplicationRules extends ActionParser[LaunchApplication] {
  import white._

  val Rule: P[LaunchApplication] = "launch".withWS ~ ("application".withWS | "app".withWS) ~ (
    ("from".withWS ~ "package".withWS ~ SingleLineString map LaunchApplication.FromPackage) |
      (SingleLineString map LaunchApplication.UsingAppName))
}
