package sta.parser.actions

import fastparse.noApi._
import sta.model.actions.LaunchApplication

object LaunchApplicationRules extends ActionParser[LaunchApplication] {
  import white._

  val Rule: P[LaunchApplication] = "launch" ~ ("application" | "app") ~ (
    ("from" ~ "package" ~ SingleLineString map LaunchApplication.fromPackage) |
      (SingleLineString map LaunchApplication.apply))
}
