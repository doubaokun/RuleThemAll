package rta.parser.actions

import fastparse.noApi._
import rta.model.actions._
import rta.parser.ActionParser

object AlterApplicationParser extends ActionParser[AlterApplication] {
  import white._
  import AlterApplication._

  private def appOrPackage(c: (String, Resolver) => AlterApplication): P[AlterApplication] =
    ("application".withWS | "app".withWS) ~! (
      ("from".withWS ~ "package".withWS ~ SingleLineString.map(pkg => c(pkg, Resolver.fromPackage))) |
        SingleLineString.map(app => c(app, Resolver.fromAppName))
      )

  def Rule: P[AlterApplication] =
    ("launch".withWS ~ appOrPackage(Launch)) | ("kill".withWS ~ appOrPackage(Kill))
}
