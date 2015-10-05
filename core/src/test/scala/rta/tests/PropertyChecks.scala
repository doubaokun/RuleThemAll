package rta.tests

trait PropertyChecks extends org.scalatest.prop.PropertyChecks {
  override implicit val generatorDrivenConfig = PropertyCheckConfig(
    minSuccessful = 1000,
    maxDiscarded = 5000,
    minSize = 10,
    workers = Runtime.getRuntime.availableProcessors()
  )
}
