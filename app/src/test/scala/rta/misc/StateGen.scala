package rta.misc

import rta.model.BaseModel
import rta.model.triggers.Implicits._
import scala.collection.Iterator
import scala.collection.mutable
import spire.math._

trait StateGen {
  final def singleSize: Int = 8
  
  def batteryGen: Iterator[Battery] = {
    def level: Iterator[UByte] = (0 to 100 by 5).iterator.map(UByte(_))
    def plugged: Iterator[Battery.Plugged] = Battery.Plugged.values.iterator
    def present: Iterator[Boolean] = Iterator(true, false)
    def status: Iterator[Battery.Status] = Battery.Status.values.iterator
    val b = for {
      l <- level
      pl <- plugged
      pr <- present
      s <- status
    } yield Battery(l, pl, pr, s)

    b
  }
  def batteryStateGen: Iterator[BatteryState] = BatteryState.values.iterator
  def powerStateGen: Iterator[PowerState] = PowerState.values.iterator
  def bluetoothStateGen: Iterator[BluetoothState] = BluetoothState.values.iterator
  def dockStateGen: Iterator[DockState] = DockState.values.iterator
  def headsetGen: Iterator[Headset] = Headset.values.iterator
  def networkGen: Iterator[Network] = {
    def connection: Iterator[Network.Connection] = Network.Connection.values.iterator
    def state: Iterator[Network.State] = Network.State.values.iterator
    val n = for {
      c <- connection
      t <- state
    } yield Network(c, t)

    n
  }
  def wiFiStateGen: Iterator[WiFiState] = WiFiState.values.iterator

  lazy val allLength = batteryGen.length * batteryStateGen.length *
    powerStateGen.length * dockStateGen.length * headsetGen.length *
    networkGen.length * bluetoothStateGen.length * wiFiStateGen.length

  @transient
  lazy val state: Int => Iterator[BaseModel] = {
    def gen = for {
      battery <- batteryGen
      batteryState <- batteryStateGen
      powerState <- powerStateGen
      dock <- dockStateGen
      headset <- headsetGen
      network <- networkGen
      bluetoothState <- bluetoothStateGen
      wiFiState <- wiFiStateGen
    } yield Iterator[BaseModel](battery, batteryState, powerState, dock, headset, network, bluetoothState, wiFiState)

    size => gen.flatten.slice(sizes.takeWhile(_ < size).sum, size)
  }

  lazy val sizes: Vector[Int] =
    Iterator.iterate(singleSize)(_ * 20).takeWhile(_ <= allLength).toVector
}
