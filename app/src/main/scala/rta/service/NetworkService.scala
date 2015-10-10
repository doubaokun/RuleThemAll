package rta.service

import android.net.ConnectivityManager
import rta.common.SystemServices._
import rta.common.Uses
import rta.model.triggers.Implicits._

class NetworkService(implicit root: RulesExecutor) extends ServiceFragment[Network] {
  final val handle: PF = {
    case intent if intent.getAction == Uses.actionFor[Network] =>
      for {
        tpe <- Network.Connection.intValues.get(intent[Int](ConnectivityManager.EXTRA_NETWORK_TYPE))
        state <- Network.State.enumValues.get(connectivityManager.getNetworkInfo(tpe.intValue).getState)
      } yield Network(tpe, state)
  }
}
