package sta.service

import android.net.ConnectivityManager
import kj.android.common.SystemServices._
import sta.model.triggers.Implicits._

class NetworkService(implicit root: RulesExecutor) extends ServiceFragment[Network] {
  final val handle: PF = {
    case intent if intent.getAction == ConnectivityManager.CONNECTIVITY_ACTION =>
      for {
        tpe <- Network.Connection.intValues.get(intent[Int](ConnectivityManager.EXTRA_NETWORK_TYPE))
        state <- Network.State.enumValues.get(connectivityManager.getNetworkInfo(tpe.intValue).getState)
      } yield Network(tpe, state)
  }
}
