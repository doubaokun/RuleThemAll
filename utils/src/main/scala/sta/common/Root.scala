package sta.common

import android.content.Context
import android.net.Uri
import com.stericson.RootShell.execution.Command
import com.stericson.RootTools.RootTools

object Root {
  def putSettings[T](scope: String, key: String, value: T): String = {
    s"settings put $scope $key $value"
  }

  def sendBroadcast(action: String, extras: (String, Any)*): String = {
    val data = extras.collect {
      case (k, v: Boolean) => s"--ez $k $v"
      case (k, v: Int) => s"--ei $k $v"
      case (k, v: Long) => s"--el $k $v"
      case (k, v: Float) => s"--ef $k $v"
      case (k, v: String) => s"--es $k $v"
      case (k, v: Uri) => s"--eu $k $v"
    }.mkString(" ")
    s"am broadcast -a $action $data"
  }

  def grantPermission(ctx: Context, permission: String): String = {
    s"pm grant ${ctx.getPackageName} $permission"
  }

  def run(cmds: String*): Unit = {
    try {
      RootTools.getShell(true).add(new Command(0, cmds: _*))
    } finally  {
      RootTools.closeAllShells()
    }
  }
}
