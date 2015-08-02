package sta

import kj.android.logging.LogTag

package object store {
  implicit val logTag = LogTag("sta.store")

  object plain {
    implicit val definitionsStore = PlainDefinitionsStore
  }

}
