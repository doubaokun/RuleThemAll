package sta

import sta.model.Rule

package object storage {

}

package storage {
  case class RegistrationInfo(addedRequirements: Set[Int], removedRequirements: Set[Int], addedRules: Set[Rule])
}
