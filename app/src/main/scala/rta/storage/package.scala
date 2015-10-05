package rta

import rta.model.Rule

package object storage {

}

package storage {
  final case class RegistrationInfo(addedRequirements: Set[Int], removedRequirements: Set[Int], addedRules: Set[Rule])
}
