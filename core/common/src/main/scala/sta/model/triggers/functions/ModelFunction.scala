package sta.model.triggers.functions

import sta.model.Model

abstract class ModelFunction[M <: Model] extends (M ⇒ Boolean)
