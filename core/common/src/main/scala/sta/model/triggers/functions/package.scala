package sta.model.triggers

package object functions {
  class Has[To] { type Conversion[From] = From ⇒ To }
}
