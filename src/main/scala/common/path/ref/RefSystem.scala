package common.path.ref

trait RefSystem { self =>
  type S <: RefSystem
  type P <: PathRef { type S = self.S }
  type F <: FileRef { type S = self.S }
  type D <: DirectoryRef { type S = self.S }
}
object RefSystem {
  /**
   * Useful to avoid having to come up with new names for type parameters. Example usage:
   * {{{
   * SomeClass[S <: RefSystem.Aux[S]](...)
   * }}}
   */
  type Aux[S0] = RefSystem { type S = S0 }
}
