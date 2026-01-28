package common.path.ref.io

import common.path.ref.RefSystem

trait IOSystem extends RefSystem {
  override type S = IOSystem
  override type P = IOPath
  override type F = IOFile
  override type D = IODirectory
}
