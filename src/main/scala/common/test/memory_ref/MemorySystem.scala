package common.test.memory_ref

import common.path.ref.RefSystem

trait MemorySystem extends RefSystem {
  override type S = MemorySystem
  override type P = MemoryPath
  override type F = MemoryFile
  override type D = MemoryDir
}
