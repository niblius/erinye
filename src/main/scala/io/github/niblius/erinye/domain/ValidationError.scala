package io.github.niblius.erinye.domain

trait ValidationError {
  def explanation: String
  def key: String = this.getClass.getSimpleName
}
