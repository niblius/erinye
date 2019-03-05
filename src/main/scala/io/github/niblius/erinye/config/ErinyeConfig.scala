package io.github.niblius.erinye.config

final case class ServerConfig(host: String, port: Int)
final case class ErinyeConfig(db: DatabaseConfig, server: ServerConfig)
