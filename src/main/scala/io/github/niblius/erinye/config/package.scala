package io.github.niblius.erinye

import io.circe.Decoder
import io.circe.generic.semiauto._

package object config {
  implicit val srDec: Decoder[ServerConfig] = deriveDecoder
  implicit val dbDec: Decoder[DatabaseConfig] = deriveDecoder
  implicit val eriDec: Decoder[ErinyeConfig] = deriveDecoder
}
