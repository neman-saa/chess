package chess.configuration

import scala.concurrent.duration.FiniteDuration

import pureconfig.generic.derivation.default.*
import pureconfig.ConfigReader

final case class SecurityConfig(secret: String, jwtExpiryDuration: FiniteDuration) derives ConfigReader
