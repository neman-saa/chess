package chess.configuration

import pureconfig.*
import pureconfig.error.CannotConvert
import pureconfig.generic.derivation.default.*

final case class PostgresConfig(nThreads: Int, url: String, username: String, password: String) derives ConfigReader
