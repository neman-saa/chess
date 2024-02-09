package chess.configuration

import pureconfig.*
import pureconfig.error.CannotConvert
import pureconfig.generic.derivation.default.*

case class JdbcDatabaseConfig(
    driver: String,
    url: String,
    user: String,
    password: String,
    migrationsTable: String,
    migrationsLocations: List[String]
) derives ConfigReader
