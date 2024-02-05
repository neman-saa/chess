package chess.configuration

import pureconfig.generic.derivation.default.*
import pureconfig.error.CannotConvert
import pureconfig.*

case class JdbcDatabaseConfig(
    driver: String,
    url: String,
    user: String,
    password: String,
    migrationsTable: String,
    migrationsLocations: List[String]
) derives ConfigReader
