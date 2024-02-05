package chess.configuration

import pureconfig.generic.derivation.default.*
import pureconfig.ConfigReader

case class AppConfig(
    emberConfig: EmberConfig,
    postgresConfig: PostgresConfig,
    securityConfig: SecurityConfig,
    jdbcDatabaseConfig: JdbcDatabaseConfig
) derives ConfigReader
