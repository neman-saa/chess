package chess.configuration

import com.comcast.ip4s.Host
import com.comcast.ip4s.Port
import pureconfig.*
import pureconfig.error.CannotConvert
import pureconfig.generic.derivation.default.*

case class EmberConfig(host: Host, port: Port) derives ConfigReader

object EmberConfig {
  given hostReader: ConfigReader[Host] = ConfigReader[String].emap { hostString =>
    Host.fromString(hostString) match {
      case None =>
        Left(CannotConvert(hostString, Host.getClass.toString, s"Invalid host string $hostString"))
      case Some(host) =>
        Right(host)
    }
  }
  given portReader: ConfigReader[Port] = ConfigReader[Int].emap { port =>
    Port.fromInt(port) match {
      case None =>
        Left(CannotConvert(port.toString, Port.getClass.toString, s"Invalid port string $port"))
      case Some(port) =>
        Right(port)
    }
  }
}
