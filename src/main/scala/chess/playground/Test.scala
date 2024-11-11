package chess.playground

import cats.effect.{IO, IOApp}
import org.http4s.Header.Raw
import org.http4s.{Headers, Method, Request}
import org.http4s.ember.client.{EmberClient, EmberClientBuilder}
import org.http4s.implicits.uri
import org.typelevel.ci.CIString

object Test extends IOApp.Simple {
  val strings =
    List(
      "Accept:text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
      "Accept-Encoding:gzip, deflate, br, zstd",
      "Accept-Language:ru-RU,ru;q=0.9,en-US;q=0.8,en;q=0.7",
      "Cache-Control:max-age=0",
      "Cookie:_octo=GH1.1.1604692411.1719599682; _device_id=002ec767fb6c956b847cd2c0e8b7b487; has_recent_activity=1; saved_user_sessions=18753031%3A-JnEPZqChcKv3_Sz4QGaLXsKlLyq_fkABUm3IPCKOn3pQWNT; user_session=-JnEPZqChcKv3_Sz4QGaLXsKlLyq_fkABUm3IPCKOn3pQWNT; __Host-user_session_same_site=-JnEPZqChcKv3_Sz4QGaLXsKlLyq_fkABUm3IPCKOn3pQWNT; logged_in=yes; dotcom_user=neman-saa; color_mode=%7B%22color_mode%22%3A%22auto%22%2C%22light_theme%22%3A%7B%22name%22%3A%22light%22%2C%22color_mode%22%3A%22light%22%7D%2C%22dark_theme%22%3A%7B%22name%22%3A%22dark%22%2C%22color_mode%22%3A%22dark%22%7D%7D; preferred_color_mode=dark; tz=Europe%2FKiev; _gh_sess=TVqIyJkJVhmmPCcZ8XjQODDDM7jv%2B99HOeMAm7ehkQrmBIRgHIko4X69Lgb1n18KlihC0qLNfwEOL%2BJcS3%2BhkPmJ6LZs6q9zViHpZFCZ9i060KyQRymbH5kjJLbYJyTM8hckkE7DgNY8fP4OlzIDf8G9zWdyIocQu9zu7CqPKwSMW97RZD9tq3h3Ia0fujDycMpltr9wLznLSvEn7LjhvFbT6eVmNlQFz%2F%2BBFZQTtSkZWjzFRsKT1X7vOGkd5cgK9iQB1eFlsyQz2ctEhKLdpP37r%2BHVj2eyFoZPi1oJLTzr646VbbBRq3hu9CqJRIbrynqMnZRTRnvWP64FryupOGKr47qODbG%2BoU9epBfoS0CzNcwDZWz8dpIhuD85d5UA--WPPlw7px762K9nly--0k06%2BLPIWwd%2FRRGXavDavw%3D%3D",
      "If-None-Match:W/\"055eff967ddb5d5da494d5fca66d4b69\"",
      "Priority:u=0, iSec-Ch-Ua:\"Not/A)Brand\";v=\"8\", \"Chromium\";v=\"126\", \"Google Chrome\";v=\"126\"",
      "Sec-Ch-Ua-Mobile:?0",
      "Sec-Ch-Ua-Platform:\"Windows\"",
      "Sec-Fetch-Dest:document",
      "Sec-Fetch-Mode:navigate",
      "Sec-Fetch-Site:same-origin",
      "Sec-Fetch-User:?1",
      "Upgrade-Insecure-Requests:1",
      "User-Agent:Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36")
  val result = strings.map(x => (x.split(":").head, x.split(":").tail.head))

  val request = Request[IO](
    Method.GET,
    uri"https://github.com",
    headers = Headers(result.map(x => Raw(CIString(x._1), x._2)))
  )

  override def run: IO[Unit] = EmberClientBuilder.default[IO].build.use(client => client.expect[String](request)).map(println)
}
