package io.shaka.http

import io.shaka.http.Http.HttpHandler
import io.shaka.http.HttpHeader.CONTENT_LENGTH
import io.shaka.http.Method.{GET, HEAD}
import io.shaka.http.Status.INTERNAL_SERVER_ERROR

import scala.util.Try

object Handlers {

  object HEADRequestHandler {
    def ~>(handler: HttpHandler): HttpHandler = {
      val adapted: HttpHandler = {
        case request@Request(HEAD, _, _, _) if handler.isDefinedAt(request.copy(method = GET)) ⇒ {
          val response = handler(request.copy(method = GET))
          response.header(CONTENT_LENGTH, response.entity.fold("0")(_.content.length.toString))
        }
      }

      adapted orElse handler
    }
  }

  object SafeRequestHandler {
    def ~>(handler: HttpHandler): HttpHandler = new HttpHandler {
      def isDefinedAt(request: Request): Boolean = Try(handler.isDefinedAt(request)).toOption.getOrElse(true)

      def apply(request: Request): Response = try {
        handler(request)
      } catch {
        case e: Throwable => Response().entity(s"Server error: ${e.getMessage}").status(INTERNAL_SERVER_ERROR)
      }
    }
  }

}
