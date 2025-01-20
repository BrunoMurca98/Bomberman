package Server

import cats.effect._
import com.comcast.ip4s.IpLiteralSyntax
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.circe.CirceEntityCodec._
import io.circe.generic.auto._
import io.circe.syntax._

import java.util.UUID
import cats.syntax.all._
import fs2.io.file.Path
import io.circe.Json

import java.nio.file.Paths



object Server extends IOApp {

  val gameSessionsRef: Ref[IO, Map[UUID, GameSession]] = Ref.unsafe[IO, Map[UUID, GameSession]](Map.empty)

  // route to create a new game session
  val createGameSessionRoute: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ POST -> Root =>
      // Decode the request body to extract the game name
      req.as[Json].flatMap { json =>
        val gameRoomName = json.hcursor.get[String]("name").getOrElse("Unnamed Game")

        // Create a new game session with the provided name
        val newGameSession = GameSession.createSession(gameRoomName)

        for {
          _ <- gameSessionsRef.update(sessions => sessions + (newGameSession.id -> newGameSession))
          response <- Ok(s"Game session: ${newGameSession.gameRoomName}, ID: ${newGameSession.id}")
        } yield response
      }
  }

  // route to retrieve all active game sessions
  val getAllGameSessionsRoute: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "games" =>
      println("Received a request to get all game sessions.")
      for {
        sessions <- gameSessionsRef.get
        response <- Ok(sessions.values.toList.asJson)
      } yield response
  }

  // route to join a game session
  val joinGameRoute: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ POST -> Root / "join" =>
      req.as[Json].flatMap { json =>
        val gameSessionId = json.hcursor.get[String]("gameSessionId").getOrElse("")

        // Ensure gameSessionId is not empty
        if (gameSessionId.isEmpty) {
          BadRequest("Missing gameSessionId in the request")
        } else {
          gameSessionsRef.get.flatMap { sessions =>
            sessions.get(UUID.fromString(gameSessionId)) match {
              case Some(gameSession) =>
                // Handle adding a player to the session (e.g., by updating the players list)
                val updatedSession = gameSession.copy(players = gameSession.players :+ "newPlayer") // Example player addition
                gameSessionsRef.update(sessions => sessions + (gameSession.id -> updatedSession)) *>
                  Ok(s"Successfully joined the game session: ${gameSession.gameRoomName}")
              case None =>
                NotFound(s"Game session with ID: $gameSessionId not found")
            }
          }
        }
      }
  }




  // route to serve the index.html file
  val serveIndexRoute: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case request @ GET -> Root / "index" =>
      val filePath = Paths.get("src/main/scala/Server/Index.html") // Convert to java.nio.file.Path
      StaticFile.fromPath(Path.fromNioPath(filePath), Some(request)).getOrElseF(NotFound())
  }

  // Serve the game state as JSON
  val serveGameStateRoute: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "bomberman" :? GameSessionIdParam(gameSessionId) =>
      gameSessionsRef.get.flatMap { sessions =>
        sessions.get(UUID.fromString(gameSessionId)) match {
          case Some(gameSession) =>
            // Here you could serve the game state or redirect to a view rendering it
            Ok(gameSession.grid) // Optionally return JSON or render HTML dynamically
          case None =>
            NotFound(s"Game session with ID: $gameSessionId not found")
        }
      }
  }

  // Helper to extract query parameter for gameSessionId
  object GameSessionIdParam extends QueryParamDecoderMatcher[String]("gameSessionId")


  // Ensure the route that handles serving the game state is set up correctly
  val getGameStateRoute: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "game-state" / gameSessionId =>
      gameSessionsRef.get.flatMap { sessions =>
        sessions.get(UUID.fromString(gameSessionId)) match {
          case Some(gameSession) =>
            Ok(gameSession.grid.asJson)  // Return the grid for the session
          case None =>
            NotFound(s"Game session with ID: $gameSessionId not found")
        }
      }
  }





  // Combine all routes
  val routes: HttpRoutes[IO] =
    createGameSessionRoute <+> getAllGameSessionsRoute <+> joinGameRoute <+> serveIndexRoute <+> serveGameStateRoute <+>
      getGameStateRoute


  override def run(args: List[String]): IO[ExitCode] = {
    val httpApp = routes.orNotFound

    EmberServerBuilder.default[IO]
      .withHost(ipv4"127.0.0.1")
      .withPort(port"8080")
      .withHttpApp(httpApp)
      .build
      .useForever
      .as(ExitCode.Success)
  }
}
