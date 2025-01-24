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
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame

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
          response <- Ok(s"Game session: ${newGameSession.name}, ID: ${newGameSession.id}")
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

        if (gameSessionId.isEmpty) {
          BadRequest("Missing gameSessionId in the request")
        } else {
          gameSessionsRef.get.flatMap { sessions =>
            sessions.get(UUID.fromString(gameSessionId)) match {
              case Some(gameSession) =>
                // Add player to grid and update session
                val updatedGrid = GameSession.addPlayerToGrid(gameSession.grid, "newPlayer")

                // Create the updated session with the new grid and player
                val updatedSession = gameSession.copy(
                  players = gameSession.players :+ "newPlayer",  // Add player to players list
                  grid = updatedGrid
                )

                gameSessionsRef.update(sessions => sessions + (gameSession.id -> updatedSession)) *>
                  Ok(s"Successfully joined the game session: ${gameSession.name}")

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
      // Serve the static HTML file
      val filePath = Paths.get("src/main/scala/Server/Index.html")
      StaticFile.fromPath(Path.fromNioPath(filePath), Some(request)).getOrElseF(NotFound())
  }


  // Serve the game state
  val serveGameStateRoute: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case request @ GET -> Root / "bomberman" :? GameSessionIdParam(gameSessionId) =>
      gameSessionsRef.get.flatMap { sessions =>
        sessions.get(UUID.fromString(gameSessionId)) match {
          case Some(gameSession) =>
            // Serve the static Bomberman.html file
            val filePath = Paths.get("src/main/scala/Server/Bomberman.html")
            StaticFile.fromPath(Path.fromNioPath(filePath), Some(request)).getOrElseF(NotFound())
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
            Ok(gameSession.grid)
          case None =>
            NotFound(s"Game session with ID: $gameSessionId not found")
        }
      }
  }



  case class PlayerMove(player: String, direction: String)
  
    // WebSocket handler for game session
    def gameWebSocketRoute(ws: WebSocketBuilder2[IO]): HttpRoutes[IO] = HttpRoutes.of[IO] {
      case GET -> Root / "game" / "ws" / gameSessionId =>
        val sessionId = UUID.fromString(gameSessionId)
        gameSessionsRef.get.map { sessions =>
          sessions.get(sessionId) match {
            case Some(gameSession) =>
              val (gameSessionWithPlayers, gameStream) = handleGameSession(gameSession)
              val webSocket = ws.build(
                send = gameStream,
                receive = LazyList(???)
              )
              webSocket
            case None =>
              ???
          }
        }
    }

  def handleGameSession(gameSession: GameSession): (GameSession, LazyList[WebSocketFrame]) = {
    val turnMovesRef = Ref.unsafe[IO, Set[String]](Set.empty) // Set of players who have moved

    val gameStream = LazyList(gameSessionsRef.update { sessions =>
      // Mark this game as ongoing and add player moves
      val updatedGameSession = gameSession.copy()
      sessions + (gameSession.id -> updatedGameSession)
    })

    // Broadcast the game state to clients after each turn or move
    val sendStream: LazyList[WebSocketFrame] = LazyList(gameSessionsRef.get).flatMap { sessions =>
      if (gameSession.players.forall(player => turnMovesRef.get.unsafeRunSync().contains(player))) {
        // All players have moved, so we can proceed to the next turn
        // Broadcast the updated grid and reset player moves
        val updatedGrid = gameSession.grid.asJson
        turnMovesRef.update(_ => Set.empty) // Reset for the next round
        LazyList(WebSocketFrame.Text(updatedGrid.noSpaces))
      } else {
        // Not all players have moved yet, wait for the next update
        LazyList.empty
      }
    }

    (gameSession, sendStream)
  }


    def handlePlayerMove(gameSession: GameSession, player: String, direction: String): GameSession = {
      val newGrid = GameSession.movePlayer(gameSession.grid, direction)
      gameSession.copy(grid = newGrid, players = gameSession.players)
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