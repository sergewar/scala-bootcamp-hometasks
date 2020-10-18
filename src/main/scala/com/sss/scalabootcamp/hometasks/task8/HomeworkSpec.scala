package com.sss.scalabootcamp.hometasks.task8

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZonedDateTime}

import cats.instances.either._
import cats.instances.list._
import cats.syntax.traverse._
import io.circe
import io.circe.generic.JsonCodec
import io.circe.parser._
import org.scalatest.EitherValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scalaj.http.Http

/**
 * HOMEWORK:
 *
 * Some classes and generated JSON codecs are provided for NBA API.
 * Unfortunately, they don't work as expected out of the box.
 * The task is to fix (rewrite) some of the codecs to make tests pass.
 * You are not supposed to change anything in _class_ HomeworkSpec,
 * instead of it you are supposed to change whatever you want inside _companion object_ for HomeworkSpec.
 *
 * It would be nice to avoid using Encoder/Decoder.forProductN where you specify all field names
 */
class HomeworkSpec extends AnyWordSpec with Matchers with EitherValues {

  import HomeworkSpec._

  "NBA JSON API client" should {
    "get info about today games" in {
      val date = LocalDate.now()
      val scoreboardOrError = fetchScoreboard(date)
      val scoreboard = scoreboardOrError.getOrElse(fail(scoreboardOrError.toString))
      val allGameIds = scoreboard.games.map(_.gameId)
      val gameInfosOrError = allGameIds.map(fetchGameInfo(date, _)).sequence
      gameInfosOrError.getOrElse(fail(gameInfosOrError.toString))
      succeed
    }

    "fetch games for 14 Feb 2020" in {
      val date = LocalDate.of(2020, 2, 14)
      val scoreboardOrError = fetchScoreboard(date)
      val scoreboard = scoreboardOrError.getOrElse(fail(scoreboardOrError.toString))
      val allGameIds = scoreboard.games.map(_.gameId)
      val gameInfosOrError = allGameIds.map(fetchGameInfo(date, _)).sequence
      val gameInfos = gameInfosOrError.getOrElse(fail(gameInfosOrError.toString))
      gameInfos.size must be(1)
    }
  }
}


object HomeworkSpec {

  @JsonCodec final case class TeamTotals(
    assists               : String,
    full_timeout_remaining: String,
    plusMinus             : String
  )

  @JsonCodec final case class TeamBoxScore(totals: TeamTotals)

  @JsonCodec final case class GameStats(
    hTeam        : TeamBoxScore,
    vTeam        : TeamBoxScore,
    timesTied    : String,
    leadChanges  : String,
    activePlayers: List[ActivePlayer]
  )

  @JsonCodec final case class PrevMatchup(
    gameDate: String,
    gameId  : String
  )

  @JsonCodec final case class BoxScore(
    basicGameData  : Game,
    previousMatchup: PrevMatchup,
    stats          : Option[GameStats]
  )

  @JsonCodec final case class JustScore(score: String)

  @JsonCodec final case class TeamStats(
    teamId    : String,
    linescore : List[JustScore],
    win       : String,
    loss      : String,
    seriesWin : String,
    seriesLoss: String,
    score     : String,
    triCode   : String
  )

  @JsonCodec final case class ActivePlayer(
    personId     : String,
    firstName    : String,
    lastName     : String,
    jersey       : String,
    teamId       : String,
    isOnCourt    : Boolean,
    points       : String,
    pos          : String,
    position_full: String,
    player_code  : String,
    min          : String,
    fgm          : String,
    fga          : String,
    fgp          : String,
    ftm          : String,
    fta          : String,
    ftp          : String,
    tpm          : String,
    tpa          : String,
    tpp          : String,
    offReb       : String,
    defReb       : String,
    totReb       : String,
    assists      : String,
    pFouls       : String,
    steals       : String,
    turnovers    : String,
    blocks       : String,
    plusMinus    : String,
    dnp          : String,
    sortKey      : SortKey,
  )

  @JsonCodec final case class SortKey(
    name     : Int,
    pos      : Int,
    points   : Int,
    min      : Int,
    fgm      : Int,
    fga      : Int,
    fgp      : Int,
    ftm      : Int,
    fta      : Int,
    ftp      : Int,
    tpm      : Int,
    tpa      : Int,
    tpp      : Int,
    offReb   : Int,
    defReb   : Int,
    totReb   : Int,
    assists  : Int,
    pFouls   : Int,
    steals   : Int,
    turnovers: Int,
    blocks   : Int,
    plusMinus: Int
  )

  @JsonCodec final case class GameDuration(hours                                                                    : String, minutes: String)

  @JsonCodec final case class Arena(
    city      : String,
    country   : String,
    isDomestic: Boolean,
    name      : String,
    stateAbbr : String
  )

  @JsonCodec final case class Game(
    arena         : Arena,
    attendance    : String,
    gameDuration  : GameDuration,
    gameId        : String,
    gameUrlCode   : String,
    hTeam         : TeamStats,
    isBuzzerBeater: Boolean,
    startTimeUTC  : ZonedDateTime,
    endTimeUTC    : Option[ZonedDateTime],
    vTeam         : TeamStats
  )

  @JsonCodec final case class Scoreboard(games: List[Game], numGames: Int)

  private def fetchScoreboard(date: LocalDate): Either[circe.Error, Scoreboard] = {
    val dateString = date.format(DateTimeFormatter.BASIC_ISO_DATE)
    val body = Http(s"https://data.nba.net/10s/prod/v1/$dateString/scoreboard.json").asString.body
    decode[Scoreboard](body)
  }

  private def fetchGameInfo(date: LocalDate, gameId: String): Either[circe.Error, BoxScore] = {
    val dateString = date.format(DateTimeFormatter.BASIC_ISO_DATE)
    val body = Http(s"https://data.nba.net/10s/prod/v1/$dateString/${gameId}_boxscore.json").asString.body
    decode[BoxScore](body)
  }
}

