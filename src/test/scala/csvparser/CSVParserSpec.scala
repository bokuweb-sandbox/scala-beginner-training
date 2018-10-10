package csvparser

import java.io.File

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

/*
4	Orion	backlog	Space Traveller
5	Merlin	backlog	Magic Speller
6	Robin	backlog	Administrator
7	Harry	backlog	Guest

 */
class CSVParserSpec extends FlatSpec with Matchers {

  def getFileContent(path: String): String =
    Source.fromFile(new File(path)).mkString
  it should "parse CSV content" in {
    val result =
      CSVParser.parse(getFileContent("testfiles/csvparser/valid.csv"))
    result.isRight should be(true)
    result should be(
      Result.success(
        Seq(
          Map(
            "id" -> "1",
            "name" -> "Marc",
            "project" -> "backlog",
            "role" -> "Developer"
          ),
          Map(
            "id" -> "2",
            "name" -> "Ben",
            "project" -> "backlog",
            "role" -> "Developer"
          ),
          Map(
            "id" -> "3",
            "name" -> "John",
            "project" -> "backlog",
            "role" -> "Developer"
          )
        )
      )
    )
  }

  it should "parse an user with valid data" in {
    val result =
      CSVParser.parse(getFileContent("testfiles/csvparser/valid.csv"))

    result.isRight should be(true)
    result.map { content =>
      val users = UserParser.fromCSV(content)

      users.isValid should be(true)
      users.map { users =>
        {
          println(users)
          users should be(
            Seq(
              User(
                id = Id(1),
                name = "Marc",
                project = "backlog",
                role = Developer
              ),
              User(
                id = Id(2),
                name = "Ben",
                project = "backlog",
                role = Developer
              ),
              User(
                id = Id(3),
                name = "John",
                project = "backlog",
                role = Developer
              )
            )
          )
        }
      }
    }
  }

  it should "be invalid since user data are incorrect" in {
    val result =
      CSVParser.parse(getFileContent("testfiles/csvparser/invalid_users.csv"))
    result.isRight should be(true)
    result.map { content =>
      val users = UserParser.fromCSV(content)
      users.isInvalid should be(true)
    }
  }
}
