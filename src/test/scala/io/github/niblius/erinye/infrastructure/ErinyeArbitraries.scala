package io.github.niblius.erinye.infrastructure

import java.time.Instant

import io.github.niblius.erinye.domain.articles.Article
import io.github.niblius.erinye.domain.users.User
import io.github.niblius.erinye.infrastructure.endpoint.user.SignupRequest
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ErinyeArbitraries {
  val userNameLength = 16
  val userNameGen: Gen[String] = Gen.listOfN(userNameLength, Gen.alphaChar).map(_.mkString)
  case class NonEmptyString(str: String)

  implicit val nonEmptyString = Arbitrary[NonEmptyString] {
    for {
      length <- Gen.posNum[Int]
      str <- Gen.listOfN(length + 1, Gen.alphaChar)
    } yield NonEmptyString(str.mkString)
  }

  implicit val instant = Arbitrary[Instant] {
    for {
      millis <- Gen.posNum[Long]
    } yield Instant.ofEpochMilli(millis)
  }

  implicit val article = Arbitrary[Article] {
    for {
      title <- arbitrary[String]
      description <- arbitrary[String]
      content <- arbitrary[String]
      numTags <- Gen.choose(0, 10)
      tags <- Gen.listOfN(numTags, Gen.alphaStr).map(_.toSet)
      dateCreated <- arbitrary[Instant]
    } yield Article(title, description, content, tags, dateCreated)
  }

  implicit val user = Arbitrary[User] {
    for {
      userName <- userNameGen
      email <- arbitrary[String]
      password <- arbitrary[String]
    } yield User(userName, email, Some(password))
  }

  implicit val userSignup = Arbitrary[SignupRequest] {
    for {
      userName <- userNameGen
      email <- arbitrary[String]
      password <- userNameGen // TODO: back arbitrary[String]
    } yield SignupRequest(userName, email, password)
  }
}

object ErinyeArbitraries extends ErinyeArbitraries
