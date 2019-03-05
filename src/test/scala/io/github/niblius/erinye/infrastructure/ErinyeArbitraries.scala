package io.github.niblius.erinye.infrastructure

import java.time.Instant

import io.github.niblius.erinye.domain.articles.Article
import io.github.niblius.erinye.domain.authentication.SignupRequest
import io.github.niblius.erinye.domain.users.User
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

trait ErinyeArbitraries {
  val userNameLength = 16
  val userNameGen: Gen[String] = Gen.listOfN(userNameLength, Gen.alphaChar).map(_.mkString)

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
    } yield User(userName, email, password)
  }

  implicit val userSignup = Arbitrary[SignupRequest] {
    for {
      userName <- userNameGen
      email <- arbitrary[String]
      password <- arbitrary[String]
    } yield SignupRequest(userName, email, password)
  }
}

object ErinyeArbitraries extends ErinyeArbitraries