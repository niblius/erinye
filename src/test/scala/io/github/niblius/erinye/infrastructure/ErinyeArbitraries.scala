package io.github.niblius.erinye.infrastructure

import java.time.Instant

import io.github.niblius.erinye.domain.articles.Article
import io.github.niblius.erinye.domain.users.User
import io.github.niblius.erinye.infrastructure.endpoint.{SignupRequest, UserUpdateRequest}
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary

import scala.util.Random

trait ErinyeArbitraries {
  val instant: Gen[Instant] = for {
    millis <- Gen.posNum[Long]
  } yield Instant.ofEpochMilli(millis)

  val userNameGen: Gen[String] = for {
    length <- Gen.choose(5, 16)
    username <- Gen.listOfN(length, Gen.alphaChar).map(_.mkString)
  } username

  val emailGen: Gen[String] = for {
    usernameLength <- Gen.choose(5, 10)
    username <- Gen
      .listOfN(usernameLength, Gen.asciiPrintableChar)
      .map(chars => chars.toArray.mkString(""))
    domain2 <- Gen.oneOf(List("gmail", "mail", "yahoo", "whatever"))
    domain1 <- Gen.oneOf(List("com", "org", "io", "me"))
  } yield s"$username@$domain2.$domain1"

  val passwordGen: Gen[String] = for {
    uppercaseLength <- Gen.choose(4, 8)
    lowercaseLength <- Gen.choose(4, 8)
    digitsLength <- Gen.choose(4, 8)
    nonAlphaNumericLength <- Gen.choose(0, 8)
    uppercase <- Gen.listOfN(uppercaseLength, Gen.alphaUpperChar)
    lowercase <- Gen.listOfN(lowercaseLength, Gen.alphaLowerChar)
    digits <- Gen.listOfN(digitsLength, Gen.numChar)
    nonAlphaNumeric <- Gen.listOfN(nonAlphaNumericLength, Gen.asciiChar)
    together = uppercase ++ lowercase ++ digits ++ nonAlphaNumeric
  } yield Random.shuffle(together).mkString

  /*
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
   */

  implicit val user: Arbitrary[User] = Arbitrary[User] {
    for {
      userName <- userNameGen
      email <- arbitrary[String]
      password <- arbitrary[String]
    } yield User(userName, email, Some(password))
  }

  implicit val userSignup: Arbitrary[SignupRequest] = Arbitrary[SignupRequest] {
    for {
      userName <- userNameGen
      email <- emailGen
      password <- userNameGen
    } yield SignupRequest(userName, email, password)
  }

  implicit val userUpdateRequest: Arbitrary[UserUpdateRequest] = Arbitrary[UserUpdateRequest] {
    for {
      name <- userNameGen
      email <- emailGen
      password <- passwordGen
    } yield UserUpdateRequest(Some(name), Some(email), Some(password), None)
  }
}

object ErinyeArbitraries extends ErinyeArbitraries
