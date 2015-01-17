package models

import org.scalatest.FunSuite

class ParticipantTest extends FunSuite {

  test("Equality") {
    assert(new Participant("same").equals(new Participant("same")))
  }

  test("Inequality") {
    assert(!new Participant("different").equals(new Participant("also different")))
  }

  test("HashCode Equality") {
    assert(new Participant("same").hashCode() === new Participant("same").hashCode())
  }

  test("HashCode Inequality") {
    assert(new Participant("different").hashCode() != new Participant("also different").hashCode())
  }

  test("toString is based on the fully-qualified class and name") {
    val participantString = new Participant("I am an individual").toString()
    assert(participantString.contains("I am an individual") &&
           participantString.startsWith("models.Participant{name :"))
  }

}
