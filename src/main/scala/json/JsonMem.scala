package json

import scala.collection.mutable.ArrayBuffer
import scala.language.dynamics

class JsonMem extends Dynamic {
  val inner = collection.mutable.Map.empty[String, Any]

  def applyDynamic(methodName: String)(args: Any*): JsonMem = {
    inner(methodName).asInstanceOf[JsonMem]
  }

  def selectDynamic(methodName: String): Any = {
    inner(methodName)
  }

  def get(name: String): Option[Any] = {
    inner.get(name)
  }

  def getJ(name: String): Option[JsonMem] = {
    inner.get(name).map(_.asInstanceOf[JsonMem])
  }

  def writeRecurse(json: JsonWriter): Unit = {
    json.startObject()
    inner.foreach(in => {
      in._2 match {
        case v: JsonMem =>
          json.startNamedObject(in._1)
          v.writeRecurse(json)
        case v: Double =>    json.writeFieldDouble(in._1, v)
        case v: Boolean =>   json.writeFieldBoolean(in._1, v)
        case v: Int =>       json.writeFieldInt(in._1, v)
        case v: String =>    json.writeFieldString(in._1, v)
        case _ =>            json.writeFieldString(in._1, in._2.toString)
      }
    })
    json.endObject()
  }

  def write(): String = {
    val json = new JsonWriter()
    writeRecurse(json)
    json.toString()
  }
}

object JsonMem {
  protected def array(reader: JsonReader, benprickleMode: Boolean = false): Seq[Any] = {
    val out = ArrayBuffer.empty[Any]
    var done = false
    while (!done) {
      val n = reader.nextToken
      n._2 match {
        case JsonType.InsideArray =>
          out += array(reader, benprickleMode)
        case JsonType.EndOfArray => done = true
        case JsonType.InsideObject =>
          val v = recurse(reader, benprickleMode)
          out += v
        case JsonType.ArrayField =>
          out += n._1
        case JsonType.ArrayElementSeparator =>
      }
    }
    out
  }

  protected def recurse(reader: JsonReader, benprickleMode: Boolean = false): JsonMem = {
    val out = new JsonMem
    var done = false

    while (!done) {
      val key = reader.nextToken
      val value = reader.nextToken

      value._2 match {
        case JsonType.InsideObject =>
          val pos = reader.getPosition()
          val peek1 = reader.nextToken
          val pos5 = reader.getPosition()
          val peek2 = reader.nextToken
          val isBenPrickleOption = if (benprickleMode && (peek1._1 == "elems" || peek1._1 == "#elems") && peek2._2 == JsonType.InsideArray) {
            val pos3 = reader.getPosition()
            val peek3 = reader.nextToken
            val pos2 = reader.getPosition()
            val peek4 = reader.nextToken

            // Handle "param1": {"elems": [1.0]}
            // Could encode as an Option, but that's silly really - JSON indicates missing values through them just
            // not being there, not as Options
            if (peek4._2 == JsonType.EndOfArray) {
              out.inner += key._1 -> peek3._1
              assert (reader.nextToken._2 == JsonType.EndOfObject)
            }
            else if (peek4._2 == JsonType.EndOfObject) {
              // "discountCode": {"elems": []}
              // This is how benprickle encodes None. We're simply not going to write the value
              // (JSONEMPTYSEQ1) Downside of this is that there is zero difference in encoded JSON between a None Option, and an empty
              // sequence
            }
            else if (peek4._2 == JsonType.ArrayElementSeparator) {
              // {"seeds": {"elems": ["string 1", "string 2"]}
              reader.setPosition(pos3)
              val v = array(reader, benprickleMode)
              out.inner += key._1 -> v
              // Read the '}'
              reader.nextToken
            }
            else {
              reader.setPosition(pos2)
              // Handle "signupStatus": {"elems": [{"codeSent": true, "codeSubmitted": true}]}
              if (peek3._2 == JsonType.InsideObject) {
                val v = recurse(reader, benprickleMode)
                val after1 = reader.nextToken

                // (BENPRICKLE1) This could be an Option(CaseClass) or a Seq(CaseClass)
                after1._2 match {
                  case JsonType.EndOfArray =>
                    out.inner += key._1 -> v
                    assert(after1._2 == JsonType.EndOfArray, after1)
                    assert (reader.nextToken._2 == JsonType.EndOfObject)

                  case JsonType.ArrayElementSeparator =>
                    val arr = array(reader, benprickleMode)
                    val fin = arr :+ v
                    out.inner += key._1 -> fin

                }
              }
            }
            true
          }
          else {
            reader.setPosition(pos)
            false
          }

          if (!isBenPrickleOption) {
            // Handle "items: {}"
            if (peek1._2 != JsonType.EndOfObject) {
              val v = recurse(reader, benprickleMode)
              out.inner += key._1 -> v
            }
            else {
              out.inner += key._1 -> new JsonMem
              reader.setPosition(pos5)
            }
          }
        case JsonType.InsideArray =>
          val v = array(reader, benprickleMode)
          out.inner += key._1 -> v
        case _ =>
          out.inner += key._1 -> value._1
      }

      val next = reader.nextToken
      next._2 match {
        case JsonType.End | JsonType.EndOfObject => done = true
        case JsonType.FieldSeparator | JsonType.ArrayElementSeparator =>
        case _ =>
          assert (false, s"Unexpected ${next} at ${key} ${value}")

      }
    }

    out
  }

  def read(in: String, benprickleMode: Boolean = false): JsonMem = {
    val reader = new JsonReader(in)
    val t = reader.nextToken
    assert (t._1 == "{", t)
    recurse(reader, benprickleMode)
  }
}
