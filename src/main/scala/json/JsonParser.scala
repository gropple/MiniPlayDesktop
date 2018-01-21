package json

import java.io.{StringWriter, Writer}

// These values cannot be placed on stack and are returned only
object JsonType extends Enumeration {
  type Type = Value
  val InsideObject,
  InsideArray,
  Key,
  StartOfKey,
  InStringField,
  InStringFieldEscapedChar,
  StartOfNumericField,
  StartOfArrayNumberField,
  StartOfArrayStringField,
  ArrayField,
  Field,
  BooleanField,
  NullField,
  FieldSeparator,
  EndOfObject,
  EndOfArray,
  ArrayElementSeparator,
  Error,
  End = Value
}

// These values can be placed on stack, i.e. is persistent state
object JsonStackType extends Enumeration {
  type Type = Value
  val InsideObject,
  InsideArray,
  Key = Value
}


case class Position(pos: Int, stack: collection.mutable.Stack[JsonStackType.Value])

// Doesn't seem to be one for ScalaJS, so rolling our own. ScalaJS has no reflection.
// This supports only a limited subset of JSON, basically what I produce
// Don't know how kosher this is, but we also support escaped strings:
//  """"""

/* Json escaping rules (JSONES1)

This isn't valid JSON, and we don't want to support it:

{\"name\":\"Graham Pople\",\"age\":33}

If we're embedding a JSON object
{name:'Graham "I'm No Hero" Pople',age:33}, which encodes to:
{"name":"Graham \"I'm No Hero\" Pople","age":33}, want:

{json:"{\"name\":\"Graham \\\"I'm No Hero\\\" Pople\",\"age\":33}"}

The multiple escapes are needed.  \\" wrecks parsers as they interpret it as a backslash then ending the json string.

When reading that back, want
json:{name:"Graham \"I'm No Hero\" Pople",age:33}

E.g. it's escaped on write, and deecaped on read (JSONES3)
We can then descape that string to

{"name":"Graham \"I'm No Hero\" Pople","age":33}

by replacing \" with " and then \\" with \"

None of this embedded-JSON escaping and unescaping can be automatic.  In the parsers, we don't know that it's an
embedded JSON.

(JSONES2) Reading
{"name":"Graham \"I'm No Hero\" Pople"}
should give
name:Graham "I'm No Hero" Pople
Otherwise we're escaping on write, and not unescaping on read, which seems weird.

(JSONES3) So the golden rule is we always escape on write, and de-escape on read
 */

class JsonReader(raw: String) {
  private var pos = 0

  private var stack = collection.mutable.Stack[JsonStackType.Value]()

  def dumpPos: String = {
    s"${pos} '${raw.substring(Math.max(0, pos - 20), pos)}'>>${raw(pos)}<<'${raw.substring(pos, Math.min(pos + 20, raw.size - 1))}'"
  }

  def getPosition() = Position(pos, stack.clone())

  def setPosition(position: Position) = {
    pos = position.pos
    stack = position.stack
  }


  def substring = raw.substring(pos)

  // Peek methods don't change state and don't advance us in the string
  def peekIsEndArray: Boolean = raw(pos) == ']'

  // Newer, better, more accurate parser than nextField
  def nextToken: (String, JsonType.Value) = {
    try {
      if (pos >= raw.length) {
        return ("", JsonType.End)
      }

      var c: Char = raw(pos)

      // Handle {"data": [
      while (c == ' ') {
        pos += 1
        c = raw(pos)
      }

      c match {
        case '{' => {pos += 1; stack.push(JsonStackType.InsideObject); return (c.toString, JsonType.InsideObject)}
        case '[' => {pos += 1; stack.push(JsonStackType.InsideArray); return (c.toString, JsonType.InsideArray)}
        case '}' => {pos += 1; stack.pop(); if (!stack.isEmpty && stack.head == JsonStackType.Key) stack.pop(); return (c.toString, JsonType.EndOfObject)}
        case ']' => {pos += 1; stack.pop(); if (!stack.isEmpty && stack.head == JsonStackType.Key) stack.pop(); return (c.toString, JsonType.EndOfArray)}
        case ',' => {
          pos += 1
          stack.head match {
            case JsonStackType.InsideArray => return (c.toString, JsonType.ArrayElementSeparator)
            case JsonStackType.InsideObject => return (c.toString, JsonType.FieldSeparator)
            case v => assert(false, s"unknown $v at $dumpPos")
          }
        }
        case _ =>
      }

      var state = stack.head match {
        case JsonStackType.InsideArray => JsonType.InsideArray
        case JsonStackType.InsideObject => JsonType.InsideObject
        case JsonStackType.Key => JsonType.Key // Last thing we found was a "key": , so we're looking for an Array, Object or Field
        case v => assert(false, s"unknown stack head type ${v} at $dumpPos")
      }
      var startPos = pos

      while (true) {

        // [>"graham">,"charlie"]
        if (state == JsonType.InsideArray || state == JsonType.ArrayElementSeparator) {
          c match {
              // Handle [true] and [false]
            case 't' =>
              assert(raw.substring(pos, pos + 4) == "true", s"expected true at $dumpPos")
              stack.pop()
              pos += 4
              return ("true", JsonType.BooleanField)
            case 'f' =>
              assert(raw.substring(pos, pos + 5) == "false", s"expected true at $dumpPos")
              stack.pop()
              pos += 5
              return ("false", JsonType.BooleanField)
            case '"' => {
              state = JsonType.StartOfArrayStringField; startPos = pos + 1
            }
            case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
              state = JsonType.StartOfArrayNumberField; startPos = pos
            }
            case _ =>
          }
        }
        else if (state == JsonType.InsideObject || state == JsonType.FieldSeparator) {
          c match {
            case '"' => {
              state = JsonType.StartOfKey; startPos = pos + 1
            }
            case _ =>
          }
        }
        else if (state == JsonType.StartOfKey) {
          c match {
            case ':' => {
              // Handle \"uuid\"
              pos += 1;
              stack.push(JsonStackType.Key);
              return (raw.substring(startPos, pos - 2), JsonType.Key)
            }
            case _ =>
          }
        }
        else if (state == JsonType.Key) {
          c match {
            case '"' => {
              state = JsonType.InStringField;
              startPos = pos + 1
            }
            case 'n' => {
              assert(raw.substring(pos, pos + 4) == "null", s"expected null at $dumpPos")
              stack.pop();
              pos += 4;
              return (null, JsonType.NullField)
            }
            case 't' =>
              assert(raw.substring(pos, pos + 4) == "true", s"expected true at $dumpPos")
              stack.pop()
              pos += 4
              return ("true", JsonType.BooleanField)
            case 'f' =>
              assert(raw.substring(pos, pos + 5) == "false", s"expected false at $dumpPos")
              stack.pop()
              pos += 5
              return ("false", JsonType.BooleanField)
            case _ =>
          }
          if (c.isDigit) state = JsonType.StartOfNumericField
        }
        else if (state == JsonType.InStringField) {
          c match {
            case '\\' => pos += 1 // (JSONES1) this is to handle escaped quotes
            case '"' => {
              stack.pop(); pos += 1; return (JsonWriter.unescape(raw.substring(startPos, pos - 1)), JsonType.Field)
            }
            case _ =>
          }
        }
        else if (state == JsonType.InStringFieldEscapedChar) {
          pos += 1
        }
        else if (state == JsonType.StartOfNumericField) {
          c match {
            case '}' => {
              stack.pop(); return (raw.substring(startPos, pos), JsonType.Field)
            }
            case ',' => {
              stack.pop(); return (raw.substring(startPos, pos), JsonType.Field)
            }
            case _ =>
          }
        }
        else if (state == JsonType.StartOfArrayStringField) {
          c match {
            case '"' => pos += 1; return (raw.substring(startPos, pos - 1), JsonType.ArrayField)
            case _ =>
          }
        }
        else if (state == JsonType.StartOfArrayNumberField) {
          c match {
//            case '"' => pos += 1; return (raw.substring(startPos, pos - 1), JsonType.ArrayField)
            case ',' => return (raw.substring(startPos, pos), JsonType.ArrayField)
            case ']' => return (raw.substring(startPos, pos), JsonType.ArrayField)
            case _ =>
          }
        }

        pos += 1
        assert(pos <= raw.size, s"${pos}")

        c = raw(pos)

      }
    }
    catch {
      case t: Throwable => System.out.println(s"Parse error at ${raw.substring(pos - 20).take(50)}: ${t}")
    }

    return (s"Error reading json at pos ${pos} state ${stack} json '${raw.substring(pos)}'", JsonType.Error)
  }

  def nextField: Tuple2[String, String] = {
    var t1 = nextToken
    while (t1._2 != JsonType.Key) {
      t1 = nextToken
    }
    var t2 = nextToken
    assert(t2._2 == JsonType.Field || t2._2 == JsonType.NullField, s"expected Field or NullField at $dumpPos")
    return (t1._1, t2._1)
  }

  def isNextFieldNull: Boolean = {
    val curPos = pos
    var t1 = nextToken
    while (t1._2 != JsonType.Key) {
      t1 = nextToken
    }
    var t2 = nextToken
    val result = (t2._2 == JsonType.NullField)
    if (!result) {
      pos = curPos
    }
    return result
  }

  def nextFieldAsString(): String = {
    val t = nextField
    t._2
  }

  def nextFieldAsInt(): Int = {
    val t = nextField
    t._2.toInt
  }

  def nextFieldAsDouble(): Double = {
    val t = nextField
    t._2.toDouble
  }

  def nextFieldAsBoolean(): Boolean = {
    nextFieldAsInt() == 1
  }

}


object JsonWriter {
  // http://stackoverflow.com/questions/42068/how-do-i-handle-newlines-in-json

  // FAILED   JSON.parse(({"json":"{\"hello\":\"world\n\"}"}).json)
  // SUCCESS  JSON.parse(({"json":"{\"hello\":\"world\\n\"}"}).json)
  // FAILED   JSON.parse(({"json":"{\"hello\":\"world\\\n\"}"}).json)
  // Note difference between control character \n and string "\n".  IntelliJ helpfully highlights former in blue.
  def escape(v: String): String = {
    var out = new StringBuilder
    val it = v.iterator
    var lastWasBackSlash = false
    while (it.hasNext) {
      var c = it.next()
      c match {
        case '\\' =>
          lastWasBackSlash = true
          out += '\\'
          out += '\\'
        //        case '\'' =>
        //          if (!lastWasBackSlash) {
        //            out += '\\'
        //          }
        //          out += '\''
        //          lastWasBackSlash = false
        case '"' =>
          //          if (!lastWasBackSlash) {
          out += '\\'
          //          }
          out += '"'
          lastWasBackSlash = false
        case '\t' =>
          if (!lastWasBackSlash) {
            out += '\\'
          }
          out += 't'
          lastWasBackSlash = false
        case '\n' =>
          if (!lastWasBackSlash) {
            out += '\\'
          }
          out += 'n'
          lastWasBackSlash = false
        case _ =>
          out += c
          lastWasBackSlash = false
      }
    }
    out.toString


    //    v.replace(""""""", """\"""").replace("""\\"""", """\\\"""").replace("""\n""", """\\n""").replace("\n","""\n""").replace("""\t""", """\\t""").replace("\t","""\t""")
  }
  //  def escape(v: String) = v.replace("\"", "\\\"")
  def unescape(v: String): String = {
    //    v.replace("""\"""", """"""").replace("""\\"""", """\"""").replace("""\n""", "\n").replace("""\\n""", "\\n").replace("\\\\\n", """\n""")
    //    v.replace("""\"""", """"""").replace("""\\"""", """\"""").replace("""[^\]\n""", "\n").replace("""\\""", """\""")
    var out = new StringBuilder
    var it = v.iterator
    while (it.hasNext) {
      var c = it.next()
      if (c == '\\') {
        var count = 1
        while (c == '\\') {
          c = it.next()
          if (c == '\\') {
            count += 1
          }
        }
        c match {
          //          case '\'' =>
          //            count match {
          //              case 1 => out ++= "'"
          //              case 2 => out ++= """\'"""
          //              case 3 => out ++= """\\'"""
          //            }
          case 'n' =>
            count match {
              case 1 => out ++= "\n"
              case 2 => out ++= """\n"""
              case 3 => out ++= """\\n"""
            }
          case 't' =>
            count match {
              case 1 => out ++= "\t"
              case 2 => out ++= """\t"""
              case 3 => out ++= """\\t"""
            }
          case '"' =>
            count match {
              case 1 => out ++= """""""
              //              case 2 => out ++= """\""""
              case 3 => out ++= """\""""
            }
          // Unicode
          case _ => out += c
        }
      }
      else {
        out += c
      }
    }
    out.toString
  }
  // Want to handle \"uuid\" but want to leave \"embeddedJson\":\"blah blah \"hello!\" blah\" alone
  //  def unescapeOnlyStartAndEnd(v:String) = v.stripSuffix("\"").stripPrefix("\"")

  // (JSONES1) Turn
  // {"name":"Graham \"I'm No Hero\" Pople","age":33}
  // into
  // {\"name\":\"Graham \\\"I'm No Hero\\\" Pople\",\"age\":33}
  //  def escapeJsonForEmbedding(v: String) = {
  //    v.replace(""""""", """\"""").replace("""\\"""", """\\\"""")
  //  }
  //
  //  // Reverese above
  //  def unescapeEmbeddedJson(v: String) = {
  //    v.replace("""\"""", """"""").replace("""\\"""", """\"""")
  //  }
}

object JsonWriterMode extends Enumeration {
  val Started,
  InsideObjectOrArrayNotWrittenField,
  InsideObjectOrArrayWrittenField,
  OutsideObjectOrArrayWrittenObject = Value
}

// Same as JsonReader. Don't have a cross-platform lib I can call from scalajs or JVM, so rolling my own. There are times
// when I've regretted this choice.
class JsonWriter(val sb: Writer = new StringWriter()) {
  def writeFieldBoolean(s: String, v: Boolean) = {
    val value = if (v) 1 else 0
    writeFieldInt(s, value)
  }

  //  val sb.append(new StringBuilder

  // 0 = Started
  // 1 = Inside { or [, not written a field
  // 2 = Inside { or [, written a field
  // 3 = Outside } or ], written a {}
  var mode = JsonWriterMode.Started

  def writeFieldString(key: String, field: String) = {
    if (mode == JsonWriterMode.InsideObjectOrArrayWrittenField) {
      sb.append(',')
    }
    sb.append('"')
    sb.append(key)
    sb.append("\":\"")
    sb.append(escape(field))
    sb.append('"')
    mode = JsonWriterMode.InsideObjectOrArrayWrittenField
  }

  def writeFieldRaw(key: String, field: String) = {
    if (mode == JsonWriterMode.InsideObjectOrArrayWrittenField) {
      sb.append(',')
    }
    sb.append('"')
    sb.append(key)
    sb.append("\":")
    sb.append(field)
    mode = JsonWriterMode.InsideObjectOrArrayWrittenField
  }

  def writeFieldNull(key: String) = {
    if (mode == JsonWriterMode.InsideObjectOrArrayWrittenField) {
      sb.append(',')
    }
    sb.append('"')
    sb.append(key)
    sb.append("\":null")
    mode = JsonWriterMode.InsideObjectOrArrayWrittenField
  }

  def writeFieldInt(key: String, field: Int) = {
    if (mode == JsonWriterMode.InsideObjectOrArrayWrittenField) {
      sb.append(',')
    }
    sb.append('"')
    sb.append(key)
    sb.append("\":")
    sb.append(field.toString)
    mode = JsonWriterMode.InsideObjectOrArrayWrittenField
  }

  def writeFieldDouble(key: String, field: Double) = {
    if (mode == JsonWriterMode.InsideObjectOrArrayWrittenField) {
      sb.append(',')
    }
    sb.append('"')
    sb.append(key)
    sb.append("\":")
    sb.append(field.toString)
    mode = JsonWriterMode.InsideObjectOrArrayWrittenField
  }

  def writeArrayValueString(field: String) = {
    if (mode == JsonWriterMode.InsideObjectOrArrayWrittenField) {
      sb.append(',')
    }
    sb.append('"')
    sb.append(escape(field))
    sb.append('"')
    mode = JsonWriterMode.InsideObjectOrArrayWrittenField
  }

  //  def escape(v: String): String = v.replaceAll("""[^\\]\"""", """\"""")

  // Simple escaping example:
  // var x = {desc:"a thing=\"blah\""};
  // JSON.stringify(x)
  // "{"desc":"a thing=\"blah\""}"

  // Complex - nesting some JSON inside an object:
  // var x = {json:"{desc:\"a thing=\"blah\"\"}"};
  // JSON.stringify(x)
  // "{"json":"{desc:\"a thing=\"blah\"\"}"}"

  // So the rule is, we want \", but not \\"
  def escape(v: String): String = JsonWriter.escape(v)// v.replace(""""""", """\"""").replace("""\\"""", """\"""")

  def startObject() = {
    if (mode == JsonWriterMode.OutsideObjectOrArrayWrittenObject) {
      sb.append(',')
    }
    sb.append('{')
    mode = JsonWriterMode.InsideObjectOrArrayNotWrittenField
  }


  def startNamedObject(v: String) = {
    if (mode == JsonWriterMode.OutsideObjectOrArrayWrittenObject || mode == JsonWriterMode.InsideObjectOrArrayWrittenField) {
      sb.append(',')
    }
    sb.append('"')
    sb.append(v)
    sb.append("\":{")
    mode = JsonWriterMode.InsideObjectOrArrayNotWrittenField
  }

  def startArray = {
    if (mode == JsonWriterMode.OutsideObjectOrArrayWrittenObject) {
      sb.append(',')
    }
    sb.append(s"[")
    mode = JsonWriterMode.InsideObjectOrArrayNotWrittenField
  }

  def startNamedArray(v: String) = {
    if (mode == JsonWriterMode.InsideObjectOrArrayWrittenField || mode == JsonWriterMode.OutsideObjectOrArrayWrittenObject) {
      sb.append(',')
    }
    sb.append('"')
    sb.append(v)
    sb.append("\":[")
    mode = JsonWriterMode.InsideObjectOrArrayNotWrittenField
  }

  def writeKey(v: String) = {
    if (mode == JsonWriterMode.InsideObjectOrArrayWrittenField || mode == JsonWriterMode.OutsideObjectOrArrayWrittenObject) {
      sb.append(',')
    }
    sb.append('"')
    sb.append(v)
    sb.append("\":")
    mode = JsonWriterMode.InsideObjectOrArrayNotWrittenField
  }

  def endArray() = {
    sb.append(']')
    mode = JsonWriterMode.OutsideObjectOrArrayWrittenObject
  }

  def endObject() = {
    sb.append('}')
    mode = JsonWriterMode.OutsideObjectOrArrayWrittenObject
  }

  override def toString(): String = {
    sb.toString
  }
}