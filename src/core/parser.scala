package merino

import annotation.*
import gossamer.*
import rudiments.*
import scala.collection.mutable.{HashMap, ListBuffer, ArrayBuffer}

import stdouts.stdout

enum Json:
  case Number(value: Long | BigDecimal | Double)
  case JString(value: String)
  case JObject(values: HashMap[String, Json])
  case JArray(values: IArray[Json])
  case True
  case False
  case Null

  override def toString(): String = this match
    case Number(value)   => value.toString
    case JString(value)  => t"\"${value}\"".s
    case JObject(values) => values.map { (k, v) => t"\"$k\": $v" }.join(t"{ ", t", ", t" }").s
    case JArray(values) => values.map(_.show).join(t"[ ", t", ", t" ]").s
    case True            => "true"
    case False           => "false"
    case Null            => "null"

object AsciiByte:
  inline final val OpenBracket: 91 = 91 // '['
  inline final val CloseBracket: 93 = 93 // ']'
  inline final val OpenBrace: 123 = 123 // '{'
  inline final val CloseBrace: 125 = 125 // '}'
  inline final val Comma: 44 = 44 // ','
  inline final val Colon: 58 = 58 // ':'
  inline final val Quote: 34 = 34 // '"'
  inline final val Minus: 45 = 45 // '-'
  inline final val Plus: 43 = 43 // '+'
  inline final val Slash: 47 = 47 // '/'
  inline final val Period: 46 = 46 // '.'
  inline final val Backslash: 92 = 92 // '\\'
  inline final val Num0: 48 = 48 //'0'
  inline final val Num1: 49 = 49 //'1'
  inline final val Num2: 50 = 50 //'2'
  inline final val Num3: 51 = 51 //'3'
  inline final val Num4: 52 = 52 //'4'
  inline final val Num5: 53 = 53 //'5'
  inline final val Num6: 54 = 54 //'6'
  inline final val Num7: 55 = 55 //'7'
  inline final val Num8: 56 = 56 //'8'
  inline final val Num9: 57 = 57 //'9'
  inline final val LowerA: 97 = 97 // 'a'
  inline final val LowerB: 98 = 98 // 'b'
  inline final val LowerC: 99 = 99 // 'c'
  inline final val LowerD: 100 = 100 // 'd'
  inline final val LowerE: 101 = 101 // 'e'
  inline final val LowerF: 102 = 102 // 'f'
  inline final val LowerN: 110 = 110 // 'n'
  inline final val LowerL: 108 = 108 // 'l'
  inline final val LowerR: 114 = 114 // 'r'
  inline final val LowerS: 115 = 115 // 's'
  inline final val LowerT: 116 = 116 // 't'
  inline final val LowerU: 117 = 117 // 'u'
  inline final val UpperA: 65 = 65 // 'A'
  inline final val UpperB: 66 = 66 // 'B'
  inline final val UpperC: 67 = 67 // 'C'
  inline final val UpperD: 68 = 68 // 'D'
  inline final val UpperE: 69 = 69 // 'E'
  inline final val UpperF: 70 = 70 // 'F'
  inline final val Tab: 9 = 9 // '\t'
  inline final val Space: 32 = 32 // ' '
  inline final val Newline: 10 = 10 // '\n'
  inline final val Return: 13 = 13 // '\r'
import AsciiByte.*

object Flag:
  final val DecimalPoint = 1 << 0
  final val Exponent = 1 << 1
  final val Negative = 1 << 2
  final val NegativeExponent = 1 << 3
  final val LeadingZero = 1 << 4
  final val Large = 1 << 5
  final val Terminible = 1 << 6

case class JsonParseError(pos: Int, message: Text) extends Exception(message.s)

object Json:
  def parse(stream: DataStream): Json throws JsonParseError | StreamCutError = try
    val block: Array[Byte] = stream.head.unsafeMutable
    val penultimate = block.length - 1
    var cur: Int = 0

    if penultimate > 2 && block(0) == -17 && block(1) == -69 && block(2) == -65 then cur = 3

    inline def current: Byte = block(cur)
    inline def next(): Unit = cur += 1

    inline def skip(): Unit =
      while
        (current: @switch) match
          case Space | Return | Newline | Tab => true
          case 11 | 12 | 14 | 15 | 16 | 17 |
                   18 | 19 | 20 | 21 | 22 |
                   23 | 24 | 25 | 26 | 27 |
                   28 | 29 | 30 | 31          => false
          case _                              => false
      do next()

    def abort(message: Text): Nothing = throw JsonParseError(cur, message)

    def parseValue(): Json =
      (current: @switch) match
        case Quote                                     => next(); parseString()
        case Minus                                     => next(); parseNumber(cur, true)
        case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 |
                 Num6 | Num7 | Num8 | Num9             => parseNumber(cur, false)
        case OpenBracket                               => next(); parseArray()
        case LowerF                                    => next(); parseFalse()
        case LowerN                                    => next(); parseNull()
        case LowerT                                    => next(); parseTrue()
        case OpenBrace                                 => next(); parseObject()
        case ch                                        => abort(t"expected a value but found '${ch.toChar}'")

    def parseObject(): Json.JObject =
      val items: HashMap[String, Json] = HashMap()
      var continue = true
      while continue do
        skip()
        current match
          case Quote =>
            next()
            val str = parseString()
            skip()
            current match
              case Colon =>
                next()
                skip()
                val value = parseValue()
                skip()
                current match
                  case CloseBrace =>
                    next()
                    continue = false
                  case Comma =>
                    next()
                    skip()
                    items += (str.value -> value)
                  case ch  => abort(t"unexpected character: '${ch.toChar}'")
              case ch => abort(t"expected a colon but found '${ch.toChar}'")
          case CloseBrace =>
            if !items.isEmpty then abort(t"closing brace appears after comma")
            next()
            continue = false
          case ch => abort(t"expected a string but found '${ch.toChar}'")
      
      Json.JObject(items)

    def parseArray(): Json.JArray =
      val items: ArrayBuffer[Json] = ArrayBuffer()
      var continue = true
      while continue do
        skip()
        current match
          case CloseBracket =>
            if !items.isEmpty then abort(t"closing bracket appears after comma")
            continue = false
          case ch =>
            val value = parseValue()
            skip()
            current match
              case Comma        => items += value
              case CloseBracket => continue = false
              case ch           => abort(t"expected ',' or ']' but found '${ch.toChar}'")
        
        next()
      
      Json.JArray(items.toArray.unsafeImmutable)

    def parseString(): Json.JString =
      val start = cur
      var continue = true
      var difference = 0
      var size = 0

      while continue do
        current match
          case Quote =>
            continue = false
            size = cur - start - difference
          
          case Tab | Newline | Return =>
            abort(t"invalid unescaped whitespace character in string")

          case Backslash =>
            next()
            current match
              case LowerU =>
                next(); next(); next(); next()
                difference += 5
              case ch =>
                difference += 1
          
          case ch =>
            // FIXME: Optimization opportunity by reversing and nesting these
            if (ch & 192) == 192 then
              if (ch & 224) == 192 then
                difference += 1
                next()
              else if (ch & 240) == 224 then
                difference += 2
                next(); next()
              else if (ch & 248) == 240 then
                difference += 3
                next(); next(); next()

        next()

      val array: Array[Char] = new Array(size)

      continue = true
      cur = start

      var offset: Int = 0
      
      inline def append(char: Char): Unit =
        array(offset) = char
        offset += 1

      inline def parseUnicode(): Char =
        next()
        var acc = fromHex(current)*4096
        next()
        acc = acc + fromHex(current)*256
        next()
        acc = acc + fromHex(current)*16
        next()
        acc = acc + fromHex(current)
        acc.toChar

      while continue do
        current match
          case Quote =>
            next()
            continue = false
          
          case Backslash =>
            next()
            current match
              case Quote     => append('"')
              case Slash     => append('/')
              case Backslash => append('\\')
              case LowerB    => append('\b')
              case LowerF    => append('\f')
              case LowerN    => append('\n')
              case LowerR    => append('\r')
              case LowerT    => append('\t')
              case LowerU    => append(parseUnicode())
              case ch        => abort(t"the character '$ch' should not be escaped")
            next()

          case ch =>
            if ch >= 0 && ch < 32 then abort(t"unescaped control character '$ch'")
            // FIXME: Optimization opportunity by reversing and nesting these
            var char = 0
            if (ch & 192) == 192 then
              if (ch & 224) == 192 then
                char = ((ch & 31) << 6)
                next()
                char += current & 63
                append(char.toChar)
              else if (ch & 240) == 224 then
                char = ((ch & 31) << 12)
                next()
                char += (current & 63) << 6
                next()
                char += current & 63
                append(char.toChar)
              else if (ch & 248) == 240 then
                char = ((ch & 31) << 18)
                next()
                char += (current & 63) << 12
                next()
                char += (current & 63) << 6
                next()
                char += current & 63
                append(char.toChar)
              else append(ch.toChar)
            
            next()
      
      Json.JString(String(array))
              
    inline def fromHex(byte: Byte): Byte = (byte: @switch) match
      case Num0  => 0
      case Num1  => 1
      case Num2  => 2
      case Num3  => 3
      case Num4  => 4
      case Num5  => 5
      case Num6  => 6
      case Num7  => 7
      case Num8  => 8
      case Num9  => 9
      case _   => (byte: @switch) match
        case LowerA  => 10
        case LowerB => 11
        case LowerC => 12
        case LowerD => 13
        case LowerE => 14
        case LowerF => 15
        case _   => (byte: @switch) match
          case UpperA  => 10
          case UpperB  => 11
          case UpperC  => 12
          case UpperD  => 13
          case UpperE  => 14
          case UpperF  => 15
          case _   => abort(t"expected a hexadecimal digit")

    def parseFalse(): Json.False.type =
      if current != LowerA then abort(t"expected 'false'")
      next()
      if current != LowerL then abort(t"expected 'false'")
      next()
      if current != LowerS then abort(t"expected 'false'")
      next()
      if current != LowerE then abort(t"expected 'false'")
      next()
      Json.False
    
    def parseTrue(): Json.True.type =
      if current != LowerR then abort(t"expected 'true'")
      next()
      if current != LowerU then abort(t"expected 'true'")
      next()
      if current != LowerE then abort(t"expected 'true'")
      next()
      Json.True
    
    def parseNull(): Json.Null.type =
      if current != LowerU then abort(t"expected 'null'")
      next()
      if current != LowerL then abort(t"expected 'null'")
      next()
      if current != LowerL then abort(t"expected 'null'")
      next()
      Json.Null

    def parseNumber(start: Int, negative: Boolean): Json.Number =
      import Flag.*
      var mantissa: Long = 0L
      lazy val bigDecimal: StringBuffer = StringBuffer()
      var fractional: Long = 0L
      var exponent: Long = 0L
      var continue: Boolean = true

      var hasExponent: Boolean = false
      var terminible: Boolean = false
      var decimalPoint: Boolean = false
      var negativeExponent: Boolean = false
      var large: Boolean = false
      var leadingZero: Boolean = current == Num0
      
      var divisor: Double = 1.0
      var result: Double | BigDecimal | Long = 0L
      
      if current == Period then abort(t"cannot start a number with a decimal point")

      while continue do
        (current: @switch) match
          case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
            if large then bigDecimal.append(current.toChar)
            else if hasExponent then exponent = exponent*10 + current - 48
            else if decimalPoint then
              fractional = fractional*10 + current - 48
              divisor *= 10
            else
              val newMantissa = mantissa*10 + current - 48
              if newMantissa < mantissa then
                if negative then bigDecimal.append('-')
                bigDecimal.append(mantissa.toString)
                bigDecimal.append(current.toChar)
                large = true
              else mantissa = newMantissa
            terminible = true
            if cur == penultimate then continue = false
            next()
          
          case Period =>
            if large then bigDecimal.append('.')
            if decimalPoint then abort(t"a number can have at most one decimal point")
            if hasExponent then abort(t"the exponent must be an integer")
            decimalPoint = true
            terminible = false
            next()
          
          case UpperE | LowerE =>
            if large then bigDecimal.append('e')
            if hasExponent then abort(t"a number can have at most one exponent")
            if !terminible then abort(t"the mantissa requires at least one digit after the decimal point")
            
            next()
            
            (current: @switch) match
              case Minus =>
                if large then bigDecimal.append('-')
                negativeExponent = true
                next()
              
              case Plus =>
                next()
              
              case _ =>
                ()
            
            hasExponent = true
            terminible = false
          
          case Tab | Return | Newline | Space | Comma | CloseBracket | CloseBrace =>
            if !terminible then abort(t"the number was incomplete")
            if leadingZero && mantissa != 0 then abort(t"the number should not have a leading zero")
            
            if mantissa == 0 && !decimalPoint && hasExponent && exponent != 1
            then abort(t"the integer 0 cannot have an exponent other than 1")
            
            result = 
              if large then
                try BigDecimal(bigDecimal.toString)
                catch
                  case err: NumberFormatException => Out.println(t"Tried to parse '${bigDecimal.toString}'"); throw err
              else if decimalPoint then (mantissa + fractional/divisor)*(if negative then -1 else 1)
              else if negative then -mantissa else mantissa
            
            continue = false
          
          case ch =>
            abort(t"found an unexpected character '$ch'")
      
      Json.Number(result)
    
    skip()
    val result = parseValue()
    while cur < block.length
    do
      current match
        case Tab | Newline | Return | Space => ()
        case other =>
          abort(t"spurious extra characters were found at the end")
      
      next()

    result
  catch
    case err: ArrayIndexOutOfBoundsException =>
      throw JsonParseError(stream.head.length, t"the JSON was not properly terminated")