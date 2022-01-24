package merino

import probably.*
import gossamer.*
import jovian.*
import eucalyptus.*
import rudiments.*

import unsafeExceptions.canThrowAny
given Log(Everything |-> SystemOut)

object Tests extends Suite(t"Merino tests"):
  def run(using Runner): Unit =
    val tests = (Unix.Pwd / t"tests" / t"test_parsing").directory(Expect)
    val tests2 = (Unix.Pwd / t"tests" / t"test_transform").directory(Expect)
    
    val file: Bytes = test(t"Read file"):
      (Unix.Pwd / t"huge.json").file(Expect).read[DataStream](1.mb).slurp(200.mb)
    .check(_ => true)
    
    (tests.files ++ tests2.files).foreach:
      file =>
        if file.name.startsWith(t"n_")
        then
          test(t"Negative test: ${file.name.drop(2).drop(5, Rtl)}"):
            try
              Json.parse(file.read[DataStream](10.kb))
              Left(t"success")
            catch
              case err: JsonParseError => err match
                case JsonParseError(_, msg) => Right(msg)
              case err: Throwable  =>
                err.printStackTrace()
                Right(err.toString.show)
          .check(_.isRight)
        else
          test(t"Positive test: ${file.name.drop(5, Rtl)}"):
            try
              val j = Json.parse(file.read[DataStream](1.mb))
              Right(j.show)
            catch
              case err: JsonParseError => err match
                case JsonParseError(_, msg) => Left(msg)
              case err: Throwable        =>
                err.printStackTrace()
                Left(err.toString.show)
          .check(_.isRight)
    
    for i <- 1 to 20 do
      test(t"Read huge file"):
        Json.parse(LazyList(file))
      .check(_ => true)

      test(t"Parse with Jawn"):
        import org.typelevel.jawn.*, ast.*
        JParser.parseFromByteBuffer(java.nio.ByteBuffer.wrap(file.unsafeMutable).nn)
      .check(_ => true)


// given realm: Realm = Realm(t"tests")

@main def run(): Unit =
  println("STARTING")
  Tests.main(IArray[Text]())
  println("Finishing")
  Tests.main(IArray[Text]())