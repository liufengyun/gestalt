object Driver {
  type Test = () => Unit
  type Suite = collection.mutable.HashMap[String, () => Unit]
  val suites = new collection.mutable.HashMap[String, Suite]

  def main(args: Array[String]): Unit = {
    // setup tests
    new MacrosTest
    new QuasiquoteSuite

    var success = false
    var total = 0
    var totalFailed = 0
    suites.foreach { case (name, tests) =>
      var failed = 0
      println(s"\n\n======= $name =========")
      tests.foreach { case (name, code) =>
        try {
          code(); success = true
        }
        catch {
          case ex: Throwable =>
            success = false
            failed += 1
            print(Console.WHITE)
            ex.printStackTrace()
        }

        if (success)
          println(s"${Console.GREEN}- $name")
        else
          println(s"${Console.RED}- $name")
      }

      total += tests.size
      totalFailed += failed

      println(s"${Console.WHITE}Tests: ${tests.size}, Success: ${tests.size - failed}, Failed: $failed")
    }

    println(s"\n\n [${suites.size} Suites] ${Console.WHITE}Tests: $total, Success: ${total - totalFailed}, Failed: $totalFailed")
  }
}

class TestSuite {
  def test(name: String)(code: => Unit): Unit = {

    val suiteName = this.getClass.getName
    val suite = if (!Driver.suites.contains(suiteName)) {
      val s = new Driver.Suite()
      Driver.suites += suiteName -> s
      s
    } else Driver.suites(suiteName)

    assert(!suite.contains(name))
    suite.update(name, () => code)
  }
}

object Decorators {
  implicit class Ops(lhs: Any) extends AnyVal {
    def ===(rhs: Any) =
      if (lhs == rhs) true
      else {
        println(Console.WHITE + "lhs: " + lhs)
        println(Console.WHITE + "rhs: " + rhs)
        false
      }

  }
}

