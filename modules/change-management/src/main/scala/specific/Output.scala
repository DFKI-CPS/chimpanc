package specific

trait TaskOutput {
  def progress(value: Int): Unit
  def info(msg: Any): Unit
  def warn(msg: Any): Unit
  def debug(msg: Any): Unit
  def error(message: String, error: Throwable): Unit
  def taskDone(): Unit
  def taskDone(result: Message): Unit
}

trait Output {
  def task(name: String): TaskOutput
}

object Output {
  object StdOut extends Output {
    def task(name: String) = new TaskOutput {
      override def progress(value: Int): Unit = println(s"[$name][progress] ${value / 10}%")
      override def warn(value: Any): Unit = println(s"[$name][error] $value")
      override def error(message: String, error: Throwable): Unit = {
        println(s"[$name][error] $message")
        error.printStackTrace()
      }
      override def debug(msg: Any): Unit = println(s"[$name][debug] $msg")
      override def taskDone(): Unit = println(s"[$name][success]")
      override def taskDone(result: Message): Unit = println(s"[$name][success] " + result.toString.take(20) + "...")
      override def info(msg: Any): Unit = println(s"[$name][info] $msg")
    }
  }

  class MultiOut extends Output { self =>
    private val outputs = collection.mutable.Set.empty[Output]
    def add(output: Output) = outputs += output
    def remove(output: Output) = outputs -= output
    def task(name: String): TaskOutput = {
      val listeners = outputs.map(_.task(name))
      new TaskOutput {
        override def progress(value: Int): Unit =
          listeners.foreach(_.progress(value))
        override def warn(value: Any): Unit =
          listeners.foreach(_.warn(value))
        override def error(message: String, error: Throwable): Unit =
          listeners.foreach(_.error(message, error))
        override def debug(msg: Any): Unit =
          listeners.foreach(_.debug(msg))
        override def taskDone(): Unit =
          listeners.foreach(_.taskDone())
        override def taskDone(result: Message): Unit =
          listeners.foreach(_.taskDone(result))
        override def info(msg: Any): Unit =
          listeners.foreach(_.info(msg))
      }
    }
  }
}