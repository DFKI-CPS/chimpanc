package specific.util

import scala.reflect.ClassTag

/**
 * Created by martin on 26.10.15.
 */
object Helpers {
  implicit class PimpedBoolean(val bool: Boolean) extends AnyVal {
    def ?[T](value: => T): Option[T] = if (bool) Option(value) else None
  }

  implicit class AsInstanceOf[T](val t: T) extends AnyVal {
    def asOpt[U](implicit ev: ClassTag[T], ev2: ClassTag[U]): Option[U] = t.isInstanceOf[U] ? t.asInstanceOf[U]
  }
}
