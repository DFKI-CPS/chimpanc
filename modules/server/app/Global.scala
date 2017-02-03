import java.io.File

import play.api.Application
import play.api.GlobalSettings
import specific.ChangeManagement

object Global extends GlobalSettings {
  override def beforeStart(app: Application): Unit = {
    val file = new File("db")
    if (file.exists() && file.isDirectory)
      file.delete()
  }
  override def onStop(app: Application) = {
    ChangeManagement.store.graphDb.shutdown()
  }
}