import play.api.Application
import play.api.GlobalSettings
import specific.ChangeManagement

object Global extends GlobalSettings {
  override def onStop(app: Application) = {
    ChangeManagement.database.shutdown()
  }
}