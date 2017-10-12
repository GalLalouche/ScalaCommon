package common.storage

import slick.jdbc.{H2Profile, JdbcProfile}

trait StorageSpecs {
  protected implicit val profile: JdbcProfile = H2Profile
  protected lazy val db: profile.backend.DatabaseDef = profile.api.Database.forURL(
    s"jdbc:h2:mem:test${System.identityHashCode(this)};DB_CLOSE_DELAY=-1", driver = "org.h2.Driver")
}
