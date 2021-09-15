package kleislis.take2

import kleislis.take2.Entity.EntityBId

case class EntityA(entityBId: EntityBId) {
  def idOfB: EntityBId = entityBId
}