package client

import client.RVar
import specific.{LayerObject, Spec}

object Layer {
  private var id_n = -1

  def id = {
    id_n += 1
    "layer" + id_n
  }
}


class Layer(initialContent: Spec) {
  val id = Layer.id

  val entities = RVar(Set.empty[LayerObject], alwaysFire = true)

  val addMapping = RVar(false)

  val active = RVar(false)

  val entityElements = collection.mutable.Map.empty[String,EntityElement]

  val content = RVar(initialContent)

  def name = content().name

  override def toString = name
}