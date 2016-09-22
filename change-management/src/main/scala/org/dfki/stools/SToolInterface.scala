package org.dfki.stools

import org.dfki.stools.editscript.SEditScript
import org.dfki.stools.similarityspec.SimilaritySpec

trait SToolInterface {
  def similarity(a: ISElement[_], b: ISElement[_]): Double
  def sdiff(a: ISElement[_], b: ISElement[_]): SEditScript
  def sdiff3(o: ISElement[_], a: ISElement[_], b: ISElement[_]): SEditScript
  def merge(a: ISElement[_], b: ISElement[_]): SEditScript
  def getSimilaritySpec(): SimilaritySpec
}