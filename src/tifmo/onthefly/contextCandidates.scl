package tifmo

import dcstree.DCSTreeNode
import dcstree.DCSTreeEdgeNormal
import dcstree.Ref
import dcstree.RefOutput
import dcstree.RefGerm
import dcstree.Declarative
import dcstree.DeclarativeSubRef
import dcstree.SelCorefBase
import inference.IEngine

import scala.collection.mutable

package onthefly {
	/**
	 * Function object `contextCandidates` generate context candidates from a path.
	 */
	object contextCandidates extends (Path => Set[Ref]) {
		
		private[this] def refChildren(x: Ref) = {
			val pre = for ((e:DCSTreeEdgeNormal, n) <- x.node.children) yield {
				RefOutput(n):Ref
			}
			x match {
				case RefOutput(n) => pre
				case RefGerm(n, r) => if (n.compare2outRole(r) < 0 || n.parent == null) {
					pre
				} else {
					val (pn, DCSTreeEdgeNormal(r)) = n.parent
					pre + RefGerm(pn, r)
				}
			}
		}
		
		private[this] def nodeGrandChildrenRefs(node: DCSTreeNode): Set[Ref] = {
			val tmp = for ((e:DCSTreeEdgeNormal, n) <- node.children) yield {
				nodeGrandChildrenRefs(n) + RefOutput(n)
			}
			tmp.flatten
		}
		
		private[this] def refGrandChildren(x: Ref) = {
			x match {
				case RefOutput(n) => nodeGrandChildrenRefs(n)
				case RefGerm(n, r) => if (n.compare2outRole(r) < 0) {
					nodeGrandChildrenRefs(n)
				} else {
					var ret = (for (x <- n.ascending.inits; if !x.isEmpty) yield {
						RefGerm(x.last._1, x.last._2.inRole):Ref
					}).toSet
					for (x <- n.ascending.inits) {
						val hn = (n +: x.map(_._1)).last
						ret ++= nodeGrandChildrenRefs(hn)
					}
					ret
				}
			}
		}
		
		/**
		 * Generated context candidates will be logically filtered during construction of on-the-fly knowledge. 
		 * Inference engine terms necessary for the later filtering process should be 
		 * constructed beforehand, otherwise these terms may be lost due to merge by inference engine.
		 */
		def init(assumption: Set[Declarative], ie: IEngine) {
			
			val refpool = mutable.Set.empty[Ref]
			for (DeclarativeSubRef(sub, sup) <- assumption) {
				refpool += sub
				refpool ++= refGrandChildren(sub)
			}
			for (ref <- refpool) {
				ie.getTerm(ref.getDenotation)
				if (ref.node.selection != null && ref.node.selection.isInstanceOf[SelCorefBase]) {
					for (n <- ref.node.selection.asInstanceOf[SelCorefBase].coreferentNodes) {
						refGrandChildren(RefGerm(n, n.outRole)).foreach(x => ie.getTerm(x.getDenotation))
					}
				}
			}
		}
		
		def apply(path: Path) = {
			
			val prepre = refGrandChildren(path.start)
			val minusasc = path.asc.map(x => RefGerm(x._1, x._2.inRole))
			val minusdec = path.dec.map(x => RefOutput(x._2))
			val rclast = refChildren(path.last).flatMap(refGrandChildren(_))
			val pre = prepre -- minusasc -- minusdec -- rclast
			
			var ret = pre
			for (x <- pre; if x.node.selection != null && x.node.selection.isInstanceOf[SelCorefBase]) {
				for (n <- x.node.selection.asInstanceOf[SelCorefBase].coreferentNodes) {
					ret ++= refGrandChildren(RefGerm(n, n.outRole))
				}
			}
			
			ret
		}
		
	}
	
}

