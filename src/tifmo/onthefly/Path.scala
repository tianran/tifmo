package tifmo

import dcstree.SemRole
import dcstree.WordBase
import dcstree.DCSTreeNode
import dcstree.DCSTreeEdgeNormal
import dcstree.Ref
import dcstree.RefOutput
import dcstree.RefGerm

package onthefly {
	/**
	 * Path in a DCS tree. 
	 * 
	 * A path in a rooted tree has an ascending part (which goes up toward the root) and 
	 * a descending part (which goes down toward leaves). 
	 */
	class Path(
		/**
		 * The germ from which this path begins.
		 */
		val start: Ref, 
		/**
		 * Ascending part. 
		 */
		val asc: Seq[(DCSTreeNode, DCSTreeEdgeNormal)], 
		/**
		 * Descending part. 
		 */
		val dec: Seq[(DCSTreeEdgeNormal, DCSTreeNode)]
	) extends Serializable {
		/**
		 * Representing the path in a `role(node)role-...-role(node)` manner. 
		 */
		val rnrs = {
			var tmpnode = start.node
			var tmprole = start.role
			var revret = Seq.empty[(SemRole, DCSTreeNode, SemRole)]
			for ((n, e) <- asc) {
				revret = ((tmprole, tmpnode, tmpnode.outRole)) +: revret
				tmpnode = n
				tmprole = e.inRole
			}
			for ((e, n) <- dec) {
				revret = ((tmprole, tmpnode, e.inRole)) +: revret
				tmpnode = n
				tmprole = n.outRole
			}
			revret = ((tmprole, tmpnode, null)) +: revret
			revret.reverse
		}
		
		val headWords = {
			var flag = true
			val tmp = rnrs.takeWhile(x => {
				val ret = flag
				if (x._1 != x._3) flag = false
				ret
			})
			tmp.map(_._2.token.getWord).toSet
		}
		
		/**
		 * The last germ of this path. 
		 */
		val last = if (!dec.isEmpty) {
			RefOutput(dec.last._2)
		} else if (!asc.isEmpty) {
			val (pn, DCSTreeEdgeNormal(r)) = asc.last
			RefGerm(pn, r)
		} else {
			start
		}
		
	}
	
}
