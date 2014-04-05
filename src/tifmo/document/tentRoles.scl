package tifmo

import dcstree.SemRole
import dcstree.WordBase

import scala.collection.mutable

package document {
	
	object tentRoles extends (Set[Document] => Map[WordBase, Set[SemRole]]) {
		
		def apply(docs: Set[Document]) = {
			val ret = mutable.Map.empty[WordBase, Set[SemRole]]
			
			def recur(x: TokenNode) {
				if (x.conj) {
					x.outRole = x.parent.outRole
					x.quantifier = x.parent.quantifier
					x.relation = x.parent.relation
				}
				val rs = x.locRoles
				val wd = x.token.getWord
				if (!wd.isStopWord) ret(wd) = ret.getOrElse(wd, Set.empty[SemRole]) ++ rs
				for ((r, n) <- x.children) recur(n)
			}
			for (doc <- docs; n <- doc.allRootNodes) recur(n)
			
			ret.toMap
		}
		
	}
}
