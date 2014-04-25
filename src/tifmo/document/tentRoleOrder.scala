package tifmo

import dcstree.SemRole
import dcstree.WordBase

import scala.collection.mutable
import scala.util.Sorting

package document {
	
	object tentRoleOrder extends ((Map[WordBase, Set[SemRole]], Document) => Unit) {
		
		def apply(rolesDic: Map[WordBase, Set[SemRole]], doc: Document) {
			
			def recur(x: TokenNode) {
				val wd = x.token.getWord
				val rs = if (wd.isStopWord) x.locRoles else rolesDic(wd)
				
				val sortc = x.children.filter(!_._2.conj).toArray
				object order extends math.Ordering[(SemRole, TokenNode)] {
					def compare(a: (SemRole, TokenNode), b: (SemRole, TokenNode)) = b._2.token.id - a._2.token.id
				}
				Sorting.quickSort(sortc)(order)
				
				val rolescache = mutable.Set.empty[SemRole]
				val beforeOutRoleQuantifier = {
					val pn = if (x.parent == null) x else x.parent
					for {
						(r, n) <- sortc.takeWhile(y => y._2.token.id > pn.token.id && y._1 != x.outRole)
						if n.quantifier != null && rolescache.add(r)
					} yield r
				}
				if (x.outRole != null) rolescache += x.outRole
				
				val afterOutRoleQuantifier = {
					(for {
						(r, n) <- sortc.reverse
						if n.quantifier != null && rolescache.add(r)
					} yield r).reverse
				}
				
				val sortrest = (rs -- rolescache).toArray
				Sorting.quickSort(sortrest)
				
				x.rseq = if (x.outRole != null) {
					(beforeOutRoleQuantifier.toSeq :+ x.outRole) ++ sortrest ++ afterOutRoleQuantifier
				} else {
					val ret = beforeOutRoleQuantifier.toSeq ++ sortrest ++ afterOutRoleQuantifier
					x.outRole = ret.head
					ret
				}
				
				for ((r, n) <- x.children) recur(n)
			}
			for (n <- doc.allRootNodes) recur(n)
		}
		
	}
}
