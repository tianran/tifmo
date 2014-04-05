package tifmo

import dcstree.SemRole

package inference {
	
	class Dimension(private val rs: Set[SemRole]) extends Serializable {
		
		assert(rs == null || rs.size >= 2)
		
		val size = if (rs == null) 1 else rs.size
		
		def relabel(r: SemRole) = {
			
			assert(rs == null ^ r == null)
			
			if (rs == null) Set(r) else rs
		}
		
		def decrease(r: SemRole) = {
			
			assert(rs.contains(r))
			
			rs - r
		}
		
		override def toString = if (rs == null) "" else rs.mkString("[", ",", "]")
		override def hashCode = if (rs == null) 0 else rs.hashCode
		override def equals(a: Any) = a.isInstanceOf[Dimension] && {
			val that = a.asInstanceOf[Dimension]
			rs == that.rs
		}
	}
	
	object Dimension extends ((Set[SemRole]) => (Dimension, SemRole)) {
		
		def apply(roles: Set[SemRole]) = {
			if (roles.size == 1) {
				(new Dimension(null), roles.head)
			} else {
				(new Dimension(roles), null)
			}
		}
		
	}
}
