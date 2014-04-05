package tifmo


package dcstree {
	
	sealed abstract class Ref {
		
		def node: DCSTreeNode
		
		def role: SemRole
		
		def getDenotation: Denotation
		
		def subOrdinate: Set[(Seq[(DCSTreeNode, DCSTreeEdgeNormal)], Seq[(DCSTreeEdgeNormal, DCSTreeNode)], Ref)]
		
		@transient private[this] lazy val qtrs = if (node.positiveTree) {
			node.rseq.takeWhile(_ != role).toSet
		} else {
			node.rseq.takeWhile(r => r != role && r != node.outRole).toSet
		}
		
		@transient lazy val quantifyALL = {
			for ((DCSTreeEdgeQuantifier(r, QuantifierALL), n) <- node.children; if qtrs.contains(r)) yield RefOutput(n)
		}
		
		@transient lazy val quantifyNO = {
			for ((DCSTreeEdgeQuantifier(r, QuantifierNO), n) <- node.children; if qtrs.contains(r)) yield RefOutput(n)
		}
	}
	
	case class RefOutput(node: DCSTreeNode) extends Ref {
		
		def role = node.outRole
		
		def getDenotation = node.output
		
		@transient lazy val subOrdinate = {
			node.descending.map(x => {
				(Seq.empty[(DCSTreeNode, DCSTreeEdgeNormal)], x._1, x._2)
			}).toSet
		}
		
	}
	
	case class RefGerm(node: DCSTreeNode, role: SemRole) extends Ref {
		
		def getDenotation = node.germ(role)
		
		@transient lazy val subOrdinate = {
			if (node.compare2outRole(role) < 0) {
				node.descending.map(x => {
					(Seq.empty[(DCSTreeNode, DCSTreeEdgeNormal)], x._1, x._2)
				}).toSet
			} else {
				val tmp1 = for (x <- node.ascending.inits; if !x.isEmpty) yield {
					(x, Seq.empty[(DCSTreeEdgeNormal, DCSTreeNode)], RefGerm(x.last._1, x.last._2.inRole):Ref)
				}
				val tmp2 = for (x <- node.ascending.inits; hn = (node +: x.map(_._1)).last; (y, ref) <- hn.descending) yield {
					(x, y, ref)
				}
				tmp1.toSet ++ tmp2
			}
		}
		
	}
	
}
