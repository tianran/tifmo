package tifmo


package dcstree {
	
	sealed abstract class DCSTreeEdge {
		
		def inRole: SemRole
	}
	
	case class DCSTreeEdgeNormal(inRole: SemRole) extends DCSTreeEdge
	
	case class DCSTreeEdgeQuantifier(inRole: SemRole, quantifier: Quantifier) extends DCSTreeEdge
	
	case class DCSTreeEdgeRelation(inRole: SemRole, relation: Relation) extends DCSTreeEdge
	
}

