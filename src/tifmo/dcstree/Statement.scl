package tifmo


package dcstree {
	
	sealed abstract class Statement
	
	case class StatementNotEmpty(x: Denotation) extends Statement
	
	case class StatementSubsume(sub: Denotation, sup: Denotation) extends Statement
	
	case class StatementDisjoint(a: Denotation, b: Denotation) extends Statement
	
	case class StatementRelation(rel: Relation, a: Denotation, b: Denotation) extends Statement
	
}
