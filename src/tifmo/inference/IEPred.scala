package tifmo

import dcstree.SemRole
import dcstree.Relation

import scala.collection.mutable

package inference {
	/**
	 * Abstract class of a predicate.
	 * 
	 * A predicate is a relation among several terms, different types of 
	 * predicates correspond to different subclasses of [[tifmo.inference.IEPred]].
	 */
	sealed abstract class IEPred extends Ordered[IEPred] {
		
		private[this] var debug_tr = null:Debug_RuleTrace
		def debug_trace = debug_tr
		
		def compare(that: IEPred) = debug_trace.getID - that.debug_trace.getID
		
		protected[this] def applyThis[T <: IEPred](a: T, pools: Seq[mutable.Set[T]], debug_ruletrace: Debug_RuleTrace) = {
			assert(debug_tr == null)
			assert(debug_ruletrace != null)
			debug_tr = debug_ruletrace
			assert(pools != null)
			val rets = pools.map(_.add(a))
			assert(rets.tail.forall(_ == rets.head))
			rets.head
		}
		protected[this] def disposeThis[T <: IEPred](a: T, pools: Seq[mutable.Set[T]]) {
			pools.foreach(_.remove(a))
		}
		
		private[inference] def apply(debug_ruletrace: Debug_RuleTrace): Boolean
		private[inference] def dispose(): Unit
		private[inference] def valid: Boolean
		private[inference] def dumpMe(tmmap: Map[TermIndex, TermIndex]): IEPred
	}
	
	case class IEPredNonEmpty(term: TermIndex) extends IEPred {
		private[this] val pl = Seq(term.kne)
		private[inference] def apply(debug_ruletrace: Debug_RuleTrace) = applyThis[IEPredNonEmpty](this, pl, debug_ruletrace)
		private[inference] def dispose() { disposeThis[IEPredNonEmpty](this, pl) }
		private[inference] def valid = term.valid
		private[inference] def dumpMe(tmmap: Map[TermIndex, TermIndex]) = IEPredNonEmpty(tmmap(term))
	}
	
	case class IEPredSubsume(subset: TermIndex, superset: TermIndex) extends IEPred {
		private[this] val pl = Seq(subset.assub, superset.assuper)
		private[inference] def apply(debug_ruletrace: Debug_RuleTrace) = applyThis[IEPredSubsume](this, pl, debug_ruletrace)
		private[inference] def dispose() { disposeThis[IEPredSubsume](this, pl) }
		private[inference] def valid = (subset.valid && superset.valid)
		private[inference] def dumpMe(tmmap: Map[TermIndex, TermIndex]) = IEPredSubsume(tmmap(subset), tmmap(superset))
	}
	
	case class IEPredDisjoint(a: TermIndex, b: TermIndex) extends IEPred {
		private[this] val pl = Seq(a.djts)
		private[inference] def apply(debug_ruletrace: Debug_RuleTrace) = applyThis[IEPredDisjoint](this, pl, debug_ruletrace)
		private[inference] def dispose() { disposeThis[IEPredDisjoint](this, pl) }
		private[inference] def valid = (a.valid && b.valid)
		private[inference] def dumpMe(tmmap: Map[TermIndex, TermIndex]) = IEPredDisjoint(tmmap(a), tmmap(b))
	}
	
	case class IEPredCP(head: TermIndex, comp: Set[(TermIndex, SemRole)]) extends IEPred {
		
		val roleMap = comp.map(x => (x._1.dim.relabel(x._2), x)).toMap
		
		private[this] val pl = head.iscps +: comp.map(_._1).toSeq.map(_.mkcps)
		private[inference] def apply(debug_ruletrace: Debug_RuleTrace) = applyThis[IEPredCP](this, pl, debug_ruletrace)
		private[inference] def dispose() { disposeThis[IEPredCP](this, pl) }
		private[inference] def valid = (head.valid && comp.forall(_._1.valid))
		private[inference] def dumpMe(tmmap: Map[TermIndex, TermIndex]) = IEPredCP(tmmap(head), comp.map(x => (tmmap(x._1), x._2)))
	}
	
	case class IEPredPI(head: TermIndex, compt: TermIndex, compr: SemRole) extends IEPred {
		
		val headrs = head.dim.relabel(compr)
		
		private[this] val pl = Seq(head.ispis, compt.mkpis)
		private[inference] def apply(debug_ruletrace: Debug_RuleTrace) = applyThis[IEPredPI](this, pl, debug_ruletrace)
		private[inference] def dispose() { disposeThis[IEPredPI](this, pl) }
		private[inference] def valid = (head.valid && compt.valid)
		private[inference] def dumpMe(tmmap: Map[TermIndex, TermIndex]) = IEPredPI(tmmap(head), tmmap(compt), compr)
	}
	
	case class IEPredIN(head: TermIndex, comp: Set[TermIndex], aux: Boolean) extends IEPred {
		private[this] val pl = head.isins +: comp.toSeq.map(_.mkins)
		private[inference] def apply(debug_ruletrace: Debug_RuleTrace) = applyThis[IEPredIN](this, pl, debug_ruletrace)
		private[inference] def dispose() { disposeThis[IEPredIN](this, pl) }
		private[inference] def valid = (!aux && head.valid && comp.forall(_.valid))
		private[inference] def dumpMe(tmmap: Map[TermIndex, TermIndex]) = IEPredIN(tmmap(head), comp.map(tmmap(_)), aux)
	}
	
	case class IEPredFunc(func: IEFunction, tms: Seq[TermIndex], param: Any) extends IEPred {
		private[this] val pl = tms.toSet[TermIndex].toSeq.map(_.funcs)
		private[inference] def apply(debug_ruletrace: Debug_RuleTrace) = applyThis[IEPredFunc](this, pl, debug_ruletrace)
		private[inference] def dispose() { disposeThis[IEPredFunc](this, pl) }
		private[inference] def valid = false
		private[inference] def dumpMe(tmmap: Map[TermIndex, TermIndex]) = IEPredFunc(func, tms.map(tmmap(_)), param)
	}
	
	case class IEPredRL(a: TermIndex, rl: Relation, b: TermIndex) extends IEPred {
		private[this] val pl = Seq(a.asarl, b.asrlb)
		private[inference] def apply(debug_ruletrace: Debug_RuleTrace) = applyThis[IEPredRL](this, pl, debug_ruletrace)
		private[inference] def dispose() { disposeThis[IEPredRL](this, pl) }
		private[inference] def valid = (a.valid && b.valid)
		private[inference] def dumpMe(tmmap: Map[TermIndex, TermIndex]) = IEPredRL(tmmap(a), rl, tmmap(b))
	}
	
}
