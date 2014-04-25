package tifmo

import dcstree.Executor
import dcstree.SemRole
import dcstree.DCSTreeNode
import dcstree.Ref
import dcstree.RefOutput
import dcstree.RefGerm
import dcstree.DCSTreeEdgeNormal
import dcstree.DCSTreeEdgeQuantifier
import dcstree.DCSTreeEdgeRelation
import dcstree.QuantifierALL
import dcstree.QuantifierNO
import dcstree.Declarative
import dcstree.DeclarativePosi
import dcstree.DeclarativeNega
import dcstree.DeclarativeNotEmptyRef
import dcstree.DeclarativeSubRef
import dcstree.DeclarativeDjtRef
import dcstree.DeclarativeRel
import dcstree.DeclarativeSubsume

import scala.collection.mutable

package onthefly {
	/**
	 * Abduction engine. 
	 * 
	 * The constructor takes a set of premises ([[tifmo.dcstree.Declarative]]s), and it 
	 * first inflates this set to produce more premises (those [[tifmo.dcstree.Declarative]]s 
	 * that are implied from the original premises) which are in turn used in the following 
	 * abduction processes. The inflated set of premises can be got from the method `premise`.
	 * 
	 * Then, you can call `addGoal` or `sufficient` or `maybeHelpful` to add assumptions. 
	 * According to the methods, the abduction engine will automatically produce additional 
	 * assumptions that may be helpful to prove the added assumption (abduction). The degree 
	 * of this abduction (the number of automatically produced additional assumptions) is 
	 * `addGoal >= sufficient >= maybeHelpful`. 
	 * 
	 * Finally, call `allAssumptions` to get all the added and automatically produced assumptions. 
	 * 
	 * @constructor AEngine constructor.
	 * @param prem The original set of premises.
	 */
	class AEngine(prem: Set[Declarative]) extends Executor {
		
		val premise = {
			var ret = mutable.Set.empty[Declarative] ++ prem
			for (DeclarativePosi(rt) <- prem) {
				def recurRel(x: DCSTreeNode) {
					for ((DCSTreeEdgeRelation(r, rel), n) <- x.children) {
						ret += DeclarativeRel(rel, RefOutput(n), RefGerm(x, r))
					}
					for ((e, n) <- x.children) recurRel(n)
				}
				recurRel(rt)
				def recur(x: DCSTreeNode) {
					for (r <- x.rseq) ret += DeclarativeNotEmptyRef(RefGerm(x, r))
					for ((e, n) <- x.children) {
						e match {
							case DCSTreeEdgeNormal(r) => recur(n)
							case DCSTreeEdgeQuantifier(r, qt) => qt match {
								case QuantifierALL => ret += DeclarativeSubRef(RefOutput(n), RefGerm(x, r))
								case QuantifierNO => if (r == x.rseq.last) ret += DeclarativeDjtRef(RefOutput(n), RefGerm(x, r))
							}
							case DCSTreeEdgeRelation(r, rel) => {}
						}
					}
				}
				recur(rt)
			}
			for (DeclarativeNega(rt) <- prem) {
				def recur(x: DCSTreeNode) {
					val rs = x.rseq.dropWhile(_ != x.outRole).toSet
					for (r <- rs) ret += DeclarativeDjtRef(RefGerm(x, r), RefGerm(x, r))
					for ((DCSTreeEdgeNormal(rr), n) <- x.children; if rs.contains(rr)) recur(n)
				}
				recur(rt)
			}
			ret.toSet
		}
		
		private[this] val helpful = mutable.Set.empty[Declarative]
		def allAssumptions = helpful.toSet
		
		def maybeHelpful(assumption: Declarative) {
			assumption match {
				case DeclarativeRel(rel, a, b) => {
					if (helpful.add(assumption)) rel.execute[Ref](this, a, b)
				}
				case DeclarativeSubsume(sub, sup) => {
					if (helpful.add(assumption)) {
						if (sub.selection != null) sub.selection.execute[(Declarative => Unit)](this, toProve)(assumption)
						if (sup.selection != null) sup.selection.execute[(Declarative => Unit)](this, toProve)(assumption)
					}
				}
				case _ => helpful += assumption
			}
		}
		
		private[this] def subHelpfulRef(sub: Ref, sup: Ref) {
			val ssub = sub.subOrdinate.map(_._3)
			val ssup = sup.subOrdinate.map(_._3)
			for (x <- ssub; y <- ssup) {
				maybeHelpful(DeclarativeSubRef(x, y))
			}
			val filsub = for (RefOutput(xn) <- ssub; if xn.selection != null) yield xn
			val filsup = for (RefOutput(yn) <- ssup; if yn.selection != null) yield yn
			for (xn <- filsub; yn <- filsup) {
				maybeHelpful(DeclarativeSubsume(xn, yn))
			}
			for (x <- ssub; y <- ssup) {
				compareQuantifier(x, y)
			}
		}
		
		private[this] def compareQuantifier(x: Ref, y: Ref) {
			for (a <- y.quantifyALL; b <- x.quantifyALL) {
				maybeHelpful(DeclarativeSubRef(a, b))
				subHelpfulRef(a, b)
			}
			for (a <- y.quantifyNO; b <- x.quantifyNO) {
				maybeHelpful(DeclarativeSubRef(a, b))
				subHelpfulRef(a, b)
			}
		}
		
		private[this] val toProve: (Declarative => Unit) = (hypo: Declarative) => hypo match {
			case DeclarativeSubRef(sub, sup) => {
				subHelpfulRef(sub, sup)
			}
			case DeclarativeSubsume(x, y) => {
				subHelpfulRef(RefOutput(x), RefOutput(y))
			}
			case _ => {}
		}
			
		def sufficient(assumption: Declarative) {
			maybeHelpful(assumption)
			toProve(assumption)
		}
		
		def addGoal(hypo: Declarative) {
			sufficient(hypo)
			hypo match {
				case DeclarativePosi(rt) => {
					def recurRel(x: DCSTreeNode) {
						for ((DCSTreeEdgeRelation(r, rel), n) <- x.children) {
							addGoal(DeclarativeRel(rel, RefOutput(n), RefGerm(x, r)))
						}
						for ((e, n) <- x.children) recurRel(n)
					}
					recurRel(rt)
					def recur(x: DCSTreeNode) {
						val rs = x.rseq.dropWhile(_ != x.outRole).toSet
						for (r <- rs) {
							addGoal(DeclarativeNotEmptyRef(RefGerm(x, r)))
						}
						for ((DCSTreeEdgeNormal(rr), n) <- x.children; if rs.contains(rr)) recur(n)
					}
					recur(rt)
					def recurQtNO(x: DCSTreeNode) {
						for ((DCSTreeEdgeQuantifier(r, qt), n) <- x.children; if qt == QuantifierNO) {
							addGoal(DeclarativeDjtRef(RefOutput(n), RefGerm(x, r)))
						}
						for ((e:DCSTreeEdgeNormal, n) <- x.children) recurQtNO(n)
					}
					recurQtNO(rt)
				}
				case DeclarativeNega(rt) => {
					def recur(x: DCSTreeNode) {
						val rs = x.rseq.dropWhile(_ != x.outRole).toSet
						for (r <- rs) addGoal(DeclarativeDjtRef(RefGerm(x, r), RefGerm(x, r)))
						for ((DCSTreeEdgeNormal(rr), n) <- x.children; if rs.contains(rr)) recur(n)
					}
					recur(rt)
				}
				case DeclarativeNotEmptyRef(ref) => {
					for (DeclarativeNotEmptyRef(x) <- premise) {
						sufficient(DeclarativeSubRef(x, ref))
					}
				}
				case DeclarativeSubRef(sub, sup) => {
					for (DeclarativeSubRef(x, y) <- premise) {
						sufficient(DeclarativeSubRef(x, sub))
						sufficient(DeclarativeSubRef(sup, y))
					}
				}
				case DeclarativeDjtRef(a, b) => {
					for (DeclarativeDjtRef(x, y) <- premise) {
						sufficient(DeclarativeSubRef(a, x))
						sufficient(DeclarativeSubRef(b, y))
						sufficient(DeclarativeSubRef(a, y))
						sufficient(DeclarativeSubRef(b, x))
					}
				}
				case DeclarativeRel(rel, a, b) => {}
				case DeclarativeSubsume(sub, sup) => {}
			}
		}
		
	}
}
