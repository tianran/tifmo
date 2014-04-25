package tifmo

import dcstree.SemRole

import mylib.misc.oneFromEach
import mylib.misc.listPartitions
import mylib.misc.listCoverings

import scala.collection.mutable

package inference {
	
	private[inference] object RulesLight {
		
		// rule: a || b, c -> a, d -> b, c = PI(C), d = PI(D) => C || D
		private[inference] def rlDjtSubPI1(ie: IEngineCore, a: TermIndex, b: TermIndex) {
			if (a == b) {
				for (x <- a.subSets) {
					ie.applyDisjoint(x, x, Debug_RuleDjtSubPI(null, x.holder, a.holder, a.holder, x.holder, null, null, ie.getNewPredID()))
					for (pi <- x.ispis) {
						ie.newQuick(() => ie.claimDisjoint(pi.compt, pi.compt, Debug_RuleDjtSubPI(pi.compt.holder, x.holder, a.holder, a.holder, x.holder, pi.compt.holder, pi.compr, ie.getNewPredID())))
					}
					for (pi <- x.mkpis) {
						ie.newQuick(() => ie.claimDisjoint(pi.head, pi.head, Debug_RuleDjtSelf(pi.head.holder, x.holder, pi.compr, ie.getNewPredID())))
					}
				}
			} else {
				val bsub = b.subSets
				for (x <- a.subSets; y <- bsub) {
					if (x == y) {
						ie.newQuick(() => ie.claimDisjoint(x, y, Debug_RuleDjtSubPI(null, x.holder, a.holder, b.holder, y.holder, null, null, ie.getNewPredID())))
					} else {
						ie.applyDisjoint(x, y, Debug_RuleDjtSubPI(null, x.holder, a.holder, b.holder, y.holder, null, null, ie.getNewPredID()))
						for (xpi <- x.ispis; ypi <- y.ispis; if xpi.compt.dim == ypi.compt.dim && xpi.compr == ypi.compr) {
							ie.applyDisjoint(xpi.compt, ypi.compt, Debug_RuleDjtSubPI(xpi.compt.holder, x.holder, a.holder, b.holder, y.holder, ypi.compt.holder, ypi.compr, ie.getNewPredID()))
						}
					}
				}
			}
		}
		private[inference] def rlDjtSubPI2(ie: IEngineCore, a: TermIndex, b: TermIndex) {
			if (b.selfDisjoint) {
				ie.newQuick(() => ie.claimDisjoint(a, a, Debug_RuleDjtSubPI(null, a.holder, b.holder, b.holder, a.holder, null, null, ie.getNewPredID())))
			} else {
				val bdjt = b.disjointSets
				for (x <- a.subSets) {
					for (y <- bdjt) {
						if (x == y) {
							ie.newQuick(() => ie.claimDisjoint(x, y, Debug_RuleDjtSubPI(null, x.holder, b.holder, y.holder, y.holder, null, null, ie.getNewPredID())))
						} else {
							ie.applyDisjoint(x, y, Debug_RuleDjtSubPI(null, x.holder, b.holder, y.holder, y.holder, null, null, ie.getNewPredID()))
							for (xpi <- x.ispis; ypi <- y.ispis; if xpi.compt.dim == ypi.compt.dim && xpi.compr == ypi.compr) {
								ie.applyDisjoint(xpi.compt, ypi.compt, Debug_RuleDjtSubPI(xpi.compt.holder, x.holder, b.holder, y.holder, y.holder, ypi.compt.holder, ypi.compr, ie.getNewPredID()))
							}
						}
					}
				}
			}
		}
		private[inference] def rlDjtSubPI3(ie: IEngineCore, h: TermIndex, t: TermIndex, r: SemRole) {
			if (h.selfDisjoint) {
				ie.newQuick(() => ie.claimDisjoint(t, t, Debug_RuleDjtSubPI(t.holder, h.holder, h.holder, h.holder, h.holder, t.holder, r, ie.getNewPredID())))
			} else {
				for (x <- h.disjointSets; pi <- x.ispis; if pi.compt.dim == t.dim && pi.compr == r) {
					ie.applyDisjoint(pi.compt, t, Debug_RuleDjtSubPI(pi.compt.holder, x.holder, x.holder, h.holder, h.holder, t.holder, r, ie.getNewPredID()))
				}
			}
		}
		
		// rule: A x B x (C x D) = A x B x C x D
		private[inference] def rlCPCP(ie: IEngineCore, h: TermIndex, t: Set[(TermIndex, SemRole)]) {
			
			var task = Nil:List[() => Boolean]
			def addCPhkCP(h: TermIndex, t: Set[(TermIndex, SemRole)]) {
				
				for (cp <- t.minBy[Int](_._1.mkcps.size)._1.mkcps; if t.subsetOf(cp.comp) && t.size != cp.comp.size) {
					val ncomp = (cp.comp -- t) + ((h, null))
					ie.newQuick(() => ie.claimCP(cp.head, ncomp, Debug_SimpleRuleTrace("CPCP", ie.getNewPredID())))
				}
				
				val cands = mutable.Map.empty[Set[(TermIndex, SemRole)], (TermIndex, SemRole)]
				cands ++= t.map(x => (Set(x), x))
				for (cp <- t.flatMap(_._1.mkcps)) {
					if (cp.comp.subsetOf(t) && t.size != cp.comp.size) {
						cands(cp.comp) = (cp.head, null)
					}
				}
				for (p <- listPartitions[(TermIndex, SemRole)](t, cands.keys)) {
					val ncomp = p.map(cands(_)).toSet
					task = (() => ie.applyCP(h, ncomp, Debug_SimpleRuleTrace("CPCP", ie.getNewPredID()))) :: task
				}
				
				val ll = for ((x, r) <- t) yield Set((x, r)) :: x.iscps.toList.map(_.comp)
				for (l <- oneFromEach[Set[(TermIndex, SemRole)]](ll)) {
					val ncomp = l.flatten.toSet
					task = (() => ie.applyCP(h, ncomp, Debug_SimpleRuleTrace("CPCP", ie.getNewPredID()))) :: task
				}
			}
			
			addCPhkCP(h, t)
			for (cp <- h.mkcps) {
				val ncomp = (cp.comp - ((h, null))) ++ t
				if (!cp.head.iscps.contains(IEPredCP(cp.head, ncomp))) addCPhkCP(cp.head, ncomp)
			}
			
			task.foreach(_())
		}
		
		// rule: PI(PI(A)) = PI(A)
		private[inference] def rlPIPI(ie: IEngineCore, h: TermIndex, t: TermIndex, r: SemRole) {
			
			var task = Nil:List[() => Boolean]
			
			def addPIhkPI(h: TermIndex, t: TermIndex, r: SemRole) {
				
				val hrs = h.dim.relabel(r)
				
				for (pi <- t.mkpis) {
					if (pi.headrs.subsetOf(hrs) && pi.headrs.size != hrs.size) {
						ie.newQuick(() => ie.claimPI(pi.head, h, pi.compr, Debug_SimpleRuleTrace("PIPI", ie.getNewPredID())))
					}
				}
				
				for (pi <- t.mkpis) {
					if (hrs.subsetOf(pi.headrs) && pi.headrs.size != hrs.size) {
						task = (() => ie.applyPI(h, pi.head, r, Debug_SimpleRuleTrace("PIPI", ie.getNewPredID()))) :: task
					}
				}
				
				for (pi <- t.ispis) {
					task = (() => ie.applyPI(h, pi.compt, r, Debug_SimpleRuleTrace("PIPI", ie.getNewPredID()))) :: task
				}
			}
			
			addPIhkPI(h, t, r)
			for (pi <- h.mkpis; if !pi.head.ispis.contains(IEPredPI(pi.head, t, pi.compr))) {
				task = (() => ie.applyPI(pi.head, t, pi.compr, Debug_SimpleRuleTrace("PIPI", ie.getNewPredID()))) :: task
				addPIhkPI(pi.head, t, pi.compr)
			}
			
			if (t.selfDisjoint) {
				ie.newQuick(() => ie.claimDisjoint(h, h, Debug_RuleDjtSelf(h.holder, t.holder, r, ie.getNewPredID())))
			}
			
			task.foreach(_())
		}
		
	}
	
}

