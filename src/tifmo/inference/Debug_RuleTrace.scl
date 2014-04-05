package tifmo

import dcstree.SemRole

package inference {
	
	import Finder._
	
	abstract class Debug_RuleTrace {
		def assertValid(): Unit
		def getID: Int
	}
	
	case class Debug_SimpleRuleTrace(str: String, getID: Int) extends Debug_RuleTrace {
		def assertValid() {}
	}
	
	case class Debug_RuleSubNE(sub: Term, sup: Term, getID: Int) extends Debug_RuleTrace {
		def assertValid() {
			assert(sub.hasSuper(sup))
			assert(sub.knownNE)
			assert(sup.knownNE)
		}
	}
	
	case class Debug_RuleINdef(a: Term, h: Term, t: Set[Term], getID: Int) extends Debug_RuleTrace {
		def assertValid() {
			assert(isIN(h, t))
			assert(t.forall(_.hasSub(a)))
			assert(h.hasSub(a))
		}
	}
	
	case class Debug_RuleSubSub(x: Term, a: Term, b: Term, y: Term, getID: Int) extends Debug_RuleTrace {
		def assertValid() {
			assert(a.hasSuper(b))
			assert(a.hasSub(x))
			assert(b.hasSuper(y))
			assert(y.hasSub(x))
		}
	}
	
	
	case class Debug_RulePINE(a: Term, b: Term, r: SemRole, getID: Int) extends Debug_RuleTrace {
		def assertValid() {
			assert(isPI(a, b, r))
			assert(a.knownNE)
			assert(b.knownNE)
		}
	}
	
	case class Debug_RuleCPNE(h: Term, c: Set[(Term, SemRole)], getID: Int) extends Debug_RuleTrace {
		def assertValid() {
			assert(isCP(h, c))
			assert(h.knownNE)
			assert(c.forall(_._1.knownNE))
		}
	}
	
	case class Debug_RulePICP(pih: Term, pit: Term, pir: SemRole, cph: Term, cpt: Set[(Term, SemRole)], getID: Int) extends Debug_RuleTrace {
		def assertValid() {
			val pihrs = pih.dim.relabel(pir)
			val rmap = cpt.map(x => (x._1.dim.relabel(x._2), x)).toMap
			val fil = rmap.keySet.filter(_.subsetOf(pihrs))
			assert(fil.flatten == pihrs)
			val comp = fil.map(rmap(_))
			val cl = cpt.map(_._1.index) -- comp.map(_._1.index)
			if (fil.size == 1) {
				assert(pih.hasSuper(comp.head._1))
				if (cl.forall(_.knownNE)) assert(pih.hasSub(comp.head._1))
			} else {
				assert(cl.forall(_.knownNE))
				assert(isCP(pih, comp))
			}
		}
	}
	
	case class Debug_RuleDjtSelf(xpi: Term, x: Term, r: SemRole, getID: Int) extends Debug_RuleTrace {
		def assertValid() {
			assert(isPI(xpi, x, r))
			assert(x.selfDisjoint)
			assert(xpi.selfDisjoint)
		}
	}
	
	case class Debug_RuleDjtSubPI(xpi: Term, x: Term, a: Term, b: Term, y: Term, ypi: Term, r: SemRole, getID: Int) extends Debug_RuleTrace {
		def assertValid() {
			assert(a.disjointTo(b))
			assert(x.hasSuper(a))
			assert(y.hasSuper(b))
			assert(x.disjointTo(y))
			if (xpi != null || ypi != null) {
				assert(isPI(x, xpi, r))
				assert(isPI(y, ypi, r))
				assert(xpi.disjointTo(ypi))
			}
		}
	}
	
	
	case class Debug_RulePISub(x: Term, pia: Term, suba: Term, getID: Int) extends Debug_RuleTrace {
		def assertValid() {
			assert(x.hasSuper(pia))
			assert(pia.index.ispis.exists(pi => pi.compt.dim == suba.dim && pi.compt.hasSub(suba.index)))
		}
	}
	
	case class Debug_RuleCPSub(a: Term, acomp: Set[(Term, SemRole)], b: Term, bcomp: Set[(Term, SemRole)], getID: Int) extends Debug_RuleTrace {
		def assertValid() {
			assert(isCP(a, acomp))
			assert(isCP(b, bcomp))
			assert(a.hasSuper(b))
			val rmap = acomp.map(x => (x._1.dim.relabel(x._2), x._1)).toMap
			for ((t, r) <- bcomp) {
				assert(t.isW || t.hasSub(rmap(t.dim.relabel(r))))
			}
		}
	}
	
	case class Debug_RulePIdef(a: Term, b: Term, bcomp: Set[(Term, SemRole)], getID: Int) extends Debug_RuleTrace {
		def assertValid() {
			assert(isCP(b, bcomp))
			assert(a.hasSuper(b))
			for ((t, r) <- bcomp) {
				assert(t.isW || a.index.mkpis.exists(pi => pi.headrs == t.dim.relabel(r) && pi.head.hasSuper(t.index)))
			}
		}
	}
	
}
