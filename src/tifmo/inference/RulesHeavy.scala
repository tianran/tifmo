package tifmo

import dcstree.SemRole

import mylib.misc.oneFromEach

import scala.collection.mutable

package inference {
	
	import RAConversion._
	
	// rule: PI(A), B \subset A => PI(B) \subset PI(A)
	private[inference] object rPISub0 extends ((IEngineCore, TermIndex, TermIndex, Set[SemRole]) => Unit) {
		def apply(ie: IEngineCore, pia: TermIndex, suba: TermIndex, rs: Set[SemRole]) {
			ie.constructPI(suba.holder, rs, Seq(pia), (x:Term, args: Seq[RuleArg]) => args match {
				case Seq(RuleArg(pia:TermIndex)) => {
					ie.claimSubsume(x.index, pia, Debug_RulePISub(x, pia.holder, suba.holder, ie.getNewPredID()))
				}
				case _ => throw new Exception("rPISub0 error!")
			})
		}
	}
	private[inference] object rPISub1 extends RuleDo[IEPredSubsume] {
		def apply(ie: IEngineCore, pred: IEPredSubsume, args: Seq[RuleArg]) {
			assert(args.isEmpty)
			val (a, b) = (pred.subset, pred.superset)
			for (pi <- b.mkpis.toList) {
				rPISub0(ie, pi.head, a, pi.headrs)
			}
		}
	}
	private[inference] object rPISub2 extends RuleDo[IEPredPI] {
		def apply(ie: IEngineCore, pred: IEPredPI, args: Seq[RuleArg]) {
			assert(args.isEmpty)
			val (h, t, rs) = (pred.head, pred.compt, pred.headrs)
			for (x <- t.subSets) {
				rPISub0(ie, h, x, rs)
			}
		}
	}
	private[inference] object rPISub extends ((IEngineCore, TermIndex) => Unit) {
		def apply(ie: IEngineCore, tm: TermIndex) {
			if (tm.dim.size >= 2) {
				ie.foreachSubset(tm, Seq.empty[RuleArg], rPISub1)
				ie.foreachMkPI(tm, Seq.empty[RuleArg], rPISub2)
			}
		}
	}
	
	// rule: A -> B, C -> D => A x C -> B x D
	private[inference] object rCPSub1 extends RuleDo[IEPredCP] {
		private[this] def filterCPs(ll: Set[Set[(TermIndex, SemRole)]]) = {
			oneFromEach.map[(TermIndex, SemRole), (mutable.Set[IEPredCP], Set[(TermIndex, SemRole)])](ll, null, (x, y) => {
				val tmp = if (x == null) Set(y) else (x._2 + y)
				(y._1.mkcps.filter(z => tmp.subsetOf(z.comp)), tmp)
			}, _._1.isEmpty)
		}
		def apply(ie: IEngineCore, pred: IEPredCP, args: Seq[RuleArg]) {
			assert(args.isEmpty)
			var task = Nil:List[() => Unit]
			val llsuper = pred.comp.map(x => x._1.superSets.map(y => (y, x._2)))
			val filsuper = filterCPs(llsuper).flatMap(x => for (y <- x._1; if y.head.dim == pred.head.dim) yield ((y.head, y.comp))).toMap
			for ((x, comp) <- filsuper) {
				task = (() => ie.claimSubsume(pred.head, x, Debug_RuleCPSub(pred.head.holder, pred.comp.map(z => (z._1.holder, z._2)), x.holder, comp.map(z => (z._1.holder, z._2)), ie.getNewPredID()))) :: task
			}
			val llsub = pred.comp.filter(!_._1.isW).map(x => x._1.subSets.map(y => (y, x._2)))
			if (!llsub.isEmpty) {
				val filsub = filterCPs(llsub).flatMap(x => for (y <- x._1; if y.head.dim == pred.head.dim) yield ((y.head, y.comp))).toMap
				for ((x, comp) <- filsub) {
					task = (() => ie.claimSubsume(x, pred.head, Debug_RuleCPSub(x.holder, comp.map(z => (z._1.holder, z._2)), pred.head.holder, pred.comp.map(z => (z._1.holder, z._2)), ie.getNewPredID()))) :: task
				}
			}
			task.foreach(_())
		}
	}
	private[inference] object rCPSub2 extends RuleDo[IEPredSubsume] {
		def apply(ie: IEngineCore, pred: IEPredSubsume, args: Seq[RuleArg]) {
			assert(args.isEmpty)
			var task = Nil:List[() => Unit]
			for (x <- pred.subset.mkcps; y <- pred.superset.mkcps; if x.head.dim == y.head.dim) {
				if (y.roleMap.keys.forall(k => y.roleMap(k)._1.isW || (x.roleMap.contains(k) && x.roleMap(k)._1.hasSuper(y.roleMap(k)._1)))) {
					task = (() => ie.claimSubsume(x.head, y.head, Debug_RuleCPSub(x.head.holder, x.comp.map(z => (z._1.holder, z._2)), y.head.holder, y.comp.map(z => (z._1.holder, z._2)), ie.getNewPredID()))) :: task
				}
			}
			task.foreach(_())
		}
	}
	private[inference] object rCPSub extends ((IEngineCore, TermIndex) => Unit) {
		def apply(ie: IEngineCore, tm: TermIndex) {
			if (tm.dim.size >= 2) {
				ie.foreachIsCP(tm, Seq.empty[RuleArg], rCPSub1)
			}
			ie.foreachSubset(tm, Seq.empty[RuleArg], rCPSub2)
		}
	}
	
	// rule: PI1(A) -> C, PI2(A) -> D, ... => A -> C x D x W
	private[inference] object rPIdef1 extends RuleDo[IEPredCP] {
		def apply(ie: IEngineCore, pred: IEPredCP, args: Seq[RuleArg]) {
			assert(args.isEmpty)
			if (!pred.head.isW) {
				val cands = for (k <- pred.roleMap.keySet; x = pred.roleMap(k)._1; if !x.isW) yield {
					for (y <- x.subSets; pi <- y.ispis; if pi.headrs == k && pi.compt.dim == pred.head.dim) yield pi.compt
				}
				val tmp = cands.minBy[Int](_.size).filter(x => cands.forall(_.contains(x)))
				tmp.foreach(x => ie.claimSubsume(x, pred.head, Debug_RulePIdef(x.holder, pred.head.holder, pred.comp.map(z => (z._1.holder, z._2)), ie.getNewPredID())))
			}
		}
	}
	private[inference] trait PIdefCheck {
		def check(cp: IEPredCP, pi: IEPredPI, tm: TermIndex) = {
			cp.roleMap.contains(pi.headrs) && cp.roleMap(pi.headrs)._1 == tm && 
				pi.compt.dim == cp.head.dim && !pi.compt.hasSuper(cp.head) && 
					cp.roleMap.keys.forall(k => k == pi.headrs || cp.roleMap(k)._1.isW || {
						pi.compt.mkpis.exists(x => x.headrs == k && x.head.hasSuper(cp.roleMap(k)._1))
					})
		}
	}
	private[inference] object rPIdef2 extends RuleDo[IEPredSubsume] with PIdefCheck {
		def apply(ie: IEngineCore, pred: IEPredSubsume, args: Seq[RuleArg]) {
			assert(args.isEmpty)
			if (!pred.superset.isW) {
				var task = Nil:List[() => Unit]
				for (pi <- pred.subset.ispis; cp <- pred.superset.mkcps; if check(cp, pi, pred.superset)) {
					task = (() => ie.claimSubsume(pi.compt, cp.head, Debug_RulePIdef(pi.compt.holder, cp.head.holder, cp.comp.map(z => (z._1.holder, z._2)), ie.getNewPredID()))) :: task
				}
				task.foreach(_())
			}
		}
	}
	private[inference] object rPIdef3 extends RuleDo[IEPredPI] with PIdefCheck {
		def apply(ie: IEngineCore, pred: IEPredPI, args: Seq[RuleArg]) {
			assert(args.isEmpty)
			var task = Nil:List[() => Unit]
			for (x <- pred.head.superSets; if !x.isW; cp <- x.mkcps; if check(cp, pred, x)) {
				task = (() => ie.claimSubsume(pred.compt, cp.head, Debug_RulePIdef(pred.compt.holder, cp.head.holder, cp.comp.map(z => (z._1.holder, z._2)), ie.getNewPredID()))) :: task
			}
			task.foreach(_())
		}
	}
	private[inference] object rPIdef extends ((IEngineCore, TermIndex) => Unit) {
		def apply(ie: IEngineCore, tm: TermIndex) {
			if (tm.dim.size >= 2) {
				ie.foreachIsCP(tm, Seq.empty[RuleArg], rPIdef1)
				ie.foreachMkPI(tm, Seq.empty[RuleArg], rPIdef3)
			}
			ie.foreachSubset(tm, Seq.empty[RuleArg], rPIdef2)
		}
	}
	
	// rule: PI(X x A \cap B) = X \cap PI(W x A \cap B)
	private[inference] object rPIWIN0 extends ((IEngineCore, TermIndex, Set[Term], Set[List[(TermIndex, Set[(Term, SemRole)])]], Set[SemRole]) => Unit) {
		def remll(ie: IEngineCore, ncs: Set[TermIndex], rs: Set[SemRole]) = {
			var rem = Set.empty[TermIndex]
			val ll = (for (b <- ncs) yield {
				val tmp = for {
					cp <- b.iscps.toList
					if cp.roleMap.contains(rs)
					(t, r) = cp.roleMap(rs)
					if !t.isW
				} yield {
					((t, cp.comp.map(x => (x._1.holder, x._2)) - ((t.holder, r)) + ((ie.getW(t.dim).holder, r))))
				}
				if (tmp.isEmpty) rem = rem + b
				tmp
			}) - Nil
			(rem.map(_.holder), ll)
		}
		def apply(ie: IEngineCore, a: TermIndex, rem: Set[Term], ll: Set[List[(TermIndex, Set[(Term, SemRole)])]], rs: Set[SemRole]) {
			for (l <- oneFromEach[(TermIndex, Set[(Term, SemRole)])](ll)) {
				val (mm, nn) = l.unzip
				val arglst = Seq[RuleArg](a, mm)
				val inpi = (tms: List[Term]) => {
					ie.constructIN(rem ++ tms, arglst, (x: Term, args: Seq[RuleArg]) => {
						ie.constructPI(x, rs, arglst, (x: Term, args: Seq[RuleArg]) => {
							val comp = mm.toSet + x.index
							ie.claimIN(a, comp, true, Debug_SimpleRuleTrace("rPIWIN", ie.getNewPredID()))
						})
					})
				}
				val cploop = (inpi /: nn)((func, comp) => (tms: List[Term]) => {
					ie.constructCP(comp, arglst, (x: Term, args: Seq[RuleArg]) => func(x :: tms))
				})
				cploop(Nil)
			}
		}
	}
	private[inference] object rPIWIN1 extends RuleDo[IEPredCP] {
		def apply(ie: IEngineCore, pred: IEPredCP, args: Seq[RuleArg]) {
			assert(args.isEmpty)
			var task = Nil:List[() => Unit]
			for (in <- pred.head.mkins; if !in.aux) {
				for (pi <- in.head.mkpis; if pred.roleMap.contains(pi.headrs)) {
					val (rem, ll) = rPIWIN0.remll(ie, in.comp, pi.headrs)
					task = (() => rPIWIN0(ie, pi.head, rem, ll, pi.headrs)) :: task
				}
			}
			task.foreach(_())
		}
	}
	private[inference] object rPIWIN2 extends RuleDo[IEPredIN] {
		def apply(ie: IEngineCore, pred: IEPredIN, args: Seq[RuleArg]) {
			assert(args.isEmpty)
			var task = Nil:List[() => Unit]
			for (pi <- pred.head.mkpis) {
				val (rem, ll) = rPIWIN0.remll(ie, pred.comp, pi.headrs)
				task = (() => rPIWIN0(ie, pi.head, rem, ll, pi.headrs)) :: task
			}
			task.foreach(_())
		}
	}
	private[inference] object rPIWIN3 extends RuleDo[IEPredPI] {
		def apply(ie: IEngineCore, pred: IEPredPI, args: Seq[RuleArg]) {
			assert(args.isEmpty)
			var task = Nil:List[() => Unit]
			for (in <- pred.compt.isins; if !in.aux) {
				val (rem, ll) = rPIWIN0.remll(ie, in.comp, pred.headrs)
				task = (() => rPIWIN0(ie, pred.head, rem, ll, pred.headrs)) :: task
			}
			task.foreach(_())
		}
	}
	private[inference] object rPIWIN extends ((IEngineCore, TermIndex) => Unit) {
		def apply(ie: IEngineCore, tm: TermIndex) {
			if (tm.dim.size >= 2) {
				ie.foreachIsCP(tm, Seq.empty[RuleArg], rPIWIN1)
				ie.foreachIsIN(tm, Seq.empty[RuleArg], rPIWIN2)
				ie.foreachMkPI(tm, Seq.empty[RuleArg], rPIWIN3)
			}
		}
	}
	
	// rule: (A x B) \cap (C x D) = (A \cap C) x (B \cap D)
	private[inference] object rCPIN0 extends ((IEngineCore, TermIndex, Set[List[IEPredCP]]) => Unit) {
		def cll(ncs: Set[TermIndex], rss: Set[Set[SemRole]]) = {
			for (b <- ncs) yield {
				b.iscps.toList.filter(x => x.roleMap.keySet == rss)
			}
		}
		def apply(ie: IEngineCore, a: TermIndex, ll: Set[List[IEPredCP]]) {
			val l = ll.flatten
			val tmp = for ((rs, (t, r)) <- l.head.roleMap.toList) yield {
				(l.map(_.roleMap(rs)._1.holder), r)
			}
			val (tl, rl) = tmp.unzip
			val arglst = Seq[RuleArg](a)
			val cpdo = (ins: List[Term]) => {
				val comp = (ins.map(_.index) zip rl).toSet
				ie.claimCP(a, comp, Debug_SimpleRuleTrace("rCPIN", ie.getNewPredID()))
			}
			val inloop = (cpdo /: tl)((func, comp) => (ins: List[Term]) => {
				ie.constructIN(comp, arglst, (x: Term, args: Seq[RuleArg]) => func(x :: ins))
			})
			inloop(Nil)
		}
	}
	private[inference] object rCPIN1 extends RuleDo[IEPredCP] {
		def apply(ie: IEngineCore, pred: IEPredCP, args: Seq[RuleArg]) {
			assert(args.isEmpty)
			var task = Nil:List[() => Unit]
			for (in <- pred.head.mkins; if !in.aux) {
				val ll = rCPIN0.cll(in.comp, pred.roleMap.keySet)
				if (!ll.contains(Nil)) {
					task = (() => rCPIN0(ie, in.head, ll)) :: task
				}
			}
			task.foreach(_())
		}
	}
	private[inference] object rCPIN2 extends RuleDo[IEPredIN] {
		def apply(ie: IEngineCore, pred: IEPredIN, args: Seq[RuleArg]) {
			assert(args.isEmpty)
			var task = Nil:List[() => Unit]
			val rsss = pred.comp.minBy[Int](_.iscps.size).iscps.map(_.roleMap.keySet)
			for (rss <- rsss) {
				val ll = rCPIN0.cll(pred.comp, rss)
				if (!ll.contains(Nil)) {
					task = (() => rCPIN0(ie, pred.head, ll)) :: task
				}
			}
			task.foreach(_())
		}
	}
	private[inference] object rCPIN extends ((IEngineCore, TermIndex) => Unit) {
		def apply(ie: IEngineCore, tm: TermIndex) {
			if (tm.dim.size >= 2) {
				ie.foreachIsCP(tm, Seq.empty[RuleArg], rCPIN1)
				ie.foreachIsIN(tm, Seq.empty[RuleArg], rCPIN2)
			}
		}
	}
	
}
