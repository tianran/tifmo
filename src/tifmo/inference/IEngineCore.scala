package tifmo

import dcstree.Executor
import dcstree.SemRole
import dcstree.Relation

import scala.collection.mutable

package inference {
	
	import Finder._
	import RulesQuick._
	import RulesLight._
	
	/**
	 * The core of inference engine. 
	 * 
	 * This class implements the forward chaining algorithm, provides the 
	 * infra structure for writing rules, and implements basic axioms of 
	 * relational algebra in a way that can avoid most combinatorial explosions. 
	 */
	class IEngineCore extends Executor {
		
		private[this] var predCounter = 0
		def getNewPredID() = {
			predCounter += 1
			predCounter
		}
		
		// lock
		/**
		 * Locking mechanism for debug.
		 * 
		 * If locked, an error will occur if the status of the inference engine is changed 
		 * (e.g. new atomic sentences proven).
		 */
		var locked = false
		
		// W
		protected[this] var W = null:TermIndex
		protected[this] val WPool = mutable.Map.empty[Dimension, TermIndex]
		/**
		 * Get the W term.
		 */
		def getW(dim: Dimension) = {
			if (W == null) {
				assert(!locked)
				W = new TermIndex(new Dimension(null))
				W.setwflag()
				IEPredNonEmpty(W).apply(Debug_SimpleRuleTrace("W", getNewPredID()))
				IEPredSubsume(W, W).apply(Debug_SimpleRuleTrace("W", getNewPredID()))
				anyTermPool.foreach(_(this, W))
				WPool(W.dim) = W
			}
			WPool.getOrElseUpdate(dim, {
				assert(!locked)
				val ret = new TermIndex(dim)
				ret.setwflag()
				IEPredNonEmpty(ret).apply(Debug_SimpleRuleTrace("W", getNewPredID()))
				IEPredSubsume(ret, ret).apply(Debug_SimpleRuleTrace("W", getNewPredID()))
				anyTermPool.foreach(_(this, ret))
				claimCP(ret, dim.relabel(null).map(r => (W, r)), Debug_SimpleRuleTrace("W", getNewPredID()))
				ret
			})
		}
		
		// new term
		def newTerm(dim: Dimension) = {
			assert(!locked)
			val w = getW(dim)
			val term = new TermIndex(w.dim)
			IEPredSubsume(term, term).apply(Debug_SimpleRuleTrace("tm id", getNewPredID()))
			anyTermPool.foreach(_(this, term))
			for (x <- w.superSets) {
				applySubsume(term, x, Debug_SimpleRuleTrace("tm sub W", getNewPredID()))
			}
			term
		}
		
		// contradiction: 
		private[this] var contraFlag = false
		/**
		 * Returns if there has been a contradiction during forward chaining. 
		 */
		def hasContradiction = contraFlag
		def contradict() {
			contraFlag = true
		}
		
		// five hierarchy forward chaining: 
		
		private[this] val quick = mutable.Queue.empty[() => Unit]
		private[inference] def newQuick(todo: () => Unit) {
			quick.enqueue(todo)
		}
		private[this] val mergePool = mutable.Set.empty[TermIndex]
		private[this] val light = mutable.Queue.empty[(() => Unit, Set[TermIndex])]
		private[inference] def newLight(todo: () => Unit, dep: Set[TermIndex]) {
			light.enqueue((todo, dep))
		}
		private[this] class Sched[T <: IEPred](pred: T, f: RuleDo[T], args: Seq[RuleArg]) {
			private[this] val dep = args.flatMap(_.terms).toSet
			def sched() { if (pred.valid && dep.forall(_.valid)) f(IEngineCore.this, pred, args) }
		}
		private[this] val heavy = mutable.Queue.empty[Sched[_ <: IEPred]]
		private[inference] def newHeavy[T <: IEPred](pred: T, f: RuleDo[T], args: Seq[RuleArg]) {
			heavy.enqueue(new Sched[T](pred, f, args))
		}
		private[this] val constructQ = mutable.Queue.empty[() => Unit]
		
		/**
		 * Forward chaining process. 
		 */
		def explore() {
			
			def loop() {
				if (!quick.isEmpty) {
					quick.dequeue()()
					loop()
				} else if (!mergePool.isEmpty) {
					val cache = mutable.Set.empty[TermIndex]
					var most = 0
					var tomerge = null:Set[TermIndex]
					for (x <- mergePool; if !cache.contains(x)) {
						val tmp = x.subSets intersect x.superSets
						cache ++= tmp
						if (tmp.size > most) {
							most = tmp.size
							tomerge = tmp
						}
					}
					mergePool --= tomerge
					assert(tomerge.size >= 2)
					val a = tomerge.find(_.isW) match {
						case Some(x) => x
						case None => {
							tomerge.maxBy[Int](x => x.iscps.size + x.mkcps.size + 
								x.isins.size + x.mkins.size + x.ispis.size + x.mkpis.size)
						}
					}
					val eqs = tomerge - a
					merge(a, eqs)
					loop()
				} else if (!light.isEmpty) {
					val (todo, dep) = light.dequeue()
					if (dep.forall(_.valid)) todo()
					loop()
				} else if (!heavy.isEmpty) {
					heavy.dequeue().sched()
					loop()
				} else if (!constructQ.isEmpty) {
					constructQ.dequeue()()
					loop()
				}
			}
			loop()
		}
		
		// apply & claim
		
		private[inference] def applyNonEmpty(a: TermIndex, debug_trace: Debug_RuleTrace) = {
			val tmp = IEPredNonEmpty(a)
			if (tmp.apply(debug_trace)) {
				assert(!locked)
				rqPINE1(this, tmp)
				rqPINE2(this, tmp)
				rqCPNE1(this, tmp)
				rqCPNE2(this, tmp)
				rqPICP3(this, tmp)
				for (x <- a.neTriggers) x.fire(this, tmp)
				if (a.selfDisjoint) contradict()
				true
			} else {
				false
			}
		}
		/**
		 * Claim a term to be non-empty.
		 */
		def claimNonEmpty(a: TermIndex, debug_trace: Debug_RuleTrace = Debug_SimpleRuleTrace("default", getNewPredID())) {
			if (applyNonEmpty(a, debug_trace)) {
				for (x <- a.superSets) {
					applyNonEmpty(x, Debug_RuleSubNE(a.holder, x.holder, getNewPredID()))
				}
			}
		}
		
		private[inference] def applyDisjoint(a: TermIndex, b: TermIndex, debug_trace: Debug_RuleTrace) = {
			val tmp = IEPredDisjoint(a, b)
			if (tmp.apply(debug_trace)) {
				assert(!locked)
				for (x <- a.djtTriggers) x.fire(this, tmp)
				if (a == b) {
					if (a.knownNE) contradict()
				} else {
					val tmp2 = IEPredDisjoint(b, a)
					val rec = tmp2.apply(Debug_SimpleRuleTrace("djt rev", getNewPredID()))
					assert(rec)
					for (x <- b.djtTriggers) x.fire(this, tmp2)
				}
				true
			} else {
				false
			}
		}
		/**
		 * Claim two terms to be disjoint.
		 */
		def claimDisjoint(a: TermIndex, b: TermIndex, debug_trace: Debug_RuleTrace = Debug_SimpleRuleTrace("default", getNewPredID())) {
			assert(a.dim == b.dim)
			
			if (applyDisjoint(a, b, debug_trace)) {
				newLight(() => rlDjtSubPI1(this, a, b), Set(a, b))
			}
		}
		
		private[inference] def applySubsume(a: TermIndex, b: TermIndex, debug_trace: Debug_RuleTrace) = {
			val tmp = IEPredSubsume(a, b)
			if (tmp.apply(debug_trace)) {
				assert(!locked)
				if (a.hasSub(b)) mergePool += a
				for (in <- b.mkins; if in.comp.forall(_.hasSub(a))) {
					newQuick(() => claimSubsume(a, in.head, Debug_RuleINdef(a.holder, in.head.holder, in.comp.map(_.holder), getNewPredID())))
				}
				for (x <- a.subTriggers) x.fire(this, tmp)
				for (x <- b.superTriggers) x.fire(this, tmp)
				true
			} else {
				false
			}
		}
		/**
		 * Claim term `a` to be a subset of term `b`.
		 */
		def claimSubsume(a: TermIndex, b: TermIndex, debug_trace: Debug_RuleTrace = Debug_SimpleRuleTrace("default", getNewPredID())) {
			assert(a.dim == b.dim)
			if (applySubsume(a, b, debug_trace)) {
				val bsup = b.superSets
				newLight(() => rlDjtSubPI2(this, a, b), Set(a, b))
				
				var task = Nil:List[() => Boolean]
				for (x <- a.subSets) {
					if (!x.hasSuper(b) || x == a) {
						for (y <- bsup) {
							task = (() => applySubsume(x, y, Debug_RuleSubSub(x.holder, a.holder, b.holder, y.holder, getNewPredID()))) :: task
						}
					} else {
						assert(bsup.forall(x.hasSuper(_)))
					}
				}
				task.foreach(_())
				
				if (a.knownNE) {
					claimNonEmpty(b, Debug_RuleSubNE(a.holder, b.holder, getNewPredID()))
				}
			}
		}
		
		private[inference] def applyCP(h: TermIndex, t: Set[(TermIndex, SemRole)], debug_trace: Debug_RuleTrace) = {
			val tmp = IEPredCP(h, t)
			if (tmp.apply(debug_trace)) {
				assert(!locked)
				val mincps = t.minBy[Int](_._1.mkcps.size)._1.mkcps
				for (cp <- mincps; if cp.comp == t && cp != tmp) {
					newQuick(() => claimSubsume(h, cp.head, Debug_SimpleRuleTrace("CP Uniqueness", getNewPredID())))
					newQuick(() => claimSubsume(cp.head, h, Debug_SimpleRuleTrace("CP Uniqueness", getNewPredID())))
				}
				rqCPNE3(this, tmp)
				rqCPNE4(this, tmp)
				rqPICP1(this, tmp)
				for (x <- t.map(_._1); y <- x.mkcpTriggers) y.fire(this, tmp)
				for (x <- h.iscpTriggers) x.fire(this, tmp)
				true
			} else {
				false
			}
		}
		/**
		 * Claim term `h` to be the Cartesian product of `t`
		 */
		def claimCP(h: TermIndex, t: Set[(TermIndex, SemRole)], debug_trace: Debug_RuleTrace = Debug_SimpleRuleTrace("default", getNewPredID())) {
			assert{
				val hrs = h.dim.relabel(null)
				val rss = t.map(x => x._1.dim.relabel(x._2))
				t.size >= 2 && hrs == rss.flatten && hrs.size == (0 /: rss)(_ + _.size)
			}
			
			if (applyCP(h, t, debug_trace)) {
				newLight(() => rlCPCP(this, h, t), t.map(_._1) + h)
			}
		}
		
		private[inference] def applyPI(h: TermIndex, t: TermIndex, r: SemRole, debug_trace: Debug_RuleTrace) = {
			val tmp = IEPredPI(h, t, r)
			if (tmp.apply(debug_trace)) {
				assert(!locked)
				for (pi <- t.mkpis; if pi.headrs == tmp.headrs && pi != tmp) {
					newQuick(() => claimSubsume(h, pi.head, Debug_SimpleRuleTrace("PI Uniqueness", getNewPredID())))
					newQuick(() => claimSubsume(pi.head, h, Debug_SimpleRuleTrace("PI Uniqueness", getNewPredID())))
				}
				rqPINE3(this, tmp)
				rqPINE4(this, tmp)
				rqPICP2(this, tmp)
				newLight(() => rlDjtSubPI3(this, h, t, r), Set(h, t))
				for (x <- t.mkpiTriggers) x.fire(this, tmp)
				for (x <- h.ispiTriggers) x.fire(this, tmp)
				true
			} else {
				false
			}
		}
		/**
		 * Claim term `h` to be the projection of term `t` into role `r`
		 */
		def claimPI(h: TermIndex, t: TermIndex, r: SemRole, debug_trace: Debug_RuleTrace = Debug_SimpleRuleTrace("default", getNewPredID())) {
			assert{
				val hrs = h.dim.relabel(r)
				val trs = t.dim.relabel(null)
				hrs.subsetOf(trs) && trs != hrs
			}
			
			if (applyPI(h, t, r, debug_trace)) {
				newLight(() => rlPIPI(this, h, t, r), Set(h, t))
			}
		}
		
		private[this] def esssub(x: Set[TermIndex], y: Set[TermIndex]) = {
			x.subsetOf(y.flatMap(z => z.subSets intersect z.superSets))
		}
		private[inference] def applyIN(h: TermIndex, t: Set[TermIndex], aux: Boolean, debug_trace: Debug_RuleTrace) = {
			h.isins.filter(in => in.aux && esssub(t, in.comp)).foreach(_.dispose())
			val tmp = IEPredIN(h, t, aux)
			if (tmp.apply(debug_trace)) {
				assert(!locked)
				val subs = t.minBy[Int](_.assuper.size).subSets.filter(x => t.forall(_.hasSub(x)))
				for (x <- subs) {
					newQuick(() => claimSubsume(x, h, Debug_RuleINdef(x.holder, h.holder, t.map(_.holder), getNewPredID())))
				}
				if (!aux) {
					for (x <- h.isinTriggers) x.fire(this, tmp)
					for (x <- t; y <- x.mkinTriggers) y.fire(this, tmp)
				}
				true
			} else {
				false
			}
		}
		/**
		 * Claim term `h` to be the intersection of terms `t`
		 */
		def claimIN(h: TermIndex, t: Set[TermIndex], aux: Boolean = false, debug_trace: Debug_RuleTrace = Debug_SimpleRuleTrace("default", getNewPredID())) {
			assert(t.forall(_.dim == h.dim))
			assert(aux || h.dim.size >= 2)
			
			t.foreach(claimSubsume(h, _, Debug_SimpleRuleTrace("IN def", getNewPredID())))
			
			def squeeze(t: Set[TermIndex]) = {
				val tmp = for (x <- t; if !t.exists(y => y.hasSuper(x) && !x.hasSuper(y))) yield {
					x.superSets intersect x.subSets
				}
				tmp.map(_.head)
			}
			
			if (aux) {
				val mt = squeeze(t)
				if (mt.size <= 1) {
					claimSubsume(mt.head, h, Debug_SimpleRuleTrace("IN Check", getNewPredID()))
				} else if (!h.isins.exists(in => esssub(in.comp, mt))) {
					applyIN(h, mt, true, debug_trace)
				}
			} else {
				applyIN(h, t, false, debug_trace)
			}
			
		}
		
		/**
		 * Claim `rl(a, b)` for the user-defined binary relation `rl`.
		 */
		def claimRL(a: TermIndex, rl: Relation, b: TermIndex, debug_trace: Debug_RuleTrace = Debug_SimpleRuleTrace("default", getNewPredID())) {
			val tmp = IEPredRL(a, rl, b)
			if (tmp.apply(debug_trace)) {
				assert(!locked)
				for (x <- a.arlTriggers) x.fire(this, tmp)
				for (x <- b.rlbTriggers) x.fire(this, tmp)
				rl.execute[TermIndex](this, a, b)
			}
		}
		
		/**
		 * Claim `tms.head = func(tms.tail, param)` for the user-defined function `func`.
		 */
		def claimFunc(func: IEFunction, tms: Seq[TermIndex], param: Any, debug_trace: Debug_RuleTrace = Debug_SimpleRuleTrace("default", getNewPredID())) {
			assert(tms.head.dim == func.headDim(null +: tms.tail.map(_.holder), param))
			if (IEPredFunc(func, tms, param).apply(debug_trace)) {
				assert(!locked)
				if (!tms.tail.isEmpty) {
					val minfuncs = tms.tail.minBy[Int](_.funcs.size).funcs
					for (x <- minfuncs; if x.func == func && x.param == param && x.tms.tail == tms.tail) {
						claimSubsume(tms.head, x.tms.head, Debug_SimpleRuleTrace("Func Uniqueness", getNewPredID()))
						claimSubsume(x.tms.head, tms.head, Debug_SimpleRuleTrace("Func Uniqueness", getNewPredID()))
					}
				}
				func.applyFunc(this, tms, param)
			}
		}
		
		// merge: 
		private[this] def merge(a: TermIndex, eqs: Set[TermIndex]) {
			
			eqs.foreach(_.holder.idx = a)
			
			for (x <- eqs) {
				assert(a.hasSuper(x))
				assert(a.hasSub(x))
				assert(a.knownNE == x.knownNE)
				for (y <- x.superSets; if !eqs.contains(y)) assert(a.hasSuper(y))
				for (y <- x.subSets; if !eqs.contains(y)) assert(a.hasSub(y))
			}
			
			for (x <- eqs) {
				x.kne.toList.foreach(_.dispose())
				x.assub.toList.foreach(_.dispose())
				x.assuper.toList.foreach(_.dispose())
			}
			
			def replace(x: TermIndex) = if (eqs.contains(x)) a else x
			
			var task = Nil:List[() => Unit]
			for (x <- eqs) {
				for (djt <- x.djts) {
					val na = replace(djt.a)
					val nb = replace(djt.b)
					val ntr = djt.debug_trace
					task = (() => claimDisjoint(na, nb, ntr)) :: task
				}
				for (cp <- x.iscps) {
					val ncomp = cp.comp.map(y => (replace(y._1), y._2))
					val ntr = cp.debug_trace
					task = (() => claimCP(a, ncomp, ntr)) :: task
				}
				for (cp <- x.mkcps) {
					val nhead = replace(cp.head)
					val ncomp = cp.comp.map(y => (replace(y._1), y._2))
					val ntr = cp.debug_trace
					task = (() => claimCP(nhead, ncomp, ntr)) :: task
				}
				for (pi <- x.ispis) {
					val ncompt = replace(pi.compt)
					val ncompr = pi.compr
					val ntr = pi.debug_trace
					task = (() => claimPI(a, ncompt, ncompr, ntr)) :: task
				}
				for (pi <- x.mkpis) {
					val nhead = replace(pi.head)
					val ncompr = pi.compr
					val ntr = pi.debug_trace
					task = (() => claimPI(nhead, a, ncompr, ntr)) :: task
				}
				for (in <- x.isins) {
					val ncomp = in.comp.map(replace(_))
					val naux = in.aux
					val ntr = in.debug_trace
					task = (() => claimIN(a, ncomp, naux, ntr)) :: task
				}
				for (in <- x.mkins) {
					val nhead = replace(in.head)
					val ncomp = in.comp.map(replace(_))
					val naux = in.aux
					val ntr = in.debug_trace
					task = (() => claimIN(nhead, ncomp, naux, ntr)) :: task
				}
				for (fc <- x.funcs) {
					val nfunc = fc.func
					val ntms = fc.tms.map(replace(_))
					val nparam = fc.param
					val ntr = fc.debug_trace
					task = (() => claimFunc(nfunc, ntms, nparam, ntr)) :: task
				}
				for (rel <- x.asarl) {
					val nb = replace(rel.b)
					val nrl = rel.rl
					val ntr = rel.debug_trace
					task = (() => claimRL(a, nrl, nb, ntr)) :: task
				}
				for (rel <- x.asrlb) {
					val na = replace(rel.a)
					val nrl = rel.rl
					val ntr = rel.debug_trace
					task = (() => claimRL(na, nrl, a, ntr)) :: task
				}
			}
			for (x <- eqs) {
				for (y <- x.djts.toList) {
					y.dispose()
					IEPredDisjoint(y.b, y.a).dispose()
				}
				x.iscps.toList.foreach(_.dispose())
				x.mkcps.toList.foreach(_.dispose())
				x.ispis.toList.foreach(_.dispose())
				x.mkpis.toList.foreach(_.dispose())
				x.isins.toList.foreach(_.dispose())
				x.mkins.toList.foreach(_.dispose())
				x.funcs.toList.foreach(_.dispose())
				x.asarl.toList.foreach(_.dispose())
				x.asrlb.toList.foreach(_.dispose())
			}
			task.foreach(_())
			
			for (x <- eqs) {
				x.disposers.toList.foreach(_())
			}
			
			val toreloc = mutable.Set.empty[Guard[_ <: IEPred]]
			for (x <- eqs) {
				x.neTriggers.foreach(tg => {
					tg.guard.change(tg, a)
					toreloc += tg.guard
				})
				x.subTriggers.foreach(tg => {
					tg.guard.change(tg, a)
					toreloc += tg.guard
				})
				x.superTriggers.foreach(tg => {
					tg.guard.change(tg, a)
					toreloc += tg.guard
				})
				x.djtTriggers.foreach(tg => {
					tg.guard.change(tg, a)
					toreloc += tg.guard
				})
				x.iscpTriggers.foreach(tg => {
					tg.guard.change(tg, a)
					toreloc += tg.guard
				})
				x.mkcpTriggers.foreach(tg => {
					tg.guard.change(tg, a)
					toreloc += tg.guard
				})
				x.ispiTriggers.foreach(tg => {
					tg.guard.change(tg, a)
					toreloc += tg.guard
				})
				x.mkpiTriggers.foreach(tg => {
					tg.guard.change(tg, a)
					toreloc += tg.guard
				})
				x.isinTriggers.foreach(tg => {
					tg.guard.change(tg, a)
					toreloc += tg.guard
				})
				x.mkinTriggers.foreach(tg => {
					tg.guard.change(tg, a)
					toreloc += tg.guard
				})
				x.arlTriggers.foreach(tg => {
					tg.guard.change(tg, a)
					toreloc += tg.guard
				})
				x.rlbTriggers.foreach(tg => {
					tg.guard.change(tg, a)
					toreloc += tg.guard
				})
			}
			toreloc.foreach(_.locate(this))
			
			for (x <- eqs) {
				assert(x.kne.isEmpty)
				assert(x.assub.isEmpty)
				assert(x.assuper.isEmpty)
				assert(x.djts.isEmpty)
				assert(x.iscps.isEmpty)
				assert(x.mkcps.isEmpty)
				assert(x.ispis.isEmpty)
				assert(x.mkpis.isEmpty)
				assert(x.isins.isEmpty)
				assert(x.mkins.isEmpty)
				assert(x.funcs.isEmpty)
				assert(x.asarl.isEmpty)
				assert(x.asrlb.isEmpty)
				assert(x.disposers.isEmpty)
				assert(x.neTriggers.isEmpty)
				assert(x.subTriggers.isEmpty)
				assert(x.superTriggers.isEmpty)
				assert(x.djtTriggers.isEmpty)
				assert(x.iscpTriggers.isEmpty)
				assert(x.mkcpTriggers.isEmpty)
				assert(x.ispiTriggers.isEmpty)
				assert(x.mkpiTriggers.isEmpty)
				assert(x.isinTriggers.isEmpty)
				assert(x.mkinTriggers.isEmpty)
				assert(x.arlTriggers.isEmpty)
				assert(x.rlbTriggers.isEmpty)
			}
		}
		
		// foreach: 
		private[this] val anyTermPool = mutable.Set.empty[(IEngineCore, TermIndex) => Unit]
		protected def forAnyTerm(f: (IEngineCore, TermIndex) => Unit) {
			anyTermPool.add(f)
		}
		/**
		 * Setup a callback function which is activated when `term` is known to be non-empty.
		 */
		def ifNotEmpty(term: TermIndex, args: Seq[RuleArg], f: RuleDo[IEPredNonEmpty]) {
			assert(term.valid)
			(new Watcher[IEPredNonEmpty]("ne", f, args)).initialize(term, this)
		}
		/**
		 * Setup a callback function which is activated whenever `term` has a new super-set.
		 */
		def foreachSuperset(term: TermIndex, args: Seq[RuleArg], f: RuleDo[IEPredSubsume]) {
			assert(term.valid)
			(new Watcher[IEPredSubsume]("sub", f, args)).initialize(term, this)
		}
		/**
		 * Setup a callback function which is activated whenever `term` has a new subset.
		 */
		def foreachSubset(term: TermIndex, args: Seq[RuleArg], f: RuleDo[IEPredSubsume]) {
			assert(term.valid)
			(new Watcher[IEPredSubsume]("super", f, args)).initialize(term, this)
		}
		/**
		 * Setup a callback function which is activated whenever `term` has a new disjoint.
		 */
		def foreachDisjoint(term: TermIndex, args: Seq[RuleArg], f: RuleDo[IEPredDisjoint]) {
			assert(term.valid)
			(new Watcher[IEPredDisjoint]("djt", f, args)).initialize(term, this)
		}
		/**
		 * Setup a callback function which is activated whenever `term` is known to be a Cartesian product.
		 */
		def foreachIsCP(term: TermIndex, args: Seq[RuleArg], f: RuleDo[IEPredCP]) {
			assert(term.valid)
			(new Watcher[IEPredCP]("iscp", f, args)).initialize(term, this)
		}
		/**
		 * Setup a callback function which is activated whenever `term` is known to be a component of some Cartesian product.
		 */
		def foreachMkCP(term: TermIndex, args: Seq[RuleArg], f: RuleDo[IEPredCP]) {
			assert(term.valid)
			(new Watcher[IEPredCP]("mkcp", f, args)).initialize(term, this)
		}
		/**
		 * Setup a callback function which is activated whenever `term` is known to be a projection.
		 */
		def foreachIsPI(term: TermIndex, args: Seq[RuleArg], f: RuleDo[IEPredPI]) {
			assert(term.valid)
			(new Watcher[IEPredPI]("ispi", f, args)).initialize(term, this)
		}
		/**
		 * Setup a callback function which is activated whenever some other term is known to be a projection of `term`.
		 */
		def foreachMkPI(term: TermIndex, args: Seq[RuleArg], f: RuleDo[IEPredPI]) {
			assert(term.valid)
			(new Watcher[IEPredPI]("mkpi", f, args)).initialize(term, this)
		}
		/**
		 * Setup a callback function which is activated whenever `term` is known to be an intersection.
		 */
		def foreachIsIN(term: TermIndex, args: Seq[RuleArg], f: RuleDo[IEPredIN]) {
			assert(term.valid)
			(new Watcher[IEPredIN]("isin", f, args)).initialize(term, this)
		}
		/**
		 * Setup a callback function which is activated whenever `term` is known to be a component of an intersection.
		 */
		def foreachMkIN(term: TermIndex, args: Seq[RuleArg], f: RuleDo[IEPredIN]) {
			assert(term.valid)
			(new Watcher[IEPredIN]("mkin", f, args)).initialize(term, this)
		}
		/**
		 * Setup a callback function which is activated whenever a relation `rl(term, x)` is known, for some user-defined relation `rl` and some term `x`.
		 */
		def foreachARLX(term: TermIndex, args: Seq[RuleArg], f: RuleDo[IEPredRL]) {
			assert(term.valid)
			(new Watcher[IEPredRL]("arl", f, args)).initialize(term, this)
		}
		/**
		 * Setup a callback function which is activated whenever a relation `rl(x, term)` is known, for some user-defined relation `rl` and some term `x`.
		 */
		def foreachXRLB(term: TermIndex, args: Seq[RuleArg], f: RuleDo[IEPredRL]) {
			assert(term.valid)
			(new Watcher[IEPredRL]("rlb", f, args)).initialize(term, this)
		}
		/**
		 * Setup a callback function which is activated when the Cartesian product of `comp` is constructed.
		 */
		def forCPof(comp: Set[(TermIndex, SemRole)], args: Seq[RuleArg], f: RuleDo[IEPredCP]) {
			comp.map(_._1).foreach(_.valid)
			(new ForCPof(f, args)).initialize(comp, this)
		}
		/**
		 * Setup a callback function which is activated when `a` is known to be subsumed by `b`.
		 */
		def ifSubsume(a: TermIndex, b: TermIndex, args: Seq[RuleArg], f: RuleDo[IEPredSubsume]) {
			assert(a.valid)
			assert(b.valid)
			(new IfSubsume(f, args)).initialize(a, b, this)
		}
		/**
		 * Setup a callback function which is activated when `a` is known to be disjoint to `b`.
		 */
		def ifDisjoint(a: TermIndex, b: TermIndex, args: Seq[RuleArg], f: RuleDo[IEPredDisjoint]) {
			assert(a.valid)
			assert(b.valid)
			(new IfDisjoint(f, args)).initialize(a, b, this)
		}
		/**
		 * Setup a callback function which is activated when there is a relation `rl(a, b)`.
		 */
		def ifRelation(a: TermIndex, b: TermIndex, args: Seq[RuleArg], f: RuleDo[IEPredRL]) {
			assert(a.valid)
			assert(b.valid)
			(new IfRelation(f, args)).initialize(a, b, this)
		}
		
		// getter: 
		/**
		 * Get Cartesian product.
		 */
		def getCP(comp: Set[(Term, SemRole)]) = findCP(comp) match {
			case Some(x) => x
			case None => {
				val dim = new Dimension(comp.flatMap(x => x._1.dim.relabel(x._2)))
				val ret = newTerm(dim)
				claimCP(ret, comp.map(x => (x._1.index, x._2)), Debug_SimpleRuleTrace("getCP", getNewPredID()))
				ret.holder
			}
		}
		/**
		 * Get projection.
		 */
		def getPI(compt: Term, headrs: Set[SemRole]) = findPI(compt, headrs) match {
			case Some(x) => x
			case None => {
				val (dim, r) = Dimension(headrs)
				val ret = newTerm(dim)
				claimPI(ret, compt.index, r, Debug_SimpleRuleTrace("getPI", getNewPredID()))
				ret.holder
			}
		}
		/**
		 * Get intersection.
		 */
		def getIN(comp: Set[Term], aux: Boolean = false) = findIN(comp) match {
			case Some(x) => {
				if (!aux && x.dim.size >= 2) {
					claimIN(x.index, comp.map(_.index), false, Debug_SimpleRuleTrace("getIN", getNewPredID()))
				}
				x
			}
			case None => {
				val dim = comp.head.dim
				val ret = newTerm(dim)
				claimIN(ret, comp.map(_.index), aux || dim.size == 1, Debug_SimpleRuleTrace("getIN", getNewPredID()))
				ret.holder
			}
		}
		/**
		 * Get `func(tms.tail, param)` for the user-defined function `func`.
		 */
		def getFunc(func: IEFunction, tms: Seq[Term], param: Any) = findFunc(func, tms, param) match {
			case Some(x) => x
			case None => {
				val dim = func.headDim(tms, param)
				val ret = newTerm(dim)
				claimFunc(func, ret +: tms.tail.map(_.index), param, Debug_SimpleRuleTrace("getFunc", getNewPredID()))
				ret.holder
			}
		}
		
		// constructors: 
		def construct(finder: () => Option[Term], getter: () => Term, args: Seq[RuleArg], f: (Term, Seq[RuleArg]) => Unit) {
			val dep = args.flatMap(_.terms).toSet
			assert(dep.forall(_.valid))
			finder() match {
				case Some(x) => f(x, args)
				case None => constructQ.enqueue(() => if (dep.forall(_.valid)) f(getter(), args))
			}
		}
		
		def constructCP(comp: Set[(Term, SemRole)], args: Seq[RuleArg], f: (Term, Seq[RuleArg]) => Unit) {
			val finder = () => findCP(comp)
			val getter = () => getCP(comp)
			construct(finder, getter, args, f)
		}
		
		def constructPI(compt: Term, headrs: Set[SemRole], args: Seq[RuleArg], f: (Term, Seq[RuleArg]) => Unit) {
			val finder = () => findPI(compt, headrs)
			val getter = () => getPI(compt, headrs)
			construct(finder, getter, args, f)
		}
		
		def constructIN(comp: Set[Term], args: Seq[RuleArg], f: (Term, Seq[RuleArg]) => Unit) {
			val finder = () => findIN(comp)
			val getter = () => getIN(comp, true)
			construct(finder, getter, args, f)
		}
		
		////////////////////////////////////////////////////
		
		forAnyTerm(rPISub)
		forAnyTerm(rCPSub)
		forAnyTerm(rPIdef)
		forAnyTerm(rPIWIN)
		forAnyTerm(rCPIN)
		
	}
	
}
