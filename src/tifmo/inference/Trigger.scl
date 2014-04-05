package tifmo

import dcstree.SemRole

import scala.collection.mutable

package inference {
	
	private[inference] class Trigger[T <: IEPred](
			private[inference] val guard: Guard[T], 
			pool: mutable.Set[Trigger[T]]) {
		
		private[inference] def dispatch() = {
			pool.add(this)
		}
		
		private[inference] def dispose() {
			pool.remove(this)
		}
		
		private[inference] def fire(ie: IEngineCore, pred: T) {
			guard.trig(ie, pred, this)
		}
		
		override def equals(a: Any) = a.isInstanceOf[Trigger[_ <: IEPred]] && {
			val that = a.asInstanceOf[Trigger[T]]
			guard == that.guard && guard.triggerRole(this) == that.guard.triggerRole(that)
		}
		override def hashCode = {
			val tmp = guard.triggerRole(this)
			assert(tmp != -1)
			(guard, tmp).hashCode
		}
	}
	
	private[inference] abstract class Guard[T <: IEPred](
			private[inference] val f: RuleDo[T], 
			private[inference] val args: Seq[RuleArg]
		) extends Serializable {
		
		private[inference] def trig(ie: IEngineCore, pred: T, tg: Trigger[T]): Unit
		private[inference] def dumpMe(tmmap: Map[TermIndex, TermIndex]): Guard[T]
		
		private[this] var curtms = null:Array[TermIndex]
		protected[this] var curcache = null:IndexedSeq[TermIndex]
		protected def cur = curcache
		private[this] var tns = null:IndexedSeq[String]
		private[inference] def setup(tms: IndexedSeq[TermIndex], tnames: IndexedSeq[String]) {
			curtms = tms.toArray
			curcache = tms
			tns = tnames
		}
		
		private[inference] def change(tg: Trigger[T], term: TermIndex) {
			curtms(curtgs.indexWhere(tg eq _)) = term
		}
		
		private[inference] val deps = args.flatMap(_.terms).toSet
		
		@transient private[this] var curtgs = null:IndexedSeq[Trigger[T]]
		private[inference] def locate(ie: IEngineCore) = {
			if (curtgs != null) curtgs.foreach(_.dispose())
			curcache = curtms.toIndexedSeq
			assert(deps.forall(_.valid))
			curtgs = (cur zip tns).map(x => new Trigger[T](this, x._1.triggerPool[T](x._2)))
			val tmp = curtgs.map(_.dispatch())
			assert(tmp.tail.forall(_ == tmp.head))
			if (tmp.head) {
				if (ie != null) for (p <- cur.head.predPool[T](tns.head)) curtgs.head.fire(ie, p)
				true
			} else {
				deps.foreach(_.disposers -= dis)
				false
			}
		}
		
		private[this] val dis: (() => Unit) = () => {
			curtgs.foreach(_.dispose())
			deps.foreach(_.disposers -= dis)
		}
		private[inference] def setDis() {
			deps.foreach(_.disposers += dis)
		}
		
		private[inference] def triggerRole(tg: Trigger[T]) = curtgs.indexWhere(tg eq _)
	}
	
	private[inference] class Watcher[T <: IEPred](val tname: String, 
			f: RuleDo[T], args: Seq[RuleArg]) extends Guard[T](f, args) {
		
		private[inference] def initialize(x: TermIndex, ie: IEngineCore) {
			setup(IndexedSeq(x), IndexedSeq(tname))
			if (locate(ie)) setDis()
		}
		private[inference] def trig(ie: IEngineCore, pred: T, tg: Trigger[T]) {
			assert(triggerRole(tg) == 0)
			ie.newHeavy[T](pred, f, args)
		}
		private[inference] def dumpMe(tmmap: Map[TermIndex, TermIndex]) = {
			val ret = new Watcher[T](tname, f, args.map(_.dumpMe(tmmap)))
			ret.setup(cur.map(tmmap(_)), IndexedSeq(tname))
			ret
		}
		
		override def equals(a: Any) = a.isInstanceOf[Watcher[_ <: IEPred]] && {
			val that = a.asInstanceOf[Watcher[_ <: IEPred]]
			cur == that.cur && tname == that.tname && f == that.f && args == that.args
		}
		override def hashCode = ("Watcher", cur, tname, f, args).hashCode
	}
	
	private[inference] class ForCPof(f: RuleDo[IEPredCP], args: Seq[RuleArg]) extends Guard[IEPredCP](f, args) {
		
		private var rs = null:IndexedSeq[SemRole]
		private[inference] def initialize(comp: Set[(TermIndex, SemRole)], ie: IEngineCore) {
			val (tms, trs) = comp.toIndexedSeq.unzip
			setup(tms, tms.map(_ => "mkcp"))
			rs = trs
			if (locate(ie)) setDis()
		}
		private def current = (cur zip rs).toSet
		private[inference] def trig(ie: IEngineCore, pred: IEPredCP, tg: Trigger[IEPredCP]) {
			if (triggerRole(tg) == 0 && pred.comp == current) ie.newHeavy[IEPredCP](pred, f, args)
		}
		private[inference] def dumpMe(tmmap: Map[TermIndex, TermIndex]) = {
			val ret = new ForCPof(f, args.map(_.dumpMe(tmmap)))
			ret.setup(cur.map(tmmap(_)), cur.map(_ => "mkcp"))
			ret.rs = rs
			ret
		}
		override def equals(a: Any) = a.isInstanceOf[ForCPof] && {
			val that = a.asInstanceOf[ForCPof]
			current == that.current && f == that.f && args == that.args
		}
		override def hashCode = ("ForCPof", current, f, args).hashCode
	}
	
	private[inference] class IfSubsume(f: RuleDo[IEPredSubsume], args: Seq[RuleArg]) extends Guard[IEPredSubsume](f, args) {
		
		private[inference] def initialize(a: TermIndex, b: TermIndex, ie: IEngineCore) {
			setup(IndexedSeq(a, b), IndexedSeq("sub", "super"))
			if (locate(ie)) setDis()
		}
		private[inference] def trig(ie: IEngineCore, pred: IEPredSubsume, tg: Trigger[IEPredSubsume]) {
			if (triggerRole(tg) == 0) {
				assert(pred.subset == cur(0))
				if (pred.superset == cur(1)) ie.newHeavy[IEPredSubsume](pred, f, args)
			}
		}
		private[inference] def dumpMe(tmmap: Map[TermIndex, TermIndex]) = {
			val ret = new IfSubsume(f, args.map(_.dumpMe(tmmap)))
			ret.setup(cur.map(tmmap(_)), IndexedSeq("sub", "super"))
			ret
		}
		override def equals(a: Any) = a.isInstanceOf[IfSubsume] && {
			val that = a.asInstanceOf[IfSubsume]
			cur == that.cur && f == that.f && args == that.args
		}
		override def hashCode = ("IfSubsume", cur, f, args).hashCode
	}
	
	private[inference] class IfDisjoint(f: RuleDo[IEPredDisjoint], args: Seq[RuleArg]) extends Guard[IEPredDisjoint](f, args) {
		
		private[inference] def initialize(a: TermIndex, b: TermIndex, ie: IEngineCore) {
			setup(IndexedSeq(a, b), IndexedSeq("djt", "djt"))
			if (locate(ie)) setDis()
		}
		private[inference] def trig(ie: IEngineCore, pred: IEPredDisjoint, tg: Trigger[IEPredDisjoint]) {
			if (triggerRole(tg) == 0) {
				assert(pred.a == cur(0))
				if (pred.b == cur(1)) ie.newHeavy[IEPredDisjoint](pred, f, args)
			}
		}
		private[inference] def dumpMe(tmmap: Map[TermIndex, TermIndex]) = {
			val ret = new IfDisjoint(f, args.map(_.dumpMe(tmmap)))
			ret.setup(cur.map(tmmap(_)), IndexedSeq("djt", "djt"))
			ret
		}
		override def equals(a: Any) = a.isInstanceOf[IfDisjoint] && {
			val that = a.asInstanceOf[IfDisjoint]
			cur == that.cur && f == that.f && args == that.args
		}
		override def hashCode = ("IfDisjoint", cur, f, args).hashCode
	}
	
	private[inference] class IfRelation(f: RuleDo[IEPredRL], args: Seq[RuleArg]) extends Guard[IEPredRL](f, args) {
		
		private[inference] def initialize(a: TermIndex, b: TermIndex, ie: IEngineCore) {
			setup(IndexedSeq(a, b), IndexedSeq("arl", "rlb"))
			if (locate(ie)) setDis()
		}
		private[inference] def trig(ie: IEngineCore, pred: IEPredRL, tg: Trigger[IEPredRL]) {
			if (triggerRole(tg) == 0) {
				assert(pred.a == cur(0))
				if (pred.b == cur(1)) ie.newHeavy[IEPredRL](pred, f, args)
			}
		}
		private[inference] def dumpMe(tmmap: Map[TermIndex, TermIndex]) = {
			val ret = new IfRelation(f, args.map(_.dumpMe(tmmap)))
			ret.setup(cur.map(tmmap(_)), IndexedSeq("arl", "rlb"))
			ret
		}
		override def equals(a: Any) = a.isInstanceOf[IfRelation] && {
			val that = a.asInstanceOf[IfRelation]
			cur == that.cur && f == that.f && args == that.args
		}
		override def hashCode = ("IfRelation", cur, f, args).hashCode
	}
	
}
