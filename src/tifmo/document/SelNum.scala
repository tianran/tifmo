package tifmo

import dcstree.SemRole
import dcstree.Executor
import dcstree.Selection
import dcstree.DCSTreeNode
import dcstree.DCSTreeEdge
import dcstree.DCSTreeEdgeNormal
import dcstree.RefOutput
import dcstree.Declarative
import dcstree.DeclarativeSubRef
import dcstree.DeclarativeNotEmptyRef
import dcstree.DeclarativeSubsume
import dcstree.Relation
import inference.Dimension
import inference.IEngineCore
import inference.IEFunction
import inference.Term
import inference.TermIndex
import inference.RuleArg
import inference.RuleDo
import inference.Debug_SimpleRuleTrace
import inference.IEPredNonEmpty
import inference.IEPredSubsume
import inference.IEPredRL
import inference.RAConversion._
import onthefly.AEngine

package document {
	
	case class SelNum(num: String, role: SemRole) extends Selection {
		
		def execute[T](ex: Executor, x: T): T = {
			(ex, x) match {
				case (ie:IEngineCore, tm:Term) => {
					val pitm = if (tm.dim.size == 1) tm else ie.getPI(tm, Set(role))
					val spitm = ie.getFunc(SelNumFunc, Seq(null, pitm), num)
					val side = if (tm.dim.size == 1) {
						spitm
					} else {
						val tmrs = tm.dim.relabel(null)
						val (wdim, wr) = Dimension(tmrs - role)
						ie.getCP(Set((spitm, role), (ie.getW(wdim).holder, wr)))
					}
					ie.getIN(Set(tm, side)).asInstanceOf[T]
				}
				case (ae:AEngine, toprv:Function1[_, _]) => ((d: Declarative) => d match {
					case DeclarativeSubsume(sub, sup) => {
						if (sub.selection != null && sub.selection.isInstanceOf[SelNum] && 
								sub.selection == sup.selection && 
								ae.premise.contains(DeclarativeNotEmptyRef(RefOutput(sub)))) {
							val toProve = toprv.asInstanceOf[(Declarative) => Unit]
							val SelNum(num, role) = sub.selection
							val nsub = new DCSTreeNode(
								for ((e:DCSTreeEdgeNormal, n) <- sub.children) yield (e, n.copy), 
								sub.rseq, 
								sub.token, 
								sub.sign, 
								null, 
								role
							)
							nsub.upward()
							nsub.downward(null)
							val nsup = new DCSTreeNode(
								for ((e:DCSTreeEdgeNormal, n) <- sup.children) yield (e, n.copy), 
								sup.rseq, 
								sup.token, 
								sup.sign, 
								null, 
								role
							)
							nsup.upward()
							nsup.downward(null)
							val dec = DeclarativeSubRef(RefOutput(nsub), RefOutput(nsup))
							ae.maybeHelpful(dec)
							toProve(dec)
						}
					}
					case _ => {}
				}).asInstanceOf[T]
				case _ => throw new Exception("unknown executor!!")
			}
		}
	}
	
	private[document] object SelNumFunc extends IEFunction {
		
		def headDim(tms: Seq[Term], param: Any) = tms(1).dim
		
		def applyFunc(ie: IEngineCore, tms: Seq[TermIndex], param: Any) {
			val num = param.asInstanceOf[String]
			tms match {
				case Seq(h, a) => {
					ie.claimSubsume(h, a, Debug_SimpleRuleTrace("SelNumFunc", ie.getNewPredID()))
					ie.claimRL(h, RelNum(num), a, Debug_SimpleRuleTrace("SelNumFunc", ie.getNewPredID()))
					ie.ifNotEmpty(a, Seq(h, num), rSelNumFunc2)
				}
				case _ => throw new Exception("SelSupFunc error!")
			}
		}
	}
	
	private[document] case class RelNum(num: String) extends Relation {
		def execute[T](ex: Executor, a: T, b: T) {}
	}
	
	private[document] object rSelNumFunc2 extends RuleDo[IEPredNonEmpty] {
		def apply(ie: IEngineCore, pred: IEPredNonEmpty, args: Seq[RuleArg]) {
			ie.foreachSuperset(pred.term, args, rSelNumFunc1)
		}
	}
	
	private[document] object rSelNumFunc1 extends RuleDo[IEPredSubsume] {
		def apply(ie: IEngineCore, pred: IEPredSubsume, args: Seq[RuleArg]) {
			ie.foreachXRLB(pred.superset, args, rSelNumFunc0)
		}
	}
	
	private[document] object rSelNumFunc0 extends RuleDo[IEPredRL] {
		def apply(ie: IEngineCore, pred: IEPredRL, args: Seq[RuleArg]) {
			args match {
				case Seq(RuleArg(h:TermIndex), RuleArg(num:String)) => pred.rl match {
					case RelNum(nm) => if (nm == num) {
						ie.claimSubsume(h, pred.a, Debug_SimpleRuleTrace("rSelNumFunc0", ie.getNewPredID()))
					}
					case _ => {}
				}
				case _ => throw new Exception("rSelNumFunc0 error!")
			}
		}
	}
	
	
}

