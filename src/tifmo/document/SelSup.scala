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
import dcstree.DeclarativeSubsume
import inference.Dimension
import inference.IEngineCore
import inference.IEFunction
import inference.Term
import inference.TermIndex
import inference.RuleArg
import inference.RuleDo
import inference.Debug_SimpleRuleTrace
import inference.IEPredRL
import inference.RAConversion._
import onthefly.AEngine

package document {
	
	case class SelSup(name: String, role: SemRole) extends Selection {
		
		def execute[T](ex: Executor, x: T): T = {
			(ex, x) match {
				case (ie:IEngineCore, tm:Term) => {
					val pitm = if (tm.dim.size == 1) tm else ie.getPI(tm, Set(role))
					val spitm = ie.getFunc(SelSupFunc, Seq(null, pitm), name)
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
						if (sub.selection != null && sub.selection.isInstanceOf[SelSup] && 
								sub.selection == sup.selection) {
							val toProve = toprv.asInstanceOf[(Declarative) => Unit]
							val SelSup(name, role) = sub.selection
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
							val dec1 = DeclarativeSubRef(RefOutput(nsub), RefOutput(nsup))
							ae.maybeHelpful(dec1)
							toProve(dec1)
							val dec2 = DeclarativeSubRef(RefOutput(nsup), RefOutput(nsub))
							ae.maybeHelpful(dec2)
							toProve(dec2)
						}
					}
					case _ => {}
				}).asInstanceOf[T]
				case _ => throw new Exception("unknown executor!!")
			}
		}
	}
	
	private[document] object SelSupFunc extends IEFunction {
		
		def headDim(tms: Seq[Term], param: Any) = tms(1).dim
		
		def applyFunc(ie: IEngineCore, tms: Seq[TermIndex], param: Any) {
			val name = param.asInstanceOf[String]
			tms match {
				case Seq(h, a) => {
					ie.claimSubsume(h, a, Debug_SimpleRuleTrace("SelSupFunc", ie.getNewPredID()))
					ie.claimRL(h, RelPartialOrder(name), a, Debug_SimpleRuleTrace("SelSupFunc", ie.getNewPredID()))
					ie.foreachXRLB(a, Seq(h, name), rSelSupFunc0)
				}
				case _ => throw new Exception("SelSupFunc error!")
			}
		}
	}
	
	private[document] object rSelSupFunc0 extends RuleDo[IEPredRL] {
		def apply(ie: IEngineCore, pred: IEPredRL, args: Seq[RuleArg]) {
			args match {
				case Seq(RuleArg(h:TermIndex), RuleArg(name:String)) => pred.rl match {
					case RelPartialOrder(nm) => if (nm == name) {
						ie.claimSubsume(pred.a, h, Debug_SimpleRuleTrace("rSelSupFunc0", ie.getNewPredID()))
					}
					case _ => {}
				}
				case _ => throw new Exception("rSelSupFunc0 error!")
			}
		}
	}
	
}

