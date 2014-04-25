package tifmo

import dcstree.Executor
import dcstree.SelCorefBase
import dcstree.Declarative
import dcstree.Ref
import inference.IEngineCore
import inference.IEFunction
import inference.Term
import inference.TermIndex
import inference.FuncSingle
import inference.Debug_SimpleRuleTrace
import onthefly.AEngine

package document {
	
	case class SelCoref(id: String) extends SelCorefBase {
		
		def execute[T](ex: Executor, x: T): T = {
			(ex, x) match {
				case (ie:IEngineCore, tm:Term) => {
					ie.getFunc(SelCorefFunc, Seq(null, tm), id).asInstanceOf[T]
				}
				case (ae:AEngine, toProve:Function1[_, _]) => ((d: Declarative) => {}).asInstanceOf[T]
				case _ => throw new Exception("unknown executor!!")
			}
		}
		
	}
	
	private[document] object SelCorefFunc extends IEFunction {
		
		def headDim(tms: Seq[Term], param: Any) = tms(1).dim
		
		def applyFunc(ie: IEngineCore, tms: Seq[TermIndex], param: Any) {
			tms match {
				case Seq(h, a) => {
					ie.claimSubsume(h, a, Debug_SimpleRuleTrace("SelCorefFunc", ie.getNewPredID()))
					ie.claimFunc(FuncSingle, Seq(h), h.dim, Debug_SimpleRuleTrace("SelCorefFunc", ie.getNewPredID()))
				}
				case _ => throw new Exception("SelCorefFunc error!")
			}
		}
	}
	
}

