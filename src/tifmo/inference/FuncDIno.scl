package tifmo

import dcstree.SemRole

package inference {
	
	import RAConversion._
	
	/**
	 * Division (QuantifierNO). 
	 * 
	 * `tms = Seq(h, a, b)`, `param = r` means that `h = q^r(a, b)`.
	 */
	object FuncDIno extends IEFunction {
		
		def headDim(tms: Seq[Term], param: Any) = {
			val r = param.asInstanceOf[SemRole]
			Dimension(tms(1).dim.decrease(r))._1
		}
		
		def applyFunc(ie: IEngineCore, tms: Seq[TermIndex], param: Any) {
			val r = param.asInstanceOf[SemRole]
			tms match {
				case Seq(h, a, b) => {
					val hrs = a.dim.decrease(r)
					val pia = ie.getPI(a.holder, hrs)
					val (hdim, hr) = Dimension(hrs)
					val tot = ie.getCP(Set((pia, hr), (ie.getW(b.dim).holder, r)))
					val acomp = ie.getFunc(FuncComplement, Seq(null, a.holder, tot), null)
					ie.claimFunc(FuncDIall, Seq(h, acomp.index, b), r, Debug_SimpleRuleTrace("FuncDIno", ie.getNewPredID()))
					val bw = ie.getCP(Set((b.holder, r), (ie.getW(hdim).holder, hr)))
					val hcompl = ie.getPI(ie.getIN(Set(bw, a.holder)), hrs)
					ie.claimFunc(FuncComplement, Seq(h, hcompl.index, pia.index), null, Debug_SimpleRuleTrace("FuncDIno", ie.getNewPredID()))
				}
				case _ => throw new Exception("FuncDIno error!")
			}
		}
	}
	
}
