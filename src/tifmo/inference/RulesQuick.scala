package tifmo


package inference {
	
	private[inference] object RulesQuick {
		
		// rule: T = PI(A), (T.ne <=> A.ne)
		private[inference] def rqPINE1(ie: IEngineCore, tmp: IEPredNonEmpty) {
			val a = tmp.term
			for (pi <- a.ispis) {
				val tr = Debug_RulePINE(a.holder, pi.compt.holder, pi.compr, ie.getNewPredID())
				ie.newQuick(() => ie.claimNonEmpty(pi.compt, tr))
			}
		}
		private[inference] def rqPINE2(ie: IEngineCore, tmp: IEPredNonEmpty) {
			val b = tmp.term
			for (pi <- b.mkpis) {
				val tr = Debug_RulePINE(pi.head.holder, b.holder, pi.compr, ie.getNewPredID())
				ie.newQuick(() => ie.claimNonEmpty(pi.head, tr))
			}
		}
		private[inference] def rqPINE3(ie: IEngineCore, tmp: IEPredPI) {
			val a = tmp.head
			if (a.knownNE) {
				val tr = Debug_RulePINE(a.holder, tmp.compt.holder, tmp.compr, ie.getNewPredID())
				ie.newQuick(() => ie.claimNonEmpty(tmp.compt, tr))
			}
		}
		private[inference] def rqPINE4(ie: IEngineCore, tmp: IEPredPI) {
			val b = tmp.compt
			if (b.knownNE) {
				val tr = Debug_RulePINE(tmp.head.holder, b.holder, tmp.compr, ie.getNewPredID())
				ie.newQuick(() => ie.claimNonEmpty(tmp.head, tr))
			}
		}
		
		// rule: T = A x B x ..., (A.ne, B.ne, ... <=> T.ne)
		private[inference] def rqCPNE1(ie: IEngineCore, tmp: IEPredNonEmpty) {
			val a = tmp.term
			for (cp <- a.iscps) {
				val tr = Debug_RuleCPNE(a.holder, cp.comp.map(x => (x._1.holder, x._2)), ie.getNewPredID())
				for (x <- cp.comp.map(_._1)) ie.newQuick(() => ie.claimNonEmpty(x, tr))
			}
		}
		private[inference] def rqCPNE2(ie: IEngineCore, tmp: IEPredNonEmpty) {
			val b = tmp.term
			for (cp <- b.mkcps; if cp.comp.forall(_._1.knownNE)) {
				val tr = Debug_RuleCPNE(cp.head.holder, cp.comp.map(x => (x._1.holder, x._2)), ie.getNewPredID())
				ie.newQuick(() => ie.claimNonEmpty(cp.head, tr))
			}
		}
		private[inference] def rqCPNE3(ie: IEngineCore, tmp: IEPredCP) {
			val a = tmp.head
			if (a.knownNE) {
				val tr = Debug_RuleCPNE(a.holder, tmp.comp.map(x => (x._1.holder, x._2)), ie.getNewPredID())
				for (x <- tmp.comp.map(_._1)) ie.newQuick(() => ie.claimNonEmpty(x, tr))
			}
		}
		private[inference] def rqCPNE4(ie: IEngineCore, tmp: IEPredCP) {
			if (tmp.comp.forall(_._1.knownNE)) {
				val tr = Debug_RuleCPNE(tmp.head.holder, tmp.comp.map(x => (x._1.holder, x._2)), ie.getNewPredID())
				ie.newQuick(() => ie.claimNonEmpty(tmp.head, tr))
			}
		}
		
		// rule: T = PI((X x Y x ...) x A x B x ...), A.ne, B.ne, ... => T = X x Y x ...
		private[this] def rqPICP0(ie: IEngineCore, pi: IEPredPI, cp: IEPredCP) {
			val fil = cp.roleMap.keySet.filter(_.subsetOf(pi.headrs))
			if (fil.flatten == pi.headrs) {
				val tr = Debug_RulePICP(pi.head.holder, pi.compt.holder, pi.compr, cp.head.holder, cp.comp.map(x => (x._1.holder, x._2)), ie.getNewPredID())
				val comp = fil.map(cp.roleMap(_))
				val cl = cp.comp.map(_._1) -- comp.map(_._1)
				if (fil.size == 1) {
					ie.newQuick(() => ie.claimSubsume(pi.head, comp.head._1, tr))
					if (cl.forall(_.knownNE)) ie.newQuick(() => ie.claimSubsume(comp.head._1, pi.head, tr))
				} else {
					if (cl.forall(_.knownNE)) ie.newQuick(() => ie.claimCP(pi.head, comp, tr))
				}
			}
		}
		private[inference] def rqPICP1(ie: IEngineCore, pred: IEPredCP) {
			for (pi <- pred.head.mkpis) {
				rqPICP0(ie, pi, pred)
			}
		}
		private[inference] def rqPICP2(ie: IEngineCore, pred: IEPredPI) {
			for (cp <- pred.compt.iscps) {
				rqPICP0(ie, pred, cp)
			}
		}
		private[inference] def rqPICP3(ie: IEngineCore, pred: IEPredNonEmpty) {
			for (cp <- pred.term.mkcps; pi <- cp.head.mkpis) {
				rqPICP0(ie, pi, cp)
			}
		}
		
	}
	
}
