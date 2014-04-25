package tifmo

import scala.collection.mutable

package dcstree {
	
	class DCSTreeNode(
		val children: Set[(DCSTreeEdge, DCSTreeNode)], 
		val rseq: Seq[SemRole], 
		val token: TokenBase,
		val sign: Boolean, 
		val selection: Selection, 
		val outRole: SemRole, 
		val context: Set[Context] = Set.empty[Context]
	) extends Serializable {
		
		private[this] var aprx = null:Denotation
		def approx = aprx
		
		private[this] var hfcl = null:Denotation
		def halfcalc = hfcl
		
		private[this] var otpt = null:Denotation
		def output = otpt
		
		private[this] val gm = mutable.Map.empty[SemRole, Denotation]
		def germ(r: SemRole) = gm(r)
		
		private[this] def calcr(d: Denotation, r: SemRole) = {
			for ((DCSTreeEdgeRelation(rr, rel), n) <- children; if rr == r) {
				n.upward()
				n.downward(null)
			}
			val fil = for ((DCSTreeEdgeQuantifier(rr, qt), n) <- children; if rr == r) yield {
				n.upward()
				n.downward(null)
				DenotationDI(qt, d, n.output, r):Denotation
			}
			if (fil.isEmpty) {
				DenotationPI(d, d.roles - r)
			} else if (fil.size == 1) {
				fil.head
			} else {
				DenotationIN(fil)
			}
		}
		
		private[this] def makePI(x: Denotation, r: SemRole) = {
			if (x.roles.size == 1) {
				assert(x.roles.head == r)
				x
			} else {
				DenotationPI(x, Set(r))
			}
		}
		
		private[this] def makeRelabel(x: Denotation, r: SemRole) = {
			assert(x.roles.size == 1)
			if (x.roles.head == r) x else DenotationRelabel(x, r)
		}
		
		private[this] def makeINCPW(c: Denotation, s: Set[(SemRole, Denotation)]) = {
			if (s.isEmpty) {
				c
			} else {
				val rs = s.map(_._1)
				assert(rs.subsetOf(c.roles))
				val side = for (r <- rs) yield {
					val fil = s.filter(_._1 == r).map(x => makeRelabel(x._2, x._1))
					if (fil.size <= 1) fil.head else DenotationIN(fil)
				}
				val rest = c.roles -- rs
				val precp = if (rest.isEmpty) side else side + DenotationW(rest)
				val cp = if (precp.size == 1) precp.head else DenotationCP(precp)
				DenotationIN(Set(c, cp))
			}
		}
		
		def upward() {
			if (otpt == null) {
				val rs = rseq.toSet
				val c = if (token.getWord.isStopWord) DenotationW(rs) else DenotationWordSign(rs, token.getWord, sign)
				val s1 = for ((DCSTreeEdgeNormal(r), n) <- children) yield {
					n.upward()
					(r, n.output)
				}
				val s2 = for (ContextA(r, ref) <- context) yield {
					(r, ref.getDenotation)
				}
				val pre = makeINCPW(c, s1 ++ s2)
				aprx = if (selection == null) pre else DenotationSelection(selection, pre)
				hfcl = (approx /: rseq.takeWhile(_ != outRole))((d, r) => {
					gm(r) = makePI(d, r)
					for ((edge @ DCSTreeEdgeNormal(rr), n) <- children; if rr == r) {
						n.downward((this, edge))
					}
					calcr(d, r)
				})
				otpt = makePI(halfcalc, outRole)
			}
		}
		
		private[this] var prt = null:(DCSTreeNode, DCSTreeEdgeNormal)
		def parent = prt
		
		private[this] var downFlag = false
		private[this] var posinega = true
		def positiveTree = {
			assert(downFlag)
			posinega
		}
		def downward(p: (DCSTreeNode, DCSTreeEdgeNormal)) {
			if (downFlag) {
				assert(parent == p)
				assert(posinega)
			} else {
				assert(p == null || p._1.children.contains((p._2, this)))
				prt = p
				downFlag = true
				val tmp1 = if (p == null) Set(output) else Set(p._1.germ(p._2.inRole))
				val tmp2 = for (ContextB(ref) <- context) yield {
					ref.getDenotation
				}
				val tmp = tmp1 ++ tmp2
				gm(outRole) = if (tmp.size == 1) {
					tmp.head
				} else {
					DenotationIN(tmp.map(makeRelabel(_, outRole)))
				}
				for ((edge @ DCSTreeEdgeNormal(rr), n) <- children; if rr == outRole) {
					n.downward((this, edge))
				}
				val halfdash = if (p == null && tmp2.isEmpty) {
					halfcalc
				} else {
					makeINCPW(halfcalc, Set((outRole, germ(outRole))))
				}
				(calcr(halfdash, outRole) /: rseq.dropWhile(_ != outRole).tail)((d, r) => {
					gm(r) = makePI(d, r)
					for ((edge @ DCSTreeEdgeNormal(rr), n) <- children; if rr == r) {
						n.downward((this, edge))
					}
					calcr(d, r)
				})
			}
		}
		
		def downwardNega(p: (DCSTreeNode, DCSTreeEdgeNormal)) {
			if (downFlag) {
				assert(parent == p)
				assert(!posinega)
			} else {
				assert(p == null || p._1.children.contains((p._2, this)))
				prt = p
				posinega = false
				downFlag = true
				gm(outRole) = if (p == null) output else p._1.germ(p._2.inRole)
				for ((edge @ DCSTreeEdgeNormal(rr), n) <- children; if rr == outRole) {
					n.downwardNega((this, edge))
				}
				val halfdash = if (p == null) {
					halfcalc
				} else {
					makeINCPW(halfcalc, Set((outRole, germ(outRole))))
				}
				calcr(halfdash, outRole)
				(DenotationPI(halfdash, halfdash.roles - outRole) /: rseq.dropWhile(_ != outRole).tail)((d, r) => {
					gm(r) = makePI(d, r)
					for ((edge @ DCSTreeEdgeNormal(rr), n) <- children; if rr == r) {
						n.downwardNega((this, edge))
					}
					calcr(d, r)
					DenotationPI(d, d.roles - r)
				})
			}
		}
		
		val outIndex = rseq.indexOf(outRole)
		
		def compare2outRole(thisRole: SemRole) = {
			rseq.indexOf(thisRole) - outIndex
		}
		
		@transient lazy val ascending: Seq[(DCSTreeNode, DCSTreeEdgeNormal)] = {
			if (parent == null) {
				Seq.empty[(DCSTreeNode, DCSTreeEdgeNormal)]
			} else {
				if (parent._1.compare2outRole(parent._2.inRole) < 0) {
					Seq(parent)
				} else {
					parent +: parent._1.ascending
				}
			}
		}
		
		@transient lazy val descending: Set[(Seq[(DCSTreeEdgeNormal, DCSTreeNode)], Ref)] = {
			if (selection == null) {
				children.flatMap(_ match {
					case (e:DCSTreeEdgeNormal, n) => {
						n.descending.map(x => (((e, n)) +: x._1, x._2)) + ((Seq((e, n)), RefOutput(n):Ref))
					}
					case _ => Set.empty[(Seq[(DCSTreeEdgeNormal, DCSTreeNode)], Ref)]
				})
			} else {
				Set.empty[(Seq[(DCSTreeEdgeNormal, DCSTreeNode)], Ref)]
			}
		}
		
		def copy: DCSTreeNode = new DCSTreeNode(
			children.map(x => (x._1, x._2.copy)), 
			rseq, 
			token, 
			sign, 
			selection, 
			outRole, 
			context
		)
		
	}
	
}

