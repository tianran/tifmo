package tifmo

import dcstree.SemRole
import dcstree.WordBase
import dcstree.TokenBase
import dcstree.DCSTreeNode
import dcstree.DCSTreeEdge
import dcstree.DCSTreeEdgeNormal
import dcstree.DCSTreeEdgeQuantifier
import dcstree.DCSTreeEdgeRelation
import dcstree.Declarative
import dcstree.DeclarativePosi
import dcstree.DeclarativeNega
import dcstree.DeclarativeSubsume

import mylib.misc.oneFromEach

import scala.collection.mutable

package document {
	
	/**
	 * Document class provides an interface for making DCS trees.
	 */
	class Document(
		val id: String, 
		val tokens: IndexedSeq[Token]
	) extends Serializable {
		
		for (i <- 0 until tokens.length; tok = tokens(i)) {
			assert(tok.doc == null)
			assert(tok.id == -1)
			tok.xdoc = this
			tok.xid = i
		}
		
		private[this] val nodes = mutable.Map.empty[Token, TokenNode]
		def getTokenNode(token: Token) = nodes.getOrElseUpdate(token, {
			assert(token.doc == this)
			assert(token.id != -1)
			new TokenNode(token)
		})
		
		def allRootNodes = {
			nodes.values.filter(x => x.parent == null && !x.children.filter(!_._2.conj).isEmpty).toSet
		}
		
		def allContentWords[T <: WordBase] = {
			val ret = mutable.Set.empty[T]
			def recur(x: TokenNode) {
				val word = x.token.getWord.asInstanceOf[T]
				if (!word.isStopWord) ret += word
				for ((r, n) <- x.children) recur(n)
			}
			for (n <- allRootNodes) recur(n)
			ret.toSet
		}
		
		def makeDeclaratives = {
			val ret = mutable.Set.empty[Declarative]
			
			val corefnums = mutable.Map.empty[String, Int]
			for (n <- allRootNodes) {
				def recur(x: TokenNode) {
					if (x.token.corefID != null) corefnums(x.token.corefID) = 
						corefnums.getOrElse(x.token.corefID, 0) + 1
					for ((r, nn) <- x.children) recur(nn)
				}
				recur(n)
			}
			
			val corefnodes = mutable.Map.empty[String, Set[DCSTreeNode]]
			for (n <- allRootNodes) {
				
				def getConjNodes(x: TokenNode) = {
					val ret = mutable.Set.empty[TokenNode]
					def recur(y: TokenNode) {
						val (conjs, notconjs) = y.children.partition(_._2.conj)
						if (!conjs.isEmpty) ret += y
						for ((r, nn) <- notconjs) recur(nn)
					}
					recur(x)
					ret.toSet
				}
				val conjFlags = mutable.Set.empty[Map[TokenNode, TokenNode]]
				def loopFlag(x: Set[(Map[TokenNode, TokenNode], Set[TokenNode])]) {
					if (!x.isEmpty) {
						val (ready, notend) = x.partition(_._2.isEmpty)
						ready.foreach(y => conjFlags += y._1)
						val next = for {
							(cur, todo) <- notend
							ll = todo.map(y => {
								(for ((r, nn) <- y.children; if nn.conj) yield (y, nn)) + ((y, null))
							})
							l <- oneFromEach[(TokenNode, TokenNode)](ll)
						} yield {
							val tmp = l.map(_._2).toSet - null
							(cur ++ l, tmp.flatMap(getConjNodes(_)))
						}
						loopFlag(next)
					}
				}
				loopFlag(Set((Map.empty[TokenNode, TokenNode], getConjNodes(n))))
				
				for (flag <- conjFlags) {
					def recurDCSTreeNode(x: TokenNode, exchildren: Set[(SemRole, TokenNode)]): DCSTreeNode = {
						val (conjs, notconjs) = x.children.partition(_._2.conj)
						val conjnode = if (conjs.isEmpty) null else flag(x)
						if (conjnode == null) {
							val reccoref = (x.token.corefID != null && corefnums(x.token.corefID) >= 2)
							val ret = new DCSTreeNode(
								(for ((r, nn) <- notconjs ++ exchildren) yield {
									val e = if (nn.quantifier != null) {
										DCSTreeEdgeQuantifier(r, nn.quantifier)
									} else if (nn.relation != null) {
										DCSTreeEdgeRelation(r, nn.relation)
									} else {
										DCSTreeEdgeNormal(r)
									}
									(e, recurDCSTreeNode(nn, Set.empty[(SemRole, TokenNode)]))
								}), 
								x.rseq, 
								x.token, 
								x.sign, 
								if (reccoref) SelCoref(id + "@" + x.token.corefID) else x.selection, 
								x.outRole
							)
							if (reccoref) corefnodes(x.token.corefID) = 
								corefnodes.getOrElse(x.token.corefID, Set.empty[DCSTreeNode]) + ret
							ret
						} else {
							recurDCSTreeNode(conjnode, notconjs ++ exchildren)
						}
					}
					val root = recurDCSTreeNode(n, Set.empty[(SemRole, TokenNode)])
					ret += (if (n.rootNeg) DeclarativeNega(root) else DeclarativePosi(root))
				}
			}
			
			val tokenmap = mutable.Map.empty[TokenBase, Set[TokenBase]]
			for (n <- allRootNodes) {
				val tokenpool = mutable.Set.empty[TokenBase]
				def recur(x: TokenNode) {
					tokenpool += x.token
					for ((r, nn) <- x.children) recur(nn)
				}
				recur(n)
				for (x <- tokenpool) {
					tokenmap(x) = tokenmap.getOrElse(x, Set.empty[TokenBase]) ++ tokenpool
				}
			}
			
			for (nodes <- corefnodes.values) {
				for (s <- nodes.subsets(2)) {
					val a = s.head
					val b = (s - a).head
					ret += DeclarativeSubsume(a, b)
					ret += DeclarativeSubsume(b, a)
				}
				
				for (n <- nodes) {
					val fil = nodes.filter(x => {
						!tokenmap(n.token).contains(x.token)
					})
					assert(n.selection.asInstanceOf[SelCoref].coreferentNodes.isEmpty)
					n.selection.asInstanceOf[SelCoref].coreferentNodes ++= fil
				}
			}
			
			ret.toSet
		}
		
	}
	
}
