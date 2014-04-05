package mylib


package misc {

	class FibonacciHeap[T >: Null <: AnyRef] {
		
		type Node = FibonacciHeap.Node[T]
		
		private[misc] var min = null:Node
		/** The minimum node in the heap. */
		def minNode = min
		
		private[misc] var n = 0
		/** Number of nodes in the heap. */
		def size = n

		/**
		 * Removes all elements from this heap.
		 */
		def clear() {
			min = null
			n = 0
		}
		
		/**
		 * Decreases the key value for a heap node, given the new value
		 * to take on. The structure of the heap may be changed, but will
		 * not be consolidated.
		 *
		 * @param  x  node to decrease the key of
		 * @param  k  new key value for node x
		 */
		def decreaseKey(x: Node, k: Double) {
			if (k < x.key) {
				x.key = k
				val y = x.parent
				if (y != null) {
					y.cut(x, min)
					y.cascadingCut(min)
				}
				if (k < min.key) {
					min = x
				}
			}
		}
		
		/**
		 * Deletes a node from the heap given the reference to the node.
		 * The trees in the heap will be consolidated, if necessary.
		 *
		 * @param  x  node to remove from heap.
		 */
		def delete(x: Node) {
			val y = x.parent
			if (y != null) {
				y.cut(x, min)
				y.cascadingCut(min)
			}
			min = x
			removeMin()
		}
		
		def isEmpty = (min == null)
		
		/**
		 * Inserts a new data element into the heap. No heap consolidation
		 * is performed at this time, the new node is simply inserted into
		 * the root list of this heap.
		 *
		 * @param  x    data object to insert into heap.
		 * @param  key  key value associated with data object.
		 * @return newly created heap node.
		 */
		def insert(x: T, key: Double) = {
			val node = new Node(x, key)
			if (min != null) {
				node.right = min
				node.left = min.left
				min.left = node
				node.left.right = node
				if (key < min.key) {
					min = node
				}
			} else {
				min = node
			}
			n += 1
			node
		}
		
		/**
		 * Removes the smallest element from the heap. This will cause
		 * the trees in the heap to be consolidated, if necessary.
		 *
		 * @return  data object with the smallest key.
		 */
		def removeMin() = {
			val z = min
			if (z == null) {
				null:T
			} else {
				if (z.child != null) {
					z.child.parent = null
					var x = z.child.right
					while (x != z.child) {
						x.parent = null
						x = x.right
					}
					val minleft = min.left
					val zchildleft = z.child.left
					min.left = zchildleft
					zchildleft.right = min
					z.child.left = minleft
					minleft.right = z.child
				}
				z.left.right = z.right
				z.right.left = z.left
				if (z == z.right) {
					min = null
				} else {
					min = z.right
					consolidate()
				}
				n -= 1
				z.data
			}
		}
		
		private[this] def consolidate() {
			val A = new Array[Node](45)
			
			var start = min
			var w = min
			do {
				var x = w
				var nextW = w.right
				var d = x.degree
				while (A(d) != null) {
					var y = A(d)
					if (x.key > y.key) {
						val tmp = y
						y = x
						x = tmp
					}
					if (y == start) {
						start = start.right
					}
					if (y == nextW) {
						nextW = nextW.right
					}
					y.link(x)
					A(d) = null
					d += 1
				}
				A(d) = x
				w = nextW
			} while (w != start)
			
			min = start
			for (a <- A; if a != null) {
				if (a.key < min.key) min = a
			}
		}
		
	}

	object FibonacciHeap {
		
		/** Implements a node of the Fibonacci heap. */
		class Node[T](val data: T, origv: Double) {
			private[misc] var key = origv
			def value = key
			
			private[misc] var parent = null:Node[T]
			private[misc] var child = null:Node[T]
			private[misc] var right = this
			private[misc] var left = this
			
			private[misc] var degree = 0
			private[misc] var mark = false
			
			private[misc] def cascadingCut(min: Node[T]) {
				val z = parent
				if (z != null) {
					if (mark) {
						z.cut(this, min)
						z.cascadingCut(min)
					} else {
						mark = true
					}
				}
			}
			
			private[misc] def cut(x: Node[T], min: Node[T]) {
				x.left.right = x.right
				x.right.left = x.left
				degree -= 1
				if (degree == 0) {
					child = null
				} else if (child == x) {
					child = x.right
				}
				x.right = min
				x.left = min.left
				min.left = x
				x.left.right = x
				x.parent = null
				x.mark = false
			}
			
			private[misc] def link(prt: Node[T]) {
				left.right = right
				right.left = left
				parent = prt
				if (prt.child == null) {
					prt.child = this
					right = this
					left = this
				} else {
					left = prt.child
					right = prt.child.right
					prt.child.right = this
					right.left = this
				}
				prt.degree += 1
				mark = false
			}
		}
		
		/**
		 * Joins two Fibonacci heaps into a new one. No heap consolidation is
		 * performed at this time. The two root lists are simply joined together.
		 *
		 * @param  H1  first heap
		 * @param  H2  second heap
		 * @return  new heap containing H1 and H2
		 */
		def union[T >: Null <: AnyRef](H1: FibonacciHeap[T], H2: FibonacciHeap[T]) = {
			val H = new FibonacciHeap[T]
			H.min = H1.min
			if (H.min != null) {
				if (H2.min != null) {
					H.min.right.left = H2.min.left
					H2.min.left.right = H.min.right
					H.min.right = H2.min
					H2.min.left = H.min
					if (H2.min.key < H1.min.key) {
						H.min = H2.min
					}
				}
			} else {
				H.min = H2.min
			}
			H.n = H1.n + H2.n
			H
		}
		
	}

}
