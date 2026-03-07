package ateams.backend

import ateams.syntax.Program.ASystem
import ateams.backend.Semantics.Loc
import ateams.backend.Semantics.St

object BufferSizes:
  /**
    * Returns a map with the maximum size of each buffer found during the traversal
    * of the state space, the number of edges traversed, and whether the traversal
    * was completed (true) or stopped due to reaching the limit (false).
    *
    * @param sy the system to analyze
    * @param max the maximum number of edges to traverse before stopping the analysis (to avoid infinite traversals in case of unbounded buffers)
    * @return a tuple containing the map of maximum buffer sizes, the number of edges traversed, and a boolean indicating whether the traversal was completed
    */
  def apply(sy: ASystem, max: Int=2000): (Map[Loc,Int],Int,Boolean) =
    var maxSizes = Map[Loc,Int]()
    // variation of the SOS.traverse method that also counts the maximum buffer size found during the traversal.
    def aux(next:Set[St],done:Set[St],edges:Int, limit:Int): (Set[St],Int,Boolean) =
      if limit <=0 then
        return (done,edges,false)
      next.headOption match
        case None =>
          (done, edges, true)
        case Some(st) if done contains st =>
          aux(next-st,done,edges,limit)
        case Some(st) => //visiting new state
          maxSizes = maxSizes ++
            st.buffers.map(b => b._1 -> math.max(maxSizes.getOrElse(b._1,0), b._2.size))
          val more = Semantics.next(st)
          aux((next-st)++more.map(_._2), done+st, edges+more.size,limit-more.size)

    val (_,edges,done) = aux(Set(St(sy,Map())), Set(), 0, max)
    (maxSizes,edges,done)

