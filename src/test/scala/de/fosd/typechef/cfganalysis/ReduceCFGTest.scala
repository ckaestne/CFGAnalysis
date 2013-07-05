package de.fosd.typechef.cfganalysis

import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory}
import de.fosd.typechef.featureexpr.FeatureExprFactory._
import java.io._
import junit.framework.Assert._
import org.junit.Test

class ReduceCFGTest {

    val f1 = new CFGNode(1,"function", null, -1, "foo", True)
    val f2 = new CFGNode(2,"function", null, -1, "bar", True)
    val s1 = new CFGNode(3,"statement", null, -1, "s1", True)
    val s2 = new CFGNode(4,"statement", null, -1, "s2", True)
    val s3 = new CFGNode(5,"statement", null, -1, "s3", True)
    val s4 = new CFGNode(6,"statement", null, -1, "s4", True)

    val fa = createDefinedExternal("A")
    val fb = createDefinedExternal("B")
    val fc = createDefinedExternal("C")
    @Test
    def testReduction {

        val cfg = new CFG(Set(f1, f2, s1, s2), Set(
            (f1, s1, True),
            (s1, s2, True),
            (s2, f1, True),
            (s2, f2, True)
        ))

        val out = new ReduceCFG().reduceMut(cfg)

        val expected = new CFG(Set(f1, f2), Set(
            (f1, f2, True),
            (f1, f1, True)
        ))
        val expected2 = new CFG(Set(f1, f2), Set(
            (f1, f2, True)
        ))

        assertEquals(expected, out)

        assertEquals(expected2, new ReduceCFG().removeSelfCycles(out))

    }

    @Test
    def testReduction2 {

        val cfg = new CFG(Set(f1, f2, s1, s2), Set(
            (f1, s1, fa),
            (s1, s2, fb),
            (s1, f1, fb.not),
            (s2, f1, True),
            (s2, f2, True)
        ))

        val out = new ReduceCFG().reduceMut(cfg)

        val expected2 = new CFG(Set(f1, f2), Set(
            (f1, f2, fa and fb)
        ))
        val expected3 = new CFG(Set(f1, f2), Set(
            (f1, f2, fa and fb),
                (f1, f1, fa)
        ))


        assertEquals(expected2, new ReduceCFG().removeSelfCycles(out))
        assertEquals(expected3, new ReduceCFG().compressRedundantEdges(out))

    }

    @Test
    def testRedundantEdges() {
        val cfg = new CFG(Set(f1, f2, s1, s2), Set(
            (f1, f2, fa),
            (f1, f2, fb)
        ))

        val expected2 = new CFG(Set(f1, f2, s2, s1), Set(
            (f1, f2, fa or fb)
        ))



        assertEquals(expected2, new ReduceCFG().compressRedundantEdges(cfg))
    }

    @Test
    def testVAReachableNodes {
        val cfg = new CFG(Set(s3, s4, s1, s2), Set(
            (s1, s2, fc),
            (s1, s3, fa),
            (s2, s3, fb),
            (s3, s4, True)
        ))

        val reach = new ReduceCFG().getVAReachableNodes(cfg, s1, True)

        println(reach)

        assertEquals(True, reach(s1))
        assertEquals(fc, reach(s2))
        assertEquals(fa or (fb and fc), reach(s3))
        assertEquals(fa or (fb and fc), reach(s4))
    }

    @Test
    def testVAReachableNodesCircle {
        val cfg = new CFG(Set(s3, s4, s1, s2), Set(
            (s1, s2, fc),
            (s1, s3, fa),
            (s2, s3, fb),
            (s3, s4, True),
                (s4, s2, fa)
        ))

        val reach = new ReduceCFG().getVAReachableNodes(cfg, s1, True)

        println(reach)

        assertEquals(True, reach(s1))
        assertEquals(fc or fa, reach(s2))
        assertEquals(fa or (fb and fc), reach(s3))
        assertEquals(fa or (fb and fc), reach(s4))
    }




    @Test
    def testRemoveUnreachableNodes {
        //remove nodes that are not target or source of any edge
        val cfg = new CFGLoader().loadCFG(new File("busybox_reduced.cfg"))
        val allSourceOrTargetNodes = cfg.edges.map(_._1).toSet ++ cfg.edges.map(_._2).toSet
        println(cfg.nodes.size)
        println(allSourceOrTargetNodes.size)

        // no outgoing edges from declarations
        assert(cfg.edges.map(_._1).filter(_.kind=="declaration").map(_.name).size == 0)

        val newcfg= new CFG(allSourceOrTargetNodes, cfg.edges)
        val w = new FileWriter("busybox_reduced2.cfg")
        newcfg.write(w)
        w.close
    }

    val blacklistedInlineFunctions =
        """
          |bb_strtol
          |bb_strtoi32
          |xatol_range
          |bb_strtou32
          |xatoul_range
          |xstrtoul_sfx
          |xatoul
          |bb_strtoul
          |xstrtoul_range
          |bb_ascii_isalnum
          |xstrtol_range_sfx
          |xstrtol
          |xstrtoul
          |xatou32
          |xatoul_sfx
          |xatol_sfx
          |xstrtoul_range_sfx
          |fileAction
          |xatol
          |xstrtol_range
          |xatoul_range_sfx
          |xatol_range_sfx
        """.stripMargin.split("\n").map(_.trim).filterNot(_.isEmpty)

    @Test
    def cfgMetrics {
        val cfg = new CFGLoader().loadCFG(new File("busybox_reduced2.cfg"))

        val functions = cfg.nodes.filter(_.kind=="function").filterNot(blacklistedInlineFunctions contains _.name)
        val functionscount = functions.size
        val uniquefunctionscount = functions.map(_.name).toSet.size

        println(functionscount)
        println(uniquefunctionscount)

//        println(functions.map(_.name).filter(_ contains "main").toList.sorted.mkString("\n"))

        val mainFuns = functions.filter(_.name contains "main")

        var totalReach = Set[CFGNode]()
        val r = new ReduceCFG()
        for (mainFun <- mainFuns) {
            val reachable = r.getReachableNodes(cfg, mainFun).filter(_.kind=="function").filterNot(blacklistedInlineFunctions contains _.name)
            totalReach = totalReach ++ reachable
            println(mainFun.name+":\t"+ reachable.size+"\t"+(100*reachable.size/functionscount))
        }
        println( totalReach.size+"\t"+(100*totalReach.size/functionscount))
        println( (functions -- totalReach).size+"\t"+(100*(functions -- totalReach).size/functionscount))
        println((functions -- totalReach).map(_.name).mkString("\n"))
    }

    //copied from http://stackoverflow.com/questions/1262741/scala-how-to-merge-a-collection-of-maps/1264772#1264772
    private def mergeMap[A, B](ms: List[Map[A, B]])(f: (B, B) => B): Map[A, B] =
        (Map[A, B]() /: (for (m <- ms; kv <- m) yield kv)) {
            (a, kv) =>
                a + (if (a.contains(kv._1)) kv._1 -> f(a(kv._1), kv._2) else kv)
        }

    @Test
    def cfgMetricsVA {

        val cfg = new CFGLoader().loadCFG(new File("busybox_reduced2.cfg"))

        val functions = cfg.nodes.filter(_.kind=="function").filterNot(blacklistedInlineFunctions contains _.name)
        val functionscount = functions.size
        val uniquefunctionscount = functions.map(_.name).toSet.size

        val mainFuns = functions.filter(_.name contains "main")

        var totalReach = functions.map(n=>(n,False)).toMap
        val r = new ReduceCFG()
        for (mainFun <- mainFuns) {
            val reachable = r.getVAReachableNodes(cfg, mainFun, mainFun.fexpr).filter(_._1.kind=="function").filterNot(blacklistedInlineFunctions contains _._1.name)
            totalReach = mergeMap(List(totalReach , reachable))(_ or _)
            println(mainFun.name+":\t"+ reachable.size+"\t"+(100*reachable.size/functionscount))
        }

        val out = totalReach.mkString("\n")
//        println(out)

        val w = new FileWriter("busybox_reachability.cfg")
        w.write(out)
        w.close
    }
}