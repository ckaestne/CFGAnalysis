import de.fosd.typechef.featureexpr.FeatureExprFactory._
import java.io._
import junit.framework.Assert._
import org.junit.{Test}

class ReduceCFGTest {

    val f1 = new CFGNode(1,"function", null, -1, "foo", True)
    val f2 = new CFGNode(2,"function", null, -1, "bar", True)
    val s1 = new CFGNode(3,"statement", null, -1, "s1", True)
    val s2 = new CFGNode(4,"statement", null, -1, "s2", True)

    val fa = createDefinedExternal("A")
    val fb = createDefinedExternal("B")
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
    def testReduceBusybox {
        println("loading")
        val cfg = new CFGLoader().loadFileCFG(new File("busybox.cfg"))
        println("reducing")
        val r = new ReduceCFG()
        val newcfg = r.removeSelfCycles(r.reduceMut(cfg))
        println("\nwriting")
        val w = new FileWriter("busybox_reduced.cfg")
        newcfg.write(w)
        w.close
    }
}