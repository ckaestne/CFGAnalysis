package de.fosd.typechef.cfganalysis

import io.Source
import java.io.{FileInputStream, FileWriter, File}
import de.fosd.typechef.featureexpr.{FeatureExprParser, FeatureExprFactory}

/**
 * currently here to avoid complication with rebuilding all ivy/maven packages
 *
 * run in busybox directory
 */
object BusyboxCFGLinker extends App {

    import FeatureExprFactory._

    FeatureExprFactory.setDefault(FeatureExprFactory.bdd)

    val path = "/usr0/home/ckaestne/work/TypeChef/BusyboxAnalysis/gitbusybox/"
    //
    var bigCFG = new CFG(Set(), Set())

    for (file <- Source.fromFile(path + "filelist").getLines()) {
        val cfgFile = path + file + ".cfg"
        print("linking " + cfgFile)

        val pcFile = new File(path + file + ".pc")
        val filePC = if (pcFile.exists()) new FeatureExprParser().parseFile(new FileInputStream(pcFile)) else True
        val cfg = new CFGLoader().loadFileCFG(new File(cfgFile), filePC)
        println(".")

        bigCFG = bigCFG link cfg
    }
    assert(bigCFG.checkConsistency)

    println("writing result")

    val writer = new FileWriter(path + "finalcfg.cfg")
    bigCFG.write(writer)
    writer.close()

    println("done.")




    def testReduceBusybox {
        FeatureExprFactory.setDefault(FeatureExprFactory.bdd)

        println("loading")
        val cfg = new CFGLoader().loadCFG(new File("busybox.cfg"))
        println("reducing")
        val r = new ReduceCFG()
        val newcfg = r.removeSelfCycles(r.reduceMut(cfg))
        println("\nwriting")
        val w = new FileWriter("busybox_reduced.cfg")
        newcfg.write(w)
        w.close
    }

}