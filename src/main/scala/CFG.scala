import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExprParser, FeatureExpr}
import java.io.{Writer, FileReader, BufferedReader, File}


class CFGNode(val id: Int, val kind: String, file: File, line: Int, val name: String, val fexpr: FeatureExpr) {
    def write(writer: Writer) {
        writer.write("N;" + id + ";" + kind + ";" + file.getPath + ";" + line + ";" + name + ";")
        fexpr.print(writer)
        writer.write("\n")
    }
    override def toString(): String = kind + "-" + name
    override def hashCode = id
    override def equals(that: Any) = if (that.isInstanceOf[CFGNode]) that.asInstanceOf[CFGNode].id == this.id else super.equals(that)

}

case class CFG(val nodes: Set[CFGNode], val edges: Set[(CFGNode, CFGNode, FeatureExpr)]) {
    def link(that: CFG): CFG = {

        val thatFunctions: Map[String, Set[CFGNode]] = that.nodes.filter(_.kind == "function").groupBy(e => e.name)
        var thisReplacements: Map[CFGNode, Set[CFGNode]] = Map()
        for (node <- this.nodes) {
            if (node.kind == "declaration") {
                val functions = thatFunctions.get(node.name)
                if (functions.isDefined)
                    thisReplacements += (node -> functions.get)
            }
        }

        val newThisEdges: Set[(CFGNode, CFGNode, FeatureExpr)] = this.edges.flatMap(
            e => if (thisReplacements.contains(e._2))
                thisReplacements(e._2).map(newTarget => (e._1, newTarget, e._3 and newTarget.fexpr)).toSet + ((e._1, e._2, thisReplacements(e._2).foldLeft(e._3)(_ andNot _.fexpr)))
            else Set(e)

        )

        val thisFunctions: Map[String, Set[CFGNode]] = this.nodes.filter(_.kind == "function").groupBy(e => e.name)
        var thatReplacements: Map[CFGNode, Set[CFGNode]] = Map()
        for (node <- that.nodes) {
            if (node.kind == "declaration") {
                val functions = thisFunctions.get(node.name)
                if (functions.isDefined)
                    thatReplacements += (node -> functions.get)
            }
        }

        val newThatEdges = that.edges.flatMap(
            e => if (thatReplacements.contains(e._2))
                thatReplacements(e._2).map(newTarget => (e._1, newTarget, e._3 and newTarget.fexpr)).toSet + ((e._1, e._2, thatReplacements(e._2).foldLeft(e._3)(_ andNot _.fexpr)))
            else Set(e)
        )


        new CFG((this.nodes ++ that.nodes).filter(_.fexpr.isSatisfiable()), (newThisEdges ++ newThatEdges).filter(_._3.isSatisfiable()))
    }

    def write(writer: Writer) {
        for (n <- nodes) n.write(writer)
        for ((s, t, f) <- edges) {
            writer.write("E;" + s.id + ";" + t.id + ";")
            f.print(writer)
            writer.write("\n")
        }
    }



    override def toString(): String = "CFG(" + nodes + ", " + edges + ")"

}


class CFGLoader {

    val featureExprParser = new FeatureExprParser(FeatureExprFactory.dflt)


    def loadNode(s: String, file: File): (Int, CFGNode) = {
        val fields = s.split(";")
        (fields(1).toInt, new CFGNode(IdGen.genId(), fields(2),new File(fields(3)), fields(4).toInt, fields(5), parseFExpr(fields(6))))
    }

    private def parseFExpr(s: String): FeatureExpr = featureExprParser.parse(s)

    def loadEdge(s: String): (Int, Int, FeatureExpr) = {
        val fields = s.split(";")
        (fields(1).toInt, fields(2).toInt, parseFExpr(fields(3)))
    }

    def loadFileCFG(cfgFile: File): CFG = {
        val reader = new BufferedReader(new FileReader(cfgFile))

        var nodes = Map[Int, CFGNode]()
        var edges = List[(CFGNode, CFGNode, FeatureExpr)]()

        var line = reader.readLine()
        while (line != null) {
            if (line.charAt(0) == 'N') {
                val node = loadNode(line, cfgFile)
                nodes = nodes + node
            }
            if (line.charAt(0) == 'E') {
                val (srcId, targetId, fexpr) = loadEdge(line)
                edges = (nodes(srcId), nodes(targetId), fexpr) :: edges
            }

            line = reader.readLine()
        }

        new CFG(nodes.values.toSet, edges.toSet)
    }

}


private object IdGen {
    private var nextId = 1;
    def genId(): Int = {
        nextId += 1
        nextId
    }
}
