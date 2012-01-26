/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openmole.tools.mgo

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import de.erichseifert.gral.io.plots.DrawableWriter
import de.erichseifert.gral.io.plots.DrawableWriterFactory
import de.erichseifert.gral.plots.XYPlot
import de.erichseifert.gral.plots.points.PointRenderer
import java.awt.Color
import java.io.File
import java.io.FileOutputStream
import org.junit.runner.RunWith

import org.openmole.tools.mgo.tools.FileUtils
import de.erichseifert.gral.data.DataTable

@RunWith(classOf[JUnitRunner]) 
class FileImportSpec extends FlatSpec with ShouldMatchers{
  "FileImportSpec " should "create and initialize with good values" in {
    var path = "/home/srey/Bureau/Schaffer.pf"
    var matrixFront = FileUtils.readFront(path)
    
    var data = new DataTable(classOf[java.lang.Double], classOf[java.lang.Double])
    for(i <- 0 to matrixFront.size - 1) {
      data.add(matrixFront(i)(0), matrixFront(i)(1))
    }

    val dest = new File("/tmp/essai.png")
    val factory = DrawableWriterFactory.getInstance()
    val writer  = factory.get("image/png")
    val plot = new XYPlot(data)
    val color = new Color(0.0f, 0.3f, 1.0f)
     
    plot.getPointRenderer(data).setSetting(PointRenderer.COLOR, color)
    writer.write(plot, new FileOutputStream(dest),400,400)
    
  }
}
