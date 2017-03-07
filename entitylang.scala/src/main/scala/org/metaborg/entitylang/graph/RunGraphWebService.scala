package org.metaborg.entitylang.graph

import java.io.File
import java.nio.file._

import org.metaborg.entitylang.analysis.{Analyzer, AttributeNodeData, EntityFieldNode, EntityNode}

import scala.collection.JavaConversions._
import org.metaborg.entitylang.graph.webservice.GraphWebService
import org.metaborg.entitylang.parser.EntityLangParserProvider
import org.metaborg.entitylang.util._

import scala.io.Source

object RunGraphWebService extends App{
  implicit val ctx = new org.strategoxt.lang.Context()

  if(args.size != 1){
    println("usage: [file-to-watch]")
  } else{
    GraphWebService.startService(ctx)
    val file = args(0)
    process(new File(file).toPath)
    watch(file)
  }

  def watch(filename: String): Unit ={
    val f = new File(args(0))
    if(!f.exists() || f.isDirectory){
      println("file does not exist: " + f.getAbsolutePath)
    } else{
      import StandardWatchEventKinds._

      val path = f.toPath.toAbsolutePath
      val dir = f.getParentFile.toPath.toAbsolutePath

      val filewatcher = FileSystems.getDefault.newWatchService()
      val watchKey = path.getParent.register(filewatcher, ENTRY_MODIFY)

      while(true){
        val key = filewatcher.take()
        for(e <- key.pollEvents()){
          if(e.kind() == StandardWatchEventKinds.ENTRY_MODIFY){
            val file = e.context().asInstanceOf[Path].toAbsolutePath
            val modifiedPath = dir.resolve(file.getFileName.toString)
            if(modifiedPath.equals(path)){
              process(modifiedPath)
            }
          }
        }
        key.reset()
      }
    }
  }

  def process(path: Path): Unit ={
    EntityLangParserProvider.parser.tryParseFile(path.toFile.getAbsolutePath) match{
      case Right(ast) => {
        val model = Analyzer.analyze(ast)
        GraphWebService.pushModel(model)
      }
      case Left(errors) => {
        println("parsing failed:")
        errors.foreach(println)
      }
    }
  }
}
