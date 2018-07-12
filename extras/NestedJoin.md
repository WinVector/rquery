NestedJoin
================

``` r
library("dplyr")
```

    ## Warning: package 'dplyr' was built under R version 3.5.1

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library("sparklyr")
db <- sparklyr::spark_connect(version='2.2.0', 
                              master = "local")

d <- data.frame(key = 1, 
                val = "a", 
                stringsAsFactors = FALSE)
d1 <- dplyr::copy_to(db, d, "d1", overwrite = TRUE)
d2 <- dplyr::copy_to(db, d, "d2", overwrite = TRUE)
d3 <- dplyr::copy_to(db, d, "d3", overwrite = TRUE)

# works
d1 %>% 
  left_join(., d2, by = "key", suffix = c("_x", "_y")) %>% 
  left_join(., d3, by = "key", suffix = c("_x", "_y"))
```

    ## # Source:   lazy query [?? x 4]
    ## # Database: spark_connection
    ##     key val_x val_y val  
    ##   <dbl> <chr> <chr> <chr>
    ## 1     1 a     a     a

``` r
# bad query
d1 %>% 
  left_join(., d2, by = "key") %>% 
  left_join(., d3, by = "key") %>%
  dbplyr::remote_query(.)
```

    ## <SQL> SELECT `TBL_LEFT`.`key` AS `key`, `TBL_LEFT`.`val`.`x` AS `val.x`, `TBL_LEFT`.`val`.`y` AS `val.y`, `TBL_RIGHT`.`val` AS `val`
    ##   FROM (SELECT `TBL_LEFT`.`key` AS `key`, `TBL_LEFT`.`val` AS `val.x`, `TBL_RIGHT`.`val` AS `val.y`
    ##   FROM `d1` AS `TBL_LEFT`
    ##   LEFT JOIN `d2` AS `TBL_RIGHT`
    ##   ON (`TBL_LEFT`.`key` = `TBL_RIGHT`.`key`)
    ## ) `TBL_LEFT`
    ##   LEFT JOIN `d3` AS `TBL_RIGHT`
    ##   ON (`TBL_LEFT`.`key` = `TBL_RIGHT`.`key`)

``` r
# fails
d1 %>% 
  left_join(., d2, by = "key") %>% 
  left_join(., d3, by = "key")
```

    ## Error: org.apache.spark.sql.AnalysisException: cannot resolve '`TBL_LEFT.val.x`' given input columns: [val.x, key, val, val.y, key]; line 1 pos 34;
    ## 'Project [key#273 AS key#276, 'TBL_LEFT.val.x AS val.x#277, 'TBL_LEFT.val.y AS val.y#278, val#159 AS val#279]
    ## +- Join LeftOuter, (key#273 = key#158)
    ##    :- SubqueryAlias TBL_LEFT
    ##    :  +- Project [key#12 AS key#273, val#13 AS val.x#274, val#86 AS val.y#275]
    ##    :     +- Join LeftOuter, (key#12 = key#85)
    ##    :        :- SubqueryAlias TBL_LEFT
    ##    :        :  +- SubqueryAlias d1
    ##    :        :     +- LogicalRDD [key#12, val#13]
    ##    :        +- SubqueryAlias TBL_RIGHT
    ##    :           +- SubqueryAlias d2
    ##    :              +- LogicalRDD [key#85, val#86]
    ##    +- SubqueryAlias TBL_RIGHT
    ##       +- SubqueryAlias d3
    ##          +- LogicalRDD [key#158, val#159]
    ## 
    ##  at org.apache.spark.sql.catalyst.analysis.package$AnalysisErrorAt.failAnalysis(package.scala:42)
    ##  at org.apache.spark.sql.catalyst.analysis.CheckAnalysis$$anonfun$checkAnalysis$1$$anonfun$apply$2.applyOrElse(CheckAnalysis.scala:88)
    ##  at org.apache.spark.sql.catalyst.analysis.CheckAnalysis$$anonfun$checkAnalysis$1$$anonfun$apply$2.applyOrElse(CheckAnalysis.scala:85)
    ##  at org.apache.spark.sql.catalyst.trees.TreeNode$$anonfun$transformUp$1.apply(TreeNode.scala:289)
    ##  at org.apache.spark.sql.catalyst.trees.TreeNode$$anonfun$transformUp$1.apply(TreeNode.scala:289)
    ##  at org.apache.spark.sql.catalyst.trees.CurrentOrigin$.withOrigin(TreeNode.scala:70)
    ##  at org.apache.spark.sql.catalyst.trees.TreeNode.transformUp(TreeNode.scala:288)
    ##  at org.apache.spark.sql.catalyst.trees.TreeNode$$anonfun$3.apply(TreeNode.scala:286)
    ##  at org.apache.spark.sql.catalyst.trees.TreeNode$$anonfun$3.apply(TreeNode.scala:286)
    ##  at org.apache.spark.sql.catalyst.trees.TreeNode$$anonfun$4.apply(TreeNode.scala:306)
    ##  at org.apache.spark.sql.catalyst.trees.TreeNode.mapProductIterator(TreeNode.scala:187)
    ##  at org.apache.spark.sql.catalyst.trees.TreeNode.mapChildren(TreeNode.scala:304)
    ##  at org.apache.spark.sql.catalyst.trees.TreeNode.transformUp(TreeNode.scala:286)
    ##  at org.apache.spark.sql.catalyst.plans.QueryPlan$$anonfun$transformExpressionsUp$1.apply(QueryPlan.scala:268)
    ##  at org.apache.spark.sql.catalyst.plans.QueryPlan$$anonfun$transformExpressionsUp$1.apply(QueryPlan.scala:268)
    ##  at org.apache.spark.sql.catalyst.plans.QueryPlan.transformExpression$1(QueryPlan.scala:279)
    ##  at org.apache.spark.sql.catalyst.plans.QueryPlan.org$apache$spark$sql$catalyst$plans$QueryPlan$$recursiveTransform$1(QueryPlan.scala:289)
    ##  at org.apache.spark.sql.catalyst.plans.QueryPlan$$anonfun$org$apache$spark$sql$catalyst$plans$QueryPlan$$recursiveTransform$1$1.apply(QueryPlan.scala:293)
    ##  at scala.collection.TraversableLike$$anonfun$map$1.apply(TraversableLike.scala:234)
    ##  at scala.collection.TraversableLike$$anonfun$map$1.apply(TraversableLike.scala:234)
    ##  at scala.collection.immutable.List.foreach(List.scala:381)
    ##  at scala.collection.TraversableLike$class.map(TraversableLike.scala:234)
    ##  at scala.collection.immutable.List.map(List.scala:285)
    ##  at org.apache.spark.sql.catalyst.plans.QueryPlan.org$apache$spark$sql$catalyst$plans$QueryPlan$$recursiveTransform$1(QueryPlan.scala:293)
    ##  at org.apache.spark.sql.catalyst.plans.QueryPlan$$anonfun$6.apply(QueryPlan.scala:298)
    ##  at org.apache.spark.sql.catalyst.trees.TreeNode.mapProductIterator(TreeNode.scala:187)
    ##  at org.apache.spark.sql.catalyst.plans.QueryPlan.mapExpressions(QueryPlan.scala:298)
    ##  at org.apache.spark.sql.catalyst.plans.QueryPlan.transformExpressionsUp(QueryPlan.scala:268)
    ##  at org.apache.spark.sql.catalyst.analysis.CheckAnalysis$$anonfun$checkAnalysis$1.apply(CheckAnalysis.scala:85)
    ##  at org.apache.spark.sql.catalyst.analysis.CheckAnalysis$$anonfun$checkAnalysis$1.apply(CheckAnalysis.scala:78)
    ##  at org.apache.spark.sql.catalyst.trees.TreeNode.foreachUp(TreeNode.scala:127)
    ##  at org.apache.spark.sql.catalyst.analysis.CheckAnalysis$class.checkAnalysis(CheckAnalysis.scala:78)
    ##  at org.apache.spark.sql.catalyst.analysis.Analyzer.checkAnalysis(Analyzer.scala:91)
    ##  at org.apache.spark.sql.execution.QueryExecution.assertAnalyzed(QueryExecution.scala:52)
    ##  at org.apache.spark.sql.Dataset$.ofRows(Dataset.scala:66)
    ##  at org.apache.spark.sql.SparkSession.sql(SparkSession.scala:623)
    ##  at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
    ##  at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:62)
    ##  at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)
    ##  at java.lang.reflect.Method.invoke(Method.java:497)
    ##  at sparklyr.Invoke.invoke(invoke.scala:137)
    ##  at sparklyr.StreamHandler.handleMethodCall(stream.scala:123)
    ##  at sparklyr.StreamHandler.read(stream.scala:66)
    ##  at sparklyr.BackendHandler.channelRead0(handler.scala:51)
    ##  at sparklyr.BackendHandler.channelRead0(handler.scala:4)
    ##  at io.netty.channel.SimpleChannelInboundHandler.channelRead(SimpleChannelInboundHandler.java:105)
    ##  at io.netty.channel.AbstractChannelHandlerContext.invokeChannelRead(AbstractChannelHandlerContext.java:357)
    ##  at io.netty.channel.AbstractChannelHandlerContext.invokeChannelRead(AbstractChannelHandlerContext.java:343)
    ##  at io.netty.channel.AbstractChannelHandlerContext.fireChannelRead(AbstractChannelHandlerContext.java:336)
    ##  at io.netty.handler.codec.MessageToMessageDecoder.channelRead(MessageToMessageDecoder.java:102)
    ##  at io.netty.channel.AbstractChannelHandlerContext.invokeChannelRead(AbstractChannelHandlerContext.java:357)
    ##  at io.netty.channel.AbstractChannelHandlerContext.invokeChannelRead(AbstractChannelHandlerContext.java:343)
    ##  at io.netty.channel.AbstractChannelHandlerContext.fireChannelRead(AbstractChannelHandlerContext.java:336)
    ##  at io.netty.handler.codec.ByteToMessageDecoder.fireChannelRead(ByteToMessageDecoder.java:293)
    ##  at io.netty.handler.codec.ByteToMessageDecoder.channelRead(ByteToMessageDecoder.java:267)
    ##  at io.netty.channel.AbstractChannelHandlerContext.invokeChannelRead(AbstractChannelHandlerContext.java:357)
    ##  at io.netty.channel.AbstractChannelHandlerContext.invokeChannelRead(AbstractChannelHandlerContext.java:343)
    ##  at io.netty.channel.AbstractChannelHandlerContext.fireChannelRead(AbstractChannelHandlerContext.java:336)
    ##  at io.netty.channel.DefaultChannelPipeline$HeadContext.channelRead(DefaultChannelPipeline.java:1294)
    ##  at io.netty.channel.AbstractChannelHandlerContext.invokeChannelRead(AbstractChannelHandlerContext.java:357)
    ##  at io.netty.channel.AbstractChannelHandlerContext.invokeChannelRead(AbstractChannelHandlerContext.java:343)
    ##  at io.netty.channel.DefaultChannelPipeline.fireChannelRead(DefaultChannelPipeline.java:911)
    ##  at io.netty.channel.nio.AbstractNioByteChannel$NioByteUnsafe.read(AbstractNioByteChannel.java:131)
    ##  at io.netty.channel.nio.NioEventLoop.processSelectedKey(NioEventLoop.java:643)
    ##  at io.netty.channel.nio.NioEventLoop.processSelectedKeysOptimized(NioEventLoop.java:566)
    ##  at io.netty.channel.nio.NioEventLoop.processSelectedKeys(NioEventLoop.java:480)
    ##  at io.netty.channel.nio.NioEventLoop.run(NioEventLoop.java:442)
    ##  at io.netty.util.concurrent.SingleThreadEventExecutor$2.run(SingleThreadEventExecutor.java:131)
    ##  at io.netty.util.concurrent.DefaultThreadFactory$DefaultRunnableDecorator.run(DefaultThreadFactory.java:144)
    ##  at java.lang.Thread.run(Thread.java:745)

``` r
sparklyr::spark_disconnect(db)
```
