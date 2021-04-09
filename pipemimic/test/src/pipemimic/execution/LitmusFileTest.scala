package pipemimic.execution

object LitmusFileTest extends App {
  for (file <- args)
    LitmusTestConstructor(filePath = file)
}
