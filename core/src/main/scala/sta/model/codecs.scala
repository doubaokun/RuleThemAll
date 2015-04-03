//package sta.model
//
//import scala.collection.concurrent.TrieMap
//import scala.reflect.ClassTag
//import scalaz.\/
//
//trait Codec[From, To] {
//  protected def ct: ClassTag[From]
//
//  final def clazz: Class[_] = ct.runtimeClass
//
//  def encode(from: From): To
//  def decode(from: To): \/[From, Exception]
//}
//
//object CodecsRegistry {
//  type To = Array[Byte]
//
//  private val _codecs: TrieMap[Class[_], Codec[_, To]] = TrieMap.empty
//
//  def +=(codec: Codec[_, To]): Unit = {
//    _codecs += (codec.clazz -> codec)
//  }
//
//  @inline def get[From](clazz: Class[_]): Option[Codec[From, To]] = _codecs.get(clazz).map(_.asInstanceOf[Codec[From, To]])
//
//  @inline def apply[From](clazz: Class[_]): Codec[From, To] = _codecs(clazz).asInstanceOf[Codec[From, To]]
//}

