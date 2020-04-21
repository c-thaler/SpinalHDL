/******************************************************************************
  *  This file describes the Axi4 Stream interface
  *
  *   _______________________
  *  | Global | Stream Data |
  *  |   -    |             |
  *  |-----------------------
  *  |  aclk  |  *tid       |
  *  |  arstn |  *tdata     |
  *  |        |  *tstrb     |
  *  |        |  *tlast     |
  *  |        |  *tuser     |
  *  |        |  tvalid     |
  *  |        |  *tready    |
  *  |        |  *tdest     |
  *  |        |  *tkeep     |
  *  |        |             |
  *  |        |             |
  *  |        |             |
  *  |        |             |
  *  |________|_____________|
  *   * Optional signal
  */

package spinal.lib.bus.amba4.axistream

import spinal.core._
import spinal.lib._

/**
 * Configuration class for the Axi4 stream
 */
case class Axi4StreamConfig(dataWidth    : Int,
                            idWidth      : Int = -1,
                            userWidth    : Int = -1,
                            destWidth    : Int = -1,
                            useLast      : Boolean = true,
                            useStrb      : Boolean = false,
                            useKeep      : Boolean = false,
                            useReady     : Boolean = true) {

  def useData = dataWidth >= 0
  def useId   = idWidth   >= 0
  def useUser = userWidth >= 0
  def useDest = destWidth >= 0

  if(useStrb || useKeep)
    require(dataWidth >= 0, "tstrb and tkeep depend on tdata")
  
  def bytePerWord = dataWidth/8
  def symbolRange = log2Up(bytePerWord)-1 downto 0

  def dataType = Bits(dataWidth bits)
  def idType   = UInt(idWidth bits)
  def strbType = Bits(bytePerWord bits)
  def keepType = Bits(bytePerWord bits)
  def destType = UInt(destWidth bits)
  def userType = Bits(userWidth bits)
}

/**
 * Definition of the Stream channel
 * @param config Axi4 Stream configuration class
 */
class Axi4StreamT (val config: Axi4StreamConfig) extends Bundle {
  val data     = if(config.useData) Bits(config.dataWidth bits)   else null
  val id       = if(config.useId)   UInt(config.idWidth bits)     else null
  val dest     = if(config.useDest) UInt(config.destWidth bits)   else null
  val keepByte = if(config.useKeep) Bits(config.bytePerWord bits) else null
  val strb     = if(config.useStrb) Bits(config.bytePerWord bits) else null
  val user     = if(config.useUser) Bits(config.userWidth bits)   else null
  val last     = if(config.useLast) Bool                          else null

  override def clone: this.type = new Axi4StreamT(config).asInstanceOf[this.type]
}

/**
 * Axi4 Stream interface definition
 * @param config Axi4 Stream configuration class
 */
case class Axi4Stream(config: Axi4StreamConfig) extends Bundle with IMasterSlave {

    val t = Stream(Axi4StreamT(config))

    def stream = t

    def <<(that : Axi4Stream) : Unit = that >> this
    def >> (that : Axi4Stream) : Unit = {
      this.stream drive that.stream
    }

    override def asMaster(): Unit = {
        master(t)
    }
}

object Axi4StreamPriv{

  def driveWeak[T <: Data](source : Bundle,sink : Bundle, by : T, to : T, defaultValue : () => T, allowResize : Boolean, allowDrop : Boolean) : Unit = {
    (to != null,by != null) match {
      case (false,false) =>
      case (true,false) => if(defaultValue != null) to := defaultValue() else LocatedPendingError(s"$source can't drive $to because the former doesn't have the corresponding pin")
      case (false,true) => if(!allowDrop) LocatedPendingError(s"$by can't drive $sink because the latter doesn't have the corresponding pin")
      case (true,true) => to := (if(allowResize) by.resized else by)
    }
  }

  def driveT[T <: Axi4StreamT](stream: Stream[T],sink: Stream[T]): Unit = {
    sink.arbitrationFrom(stream)
    assert(stream.config.idWidth <= sink.config.idWidth, s"$stream idWidth > $sink idWidth")
    assert(stream.config.destWidth <= sink.config.destWidth, s"$stream  destWidth > $sink destWidth")

    driveWeak(stream, sink, stream.id,       sink.id,       () => U(sink.id.range -> false),      true,  false)
    driveWeak(stream, sink, stream.dest,     sink.dest,     () => U(sink.dest.range -> false),    true,  false)
    driveWeak(stream, sink, stream.user,     sink.user,     () => B(sink.user.range -> false),    false, true)
    driveWeak(stream, sink, stream.keepByte, sink.keepByte, () => B(sink.keepByte.range -> true), false, false)
    driveWeak(stream, sink, stream.strb,     sink.strb,
      () => if(stream.config.useKeep) stream.keepByte else B(sink.strb.range -> true),            false, false)
    driveWeak(stream, sink, stream.last,     sink.last,     () => True,                           false, true)
    driveWeak(stream, sink, stream.data,     sink.data,     () => B(sink.data.range -> false),    false, true)
  }
}

object Axi4StreamT {
  def apply(config: Axi4StreamConfig) = new Axi4StreamT(config)

  implicit class StreamPimper(stream : Stream[Axi4StreamT]) {
    def drive(sink: Stream[Axi4StreamT]): Unit = Axi4StreamPriv.driveT(stream,sink)
  }
}