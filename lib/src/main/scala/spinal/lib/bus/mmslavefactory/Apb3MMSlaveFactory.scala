package spinal.lib.bus.mmslavefactory

import spinal.core._
import spinal.lib.bus.amba3.apb.Apb3
import spinal.lib.bus.misc.SizeMapping

case class Apb3MMSlaveFactory(bus: Apb3, sizeMap: SizeMapping, selId: Int = 0, readSync: Boolean = true) extends MMSlaveFactory {

  val readError = Bool()
  val readData  = Bits(bus.config.dataWidth bits)

  if(readSync) {
    readError.setAsReg() init False
    readData.setAsReg()  init 0
  } else {
    readError := False
    readData  := 0
  }

  bus.PREADY := True
  bus.PRDATA := readData
  if(bus.config.useSlaveError) bus.PSLVERROR := readError

  val writeReq  = (bus.PSEL(selId) && bus.PENABLE && bus.PWRITE).allowPruning()
  val readReq   = (bus.PSEL(selId) && bus.PENABLE && !bus.PWRITE).allowPruning()
  val writeResp = (bus.PSEL(selId) && bus.PENABLE && bus.PREADY &&  bus.PWRITE).allowPruning()
  val readResp  = (bus.PSEL(selId) && bus.PENABLE && bus.PREADY && !bus.PWRITE).allowPruning()
  val writeData = bus.PWDATA

  override def readAddress()  = bus.PADDR
  override def writeAddress() = bus.PADDR

  override def readHalt()  = bus.PREADY := False
  override def writeHalt() = bus.PREADY := False

  override def readAccept() = {

  }

  override def writeAccept() = {
    
  }

  override def readRespond(data : Bits, error : Boolean) = {
    readData := data
    if(bus.config.useSlaveError) {
      if(error)
        readError := True
    }
  }

  override def writeRespond(error : Boolean) = {
    if(bus.config.useSlaveError) {
      if(error)
        readError := True
    }
  }

  override def busDataWidth   = bus.config.dataWidth
}
