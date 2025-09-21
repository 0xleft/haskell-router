import Test.Hspec

import Router.Layers.Ethernet as Ethernet
    ( PacketType(IPv4Packet) )
import Router.Layers as Layers
    ( Ethernet(Ethernet),
      Ip(Ip),
      LinkLayer(LinkLayerEth),
      NetworkLayer(NetworkLayerIp),
      PacketLayer(PacketTransferLayer, PacketLinkLayer,
                  PacketNetworkLayer),
      Tcp(Tcp),
      TransferLayer(TransferLayerTcp) )
import Router.Packet as Packet ( Packet(topLayer, Packet) )
import Router.Packer as Packer
    ( convertFields, getFields, getRecursiveFields, hasField, pack )
import Network.Pcap (openLive, loopBS, sendPacketBS)

main :: IO ()
main = hspec $ do
  describe "Sanity test" $ do
    it "Shoud not fail" $ do
      True `shouldBe` True

  describe "Should test packet packer" $ do
    it "Should return if packet has parent and not correctly" $ do
      let eth = Layers.Ethernet [] [] Ethernet.IPv4Packet
          ethernetLayer = Layers.PacketLinkLayer (Layers.LinkLayerEth eth)
          packet = Packet.Packet ethernetLayer

      (Packer.hasField "sourceMac" ethernetLayer) `shouldBe` True
      (Packer.hasField "notexistantfield" ethernetLayer) `shouldBe` False
    
    it "Should " $ do -- todo actualy finish this test
      let eth = Layers.Ethernet [0] [0] Ethernet.IPv4Packet
          ethernetLayer = Layers.PacketLinkLayer (Layers.LinkLayerEth eth)
          ip = Layers.Ip 0 0 0 0 0 0 0 0 0 0 [] ethernetLayer
          ipLayer = Layers.PacketNetworkLayer (Layers.NetworkLayerIp ip)
          tcp = Layers.Tcp 0 0 0 0 0 0 0 0 0 0 0 ipLayer
          tcpLayer = Layers.PacketTransferLayer (Layers.TransferLayerTcp tcp)

          

          packet = Packet.Packet tcpLayer
          fields = Packer.getFields packet
          recursiveFields = Packer.getRecursiveFields (topLayer packet)
          convertedFields = Packer.convertFields recursiveFields 

      _ <- putStrLn (Prelude.show $ Packer.pack packet)
      -- _ <- putStrLn (Prelude.show fields)
      True `shouldBe` True
