import Test.Hspec

import Router.Layers.Ethernet as Ethernet
import Router.Layers as Layers
import Router.Packet as Packet

import Data.Generics
import Data.Text as Text

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

      (Packet.hasField "sourceMac" ethernetLayer) `shouldBe` True
      (Packet.hasField "notexistantfield" ethernetLayer) `shouldBe` False
    
    it "Should " $ do
      let eth = Layers.Ethernet [] [] Ethernet.IPv4Packet
          ethernetLayer = Layers.PacketLinkLayer (Layers.LinkLayerEth eth)
          ip = Layers.Ip 0 0 0 0 0 0 0 0 0 0 [] ethernetLayer
          ipLayer = Layers.PacketNetworkLayer (Layers.NetworkLayerIp ip)
          tcp = Layers.Tcp 0 0 0 0 0 0 0 0 0 0 0 ipLayer
          tcpLayer = Layers.PacketTransferLayer (Layers.TransferLayerTcp tcp)

          packet = Packet.Packet tcpLayer
          fields = Packet.getFields packet
          recursiveFields = Packet.getRecursiveFields (topLayer packet)

      _ <- putStrLn (Prelude.show recursiveFields)
      -- _ <- putStrLn (Prelude.show fields)
      True `shouldBe` True
