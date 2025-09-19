import Test.Hspec

import Router.Layers.Ethernet as Ethernet
import Router.Layers as Layers
import Router.Packet as Packet

import Data.Generics
import Data.Text as Text

main :: IO ()
main = hspec $ do
  describe "This test shoudl not fail" $ do
    it "shoud not fail" $ do
      True `shouldBe` True
  describe "Should test packet packer" $ do
    it "Should return if packet has parent and not correctly" $ do
      let eth = Layers.Ethernet [] [] Ethernet.IPv4Packet
          ethernetLayer = Layers.PacketLinkLayer (Layers.LinkLayerEth eth)
          packet = Packet.Packet ethernetLayer

      (Packet.hasField "sourceMac" ethernetLayer) `shouldBe` True
      (Packet.hasField "notexistantfield" ethernetLayer) `shouldBe` False
