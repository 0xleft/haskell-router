import Test.Hspec ( hspec, describe, it, shouldBe )
import Router.Layers.Ethernet as Ethernet
import Router.Layers as Layers
import Router.Packet as Packet
import Router.Packer as Packer
import Network.Pcap (openLive, sendPacketBS)

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
      let eth = Layers.Ethernet [0, 0, 0, 0, 0, 0] [0, 0, 0, 0, 0, 0] Ethernet.IPv4Packet
          ethernetLayer = Layers.PacketLinkLayer (Layers.LinkLayerEth eth)
          ip = Layers.Ip 0x45 0 0 0 0 0 0 0 0 0 [] ethernetLayer
          ipLayer = Layers.PacketNetworkLayer (Layers.NetworkLayerIp ip)
          tcp = Layers.Tcp 0 0 0 0 0 0 0 0 0 0 0 ipLayer
          tcpLayer = Layers.PacketTransferLayer (Layers.TransferLayerTcp tcp)

          packet = Packet.Packet tcpLayer
          fields = Packer.getFields packet
          recursiveFields = Packer.getRecursiveFields (topLayer packet)
          convertedFields = Packer.convertFields recursiveFields 

          packed = Packer.pack packet

      putStrLn $ Prelude.show packed

      -- interface <- openLive "wlo1" 65535 False 0
      -- sendPacketBS interface ptr len

      -- loopBS interface (-1) (\header body -> putStrLn (show body))

      -- _ <- putStrLn (Prelude.show fields)
      True `shouldBe` True
  
  describe "Should test packet parser" $ do
    it "Shoud " $ do
      True `shouldBe` True
